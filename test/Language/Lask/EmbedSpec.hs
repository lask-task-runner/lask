{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Lask.EmbedSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_)
import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Command.Lask.Envs (EnvRef (..), collectEnvReadsFrom, collectEnvRefsFrom)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Core.AST
import Language.Lask.Core.Pretty (renderDecl)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), elaborateProgram)
import Language.Lask.Embed
import qualified Language.Lask.Embed.Do as L
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Value (EnvValue (..), LaskFailure (..), Value (..), runtimeFailure)
import Language.Lask.Span (Span (..))
import Test.Hspec hiding (parallel)

-- One program, twice: as .lask source and in the embedding --------------------

source :: Text
source =
  T.unlines
    [ "command { \"go\" } on #golang:1.22",
      "command { \"npm\" } on #node:20",
      "command { \"git\" } on #alpine/git:2.45.2",
      "",
      "test_api(): String = $ go test ./...",
      "",
      "test_web(): String = $ npm test",
      "",
      "both(): String = do {",
      "  api = async test_api()",
      "  web = async test_web()",
      "  a = await api",
      "  w = await web",
      "  \"#{a} #{w}\"",
      "}",
      "",
      "fact(n: Number): Number = if (n == 0) { 1 } else { n * fact(n - 1) }",
      "",
      "changed(): Array<String> = do {",
      "  out = $ git diff --name-only origin/main",
      "  lines(out)",
      "}",
      "",
      "test_each(): Array<String> = for (pkg : changed()) {",
      "  $ go test -count=1 #{pkg}",
      "}",
      "",
      "release(target: String, --dry_run: Bool = true): String = do {",
      "  tag = $ git describe --tags --abbrev=0",
      "  if (dry_run) {",
      "    \"would publish #{tag} to #{target}\"",
      "  } else {",
      "    $ go run ./cmd/publish --tag #{tag} --target #{target}",
      "    \"published #{tag}\"",
      "  }",
      "}",
      "",
      "ship(): String = release(\"prod\", dry_run = false)",
      "",
      "deploy(): String = $ go run ./cmd/deploy --context #{get_env_or(\"KUBE_CONTEXT\", \"staging\")}",
      "",
      "healthy(n: Number): String = do {",
      "  r = $* go version",
      "  ok = r.code == 0 && !(r.stdout == \"\")",
      "  \"#{n}: #{ok}\"",
      "}",
      "",
      "clean(): Void = do {",
      "  $ go",
      "  do {}",
      "}"
    ]

go, npm, git :: Command
go = command "go" (image "golang:1.22")
npm = command "npm" (image "node:20")
git = command "git" (image "alpine/git:2.45.2")

testApi :: Task '[] 'TString
testApi = task "test_api" $ run go "test ./..."

testWeb :: Task '[] 'TString
testWeb = task "test_web" $ run npm "test"

both :: Task '[] 'TString
both = task "both" $ parallel $ (\a w -> a <> " " <> w) <$> par (call testApi) <*> par (call testWeb)

fact :: Task '["n" ::: 'TNumber] 'TNumber
fact = task "fact" $ \n -> if_ (n ==. 0) 1 (n * call fact (n - 1))

changed :: Task '[] ('TArray 'TString)
changed = task "changed" $ L.do
  out <- run git "diff --name-only origin/main"
  lines_ out

testEach :: Task '[] ('TArray 'TString)
testEach = task "test_each" $ mapE (call changed) $ \pkg -> run go ("test -count=1 " <> pkg)

release :: Task '["target" ::: 'TString] 'TString
release = taskWith "release" $ body <$> kw "dry_run" true
  where
    body dryRun target = L.do
      tag <- run git "describe --tags --abbrev=0"
      if_
        dryRun
        ("would publish " <> tag <> " to " <> target)
        ( L.do
            run go ("run ./cmd/publish --tag " <> tag <> " --target " <> target)
            "published " <> tag
        )

ship :: Task '[] 'TString
ship = task "ship" $ callWith release ["dry_run" .= false] "prod"

deploy :: Task '[] 'TString
deploy = task "deploy" $ run go ("run ./cmd/deploy --context " <> getEnvOr "KUBE_CONTEXT" "staging")

healthy :: Task '["n" ::: 'TNumber] 'TString
healthy = task "healthy" $ \n -> L.do
  r <- runAll go "version"
  ok <- field @"code" r ==. 0 &&. not_ (field @"stdout" r ==. "")
  str n <> ": " <> str ok

clean :: Task '[] 'TVoid
clean = task "clean" $ L.do
  run go ""
  done

embedded :: Program
embedded = case assemble [export both, export testEach, export ship, export deploy, export healthy, export clean, internal fact] of
  Right p -> p
  Left errs -> error (T.unpack (T.unlines errs))

elaborated :: IO CoreProgram
elaborated = do
  r <- loadProgramWith (\p -> pure (if p == "main.lask" then Right source else Left "not found")) "main.lask"
  case r >>= \prog -> validateProgram prog >>= elaborateProgram prog of
    Right cp -> pure cp
    Left ds -> fail ("main.lask does not elaborate: " <> show ds)

-- Comparison up to spans and bound names ------------------------------------------

-- | Spans and module paths erased, anonymous lambdas unnamed, and
-- locals renamed in binding order. Parameter names are kept: they are
-- part of a declaration's interface.
normalise :: Core -> Core
normalise c0 = evalState (norm Map.empty c0) (0 :: Int)
  where
    norm env (Core _ f) = Core NoSpan <$> case f of
      CVar (LocalRef n) -> pure (CVar (LocalRef (Map.findWithDefault n n env)))
      CVar (TopRef _ n) -> pure (CVar (TopRef "" n))
      CLam lam -> do
        let named = not ("<" `T.isPrefixOf` lamName lam)
        ps <- if named then pure (lamPositional lam) else mapM (const freshName) (lamPositional lam)
        let env' = Map.union (Map.fromList (zip (lamPositional lam) ps)) env
        kws <- mapM (\(k, d) -> (,) k <$> norm env' d) (lamKeywords lam)
        b <- norm env' (lamBody lam)
        pure
          ( CLam
              lam
                { lamName = if named then lamName lam else "<lambda>",
                  lamModule = "",
                  lamPositional = ps,
                  lamKeywords = kws,
                  lamBody = b
                }
          )
      CDo ss -> CDo <$> stmts env ss
      CEnv k args -> CEnv k <$> mapM (\(n, a) -> (,) n <$> norm env a) args
      _ -> rebuild env f
    stmts _ [] = pure []
    stmts env (CSExpr e : rest) = (:) <$> (CSExpr <$> norm env e) <*> stmts env rest
    stmts env (CSBind n e : rest) = do
      e' <- norm env e
      n' <- freshName
      (CSBind n' e' :) <$> stmts (Map.insert n n' env) rest
    freshName :: State Int Text
    freshName = do
      i <- gets id
      modify (+ 1)
      pure ("v" <> T.pack (show i))
    -- Every other node: normalise the children in place.
    rebuild env f = case f of
      CStr ps -> CStr <$> mapM (part env) ps
      CArray es -> CArray <$> mapM (norm env) es
      CMapLit kvs -> CMapLit <$> mapM (\(k, v) -> (,) k <$> norm env v) kvs
      CRecordLit kvs -> CRecordLit <$> mapM (\(k, v) -> (,) k <$> norm env v) kvs
      CApp fn pos kws -> CApp <$> norm env fn <*> mapM (norm env) pos <*> mapM (\(k, v) -> (,) k <$> norm env v) kws
      CDot e n -> (`CDot` n) <$> norm env e
      CIndex k a b -> CIndex k <$> norm env a <*> norm env b
      CIf a b e -> CIf <$> norm env a <*> norm env b <*> norm env e
      CAnd a b -> CAnd <$> norm env a <*> norm env b
      COr a b -> COr <$> norm env a <*> norm env b
      CNot a -> CNot <$> norm env a
      CBin op a b -> CBin op <$> norm env a <*> norm env b
      CAwait a -> CAwait <$> norm env a
      CRunnable e args -> CRunnable <$> norm env e <*> mapM (\(n, a) -> (,) n <$> norm env a) args
      CCast a t -> (`CCast` t) <$> norm env a
      CIsType a t -> (`CIsType` t) <$> norm env a
      other -> pure other
    part env (CPExpr e) = CPExpr <$> norm env e
    part _ p = pure p

declsByName :: CoreProgram -> Map Text CoreDecl
declsByName cp = Map.fromList [(n, cd) | ((_, n), cd) <- Map.toList (cpDecls cp)]

-- A scripted runner ---------------------------------------------------------------

-- | Records every command with the image it ran in, and answers the few
-- the programs here read.
scripted :: IO (RtHooks, IO [(Text, Text)])
scripted = do
  ran <- newIORef []
  let runner env cmd = do
        modifyIORef' ran ((imageOf env, cmd) :)
        pure (Right (0, reply cmd, ""))
      hooks =
        RtHooks
          { hookRunCommand = runner,
            hookRunFile = \_ _ -> pure (Left (runtimeFailure ERuntimeValue "no files here")),
            hookLog = noLogSink,
            hookAsync = noAsyncTracker,
            hookReadEnv = \_ -> pure Nothing
          }
  pure (hooks, reverse <$> readIORef ran)
  where
    imageOf e = case Map.lookup "image" (envParams e) of
      Just (VString i) -> i
      _ -> envKind e
    reply cmd
      | "git describe" `T.isPrefixOf` cmd = "v1.4.0"
      | "git diff" `T.isPrefixOf` cmd = "a\nb"
      | otherwise = "ok"

runScripted :: Program -> Text -> [Value] -> [(Text, Value)] -> IO (Either LaskFailure Value, [(Text, Text)])
runScripted prog name args kws = do
  (hooks, ran) <- scripted
  r <- try (runTask prog hooks name args kws)
  (,) r <$> ran

-- Sharing -------------------------------------------------------------------------

versionBind :: Task '[] 'TString
versionBind = task "version_bind" $ L.do
  v <- run go "version"
  v <> v

versionLet :: Task '[] 'TString
versionLet = task "version_let" $
  let v = run go "version" in v <> v

errorsOf :: [Export] -> [Text]
errorsOf = either id (const []) . assemble

spec :: Spec
spec = do
  describe "reification" $ do
    it "produces the Core the elaborator produces for the same program, up to spans and bound names" $ do
      cp <- elaborated
      let fromSource = declsByName cp
          fromEmbedding = declsByName (progCore embedded)
      Map.keysSet fromEmbedding `shouldBe` Map.keysSet fromSource
      forM_ (Map.toList fromEmbedding) $ \(name, e) -> case Map.lookup name fromSource of
        Nothing -> expectationFailure ("not in the source: " <> T.unpack name)
        Just s -> do
          let shown cd = T.unpack (renderDecl cd)
          (name, cdType e, cdParams e) `shouldBe` (name, cdType s, cdParams s)
          if normalise (cdCore e) == normalise (cdCore s)
            then pure ()
            else expectationFailure ("embedded:\n" <> shown e <> "\n\nelaborated:\n" <> shown s)

    it "records the command words the program uses, with their environments" $ do
      cp <- elaborated
      let words' p = Map.map normalise (Map.findWithDefault Map.empty (cpEntry p) (cpCommands p))
      words' (progCore embedded) `shouldBe` words' cp

  describe "static analysis" $ do
    it "sees every environment a task can reach, including those of commands chosen by run-time output" $ do
      let envs name = sort (Set.toList (Set.fromList (map refLabel (collectEnvRefsFrom (progCore embedded) (programModule, name)))))
      envs "test_each" `shouldBe` ["alpine/git:2.45.2", "golang:1.22"]
      envs "ship" `shouldBe` ["alpine/git:2.45.2", "golang:1.22"]
      envs "both" `shouldBe` ["golang:1.22", "node:20"]

    it "sees the environment variables a task reads" $
      collectEnvReadsFrom (progCore embedded) (programModule, "deploy") `shouldBe` Just (Set.fromList ["KUBE_CONTEXT"])

  describe "evaluation" $ do
    it "recurses through a name on a run-time value" $ do
      (r, _) <- runScripted embedded "fact" [VNumber 5] []
      either (Left . lfError) Right r `shouldBe` Right (VNumber 120)

    it "runs a command for each line an earlier command printed" $ do
      (_, ran) <- runScripted embedded "test_each" [] []
      ran
        `shouldBe` [ ("alpine/git:2.45.2", "git diff --name-only origin/main"),
                     ("golang:1.22", "go test -count=1 a"),
                     ("golang:1.22", "go test -count=1 b")
                   ]

    it "takes keyword defaults, and keyword arguments at a call" $ do
      (dry, _) <- runScripted embedded "release" [VString "prod"] []
      either (Left . lfError) Right dry `shouldBe` Right (VString "would publish v1.4.0 to prod")
      (_, ran) <- runScripted embedded "ship" [] []
      map snd ran `shouldBe` ["git describe --tags --abbrev=0", "go run ./cmd/publish --tag v1.4.0 --target prod"]

    it "binds the argument of signum once" $ do
      let probing = task "probing" $ signum (L.do { _ <- run go "version"; 0 - 3 }) :: Task '[] 'TNumber
      p <- either (fail . show) pure (assemble [export probing])
      (r, ran) <- runScripted p "probing" [] []
      either (Left . lfError) Right r `shouldBe` Right (VNumber (-1))
      length ran `shouldBe` 1

    it "runs a bare command word, as $ go does" $ do
      (_, ran) <- runScripted embedded "clean" [] []
      ran `shouldBe` [("golang:1.22", "go")]

    it "shares a result bound with <-, and copies a term bound with let" $ do
      sharing <- either (fail . show) pure (assemble [export versionBind, export versionLet])
      (_, bound) <- runScripted sharing "version_bind" [] []
      (_, copied) <- runScripted sharing "version_let" [] []
      length bound `shouldBe` 1
      length copied `shouldBe` 2

  describe "assemble" $ do
    it "rejects two declarations under one name" $
      errorsOf [export fact, export (task "fact" (\n -> n + 1) :: Task '["n" ::: 'TNumber] 'TNumber)]
        `shouldBe` ["two different declarations are named 'fact'"]

    it "rejects a keyword argument the callee does not declare" $ do
      let bad = task "bad" $ callWith release ["dryrun" .= false] "prod" :: Task '[] 'TString
      errorsOf [export bad]
        `shouldBe` ["'bad' calls 'release' with --dryrun, which 'release' does not declare"]

    it "rejects a keyword argument of the wrong type, or given twice" $ do
      let wrongType = task "wrong_type" $ callWith release ["dry_run" .= trim "yes"] "prod" :: Task '[] 'TString
          twice = task "twice" $ callWith release ["dry_run" .= false, "dry_run" .= true] "prod" :: Task '[] 'TString
      errorsOf [export wrongType, export twice]
        `shouldBe` [ "'wrong_type' calls 'release' with --dry_run of type String, but it is declared Bool",
                     "'twice' calls 'release' with --dry_run more than once"
                   ]

    it "rejects one command word bound to two environments" $ do
      let go23 = command "go" (image "golang:1.23")
          other = task "other" $ run go23 "version" :: Task '[] 'TString
      errorsOf [export testApi, export other]
        `shouldBe` ["the command 'go' is bound to more than one environment: #golang:1.22, #golang:1.23"]

    it "makes everything not exported internal" $
      cpInternal (progCore embedded) `shouldBe` Set.fromList ["changed", "fact", "release", "test_api", "test_web"]

    it "rejects names Lask cannot write" $ do
      let bad = task "Deploy" "x" :: Task '[] 'TString
          reserved = task "for" "x" :: Task '[] 'TString
          param = task "p" id :: Task '["Bad" ::: 'TString] 'TString
      errorsOf [export bad, export reserved, export param]
        `shouldBe` [ "'Deploy' is not a valid declaration name",
                     "'for' is not a valid declaration name",
                     "'p': 'Bad' is not a valid parameter name"
                   ]

    it "follows recursion without looping, and keeps one declaration per name" $
      map (snd . declKey) (progDecls embedded)
        `shouldBe` ["both", "test_api", "test_web", "test_each", "changed", "ship", "release", "deploy", "healthy", "clean", "fact"]
