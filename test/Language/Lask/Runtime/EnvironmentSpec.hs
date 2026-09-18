{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EnvironmentSpec (spec) where

import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (FileOp (..))
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Secrets (registerSecret, resetSecretRegistryForTests)
import Language.Lask.Runtime.Value
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

env :: Text -> [(Text, Value)] -> EnvValue
env k ps = EnvValue k (Map.fromList ps)

-- | A successful operation returning Void. It cannot be compared with
-- '==': Void is not a comparable type (spec 6.2), so 'Eq' 'Value'
-- reports even two Voids unequal.
returnsVoid :: Either LaskFailure Value -> Expectation
returnsVoid r = case r of
  Right VVoid -> pure ()
  other -> expectationFailure ("expected Void, got " <> show other)

failsWithFs :: Either LaskFailure Value -> Expectation
failsWithFs r = case r of
  Left lf -> lfCode lf `shouldBe` Just EIoFs
  Right v -> expectationFailure ("expected E-IO-FS, got " <> show v)

spec :: Spec
spec = do
  describe "environment resolution (spec 10.4)" $ do
    it "resolves local" $
      resolveEnv (env "local" []) `shouldBe` Right ResolvedLocal
    it "resolves a registry reference" $
      resolveEnv (env "docker" [("image", VString "alpine:3.20")])
        `shouldBe` Right (ResolvedDocker "alpine:3.20" Map.empty)
    it "resolves a recipe, defaulting the context to the Dockerfile's directory" $
      resolveEnv (env "docker" [("dockerfile", VString "infra/Dockerfile")])
        `shouldBe` Right (ResolvedRecipe "infra/Dockerfile" "infra" Map.empty)
    it "keeps an explicit context" $
      resolveEnv (env "docker" [("dockerfile", VString "infra/Dockerfile"), ("context", VString ".")])
        `shouldBe` Right (ResolvedRecipe "infra/Dockerfile" "." Map.empty)
    it "fails on docker without an image or a recipe" $
      case resolveEnv (env "docker" []) of
        Left lf -> lfCode lf `shouldBe` Just EIoEnvResolve
        Right r -> expectationFailure (show r)
    it "fails on unknown kinds" $
      resolveEnv (env "remote" [("host", VString "h")]) `shouldSatisfy` isLeft

  describe "launch argument construction (spec 10.5)" $ do
    it "builds docker run arguments with mounted workdir" $
      dockerArgs "/proj" "alpine:3.20" (Map.fromList [("memory", VString "4g")]) "uname -a"
        `shouldBe` [ "run", "--rm",
                     "-v", "/proj:/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "--memory", "4g",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

    it "attaches stdin for a container write, so no content rides on the command line" $
      dockerShellArgs "/proj" "alpine:3.20" Map.empty True "cat > 'out.txt'"
        `shouldBe` [ "run", "--rm",
                     "-i",
                     "-v", "/proj:/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "alpine:3.20",
                     "-c", "cat > 'out.txt'"
                   ]

  describe "local execution (spec 8.7, real process)" $ do
    it "runs a local command and captures streams and exit code" $ do
      runner <- mkCommandRunner "/tmp" noCommandLog
      r <- runner (env "local" []) "echo out; echo err 1>&2; exit 3"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 3
          out `shouldBe` "out\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
    it "uses the base directory as the working directory" $ do
      runner <- mkCommandRunner "/tmp" noCommandLog
      r <- runner (env "local" []) "pwd"
      case r of
        Right (0, out, _) -> out `shouldSatisfy` (\o -> o == "/tmp\n" || o == "/private/tmp\n")
        other -> expectationFailure (show other)

  describe "command execution log relay (spec 12.3)" $ do
    let mkLoggedRunner = do
          logRef <- newIORef []
          -- The runner relays stdout and stderr from two concurrent
          -- threads, so a sink must be atomic: a plain read-modify-write
          -- here loses entries.
          let sink cl = atomicModifyIORef' logRef (\ls -> (ls <> [cl], ()))
          runner <- mkCommandRunner "/tmp" sink
          pure (runner, readIORef logRef)
        runWithLog cmd = do
          (runner, readLog) <- mkLoggedRunner
          r <- runner (env "local" []) cmd
          entries <- readLog
          pure (r, entries)
        kinds = map clKind

    it "relays lines in order between start and exit" $ do
      (r, entries) <- runWithLog "echo one; echo two; echo err 1>&2; exit 5"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 5
          out `shouldBe` "one\ntwo\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
      listToMaybe (kinds entries) `shouldBe` Just ClStart
      last (kinds entries) `shouldBe` ClExit 5
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["one", "two"]
      [l | ClLine 2 l <- kinds entries] `shouldBe` ["err"]

    it "always emits start and exit even without output" $ do
      (_, entries) <- runWithLog "true"
      kinds entries `shouldBe` [ClStart, ClExit 0]

    it "keeps CommandResult faithful for unterminated output" $ do
      (r, entries) <- runWithLog "printf 'no-newline'"
      case r of
        Right (0, out, _) -> out `shouldBe` "no-newline"
        other -> expectationFailure (show other)
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["no-newline"]

    it "stamps log entries with the environment summary and command" $ do
      (_, entries) <- runWithLog "true"
      map clEnvSummary entries `shouldSatisfy` all (== "#local")
      map clCommand entries `shouldSatisfy` all (== "true")

    it "assigns unique 1-based execution numbers per command" $ do
      (runner, readLog) <- mkLoggedRunner
      _ <- runner (env "local" []) "echo a"
      _ <- runner (env "local" []) "echo b"
      entries <- readLog
      let execsOf cmd = [clExec cl | cl <- entries, clCommand cl == cmd]
      execsOf "echo a" `shouldSatisfy` all (== 1)
      execsOf "echo b" `shouldSatisfy` all (== 2)

    it "masks a registered secret out of the log without touching the captured result (spec 12.8)" $ do
      resetSecretRegistryForTests
      registerSecret "sup3rsecret"
      (r, entries) <- runWithLog "echo sup3rsecret; echo sup3rsecret 1>&2"
      resetSecretRegistryForTests
      case r of
        Right (0, out, errOut) -> do
          -- The value returned to the running program is the real one.
          out `shouldBe` "sup3rsecret\n"
          errOut `shouldBe` "sup3rsecret\n"
        other -> expectationFailure (show other)
      -- The observed log copy is masked, on both the relayed lines...
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["***"]
      [l | ClLine 2 l <- kinds entries] `shouldBe` ["***"]
      -- ...and the command string on the start line.
      map clCommand entries `shouldSatisfy` (not . any (T.isInfixOf "sup3rsecret"))

  describe "environment log info (spec 12.3)" $ do
    it "summarizes environments in environment-expression notation" $ do
      fst (envLogInfo (env "local" []) ResolvedLocal) `shouldBe` "#local"
      fst (envLogInfo (env "docker" [("image", VString "alpine:3.20")]) (ResolvedDocker "alpine:3.20" Map.empty))
        `shouldBe` "#alpine:3.20"
    it "summarizes a recipe by its Dockerfile" $
      fst
        ( envLogInfo
            (env "docker" [("dockerfile", VString "infra/Dockerfile")])
            (ResolvedRecipe "infra/Dockerfile" "infra" Map.empty)
        )
        `shouldBe` "#docker(dockerfile = \"infra/Dockerfile\")"

  describe "filesystem functions (spec 15.11, real filesystem)" $ do
    let withProject act = withSystemTempDirectory "lask-fs" $ \dir -> do
          runner <- mkFileRunner dir
          act dir (\op -> runner (env "local" []) op)

    it "writes a file and reads it back" $ withProject $ \_ run -> do
      run (FileWrite "note.txt" "hello\n") >>= returnsVoid
      run (FileRead "note.txt") `shouldReturn` Right (VString "hello\n")

    it "resolves a relative path against the base directory" $ withProject $ \dir run -> do
      _ <- run (FileWrite "note.txt" "x")
      doesFileExist (dir <> "/note.txt") `shouldReturn` True

    it "reports a read of a missing file as E-IO-FS" $ withProject $ \_ run -> do
      run (FileRead "absent.txt") >>= failsWithFs

    it "answers file_exists without failing on a missing path" $ withProject $ \_ run -> do
      run (FileExists "absent.txt") `shouldReturn` Right (VBool False)
      _ <- run (FileWrite "there.txt" "x")
      run (FileExists "there.txt") `shouldReturn` Right (VBool True)

    it "creates parents with make_dir and refuses them in write_file" $ withProject $ \_ run -> do
      run (FileWrite "sub/deep/note.txt" "x") >>= failsWithFs
      run (FileMakeDir "sub/deep") >>= returnsVoid
      run (FileWrite "sub/deep/note.txt" "x") >>= returnsVoid

    it "removes a file, and removing what is absent succeeds" $ withProject $ \_ run -> do
      _ <- run (FileWrite "gone.txt" "x")
      run (FileRemove "gone.txt") >>= returnsVoid
      run (FileRemove "gone.txt") >>= returnsVoid
      run (FileExists "gone.txt") `shouldReturn` Right (VBool False)

    it "refuses to remove a directory" $ withProject $ \_ run -> do
      _ <- run (FileMakeDir "adir")
      run (FileRemove "adir") >>= failsWithFs

    it "lists a directory in a deterministic order" $ withProject $ \_ run -> do
      _ <- run (FileMakeDir "d")
      mapM_ (\n -> run (FileWrite ("d/" <> n) "x")) ["c.txt", "a.txt", "b.txt"]
      run (FileListDir "d")
        `shouldReturn` Right (VArray (V.fromList (map VString ["a.txt", "b.txt", "c.txt"])))

    it "reports list_dir on a non-directory as E-IO-FS" $ withProject $ \_ run -> do
      _ <- run (FileWrite "afile" "x")
      run (FileListDir "afile") >>= failsWithFs

    it "globs across components and hides dot entries" $ withProject $ \dir run -> do
      createDirectoryIfMissing True (dir <> "/src/lib")
      createDirectoryIfMissing True (dir <> "/.git")
      mapM_ (\n -> run (FileWrite n "x"))
        ["src/main.lask", "src/lib/util.lask", "src/notes.md", ".git/config.lask"]
      run (FileGlob "src/**/*.lask")
        `shouldReturn` Right (VArray (V.fromList (map VString ["src/lib/util.lask", "src/main.lask"])))
      run (FileGlob "**/*.lask")
        `shouldReturn` Right (VArray (V.fromList (map VString ["src/lib/util.lask", "src/main.lask"])))

    it "returns no match as an empty array rather than a failure" $ withProject $ \_ run -> do
      run (FileGlob "nowhere/*.lask") `shouldReturn` Right (VArray V.empty)
      run (FileGlob "*.absent") `shouldReturn` Right (VArray V.empty)
