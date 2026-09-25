{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry points: subcommand implementations, stdin capture,
-- diagnostics output and exit-code mapping (spec 9, 11, 14.8).
module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.ArgCodec
import Command.Lask.Complete (completionScript)
import Command.Lask.Envs (EnvRef (..), collectEnvRefs, collectEnvRefsFrom, collectRecipes)
import Language.Lask.Core.AST (Core (..))
import Command.Lask.Help
import Command.Lask.Options
import Control.Exception (try)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (nub, sort)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Language.LSP.Lask as LSP
import Language.Lask (Compiled (..), Partial (..), compileFile, compileFilePartial)
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import Language.Lask.Module.Resolve (entryPublicValues)
import Language.Lask.Deps.Cache (cacheDirFor)
import Language.Lask.Deps.Fetch (DepSource (..), fetchAndStore, resolveGitRev, syncAll)
import Language.Lask.Deps.File
import Language.Lask.Deps.Lock
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Doc (DocComment, docBlockAbove, emptyDoc, parseDoc)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.ErrorCode
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Obs.CommandLog
import Language.Lask.Obs.Events (TraceId, encodeEvent, newTraceId, noSink)
import Language.Lask.Repl (runRepl)
import Language.Lask.Runtime.Environment
import Command.Lask.Images (ImageRow (..), Materialized (..), imageRows, loadPins, materialize)
import Language.Lask.Runtime.Image (ImagePins, imageExists, recipeTag, resolveRegistry)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Obs.ExecLog (jsonLogSink, textLogSink)
import Language.Lask.Runtime.Eval (RtCtx (..), applyValue, evalCore, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, failureMessage, renderValueText)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (Type (..), applySubst)
import Language.Lask.Utils (Pretty (pretty), kebabToSnake)
import Paths_lask (version)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin)
import System.Process (proc)

runRootCommand :: RootCommand -> IO ()
runRootCommand cmd = case cmd of
  CmdServe -> LSP.serve >> pure ()
  CmdCheck opts -> cmdCheck opts
  CmdRun runOpts -> cmdRunEval False runOpts
  CmdEval runOpts -> cmdRunEval True runOpts
  CmdRepl opts -> cmdRepl opts
  CmdEnvs envsOpts -> cmdEnvs envsOpts
  CmdDepsSync opts frozen -> cmdDepsSync opts frozen
  CmdDepsAdd opts name source -> cmdDepsAdd opts name source
  CmdDepsWhy opts name -> cmdDepsWhy opts name
  CmdDepsDiff opts name -> cmdDepsDiff opts name
  CmdEnvBuild opts -> cmdEnvBuild opts
  CmdEnvList opts -> cmdEnvList opts
  CmdCmd cmdOpts -> cmdCmd cmdOpts
  CmdCompletion sh -> TIO.putStr (completionScript sh)
  CmdVersion -> cmdVersion

-- version ---------------------------------------------------------------------

cmdVersion :: IO ()
cmdVersion = putStrLn ("lask " ++ showVersion version)

-- check ---------------------------------------------------------------------

cmdCheck :: CommonOpts -> IO ()
cmdCheck opts = do
  r <- compileFile (optModule opts)
  case r of
    Left ds -> do
      TIO.putStrLn (renderDiags (optJsonFormat opts) ds)
      exitWith (ExitFailure 1)
    Right _ -> do
      if optJsonFormat opts
        then TIO.putStrLn "[]"
        else putStrLn "the module is valid"
      exitSuccess

-- run / eval -----------------------------------------------------------------

cmdRunEval :: Bool -> RunOpts -> IO ()
cmdRunEval printResult runOpts
  -- Help never evaluates anything, so it is decided before the module
  -- is compiled for execution (spec 11.6).
  | runHelp runOpts || helpAfterFunction (runArgs runOpts) =
      cmdHelp (if printResult then "eval" else "run") runOpts
cmdRunEval printResult runOpts = do
  let opts = runCommon runOpts
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      baseDir = cpBaseDir core

  rawName <- case runFunction runOpts of
    Just n -> pure n
    Nothing -> usageError opts "no function specified (use --help to list the module's functions)"
  let fnName = kebabToSnake rawName
  -- The public symbols of the module, re-exported ones included; a
  -- declaration marked `internal` is not part of any surface (spec 5),
  -- so it is not callable from the CLI either.
  (key, cd) <- case publicDecl compiled fnName of
    Just found -> pure found
    Nothing -> usageError opts ("no such function: '" <> rawName <> "'")

  cliArgs <- case parseCliArgs (dropArgSeparator (runArgs runOpts)) of
    Right as -> pure as
    Left e -> usageError opts e

  let orUsageError = either (usageError opts) pure
      instantiateAtAny vs ps
        | null vs = ps
        | otherwise =
            let at = applySubst (Map.fromList [(v, TyAny) | v <- vs])
             in StaticParams
                  [(n, at t) | (n, t) <- spPositional ps]
                  (fmap (fmap at) (spVariadic ps))
                  [(n, at t) | (n, t) <- spKeywords ps]
  (posVals, kwVals) <- case cdParams cd of
    -- A declaration with type parameters is invoked with every one of
    -- them at Any (spec 11.2): the CLI has no type to instantiate them
    -- from, and the body cannot misuse what it is handed, a type
    -- parameter being opaque inside it (4.4).
    Just params ->
      orUsageError
        (bindCliArgs (instantiateAtAny (cdTypeVars cd) params) (runArgDecode runOpts) cliArgs)
    Nothing -> case cdType cd of
      TyFun paramTys _ ->
        -- A function-typed value declaration: positional only
        -- (spec 11.2, example 16.3).
        orUsageError $
          bindCliArgs
            (StaticParams (zip (map (const "arg") paramTys) paramTys) Nothing [])
            (runArgDecode runOpts)
            cliArgs
      _ -> usageError opts ("'" <> fnName <> "' is not a callable function")

  stdinText <- readStdinOrExit opts
  traceId <- maybe newTraceId pure (optTraceId opts)
  -- One serialized stderr line writer shared by command logs and
  -- execution events, so concurrent emitters never interleave.
  writeErr <- newLineWriter stderr
  let -- Command execution logs relay to stderr in real time
      -- (spec 12.3); JSON Lines under --format json (12.2).
      cmdLogSink
        | optJsonFormat opts = jsonCommandLog traceId writeErr
        | otherwise = textCommandLog writeErr
  -- Images resolve through the lock, and are never pulled or built
  -- here (spec 10.3, 10.4).
  pins <- loadPins core
  runner <- mkCommandRunner pins baseDir cmdLogSink
  fileRunner <- mkFileRunner pins baseDir
  let -- `log` (spec 15.12) writes execution log lines to stderr,
      -- through the same serialized writer as the command logs.
      logSink
        | optJsonFormat opts = jsonLogSink traceId writeErr
        | otherwise = textLogSink writeErr
  ctx0 <- mkRtCtx core stdinText (RtHooks runner fileRunner logSink)
  let sink
        | optJsonFormat opts = writeErr . encodeEvent
        | otherwise = noSink
      ctx = ctx0 {rtTraceId = traceId, rtEmit = sink}
  result <- try $ do
    fv <- topValue ctx key
    case fv of
      VClosure _ -> applyValue ctx fv posVals kwVals
      VBuiltin _ -> applyValue ctx fv posVals kwVals
      v
        | null posVals && null kwVals -> pure v
        | otherwise -> applyValue ctx fv posVals kwVals
  case result of
    Left lf -> failureExit opts traceId lf
    Right v -> do
      when printResult $ case v of
        VVoid -> pure ()
        _ -> TIO.putStrLn (encodeResult (runStdoutEncode runOpts) v)
      exitSuccess

-- help (spec 11.6) -------------------------------------------------------------

-- | @--help@ after the function name is the one exception to the
-- argument boundary rule (spec 11.2). Only a standalone token counts,
-- and the scan stops at @--@ so that a literal @--help@ can still be
-- passed to the function.
helpAfterFunction :: [Text] -> Bool
helpAfterFunction = elem "--help" . takeWhile (/= argSeparator)

-- | Remove the @--@ marker; everything after it is a plain argument
-- (spec 11.2).
dropArgSeparator :: [Text] -> [Text]
dropArgSeparator args = case break (== argSeparator) args of
  (before, _ : after) -> before <> after
  _ -> args

cmdHelp :: Text -> RunOpts -> IO ()
cmdHelp subcommand runOpts = do
  let opts = runCommon runOpts
  (diags, partial) <- compileFilePartial (optModule opts)
  -- Static errors do not hide the help; a module that does not
  -- compile is exactly when its usage is wanted (spec 11.6).
  unless (null diags) $
    TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) diags)
  src <- either (const "") id <$> try' (TIO.readFile (optModule opts))
  let core = partialCore partial
      path = optModule opts
      entry = maybe path cpEntry core
      comments = either (const []) snd (lexTokensWithComments path src)
      ownDecls =
        [ (n, d)
        | m <- maybe [] pure (partialModule partial),
          d <- AST.moduleDecls m,
          Just n <- [declaredName d],
          not (n `Set.member` maybe Set.empty cpInternal core)
        ]
  -- The public functions of the module, re-exported ones included
  -- (spec 11.6), each described from the file that declares it: that
  -- is where its documentation comment and its parameters are written.
  -- Without a loaded program only the module's own are known.
  declsByName <- case partialProgram partial of
    Nothing -> pure [(n, HelpSource path src comments d (entry, n)) | (n, d) <- ownDecls]
    Just prog ->
      fmap catMaybes . forM (entryPublicValues prog (partialScopes partial)) $ \(n, key@(defPath, defName)) ->
        if defPath == progEntry prog
          then pure ((\d -> (n, HelpSource path src comments d key)) <$> lookup defName ownDecls)
          else do
            defSrc <- either (const "") id <$> try' (TIO.readFile defPath)
            let defComments = either (const []) snd (lexTokensWithComments defPath defSrc)
                defDecl = do
                  lm <- Map.lookup defPath (progModules prog)
                  listToMaybe [d | d <- AST.moduleDecls (lmModule lm), declaredName d == Just defName]
            pure ((\d -> (n, HelpSource defPath defSrc defComments d key)) <$> defDecl)
  let coreOf key = core >>= Map.lookup key . cpDecls
      -- The name is the one the module publishes, which a renaming
      -- re-export makes different from the declaration's.
      helpOf envs (n, HelpSource hp hsrc hcomments d key) =
        (buildFunctionHelp hp hsrc d (coreOf key) (docFor hsrc hcomments d) envs) {fhName = n}

  case runFunction runOpts of
    -- The option help is always available, whatever state the module
    -- is in; only the function list depends on it (spec 11.6).
    Nothing -> do
      putStrLn (runOptionsHelp (T.unpack subcommand))
      let listing = map (helpOf []) declsByName
      case (optJsonFormat opts, listing) of
        (True, _) -> TIO.putStrLn (encodeJsonText (renderListJson path listing))
        (False, []) -> pure ()
        (False, _) -> do
          let rendered = renderListText path listing
          unless (T.null rendered) $ TIO.putStrLn ("\n" <> rendered)
      exitSuccess
    Just rawName -> do
      -- Without a parse there are no declarations to describe.
      when (isNothing (partialModule partial)) $ exitWith (ExitFailure 1)
      let fnName = kebabToSnake rawName
      found <- case lookup fnName declsByName of
        Just h -> pure h
        Nothing -> usageError opts (noSuchFunction rawName (map fst declsByName))
      let envs = case core of
            Just c -> nub (sort (collectEnvRefsFrom c (hsKey found)))
            Nothing -> []
          fh = helpOf envs (fnName, found)
      if optJsonFormat opts
        then TIO.putStrLn (encodeJsonText (renderHelpJson fh))
        else TIO.putStr (renderHelpText subcommand fh)
      exitSuccess
  where
    try' :: IO Text -> IO (Either IOError Text)
    try' = try

    -- Help is read-only: an unreadable or malformed environment file
    -- costs the environment targets, not the help.

-- | Where the help of one public function is read from: the file
-- that declares it, its text and comments, the declaration, and its
-- key in the core program.
data HelpSource = HelpSource
  { _hsPath :: FilePath,
    _hsSrc :: Text,
    _hsComments :: [Span],
    _hsDecl :: AST.Decl,
    hsKey :: (FilePath, Text)
  }

-- | The declaration a CLI name of the entry module invokes (spec 11.2),
-- with its key: a public symbol the module declares or re-exports,
-- followed to where it is declared.
publicDecl :: Compiled -> Text -> Maybe ((FilePath, Text), CoreDecl)
publicDecl compiled n = do
  key <- lookup n (entryPublicValues (compiledProgram compiled) (compiledScopes compiled))
  cd <- Map.lookup key (cpDecls (compiledCore compiled))
  pure (key, cd)

declaredName :: AST.Decl -> Maybe Text
declaredName d = case AST.declF d of
  AST.DValue n _ _ _ -> Just n
  AST.DFunction n _ _ _ _ -> Just n
  _ -> Nothing

-- | The documentation comment directly above a declaration (spec 3.1).
docFor :: Text -> [Span] -> AST.Decl -> DocComment
docFor src comments decl = case AST.declSpan decl of
  Span (Position _ l _) _ -> maybe emptyDoc parseDoc (docBlockAbove src comments l)
  NoSpan -> emptyDoc

noSuchFunction :: Text -> [Text] -> Text
noSuchFunction wanted names =
  "no such function: '"
    <> wanted
    <> "'"
    <> case near of
      [] -> ""
      (n : _) -> "; did you mean '" <> n <> "'?"
  where
    target = kebabToSnake wanted
    near = [n | n <- names, T.isPrefixOf (T.take 2 target) n || T.isInfixOf target n]

encodeJsonText :: A.Value -> Text
encodeJsonText = TE.decodeUtf8 . BL.toStrict . A.encode

encodeResult :: StdoutEncode -> Value -> Text
encodeResult enc v = case enc of
  EncodeJson -> encodeValue v
  EncodePrettyJson -> encodeValuePretty v
  EncodeText -> renderValueText v

-- | Uncaught failure: report to stderr (with the collected stack
-- trace, spec 12.3) and exit with the error value's code, normalized
-- to 1..255 (spec 8.10, 11.3).
failureExit :: CommonOpts -> TraceId -> LaskFailure -> IO a
failureExit opts traceId lf = do
  let codeLabel = maybe "E-RUNTIME" codeText (lfCode lf)
      -- Stage discriminates error-diagnostic lines in the JSON Lines
      -- stream (spec 12.2: code + stage).
      stage = case lfCode lf of
        Just c
          | c `elem` [EIoStdinRead, EIoEnvResolve, EIoImageMissing, EIoImageDigest, EIoFs, EIoDataDecode] ->
              StageIo
        _ -> StageRuntime
      msg = failureMessage lf
  if optJsonFormat opts
    then
      hPutStrLn stderr . T.unpack . TE.decodeUtf8 . BL.toStrict . A.encode $
        A.object
          [ ("code", A.String codeLabel),
            ("stage", A.String (stageText stage)),
            ("message", A.String msg),
            ("traceId", A.String traceId),
            ("error", A.String (encodeValue (lfError lf))),
            ("frames", A.toJSON (lfFrames lf))
          ]
    else do
      hPutStrLn stderr (T.unpack (codeLabel <> ": " <> msg))
      unless (null (lfFrames lf)) $ do
        hPutStrLn stderr "stack trace (innermost first):"
        mapM_ (hPutStrLn stderr . T.unpack . ("  at " <>)) (lfFrames lf)
  exitWith (ExitFailure (exitCodeOf (lfError lf)))

exitCodeOf :: Value -> Int
exitCodeOf v = case v of
  VRecord m
    | Just (VNumber n) <- Map.lookup "code" m ->
        let d = toRealFloat n :: Double
            i = round d :: Int
         in if fromIntegral i == d && i >= 1 && i <= 255 then i else 1
  _ -> 1

-- repl -----------------------------------------------------------------------

cmdRepl :: CommonOpts -> IO ()
cmdRepl opts = runRepl (optModule opts)

-- envs -----------------------------------------------------------------------

cmdEnvs :: EnvsOpts -> IO ()
cmdEnvs envsOpts = do
  let opts = envsCommon envsOpts
  compiled <- compileOrExit opts
  let core = compiledCore compiled
  -- Without a function, the whole module; with one, only what its call
  -- graph can reach (spec 11.4).
  scope <- case envsFunction envsOpts of
    Nothing -> pure Nothing
    Just fn -> case publicDecl compiled (kebabToSnake fn) of
      Just (key, _) -> pure (Just key)
      Nothing -> usageError opts ("no such function: '" <> fn <> "'")
  pins <- loadPins core
  traceId <- maybe newTraceId pure (optTraceId opts)
  writeErr <- newLineWriter stderr
  -- Probe processes get execution numbers too (spec 12.3).
  execCounter <- newIORef (0 :: Int)
  let cmdLogSink
        | optJsonFormat opts = jsonCommandLog traceId writeErr
        | otherwise = textCommandLog writeErr
      nextExec = atomicModifyIORef' execCounter (\n -> (n + 1, n + 1))
      refs =
        nub . sort $ case scope of
          Nothing -> collectEnvRefs core
          Just key -> collectEnvRefsFrom core key
  results <-
    mapM
      ( \ref -> do
          status <-
            if envsCheck envsOpts
              then Just <$> checkEnvRef cmdLogSink nextExec (imageCheck pins core) ref
              else pure Nothing
          pure (ref, status)
      )
      refs
  if optJsonFormat opts
    then
      TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . A.encode $
        [ A.object $
            [ (AK.fromText "name", A.String (refLabel ref)),
              (AK.fromText "kind", A.String (refKind ref)),
              (AK.fromText "target", A.String (refTarget ref))
            ]
              <> maybe [] (\s -> [(AK.fromText "status", A.String (either id (const "ok") s))]) status
        | (ref, status) <- results
        ]
    else
      mapM_
        ( \(ref, status) ->
            TIO.putStrLn $
              refLabel ref
                <> " ("
                <> refKind ref
                <> ": "
                <> refTarget ref
                <> ")"
                <> maybe "" (either (" NG: " <>) (const " ok")) status
        )
        results
  let failed = [() | (_, Just (Left _)) <- results]
  exitWith (if null failed then ExitSuccess else ExitFailure 3)

-- | Probe accessibility (spec 11.4): no command execution, no side
-- effects; docker checks daemon connectivity, remote checks SSH
-- session establishment. Probe processes relay through the command
-- execution log (spec 12.3: @envs --check@ is a relay target).
checkEnvRef :: CommandLogSink -> IO Int -> (EnvRef -> IO (Either Text ())) -> EnvRef -> IO (Either Text ())
checkEnvRef sink nextExec presence ref = case refKind ref of
  "local" -> pure (Right ())
  "docker" -> do
    let probeCmd = "docker version"
        envJson = A.object [("$type", A.String "Environment"), ("kind", A.String "docker")]
    execNo <- nextExec
    r <-
      try . runLoggedProcess sink ("#" <> refTarget ref) envJson execNo probeCmd $
        proc "docker" ["version", "--format", "{{.Server.Version}}"]
    case r of
      Right (0, _, _) -> presence ref
      Right (_, _, errOut) -> pure (Left (codeText EIoEnvResolve <> ": " <> T.strip errOut))
      Left e -> pure (Left (codeText EIoEnvResolve <> ": " <> T.pack (show (e :: IOError))))
  _ -> pure (Right ())

-- | Whether the image an enumerated environment needs is on the daemon,
-- as the lock resolves it (spec 11.4). A reference computed at run time
-- has nothing to check before it is computed.
imageCheck :: ImagePins -> CoreProgram -> EnvRef -> IO (Either Text ())
imageCheck pins core ref = case T.stripPrefix "recipe " (refTarget ref) of
  Just dockerfile -> do
    tags <-
      mapM
        (\(df, ctx, buildArgs) -> recipeTag (cpBaseDir core) df ctx buildArgs)
        [r | r@(df, _, _) <- collectRecipes core, df == dockerfile]
    present <- mapM (either (const (pure False)) imageExists) tags
    pure $
      if and present
        then Right ()
        else Left (codeText EIoImageMissing <> ": image for recipe '" <> dockerfile <> "' is not materialized; run 'lask env build'")
  Nothing
    | refLabel ref == "<dynamic>" -> pure (Right ())
    | otherwise -> either (Left . renderFailure) (const (Right ())) <$> resolveRegistry pins (refTarget ref)
  where
    renderFailure lf = maybe "" (\c -> codeText c <> ": ") (lfCode lf) <> failureMessage lf

-- deps (spec 11.5) ------------------------------------------------------------

-- | @lask deps sync@: fetch and verify every declared dependency
-- (including transitive ones) into the cache. This is the only
-- subcommand allowed to access the network for module resolution.
cmdDepsSync :: CommonOpts -> Bool -> IO ()
cmdDepsSync opts frozen = do
  let baseDir = takeDirectory (optModule opts)
      depsPath = baseDir </> defaultDepsFileName
  cacheDir <- cacheDirFor baseDir
  r <- loadDepsFile depsPath
  case r of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right Nothing -> do
      putStrLn "no dependencies declared"
      prior <- either (const Nothing) id <$> loadLockFile (baseDir </> defaultLockFileName)
      syncImages opts frozen (baseDir </> defaultLockFileName) (maybe emptyLock id prior)
    Right (Just df) -> do
      prior <- either (const Nothing) id <$> loadLockFile (baseDir </> defaultLockFileName)
      -- A declared reference that no longer matches the locked one must
      -- be fetched again, so that changing `rev` without changing
      -- `hash` is caught as E-MODULE-HASH-MISMATCH (spec 11.5).
      let declaredRef e = case e of
            DepGit _ ref -> Just ref
            DepUrl {} -> Nothing
          needsRecheck path e =
            case Map.lookup path (maybe Map.empty lockModules prior) of
              Nothing -> False
              Just locked -> lkRequested locked /= declaredRef e
          lockedHash path = lkHash <$> Map.lookup path (maybe Map.empty lockModules prior)
      results <- syncAll cacheDir lockedHash needsRecheck df
      mapM_
        ( \(path, _, status) -> case status of
            Right _ -> TIO.putStrLn (path <> " ok")
            Left d -> do
              TIO.putStrLn (path <> " NG")
              TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
        )
        results
      let failed = [() | (_, _, Left _) <- results]
          lockPath = baseDir </> defaultLockFileName
      existing0 <- either (const Nothing) id <$> loadLockFile lockPath
      -- Resolve each reference to the commit it currently names, so a
      -- tag that has been repointed is detected (spec 11.5).
      entries <- mapM (resolveEntry existing0) [(p, e, h) | (p, e, Right h) <- results]
      let moved = [m | Left m <- entries]
          newLock = LockFile (Map.fromList [ok | Right ok <- entries]) Map.empty
      unless (null moved) $ do
        mapM_ (TIO.hPutStrLn stderr) moved
        exitWith (ExitFailure 3)
      if null failed
        then do
          -- --frozen (spec 11.5): CI asserts that the committed lock is
          -- what resolution produces, rather than updating it.
          let existing = existing0
              -- The modules are written before the images are
              -- resolved: reading the program needs them locked.
              withImages = newLock {lockImages = maybe Map.empty lockImages existing}
          if frozen && Just (lockModules newLock) /= fmap lockModules existing
            then do
              TIO.hPutStrLn stderr
                (codeText EModuleLockStale <> ": the lock file is out of date (--frozen)")
              exitWith (ExitFailure 1)
            else do
              unless frozen $ BL.writeFile lockPath (renderLockFile withImages)
              syncImages opts frozen lockPath withImages
        else exitWith (ExitFailure 3)

-- | The images half of @deps sync@ (spec 11.5): with the modules in the
-- cache, the program can be read, and every image it requires is
-- materialized and pinned the way its modules are. A program that does
-- not compile keeps its modules synced and stops here, since its images
-- cannot be enumerated. Under @--frozen@ nothing is written, and a lock
-- the images would change is out of date.
syncImages :: CommonOpts -> Bool -> FilePath -> LockFile -> IO ()
syncImages opts frozen lockPath lock = do
  compiled <- compileOrExit opts
  m <- materialize (compiledCore compiled) (lockImages lock)
  mapM_ TIO.putStrLn (matReport m)
  mapM_ (TIO.hPutStrLn stderr) (matFailures m)
  let updated = lock {lockImages = matImages m}
  if frozen && updated /= lock
    then do
      TIO.hPutStrLn stderr
        (codeText EModuleLockStale <> ": the images in the lock file are out of date (--frozen)")
      exitWith (ExitFailure 1)
    else when (updated /= lock) $ BL.writeFile lockPath (renderLockFile updated)
  if null (matFailures m) then exitSuccess else exitWith (ExitFailure 3)

-- | Resolve a git reference to the commit it names and compare it with
-- what the lock already pins (spec 11.5). A reference that resolves to
-- a different commit than before is @E-MODULE-REV-MOVED@.
resolveEntry ::
  Maybe LockFile ->
  (Text, DepEntry, Text) ->
  IO (Either Text (Text, LockEntry))
resolveEntry existing (path, entry, hash) = case entry of
  DepUrl {} -> pure (Right (path, lockEntryOf entry hash))
  DepGit url rev -> do
    resolved <- resolveGitRev url rev
    let base = lockEntryOf entry hash
        wasRev = Map.lookup path (maybe Map.empty lockModules existing) >>= lkRev
    pure $ case (resolved, wasRev) of
      (Just sha, Just old)
        | sha /= old ->
            Left $
              codeText EModuleRevMoved
                <> ": '"
                <> path
                <> "': "
                <> rev
                <> " now resolves to "
                <> sha
                <> " (locked: "
                <> old
                <> ")"
      (Just sha, _) -> Right (path, base {lkRev = Just sha})
      (Nothing, _) -> Right (path, base)

-- | The lock record of a declared entry (spec chapter 5). @requested@
-- keeps the reference that was written; @rev@ is filled in only when
-- that reference is already a full commit SHA.
lockEntryOf :: DepEntry -> Text -> LockEntry
lockEntryOf (DepGit u r) h =
  LockEntry (Just u) Nothing (Just r) (if isFullSha r then Just r else Nothing) h
lockEntryOf (DepUrl u) h = LockEntry Nothing (Just u) Nothing Nothing h

isFullSha :: Text -> Bool
isFullSha r = T.length r == 40 && T.all (\c -> c `elem` ("0123456789abcdef" :: String)) r

-- | @lask deps add@: fetch the source, pin its content hash (trust on
-- first use), record the entry and place the verified source in the
-- cache.
cmdDepsAdd :: CommonOpts -> Text -> DepsAddSource -> IO ()
cmdDepsAdd opts name source = do
  unless (isLowerIdent name) $
    usageError opts ("dependency name must be a lower-case identifier: '" <> name <> "'")
  let baseDir = takeDirectory (optModule opts)
      depsPath = baseDir </> defaultDepsFileName
      depSource = case source of
        AddGit url rev -> SrcGit url rev
        AddUrl url -> SrcUrl url
  cacheDir <- cacheDirFor baseDir
  existingE <- loadDepsFile depsPath
  existing <- case existingE of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right mDf -> pure (maybe emptyDepsFile id mDf)
  fetched <- fetchAndStore cacheDir depSource
  case fetched of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 3)
    Right hash -> do
      let entry = case depSource of
            SrcGit url rev -> DepGit url rev
            SrcUrl url -> DepUrl url
          updated = existing {depsEntries = Map.insert name entry (depsEntries existing)}
      BL.writeFile depsPath (renderDepsFile updated)
      -- The resolution is recorded in the lock as well (spec 11.5), so
      -- the project is immediately resolvable without a second step.
      results <- syncAll cacheDir (const Nothing) (\_ _ -> False) updated
      BL.writeFile (baseDir </> defaultLockFileName)
        . renderLockFile
        . (\ms -> LockFile ms Map.empty)
        $ Map.fromList [(p, lockEntryOf e h) | (p, e, Right h) <- results]
      TIO.putStrLn (name <> " " <> hash)
      exitSuccess
  where
    isLowerIdent t = case T.uncons t of
      Just (c, rest) ->
        (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest
      Nothing -> False
    identChar c =
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

-- Shared helpers -----------------------------------------------------------------

compileOrExit :: CommonOpts -> IO Compiled
compileOrExit opts = do
  r <- compileFile (optModule opts)
  case r of
    Right compiled -> pure compiled
    Left ds -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) ds)
      exitWith (ExitFailure 1)


usageError :: CommonOpts -> Text -> IO a
usageError opts msg = do
  if optJsonFormat opts
    then
      TIO.hPutStrLn stderr . TE.decodeUtf8 . BL.toStrict . A.encode $
        A.object [("code", A.String (codeText ECliUsage)), ("message", A.String msg)]
    else TIO.hPutStrLn stderr (codeText ECliUsage <> ": " <> msg)
  exitWith (ExitFailure 4)

readStdinOrExit :: CommonOpts -> IO Text
readStdinOrExit opts = do
  isTty <- hIsTerminalDevice stdin
  if isTty
    then pure ""
    else do
      bytes <- BS.getContents
      case TE.decodeUtf8' bytes of
        Right t -> pure t
        Left e -> do
          if optJsonFormat opts
            then
              TIO.hPutStrLn stderr . TE.decodeUtf8 . BL.toStrict . A.encode $
                A.object
                  [ ("code", A.String (codeText EIoStdinRead)),
                    ("message", A.String (T.pack (show e)))
                  ]
            else TIO.hPutStrLn stderr (codeText EIoStdinRead <> ": " <> T.pack (show e))
          exitWith (ExitFailure 3)

-- | Diagnostics for stdout (@check@): a JSON array in json mode.
renderDiags :: Bool -> [Diagnostic] -> Text
renderDiags jsonFormat ds
  | jsonFormat = TE.decodeUtf8 (BL.toStrict (A.encode (map diagJson ds)))
  | otherwise = T.intercalate "\n" (map (T.pack . pretty) ds)

-- | Diagnostics for stderr: JSON Lines, one object per line
-- (spec 12.2 canonical form; discriminated by code + stage).
renderDiagsLines :: Bool -> [Diagnostic] -> Text
renderDiagsLines jsonFormat ds
  | jsonFormat =
      T.intercalate "\n" (map (TE.decodeUtf8 . BL.toStrict . A.encode . diagJson) ds)
  | otherwise = T.intercalate "\n" (map (T.pack . pretty) ds)

diagJson :: Diagnostic -> A.Value
diagJson d =
  A.object $
    [ ("code", A.String (codeText (diagCode d))),
      ("stage", A.String (stageText (diagStage d))),
      ("message", A.String (diagMessage d))
    ]
      <> location (diagSpan d)
      <> maybe [] (\e -> [("expected", A.String e)]) (diagExpected d)
      <> maybe [] (\a -> [("actual", A.String a)]) (diagActual d)
  where
    location (Span (Position file l c) _) =
      [ ( "location",
          A.object
            [ ("file", A.String (T.pack file)),
              ("line", A.Number (fromIntegral l)),
              ("column", A.Number (fromIntegral c))
            ]
        )
      ]
    location NoSpan = []

-- | @lask env build@ (spec 11.7): materialize every image the program
-- requires — registry references pulled and pinned, recipes built — and
-- record in the lock what each resolved to. With @deps sync@, the only
-- subcommand permitted to pull or to start a build.
cmdEnvBuild :: CommonOpts -> IO ()
cmdEnvBuild opts = do
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      lockPath = cpBaseDir core </> defaultLockFileName
  existing <- either (const Nothing) id <$> loadLockFile lockPath
  m <- materialize core (maybe Map.empty lockImages existing)
  mapM_ TIO.putStrLn (matReport m)
  -- A project with no image and no lock gets no lock file for nothing.
  unless (isNothing existing && Map.null (matImages m)) $
    BL.writeFile lockPath . renderLockFile $
      (maybe emptyLock id existing) {lockImages = matImages m}
  unless (null (matFailures m)) $ do
    mapM_ (TIO.hPutStrLn stderr) (matFailures m)
    exitWith (ExitFailure 3)

-- | @lask env list@ (spec 11.7): every image the program references,
-- what the lock resolves it to, and whether it is on the daemon. No
-- network access and no build.
cmdEnvList :: CommonOpts -> IO ()
cmdEnvList opts = do
  compiled <- compileOrExit opts
  rows <- imageRows (compiledCore compiled)
  if optJsonFormat opts
    then
      TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . A.encode $
        [ A.object
            [ (AK.fromText "source", A.String (irSource r)),
              (AK.fromText "kind", A.String (irKind r)),
              (AK.fromText "resolved", maybe A.Null A.String (irResolved r)),
              (AK.fromText "present", A.Bool (irPresent r))
            ]
        | r <- rows
        ]
    else forM_ rows $ \r ->
      TIO.putStrLn $
        irSource r
          <> "  "
          <> irKind r
          <> "  "
          <> maybe "not pinned (lask env build)" id (irResolved r)
          <> (if irPresent r then "  present" else "  MISSING")

-- | @lask cmd@ (spec 11.8): run a declared command in its declared
-- environment, as an argument vector rather than through a shell.
cmdCmd :: CmdOpts -> IO ()
cmdCmd cmdOpts = do
  let opts = cmdCommon cmdOpts
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      table = Map.findWithDefault Map.empty (cpEntry core) (cpCommands core)
  if cmdList cmdOpts
    then listCommands opts core table
    else case cmdName cmdOpts of
      Nothing -> usageError opts "no command given; try 'lask cmd --list'"
      Just name -> case Map.lookup name table of
        Nothing ->
          usageError opts $
            "'" <> name <> "' is not a command of this module; try 'lask cmd --list'"
        Just envCore -> do
          traceId <- maybe newTraceId pure (optTraceId opts)
          envValue <- evalCommandEnv core envCore >>= either (failureExit opts traceId) pure
          writeErr <- newLineWriter stderr
          let sink
                | optJsonFormat opts = jsonCommandLog traceId writeErr
                | otherwise = textCommandLog writeErr
          pins <- loadPins core
          r <- runDeclaredCommand pins (cpBaseDir core) sink (optJsonFormat opts) envValue name (cmdArgs cmdOpts)
          case r of
            -- Failures before the program starts keep the existing
            -- classification (spec 11.8); the program's own exit code
            -- passes through verbatim.
            Left failure -> failureExit opts traceId failure
            Right 0 -> exitSuccess
            Right code -> exitWith (ExitFailure code)

-- | The declared commands of the entry module, with the state of the
-- image each needs (spec 11.8). No network access and no build. An
-- environment that cannot be evaluated here — a variable it reads is
-- unset, say — is listed with the failure rather than ending the list.
listCommands :: CommonOpts -> CoreProgram -> Map.Map Text Core -> IO ()
listCommands opts core table = do
  pins <- loadPins core
  rows <- mapM (row pins) (Map.toList table)
  if optJsonFormat opts
    then
      TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . A.encode $
        [ A.object
            [ (AK.fromText "name", A.String name),
              (AK.fromText "kind", A.String kind),
              (AK.fromText "target", A.String target),
              (AK.fromText "present", A.Bool present)
            ]
        | (name, kind, target, present) <- rows
        ]
    else do
      let width = maximum (8 : [T.length n | (n, _, _, _) <- rows])
      forM_ rows $ \(name, kind, target, present) ->
        TIO.putStrLn
          ( T.justifyLeft width ' ' name
              <> "  "
              <> T.justifyLeft 6 ' ' kind
              <> "  "
              <> target
              <> (if present then "" else "  MISSING (lask env build)")
          )
  where
    row pins (name, envCore) = do
      r <- evalCommandEnv core envCore
      case r >>= resolveEnv of
        Left lf -> pure (name, "?", "<" <> failureMessage lf <> ">", False)
        Right resolved -> do
          present <- imagePresent pins (cpBaseDir core) resolved
          let (kind, target) = describeResolved resolved
          pure (name, kind, target, present)

    describeResolved resolved = case resolved of
      ResolvedLocal -> ("local", "local")
      ResolvedDocker image _ -> ("docker", image)
      ResolvedRecipe df _ _ -> ("docker", "recipe " <> df)

-- | Whether the image a command needs is on the target daemon. No
-- network access and no build (spec 11.8, 10.3).
imagePresent :: ImagePins -> FilePath -> ResolvedEnv -> IO Bool
imagePresent pins baseDir resolved = case resolved of
  ResolvedLocal -> pure True
  ResolvedDocker ref _ -> either (const False) (const True) <$> resolveRegistry pins ref
  ResolvedRecipe df ctx opts -> do
    tagE <- recipeTag baseDir df ctx (recipeBuildArgs opts)
    either (const (pure False)) imageExists tagE

-- | Evaluate a command's environment (spec 11.8). The environment of a
-- command declaration can reach no effect (ch. 5), so nothing here can
-- run a command, touch a file or read the standard input, which
-- belongs to the program. The hooks refuse rather than run anything if
-- that guarantee is ever broken.
evalCommandEnv :: CoreProgram -> Core -> IO (Either LaskFailure EnvValue)
evalCommandEnv core c = do
  ctx <- mkRtCtx core "" (RtHooks refuseCommand refuseFile (const (pure ())))
  r <- try (evalCore ctx Map.empty c)
  pure $ case r of
    Left lf -> Left lf
    Right (VEnv ev) -> Right ev
    Right _ -> Left (refusal "the command's environment did not evaluate to an Environment")
  where
    refuseCommand _ _ = pure (Left (refusal "the environment of a command declaration tried to run a command"))
    refuseFile _ _ = pure (Left (refusal "the environment of a command declaration tried to access a file"))
    refusal = ioFailure EIoEnvResolve

-- | @lask deps why@ (spec 11.5): the graph paths through which a
-- dependency is reached. A name may appear in the lock without
-- appearing in the project file, because a dependency can pull it in
-- or re-export it (chapter 5).
cmdDepsWhy :: CommonOpts -> Text -> IO ()
cmdDepsWhy opts name = do
  lock <- loadLockOrExit opts
  let paths = [p | p <- Map.keys (lockModules lock), name `elem` T.splitOn ">" p]
  if null paths
    then usageError opts ("no such dependency in the lock file: '" <> name <> "'")
    else mapM_ (TIO.putStrLn . T.replace ">" " -> ") paths

-- | @lask deps diff@ (spec 11.5): what changes between the locked
-- revision and the one the project file currently requests. The
-- capability delta comes first, because that is the part a reviewer
-- can check quickly.
cmdDepsDiff :: CommonOpts -> Text -> IO ()
cmdDepsDiff opts name = do
  let baseDir = takeDirectory (optModule opts)
  lock <- loadLockOrExit opts
  dfE <- loadDepsFile (baseDir </> defaultDepsFileName)
  df <- case dfE of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right mDf -> pure (maybe emptyDepsFile id mDf)
  entry <- case Map.lookup name (depsEntries df) of
    Just e -> pure e
    Nothing -> usageError opts ("no such dependency: '" <> name <> "'")
  locked <- case Map.lookup name (lockModules lock) of
    Just e -> pure e
    Nothing -> usageError opts ("dependency '" <> name <> "' is not in the lock file")
  let requested = case entry of
        DepGit _ r -> Just r
        DepUrl _ -> Nothing
      diffLine l r
        | l == r = "  = " <> maybe "-" id l
        | otherwise = "  - " <> maybe "-" id l <> "\n  + " <> maybe "-" id r
  TIO.putStrLn "revision:"
  TIO.putStrLn (diffLine (lkRequested locked) requested)
  TIO.putStrLn "content hash:"
  TIO.putStrLn ("  = " <> lkHash locked)
  when (lkRequested locked /= requested) $
    TIO.putStrLn "run 'lask deps sync' to resolve and review the new revision"


loadLockOrExit :: CommonOpts -> IO LockFile
loadLockOrExit opts = do
  let baseDir = takeDirectory (optModule opts)
  r <- loadLockFile (baseDir </> defaultLockFileName)
  case r of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right Nothing -> do
      TIO.hPutStrLn stderr (codeText EModuleLockStale <> ": no lock file; run 'lask deps sync'")
      exitWith (ExitFailure 1)
    Right (Just lf) -> pure lf

