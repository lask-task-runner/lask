{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry points: subcommand implementations, stdin capture,
-- diagnostics output and exit-code mapping (spec 9, 11, 14.8).
module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.ArgCodec
import Command.Lask.Envs (EnvRef (..), collectEnvRefs, collectEnvRefsFrom, collectRecipes)
import Command.Lask.Help
import Command.Lask.Options
import Control.Exception (try)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (isNothing, listToMaybe)
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
import Language.Lask.Deps.Cache (cacheDirFor)
import Language.Lask.Deps.Fetch (DepSource (..), fetchAndStore, resolveGitRev, syncAll)
import Language.Lask.Deps.File
import Language.Lask.Deps.Lock
import Language.Lask.Diagnostic (Diagnostic (..), mkDiagnostic)
import Language.Lask.Doc (DocComment, docBlockAbove, emptyDoc, parseDoc)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.ErrorCode
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Obs.CommandLog
import Language.Lask.Obs.Events (TraceId, encodeEvent, newTraceId, noSink)
import Language.Lask.Repl (runRepl)
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Image (buildRecipe, imageExists, recipeTag)
import Language.Lask.Runtime.Eval (RtCtx (..), applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, renderValueText)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (Type (..), renderType)
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
  CmdInfer opts symbol -> cmdInfer opts symbol
  CmdRepl opts -> cmdRepl opts
  CmdEnvs envsOpts -> cmdEnvs envsOpts
  CmdDepsSync opts frozen -> cmdDepsSync opts frozen
  CmdDepsAdd opts name source -> cmdDepsAdd opts name source
  CmdDepsWhy opts name -> cmdDepsWhy opts name
  CmdDepsDiff opts name -> cmdDepsDiff opts name
  CmdEnvBuild opts -> cmdEnvBuild opts
  CmdEnvList opts -> cmdEnvList opts
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
    Right compiled -> do
      if optJsonFormat opts
        then TIO.putStrLn "[]"
        else putStrLn "the module is valid"
      exitSuccess

-- infer ---------------------------------------------------------------------

cmdInfer :: CommonOpts -> Maybe Text -> IO ()
cmdInfer opts symbol = do
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      entry = cpEntry core
      entryDecls =
        Map.toAscList (Map.filterWithKey (\(p, _) _ -> p == entry) (cpDecls core))
  case symbol of
    Just name ->
      case Map.lookup (entry, kebabToSnake name) (cpDecls core) of
        Just cd -> TIO.putStrLn (cdName cd <> ": " <> renderType (cdType cd))
        Nothing -> usageError opts ("no such symbol: '" <> name <> "'")
    Nothing ->
      mapM_
        (\(_, cd) -> TIO.putStrLn (cdName cd <> ": " <> renderType (cdType cd)))
        entryDecls

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
      entry = cpEntry core
      baseDir = cpBaseDir core

  rawName <- case runFunction runOpts of
    Just n -> pure n
    Nothing -> usageError opts "no function specified (use --help to list the module's functions)"
  let fnName = kebabToSnake rawName
  -- A declaration marked `internal` is not part of any surface
  -- (spec 5), so it is not callable from the CLI either.
  cd <- case Map.lookup (entry, fnName) (cpDecls core) of
    Just cd | not (fnName `Set.member` cpInternal core) -> pure cd
    _ -> usageError opts ("no such function: '" <> rawName <> "'")

  cliArgs <- case parseCliArgs (dropArgSeparator (runArgs runOpts)) of
    Right as -> pure as
    Left e -> usageError opts e

  (posVals, kwVals) <- do
    r <- case cdParams cd of
      Just params -> bindCliArgs params (runArgDecode runOpts) cliArgs
      Nothing -> case cdType cd of
        TyFun paramTys _ ->
          -- A function-typed value declaration: positional only
          -- (spec 11.2, example 16.3).
          bindCliArgs
            (StaticParams (zip (map (const "arg") paramTys) paramTys) Nothing [])
            (runArgDecode runOpts)
            cliArgs
        _ -> pure (Left ("'" <> fnName <> "' is not a callable function"))
    case r of
      Right bound -> pure bound
      Left e -> usageError opts e

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
  runner <- mkCommandRunner baseDir cmdLogSink
  ctx0 <- mkRtCtx core stdinText runner
  let sink
        | optJsonFormat opts = writeErr . encodeEvent
        | otherwise = noSink
      ctx = ctx0 {rtTraceId = traceId, rtEmit = sink}
  result <- try $ do
    fv <- topValue ctx (entry, fnName)
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
      declsByName =
        [ (n, d)
        | m <- maybe [] pure (partialModule partial),
          d <- AST.moduleDecls m,
          Just n <- [declaredName d],
          not (n `Set.member` maybe Set.empty cpInternal core)
        ]
      coreOf n = core >>= Map.lookup (entry, n) . cpDecls
      helpOf envs (n, d) = buildFunctionHelp path src d (coreOf n) (docFor src comments d) envs

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
      decl <- case lookup fnName declsByName of
        Just d -> pure d
        Nothing -> usageError opts (noSuchFunction rawName (map fst declsByName))
      let envs = case core of
            Just c -> nub (sort (collectEnvRefsFrom c (entry, fnName)))
            Nothing -> []
          fh = helpOf envs (fnName, decl)
      if optJsonFormat opts
        then TIO.putStrLn (encodeJsonText (renderHelpJson fh))
        else TIO.putStr (renderHelpText subcommand fh)
      exitSuccess
  where
    try' :: IO Text -> IO (Either IOError Text)
    try' = try

    -- Help is read-only: an unreadable or malformed environment file
    -- costs the environment targets, not the help.

declaredName :: AST.Decl -> Maybe Text
declaredName d = case AST.declF d of
  AST.DValue n _ _ _ -> Just n
  AST.DFunction n _ _ _ -> Just n
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
      msg = case lfError lf of
        VRecord m | Just (VString s) <- Map.lookup "message" m -> s
        other -> encodeValue other
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
      baseDir = cpBaseDir core
  -- Without a function, the whole module; with one, only what its call
  -- graph can reach (spec 11.4).
  scope <- case envsFunction envsOpts of
    Nothing -> pure Nothing
    Just fn -> do
      let key = (cpEntry core, kebabToSnake fn)
      unless (Map.member key (cpDecls core)) $
        usageError opts ("no such function: '" <> fn <> "'")
      pure (Just key)
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
              then Just <$> checkEnvRef cmdLogSink nextExec ref
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
checkEnvRef :: CommandLogSink -> IO Int -> EnvRef -> IO (Either Text ())
checkEnvRef sink nextExec ref = case refKind ref of
  "local" -> pure (Right ())
  "docker" -> do
    let probeCmd = "docker version"
        envJson = A.object [("$type", A.String "Environment"), ("kind", A.String "docker")]
    execNo <- nextExec
    r <-
      try . runLoggedProcess sink ("#" <> refTarget ref) envJson execNo probeCmd $
        proc "docker" ["version", "--format", "{{.Server.Version}}"]
    pure $ case r of
      Right (0, _, _) -> Right ()
      Right (_, _, errOut) -> Left (codeText EIoEnvResolve <> ": " <> T.strip errOut)
      Left e -> Left (codeText EIoEnvResolve <> ": " <> T.pack (show (e :: IOError)))
  _ -> pure (Right ())

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
      exitSuccess
    Right (Just df) -> do
      prior <- either (const Nothing) id <$> loadLockFile (baseDir </> defaultLockFileName)
      -- A declared reference that no longer matches the locked one must
      -- be fetched again, so that changing `rev` without changing
      -- `hash` is caught as E-MODULE-HASH-MISMATCH (spec 11.5).
      let declaredRef e = case e of
            DepGit _ r _ -> Just r
            DepUrl {} -> Nothing
          needsRecheck path e =
            case Map.lookup path (maybe Map.empty lockModules prior) of
              Nothing -> False
              Just locked -> lkRequested locked /= declaredRef e
      results <- syncAll cacheDir needsRecheck df
      mapM_
        ( \(path, _, status) -> case status of
            Right () -> TIO.putStrLn (path <> " ok")
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
      entries <- mapM (resolveEntry existing0) [(p, e) | (p, e, Right ()) <- results]
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
          if frozen && Just (lockModules newLock) /= fmap lockModules existing
            then do
              TIO.hPutStrLn stderr
                (codeText EModuleLockStale <> ": the lock file is out of date (--frozen)")
              exitWith (ExitFailure 1)
            else do
              BL.writeFile lockPath (renderLockFile newLock)
              exitSuccess
        else exitWith (ExitFailure 3)

-- | Resolve a git reference to the commit it names and compare it with
-- what the lock already pins (spec 11.5). A reference that resolves to
-- a different commit than before is @E-MODULE-REV-MOVED@.
resolveEntry ::
  Maybe LockFile ->
  (Text, DepEntry) ->
  IO (Either Text (Text, LockEntry))
resolveEntry existing (path, entry) = case entry of
  DepUrl {} -> pure (Right (path, lockEntryOf entry))
  DepGit url rev _ -> do
    resolved <- resolveGitRev url rev
    let base = lockEntryOf entry
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
lockEntryOf :: DepEntry -> LockEntry
lockEntryOf (DepGit u r h) =
  LockEntry (Just u) Nothing (Just r) (if isFullSha r then Just r else Nothing) h
lockEntryOf (DepUrl u h) = LockEntry Nothing (Just u) Nothing Nothing h

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
            SrcGit url rev -> DepGit url rev hash
            SrcUrl url -> DepUrl url hash
          updated = existing {depsEntries = Map.insert name entry (depsEntries existing)}
      BL.writeFile depsPath (renderDepsFile updated)
      -- The resolution is recorded in the lock as well (spec 11.5), so
      -- the project is immediately resolvable without a second step.
      results <- syncAll cacheDir (\_ _ -> False) updated
      BL.writeFile (baseDir </> defaultLockFileName)
        . renderLockFile
        . (\ms -> LockFile ms Map.empty)
        $ Map.fromList [(p, lockEntryOf e) | (p, e, Right ()) <- results]
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

-- | @lask env build@ (spec 11.7): materialize every recipe image the
-- program requires. With @deps sync@, the only subcommand permitted to
-- start a build.
cmdEnvBuild :: CommonOpts -> IO ()
cmdEnvBuild opts = withRecipes opts $ \baseDir recipes -> do
  results <- mapM (buildOne baseDir) recipes
  let failures = [e | Left e <- results]
      built = [(df, tag) | Right (df, tag) <- results]
  mapM_ (\(df, tag) -> TIO.putStrLn (df <> " -> " <> tag)) built
  -- Record what was materialized, so `lask.lock.json` names the images
  -- the resolved graph requires (spec chapter 5, 10.3).
  let lockPath = baseDir </> defaultLockFileName
  existing <- either (const Nothing) id <$> loadLockFile lockPath
  BL.writeFile lockPath . renderLockFile $
    (maybe emptyLock id existing)
      { lockImages =
          Map.fromList [("#" <> df, LockImage "recipe" Nothing (Just tag)) | (df, tag) <- built]
      }
  unless (null failures) $ do
    mapM_ (TIO.hPutStrLn stderr) failures
    exitWith (ExitFailure 3)
  where
    buildOne baseDir (df, ctx) = do
      tagE <- recipeTag baseDir df ctx
      case tagE of
        Left e -> pure (Left e)
        Right tag -> do
          r <- buildRecipe baseDir df ctx tag
          pure $ case r of
            Left e -> Left (df <> ": " <> e)
            Right () -> Right (df, tag)

-- | @lask env list@ (spec 11.7): report every recipe image and whether
-- it is present. Performs no network access and no build.
cmdEnvList :: CommonOpts -> IO ()
cmdEnvList opts = withRecipes opts $ \baseDir recipes ->
  forM_ recipes $ \(df, ctx) -> do
    tagE <- recipeTag baseDir df ctx
    case tagE of
      Left e -> TIO.hPutStrLn stderr e
      Right tag -> do
        ok <- imageExists tag
        TIO.putStrLn (df <> "  recipe  " <> tag <> (if ok then "  present" else "  MISSING"))

-- | Load the target module and hand its recipe environments to the
-- action, exiting on static errors.
withRecipes :: CommonOpts -> (FilePath -> [(Text, Text)] -> IO ()) -> IO ()
withRecipes opts action = do
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      baseDir = cpBaseDir core
      recipes = nubOrd (collectRecipes core)
  action baseDir recipes
  where
    nubOrd = Set.toList . Set.fromList

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
        DepGit _ r _ -> Just r
        DepUrl _ _ -> Nothing
      line l r
        | l == r = "  = " <> maybe "-" id l
        | otherwise = "  - " <> maybe "-" id l <> "\n  + " <> maybe "-" id r
  TIO.putStrLn "revision:"
  TIO.putStrLn (line (lkRequested locked) requested)
  TIO.putStrLn "content hash:"
  TIO.putStrLn
    ( if Just (lkHash locked) == entryHashMaybe entry
        then "  = " <> lkHash locked
        else "  - " <> lkHash locked <> "\n  + (unresolved; run 'lask deps sync')"
    )
  when (lkRequested locked /= requested) $
    TIO.putStrLn "run 'lask deps sync' to resolve and review the new revision"

entryHashMaybe :: DepEntry -> Maybe Text
entryHashMaybe = Just . entryHash

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

