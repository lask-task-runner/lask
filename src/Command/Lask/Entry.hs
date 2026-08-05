{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry points: subcommand implementations, stdin capture,
-- diagnostics output and exit-code mapping (spec 9, 11, 14.8).
module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.ArgCodec
import Command.Lask.Options
import Control.Exception (try)
import Control.Monad (unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Language.LSP.Lask as LSP
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Core.AST
import Language.Lask.Deps.Cache (defaultCacheDir)
import Language.Lask.Deps.Fetch (DepSource (..), fetchAndStore, syncAll)
import Language.Lask.Deps.File
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.EnvFile
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Obs.Events (TraceId, encodeEvent, newTraceId, noSink)
import Language.Lask.Repl (runRepl)
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Eval (RtCtx (..), applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, renderValueText)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Types (Type (..), renderType)
import Language.Lask.Utils (Pretty (pretty))
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
  CmdDepsSync opts -> cmdDepsSync opts
  CmdDepsAdd opts name source -> cmdDepsAdd opts name source
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
cmdRunEval printResult runOpts = do
  let opts = runCommon runOpts
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      entry = cpEntry core
      baseDir = cpBaseDir core
  envFile <- loadEnvFileOrExit opts (envFilePathOf baseDir (runEnvFile runOpts))
  validateEnvNamesOrExit opts core envFile

  let fnName = kebabToSnake (runFunction runOpts)
  cd <- case Map.lookup (entry, fnName) (cpDecls core) of
    Just cd -> pure cd
    Nothing -> usageError opts ("no such function: '" <> runFunction runOpts <> "'")

  cliArgs <- case parseCliArgs (runArgs runOpts) of
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
  let sshSettings =
        SshSettings
          { sshKnownHosts = runSshKnownHosts runOpts,
            sshStrictHostKeyChecking = runSshStrictHostKey runOpts,
            sshConnectTimeout = runSshConnectTimeout runOpts
          }
      -- Command execution logs relay to stderr in real time
      -- (spec 12.3); JSON Lines under --format json (12.2).
      cmdLogSink
        | optJsonFormat opts = jsonCommandLog traceId writeErr
        | otherwise = textCommandLog writeErr
  runner <- mkCommandRunner baseDir envFile sshSettings cmdLogSink
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
          | c `elem` [EIoStdinRead, EIoSshConnect, EIoSshAuth, EIoEnvResolve, EIoFs, EIoDataDecode] ->
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

data EnvRef = EnvRef
  { refLabel :: Text,
    refKind :: Text,
    refTarget :: Text
  }
  deriving (Show, Eq, Ord)

cmdEnvs :: EnvsOpts -> IO ()
cmdEnvs envsOpts = do
  let opts = envsCommon envsOpts
  compiled <- compileOrExit opts
  let core = compiledCore compiled
      baseDir = cpBaseDir core
  envFile <- loadEnvFileOrExit opts (envFilePathOf baseDir (envsEnvFile envsOpts))
  validateEnvNamesOrExit opts core envFile
  -- With a function filter, listing every referenced environment is a
  -- permitted over-approximation of reachability (spec 11.4).
  case envsFunction envsOpts of
    Just fn ->
      unless (Map.member (cpEntry core, kebabToSnake fn) (cpDecls core)) $
        usageError opts ("no such function: '" <> fn <> "'")
    Nothing -> pure ()
  traceId <- maybe newTraceId pure (optTraceId opts)
  writeErr <- newLineWriter stderr
  -- Probe processes get execution numbers too (spec 12.3).
  execCounter <- newIORef (0 :: Int)
  let cmdLogSink
        | optJsonFormat opts = jsonCommandLog traceId writeErr
        | otherwise = textCommandLog writeErr
      nextExec = atomicModifyIORef' execCounter (\n -> (n + 1, n + 1))
      refs = nub (sort (collectEnvRefs core envFile))
  results <-
    mapM
      ( \ref -> do
          status <-
            if envsCheck envsOpts
              then Just <$> checkEnvRef envsOpts cmdLogSink nextExec envFile ref
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

-- | All environment constructions in the core program.
collectEnvRefs :: CoreProgram -> Maybe EnvFile -> [EnvRef]
collectEnvRefs core envFile =
  concatMap (fromCore . cdCore) (Map.elems (cpDecls core))
    <> concatMap (fromCore . snd) (concatMap (lamKeywords' . cdCore) (Map.elems (cpDecls core)))
  where
    lamKeywords' c = case coreF c of
      CLam lam -> lamKeywords lam
      _ -> []

    fromCore c = case coreF c of
      CEnv kind args -> mkRef kind args : concatMap (fromCore . snd) args
      _ -> concatMap fromCore (children c)

    mkRef kind args = case kind of
      "docker" -> case lookup "image" args of
        Just (Core _ (CStrLit img)) -> EnvRef img "docker" img
        _ -> EnvRef "<dynamic>" "docker" "<dynamic image>"
      "env" -> case lookup "name" args of
        Just (Core _ (CStrLit name)) ->
          case envFile >>= Map.lookup name . envFileEntries of
            Just (EnvEntry k ps) -> EnvRef name k (entryTarget k ps)
            Nothing -> EnvRef name "env" "<undefined>"
        _ -> EnvRef "<env>" "env" "<unknown>"
      _ -> EnvRef kind kind kind

    entryTarget k ps = case k of
      "remote" -> case Map.lookup "host" ps of
        Just (VString h) -> h
        _ -> "<unknown host>"
      "docker" -> case Map.lookup "image" ps of
        Just (VString i) -> i
        _ -> "<unknown image>"
      _ -> k

    children c = case coreF c of
      CStr ps -> [e | CPExpr e <- ps]
      CArray es -> es
      CMapLit kvs -> map snd kvs
      CRecordLit kvs -> map snd kvs
      CLam lam -> map snd (lamKeywords lam) <> [lamBody lam]
      CApp fn pos kw -> fn : pos <> map snd kw
      CDot e _ -> [e]
      CIndex _ a b -> [a, b]
      CIf a b c' -> [a, b, c']
      CAnd a b -> [a, b]
      COr a b -> [a, b]
      CNot a -> [a]
      CBin _ a b -> [a, b]
      CDo stmts -> concatMap stmtExpr stmts
      CAwait a -> [a]
      CCast a _ -> [a]
      CEnv _ args -> map snd args
      _ -> []
    stmtExpr (CSBind _ e) = [e]
    stmtExpr (CSExpr e) = [e]

-- | Probe accessibility (spec 11.4): no command execution, no side
-- effects; docker checks daemon connectivity, remote checks SSH
-- session establishment. Probe processes relay through the command
-- execution log (spec 12.3: @envs --check@ is a relay target).
checkEnvRef :: EnvsOpts -> CommandLogSink -> IO Int -> Maybe EnvFile -> EnvRef -> IO (Either Text ())
checkEnvRef envsOpts sink nextExec envFile ref = case refKind ref of
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
  "remote" -> do
    let entry = envFile >>= Map.lookup (refLabel ref) . envFileEntries
    case entry of
      Just (EnvEntry _ ps) -> do
        let host = case Map.lookup "host" ps of
              Just (VString h) -> h
              _ -> ""
            user = case Map.lookup "user" ps of
              Just (VString u) -> Just u
              _ -> Nothing
            port = case Map.lookup "port" ps of
              Just (VNumber n) -> Just (round (toRealFloat n :: Double))
              _ -> Nothing
            settings =
              SshSettings
                { sshKnownHosts = envsSshKnownHosts envsOpts,
                  sshStrictHostKeyChecking = envsSshStrictHostKey envsOpts,
                  sshConnectTimeout = envsSshConnectTimeout envsOpts
                }
            envJson =
              A.object
                [ ("$type", A.String "Environment"),
                  ("kind", A.String "remote"),
                  ("name", A.String (refLabel ref))
                ]
        execNo <- nextExec
        r <-
          try . runLoggedProcess sink ("#env(\"" <> refLabel ref <> "\")") envJson execNo "ssh true" $
            proc "ssh" (sshArgs settings host user port "true")
        pure $ case r of
          Right (0, _, _) -> Right ()
          Right (_, _, errOut) -> Left (codeText EIoSshConnect <> ": " <> T.strip errOut)
          Left e -> Left (codeText EIoSshConnect <> ": " <> T.pack (show (e :: IOError)))
      Nothing -> pure (Left (codeText EIoEnvResolve <> ": undefined environment"))
  _ -> pure (Left (codeText EIoEnvResolve <> ": " <> refTarget ref))

-- deps (spec 11.5) ------------------------------------------------------------

-- | @lask deps sync@: fetch and verify every declared dependency
-- (including transitive ones) into the cache. This is the only
-- subcommand allowed to access the network for module resolution.
cmdDepsSync :: CommonOpts -> IO ()
cmdDepsSync opts = do
  cacheDir <- defaultCacheDir
  let depsPath = takeDirectory (optModule opts) </> defaultDepsFileName
  r <- loadDepsFile depsPath
  case r of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right Nothing -> do
      putStrLn "no dependencies declared"
      exitSuccess
    Right (Just df) -> do
      results <- syncAll cacheDir df
      mapM_
        ( \(name, status) -> case status of
            Right () -> TIO.putStrLn (name <> " ok")
            Left d -> do
              TIO.putStrLn (name <> " NG")
              TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
        )
        results
      let failed = [() | (_, Left _) <- results]
      exitWith (if null failed then ExitSuccess else ExitFailure 3)

-- | @lask deps add@: fetch the source, pin its content hash (trust on
-- first use), record the entry and place the verified source in the
-- cache.
cmdDepsAdd :: CommonOpts -> Text -> DepsAddSource -> IO ()
cmdDepsAdd opts name source = do
  unless (isLowerIdent name) $
    usageError opts ("dependency name must be a lower-case identifier: '" <> name <> "'")
  cacheDir <- defaultCacheDir
  let depsPath = takeDirectory (optModule opts) </> defaultDepsFileName
      depSource = case source of
        AddGit url rev -> SrcGit url rev
        AddUrl url -> SrcUrl url
  existingE <- loadDepsFile depsPath
  existing <- case existingE of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right mDf -> pure (maybe (DepsFile Map.empty) id mDf)
  fetched <- fetchAndStore cacheDir depSource
  case fetched of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 3)
    Right hash -> do
      let entry = case depSource of
            SrcGit url rev -> DepGit url rev hash
            SrcUrl url -> DepUrl url hash
          updated = DepsFile (Map.insert name entry (depsEntries existing))
      BL.writeFile depsPath (renderDepsFile updated)
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

envFilePathOf :: FilePath -> Maybe FilePath -> FilePath
envFilePathOf baseDir = maybe (baseDir </> defaultEnvFileName) id

loadEnvFileOrExit :: CommonOpts -> FilePath -> IO (Maybe EnvFile)
loadEnvFileOrExit opts path = do
  r <- loadEnvFile path
  case r of
    Right mFile -> pure mFile
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)

-- | Static cross-check of @#env("name")@ references against the
-- environment definition file (spec 10.3).
validateEnvNamesOrExit :: CommonOpts -> CoreProgram -> Maybe EnvFile -> IO ()
validateEnvNamesOrExit opts core envFile = do
  let names = nub [n | EnvRef n "env" "<undefined>" <- collectEnvRefs core envFile]
  unless (null names) $ do
    TIO.hPutStrLn stderr . renderDiagsLines (optJsonFormat opts) $
      [ Diagnostic
          ETypeEnvConstruct
          StageStatic
          NoSpan
          ("environment '" <> n <> "' is not defined in the environment definition file")
          Nothing
          Nothing
          []
      | n <- names
      ]
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
