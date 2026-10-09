{-# LANGUAGE OverloadedStrings #-}

-- | CLI entry points: subcommand implementations, stdin capture,
-- diagnostics output and exit-code mapping (spec 9, 11, 14.8).
module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.ArgCodec
import Command.Lask.Common
import Command.Lask.Project (cmdDepsAdd, cmdDepsGraph, cmdDepsList, cmdDepsRm, cmdEnvsList, cmdSync)
import Command.Lask.Complete (completionScript)
import Command.Lask.Envs (collectEnvReadsFrom, collectEnvRefsFrom, readsStdin)
import Command.Lask.Secrets (Scope (..), secretsCheck, secretsList)
import Language.Lask.Confirm (Prompt (..), confirmationFor, describeRule, ruleFor)
import Language.Lask.Core.AST (Core (..))
import Command.Lask.Help
import Command.Lask.Options
import Control.Exception (IOException, SomeException, fromException, toException, try)
import Control.Monad (forM, unless, when, (>=>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
import Language.Lask.Diagnostic (Advisory (..))
import Language.Lask.Doc (DocComment, docBlockAbove, emptyDoc, parseDoc)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.ErrorCode
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Obs.CommandLog
import Language.Lask.Obs.Events (TraceId, encodeEvent, newTraceId, noSink)
import Language.Lask.Repl (runRepl)
import Language.Lask.Runtime.Environment
import Command.Lask.Images (loadPins)
import Language.Lask.Runtime.Image (ImagePins, Recipe (..), imageExists, recipeSource, recipeTag, resolveRegistry)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.SecretStore.Resolve (newSecretResolver, readEnvVar, readEnvVarUnresolved)
import Language.Lask.Obs.ExecLog (jsonLogSink, textLogSink)
import Language.Lask.Runtime.AsyncTrack (AsyncSite (..), AsyncTracker (..), newAsyncTracker, noAsyncTracker, renderSite)
import Language.Lask.Runtime.Eval (RtCtx (..), applyValue, evalCore, mkRtCtx, topValue)
import Language.Lask.Runtime.Secrets (maskFailure)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, failureMessage, renderValueText)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (Type (..))
import Language.Lask.Utils (Pretty (pretty), kebabToSnake)
import Paths_lask (version)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdin)

runRootCommand :: RootCommand -> IO ()
runRootCommand cmd = case cmd of
  CmdServe -> LSP.serve >> pure ()
  CmdCheck opts -> cmdCheck opts
  CmdRun runOpts -> cmdRunEval False runOpts
  CmdEval runOpts -> cmdRunEval True runOpts
  CmdRepl opts -> cmdRepl opts
  CmdEnvsList envsOpts -> cmdEnvsList (envsCommon envsOpts) (envsFunction envsOpts)
  CmdSync opts frozen prune -> cmdSync opts frozen prune
  CmdDepsList opts -> cmdDepsList opts
  CmdDepsGraph opts name depth -> cmdDepsGraph opts name depth
  CmdDepsAdd opts name source -> cmdDepsAdd opts name source
  CmdDepsRm opts name -> cmdDepsRm opts name
  CmdSecretsList o -> cmdSecrets False o
  CmdSecretsCheck o -> cmdSecrets True o
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
    -- Advisories are reported alongside a valid module and leave the
    -- exit code at 0 (spec 14.2).
    Right compiled -> do
      let advisories = cpAdvisories (compiledCore compiled)
      if optJsonFormat opts
        then TIO.putStrLn (TE.decodeUtf8 (BL.toStrict (A.encode (map advisoryJson advisories))))
        else do
          mapM_ (putStrLn . pretty) advisories
          putStrLn "the module is valid"
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
  (posVals, kwVals) <- case cdParams cd of
    -- A declaration with type parameters is invoked with each one
    -- decoded at the widest type it admits, and a named bound checked
    -- against what was decoded (spec 11.2): a bounded parameter is not
    -- opaque inside the body, which may compare or sort it (4.4).
    Just params -> do
      bound <-
        orUsageError
          (bindCliArgs (instantiateForCli (cdTypeVars cd) (cdBounds cd) params) (runArgDecode runOpts) cliArgs)
      orUsageError (uncurry (checkCliBounds (cdBounds cd) params) bound)
      pure bound
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

  -- Before anything is evaluated, and before stdin is read: a refused
  -- confirmation leaves nothing half done (spec 11.2).
  confirmOrExit opts (runConfirm runOpts) compiled fnName key cd posVals kwVals
  -- Read only by a function that can refer to it: a program that
  -- never names stdin cannot tell it was left unread (9.2), and a
  -- script that starts lask with stdin open, as a CI step or a
  -- process manager does, would otherwise wait on it for nothing.
  stdinText <- if readsStdin core key then readStdinOrExit opts else pure ""
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
  tracker <- newAsyncTracker
  secrets <- newSecretResolver
  ctx0 <- mkRtCtx core stdinText (RtHooks runner fileRunner logSink tracker (readEnvVar secrets))
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
  -- A computation nothing awaited is waited for rather than cut short
  -- by the end of the process, and reported (spec 6.3). Whatever it
  -- did, the run keeps its own outcome and exit code.
  unawaited <- drainUnawaited tracker
  mapM_ (fmap (unawaitedDiagnostic opts traceId) . maskOutcome >=> writeErr) unawaited
  case result of
    Left lf -> failureExit opts traceId lf
    Right v -> do
      when printResult $ case v of
        VVoid -> pure ()
        _ -> TIO.putStrLn (encodeResult (runStdoutEncode runOpts) v)
      exitSuccess

-- | A computation's outcome with the failure it ended in masked, for
-- the advisory that reports it (spec 12.8).
maskOutcome :: (AsyncSite, Either SomeException Value) -> IO (AsyncSite, Either SomeException Value)
maskOutcome (site, Left ex)
  | Just lf <- fromException ex = (\m -> (site, Left (toException m))) <$> maskFailure lf
maskOutcome outcome = pure outcome

-- | The advisory @W-ASYNC-UNAWAITED@ (spec 6.3, 14.2) for one
-- computation that was never awaited, with how it ended.
unawaitedDiagnostic :: CommonOpts -> TraceId -> (AsyncSite, Either SomeException Value) -> Text
unawaitedDiagnostic opts traceId (site, outcome)
  | optJsonFormat opts =
      TE.decodeUtf8 . BL.toStrict . A.encode . A.object $
        [ ("code", A.String code),
          ("severity", "warning"),
          ("stage", "runtime"),
          ("message", A.String message),
          ("traceId", A.String traceId)
        ]
          <> [ ( "location",
                 A.object
                   [ ("file", A.String (T.pack (siteModule site))),
                     ("line", A.toJSON l),
                     ("column", A.toJSON c)
                   ]
               )
             | Just (l, c) <- [sitePosition site]
             ]
          <> [("failure", A.object [("code", A.String fc), ("message", A.String fm)]) | Left (fc, fm, _) <- [ended]]
  | otherwise = code <> ": " <> message
  where
    code = "W-ASYNC-UNAWAITED"
    ended = case outcome of
      Right _ -> Right ()
      Left ex -> Left $ case fromException ex of
        Just lf -> (maybe "E-RUNTIME" codeText (lfCode lf), failureMessage lf, Just (exitCodeOf (lfError lf)))
        Nothing -> ("E-RUNTIME", T.pack (show ex), Nothing)
    message =
      "the async at "
        <> renderSite site
        <> " was never awaited; it was waited for at the end of the run and "
        <> either failed (const "completed") ended
    failed (fc, fm, exit) =
      "failed with "
        <> fc
        <> maybe "" (\n -> " (exit code " <> T.pack (show n) <> ")") exit
        <> (if T.null (T.strip fm) then "" else ": " <> fm)

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
        (buildFunctionHelp hp hsrc d (coreOf key) (docFor hsrc hcomments d) envs)
          { fhName = n,
            fhConfirm = do
              prog <- partialProgram partial
              describeRule . snd <$> ruleFor prog (partialScopes partial) key
          }

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

encodeResult :: StdoutEncode -> Value -> Text
encodeResult enc v = case enc of
  EncodeJson -> encodeValue v
  EncodePrettyJson -> encodeValuePretty v
  EncodeText -> renderValueText v

-- | Uncaught failure: report to stderr (with the collected stack
-- trace, spec 12.3) and exit with the error value's code, normalized
-- to 1..255 (spec 8.10, 11.3).
failureExit :: CommonOpts -> TraceId -> LaskFailure -> IO a
failureExit opts traceId uncaught = do
  -- The message of a failed command is its stderr, so a secret it
  -- printed is in the failure as much as in the relayed lines (12.8).
  lf <- maskFailure uncaught
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

-- secrets ----------------------------------------------------------------------

-- | @lask secrets list@ \/ @check@ (spec 11.10). Without a function,
-- every variable; with one, the variables it reads by name, and every
-- variable when a name it reads is computed. The module is compiled
-- only when a function is named, so the command also works outside a
-- project.
cmdSecrets :: Bool -> SecretsOpts -> IO ()
cmdSecrets isCheck o = do
  let opts = secretsCommon o
  env <- Map.fromList <$> getEnvironment
  scope <- case secretsFunction o of
    Nothing -> pure AllVariables
    Just fn -> do
      compiled <- compileOrExit opts
      case publicDecl compiled (kebabToSnake fn) of
        Nothing -> usageError opts ("no such function: '" <> fn <> "'")
        Just (key, _) ->
          pure (maybe AllVariables (OnlyVariables fn) (collectEnvReadsFrom (compiledCore compiled) key))
  if isCheck
    then secretsCheck (optJsonFormat opts) (secretsRead o) env scope
    else secretsList (optJsonFormat opts) env scope

-- Shared helpers -----------------------------------------------------------------

-- | The confirmation the project file asks for before this call
-- (spec 5, 11.2). Asked at the terminal when stdin and stderr are
-- terminals; @--confirm@ approves it instead, unless @LASK_CONFIRM=tty@
-- says that only a typed confirmation counts. Exits 4 when refused.
confirmOrExit :: CommonOpts -> Bool -> Compiled -> Text -> (FilePath, Text) -> CoreDecl -> [Value] -> [(Text, Value)] -> IO ()
confirmOrExit opts approved compiled fnName key cd posVals kwVals = do
  mode <- lookupEnv "LASK_CONFIRM"
  ttyOnly <- case mode of
    Nothing -> pure False
    Just "" -> pure False
    Just "tty" -> pure True
    Just other -> usageError opts ("LASK_CONFIRM must be 'tty', not '" <> T.pack other <> "'")
  when (approved && ttyOnly) $
    usageError opts "--confirm is not accepted while LASK_CONFIRM=tty; confirm at the terminal"
  let prompt = do
        (_, rule) <- ruleFor (compiledProgram compiled) (compiledScopes compiled) key
        confirmationFor fnName rule cd posVals kwVals
  case prompt of
    Nothing -> pure ()
    Just _ | approved -> pure ()
    Just p -> do
      terminal <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stderr
      unless terminal . notConfirmed p $
        "there is no terminal to confirm at; pass --confirm to approve"
          <> (if ttyOnly then " (not accepted while LASK_CONFIRM=tty)" else "")
      TIO.hPutStr stderr $
        promptFunction p
          <> " will run"
          <> (if null (promptMatched p) then "" else " with " <> T.intercalate ", " (promptMatched p))
          <> ".\nType '"
          <> promptPhrase p
          <> "' to continue: "
      hFlush stderr
      typed <- try TIO.getLine :: IO (Either IOException Text)
      case typed of
        Right t | T.strip t == promptPhrase p -> pure ()
        Right _ -> notConfirmed p "what was typed does not match"
        Left _ -> notConfirmed p "the input ended"
  where
    notConfirmed p why = do
      let msg = "'" <> promptFunction p <> "' was not confirmed (expected '" <> promptPhrase p <> "'): " <> why
      if optJsonFormat opts
        then
          TIO.hPutStrLn stderr . TE.decodeUtf8 . BL.toStrict . A.encode $
            A.object [("code", A.String (codeText ECliNotConfirmed)), ("message", A.String msg)]
        else TIO.hPutStrLn stderr (codeText ECliNotConfirmed <> ": " <> msg)
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

-- | An advisory in the JSON form of 14.3, marked by @severity@ (14.2).
advisoryJson :: Advisory -> A.Value
advisoryJson a =
  A.object $
    [ ("code", A.String (advisoryText (advCode a))),
      ("severity", "warning"),
      ("stage", "static"),
      ("message", A.String (advMessage a))
    ]
      <> case advSpan a of
        Span (Position file l c) _ ->
          [ ( "location",
              A.object
                [ ("file", A.String (T.pack file)),
                  ("line", A.Number (fromIntegral l)),
                  ("column", A.Number (fromIntegral c))
                ]
            )
          ]
        NoSpan -> []

-- | @lask cmd@ (spec 11.8): run a declared command in its declared
-- environment, as an argument vector rather than through a shell.
cmdCmd :: CmdOpts -> IO ()
cmdCmd cmdOpts
  | cmdShowHelp cmdOpts = cmdCmdHelp (cmdCommon cmdOpts)
  | otherwise = do
      let opts = cmdCommon cmdOpts
      compiled <- compileOrExit opts
      let core = compiledCore compiled
          table = Map.findWithDefault Map.empty (cpEntry core) (cpCommands core)
      case cmdName cmdOpts of
        Nothing -> usageError opts "no command given; try 'lask cmd --help'"
        Just name -> case Map.lookup name table of
          Nothing ->
            usageError opts $
              "'" <> name <> "' is not a command of this module; try 'lask cmd --help'"
          Just envCore -> do
            traceId <- maybe newTraceId pure (optTraceId opts)
            secrets <- newSecretResolver
            envValue <- evalCommandEnv (readEnvVar secrets) core envCore >>= either (failureExit opts traceId) pure
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

-- | @lask cmd --help@: the option help, then every command word of
-- the entry module with the state of the image it needs, as @lask run
-- --help@ lists its functions (spec 11.6, 11.8). The option help is
-- always printed; a module that does not compile only loses the list.
cmdCmdHelp :: CommonOpts -> IO ()
cmdCmdHelp opts = do
  putStrLn cmdOptionsHelp
  r <- compileFile (optModule opts)
  case r of
    Left ds -> TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) ds)
    Right compiled -> do
      let core = compiledCore compiled
          table = Map.findWithDefault Map.empty (cpEntry core) (cpCommands core)
      rows <- commandRows core table
      case (optJsonFormat opts, rows) of
        (True, _) -> TIO.putStrLn (commandRowsJson rows)
        (False, []) -> pure ()
        (False, _) ->
          TIO.putStrLn . T.intercalate "\n" $
            ("\nCommands in " <> T.pack (optModule opts) <> ":")
              : map ("  " <>) (commandRowsText rows)
  exitSuccess

-- | A command's name, the kind and target of its environment, and
-- whether that image is present (spec 11.8).
type CommandRow = (Text, Text, Text, Bool)

commandRowsJson :: [CommandRow] -> Text
commandRowsJson rows =
  TE.decodeUtf8 . BL.toStrict . A.encode $
    [ A.object
        [ (AK.fromText "name", A.String name),
          (AK.fromText "kind", A.String kind),
          (AK.fromText "target", A.String target),
          (AK.fromText "present", A.Bool present)
        ]
    | (name, kind, target, present) <- rows
    ]

commandRowsText :: [CommandRow] -> [Text]
commandRowsText rows =
  [ T.justifyLeft width ' ' name
      <> "  "
      <> T.justifyLeft 6 ' ' kind
      <> "  "
      <> target
      <> (if present then "" else "  MISSING (lask sync)")
  | (name, kind, target, present) <- rows
  ]
  where
    width = maximum (8 : [T.length n | (n, _, _, _) <- rows])

-- | No network access and no build (spec 11.8). An environment that
-- cannot be evaluated is a row with the failure rather than the end of
-- the list.
commandRows :: CoreProgram -> Map.Map Text Core -> IO [CommandRow]
commandRows core table = do
  pins <- loadPins core
  mapM (row pins) (Map.toList table)
  where
    row pins (name, envCore) = do
      r <- evalCommandEnv readEnvVarUnresolved core envCore
      case r >>= resolveEnv of
        Left lf -> pure (name, "?", "<" <> failureMessage lf <> ">", False)
        Right resolved -> do
          present <- imagePresent pins (cpBaseDir core) resolved
          let (kind, target) = describeResolved resolved
          pure (name, kind, target, present)

    describeResolved resolved = case resolved of
      ResolvedLocal -> ("local", "local")
      ResolvedDocker image _ -> ("docker", image)
      ResolvedRecipe r _ -> ("docker", "recipe " <> recipeSource (rcDockerfile r))

-- | Whether the image a command needs is on the target daemon. No
-- network access and no build (spec 11.8, 10.3).
imagePresent :: ImagePins -> FilePath -> ResolvedEnv -> IO Bool
imagePresent pins baseDir resolved = case resolved of
  ResolvedLocal -> pure True
  ResolvedDocker ref _ -> either (const False) (const True) <$> resolveRegistry pins ref
  ResolvedRecipe r _ -> do
    tagE <- recipeTag baseDir r
    either (const (pure False)) imageExists tagE

-- | Evaluate a command's environment (spec 11.8). The environment of a
-- command declaration can reach no effect (ch. 5), so nothing here can
-- run a command, touch a file or read the standard input, which
-- belongs to the program. The hooks refuse rather than run anything if
-- that guarantee is ever broken. It may read the environment, through
-- the reader given (spec 9.8).
evalCommandEnv :: (Text -> IO (Maybe Text)) -> CoreProgram -> Core -> IO (Either LaskFailure EnvValue)
evalCommandEnv readEnv core c = do
  ctx <- mkRtCtx core "" (RtHooks refuseCommand refuseFile (const (pure ())) noAsyncTracker readEnv)
  r <- try (evalCore ctx Map.empty c)
  pure $ case r of
    Left lf -> Left lf
    Right (VEnv ev) -> Right ev
    Right _ -> Left (refusal "the command's environment did not evaluate to an Environment")
  where
    refuseCommand _ _ = pure (Left (refusal "the environment of a command declaration tried to run a command"))
    refuseFile _ _ = pure (Left (refusal "the environment of a command declaration tried to access a file"))
    refusal = ioFailure EIoEnvResolve
