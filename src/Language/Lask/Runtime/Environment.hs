{-# LANGUAGE OverloadedStrings #-}

-- | Environment resolution and command launching (spec 10, 12.3).
--
-- An 'EnvValue' is resolved to a concrete runtime configuration just
-- before command launch (10.4): @env@ entries substitute their
-- definition from the environment file; @docker@\/@local@ are used
-- directly. Commands run via the host shell, the @docker@ CLI, or the
-- @ssh@ CLI (10.9; implementation choice permitted by the spec).
--
-- Child process stdout\/stderr are relayed line by line, in real
-- time, to the injected 'CommandLogSink' while also being captured
-- verbatim for the 'CommandResult' contract (spec 12.3: relaying must
-- not affect value semantics).
module Language.Lask.Runtime.Environment
  ( ResolvedEnv (..),
    resolveEnv,
    mkCommandRunner,
    runLoggedProcess,
    runDeclaredCommand,
    envLogInfo,
    dockerArgs,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import Language.Lask.Runtime.Image (imageExists, recipeTag)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import qualified Data.Map.Strict as Map
import Data.Scientific (formatScientific, isInteger)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Language.Lask.Builtins.Impl (CommandRunner)
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Secrets (maskSecrets)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (valueToJson)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsTerminalDevice, stderr, stdin, stdout)
import System.Process
  ( CreateProcess (cwd, delegate_ctlc, std_err, std_in, std_out),
    StdStream (CreatePipe, Inherit),
    createProcess,
    proc,
    shell,
    waitForProcess,
  )

data ResolvedEnv
  = ResolvedLocal
  | -- | Image and remaining options.
    ResolvedDocker Text (Map Text Value)
  | -- | Dockerfile, context, and remaining options (spec 10.2 recipe form).
    ResolvedRecipe Text Text (Map Text Value)
  deriving (Show, Eq)

-- | Resolve an environment value to a concrete configuration
-- (spec 10.4). The kinds are @local@ and @docker@; a @docker@ image is
-- either a registry reference or a recipe (10.2).
resolveEnv :: EnvValue -> Either LaskFailure ResolvedEnv
resolveEnv (EnvValue kind params) = case kind of
  "local" -> Right ResolvedLocal
  "docker" -> case (Map.lookup "image" params, Map.lookup "dockerfile" params) of
    (Just (VString img), _)
      | not (T.null img) -> Right (ResolvedDocker img (Map.delete "image" params))
    (_, Just (VString df))
      | not (T.null df) ->
          let ctx = case Map.lookup "context" params of
                Just (VString c) | not (T.null c) -> c
                _ -> T.pack (takeDirectory (T.unpack df))
           in Right (ResolvedRecipe df ctx (Map.delete "dockerfile" (Map.delete "context" params)))
    _ -> Left (ioFailure EIoEnvResolve "docker environment requires an image reference or a recipe")
  other -> Left (ioFailure EIoEnvResolve ("unknown environment kind: '" <> other <> "'"))

-- | The environment summary and 13.1 metadata JSON used by command
-- execution logs (spec 12.3). The summary follows environment
-- expression notation: @#local@, @#\<image\>@ for a registry
-- reference, and @#docker(dockerfile = ...)@ for a recipe.
envLogInfo :: EnvValue -> ResolvedEnv -> (Text, A.Value)
envLogInfo _ resolved = (summary, json)
  where
    summary = case resolved of
      ResolvedLocal -> "#local"
      ResolvedDocker img _ -> "#" <> img
      ResolvedRecipe df _ _ -> "#docker(dockerfile = \"" <> df <> "\")"
    resolvedEnvValue = case resolved of
      ResolvedLocal -> EnvValue "local" Map.empty
      ResolvedDocker img opts -> EnvValue "docker" (Map.insert "image" (VString img) opts)
      ResolvedRecipe df _ opts -> EnvValue "docker" (Map.insert "dockerfile" (VString df) opts)
    json = valueToJson (VEnv resolvedEnvValue)

-- | Arguments for @docker run@ (spec 10.5: base directory mounted as
-- the working directory inside the container).
dockerArgs :: FilePath -> Text -> Map Text Value -> Text -> [String]
dockerArgs baseDir image opts cmd =
  ["run", "--rm", "-v", baseDir <> ":/work", "-w", "/work", "--entrypoint", "/bin/sh"]
    <> dockerOptArgs opts
    <> [T.unpack image, "-c", T.unpack cmd]

-- | Implementation-defined environment options (spec 10.2).
dockerOptArgs :: Map Text Value -> [String]
dockerOptArgs opts =
  concat
    [ case (k, v) of
        ("memory", VString m) -> ["--memory", T.unpack m]
        ("cpus", VNumber n) -> ["--cpus", formatNum n]
        _ -> []
    | (k, v) <- Map.toList opts
    ]
  where
    formatNum n
      | isInteger n = formatScientific Sci.Fixed (Just 0) n
      | otherwise = formatScientific Sci.Fixed Nothing n

-- | Arguments for @docker run@ of one program with its argument
-- vector (spec 11.8): no shell is created, so the program name becomes
-- the entrypoint and the remaining words are passed as they stand.
dockerExecArgs :: FilePath -> Text -> Map Text Value -> Bool -> Text -> [Text] -> [String]
dockerExecArgs baseDir image opts interactive prog argv =
  ["run", "--rm"]
    <> (if interactive then ["-i", "-t"] else [])
    <> ["-v", baseDir <> ":/work", "-w", "/work", "--entrypoint", T.unpack prog]
    <> dockerOptArgs opts
    <> [T.unpack image]
    <> map T.unpack argv

-- | Run one declared command with its argument vector (spec 11.8).
--
-- Unlike a command execution expression this creates no shell, so an
-- argument containing whitespace cannot be re-split.
--
-- The program's stdout is always Lask's stdout, so @lask cmd@ can be
-- piped like the program it runs (spec 11.3: its stdout belongs to the
-- program). What varies is stderr. When all three standard streams are
-- terminals it is attached directly, because a prefixed line-buffered
-- relay cannot carry a prompt, a pager, or a progress display, and
-- interactivity takes precedence over the relay; otherwise it is
-- relayed as the command execution log (12.3). The start and exit
-- lines are written either way.
runDeclaredCommand ::
  FilePath ->
  CommandLogSink ->
  -- | Force the relay even on a terminal (@--format json@).
  Bool ->
  EnvValue ->
  -- | Program name (the declared command word).
  Text ->
  -- | Its arguments, each preserved as one word.
  [Text] ->
  IO (Either LaskFailure Int)
runDeclaredCommand baseDir0 sink forceRelay envValue prog argv = do
  baseDir <- makeAbsolute baseDir0
  tty <- allTerminals
  let interactive = tty && not forceRelay
  case resolveEnv envValue of
    Left failure -> pure (Left failure)
    Right resolved -> do
      let (sm, ej) = envLogInfo envValue resolved
          rendered = T.unwords (prog : argv)
          launch cp = do
            r <- try (runAttachedProcess sink sm ej rendered interactive cp)
            pure (mapLaunchFailure r)
      case resolved of
        ResolvedLocal ->
          launch ((proc (T.unpack prog) (map T.unpack argv)) {cwd = Just baseDir})
        ResolvedDocker image opts ->
          launch (proc "docker" (dockerExecArgs baseDir image opts interactive prog argv))
        ResolvedRecipe df ctx opts -> do
          tagE <- recipeTag baseDir df ctx
          case tagE of
            Left e -> pure (Left (ioFailure EIoImageMissing e))
            Right tag -> do
              ok <- imageExists tag
              if not ok
                then
                  pure . Left . ioFailure EIoImageMissing $
                    "image for recipe '" <> df <> "' is not materialized; run 'lask env build'"
                else launch (proc "docker" (dockerExecArgs baseDir tag opts interactive prog argv))
  where
    allTerminals =
      and <$> mapM hIsTerminalDevice [stdin, stdout, stderr]
    mapLaunchFailure r = case r of
      Left e -> Left (ioFailure EIoEnvResolve ("cannot launch command: " <> T.pack (show (e :: IOException))))
      Right code -> Right code

-- | Run a process with the caller's stdin and stdout attached,
-- emitting the start and exit lines of the command execution log. When
-- not interactive, stderr is relayed line by line as @2|@ entries
-- instead of being attached (spec 11.8).
runAttachedProcess ::
  CommandLogSink ->
  Text ->
  A.Value ->
  Text ->
  -- | Attach stderr directly rather than relaying it.
  Bool ->
  CreateProcess ->
  IO Int
runAttachedProcess sink summary envJson rendered interactive cp = do
  maskedCmd <- maskSecrets rendered
  emit maskedCmd ClStart
  (_, _, mErr, ph) <-
    createProcess
      cp
        { std_in = Inherit,
          std_out = Inherit,
          std_err = if interactive then Inherit else CreatePipe,
          delegate_ctlc = True
        }
  mapM_ (relayErr maskedCmd) mErr
  exitCode <- waitForProcess ph
  let code = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  emit maskedCmd (ClExit code)
  pure code
  where
    emit maskedCmd kind = do
      now <- getCurrentTime
      sink (CommandLog now summary envJson 1 maskedCmd kind)

    relayErr maskedCmd h = go ""
      where
        go pending = do
          chunkT <- TIO.hGetChunk h
          if T.null chunkT
            then unless (T.null pending) (emitLine maskedCmd pending)
            else do
              let (ls, rest) = splitLines (pending <> chunkT)
              mapM_ (emitLine maskedCmd) ls
              go rest
    emitLine maskedCmd l = do
      now <- getCurrentTime
      masked <- maskSecrets l
      sink (CommandLog now summary envJson 1 maskedCmd (ClLine 2 masked))
    splitLines t = case T.breakOn "\n" t of
      (_, rest) | T.null rest -> ([], t)
      (l, rest) -> let (ls, r) = splitLines (T.drop 1 rest) in (T.stripEnd l : ls, r)

-- | Real command runner over the three environment families
-- (spec 10.2, 10.8): a unified result contract regardless of the
-- environment, with infrastructure failures mapped to external I\/O
-- errors, and child output relayed to the command execution log
-- (spec 12.3). Allocated in IO: it carries the execution-number
-- counter, unique within the top-level execution even across
-- concurrent commands (12.3).
mkCommandRunner :: FilePath -> CommandLogSink -> IO CommandRunner
mkCommandRunner baseDir0 sink = do
  -- The base directory is mounted into containers (spec 10.5), and a
  -- bind mount requires an absolute path.
  baseDir <- makeAbsolute baseDir0
  counter <- newIORef 0
  pure $ \envValue cmd ->
    case resolveEnv envValue of
      Left failure -> pure (Left failure)
      Right resolved -> do
        execNo <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
        let (summary, envJson) = envLogInfo envValue resolved
            run infraCode cp infraExit = do
              r <- try (runLoggedProcess sink summary envJson execNo cmd cp)
              pure $ case r of
                Left e ->
                  Left (ioFailure infraCode ("cannot launch command: " <> T.pack (show (e :: IOException))))
                Right (code, out, errOut)
                  | Just code == infraExit -> Left (ioFailure infraCode (T.strip errOut))
                  | otherwise -> Right (code, out, errOut)
        case resolved of
          ResolvedLocal ->
            run EIoEnvResolve ((shell (T.unpack cmd)) {cwd = Just baseDir}) Nothing
          ResolvedDocker image opts ->
            -- docker exit code 125 = daemon/run infrastructure error.
            run EIoEnvResolve (proc "docker" (dockerArgs baseDir image opts cmd)) (Just 125)
          ResolvedRecipe df ctx opts -> do
            -- A recipe resolves to its content-addressed tag; building
            -- is never implicit (spec 10.3).
            tagE <- recipeTag baseDir df ctx
            case tagE of
              Left e -> pure (Left (ioFailure EIoImageMissing e))
              Right tag -> do
                ok <- imageExists tag
                if not ok
                  then
                    pure . Left . ioFailure EIoImageMissing $
                      "image for recipe '" <> df <> "' is not materialized; run 'lask env build'"
                  else run EIoEnvResolve (proc "docker" (dockerArgs baseDir tag opts cmd)) (Just 125)

-- | Run a process, relaying its output line by line to the command
-- execution log (spec 12.3) while capturing both streams verbatim.
-- Always emits the @start@ and @exit \<code\>@ log entries.
runLoggedProcess ::
  CommandLogSink ->
  -- | Environment summary for log lines.
  Text ->
  -- | Environment metadata JSON (13.1).
  A.Value ->
  -- | Execution number (spec 12.3).
  Int ->
  -- | Command string (rendered on the start line).
  Text ->
  CreateProcess ->
  IO (Int, Text, Text)
runLoggedProcess sink summary envJson execNo cmd cp = do
  -- Masked against the registry as it stands now (spec 12.8), before
  -- the command runs and before any sink can retain the log record.
  maskedCmd <- maskSecrets cmd
  (mIn, mOut, mErr, ph) <-
    createProcess cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  mapM_ hClose mIn
  emit maskedCmd ClStart
  (out, errOut) <- case (mOut, mErr) of
    (Just hOut, Just hErr) ->
      concurrently (relayStream maskedCmd 1 hOut) (relayStream maskedCmd 2 hErr)
    _ -> pure ("", "")
  exitCode <- waitForProcess ph
  let code = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  emit maskedCmd (ClExit code)
  pure (code, out, errOut)
  where
    -- `cmd` (unmasked) already drove `cp` before this function was
    -- even called, so passing the masked copy here only affects what
    -- gets logged, never execution.
    emit maskedCmd kind = do
      now <- getCurrentTime
      sink (CommandLog now summary envJson execNo maskedCmd kind)

    -- Read a stream in chunks: accumulate the raw text verbatim for
    -- the CommandResult (trailing newlines and unterminated final
    -- lines preserved; never masked — the language must see the real
    -- value, spec 8.7), and relay completed lines as they arrive
    -- (masked, since only the log copy is observation data).
    relayStream :: Text -> Int -> Handle -> IO Text
    relayStream maskedCmd fd h = go [] ""
      where
        go rawAcc pending = do
          chunk <- TIO.hGetChunk h
          if T.null chunk
            then do
              unless (T.null pending) (relayLine pending)
              hClose h
              pure (T.concat (reverse rawAcc))
            else do
              let combined = pending <> chunk
                  pieces = T.splitOn "\n" combined
                  completeLines = init pieces
                  pending' = last pieces
              mapM_ relayLine completeLines
              go (chunk : rawAcc) pending'
        relayLine line = maskSecrets (stripCR line) >>= emit maskedCmd . ClLine fd
        stripCR = T.dropWhileEnd (== '\r')
