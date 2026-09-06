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
    envLogInfo,
    dockerArgs,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
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
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess (cwd, std_err, std_in, std_out),
    StdStream (CreatePipe),
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
    <> optArgs
    <> [T.unpack image, "-c", T.unpack cmd]
  where
    optArgs =
      concat
        [ case (k, v) of
            ("memory", VString m) -> ["--memory", T.unpack m]
            ("cpus", VNumber n) -> ["--cpus", formatNum n]
            _ -> []
        | (k, v) <- Map.toList opts
        ]
    formatNum n
      | isInteger n = formatScientific Sci.Fixed (Just 0) n
      | otherwise = formatScientific Sci.Fixed Nothing n

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
