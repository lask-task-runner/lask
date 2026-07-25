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
    SshSettings (..),
    defaultSshSettings,
    resolveEnv,
    mkCommandRunner,
    runLoggedProcess,
    envLogInfo,
    dockerArgs,
    sshArgs,
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
import qualified Data.Map.Strict as Map
import Data.Scientific (formatScientific, isInteger)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Language.Lask.Builtins.Impl (CommandRunner)
import Language.Lask.EnvFile (EnvEntry (..), EnvFile (..))
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
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
  | -- | Host, user, port.
    ResolvedRemote Text (Maybe Text) (Maybe Int)
  deriving (Show, Eq)

data SshSettings = SshSettings
  { sshKnownHosts :: Maybe FilePath,
    -- | @yes@ \/ @accept-new@ \/ @no@; host-key verification is on by
    -- default (spec 10.9).
    sshStrictHostKeyChecking :: Maybe Text,
    sshConnectTimeout :: Maybe Int
  }
  deriving (Show, Eq)

defaultSshSettings :: SshSettings
defaultSshSettings = SshSettings Nothing Nothing Nothing

-- | Resolve an environment value to a concrete configuration
-- (spec 10.4). @env@ kinds substitute the entry from the environment
-- file.
resolveEnv :: Maybe EnvFile -> EnvValue -> Either LaskFailure ResolvedEnv
resolveEnv mFile (EnvValue kind params) = case kind of
  "local" -> Right ResolvedLocal
  "docker" -> case Map.lookup "image" params of
    Just (VString img)
      | not (T.null img) -> Right (ResolvedDocker img (Map.delete "image" params))
    _ -> Left (ioFailure EIoEnvResolve "docker environment requires a non-empty image")
  "env" -> case Map.lookup "name" params of
    Just (VString name) -> case mFile of
      Nothing ->
        Left . ioFailure EIoEnvResolve $
          "environment '" <> name <> "' is not defined: no environment definition file"
      Just file -> case Map.lookup name (envFileEntries file) of
        Nothing ->
          Left (ioFailure EIoEnvResolve ("environment '" <> name <> "' is not defined"))
        Just (EnvEntry ekind eparams) ->
          resolveEnv Nothing (EnvValue ekind eparams)
    _ -> Left (ioFailure EIoEnvResolve "env environment requires a name")
  "remote" -> do
    host <- case Map.lookup "host" params of
      Just (VString h) | not (T.null h) -> Right h
      _ -> Left (ioFailure EIoEnvResolve "remote environment requires a host")
    let user = case Map.lookup "user" params of
          Just (VString u) -> Just u
          _ -> Nothing
        port = case Map.lookup "port" params of
          Just (VNumber n) -> Just (round (Sci.toRealFloat n :: Double))
          _ -> Nothing
    Right (ResolvedRemote host user port)
  other -> Left (ioFailure EIoEnvResolve ("unknown environment kind: '" <> other <> "'"))

-- | The environment summary and 13.1 metadata JSON used by command
-- execution logs (spec 12.3). The summary follows environment
-- expression notation: @#local@, @#\<image\>@ for docker, and
-- @#env("name")@ for named environments; the JSON carries the
-- resolved kind\/params plus @name@ for named environments.
envLogInfo :: EnvValue -> ResolvedEnv -> (Text, A.Value)
envLogInfo original resolved = (summary, json)
  where
    namedRef = case original of
      EnvValue "env" ps | Just (VString n) <- Map.lookup "name" ps -> Just n
      _ -> Nothing
    summary = case (namedRef, resolved) of
      (Just n, _) -> "#env(\"" <> n <> "\")"
      (_, ResolvedLocal) -> "#local"
      (_, ResolvedDocker img _) -> "#" <> img
      (_, ResolvedRemote host _ _) -> "#remote:" <> host -- unreachable: remote only via named envs
    resolvedEnvValue = case resolved of
      ResolvedLocal -> EnvValue "local" Map.empty
      ResolvedDocker img opts -> EnvValue "docker" (Map.insert "image" (VString img) opts)
      ResolvedRemote host user port ->
        EnvValue "remote" . Map.fromList $
          [("host", VString host)]
            <> maybe [] (\u -> [("user", VString u)]) user
            <> maybe [] (\p -> [("port", VNumber (fromIntegral p))]) port
    json = case valueToJson (VEnv resolvedEnvValue) of
      A.Object o -> case namedRef of
        Just n -> A.Object (KM.insert (AK.fromText "name") (A.String n) o)
        Nothing -> A.Object o
      other -> other

-- | Arguments for @docker run@ (spec 10.5: base directory mounted as
-- the working directory inside the container).
dockerArgs :: FilePath -> Text -> Map Text Value -> Text -> [String]
dockerArgs baseDir image opts cmd =
  ["run", "--rm", "-v", baseDir <> ":/work", "-w", "/work"]
    <> optArgs
    <> [T.unpack image, "/bin/sh", "-c", T.unpack cmd]
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

-- | Arguments for non-interactive @ssh@ (spec 10.9): host-key
-- verification on by default, credentials never passed on the
-- command line.
sshArgs :: SshSettings -> Text -> Maybe Text -> Maybe Int -> Text -> [String]
sshArgs settings host user port cmd =
  ["-o", "BatchMode=yes"]
    <> ["-o", "StrictHostKeyChecking=" <> maybe "yes" T.unpack (sshStrictHostKeyChecking settings)]
    <> maybe [] (\p -> ["-o", "UserKnownHostsFile=" <> p]) (sshKnownHosts settings)
    <> maybe [] (\t -> ["-o", "ConnectTimeout=" <> show t]) (sshConnectTimeout settings)
    <> maybe [] (\p -> ["-p", show p]) port
    <> [T.unpack (maybe host (\u -> u <> "@" <> host) user)]
    <> [T.unpack cmd]

-- | Real command runner over the three environment families
-- (spec 10.2, 10.8): a unified result contract regardless of the
-- environment, with infrastructure failures mapped to external I\/O
-- errors, and child output relayed to the command execution log
-- (spec 12.3). Allocated in IO: it carries the execution-number
-- counter, unique within the top-level execution even across
-- concurrent commands (12.3).
mkCommandRunner :: FilePath -> Maybe EnvFile -> SshSettings -> CommandLogSink -> IO CommandRunner
mkCommandRunner baseDir mFile settings sink = do
  counter <- newIORef 0
  pure $ \envValue cmd ->
    case resolveEnv mFile envValue of
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
          ResolvedRemote host user port ->
            -- ssh exit code 255 = connection/authentication failure.
            run EIoSshConnect (proc "ssh" (sshArgs settings host user port cmd)) (Just 255)

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
  (mIn, mOut, mErr, ph) <-
    createProcess cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  mapM_ hClose mIn
  emit ClStart
  (out, errOut) <- case (mOut, mErr) of
    (Just hOut, Just hErr) ->
      concurrently (relayStream 1 hOut) (relayStream 2 hErr)
    _ -> pure ("", "")
  exitCode <- waitForProcess ph
  let code = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  emit (ClExit code)
  pure (code, out, errOut)
  where
    emit kind = do
      now <- getCurrentTime
      sink (CommandLog now summary envJson execNo cmd kind)

    -- Read a stream in chunks: accumulate the raw text verbatim for
    -- the CommandResult (trailing newlines and unterminated final
    -- lines preserved), and relay completed lines as they arrive.
    relayStream :: Int -> Handle -> IO Text
    relayStream fd h = go [] ""
      where
        go rawAcc pending = do
          chunk <- TIO.hGetChunk h
          if T.null chunk
            then do
              unless (T.null pending) (emit (ClLine fd (stripCR pending)))
              hClose h
              pure (T.concat (reverse rawAcc))
            else do
              let combined = pending <> chunk
                  pieces = T.splitOn "\n" combined
                  completeLines = init pieces
                  pending' = last pieces
              mapM_ (emit . ClLine fd . stripCR) completeLines
              go (chunk : rawAcc) pending'
        stripCR = T.dropWhileEnd (== '\r')
