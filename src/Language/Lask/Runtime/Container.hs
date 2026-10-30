{-# LANGUAGE OverloadedStrings #-}

-- | The container engine lask runs container environments on (spec
-- 10.10): what lask asks of it, and the command line clients that
-- answer.
--
-- Every image and container operation goes through a
-- 'ContainerBackend', so that nothing else in lask names an engine.
-- The backends here drive an engine's Docker-compatible client —
-- @docker@, @podman@ or @nerdctl@ — as a subprocess, and absorb where
-- the three differ: how a client asks whether its engine answers, how
-- a build reports its steps, how a short image name is read, how an
-- infrastructure failure is told from the exit code of the command,
-- and which variables configure the client itself.
module Language.Lask.Runtime.Container
  ( Engine (..),
    engineName,
    engineFromText,
    ContainerBackend (..),
    BuildSpec (..),
    RunSpec (..),
    ExecSpec (..),
    resolveBackend,
    cliBackend,
    runArgs,
    execArgs,
    clientEnv,
    containerWorkdir,
  )
where

import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Scientific as Sci
import Data.Scientific (formatScientific, isInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Language.Lask.Runtime.Value (Value (..))
import System.Directory (findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hGetLine, hIsEOF)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readCreateProcessWithExitCode, waitForProcess)

-- | The engines whose client lask drives (spec 10.10).
data Engine = Docker | Podman | Nerdctl
  deriving (Show, Eq, Enum, Bounded)

-- | The engine's name, which is also the name of its client program
-- and the value of @LASK_CONTAINER_ENGINE@ that selects it.
engineName :: Engine -> Text
engineName e = case e of
  Docker -> "docker"
  Podman -> "podman"
  Nerdctl -> "nerdctl"

engineFromText :: Text -> Maybe Engine
engineFromText t = find ((== T.toLower (T.strip t)) . engineName) [minBound .. maxBound]

-- | A recipe to build (spec 10.3): absolute paths, and the
-- content-addressed tag to give the image.
data BuildSpec = BuildSpec
  { bsFile :: FilePath,
    bsContext :: FilePath,
    bsTag :: Text,
    -- | In name order.
    bsArgs :: [(Text, Text)],
    bsPlatform :: Maybe Text
  }

-- | One container run: the base directory mounted as its working
-- directory (spec 10.5), the image, the environment's run options
-- (10.2), and the program it runs with its arguments.
data RunSpec = RunSpec
  { runBaseDir :: FilePath,
    -- | A name, for a run that may have to be stopped (8.7).
    runName :: Maybe Text,
    runImage :: Text,
    runOptions :: Map Text Value,
    -- | Keep stdin attached.
    runStdin :: Bool,
    -- | Allocate a terminal.
    runTty :: Bool,
    runEntrypoint :: Text,
    runCommand :: [Text]
  }

-- | One command run in a container already running (spec 15.5): the
-- run options were given when it was started, so only what tells this
-- command apart is given here.
data ExecSpec = ExecSpec
  { execContainer :: Text,
    -- | Keep stdin attached.
    execStdin :: Bool,
    -- | Run as this user rather than the container's.
    execUser :: Maybe Text,
    -- | Variables this command alone is given, on the command line:
    -- none of them is a value of the program.
    execEnv :: [(Text, Text)],
    execCommand :: [Text]
  }

-- | What lask asks of a container engine. A failure is a short
-- message, as the engine gave it.
data ContainerBackend = ContainerBackend
  { cbEngine :: Engine,
    -- | The engine, as a diagnostic names it: "the Docker daemon".
    cbDaemon :: Text,
    -- | Whether the engine answers; why not when it does not.
    cbReachable :: IO (Either Text ()),
    -- | Whether an image is in the engine's store.
    cbImageExists :: Text -> IO Bool,
    -- | The repository digests the engine records for an image.
    cbRepoDigests :: Text -> IO (Either Text [Text]),
    -- | Pull a registry reference, for a platform if one is given,
    -- reporting progress as it goes.
    cbPull :: (Text -> IO ()) -> Maybe Text -> Text -> IO (Either Text ()),
    -- | Build a recipe, reporting each step as it starts.
    cbBuild :: (Text -> IO ()) -> BuildSpec -> IO (Either Text ()),
    -- | The client process that runs a container.
    cbRunProcess :: RunSpec -> IO CreateProcess,
    -- | The client process that runs a command in a running container.
    cbExecProcess :: ExecSpec -> CreateProcess,
    -- | Given the exit code and stderr of a run, the engine's own
    -- failure — the container never ran — rather than the command's.
    cbRunFailure :: Int -> Text -> Maybe Text,
    -- | Stop and remove a named container, waiting this many seconds
    -- before it is killed.
    cbStop :: Int -> Text -> IO ()
  }

-- | The backend for the engine @LASK_CONTAINER_ENGINE@ names, or, when
-- it names none, the first of docker, podman and nerdctl on @PATH@
-- (spec 10.10). With none of them installed it is docker, whose
-- absence the first operation reports.
--
-- A value that names no engine leaves a backend that answers every
-- operation with that, so a program that runs nothing in a container
-- is not stopped by it.
resolveBackend :: IO ContainerBackend
resolveBackend = do
  chosen <- lookupEnv "LASK_CONTAINER_ENGINE"
  case T.strip . T.pack <$> chosen of
    Just t | not (T.null t) -> pure $ case engineFromText t of
      Just e -> cliBackend e
      Nothing ->
        unusableBackend
          ("LASK_CONTAINER_ENGINE is '" <> t <> "', which is none of " <> T.intercalate ", " (map engineName [minBound .. maxBound]))
    _ -> do
      found <- mapM (\e -> (,) e . isJust <$> findExecutable (T.unpack (engineName e))) [minBound .. maxBound]
      pure (cliBackend (maybe Docker fst (find snd found)))

-- | A backend that fails every operation with the same message.
unusableBackend :: Text -> ContainerBackend
unusableBackend msg =
  (cliBackend Docker)
    { cbDaemon = "a container engine",
      cbReachable = pure (Left msg),
      cbImageExists = const (pure False),
      cbRepoDigests = const (pure (Left msg)),
      cbPull = \_ _ _ -> pure (Left msg),
      cbBuild = \_ _ -> pure (Left msg),
      cbRunProcess = \_ -> pure (proc "sh" ["-c", "echo \"$0\" >&2; exit 125", T.unpack msg]),
      cbExecProcess = \_ -> proc "sh" ["-c", "echo \"$0\" >&2; exit 125", T.unpack msg],
      cbRunFailure = \_ _ -> Just msg,
      cbStop = \_ _ -> pure ()
    }

-- | The backend that drives an engine's client.
cliBackend :: Engine -> ContainerBackend
cliBackend engine =
  ContainerBackend
    { cbEngine = engine,
      cbDaemon = case engine of
        Docker -> "the Docker daemon"
        Podman -> "Podman"
        Nerdctl -> "containerd (nerdctl)",
      cbReachable = reachable,
      cbImageExists = \ref -> case engine of
        Nerdctl -> either (const False) (not . null) <$> nativeImages ref
        _ -> either (const False) (const True) <$> client ["image", "inspect", T.unpack ref],
      cbRepoDigests = \ref -> case engine of
        Nerdctl -> fmap (map (\(name, digest) -> repositoryOfName name <> "@" <> digest)) <$> nativeImages ref
        _ -> do
          r <- client ["image", "inspect", "--format", "{{json .RepoDigests}}", T.unpack ref]
          pure $ case r of
            Left e -> Left e
            Right out -> case A.decode (BL.fromStrict (TE.encodeUtf8 (T.strip out))) of
              Just ds -> Right ds
              -- Podman writes null for an image that has none.
              Nothing
                | T.strip out == "null" -> Right []
                | otherwise -> Left ("unreadable repository digests for '" <> ref <> "'"),
      cbPull = pull,
      cbBuild = build,
      cbRunProcess = \spec -> do
        let cp = proc program (runArgs engine spec)
        case clientEnv engine (runOptions spec) of
          [] -> pure cp
          extra -> do
            inherited <- getEnvironment
            let names = map fst extra
            pure cp {env = Just (extra <> [kv | kv@(k, _) <- inherited, k `notElem` names])},
      cbExecProcess = proc program . execArgs,
      cbRunFailure = runFailure,
      cbStop = \grace name -> do
        _ <- client ["stop", "-t", show grace, T.unpack name]
        _ <- client ["rm", "--force", T.unpack name]
        pure ()
    }
  where
    program = T.unpack (engineName engine)

    -- Run the client to completion: its stdout, or the reason it failed.
    client args = do
      r <- try (readCreateProcessWithExitCode (proc program args) "")
      pure $ case r of
        Left e -> Left ("cannot run " <> engineName engine <> ": " <> T.pack (show (e :: IOException)))
        Right (ExitSuccess, out, _) -> Right (T.pack out)
        Right (_, _, err) -> Left (clientError (T.pack err))

    -- The images in nerdctl's store a reference names, each with the
    -- digest it was pulled by. Its Docker-compatible inspection does
    -- not resolve a reference written with a digest, which is how a
    -- pinned image is named, so the store is read in nerdctl's own
    -- form, where a reference that names nothing is an empty answer.
    nativeImages ref = do
      r <- client ["image", "inspect", "--mode=native", "--format", "{{.Image.Name}} {{.Image.Target.Digest}}", T.unpack ref]
      pure $ fmap (\out -> [(name, T.strip digest) | l <- T.lines out, let (name, digest) = T.breakOn " " (T.strip l), not (T.null name), not (T.null (T.strip digest))]) r

    -- Docker asks the daemon for its version, which a client alone
    -- cannot answer. Podman's and nerdctl's version is answered by the
    -- client, so they ask for the engine's information instead.
    reachable = do
      r <- client $ case engine of
        Docker -> ["version", "--format", "{{.Server.Version}}"]
        Podman -> ["info", "--format", "{{.Host.Arch}}"]
        Nerdctl -> ["info", "--format", "{{.ServerVersion}}"]
      pure (() <$ r)

    -- Docker reads a short name as one on Docker Hub, and so does
    -- nerdctl. Podman resolves it through its own configuration, which
    -- may name other registries or refuse to choose, so a reference is
    -- pulled by the name Docker would read it as. Looked up in the
    -- store, a short name still finds the image this pulled.
    pull report platform ref = do
      let ref' = case engine of
            Podman -> dockerHubName ref
            _ -> ref
          args = ["pull"] <> maybe [] (\p -> ["--platform", T.unpack p]) platform <> [T.unpack ref']
      layers <- newIORef (Map.empty :: Map Text Bool)
      r <- streamProcess ("cannot run " <> engineName engine <> " pull") (proc program args) (pullLine layers) (pullLine layers)
      pure (() <$ r)
      where
        -- Docker reports each layer as @<id>: Pull complete@; the others
        -- report no progress lask reads.
        pullLine layers line = case (engine, T.breakOn ": " line) of
          (Docker, (layer, status))
            | T.length layer == 12,
              T.all (`elem` ("0123456789abcdef" :: String)) layer -> do
                let done = T.drop 2 status `elem` ["Pull complete", "Already exists"]
                counts <- atomicModifyIORef' layers $ \m ->
                  let m' = Map.insertWith (||) layer done m
                   in (m', (length (filter id (Map.elems m')), Map.size m'))
                report ("layers " <> T.pack (show (fst counts)) <> "/" <> T.pack (show (snd counts)))
          _ -> pure ()

    -- BuildKit (docker, nerdctl) writes its plain progress to stderr,
    -- a step being a line such as @#7 [3/6] RUN apk add ...@. Podman
    -- has no such option, and writes @STEP 3/6: RUN apk add ...@ to
    -- stdout.
    build report spec = do
      let args =
            ["build"]
              <> (if engine == Podman then [] else ["--progress=plain"])
              <> ["-f", bsFile spec, "-t", T.unpack (bsTag spec)]
              <> maybe [] (\p -> ["--platform", T.unpack p]) (bsPlatform spec)
              <> concat [["--build-arg", T.unpack (k <> "=" <> v)] | (k, v) <- sortOn fst (bsArgs spec)]
              <> [bsContext spec]
          step line = case T.breakOn " [" line of
            (n, s) | "#" `T.isPrefixOf` n, not (T.null s) -> report (T.strip s)
            _ -> case T.stripPrefix "STEP " line of
              Just rest | (n, s) <- T.breakOn ": " rest, not (T.null s) -> report ("[" <> n <> "] " <> T.drop 2 s)
              _ -> pure ()
      r <- streamProcess ("cannot run " <> engineName engine <> " build") (proc program args) step step
      pure (() <$ r)

    -- Docker and Podman exit 125 when the engine could not run the
    -- container, which no command's own exit code is taken to be.
    -- nerdctl exits 1, as a command may, so its failure is the fatal
    -- line it logs last instead.
    runFailure code err = case engine of
      Nerdctl
        | code /= 0,
          Just msg <- fatalMessage (lastLine err) ->
            Just msg
        | otherwise -> Nothing
      _
        | code == 125 -> Just (T.strip err)
        | otherwise -> Nothing

    lastLine t = case reverse (filter (not . T.null) (map T.strip (T.lines t))) of
      l : _ -> l
      [] -> ""

    clientError err = case filter (not . T.null) (map T.strip (T.lines err)) of
      l : _ -> maybe l id (fatalMessage l)
      [] -> engineName engine <> " failed"

-- | The message of a line nerdctl logs on a fatal error:
-- @time="..." level=fatal msg="image not available: \"x\""@.
fatalMessage :: Text -> Maybe Text
fatalMessage line = case T.breakOn "level=fatal msg=" line of
  (_, rest)
    | not (T.null rest) ->
        let quoted = T.drop (T.length "level=fatal msg=") rest
         in Just $ case A.decode (BL.fromStrict (TE.encodeUtf8 quoted)) of
              Just m -> m
              Nothing -> quoted
  _ -> Nothing

-- | The repository an image name is in: the name without its tag or
-- digest. The tag separator is the last @:@ of the last path segment,
-- so a registry port — @localhost:5000/app:1@ — is kept.
repositoryOfName :: Text -> Text
repositoryOfName name =
  let (dir, lastSeg) = T.breakOnEnd "/" name
      base = T.takeWhile (/= '@') lastSeg
   in dir <> case T.breakOnEnd ":" base of
        ("", _) -> base
        (pre, _) -> T.dropEnd 1 pre

-- | A reference as Docker reads it: a short name is on Docker Hub, and
-- one with a single path component is an official image there.
dockerHubName :: Text -> Text
dockerHubName ref
  | registryHost first && length parts > 1 = ref
  | length parts == 1 = "docker.io/library/" <> ref
  | otherwise = "docker.io/" <> ref
  where
    parts = T.splitOn "/" ref
    first = case parts of
      p : _ -> p
      [] -> ""
    registryHost p = T.any (`elem` (".:" :: String)) p || p == "localhost"

-- | Run a process, handing each line of its stdout and its stderr to
-- the given actions as it arrives. A failure is the tail of stderr.
streamProcess :: Text -> CreateProcess -> (Text -> IO ()) -> (Text -> IO ()) -> IO (Either Text ())
streamProcess cannot cp onOut onErr = do
  started <-
    try $
      createProcess cp {std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe}
  case started of
    Left e -> pure (Left (cannot <> ": " <> T.pack (show (e :: IOException))))
    Right (_, Just out, Just err, ph) -> do
      tailRef <- newIORef []
      let readLines h act = do
            eof <- hIsEOF h
            if eof
              then pure ()
              else do
                line <- T.pack <$> hGetLine h
                _ <- act line
                readLines h act
          keep line = do
            atomicModifyIORef' tailRef (\ls -> (take 20 (line : ls), ()))
            onErr line
      (_, code) <- concurrently (concurrently_ (readLines out onOut) (readLines err keep)) (waitForProcess ph)
      errTail <- reverse <$> readIORef tailRef
      pure $ case code of
        ExitSuccess -> Right ()
        _ -> Left (T.strip (T.unlines (map (\l -> maybe l id (fatalMessage l)) (filter (not . T.null . T.strip) errTail))))
    Right _ -> pure (Left cannot)

-- Running a container -----------------------------------------------------------

-- | The path the base directory is mounted at inside the container.
containerWorkdir :: String
containerWorkdir = "/work"

-- | The arguments of the client's @run@ (spec 10.5): the base
-- directory mounted as the working directory, an init process, the run
-- options, then the image and what it runs. The three clients take the
-- same ones.
runArgs :: Engine -> RunSpec -> [String]
runArgs engine spec =
  ["run", "--rm", "--pull=never"]
    <> maybe [] (\n -> ["--name", T.unpack n]) (runName spec)
    <> (if runStdin spec then ["-i"] else [])
    <> (if runTty spec then ["-t"] else [])
    <> initArgs (runOptions spec)
    <> workdirMountArgs (runBaseDir spec)
    <> ["--entrypoint", T.unpack (runEntrypoint spec)]
    <> optionArgs engine (runOptions spec)
    <> [T.unpack (runImage spec)]
    <> map T.unpack (runCommand spec)

-- | The arguments of the client's @exec@. The working directory, the
-- user and the variables of the container's run options are those its
-- commands start with, so they are not given again. The three clients
-- take the same ones.
execArgs :: ExecSpec -> [String]
execArgs spec =
  ["exec"]
    <> (if execStdin spec then ["-i"] else [])
    <> maybe [] (\u -> ["--user", T.unpack u]) (execUser spec)
    <> concat [["--env", T.unpack (k <> "=" <> v)] | (k, v) <- execEnv spec]
    <> [T.unpack (execContainer spec)]
    <> map T.unpack (execCommand spec)

-- | Mount the base directory as the container working directory
-- (spec 10.5).
--
-- @--mount@ rather than @-v@, because @-v@ packs source, target and
-- mode into one colon-separated field. A Windows base directory such
-- as @C:\\proj@ is then read as source @C@, target @\\proj@ and mode
-- @\/work@, and the daemon rejects it with @invalid mode: \/work@; a
-- POSIX directory whose name contains a colon breaks the same way.
-- @--mount@ names each part, so the separator never has to be guessed
-- — which is what 10.5 asks for when it requires implementations to
-- absorb path separator differences.
--
-- Two consequences are deliberate. @--mount@ refuses a source that
-- does not exist where @-v@ would silently create it, which is the
-- better failure for a base directory. And its fields are split on
-- commas, so a base directory containing one is still out of reach;
-- that is rarer than a drive letter, which every Windows path has.
workdirMountArgs :: FilePath -> [String]
workdirMountArgs baseDir =
  [ "--mount",
    "type=bind,source=" <> baseDir <> ",target=" <> containerWorkdir,
    "-w",
    containerWorkdir
  ]

-- | An init process as PID 1, unless the environment sets @init@ to
-- @false@ (spec 10.2). A program that is PID 1 itself gets no default
-- signal handlers, so it would ignore the termination request that
-- stops an abandoned command (8.7) and run out the whole grace period;
-- the init process passes the request on.
initArgs :: Map Text Value -> [String]
initArgs opts = case Map.lookup "init" opts of
  Just (VBool False) -> []
  _ -> ["--init"]

-- | Implementation-defined environment options (spec 10.2).
--
-- Options are emitted in name order, so one environment value always
-- produces the same argument vector however its arguments were
-- written. @workdir@ is emitted here rather than in
-- 'workdirMountArgs', which is why it overrides the default @-w@: the
-- later @-w@ is the one the daemon takes, and 10.5 gives an explicit
-- working directory precedence over the default.
--
-- The image reference, the recipe and its build arguments are not run
-- options and are consumed before this point.
optionArgs :: Engine -> Map Text Value -> [String]
optionArgs engine opts = concatMap emit (Map.toAscList opts)
  where
    emit (k, v) = case k of
      -- Resource limits.
      "memory" -> one "--memory" v
      "memory_swap" -> one "--memory-swap" v
      "memory_reservation" -> one "--memory-reservation" v
      "cpus" -> one "--cpus" v
      "cpu_shares" -> one "--cpu-shares" v
      "cpuset_cpus" -> one "--cpuset-cpus" v
      "cpuset_mems" -> one "--cpuset-mems" v
      "pids_limit" -> one "--pids-limit" v
      "shm_size" -> one "--shm-size" v
      "blkio_weight" -> one "--blkio-weight" v
      "ulimits" -> each "--ulimit" v
      -- Execution context.
      "workdir" -> one "-w" v
      "user" -> one "--user" v
      "env" -> envArgs v
      "platform" -> one "--platform" v
      "hostname" -> one "--hostname" v
      -- Given by 'initArgs', since it is on unless turned off.
      "init" -> []
      -- Confinement.
      "read_only" -> switch "--read-only" v
      "tmpfs" -> each "--tmpfs" v
      "cap_drop" -> each "--cap-drop" v
      -- Network.
      "network" -> one "--network" v
      "dns" -> each "--dns" v
      "dns_search" -> each "--dns-search" v
      "add_hosts" -> pairs ":" "--add-host" v
      "publish" -> each "--publish" v
      -- Host filesystem.
      "volumes" -> each "--volume" v
      _ -> []

    one flag v = maybe [] (\t -> [flag, T.unpack t]) (optionScalar v)

    each flag v = case v of
      VArray xs -> concat [[flag, T.unpack t] | Just t <- map optionScalar (V.toList xs)]
      _ -> []

    pairs sep flag v = case v of
      VMap m -> concat [[flag, T.unpack (k <> sep <> t)] | (k, Just t) <- entries m]
      _ -> []
      where
        entries m = [(k, optionScalar x) | (k, x) <- Map.toAscList m]

    -- A variable the client is given (see 'clientEnv') is only named
    -- here, so its value never reaches the process list.
    envArgs v = case v of
      VMap m ->
        concat
          [ ["--env", T.unpack (if passedByName engine k then k else k <> "=" <> t)]
          | (k, Just t) <- [(k, optionScalar x) | (k, x) <- Map.toAscList m]
          ]
      _ -> []

    -- A false switch is the daemon's default, so it is left unsaid
    -- rather than passed as @--flag=false@.
    switch flag v = case v of
      VBool True -> [flag]
      _ -> []

-- | An option value as the text its flag takes; 'Nothing' for null,
-- which leaves the option out (spec 10.2).
optionScalar :: Value -> Maybe Text
optionScalar v = case v of
  VString t -> Just t
  VNumber n -> Just (T.pack (formatNum n))
  VBool b -> Just (if b then "true" else "false")
  _ -> Nothing
  where
    formatNum n
      | isInteger n = formatScientific Sci.Fixed (Just 0) n
      | otherwise = formatScientific Sci.Fixed Nothing n

-- | The variables of the @env@ option that reach the container through
-- the environment of the client rather than its command line (spec
-- 10.2). A command line is visible to every user of the host through
-- the process list, and @env@ is where a program passes a credential
-- to a container, so @--env NAME@ is given with the value set in the
-- client's environment, from which the client takes it.
clientEnv :: Engine -> Map Text Value -> [(String, String)]
clientEnv engine opts = case Map.lookup "env" opts of
  Just (VMap m) -> [(T.unpack k, T.unpack t) | (k, x) <- Map.toAscList m, passedByName engine k, Just t <- [optionScalar x]]
  _ -> []

-- | Whether a variable is passed by name. Not one the client itself
-- reads — which engine it talks to, through which proxy, with which
-- configuration, where its rootless state lives — since setting it in
-- the client's environment would change what the client does; nor a
-- name no client could take as one, containing @=@. Those stay on the
-- command line.
passedByName :: Engine -> Text -> Bool
passedByName engine k =
  not (T.null k)
    && not (T.any (== '=') k)
    && not ("_PROXY" `T.isSuffixOf` upper)
    && upper `notElem` ["PATH", "HOME", "SSH_AUTH_SOCK"]
    && not (any (`T.isPrefixOf` upper) prefixes)
    && upper `notElem` names
  where
    upper = T.toUpper k
    (prefixes, names) = case engine of
      Docker -> (["DOCKER_"], [])
      Podman -> (["CONTAINER_", "CONTAINERS_", "PODMAN_", "BUILDAH_", "XDG_"], ["TMPDIR", "REGISTRY_AUTH_FILE"])
      Nerdctl -> (["CONTAINERD_", "NERDCTL_", "BUILDKIT_", "CNI_", "XDG_"], ["TMPDIR"])
