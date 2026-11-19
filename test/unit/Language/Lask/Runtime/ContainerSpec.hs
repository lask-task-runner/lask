{-# LANGUAGE OverloadedStrings #-}

-- | What lask asks of a container engine's client (spec 10.10), against
-- a client that records its arguments and answers as each test says
-- ("Support.FakeClient"): the operations of 'cliBackend' for each
-- engine, how a failure of the client is read, which backend
-- @LASK_CONTAINER_ENGINE@ selects, and the run options of 10.2 that
-- "Language.Lask.Runtime.EnvironmentSpec" leaves out.
module Language.Lask.Runtime.ContainerSpec (spec) where

import Control.Exception (IOException, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Runtime.Container
import Language.Lask.Runtime.Value
import Support.FakeClient
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.Process (CmdSpec (..), CreateProcess (..), readCreateProcessWithExitCode)
import Test.Hspec

-- | The arguments between the fixed prologue of a run and its image.
optionsOf :: Engine -> [(Text, Value)] -> [String]
optionsOf e ps =
  takeWhile (/= "img") . drop 1 . dropWhile (/= "/bin/sh") $
    runArgs e (RunSpec "/p" Nothing "img" (Map.fromList ps) False False "/bin/sh" [] (defaultBaseMounts "/p"))

strs :: [Text] -> Value
strs = VArray . V.fromList . map VString

-- | Pull with a backend, collecting what it reports.
pullWith :: ContainerBackend -> Maybe Text -> Text -> IO (Either Text (), [Text])
pullWith b platform ref = do
  seen <- newIORef []
  r <- cbPull b (\t -> atomicModifyIORef' seen (\xs -> (xs <> [t], ()))) platform ref
  (,) r <$> readIORef seen

buildWith :: ContainerBackend -> BuildSpec -> IO (Either Text (), [Text])
buildWith b s = do
  seen <- newIORef []
  r <- cbBuild b (\t -> atomicModifyIORef' seen (\xs -> (xs <> [t], ()))) s
  (,) r <$> readIORef seen

recipe :: BuildSpec
recipe = BuildSpec "/p/Dockerfile" "/p" "lask/abc" [("B", "2"), ("A", "1")] (Just "linux/arm64") True

spec :: Spec
spec = do
  describe "the run options of 10.2" $ do
    it "passes each resource limit as its flag" $
      optionsOf
        Docker
        [ ("memory_swap", VString "2g"),
          ("memory_reservation", VString "512m"),
          ("cpu_shares", VNumber 512),
          ("cpuset_cpus", VString "0-1"),
          ("cpuset_mems", VString "0"),
          ("shm_size", VString "64m"),
          ("blkio_weight", VNumber 300),
          ("ulimits", strs ["nofile=1024:2048", "nproc=64"])
        ]
        `shouldBe` [ "--blkio-weight", "300",
                     "--cpu-shares", "512",
                     "--cpuset-cpus", "0-1",
                     "--cpuset-mems", "0",
                     "--memory-reservation", "512m",
                     "--memory-swap", "2g",
                     "--shm-size", "64m",
                     "--ulimit", "nofile=1024:2048", "--ulimit", "nproc=64"
                   ]
    it "passes the execution context, the confinement and the network as their flags" $
      optionsOf
        Docker
        [ ("hostname", VString "box"),
          ("cap_drop", strs ["ALL"]),
          ("dns", strs ["1.1.1.1"]),
          ("dns_search", strs ["example.com"]),
          ("publish", strs ["8080:80"]),
          ("volumes", strs ["cache:/cache"])
        ]
        `shouldBe` [ "--cap-drop", "ALL",
                     "--dns", "1.1.1.1",
                     "--dns-search", "example.com",
                     "--hostname", "box",
                     "--publish", "8080:80",
                     "--volume", "cache:/cache"
                   ]
    it "writes a number as the integer or the decimal it is, never in an exponent" $ do
      optionsOf Docker [("cpus", VNumber 0.25)] `shouldBe` ["--cpus", "0.25"]
      optionsOf Docker [("pids_limit", VNumber 1e3)] `shouldBe` ["--pids-limit", "1000"]
    it "gives a list or a table that is not one nothing, and an empty one nothing" $ do
      optionsOf Docker [("tmpfs", VString "/tmp"), ("add_hosts", strs ["a"]), ("env", VString "A=1")] `shouldBe` []
      optionsOf Docker [("dns", strs []), ("add_hosts", VMap Map.empty)] `shouldBe` []
    it "leaves out the options that are not passed as flags" $
      optionsOf Docker [("base_mount", VString "/src"), ("base_access", VString "ro"), ("egress", strs ["a:1"]), ("no_such_option", VString "x")]
        `shouldBe` []
    it "gives the same options to the three clients" $
      mapM_
        (\e -> optionsOf e [("memory", VString "1g"), ("user", VString "1")] `shouldBe` ["--memory", "1g", "--user", "1"])
        [minBound .. maxBound]
    it "attaches stdin, allocates a terminal and names the container when asked" $
      take 8 (runArgs Docker (RunSpec "/p" (Just "lask-1") "img" Map.empty True True "/bin/sh" [] []))
        `shouldBe` ["run", "--rm", "--pull=never", "--name", "lask-1", "-i", "-t", "--init"]
    it "labels a run with its owner right after run, and nothing else" $ do
      labelled ["run", "--rm"] `shouldSatisfy` (\as -> take 2 as == ["run", "--label"] && last as == "--rm")
      labelled ["exec", "x"] `shouldBe` ["exec", "x"]

  describe "the mounts of the base directory (spec 10.2, 10.5)" $ do
    it "mounts a read-only path, an empty directory and a hidden file each its own way" $
      runArgs Docker (RunSpec "/p" Nothing "img" Map.empty False False "/bin/sh" [] [BaseBind "/p" "/work" True, BaseHideDir "/work/secret", BaseHideFile "/work/.env"])
        `shouldSatisfy` ( \as ->
                            all
                              (`elem` as)
                              [ "type=bind,source=/p,target=/work,readonly",
                                "type=tmpfs,destination=/work/secret",
                                "type=bind,source=/dev/null,target=/work/.env,readonly"
                              ]
                        )
    it "moves the working directory with base_mount" $
      baseMountTarget (Map.fromList [("base_mount", VString "/src")]) `shouldBe` "/src"
    it "refuses a base_mount that is not a string, a mode that is not one, and base_access of another type" $ do
      let invalid = either (\e -> case e of MountInvalid _ -> True; _ -> False) (const False)
      resolveBaseMounts "/tmp" (Map.fromList [("base_mount", VNumber 1)]) >>= (`shouldSatisfy` invalid)
      resolveBaseMounts "/tmp" (Map.fromList [("base_access", VNumber 1)]) >>= (`shouldSatisfy` invalid)
      resolveBaseMounts "/tmp" (Map.fromList [("base_access", VMap (Map.fromList [(".", VNumber 1)]))]) >>= (`shouldSatisfy` invalid)
    it "takes null for base_mount and base_access as not given" $
      resolveBaseMounts "/tmp" (Map.fromList [("base_mount", VNull), ("base_access", VNull)])
        `shouldReturn` Right [BaseBind "/tmp" "/work" False]

  describe "the variables of env (spec 10.2)" $ do
    it "refuses, when it is launched, a variable the client reads that a file cannot hold" $
      withClient "docker" "exit 0" $ \_ -> do
        let opts n v = Map.fromList [("env", VMap (Map.fromList [(n, VString v)]))]
            spec' o = RunSpec "/p" Nothing "img" o False False "/bin/sh" [] []
        r <- try (cbRunProcess (cliBackend Docker) (spec' (opts "DOCKER_HOST" "a\nb")))
        either (\e -> show (e :: IOException) `shouldSatisfy` isInfixOf "DOCKER_HOST") (const (expectationFailure "launched")) r
    it "writes the file of variables the client reads itself, and removes it with the run" $
      withClient "docker" "exit 0" $ \_ -> do
        let opts = Map.fromList [("env", VMap (Map.fromList [("DOCKER_X", VString "1"), ("TOKEN", VString "t")]))]
        (cp, cleanup) <- cbRunProcess (cliBackend Docker) (RunSpec "/p" Nothing "img" opts False False "/bin/sh" [] [])
        let file = [f | ("--env-file", f) <- pairs (argsOf cp)]
        length file `shouldBe` 1
        mapM doesFileExist file `shouldReturn` [True]
        fmap (lookup "TOKEN") (env cp) `shouldBe` Just (Just "t")
        cleanup
        mapM doesFileExist file `shouldReturn` [False]

  describe "the operations of the client (spec 10.10)" $ do
    it "asks each engine whether it answers in the way its client can" $
      mapM_
        ( \(e, args) -> withClient (T.unpack (engineName e)) "echo 1" $ \dir -> do
            cbReachable (cliBackend e) `shouldReturn` Right ()
            clientCalls dir `shouldReturn` [args]
        )
        [ (Docker, "version --format {{.Server.Version}}"),
          (Podman, "info --format {{.Host.Arch}}"),
          (Nerdctl, "info --format {{.ServerVersion}}")
        ]
    it "reads why the client failed: its first line, nerdctl's fatal message, or that it failed" $ do
      withClient "docker" "echo 'Cannot connect to the Docker daemon' >&2; echo second >&2; exit 1" $ \_ ->
        cbReachable (cliBackend Docker) `shouldReturn` Left "Cannot connect to the Docker daemon"
      withClient "nerdctl" "echo 'time=\"x\" level=fatal msg=\"cannot access containerd socket\"' >&2; exit 1" $ \_ ->
        cbReachable (cliBackend Nerdctl) `shouldReturn` Left "cannot access containerd socket"
      withClient "podman" "exit 3" $ \_ ->
        cbReachable (cliBackend Podman) `shouldReturn` Left "podman failed"
    it "says it cannot run a client that is not installed" $
      withClient "other" "exit 0" $ \_ -> withEnvVars [("PATH", Just "/nonexistent")] $ do
        r <- cbReachable (cliBackend Docker)
        either (T.isPrefixOf "cannot run docker") (const False) r `shouldBe` True
    it "finds an image in the store by inspecting it, and nerdctl's in its own form" $ do
      withClient "docker" "[ \"$3\" = present ] || exit 1" $ \dir -> do
        cbImageExists (cliBackend Docker) "present" `shouldReturn` True
        cbImageExists (cliBackend Docker) "absent" `shouldReturn` False
        clientCalls dir `shouldReturn` ["image inspect present", "image inspect absent"]
      withClient "nerdctl" "case \"$*\" in *present*) echo 'docker.io/library/present:1 sha256:abc';; esac" $ \_ -> do
        cbImageExists (cliBackend Nerdctl) "present" `shouldReturn` True
        cbImageExists (cliBackend Nerdctl) "absent" `shouldReturn` False
    it "reads the repository digests as JSON, null as none, and nerdctl's from the names it lists" $ do
      withClient "docker" "echo '[\"alpine@sha256:aaa\"]'" $ \_ ->
        cbRepoDigests (cliBackend Docker) "alpine" `shouldReturn` Right ["alpine@sha256:aaa"]
      withClient "podman" "echo null" $ \_ ->
        cbRepoDigests (cliBackend Podman) "alpine" `shouldReturn` Right []
      withClient "docker" "echo garbage" $ \_ ->
        cbRepoDigests (cliBackend Docker) "alpine" `shouldReturn` Left "unreadable repository digests for 'alpine'"
      withClient "nerdctl" "printf 'localhost:5000/app:1 sha256:b\\ndocker.io/library/alpine:3.20@sha256:c sha256:c\\nnodigest\\n'" $ \_ ->
        cbRepoDigests (cliBackend Nerdctl) "x" `shouldReturn` Right ["localhost:5000/app@sha256:b", "docker.io/library/alpine@sha256:c"]
    it "reports docker's layers as they complete" $
      withClient
        "docker"
        "printf '3.20: Pulling from library/alpine\\nabcdef012345: Pulling fs layer\\n0123456789ab: Already exists\\nabcdef012345: Pull complete\\nDigest: sha256:x\\n'"
        $ \dir -> do
          r <- pullWith (cliBackend Docker) (Just "linux/amd64") "alpine:3.20"
          r `shouldBe` (Right (), ["layers 0/1", "layers 1/2", "layers 2/2"])
          clientCalls dir `shouldReturn` ["pull --platform linux/amd64 alpine:3.20"]
    it "reports the failure of a pull as the end of what the client wrote" $
      withClient "docker" "echo 'Error response from daemon: manifest unknown' >&2; exit 1" $ \_ ->
        fst <$> pullWith (cliBackend Docker) Nothing "nope:1" `shouldReturn` Left "Error response from daemon: manifest unknown"
    it "pulls a short name with podman by the name Docker reads it as" $
      withClient "podman" "exit 0" $ \dir -> do
        mapM_ (pullWith (cliBackend Podman) Nothing) ["alpine:3.20", "library/alpine", "ghcr.io/o/app:1", "localhost/app", "localhost:5000/app", "registry.example.com"]
        clientCalls dir
          `shouldReturn` [ "pull docker.io/library/alpine:3.20",
                           "pull docker.io/library/alpine",
                           "pull ghcr.io/o/app:1",
                           "pull localhost/app",
                           "pull localhost:5000/app",
                           "pull docker.io/library/registry.example.com"
                         ]
    it "says it cannot run a client that cannot pull or build" $
      withClient "other" "exit 0" $ \_ -> withEnvVars [("PATH", Just "/nonexistent")] $ do
        (r, _) <- pullWith (cliBackend Docker) Nothing "alpine"
        either (T.isPrefixOf "cannot run docker pull") (const False) r `shouldBe` True
        (b, _) <- buildWith (cliBackend Docker) recipe
        either (T.isPrefixOf "cannot run docker build") (const False) b `shouldBe` True
    it "builds a recipe with BuildKit's plain progress, reporting each step" $
      withClient "docker" "echo '#1 [internal] load build definition' >&2; echo '#7 [2/3] RUN apk add make' >&2; echo '#8 DONE 0.1s' >&2" $ \dir -> do
        r <- buildWith (cliBackend Docker) recipe
        r `shouldBe` (Right (), ["[internal] load build definition", "[2/3] RUN apk add make"])
        clientCalls dir
          `shouldReturn` ["build --progress=plain -f /p/Dockerfile -t lask/abc --platform linux/arm64 --network none --build-arg A=1 --build-arg B=2 /p"]
    it "builds a recipe with podman, reading its STEP lines" $
      withClient "podman" "echo 'STEP 1/2: FROM alpine'; echo 'STEP 2/2: RUN true'; echo '--> abc'" $ \dir -> do
        r <- buildWith (cliBackend Podman) recipe {bsPlatform = Nothing, bsOffline = False, bsArgs = []}
        r `shouldBe` (Right (), ["[1/2] FROM alpine", "[2/2] RUN true"])
        clientCalls dir `shouldReturn` ["build -f /p/Dockerfile -t lask/abc /p"]
    it "stops a container and then removes it" $
      withClient "docker" "exit 0" $ \dir -> do
        cbStop (cliBackend Docker) 3 "lask-9"
        clientCalls dir `shouldReturn` ["stop -t 3 lask-9", "rm --force lask-9"]

  describe "telling the engine's failure from the command's (spec 10.10)" $ do
    it "takes 125 as docker's and podman's own failure, and any other code as the command's" $ do
      cbRunFailure (cliBackend Docker) 125 "Unable to find image\n" `shouldBe` Just "Unable to find image"
      cbRunFailure (cliBackend Podman) 125 "x" `shouldBe` Just "x"
      cbRunFailure (cliBackend Docker) 1 "x" `shouldBe` Nothing
    it "takes nerdctl's last fatal line as its own failure, and any other exit as the command's" $ do
      cbRunFailure (cliBackend Nerdctl) 1 "output\ntime=\"t\" level=fatal msg=\"image not available: \\\"x\\\"\"\n" `shouldBe` Just "image not available: \"x\""
      cbRunFailure (cliBackend Nerdctl) 1 "plain error\n" `shouldBe` Nothing
      cbRunFailure (cliBackend Nerdctl) 0 "time=\"t\" level=fatal msg=\"x\"" `shouldBe` Nothing
    it "keeps a fatal message that is not a JSON string as written" $
      cbRunFailure (cliBackend Nerdctl) 1 "level=fatal msg=unquoted" `shouldBe` Just "unquoted"

  describe "choosing the engine (spec 10.10)" $ do
    it "takes the engine LASK_CONTAINER_ENGINE names, whatever its case and spaces" $
      withEnvVars [("LASK_CONTAINER_ENGINE", Just " Podman ")] $
        (cbEngine <$> resolveBackend) `shouldReturn` Podman
    it "takes the first client on PATH when none is named" $
      withClient "nerdctl" "exit 0" $ \dir -> withEnvVars [("LASK_CONTAINER_ENGINE", Nothing), ("PATH", Just (dir <> "/bin"))] $
        (cbEngine <$> resolveBackend) `shouldReturn` Nerdctl
    it "takes docker when no client is installed" $
      withEnvVars [("LASK_CONTAINER_ENGINE", Just ""), ("PATH", Just "/nonexistent")] $
        (cbEngine <$> resolveBackend) `shouldReturn` Docker
    it "answers every operation with what is wrong when it names no engine" $
      withEnvVars [("LASK_CONTAINER_ENGINE", Just "lxc")] $ do
        b <- resolveBackend
        let why = "LASK_CONTAINER_ENGINE is 'lxc', which is none of docker, podman, nerdctl"
        cbDaemon b `shouldBe` "a container engine"
        cbReachable b `shouldReturn` Left why
        cbImageExists b "x" `shouldReturn` False
        cbRepoDigests b "x" `shouldReturn` Left why
        fst <$> pullWith b Nothing "x" `shouldReturn` Left why
        fst <$> buildWith b recipe `shouldReturn` Left why
        cbClient b ["ps"] `shouldReturn` Left why
        cbRunFailure b 0 "" `shouldBe` Just why
        cbStop b 0 "x"
        (cp, _) <- cbRunProcess b (RunSpec "/p" Nothing "img" Map.empty False False "/bin/sh" [] [])
        (code, _, err) <- readCreateProcessWithExitCode cp ""
        (code, err) `shouldBe` (ExitFailure 125, T.unpack why <> "\n")
        (code', _, _) <- readCreateProcessWithExitCode (cbExecProcess b (ExecSpec "c" False Nothing [] ["true"])) ""
        code' `shouldBe` ExitFailure 125
    it "names each engine and reads its name back" $ do
      map engineName [minBound .. maxBound] `shouldBe` ["docker", "podman", "nerdctl"]
      map engineFromText ["DOCKER", "nerdctl", "lxc"] `shouldBe` [Just Docker, Just Nerdctl, Nothing]
      map (cbDaemon . cliBackend) [minBound .. maxBound] `shouldBe` ["the Docker daemon", "Podman", "containerd (nerdctl)"]
  where
    argsOf cp = case cmdspec cp of
      RawCommand _ as -> as
      ShellCommand _ -> []
    pairs as = zip as (drop 1 as)
