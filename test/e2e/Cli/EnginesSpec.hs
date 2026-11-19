{-# LANGUAGE OverloadedStrings #-}

-- | The container engines lask drives (spec 10.10).
module Cli.EnginesSpec (spec) where

import Data.List (isInfixOf, isPrefixOf)
import Support.Harness
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  -- The same program on podman or nerdctl, through their clients
  -- (spec 10.10).
  describe "container engines (spec 10.10)" $ do
    let proj =
          [ ( "main.lask",
              "command { \"cat\" } on #alpine:3.22.2\n\
              \hi(): String = $ cat x\n"
            )
          ]
        engineIs e extra = ("LASK_CONTAINER_ENGINE", e) : [kv | kv@(k, _) <- extra, k /= "LASK_CONTAINER_ENGINE"]

    -- Podman reads a short name through its own registry configuration,
    -- which may not name Docker Hub, so the reference a program writes
    -- is pulled as Docker reads it.
    it "pulls a short name on podman as the Docker Hub name, and runs the pinned image" $ \lask ->
      withFakeEngine "podman" $ \state extra -> withProject proj $ \dir -> do
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 0
        resOut b `shouldContain` "#alpine:3.22.2  registry  sha256:aaa  main.lask: command cat  pulled"
        cs <- calls state
        [c | c <- cs, "pull " `isPrefixOf` c]
          `shouldBe` ["pull docker.io/library/alpine:3.22.2", "pull docker.io/library/alpine@sha256:aaa"]
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 0
        runs <- filter ("run " `isPrefixOf`) <$> calls state
        runs `shouldSatisfy` (not . null)
        runs `shouldSatisfy` all ("alpine@sha256:aaa" `isInfixOf`)

    it "asks podman and nerdctl for the engine's information to tell whether it answers" $ \lask ->
      mapM_
        ( \(engine, daemon) -> withFakeEngine engine $ \state extra -> withProject proj $ \dir -> do
            _ <- runLaskEnv lask dir extra ["sync"] ""
            writeFile (state </> "calls") ""
            r <- runLaskEnv lask dir extra ["envs", "list"] ""
            resExit r `shouldBe` 0
            cs <- calls state
            [c | c <- cs, not ("image inspect" `isPrefixOf` c)] `shouldSatisfy` all ("info " `isPrefixOf`)
            writeFile (state </> "daemon-down") ""
            down <- runLaskEnv lask dir extra ["envs", "list"] ""
            resErr down `shouldContain` ("warning: cannot reach " <> daemon)
        )
        [("podman", "Podman"), ("nerdctl", "containerd (nerdctl)")]

    -- nerdctl exits 1 when it cannot run the container, as a command may,
    -- and says why on a fatal log line.
    it "tells nerdctl's own failure from the command's exit code" $ \lask ->
      withFakeEngine "nerdctl" $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "run-exit") "1"
        writeFile (state </> "run-fails") "time=\"2026-01-01T00:00:00Z\" level=fatal msg=\"failed to mount: \\\"/work\\\"\"\n"
        infra <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit infra `shouldBe` 3
        resErr infra `shouldContain` "E-IO-ENV-RESOLVE"
        resErr infra `shouldContain` "failed to mount: \"/work\""
        writeFile (state </> "run-fails") "cat: x: No such file or directory\n"
        own <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit own `shouldBe` 1
        resErr own `shouldNotContain` "E-IO-ENV-RESOLVE"

    it "takes docker's exit code 125 as its own failure" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "run-exit") "125"
        writeFile (state </> "run-fails") "docker: Error response from daemon: invalid mount config\n"
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 3
        resErr r `shouldContain` "E-IO-ENV-RESOLVE"
        resErr r `shouldContain` "invalid mount config"

    it "reports an engine LASK_CONTAINER_ENGINE does not know when a container is needed" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        r <- runLaskEnv lask dir (engineIs "lxc" extra) ["eval", "hi"] ""
        resExit r `shouldBe` 3
        resErr r `shouldContain` "E-IO-ENV-RESOLVE"
        resErr r `shouldContain` "LASK_CONTAINER_ENGINE is 'lxc', which is none of docker, podman, nerdctl"
