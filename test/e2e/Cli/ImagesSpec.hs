{-# LANGUAGE OverloadedStrings #-}

-- | Images: pinning them with sync, building recipes, and resolving them through the lock (spec 10.3, 10.4).
module Cli.ImagesSpec (spec) where

import Control.Monad (void)
import Data.List (isInfixOf, isPrefixOf)
import Support.Harness
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  -- Images are pinned by `lask sync`, and
  -- resolved through the lock when a command runs (spec 10.3, 10.4).
  describe "image pinning (spec 10.3, 10.4, 11.7)" $ do
    let proj =
          [ ( "main.lask",
              "command { \"cat\" } on #alpine:3.22.2\n\
              \hi(): String = $ cat x\n"
            )
          ]
        lockText dir = readFile (dir </> "lask.lock.json")

    it "refuses to run an image the lock does not pin, naming sync" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 3
        resErr r `shouldContain` "E-IO-IMAGE-MISSING"
        resErr r `shouldContain` "lask sync"

    it "pins the digest on sync, and runs the pinned image" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 0
        resOut b `shouldContain` "#alpine:3.22.2  registry  sha256:aaa  main.lask: command cat  pulled"
        lockText dir >>= (`shouldContain` "\"digest\": \"sha256:aaa\"")
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 0
        cs <- calls state
        [c | c <- cs, "run " `isPrefixOf` c] `shouldSatisfy` all ("alpine@sha256:aaa" `isInfixOf`)

    it "invokes a function of a re-exported namespace by its qualified name (spec 5, 11.2)" $ \lask ->
      withProject
        [ ("main.lask", "export * as py from \"./py.lask\"\n"),
          ("py.lask", "greet(--name: String = \"x\"): String = \"hi #{name}\"\n")
        ]
        $ \dir -> do
          r <- runLaskEnv lask dir [] ["eval", "py.greet", "--name", "lask"] ""
          resExit r `shouldBe` 0
          resOut r `shouldContain` "hi lask"
          h <- runLaskEnv lask dir [] ["run", "--help"] ""
          resOut h `shouldContain` "py.greet"

    it "runs the commands of a with_container body in one container, removed afterwards (spec 15.5)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "f(): String = with_container(#alpine:3.22.2{memory: \"1g\"}, \\(box) -> do {\n\
              \  a = $[box] first\n\
              \  b = $[box] second\n\
              \  \"#{trim(a)}, #{trim(b)}\"\n\
              \})\n"
            )
          ]
          $ \dir -> do
            void (runLaskEnv lask dir extra ["sync"] "")
            r <- runLaskEnv lask dir extra ["eval", "f"] ""
            resExit r `shouldBe` 0
            resOut r `shouldContain` "exec: first, exec: second"
            cs <- calls state
            let runs = [c | c <- cs, "run " `isPrefixOf` c]
                execs = [c | c <- cs, "exec " `isPrefixOf` c]
                name = case dropWhile (/= "--name") (words (concat runs)) of
                  _ : n : _ -> n
                  _ -> "--name missing"
            length runs `shouldBe` 1
            concat runs `shouldContain` "--memory 1g"
            length execs `shouldBe` 2
            execs `shouldSatisfy` all ((" " <> name <> " /bin/sh -c ") `isInfixOf`)
            [c | c <- cs, "rm " `isPrefixOf` c] `shouldSatisfy` any (name `isInfixOf`)

    it "probes the declared command words on sync and records them in the lock (spec 11.7)" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 0
        lockText dir >>= (`shouldContain` "\"cat\": \"present\"")
        -- An image pinned to the same content is not probed again.
        b2 <- runLaskEnv lask dir extra ["sync", "--frozen"] ""
        resExit b2 `shouldBe` 0
        cs <- calls state
        length [c | c <- cs, "command -v" `isInfixOf` c] `shouldBe` 1

    it "probes with the module's directory mounted by its absolute path, under --module sub/main.lask" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject [("sub/main.lask", "command { \"cat\" } on #alpine:3.22.2\nhi(): String = $ cat x\n")] $ \dir -> do
          b <- runLaskEnv lask dir extra ["sync", "--module", "sub/main.lask"] ""
          resExit b `shouldBe` 0
          cs <- calls state
          let probes = [c | c <- cs, "command -v" `isInfixOf` c]
          probes `shouldSatisfy` (not . null)
          probes `shouldSatisfy` all ("type=bind,source=/" `isInfixOf`)

    it "probes a declaration whose environment is made by a call, with its run options (spec ch. 5)" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "tool(--image: Environment = #alpine:3.22.2): Runnable = runnable(image, env = {\"TOOL_HOME\": \"/opt\"})\n\
              \command { \"cat\" } on tool()\n\
              \command { \"ls\" } on #local\n\
              \hi(): String = $ cat x\n"
            )
          ]
          $ \dir -> do
            b <- runLaskEnv lask dir extra ["sync"] ""
            resExit b `shouldBe` 0
            lockText dir >>= (`shouldContain` "\"cat\": \"present\"")
            lockText dir >>= (`shouldNotContain` "\"ls\"")
            cs <- calls state
            [c | c <- cs, "command -v" `isInfixOf` c] `shouldSatisfy` all ("--env TOOL_HOME" `isInfixOf`)

    it "refuses to run a declared command its image does not provide (spec ch. 5, 11.7)" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        createDirectoryIfMissing True (state </> "missing")
        writeFile (state </> "missing" </> "cat") ""
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 3
        resErr b `shouldContain` "E-IO-COMMAND-MISSING"
        lockText dir >>= (`shouldContain` "\"cat\": \"missing\"")
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 3
        resErr r `shouldContain` "E-IO-COMMAND-MISSING"
        c <- runLaskEnv lask dir extra ["cmd", "cat", "x"] ""
        resExit c `shouldBe` 3
        cs <- calls state
        [x | x <- cs, "run " `isPrefixOf` x, not ("command -v" `isInfixOf` x)] `shouldBe` []

    -- A command line is visible to every user of the host (spec 10.2).
    it "passes env values to the container without putting them on the command line" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "command { \"cat\" } on #alpine:3.22.2{env: {\"TOKEN\": \"s3cr3t\", \"EMPTY\": \"\", \"DOCKER_HOST\": \"tcp://x\"}}\n\
              \hi(): String = $ cat x\n"
            )
          ]
          $ \dir -> do
            _ <- runLaskEnv lask dir extra ["sync"] ""
            -- Only the command's own run, not the probe of sync (11.7).
            writeFile (state </> "calls") ""
            r <- runLaskEnv lask dir extra ["eval", "hi"] ""
            resExit r `shouldBe` 0
            runs <- filter ("run " `isPrefixOf`) <$> calls state
            runs `shouldSatisfy` all (not . isInfixOf "s3cr3t")
            runs `shouldSatisfy` all (isInfixOf "--env TOKEN ")
            -- docker reads DOCKER_HOST itself, so it is given in a file.
            runs `shouldSatisfy` all (not . isInfixOf "tcp://x")
            runs `shouldSatisfy` all (isInfixOf "--env-file ")
            seen <- lines <$> readFile (state </> "env")
            seen `shouldSatisfy` elem "TOKEN=s3cr3t"
            seen `shouldSatisfy` elem "EMPTY="
            seen `shouldSatisfy` elem "DOCKER_HOST=tcp://x"

    it "keeps the pinned image when the tag moves upstream" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "registry" </> "alpine_3.22.2") "sha256:bbb"
        removeDirectoryRecursive (state </> "present")
        writeFile (state </> "calls") ""
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 0
        cs <- calls state
        [c | c <- cs, "pull " `isPrefixOf` c] `shouldBe` ["pull alpine@sha256:aaa"]
        lockText dir >>= (`shouldContain` "sha256:aaa")

    it "reports E-IO-IMAGE-DIGEST when the pinned image carries another digest" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "present" </> "alpine_sha256_aaa") "[\"alpine@sha256:ccc\"]"
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 3
        resErr b `shouldContain` "E-IO-IMAGE-DIGEST"
        lockText dir >>= (`shouldContain` "sha256:aaa")

    -- A failed sync must not cost the pins that still work.
    it "leaves the lock as it was when an image fails to materialize" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        pinned <- lockText dir
        length pinned `seq` writeFile (dir </> "main.lask") "command { \"cat\" } on #alpine:0.0.404\nhi(): String = $ cat x\n"
        b <- runLaskEnv lask dir extra ["sync"] ""
        resExit b `shouldBe` 3
        resOut b `shouldContain` "lask.lock.json unchanged"
        lockText dir `shouldReturn` pinned
        mapM_ (\(name, content) -> writeFile (dir </> name) content) proj
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 0

    -- `echo hello | lask cmd cat` printed nothing: -i was given only
    -- when every stream was a terminal, so a piped stdin never reached
    -- the container (spec 11.8).
    it "attaches stdin to a containerized cmd even when it is not a terminal" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "calls") ""
        r <- runLaskEnv lask dir extra ["cmd", "cat"] "hello\n"
        resExit r `shouldBe` 0
        runs <- filter ("run " `isPrefixOf`) <$> calls state
        runs `shouldSatisfy` any (isInfixOf "--entrypoint cat")
        runs `shouldSatisfy` all (\c -> " -i " `isInfixOf` c && not (" -t " `isInfixOf` c))

    -- A run never pulls, and keeps the daemon from pulling on its
    -- behalf (spec 10.3).
    it "runs with --pull=never" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "calls") ""
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 0
        cs <- calls state
        [c | c <- cs, "pull " `isPrefixOf` c] `shouldBe` []
        [c | c <- cs, "run " `isPrefixOf` c] `shouldSatisfy` all ("--pull=never" `isInfixOf`)

    -- The images a program requires are the heads reachable from its
    -- entry module (spec 10.3): a function's default image counts, and
    -- one in a declaration nothing references does not. The registry
    -- of the fake docker has no busybox, so pulling it would fail.
    it "materializes only the images reachable from the entry module" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ("main.lask", "import { tool } from \"./lib.lask\"\nhi(): String = $[tool()] cat x\n"),
            ( "lib.lask",
              "tool(--image: Environment = #alpine:3.22.2): Runnable = runnable(image, memory = \"1g\")\n\
              \unused(): Environment = #busybox:1.37\n"
            )
          ]
          $ \dir -> do
            b <- runLaskEnv lask dir extra ["sync"] ""
            resExit b `shouldBe` 0
            lockText dir >>= (`shouldContain` "alpine:3.22.2")
            lockText dir >>= (`shouldNotContain` "busybox")
            cs <- calls state
            cs `shouldSatisfy` all (not . isInfixOf "busybox")
            r <- runLaskEnv lask dir extra ["eval", "hi"] ""
            resExit r `shouldBe` 0

    -- One reference used for two platforms is one lock entry, pulled
    -- for each by the pinned digest (spec 10.3).
    it "pulls a reference for each platform it is used with" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "a(): String = $[#alpine:3.22.2] cat x\n\
              \b(): String = $[#alpine:3.22.2(platform = \"linux/amd64\")] cat x\n"
            )
          ]
          $ \dir -> do
            b <- runLaskEnv lask dir extra ["sync"] ""
            resExit b `shouldBe` 0
            cs <- calls state
            [c | c <- cs, "pull " `isPrefixOf` c]
              `shouldBe` [ "pull alpine:3.22.2",
                           "pull alpine@sha256:aaa",
                           "pull --platform linux/amd64 alpine@sha256:aaa"
                         ]
            r <- runLaskEnv lask dir extra ["eval", "b"] ""
            resExit r `shouldBe` 0
            runs <- filter ("run " `isPrefixOf`) <$> calls state
            runs `shouldSatisfy` any ("--platform linux/amd64" `isInfixOf`)

    it "--frozen refuses a lock that would change" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        f <- runLaskEnv lask dir extra ["sync", "--frozen"] ""
        resExit f `shouldBe` 1
        doesFileExist (dir </> "lask.lock.json") `shouldReturn` False
        s' <- runLaskEnv lask dir extra ["sync"] ""
        resExit s' `shouldBe` 0
        lockText dir >>= (`shouldContain` "sha256:aaa")
        f2 <- runLaskEnv lask dir extra ["sync", "--frozen"] ""
        resExit f2 `shouldBe` 0

    it "drops the entry of an image nothing references any more" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (dir </> "main.lask") "hi(): String = $[#local] echo hi\n"
        _ <- runLaskEnv lask dir extra ["sync"] ""
        lockText dir >>= (`shouldNotContain` "alpine")

    -- envs list reads the lock, and asks the daemon, read only, whether
    -- each image is there (spec 11.4). It reports; it does not fail.
    it "lists what the lock resolves each environment to, what requires it, and whether it is present" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        unsynced <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit unsynced `shouldBe` 0
        resOut unsynced `shouldContain` "#alpine:3.22.2  registry  —       main.lask: command cat  not pinned (lask sync)"
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "calls") ""
        synced <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit synced `shouldBe` 0
        resOut synced `shouldContain` "#alpine:3.22.2  registry  sha256:aaa  main.lask: command cat  present"
        -- Asked once whether the daemon answers, and once per image.
        cs <- calls state
        [c | c <- cs, not ("image inspect" `isPrefixOf` c)] `shouldBe` ["version --format {{.Server.Version}}"]
        removeDirectoryRecursive (state </> "present")
        gone <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit gone `shouldBe` 0
        resOut gone `shouldContain` "missing (lask sync)"

    it "warns, and leaves the status unknown, when the daemon cannot be reached" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "daemon-down") ""
        r <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit r `shouldBe` 0
        resErr r `shouldContain` "warning: cannot reach the Docker daemon"
        resOut r `shouldContain` "sha256:aaa  main.lask: command cat  ?"

    -- A run that finds no image asks whether the daemon answers before
    -- telling to run sync, which could not help (spec 10.4).
    it "reports a daemon it cannot reach, not a missing image" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "daemon-down") ""
        r <- runLaskEnv lask dir extra ["eval", "hi"] ""
        resExit r `shouldBe` 3
        resErr r `shouldContain` "E-IO-ENV-RESOLVE"
        resErr r `shouldNotContain` "E-IO-IMAGE-MISSING"

  -- A recipe path is written relative to the module that declares it
  -- (spec 10.2), wherever the module is imported from.
  describe "recipes in an imported module (spec 10.2, 10.3)" $ do
    let proj =
          [ ("app/main.lask", "import command { \"cat\" } from \"../tools/main.lask\"\nhi(): String = $ cat /greeting\n"),
            ( "tools/main.lask",
              "greeter(): Environment = #./images/greeter/Dockerfile\nexport command { \"cat\" } on greeter()\n"
            ),
            ("tools/images/greeter/Dockerfile", "FROM scratch\n"),
            ("Dockerfile", "FROM scratch\n"),
            ("main.lask", "beside = #./Dockerfile\n")
          ]

    it "reads it from the tree of the module that declares it" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir extra ["envs", "list", "--module", "app/main.lask"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "../tools/images/greeter/Dockerfile  recipe  lask/"

    it "builds it and runs the command in it from the importing project" $ \lask ->
      withFakeDocker $ \state extra -> withProject proj $ \dir -> do
        b <- runLaskEnv lask dir extra ["sync", "--module", "app/main.lask"] ""
        resExit b `shouldBe` 0
        r <- runLaskEnv lask dir extra ["eval", "--module", "app/main.lask", "hi"] ""
        resExit r `shouldBe` 0
        cs <- calls state
        [c | c <- cs, "run " `isPrefixOf` c] `shouldSatisfy` (\rs -> not (null rs) && all ("lask/" `isInfixOf`) rs)

    it "keeps the path of a recipe beside the entry module" $ \lask ->
      withFakeDocker $ \_ extra -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir extra ["envs", "list"] ""
        resOut r `shouldContain` "./Dockerfile  recipe  lask/"
        resOut r `shouldNotContain` "../"
