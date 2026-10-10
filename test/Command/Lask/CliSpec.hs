{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests against the built @lask@ binary, covering the
-- CLI examples of spec chapter 16 (source + argv + stdin ->
-- stdout\/stderr\/exit code).
module Command.Lask.CliSpec (spec) where

import Command.Lask.Complete (Opt (..), Plan (..), classify)
import Command.Lask.Harness
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Text as T
import Language.Lask.SecretStore.FakeVault (FakeVault (..), requestsTo, withFakeVault)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents)
import System.Process (CreateProcess (cwd, std_err, std_in, std_out), StdStream (CreatePipe), createProcess, proc, readCreateProcessWithExitCode, waitForProcess)
import System.Timeout (timeout)
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
            r <- runLaskEnv lask dir extra ["eval", "hi"] ""
            resExit r `shouldBe` 0
            runs <- filter ("run " `isPrefixOf`) <$> calls state
            runs `shouldSatisfy` all (not . isInfixOf "s3cr3t")
            runs `shouldSatisfy` all (isInfixOf "--env TOKEN ")
            -- docker reads DOCKER_HOST itself, so it stays an argument.
            runs `shouldSatisfy` all (isInfixOf "--env DOCKER_HOST=tcp://x")
            seen <- lines <$> readFile (state </> "env")
            seen `shouldSatisfy` elem "TOKEN=s3cr3t"
            seen `shouldSatisfy` elem "EMPTY="

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
        before <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit before `shouldBe` 0
        resOut before `shouldContain` "#alpine:3.22.2  registry  —       main.lask: command cat  not pinned (lask sync)"
        _ <- runLaskEnv lask dir extra ["sync"] ""
        writeFile (state </> "calls") ""
        after <- runLaskEnv lask dir extra ["envs", "list"] ""
        resExit after `shouldBe` 0
        resOut after `shouldContain` "#alpine:3.22.2  registry  sha256:aaa  main.lask: command cat  present"
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

  describe "retired commands (spec 11.1)" $ do
    it "names the command that replaced each" $ \lask ->
      withProject [("main.lask", "f() = $[#local] ls\n")] $ \dir ->
        sequence_
          [ do
              r <- runLask lask dir old ""
              (old, resExit r) `shouldBe` (old, 4)
              resErr r `shouldContain` ("was replaced by 'lask " <> new <> "'")
          | (old, new) <-
              [ (["env", "build"], "sync"),
                (["env", "list"], "envs list"),
                (["deps", "sync", "--frozen"], "sync"),
                (["envs"], "envs list"),
                (["envs", "f"], "envs list f"),
                (["envs", "--module", "main.lask", "f", "--check"], "envs list"),
                (["envs", "check"], "envs list"),
                (["deps", "why", "kit"], "deps graph"),
                (["deps", "diff", "kit"], "deps list"),
                (["cmd", "--list"], "cmd --help"),
                (["cmd", "--module", "main.lask", "--list"], "cmd --help")
              ]
          ]

  describe "generic functions from the CLI (spec 11.2)" $ do
    let proj =
          [ ( "main.lask",
              "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\n"
            )
          ]

    it "instantiates every unbounded type parameter at Any" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "first_or", "[1,2]", "0"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "1\n"
        r2 <- runLask lask dir ["eval", "first_or", "[]", "\"none\""] ""
        resExit r2 `shouldBe` 0
        resOut r2 `shouldBe` "\"none\"\n"

    it "shows the type parameters where it describes the declaration" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "first_or", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "first_or<T>"
        -- but not in the line the user is meant to type
        resOut r `shouldContain` "lask run first_or <xs> <fallback>"

  describe "bounded type parameters from the CLI (spec 4.2, 11.2)" $ do
    let proj =
          [ ( "main.lask",
              "largest<T: orderable>(xs: Array<T>): T = last(sort(xs))\n\
              \label<T: Number | String>(x: T): String = \"value: #{x}\"\n"
            )
          ]

    it "instantiates a named bound from the decoded arguments" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "largest", "[3,1,2]"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "3\n"
        s' <- runLask lask dir ["eval", "largest", "[\"b\",\"a\"]"] ""
        resOut s' `shouldBe` "\"b\"\n"

    it "refuses arguments outside the bound, or disagreeing on the type" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "largest", "[true,false]"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "the arguments make T Bool, which is not orderable"
        m <- runLask lask dir ["eval", "largest", "[1,\"a\"]"] ""
        resExit m `shouldBe` 4
        resErr m `shouldContain` "more than one type"

    it "decodes a type-bounded parameter against its bound" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "label", "5"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"value: 5\"\n"

    it "shows the bounds where it describes the declaration" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "--help"] ""
        resOut r `shouldContain` "largest<T: orderable>"
        resOut r `shouldContain` "label<T: Number | String>"

  -- A re-exported name is a public symbol of the module (spec 5), so
  -- the CLI reaches it as it reaches one the module declares.
  describe "re-exported functions from the CLI (spec 5, 11.2, 11.6)" $ do
    let proj =
          [ ("main.lask", "export { greet, greet as hello } from \"./lib.lask\"\nown(): String = \"own\"\n"),
            ( "lib.lask",
              "// Greets someone.\n//\n// @param name  Who to greet.\ngreet(--name: String = \"World\"): String = \"hello #{name}\"\n\ninternal secret(): String = \"s\"\n"
            )
          ]

    it "invokes a re-exported function, keyword arguments included" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "greet", "--name", "Lask"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"hello Lask\"\n"

    it "invokes it under the name a renaming re-export publishes" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "hello"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"hello World\"\n"

    it "does not reach what the module does not publish" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "secret"] ""
        resExit r `shouldBe` 4

    it "describes it from the file that declares it" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "hello", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "Greets someone."
        resOut r `shouldContain` "Who to greet."
        resOut r `shouldContain` "lask run hello"
        resOut r `shouldContain` "lib.lask"

    it "lists it among the module's functions" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "--help"] ""
        resExit r `shouldBe` 0
        mapM_ (resOut r `shouldContain`) ["greet", "hello", "own"]
        resOut r `shouldNotContain` "secret"

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

  describe "cmd (spec 11.8)" $ do
    let proj =
          [ ( "main.lask",
              "command { \"echo\", \"printf\", \"false\" } on #local\n\nhello() = $ echo hi\n"
            )
          ]

    it "runs a declared command in its declared environment" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "echo", "hello", "world"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "hello world\n"

    it "passes each argument as one word, with no shell" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "printf", "[%s]", "two words"] ""
        resOut r `shouldBe` "[two words]"

    it "relays the program's stderr with a 2| prefix" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "printf", "boom"] ""
        resOut r `shouldBe` "boom"

    it "always writes the start and exit lines of 12.3" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "echo", "hi"] ""
        resErr r `shouldSatisfy` isInfixOf "[#local:1] $ echo hi"
        resErr r `shouldSatisfy` isInfixOf "[#local:1] exit 0"

    it "passes the program's exit code through" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "false"] ""
        resExit r `shouldBe` 1

    -- printf takes its format first, so every implementation treats a
    -- following --help as an operand; echo does not, and GNU's acts on
    -- it. The point is that lask passed the token on either way.
    it "does not intercept --help after the command name" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "printf", "%s", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "--help"

    it "passes --list after the command name to the program" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "printf", "%s", "--list"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "--list"

    it "reports an unknown command as a usage error (exit 4)" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "nope"] ""
        resExit r `shouldBe` 4
        resErr r `shouldSatisfy` isInfixOf "lask cmd --help"

    it "reports a missing command name as a usage error" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd"] ""
        resExit r `shouldBe` 4

    it "lists the declared commands after the option help under --help" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldSatisfy` isInfixOf "--module"
        resOut r `shouldSatisfy` isInfixOf "Commands in main.lask:"
        resOut r `shouldSatisfy` isInfixOf "  echo      local   local"

    it "lists commands as JSON under --format json --help" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "--format", "json", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldSatisfy` isInfixOf "\"name\":\"echo\""

    it "still prints the option help under --help when the module does not compile" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\ncommand { \"echo\" } on #local\n")] $ \dir -> do
        r <- runLask lask dir ["cmd", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldSatisfy` isInfixOf "--module"
        resOut r `shouldNotSatisfy` isInfixOf "Commands in"

    it "exits 1 on a static error before running anything" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\ncommand { \"echo\" } on #local\n")] $ \dir -> do
        r <- runLask lask dir ["cmd", "echo", "hi"] ""
        resExit r `shouldBe` 1
        resOut r `shouldBe` ""

  describe "secret references (spec 9.8, 11.10)" $ do
    let proj =
          [ ( "main.lask",
              "command { \"echo\" } on #local\n\
              \show(): String = $ echo \"pw #{get_env(\"PW\")}\"\n\
              \pw(): String = get_env(\"PW\")\n\
              \deploy(--key!!: String = get_env(\"AK\")): String = key\n"
            )
          ]
        refs =
          [ ("PW", "{vault://secret/app#password}"),
            ("AK", "{vault://aws/creds/deploy#access_key}"),
            ("DB", "{vault://kv1/db#url}")
          ]
        vaultWith fv token = [("LASK_SECRETS", "vault"), ("VAULT_ADDR", fvAddr fv), ("VAULT_TOKEN", token)]

    it "resolves a reference where the program reads it, and masks the value" $ \lask ->
      withFakeVault $ \fv -> withProject proj $ \dir -> do
        e <- runLaskEnv lask dir (refs <> vaultWith fv "root") ["eval", "pw"] ""
        resExit e `shouldBe` 0
        resOut e `shouldBe` "\"v2pass\"\n"
        r <- runLaskEnv lask dir (refs <> vaultWith fv "root") ["run", "show"] ""
        resExit r `shouldBe` 0
        resErr r `shouldContain` "pw ***"
        resErr r `shouldNotContain` "v2pass"

    it "runs the same program with the value itself in the environment" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLaskEnv lask dir [("PW", "plain"), ("LASK_SECRETS", "")] ["eval", "pw"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"plain\"\n"

    it "lists references without reaching a store" $ \lask ->
      withProject proj $ \dir -> do
        let env = refs <> [("OP", "{op://vault/item/field}"), ("LASK_SECRETS", "vault"), ("VAULT_ADDR", "http://127.0.0.1:1"), ("VAULT_TOKEN", "root")]
        r <- runLaskEnv lask dir env ["secrets", "list"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "PW"
        resOut r `shouldContain` "secret/app#password"
        resOut r `shouldContain` "E-IO-SECRET-PROVIDER"
        scoped <- runLaskEnv lask dir env ["secrets", "list", "deploy"] ""
        resOut scoped `shouldContain` "AK"
        resOut scoped `shouldNotContain` "PW"

    it "checks each stage without reading a value, and exits 0 when all pass" $ \lask ->
      withFakeVault $ \fv -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir (refs <> vaultWith fv "root") ["secrets", "check"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "active, v1.20.4"
        resOut r `shouldContain` "readable, version 2 (latest) (value not read)"
        requestsTo fv "GET aws/creds/deploy" `shouldReturn` 0

    it "reads and gives back a dynamic secret with --read" $ \lask ->
      withFakeVault $ \fv -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir (refs <> vaultWith fv "root") ["secrets", "check", "--read", "deploy"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "lease revoked"
        resOut r `shouldNotContain` "AK1"
        readIORef (fvRevoked fv) `shouldReturn` ["aws/creds/deploy/L1"]

    it "exits 3 and names what failed, in text and JSON" $ \lask ->
      withFakeVault $ \fv -> withProject proj $ \dir -> do
        r <- runLaskEnv lask dir (refs <> vaultWith fv "limited") ["secrets", "check"] ""
        resExit r `shouldBe` 3
        resOut r `shouldContain` "E-IO-SECRET-AUTH"
        j <- runLaskEnv lask dir (refs <> vaultWith fv "limited") ["secrets", "check", "--format", "json"] ""
        resExit j `shouldBe` 3
        resOut j `shouldContain` "\"code\":\"E-IO-SECRET-AUTH\""
        bad <- runLaskEnv lask dir (refs <> vaultWith fv "nope") ["secrets", "check"] ""
        resExit bad `shouldBe` 3
        resOut bad `shouldContain` "not checked: the store is not available"

  describe "confirmation before a task runs (spec 5, 11.2)" $ do
    let proj =
          [ ( "main.lask",
              "deploy(--env: String = \"staging\"): String = $[#local] touch deployed-#{env} && echo #{env}\n\
              \destroy(): String = $[#local] touch destroyed && echo gone\n\
              \release(): String = deploy(env = \"prod\")\n"
            ),
            ("lask.json", "{\"confirm\": {\"deploy\": {\"when\": {\"env\": [\"prod\"]}}, \"destroy\": {}}}")
          ]

    it "refuses without a terminal, before anything runs" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "destroy"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "E-CLI-NOT-CONFIRMED"
        resErr r `shouldContain` "--confirm"
        doesFileExist (dir </> "destroyed") `shouldReturn` False

    it "runs with --confirm, and when no condition holds" $ \lask ->
      withProject proj $ \dir -> do
        confirmed <- runLask lask dir ["run", "--confirm", "destroy"] ""
        resExit confirmed `shouldBe` 0
        doesFileExist (dir </> "destroyed") `shouldReturn` True
        staging <- runLask lask dir ["run", "deploy"] ""
        resExit staging `shouldBe` 0
        prod <- runLask lask dir ["run", "deploy", "--env", "prod"] ""
        resExit prod `shouldBe` 4

    it "asks only about the function the CLI calls" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "release"] ""
        resExit r `shouldBe` 0
        doesFileExist (dir </> "deployed-prod") `shouldReturn` True

    it "accepts only a typed confirmation under LASK_CONFIRM=tty" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLaskEnv lask dir [("LASK_CONFIRM", "tty")] ["run", "--confirm", "destroy"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "E-CLI-USAGE"
        doesFileExist (dir </> "destroyed") `shouldReturn` False
        bad <- runLaskEnv lask dir [("LASK_CONFIRM", "yes")] ["run", "deploy"] ""
        resExit bad `shouldBe` 4

    it "asks at a terminal, and runs only on the phrase" $ \lask ->
      withProject proj $ \dir -> do
        typedRight <- runLaskInTerminal lask dir ["run", "deploy", "--env", "prod"] "prod"
        resOut typedRight `shouldContain` "deploy will run with env=prod."
        resOut typedRight `shouldContain` "Type 'prod' to continue"
        resExit typedRight `shouldBe` 0
        doesFileExist (dir </> "deployed-prod") `shouldReturn` True
        typedWrong <- runLaskInTerminal lask dir ["run", "destroy"] "yes"
        resExit typedWrong `shouldBe` 4
        resOut typedWrong `shouldContain` "E-CLI-NOT-CONFIRMED"
        doesFileExist (dir </> "destroyed") `shouldReturn` False

    it "shows the requirement in help" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "deploy", "--help"] ""
        resOut r `shouldContain` "Confirmation:"
        resOut r `shouldContain` "requires confirmation when env is prod"

    it "is checked by lask check, pointing into lask.json" $ \lask ->
      withProject [("main.lask", "deploy(--env: String = \"staging\"): String = env\n"), ("lask.json", "{\n  \"confirm\": {\n    \"deploi\": {}\n  }\n}")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resExit r `shouldBe` 1
        resOut r `shouldContain` "lask.json:3:5: E-MODULE-CONFIRM-TARGET"
        resOut r `shouldContain` "did you mean 'deploy'"
        typo <- withProject [("main.lask", "a(): String = \"a\"\n"), ("lask.json", "{\"confrim\": {}}")] $ \d ->
          runLask lask d ["check"] ""
        resExit typo `shouldBe` 1
        resOut typo `shouldContain` "unknown key: 'confrim'"

  describe "spec 16.1: minimal program" $ do
    it "eval prints the JSON result, run prints nothing" $ \lask ->
      withProject [("main.lask", "hello() = \"hello, lask\"\n")] $ \dir -> do
        e <- runLask lask dir ["eval", "hello"] ""
        e `shouldBe` Result 0 "\"hello, lask\"\n" ""
        r <- runLask lask dir ["run", "hello"] ""
        r `shouldBe` Result 0 "" ""

  describe "spec 16.2: arguments" $ do
    let src =
          "greet(name: String, --prefix: String = \"hello\"): String =\n\
          \  concat(prefix, concat(\", \", name))\n\
          \add(x: Number, y: Number): Number = x + y\n"
    it "binds positional and keyword arguments with auto decode" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        g <- runLask lask dir ["eval", "greet", "alice", "--prefix", "hi"] ""
        g `shouldBe` Result 0 "\"hi, alice\"\n" ""
        a <- runLask lask dir ["eval", "add", "1", "2"] ""
        a `shouldBe` Result 0 "3\n" ""
    it "reports missing positionals as usage errors (exit 4)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "add", "1"] ""
        resExit r `shouldBe` 4
    -- Issue #28: the type mismatch was reported by interpolating the
    -- `Show` output of the internal failure record, so the one fact
    -- the user needs arrived wrapped in Haskell syntax. The message
    -- is worded by the CLI rather than reused from `cast`, because
    -- the user wrote a command line and not a cast.
    it "reports an argument that does not fit its parameter type in the user's terms" $ \lask ->
      withProject [("main.lask", "f(n: Number): Number = n\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f", "abc"] ""
        resExit r `shouldBe` 4
        resErr r
          `shouldBe` "E-CLI-USAGE: argument 'abc' does not fit the parameter type: expected Number, got String\n"
    it "reports a keyword argument that does not fit its parameter type in the user's terms" $ \lask ->
      withProject [("main.lask", "g(--n: Number = 1): Number = n\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "g", "--n", "abc"] ""
        resExit r `shouldBe` 4
        resErr r
          `shouldBe` "E-CLI-USAGE: keyword argument '--n' 'abc' does not fit the parameter type: expected Number, got String\n"
    it "points at the offending field when the mismatch is nested" $ \lask ->
      withProject [("main.lask", "h(r: Record<a: String>): String = r.a\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "h", "{\"a\": 1}"] ""
        resExit r `shouldBe` 4
        resErr r
          `shouldBe` "E-CLI-USAGE: argument '{\"a\": 1}' does not fit the parameter type at a: expected String, got Number\n"
    it "arg-decode text keeps arguments as strings" $ \lask ->
      withProject [("main.lask", "id2(x: String): String = x\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--arg-decode", "text", "id2", "5"] ""
        r `shouldBe` Result 0 "\"5\"\n" ""
    -- Reproduces a report against example/01-projects/02-webapp-on-aws/main.lask:
    -- `--access_key_id!!: String = get_env("AWS_ACCESS_KEY_ID")` works
    -- when the default (a get_env call, always a String) is used, but
    -- passing an explicit CLI value that happens to look like JSON
    -- (all digits, `true`, `false`, `null`) fails to bind even though
    -- the parameter is typed `String`. Spec 11.2 says auto mode must
    -- prefer `String` in this ambiguous case; the CLI currently
    -- decodes JSON first and only checks conformance afterward, so it
    -- errors out instead of falling back.
    it "auto decode prefers String for a String-typed keyword parameter (spec 11.2)" $ \lask ->
      withProject [("main.lask", "show(--access_key_id: String = \"default\"): String = access_key_id\n")] $ \dir -> do
        digits <- runLask lask dir ["eval", "show", "--access-key-id", "123456789012"] ""
        digits `shouldBe` Result 0 "\"123456789012\"\n" ""
        boolLike <- runLask lask dir ["eval", "show", "--access-key-id", "true"] ""
        boolLike `shouldBe` Result 0 "\"true\"\n" ""
    it "auto decode prefers String for a String-typed positional parameter (spec 11.2)" $ \lask ->
      withProject [("main.lask", "id3(x: String): String = x\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "id3", "123456789012"] ""
        r `shouldBe` Result 0 "\"123456789012\"\n" ""
    it "auto decode prefers String for a !!-marked secret keyword parameter (spec 6.10, 11.2)" $ \lask ->
      withProject [("main.lask", "show(--access_key_id!!: String = \"default\"): String = access_key_id\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "show", "--access-key-id", "123456789012"] ""
        r `shouldBe` Result 0 "\"123456789012\"\n" ""

  describe "spec 16.3: function values" $ do
    let src =
          "inc(x: Number): Number = x + 1\n\
          \double(x: Number): Number = x * 2\n\
          \incThenDouble = inc >> double\n"
    it "calls function-valued declarations positionally" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "incThenDouble", "3"] ""
        r `shouldBe` Result 0 "8\n" ""

  describe "kebab-case name mapping (spec 11.2)" $ do
    it "maps function and keyword names" $ \lask ->
      withProject [("main.lask", "show_version(--out_dir: String = \".\"): String = out_dir\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "show-version", "--out-dir", "/tmp"] ""
        r `shouldBe` Result 0 "\"/tmp\"\n" ""

  describe "stdin (spec 9)" $ do
    -- A CI step or a process manager may start lask with stdin open and
    -- never write to it. A function that cannot refer to stdin must not
    -- wait for it to close.
    it "does not wait on an open stdin for a function that cannot refer to it" $ \lask ->
      withProject [("main.lask", "hello(): String = \"hi\"\nshout(): String = to_upper(stdin)\n")] $ \dir -> do
        (Just hin, Just hout, _, ph) <-
          createProcess (proc lask ["eval", "hello"]) {cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
        done <- timeout 10000000 (waitForProcess ph)
        hClose hin
        done `shouldBe` Just ExitSuccess
        hGetContents hout >>= (`shouldBe` "\"hi\"\n")
    it "still reads stdin to the end for a function that refers to it, through another function" $ \lask ->
      withProject [("main.lask", "shout(): String = upper()\nupper(): String = to_upper(stdin)\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "shout"] "abc"
        r `shouldBe` Result 0 "\"ABC\"\n" ""
    it "binds stdin as a String" $ \lask ->
      withProject [("main.lask", "shout(): String = to_upper(trim(stdin))\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "shout"] "  hello  \n"
        r `shouldBe` Result 0 "\"HELLO\"\n" ""

  describe "exit codes (spec 11.3, 16.9)" $ do
    it "passes command exit codes through" $ \lask ->
      withProject [("main.lask", "f() = $[#local] exit 42\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 42
        resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-COMMAND-NONZERO"
    -- A linter reports on stdout and leaves stderr empty.
    it "says what happened when a failed command wrote nothing to stderr, leaving the error value alone" $ \lask ->
      withProject
        [ ( "main.lask",
            "lint() = $[#local] echo 'app.py:1: unused import'; exit 3\n\
            \caught(): String = try { lint() } catch (e) { \"[#{e.message}]\" }\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "lint"] ""
          resExit r `shouldBe` 3
          resErr r `shouldContain` "E-RUNTIME-COMMAND-NONZERO: the command exited with code 3 and wrote nothing to stderr"
          c <- runLask lask dir ["eval", "caught"] ""
          resOut c `shouldBe` "\"[]\"\n"
    it "passes a command's exit code through await" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] exit 75\n  await h\n}\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 75
        resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-COMMAND-NONZERO"
    it "uses the Error code of uncaught fail" $ \lask ->
      withProject [("main.lask", "f(): Number = fail({code: 75, message: \"retry later\"})\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 75
        resErr r `shouldSatisfy` isInfixOf "retry later"
    it "normalizes out-of-range codes to 1" $ \lask ->
      withProject [("main.lask", "f(): Number = fail({code: 0, message: \"zero\"})\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
    it "exits 0 when the failure is caught" $ \lask ->
      withProject
        [ ( "main.lask",
            "f(): Number = try {\n\
            \  fail({code: 75, message: \"m\"})\n\
            \} catch (e) {\n\
            \  e.code\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          r `shouldBe` Result 0 "75\n" ""
    it "exits 1 on static errors without evaluating" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\nf() = $[#local] echo should-not-run\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
    it "exits 4 on unknown functions" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["run", "nope"] ""
        resExit r `shouldBe` 4

  -- A computation nothing awaits is waited for at the end of the run,
  -- and reported as an advisory that leaves the outcome alone.
  describe "stopping the commands race cancels (spec 8.7, 15.6)" $ do
    it "stops the losing command and what it started, and logs it as killed" $ \lask ->
      withProject
        [ ( "main.lask",
            "slow(): String = $[#local] echo $$ > pids; sleep 30 & echo $! >> pids; wait\n\
            \fast(): String = $[#local] sleep 1; echo fast\n\
            \f(): String = race([async slow(), async fast()])\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          resExit r `shouldBe` 0
          resOut r `shouldBe` "\"fast\\n\"\n"
          resErr r `shouldContain` "killed"
          pids <- lines <$> readFile (dir </> "pids")
          length pids `shouldBe` 2
          alive <- mapM (\p -> readCreateProcessWithExitCode (proc "kill" ["-0", p]) "") pids
          [c | (c, _, _) <- alive] `shouldSatisfy` notElem ExitSuccess

    it "stops the losing container through the daemon, by its name" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "slow(): String = $[#alpine:3.22.2] sleep 30\n\
              \fast(): String = $[#local] sleep 1; echo fast\n\
              \f(): String = race([async slow(), async fast()])\n"
            )
          ]
          $ \dir -> do
            _ <- runLaskEnv lask dir extra ["sync"] ""
            r <- runLaskEnv lask dir extra ["eval", "f"] ""
            resExit r `shouldBe` 0
            cs <- calls state
            let named = [w | c <- cs, "run " `isPrefixOf` c, (flag, w) <- zip (words c) (drop 1 (words c)), flag == "--name"]
            case named of
              [name] -> do
                cs `shouldContain` ["stop -t 3 " <> name]
                cs `shouldContain` ["rm --force " <> name]
              _ -> expectationFailure ("expected one named run, got " <> show named)

  describe "stopping the commands all cancels when one fails (spec 8.7, 15.6)" $ do
    it "fails at the first failure, wherever it is, and stops the rest" $ \lask ->
      withProject
        [ ( "main.lask",
            "slow() = $[#local] sleep 3; touch marker\n\
            \quick() = $[#local] sleep 0.5; exit 7\n\
            \f() = all([async slow(), async quick()])\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 7
          resErr r `shouldContain` "killed"
          resErr r `shouldNotContain` "W-ASYNC-UNAWAITED"
          threadDelay 4000000
          doesFileExist (dir </> "marker") `shouldReturn` False

  describe "never-awaited async (spec 6.3, 14.2)" $ do
    it "runs it to completion and reports it, keeping the exit code" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] sh -c 'sleep 1; echo done > side.txt'\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"ok\"\n"
        resErr r `shouldContain` "W-ASYNC-UNAWAITED: the async at main.lask:2:7 was never awaited"
        resErr r `shouldContain` "and completed"
        readFile (dir </> "side.txt") `shouldReturn` "done\n"
    it "reports its failure without taking over the exit code" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] exit 3\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resErr r `shouldContain` "failed with E-RUNTIME-COMMAND-NONZERO (exit code 3)"
    it "reports it when the function itself fails, which keeps its own exit code" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] echo side\n  $[#local] exit 5\n}\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 5
        resErr r `shouldContain` "W-ASYNC-UNAWAITED: the async at main.lask:2:7"
    it "does not report a handle consumed by await, all, race, or another function" $ \lask ->
      withProject
        [ ( "main.lask",
            "wait(h: AsyncHandle<String>): String = await h\n\
            \f(): String = do {\n\
            \  a = async \"a\"\n\
            \  b = async \"b\"\n\
            \  c = async \"c\"\n\
            \  d = async \"d\"\n\
            \  e = async \"e\"\n\
            \  x = await a\n\
            \  ys = all([b, c])\n\
            \  z = race([d])\n\
            \  w = wait(e)\n\
            \  \"ok\"\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          r `shouldBe` Result 0 "\"ok\"\n" ""
    it "does not report the rest of an all whose first handle failed" $ \lask ->
      withProject [("main.lask", "f(): Array<String> = do {\n  a = async $[#local] exit 4\n  b = async $[#local] echo b\n  all([a, b])\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 4
        resErr r `shouldNotContain` "W-ASYNC-UNAWAITED"
    it "reports it as a JSON Lines warning under --format json" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] exit 3\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--format", "json", "f"] ""
        resExit r `shouldBe` 0
        let warnings = [l | l <- lines (resErr r), "W-ASYNC-UNAWAITED" `isInfixOf` l]
        length warnings `shouldBe` 1
        mapM_
          (\field -> concat warnings `shouldContain` field)
          [ "\"severity\":\"warning\"",
            "\"stage\":\"runtime\"",
            "\"location\":{\"column\":7,\"file\":\"main.lask\",\"line\":2}",
            "\"failure\":{\"code\":\"E-RUNTIME-COMMAND-NONZERO\""
          ]

  describe "output encodings (spec 11.3, 13.1)" $ do
    it "encodes records as JSON by default" $ \lask ->
      withProject [("main.lask", "u() = {name: \"a\", age: 20}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "u"] ""
        r `shouldBe` Result 0 "{\"age\":20,\"name\":\"a\"}\n" ""
    it "prints raw text with --stdout-encode text" $ \lask ->
      withProject [("main.lask", "s() = \"plain\"\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--stdout-encode", "text", "s"] ""
        r `shouldBe` Result 0 "plain\n" ""
    it "prints Void as nothing" $ \lask ->
      withProject [("main.lask", "f() = for (x : []) {}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        r `shouldBe` Result 0 "" ""

  describe "check (spec 11.1)" $ do
    it "check reports validity" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        r `shouldBe` Result 0 "the module is valid\n" ""
    it "check reports advisories and still exits 0 (spec 14.2)" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  async \"x\"\n  \"done\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        r
          `shouldBe` Result
            0
            "main.lask:2:3-2:12: W-ASYNC-UNUSED [static]: this statement discards an async handle, so it is never awaited\nthe module is valid\n"
            ""
        j <- runLask lask dir ["check", "--format", "json"] ""
        resExit j `shouldBe` 0
        mapM_ (resOut j `shouldContain`) ["\"code\":\"W-ASYNC-UNUSED\"", "\"severity\":\"warning\"", "\"stage\":\"static\""]
    it "check reports diagnostics as JSON with --format json" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\n")] $ \dir -> do
        r <- runLask lask dir ["check", "--format", "json"] ""
        resExit r `shouldBe` 1
        resOut r `shouldSatisfy` isInfixOf "E-TYPE-MISMATCH"
    it "check suggests a correction for a misspelt name (spec 14.3)" $ \lask ->
      withProject [("main.lask", "hello = \"hi\"\nmain() = helo\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resExit r `shouldBe` 1
        resOut r `shouldContain` "undefined name: 'helo'\n  note: did you mean 'hello'?"
        j <- runLask lask dir ["check", "--format", "json"] ""
        resOut j `shouldContain` "\"suggestions\":[\"hello\"]"
    it "check keeps notes in JSON (spec 14.3)" $ \lask ->
      withProject [("main.lask", "import { a } from \"./missing.lask\"\nb = a\n")] $ \dir -> do
        j <- runLask lask dir ["check", "--format", "json"] ""
        resExit j `shouldBe` 1
        resOut j `shouldContain` "\"notes\":["

  describe "did you mean, on the command line (spec 11.2, 14.3)" $ do
    let src = "command { \"python\" } on #local\ncowsay_hello(--dry_run: Bool = false): Bool = dry_run\n"
    it "suggests a function in its CLI form" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "cowsay-helo"] ""
        resExit r `shouldBe` 4
        resErr r `shouldBe` "E-CLI-USAGE: no such function: 'cowsay-helo'\n  note: did you mean 'cowsay-hello'?\n"
        j <- runLask lask dir ["run", "--format", "json", "cowsay-helo"] ""
        resErr j `shouldContain` "\"suggestions\":[\"cowsay-hello\"]"
    it "suggests a keyword option in its CLI form" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "cowsay-hello", "--dry-rnu=true"] ""
        resExit r `shouldBe` 4
        resErr r `shouldBe` "E-CLI-USAGE: unknown keyword argument: '--dry-rnu'\n  note: did you mean '--dry-run'?\n"
    it "suggests a command word" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["cmd", "pyhton"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "did you mean 'python'?"

  describe "commands and environments (spec 16.5, 16.7)" $ do
    it "runs local commands with interpolation" $ \lask ->
      withProject [("main.lask", "n = \"world\"\nf() = $[#local] echo hello #{n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"hello world\\n\"\n"
    it "envs lists referenced environments" $ \lask ->
      withProject
        [ ("main.lask", "f() = $[#./infra/Dockerfile] make\ng() = $[#alpine:3.20] ls\n")
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs", "list"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "infra/Dockerfile"
          resOut r `shouldSatisfy` isInfixOf "alpine:3.20"
    it "limits envs to the call graph of the given function (spec 11.4)" $ \lask ->
      withProject
        [ ( "main.lask",
            "build_go() = $[#golang:1.22] go build ./...\n\
            \build_node() = $[#node:20] npm run build\n\
            \backend() = build_go()\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs", "list", "backend"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "golang:1.22"
          resOut r `shouldNotContain` "node:20"
          whole <- runLask lask dir ["envs", "list"] ""
          resOut whole `shouldSatisfy` isInfixOf "node:20"
    it "rejects undefined environment names before evaluation" $ \lask ->
      withProject [("main.lask", "f() = $[#env(\"missing\")] ls\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "E-TYPE-ENV-CONSTRUCT"

  describe "command execution logs (spec 12.3)" $ do
    let src = "f() = $*[#local] sh -lc \"echo out; echo err 1>&2\"\n"
    it "relays child output to stderr in the text format" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` ""
        resErr r `shouldSatisfy` isInfixOf "[#local:1] $ sh -lc"
        resErr r `shouldSatisfy` isInfixOf "[#local:1] 1| out"
        resErr r `shouldSatisfy` isInfixOf "[#local:1] 2| err"
        resErr r `shouldSatisfy` isInfixOf "[#local:1] exit 0"
    it "keeps stdout clean: only the eval result" $ \lask ->
      withProject [("main.lask", "f() = do {\n  v = $[#local] echo value\n  trim(v)\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"value\"\n"
        resErr r `shouldSatisfy` isInfixOf "1| value"
    it "emits JSON Lines with stream/event fields under --format json" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "--format", "json", "--trace-id", "t-9", "f"] ""
        resExit r `shouldBe` 0
        resErr r `shouldSatisfy` isInfixOf "\"event\":\"start\""
        resErr r `shouldSatisfy` isInfixOf "\"stream\":\"1\""
        resErr r `shouldSatisfy` isInfixOf "\"stream\":\"2\""
        resErr r `shouldSatisfy` isInfixOf "\"event\":\"exit\""
        resErr r `shouldSatisfy` isInfixOf "\"code\":0"
        resErr r `shouldSatisfy` isInfixOf "\"exec\":1"
        resErr r `shouldSatisfy` isInfixOf "\"traceId\":\"t-9\""
        -- The command appears on the start line only (spec 12.3).
        let cmdLogLines = filter (isInfixOf "\"exec\"") (lines (resErr r))
            startLines = filter (isInfixOf "\"event\":\"start\"") cmdLogLines
            relayLines = filter (isInfixOf "\"stream\"") cmdLogLines
        all (isInfixOf "\"command\"") startLines `shouldBe` True
        any (isInfixOf "\"command\"") relayLines `shouldBe` False
        -- Every stderr line is a single JSON object (spec 12.2).
        all (\l -> take 1 l == "{") (lines (resErr r)) `shouldBe` True
    it "logs exit with level warn on non-zero codes" $ \lask ->
      withProject [("main.lask", "f() = $*[#local] sh -lc \"exit 3\"\n")] $ \dir -> do
        r <- runLask lask dir ["run", "--format", "json", "f"] ""
        resExit r `shouldBe` 0
        resErr r `shouldSatisfy` isInfixOf "\"code\":3"
        resErr r `shouldSatisfy` isInfixOf "\"level\":\"warn\""
    it "reports static errors as JSON Lines on stderr" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\ny: Number = true\nf() = 1\n")] $ \dir -> do
        r <- runLask lask dir ["run", "--format", "json", "f"] ""
        resExit r `shouldBe` 1
        let errLines = lines (resErr r)
        all (\l -> take 1 l == "{") errLines `shouldBe` True
        resErr r `shouldSatisfy` isInfixOf "\"stage\":\"static\""

  describe "log (spec 15.12)" $ do
    it "writes to stderr, leaving stdout to the result alone (9.5)" $ \lask ->
      withProject [("main.lask", "f() = do {\n  log(\"building\")\n  \"done\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"done\"\n"
        resErr r `shouldSatisfy` isInfixOf "building"
    it "carries a level and a message as JSON under --format json (12.2)" $ \lask ->
      withProject [("main.lask", "f() = do {\n  log(\"building\")\n  \"done\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--format", "json", "f"] ""
        resExit r `shouldBe` 0
        resErr r `shouldSatisfy` isInfixOf "\"level\":\"info\""
        resErr r `shouldSatisfy` isInfixOf "\"message\":\"building\""
    it "masks a registered secret (12.8)" $ \lask ->
      withProject
        [ ( "main.lask",
            "f() = do {\n  token!! = \"s3cret\"\n  log(concat(\"using \", token))\n  \"done\"\n}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          resExit r `shouldBe` 0
          resErr r `shouldSatisfy` (not . isInfixOf "s3cret")
    let leaky =
          "use(t: String): String = t\n\
          \leak(--token!!: String = \"s3cr3t-value\"): String = do {\n\
          \  use(token)\n\
          \  $[#local] sh -c \"echo #{token} >&2; exit 1\"\n\
          \}\n\
          \caught(--token!!: String = \"s3cr3t-value\"): String = try {\n\
          \  $[#local] sh -c \"echo #{token} >&2; exit 1\"\n\
          \} catch (e) {\n\
          \  e.message\n\
          \}\n"
    it "masks a secret a failed command printed in the final diagnostic (12.8)" $ \lask ->
      withProject [("main.lask", leaky)] $ \dir -> do
        r <- runLask lask dir ["run", "leak"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-COMMAND-NONZERO: ***"
        resErr r `shouldSatisfy` (not . isInfixOf "s3cr3t")
    it "masks secrets in execution events and JSON diagnostics (12.6, 12.8)" $ \lask ->
      withProject [("main.lask", leaky)] $ \dir -> do
        r <- runLask lask dir ["run", "--format", "json", "leak"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "\"kind\":\"call\""
        resErr r `shouldSatisfy` isInfixOf "\"code\":\"E-RUNTIME-COMMAND-NONZERO\""
        resErr r `shouldSatisfy` (not . isInfixOf "s3cr3t")
    it "leaves a caught error value unmasked for the program (12.8)" $ \lask ->
      withProject [("main.lask", leaky)] $ \dir -> do
        r <- runLask lask dir ["eval", "caught"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"s3cr3t-value\\n\"\n"

  describe "observability (spec 12, 13.3)" $ do
    let src =
          "inner(): Number = fail({code: 9, message: \"deep\"})\n\
          \outer(): Number = inner()\n"
    it "prints a stack trace for uncaught failures" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "outer"] ""
        resExit r `shouldBe` 9
        resErr r `shouldSatisfy` isInfixOf "at inner (main.lask)"
        resErr r `shouldSatisfy` isInfixOf "at outer (main.lask)"
    it "emits call/return events with the given trace id" $ \lask ->
      withProject [("main.lask", "f(x: Number): Number = x + 1\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--format", "json", "--trace-id", "t-1", "f", "41"] ""
        resExit r `shouldBe` 0
        resErr r `shouldSatisfy` isInfixOf "\"kind\":\"call\""
        resErr r `shouldSatisfy` isInfixOf "\"kind\":\"return\""
        resErr r `shouldSatisfy` isInfixOf "\"traceId\":\"t-1\""
        resErr r `shouldSatisfy` isInfixOf "\"name\":\"f\""
    it "emits a FailEvent even when the failure is caught (12.5)" $ \lask ->
      withProject
        [ ( "main.lask",
            "boom(): Number = fail({code: 5, message: \"m\"})\n\
            \f(): Number = try {\n\
            \  boom()\n\
            \} catch (e) {\n\
            \  0\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "--format", "json", "f"] ""
          resExit r `shouldBe` 0
          resErr r `shouldSatisfy` isInfixOf "\"kind\":\"fail\""

  describe "modules (spec 5)" $ do
    it "imports across files" $ \lask ->
      withProject
        [ ("main.lask", "import { add } from \"./lib.lask\"\nsum2(a: Number, b: Number): Number = add(a, b)\n"),
          ("lib.lask", "add(x: Number, y: Number): Number = x + y\n")
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "sum2", "20", "22"] ""
          r `shouldBe` Result 0 "42\n" ""

  describe "external dependencies (spec 5, 11.5)" $ do
    it "adds, resolves, syncs and verifies a single-file url dependency" $ \lask ->
      withSystemTempDirectory "lask-deps" $ \root -> do
        let cache = root </> "cache"
            srcDir = root </> "published"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True proj
        writeFile (srcDir </> "notify.lask") "send(x: String): String = concat(\"sent:\", x)\n"
        writeFile (proj </> "main.lask") "import { send } from \"notify\"\nf(): String = send(\"a\")\n"

        -- Before the dependency is declared: E-MODULE-UNRESOLVED
        -- (check prints diagnostics to stdout, spec 11.3).
        r0 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r0 `shouldBe` 1
        resOut r0 `shouldSatisfy` isInfixOf "E-MODULE-UNRESOLVED"

        -- deps add fetches (file:// URL, no network), records and caches.
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "notify", "--url", "file://" <> srcDir </> "notify.lask"] ""
        resExit r1 `shouldBe` 0
        doesFileExist (proj </> "lask.json") `shouldReturn` True

        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"sent:a\"\n" ""

        -- Wiping the cache: resolution must not touch the network.
        removeDirectoryRecursive cache
        r3 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r3 `shouldBe` 1
        resOut r3 `shouldSatisfy` isInfixOf "E-MODULE-UNRESOLVED"
        resOut r3 `shouldSatisfy` isInfixOf "lask sync"

        -- sync restores the cache and verifies the hash.
        r4 <- runLaskEnv lask proj extraEnv ["sync"] ""
        resExit r4 `shouldBe` 0
        r5 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r5 `shouldBe` Result 0 "\"sent:a\"\n" ""

        -- Tampering with the published source: sync must detect the
        -- mismatch and place nothing in the cache (exit 3).
        removeDirectoryRecursive cache
        writeFile (srcDir </> "notify.lask") "send(x: String): String = concat(\"evil:\", x)\n"
        r6 <- runLaskEnv lask proj extraEnv ["sync"] ""
        resExit r6 `shouldBe` 3
        resErr r6 `shouldSatisfy` isInfixOf "E-MODULE-HASH-MISMATCH"
        r7 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r7 `shouldBe` 1

    it "adds and imports a git tree dependency through its entry module" $ \lask ->
      withSystemTempDirectory "lask-deps-git" $ \root -> do
        let cache = root </> "cache"
            repo = root </> "repo"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
        createDirectoryIfMissing True repo
        createDirectoryIfMissing True proj
        -- The re-export binds `u` locally as well as publishing it.
        writeFile (repo </> "main.lask") $
          "export { u } from \"./util.lask\"\n"
            <> "hello(): String = u\n"
        writeFile (repo </> "util.lask") "u: String = \"from-kit\"\n"
        git repo ["init", "--quiet"]
        git repo ["add", "."]
        git repo ["commit", "--quiet", "-m", "init"]
        git repo ["tag", "v1"]
        -- Only the entry module is importable; `u` reaches the
        -- consumer through the re-export in main.lask (spec 5).
        writeFile (proj </> "main.lask") $
          "import { hello, u } from \"kit\"\n"
            <> "f(): String = concat(hello(), u)\n"
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "kit", "--git", "file://" <> repo, "--rev", "v1"] ""
        resExit r1 `shouldBe` 0
        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"from-kitfrom-kit\"\n" ""

    -- A dependency reached through another is locked as parent>child
    -- (spec chapter 5), the key the loader looks it up by.
    it "locks a transitive dependency under its parent, and imports through it" $ \lask ->
      withSystemTempDirectory "lask-deps-transitive" $ \root -> do
        let cache = root </> "cache"
            util = root </> "util"
            kit = root </> "kit"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
            repoWith dir files = do
              createDirectoryIfMissing True dir
              mapM_ (\(f, c) -> writeFile (dir </> f) c) files
              git dir ["init", "--quiet"]
              git dir ["add", "."]
              git dir ["commit", "--quiet", "-m", "init"]
              git dir ["tag", "v1"]
        repoWith util [("main.lask", "u(): String = \"from-util\"\n")]
        repoWith
          kit
          [ ("lask.json", "{\"dependencies\": {\"util\": {\"git\": \"file://" <> util <> "\", \"rev\": \"v1\"}}}\n"),
            ("main.lask", "import { u } from \"util\"\nhello(): String = u()\n")
          ]
        createDirectoryIfMissing True proj
        writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
        resExit r1 `shouldBe` 0
        lock <- readFile (proj </> "lask.lock.json")
        lock `shouldSatisfy` isInfixOf "\"kit>util\""
        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"from-util\"\n" ""
        r3 <- runLaskEnv lask proj extraEnv ["sync", "--frozen"] ""
        resExit r3 `shouldBe` 0

    -- A lock is committed and read on other machines, so it must not
    -- record where the machine that wrote it kept its cache (spec 11.7).
    it "writes the same lock for a dependency's recipe wherever LASK_CACHE_DIR puts the cache" $ \lask ->
      withFakeDocker $ \_ extra ->
        withSystemTempDirectory "lask-deps-recipe" $ \root -> do
          let kit = root </> "kit"
              defaultProj = root </> "default"
              movedProj = root </> "moved"
              moved = ("LASK_CACHE_DIR", root </> "cache") : extra
          createDirectoryIfMissing True (kit </> "images")
          writeFile (kit </> "images" </> "Dockerfile") "FROM scratch\n"
          writeFile (kit </> "main.lask") "hello(): String = $[#./images/Dockerfile] cat x\n"
          git kit ["init", "--quiet"]
          git kit ["add", "."]
          git kit ["commit", "--quiet", "-m", "init"]
          git kit ["tag", "v1"]
          let setUp proj env = do
                createDirectoryIfMissing True proj
                writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
                a <- runLaskEnv lask proj env ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
                resExit a `shouldBe` 0
                b <- runLaskEnv lask proj env ["sync"] ""
                resExit b `shouldBe` 0
                readFile (proj </> "lask.lock.json")
          lockDefault <- setUp defaultProj extra
          lockMoved <- setUp movedProj moved
          lockMoved `shouldBe` lockDefault
          lockDefault `shouldContain` "\"#.lask/deps/"
          -- And a lock written with the default cache holds under the
          -- moved one.
          writeFile (movedProj </> "lask.lock.json") lockDefault
          r <- runLaskEnv lask movedProj moved ["sync", "--frozen"] ""
          resExit r `shouldBe` 0

    it "reports a dependency whose own project file cannot be read, and writes nothing" $ \lask ->
      withSystemTempDirectory "lask-deps-transitive" $ \root -> do
        let kit = root </> "kit"
            proj = root </> "proj"
        createDirectoryIfMissing True kit
        writeFile (kit </> "lask.json") "{\"dependencies\": {\"util\": {\"git\": \"https://x/util\"}}}\n"
        writeFile (kit </> "main.lask") "hello(): String = \"hi\"\n"
        git kit ["init", "--quiet"]
        git kit ["add", "."]
        git kit ["commit", "--quiet", "-m", "init"]
        git kit ["tag", "v1"]
        createDirectoryIfMissing True proj
        writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
        r <- runLaskEnv lask proj [("LASK_CACHE_DIR", root </> "cache")] ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
        resExit r `shouldBe` 3
        resErr r `shouldSatisfy` isInfixOf "of dependency 'kit'"
        doesFileExist (proj </> "lask.json") `shouldReturn` False

    it "requires a source option for deps add (exit 4)" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["deps", "add", "kit"] ""
        resExit r `shouldSatisfy` (/= 0)

    it "refuses a deps add source that git would read as an option, and writes nothing" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        let marker = dir </> "pwned"
        r <- runLask lask dir ["deps", "add", "kit", "--git=--upload-pack=touch " <> marker, "--rev", "v1"] ""
        resExit r `shouldBe` 1
        resErr r `shouldContain` "must not start with '-'"
        doesFileExist marker `shouldReturn` False
        doesFileExist (dir </> "lask.json") `shouldReturn` False

    it "reports malformed dependency files with exit 1" $ \lask ->
      withProject
        [ ("main.lask", "a = 1\n"),
          ("lask.json", "{\"dependencies\": {\"kit\": {\"git\": \"https://x\"}}}")
        ]
        $ \dir -> do
          r <- runLask lask dir ["sync"] ""
          resExit r `shouldBe` 1

  describe "spec 11.6: help display" $ do
    let src =
          "// Build the project.\n\
          \//\n\
          \// The long form of the description.\n\
          \//\n\
          \// @param target   Build target name.\n\
          \// @param out_dir  Where the artifact goes.\n\
          \// @return The artifact path.\n\
          \// @example lask run build release\n\
          \build(target: String, --out_dir: String = \"dist\"): String =\n\
          \  concat(target, out_dir)\n\
          \\n\
          \// Run the tests.\n\
          \test(): String = \"ok\"\n\
          \\n\
          \// Internal.\n\
          \//\n\
          \// @hidden\n\
          \scratch(): String = \"x\"\n\
          \\n\
          \deploy(host: String, --token!!: String = \"s3cret\"): String = concat(host, token)\n"

    it "prints the signature, docs and defaults on stdout with exit 0" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "build", "--help"] ""
        resExit r `shouldBe` 0
        resErr r `shouldBe` ""
        resOut r `shouldContain` "build - Build the project."
        resOut r `shouldContain` "lask run build <target> [--out_dir <String>]"
        resOut r `shouldContain` "The long form of the description."
        resOut r `shouldContain` "--out_dir : String = \"dist\""
        resOut r `shouldContain` "Build target name."
        resOut r `shouldContain` "The artifact path."
        resOut r `shouldContain` "lask run build release"
        resOut r `shouldContain` "Defined at main.lask:9"

    it "names the invoked subcommand in the usage line" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "build", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "lask eval build <target>"

    it "never reveals the default of a secret parameter (spec 12.8)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "deploy", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "--token : String = <secret>"
        resOut r `shouldNotContain` "s3cret"

    it "lists the module's functions, excluding @hidden ones" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "Functions in main.lask:"
        resOut r `shouldContain` "build   Build the project."
        resOut r `shouldContain` "test    Run the tests."
        resOut r `shouldNotContain` "scratch"

    it "lists functions only, but still helps on a plain value binding" $ \lask ->
      withProject [("main.lask", "out_dir = \"dist\"\n" <> src)] $ \dir -> do
        l <- runLask lask dir ["run", "--help"] ""
        l `shouldSatisfy` (not . isInfixOf "out_dir " . resOut)
        v <- runLask lask dir ["run", "out_dir", "--help"] ""
        resExit v `shouldBe` 0
        resOut v `shouldContain` "out_dir"
        resOut v `shouldContain` "Returns:"

    it "wins over argument binding errors (spec 11.6)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "build", "--nosuch", "1", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "lask run build <target>"

    it "passes a literal --help to the function after -- (spec 11.2)" $ \lask ->
      withProject [("main.lask", "id1(a: String): String = a\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "id1", "--", "--help"] ""
        resOut r `shouldNotContain` "Usage:"
        resExit r `shouldBe` 4

    it "reports an unknown function as a usage error (exit 4)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "buidl", "--help"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "E-CLI-USAGE"
        resErr r `shouldContain` "did you mean 'build'?"

    it "still prints help when the module does not type check" $ \lask ->
      withProject [("main.lask", src <> "\nbroken(): Number = \"not a number\"\n")] $ \dir -> do
        r <- runLask lask dir ["run", "build", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "lask run build <target>"
        resErr r `shouldContain` "E-TYPE"

    it "exits 1 when the module cannot be parsed" $ \lask ->
      withProject [("main.lask", "build( = \n")] $ \dir -> do
        r <- runLask lask dir ["run", "build", "--help"] ""
        resExit r `shouldBe` 1
        resOut r `shouldBe` ""

    it "still shows the option help when the module cannot be parsed" $ \lask ->
      withProject [("main.lask", "build( = \n")] $ \dir -> do
        r <- runLask lask dir ["run", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "Usage: lask run"
        resOut r `shouldNotContain` "Functions in"

    it "reports the same information as JSON under --format json" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "--format", "json", "build", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "\"kind\":\"function-help\""
        resOut r `shouldContain` "\"name\":\"out_dir\""
        resOut r `shouldContain` "\"kind\":\"keyword\""
        resOut r `shouldContain` "\"returns\":{\"doc\":\"The artifact path.\",\"type\":\"String\"}"
        l <- runLask lask dir ["run", "--format", "json", "--help"] ""
        resOut l `shouldContain` "\"kind\":\"function-list\""
        resOut l `shouldContain` "\"signature\":\"test(): String\""

  describe "shell completion (spec 11.7)" $ do
    let src =
          "// Build it.\n\
          \build(target: String, --out_dir: String = \"dist\") = $ echo #{target}\n\
          \// @hidden\n\
          \scratch() = $ echo x\n"

    it "completes the module's functions, and only the callable ones" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["__complete", "--", "run", ""] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "build\tBuild it.\n:4\n"

    it "hands the function's own parameters over after the function name (spec 11.2)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["__complete", "--", "run", "build", "--"] ""
        resOut r `shouldContain` "--out_dir"
        resOut r `shouldNotContain` "--module"

    -- The contract that lets a script call this on every keystroke.
    it "always exits 0 and stays silent, whatever the module is in" $ \lask -> do
      let requests =
            [ ["__complete", "--", "run", ""],
              ["__complete", "--", "check", "--module", ""],
              ["__complete", "--"],
              ["__complete"],
              ["__complete", "--", "no-such-command", "--", "-"]
            ]
          projects =
            [ [("main.lask", src)],
              [("main.lask", "build( = oops\n")],
              [("other.lask", "x() = 1\n")]
            ]
      sequence_
        [ withProject files $ \dir -> do
            r <- runLask lask dir args ""
            (args, resExit r) `shouldBe` (args, 0)
            (args, resErr r) `shouldBe` (args, "")
        | files <- projects,
          args <- requests
        ]

    it "prints a script for each shell" $ \lask ->
      withProject [] $ \dir ->
        sequence_
          [ do
              r <- runLask lask dir ["completion", shell] ""
              resExit r `shouldBe` 0
              resOut r `shouldContain` "__complete"
          | shell <- ["bash", "zsh", "fish"]
          ]

    it "rejects a shell it has no script for" $ \lask ->
      withProject [] $ \dir -> do
        r <- runLask lask dir ["completion", "tcsh"] ""
        resExit r `shouldBe` 1

    -- The completion grammar is written by hand beside the parser, so
    -- it can drift from it. optparse-applicative's built-in completer
    -- is wrong about context (it does not know spec 11.2's boundary
    -- rule) but authoritative about which options a parser has, which
    -- is exactly the part that drifts.
    it "offers the same options the parser accepts" $ \lask ->
      withProject [] $ \dir ->
        sequence_
          [ do
              let request =
                    -- One past the last word: the position after the
                    -- subcommand, where its options are offered.
                    ["--bash-completion-index", show (length path + 1)]
                      <> concat [["--bash-completion-word", w] | w <- "lask" : path]
              r <- runLask lask dir request ""
              let fromParser = sort (nub (filter ("--" `isPrefixOf`) (lines (resOut r))))
              (path, fromGrammar path) `shouldBe` (path, fromParser)
          | path <-
              [ ["serve"],
                ["check"],
                ["run"],
                ["eval"],
                ["repl"],
                ["envs", "list"],
                ["sync"],
                ["version"],
                ["completion"],
                ["deps", "list"],
                ["deps", "graph"],
                ["deps", "add"],
                ["deps", "rm"],
                ["cmd"]
              ]
          ]

-- | The long options the completion grammar offers for a subcommand.
fromGrammar :: [String] -> [String]
fromGrammar path =
  case classify (map T.pack path <> ["--"]) of
    POptions opts _ _ -> sort (nub ["--" <> T.unpack (optLong o) | o <- opts])
    _ -> []
