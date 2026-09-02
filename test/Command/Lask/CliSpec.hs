{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests against the built @lask@ binary, covering the
-- CLI examples of spec chapter 16 (source + argv + stdin ->
-- stdout\/stderr\/exit code).
module Command.Lask.CliSpec (spec) where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode, readProcess)
import Test.Hspec

-- | Locate the freshly built binary via stack.
findLask :: IO FilePath
findLask = do
  stackBin <- findExecutable "stack"
  case stackBin of
    Nothing -> fail "stack not found"
    Just _ -> do
      root <- readProcess "stack" ["path", "--local-install-root"] ""
      pure (takeWhile (/= '\n') root </> "bin" </> "lask")

data Result = Result
  { resExit :: Int,
    resOut :: String,
    resErr :: String
  }
  deriving (Show, Eq)

runLask :: FilePath -> FilePath -> [String] -> String -> IO Result
runLask lask dir = runLaskEnv lask dir []

-- | Run the binary with extra environment variables (e.g.
-- @LASK_CACHE_DIR@ for hermetic dependency tests).
runLaskEnv :: FilePath -> FilePath -> [(String, String)] -> [String] -> String -> IO Result
runLaskEnv lask dir extraEnv args input = do
  baseEnv <- getEnvironment
  let fullEnv = extraEnv <> [(k, v) | (k, v) <- baseEnv, k `notElem` map fst extraEnv]
  (code, out, err) <-
    readCreateProcessWithExitCode ((proc lask args) {cwd = Just dir, env = Just fullEnv}) input
  pure (Result (exitOf code) out err)
  where
    exitOf ExitSuccess = 0
    exitOf (ExitFailure n) = n

withProject :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withProject files action =
  withSystemTempDirectory "lask-e2e" $ \dir -> do
    mapM_
      ( \(name, content) -> do
          createDirectoryIfMissing True dir
          writeFile (dir </> name) content
      )
      files
    action dir

spec :: Spec
spec = beforeAll findLask $ do
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
    it "arg-decode text keeps arguments as strings" $ \lask ->
      withProject [("main.lask", "id2(x: String): String = x\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--arg-decode", "text", "id2", "5"] ""
        r `shouldBe` Result 0 "\"5\"\n" ""
    -- Reproduces a report against example/04-webapp/main.lask:
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
    it "binds stdin as a String" $ \lask ->
      withProject [("main.lask", "shout(): String = to_upper(trim(stdin))\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "shout"] "  hello  \n"
        r `shouldBe` Result 0 "\"HELLO\"\n" ""

  describe "exit codes (spec 11.3, 16.9)" $ do
    it "passes command exit codes through" $ \lask ->
      withProject [("main.lask", "f() = $ exit 42\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 42
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
      withProject [("main.lask", "x: Number = \"s\"\nf() = $ echo should-not-run\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
    it "exits 4 on unknown functions" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["run", "nope"] ""
        resExit r `shouldBe` 4

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
    it "check reports diagnostics as JSON with --format json" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\n")] $ \dir -> do
        r <- runLask lask dir ["check", "--format", "json"] ""
        resExit r `shouldBe` 1
        resOut r `shouldSatisfy` isInfixOf "E-TYPE-MISMATCH"

  describe "commands and environments (spec 16.5, 16.7)" $ do
    it "runs local commands with interpolation" $ \lask ->
      withProject [("main.lask", "n = \"world\"\nf() = $ echo hello #{n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"hello world\\n\"\n"
    it "envs lists referenced environments" $ \lask ->
      withProject
        [ ("main.lask", "f() = $[#docker(dockerfile = \"infra/Dockerfile\")] make\ng() = $[#alpine:3.20] ls\n")
        ]
        $ \dir -> do
          r <- runLask lask dir ["envs"] ""
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
          r <- runLask lask dir ["envs", "backend"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "golang:1.22"
          resOut r `shouldNotContain` "node:20"
          whole <- runLask lask dir ["envs"] ""
          resOut whole `shouldSatisfy` isInfixOf "node:20"
    it "rejects undefined environment names before evaluation" $ \lask ->
      withProject [("main.lask", "f() = $[#env(\"missing\")] ls\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 1
        resErr r `shouldSatisfy` isInfixOf "E-TYPE-ENV-CONSTRUCT"

  describe "command execution logs (spec 12.3)" $ do
    let src = "f() = $* sh -lc \"echo out; echo err 1>&2\"\n"
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
      withProject [("main.lask", "f() = do {\n  v = $ echo value\n  trim(v)\n}\n")] $ \dir -> do
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
      withProject [("main.lask", "f() = $* sh -lc \"exit 3\"\n")] $ \dir -> do
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
        resOut r3 `shouldSatisfy` isInfixOf "deps sync"

        -- deps sync restores the cache and verifies the hash.
        r4 <- runLaskEnv lask proj extraEnv ["deps", "sync"] ""
        resExit r4 `shouldBe` 0
        r5 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r5 `shouldBe` Result 0 "\"sent:a\"\n" ""

        -- Tampering with the published source: sync must detect the
        -- mismatch and place nothing in the cache (exit 3).
        removeDirectoryRecursive cache
        writeFile (srcDir </> "notify.lask") "send(x: String): String = concat(\"evil:\", x)\n"
        r6 <- runLaskEnv lask proj extraEnv ["deps", "sync"] ""
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
        let git args = readCreateProcessWithExitCode ((proc "git" args) {cwd = Just repo}) ""
        _ <- git ["init", "--quiet"]
        _ <- git ["add", "."]
        _ <- git ["-c", "user.email=t@example.com", "-c", "user.name=t", "commit", "--quiet", "-m", "init"]
        _ <- git ["tag", "v1"]
        -- Only the entry module is importable; `u` reaches the
        -- consumer through the re-export in main.lask (spec 5).
        writeFile (proj </> "main.lask") $
          "import { hello, u } from \"kit\"\n"
            <> "f(): String = concat(hello(), u)\n"
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "kit", "--git", "file://" <> repo, "--rev", "v1"] ""
        resExit r1 `shouldBe` 0
        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"from-kitfrom-kit\"\n" ""

    it "requires a source option for deps add (exit 4)" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["deps", "add", "kit"] ""
        resExit r `shouldSatisfy` (/= 0)

    it "reports malformed dependency files with exit 1" $ \lask ->
      withProject
        [ ("main.lask", "a = 1\n"),
          ("lask.json", "{\"dependencies\": {\"kit\": {\"git\": \"https://x\"}}}")
        ]
        $ \dir -> do
          r <- runLask lask dir ["deps", "sync"] ""
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
