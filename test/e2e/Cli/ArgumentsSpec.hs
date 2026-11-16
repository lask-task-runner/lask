{-# LANGUAGE OverloadedStrings #-}

-- | Calling functions from the command line: arguments, their types and names, standard input, and suggestions (spec 11.2).
module Cli.ArgumentsSpec (spec) where

import Support.Harness
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents)
import System.Process (CreateProcess (cwd, std_err, std_in, std_out), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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
        resOut r `shouldContain` "first-or<T>"
        -- but not in the line the user is meant to type
        resOut r `shouldContain` "lask run first-or <xs> <fallback>"

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

  describe "functions with inferred parameter types from the CLI (spec 4.3, 11.2)" $ do
    let proj =
          [ ( "main.lask",
              "tag(version) = \"v#{version}\"\n\
              \inc(n) = n + 1\n\
              \ident(x) = x\n"
            )
          ]

    it "takes the text as written for an inferred bounded type parameter" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "tag", "1.0"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"v1.0\"\n"

    it "decodes a parameter at the type inferred for it" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "inc", "41"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "42\n"
        bad <- runLask lask dir ["eval", "inc", "x"] ""
        resExit bad `shouldBe` 4

    it "instantiates an inferred unbounded type parameter at Any" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "ident", "[1,2]"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "[1,2]\n"

    it "shows the inferred type parameters where it describes the declaration" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["run", "--help"] ""
        resOut r `shouldContain` "tag<T: stringifiable>"
        resOut r `shouldContain` "ident<T>"
        resOut r `shouldNotContain` "%"

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
    -- `-1` was read as the short form of a parameter named `1`, which
    -- no identifier can be (spec 3.2), while `-12` passed as a number.
    it "binds a negative single-digit number positionally" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "add", "-1", "-12"] ""
        r `shouldBe` Result 0 "-13\n" ""
    it "binds every token after -- positionally, dashes included" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "greet", "--prefix", "hi", "--", "-x"] ""
        r `shouldBe` Result 0 "\"hi, -x\"\n" ""
        k <- runLask lask dir ["eval", "greet", "--", "--prefix"] ""
        k `shouldBe` Result 0 "\"hello, --prefix\"\n" ""
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

  describe "required keyword arguments and flags (spec 11.2)" $ do
    let src =
          "deploy(name: String, --target: String, --dry_run: Bool = false, --force: Bool | Null = null, --n = 1): String =\n\
          \  \"#{name} #{target} #{dry_run} #{force == true} #{n}\"\n\
          \tags(...xs: Array<String>, --all: Bool = false): String = \"#{join(xs, \",\")} #{all}\"\n"
    it "binds a required keyword argument and refuses a call without it" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "deploy", "a", "--target", "prod"] ""
        r `shouldBe` Result 0 "\"a prod false false 1\"\n" ""
        m <- runLask lask dir ["eval", "deploy", "a"] ""
        m `shouldBe` Result 4 "" "E-CLI-USAGE: missing keyword argument: '--target'\n"
    it "binds true to a Bool keyword argument written without a value" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        end <- runLask lask dir ["eval", "deploy", "a", "--target", "prod", "--dry-run"] ""
        end `shouldBe` Result 0 "\"a prod true false 1\"\n" ""
        leading <- runLask lask dir ["eval", "deploy", "--dry-run", "a", "--target", "prod", "--force"] ""
        leading `shouldBe` Result 0 "\"a prod true true 1\"\n" ""
        text <- runLask lask dir ["eval", "--arg-decode", "text", "deploy", "a", "--target", "prod", "--dry-run"] ""
        text `shouldBe` Result 0 "\"a prod true false 1\"\n" ""
    it "still takes true or false after a Bool keyword argument as its value" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "deploy", "a", "--dry-run", "true", "--force", "false", "--target", "prod"] ""
        r `shouldBe` Result 0 "\"a prod true false 1\"\n" ""
        e <- runLask lask dir ["eval", "deploy", "a", "--dry-run=false", "--target=prod"] ""
        e `shouldBe` Result 0 "\"a prod false false 1\"\n" ""
    it "leaves any other word after a flag to the positional arguments" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "tags", "--all", "x", "y"] ""
        r `shouldBe` Result 0 "\"x,y true\"\n" ""
    it "marks required keyword parameters and flags in the help" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["eval", "deploy", "--help"] ""
        resOut r `shouldContain` "lask eval deploy <name> --target <String> [--dry-run] [--force] [--n <Number>]"
        resOut r `shouldContain` "--target : String  (required)"
        j <- runLask lask dir ["eval", "--output", "json", "deploy", "--help"] ""
        resOut j `shouldContain` "\"name\":\"target\",\"required\":true"

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

  describe "did you mean, on the command line (spec 11.2, 14.3)" $ do
    let src = "command { \"python\" } on #local\ncowsay_hello(--dry_run: Bool = false): Bool = dry_run\n"
    it "suggests a function in its CLI form" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "cowsay-helo"] ""
        resExit r `shouldBe` 4
        resErr r `shouldBe` "E-CLI-USAGE: no such function: 'cowsay-helo'\n  note: did you mean 'cowsay-hello'?\n"
        j <- runLask lask dir ["run", "--log-format", "json", "cowsay-helo"] ""
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
