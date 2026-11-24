{-# LANGUAGE OverloadedStrings #-}

-- | The command line around a function call: the subcommands and their
-- usage errors (spec 11.1), how the words after the function name bind
-- to its parameters (11.2), what goes to each stream and the exit code
-- of each kind of failure (11.3), the help of a function as JSON (11.6),
-- and the argument vector of @cmd@ (11.8).
--
-- What "Cli.ArgumentsSpec", "Cli.HelpSpec" and "Cli.CmdSpec" already
-- check is not repeated here.
module Cli.InvocationSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Support.Harness
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Hspec

program :: String
program =
  unlines
    [ "// Greet someone.",
      "//",
      "// @param name Who to greet.",
      "greet(name: String, --prefix: String = \"hello\", --loud: Bool = false): String =",
      "  loud ? to_upper(\"#{prefix} #{name}\") : \"#{prefix} #{name}\"",
      "",
      "sum(...xs: Array<Number>): Number = reduce(xs, 0, \\(a: Number, x: Number) -> a + x)",
      "short(--n: Number = 3): Number = n",
      "h_param(--h: Number = 1): Number = h",
      "maybe(--n: Number | Null = null): String = n == null ? \"none\" : \"some\"",
      "opt(--v: Bool | Null = null): String = v == null ? \"null\" : (v == true ? \"t\" : \"f\")",
      "need(--target: String): String = target",
      "with_help(--help: String = \"x\"): String = help",
      "camelCase(): String = \"camel\"",
      "internal hidden_fn(): String = \"hidden\"",
      "on_env(e: Environment): String = $[e] echo hi",
      "kw_env(--e: Environment = #local): String = \"default\"",
      "echo_stdin(): String = stdin",
      "rec(): Record<b: String, a: Number> = {b: \"x\", a: 1}",
      "inc = \\(x: Number) -> x + 1",
      "failing(): Number = 1 / 0",
      "missing(): String = read_file(\"missing.txt\", #local)"
    ]

-- | Run lask in a project holding the program, with this standard input.
lask' :: FilePath -> [String] -> String -> IO Result
lask' lask args input = withProject [("main.lask", program)] $ \dir -> runLask lask dir args input

usage :: FilePath -> [String] -> String -> Expectation
usage lask args message = do
  r <- lask' lask args ""
  resExit r `shouldBe` 4
  resOut r `shouldBe` ""
  resErr r `shouldSatisfy` isInfixOf message

evals :: FilePath -> [String] -> String -> Expectation
evals lask args out = do
  r <- lask' lask ("eval" : args) ""
  (resExit r, resOut r) `shouldBe` (0, out <> "\n")

json :: String -> Maybe A.Value
json = A.decode . utf8

-- | What a process wrote, as the bytes it wrote.
utf8 :: String -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8 . T.pack

spec :: Spec
spec = beforeAll findLask $ do
  describe "subcommands and their options (spec 11.1)" $ do
    it "reports an unknown subcommand as a usage error" $ \lask ->
      usage lask ["frobnicate"] "E-CLI-USAGE"
    it "reports a subcommand group with no subcommand as a usage error" $ \lask -> do
      usage lask ["envs"] "E-CLI-USAGE"
      usage lask ["secrets"] "E-CLI-USAGE"
      usage lask ["envs", "bogus"] "E-CLI-USAGE"
    it "lists the subcommands of a group given none" $ \lask -> do
      r <- lask' lask ["envs"] ""
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` ("E-CLI-USAGE: a subcommand is needed\n" `isPrefixOf`)
      resErr r `shouldSatisfy` isInfixOf "Available commands:"
      resErr r `shouldSatisfy` isInfixOf "  files "
    it "reports a value outside the closed set of --output, --log-format and --arg-decode" $ \lask -> do
      usage lask ["eval", "--output", "yaml", "rec"] "--output"
      usage lask ["eval", "--log-format", "xml", "rec"] "--log-format"
      usage lask ["eval", "--arg-decode", "bytes", "rec"] "--arg-decode"
    it "takes no --arg-decode for cmd, whose arguments are not decoded (spec 11.8)" $ \lask ->
      usage lask ["cmd", "--arg-decode", "json", "echo"] "--arg-decode"
    it "prints the version" $ \lask -> do
      r <- lask' lask ["version"] ""
      resExit r `shouldBe` 0
      resOut r `shouldSatisfy` ("lask " `isPrefixOf`)

  describe "the entry module (spec 11.1)" $ do
    let nested =
          [ ("main.lask", "notes() = read_file(\"notes.txt\", #local)\n"),
            ("notes.txt", "from the project"),
            ("app/deep/keep", "")
          ]

    it "finds main.lask in the nearest directory above, and resolves paths against it" $ \lask ->
      withProject nested $ \dir -> do
        r <- runLask lask (dir </> "app/deep") ["eval", "notes"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"from the project\"\n"

    it "names the module it found relative to where lask runs" $ \lask ->
      withProject (("main.lask", "notes(): Number = \"x\"\n") : drop 1 nested) $ \dir -> do
        r <- runLask lask (dir </> "app/deep") ["check"] ""
        resExit r `shouldBe` 1
        resOut r `shouldContain` "../../main.lask:1:"

    it "takes the nearest main.lask when several directories have one" $ \lask ->
      withProject (("app/main.lask", "notes() = \"from app\"\n") : nested) $ \dir -> do
        r <- runLask lask (dir </> "app/deep") ["eval", "notes"] ""
        resOut r `shouldBe` "\"from app\"\n"

    it "does not look above for a module --module names" $ \lask ->
      withProject nested $ \dir -> do
        r <- runLask lask (dir </> "app") ["eval", "--module", "main.lask", "notes"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "E-CLI-USAGE: no such module: main.lask"

    it "is a usage error when no directory above has one" $ \lask ->
      withProject [("readme", "")] $ \dir -> do
        r <- runLask lask dir ["run", "build"] ""
        resExit r `shouldBe` 4
        resErr r `shouldContain` "E-CLI-USAGE: no main.lask in this directory or any directory above it"

  describe "the boundary rule (spec 11.2)" $ do
    it "passes an option of lask written after the function name to the function" $ \lask ->
      usage lask ["eval", "greet", "bob", "--output", "text"] "unknown keyword argument: '--output'"
    it "reads -h before the function name as help" $ \lask -> do
      r <- lask' lask ["eval", "-h", "greet"] ""
      resExit r `shouldBe` 0
      resOut r `shouldSatisfy` ("greet - Greet someone." `isPrefixOf`)
    it "reads -h after the function name as the short form of a parameter h" $ \lask ->
      evals lask ["h-param", "-h", "5"] "5"
    it "reads --help as the value of a keyword argument as help, being a standalone token" $ \lask -> do
      r <- lask' lask ["eval", "greet", "bob", "--prefix", "--help"] ""
      resExit r `shouldBe` 0
      resOut r `shouldSatisfy` ("greet - " `isPrefixOf`)
    it "passes every token after -- positionally, to a variadic parameter too" $ \lask -> do
      evals lask ["sum", "1", "--", "2", "-3"] "0"
      usage lask ["eval", "greet", "bob", "--", "--prefix", "x"] "too many positional arguments: expected 1, got 3"

  describe "keyword arguments (spec 11.2)" $ do
    it "takes --name value, --name=value and the short form -n value" $ \lask -> do
      evals lask ["greet", "bob", "--prefix", "hi"] "\"hi bob\""
      evals lask ["greet", "bob", "--prefix=hi"] "\"hi bob\""
      evals lask ["short", "-n", "5"] "5"
      evals lask ["short", "--n=5"] "5"
    it "reports a duplicate keyword argument before evaluation" $ \lask ->
      usage lask ["eval", "greet", "bob", "--prefix", "a", "--prefix", "b"] "duplicate keyword argument: '--prefix'"
    it "reports an unknown keyword argument before evaluation" $ \lask ->
      usage lask ["eval", "greet", "bob", "--nope", "1"] "unknown keyword argument: '--nope'"
    it "reports an unknown keyword argument as unknown when it ends the line" $ \lask -> do
      usage lask ["eval", "greet", "bob", "-x"] "unknown keyword argument: '--x'"
      usage lask ["eval", "greet", "bob", "--prefx"] "unknown keyword argument: '--prefx'"
    it "reports a keyword argument that is not a flag as needing a value when it ends the line" $ \lask -> do
      usage lask ["eval", "greet", "bob", "--prefix"] "keyword argument '--prefix' needs a value"
      usage lask ["eval", "short", "-n"] "keyword argument '--n' needs a value"
    it "binds a flag of type Bool | Null to true written alone, and to null by --flag=null" $ \lask -> do
      evals lask ["opt"] "\"null\""
      evals lask ["opt", "--v"] "\"t\""
      evals lask ["opt", "--v", "false"] "\"f\""
      evals lask ["opt", "--v=null"] "\"null\""
    it "reads a word after a flag that is not true or false as a positional argument, null included" $ \lask ->
      usage lask ["eval", "opt", "--v", "null"] "too many positional arguments"
    it "binds a union parameter to the member the decoded value conforms to" $ \lask -> do
      evals lask ["maybe", "--n", "8080"] "\"some\""
      evals lask ["maybe", "--n", "null"] "\"none\""
      usage lask ["eval", "--arg-decode", "text", "maybe", "--n", "8080"] "does not fit the parameter type"
    it "reports an argument that is not JSON under --arg-decode json" $ \lask ->
      usage lask ["eval", "--arg-decode", "json", "greet", "notjson"] "not valid JSON"

  describe "binding (spec 11.2)" $ do
    it "reports a missing required keyword argument in its command-line form" $ \lask ->
      usage lask ["eval", "need"] "missing keyword argument: '--target'"
    it "reports too many or too few positional arguments" $ \lask -> do
      usage lask ["eval", "greet"] "missing positional arguments: expected 1, got 0"
      usage lask ["eval", "greet", "a", "b"] "too many positional arguments: expected 1, got 2"
    it "collects no positional argument into an empty variadic parameter" $ \lask ->
      evals lask ["sum"] "0"
    it "calls a value binding whose value is a function" $ \lask ->
      evals lask ["inc", "2"] "3"
    it "does not reach an internal declaration" $ \lask ->
      usage lask ["eval", "hidden-fn"] "no such function"
    it "does not map a camelCase name, which is called as written" $ \lask -> do
      evals lask ["camelCase"] "\"camel\""
      usage lask ["eval", "camel-case"] "no such function"
    it "refuses a function with a positional Environment parameter, and an Environment given as a keyword" $ \lask -> do
      usage lask ["eval", "on-env", "x"] "Environment"
      usage lask ["eval", "kw-env", "--e", "x"] "cannot be set from the CLI"
      evals lask ["kw-env"] "\"default\""

  describe "standard input (spec 9.2, 9.4, 11.3)" $ do
    it "binds the input as it was given, line breaks and all" $ \lask -> do
      r <- lask' lask ["eval", "echo-stdin"] "héllo\r\nworld\n"
      resOut r `shouldBe` "\"héllo\\r\\nworld\\n\"\n"
    it "binds an empty input as the empty string" $ \lask -> do
      r <- lask' lask ["eval", "echo-stdin"] ""
      resOut r `shouldBe` "\"\"\n"
    it "reports input that is not UTF-8 as E-IO-DATA-DECODE, exiting 3" $ \lask ->
      withProject [("main.lask", program)] $ \dir -> do
        -- The harness writes text, so the bytes are written by printf.
        (code, _, err) <- readCreateProcessWithExitCode ((shell ("printf '\\377\\376' | '" <> lask <> "' eval echo-stdin")) {cwd = Just dir}) ""
        code `shouldBe` ExitFailure 3
        err `shouldSatisfy` isInfixOf "E-IO-DATA-DECODE"

  describe "standard output (spec 11.3)" $ do
    it "writes the result of eval as compact JSON, pretty JSON or text" $ \lask -> do
      evals lask ["rec"] "{\"a\":1,\"b\":\"x\"}"
      evals lask ["--output", "pretty-json", "rec"] "{\n    \"a\": 1,\n    \"b\": \"x\"\n}"
      evals lask ["--output", "text", "greet", "bob"] "hello bob"
    it "writes nothing to stdout for run, whatever --output says" $ \lask -> do
      r <- lask' lask ["run", "--output", "pretty-json", "rec"] ""
      (resExit r, resOut r) `shouldBe` (0, "")
    it "writes the diagnostics of check as JSON under --output json and pretty-json" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\n")] $ \dir -> do
        forM' ["json", "pretty-json"] $ \fmt -> do
          r <- runLask lask dir ["check", "--output", fmt] ""
          resExit r `shouldBe` 1
          case json (resOut r) of
            Just (A.Array ds) -> [KM.lookup "code" o | A.Object o <- V.toList ds] `shouldBe` [Just "E-TYPE-MISMATCH"]
            other -> expectationFailure ("not a JSON array: " <> show other)

  describe "exit codes (spec 11.3)" $ do
    it "exits 1 for a static error, 2 for a runtime error, 3 for an external I/O error and 4 for a usage error" $ \lask -> do
      withProject [("main.lask", "x: Number = \"s\"\nf(): Number = 1\n")] $ \dir ->
        (resExit <$> runLask lask dir ["run", "f"] "") `shouldReturn` 1
      (resExit <$> lask' lask ["run", "failing"] "") `shouldReturn` 2
      (resExit <$> lask' lask ["run", "missing"] "") `shouldReturn` 3
      (resExit <$> lask' lask ["run", "greet"] "") `shouldReturn` 4
    it "writes the trace id it is given into every JSON line" $ \lask -> do
      r <- lask' lask ["run", "--log-format", "json", "--trace-id", "trace-123", "failing"] ""
      let ls = lines (resErr r)
      ls `shouldNotBe` []
      [l | l <- ls, not ("\"traceId\":\"trace-123\"" `isInfixOf` l)] `shouldBe` []

  describe "the help of a function as JSON (spec 11.6)" $ do
    it "lists the functions as a function-list, with their names as declared" $ \lask -> do
      r <- lask' lask ["run", "--output", "json", "--help"] ""
      resExit r `shouldBe` 0
      -- The option help comes first, as text; the list is the last line.
      case json (last (lines (resOut r))) of
        Just (A.Object o) -> do
          KM.lookup "kind" o `shouldBe` Just "function-list"
          KM.lookup "module" o `shouldBe` Just "main.lask"
          case KM.lookup "functions" o of
            Just (A.Array fs) -> do
              let names = [n | A.Object f <- V.toList fs, Just (A.String n) <- [KM.lookup "name" f]]
              names `shouldSatisfy` elem "kw_env"
              names `shouldNotSatisfy` elem "hidden_fn"
              [KM.keys f | A.Object f <- V.toList fs] `shouldSatisfy` all (== ["name", "signature", "summary"])
            other -> expectationFailure ("no functions: " <> show other)
        other -> expectationFailure ("not a JSON object: " <> show other)
    it "marks a required keyword parameter and a variadic one" $ \lask -> do
      need <- lask' lask ["eval", "--output", "json", "need", "--help"] ""
      fmap paramOf (json (resOut need)) `shouldBe` Just (Just ("keyword", A.Bool True, A.Null))
      sumHelp <- lask' lask ["eval", "--output", "json", "sum", "--help"] ""
      fmap paramOf (json (resOut sumHelp)) `shouldBe` Just (Just ("variadic", A.Bool False, A.Null))
    it "carries confirm as null when nothing asks for confirmation, and empty sections as empty arrays" $ \lask -> do
      r <- lask' lask ["eval", "--output", "json", "need", "--help"] ""
      case json (resOut r) of
        Just (A.Object o) -> do
          KM.lookup "confirm" o `shouldBe` Just A.Null
          KM.lookup "environments" o `shouldBe` Just (A.Array V.empty)
          KM.lookup "examples" o `shouldBe` Just (A.Array V.empty)
        other -> expectationFailure ("not a JSON object: " <> show other)

  describe "a keyword parameter named help (spec 11.6, 14.2)" $ do
    it "is bound only by --help=<value>, --help alone showing the help, and is advised of" $ \lask -> do
      evals lask ["with-help", "--help=y"] "\"y\""
      r <- lask' lask ["eval", "with-help", "--help"] ""
      resOut r `shouldSatisfy` ("with-help" `isPrefixOf`)
      c <- lask' lask ["check"] ""
      (resExit c, resOut c <> resErr c) `shouldSatisfy` (\(e, out) -> e == 0 && "W-CLI-PARAM-SHADOWED" `isInfixOf` out)
    it "is not offered by completion as --help, which shows the help" $ \lask -> do
      r <- lask' lask ["__complete", "--", "run", "with-help", "--"] ""
      let candidates = [takeWhile (/= '\t') l | l <- lines (resOut r), not (":" `isPrefixOf` l)]
      length (filter (== "--help") candidates) `shouldBe` 1

  describe "the argument vector of cmd (spec 11.8)" $ do
    let words' = [("main.lask", "command { \"echo\", \"my_tool\" } on #local\ninternal command { \"printf\" } on #local\n")]
    it "resolves a command word by exact text, without the - to _ mapping" $ \lask ->
      withProject words' $ \dir -> do
        r <- runLask lask dir ["cmd", "my-tool"] ""
        resExit r `shouldBe` 4
        resErr r `shouldSatisfy` isInfixOf "lask cmd --help"
    it "runs an internal command word of the module" $ \lask ->
      withProject words' $ \dir -> do
        r <- runLask lask dir ["cmd", "printf", "%s|", "a b", "c"] ""
        (resExit r, resOut r) `shouldBe` (0, "a b|c|")
    it "passes an option of lask after the command word to the program" $ \lask ->
      withProject words' $ \dir -> do
        r <- runLask lask dir ["cmd", "echo", "--module", "x"] ""
        (resExit r, resOut r) `shouldBe` (0, "--module x\n")
    it "records the argument vector on the start line, so that one argument is told from two" $ \lask ->
      withProject words' $ \dir -> do
        one <- runLask lask dir ["cmd", "echo", "a b"] ""
        two <- runLask lask dir ["cmd", "echo", "a", "b"] ""
        let start r = [dropWhile (/= '$') l | l <- lines (resErr r), "] $ " `isInfixOf` l]
        start one `shouldBe` ["$ echo 'a b'"]
        start two `shouldBe` ["$ echo a b"]
  where
    forM' xs f = mapM_ f xs
    paramOf v = case v of
      A.Object o | Just (A.Array ps) <- KM.lookup "params" o, [A.Object p] <- V.toList ps ->
        (,,) <$> (KM.lookup "kind" p >>= str) <*> KM.lookup "required" p <*> KM.lookup "default" p
      _ -> Nothing
    str (A.String s) = Just s
    str _ = Nothing
