{-# LANGUAGE OverloadedStrings #-}

-- | Help display (spec 11.6).
module Cli.HelpSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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
        resOut r `shouldContain` "lask run build <target> [--out-dir <String>]"
        resOut r `shouldContain` "The long form of the description."
        resOut r `shouldContain` "--out-dir : String = \"dist\""
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

    -- A leading `_` stays: `-build-all` would be read as an option
    -- (spec 11.6).
    it "keeps the underscore that leads a name, so that what it shows can be typed" $ \lask ->
      withProject [("main.lask", "// Build.\n_build_all(--_dry_run = false, --out_dir = \"dist\"): String = \"b #{out_dir}\"\n")] $ \dir -> do
        l <- runLask lask dir ["run", "--help"] ""
        resOut l `shouldContain` "_build-all  Build."
        h <- runLask lask dir ["run", "_build-all", "--help"] ""
        resOut h `shouldContain` "lask run _build-all [--_dry-run] [--out-dir <String>]"
        r <- runLask lask dir ["eval", "_build-all", "--_dry-run", "--out-dir", "x"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"b x\"\n"

    -- The environments a function reaches are named as envs list names
    -- them: the head and its kind, the host first (spec 11.4, 11.6).
    it "shows the environments a function uses as envs list does" $ \lask ->
      withProject
        [ ("main.lask", "both(): String = do {\n  $[#golang:1.22] go version\n  $[#./infra/builder.Dockerfile] make\n  $[#local] uname\n}\n"),
          ("infra/builder.Dockerfile", "FROM alpine:3.22.2\n")
        ]
        $ \dir -> do
          t <- runLask lask dir ["run", "both", "--help"] ""
          resOut t
            `shouldContain` "Environments:\n  #local                       local\n  #./infra/builder.Dockerfile  recipe\n  #golang:1.22                 registry\n"
          j <- runLask lask dir ["run", "--output", "json", "both", "--help"] ""
          resOut j `shouldContain` "\"environments\":[{\"environment\":\"#local\",\"kind\":\"local\"},{\"environment\":\"#./infra/builder.Dockerfile\",\"kind\":\"recipe\"},{\"environment\":\"#golang:1.22\",\"kind\":\"registry\"}]"

    -- A plain value binding is not a function the command line calls
    -- (spec 11.2), so it has no help either.
    it "lists functions only, and gives no help on a plain value binding" $ \lask ->
      withProject [("main.lask", "out_dir = \"dist\"\n" <> src)] $ \dir -> do
        l <- runLask lask dir ["run", "--help"] ""
        l `shouldSatisfy` (not . isInfixOf "out_dir " . resOut)
        v <- runLask lask dir ["run", "out_dir", "--help"] ""
        resExit v `shouldBe` 4
        resErr v `shouldContain` "'out_dir' is a value, not a function"

    it "calls functions only, wherever a subcommand takes a function name (spec 11.2)" $ \lask ->
      withProject
        [("main.lask", "version = \"1.0\"\ninc = \\(x: Number) -> x + 1\nshow(): String = version\ninternal hidden(): String = \"h\"\n")]
        $ \dir -> do
          let usage args = do
                r <- runLask lask dir args ""
                resExit r `shouldBe` 4
                pure (resErr r)
          usage ["eval", "version"] >>= (`shouldContain` "'version' is a value, not a function")
          usage ["envs", "list", "version"] >>= (`shouldContain` "'version' is a value, not a function")
          usage ["secrets", "list", "version"] >>= (`shouldContain` "'version' is a value, not a function")
          -- An internal declaration is not one, for envs as for run.
          usage ["envs", "list", "hidden"] >>= (`shouldContain` "'hidden' is internal")
          inc <- runLask lask dir ["eval", "inc", "2"] ""
          resOut inc `shouldBe` "3\n"
          shown <- runLask lask dir ["eval", "show"] ""
          resOut shown `shouldBe` "\"1.0\"\n"

    it "wins over argument binding errors (spec 11.6)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "build", "--nosuch", "1", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "lask run build <target>"

    it "passes a literal --help to the function after -- (spec 11.2)" $ \lask ->
      withProject [("main.lask", "id1(a: String): String = a\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "id1", "--", "--help"] ""
        r `shouldBe` Result 0 "\"--help\"\n" ""

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

    -- A declaration whose body fails showed `n : ?` and no return
    -- type, though both were written down.
    it "shows the annotated types of a declaration that does not elaborate" $ \lask ->
      withProject [("main.lask", "bad(n: Number, ...xs: Array<String>, --k: Bool = true): String = n + nope\n")] $ \dir -> do
        r <- runLask lask dir ["run", "bad", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "n : Number"
        resOut r `shouldContain` "xs : Array<String>"
        resOut r `shouldContain` "--k : Bool = true"
        resOut r `shouldContain` "Returns:\n  String"
        resErr r `shouldContain` "E-NAME-UNDEFINED"

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

    it "reports the same information as JSON under --output json" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "--output", "json", "build", "--help"] ""
        resExit r `shouldBe` 0
        resOut r `shouldContain` "\"kind\":\"function-help\""
        resOut r `shouldContain` "\"name\":\"out_dir\""
        resOut r `shouldContain` "\"kind\":\"keyword\""
        resOut r `shouldContain` "\"returns\":{\"doc\":\"The artifact path.\",\"type\":\"String\"}"
        l <- runLask lask dir ["run", "--output", "json", "--help"] ""
        resOut l `shouldContain` "\"kind\":\"function-list\""
        resOut l `shouldContain` "\"signature\":\"test(): String\""
