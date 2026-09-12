{-# LANGUAGE OverloadedStrings #-}

-- | Shell completion (spec 11.7). The candidate computation is a pure
-- function of the words typed and the module on disk, so the module
-- here is an in-memory one and every case is a table row: words in,
-- candidates out.
module Command.Lask.CompleteSpec (spec) where

import Command.Lask.Complete
import Data.Bits ((.&.))
import Data.List (isPrefixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- | A project that exists only in the test.
memResolver :: [(FilePath, Text)] -> Resolver
memResolver files =
  Resolver
    { resolveRead = \p -> pure (lookup p files),
      resolveList = \d -> pure (entriesOf d)
    }
  where
    entriesOf d =
      let prefix = if d == "." then "" else d <> "/"
       in nub
            [ (head parts, not (null (tail parts)))
            | (p, _) <- files,
              prefix `isPrefixOf` p,
              let parts = splitOn '/' (drop (length prefix) p),
              not (null parts)
            ]

    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest

-- | The module every function test runs against.
buildModule :: Text
buildModule =
  T.unlines
    [ "envs: Map<Environment> = { \"local\": #local, \"docker\": #alpine:3.20 }",
      "",
      "// Build the project and publish the artifact.",
      "//",
      "// @param target   Build target name.",
      "// @param out_dir  Directory the artifact is written to.",
      "// @param publish  Whether to upload the artifact.",
      "// @complete env @keys envs",
      "// @complete out_dir @dir",
      "build(target: String, --out_dir: String = \"dist\", --publish: Bool = false, --env: String = \"local\") =",
      "  $ make #{target}",
      "",
      "// Show the version.",
      "show_version() = $ echo 1",
      "",
      "// @hidden",
      "scratch() = $ echo scratch",
      "",
      "internal helper() = $ echo helper",
      "",
      "// Needs an environment value, which no decoding mode can build.",
      "on_env(e: Environment) = $[e] echo hi",
      "",
      "// Keeps its own environment.",
      "packaged(--env2: Environment = #local) = $[env2] echo hi",
      "",
      "// A secret default must never be read, let alone shown.",
      "login(--password!!: String = \"hunter2\") = $ echo #{password}"
    ]

project :: [(FilePath, Text)]
project =
  [ ("main.lask", buildModule),
    ("ci.lask", "// Run CI.\nci_only() = $ echo ci\n"),
    ("lask.json", "{\"dependencies\":{\"terraform\":{\"git\":\"https://example.com/t\",\"rev\":\"v1\",\"hash\":\"sha256-00\"}}}"),
    ("sub/nested.lask", "nested() = $ echo n\n"),
    ("notes.txt", "not a module")
  ]

-- | @shouldContain@ on lists is a contiguous-sublist check; these two
-- are the membership checks the tables below want.
shouldOffer :: [Text] -> [Text] -> Expectation
shouldOffer vs = mapM_ (\x -> vs `shouldContain` [x])

shouldNotOffer :: [Text] -> [Text] -> Expectation
shouldNotOffer vs = mapM_ (\x -> vs `shouldNotContain` [x])

-- | The candidate values for a request, against 'project'.
valuesFor :: [Text] -> IO [Text]
valuesFor ws = map candValue . resCandidates <$> complete (memResolver project) ws

respondTo :: [Text] -> IO Response
respondTo = complete (memResolver project)

describesAs :: [Text] -> IO [(Text, Maybe Text)]
describesAs ws =
  map (\c -> (candValue c, candDesc c)) . resCandidates <$> complete (memResolver project) ws

spec :: Spec
spec = do
  describe "subcommands and options" $ do
    it "offers the subcommands of spec 11.1" $ do
      vs <- valuesFor [""]
      vs `shouldOffer` ["run", "eval", "check", "envs", "deps", "completion"]

    it "offers a subcommand's options once a dash is typed" $ do
      vs <- valuesFor ["check", "--"]
      vs `shouldOffer` ["--module", "--format", "--no-color"]

    it "completes the values of a closed-set option" $ do
      valuesFor ["check", "--format", ""] `shouldReturn` ["text", "json"]
      valuesFor ["run", "--arg-decode", "j"] `shouldReturn` ["json"]

    it "does not offer an option that is already on the line" $ do
      vs <- valuesFor ["check", "--no-color", "--"]
      vs `shouldNotOffer` ["--no-color"]
      vs `shouldOffer` ["--module"]

    it "keeps the two deps add sources exclusive" $ do
      vs <- valuesFor ["deps", "add", "thing", "--git", "u", "--"]
      vs `shouldOffer` ["--rev"]
      vs `shouldNotOffer` ["--url"]

    it "offers --rev only once --git is there" $ do
      vs <- valuesFor ["deps", "add", "thing", "--"]
      vs `shouldNotOffer` ["--rev"]

    it "completes the shells of `lask completion`" $
      valuesFor ["completion", ""] `shouldReturn` ["bash", "zsh", "fish"]

    it "completes dependency names from the project file" $
      valuesFor ["deps", "why", ""] `shouldReturn` ["terraform"]

  describe "paths" $ do
    it "filters --module to modules and directories" $ do
      vs <- valuesFor ["check", "--module", ""]
      vs `shouldOffer` ["main.lask", "ci.lask", "sub/"]
      vs `shouldNotOffer` ["notes.txt"]

    it "descends into a directory without a trailing space" $ do
      resp <- respondTo ["check", "--module", "sub/"]
      map candValue (resCandidates resp) `shouldBe` ["sub/nested.lask"]

    it "asks for no space while the candidates are all directories" $ do
      resp <- respondTo ["check", "--module", "s"]
      map candValue (resCandidates resp) `shouldBe` ["sub/"]
      (resDirective resp .&. dirNoSpace) `shouldBe` dirNoSpace

  describe "function names (spec 11.2)" $ do
    it "offers the callable functions of the entry module, with their summaries" $
      describesAs ["run", ""]
        `shouldReturn` [ ("build", Just "Build the project and publish the artifact."),
                         ("show_version", Just "Show the version."),
                         ("packaged", Just "Keeps its own environment."),
                         ("login", Just "A secret default must never be read, let alone shown.")
                       ]

    it "leaves out values, @hidden, internal, and Environment-taking functions" $ do
      vs <- valuesFor ["run", ""]
      vs `shouldNotOffer` ["envs"] -- a value binding, not a function
      vs `shouldNotOffer` ["scratch"] -- @hidden (spec 3.1)
      vs `shouldNotOffer` ["helper"] -- internal (spec 5)
      vs `shouldNotOffer` ["on_env"] -- positional Environment (spec 11.2)

    it "takes the functions from the module --module names" $
      valuesFor ["run", "--module", "ci.lask", ""] `shouldReturn` ["ci_only"]

    it "matches a kebab-case prefix and answers in kind" $ do
      valuesFor ["run", "show-"] `shouldReturn` ["show-version"]
      valuesFor ["run", "show_"] `shouldReturn` ["show_version"]
      evalNames <- valuesFor ["eval", ""]
      evalNames `shouldOffer` ["show_version"]

  describe "the boundary rule (spec 11.2)" $ do
    it "replaces lask's options with the function's parameters" $ do
      vs <- valuesFor ["run", "build", "--"]
      vs `shouldOffer` ["--out_dir", "--publish", "--env"]
      vs `shouldNotOffer` ["--module", "--format", "--arg-decode"]

    it "offers --help after the function name, but never -h" $ do
      vs <- valuesFor ["run", "build", "--"]
      vs `shouldOffer` ["--help"]
      vs `shouldNotOffer` ["-h"]

    it "does not offer a keyword parameter twice" $ do
      vs <- valuesFor ["run", "build", "--out_dir", "dist", "--"]
      vs `shouldNotOffer` ["--out_dir"]
      vs `shouldOffer` ["--publish"]

    it "does not offer an Environment keyword parameter at all" $
      valuesFor ["run", "packaged", "--"] `shouldReturn` ["--help"]

    it "stops completing after a bare --" $ do
      resp <- respondTo ["run", "build", "--", "--"]
      resCandidates resp `shouldBe` []
      (resDirective resp .&. dirNoFileComp) `shouldBe` 0

    it "completes a parameter in the --out-dir form when that is what is typed" $
      valuesFor ["run", "build", "--out-d"] `shouldReturn` ["--out-dir"]

  describe "parameter values" $ do
    it "completes a Bool parameter" $
      valuesFor ["run", "build", "--publish", ""] `shouldReturn` ["true", "false"]

    it "completes @complete @keys from the map literal, without evaluating it" $
      valuesFor ["run", "build", "--env", ""] `shouldReturn` ["local", "docker"]

    it "completes only directories for @complete @dir" $
      valuesFor ["run", "build", "--out_dir", ""] `shouldReturn` ["sub/"]

    it "completes the inline --opt=value form as a whole word" $
      valuesFor ["run", "build", "--env=d"] `shouldReturn` ["--env=docker"]

    it "never shows a secret default" $ do
      cs <- describesAs ["run", "login", "--"]
      concatMap (maybe "" T.unpack . snd) cs `shouldNotContain` "hunter2"

    it "names the positional being typed instead of guessing its value" $ do
      cs <- describesAs ["run", "build", ""]
      map fst cs `shouldBe` [activeHelpMarker]
      snd (head cs) `shouldBe` Just "target : String - Build target name."

  describe "degradation (spec 11.7)" $ do
    it "still finds declaration heads and keyword parameters in a module that does not parse" $ do
      let broken = [("main.lask", "deploy(--version: String = \"1\", --dry_run: Bool = false) = do {\n  $ echo #{version\n}\n\nteardown() = $ echo bye\n")]
          ask ws = map candValue . resCandidates <$> complete (memResolver broken) ws
      ask ["run", ""] `shouldReturn` ["deploy", "teardown"]
      vs <- ask ["run", "deploy", "--"]
      vs `shouldOffer` ["--version", "--dry_run"]

    it "falls back to the static grammar when the module is missing" $ do
      let ask ws = complete (memResolver []) ws
      resp <- ask ["run", ""]
      resCandidates resp `shouldBe` []
      vs <- map candValue . resCandidates <$> ask ["check", "--"]
      vs `shouldOffer` ["--module"]

  describe "the response itself" $ do
    it "renders values, descriptions and the directive line" $
      renderResponse (Response [Candidate "a" (Just "first"), Candidate "b" Nothing] 6)
        `shouldBe` "a\tfirst\nb\n:6\n"

    it "strips anything that would break the line protocol" $
      renderResponse (Response [Candidate "a\nb" (Just "one\ttwo\nthree")] 0)
        `shouldBe` "ab\tonetwothree\n:0\n"

    it "always answers with a directive, even with no candidates" $
      renderResponse (Response [] 0) `shouldBe` ":0\n"
