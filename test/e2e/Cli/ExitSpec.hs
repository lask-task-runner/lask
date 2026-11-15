{-# LANGUAGE OverloadedStrings #-}

-- | Exit codes and the encodings of what a run writes (spec 11.3, 13.1).
module Cli.ExitSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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

  describe "output encodings (spec 11.3, 13.1)" $ do
    it "encodes records as JSON by default" $ \lask ->
      withProject [("main.lask", "u() = {name: \"a\", age: 20}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "u"] ""
        r `shouldBe` Result 0 "{\"age\":20,\"name\":\"a\"}\n" ""
    it "keeps stdout and stderr formats apart (spec 11.1)" $ \lask ->
      withProject [("main.lask", "f(): Record<a: Number> = do {\n  log(\"hi\")\n  {a: 1}\n}\n")] $ \dir -> do
        -- JSON Lines on stderr, and the result as formatted JSON on stdout.
        r <- runLask lask dir ["eval", "--log-format", "json", "--output", "pretty-json", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "{\n    \"a\": 1\n}\n"
        resErr r `shouldSatisfy` isInfixOf "\"message\":\"hi\""
        -- The report of a command takes --output too, and its logs stay text.
        c <- runLask lask dir ["check", "--output", "pretty-json"] ""
        resOut c `shouldBe` "[]\n"
        -- The options they replace are gone.
        old <- runLask lask dir ["eval", "--format", "json", "f"] ""
        resExit old `shouldNotBe` 0
        enc <- runLask lask dir ["eval", "--stdout-encode", "text", "f"] ""
        resExit enc `shouldNotBe` 0
    it "prints raw text with --output text" $ \lask ->
      withProject [("main.lask", "s() = \"plain\"\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--output", "text", "s"] ""
        r `shouldBe` Result 0 "plain\n" ""
    it "prints Void as nothing" $ \lask ->
      withProject [("main.lask", "f() = for (x : []) {}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        r `shouldBe` Result 0 "" ""
