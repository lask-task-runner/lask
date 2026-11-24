{-# LANGUAGE OverloadedStrings #-}

-- | Command execution logs, log, and observability (spec 12, 13.3, 15.12).
module Cli.LogsSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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
    it "emits JSON Lines with stream/event fields under --log-format json" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["run", "--log-format", "json", "--trace-id", "t-9", "f"] ""
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
        r <- runLask lask dir ["run", "--log-format", "json", "f"] ""
        resExit r `shouldBe` 0
        resErr r `shouldSatisfy` isInfixOf "\"code\":3"
        resErr r `shouldSatisfy` isInfixOf "\"level\":\"warn\""
    it "reports static errors as JSON Lines on stderr" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\ny: Number = true\nf() = 1\n")] $ \dir -> do
        r <- runLask lask dir ["run", "--log-format", "json", "f"] ""
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
    it "carries a level and a message as JSON under --log-format json (12.2)" $ \lask ->
      withProject [("main.lask", "f() = do {\n  log(\"building\")\n  \"done\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--log-format", "json", "f"] ""
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
        r <- runLask lask dir ["run", "--log-format", "json", "leak"] ""
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
        resErr r `shouldSatisfy` isInfixOf "  at inner (main.lask:1:19)\n  at outer (main.lask:2:19)\n"
    it "puts in each frame where in its function the failure was, and names anonymous ones by what made them (12.4)" $ \lask ->
      withProject
        [ ( "main.lask",
            "ratio(a: Number, b: Number) = a / b\n\
            \halves(xs: Array<Number>) = map(xs, \\(x: Number) -> ratio(x, 0))\n\
            \late() = do {\n\
            \  h = async halves([1])\n\
            \  await h\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "late"] ""
          resExit r `shouldBe` 2
          resErr r
            `shouldSatisfy` isInfixOf
              "  at ratio (main.lask:1:31)\n\
              \  at lambda (main.lask:2:53)\n\
              \  at halves (main.lask:2:29)\n\
              \  at async (main.lask:4:13)\n\
              \  at late (main.lask:5:3)\n"
    it "emits call/return events with the given trace id" $ \lask ->
      withProject [("main.lask", "f(x: Number): Number = x + 1\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--log-format", "json", "--trace-id", "t-1", "f", "41"] ""
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
          r <- runLask lask dir ["eval", "--log-format", "json", "f"] ""
          resExit r `shouldBe` 0
          resErr r `shouldSatisfy` isInfixOf "\"kind\":\"fail\""
