{-# LANGUAGE OverloadedStrings #-}

-- | lask cmd (spec 11.8).
module Cli.CmdSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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

    it "reports an option value the command line does not accept as a usage error (exit 4)" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["eval", "--stdout-encode", "xml", "f"] ""
        resExit r `shouldBe` 4
        resErr r `shouldSatisfy` isInfixOf "E-CLI-USAGE"
        resErr r `shouldSatisfy` isInfixOf "--stdout-encode"

    it "captures the whole output of a command, however long (spec 8.7)" $ \lask ->
      withProject [("main.lask", "f(): Number = length($[#local] { head -c 3000000 /dev/zero | tr -c x x })\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resOut r `shouldBe` "3000000\n"

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
        resOut r `shouldSatisfy` isInfixOf "  echo    #local  local\n"

    it "lists commands as JSON under --output json --help" $ \lask ->
      withProject proj $ \dir -> do
        r <- runLask lask dir ["cmd", "--output", "json", "--help"] ""
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
