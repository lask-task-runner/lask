{-# LANGUAGE OverloadedStrings #-}

-- | Confirmation before a task runs (spec 5, 11.2).
module Cli.ConfirmSpec (spec) where

import Support.Harness
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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
