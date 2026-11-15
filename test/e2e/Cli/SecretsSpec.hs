{-# LANGUAGE OverloadedStrings #-}

-- | Secret references on the command line (spec 9.8, 11.10).
module Cli.SecretsSpec (spec) where

import Data.IORef (readIORef)
import Support.FakeVault (FakeVault (..), requestsTo, withFakeVault)
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
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
        j <- runLaskEnv lask dir (refs <> vaultWith fv "limited") ["secrets", "check", "--output", "json"] ""
        resExit j `shouldBe` 3
        resOut j `shouldContain` "\"code\":\"E-IO-SECRET-AUTH\""
        bad <- runLaskEnv lask dir (refs <> vaultWith fv "nope") ["secrets", "check"] ""
        resExit bad `shouldBe` 3
        resOut bad `shouldContain` "not checked: the store is not available"
