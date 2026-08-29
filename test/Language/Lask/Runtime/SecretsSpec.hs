{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.SecretsSpec (spec) where

import Language.Lask.Runtime.Secrets
import Test.Hspec

spec :: Spec
spec = before_ resetSecretRegistryForTests . after_ resetSecretRegistryForTests $ do
  describe "secret masking (spec 12.8)" $ do
    it "leaves text alone when nothing is registered" $
      maskSecrets "terraform apply -var aws_secret_access_key=abcd1234"
        `shouldReturn` "terraform apply -var aws_secret_access_key=abcd1234"

    it "masks every occurrence of a registered value" $ do
      registerSecret "abcd1234"
      maskSecrets "key=abcd1234 again=abcd1234" `shouldReturn` "key=*** again=***"

    it "masks a registered value embedded in a larger command string" $ do
      registerSecret "d4k2GgGmiPQ6MLehdouDTPcMI+Ka0P9mtjcetOP/"
      maskSecrets "AWS_SECRET_ACCESS_KEY=\"d4k2GgGmiPQ6MLehdouDTPcMI+Ka0P9mtjcetOP/\" aws s3 sync"
        `shouldReturn` "AWS_SECRET_ACCESS_KEY=\"***\" aws s3 sync"

    it "registers short values too (spec 12.8 has no length exemption)" $ do
      registerSecret "42"
      maskSecrets "num=42" `shouldReturn` "num=***"

    it "does not register the empty string (a no-op match anyway)" $ do
      registerSecret ""
      maskSecrets "anything at all" `shouldReturn` "anything at all"

    it "matches the longest registered value first to avoid partial masking" $ do
      registerSecret "secret"
      registerSecret "secret-plus-more"
      maskSecrets "prefix secret-plus-more suffix" `shouldReturn` "prefix *** suffix"

    it "is a no-op for an unregistered value that happens to be short" $
      maskSecrets "us-west-1" `shouldReturn` "us-west-1"

    it "reflects registrations made after an earlier call" $ do
      -- Guards the mutable-state hazard the IO signature exists for:
      -- an earlier result must never be reused for a later call.
      maskSecrets "value=late-secret" `shouldReturn` "value=late-secret"
      registerSecret "late-secret"
      maskSecrets "value=late-secret" `shouldReturn` "value=***"
