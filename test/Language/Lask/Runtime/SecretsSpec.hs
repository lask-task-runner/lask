{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.SecretsSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Language.Lask.ErrorCode (ErrorCode (ERuntimeCommandNonzero))
import Language.Lask.Runtime.Secrets
import Language.Lask.Runtime.Value
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

    it "masks a short value only where it stands as a word of its own" $ do
      registerSecret "x"
      maskSecrets "$ echo x nonexistent example x-ray xx" `shouldReturn` "$ echo *** nonexistent example ***-ray xx"

    it "masks a short number as a value, not inside a longer one" $ do
      registerSecret "42"
      maskSecrets "num=42 port=8042 v42" `shouldReturn` "num=*** port=8042 v42"

    it "masks a value of 8 characters or more wherever it appears" $ do
      registerSecret "hunter22"
      maskSecrets "pw=xhunter22x" `shouldReturn` "pw=x***x"

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

  describe "multi-line secrets (spec 12.8)" $ do
    let pem = "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASC\r\nq1w2e3\n-----END PRIVATE KEY-----\n"
    it "masks each line of the value, as output is relayed a line at a time" $ do
      registerSecret pem
      maskSecrets "MIIEvQIBADANBgkqhkiG9w0BAQEFAASC" `shouldReturn` "***"
      maskSecrets "-----BEGIN PRIVATE KEY-----" `shouldReturn` "***"
    it "still masks the whole value where it appears whole" $ do
      registerSecret pem
      maskSecrets ("key=" <> pem) `shouldReturn` "key=***"
    it "does not register a line shorter than 8 characters on its own" $ do
      registerSecret pem
      maskSecrets "q1w2e3" `shouldReturn` "q1w2e3"
    it "keeps a single-line secret whole, however short" $ do
      registerSecret "q1w2"
      maskSecrets "q1w2" `shouldReturn` "***"

  describe "masking values and failures (spec 12.8)" $ do
    it "masks every string a value holds, map keys included" $ do
      registerSecret "tok-123"
      let v = VRecord (Map.fromList [("args", VArray (V.fromList [VString "a tok-123"])), ("m", VMap (Map.fromList [("tok-123", VNumber 1)]))])
      masked <- maskValue v
      masked `shouldBe` VRecord (Map.fromList [("args", VArray (V.fromList [VString "a ***"])), ("m", VMap (Map.fromList [("***", VNumber 1)]))])
    it "masks a failure's error value and frames, keeping its code" $ do
      registerSecret "tok-123"
      let lf = LaskFailure (Just ERuntimeCommandNonzero) (errorValue 1 "denied for tok-123\n") ["deploy (main.lask)"]
      masked <- maskFailure lf
      masked `shouldBe` LaskFailure (Just ERuntimeCommandNonzero) (errorValue 1 "denied for ***\n") ["deploy (main.lask)"]
