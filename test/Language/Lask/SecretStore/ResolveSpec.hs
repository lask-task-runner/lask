{-# LANGUAGE OverloadedStrings #-}

-- | Secret references (spec 9.8): recognizing them, the @vault@
-- provider against a fake Vault, and resolution as the environment
-- built-ins see it.
module Language.Lask.SecretStore.ResolveSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try)
import Data.Either (isLeft)
import Data.IORef (writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Runtime.Secrets (maskSecrets, resetSecretRegistryForTests)
import Language.Lask.Runtime.Value (LaskFailure (..))
import Language.Lask.SecretStore.FakeVault
import Language.Lask.SecretStore.Resolve
import Language.Lask.SecretStore.Types
import Language.Lask.SecretStore.Vault (VaultRef (..), parseVaultRef)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "detectRef" $ do
    it "leaves an ordinary value alone" $ do
      detectRef "abc" `shouldBe` Nothing
      detectRef "" `shouldBe` Nothing
      detectRef "{\"a\": 1}" `shouldBe` Nothing
      detectRef "x-{vault://a#b}" `shouldBe` Nothing
    it "recognizes a whole value of the form {scheme://body}" $
      detectRef "{vault://secret/app#password}" `shouldBe` Just (Right (RawRef "vault" "secret/app#password"))
    it "reports a reference with no closing brace as malformed, not as a value" $ do
      fmap errorCode (detectRef "{vault://secret/app#password") `shouldBe` Just (Just EIoSecretRef)
      fmap isLeft (detectRef "{vault://a#b}tail") `shouldBe` Just True

  describe "parseVaultRef" $ do
    it "takes a path, an optional version and a field" $ do
      parseVaultRef "secret/app#password" `shouldBe` Right (VaultRef "secret/app" Nothing "password")
      parseVaultRef "secret/cert?version=3#key" `shouldBe` Right (VaultRef "secret/cert" (Just 3) "key")
    it "rejects what it cannot read unambiguously" $
      mapM_
        (\b -> errorCode (parseVaultRef b) `shouldBe` Just EIoSecretRef)
        ["secret/app", "secret/app#", "/secret/app#k", "secret//app#k", "secret/app?version=0#k", "secret/app?v=1#k", "secret/../app#k"]

  describe "enabling a store" $ do
    it "lists the schemes LASK_SECRETS names" $
      enabledSchemes (Map.fromList [("LASK_SECRETS", "vault, op")]) `shouldBe` ["vault", "op"]
    it "refuses a scheme it does not know, or one not enabled" $ do
      unknown <- resolveWith [("LASK_SECRETS", "op"), ("X", "{op://a/b}")] "X"
      unknown `shouldFailWith` EIoSecretProvider
      disabled <- resolveWith [("X", "{vault://secret/app#password}")] "X"
      disabled `shouldFailWith` EIoSecretProvider
    it "refuses a Vault with no address or no credentials" $ do
      noAddr <- resolveWith [("LASK_SECRETS", "vault"), ("X", "{vault://secret/app#password}")] "X"
      noAddr `shouldFailWith` EIoSecretProvider
      noCreds <- resolveWith [("LASK_SECRETS", "vault"), ("VAULT_ADDR", "http://127.0.0.1:1"), ("X", "{vault://secret/app#password}")] "X"
      noCreds `shouldFailWith` EIoSecretProvider

  describe "resolving through Vault" $ do
    it "passes an ordinary value through, and reads nothing" $
      withFakeVault $ \fv -> do
        r <- resolveWith (vaultEnv fv <> [("X", "plain")]) "X"
        r `shouldBe` Right (Just "plain")
        requestsTo fv "" `shouldReturn` 0
    it "reads the latest version of a KV version 2 secret, or the one named" $
      withFakeVault $ \fv -> do
        latest <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app#password}")]) "X"
        latest `shouldBe` Right (Just "v2pass")
        first <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app?version=1#password}")]) "X"
        first `shouldBe` Right (Just "s3cr3t")
    it "reads a path on any other mount as written" $
      withFakeVault $ \fv -> do
        r <- resolveWith (vaultEnv fv <> [("X", "{vault://kv1/db#url}")]) "X"
        r `shouldBe` Right (Just "postgres://db")
    it "refuses a version on a mount that keeps none" $
      withFakeVault $ \fv -> do
        r <- resolveWith (vaultEnv fv <> [("X", "{vault://kv1/db?version=2#url}")]) "X"
        r `shouldFailWith` EIoSecretRef
    it "reads a secret once per run, so the fields of a dynamic secret agree" $
      withFakeVault $ \fv -> do
        sr <- newSecretResolverWith (Map.fromList (vaultEnv fv <> dynamicPair))
        values <- mapConcurrently (readEnvVar sr) (concat (replicate 5 ["AK", "SK"]))
        values `shouldBe` concat (replicate 5 [Just "AK1", Just "SK1"])
        requestsTo fv "GET aws/creds/deploy" `shouldReturn` 1
        requestsTo fv "GET auth/token/lookup-self" `shouldReturn` 1
    it "registers a resolved value for masking" $
      withFakeVault $ \fv -> do
        resetSecretRegistryForTests
        _ <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app#password}")]) "X"
        maskSecrets "echo v2pass" `shouldReturn` "echo ***"
    it "names a missing field, path or version" $
      withFakeVault $ \fv -> do
        field <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app#nope}")]) "X"
        field `shouldFailWith` EIoSecretNotFound
        either (\(LaskFailure _ v _) -> show v) (const "") field `shouldContain` "fields: password, user"
        path <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/nope#k}")]) "X"
        path `shouldFailWith` EIoSecretNotFound
        version <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app?version=9#password}")]) "X"
        version `shouldFailWith` EIoSecretNotFound
    it "reports a rejected token, or a path the token may not read" $
      withFakeVault $ \fv -> do
        badToken <- resolveWith (vaultEnvWith fv [("VAULT_TOKEN", "nope")] <> [("X", "{vault://secret/app#password}")]) "X"
        badToken `shouldFailWith` EIoSecretAuth
        forbidden <- resolveWith (vaultEnvWith fv [("VAULT_TOKEN", "limited")] <> [("X", "{vault://kv1/db#url}")]) "X"
        forbidden `shouldFailWith` EIoSecretAuth
    it "logs in with AppRole" $
      withFakeVault $ \fv -> do
        let env = vaultEnvWith fv [("VAULT_ROLE_ID", "role"), ("VAULT_SECRET_ID", "secret")]
        ok <- resolveWith (env <> [("X", "{vault://secret/app#password}")]) "X"
        ok `shouldBe` Right (Just "v2pass")
        bad <- resolveWith (vaultEnvWith fv [("VAULT_ROLE_ID", "role"), ("VAULT_SECRET_ID", "wrong")] <> [("X", "{vault://secret/app#password}")]) "X"
        bad `shouldFailWith` EIoSecretAuth
    it "falls back to the token `vault login` leaves" $
      withFakeVault $ \fv -> withSystemTempDirectory "home" $ \home -> do
        writeFile (home </> ".vault-token") "root\n"
        r <- resolveWith (vaultEnvWith fv [("HOME", home)] <> [("X", "{vault://secret/app#password}")]) "X"
        r `shouldBe` Right (Just "v2pass")
    it "reports a store it cannot reach, or one that is sealed" $ do
      down <- resolveWith [("LASK_SECRETS", "vault"), ("VAULT_ADDR", "http://127.0.0.1:1"), ("VAULT_TOKEN", "root"), ("X", "{vault://secret/app#password}")] "X"
      down `shouldFailWith` EIoSecretUnreachable
      withFakeVault $ \fv -> do
        writeIORef (fvSealed fv) True
        sealed <- resolveWith (vaultEnv fv <> [("X", "{vault://secret/app#password}")]) "X"
        sealed `shouldFailWith` EIoSecretUnreachable
    it "reports a malformed reference before asking the store anything" $ do
      r <- resolveWith [("LASK_SECRETS", "vault"), ("VAULT_ADDR", "http://127.0.0.1:1"), ("VAULT_TOKEN", "root"), ("X", "{vault://secret/app}")] "X"
      r `shouldFailWith` EIoSecretRef

  describe "the stages of a check" $
    it "establish readability without reading, and release what a read issued" $
      withFakeVault $ \fv -> do
        Right prov <- openProvider (Map.fromList (vaultEnv fv)) "vault"
        Just health <- pure (provHealth prov)
        health `shouldReturn` Right "active, v1.20.4"
        Right session <- provLogin prov
        sessSummary session `shouldBe` "token, no expiry, policies [root]"
        Right loc <- pure (provParse prov "aws/creds/deploy#access_key")
        Right _ <- provProbe prov session loc
        requestsTo fv "GET aws/creds/deploy" `shouldReturn` 0
        Right v1 <- pure (provParse prov "secret/app?version=1#password")
        provProbe prov session v1 `shouldReturn` Right "readable, version 1"
        Right d <- provRead prov session loc
        sdLease d `shouldBe` Just "aws/creds/deploy/L1"
        provRelease prov session "aws/creds/deploy/L1"
        requestsTo fv "PUT sys/leases/revoke" `shouldReturn` 1
  where
    dynamicPair = [("AK", "{vault://aws/creds/deploy#access_key}"), ("SK", "{vault://aws/creds/deploy#secret_key}")]

errorCode :: Either SecretError a -> Maybe ErrorCode
errorCode = either (Just . seCode) (const Nothing)

vaultEnv :: FakeVault -> [(String, String)]
vaultEnv fv = vaultEnvWith fv [("VAULT_TOKEN", "root")]

-- | An environment for the fake, with the credentials given. @HOME@
-- points nowhere unless given, so that a developer's own
-- @~/.vault-token@ is never picked up. Later entries win.
vaultEnvWith :: FakeVault -> [(String, String)] -> [(String, String)]
vaultEnvWith fv extra =
  [("LASK_SECRETS", "vault"), ("VAULT_ADDR", fvAddr fv), ("HOME", "/nonexistent")] <> extra

resolveWith :: [(String, String)] -> Text -> IO (Either LaskFailure (Maybe Text))
resolveWith env name = do
  sr <- newSecretResolverWith (Map.fromList env)
  try (readEnvVar sr name)

shouldFailWith :: (Show a) => Either LaskFailure a -> ErrorCode -> Expectation
shouldFailWith r code = case r of
  Left (LaskFailure c _ _) -> c `shouldBe` Just code
  Right v -> expectationFailure ("expected " <> show code <> ", got " <> show v)
