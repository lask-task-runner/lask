{-# LANGUAGE OverloadedStrings #-}

-- | The @vault@ provider (spec 9.8, 11.10) against a server that answers
-- each request as the example scripts it: the configuration it
-- refuses before any request, and how it reads each answer of the
-- stages of a check. The provider against a Vault that behaves as one
-- does is in "Language.Lask.SecretStore.ResolveSpec".
module Language.Lask.SecretStore.VaultSpec (spec) where

import qualified Data.Aeson as A
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.SecretStore.Types
import Language.Lask.SecretStore.Vault (vaultProvider)
import Network.HTTP.Types (mkStatus)
import Network.Wai (pathInfo, rawQueryString, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | An answer: status, body and the headers beyond its content type.
type Answer = (Int, A.Value, [(Text, Text)])

-- | Serve a script — the answer to @METHOD path@, and 404 to anything
-- else — and give the action the address and the requests made.
withScript :: [(Text, Answer)] -> (String -> IORef [Text] -> IO a) -> IO a
withScript script action = do
  seen <- newIORef []
  let app req respond = do
        let key = TE.decodeUtf8 (requestMethod req) <> " " <> T.intercalate "/" (drop 1 (pathInfo req)) <> TE.decodeUtf8 (rawQueryString req)
        atomicModifyIORef' seen (\ks -> (ks <> [key], ()))
        let (status, body, headers) = maybe (404, A.object [], []) id (lookup key script)
        respond $
          responseLBS
            (mkStatus status "")
            (("Content-Type", "application/json") : [(fromString (T.unpack k), TE.encodeUtf8 v) | (k, v) <- headers])
            (A.encode body)
  testWithApplication (pure app) $ \port -> action ("http://127.0.0.1:" <> show port) seen

-- | The provider for a configuration, which must be accepted.
providerFor :: [(String, String)] -> IO SecretProvider
providerFor env = vaultProvider (Map.fromList env) >>= either (\e -> fail (show e)) pure

-- | The provider with a token, at the address.
tokenAt :: String -> IO SecretProvider
tokenAt addr = providerFor [("VAULT_ADDR", addr), ("VAULT_TOKEN", "t")]

-- | The message of a configuration the provider refuses.
refused :: [(String, String)] -> IO (Maybe SecretError)
refused env = either Just (const Nothing) <$> vaultProvider (Map.fromList env)

session :: Session
session = Session "t" ""

locator :: SecretProvider -> Text -> Locator
locator p body = either (error . show) id (provParse p body)

codeOf :: Either SecretError a -> Maybe ErrorCode
codeOf = either (Just . seCode) (const Nothing)

errors :: [Text] -> A.Value
errors es = A.object [("errors", A.toJSON es)]

kv2Mount :: (Text, Answer)
kv2Mount = ("GET sys/internal/ui/mounts/secret/app", (200, A.object [("data", A.object [("path", "secret/"), ("type", "kv"), ("options", A.object [("version", "2")])])], []))

spec :: Spec
spec = do
  describe "the configuration (spec 9.8)" $ do
    it "refuses an address that is not an http or https URL" $
      refused [("VAULT_ADDR", "vault.example.com:8200"), ("VAULT_TOKEN", "t")]
        `shouldReturn` Just (SecretError EIoSecretProvider "vault: VAULT_ADDR must be an http:// or https:// URL, not 'vault.example.com:8200'")
    it "refuses half of an AppRole, either half" $ do
      refused [("VAULT_ADDR", "http://v"), ("VAULT_ROLE_ID", "r"), ("HOME", "/nonexistent")]
        `shouldReturn` Just (SecretError EIoSecretProvider "vault: VAULT_ROLE_ID is set but VAULT_SECRET_ID is not")
      refused [("VAULT_ADDR", "http://v"), ("VAULT_SECRET_ID", "s"), ("HOME", "/nonexistent")]
        `shouldReturn` Just (SecretError EIoSecretProvider "vault: VAULT_SECRET_ID is set but VAULT_ROLE_ID is not")
    it "refuses no credentials, an empty token file counting as none" $
      withSystemTempDirectory "vault-home" $ \home -> do
        writeFile (home </> ".vault-token") "  \n"
        r <- refused [("VAULT_ADDR", "http://v"), ("HOME", home), ("VAULT_TOKEN", "")]
        fmap seCode r `shouldBe` Just EIoSecretProvider
        fmap (T.isInfixOf "no credentials") (fmap seMessage r) `shouldBe` Just True
    it "refuses a VAULT_CACERT it cannot read a certificate from" $
      withSystemTempDirectory "vault-ca" $ \dir -> do
        writeFile (dir </> "ca.pem") "not a certificate\n"
        refused [("VAULT_ADDR", "https://v:8200"), ("VAULT_TOKEN", "t"), ("VAULT_CACERT", dir </> "ca.pem")]
          `shouldReturn` Just (SecretError EIoSecretProvider ("vault: VAULT_CACERT: no certificate could be read from '" <> T.pack (dir </> "ca.pem") <> "'"))
    it "takes the address without its trailing slashes" $ do
      p <- providerFor [("VAULT_ADDR", "https://vault.example.com:8200//"), ("VAULT_TOKEN", "t")]
      provTarget p `shouldBe` "https://vault.example.com:8200"
      provScheme p `shouldBe` "vault"
    it "sends the namespace with every request" $
      withScript [("GET sys/health", (200, A.object [], []))] $ \addr _ -> do
        p <- providerFor [("VAULT_ADDR", addr), ("VAULT_TOKEN", "t"), ("VAULT_NAMESPACE", "team")]
        maybe (pure (Left (SecretError EIoSecretProvider ""))) id (provHealth p) `shouldReturn` Right "active"

  describe "the reachable stage (spec 11.10)" $
    it "tells an active node, a standby and a performance standby from a server that cannot serve reads" $
      mapM_
        ( \(status, expected) -> withScript [("GET sys/health", (status, A.object [("version", "1.21.0")], []))] $ \addr _ -> do
            p <- tokenAt addr
            r <- maybe (fail "no health stage") id (provHealth p)
            either (\e -> Left (snd (T.breakOnEnd (T.pack addr <> " ") (seMessage e)))) Right r `shouldBe` expected
        )
        [ (200, Right "active, v1.21.0"),
          (429, Right "standby, v1.21.0"),
          (473, Right "performance standby, v1.21.0"),
          (472, Left "is a disaster recovery secondary"),
          (501, Left "is not initialized"),
          (503, Left "is sealed"),
          (500, Left "is answering with status 500")
        ]

  describe "the auth stage (spec 11.10)" $ do
    it "summarizes an AppRole login by its lease and policies" $
      withScript [("POST auth/approle/login", (200, A.object [("auth", A.object [("client_token", "issued"), ("lease_duration", A.Number 7385), ("policies", A.toJSON ["default" :: Text])])], []))] $ \addr _ -> do
        p <- providerFor [("VAULT_ADDR", addr), ("VAULT_ROLE_ID", "r"), ("VAULT_SECRET_ID", "s")]
        r <- provLogin p
        fmap sessSummary r `shouldBe` Right "approle, ttl 2h3m5s, policies [default]"
        fmap sessToken r `shouldBe` Right "issued"
    it "reports an AppRole refused with 400 or 403 as E-IO-SECRET-AUTH, with Vault's errors" $
      mapM_
        ( \status -> withScript [("POST auth/approle/login", (status, errors ["invalid  role\nID"], []))] $ \addr _ -> do
            p <- providerFor [("VAULT_ADDR", addr), ("VAULT_ROLE_ID", "r"), ("VAULT_SECRET_ID", "s")]
            provLogin p `shouldReturnError` SecretError EIoSecretAuth "vault: AppRole login failed: invalid role ID"
        )
        [400, 403]
    it "reports a login answered with no token, or another status, as unexpected" $ do
      withScript [("POST auth/approle/login", (200, A.object [], []))] $ \addr _ -> do
        p <- providerFor [("VAULT_ADDR", addr), ("VAULT_ROLE_ID", "r"), ("VAULT_SECRET_ID", "s")]
        provLogin p `shouldReturnError` SecretError EIoSecretUnreachable "vault: unexpected response (status 200)"
      withScript [("GET auth/token/lookup-self", (500, errors ["internal error"], []))] $ \addr _ -> do
        p <- tokenAt addr
        provLogin p `shouldReturnError` SecretError EIoSecretUnreachable "vault: unexpected response (status 500): internal error"
    it "reports a token from the token file as such when it is rejected" $
      withSystemTempDirectory "vault-home" $ \home -> withScript [("GET auth/token/lookup-self", (403, errors ["permission denied"], []))] $ \addr _ -> do
        writeFile (home </> ".vault-token") "from-file\n"
        p <- providerFor [("VAULT_ADDR", addr), ("HOME", home)]
        provLogin p `shouldReturnError` SecretError EIoSecretAuth "vault: the token from ~/.vault-token was rejected (expired or revoked?): permission denied"
    it "summarizes a token with no TTL or policies it can read" $
      withScript [("GET auth/token/lookup-self", (200, A.object [("data", A.object [])], []))] $ \addr _ -> do
        p <- tokenAt addr
        fmap sessSummary <$> provLogin p `shouldReturn` Right "token, ttl unknown, policies []"

  describe "redirects (spec 9.8)" $ do
    it "refuses a redirect other than a standby's, one with no location, and one to another kind of place" $
      mapM_
        ( \(answer, expected) -> withScript [("GET sys/health", answer)] $ \addr _ -> do
            p <- tokenAt addr
            r <- maybe (fail "no health stage") id (provHealth p)
            either (\e -> Just (seCode e, snd (T.breakOnEnd (T.pack addr <> ": ") (seMessage e)))) (const Nothing) r `shouldBe` Just (EIoSecretUnreachable, expected)
        )
        [ ((302, A.object [], [("Location", "/v1/sys/health")]), "refused a redirect with status 302"),
          ((307, A.object [], []), "redirected with no location"),
          ((307, A.object [], [("Location", "ftp://elsewhere/")]), "refused a redirect to 'ftp://elsewhere/'"),
          ((308, A.object [], [("Location", "//elsewhere/v1/sys/health")]), "refused a redirect to '//elsewhere/v1/sys/health'")
        ]
    it "follows the one redirect of a standby to the active node, and refuses a second" $ do
      withScript [("GET sys/health", (307, A.object [], [("Location", "/v1/active/sys/health")])), ("GET active/sys/health", (200, A.object [], []))] $ \addr seen -> do
        p <- tokenAt addr
        maybe (fail "no health stage") id (provHealth p) `shouldReturn` Right "active"
        readIORef seen `shouldReturn` ["GET sys/health", "GET active/sys/health"]
      withScript [("GET sys/health", (307, A.object [], [("Location", "/v1/again")])), ("GET again", (308, A.object [], [("Location", "/v1/sys/health")]))] $ \addr _ -> do
        p <- tokenAt addr
        r <- maybe (fail "no health stage") id (provHealth p)
        either (Left . seMessage) Right r `shouldBe` Left ("vault: " <> T.pack addr <> ": redirected more than once")
    it "says when it cannot connect at all" $ do
      p <- tokenAt "http://127.0.0.1:1"
      r <- maybe (fail "no health stage") id (provHealth p)
      codeOf r `shouldBe` Just EIoSecretUnreachable

  describe "probing a reference (spec 11.10)" $ do
    it "reports a capabilities answer that is not 200 as unexpected" $
      withScript [kv2Mount, ("POST sys/capabilities-self", (500, errors ["boom"], []))] $ \addr _ -> do
        p <- tokenAt addr
        provProbe p session (locator p "secret/app#password") `shouldReturnError` SecretError EIoSecretUnreachable "vault: unexpected response (status 500): boom"
    it "takes the root capability as read, and a path off a KV version 2 mount as readable without metadata" $
      withScript [("POST sys/capabilities-self", (200, A.object [("kv1/db", A.toJSON ["root" :: Text])], []))] $ \addr seen -> do
        p <- tokenAt addr
        provProbe p session (locator p "kv1/db#url") `shouldReturn` Right "readable"
        readIORef seen >>= (`shouldSatisfy` notElem "GET kv1/metadata/db")
    it "reads the metadata of a KV version 2 path: the version asked for, a deleted one and a missing path" $ do
      let caps = ("POST sys/capabilities-self", (200, A.object [("secret/data/app", A.toJSON ["read" :: Text])], []))
          meta versions = ("GET secret/metadata/app", (200, A.object [("data", A.object [("current_version", A.Number 3), ("versions", versions)])], []))
          v n deleted destroyed = (n, A.object [("deletion_time", A.String deleted), ("destroyed", A.Bool destroyed)])
      withScript [kv2Mount, caps, meta (A.object [v "1" "" False, v "2" "2026-01-01T00:00:00Z" False, v "3" "" True])] $ \addr _ -> do
        p <- tokenAt addr
        provProbe p session (locator p "secret/app?version=1#password") `shouldReturn` Right "readable, version 1"
        provProbe p session (locator p "secret/app?version=2#password")
          `shouldReturnError` SecretError EIoSecretNotFound "vault: 'secret/app?version=2': version 2 does not exist or was deleted (latest 3)"
        provProbe p session (locator p "secret/app#password")
          `shouldReturnError` SecretError EIoSecretNotFound "vault: 'secret/app': version 3 does not exist or was deleted (latest 3)"
      withScript [kv2Mount, caps] $ \addr _ -> do
        p <- tokenAt addr
        provProbe p session (locator p "secret/app#password") `shouldReturnError` SecretError EIoSecretNotFound "vault: 'secret/app': no secret at this path"
      withScript [kv2Mount, caps, ("GET secret/metadata/app", (500, A.object [], []))] $ \addr _ -> do
        p <- tokenAt addr
        provProbe p session (locator p "secret/app#password") `shouldReturnError` SecretError EIoSecretUnreachable "vault: unexpected response (status 500)"

  describe "reading a reference (spec 9.8)" $ do
    it "names a missing path, a path the store refuses to read, and another status, each its own way" $
      mapM_
        ( \(answer, expected) -> withScript [kv2Mount, ("GET secret/data/app", answer)] $ \addr _ -> do
            p <- tokenAt addr
            provRead p session (locator p "secret/app#password") `shouldReturnError` expected
        )
        [ ((404, A.object [], []), SecretError EIoSecretNotFound "vault: 'secret/app': no secret at this path"),
          ((400, errors ["bad path"], []), SecretError EIoSecretNotFound "vault: 'secret/app': the store refused the request: bad path"),
          ((403, errors ["permission denied"], []), SecretError EIoSecretAuth "vault: permission denied reading 'secret/app': permission denied"),
          ((200, A.object [("data", A.object [("data", A.Null)])], []), SecretError EIoSecretNotFound "vault: 'secret/app': the version was deleted"),
          ((502, A.object [], []), SecretError EIoSecretUnreachable "vault: unexpected response (status 502)")
        ]
    it "keeps a field that is not a string as JSON, drops a null one, and keeps a lease" $
      withScript [("GET dyn/x", (200, A.object [("lease_id", "dyn/x/L1"), ("data", A.object [("n", A.Number 1), ("o", A.object [("a", A.Bool True)]), ("gone", A.Null)])], []))] $ \addr _ -> do
        p <- tokenAt addr
        r <- provRead p session (locator p "dyn/x#n")
        fmap sdFields r `shouldBe` Right (Map.fromList [("n", "1"), ("o", "{\"a\":true}")])
        fmap sdLease r `shouldBe` Right (Just "dyn/x/L1")
    it "refuses a version on a path the store does not say is on a KV version 2 mount" $
      withScript [] $ \addr _ -> do
        p <- tokenAt addr
        provRead p session (locator p "kv1/db?version=2#url")
          `shouldReturnError` SecretError EIoSecretRef "vault: 'kv1/db' is not on a KV version 2 mount, so it has no versions"
  where
    shouldReturnError act e = act >>= (`shouldBe` Left e) . either Left (const (Right ()))
