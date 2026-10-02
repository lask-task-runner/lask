{-# LANGUAGE OverloadedStrings #-}

-- | The @vault@ scheme (spec 9.8): HashiCorp Vault, and OpenBao,
-- which serves the same HTTP API.
--
-- A reference is @vault://\<path\>[?version=\<n\>]#\<field\>@, with the
-- path written as @vault kv get@ takes it. Whether the path is on a KV
-- version 2 mount — where the API inserts @data/@ and keeps versions —
-- is asked of the server, as the Vault CLI does, so a reference never
-- has to spell out the API layout.
--
-- Configuration comes from Vault's own variables (@VAULT_ADDR@,
-- @VAULT_TOKEN@, @VAULT_NAMESPACE@, @VAULT_CACERT@) so that an
-- environment already set up for the Vault CLI works unchanged, plus
-- @VAULT_ROLE_ID@ / @VAULT_SECRET_ID@ for AppRole login.
module Language.Lask.SecretStore.Vault
  ( VaultRef (..),
    parseVaultRef,
    redirectTarget,
    vaultProvider,
  )
where

import Control.Exception (try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.X509.CertificateStore (readCertificateStore)
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.SecretStore.Types
import qualified Network.Connection as NC
import Network.HTTP.Client hiding (host, path)
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import Network.HTTP.Types (hContentType, hLocation)
import qualified Network.TLS as TLS
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | A parsed @vault://@ reference.
data VaultRef = VaultRef
  { vrPath :: Text,
    vrVersion :: Maybe Int,
    vrField :: Text
  }
  deriving (Show, Eq)

-- | Parse the body of a @vault://@ reference.
--
-- >>> parseVaultRef (T.pack "secret/app#password")
-- Right (VaultRef {vrPath = "secret/app", vrVersion = Nothing, vrField = "password"})
-- >>> parseVaultRef (T.pack "secret/cert?version=3#key")
-- Right (VaultRef {vrPath = "secret/cert", vrVersion = Just 3, vrField = "key"})
-- >>> either (Just . seMessage) (const Nothing) (parseVaultRef (T.pack "secret/app"))
-- Just "malformed vault reference 'secret/app': a field is required, as in 'secret/app#field'"
parseVaultRef :: Text -> Either SecretError VaultRef
parseVaultRef body = do
  let (pathQuery, fieldPart) = T.breakOn "#" body
  field <- case T.stripPrefix "#" fieldPart of
    Just f | not (T.null f) -> Right f
    _ -> bad ("a field is required, as in '" <> pathQuery <> "#field'")
  let (path, queryPart) = T.breakOn "?" pathQuery
      segments = T.splitOn "/" path
  if T.null path || any (`elem` ["", ".", ".."]) segments
    then bad "the path must be one or more '/'-separated names, with no leading or trailing '/'"
    else Right ()
  version <- case T.stripPrefix "?" queryPart of
    Nothing -> Right Nothing
    Just q -> case T.splitOn "&" q of
      [param] | Just v <- T.stripPrefix "version=" param -> case readMaybe (T.unpack v) of
        Just n | n > 0 -> Right (Just n)
        _ -> bad "version must be a positive integer"
      _ -> bad "the only parameter a reference takes is version"
  Right (VaultRef path version field)
  where
    bad why = Left (SecretError EIoSecretRef ("malformed vault reference '" <> body <> "': " <> why))

-- | The identity of one read: two fields of the same path and version
-- share it, and so a read.
locate :: Text -> Either SecretError Locator
locate body = do
  r <- parseVaultRef body
  let key = vrPath r <> maybe "" (\v -> "?version=" <> T.pack (show v)) (vrVersion r)
  Right (Locator key (vrField r) body)

data Config = Config
  { cfgAddr :: Text,
    cfgNamespace :: Maybe Text,
    cfgAuth :: Auth,
    cfgManager :: Manager
  }

data Auth = AuthToken Text Text | AuthAppRole Text Text

-- | The provider, configured from the environment. A configuration
-- problem is 'EIoSecretProvider' and is reported before any request.
vaultProvider :: Map String String -> IO (Either SecretError SecretProvider)
vaultProvider env = case nonEmpty "VAULT_ADDR" of
  Nothing -> pure (configError "VAULT_ADDR is not set")
  Just addr0
    | not (any (`T.isPrefixOf` addr0) ["http://", "https://"]) ->
        pure (configError ("VAULT_ADDR must be an http:// or https:// URL, not '" <> addr0 <> "'"))
    | otherwise -> do
        let addr = T.dropWhileEnd (== '/') addr0
        tokenFile <- readTokenFile
        let auth = case (nonEmpty "VAULT_TOKEN", nonEmpty "VAULT_ROLE_ID", nonEmpty "VAULT_SECRET_ID") of
              (Just t, _, _) -> Right (AuthToken "token" t)
              (_, Just r, Just s) -> Right (AuthAppRole r s)
              (_, Just _, Nothing) -> Left "VAULT_ROLE_ID is set but VAULT_SECRET_ID is not"
              (_, Nothing, Just _) -> Left "VAULT_SECRET_ID is set but VAULT_ROLE_ID is not"
              _ -> maybe (Left noCredentials) (Right . AuthToken "token file") tokenFile
        case auth of
          Left why -> pure (configError why)
          Right a -> do
            mgr <- newManagerFor addr (nonEmpty "VAULT_CACERT")
            pure $ case mgr of
              Left why -> configError why
              Right m -> Right (provider (Config addr (nonEmpty "VAULT_NAMESPACE") a m))
  where
    nonEmpty k = case Map.lookup k env of
      Just v | not (null v) -> Just (T.pack v)
      _ -> Nothing
    -- Where `vault login` leaves its token.
    readTokenFile = case Map.lookup "HOME" env of
      Nothing -> pure Nothing
      Just home -> do
        let f = home </> ".vault-token"
        exists <- doesFileExist f
        if exists
          then (\t -> if T.null t then Nothing else Just t) . T.strip . T.pack <$> readFile f
          else pure Nothing
    noCredentials = "no credentials: set VAULT_TOKEN, or VAULT_ROLE_ID and VAULT_SECRET_ID, or run `vault login`"
    configError why = Left (SecretError EIoSecretProvider ("vault: " <> why))

-- | A manager that trusts the system's certificate authorities, or
-- only the one @VAULT_CACERT@ names, as the Vault CLI does. Responses
-- are bounded, so that a store that hangs does not hang the run.
newManagerFor :: Text -> Maybe Text -> IO (Either Text Manager)
newManagerFor addr caCert = case caCert of
  Nothing -> Right <$> newManager tlsManagerSettings {managerResponseTimeout = timeout}
  Just path -> do
    store <- readCertificateStore (T.unpack path)
    case store of
      Nothing -> pure (Left ("VAULT_CACERT: no certificate could be read from '" <> path <> "'"))
      Just s -> do
        let host = T.unpack (T.takeWhile (\c -> c /= ':' && c /= '/') (dropScheme addr))
            params0 = TLS.defaultParamsClient host ""
            params = params0 {TLS.clientShared = (TLS.clientShared params0) {TLS.sharedCAStore = s}}
        Right <$> newManager (mkManagerSettings (NC.TLSSettings params) Nothing) {managerResponseTimeout = timeout}
  where
    dropScheme a = snd (T.breakOnEnd "://" a)
    timeout = responseTimeoutMicro 10000000

provider :: Config -> SecretProvider
provider cfg =
  SecretProvider
    { provScheme = "vault",
      provTarget = cfgAddr cfg,
      provParse = locate,
      provHealth = Just (health cfg),
      provLogin = login cfg,
      provProbe = probe cfg,
      provRead = readSecret cfg,
      provRelease = release cfg
    }

-- HTTP ---------------------------------------------------------------------

data Reply = Reply
  { replyStatus :: Int,
    replyBody :: Maybe A.Value
  }

-- | One request to @/v1/\<path\>@. A failure to get any response is
-- 'EIoSecretUnreachable'. The message is built from the failure alone:
-- the request carries the token in a header, so it is never shown.
--
-- Redirects are not left to the HTTP client, which would follow any of
-- them, several times over, with the token still attached. A standby
-- node answers 307 or 308 with the active node's address, and that one
-- redirect is followed, as the Vault CLI follows it; any other
-- redirect, a second one, or one from https to http is a failure.
call :: Config -> Maybe Session -> BC.ByteString -> Text -> Maybe A.Value -> IO (Either SecretError Reply)
call cfg session verb path body = do
  r <- try $ do
    let url = cfgAddr cfg <> "/v1/" <> path
    resp <- send url
    case statusCodeOf resp of
      s
        | s `elem` [307, 308] -> case redirectTarget url (locationOf resp) of
            Left why -> pure (Left why)
            Right next -> do
              resp' <- send next
              pure $
                if isRedirect (statusCodeOf resp')
                  then Left "redirected more than once"
                  else Right resp'
        | isRedirect s -> pure (Left ("refused a redirect with status " <> T.pack (show s)))
        | otherwise -> pure (Right resp)
  pure $ case r of
    Left e -> Left (unreachable (describe e))
    Right (Left why) -> Left (unreachable why)
    Right (Right resp) ->
      Right (Reply (statusCodeOf resp) (A.decode (responseBody resp)))
  where
    send url = do
      req0 <- parseRequest (T.unpack url)
      let headers =
            [("X-Vault-Token", TE.encodeUtf8 (sessToken s)) | Just s <- [session]]
              <> [("X-Vault-Namespace", TE.encodeUtf8 ns) | Just ns <- [cfgNamespace cfg]]
              <> [(hContentType, "application/json") | Just _ <- [body]]
          req =
            req0
              { method = verb,
                requestHeaders = headers,
                requestBody = maybe (requestBody req0) (RequestBodyLBS . A.encode) body,
                redactHeaders = Set.insert "X-Vault-Token" (redactHeaders req0),
                redirectCount = 0
              }
      httpLbs req (cfgManager cfg)
    unreachable why = SecretError EIoSecretUnreachable ("vault: " <> cfgAddr cfg <> ": " <> why)
    statusCodeOf = fromEnum . responseStatus
    isRedirect s = s >= 300 && s < 400
    locationOf resp = TE.decodeUtf8Lenient <$> lookup hLocation (responseHeaders resp)
    describe e = case e of
      HttpExceptionRequest _ content -> case content of
        ConnectionFailure inner -> "cannot connect: " <> T.pack (show inner)
        ConnectionTimeout -> "connection timed out"
        ResponseTimeout -> "no response within 10 seconds"
        InternalException inner -> T.pack (show inner)
        other -> T.pack (takeWhile (/= '\n') (show other))
      InvalidUrlException url why -> "invalid URL '" <> T.pack url <> "': " <> T.pack why

-- | Where a redirect from a URL leads, or why it is not followed. A
-- path is taken against the URL's own origin; a redirect may not leave
-- https for http, which would send the token in the clear.
--
-- >>> redirectTarget (T.pack "https://standby:8200/v1/a") (Just (T.pack "https://active:8200/v1/a"))
-- Right "https://active:8200/v1/a"
-- >>> redirectTarget (T.pack "http://standby:8200/v1/a") (Just (T.pack "/v1/b"))
-- Right "http://standby:8200/v1/b"
-- >>> redirectTarget (T.pack "https://standby:8200/v1/a") (Just (T.pack "http://active:8200/v1/a"))
-- Left "refused a redirect from https to http: 'http://active:8200/v1/a'"
redirectTarget :: Text -> Maybe Text -> Either Text Text
redirectTarget from location = case location of
  Nothing -> Left "redirected with no location"
  Just loc
    | "https://" `T.isPrefixOf` loc -> Right loc
    | "http://" `T.isPrefixOf` loc ->
        if "https://" `T.isPrefixOf` from
          then Left ("refused a redirect from https to http: '" <> loc <> "'")
          else Right loc
    | "/" `T.isPrefixOf` loc, not ("//" `T.isPrefixOf` loc) -> Right (origin <> loc)
    | otherwise -> Left ("refused a redirect to '" <> loc <> "'")
  where
    (scheme, rest) = T.breakOn "://" from
    origin = scheme <> "://" <> T.takeWhile (/= '/') (T.drop 3 rest)

-- | The @errors@ Vault puts in a failed response, for a message.
vaultErrors :: Reply -> Text
vaultErrors reply = case replyBody reply >>= at ["errors"] of
  Just (A.Array es) | not (V.null es) -> ": " <> T.intercalate "; " [T.unwords (T.words e) | A.String e <- V.toList es]
  _ -> ""

at :: [Text] -> A.Value -> Maybe A.Value
at [] v = Just v
at (k : ks) (A.Object o) = KM.lookup (AK.fromText k) o >>= at ks
at _ _ = Nothing

textAt :: [Text] -> A.Value -> Maybe Text
textAt ks v = case at ks v of
  Just (A.String t) -> Just t
  _ -> Nothing

intAt :: [Text] -> A.Value -> Maybe Int
intAt ks v = case at ks v of
  Just (A.Number n) -> toBoundedInteger n
  _ -> Nothing

stringsAt :: [Text] -> A.Value -> [Text]
stringsAt ks v = case at ks v of
  Just (A.Array xs) -> [t | A.String t <- V.toList xs]
  _ -> []

-- Stages ---------------------------------------------------------------------

-- | Reachability, unauthenticated. A standby answers reads by
-- forwarding them, so it counts as reachable; a sealed or
-- uninitialized server cannot answer any.
health :: Config -> IO (Either SecretError Text)
health cfg = do
  r <- call cfg Nothing "GET" "sys/health" Nothing
  pure $ do
    reply <- r
    let version = maybe "" (", v" <>) (replyBody reply >>= textAt ["version"])
        down why = Left (SecretError EIoSecretUnreachable ("vault: " <> cfgAddr cfg <> " is " <> why))
    case replyStatus reply of
      200 -> Right ("active" <> version)
      429 -> Right ("standby" <> version)
      473 -> Right ("performance standby" <> version)
      472 -> down "a disaster recovery secondary"
      501 -> down "not initialized"
      503 -> down "sealed"
      s -> down ("answering with status " <> T.pack (show s))

login :: Config -> IO (Either SecretError Session)
login cfg = case cfgAuth cfg of
  AuthToken how token -> do
    let s0 = Session token ""
    r <- call cfg (Just s0) "GET" "auth/token/lookup-self" Nothing
    pure $ do
      reply <- r
      case replyStatus reply of
        200 ->
          let d = replyBody reply
           in Right s0 {sessSummary = summary how (d >>= intAt ["data", "ttl"]) (maybe [] (stringsAt ["data", "policies"]) d)}
        403 -> Left (authError ("the token from " <> tokenSource how <> " was rejected (expired or revoked?)" <> vaultErrors reply))
        s -> Left (unexpected s reply)
  AuthAppRole roleId secretId -> do
    r <-
      call cfg Nothing "POST" "auth/approle/login" . Just $
        A.object [("role_id", A.String roleId), ("secret_id", A.String secretId)]
    pure $ do
      reply <- r
      case (replyStatus reply, replyBody reply >>= textAt ["auth", "client_token"]) of
        (200, Just token) ->
          let d = replyBody reply
           in Right (Session token (summary "approle" (d >>= intAt ["auth", "lease_duration"]) (maybe [] (stringsAt ["auth", "policies"]) d)))
        (s, _)
          | s `elem` [400, 403] -> Left (authError ("AppRole login failed" <> vaultErrors reply))
          | otherwise -> Left (unexpected s reply)
  where
    tokenSource how = if how == "token" then "VAULT_TOKEN" else "~/.vault-token"
    summary how ttl policies =
      how
        <> ", "
        <> maybe "ttl unknown" (\t -> if t == 0 then "no expiry" else "ttl " <> duration t) ttl
        <> ", policies ["
        <> T.intercalate ", " policies
        <> "]"
    authError = SecretError EIoSecretAuth . ("vault: " <>)

-- | Where a path lives: on a KV version 2 mount, the API reads
-- @\<mount\>data/\<rest\>@ and keeps versions and metadata; anywhere
-- else the path is read as written. Asked of the server; when it will
-- not say (no permission on the mount listing), the path is taken as
-- written, which is what the Vault CLI falls back to as well.
data Layout = Kv2 Text Text | Plain Text

layoutOf :: Config -> Session -> VaultRef -> IO (Either SecretError Layout)
layoutOf cfg session ref = do
  r <- call cfg (Just session) "GET" ("sys/internal/ui/mounts/" <> vrPath ref) Nothing
  let layout = case r of
        Right reply
          | replyStatus reply == 200,
            Just d <- replyBody reply,
            Just mount <- textAt ["data", "path"] d,
            textAt ["data", "type"] d == Just "kv",
            textAt ["data", "options", "version"] d == Just "2",
            Just rest <- T.stripPrefix mount (vrPath ref) ->
              Kv2 mount rest
        _ -> Plain (vrPath ref)
  pure $ case (layout, vrVersion ref) of
    (Plain _, Just _) ->
      Left (SecretError EIoSecretRef ("vault: '" <> vrPath ref <> "' is not on a KV version 2 mount, so it has no versions"))
    _ -> Right layout

dataPath :: Layout -> Text
dataPath (Kv2 mount rest) = mount <> "data/" <> rest
dataPath (Plain p) = p

readSecret :: Config -> Session -> Locator -> IO (Either SecretError SecretData)
readSecret cfg session loc = withRef loc $ \ref -> do
  layoutE <- layoutOf cfg session ref
  case layoutE of
    Left e -> pure (Left e)
    Right layout -> do
      let query = maybe "" (\v -> "?version=" <> T.pack (show v)) (vrVersion ref)
      r <- call cfg (Just session) "GET" (dataPath layout <> query) Nothing
      pure $ do
        reply <- r
        let fields = case layout of
              Kv2 _ _ -> replyBody reply >>= at ["data", "data"]
              Plain _ -> replyBody reply >>= at ["data"]
        case (replyStatus reply, fields) of
          (200, Just (A.Object o)) ->
            Right
              SecretData
                { sdFields = Map.fromList [(AK.toText k, fieldText v) | (k, v) <- KM.toList o, v /= A.Null],
                  sdLease = replyBody reply >>= textAt ["lease_id"] >>= \l -> if T.null l then Nothing else Just l
                }
          (200, _) -> Left (notFound ref "the version was deleted")
          (404, _) -> Left (notFound ref "no secret at this path")
          (403, _) -> Left (denied ref reply)
          (400, _) -> Left (notFound ref ("the store refused the request" <> vaultErrors reply))
          (s, _) -> Left (unexpected s reply)
  where
    fieldText v = case v of
      A.String t -> t
      other -> TE.decodeUtf8 (BL.toStrict (A.encode other))

-- | Whether a reference can be read, without reading it: the token's
-- capabilities on the path, then, on a KV version 2 mount, whether
-- the version exists — from the metadata, which holds no values.
probe :: Config -> Session -> Locator -> IO (Either SecretError Text)
probe cfg session loc = withRef loc $ \ref -> do
  layoutE <- layoutOf cfg session ref
  case layoutE of
    Left e -> pure (Left e)
    Right layout -> do
      let path = dataPath layout
      capsR <- call cfg (Just session) "POST" "sys/capabilities-self" (Just (A.object [("paths", A.toJSON [path])]))
      case capsR of
        Left e -> pure (Left e)
        Right reply
          | replyStatus reply /= 200 -> pure (Left (unexpected (replyStatus reply) reply))
          | otherwise -> do
              let caps = maybe [] (stringsAt [path]) (replyBody reply)
              if not (any (`elem` ["read", "root"]) caps)
                then
                  pure . Left . SecretError EIoSecretAuth $
                    "vault: no read capability on '" <> path <> "' (capabilities: " <> T.intercalate ", " caps <> ")"
                else case layout of
                  Plain _ -> pure (Right "readable")
                  Kv2 mount rest -> existence ref (mount <> "metadata/" <> rest)
  where
    existence ref metaPath = do
      r <- call cfg (Just session) "GET" metaPath Nothing
      pure $ do
        reply <- r
        case replyStatus reply of
          200 -> do
            let d = fromMaybe A.Null (replyBody reply)
                current = fromMaybe 0 (intAt ["data", "current_version"] d)
                wanted = fromMaybe current (vrVersion ref)
                key = T.pack (show wanted)
                live =
                  wanted > 0
                    && at ["data", "versions", key] d /= Nothing
                    && at ["data", "versions", key, "destroyed"] d /= Just (A.Bool True)
                    && maybe True T.null (textAt ["data", "versions", key, "deletion_time"] d)
            if live
              then Right ("readable, version " <> key <> (if vrVersion ref == Nothing then " (latest)" else ""))
              else
                Left
                  ( notFound
                      ref
                      ( "version "
                          <> key
                          <> " does not exist or was deleted (latest "
                          <> T.pack (show current)
                          <> ")"
                      )
                  )
          404 -> Left (notFound ref "no secret at this path")
          -- Policies often grant data/ and not metadata/: readable,
          -- with existence left to the read.
          403 -> Right "readable (existence not checked: no access to metadata)"
          s -> Left (unexpected s reply)

release :: Config -> Session -> Text -> IO ()
release cfg session lease =
  () <$ call cfg (Just session) "PUT" "sys/leases/revoke" (Just (A.object [("lease_id", A.String lease)]))

withRef :: Locator -> (VaultRef -> IO (Either SecretError a)) -> IO (Either SecretError a)
withRef loc k = either (pure . Left) k (parseVaultRef (locBody loc))

notFound :: VaultRef -> Text -> SecretError
notFound ref why = SecretError EIoSecretNotFound ("vault: '" <> vrPath ref <> version <> "': " <> why)
  where
    version = maybe "" (\v -> "?version=" <> T.pack (show v)) (vrVersion ref)

denied :: VaultRef -> Reply -> SecretError
denied ref reply = SecretError EIoSecretAuth ("vault: permission denied reading '" <> vrPath ref <> "'" <> vaultErrors reply)

unexpected :: Int -> Reply -> SecretError
unexpected s reply =
  SecretError EIoSecretUnreachable ("vault: unexpected response (status " <> T.pack (show s) <> ")" <> vaultErrors reply)

-- | A TTL in seconds, as Vault prints one.
--
-- >>> duration 3540
-- "59m"
-- >>> duration 7385
-- "2h3m5s"
duration :: Int -> Text
duration total =
  let (h, r) = total `divMod` 3600
      (m, s) = r `divMod` 60
      parts = [show h <> "h" | h > 0] <> [show m <> "m" | m > 0] <> [show s <> "s" | s > 0]
   in T.pack (if null parts then "0s" else intercalate "" parts)
