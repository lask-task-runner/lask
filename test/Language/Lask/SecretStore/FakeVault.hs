{-# LANGUAGE OverloadedStrings #-}

-- | An in-process stand-in for the parts of the Vault HTTP API the
-- @vault@ provider uses (spec 9.8), for tests of the provider, of the
-- resolver, and of the built binary.
--
-- What it serves:
--
-- * @secret/@, a key/value version 2 mount: @secret/app@ has version 1
--   (@password=s3cr3t@) and version 2 (@password=v2pass@), both with
--   @user=admin@.
-- * @kv1/@, a key/value version 1 mount: @kv1/db@ has @url@.
-- * @aws/creds/deploy@, a dynamic secret: every read issues a new
--   @access_key@ / @secret_key@ pair under a new lease.
--
-- Tokens: @root@ may do anything; @limited@ may read @secret/app@ and
-- nothing else, not even its metadata. AppRole login with role
-- @role@ and secret @secret@ issues @limited@.
--
-- When 'fvRedirect' is set, every request is answered with that status
-- and a @Location@ of the given base followed by the request's path,
-- as a standby node or a hostile server would answer it.
module Language.Lask.SecretStore.FakeVault
  ( FakeVault (..),
    withFakeVault,
    requestsTo,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Types (Status, status200, status204, status400, status403, status404, status503)
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)

data FakeVault = FakeVault
  { fvAddr :: String,
    -- | Every request, as @METHOD path?query@.
    fvRequests :: IORef [Text],
    -- | The leases revoked, in order.
    fvRevoked :: IORef [Text],
    fvSealed :: IORef Bool,
    -- | Every @X-Vault-Token@ received, in order.
    fvTokens :: IORef [Text],
    fvRedirect :: IORef (Maybe (Status, String))
  }

-- | Serve the fake on a free port for the duration of an action.
withFakeVault :: (FakeVault -> IO a) -> IO a
withFakeVault action = do
  requests <- newIORef []
  revoked <- newIORef []
  sealed <- newIORef False
  issued <- newIORef (0 :: Int)
  tokens <- newIORef []
  redirect <- newIORef Nothing
  testWithApplication (pure (app requests revoked sealed issued tokens redirect)) $ \port ->
    action (FakeVault ("http://127.0.0.1:" <> show port) requests revoked sealed tokens redirect)

-- | How many requests had this @METHOD path@ prefix.
requestsTo :: FakeVault -> Text -> IO Int
requestsTo fv prefix = length . filter (prefix `T.isPrefixOf`) <$> readIORef (fvRequests fv)

app :: IORef [Text] -> IORef [Text] -> IORef Bool -> IORef Int -> IORef [Text] -> IORef (Maybe (Status, String)) -> Application
app requests revoked sealedRef issued tokens redirectRef req respond = do
  body <- strictRequestBody req
  let verb = TE.decodeUtf8 (requestMethod req)
      path = T.intercalate "/" (drop 1 (pathInfo req))
      query = TE.decodeUtf8 (rawQueryString req)
      token = TE.decodeUtf8 <$> lookup "X-Vault-Token" (requestHeaders req)
      json = A.decode body :: Maybe A.Value
      reply status v = respond (responseLBS status [("Content-Type", "application/json")] (A.encode v))
      denied = reply status403 (A.object [("errors", A.toJSON ["permission denied" :: Text])])
      notFound = reply status404 (A.object [("errors", A.toJSON ([] :: [Text]))])
      root = token == Just "root"
      limited = token == Just "limited"
  atomicModifyIORef' requests (\rs -> (rs <> [verb <> " " <> path <> query], ()))
  mapM_ (\t -> atomicModifyIORef' tokens (\ts -> (ts <> [t], ()))) token
  sealed <- readIORef sealedRef
  redirect <- readIORef redirectRef
  case (verb, path) of
    _
      | Just (status, base) <- redirect ->
          respond (responseLBS status [("Location", TE.encodeUtf8 (T.pack base) <> rawPathInfo req <> rawQueryString req)] "")
    ("GET", "sys/health")
      | sealed -> reply status503 (A.object [("sealed", A.Bool True), ("version", "1.20.4")])
      | otherwise -> reply status200 (A.object [("sealed", A.Bool False), ("version", "1.20.4")])
    _ | sealed -> reply status503 (A.object [("errors", A.toJSON ["Vault is sealed" :: Text])])
    ("GET", "auth/token/lookup-self")
      | root -> reply status200 (A.object [("data", A.object [("ttl", A.Number 0), ("policies", A.toJSON ["root" :: Text])])])
      | limited -> reply status200 (A.object [("data", A.object [("ttl", A.Number 3600), ("policies", A.toJSON ["default", "limited" :: Text])])])
      | otherwise -> reply status403 (A.object [("errors", A.toJSON ["permission denied", "invalid token" :: Text])])
    ("POST", "auth/approle/login")
      | field "role_id" json == Just "role" && field "secret_id" json == Just "secret" ->
          reply status200 . A.object $
            [ ( "auth",
                A.object
                  [ ("client_token", "limited"),
                    ("lease_duration", A.Number 3600),
                    ("policies", A.toJSON ["default", "limited" :: Text])
                  ]
              )
            ]
      | otherwise -> reply status400 (A.object [("errors", A.toJSON ["invalid role or secret ID" :: Text])])
    _ | not (root || limited) -> denied
    ("GET", p)
      | Just rest <- T.stripPrefix "sys/internal/ui/mounts/" p -> case mountOf rest of
          Just (mount, kind, version) ->
            reply status200 . A.object $
              [ ( "data",
                  A.object
                    [ ("path", A.String mount),
                      ("type", A.String kind),
                      ("options", maybe A.Null (\v -> A.object [("version", A.String v)]) version)
                    ]
                )
              ]
          Nothing -> denied
    ("POST", "sys/capabilities-self") -> do
      let paths = case json of
            Just (A.Object o) | Just (A.Array ps) <- KM.lookup "paths" o -> [p | A.String p <- V.toList ps]
            _ -> []
          caps p
            | root = ["root"]
            | p == "secret/data/app" = ["read"]
            | otherwise = ["deny"] :: [Text]
      reply status200 (A.object [(AK.fromText p, A.toJSON (caps p)) | p <- paths])
    ("PUT", "sys/leases/revoke") -> do
      mapM_ (\l -> atomicModifyIORef' revoked (\ls -> (ls <> [l], ()))) (field "lease_id" json)
      respond (responseLBS status204 [] "")
    ("GET", "secret/data/app") -> case T.stripPrefix "?version=" query of
      Nothing -> kv2 reply "2"
      Just v
        | v `elem` ["1", "2"] -> kv2 reply v
        | otherwise -> notFound
    ("GET", p) | limited, p /= "secret/data/app" -> denied
    ("GET", "secret/metadata/app") ->
      reply status200 . A.object $
        [ ( "data",
            A.object
              [ ("current_version", A.Number 2),
                ( "versions",
                  A.object
                    [ ("1", A.object [("deletion_time", ""), ("destroyed", A.Bool False)]),
                      ("2", A.object [("deletion_time", ""), ("destroyed", A.Bool False)])
                    ]
                )
              ]
          )
        ]
    ("GET", "kv1/db") -> reply status200 (A.object [("data", A.object [("url", "postgres://db")])])
    ("GET", "aws/creds/deploy") -> do
      n <- atomicModifyIORef' issued (\k -> (k + 1, k + 1))
      let tag = T.pack (show n)
      reply status200 . A.object $
        [ ("lease_id", A.String ("aws/creds/deploy/L" <> tag)),
          ("data", A.object [("access_key", A.String ("AK" <> tag)), ("secret_key", A.String ("SK" <> tag))])
        ]
    _ -> notFound
  where
    field k v = case v of
      Just (A.Object o) | Just (A.String t) <- KM.lookup (AK.fromText k) o -> Just t
      _ -> Nothing
    mountOf p
      | "secret/" `T.isPrefixOf` p = Just ("secret/", "kv", Just "2")
      | "kv1/" `T.isPrefixOf` p = Just ("kv1/", "kv", Nothing)
      | "aws/" `T.isPrefixOf` p = Just ("aws/", "aws", Nothing)
      | otherwise = Nothing

kv2 :: (Status -> A.Value -> IO a) -> Text -> IO a
kv2 reply version =
  reply status200 . A.object $
    [ ("lease_id", ""),
      ( "data",
        A.object
          [ ( "data",
              A.object
                [ ("password", if version == "1" then "s3cr3t" else "v2pass"),
                  ("user", "admin")
                ]
            ),
            ("metadata", A.object [("version", A.Number (if version == "1" then 1 else 2))])
          ]
      )
    ]
