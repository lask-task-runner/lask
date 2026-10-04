{-# LANGUAGE OverloadedStrings #-}

-- | Resolving secret references when a program reads the environment
-- (spec 9.8), and the choice of which stores a run may use.
--
-- A run reads each secret at most once. The first reader of a
-- @(scheme, key)@ starts the read and every later one, from any
-- @async@, waits for the same answer: for a dynamic secret, whose
-- every read issues a new credential, this is what keeps an access
-- key and its secret key from two different leases.
module Language.Lask.SecretStore.Resolve
  ( knownSchemes,
    enabledSchemes,
    openProvider,
    SecretResolver,
    newSecretResolver,
    newSecretResolverWith,
    readEnvVar,
    readEnvVarUnresolved,
  )
where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (throwIO)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Runtime.Secrets (registerSecret)
import Language.Lask.SecretStore.Types
import Language.Lask.SecretStore.Vault (vaultProvider)
import System.Environment (getEnvironment, lookupEnv)

-- | The schemes this implementation has a provider for.
knownSchemes :: [Text]
knownSchemes = ["vault"]

-- | The schemes @LASK_SECRETS@ enables, separated by commas or
-- spaces. The list comes from the environment, and so from outside the
-- repository a program lives in.
--
-- >>> enabledSchemes (Map.fromList [("LASK_SECRETS", "vault, op")])
-- ["vault","op"]
enabledSchemes :: Map String String -> [Text]
enabledSchemes env =
  filter (not . T.null) . T.split (\c -> c == ',' || isSpace c) . T.pack $
    Map.findWithDefault "" "LASK_SECRETS" env

-- | The provider for a scheme, if the scheme is known and enabled and
-- the provider is configured.
openProvider :: Map String String -> Text -> IO (Either SecretError SecretProvider)
openProvider env scheme
  | scheme `notElem` knownSchemes =
      pure . Left . providerError $
        "unknown secret scheme '" <> scheme <> "' (known: " <> T.intercalate ", " knownSchemes <> ")"
  | scheme `notElem` enabledSchemes env =
      pure . Left . providerError $
        "the secret scheme '" <> scheme <> "' is not enabled: add it to LASK_SECRETS"
  | otherwise = vaultProvider env
  where
    providerError = SecretError EIoSecretProvider

data SecretResolver = SecretResolver
  { srEnv :: Map String String,
    srProviders :: MVar (Map Text (Async (Either SecretError SecretProvider))),
    srSessions :: MVar (Map Text (Async (Either SecretError Session))),
    srReads :: MVar (Map (Text, Text) (Async (Either SecretError SecretData)))
  }

-- | A resolver over the process environment, for one run.
newSecretResolver :: IO SecretResolver
newSecretResolver = newSecretResolverWith . Map.fromList =<< getEnvironment

newSecretResolverWith :: Map String String -> IO SecretResolver
newSecretResolverWith env = SecretResolver env <$> newMVar Map.empty <*> newMVar Map.empty <*> newMVar Map.empty

-- | A variable as @get_env@, @find_env@ and @get_env_or@ read it: an
-- ordinary value as it is, a reference resolved, and registered for
-- masking (spec 12.8). A reference that cannot be resolved is an
-- external I/O error naming the variable, never a value.
readEnvVar :: SecretResolver -> Text -> IO (Maybe Text)
readEnvVar sr name = case Map.lookup (T.unpack name) (srEnv sr) of
  Nothing -> pure Nothing
  Just raw -> case detectRef (T.pack raw) of
    Nothing -> pure (Just (T.pack raw))
    Just (Left e) -> failWith e
    Just (Right ref) -> do
      r <- resolve sr ref
      case r of
        Left e -> failWith e
        Right v -> do
          registerSecret v
          pure (Just v)
  where
    failWith (SecretError code msg) =
      throwIO (secretFailure (SecretError code ("environment variable '" <> name <> "': " <> msg)))

-- | A reference is parsed before anything is asked of the store, so
-- that a malformed one is reported as such wherever the store is.
resolve :: SecretResolver -> RawRef -> IO (Either SecretError Text)
resolve sr ref = do
  let scheme = rawScheme ref
  provE <- once (srProviders sr) scheme (openProvider (srEnv sr) scheme)
  case provE of
    Left e -> pure (Left e)
    Right prov -> case provParse prov (rawBody ref) of
      Left e -> pure (Left e)
      Right loc -> do
        sessionE <- once (srSessions sr) scheme (provLogin prov)
        case sessionE of
          Left e -> pure (Left e)
          Right session -> do
            dataE <- once (srReads sr) (scheme, locKey loc) (provRead prov session loc)
            pure $ do
              d <- dataE
              case Map.lookup (locField loc) (sdFields d) of
                Just v -> Right v
                Nothing ->
                  Left . SecretError EIoSecretNotFound $
                    renderRawRef ref
                      <> ": no field '"
                      <> locField loc
                      <> "' (fields: "
                      <> T.intercalate ", " (Map.keys (sdFields d))
                      <> ")"

-- | Run an action at most once per key; every caller gets its result.
-- The action runs in its own thread, so a caller that is cancelled
-- (by @timeout@, or a failed sibling) does not leave the others
-- waiting on a read nobody finishes.
once :: (Ord k) => MVar (Map k (Async a)) -> k -> IO a -> IO a
once table key action = do
  a <- modifyMVar table $ \m -> case Map.lookup key m of
    Just existing -> pure (m, existing)
    Nothing -> do
      started <- async action
      pure (Map.insert key started m, started)
  wait a

-- | A variable read where no store may be reached (@lask cmd --list@
-- reports without network access): an ordinary value as it is, and a
-- reference as a failure saying so.
readEnvVarUnresolved :: Text -> IO (Maybe Text)
readEnvVarUnresolved name = do
  found <- lookupEnv (T.unpack name)
  case found of
    Just raw | Just _ <- detectRef (T.pack raw) ->
      throwIO . secretFailure . SecretError EIoSecretProvider $
        "environment variable '" <> name <> "' is a secret reference, which is not resolved here"
    _ -> pure (T.pack <$> found)
