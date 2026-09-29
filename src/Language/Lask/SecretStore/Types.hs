{-# LANGUAGE OverloadedStrings #-}

-- | Secret references (spec 9.8): an environment variable whose
-- whole value is @{scheme://reference}@ names a secret in a store,
-- and is resolved when a program reads the variable.
--
-- This module holds what every store shares: recognizing a reference,
-- and the interface a provider implements. The interface is shaped by
-- the stages of @lask secrets check@ (spec 11.10), so that the check is
-- written once for every scheme.
module Language.Lask.SecretStore.Types
  ( SecretError (..),
    secretFailure,
    RawRef (..),
    detectRef,
    renderRawRef,
    Locator (..),
    Session (..),
    SecretData (..),
    SecretProvider (..),
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Runtime.Value (LaskFailure, ioFailure)

-- | A failure at the boundary of a store (spec 14.6). The message
-- names what was asked for and never a value or a credential.
data SecretError = SecretError
  { seCode :: ErrorCode,
    seMessage :: Text
  }
  deriving (Show, Eq)

-- | The failure a program sees: an external I/O error, catchable,
-- exiting with code 3 when uncaught (spec 14.8).
secretFailure :: SecretError -> LaskFailure
secretFailure (SecretError code msg) = ioFailure code msg

-- | A reference as written, split at its scheme: @{vault://a/b#c}@ is
-- scheme @vault@ and body @a/b#c@.
data RawRef = RawRef
  { rawScheme :: Text,
    rawBody :: Text
  }
  deriving (Show, Eq, Ord)

renderRawRef :: RawRef -> Text
renderRawRef r = rawScheme r <> "://" <> rawBody r

-- | Whether a variable's value is a reference. @Nothing@ is an
-- ordinary value; @Just (Left _)@ is one that starts like a reference
-- but is not well formed, which is an error rather than a value: a
-- mistyped reference must never be handed on as a password.
--
-- >>> detectRef (T.pack "abc")
-- Nothing
-- >>> detectRef (T.pack "{vault://secret/app#key}")
-- Just (Right (RawRef {rawScheme = "vault", rawBody = "secret/app#key"}))
-- >>> fmap (either (Just . seCode) (const Nothing)) (detectRef (T.pack "{vault://secret/app#key"))
-- Just (Just EIoSecretRef)
-- >>> detectRef (T.pack "{\"json\": 1}")
-- Nothing
detectRef :: Text -> Maybe (Either SecretError RawRef)
detectRef value = do
  inner <- T.stripPrefix "{" value
  let (scheme, rest) = T.span isSchemeChar inner
  body0 <- T.stripPrefix "://" rest
  if T.null scheme || not (isAsciiLower (T.head scheme))
    then Nothing
    else Just $ case T.unsnoc body0 of
      Just (body, '}')
        | not (T.null body) -> Right (RawRef scheme body)
      _ ->
        Left
          ( SecretError
              EIoSecretRef
              ("a secret reference must have the form {" <> scheme <> "://...}, with nothing after the closing brace")
          )
  where
    isSchemeChar c = isAsciiLower c || isDigit c || c `elem` ("+.-" :: String)

-- | A parsed reference, reduced to what the per-run cache and the
-- field selection need. 'locKey' identifies one read of the store —
-- two references with the same key are answered by the same read, so
-- that the fields of a dynamic secret come from one lease (spec
-- 9.8). The provider keeps whatever else it parsed in 'locBody'.
data Locator = Locator
  { locKey :: Text,
    locField :: Text,
    locBody :: Text
  }
  deriving (Show, Eq)

-- | An authenticated session with a store. 'sessToken' is opaque to
-- everything but the provider, and is never printed.
data Session = Session
  { sessToken :: Text,
    -- | What @lask secrets check@ reports for the auth stage.
    sessSummary :: Text
  }

-- | What one read of a store returned.
data SecretData = SecretData
  { sdFields :: Map Text Text,
    -- | The lease a dynamic secret was issued under, if any.
    sdLease :: Maybe Text
  }

-- | One store. Each field is a stage of @lask secrets check@; the
-- run-time path uses 'provParse', 'provLogin' and 'provRead' only.
data SecretProvider = SecretProvider
  { provScheme :: Text,
    -- | What the provider talks to, for reports (e.g. the Vault
    -- address).
    provTarget :: Text,
    provParse :: Text -> Either SecretError Locator,
    -- | Reachability, without credentials. 'Nothing' when the store
    -- has no such notion; the stage is then reported as skipped.
    provHealth :: Maybe (IO (Either SecretError Text)),
    provLogin :: IO (Either SecretError Session),
    -- | Whether a reference can be read, established without reading
    -- it: reading a dynamic secret issues a credential.
    provProbe :: Session -> Locator -> IO (Either SecretError Text),
    -- | Read one secret: every field of what 'locKey' names.
    provRead :: Session -> Locator -> IO (Either SecretError SecretData),
    -- | Give back a lease a read was issued under. Used by
    -- @lask secrets check --read@, which reads only to verify.
    provRelease :: Session -> Text -> IO ()
  }
