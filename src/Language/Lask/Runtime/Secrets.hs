{-# LANGUAGE OverloadedStrings #-}

-- | Secret masking for what lask writes to stderr (spec 12.8): the
-- execution and command execution logs, execution events, and error
-- diagnostics.
--
-- Masking is opt-in: a value enters the registry only by being bound
-- to a @!!@-marked name, or by an explicit @mark_secret@ call (spec
-- 6.10). Nothing is inferred from where a value came from — reading
-- one with @get_env@ does not make it secret, since most environment
-- variables (a region, a log level) are not sensitive and masking them
-- would only make logs harder to read.
--
-- The registry is process-global (an 'unsafePerformIO'/'NOINLINE'
-- 'IORef', the standard idiom for cross-cutting runtime state), as
-- there is no natural threading path between the builtin evaluator
-- ("Language.Lask.Builtins.Impl") and command execution logging
-- ("Language.Lask.Runtime.Environment"). Values are matched by exact
-- substring (one shorter than 8 characters as a whole word only), so a registered secret stays masked wherever it later
-- appears in a command string or relayed output line, however it got
-- there (string interpolation, a shell variable assignment, ...) — but
-- equally, a value that has been *transformed* since registration no
-- longer matches (spec 12.8 records this limitation).
module Language.Lask.Runtime.Secrets
  ( registerSecret,
    maskSecrets,
    maskSecretsJson,
    maskValue,
    maskFailure,
    resetSecretRegistryForTests,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Runtime.Value (EnvValue (..), LaskFailure (..), Value (..))
import System.IO.Unsafe (unsafePerformIO)

-- | The fixed replacement text for a masked secret.
mask :: Text
mask = "***"

{-# NOINLINE secretRegistry #-}
secretRegistry :: IORef [Text]
secretRegistry = unsafePerformIO (newIORef [])

-- | Registers a value as sensitive. The one call site is the
-- @mark_secret@ builtin, which @!!@ desugars to (spec 6.10).
-- Idempotent in effect (duplicates in the registry are harmless:
-- 'maskSecrets' just replaces the same substring twice), so call sites
-- don't need to deduplicate.
--
-- Registration happens for every value bound to a @!!@-marked name,
-- with no length exemption (spec 12.8): a short credential is still a
-- credential. The empty string is skipped since 'replaceAll' treats
-- it as a no-op match anyway; registering it would only grow the
-- registry for nothing.
--
-- A value that spans lines (a PEM private key, say) is also registered
-- line by line, since output is relayed a line at a time and the whole
-- value never appears in one line. A line shorter than
-- 'minLineLength' is not: it is a fragment rather than a credential,
-- and a line such as @}@ would otherwise be masked wherever it occurs.
registerSecret :: Text -> IO ()
registerSecret value
  | T.null value = pure ()
  | otherwise = atomicModifyIORef' secretRegistry (\vs -> (value : fragments <> vs, ()))
  where
    fragments
      | T.any (== '\n') value =
          filter ((>= minLineLength) . T.length) (map (T.dropWhileEnd (== '\r')) (T.lines value))
      | otherwise = []

-- | The shortest line of a multi-line secret that is registered on its
-- own.
minLineLength :: Int
minLineLength = 8

-- | Replaces every occurrence of every registered secret with 'mask'.
-- Longest values are matched first, so a registered secret that is a
-- prefix\/substring of another registered secret doesn't leave a
-- partially-masked remainder (e.g. a password and a longer token that
-- happens to embed it).
--
-- Only for the copy of a line written to stderr (spec 12.8) — never
-- apply this to a 'CommandResult'
-- returned to a running Lask program; the language must still see the
-- real value (8.7).
--
-- This is deliberately an IO action rather than a pure function over
-- an 'unsafePerformIO' read. Masking depends on mutable state, so as a
-- pure function its result is only correct relative to /when/ it is
-- forced: GHC is free to share and float such an application (a call
-- with a literal argument becomes a CAF computed once), and a lazily
-- retained result can be forced against a later registry than the one
-- in effect when the line was produced. In IO the read is ordered with
-- respect to 'registerSecret' by construction.
maskSecrets :: Text -> IO Text
maskSecrets input = do
  secrets <- readIORef secretRegistry
  let ordered = sortOn (Down . T.length) secrets
      replace s
        | T.length s < minLineLength = replaceWhole s mask
        | otherwise = replaceAll s mask
  pure (foldl' (\acc s -> replace s acc) input ordered)

-- | 'maskSecrets' over every string in a JSON document, keys
-- included. The environment metadata of a command execution log
-- (spec 12.3, 13.1) carries the environment's own arguments, and a
-- @docker@ environment may pass variables to the container, so a
-- secret can reach the log through the environment value as easily as
-- through the command string.
maskSecretsJson :: A.Value -> IO A.Value
maskSecretsJson v = case v of
  A.String t -> A.String <$> maskSecrets t
  A.Array xs -> A.Array <$> traverse maskSecretsJson xs
  A.Object o ->
    fmap (A.Object . KM.fromList) . traverse entry $ KM.toList o
  _ -> pure v
  where
    entry (k, x) = (,) <$> (AK.fromText <$> maskSecrets (AK.toText k)) <*> maskSecretsJson x

-- | 'maskSecrets' over every string a value holds, map keys included.
-- For a value about to be written to stderr — an event's arguments or
-- result, or an error value in a diagnostic — and never for one the
-- program goes on to use.
maskValue :: Value -> IO Value
maskValue v = case v of
  VString t -> VString <$> maskSecrets t
  VArray xs -> VArray <$> traverse maskValue xs
  VMap m -> VMap . Map.fromList <$> traverse entry (Map.toList m)
  VRecord m -> VRecord <$> traverse maskValue m
  VEnv (EnvValue kind params) -> VEnv . EnvValue kind <$> traverse maskValue params
  _ -> pure v
  where
    entry (k, x) = (,) <$> maskSecrets k <*> maskValue x

-- | A failure as an error diagnostic reports it (spec 12.8, 14.3): its
-- error value and stack frames masked. Only for the copy written out;
-- a failure the program can still catch keeps its real value.
maskFailure :: LaskFailure -> IO LaskFailure
maskFailure lf = do
  err <- maskValue (lfError lf)
  frames <- traverse maskSecrets (lfFrames lf)
  pure lf {lfError = err, lfFrames = frames}

-- | As 'replaceAll', for an occurrence that stands as a word of its
-- own only: one not preceded or followed by a letter, a digit or @_@.
-- A value this short found inside another word is not the secret
-- appearing; masking it there hides unrelated output, and the letters
-- around the mask give the secret away (@none***istent@ for @x@).
--
-- >>> :set -XOverloadedStrings
-- >>> replaceWhole "x" "***" "x nonexistent x-ray box"
-- "*** nonexistent ***-ray box"
-- >>> replaceWhole "x" "***" "xx x"
-- "xx ***"
replaceWhole :: Text -> Text -> Text -> Text
replaceWhole needle replacement haystack
  | T.null needle = haystack
  | otherwise = case pieces of
      [] -> haystack
      first : rest -> T.concat (first : go first (zip [1 :: Int ..] rest))
  where
    pieces = T.splitOn needle haystack
    lastIx = length pieces - 1
    word c = isAlphaNum c || c == '_'
    -- The character before an occurrence: the end of the piece before
    -- it, or, when that piece is empty, the previous occurrence.
    before i left
      | not (T.null left) = Just (T.last left)
      | i > 1 = Just (T.last needle)
      | otherwise = Nothing
    -- The character after it, likewise.
    after i right
      | not (T.null right) = Just (T.head right)
      | i < lastIx = Just (T.head needle)
      | otherwise = Nothing
    go _ [] = []
    go left ((i, right) : more) =
      let standsAlone = not (maybe False word (before i left)) && not (maybe False word (after i right))
       in (if standsAlone then replacement else needle) : right : go right more

replaceAll :: Text -> Text -> Text -> Text
replaceAll needle replacement haystack
  | T.null needle = haystack
  | otherwise = T.intercalate replacement (T.splitOn needle haystack)

-- | Test-only: clears the registry so specs don't leak secrets into
-- each other across the shared test process (the registry is
-- process-global; see the module note above).
resetSecretRegistryForTests :: IO ()
resetSecretRegistryForTests = writeIORef secretRegistry []
