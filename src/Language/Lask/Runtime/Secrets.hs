{-# LANGUAGE OverloadedStrings #-}

-- | Secret masking for command execution logs (spec 12.8).
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
-- substring, so a registered secret stays masked wherever it later
-- appears in a command string or relayed output line, however it got
-- there (string interpolation, a shell variable assignment, ...) — but
-- equally, a value that has been *transformed* since registration no
-- longer matches (spec 12.8 records this limitation).
module Language.Lask.Runtime.Secrets
  ( registerSecret,
    maskSecrets,
    resetSecretRegistryForTests,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (foldl', sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
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
registerSecret :: Text -> IO ()
registerSecret value
  | T.null value = pure ()
  | otherwise = atomicModifyIORef' secretRegistry (\vs -> (value : vs, ()))

-- | Replaces every occurrence of every registered secret with 'mask'.
-- Longest values are matched first, so a registered secret that is a
-- prefix\/substring of another registered secret doesn't leave a
-- partially-masked remainder (e.g. a password and a longer token that
-- happens to embed it).
--
-- Only for the copy of a command\/output line written to the command
-- execution log (spec 12.3) — never apply this to a 'CommandResult'
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
  pure (foldl' (\acc s -> replaceAll s mask acc) input ordered)

replaceAll :: Text -> Text -> Text -> Text
replaceAll needle replacement haystack
  | T.null needle = haystack
  | otherwise = T.intercalate replacement (T.splitOn needle haystack)

-- | Test-only: clears the registry so specs don't leak secrets into
-- each other across the shared test process (the registry is
-- process-global; see the module note above).
resetSecretRegistryForTests :: IO ()
resetSecretRegistryForTests = writeIORef secretRegistry []
