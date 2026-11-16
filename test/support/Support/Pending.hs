-- | A test of a known bug: it states what should hold, is pending while
-- it does not, and fails once it does, so that the mark is removed with
-- the fix (doc/development.md, "Pending").
module Support.Pending (knownBug) where

import Control.Exception (SomeException, try)
import Test.Hspec (Expectation, expectationFailure, pendingWith)

-- | Run the expectation: pending with the reason while it fails, and a
-- failure once it passes.
knownBug :: String -> Expectation -> Expectation
knownBug why expectation = do
  r <- try expectation :: IO (Either SomeException ())
  case r of
    Left _ -> pendingWith why
    Right () -> expectationFailure ("passes now: remove its knownBug mark (" <> why <> ")")
