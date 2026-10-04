{-# LANGUAGE OverloadedStrings #-}

-- | Bookkeeping for the asynchronous computations a run starts (spec
-- 6.3): which handles were created, and which were consumed by
-- @await@, @all@ or @race@. What is left when the run ends was never
-- awaited; it is waited for rather than cut short, and reported as the
-- advisory @W-ASYNC-UNAWAITED@.
module Language.Lask.Runtime.AsyncTrack
  ( AsyncSite (..),
    AsyncTracker (..),
    noAsyncTracker,
    newAsyncTracker,
    siteOf,
    renderSite,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async, asyncThreadId, waitCatch)
import Control.Exception (SomeException)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Language.Lask.Core.AST (Lam (..))
import Language.Lask.Runtime.Value (Closure (..), Value (..))

-- | Where a computation was started: the module, the line and column
-- of the @async@ that started it when there is one, and otherwise the
-- name of the function given to @spawn@.
data AsyncSite = AsyncSite
  { siteModule :: FilePath,
    sitePosition :: Maybe (Int, Int),
    siteName :: Text
  }
  deriving (Show, Eq)

data AsyncTracker = AsyncTracker
  { -- | A computation was started.
    trackSpawned :: AsyncSite -> Async Value -> IO (),
    -- | A handle was consumed by @await@, @all@ or @race@.
    trackAwaited :: Async Value -> IO (),
    -- | Wait for every computation that was never consumed, including
    -- any those start in turn, and return them in the order they were
    -- started.
    drainUnawaited :: IO [(AsyncSite, Either SomeException Value)]
  }

-- | A tracker that records nothing, for contexts with no end of run
-- to report at (the REPL, static enumeration).
noAsyncTracker :: AsyncTracker
noAsyncTracker = AsyncTracker (\_ _ -> pure ()) (\_ -> pure ()) (pure [])

newAsyncTracker :: IO AsyncTracker
newAsyncTracker = do
  ref <- newIORef (0 :: Int, Map.empty :: Map.Map ThreadId (Int, AsyncSite, Async Value))
  let spawned site a =
        atomicModifyIORef' ref $ \(n, m) ->
          ((n + 1, Map.insert (asyncThreadId a) (n, site, a) m), ())
      awaited a =
        atomicModifyIORef' ref $ \(n, m) -> ((n, Map.delete (asyncThreadId a) m), ())
      drain = do
        pending <- atomicModifyIORef' ref $ \(n, m) -> ((n, Map.empty), Map.elems m)
        if null pending
          then pure []
          else do
            done <- mapM (\(_, site, a) -> (,) site <$> waitCatch a) (sortOn (\(i, _, _) -> i) pending)
            -- Waiting may have let them start computations of their own.
            (done <>) <$> drain
  pure (AsyncTracker spawned awaited drain)

-- | The site of the function value given to @spawn@. The thunk that
-- @async e@ builds is named after the position of the @async@ (13.2).
siteOf :: Value -> AsyncSite
siteOf v = case v of
  VClosure (Closure lam _) ->
    AsyncSite (lamModule lam) (anonymousAt (lamName lam)) (lamName lam)
  VBuiltin n -> AsyncSite "" Nothing n
  _ -> AsyncSite "" Nothing "<async>"
  where
    anonymousAt name = do
      lc <- T.stripPrefix "<lambda@" name >>= T.stripSuffix ">"
      case T.splitOn ":" lc of
        [l, c] | Right (line, "") <- TR.decimal l, Right (col, "") <- TR.decimal c -> Just (line, col)
        _ -> Nothing

-- | @main.lask:2:7@ for an @async@, @deploy (main.lask)@ for a named
-- function given to @spawn@.
renderSite :: AsyncSite -> Text
renderSite s = case sitePosition s of
  Just (l, c) -> T.pack (siteModule s) <> ":" <> tshow l <> ":" <> tshow c
  Nothing
    | null (siteModule s) -> siteName s
    | otherwise -> siteName s <> " (" <> T.pack (siteModule s) <> ")"
  where
    tshow = T.pack . show
