-- | Cancellation scopes for @timeout@ (spec 15.7): the computations a
-- body starts with @async@ are cancelled with it when it is abandoned.
--
-- A scope belongs to the thread that opened it, and to every
-- computation started from inside it, however deeply, which is why
-- membership is looked up by thread: @spawn@ has no other way to know
-- which bodies it is running inside. The registry is process-wide for
-- the same reason the secret registry is (12.8): a scope must be seen
-- from any thread the run starts.
module Language.Lask.Runtime.CancelScope
  ( ScopeId,
    withScope,
    spawnInScopes,
    cancelScope,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel)
import Control.Exception (finally, mask_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Language.Lask.Runtime.Value (Value)
import System.IO.Unsafe (unsafePerformIO)

newtype ScopeId = ScopeId Int
  deriving (Eq, Ord, Show)

data Registry = Registry
  { regNext :: !Int,
    -- | The scopes each thread is running inside, innermost first.
    regThreads :: !(Map.Map ThreadId [ScopeId]),
    -- | The computations started inside each open scope.
    regMembers :: !(Map.Map ScopeId [Async Value])
  }

registry :: IORef Registry
registry = unsafePerformIO (newIORef (Registry 0 Map.empty Map.empty))
{-# NOINLINE registry #-}

-- | Run an action inside a new scope, opened for the current thread
-- and closed when the action ends, however it ends.
withScope :: (ScopeId -> IO a) -> IO a
withScope action = do
  tid <- myThreadId
  sid <- atomicModifyIORef' registry $ \r ->
    let sid = ScopeId (regNext r)
     in ( r
            { regNext = regNext r + 1,
              regThreads = Map.insertWith (<>) tid [sid] (regThreads r),
              regMembers = Map.insert sid [] (regMembers r)
            },
          sid
        )
  action sid `finally` atomicModifyIORef' registry (\r -> (close tid sid r, ()))
  where
    close tid sid r =
      r
        { regThreads = Map.update (nonEmpty . filter (/= sid)) tid (regThreads r),
          regMembers = Map.delete sid (regMembers r)
        }
    nonEmpty xs = if null xs then Nothing else Just xs

-- | Start a computation that belongs to every scope the current
-- thread is inside. It is registered before the caller can be
-- interrupted, so a scope abandoned at that moment still reaches it.
spawnInScopes :: IO Value -> IO (Async Value)
spawnInScopes action = do
  tid <- myThreadId
  scopes <- Map.findWithDefault [] tid . regThreads <$> readIORef registry
  mask_ $ do
    a <- asyncWithUnmask $ \unmask -> do
      child <- myThreadId
      atomicModifyIORef' registry $ \r ->
        (r {regThreads = if null scopes then regThreads r else Map.insert child scopes (regThreads r)}, ())
      unmask action `finally` atomicModifyIORef' registry (\r -> (r {regThreads = Map.delete child (regThreads r)}, ()))
    atomicModifyIORef' registry $ \r ->
      (r {regMembers = foldr (Map.adjust (a :)) (regMembers r) scopes}, ())
    pure a

-- | Cancel every computation started inside the scope, waiting for
-- each to stop (which stops the commands they run, 8.7), and return
-- them.
cancelScope :: ScopeId -> IO [Async Value]
cancelScope sid = do
  members <- Map.findWithDefault [] sid . regMembers <$> readIORef registry
  mapM_ cancel members
  pure members
