{-# LANGUAGE OverloadedStrings #-}

-- | Stopping a run on a signal (spec 11.12).
--
-- The first SIGINT or SIGTERM abandons the evaluation of @run@ or
-- @eval@, as a @timeout@ abandons its body (8.7): its commands are
-- stopped and its @finally@ blocks run (8.10), and the run then exits
-- @128 + n@. A second one stops at once: lask exits without running
-- what is left, and the reaper removes its containers
-- ("Language.Lask.Runtime.Reaper").
module Language.Lask.Runtime.Shutdown
  ( Interrupted (..),
    signalName,
    withGracefulShutdown,
    withForwardedSignals,
  )
where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, finally)
import Control.Monad (void)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.IO (stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), Signal, installHandler, sigINT, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import Language.Lask.Runtime.ProcessStop (killRunningProcesses)

-- | The abandonment of a run by a signal. It is an asynchronous
-- exception, so that a @finally@ block runs for it as it runs for any
-- other abandonment (8.10), and no @catch@ block sees it.
newtype Interrupted = Interrupted Signal
  deriving (Show)

instance Exception Interrupted where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | The name of a signal this module handles.
signalName :: Signal -> Text
signalName s
  | s == sigINT = "SIGINT"
  | s == sigTERM = "SIGTERM"
  | otherwise = "signal"

-- | Run an action with SIGINT and SIGTERM stopping it: the first one
-- throws 'Interrupted' to the thread that runs the action, and a
-- second one exits at once with @128 + n@. The handlers lask had
-- before are put back when the action ends.
withGracefulShutdown :: IO a -> IO a
withGracefulShutdown action = do
  main <- myThreadId
  received <- newIORef (0 :: Int)
  let handler sig = Catch $ do
        n <- atomicModifyIORef' received (\k -> (k + 1, k))
        if n == 0
          then do
            TIO.hPutStrLn stderr $
              "lask: received " <> signalName sig <> ": stopping; finally blocks run first. Send it again to stop at once."
            throwTo main (Interrupted sig)
          else do
            TIO.hPutStrLn stderr ("lask: received " <> signalName sig <> " again: stopping at once")
            -- No finally block runs from here, and the reaper removes
            -- the containers once lask is gone.
            killRunningProcesses
            exitImmediately (ExitFailure (128 + fromIntegral sig))
  previous <- mapM (\s -> (,) s <$> installHandler s (handler s) Nothing) [sigINT, sigTERM]
  action `finally` mapM_ (\(s, h) -> void (installHandler s h Nothing)) previous

-- | Run an action with SIGTERM passed on to a process instead of
-- stopping lask, as @cmd@ does for the program it runs (11.8). SIGINT
-- is left as it is: the terminal sends it to the program as well.
withForwardedSignals :: Maybe ProcessID -> IO a -> IO a
withForwardedSignals Nothing action = action
withForwardedSignals (Just pid) action = do
  previous <- installHandler sigTERM (Catch (signalProcess sigTERM pid)) Nothing
  action `finally` void (installHandler sigTERM previous Nothing)
