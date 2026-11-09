{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The reaper: a process of its own that removes the containers an
-- invocation started when the invocation ends without removing them
-- itself (spec 11.12). SIGKILL cannot be handled by the process it
-- kills, so the clean-up it skips has to be done by another one.
--
-- The invocation starts the reaper before its first container, in a
-- session of its own, and holds the write end of a pipe the reaper
-- reads. An invocation that ends as it should says so on the pipe
-- before closing it ('releaseReaper'), and the reaper exits. A pipe
-- that closes without that word means the invocation was killed, or
-- left at once on a second signal: the reaper then force-removes
-- every container, and the networks of @egress@, labelled with the
-- invocation as their owner.
module Language.Lask.Runtime.Reaper
  ( ownerLabel,
    egressOwnerLabel,
    processOwner,
    ownerArgs,
    enableReaper,
    ensureReaper,
    releaseReaper,
    reaperMain,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.IO (hGetContents, hSetBinaryMode, stdin)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (FdOption (CloseOnExec), closeFd, createPipe, fdToHandle, fdWrite, setFdOption)
import System.Posix.Signals (Handler (Ignore), installHandler, sigHUP, sigINT, sigTERM)
import System.Posix.Types (ByteCount, Fd)
import qualified System.Process as P

-- | The label every container an invocation starts carries, naming the
-- invocation (spec 11.12).
ownerLabel :: Text
ownerLabel = "io.lask.owner"

-- | The label of the networks and the proxy of @egress@ (spec 10.12),
-- which a later invocation also uses to remove what one left.
egressOwnerLabel :: Text
egressOwnerLabel = "io.lask.egress.owner"

-- | This process, as its labels name it: @<host>:<pid>@. It does not
-- change for the life of the process, so it is read once.
processOwner :: Text
processOwner = unsafePerformIO $ do
  host <- hostName
  pid <- P.getCurrentPid
  pure (T.pack host <> ":" <> T.pack (show pid))
{-# NOINLINE processOwner #-}

-- | The name of this host, as @hostname@ prints it, which every
-- platform Lask runs on has.
hostName :: IO String
hostName = do
  r <- try (P.readProcess "hostname" [] "") :: IO (Either SomeException String)
  pure (either (const "unknown") (takeWhile (`notElem` ("\r\n" :: String))) r)

-- | The client arguments that label a container with this invocation,
-- placed after @run@.
ownerArgs :: [String]
ownerArgs = ["--label", T.unpack (ownerLabel <> "=" <> processOwner)]

-- | The write end of the reaper's pipe, once it is started.
reaper :: MVar (Maybe Fd)
reaper = unsafePerformIO (newMVar Nothing)
{-# NOINLINE reaper #-}

-- | Whether this process may start a reaper. The reaper is this
-- executable run as @__reap@, which only lask's own @main@ answers, so
-- a program that uses lask as a library never starts one.
enabled :: IORef Bool
enabled = unsafePerformIO (newIORef False)
{-# NOINLINE enabled #-}

-- | Let this process start a reaper: called by lask's @main@.
enableReaper :: IO ()
enableReaper = writeIORef enabled True

-- | Start the reaper for the client named, unless it is running. A
-- reaper that cannot be started costs only the clean-up after a kill,
-- so it is not an error.
ensureReaper :: FilePath -> IO ()
ensureReaper client = do
  on <- readIORef enabled
  when on (start client)

start :: FilePath -> IO ()
start client = modifyMVar_ reaper $ \running -> case running of
  Just fd -> pure (Just fd)
  Nothing -> do
    r <- try $ do
      exe <- getExecutablePath
      (readEnd, writeEnd) <- createPipe
      -- No other process lask starts may hold the write end: one that
      -- outlived lask, as an orphaned engine client does, would keep
      -- the pipe open and the reaper waiting.
      setFdOption writeEnd CloseOnExec True
      readHandle <- fdToHandle readEnd
      _ <-
        P.createProcess
          (P.proc exe ["__reap", client, T.unpack processOwner])
            { P.std_in = P.UseHandle readHandle,
              P.std_out = P.NoStream,
              P.std_err = P.Inherit,
              -- Nothing else of lask's: a pipe lask writes its result
              -- to must end when lask does, not when the reaper does.
              P.close_fds = True,
              -- Out of the terminal's process group, so that the
              -- signal that stops lask does not stop the reaper too.
              P.new_session = True
            }
      pure writeEnd
    pure $ case r of
      Right fd -> Just fd
      Left (_ :: SomeException) -> Nothing

-- | Tell the reaper the invocation cleaned up after itself, and let it
-- go. Called on every way out of an invocation but an immediate one.
releaseReaper :: IO ()
releaseReaper = modifyMVar_ reaper $ \running -> do
  case running of
    Nothing -> pure ()
    Just fd -> do
      _ <- try (fdWrite fd "done\n") :: IO (Either IOException ByteCount)
      _ <- try (closeFd fd) :: IO (Either IOException ())
      pure ()
  pure Nothing

-- | The reaper itself: @lask __reap <client> <owner>@. It waits for its
-- stdin to end, and removes what the owner left unless it was told the
-- owner is done.
reaperMain :: [String] -> IO ()
reaperMain args = case args of
  [client, owner] -> do
    -- It outlives what stops lask; only SIGKILL stops it.
    mapM_ (\s -> installHandler s Ignore Nothing) [sigINT, sigTERM, sigHUP]
    hSetBinaryMode stdin True
    said <- try (hGetContents stdin >>= \s -> length s `seq` pure s) :: IO (Either IOException String)
    let done = either (const False) (elem "done" . lines) said
    unless done (removeLeftovers client (T.pack owner))
  _ -> pure ()

-- | Force-remove the containers labelled with the owner, then the
-- networks of its @egress@.
removeLeftovers :: FilePath -> Text -> IO ()
removeLeftovers client owner = do
  let filterOf label = ["--filter", T.unpack ("label=" <> label <> "=" <> owner)]
  containers <- output (["ps", "-aq"] <> filterOf ownerLabel)
  proxies <- output (["ps", "-aq"] <> filterOf egressOwnerLabel)
  let ids = containers <> proxies
  unless (null ids) (void (quietly ("rm" : "-f" : ids)))
  networks <- output (["network", "ls", "-q"] <> filterOf egressOwnerLabel)
  unless (null networks) (void (quietly ("network" : "rm" : networks)))
  where
    output as = do
      r <- try (P.readCreateProcessWithExitCode (P.proc client as) "") :: IO (Either IOException (ExitCode, String, String))
      pure $ case r of
        Right (ExitSuccess, out, _) -> words out
        _ -> []
    quietly as = try (P.readCreateProcessWithExitCode (P.proc client as) "") :: IO (Either IOException (ExitCode, String, String))
