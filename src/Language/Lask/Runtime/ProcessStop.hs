{-# LANGUAGE OverloadedStrings #-}

-- | Stopping a command whose evaluation is abandoned (spec 8.7).
--
-- A thread running a command can be cancelled before the command
-- exits: 'race' cancels the computations that lost (15.6). Cancelling
-- the thread does not stop the process it started, so the process and
-- everything it started are stopped here, and a container is stopped
-- through the daemon, since stopping the @docker run@ client leaves it
-- running.
--
-- The command keeps lask's process group, so a password prompt read
-- from the terminal and Ctrl-C behave as they do for any child. The
-- processes to stop are found by walking the process tree instead.
module Language.Lask.Runtime.ProcessStop
  ( withStoppableProcess,
    stopProcessTree,
    stopContainer,
    newContainerName,
    stopGraceSeconds,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, mask, onException, try)
import Control.Monad (unless, void)
import qualified Data.ByteString.Base16 as B16
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Entropy (getEntropy)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Info (os)
import System.Process
  ( CreateProcess,
    ProcessHandle,
    createProcess,
    getPid,
    getProcessExitCode,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
  )
import Text.Read (readMaybe)

-- | How long a stopped command is given to exit on its own before it
-- is killed (spec 8.7: implementation-defined).
stopGraceSeconds :: Int
stopGraceSeconds = 3

-- | Start a process and run an action on it. If the action does not
-- complete — its thread cancelled, or any other exception — the
-- process is stopped with the given action, its pipes are closed, and
-- then the exception continues.
withStoppableProcess ::
  CreateProcess ->
  (ProcessHandle -> IO ()) ->
  ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) ->
  IO a
withStoppableProcess cp stop body = mask $ \restore -> do
  -- Masked from creation until the handler is in place, so a
  -- cancellation cannot land between the two and leave the process
  -- running.
  p@(mIn, mOut, mErr, ph) <- createProcess cp
  restore (body p) `onException` do
    stop ph
    mapM_ closeQuietly (mIn : [mOut, mErr])
  where
    closeQuietly = mapM_ (\h -> void (try (hClose h) :: IO (Either IOException ())))

-- | Stop a process and every process it started: a termination
-- request first, then, for whatever is still there after the grace
-- period, a kill. Returns once the process has been reaped.
stopProcessTree :: Int -> ProcessHandle -> IO ()
stopProcessTree graceSeconds ph = do
  mpid <- getPid ph
  case mpid of
    -- Already exited and reaped.
    Nothing -> pure ()
    Just pid
      -- Windows has no termination request a console program answers,
      -- so the tree is killed at once.
      | os == "mingw32" -> do
          quietly "taskkill" ["/PID", show pid, "/T", "/F"]
          void (waitForProcess ph)
      | otherwise -> do
          -- The tree is read before anything is signalled: once a
          -- parent is gone, its children are re-parented and can no
          -- longer be found under it.
          tree <- descendantsOf (fromIntegral pid)
          signal "TERM" tree
          waitGone (graceSeconds * 20) tree
          live <- liveOf tree
          unless (null live) (signal "KILL" live)
          void (waitForProcess ph)
  where
    -- Polls every 50ms. The direct child stays a zombie until it is
    -- reaped, so it is reaped here to be seen as gone.
    waitGone :: Int -> [Int] -> IO ()
    waitGone ticks tree
      | ticks <= 0 = pure ()
      | otherwise = do
          _ <- getProcessExitCode ph
          live <- liveOf tree
          unless (null live) $ do
            threadDelay 50000
            waitGone (ticks - 1) tree

-- | Stop and remove a container by name. The daemon sends the
-- termination request and kills after the grace period, and the
-- removal covers a container that was created but never started.
stopContainer :: Int -> Text -> IO ()
stopContainer graceSeconds name = do
  quietly "docker" ["stop", "-t", show graceSeconds, T.unpack name]
  quietly "docker" ["rm", "--force", T.unpack name]

-- | A name for one container run, so that it can be stopped by name.
newContainerName :: IO Text
newContainerName = do
  bytes <- getEntropy 8
  pure ("lask-" <> TE.decodeUtf8 (B16.encode bytes))

-- | The process and all its descendants, as the process table shows
-- them now. If the table cannot be read, only the process itself.
descendantsOf :: Int -> IO [Int]
descendantsOf root = do
  table <- processTable
  pure (maybe [root] (\t -> closure t [root]) table)

-- | Which of the given processes still exist, together with any
-- descendants they started since.
liveOf :: [Int] -> IO [Int]
liveOf pids = do
  table <- processTable
  pure $ case table of
    Nothing -> pids
    Just t ->
      let present = Set.fromList (Map.keys t)
       in closure t (filter (`Set.member` present) pids)

-- | Everything reachable from the given processes through the
-- parent relation, the given ones included.
closure :: Map.Map Int Int -> [Int] -> [Int]
closure table = go Set.empty
  where
    children = Map.fromListWith (<>) [(ppid, [pid]) | (pid, ppid) <- Map.toList table]
    go seen [] = Set.toList seen
    go seen (p : rest)
      | p `Set.member` seen = go seen rest
      | otherwise = go (Set.insert p seen) (Map.findWithDefault [] p children <> rest)

-- | pid to parent pid, for every process.
processTable :: IO (Maybe (Map.Map Int Int))
processTable = do
  r <- try (readCreateProcessWithExitCode (proc "ps" ["-A", "-o", "pid=", "-o", "ppid="]) "")
  pure $ case r :: Either IOException (ExitCode, String, String) of
    Right (_, out, _) -> Just (Map.fromList (mapMaybe row (lines out)))
    Left _ -> Nothing
  where
    row l = case words l of
      [a, b] -> (,) <$> readMaybe a <*> readMaybe b
      _ -> Nothing

signal :: String -> [Int] -> IO ()
signal _ [] = pure ()
signal sig pids = quietly "kill" (["-s", sig] <> map show (nub pids))

-- | Run a helper for its effect alone. Its failure changes nothing:
-- a process already gone is what stopping it wants.
quietly :: FilePath -> [String] -> IO ()
quietly cmd args =
  void (try (readCreateProcessWithExitCode (proc cmd args) "") :: IO (Either IOException (ExitCode, String, String)))
