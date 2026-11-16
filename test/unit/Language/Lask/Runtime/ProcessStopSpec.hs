-- | Stopping a command and every process it started (spec 8.7),
-- against real processes.
module Language.Lask.Runtime.ProcessStopSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (SomeException, try)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Language.Lask.Runtime.ProcessStop
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), createProcess, getPid, proc, waitForProcess)
import Test.Hspec
import Text.Read (readMaybe)

-- | Start @sh -c script@ in a directory, wait until it has written
-- @pids@, and return its handle with the pids it wrote.
startTree :: FilePath -> String -> IO (Maybe Int, [Int], IO ())
startTree dir script = do
  (_, _, _, ph) <- createProcess (proc "sh" ["-c", script]) {cwd = Just dir}
  pid <- fmap fromIntegral <$> getPid ph
  pids <- awaitPids 200
  pure (pid, pids, stopProcessTree 1 ph)
  where
    awaitPids :: Int -> IO [Int]
    awaitPids 0 = pure []
    awaitPids n = do
      ok <- doesFileExist (dir </> "ready")
      if ok
        then concatMap (maybe [] pure . readMaybe) . lines <$> readFile (dir </> "pids")
        else threadDelay 25000 >> awaitPids (n - 1)

spec :: Spec
spec = describe "stopping a process tree (spec 8.7)" $ do
  it "stops the process and every process it started" $
    withSystemTempDirectory "lask-stop" $ \dir -> do
      (root, kids, stop) <- startTree dir "sleep 30 & echo $! > pids; sleep 30 & echo $! >> pids; touch ready; wait"
      length kids `shouldBe` 2
      stop
      liveOf (maybe kids (: kids) root) `shouldReturn` []

  it "stops a grandchild started through a nested shell" $
    withSystemTempDirectory "lask-stop" $ \dir -> do
      (_, kids, stop) <- startTree dir "sh -c 'sleep 30 & echo $! > pids; touch ready; wait' & wait"
      length kids `shouldBe` 1
      stop
      liveOf kids `shouldReturn` []

  it "kills what ignores the termination request once the grace period is over" $
    withSystemTempDirectory "lask-stop" $ \dir -> do
      (_, kids, stop) <- startTree dir "sh -c 'trap \"\" TERM; echo $$ > pids; touch ready; while :; do sleep 0.1; done' & wait"
      length kids `shouldBe` 1
      t0 <- getCurrentTime
      stop
      t1 <- getCurrentTime
      liveOf kids `shouldReturn` []
      realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (\s -> s >= (0.9 :: Double) && s < 5)

  it "returns at once for a process that has already exited" $ do
    (_, _, _, ph) <- createProcess (proc "true" [])
    _ <- waitForProcess ph
    stopProcessTree 1 ph `shouldReturn` ()

  it "stops the process of an action that is cancelled" $
    withSystemTempDirectory "lask-stop" $ \dir -> do
      a <- async $
        withStoppableProcess
          (proc "sh" ["-c", "echo $$ > pids; touch ready; exec sleep 30"]) {cwd = Just dir}
          (stopProcessTree 1)
          (\(_, _, _, ph) -> waitForProcess ph)
      let wait n = doesFileExist (dir </> "ready") >>= \ok -> if ok || n <= (0 :: Int) then pure () else threadDelay 25000 >> wait (n - 1)
      wait 200
      pids <- concatMap (maybe [] pure . readMaybe) . lines <$> readFile (dir </> "pids")
      cancel a
      liveOf pids `shouldReturn` []

  it "stops the process when the action fails, and lets the failure go on" $
    withSystemTempDirectory "lask-stop" $ \dir -> do
      r <-
        try
          ( withStoppableProcess
              (proc "sh" ["-c", "echo $$ > pids; exec sleep 30"]) {cwd = Just dir}
              (stopProcessTree 1)
              (\_ -> threadDelay 200000 >> ioError (userError "boom"))
          ) ::
          IO (Either SomeException ())
      either (const True) (const False) r `shouldBe` True
      pids <- concatMap (maybe [] pure . readMaybe) . lines <$> readFile (dir </> "pids")
      liveOf pids `shouldReturn` []

  it "finds no live process among pids that have exited" $ do
    (_, _, _, ph) <- createProcess (proc "true" [])
    Just pid <- getPid ph
    _ <- waitForProcess ph
    liveOf [fromIntegral pid] `shouldReturn` []
