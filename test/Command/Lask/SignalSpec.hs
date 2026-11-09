-- | Stopping a run on a signal, and removing what a killed one left
-- (spec 11.12), against the fake container engine.
module Command.Lask.SignalSpec (spec) where

import Command.Lask.Harness
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.List (isInfixOf, isPrefixOf)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process
import Test.Hspec

-- | A task that runs a container command for 30 seconds, with a
-- finally block that records that it ran.
project :: [(FilePath, String)]
project =
  [ ( "main.lask",
      unlines
        [ "long(): String = try {",
          "  $[#alpine:3.22.2] sleep 30",
          "} finally {",
          "  $[#local] touch cleaned",
          "}"
        ]
    )
  ]

-- | As 'project', with a finally block that takes 20 seconds, so that a
-- second signal arrives while it runs.
slowCleanup :: [(FilePath, String)]
slowCleanup =
  [ ( "main.lask",
      unlines
        [ "long(): String = try {",
          "  $[#alpine:3.22.2] sleep 30",
          "} finally {",
          "  $[#local] sleep 20",
          "}"
        ]
    )
  ]

-- | Start @lask run long@ in the background, wait until its container
-- command has started, and send it the signals given, a second apart.
-- Returns its exit code and stderr.
runSignalled :: FilePath -> FilePath -> FilePath -> [(String, String)] -> [String] -> IO (Int, String)
runSignalled lask dir state extra sigs = do
  void (runLaskEnv lask dir extra ["sync"] "")
  baseEnv <- getEnvironment
  let fullEnv = extra <> [(k, v) | (k, v) <- baseEnv, k `notElem` map fst extra]
      errFile = dir </> "stderr.txt"
  (_, _, _, ph) <-
    createProcess
      (proc "sh" ["-c", "exec \"$0\" run long 2> stderr.txt", lask])
        { cwd = Just dir,
          env = Just fullEnv,
          std_in = NoStream
        }
  waitFor 200 (any ("run " `isPrefixOf`) <$> calls state)
  -- The command has started once its run is recorded; give lask a
  -- moment to be waiting on it.
  threadDelay 300000
  Just pid <- getPid ph
  mapM_ (\s -> void (readProcess "kill" ["-s", s, show pid] "") >> threadDelay 1000000) sigs
  code <- waitForProcess ph
  err <- readFile errFile
  length err `seq` pure (exitOf code, err)
  where
    exitOf ExitSuccess = 0
    exitOf (ExitFailure n) = n

-- | Poll every 50ms until the condition holds, failing after @ticks@.
waitFor :: Int -> IO Bool -> IO ()
waitFor ticks cond
  | ticks <= 0 = expectationFailure "timed out waiting"
  | otherwise = do
      ok <- cond
      if ok then pure () else threadDelay 50000 >> waitFor (ticks - 1) cond

-- | Whether the reaper asked the engine for what the invocation left.
reaped :: FilePath -> IO Bool
reaped state = any ("rm -f lask-left-behind" `isInfixOf`) <$> calls state

spec :: Spec
spec = beforeAll findLask $ describe "signals (spec 11.12)" $ do
  it "labels every container with the invocation that started it" $ \lask ->
    withFakeDocker $ \state extra -> withProject [("main.lask", "f(): String = $[#alpine:3.22.2] echo hi\n")] $ \dir -> do
      void (runLaskEnv lask dir extra ["sync"] "")
      r <- runLaskEnv lask dir extra ["run", "f"] ""
      resExit r `shouldBe` 0
      cs <- calls state
      filter ("run " `isPrefixOf`) cs `shouldSatisfy` all ("--label io.lask.owner=" `isInfixOf`)

  it "runs the finally blocks on SIGTERM and exits 143, leaving nothing to the reaper" $ \lask ->
    withFakeDocker $ \state extra -> withProject project $ \dir -> do
      (code, err) <- runSignalled lask dir state extra ["TERM"]
      code `shouldBe` 143
      err `shouldSatisfy` isInfixOf "E-RUNTIME-INTERRUPTED"
      readFile (dir </> "cleaned") `shouldReturn` ""
      threadDelay 500000
      reaped state `shouldReturn` False

  it "runs the finally blocks on SIGINT and exits 130" $ \lask ->
    withFakeDocker $ \state extra -> withProject project $ \dir -> do
      (code, _) <- runSignalled lask dir state extra ["INT"]
      code `shouldBe` 130
      readFile (dir </> "cleaned") `shouldReturn` ""

  it "stops at once on a second signal, and the reaper removes the containers" $ \lask ->
    withFakeDocker $ \state extra -> withProject slowCleanup $ \dir -> do
      (code, err) <- runSignalled lask dir state extra ["TERM", "TERM"]
      code `shouldBe` 143
      err `shouldSatisfy` isInfixOf "again: stopping at once"
      waitFor 100 (reaped state)
      -- The clean-up's own command is killed with lask, not left behind.
      out <- readProcess "sh" ["-c", "pgrep -f 'slee[p] 20' || true"] ""
      out `shouldBe` ""

  it "has the reaper remove the containers of an invocation killed with SIGKILL" $ \lask ->
    withFakeDocker $ \state extra -> withProject project $ \dir -> do
      (code, _) <- runSignalled lask dir state extra ["KILL"]
      code `shouldBe` (-9)
      waitFor 100 (reaped state)

  it "passes SIGTERM on to the program cmd runs (spec 11.8)" $ \lask ->
    withProject [("main.lask", "internal command { \"sh\" } on #local\n")] $ \dir -> do
      (code, out, _) <-
        readCreateProcessWithExitCode
          ( ( proc
                "sh"
                [ "-c",
                  "\"$0\" cmd sh -c 'trap \"echo got-TERM; exit 7\" TERM; sleep 30 & wait' 2> err.txt & p=$!; "
                    <> "for i in $(seq 100); do grep -q 'sh -c' err.txt 2>/dev/null && break; sleep 0.1; done; sleep 0.3; "
                    <> "kill -TERM $p; wait $p",
                  lask
                ]
            )
              {cwd = Just dir}
          )
          ""
      code `shouldBe` ExitFailure 7
      out `shouldSatisfy` isInfixOf "got-TERM"
