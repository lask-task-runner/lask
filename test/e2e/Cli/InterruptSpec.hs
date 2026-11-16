-- | Stopping a run of local commands on a signal (spec 11.12), and the
-- limits timeout puts on them (spec 8.7, 15.7), seen from the command
-- line against real processes.
module Cli.InterruptSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.List (isInfixOf)
import Support.Harness
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process
import Test.Hspec
import Text.Read (readMaybe)

-- | Start @lask run f@ in the background, wait until the file @started@
-- exists, and send it the signals given, a second apart. Returns its
-- exit code and stderr.
runSignalled :: FilePath -> FilePath -> [String] -> IO (Int, String)
runSignalled lask dir sigs = do
  (_, _, _, ph) <-
    createProcess
      (proc "sh" ["-c", "exec \"$0\" run f 2> stderr.txt", lask])
        { cwd = Just dir,
          std_in = NoStream
        }
  waitFor 400 (doesFileExist (dir </> "started"))
  threadDelay 300000
  Just pid <- getPid ph
  mapM_ (\s -> void (readProcess "kill" ["-s", s, show pid] "") >> threadDelay 1000000) sigs
  code <- waitForProcess ph
  err <- readFile (dir </> "stderr.txt")
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

-- | The pids a test's commands wrote to @pids@, one a line.
pidsIn :: FilePath -> IO [String]
pidsIn dir = do
  ls <- lines <$> readFile (dir </> "pids")
  length ls `seq` pure [l | l <- ls, Just _ <- [readMaybe l :: Maybe Int]]

alive :: [String] -> IO [String]
alive ps = do
  rs <- mapM (\p -> (\(c, _, _) -> (p, c)) <$> readCreateProcessWithExitCode (proc "kill" ["-0", p]) "") ps
  pure [p | (p, ExitSuccess) <- rs]

spec :: Spec
spec = beforeAll findLask $ do
  describe "a signal stops a run of local commands (spec 11.12)" $ do
    it "stops the command and what it started, runs finally, and exits 130 on SIGINT" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = try {",
                "  $[#local]{ sleep 30 & echo $! > pids; touch started; wait }",
                "} finally {",
                "  $[#local]{ touch cleaned }",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, err) <- runSignalled lask dir ["INT"]
          code `shouldBe` 130
          err `shouldContain` "E-RUNTIME-INTERRUPTED"
          doesFileExist (dir </> "cleaned") `shouldReturn` True
          pidsIn dir >>= alive >>= (`shouldBe` [])

    it "runs the finally block of every computation started with async, and exits 143 on SIGTERM" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "part(tag: String): String = try {",
                "  $[#local]{ sleep 30 }",
                "} finally {",
                "  $[#local]{ touch cleaned-#!{tag} }",
                "}",
                "f(): Array<String> = do {",
                "  a = async part(\"a\")",
                "  b = async part(\"b\")",
                "  c = async $[#local]{ touch started; sleep 30 }",
                "  all([a, b, c])",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, err) <- runSignalled lask dir ["TERM"]
          code `shouldBe` 143
          doesFileExist (dir </> "cleaned-a") `shouldReturn` True
          doesFileExist (dir </> "cleaned-b") `shouldReturn` True
          err `shouldNotContain` "W-ASYNC-UNAWAITED"

    it "cancels a computation nothing awaited, and does not report it" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = do {",
                "  h = async try { $[#local]{ sleep 30 } } finally { $[#local]{ touch cleaned } }",
                "  $[#local]{ touch started; sleep 30 }",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, err) <- runSignalled lask dir ["TERM"]
          code `shouldBe` 143
          doesFileExist (dir </> "cleaned") `shouldReturn` True
          err `shouldNotContain` "W-ASYNC-UNAWAITED"

    it "starts nothing that comes after the abandonment" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = do {",
                "  x = $[#local]{ touch started; sleep 30 }",
                "  $[#local]{ touch next }",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, _) <- runSignalled lask dir ["INT"]
          code `shouldBe` 130
          threadDelay 300000
          doesFileExist (dir </> "next") `shouldReturn` False

    it "runs no catch block for the signal" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = try {",
                "  $[#local]{ touch started; sleep 30 }",
                "} catch (e) {",
                "  $[#local]{ touch caught }",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, _) <- runSignalled lask dir ["TERM"]
          code `shouldBe` 143
          doesFileExist (dir </> "caught") `shouldReturn` False

    it "stops a finally block at once on a second signal, keeping the exit code" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = try {",
                "  $[#local]{ touch started; sleep 30 }",
                "} finally {",
                "  $[#local]{ sleep 30 & echo $! > pids; wait }",
                "  $[#local]{ touch after }",
                "}"
              ]
          )
        ]
        $ \dir -> do
          (code, err) <- runSignalled lask dir ["INT", "INT"]
          code `shouldBe` 130
          err `shouldContain` "again: stopping at once"
          threadDelay 300000
          doesFileExist (dir </> "after") `shouldReturn` False
          pidsIn dir >>= alive >>= (`shouldBe` [])

  describe "what all and race cancel (spec 15.6)" $
    it "leaves nothing running that a cancelled computation started with async" $ \lask -> do
      pendingWith "known bug: all and race cancel the other computations but not what those started with async, which runs on after the run has ended (spec 15.6, 8.7)"
      withProject
        [ ( "main.lask",
            unlines
              [ "f() = all([async do {",
                "  h = async $[#local]{ sleep 1; touch grand }",
                "  await h",
                "}, async $[#local]{ sleep 0.2; exit 4 }])"
              ]
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 4
          threadDelay 1500000
          doesFileExist (dir </> "grand") `shouldReturn` False

  describe "timeout on local commands (spec 8.7, 15.7)" $ do
    it "stops the command and what it started, logs it as killed, and exits 124" $ \lask ->
      withProject
        [("main.lask", "f(): String = timeout(0.5, \\() -> $[#local]{ sleep 30 & echo $! > pids; echo $$ >> pids; wait })\n")]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 124
          resErr r `shouldContain` "killed"
          resErr r `shouldContain` "E-RUNTIME-TIMEOUT"
          pidsIn dir >>= alive >>= (`shouldBe` [])

    it "waits for the finally block of the body it abandons" $ \lask ->
      withProject
        [ ( "main.lask",
            unlines
              [ "f(): String = timeout(0.3, \\() -> try {",
                "  $[#local]{ sleep 30 }",
                "} finally {",
                "  $[#local]{ sleep 1; touch cleaned }",
                "})"
              ]
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 124
          doesFileExist (dir </> "cleaned") `shouldReturn` True

    it "kills a command that ignores the termination request" $ \lask ->
      withProject
        [("main.lask", "f(): String = timeout(0.3, \\() -> $[#local]{ trap '' TERM; echo $$ > pids; while :; do sleep 0.1; done })\n")]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 124
          pidsIn dir >>= alive >>= (`shouldBe` [])

    it "names the limit in the failure a handler sees" $ \lask ->
      withProject
        [("main.lask", "f(): String = try { timeout(0.2, \\() -> $[#local]{ sleep 30 }) } catch (e) { \"#{e.code} #{e.message}\" }\n")]
        $ \dir -> do
          r <- runLask lask dir ["eval", "--output", "text", "f"] ""
          resExit r `shouldBe` 0
          resOut r `shouldSatisfy` isInfixOf "124"
          resOut r `shouldSatisfy` isInfixOf "0.2"
