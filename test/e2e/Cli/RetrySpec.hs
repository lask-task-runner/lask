{-# LANGUAGE OverloadedStrings #-}

-- | Failures as the process ends with them, and retrying and waiting
-- with real commands: the exit code an uncaught 'Error' gives
-- (spec 8.10, 11.3), the masking of its message (12.8), the lines a
-- retry writes to the execution log (12.2, 15.7), and the stopping of
-- a command a @timeout@ abandons (15.7).
--
-- The waits are fractions of a second, and a bound on how long a run
-- takes is only there to tell a stopped command from one that ran to
-- its end, several seconds later.
module Cli.RetrySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Support.Harness
import Support.Pending (knownBug)
import Test.Hspec

program :: String
program =
  unlines
    [ "code(n: Number): Number = fail(error(n, \"code #{n}\"))",
      "exit_seven(): String = $[#local] sh -c 'echo nope >&2; exit 7'",
      "retried(): String = retry([0.01, 0.02], \\() -> exit_seven())",
      "declined(): String = retry_if([0, 0], \\(e: Error) -> e.code == 2, \\() -> exit_seven())",
      "polled(): String = until([0, 0], \\(v: String) -> v == \"ready\", \\() -> $[#local]{echo waiting})",
      "slow(): String = $[#local] sleep 5",
      "limited(): String = timeout(0.2, \\() -> slow())",
      "per_attempt(): String = retry([0], \\() -> timeout(0.2, \\() -> slow()))",
      "divided(): Number = 1 / 0",
      "missing(): String = read_file(\"missing.txt\", #local)",
      "leaked(--token!!: String): Number = fail(error(3, \"token #{token} rejected\"))",
      "faithful(--token!!: String): String = try { fail(error(3, \"token #{token} rejected\")) } catch (e) { e.message }"
    ]

-- | Run the program with these arguments, and how long it took.
timedRun :: FilePath -> [String] -> IO (Result, Double)
timedRun lask args =
  withProject [("main.lask", program)] $ \dir -> do
    start <- getMonotonicTime
    r <- runLask lask dir args ""
    end <- getMonotonicTime
    pure (r, end - start)

runIt :: FilePath -> [String] -> IO Result
runIt lask args = fst <$> timedRun lask args

spec :: Spec
spec = beforeAll findLask $ do
  describe "the exit code of an uncaught Error (spec 8.10, 11.3)" $ do
    let exitsWith code expected = it ("exits " <> show expected <> " for the code " <> code) $ \lask -> do
          r <- runIt lask ["run", "code", code]
          resExit r `shouldBe` expected
          resErr r `shouldSatisfy` isInfixOf ("code " <> code)
    exitsWith "1" 1
    exitsWith "42" 42
    exitsWith "255" 255
    exitsWith "0" 1
    exitsWith "-3" 1
    exitsWith "1.5" 1
    exitsWith "256" 1
    exitsWith "18446744073709551623" 1
    it "exits 1 for a code that is not an integer, however close to one" $ \lask ->
      knownBug "the code 7.0000000000000001 exits 7: exitCodeOf rounds the code through a Double, where it is 7, although the code is not an integer (7.0000000000000001 == 7 is false)" $ do
        r <- runIt lask ["run", "code", "7.0000000000000001"]
        resExit r `shouldBe` 1
    it "exits 2 for a runtime error and 3 for an external I/O error" $ \lask -> do
      runtime <- runIt lask ["run", "divided"]
      resExit runtime `shouldBe` 2
      resErr runtime `shouldSatisfy` isInfixOf "E-RUNTIME-DIV-BY-ZERO"
      io <- runIt lask ["run", "missing"]
      resExit io `shouldBe` 3
      resErr io `shouldSatisfy` isInfixOf "E-IO-FS"

  describe "the code a retry re-raises (spec 15.7)" $ do
    it "exits with the command's code once the delays run out" $ \lask -> do
      r <- runIt lask ["run", "retried"]
      resExit r `shouldBe` 7
      resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-COMMAND-NONZERO: nope"
    it "exits with the command's code when retry_if declines the failure" $ \lask -> do
      r <- runIt lask ["run", "declined"]
      resExit r `shouldBe` 7
      length (filter (" $ sh -c" `isInfixOf`) (lines (resErr r))) `shouldBe` 1
    it "exits 124 when the delays of until run out" $ \lask -> do
      r <- runIt lask ["run", "polled"]
      resExit r `shouldBe` 124
      resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-UNTIL-EXHAUSTED: condition not met after 3 checks"

  describe "the message of an uncaught Error (spec 8.10, 12.8)" $ do
    it "masks a secret in the diagnostic" $ \lask -> do
      r <- runIt lask ["run", "leaked", "--token", "recovery-e2e-token"]
      resExit r `shouldBe` 3
      resErr r `shouldNotSatisfy` isInfixOf "recovery-e2e-token"
      resErr r `shouldSatisfy` isInfixOf "token *** rejected"
    it "leaves the message alone as the program sees it" $ \lask -> do
      r <- runIt lask ["eval", "faithful", "--token", "recovery-e2e-token"]
      resOut r `shouldBe` "\"token recovery-e2e-token rejected\"\n"

  describe "the execution log of a retry (spec 12.2, 15.7)" $ do
    it "writes one line for each retry, at info, naming the attempt, the code and the delay" $ \lask -> do
      r <- runIt lask ["run", "retried"]
      let retries = filter ("retry:" `isInfixOf`) (lines (resErr r))
      length retries `shouldBe` 2
      mapM_ (`shouldSatisfy` isInfixOf " info ") retries
      zip3 retries ["attempt 1 of 3", "attempt 2 of 3"] ["in 0.01s", "in 0.02s"]
        `shouldSatisfy` all (\(l, a, d) -> a `isInfixOf` l && "code 7" `isInfixOf` l && d `isInfixOf` l)
    it "writes one line for each miss of until that a delay follows" $ \lask -> do
      r <- runIt lask ["run", "polled"]
      length (filter ("until:" `isInfixOf`) (lines (resErr r))) `shouldBe` 2
    it "writes the lines in JSON with the fields 12.2 requires" $ \lask -> do
      r <- runIt lask ["run", "--log-format", "json", "retried"]
      let objects = mapMaybe (A.decode . BL.pack) (lines (resErr r)) :: [A.Object]
          retries = [o | o <- objects, Just (A.String m) <- [KM.lookup "message" o], "retry:" `T.isPrefixOf` m]
      length retries `shouldBe` 2
      mapM_ (\o -> KM.lookup "level" o `shouldBe` Just "info") retries
      knownBug "an execution log line in JSON names its time ts, not timestamp, and has no context (12.2 requires timestamp, level, traceId, message and context); Obs.ExecLog.renderLogJson writes ts, level, message and traceId" $
        mapM_ (\o -> filter (not . (`KM.member` o)) ["timestamp", "level", "traceId", "message", "context"] `shouldBe` []) retries

  describe "timeout with real commands (spec 15.7, 8.7)" $ do
    it "stops the command at the limit and exits 124" $ \lask -> do
      (r, t) <- timedRun lask ["run", "limited"]
      resExit r `shouldBe` 124
      resErr r `shouldSatisfy` isInfixOf "E-RUNTIME-TIMEOUT: timed out after 0.2s"
      resErr r `shouldSatisfy` isInfixOf "] killed"
      t `shouldSatisfy` (< 4.5)
    it "bounds each attempt when it is inside a retry" $ \lask -> do
      (r, t) <- timedRun lask ["run", "per_attempt"]
      resExit r `shouldBe` 124
      let ls = lines (resErr r)
      length (filter (" $ sleep 5" `isInfixOf`) ls) `shouldBe` 2
      length (filter ("] killed" `isInfixOf`) ls) `shouldBe` 2
      length (filter ("retry: attempt 1 of 2 failed with code 124" `isInfixOf`) ls) `shouldBe` 1
      t `shouldSatisfy` (< 9)
