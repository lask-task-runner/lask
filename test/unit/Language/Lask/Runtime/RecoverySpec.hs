{-# LANGUAGE OverloadedStrings #-}

-- | Failure and recovery, evaluated in process with a scripted command
-- runner: how a failure becomes an 'Error' value and propagates
-- (spec 8.10), @try@ / @catch@ / @finally@ on the paths that are not
-- abandonment (6.9, 8.10), and the functions that retry and wait
-- (15.7), with the lines they write to the execution log (12.2).
--
-- Delays are 0 wherever the waiting itself is not the point, so that
-- no example waits for real time but those about timing, and those
-- only bound how long a wait takes from below.
module Language.Lask.Runtime.RecoverySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forM_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner, RtHooks (..))
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.Elaborate (elaborateProgram)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue)
import Test.Hspec

-- | What a run of @f@ came to: its value as canonical JSON, or the code
-- and the 'Error' value of its failure; the commands it ran, in order;
-- and the lines it wrote to the execution log.
data Run = Run
  { outcome :: Either (Maybe ErrorCode, Text) Text,
    ran :: [Text],
    logged :: [Text]
  }

-- | The commands of the scripted runner, by their first word:
--
--   * @ok X@ prints @X@;
--   * @exit N@ exits @N@, printing @exit N@ to stderr;
--   * @codes A B ...@ exits with the next of its codes at each call,
--     the last one again once they run out, and prints @ok@ for a 0;
--   * @count@ prints how many times it has been run;
--   * @hang@ waits until it is stopped, and @hang_once@ only the first
--     time it is run.
--
-- Anything else succeeds with no output.
scripted :: IO (CommandRunner, IO [Text])
scripted = do
  seen <- newIORef []
  calls <- newIORef (Map.empty :: Map.Map Text Int)
  let runner _ cmd = do
        atomicModifyIORef' seen (\xs -> (xs <> [cmd], ()))
        n <- atomicModifyIORef' calls (\m -> let k = Map.findWithDefault 0 cmd m + 1 in (Map.insert cmd k m, k))
        case T.words cmd of
          ["ok", x] -> pure (Right (0, x, ""))
          ["exit", c] -> pure (Right (read (T.unpack c), "", "exit " <> c))
          ("codes" : cs) -> case read (T.unpack (cs !! min (n - 1) (length cs - 1))) of
            0 -> pure (Right (0, "ok", ""))
            c -> pure (Right (c, "", "exit " <> T.pack (show c)))
          ["count"] -> pure (Right (0, T.pack (show n), ""))
          ["hang"] -> threadDelay 10000000 >> pure (Right (0, "", ""))
          ["hang_once"]
            | n == 1 -> threadDelay 10000000 >> pure (Right (0, "", ""))
            | otherwise -> pure (Right (0, "ok", ""))
          _ -> pure (Right (0, "", ""))
  pure (runner, readIORef seen)

-- | No file exists.
noFiles :: FileRunner
noFiles _ op = pure $ case op of
  FileRead p -> Left (ioFailure EIoFs ("no such file: '" <> p <> "'"))
  FileExists _ -> Right (VBool False)
  FileListDir _ -> Right (VArray V.empty)
  FileGlob _ -> Right (VArray V.empty)
  FileWrite {} -> Right VVoid
  FileRemove _ -> Right VVoid
  FileMakeDir _ -> Right VVoid

-- | Evaluate @f@ of the program; a closure is applied to no arguments.
runF :: Text -> IO Run
runF src = do
  (runner, seen) <- scripted
  logs <- newIORef []
  let reader p = pure (if p == "main.lask" then Right src else Left "not found")
      sink line = atomicModifyIORef' logs (\xs -> (xs <> [line], ()))
  loaded <- loadProgramWith reader "main.lask"
  out <- case loaded >>= \prog -> (,) prog <$> validateProgram prog of
    Left ds -> pure (Left (Nothing, "static: " <> T.pack (show (map diagCode ds))))
    Right (prog, scopes) -> case elaborateProgram prog scopes of
      Left ds -> pure (Left (Nothing, "static: " <> T.pack (show (map diagCode ds))))
      Right cp -> do
        ctx <- mkRtCtx cp "" (RtHooks runner noFiles (\_ env k -> k env) sink noAsyncTracker (\_ -> pure Nothing))
        res <- try $ do
          v <- topValue ctx ("main.lask", "f")
          case v of
            VClosure _ -> applyValue ctx v [] []
            _ -> pure v
        pure $ case res of
          Right v -> Right (encodeValue v)
          Left lf -> Left (lfCode lf, encodeValue (lfError lf))
  Run out <$> seen <*> readIORef logs

-- | 'runF' of @f() = <expression>@.
runE :: Text -> IO Run
runE e = runF ("f() = " <> e <> "\n")

-- | 'runE', and how long it took, in seconds.
timed :: Text -> IO (Run, Double)
timed e = do
  start <- getMonotonicTime
  r <- runE e
  end <- getMonotonicTime
  pure (r, end - start)

evalsTo :: Text -> Text -> Expectation
evalsTo e v = (outcome <$> runE e) `shouldReturn` Right v

-- | Fails with this code and this 'Error' value.
failsWith :: Text -> Maybe ErrorCode -> Text -> Expectation
failsWith e c err = (outcome <$> runE e) `shouldReturn` Left (c, err)

-- | The code of the failure, if it failed.
codeOf :: Run -> Maybe (Maybe ErrorCode)
codeOf = either (Just . fst) (const Nothing) . outcome

-- | Fails with this code, whatever the 'Error' value.
failsWithCode :: Text -> ErrorCode -> Expectation
failsWithCode e c = (codeOf <$> runE e) `shouldReturn` Just (Just c)

-- | Bodies that fail in each way a failure can come about (8.10), the
-- code they fail with, and the 'Error' value it becomes.
failures :: [(Text, Maybe ErrorCode, Text)]
failures =
  [ ("$[#local]{exit 7}", Just ERuntimeCommandNonzero, "{\"code\":7,\"message\":\"exit 7\"}"),
    ("fail(error(42, \"mine\"))", Nothing, "{\"code\":42,\"message\":\"mine\"}"),
    ("fail(error(300, \"out of range\"))", Nothing, "{\"code\":300,\"message\":\"out of range\"}"),
    ("fail(error(1.5, \"not an integer\"))", Nothing, "{\"code\":1.5,\"message\":\"not an integer\"}"),
    ("to_string(1 / 0)", Just ERuntimeDivByZero, "{\"code\":2,\"message\":\"division by zero\"}"),
    ("to_string([1][5])", Just ERuntimeAccess, "{\"code\":2,\"message\":\"array index out of range: 5 (length 1)\"}"),
    ("to_json(backoff_fixed(0 - 1, 1))", Just ERuntimeValue, "{\"code\":2,\"message\":\"backoff_fixed takes a finite, non-negative delay, not -1\"}"),
    ("read_file(\"missing.txt\", #local)", Just EIoFs, "{\"code\":3,\"message\":\"no such file: 'missing.txt'\"}"),
    ("timeout(0.05, \\() -> $[#local]{hang})", Just ERuntimeTimeout, "{\"code\":124,\"message\":\"timed out after 0.05s\"}"),
    ("to_string(until([0], \\(v: Number) -> v > 1, \\() -> 1))", Just ERuntimeUntilExhausted, "{\"code\":124,\"message\":\"condition not met after 2 checks; last value: 1\"}")
  ]

-- | A body as a zero-argument function of @String@: the result type is
-- written, since a lambda's body does not take it from the call.
body :: Text -> Text
body b = "\\(): String -> " <> b

spec :: Spec
spec = do
  describe "the Error value of a failure (spec 8.10)" $ do
    forM_ failures $ \(b, code, err) -> do
      it ("propagates " <> T.unpack b <> " uncaught, as that Error") $
        (outcome <$> runF ("f(): String = " <> b <> "\n")) `shouldReturn` Left (code, err)
      it ("gives the handler of recover the same Error for " <> T.unpack b) $
        evalsTo ("recover(" <> body b <> ", \\(e: Error) -> to_json(e))") (encodeValue (VString err))
    it "keeps a code that is not an exit code as it is, when caught" $ do
      evalsTo "try { fail(error(0, \"zero\")) } catch (e) { e.code }" "0"
      evalsTo "try { fail(error(0 - 3, \"negative\")) } catch (e) { e.code }" "-3"
    it "makes error(code, message) the record {code, message}" $
      evalsTo "error(3, \"m\") == {code: 3, message: \"m\"}" "true"

  describe "recover (spec 8.10)" $ do
    it "returns the body's value and does not evaluate the handler" $ do
      r <- runE "recover(\\() -> $[#local]{ok body}, \\(e: Error) -> $[#local]{ok handler})"
      outcome r `shouldBe` Right "\"body\""
      ran r `shouldBe` ["ok body"]
    it "returns the handler's value when the body fails" $ do
      r <- runE "recover(\\() -> $[#local]{exit 7}, \\(e: Error) -> $[#local]{ok handler})"
      outcome r `shouldBe` Right "\"handler\""
      ran r `shouldBe` ["exit 7", "ok handler"]
    it "lets a failure of the handler propagate, without catching it again" $ do
      r <- runE "recover(\\() -> $[#local]{exit 7}, \\(e: Error) -> $[#local]{exit 8})"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":8,\"message\":\"exit 8\"}")
      ran r `shouldBe` ["exit 7", "exit 8"]
    it "lets an enclosing recover catch the failure of a handler" $
      evalsTo
        "recover(\\() -> recover(\\(): Number -> fail(error(1, \"a\")), \\(e: Error): Number -> fail(error(e.code + 1, \"b\"))), \\(e: Error) -> e.code)"
        "2"
    it "re-raises with fail the Error it was given, unchanged" $
      failsWith "recover(\\() -> $[#local]{exit 7}, \\(e: Error): String -> fail(e))" Nothing "{\"code\":7,\"message\":\"exit 7\"}"

  describe "try and catch (spec 6.9, 8.10)" $ do
    it "does not catch a failure of its own catch block" $ do
      r <- runE "try { $[#local]{exit 7} } catch (e) { $[#local]{exit 8} }"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":8,\"message\":\"exit 8\"}")
      ran r `shouldBe` ["exit 7", "exit 8"]
    it "binds the Error of the failure to the name of the catch" $
      evalsTo "try { $[#local]{exit 7} } catch (e) { \"#{e.code}: #{e.message}\" }" "\"7: exit 7\""

  describe "try and finally, on success and failure (spec 6.9, 8.10)" $ do
    it "runs the finally block once after a body that succeeds, and returns the body's value" $ do
      r <- runE "try { $[#local]{ok body} } finally { $[#local]{ok finally} }"
      outcome r `shouldBe` Right "\"body\""
      ran r `shouldBe` ["ok body", "ok finally"]
    it "runs the finally block once after a body that fails, and propagates the body's failure unchanged" $ do
      r <- runE "try { $[#local]{exit 7} } finally { $[#local]{ok finally} }"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":7,\"message\":\"exit 7\"}")
      ran r `shouldBe` ["exit 7", "ok finally"]
    it "propagates a failure of the finally block after a body that succeeds" $ do
      r <- runE "try { $[#local]{ok body} } finally { $[#local]{exit 9} }"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":9,\"message\":\"exit 9\"}")
      ran r `shouldBe` ["ok body", "exit 9"]
    it "propagates a failure of the finally block in place of the body's" $ do
      r <- runE "try { $[#local]{exit 7} } finally { $[#local]{exit 9} }"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":9,\"message\":\"exit 9\"}")
      ran r `shouldBe` ["exit 7", "exit 9"]
    -- The same Error and the same commands; the expansion's fail(e)
    -- re-raises without the diagnostic code of the original failure.
    it "behaves as the expansion through recover that 6.9 gives it" $
      forM_ [(b, f) | b <- ["$[#local]{ok body}", "$[#local]{exit 7}", "fail(error(300, \"m\"))"], f <- ["$[#local]{ok finally}", "$[#local]{exit 9}"]] $ \(b, f) -> do
        sugar <- runF ("f(): String = try { " <> b <> " } finally { " <> f <> " }")
        expanded <- runF ("f(): String = do {\n  v = recover(" <> body b <> ", \\(e: Error): String -> do {\n    " <> f <> "\n    fail(e)\n  })\n  " <> f <> "\n  v\n}")
        let seen r = (either (Left . snd) Right (outcome r), ran r)
        seen sugar `shouldBe` seen expanded

  describe "try, catch and finally together (spec 6.9, 8.10)" $ do
    let whole b h = "try { " <> b <> " } catch (e) { " <> h <> " } finally { $[#local]{ok finally} }"
    it "runs only the body and the finally block when the body succeeds" $ do
      r <- runE (whole "$[#local]{ok body}" "$[#local]{ok handler}")
      outcome r `shouldBe` Right "\"body\""
      ran r `shouldBe` ["ok body", "ok finally"]
    it "returns the catch block's value, the finally block run once after it" $ do
      r <- runE (whole "$[#local]{exit 7}" "$[#local]{ok handler}")
      outcome r `shouldBe` Right "\"handler\""
      ran r `shouldBe` ["exit 7", "ok handler", "ok finally"]
    it "propagates a failure of the catch block, the finally block run once after it" $ do
      r <- runE (whole "$[#local]{exit 7}" "$[#local]{exit 8}")
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":8,\"message\":\"exit 8\"}")
      ran r `shouldBe` ["exit 7", "exit 8", "ok finally"]
    it "does not catch a failure of the finally block in its own catch block" $ do
      r <- runE "try { $[#local]{ok body} } catch (e) { $[#local]{ok handler} } finally { $[#local]{exit 9} }"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":9,\"message\":\"exit 9\"}")
      ran r `shouldBe` ["ok body", "exit 9"]
    it "runs the finally block of each attempt a retry makes" $ do
      r <- runE "retry([0, 0], \\() -> try { $[#local]{exit 7} } finally { $[#local]{ok finally} })"
      ran r `shouldBe` concat (replicate 3 ["exit 7", "ok finally"])
    it "runs the finally block once around a timeout that fails" $ do
      r <- runE "try { timeout(0.05, \\() -> $[#local]{hang}) } finally { $[#local]{ok finally} }"
      outcome r `shouldBe` Left (Just ERuntimeTimeout, "{\"code\":124,\"message\":\"timed out after 0.05s\"}")
      ran r `shouldBe` ["hang", "ok finally"]

  describe "strategies (spec 15.7)" $ do
    it "builds each strategy count delays long" $ do
      evalsTo "backoff_fixed(0.5, 2)" "[0.5,0.5]"
      evalsTo "backoff_linear(1, 0.5, 3)" "[1,1.5,2]"
      evalsTo "backoff_linear(0, 0, 2)" "[0,0]"
      evalsTo "backoff_exponential(2, 0, 3)" "[2,0,0]"
      evalsTo "backoff_exponential(3, 1, 2)" "[3,3]"
      evalsTo "backoff_exponential(1, 2, 0)" "[]"
      evalsTo "backoff_linear(1, 2, 0)" "[]"
    forM_
      [ "backoff_fixed(1, 0 - 1)",
        "backoff_fixed(1, 0.5)",
        "backoff_linear(0 - 1, 1, 1)",
        "backoff_linear(1, 0 - 1, 1)",
        "backoff_linear(1, 1, 0 - 1)",
        "backoff_linear(1, 1, 2.5)",
        "backoff_exponential(0 - 1, 2, 1)",
        "backoff_exponential(1, 0 - 2, 1)",
        "backoff_exponential(1, 2, 0 - 1)",
        "backoff_exponential(1, 2, 0.5)"
      ]
      $ \e ->
        it ("rejects " <> T.unpack e) $ failsWithCode ("to_json(" <> e <> ")") ERuntimeValue
    it "rejects a linear strategy growing past a representable delay" $
      failsWithCode "to_json(backoff_linear(pow(10, 308), pow(10, 308), 2))" ERuntimeValue
    it "accepts an exponential strategy of zeros, which grows past nothing" $
      evalsTo "backoff_exponential(0, pow(10, 300), 3)" "[0,0,0]"
    it "accepts an exponential strategy whose delays are representable, though a power of its factor is not" $
      evalsTo "size(backoff_exponential(pow(10, 0 - 300), pow(10, 200), 3))" "3"
    it "accepts a count of up to 1000000" $
      evalsTo "size(backoff_fixed(0, 1000000))" "1000000"
    it "rejects a count above 1000000, saying so" $ do
      r <- runE "backoff_fixed(0, 1000001)"
      codeOf r `shouldBe` Just (Just ERuntimeValue)
      either snd (const "") (outcome r) `shouldSatisfy` T.isInfixOf "takes a count of at most 1000000, not 1000001"

  describe "the number of attempts (spec 15.7)" $ do
    forM_ [0 .. 3 :: Int] $ \n -> do
      let delays = "[" <> T.intercalate ", " (replicate n "0") <> "]"
      it ("makes " <> show (n + 1) <> " attempts for " <> show n <> " delays, in retry, retry_if and until") $ do
        r <- runE ("retry(" <> delays <> ", \\() -> $[#local]{exit 7})")
        length (ran r) `shouldBe` n + 1
        r' <- runE ("retry_if(" <> delays <> ", \\(e: Error) -> true, \\() -> $[#local]{exit 7})")
        length (ran r') `shouldBe` n + 1
        r'' <- runE ("until(" <> delays <> ", \\(v: String) -> false, \\() -> $[#local]{count})")
        length (ran r'') `shouldBe` n + 1
    it "stops at the attempt that succeeds, and returns its value" $ do
      r <- runE "retry([0, 0, 0, 0], \\() -> $[#local]{codes 7 7 0})"
      outcome r `shouldBe` Right "\"ok\""
      length (ran r) `shouldBe` 3
    it "checks a strategy before the first attempt, whatever the attempts would do" $ do
      forM_
        [ "retry([0, 0 - 1], \\() -> $[#local]{ok body})",
          "retry_if([0 - 1], \\(e: Error) -> $[#local]{ok when} == \"when\", \\() -> $[#local]{exit 7})",
          "until([0, 0 - 1], \\(v: String) -> true, \\() -> $[#local]{ok body})"
        ]
        $ \e -> do
          r <- runE e
          codeOf r `shouldBe` Just (Just ERuntimeValue)
          ran r `shouldBe` []

  describe "the failure retry re-raises (spec 15.7)" $ do
    forM_ failures $ \(b, code, err) -> do
      it ("re-raises " <> T.unpack b <> " unchanged once the delays run out") $
        failsWith ("retry([0], " <> body b <> ")") code err
      it ("re-raises " <> T.unpack b <> " unchanged when retry_if declines it") $
        failsWith ("retry_if([0, 0], \\(e: Error) -> false, " <> body b <> ")") code err

  describe "the predicate of retry_if (spec 15.7)" $ do
    it "is given the Error of the failure" $ do
      r <- runE "retry_if([0], \\(e: Error) -> e.code == 7 && e.message == \"exit 7\", \\() -> $[#local]{codes 7 0})"
      outcome r `shouldBe` Right "\"ok\""
    it "stops at the first failure it declines, which is re-raised" $ do
      r <- runE "retry_if([0, 0, 0], \\(e: Error) -> e.code == 7, \\() -> $[#local]{codes 7 8 0})"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":8,\"message\":\"exit 8\"}")
      length (ran r) `shouldBe` 2
    it "propagates its own failure, in place of the body's, without retrying" $ do
      r <- runE "retry_if([0, 0], \\(e: Error): Bool -> fail(error(9, \"when\")), \\() -> $[#local]{exit 7})"
      outcome r `shouldBe` Left (Nothing, "{\"code\":9,\"message\":\"when\"}")
      ran r `shouldBe` ["exit 7"]

  describe "until (spec 15.7)" $ do
    it "checks at once, without a delay, and returns the value accepted" $ do
      (r, t) <- timed "until([5], \\(v: String) -> v == \"1\", \\() -> $[#local]{count})"
      outcome r `shouldBe` Right "\"1\""
      t `shouldSatisfy` (< 4)
    it "gives the condition each value, and returns the first it accepts" $ do
      r <- runE "until([0, 0, 0, 0], \\(v: String) -> v == \"3\", \\() -> $[#local]{count})"
      outcome r `shouldBe` Right "\"3\""
      length (ran r) `shouldBe` 3
    it "fails with code 124, the number of checks and the last value, once the delays run out" $
      failsWith
        "until([0, 0], \\(v: String) -> false, \\() -> $[#local]{count})"
        (Just ERuntimeUntilExhausted)
        "{\"code\":124,\"message\":\"condition not met after 3 checks; last value: \\\"3\\\"\"}"
    it "masks a secret in the last value it names" $ do
      r <- runE "until([], \\(v: String) -> false, \\() -> mark_secret(\"recovery-spec-secret-value\"))"
      either snd id (outcome r) `shouldSatisfy` (not . T.isInfixOf "recovery-spec-secret-value")
    it "lets a failure of the body propagate unchanged, without checking again" $ do
      r <- runE "until([0, 0], \\(v: String) -> false, \\() -> $[#local]{exit 7})"
      outcome r `shouldBe` Left (Just ERuntimeCommandNonzero, "{\"code\":7,\"message\":\"exit 7\"}")
      length (ran r) `shouldBe` 1
    it "lets a failure of the condition propagate, without checking again" $ do
      r <- runE "until([0, 0], \\(v: String): Bool -> fail(error(9, \"done\")), \\() -> $[#local]{count})"
      outcome r `shouldBe` Left (Nothing, "{\"code\":9,\"message\":\"done\"}")
      length (ran r) `shouldBe` 1

  describe "timeout (spec 15.7)" $ do
    it "rejects a limit that is not positive before running the body" $
      forM_ ["0", "0 - 1", "0 - 0.5"] $ \s -> do
        r <- runE ("timeout(" <> s <> ", \\() -> $[#local]{ok body})")
        codeOf r `shouldBe` Just (Just ERuntimeValue)
        ran r `shouldBe` []
    it "accepts a fraction of a second" $
      evalsTo "timeout(2.5, \\() -> $[#local]{ok body})" "\"body\""
    it "returns the body's own failure when it fails in time, even one of code 124" $ do
      failsWith "timeout(5, \\() -> $[#local]{exit 7})" (Just ERuntimeCommandNonzero) "{\"code\":7,\"message\":\"exit 7\"}"
      failsWith "timeout(5, \\(): String -> fail(error(124, \"mine\")))" Nothing "{\"code\":124,\"message\":\"mine\"}"
    it "is the inner limit that passes first, when the inner one is shorter" $
      failsWith "timeout(5, \\() -> timeout(0.05, \\() -> $[#local]{hang}))" (Just ERuntimeTimeout) "{\"code\":124,\"message\":\"timed out after 0.05s\"}"
    it "is the outer limit that passes first, when the outer one is shorter" $
      failsWith "timeout(0.05, \\() -> timeout(5, \\() -> $[#local]{hang}))" (Just ERuntimeTimeout) "{\"code\":124,\"message\":\"timed out after 0.05s\"}"
    it "abandons an inner try along with the rest of the body" $
      failsWith "timeout(0.1, \\() -> try { timeout(5, \\() -> $[#local]{hang}) } catch (e) { \"caught\" })" (Just ERuntimeTimeout) "{\"code\":124,\"message\":\"timed out after 0.1s\"}"
    it "raises an inner timeout as a failure a try in the outer body catches" $
      evalsTo "timeout(5, \\() -> try { timeout(0.05, \\() -> $[#local]{hang}) } catch (e) { \"caught #{e.code}\" })" "\"caught 124\""

  describe "timeout composed with retrying and waiting (spec 15.7)" $ do
    it "bounds each attempt when it is inside the retry" $ do
      r <- runE "retry([0, 0], \\() -> timeout(0.05, \\() -> $[#local]{hang}))"
      outcome r `shouldBe` Left (Just ERuntimeTimeout, "{\"code\":124,\"message\":\"timed out after 0.05s\"}")
      ran r `shouldBe` ["hang", "hang", "hang"]
    it "lets a later attempt succeed after an attempt that timed out" $ do
      r <- runE "retry([0], \\() -> timeout(0.05, \\() -> $[#local]{hang_once}))"
      outcome r `shouldBe` Right "\"ok\""
      ran r `shouldBe` ["hang_once", "hang_once"]
    it "bounds every attempt and every delay together when it is around the retry" $ do
      (r, t) <- timed "timeout(0.2, \\() -> retry([5, 5], \\() -> $[#local]{exit 7}))"
      outcome r `shouldBe` Left (Just ERuntimeTimeout, "{\"code\":124,\"message\":\"timed out after 0.2s\"}")
      ran r `shouldBe` ["exit 7"]
      t `shouldSatisfy` (< 4)
    it "gives up on an until with code 124 of the timeout, not of until" $ do
      (r, t) <- timed "timeout(0.2, \\() -> until([5], \\(v: String) -> false, \\() -> $[#local]{count}))"
      outcome r `shouldBe` Left (Just ERuntimeTimeout, "{\"code\":124,\"message\":\"timed out after 0.2s\"}")
      t `shouldSatisfy` (< 4)
    it "is not retried by until, whose body's failure propagates" $ do
      r <- runE "until([0, 0], \\(v: String) -> true, \\() -> timeout(0.05, \\() -> $[#local]{hang}))"
      codeOf r `shouldBe` Just (Just ERuntimeTimeout)
      ran r `shouldBe` ["hang"]

  describe "waiting for the delays (spec 15.7)" $ do
    it "waits for each delay between attempts" $ do
      (r, t) <- timed "retry([0.1, 0.1], \\() -> $[#local]{exit 7})"
      length (ran r) `shouldBe` 3
      t `shouldSatisfy` (>= 0.2)
    it "waits for each delay between checks" $ do
      (r, t) <- timed "until([0.1, 0.1], \\(v: String) -> false, \\() -> $[#local]{count})"
      length (ran r) `shouldBe` 3
      t `shouldSatisfy` (>= 0.2)

  describe "the execution log of retrying and waiting (spec 12.2, 15.7)" $ do
    it "writes one line for each retry, naming the attempt, the code and the delay" $ do
      r <- runE "retry([0.01, 0.02], \\() -> $[#local]{exit 7})"
      length (logged r) `shouldBe` 2
      forM_ (zip3 (logged r) ["1", "2"] ["0.01s", "0.02s"]) $ \(line, attempt, delay) -> do
        line `shouldSatisfy` T.isInfixOf ("attempt " <> attempt)
        line `shouldSatisfy` T.isInfixOf "code 7"
        line `shouldSatisfy` T.isInfixOf delay
    it "writes no line for the attempt that succeeds or for the last one" $ do
      ok <- runE "retry([0, 0, 0], \\() -> $[#local]{codes 7 0})"
      length (logged ok) `shouldBe` 1
      out <- runE "retry([0], \\() -> $[#local]{exit 7})"
      length (logged out) `shouldBe` 1
    it "writes no line for a failure retry_if declines" $ do
      r <- runE "retry_if([0, 0], \\(e: Error) -> false, \\() -> $[#local]{exit 7})"
      logged r `shouldBe` []
    it "writes one line for each miss of until that a delay follows" $ do
      r <- runE "until([0, 0], \\(v: String) -> v == \"3\", \\() -> $[#local]{count})"
      length (logged r) `shouldBe` 2
      hit <- runE "until([0, 0], \\(v: String) -> true, \\() -> $[#local]{count})"
      logged hit `shouldBe` []
