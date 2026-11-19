{-# LANGUAGE OverloadedStrings #-}

-- | Asynchrony and cancellation through the evaluator (spec 6.3, 6.9,
-- 8.6, 8.7, 8.10, 15.6, 15.7), against a scripted command runner, so
-- that what was started, finished and stopped can be read back.
module Language.Lask.Runtime.ConcurrencySpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Language.Lask.ErrorCode
import Support.Eval
import Test.Hspec

-- A computation that fails at once can fail before the others have
-- started their commands, leaving nothing to stop and no finally
-- block entered. The ones that fail here first wait a tenth of a
-- second, so that what race and all do to the others is seen.

-- | Evaluate @f@ against a fresh scripted runner, returning the result,
-- what the script recorded once the evaluation returned, and how many
-- seconds it took.
evalF :: Text -> IO (Either (Maybe ErrorCode, Text) Text, [Text], Double)
evalF src = do
  (runner, script) <- scripted
  t0 <- getCurrentTime
  r <- evalWith runner src "f"
  t1 <- getCurrentTime
  seen <- events script
  pure (r, seen, realToFrac (diffUTCTime t1 t0))

-- | What the script recorded a while after the evaluation returned, to
-- see that nothing it abandoned goes on.
evalSettled :: Text -> IO (Either (Maybe ErrorCode, Text) Text, [Text])
evalSettled src = do
  (runner, script) <- scripted
  r <- evalWith runner src "f"
  threadDelay 600000
  seen <- events script
  pure (r, seen)

codeOf :: Either (Maybe ErrorCode, Text) Text -> Maybe ErrorCode
codeOf = either fst (const Nothing)

-- | The @code@ of the error value of a failure.
errorCode :: Either (Maybe ErrorCode, Text) Text -> Maybe Text
errorCode (Left (_, j)) = Just (T.takeWhile (/= ',') (T.drop 1 (snd (T.breakOn ":" j))))
errorCode _ = Nothing

count :: Text -> [Text] -> Int
count x = length . filter (== x)

spec :: Spec
spec = do
  describe "race (spec 15.6)" $ do
    it "returns the first to finish, wherever it stands, and stops the others" $ do
      (r, seen, _) <- evalF "f() = race([async $[#local]{wait 2 slow}, async $[#local]{wait 0.05 fast}])"
      r `shouldBe` Right "\"fast\""
      seen `shouldContain` ["stop slow"]
      seen `shouldNotContain` ["end slow"]
    it "fails with a failure that comes first, unchanged" $ do
      (r, seen, _) <- evalF "f() = race([async $[#local]{wait 2 slow}, async do {\n  w = $[#local]{wait 0.1 settle}\n  $[#local]{exit 5 bad}\n}])"
      codeOf r `shouldBe` Just ERuntimeCommandNonzero
      errorCode r `shouldBe` Just "5"
      seen `shouldContain` ["stop slow"]
    it "returns the result of a single handle" $ do
      (r, _, _) <- evalF "f() = race([async $[#local]{note only}])"
      r `shouldBe` Right "\"only\""
    it "returns once the finally blocks of the others have ended" $ do
      (r, seen, _) <- evalF "f() = race([async try { $[#local]{wait 2 loser} } finally { $[#local]{wait 0.2 cleanup} }, async $[#local]{wait 0.05 fast}])"
      r `shouldBe` Right "\"fast\""
      seen `shouldContain` ["end cleanup"]
    it "lets nothing a loser would have done next happen" $ do
      (r, seen) <- evalSettled "f() = race([async do {\n  x = $[#local]{wait 0.3 first}\n  $[#local]{note after}\n}, async $[#local]{wait 0.05 fast}])"
      r `shouldBe` Right "\"fast\""
      seen `shouldNotContain` ["after"]
    it "stops what a loser started with async" $ do
      (r, seen) <- evalSettled "f() = race([async do {\n  h = async $[#local]{wait 0.4 grand}\n  await h\n}, async $[#local]{wait 0.05 fast}])"
      r `shouldBe` Right "\"fast\""
      seen `shouldNotContain` ["end grand"]

  describe "all (spec 15.6)" $ do
    it "returns the results in the order of the handles, not of finishing" $ do
      (r, _, _) <- evalF "f() = all([async $[#local]{wait 0.2 a}, async $[#local]{wait 0.05 b}])"
      r `shouldBe` Right "[\"a\",\"b\"]"
    it "runs the computations at the same time" $ do
      (r, _, secs) <- evalF "f() = all([async $[#local]{wait 0.4 a}, async $[#local]{wait 0.4 b}, async $[#local]{wait 0.4 c}])"
      r `shouldBe` Right "[\"a\",\"b\",\"c\"]"
      secs `shouldSatisfy` (< 1.1)
    it "fails at the first failure in time, wherever it stands, and stops the rest" $ do
      (r, seen, secs) <- evalF "f() = all([async $[#local]{wait 2 slow}, async do {\n  w = $[#local]{wait 0.1 settle}\n  $[#local]{exit 4 bad}\n}])"
      errorCode r `shouldBe` Just "4"
      seen `shouldContain` ["stop slow"]
      secs `shouldSatisfy` (< 1.5)
    it "fails with the failure that came first when several fail" $ do
      (r, _, _) <- evalF "f() = all([async do {\n  x = $[#local]{wait 0.2 x}\n  $[#local]{exit 3 late}\n}, async $[#local]{exit 4 early}])"
      errorCode r `shouldBe` Just "4"
    it "returns once the finally blocks of the others have ended" $ do
      (r, seen, _) <- evalF "f() = all([async try { $[#local]{wait 2 other} } finally { $[#local]{wait 0.2 cleanup} }, async do {\n  w = $[#local]{wait 0.1 settle}\n  $[#local]{exit 4 bad}\n}])"
      errorCode r `shouldBe` Just "4"
      seen `shouldContain` ["end cleanup"]
    it "stops what the others started with async, leaving none running" $ do
      (r, seen) <- evalSettled "f() = all([async do {\n  h = async $[#local]{wait 0.4 grand}\n  await h\n}, async do {\n  w = $[#local]{wait 0.1 settle}\n  $[#local]{exit 4 bad}\n}])"
      errorCode r `shouldBe` Just "4"
      seen `shouldNotContain` ["end grand"]
    it "is the empty array for no handles" $ do
      (r, _, _) <- evalF "hs: Array<AsyncHandle<Number>> = []\nf() = all(hs)"
      r `shouldBe` Right "[]"
    it "reads a handle given twice, or already awaited, as await does" $ do
      (r, seen, _) <- evalF "f() = do {\n  h = async $[#local]{note once}\n  x = await h\n  all([h, h])\n}"
      r `shouldBe` Right "[\"once\",\"once\"]"
      count "once" seen `shouldBe` 1

  describe "await (spec 6.3, 8.6)" $ do
    it "re-raises the same failure every time" $ do
      (r, seen, _) <- evalF "f() = do {\n  h = async $[#local]{exit 6 bad}\n  a = try {\n    x = await h\n    0\n  } catch (e) { e.code }\n  b = try {\n    y = await h\n    0\n  } catch (e) { e.code }\n  a + b\n}"
      r `shouldBe` Right "12"
      count "bad" seen `shouldBe` 1
    it "starts the computation without waiting for it" $ do
      (r, seen, _) <- evalF "f() = do {\n  h = async $[#local]{wait 0.2 bg}\n  x = $[#local]{note caller}\n  await h\n}"
      r `shouldBe` Right "\"bg\""
      -- The caller goes on while the computation runs: it gets there
      -- before the computation ends.
      takeWhile (/= "end bg") seen `shouldContain` ["caller"]

  describe "timeout and the finally blocks it abandons (spec 8.10, 15.7)" $ do
    it "waits for the finally block of the body it abandons, past its limit" $ do
      (r, seen, secs) <- evalF "f() = timeout(0.1, \\() -> try { $[#local]{wait 2 body} } finally { $[#local]{wait 0.3 cleanup} })"
      codeOf r `shouldBe` Just ERuntimeTimeout
      seen `shouldBe` ["start body", "stop body", "start cleanup", "end cleanup"]
      secs `shouldSatisfy` (>= 0.4)
    it "abandons a finally block already running at the limit, and does not start it again" $ do
      (r, seen, _) <- evalF "f() = timeout(0.1, \\() -> try { $[#local]{note body} } finally { $[#local]{wait 2 cleanup} })"
      codeOf r `shouldBe` Just ERuntimeTimeout
      seen `shouldBe` ["body", "start cleanup", "stop cleanup"]
    it "lets an enclosing timeout abandon the finally block an inner one waits for" $ do
      (r, seen, secs) <- evalF "f() = timeout(0.3, \\() -> timeout(0.05, \\() -> try { $[#local]{wait 5 body} } finally { $[#local]{wait 5 cleanup} }))"
      codeOf r `shouldBe` Just ERuntimeTimeout
      seen `shouldContain` ["stop cleanup"]
      secs `shouldSatisfy` (< 2)
    it "stops what the body started with async and left running" $ do
      (r, seen) <- evalSettled "f() = timeout(0.1, \\() -> do {\n  h = async $[#local]{wait 0.5 bg}\n  $[#local]{wait 2 body}\n})"
      codeOf r `shouldBe` Just ERuntimeTimeout
      seen `shouldContain` ["stop bg"]
      seen `shouldNotContain` ["end bg"]
    it "runs the finally block of a computation the body started" $ do
      (r, seen, _) <- evalF "f() = timeout(0.1, \\() -> do {\n  h = async try { $[#local]{wait 2 bg} } finally { $[#local]{note fin} }\n  await h\n})"
      codeOf r `shouldBe` Just ERuntimeTimeout
      seen `shouldContain` ["fin"]
    it "leaves alone what a body that ends in time started" $ do
      (r, seen) <- evalSettled "f() = timeout(2, \\() -> do {\n  h = async $[#local]{wait 0.2 bg}\n  \"ok\"\n})"
      r `shouldBe` Right "\"ok\""
      seen `shouldContain` ["end bg"]

  describe "finally runs exactly once (spec 6.9, 8.10)" $ do
    it "on success" $ do
      (r, seen, _) <- evalF "f() = try { $[#local]{note body} } finally { $[#local]{note fin} }"
      r `shouldBe` Right "\"body\""
      seen `shouldBe` ["body", "fin"]
    it "on failure, which goes on" $ do
      (r, seen, _) <- evalF "f() = try { $[#local]{exit 3 body} } finally { $[#local]{note fin} }"
      errorCode r `shouldBe` Just "3"
      seen `shouldBe` ["body", "fin"]
    it "after the catch block, with catch and finally both given" $ do
      (r, seen, _) <- evalF "f() = try { $[#local]{exit 3 body} } catch (e) { $[#local]{note handler} } finally { $[#local]{note fin} }"
      r `shouldBe` Right "\"handler\""
      seen `shouldBe` ["body", "handler", "fin"]
    it "after a catch block that fails, whose failure goes on" $ do
      (r, seen, _) <- evalF "f() = try { $[#local]{exit 3 body} } catch (e) { $[#local]{exit 8 handler} } finally { $[#local]{note fin} }"
      errorCode r `shouldBe` Just "8"
      seen `shouldBe` ["body", "handler", "fin"]
    it "innermost first, when a failure crosses several" $ do
      (r, seen, _) <- evalF "f() = try {\n  try { $[#local]{exit 3 body} } finally { $[#local]{note inner} }\n} finally { $[#local]{note outer} }"
      errorCode r `shouldBe` Just "3"
      seen `shouldBe` ["body", "inner", "outer"]

  describe "sequencing (spec 8.4, 8.5, 8.10)" $ do
    it "runs the statements of a do block in order, stopping at a failure" $ do
      (r, seen, _) <- evalF "f() = do {\n  a = $[#local]{note one}\n  b = $[#local]{exit 2 two}\n  $[#local]{note three}\n}"
      errorCode r `shouldBe` Just "2"
      seen `shouldBe` ["one", "two"]
    it "stops map at the first failure" $ do
      (r, seen, _) <- evalF "f() = map([\\() -> $[#local]{note a}, \\() -> $[#local]{exit 5 b}, \\() -> $[#local]{note c}], \\(g: Function<String>) -> g())"
      errorCode r `shouldBe` Just "5"
      seen `shouldBe` ["a", "b"]
    it "evaluates only the branch taken" $ do
      (r, seen, _) <- evalF "f() = if (1 < 2) { $[#local]{note yes} } else { $[#local]{note no} }"
      r `shouldBe` Right "\"yes\""
      seen `shouldBe` ["yes"]
