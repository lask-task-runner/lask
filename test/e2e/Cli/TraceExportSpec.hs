{-# LANGUAGE OverloadedStrings #-}

-- | The trace of a run, exported over OTLP (spec 12.10), and the span
-- identifiers of the JSON Lines it is read off (12.3, 13.3).
module Cli.TraceExportSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isInfixOf, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Support.FakeCollector
import Support.Harness
import Test.Hspec

src :: String
src =
  "token!!: String = \"s3cr3t-value-123\"\n\
  \build() = $[#local] echo built #{token}\n\
  \all() = do {\n\
  \  build()\n\
  \  a = async build()\n\
  \  await(a)\n\
  \}\n\
  \bad() = $[#local] sh -c \"exit 3\"\n"

traceparent :: String
traceparent = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

spec :: Spec
spec = beforeAll findLask $ describe "trace export (spec 12.10)" $ do
  it "sends a span for the run, each call and each command, nested as they ran" $ \lask ->
    withFakeCollector $ \url collected -> withProject [("main.lask", src)] $ \dir -> do
      r <- runLask lask dir ["run", "--otlp-endpoint", url, "all"] ""
      resExit r `shouldBe` 0
      (spans, _) <- collected
      let named n = filter ((== n) . csName) spans
          commands = filter (T.isPrefixOf "$ " . csName) spans
      [root] <- pure (named "lask run all")
      csParent root `shouldBe` Nothing
      attrText "lask.function" root `shouldBe` Just "all"
      [allSpan] <- pure (named "all")
      csParent allSpan `shouldBe` Just (csSpanId root)
      let builds = named "build"
      length builds `shouldBe` 2
      map csParent builds `shouldBe` replicate 2 (Just (csSpanId allSpan))
      length commands `shouldBe` 2
      sort (map csParent commands) `shouldBe` sort (map (Just . csSpanId) builds)
      length (filter ((== csTraceId root) . csTraceId) spans) `shouldBe` length spans
      -- What is exported is masked as stderr is (12.8).
      show spans `shouldNotSatisfy` isInfixOf "s3cr3t-value-123"
      mapMaybe (attrText "process.command_line") commands `shouldSatisfy` all (T.isInfixOf "***")

  it "marks a failed run, call and command as errors, and keeps the exit code" $ \lask ->
    withFakeCollector $ \url collected -> withProject [("main.lask", src)] $ \dir -> do
      r <- runLaskEnv lask dir [("OTEL_EXPORTER_OTLP_ENDPOINT", url)] ["run", "bad"] ""
      resExit r `shouldBe` 3
      (spans, _) <- collected
      let statusOf n = map csStatus (filter ((== n) . csName) spans)
      statusOf "lask run bad" `shouldBe` [2]
      statusOf "bad" `shouldBe` [2]
      statusOf "$ sh -c \"exit 3\"" `shouldBe` [2]
      [root] <- pure (filter ((== "lask run bad") . csName) spans)
      lookup "process.exit.code" (csAttrs root) `shouldBe` Just (A.object [("intValue", "3")])

  it "continues the trace of TRACEPARENT, which also names the run's trace" $ \lask ->
    withFakeCollector $ \url collected -> withProject [("main.lask", src)] $ \dir -> do
      r <- runLaskEnv lask dir [("TRACEPARENT", traceparent)] ["run", "--log-format", "json", "--otlp-endpoint", url, "all"] ""
      resExit r `shouldBe` 0
      (spans, _) <- collected
      map csTraceId spans `shouldSatisfy` all (== "4bf92f3577b34da6a3ce929d0e0e4736")
      [root] <- pure (filter ((== "lask run all") . csName) spans)
      csParent root `shouldBe` Just "00f067aa0ba902b7"
      resErr r `shouldSatisfy` isInfixOf "\"traceId\":\"4bf92f3577b34da6a3ce929d0e0e4736\""

  it "maps a --trace-id that is not a trace id, and keeps it as an attribute" $ \lask ->
    withFakeCollector $ \url collected -> withProject [("main.lask", src)] $ \dir -> do
      _ <- runLask lask dir ["run", "--trace-id", "build-42", "--otlp-endpoint", url, "all"] ""
      (spans, _) <- collected
      [root] <- pure (filter ((== "lask run all") . csName) spans)
      attrText "lask.trace_id" root `shouldBe` Just "build-42"
      T.length (csTraceId root) `shouldBe` 32

  it "sends the headers of OTEL_EXPORTER_OTLP_HEADERS" $ \lask ->
    withFakeCollector $ \url collected -> withProject [("main.lask", src)] $ \dir -> do
      _ <- runLaskEnv lask dir [("OTEL_EXPORTER_OTLP_HEADERS", "x-token=abc%3D")] ["run", "--otlp-endpoint", url, "all"] ""
      (_, headers) <- collected
      headers `shouldSatisfy` (not . null)
      map (lookup "x-token") headers `shouldSatisfy` all (== Just "abc=")

  it "warns when the trace cannot be sent, and the run is unaffected" $ \lask ->
    withProject [("main.lask", src)] $ \dir -> do
      r <- runLask lask dir ["eval", "--otlp-endpoint", "http://127.0.0.1:9", "all"] ""
      resExit r `shouldBe` 0
      resErr r `shouldSatisfy` isInfixOf "W-OBS-EXPORT: could not send the trace to http://127.0.0.1:9/v1/traces"

  it "puts the span ids on the events and the start lines of the JSON Lines" $ \lask ->
    withProject [("main.lask", src)] $ \dir -> do
      r <- runLask lask dir ["run", "--log-format", "json", "all"] ""
      resExit r `shouldBe` 0
      let objs = mapMaybe (A.decode . BLC.pack) (lines (resErr r)) :: [A.Object]
          get k o = case KM.lookup (AK.fromText k) o of
            Just (A.String s) -> Just s
            _ -> Nothing
          callsOf n = [o | o <- objs, get "kind" o == Just "call", (KM.lookup "function" o >>= nameOf) == Just n]
          nameOf v = case v of
            A.Object f -> get "name" f
            _ -> Nothing
          starts = [o | o <- objs, get "event" o == Just "start"]
          buildSpans = mapMaybe (get "spanId") (callsOf "build")
      [allCall] <- pure (callsOf "all")
      length buildSpans `shouldBe` 2
      map (get "parentSpanId") (callsOf "build") `shouldBe` replicate 2 (get "spanId" allCall)
      sort (map (get "parentSpanId") starts) `shouldBe` sort (map Just buildSpans)
      map (get "spanId") starts `shouldSatisfy` all (/= Nothing)
      -- The run's own span is the parent of the first call.
      (get "parentSpanId" allCall :: Maybe Text) `shouldSatisfy` (/= Nothing)
