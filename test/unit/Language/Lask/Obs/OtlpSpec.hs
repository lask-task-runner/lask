{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Obs.OtlpSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Language.Lask.Obs.Otlp
import Test.Hspec

spec :: Spec
spec = describe "OTLP export (spec 12.10)" $ do
  describe "otlpConfigFrom" $ do
    let endpointOf flag env = ocEndpoint <$> fst (otlpConfigFrom flag env)
    it "is off when nothing asks for it" $
      endpointOf Nothing [] `shouldBe` Nothing
    it "takes the option as a base endpoint" $ do
      endpointOf (Just "http://c:4318") [] `shouldBe` Just "http://c:4318/v1/traces"
      endpointOf (Just "http://c:4318/") [] `shouldBe` Just "http://c:4318/v1/traces"
      endpointOf (Just "http://c:4318/v1/traces") [] `shouldBe` Just "http://c:4318/v1/traces"
    it "reads the standard variables, the traces URL as it is" $ do
      endpointOf Nothing [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://e:4318")] `shouldBe` Just "http://e:4318/v1/traces"
      endpointOf Nothing [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://e:4318"), ("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT", "http://t/x")] `shouldBe` Just "http://t/x"
    it "is turned off by the variables, but not against the option" $ do
      endpointOf Nothing [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://e"), ("OTEL_SDK_DISABLED", "TRUE")] `shouldBe` Nothing
      endpointOf Nothing [("OTEL_EXPORTER_OTLP_ENDPOINT", "http://e"), ("OTEL_TRACES_EXPORTER", "none")] `shouldBe` Nothing
      endpointOf (Just "http://f") [("OTEL_SDK_DISABLED", "true")] `shouldBe` Just "http://f/v1/traces"
    it "reads headers, service name and resource attributes" $ do
      let Just cfg =
            fst $
              otlpConfigFrom
                (Just "http://c")
                [ ("OTEL_EXPORTER_OTLP_HEADERS", "authorization=Basic%20abc, x-team = ops"),
                  ("OTEL_RESOURCE_ATTRIBUTES", "service.name=ci,deployment.environment=prod")
                ]
      ocHeaders cfg `shouldBe` [("authorization", "Basic abc"), ("x-team", "ops")]
      ocServiceName cfg `shouldBe` "ci"
      ocResource cfg `shouldBe` [("deployment.environment", "prod")]
      ocServiceName <$> fst (otlpConfigFrom (Just "http://c") [("OTEL_SERVICE_NAME", "svc"), ("OTEL_RESOURCE_ATTRIBUTES", "service.name=ci")]) `shouldBe` Just "svc"
    it "warns of a protocol other than http/json" $ do
      snd (otlpConfigFrom (Just "http://c") [("OTEL_EXPORTER_OTLP_PROTOCOL", "grpc")]) `shouldSatisfy` (not . null)
      snd (otlpConfigFrom (Just "http://c") [("OTEL_EXPORTER_OTLP_PROTOCOL", "http/json")]) `shouldBe` []
  describe "trace identity" $ do
    it "keeps a 32-digit lowercase hex trace id, and hashes any other" $ do
      otelTraceId "4bf92f3577b34da6a3ce929d0e0e4736" `shouldBe` "4bf92f3577b34da6a3ce929d0e0e4736"
      otelTraceId "build-42" `shouldNotBe` "build-42"
      otelTraceId "build-42" `shouldBe` otelTraceId "build-42"
      length (show (otelTraceId "build-42")) `shouldBe` 34
      otelTraceId "00000000000000000000000000000000" `shouldNotBe` "00000000000000000000000000000000"
    it "reads a traceparent" $ do
      parseTraceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
        `shouldBe` Just ("4bf92f3577b34da6a3ce929d0e0e4736", "00f067aa0ba902b7")
      parseTraceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01" `shouldBe` Nothing
      parseTraceparent "garbage" `shouldBe` Nothing
  describe "encodeExport" $ do
    it "encodes spans in the OTLP JSON mapping" $ do
      let t0 = UTCTime (fromGregorian 2026 7 23) 0
          cfg = OtlpConfig "http://c/v1/traces" [] "lask" []
          sp = Span "build" "00f067aa0ba902b7" (Just "1111111111111111") t0 (addUTCTime 1.5 t0) [("lask.exec", AInt 3)] (StatusError "exit 1") []
          A.Object o = encodeExport cfg "4bf92f3577b34da6a3ce929d0e0e4736" "0.19.0" [sp]
          encoded = A.encode (A.Object o)
      KM.member "resourceSpans" o `shouldBe` True
      encoded `shouldSatisfy` contains "\"startTimeUnixNano\":\"1784764800000000000\""
      encoded `shouldSatisfy` contains "\"endTimeUnixNano\":\"1784764801500000000\""
      encoded `shouldSatisfy` contains "\"intValue\":\"3\""
      encoded `shouldSatisfy` contains "\"parentSpanId\":\"1111111111111111\""
      encoded `shouldSatisfy` contains "\"status\":{\"code\":2,\"message\":\"exit 1\"}"
  where
    contains needle hay = needle `BS.isInfixOf` BL.toStrict hay
