{-# LANGUAGE OverloadedStrings #-}

-- | A stand-in for an OpenTelemetry collector (spec 12.10): it accepts
-- OTLP/HTTP JSON on @/v1/traces@ and keeps the spans it was sent.
module Support.FakeCollector
  ( CollectedSpan (..),
    withFakeCollector,
    attrText,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)

data CollectedSpan = CollectedSpan
  { csTraceId :: Text,
    csSpanId :: Text,
    csParent :: Maybe Text,
    csName :: Text,
    -- | Attribute values, each as its OTLP @AnyValue@ object.
    csAttrs :: [(Text, A.Value)],
    -- | 0 unset, 1 ok, 2 error.
    csStatus :: Int
  }
  deriving (Show)

-- | Serve on a free port; the action gets the base endpoint and a way
-- to read the spans received so far, with the headers of each request.
withFakeCollector :: (String -> IO ([CollectedSpan], [[(Text, Text)]]) -> IO a) -> IO a
withFakeCollector action = do
  received <- newIORef []
  let app req respond
        | rawPathInfo req == "/v1/traces" = do
            body <- strictRequestBody req
            let hs = [(TE.decodeUtf8Lenient (CI.foldedCase k), TE.decodeUtf8Lenient v) | (k, v) <- requestHeaders req]
            atomicModifyIORef' received (\xs -> (xs <> [(body, hs)], ()))
            respond (responseLBS status200 [("Content-Type", "application/json")] "{}")
        | otherwise = respond (responseLBS status404 [] "")
      collected = do
        xs <- readIORef received
        pure (concatMap (spansOf . fst) xs, map snd xs)
  testWithApplication (pure app) $ \port -> action ("http://127.0.0.1:" <> show port) collected

-- | The string value of an attribute.
attrText :: Text -> CollectedSpan -> Maybe Text
attrText k sp = lookup k (csAttrs sp) >>= field "stringValue" >>= str

spansOf :: BL.ByteString -> [CollectedSpan]
spansOf body = fromMaybe [] $ do
  doc <- A.decode body
  rss <- field "resourceSpans" doc >>= arr
  pure
    [ CollectedSpan
        { csTraceId = fromMaybe "" (field "traceId" sp >>= str),
          csSpanId = fromMaybe "" (field "spanId" sp >>= str),
          csParent = field "parentSpanId" sp >>= str,
          csName = fromMaybe "" (field "name" sp >>= str),
          csAttrs =
            [ (k, v)
            | a <- fromMaybe [] (field "attributes" sp >>= arr),
              Just k <- [field "key" a >>= str],
              Just v <- [field "value" a]
            ],
          csStatus = fromMaybe 0 (field "status" sp >>= field "code" >>= num)
        }
    | rs <- rss,
      ss <- fromMaybe [] (field "scopeSpans" rs >>= arr),
      sp <- fromMaybe [] (field "spans" ss >>= arr)
    ]
  where
    arr v = case v of
      A.Array xs -> Just (V.toList xs)
      _ -> Nothing
    num v = case v of
      A.Number n -> toBoundedInteger n
      _ -> Nothing

field :: Text -> A.Value -> Maybe A.Value
field k v = case v of
  A.Object o -> KM.lookup (AK.fromText k) o
  _ -> Nothing

str :: A.Value -> Maybe Text
str v = case v of
  A.String t -> Just t
  _ -> Nothing
