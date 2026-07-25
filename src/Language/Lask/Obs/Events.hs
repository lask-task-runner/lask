{-# LANGUAGE OverloadedStrings #-}

-- | Observability (spec chapter 12): trace identifiers and execution
-- events (CallEvent \/ ReturnEvent \/ FailEvent, 12.5\/13.3).
module Language.Lask.Obs.Events
  ( TraceId,
    newTraceId,
    Event (..),
    EventKind (..),
    EventSink,
    noSink,
    eventJson,
    encodeEvent,
    summarizeValue,
    formatTimestamp,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Numeric (showHex)
import Language.Lask.Runtime.Value (Value (..))
import Language.Lask.Serialize (valueToJson)
import System.Random (randomRIO)

type TraceId = Text

-- | UTC ISO 8601 with fixed 3-digit (millisecond) fraction, matching
-- the spec examples (12.3): @2026-07-23T10:15:04.123Z@.
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

-- | Random 128-bit hex identifier (spec 12.4).
newTraceId :: IO TraceId
newTraceId = do
  parts <- mapM (\_ -> randomRIO (0 :: Word, maxBound)) [1 :: Int, 2]
  pure (T.concat (map (T.pack . pad . flip showHex "") parts))
  where
    pad s = replicate (16 - length s) '0' <> s

data EventKind = EvCall | EvReturn | EvFail
  deriving (Show, Eq)

data Event = Event
  { evKind :: EventKind,
    evTraceId :: TraceId,
    evTimestamp :: UTCTime,
    -- | 'Language.Lask.Serialize.functionRefJson' of the callee.
    evFunction :: A.Value,
    -- | @args@ (call), @result@ (return) or @error@ (fail) payload.
    evPayload :: [(Text, A.Value)]
  }
  deriving (Show)

type EventSink = Event -> IO ()

noSink :: EventSink
noSink _ = pure ()

eventJson :: Event -> A.Value
eventJson ev =
  A.object $
    [ ("kind", A.String kindText),
      ("traceId", A.String (evTraceId ev)),
      ("timestamp", A.String (formatTimestamp (evTimestamp ev))),
      ("function", evFunction ev)
    ]
      <> [(AK.fromText k, v) | (k, v) <- evPayload ev]
  where
    kindText = case evKind ev of
      EvCall -> "call"
      EvReturn -> "return"
      EvFail -> "fail"

encodeEvent :: Event -> Text
encodeEvent = TE.decodeUtf8 . BL.toStrict . A.encode . eventJson

-- | Bounded value summary for event payloads (spec 12.7: summarize,
-- do not dump full values).
summarizeValue :: Value -> A.Value
summarizeValue = valueToJson . limit (3 :: Int)
  where
    limit depth v = case v of
      VString t
        | T.length t > 256 -> VString (T.take 253 t <> "...")
        | otherwise -> v
      VArray xs
        | depth <= 0 -> VString ("<array of " <> tshow (V.length xs) <> ">")
        | otherwise -> VArray (V.map (limit (depth - 1)) xs)
      VMap m
        | depth <= 0 -> VString "<map>"
        | otherwise -> VMap (Map.map (limit (depth - 1)) m)
      VRecord m
        | depth <= 0 -> VString "<record>"
        | otherwise -> VRecord (Map.map (limit (depth - 1)) m)
      _ -> v
    tshow = T.pack . show
