{-# LANGUAGE OverloadedStrings #-}

-- | Execution log lines written by the built-in @log@ (spec 15.12).
--
-- Stdout carries the evaluation result and nothing else (9.5), so a
-- task's own progress messages go to stderr as execution log entries
-- (9.6, 12.2). Renderers are pure so the formats can be unit-tested;
-- the sinks are thin writers over them, as in
-- "Language.Lask.Obs.CommandLog".
module Language.Lask.Obs.ExecLog
  ( LogSink,
    noLogSink,
    textLogSink,
    jsonLogSink,
    renderLogText,
    renderLogJson,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Language.Lask.Obs.Events (TraceId, formatTimestamp)

-- | Emit one already-masked log message. A sink must be thread-safe:
-- concurrent tasks (6.3) share one.
type LogSink = Text -> IO ()

noLogSink :: LogSink
noLogSink _ = pure ()

textLogSink :: (Text -> IO ()) -> LogSink
textLogSink write message = do
  now <- getCurrentTime
  write (renderLogText now message)

jsonLogSink :: TraceId -> (Text -> IO ()) -> LogSink
jsonLogSink traceId write message = do
  now <- getCurrentTime
  write (renderLogJson traceId now message)

-- | @<timestamp> info <message>@: the line carries a level and a
-- message, which is what 12.2 requires of the text format.
renderLogText :: UTCTime -> Text -> Text
renderLogText now message = formatTimestamp now <> " info " <> message

renderLogJson :: TraceId -> UTCTime -> Text -> Text
renderLogJson traceId now message =
  TE.decodeUtf8 . BL.toStrict . A.encode $
    A.object
      [ ("ts", A.toJSON (formatTimestamp now)),
        ("level", "info"),
        ("message", A.toJSON message),
        ("traceId", A.toJSON traceId)
      ]
