{-# LANGUAGE OverloadedStrings #-}

-- | Command execution logs (spec 12.3): real-time relay of child
-- process output to lask's own stderr, with a @start@\/@exit@ pair
-- per command execution.
--
-- Renderers are pure ('renderCommandLogText', 'renderCommandLogJson')
-- so the formats can be unit-tested; the sinks are thin handle
-- writers over them.
module Language.Lask.Obs.CommandLog
  ( CommandLogKind (..),
    CommandLog (..),
    CommandLogSink,
    noCommandLog,
    newLineWriter,
    textCommandLog,
    jsonCommandLog,
    renderCommandLogText,
    renderCommandLogJson,
    summarizeCommand,
  )
where

import Control.Concurrent.MVar (newMVar, withMVar)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Language.Lask.Obs.Events (TraceId, formatTimestamp)
import System.IO (Handle, hFlush)

data CommandLogKind
  = ClStart
  | -- | Relayed output line: file descriptor (1 = stdout,
    -- 2 = stderr, spec 6.6 stream numbering) and content.
    ClLine Int Text
  | ClExit Int
  deriving (Show, Eq)

data CommandLog = CommandLog
  { clTimestamp :: UTCTime,
    -- | Environment summary in environment-expression notation
    -- (spec 12.3): @#local@, @#\<image\>@, or @#env("name")@.
    clEnvSummary :: Text,
    -- | 13.1 metadata representation; named environments include
    -- @name@ (spec 12.3 JSON fields).
    clEnvJson :: A.Value,
    -- | Execution number: 1-based, unique within the top-level
    -- execution, also across concurrent commands (spec 12.3).
    clExec :: Int,
    -- | Full command string; rendered on the start line only.
    clCommand :: Text,
    clKind :: CommandLogKind
  }
  deriving (Show, Eq)

type CommandLogSink = CommandLog -> IO ()

noCommandLog :: CommandLogSink
noCommandLog _ = pure ()

-- | A whole-line writer serialized by a lock: relay lines are emitted
-- from concurrent stream readers (and interleave with execution
-- events), and 'TIO.hPutStrLn' alone is not atomic.
newLineWriter :: Handle -> IO (Text -> IO ())
newLineWriter h = do
  lock <- newMVar ()
  pure $ \line -> withMVar lock $ \_ -> do
    TIO.hPutStrLn h line
    hFlush h

-- | Default text format sink (spec 12.3):
-- @\<timestamp\> [\<env\>:\<exec\>] \<kind\> \<content\>@.
textCommandLog :: (Text -> IO ()) -> CommandLogSink
textCommandLog write = write . renderCommandLogText

-- | JSON Lines sink for @--format json@ (spec 12.2, 12.3).
jsonCommandLog :: TraceId -> (Text -> IO ()) -> CommandLogSink
jsonCommandLog traceId write = write . renderCommandLogJson traceId

-- | Text format (spec 12.3): the command string appears on the start
-- line (@$@) only; later lines correlate through the execution
-- number.
renderCommandLogText :: CommandLog -> Text
renderCommandLogText cl =
  formatTimestamp (clTimestamp cl)
    <> " ["
    <> clEnvSummary cl
    <> ":"
    <> T.pack (show (clExec cl))
    <> "] "
    <> kindText
  where
    kindText = case clKind cl of
      ClStart -> "$ " <> summarizeCommand (clCommand cl)
      ClLine fd content -> T.pack (show fd) <> "| " <> content
      ClExit code -> "exit " <> T.pack (show code)

-- | JSON Lines format (spec 12.2, 12.3): every line carries
-- timestamp\/level\/traceId\/exec; @command@ and @env@ appear on the
-- start line only, relay lines correlate through @exec@.
renderCommandLogJson :: TraceId -> CommandLog -> Text
renderCommandLogJson traceId cl =
  TE.decodeUtf8 . BL.toStrict . A.encode . A.object $
    [ ("timestamp", A.String (formatTimestamp (clTimestamp cl))),
      ("level", A.String level),
      ("traceId", A.String traceId),
      ("exec", A.Number (fromIntegral (clExec cl)))
    ]
      <> kindFields
  where
    -- Relay lines and the start line are info; exit is info on 0 and
    -- warn on non-zero (spec 12.3 level rules).
    level = case clKind cl of
      ClExit code | code /= 0 -> "warn"
      _ -> "info"
    kindFields = case clKind cl of
      ClStart ->
        [ ("event", A.String "start"),
          ("command", A.String (summarizeCommand (clCommand cl))),
          ("env", clEnvJson cl)
        ]
      ClLine fd content ->
        [ ("stream", A.String (T.pack (show fd))),
          ("message", A.String content)
        ]
      ClExit code ->
        [ ("event", A.String "exit"),
          (AK.fromText "code", A.Number (fromIntegral code))
        ]

-- | Command summary: implementation-defined truncation (spec 12.3).
summarizeCommand :: Text -> Text
summarizeCommand cmd
  | T.length cmd > 256 = T.take 253 cmd <> "..."
  | otherwise = cmd
