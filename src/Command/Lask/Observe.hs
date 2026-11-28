{-# LANGUAGE OverloadedStrings #-}

-- | What a command that evaluates reports of itself besides its logs:
-- the run summary of @--summary@ (spec 12.9) and the trace exported
-- over OTLP (12.10). Both are read off the sinks the run already
-- writes to, and both are finished however the run ends.
module Command.Lask.Observe
  ( resolveTrace,
    Observation (..),
    withObservation,
  )
where

import Command.Lask.Options (CommonOpts (..), ObsOpts (..))
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM, forM_, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Version (showVersion)
import Language.Lask.ErrorCode (AdvisoryCode (..), advisoryText)
import Language.Lask.Obs.CommandLog (CommandLogSink)
import Language.Lask.Obs.Events (EventSink, SpanId, TraceId, newSpanId, newTraceId)
import Language.Lask.Obs.Otlp
import Language.Lask.Obs.Summary
import Paths_lask (version)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))

-- | The trace identifier of a run (spec 12.5): @--trace-id@, else the
-- trace of @TRACEPARENT@, whose span the run then continues, else a new
-- one.
resolveTrace :: CommonOpts -> IO (TraceId, Maybe SpanId)
resolveTrace opts = case optTraceId opts of
  Just t -> pure (t, Nothing)
  Nothing -> do
    tp <- lookupEnv "TRACEPARENT"
    case tp >>= parseTraceparent . T.pack of
      Just (t, s) -> pure (t, Just s)
      Nothing -> (\t -> (t, Nothing)) <$> newTraceId

data Observation = Observation
  { -- | Wrap the command execution log sink.
    obCommandLog :: CommandLogSink -> CommandLogSink,
    -- | Wrap the execution event sink.
    obEvents :: EventSink -> EventSink,
    -- | The span of the run itself, the parent of its first calls.
    obRootSpan :: SpanId,
    -- | The run is being stopped by a signal.
    obInterrupted :: IO ()
  }

-- | Run the rest of a command that evaluates, observed. However it ends
-- — 'exitWith' is an exception, so a failure's exit passes through
-- here, after the failure is reported — the trace is sent and then the
-- summary written, last on stderr.
withObservation ::
  ObsOpts ->
  CommonOpts ->
  (TraceId, Maybe SpanId) ->
  (Text -> IO ()) ->
  -- | The subcommand: @run@, @eval@ or @cmd@.
  Text ->
  Subject ->
  FilePath ->
  (Observation -> IO a) ->
  IO a
withObservation obs opts (traceId, parent) writeErr verb subject modulePath body = do
  sc <- newSummaryCollector
  root <- newSpanId
  env <- getEnvironment
  let (cfg, warnings) = otlpConfigFrom (obsOtlpEndpoint obs) env
  mapM_ (writeErr . advisoryLine opts traceId) warnings
  tracer <- forM cfg $ \c -> newTracer c traceId (T.pack (showVersion version)) root (RootInfo rootName rootAttrs parent)
  let ob =
        Observation
          { obCommandLog = \s -> recordCommands sc (maybe s (`tracerCommandLog` s) tracer),
            obEvents = maybe id tracerEvents tracer,
            obRootSpan = root,
            obInterrupted = markInterrupted sc
          }
  r <- try (body ob)
  let code = case r of
        Right _ -> ExitSuccess
        Left ex -> fromMaybe (ExitFailure 1) (fromException ex)
  interrupted <- wasInterrupted sc
  forM_ tracer $ \t -> do
    warning <- finishTracer t (exitInt code) interrupted
    mapM_ (writeErr . advisoryLine opts traceId) warning
  when (obsSummary obs) $ do
    sm <- collectSummary sc subject modulePath code
    now <- getCurrentTime
    if optLogJson opts
      then writeErr (renderSummaryJson traceId now sm)
      else mapM_ writeErr (renderSummaryText sm)
  either (\ex -> throwIO (ex :: SomeException)) pure r
  where
    (subjectName, subjectAttr) = case subject of
      SubjectFunction f -> (f, ("lask.function", AStr f))
      SubjectCommand w -> (w, ("lask.command", AStr w))
    rootName = "lask " <> verb <> " " <> subjectName
    rootAttrs =
      [("lask.module", AStr (T.pack modulePath)), subjectAttr]
        <> [("lask.trace_id", AStr traceId) | otelTraceId traceId /= traceId]
    exitInt c = case c of
      ExitSuccess -> 0
      ExitFailure n -> n

-- | The advisory @W-OBS-EXPORT@ (spec 14.2) at run time.
advisoryLine :: CommonOpts -> TraceId -> Text -> Text
advisoryLine opts traceId message
  | optLogJson opts =
      TE.decodeUtf8 . BL.toStrict . A.encode . A.object $
        [ ("code", A.String code),
          ("severity", "warning"),
          ("stage", "runtime"),
          ("message", A.String message),
          ("traceId", A.String traceId)
        ]
  | otherwise = code <> ": " <> message
  where
    code = advisoryText WObsExport
