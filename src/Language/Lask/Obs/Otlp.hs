{-# LANGUAGE OverloadedStrings #-}

-- | Trace export over OTLP\/HTTP with JSON encoding (spec 12.10).
--
-- A run is one trace: a root span for the run, a span per declared
-- function call, read off the execution events (12.6), and a span per
-- command execution, read off the command execution log (12.3). Both
-- are masked before they reach a sink (12.8), so what is exported is
-- what stderr would show. Spans are sent in batches as they end, every
-- few seconds and at the end of the run; a failure to send is a
-- warning and never changes the run.
--
-- The encoding and the configuration are pure so that they can be
-- unit-tested; 'Tracer' is the stateful part.
module Language.Lask.Obs.Otlp
  ( -- * Configuration
    OtlpConfig (..),
    otlpConfigFrom,
    -- * Trace identity
    otelTraceId,
    parseTraceparent,
    -- * Spans
    Attr (..),
    SpanStatus (..),
    SpanEvent (..),
    Span (..),
    encodeExport,
    -- * Tracing a run
    Tracer,
    RootInfo (..),
    newTracer,
    tracerEvents,
    tracerCommandLog,
    finishTracer,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isHexDigit, isLower)
import qualified Data.CaseInsensitive as CI
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Lask.Obs.CommandLog (CommandLog (..), CommandLogKind (..), CommandLogSink, summarizeCommand)
import Language.Lask.Obs.Events (Event (..), EventKind (..), EventSink, SpanId, TraceId, newSpanId)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType, statusCode, urlDecode)
import System.Timeout (timeout)

-- Configuration --------------------------------------------------------------

data OtlpConfig = OtlpConfig
  { -- | The traces URL spans are posted to.
    ocEndpoint :: Text,
    ocHeaders :: [(Text, Text)],
    ocServiceName :: Text,
    -- | Resource attributes besides @service.name@.
    ocResource :: [(Text, Text)]
  }
  deriving (Show, Eq)

-- | The export the option and the environment ask for (spec 12.10),
-- with the warnings they give. @--otlp-endpoint@ is a base endpoint,
-- as @OTEL_EXPORTER_OTLP_ENDPOINT@ is, and wins over the variables that
-- turn the export off; @OTEL_EXPORTER_OTLP_TRACES_ENDPOINT@ is taken as
-- the traces URL itself.
otlpConfigFrom :: Maybe Text -> [(String, String)] -> (Maybe OtlpConfig, [Text])
otlpConfigFrom flag env =
  case endpoint of
    Nothing -> (Nothing, [])
    Just url -> (Just (OtlpConfig url headers service resource), protocolWarning)
  where
    var k = T.pack <$> lookup k env
    nonEmpty k = case T.strip <$> var k of
      Just v | not (T.null v) -> Just v
      _ -> Nothing
    disabled =
      fmap T.toLower (nonEmpty "OTEL_SDK_DISABLED") == Just "true"
        || fmap T.toLower (nonEmpty "OTEL_TRACES_EXPORTER") == Just "none"
    endpoint = case flag of
      Just f -> Just (tracesUrl f)
      Nothing
        | disabled -> Nothing
        | Just t <- nonEmpty "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" -> Just t
        | otherwise -> tracesUrl <$> nonEmpty "OTEL_EXPORTER_OTLP_ENDPOINT"
    tracesUrl base
      | "/v1/traces" `T.isSuffixOf` base = base
      | otherwise = T.dropWhileEnd (== '/') base <> "/v1/traces"
    headers = pairs (firstOf ["OTEL_EXPORTER_OTLP_TRACES_HEADERS", "OTEL_EXPORTER_OTLP_HEADERS"])
    attrs = pairs (nonEmpty "OTEL_RESOURCE_ATTRIBUTES")
    service = fromMaybe "lask" (nonEmpty "OTEL_SERVICE_NAME" `orElse` lookup "service.name" attrs)
    resource = filter ((/= "service.name") . fst) attrs
    firstOf ks = case mapMaybe nonEmpty ks of
      v : _ -> Just v
      [] -> Nothing
    pairs = maybe [] (mapMaybe pair . T.splitOn ",")
    pair item = case T.breakOn "=" item of
      (k, v)
        | not (T.null v), not (T.null (T.strip k)) -> Just (decode (T.strip k), decode (T.strip (T.drop 1 v)))
      _ -> Nothing
    decode = TE.decodeUtf8Lenient . urlDecode False . TE.encodeUtf8
    protocolWarning =
      [ "OTLP protocol '" <> p <> "' is not supported; spans are sent as http/json"
      | Just p <- [firstOf ["OTEL_EXPORTER_OTLP_TRACES_PROTOCOL", "OTEL_EXPORTER_OTLP_PROTOCOL"]],
        T.toLower p /= "http/json"
      ]
    orElse a b = maybe b Just a

-- Trace identity ---------------------------------------------------------------

-- | The OpenTelemetry trace id of a lask trace id (12.5): itself when it
-- is already 32 lowercase hex digits, not all zero; otherwise the first
-- 16 bytes of its SHA-256, so that a given identifier always maps to
-- the same trace.
otelTraceId :: TraceId -> Text
otelTraceId t
  | isHexId 32 t = t
  | otherwise = TE.decodeUtf8 (B16.encode (BS.take 16 (SHA256.hash (TE.encodeUtf8 t))))

isHexId :: Int -> Text -> Bool
isHexId n t =
  T.length t == n
    && T.all (\c -> isDigit c || (isHexDigit c && isLower c)) t
    && T.any (/= '0') t

-- | The trace and parent span of a W3C @traceparent@ header value
-- (version 00).
parseTraceparent :: Text -> Maybe (TraceId, SpanId)
parseTraceparent v = case T.splitOn "-" (T.toLower (T.strip v)) of
  [ver, tid, sid, flags]
    | isHexId 2 ver || ver == "00",
      ver /= "ff",
      isHexId 32 tid,
      isHexId 16 sid,
      T.length flags == 2 && T.all isHexDigit flags ->
        Just (tid, sid)
  _ -> Nothing

-- Spans -----------------------------------------------------------------------

data Attr = AStr Text | AInt Int | ABool Bool
  deriving (Show, Eq)

data SpanStatus = StatusUnset | StatusOk | StatusError Text
  deriving (Show, Eq)

data SpanEvent = SpanEvent UTCTime Text [(Text, Attr)]
  deriving (Show, Eq)

data Span = Span
  { spName :: Text,
    spId :: SpanId,
    spParent :: Maybe SpanId,
    spStart :: UTCTime,
    spEnd :: UTCTime,
    spAttrs :: [(Text, Attr)],
    spStatus :: SpanStatus,
    spEvents :: [SpanEvent]
  }
  deriving (Show, Eq)

-- | An @ExportTraceServiceRequest@ in the OTLP JSON encoding.
encodeExport :: OtlpConfig -> Text -> Text -> [Span] -> A.Value
encodeExport cfg traceId version spans =
  A.object
    [ ( "resourceSpans",
        A.toJSON
          [ A.object
              [ ("resource", A.object [("attributes", attrsJson (("service.name", AStr (ocServiceName cfg)) : [(k, AStr v) | (k, v) <- ocResource cfg]))]),
                ( "scopeSpans",
                  A.toJSON
                    [ A.object
                        [ ("scope", A.object [("name", "lask"), ("version", A.String version)]),
                          ("spans", A.toJSON (map spanJson spans))
                        ]
                    ]
                )
              ]
          ]
      )
    ]
  where
    spanJson sp =
      A.object $
        [ ("traceId", A.String traceId),
          ("spanId", A.String (spId sp)),
          ("name", A.String (spName sp)),
          -- SPAN_KIND_INTERNAL
          ("kind", A.Number 1),
          ("startTimeUnixNano", nanos (spStart sp)),
          ("endTimeUnixNano", nanos (spEnd sp)),
          ("attributes", attrsJson (spAttrs sp)),
          ("events", A.toJSON (map eventJson (spEvents sp))),
          ("status", statusJson (spStatus sp))
        ]
          <> [("parentSpanId", A.String p) | Just p <- [spParent sp]]
    eventJson (SpanEvent t n as) =
      A.object [("timeUnixNano", nanos t), ("name", A.String n), ("attributes", attrsJson as)]
    statusJson s = case s of
      StatusUnset -> A.object []
      StatusOk -> A.object [("code", A.Number 1)]
      StatusError m -> A.object [("code", A.Number 2), ("message", A.String m)]
    attrsJson as = A.toJSON [A.object [("key", A.String k), ("value", attrValue v)] | (k, v) <- as]
    attrValue v = case v of
      AStr t -> A.object [("stringValue", A.String t)]
      -- int64 is a string in the protobuf JSON mapping.
      AInt n -> A.object [("intValue", A.String (T.pack (show n)))]
      ABool b -> A.object [("boolValue", A.Bool b)]
    nanos t = A.String (T.pack (show (floor (utcTimeToPOSIXSeconds t * 1000000000) :: Integer)))

-- Tracing a run ---------------------------------------------------------------

-- | The root span: what the command line ran.
data RootInfo = RootInfo
  { -- | @lask run deploy@, @lask cmd npm@.
    riName :: Text,
    riAttrs :: [(Text, Attr)],
    -- | The span the run continues, from @TRACEPARENT@.
    riParent :: Maybe SpanId
  }

data OpenCall = OpenCall UTCTime Text (Maybe SpanId) [(Text, Attr)]

data OpenCommand = OpenCommand SpanId (Maybe SpanId) UTCTime [(Text, Attr)]

data Tracer = Tracer
  { trConfig :: OtlpConfig,
    trManager :: Manager,
    trTraceId :: Text,
    trVersion :: Text,
    trRoot :: SpanId,
    trRootInfo :: RootInfo,
    trStart :: UTCTime,
    trCalls :: IORef (Map.Map SpanId OpenCall),
    trCommands :: IORef (Map.Map Int OpenCommand),
    -- | Ended spans not yet sent, newest first.
    trDone :: IORef [Span],
    -- | The first failure to send, reported once at the end.
    trFailure :: IORef (Maybe Text),
    trFlusher :: IORef (Maybe (IO ()))
  }

-- | Start tracing a run: the root span starts now, and ended spans are
-- sent every five seconds until 'finishTracer'.
newTracer :: OtlpConfig -> TraceId -> Text -> SpanId -> RootInfo -> IO Tracer
newTracer cfg laskTraceId version root info = do
  mgr <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro 5000000}
  now <- getCurrentTime
  tr <-
    Tracer cfg mgr (otelTraceId laskTraceId) version root info now
      <$> newIORef Map.empty
      <*> newIORef Map.empty
      <*> newIORef []
      <*> newIORef Nothing
      <*> newIORef Nothing
  tid <- forkIO . forever $ do
    threadDelay 5000000
    flush tr
  atomicModifyIORef' (trFlusher tr) (const (Just (killThread tid), ()))
  pure tr

-- | Pass every execution event on, making a span of each call.
tracerEvents :: Tracer -> EventSink -> EventSink
tracerEvents tr sink ev = do
  case evKind ev of
    EvCall ->
      atomicModifyIORef' (trCalls tr) $ \m ->
        (Map.insert (evSpanId ev) (OpenCall (evTimestamp ev) name (evParentSpanId ev) (callAttrs <> payloadAttr "lask.args")) m, ())
    EvReturn -> close StatusUnset (payloadAttr "lask.result") []
    EvFail -> do
      let err = lookup "error" (evPayload ev)
          message = fromMaybe "failed" (err >>= field "message" >>= str)
          code = err >>= field "code"
      close
        (StatusError message)
        ([("lask.error.code", AInt c) | Just c <- [code >>= int]])
        [SpanEvent (evTimestamp ev) "exception" ([("exception.type", AStr "lask.Error"), ("exception.message", AStr message)])]
  sink ev
  where
    fn = evFunction ev
    name = fromMaybe "function" (field "name" fn >>= str)
    callAttrs =
      [("code.function.name", AStr name)]
        <> [("code.file.path", AStr m) | Just m <- [field "module" fn >>= str]]
        <> [("lask.function.type", AStr t) | Just t <- [field "type" fn >>= str]]
    payloadAttr key = [(key, AStr (clip (encodeJson v))) | (_, v) <- take 1 (evPayload ev)]
    close status extra events = do
      open <- atomicModifyIORef' (trCalls tr) $ \m -> (Map.delete (evSpanId ev) m, Map.lookup (evSpanId ev) m)
      case open of
        Nothing -> pure ()
        Just (OpenCall start n parent attrs) ->
          done tr (Span n (evSpanId ev) parent start (evTimestamp ev) (attrs <> extra) status events)

-- | Pass every command log line on, making a span of each execution.
tracerCommandLog :: Tracer -> CommandLogSink -> CommandLogSink
tracerCommandLog tr sink cl = do
  case clKind cl of
    ClStart -> do
      sid <- maybe newSpanId pure (clSpanId cl)
      let attrs =
            [ ("process.command_line", AStr (summarizeCommand (clCommand cl))),
              ("lask.env", AStr (clEnvSummary cl)),
              ("lask.exec", AInt (clExec cl))
            ]
      atomicModifyIORef' (trCommands tr) $ \m ->
        (Map.insert (clExec cl) (OpenCommand sid (Just (fromMaybe (trRoot tr) (clParentSpanId cl))) (clTimestamp cl) attrs) m, ())
    ClExit code ->
      close
        (if code == 0 then StatusUnset else StatusError ("exit " <> T.pack (show code)))
        [("process.exit.code", AInt code)]
    ClKilled -> close (StatusError "killed") [("lask.killed", ABool True)]
    ClLine _ _ -> pure ()
  sink cl
  where
    close status extra = do
      open <- atomicModifyIORef' (trCommands tr) $ \m -> (Map.delete (clExec cl) m, Map.lookup (clExec cl) m)
      case open of
        Nothing -> pure ()
        Just (OpenCommand sid parent start attrs) ->
          done tr (Span (commandName (clCommand cl)) sid parent start (clTimestamp cl) (attrs <> extra) status [])

-- | End the run: the root span ends with the exit code, a span still
-- open ends with it, and everything not yet sent is sent, waiting at
-- most five seconds. The result is the warning to report, if sending
-- failed.
finishTracer :: Tracer -> Int -> Bool -> IO (Maybe Text)
finishTracer tr code interrupted = do
  readIORef (trFlusher tr) >>= sequence_
  now <- getCurrentTime
  calls <- atomicModifyIORef' (trCalls tr) (\m -> (Map.empty, Map.toList m))
  cmds <- atomicModifyIORef' (trCommands tr) (\m -> (Map.empty, Map.elems m))
  let unfinished = [("lask.unfinished", ABool True)]
  mapM_ (\(sid, OpenCall start n parent attrs) -> done tr (Span n sid parent start now (attrs <> unfinished) (StatusError "unfinished") [])) calls
  mapM_ (\(OpenCommand sid parent start attrs) -> done tr (Span "command" sid parent start now (attrs <> unfinished) (StatusError "unfinished") [])) cmds
  let info = trRootInfo tr
      status
        | interrupted = StatusError ("interrupted (exit code " <> T.pack (show code) <> ")")
        | code /= 0 = StatusError ("exit code " <> T.pack (show code))
        | otherwise = StatusUnset
  done tr (Span (riName info) (trRoot tr) (riParent info) (trStart tr) now (riAttrs info <> [("process.exit.code", AInt code)]) status [])
  finished <- timeout 5000000 (flush tr)
  failure <- readIORef (trFailure tr)
  pure $ case (finished, failure) of
    (_, Just f) -> Just f
    (Nothing, _) -> Just ("could not send the trace to " <> ocEndpoint (trConfig tr) <> ": timed out")
    _ -> Nothing

done :: Tracer -> Span -> IO ()
done tr sp = atomicModifyIORef' (trDone tr) (\xs -> (sp : xs, ()))

-- | Send the ended spans. A failure is kept for the end, and the spans
-- are not sent again: a collector that is down is not retried at the
-- cost of the run.
flush :: Tracer -> IO ()
flush tr = do
  spans <- atomicModifyIORef' (trDone tr) (\xs -> ([], reverse xs))
  unless (null spans) $ do
    r <- try (try (post tr spans))
    let failed reason = atomicModifyIORef' (trFailure tr) (\f -> (Just (fromMaybe ("could not send the trace to " <> ocEndpoint (trConfig tr) <> ": " <> reason) f), ()))
    case r of
      Left e -> failed (oneLine (T.pack (show (e :: SomeException))))
      Right (Left e) -> failed (httpReason e)
      Right (Right s)
        | s >= 200 && s < 300 -> pure ()
        | otherwise -> failed ("HTTP " <> T.pack (show s))

post :: Tracer -> [Span] -> IO Int
post tr spans = do
  req0 <- parseRequest (T.unpack (ocEndpoint (trConfig tr)))
  let req =
        req0
          { method = "POST",
            requestHeaders =
              (hContentType, "application/json")
                : [(CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- ocHeaders (trConfig tr)],
            requestBody = RequestBodyLBS (A.encode (encodeExport (trConfig tr) (trTraceId tr) (trVersion tr) spans))
          }
  statusCode . responseStatus <$> httpLbs req (trManager tr)

-- | Why a request failed, in a few words rather than the request.
httpReason :: HttpException -> Text
httpReason e = case e of
  HttpExceptionRequest _ content -> case content of
    ConnectionFailure ioe -> oneLine (T.pack (displayException ioe))
    ResponseTimeout -> "timed out"
    ConnectionTimeout -> "timed out connecting"
    other -> oneLine (T.pack (show other))
  InvalidUrlException url why -> "invalid URL " <> T.pack url <> ": " <> T.pack why

oneLine :: Text -> Text
oneLine = T.unwords . T.words

-- | A span name for a command: its first line, cut short. The command
-- line in full is an attribute.
commandName :: Text -> Text
commandName cmd =
  let first = T.strip (T.takeWhile (/= '\n') cmd)
   in "$ " <> (if T.length first > 60 then T.take 57 first <> "..." else first)

field :: Text -> A.Value -> Maybe A.Value
field k v = case v of
  A.Object o -> KM.lookup (AK.fromText k) o
  _ -> Nothing

str :: A.Value -> Maybe Text
str v = case v of
  A.String t -> Just t
  _ -> Nothing

int :: A.Value -> Maybe Int
int v = case v of
  A.Number n -> toBoundedInteger n
  _ -> Nothing

encodeJson :: A.Value -> Text
encodeJson = TE.decodeUtf8 . BL.toStrict . A.encode

clip :: Text -> Text
clip t
  | T.length t > 1024 = T.take 1021 t <> "..."
  | otherwise = t
