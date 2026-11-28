{-# LANGUAGE OverloadedStrings #-}

-- | The run summary of @--summary@ (spec 12.9): what was run, how it
-- ended and how long it took, and one row per command execution,
-- written last on stderr.
--
-- The rows are collected from the command execution log (12.3), whose
-- start and end lines every execution has, so the summary sees exactly
-- what the log saw, masked as it was (12.8). Renderers are pure, as in
-- "Language.Lask.Obs.CommandLog".
module Language.Lask.Obs.Summary
  ( Subject (..),
    Outcome (..),
    CommandEnd (..),
    CommandRow (..),
    Summary (..),
    SummaryCollector,
    newSummaryCollector,
    recordCommands,
    markInterrupted,
    wasInterrupted,
    collectSummary,
    outcomeOfExit,
    renderSummaryText,
    renderSummaryJson,
    formatDuration,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Language.Lask.Obs.CommandLog (CommandLog (..), CommandLogKind (..), CommandLogSink)
import Language.Lask.Obs.Events (TraceId, formatTimestamp)
import System.Exit (ExitCode (..))
import Text.Printf (printf)

-- | What the command line ran.
data Subject
  = -- | @run@ \/ @eval@: the function called.
    SubjectFunction Text
  | -- | @cmd@: the command word (11.8).
    SubjectCommand Text
  deriving (Show, Eq)

data Outcome
  = Succeeded
  | -- | The exit code (11.3).
    Failed Int
  | -- | Stopped by a signal (11.12), with the exit code it gave.
    Interrupted Int
  deriving (Show, Eq)

data CommandEnd = EndExit Int | EndKilled
  deriving (Show, Eq)

data CommandRow = CommandRow
  { crExec :: Int,
    crEnvSummary :: Text,
    crEnvJson :: A.Value,
    crCommand :: Text,
    crStart :: UTCTime,
    -- | Nothing when the log has no end line for it: a run cut short
    -- before the command was stopped.
    crEnd :: Maybe (UTCTime, CommandEnd)
  }
  deriving (Show, Eq)

data Summary = Summary
  { smSubject :: Subject,
    smModule :: FilePath,
    smOutcome :: Outcome,
    smDuration :: NominalDiffTime,
    -- | In execution-number order.
    smCommands :: [CommandRow]
  }
  deriving (Show, Eq)

data SummaryCollector = SummaryCollector
  { scStart :: UTCTime,
    scRows :: IORef (Map.Map Int CommandRow),
    scInterrupted :: IORef Bool
  }

newSummaryCollector :: IO SummaryCollector
newSummaryCollector =
  SummaryCollector <$> getCurrentTime <*> newIORef Map.empty <*> newIORef False

-- | Pass every line on to the sink, recording the start and end lines
-- on the way. Thread-safe: concurrent commands share one sink (12.3).
recordCommands :: SummaryCollector -> CommandLogSink -> CommandLogSink
recordCommands sc sink cl = do
  case clKind cl of
    ClStart ->
      atomicModifyIORef' (scRows sc) $ \m ->
        (Map.insert (clExec cl) (CommandRow (clExec cl) (clEnvSummary cl) (clEnvJson cl) (clCommand cl) (clTimestamp cl) Nothing) m, ())
    ClExit code -> ended (EndExit code)
    ClKilled -> ended EndKilled
    ClLine _ _ -> pure ()
  sink cl
  where
    ended e =
      atomicModifyIORef' (scRows sc) $ \m ->
        (Map.adjust (\r -> r {crEnd = Just (clTimestamp cl, e)}) (clExec cl) m, ())

-- | The run was stopped by a signal, whatever exit code it ends with.
markInterrupted :: SummaryCollector -> IO ()
markInterrupted sc = writeIORef (scInterrupted sc) True

wasInterrupted :: SummaryCollector -> IO Bool
wasInterrupted = readIORef . scInterrupted

outcomeOfExit :: Bool -> ExitCode -> Outcome
outcomeOfExit interrupted code = case code of
  ExitSuccess -> Succeeded
  ExitFailure n
    | interrupted -> Interrupted n
    | otherwise -> Failed n

collectSummary :: SummaryCollector -> Subject -> FilePath -> ExitCode -> IO Summary
collectSummary sc subject modulePath code = do
  now <- getCurrentTime
  rows <- readIORef (scRows sc)
  interrupted <- readIORef (scInterrupted sc)
  pure
    Summary
      { smSubject = subject,
        smModule = modulePath,
        smOutcome = outcomeOfExit interrupted code,
        smDuration = diffUTCTime now (scStart sc),
        smCommands = sortOn crExec (Map.elems rows)
      }

-- | @0.4s@, @12.4s@, @3m05s@, @1h02m@.
formatDuration :: NominalDiffTime -> Text
formatDuration d
  | s < 60 = T.pack (printf "%.1fs" s)
  | total < 3600 = T.pack (printf "%dm%02ds" (total `div` 60) (total `mod` 60))
  | otherwise = T.pack (printf "%dh%02dm" (total `div` 3600) ((total `mod` 3600) `div` 60))
  where
    s = realToFrac d :: Double
    total = floor s :: Int

subjectText :: Subject -> Text
subjectText s = case s of
  SubjectFunction f -> f
  SubjectCommand w -> "cmd " <> w

outcomeText :: Outcome -> Text
outcomeText o = case o of
  Succeeded -> "succeeded"
  Failed n -> "failed with exit code " <> tshow n
  Interrupted n -> "was interrupted (exit code " <> tshow n <> ")"

-- | One line for the run, a row per command, and a count of how they
-- ended:
--
-- > summary: deploy (main.lask) failed with exit code 1 after 12.4s
-- >   #1  #local          make build  3.1s  exit 0
-- >   #2  #node:20        npm test    8.9s  exit 1
-- >   2 commands: 1 succeeded, 1 failed
renderSummaryText :: Summary -> [Text]
renderSummaryText sm =
  header : if null rows then [] else map ("  " <>) (table rows) <> ["  " <> counts]
  where
    header =
      "summary: "
        <> subjectText (smSubject sm)
        <> " ("
        <> T.pack (smModule sm)
        <> ") "
        <> outcomeText (smOutcome sm)
        <> " after "
        <> formatDuration (smDuration sm)
    rows = smCommands sm
    table rs =
      let cells = map rowCells rs
          widths = foldr (zipWith max . map T.length) (repeat 0) cells
       in map (T.stripEnd . T.intercalate "  " . zipWith pad widths) cells
    pad w t = t <> T.replicate (w - T.length t) " "
    rowCells r =
      [ "#" <> tshow (crExec r),
        crEnvSummary r,
        shortCommand (crCommand r),
        maybe "-" (\(t, _) -> formatDuration (diffUTCTime t (crStart r))) (crEnd r),
        endText (snd <$> crEnd r)
      ]
    endText e = case e of
      Just (EndExit n) -> "exit " <> tshow n
      Just EndKilled -> "killed"
      Nothing -> "unfinished"
    counts =
      let n = length rows
          tally p = length (filter p (map (fmap snd . crEnd) rows))
          parts =
            [ tshow k <> " " <> label
            | (k, label) <-
                [ (tally (== Just (EndExit 0)), "succeeded"),
                  (tally (\e -> case e of Just (EndExit c) -> c /= 0; _ -> False), "failed"),
                  (tally (== Just EndKilled), "killed"),
                  (tally (== Nothing), "unfinished")
                ],
              k > 0
            ]
       in tshow n <> (if n == 1 then " command: " else " commands: ") <> T.intercalate ", " parts

-- | The first line of a command, cut to a width a table row can hold;
-- the start line of the log has it in full (12.3).
shortCommand :: Text -> Text
shortCommand cmd =
  let (first, rest) = T.breakOn "\n" (T.strip cmd)
      cut = if T.length first > 60 then T.take 57 first <> "..." else first
   in if T.null rest || T.isSuffixOf "..." cut then cut else cut <> " ..."

-- | One JSON Lines object (12.2), told apart from the other lines by
-- its @summary@ field.
renderSummaryJson :: TraceId -> UTCTime -> Summary -> Text
renderSummaryJson traceId now sm =
  TE.decodeUtf8 . BL.toStrict . A.encode . A.object $
    [ ("timestamp", A.String (formatTimestamp now)),
      ("level", A.String (if smOutcome sm == Succeeded then "info" else "warn")),
      ("traceId", A.String traceId),
      ( "summary",
        A.object $
          [("module", A.String (T.pack (smModule sm)))]
            <> subjectField
            <> outcomeFields
            <> [ ("durationMs", ms (smDuration sm)),
                 ("commands", A.toJSON (map rowJson (smCommands sm)))
               ]
      )
    ]
  where
    subjectField = case smSubject sm of
      SubjectFunction f -> [("function", A.String f)]
      SubjectCommand w -> [("command", A.String w)]
    outcomeFields = case smOutcome sm of
      Succeeded -> [("outcome", "succeeded"), ("exitCode", A.Number 0)]
      Failed n -> [("outcome", "failed"), ("exitCode", A.toJSON n)]
      Interrupted n -> [("outcome", "interrupted"), ("exitCode", A.toJSON n)]
    rowJson r =
      A.object $
        [ ("exec", A.toJSON (crExec r)),
          ("env", crEnvJson r),
          ("command", A.String (crCommand r))
        ]
          <> case crEnd r of
            Just (t, e) ->
              ("durationMs", ms (diffUTCTime t (crStart r))) : case e of
                EndExit n -> [("end", "exit"), ("code", A.toJSON n)]
                EndKilled -> [("end", "killed")]
            Nothing -> [("end", A.Null)]
    ms d = A.toJSON (round (realToFrac d * 1000 :: Double) :: Int)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
