{-# LANGUAGE OverloadedStrings #-}

-- | Progress of @lask sync@ on stderr (spec 11.7): what it is fetching,
-- pulling and building while it does, so a long pull does not look
-- like a hang.
--
-- On a terminal the item in progress is one line, redrawn in place
-- with a spinner, its latest detail and the time so far, and replaced
-- by its outcome when it ends. Elsewhere — CI, a pipe — each item is a
-- timestamped line when it starts and when it ends, with a line every
-- 30 seconds in between. With @--format json@ every one of these is a
-- JSON Lines event instead.
module Command.Lask.Progress
  ( Progress,
    Item,
    withProgress,
    section,
    note,
    start,
    update,
    finish,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Command.Lask.Table (formatSeconds)
import Language.Lask.Obs.Events (formatTimestamp)
import System.IO (hFlush, hIsTerminalDevice, stderr)
import System.Process (readCreateProcessWithExitCode, shell)
import Text.Read (readMaybe)

data Mode = Tty Int | Lines | Json

data Progress = Progress
  { pgMode :: Mode,
    pgLock :: MVar (),
    pgColor :: Bool
  }

data Item = Item
  { itProgress :: Progress,
    itKind :: Text,
    itCounter :: Maybe (Int, Int),
    itLabel :: Text,
    itVerb :: Text,
    itStarted :: UTCTime,
    itDetail :: IORef Text,
    itTicker :: IORef (Maybe (IO ()))
  }

-- | Run with a progress reporter on stderr. @json@ is @--format json@;
-- @color@ whether the outcome marks may be coloured.
withProgress :: Bool -> Bool -> (Progress -> IO a) -> IO a
withProgress json color act = do
  tty <- hIsTerminalDevice stderr
  mode <-
    if json
      then pure Json
      else if tty then Tty <$> terminalWidth else pure Lines
  lock <- newMVar ()
  act (Progress mode lock color)

-- | The width of the terminal stderr is on, or 80 when it cannot be
-- read.
terminalWidth :: IO Int
terminalWidth = do
  r <- try (readCreateProcessWithExitCode (shell "stty size < /dev/tty") "")
  pure $ case r of
    Right (_, out, _) | [_, cols] <- words out, Just w <- readMaybe cols, w > 20 -> w
    Left e -> const 80 (e :: SomeException)
    _ -> 80

-- | A heading: @Modules@, @Images (10)@.
section :: Progress -> Text -> IO ()
section pg title = case pgMode pg of
  Json -> event pg [("event", "section"), ("title", A.String title)]
  _ -> say pg title

-- | A line of its own, between items: a warning, say.
note :: Progress -> Text -> IO ()
note pg msg = case pgMode pg of
  Json -> event pg [("event", "note"), ("message", A.String msg)]
  Lines -> stamped pg msg
  Tty _ -> say pg msg

-- | Start an item: its kind (@deps@, @image@), its place in its
-- section, what it is, and what is being done to it (@fetching@,
-- @pulling@, @building@).
start :: Progress -> Text -> Maybe (Int, Int) -> Text -> Text -> IO Item
start pg kind counter label verb = do
  now <- getCurrentTime
  detail <- newIORef ""
  ticker <- newIORef Nothing
  let it = Item pg kind counter label verb now detail ticker
  case pgMode pg of
    Json -> event pg (itemFields it <> [("event", "start"), ("verb", A.String verb)])
    Lines -> do
      stamped pg (prefix it <> verb)
      -- A line every 30 seconds, so a long pull is seen to be alive.
      tid <- forkIO . forever $ do
        threadDelay 30000000
        t <- getCurrentTime
        d <- readIORef detail
        stamped pg (prefix it <> "still " <> verb <> " (" <> formatSeconds (elapsed now t) <> (if T.null d then "" else ", " <> d) <> ")")
      writeIORef ticker (Just (killThread tid))
    Tty _ -> do
      frame <- newIORef (0 :: Int)
      tid <- forkIO . forever $ do
        i <- atomicModifyIORef' frame (\n -> (n + 1, n))
        redraw it i
        threadDelay 100000
      writeIORef ticker (Just (killThread tid))
  pure it

-- | The latest detail of an item in progress: @layers 4/9@, a build
-- step.
update :: Item -> Text -> IO ()
update it d = do
  old <- atomicModifyIORef' (itDetail it) (\o -> (d, o))
  case pgMode (itProgress it) of
    Json | d /= old -> event (itProgress it) (itemFields it <> [("event", "progress"), ("detail", A.String d)])
    _ -> pure ()

-- | End an item with its outcome (@pulled@, @present@, a failure), and
-- return how long it took, in seconds.
finish :: Item -> Bool -> Text -> IO Double
finish it ok outcome = do
  readIORef (itTicker it) >>= maybe (pure ()) id
  t <- getCurrentTime
  let secs = elapsed (itStarted it) t
      pg = itProgress it
  case pgMode pg of
    Json ->
      event pg (itemFields it <> [("event", "done"), ("ok", A.Bool ok), ("outcome", A.String outcome), ("seconds", A.toJSON secs)])
    Lines -> stamped pg (prefix it <> outcome <> " (" <> formatSeconds secs <> ")")
    Tty w -> withMVar (pgLock pg) $ \_ -> do
      let mark = if ok then paint pg "32" "✓" else paint pg "31" "✗"
          body = fit w (prefix it <> outcome <> "  " <> formatSeconds secs)
      TIO.hPutStr stderr ("\r\ESC[2K  " <> mark <> " " <> body <> "\n")
      hFlush stderr
  pure secs

-- Rendering -------------------------------------------------------------------

redraw :: Item -> Int -> IO ()
redraw it i = case pgMode (itProgress it) of
  Tty w -> do
    t <- getCurrentTime
    d <- readIORef (itDetail it)
    let spinner = T.singleton ("⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" !! (i `mod` 10))
        body = prefix it <> itVerb it <> (if T.null d then "" else "  " <> d) <> "  " <> formatSeconds (elapsed (itStarted it) t)
    withMVar (pgLock (itProgress it)) $ \_ -> do
      TIO.hPutStr stderr ("\r\ESC[2K  " <> spinner <> " " <> fit w body)
      hFlush stderr
  _ -> pure ()

-- | What every line of an item begins with: @[4/10] node:20  @.
prefix :: Item -> Text
prefix it = maybe "" (\(i, n) -> "[" <> tshow i <> "/" <> tshow n <> "] ") (itCounter it) <> itLabel it <> "  "

-- | Cut a line to the terminal, so redrawing it in place never wraps.
fit :: Int -> Text -> Text
fit w t
  | T.length t <= w - 5 = t
  | otherwise = T.take (w - 6) t <> "…"

say :: Progress -> Text -> IO ()
say pg t = withMVar (pgLock pg) $ \_ -> TIO.hPutStrLn stderr t

stamped :: Progress -> Text -> IO ()
stamped pg t = do
  now <- getCurrentTime
  say pg (formatTimestamp now <> " " <> t)

paint :: Progress -> Text -> Text -> Text
paint pg code t
  | pgColor pg = "\ESC[" <> code <> "m" <> t <> "\ESC[0m"
  | otherwise = t

itemFields :: Item -> [(A.Key, A.Value)]
itemFields it =
  [("kind", A.String (itKind it)), ("item", A.String (itLabel it))]
    <> maybe [] (\(i, n) -> [("index", A.toJSON i), ("total", A.toJSON n)]) (itCounter it)

event :: Progress -> [(A.Key, A.Value)] -> IO ()
event pg fields = do
  now <- getCurrentTime
  say pg (TE.decodeUtf8 (BL.toStrict (A.encode (A.object (("timestamp", A.String (formatTimestamp now)) : fields)))))

elapsed :: UTCTime -> UTCTime -> Double
elapsed a b = realToFrac (diffUTCTime b a)

tshow :: Int -> Text
tshow = T.pack . show
