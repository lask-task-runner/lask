{-# LANGUAGE OverloadedStrings #-}

-- | The tables @lask sync@, @lask envs list@ and @lask deps list@
-- print (spec 11.4, 11.5, 11.7): one row per item, columns padded to
-- the widest cell, and a status column coloured when the output is a
-- terminal.
module Command.Lask.Table
  ( Tone (..),
    Cell,
    plain,
    toned,
    renderTable,
    colorFor,
    shortDigest,
    shortPath,
    formatSeconds,
  )
where

import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)
import Text.Printf (printf)

-- | How a status reads: settled, worth a look, or failed.
data Tone = ToneNone | ToneOk | ToneWarn | ToneBad
  deriving (Show, Eq)

type Cell = (Text, Tone)

plain :: Text -> Cell
plain t = (t, ToneNone)

toned :: Tone -> Text -> Cell
toned tone t = (t, tone)

-- | Rows under a header, each column as wide as its widest cell. The
-- last column is not padded, so no line ends in spaces.
renderTable :: Bool -> [Text] -> [[Cell]] -> [Text]
renderTable color header rows = map line allRows
  where
    allRows = map plain header : rows
    lastColumn = length header - 1
    widths = [maximum [T.length (fst c) | r <- allRows, c <- take 1 (drop i r)] | i <- [0 .. lastColumn]]
    line cells = T.stripEnd (T.intercalate "  " (zipWith3 cell [0 ..] widths cells))
    cell i w (t, tone)
      | i == lastColumn = paint tone t
      | otherwise = paint tone t <> T.replicate (w - T.length t) " "
    paint tone t
      | not color = t
      | otherwise = case tone of
          ToneNone -> t
          ToneOk -> "\ESC[32m" <> t <> "\ESC[0m"
          ToneWarn -> "\ESC[33m" <> t <> "\ESC[0m"
          ToneBad -> "\ESC[31m" <> t <> "\ESC[0m"

-- | Whether to colour what goes to a handle: a terminal, @--no-color@
-- not given, and @NO_COLOR@ unset.
colorFor :: Bool -> Handle -> IO Bool
colorFor noColor h = do
  tty <- hIsTerminalDevice h
  noColorEnv <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  pure (tty && not noColor && not noColorEnv)

-- | A digest, content hash or content-addressed tag cut to its first
-- twelve hexadecimal digits: @sha256:be7c8de0160a@,
-- @sha256-df8d3acd40bd@, @lask/b61fd44bd7eb@. A pinned reference
-- (@repo\@sha256:...@) is shown by its digest. Anything else is left
-- as it is.
shortDigest :: Text -> Text
shortDigest t
  | (_, rest) <- T.breakOn "@" t, not (T.null rest) = shortDigest (T.drop 1 rest)
  | otherwise = case [(p, h) | p <- ["sha256:", "sha256-", "lask/"], Just h <- [T.stripPrefix p t]] of
      (p, h) : _ -> p <> T.take 12 h
      [] -> t

-- | A path with the content-hash directory of a cached dependency
-- shown as @…@: @.lask/deps/…/lib/images/unix/Dockerfile@.
shortPath :: Text -> Text
shortPath = T.intercalate "/" . map seg . T.splitOn "/"
  where
    seg s
      | "sha256-" `T.isPrefixOf` s = "…"
      | otherwise = s

-- | An elapsed time: @0.4s@, @14.2s@, @1m12s@.
formatSeconds :: Double -> Text
formatSeconds s
  | s < 60 = T.pack (printf "%.1fs" s)
  | otherwise =
      let total = round s :: Int
       in T.pack (show (total `div` 60) <> "m" <> printf "%02d" (total `mod` 60) <> "s")
