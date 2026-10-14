{-# LANGUAGE OverloadedStrings #-}

-- | Correction candidates for a misspelt name (spec 14.3): the names of
-- the same kind in scope that lie close to what was written.
module Language.Lask.Suggest
  ( suggest,
    suggestAny,
    editDistance,
  )
where

import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

-- | The candidates for a misspelt name, closest first and at most
-- three. Names are compared case-insensitively, with @-@ and @_@ taken
-- as one character, so that the CLI form @cowsay-hello@ finds
-- @cowsay_hello@. A candidate is kept within distance
-- @max 1 (length \/ 3)@ of the name written.
--
-- >>> suggest (T.pack "helo") (map T.pack ["hello", "help", "world"])
-- ["hello","help"]
-- >>> suggest (T.pack "cowsay-helo") (map T.pack ["cowsay_hello"])
-- ["cowsay_hello"]
suggest :: Text -> [Text] -> [Text]
suggest n = suggestAny [n]

-- | 'suggest' for several names written at once, each candidate ranked
-- by its distance to the closest of them.
suggestAny :: [Text] -> [Text] -> [Text]
suggestAny written pool =
  map snd . take 3 . sortOn id $
    [(d, c) | c <- nub pool, c `notElem` written, Just d <- [closest c]]
  where
    closest c = case [d | w <- written, let d = editDistance (normalise w) (normalise c), d <= threshold w] of
      [] -> Nothing
      ds -> Just (minimum ds)
    threshold w = max 1 (T.length w `div` 3)

normalise :: Text -> Text
normalise = T.map (\c -> if c == '-' then '_' else c) . T.toLower

-- | The optimal string alignment distance: insertions, deletions,
-- substitutions, and transpositions of two adjacent characters, so
-- that @hlelo@ is one edit from @hello@.
--
-- >>> editDistance (T.pack "hlelo") (T.pack "hello")
-- 1
-- >>> editDistance (T.pack "pyhton") (T.pack "python")
-- 1
-- >>> editDistance (T.pack "kitten") (T.pack "sitting")
-- 3
editDistance :: Text -> Text -> Int
editDistance a b = go 1 (V.enumFromN 0 (lb + 1)) (V.enumFromN 0 (lb + 1))
  where
    va = V.fromList (T.unpack a)
    vb = V.fromList (T.unpack b)
    la = V.length va
    lb = V.length vb
    -- Rows i-2 and i-1 of the distance table, built row by row.
    go i prev2 prev
      | i > la = prev V.! lb
      | otherwise = go (i + 1) prev (row i prev2 prev)
    row i prev2 prev = V.constructN (lb + 1) cell
      where
        ca = va V.! (i - 1)
        cell cur
          | j == 0 = i
          | otherwise =
              let cb = vb V.! (j - 1)
                  cost = if ca == cb then 0 else 1
                  best =
                    minimum
                      [ prev V.! j + 1,
                        cur V.! (j - 1) + 1,
                        prev V.! (j - 1) + cost
                      ]
               in if i > 1 && j > 1 && ca == vb V.! (j - 2) && va V.! (i - 2) == cb
                    then min best (prev2 V.! (j - 2) + 1)
                    else best
          where
            j = V.length cur
