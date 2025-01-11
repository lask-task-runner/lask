{-# LANGUAGE InstanceSigs #-}

module Language.Lask.Span
  ( Position (..),
    Span (..),
    mkSpan,
    toSourcePos,
    fromSourcePos,
  )
where

import Data.List (sort)
import Language.Lask.Utils (Pretty (pretty))
import Text.Megaparsec (SourcePos (..), mkPos, unPos)

data Position = Position
  { fileName :: FilePath,
    line :: Int,
    column :: Int
  }
  deriving (Show, Ord, Eq)

data Span
  = Span Position Position
  | NoSpan
  deriving (Show, Ord, Eq)

instance Semigroup Span where
  Span s1 e1 <> Span s2 e2 =
    let ps = sort [s1, e1, s2, e2]
     in Span (head ps) (last ps)
  s@Span {} <> NoSpan = s
  NoSpan <> s@Span {} = s
  NoSpan <> NoSpan = NoSpan

-- | Create a span from two pairs of line and column numbers.
--
-- Example:
--
-- >>> mkSpan "example.lask" 1 2 3 4
-- Span (Position {fileName = "example.lask", line = 1, column = 2}) (Position {fileName = "example.lask", line = 3, column = 4})
mkSpan :: FilePath -> Int -> Int -> Int -> Int -> Span
mkSpan f l1 c1 l2 c2 = Span (Position f l1 c1) (Position f l2 c2)

toSourcePos :: Position -> SourcePos
toSourcePos (Position file l c) = SourcePos file (mkPos l) (mkPos c)

fromSourcePos :: SourcePos -> Position
fromSourcePos (SourcePos file l c) = Position file (unPos l) (unPos c)

instance Pretty Span where
  pretty :: Span -> String
  pretty (Span p1@(Position file l1 c1) p2@(Position _ l2 c2)) =
    if p1 == p2
      then file <> ":" <> show l1 <> ":" <> show c1
      else file <> ":" <> show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2
  pretty NoSpan = "(no location)"
