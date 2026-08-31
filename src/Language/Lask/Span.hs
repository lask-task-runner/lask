{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Span
  ( Position (..),
    Span (..),
    mkSpan,
    spanText,
    toSourcePos,
    fromSourcePos,
  )
where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
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

-- | The source text a span covers. Positions are 1-based and the end
-- is exclusive, matching the spans the lexer produces.
--
-- Example:
--
-- >>> spanText (T.pack "let x = 1\nlet y = 2") (mkSpan "m.lask" 2 9 2 10)
-- "2"
spanText :: Text -> Span -> Text
spanText src (Span (Position _ l1 c1) (Position _ l2 c2))
  | l1 == l2 = T.take (c2 - c1) (T.drop (c1 - 1) (lineAt l1))
  | otherwise =
      T.intercalate "\n" $
        [T.drop (c1 - 1) (lineAt l1)]
          <> [lineAt l | l <- [l1 + 1 .. l2 - 1]]
          <> [T.take (c2 - 1) (lineAt l2)]
  where
    srcLines = T.splitOn "\n" src
    lineAt l = case drop (l - 1) srcLines of
      (x : _) -> x
      [] -> ""
spanText _ NoSpan = ""

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
