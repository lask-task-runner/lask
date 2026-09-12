{-# LANGUAGE OverloadedStrings #-}

-- | The command-word scan of spec 10.9.
--
-- Given the parts of a command string, this finds the words that stand
-- in command position, so that dispatch can look them up in the
-- module's command declarations. It is a lexical procedure, not an
-- interpretation of shell syntax: beyond the regions and separators
-- named in 10.9 nothing is given meaning.
--
-- The determinacy principle of 10.9 governs every rule here. The scan
-- decides only where the text alone determines the command words, and
-- reports nothing elsewhere; a caller that receives no candidate emits
-- @E-TYPE-COMMAND-NOENV@ rather than guessing.
module Language.Lask.Syntax.CommandWords
  ( CommandWord (..),
    Analysis (..),
    commandWords,
    validCommandName,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST (TextPart (..))

-- | A word found in command position.
data CommandWord = CommandWord
  { cwSpan :: Span,
    cwText :: Text,
    -- | Whether the word satisfies the candidacy conditions of 10.9:
    -- no quotation character, backslash, interpolation hole or nested
    -- region, and no @/@. A non-candidate never matches a declaration.
    cwCandidate :: Bool
  }
  deriving (Show, Eq)

-- | The outcome of scanning one command string.
data Analysis
  = Analysed [CommandWord]
  | -- | A quoted or nested region was left open, so the string cannot
    -- be segmented at all (10.9). Carries the span of the opener.
    NotAnalysable Span Text
  deriving (Show, Eq)

-- | One source character, or one opaque interpolation hole (10.9: a
-- hole may expand to any text, so no rule may look through one). The
-- 'Int' is the atom's position in the flattened command string. It
-- orders the words a nested region contributes against the words
-- around it without depending on spans being present.
data Atom
  = AChar Int Span Char
  | AHole Int Span

atomSpan :: Atom -> Span
atomSpan (AChar _ sp _) = sp
atomSpan (AHole _ sp) = sp

atomIndex :: Atom -> Int
atomIndex (AChar i _ _) = i
atomIndex (AHole i _) = i

-- | Words accumulate their atoms and whether they remain plain, i.e.
-- still eligible to be a candidate.
data Word' = Word' {wAtoms :: [Atom], wPlain :: Bool}

emptyWord :: Word'
emptyWord = Word' [] True

-- | Scan a command string for its command words, in source order.
commandWords :: [TextPart] -> Analysis
commandWords parts = case scan (atoms parts) of
  Left (sp, why) -> NotAnalysable sp why
  Right ws -> Analysed (map snd (sortOn fst ws))

-- | Whether a string literal is usable as a command name (spec ch. 5):
-- submitting it alone as a whole command string must yield exactly one
-- candidate whose text is the name itself.
validCommandName :: Text -> Bool
validCommandName t = case commandWords [TPChunk NoSpan t] of
  Analysed [w] -> cwCandidate w && cwText w == t
  _ -> False

-- Atoms ---------------------------------------------------------------------

-- | Flatten the parts into positioned characters. Positions inside a
-- chunk are recovered by walking its text from the chunk span's start;
-- see the note on 'Language.Lask.Lexer.Token.Chunk' for the one case
-- where the walk is approximate.
atoms :: [TextPart] -> [Atom]
atoms = go 0
  where
    go _ [] = []
    go i (TPInterp _ : ps) = AHole i NoSpan : go (i + 1) ps
    go i (TPChunk sp t : ps) =
      let cs = T.unpack t
       in walk i (startOf sp) cs <> go (i + length cs) ps

    startOf (Span s _) = Just s
    startOf NoSpan = Nothing

    walk _ _ [] = []
    walk i Nothing (c : cs) = AChar i NoSpan c : walk (i + 1) Nothing cs
    walk i (Just p) (c : cs) =
      let p' = advance p c
       in AChar i (Span p p') c : walk (i + 1) (Just p') cs

    advance p '\n' = p {line = line p + 1, column = 1}
    advance p _ = p {column = column p + 1}

-- Scanning -------------------------------------------------------------------

-- | Walk the atoms, accumulating words and segments and recursing into
-- nested regions. Each result carries its first atom's index, so the
-- caller can restore source order. 'Left' reports an unclosed region.
scan :: [Atom] -> Either (Span, Text) [(Int, CommandWord)]
scan = go emptyWord [] []
  where
    -- cur: the word being accumulated
    -- seg: words of the current segment, in reverse
    -- acc: command words found so far
    go cur seg acc [] = Right (acc <> segment (finish cur seg))
    go cur seg acc (a : rest) = case a of
      AHole _ _ -> go (push a cur {wPlain = False}) seg acc rest
      AChar _ sp c
        | c == '\'' -> quoted sp '\'' False cur seg acc rest
        | c == '"' -> quoted sp '"' True cur seg acc rest
        | c == '\\' -> case rest of
            (nxt : rest') -> go (push nxt (push a cur {wPlain = False})) seg acc rest'
            [] -> go (push a cur {wPlain = False}) seg acc []
        | c == '$', (open@(AChar _ _ '(') : rest') <- rest -> nested sp cur seg acc rest' [a, open]
        | c == '(' -> nested sp cur seg acc rest [a]
        | c == '`' -> backquoted sp cur seg acc rest
        | c `elem` ("&|;" :: String) ->
            go emptyWord [] (acc <> segment (finish cur seg)) (dropDoubled c rest)
        | c == '\n' || c == '\r' -> go emptyWord [] (acc <> segment (finish cur seg)) rest
        | c == ' ' || c == '\t' -> go emptyWord (finish cur seg) acc rest
        | otherwise -> go (push a cur) seg acc rest

    dropDoubled c (AChar _ _ c' : rest) | c == c', c /= ';' = rest
    dropDoubled _ rest = rest

    push a w = w {wAtoms = a : wAtoms w}

    finish w seg
      | null (wAtoms w) = seg
      | otherwise = w : seg

    -- A quoted region: everything up to the closing quote joins the
    -- current word, which stops being plain.
    quoted sp q escapes cur0 seg acc = walkQ (pushOpen cur0)
      where
        pushOpen w = w {wPlain = False, wAtoms = AChar (-1) sp q : wAtoms w}
        walkQ _ [] = Left (sp, "unterminated quoted region")
        walkQ cur (a : rest) = case a of
          AChar _ _ '\\' | escapes -> case rest of
            (nxt : rest') -> walkQ (push nxt (push a cur)) rest'
            [] -> Left (sp, "unterminated quoted region")
          AChar _ _ c | c == q -> go (push a cur) seg acc rest
          _ -> walkQ (push a cur) rest

    -- A nested region: its content is scanned by the same rules and
    -- contributes its own command words, while in the enclosing text
    -- it occupies part of one word.
    nested sp cur seg acc rest opened =
      walkN (1 :: Int) (foldl (flip push) cur {wPlain = False} opened) [] rest
      where
        walkN _ _ _ [] = Left (sp, "unterminated nested region")
        walkN d cur' inner (a : more) = case a of
          AChar _ _ '(' -> walkN (d + 1) (push a cur') (a : inner) more
          AChar _ _ ')'
            | d == 1 -> do
                ws <- scan (reverse inner)
                go (push a cur') seg (acc <> ws) more
            | otherwise -> walkN (d - 1) (push a cur') (a : inner) more
          _ -> walkN d (push a cur') (a : inner) more

    backquoted sp cur seg acc rest =
      walkB (push (AChar (-1) sp '`') cur {wPlain = False}) [] rest
      where
        walkB _ _ [] = Left (sp, "unterminated nested region")
        walkB cur' inner (a : more) = case a of
          AChar _ _ '`' -> do
            ws <- scan (reverse inner)
            go (push a cur') seg (acc <> ws) more
          _ -> walkB (push a cur') (a : inner) more

-- | The command word of one segment: skip leading assignment words,
-- then take the next word (10.9).
segment :: [Word'] -> [(Int, CommandWord)]
segment revWords = case dropWhile isAssignment (reverse revWords) of
  (w : _) -> [(wordIndex w, toCommandWord w)]
  [] -> []
  where
    isAssignment w =
      let t = wordText w
          (name, rest) = T.breakOn "=" t
       in not (T.null rest)
            && not (T.null name)
            && T.all identChar name
            && identStart (T.head name)
    identStart c = c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c == '_'
    identChar c = identStart c || c `elem` ['0' .. '9']

toCommandWord :: Word' -> CommandWord
toCommandWord w =
  CommandWord
    { cwSpan = wordSpan w,
      cwText = t,
      cwCandidate = wPlain w && not (T.any (== '/') t)
    }
  where
    t = wordText w

-- | The word's position in the command string, for source ordering.
-- Atoms synthesised by the scanner carry @-1@ and never stand alone.
wordIndex :: Word' -> Int
wordIndex w = case [i | i <- map atomIndex (wAtoms w), i >= 0] of
  [] -> 0
  is -> minimum is

wordText :: Word' -> Text
wordText w = T.pack [c | AChar _ _ c <- reverse (wAtoms w)]

wordSpan :: Word' -> Span
wordSpan w = foldr ((<>) . atomSpan) NoSpan (wAtoms w)
