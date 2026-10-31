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
    programWords,
    validCommandName,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST (Expr, TextPart (..))

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
  | -- | A hole, with the text it is shown as.
    AHole Int Span Text

atomSpan :: Atom -> Span
atomSpan (AChar _ sp _) = sp
atomSpan (AHole _ sp _) = sp

atomIndex :: Atom -> Int
atomIndex (AChar i _ _) = i
atomIndex (AHole i _ _) = i

-- | Words accumulate their atoms and whether they remain plain, i.e.
-- still eligible to be a candidate.
data Word' = Word' {wAtoms :: [Atom], wPlain :: Bool}

emptyWord :: Word'
emptyWord = Word' [] True

-- | Scan a command string for its command words, in source order.
commandWords :: [TextPart] -> Analysis
commandWords = analyse segment (\_ _ -> "")

-- | The programs a command string runs, for @lask envs commands@
-- (spec 11.4): the command words of 'commandWords', except that a
-- shell reserved word or a redirection in command position is looked
-- past, and that each hole is part of a word's text, as the given
-- function shows it ('True' for a raw hole). Dispatch gives neither a
-- meaning (10.9), so this is for listing only.
programWords :: (Bool -> Expr -> Text) -> [TextPart] -> Analysis
programWords = analyse programSegment

analyse :: ([Word'] -> [(Int, CommandWord)]) -> (Bool -> Expr -> Text) -> [TextPart] -> Analysis
analyse seg shown parts = case scan seg (atoms shown parts) of
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
atoms :: (Bool -> Expr -> Text) -> [TextPart] -> [Atom]
atoms shown = go 0
  where
    go _ [] = []
    go i (TPInterp e : ps) = AHole i NoSpan (shown False e) : go (i + 1) ps
    go i (TPRaw e : ps) = AHole i NoSpan (shown True e) : go (i + 1) ps
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
scan :: ([Word'] -> [(Int, CommandWord)]) -> [Atom] -> Either (Span, Text) [(Int, CommandWord)]
scan segmentOf = go emptyWord [] []
  where
    -- cur: the word being accumulated
    -- seg: words of the current segment, in reverse
    -- acc: command words found so far
    go cur seg acc [] = Right (acc <> segmentOf (finish cur seg))
    go cur seg acc (a : rest) = case a of
      AHole {} -> go (push a cur {wPlain = False}) seg acc rest
      AChar _ sp c
        | c == '\'' -> quoted sp '\'' False cur seg acc rest
        | c == '"' -> quoted sp '"' True cur seg acc rest
        | c == '\\' -> case rest of
            (nxt : rest') -> go (push nxt (push a cur {wPlain = False})) seg acc rest'
            [] -> go (push a cur {wPlain = False}) seg acc []
        | c == '$', (open@(AChar _ _ '(') : rest') <- rest -> nested sp cur seg acc rest' [a, open]
        | c == '(' -> nested sp cur seg acc rest [a]
        | c == '`' -> backquoted sp cur seg acc rest
        -- The & or | of a redirection operator joins the word (10.9).
        | c `elem` ("&|" :: String), inRedirection c cur rest -> go (push a cur) seg acc rest
        | c `elem` ("&|;" :: String) ->
            go emptyWord [] (acc <> segmentOf (finish cur seg)) (dropDoubled c rest)
        | c == '\n' || c == '\r' -> go emptyWord [] (acc <> segmentOf (finish cur seg)) rest
        | c == ' ' || c == '\t' -> go emptyWord (finish cur seg) acc rest
        | otherwise -> go (push a cur) seg acc rest

    -- @>&@, @<&@ and @>|@ follow a redirection character in the same
    -- word; @&>@ precedes one.
    inRedirection c cur rest =
      case wAtoms cur of
        AChar _ _ prev : _ | prev == '>' || (c == '&' && prev == '<') -> True
        _ -> c == '&' && case rest of
          AChar _ _ '>' : _ -> True
          _ -> False

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
                ws <- scan segmentOf (reverse inner)
                go (push a cur') seg (acc <> ws) more
            | otherwise -> walkN (d - 1) (push a cur') (a : inner) more
          _ -> walkN d (push a cur') (a : inner) more

    backquoted sp cur seg acc rest =
      walkB (push (AChar (-1) sp '`') cur {wPlain = False}) [] rest
      where
        walkB _ _ [] = Left (sp, "unterminated nested region")
        walkB cur' inner (a : more) = case a of
          AChar _ _ '`' -> do
            ws <- scan segmentOf (reverse inner)
            go (push a cur') seg (acc <> ws) more
          _ -> walkB (push a cur') (a : inner) more

-- | The command word of one segment: skip leading assignment words,
-- then take the next word (10.9).
segment :: [Word'] -> [(Int, CommandWord)]
segment revWords = case dropWhile isAssignment (reverse revWords) of
  (w : _) -> [(wordIndex w, toCommandWord w)]
  [] -> []

-- | The program of one segment (spec 11.4): its command word, looking
-- past a reserved word that a command follows and a redirection, and
-- nothing for a reserved word that only words that are not commands
-- follow.
programSegment :: [Word'] -> [(Int, CommandWord)]
programSegment = go . reverse
  where
    go ws = case dropWhile isAssignment ws of
      (w : rest)
        | wordText w `elem` passed -> go rest
        -- An operator standing alone takes the next word as its target.
        | Just target <- redirection (wordText w) -> go (if T.null target then drop 1 rest else rest)
        | wordText w `elem` stops -> []
        | otherwise -> [(wordIndex w, toCommandWord w)]
      [] -> []
    passed = ["!", "{", "}", "if", "then", "elif", "else", "fi", "while", "until", "do", "done", "esac"]
    stops = ["for", "case", "select", "in"]

-- | A redirection word (@>out@, @2>&1@, @<@), with the target written
-- in it, empty when the target is the next word.
redirection :: Text -> Maybe Text
redirection t =
  let rest = T.dropWhile (`elem` ['0' .. '9']) t
      (op, target) = T.span (`elem` ("<>&|" :: String)) rest
   in if T.any (`elem` ("<>" :: String)) op && (T.take 1 op /= "|") then Just target else Nothing

-- | A word naming a variable to assign, which stands before a command
-- word rather than being one (10.9).
isAssignment :: Word' -> Bool
isAssignment w =
  let t = wordText w
      (name, rest) = T.breakOn "=" t
   in not (T.null rest)
        && not (T.null name)
        && T.all identChar name
        && identStart (T.head name)
  where
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
wordText w = T.concat (map piece (reverse (wAtoms w)))
  where
    piece (AChar _ _ c) = T.singleton c
    piece (AHole _ _ t) = t

wordSpan :: Word' -> Span
wordSpan w = foldr ((<>) . atomSpan) NoSpan (wAtoms w)
