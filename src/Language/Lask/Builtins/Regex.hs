{-# LANGUAGE OverloadedStrings #-}

-- | Regular expressions of spec 15.3.
--
-- The engine is regex-tdfa: a DFA implementation with no
-- backtracking, so matching is linear in the length of the input
-- whatever the pattern is. That is the property the specification
-- requires, because a task definition runs unattended and a pattern
-- that backtracked catastrophically would stall the run instead of
-- reporting an error.
--
-- The surface syntax the specification fixes is POSIX ERE plus the
-- @\\d@ @\\w@ @\\s@ escapes, which POSIX itself does not have;
-- 'translate' rewrites those into the classes they stand for before
-- the pattern reaches the engine.
module Language.Lask.Builtins.Regex
  ( Regex,
    compile,
    test,
    matchGroups,
    replaceAll,
    translate,
  )
where

import Data.Array ((!))
import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA (Regex, makeRegexM, matchAllText, matchOnceText, matchTest)
import Text.Regex.TDFA.Text ()

-- | Compile a pattern, or report why it cannot be compiled.
compile :: Text -> Either Text Regex
compile pat = do
  ere <- translate pat
  case makeRegexM (T.unpack ere) of
    Just r -> Right r
    Nothing -> Left ("malformed regular expression: '" <> pat <> "'")

test :: Regex -> Text -> Bool
test = matchTest

-- | The whole match followed by each capture group, or the empty list
-- when the pattern does not match. A group that did not participate
-- yields the empty string.
matchGroups :: Regex -> Text -> [Text]
matchGroups re s = case matchOnceText re s of
  Nothing -> []
  Just (_, groups, _) -> [fst (groups ! i) | i <- Array.indices groups]

-- | Replace every non-overlapping match, left to right. @$0@ to @$9@
-- stand for the whole match and the capture groups, and @$$@ for a
-- literal @$@.
replaceAll :: Regex -> Text -> Text -> Text
replaceAll re replacement s = go 0 (matchAllText re s)
  where
    go from [] = T.drop from s
    go from (groups : rest) =
      let (_, (off, len)) = groups ! 0
          before = T.take (off - from) (T.drop from s)
          expanded = expand groups
       in if len == 0
            then -- An empty match advances one character, so the scan
            -- cannot stand still on it.
              before <> expanded <> T.take 1 (T.drop off s) <> go (off + 1) (dropUntil (off + 1) rest)
            else before <> expanded <> go (off + len) rest

    dropUntil from = dropWhile (\g -> let (_, (o, _)) = g ! 0 in o < from)

    expand groups = build (T.unpack replacement)
      where
        build [] = ""
        build ('$' : '$' : cs) = "$" <> build cs
        build ('$' : d : cs)
          | d >= '0' && d <= '9' =
              let i = fromEnum d - fromEnum '0'
                  (lo, hi) = Array.bounds groups
               in (if i >= lo && i <= hi then fst (groups ! i) else "") <> build cs
        build (c : cs) = T.singleton c <> build cs

-- | Rewrite the specification's syntax into POSIX ERE.
--
-- Only the escapes POSIX lacks are rewritten; everything else,
-- including an escaped metacharacter, is passed through untouched.
translate :: Text -> Either Text Text
translate pat = fmap T.concat (outside (T.unpack pat))
  where
    outside [] = Right []
    outside ('\\' : c : cs) = case classFor c of
      Just (positive, set) ->
        ((if positive then "[" else "[^") <> set <> "]" :) <$> outside cs
      Nothing -> ((T.pack ['\\', c]) :) <$> outside cs
    outside ('\\' : []) = Left "regular expression ends in a trailing backslash"
    -- 'inside' takes over at the '[', with the leading-position rules
    -- for ']' and '^' that a bracket expression has.
    outside ('[' : cs) = do
      (body, after) <- inside cs True
      (("[" <> body) :) <$> outside after
    outside (c : cs) = (T.singleton c :) <$> outside cs

    -- Inside a bracket expression the escapes stand for their sets
    -- without brackets of their own; a negated one has no bracket-free
    -- form, so it is rejected rather than silently mismatched.
    inside [] _ = Left "unterminated character class in a regular expression"
    inside (']' : cs) leading
      | leading = do
          (body, after) <- inside cs False
          Right ("]" <> body, after)
      | otherwise = Right ("]", cs)
    inside ('^' : cs) leading
      | leading = do
          (body, after) <- inside cs True
          Right ("^" <> body, after)
    inside ('\\' : c : cs) _ = case classFor c of
      Just (True, set) -> do
        (body, after) <- inside cs False
        Right (set <> body, after)
      Just (False, _) ->
        Left ("'\\" <> T.singleton c <> "' cannot be used inside a character class")
      Nothing -> do
        (body, after) <- inside cs False
        Right (T.pack ['\\', c] <> body, after)
    inside (c : cs) _ = do
      (body, after) <- inside cs False
      Right (T.singleton c <> body, after)

    classFor c = case c of
      'd' -> Just (True, digits)
      'D' -> Just (False, digits)
      'w' -> Just (True, word)
      'W' -> Just (False, word)
      's' -> Just (True, spaces)
      'S' -> Just (False, spaces)
      _ -> Nothing

    digits = "0-9"
    word = "0-9A-Za-z_"
    spaces = " \t\n\r\f\v"
