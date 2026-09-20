{-# LANGUAGE OverloadedStrings #-}

-- | Glob pattern matching for the built-in @glob@ (spec 15.11).
--
-- The syntax is the one the specification fixes: @*@ and @?@ within a
-- single path component, @[...]@ character classes, and @**@ as a
-- whole component standing for any number of components. Matching is
-- purely lexical: it never touches the filesystem, so the same matcher
-- serves a local traversal and the output of @find@ inside a
-- container, and both environments agree on what a pattern selects.
module Language.Lask.Runtime.Glob
  ( matchGlob,
    globPrefix,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | True when the path matches the pattern. Both are POSIX paths
-- (spec 15.10: @\/@ is the only separator).
--
-- A component beginning with @.@ matches only a pattern component that
-- also begins with a literal @.@, so neither @*@ nor @**@ descends
-- into dot directories on its own.
matchGlob :: Text -> Text -> Bool
matchGlob pat path = comps (split pat) (split path)
  where
    comps [] [] = True
    comps [] _ = False
    comps ("**" : ps) cs =
      comps ps cs || case cs of
        (c : rest) | not (hidden c) -> comps ("**" : ps) rest
        _ -> False
    comps (p : ps) (c : cs) = component p c && comps ps cs
    comps _ [] = False

    hidden = T.isPrefixOf "."

-- | The leading run of literal components of a pattern, as a path.
-- Traversal starts there instead of at the working directory, so an
-- absolute pattern does not walk the whole filesystem. The empty text
-- means \"start where the pattern is rooted\".
globPrefix :: Text -> Text
globPrefix pat = case split pat of
  [] -> ""
  cs -> T.intercalate "/" (takeWhile literal (init' cs))
  where
    init' xs = take (length xs - 1) xs
    literal c = not (T.any (`elem` ("*?[" :: String)) c)

split :: Text -> [Text]
split = filter (not . T.null) . T.splitOn "/"

-- | Match one path component against one pattern component.
component :: Text -> Text -> Bool
component p c
  | T.isPrefixOf "." c && not (T.isPrefixOf "." p) = False
  | otherwise = go (T.unpack p) (T.unpack c)
  where
    go [] [] = True
    go ('*' : ps) cs = go ps cs || (not (null cs) && go ('*' : ps) (drop 1 cs))
    go ('?' : ps) (_ : cs) = go ps cs
    go ('[' : ps) (ch : cs) = case charClass ps of
      Just (test, rest) -> test ch && go rest cs
      -- An unterminated class is a literal '['.
      Nothing -> ch == '[' && go ps cs
    go (x : ps) (ch : cs) = x == ch && go ps cs
    go _ _ = False

-- | Parse a @[...]@ class, returning its test and the rest of the
-- pattern. @!@ or @^@ right after @[@ negates; a @]@ in first position
-- is a literal.
charClass :: String -> Maybe (Char -> Bool, String)
charClass s0 =
  let (negated, s1) = case s0 of
        ('!' : r) -> (True, r)
        ('^' : r) -> (True, r)
        r -> (False, r)
      (leading, s2) = case s1 of
        (']' : r) -> ([Left ']'], r)
        r -> ([], r)
   in do
        (items, rest) <- collect s2
        let test ch = any (matches ch) (leading <> items)
        pure (if negated then not . test else test, rest)
  where
    collect (']' : r) = Just ([], r)
    collect (a : '-' : b : r)
      | b /= ']' = do
          (items, rest) <- collect r
          pure (Right (a, b) : items, rest)
    collect (a : r) = do
      (items, rest) <- collect r
      pure (Left a : items, rest)
    collect [] = Nothing

    matches ch (Left a) = ch == a
    matches ch (Right (a, b)) = ch >= a && ch <= b
