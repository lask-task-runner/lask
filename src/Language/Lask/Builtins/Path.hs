{-# LANGUAGE OverloadedStrings #-}

-- | Path operations of spec 15.10.
--
-- These are lexical operations on strings and never consult the
-- filesystem. Paths are POSIX paths whatever the host is: @\/@ is the
-- only separator, so a path computed on the host still means the same
-- thing inside a container (10.8). That is why "System.FilePath",
-- whose behaviour follows the host operating system, is deliberately
-- not used here.
module Language.Lask.Builtins.Path
  ( pathJoin,
    dirname,
    basename,
    extname,
    normalizePath,
    isAbsolutePath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | Join the parts with a single separator and normalize. An absolute
-- part discards everything before it.
pathJoin :: [Text] -> Text
pathJoin = normalizePath . foldl step ""
  where
    step acc part
      | T.null part = acc
      | isAbsolutePath part = part
      | T.null acc = part
      | otherwise = acc <> "/" <> part

-- | Everything before the final component: @\"a\/b\/c\"@ gives
-- @\"a\/b\"@, a path with no separator gives @\".\"@, and a child of
-- the root gives @\"\/\"@.
dirname :: Text -> Text
dirname p = case splitLast (dropTrailing p) of
  (_, "") -> "."
  ("", _) | isAbsolutePath p -> "/"
  ("", _) -> "."
  (parent, _)
    | T.all (== '/') parent -> "/"
    | otherwise -> parent

-- | The final component, ignoring trailing separators.
basename :: Text -> Text
basename p = case splitLast (dropTrailing p) of
  (_, name) | not (T.null name) -> name
  _ -> if isAbsolutePath p then "/" else p

-- | The final extension of the final component, including the dot.
-- A component with no dot, or whose only dot is its first character,
-- has no extension.
extname :: Text -> Text
extname p =
  let name = basename p
   in case T.breakOnEnd "." name of
        (before, after)
          | T.null before -> "" -- no dot at all
          | before == "." -> "" -- a dotfile such as ".env"
          | T.null after -> "" -- a trailing dot
          | otherwise -> "." <> after

-- | Collapse repeated separators and resolve @.@ and @..@ lexically.
-- A leading @..@ of a relative path is kept: resolving it would need
-- the filesystem, which these functions never touch.
normalizePath :: Text -> Text
normalizePath p
  | T.null p = ""
  | otherwise =
      let absolute = isAbsolutePath p
          parts = foldl (step absolute) [] (T.splitOn "/" p)
          joined = T.intercalate "/" (reverse parts)
       in case (absolute, joined) of
            (True, "") -> "/"
            (True, j) -> "/" <> j
            (False, "") -> "."
            (False, j) -> j
  where
    step absolute acc part = case part of
      "" -> acc
      "." -> acc
      ".." -> case acc of
        (prev : rest) | prev /= ".." -> rest
        -- Above the root there is nothing, so the ascent is dropped.
        [] | absolute -> []
        other -> ".." : other
      _ -> part : acc

isAbsolutePath :: Text -> Bool
isAbsolutePath = T.isPrefixOf "/"

-- | Split off the final component, keeping the parent without its
-- trailing separator.
splitLast :: Text -> (Text, Text)
splitLast p = case T.breakOnEnd "/" p of
  (before, name) -> (dropTrailing before, name)

-- | Drop trailing separators, except from the root itself.
dropTrailing :: Text -> Text
dropTrailing t
  | T.all (== '/') t = t
  | otherwise = T.dropWhileEnd (== '/') t
