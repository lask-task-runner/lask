{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Utils
  ( Pretty (..),
    kebabToSnake,
    snakeToKebab,
    cliPath,
    functionPath,
    namespacesOf,
    safeReadFile,
    showWithBrackets,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)


class Pretty a where
  pretty :: a -> String

-- | The CLI name mapping of spec 11.2: identifiers cannot contain
-- @-@, so a kebab-case name on the command line designates the
-- snake_case declaration.
--
-- Example:
--
-- >>> kebabToSnake (T.pack "show-version")
-- "show_version"
kebabToSnake :: Text -> Text
kebabToSnake = T.map (\c -> if c == '-' then '_' else c)

-- | The inverse of 'kebabToSnake': how the CLI writes a name (spec
-- 11.6). An @_@ that separates words is written @-@; one that leads
-- the name is kept, since a word beginning with @-@ on the command
-- line is read as an option.
--
-- Example:
--
-- >>> snakeToKebab (T.pack "show_version")
-- "show-version"
-- >>> snakeToKebab (T.pack "_build_all")
-- "_build-all"
snakeToKebab :: Text -> Text
snakeToKebab t = leading <> T.map (\c -> if c == '_' then '-' else c) rest
  where
    (leading, rest) = T.span (== '_') t

-- | How the CLI writes the qualified name of a function a namespace
-- holds (spec 11.2, 11.6): its namespaces and its name as words, each
-- in kebab-case.
--
-- >>> cliPath (T.pack "db_tools.run_migrations")
-- "db-tools run-migrations"
cliPath :: Text -> Text
cliPath = T.intercalate " " . map snakeToKebab . T.splitOn "."

-- | The namespaces that qualified names go through, as declared:
-- @db@ and @db.tools@ for @db.tools.migrate@.
--
-- >>> Set.toList (namespacesOf [T.pack "db.tools.migrate", T.pack "build"])
-- ["db","db.tools"]
namespacesOf :: [Text] -> Set Text
namespacesOf names =
  Set.fromList
    [ T.intercalate "." (take k segs)
    | n <- names,
      let segs = T.splitOn "." n,
      k <- [1 .. length segs - 1]
    ]

-- | The function path at the head of the words of a command line
-- (spec 11.2): the first word, and each word after it while what has
-- been read names a namespace. A word is a name or several joined by
-- @.@, so @db migrate@, @db.migrate@ and @db.tools migrate@ are all
-- read. A word beginning with @-@ ends the path. Returns the path as
-- declared, the words it took, and the words after it.
--
-- >>> functionPath (Set.fromList [T.pack "db"]) (map T.pack ["db", "migrate", "--dry-run"])
-- ("db.migrate",["db","migrate"],["--dry-run"])
functionPath :: Set Text -> [Text] -> (Text, [Text], [Text])
functionPath _ [] = ("", [], [])
functionPath namespaces (w0 : ws0) = go (kebabToSnake w0) [w0] ws0
  where
    go n taken (w : ws)
      | n `Set.member` namespaces,
        not ("-" `T.isPrefixOf` w) =
          go (n <> "." <> kebabToSnake w) (taken <> [w]) ws
    go n taken ws = (n, taken, ws)

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ TIO.readFile filePath
    else throwError ("does not exist: " <> filePath)

-- | Show with brackets if the value contains a space.
--
-- Example:
--
-- >>> showWithBrackets "a"
-- "a"
--
-- >>> showWithBrackets "a b"
-- "(a b)"
showWithBrackets :: String -> String
showWithBrackets s =
  if " " `isInfixOf` s
    then "(" <> s <> ")"
    else s
