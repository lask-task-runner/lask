module Language.Lask.Utils
  ( Pretty (..),
    kebabToSnake,
    snakeToKebab,
    safeReadFile,
    showWithBrackets,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isInfixOf)
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
