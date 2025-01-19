module Language.Lask.Utils
  ( tupleToCofree,
    Pretty (..),
    SwitchCofree (..),
    safeReadFile,
    coFst,
    coSnd,
    showWithBrackets,
  )
where

import Control.Comonad.Cofree
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)

tupleToCofree :: (a, f (Cofree f a)) -> Cofree f a
tupleToCofree (s, v) = s :< v

coFst :: Cofree f a -> a
coFst (m :< _) = m

coSnd :: Cofree f a -> f (Cofree f a)
coSnd (_ :< v) = v

class SwitchCofree f where
  switchCofree :: (a -> b) -> Cofree (f a) a -> Cofree (f b) b

class Pretty a where
  pretty :: a -> String

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ T.readFile filePath
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
