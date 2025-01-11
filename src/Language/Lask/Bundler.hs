module Language.Lask.Bundler
  ( Environment (..),
    readModule,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Except (ExceptT, liftEither, withExceptT)
import qualified Language.Lask.AST as AST
import Language.Lask.Error (LanguageError, LanguageError' (..))
import Language.Lask.Parser (pModule, parse)
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Utils (safeReadFile)

data Environment a = Environment
  { preludeModule :: AST.Module a,
    currentModule :: AST.Module a
  }
  deriving (Show, Eq)

readModule :: FilePath -> ExceptT [LanguageError Span] IO (AST.Module Span)
readModule fileName = do
  src <- withExceptT (\e -> [NoSpan :< BundleError e]) $ safeReadFile fileName
  (m, _) <- liftEither $ parse pModule fileName src
  pure m
