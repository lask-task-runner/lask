module Language.Lask
  ( tokenize,
    parse,
    load,
    validate,
    infer,
    evaluate,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Data.Text (Text)
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (Environment), findExpr)
import Language.Lask.Error (LanguageError, LanguageError' (SemanticError))
import qualified Language.Lask.Executor as E
import Language.Lask.Fixture (mPrelude)
import qualified Language.Lask.Parser as P
import Language.Lask.Span (Span (NoSpan))
import qualified Language.Lask.Validator as V

tokenize :: FilePath -> Text -> Either [LanguageError Span] [P.Token]
tokenize file src = snd <$> P.parse P.pModule file src

parse :: FilePath -> Text -> Either [LanguageError Span] (AST.Module Span)
parse file src = fst <$> P.parse P.pModule file src

load :: FilePath -> Text -> Either [LanguageError Span] (Environment Span)
load file src = Environment mPrelude <$> parse file src

validate :: FilePath -> Text -> [LanguageError Span]
validate file src = case load file src of
  Left es -> es
  Right env -> V.validateEnvironment env

infer :: FilePath -> Text -> String -> Either [LanguageError Span] (AST.Type Span)
infer file src name = do
  env <- load file src
  case findExpr env [] name of
    Just e -> V.infer env [] e
    Nothing -> Left [NoSpan :< SemanticError ("Not defined: " <> name)]

evaluate :: FilePath -> Text -> String -> [AST.Argument Span] -> ExceptT [LanguageError Span] IO (AST.Expr Span)
evaluate file src func args = do
  env <- liftEither $ load file src
  case findExpr env [] func of
    Just e@(s :< AST.Lambda {}) -> E.eval env [] (s :< AST.Call e args)
    Just e -> E.eval env [] e
    Nothing -> throwError [NoSpan :< SemanticError ("Not defined: " <> func)]
