{-# LANGUAGE LambdaCase #-}

module Language.Lask.Bundler
  ( Environment (..),
    readModule,
    findExpr,
    findExprFromArguments,
    findExprFromEnvironment,
    findExprFromStatements,
    findStatement,
    findParameter,
    packArguments,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (join)
import Control.Monad.Except (ExceptT, liftEither, withExceptT)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Language.Lask.AST as AST
import Language.Lask.Error (LanguageError, LanguageError' (..))
import Language.Lask.Parser (pModule, parse)
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Utils (coFst, safeReadFile)

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

findExpr :: Environment a -> [AST.PackedArgument a] -> String -> Maybe (AST.Expr a)
findExpr env args name =
  join $
    find
      isJust
      [ findExprFromArguments args name,
        findExprFromEnvironment env name
      ]

findExprFromArguments :: [AST.PackedArgument a] -> String -> Maybe (AST.Expr a)
findExprFromArguments args name =
  case find (\(n, _) -> n == name) args of
    Just (_, (e, _)) -> e
    Nothing -> Nothing

findExprFromEnvironment :: Environment a -> String -> Maybe (AST.Expr a)
findExprFromEnvironment
  ( Environment
      (_ :< AST.Module preludeStatements)
      (_ :< AST.Module currentStatements)
    )
  name =
    join $ find isJust $ [maybeExprFromCurrentModule] <> [maybeExprFromPrelude]
    where
      -- find expr from current module
      maybeExprFromCurrentModule = findExprFromStatements currentStatements name
      -- find expr from prelude
      maybeExprFromPrelude = findExprFromStatements preludeStatements name

findExprFromStatements :: [AST.Statement a] -> String -> Maybe (AST.Expr a)
findExprFromStatements statements name =
  case find (\(_ :< AST.ExprStatement name' _) -> name' == name) (filter AST.isExpr statements) of
    Just (_ :< AST.ExprStatement _ e) -> Just e
    Nothing -> Nothing

findStatement :: [AST.Statement a] -> String -> Maybe (AST.Statement a)
findStatement statements name =
  find (\(_ :< AST.ExprStatement name' _) -> name' == name) (filter AST.isExpr statements)

findParameter :: [AST.Parameter a] -> String -> Maybe (AST.Parameter a)
findParameter params name =
  find
    ( \case
        _ :< AST.PositionedParameter name' _ _ _ _ -> name' == name
        _ :< AST.KeywordParameter name' _ _ _ _ -> name' == name
    )
    params

packArguments ::
  (Semigroup ann) =>
  [AST.Parameter ann] ->
  [AST.Argument ann] ->
  [AST.PackedArgument ann]
packArguments params args =
  packPositionedArg positionedParams positionedArgs
    <> packKeywordArg keywordParams keywordArgs
  where
    positionedParams = filter AST.isPositionedParameter params
    keywordParams = filter AST.isKeywordParameter params
    positionedArgs = flattenPositionedArg $ filter AST.isPositionedArgument args
    keywordArgs = filter AST.isKeywordArgument args
    flattenPositionedArg :: [AST.Argument ann] -> [AST.Expr ann]
    flattenPositionedArg
      ((_ :< AST.PositionedArgument True (_ :< AST.Array es)) : as) =
        es <> flattenPositionedArg as
    flattenPositionedArg
      ((_ :< AST.PositionedArgument _ a) : as) =
        a : flattenPositionedArg as
    flattenPositionedArg [] = []
    flattenPositionedArg ((_ :< AST.KeywordArgument {}) : _) = error "internal error"
    packPositionedArg ::
      (Semigroup ann) =>
      [AST.Parameter ann] ->
      [AST.Expr ann] ->
      [AST.PackedArgument ann]
    packPositionedArg [] _ = []
    packPositionedArg [_ :< AST.PositionedParameter name True _ t _] as =
      [(name, (Just $ foldl1 (<>) (map coFst $ tail as) :< AST.Array as, t))]
    packPositionedArg [_ :< AST.PositionedParameter name _ _ t _] (a : _) =
      [(name, (Just a, t))]
    packPositionedArg ((_ :< AST.PositionedParameter name _ _ t defaultValue) : ps) [] =
      (name, (defaultValue, t)) : packPositionedArg ps []
    packPositionedArg ((_ :< AST.PositionedParameter name _ _ t _) : ps) (a : as) =
      (name, (Just a, t)) : packPositionedArg ps as
    packPositionedArg ((_ :< AST.KeywordParameter {}) : _) _ = error "internal error"
    packKeywordArg ::
      [AST.Parameter ann] ->
      [AST.Argument ann] ->
      [AST.PackedArgument ann]
    packKeywordArg ((_ :< AST.KeywordParameter name _ _ t defaultValue) : ps) as =
      (name, (join $ find isJust [maybeValue, defaultValue], t)) : packKeywordArg ps as
      where
        maybeValue = case find (\p -> name == argName p) as of
          Just (_ :< AST.KeywordArgument _ a) -> Just a
          _ -> Nothing
    packKeywordArg [] _ = []
    packKeywordArg ((_ :< AST.PositionedParameter {}) : _) _ = error "internal error"
    argName :: AST.Argument ann -> String
    argName (_ :< AST.KeywordArgument name _) = name
    argName (_ :< AST.PositionedArgument {}) = error "internal error"
