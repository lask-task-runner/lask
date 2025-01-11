{-# LANGUAGE LambdaCase #-}

module Language.Lask.Executor
  ( eval,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (isJust)
import GHC.IO.Exception (ExitCode (ExitFailure))
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (Environment))
import Language.Lask.Error (LanguageError, LanguageError' (RuntimeError))
import Language.Lask.Utils (coFst)

eval ::
  (Semigroup a, Eq a) =>
  Environment a ->
  [AST.PackedArgument a] ->
  AST.Expr a ->
  ExceptT (LanguageError a) IO (AST.Expr a)
eval env args v = case v of
  e@(_ :< AST.Error {}) -> pure e
  s :< AST.Array vs -> (:<) s . AST.Array <$> mapM (eval env args) vs
  s :< AST.Object vs -> (:<) s . AST.Object <$> mapM (\(k, e) -> (,) k <$> eval env args e) vs
  s :< AST.Var name -> case findExpr env args name of
    Just a -> eval env args a
    Nothing -> throwError $ s :< RuntimeError ("Not defined: " <> name) (ExitFailure 1)
  _ :< AST.Call f args' -> do
    evaluatedArgs <-
      mapM
        ( \case
            (s :< AST.PositionedArgument ex e) -> (s :<) . AST.PositionedArgument ex <$> eval env args e
            (s :< AST.KeywordArgument i e) -> (s :<) . AST.KeywordArgument i <$> eval env args e
        )
        args'
    evaluatedFunc <- eval env args f
    case evaluatedFunc of
      (_ :< AST.Lambda pms (s' :< e) _) -> do
        let packedArgs = packArguments pms evaluatedArgs
        eval env (packedArgs <> args) (s' :< e)
      (_ :< AST.FixtureFun pms e _) -> do
        let packedArgs = packArguments pms evaluatedArgs
        res <- liftIO $ e packedArgs
        eval env args res
      (s :< e) ->
        case args of
          [] -> pure $ s :< e
          _ -> throwError $ s :< RuntimeError "Not a function" (ExitFailure 1)
  s :< AST.Accessor e (_ :< key) -> do
    evaluatedExpr <- eval env args e
    case evaluatedExpr of
      (_ :< AST.Object vs) -> case find (\(_ :< key', _) -> key == key') vs of -- TODO: Implement equality
        Just (_, foundValue) -> pure foundValue
        Nothing -> pure $ s :< AST.Null
      _ -> throwError $ s :< RuntimeError "Accessors can only be used on objects" (ExitFailure 1)
  _ -> pure v

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
