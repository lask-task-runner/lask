{-# LANGUAGE LambdaCase #-}

module Language.Lask.Executor (eval) where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import GHC.IO.Exception (ExitCode (ExitFailure))
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment, findExpr, packArguments)
import Language.Lask.Error (LanguageError, LanguageError' (RuntimeError))

eval ::
  (Semigroup a, Eq a) =>
  Environment a ->
  [AST.PackedArgument a] ->
  AST.Expr a ->
  ExceptT [LanguageError a] IO (AST.Expr a)
eval env args v = case v of
  e@(_ :< AST.Error {}) -> pure e
  s :< AST.Array vs -> (:<) s . AST.Array <$> mapM (eval env args) vs
  s :< AST.Object vs -> (:<) s . AST.Object <$> mapM (\(k, e) -> (,) k <$> eval env args e) vs
  s :< AST.Var name -> case findExpr env args name of
    Just a -> eval env args a
    Nothing -> throwError [s :< RuntimeError ("Not defined: " <> name) (ExitFailure 1)]
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
          _ -> throwError [s :< RuntimeError "Not a function" (ExitFailure 1)]
  s :< AST.Accessor e (_ :< key) -> do
    evaluatedExpr <- eval env args e
    case evaluatedExpr of
      (_ :< AST.Object vs) -> case find (\(_ :< key', _) -> key == key') vs of -- TODO: Implement equality
        Just (_, foundValue) -> pure foundValue
        Nothing -> pure $ s :< AST.Null
      _ -> throwError [s :< RuntimeError "Accessors can only be used on objects" (ExitFailure 1)]
  _ -> pure v
