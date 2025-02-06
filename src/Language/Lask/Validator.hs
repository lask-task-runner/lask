{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lask.Validator
  ( validateEnvironment,
    infer,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Maybe (fromMaybe)
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (..), findExpr, findStatement)
import Language.Lask.Error (LanguageError, LanguageError' (SemanticError))
import Language.Lask.Fixture (tAny, tBool, tImage, tNull, tNumber, tString)
import Language.Lask.Span (Span (NoSpan))

validateEnvironment :: (Eq a) => Environment a -> [LanguageError a]
validateEnvironment (Environment _ (_ :< AST.Module statements)) = do
  join $ map (validateStatementDuplicate statements) statements

validateStatementDuplicate ::
  (Eq a) =>
  [AST.Statement a] ->
  AST.Statement a ->
  [LanguageError a]
validateStatementDuplicate statements s@(sp :< AST.ExprStatement name _) = case findStatement statements name of
  Just s' -> ([sp :< SemanticError ("Already defined: " <> name) | s /= s'])
  Nothing -> []

class Inferable a where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    a ->
    Either [LanguageError Span] (AST.Type Span)

instance Inferable (AST.Expr Span) where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    AST.Expr Span ->
    Either [LanguageError Span] (AST.Type Span)
  infer env args (s :< e) = case e of
    AST.Null -> pure tNull
    AST.Bool _ -> pure tBool
    AST.Number _ -> pure tNumber
    AST.String _ -> pure tString
    AST.Array _ -> pure tAny
    AST.Object _ -> pure tAny
    AST.Image _ -> pure tImage
    AST.Var name -> case findExpr env args name of
      Just a -> infer env args a
      Nothing -> throwError [s :< SemanticError ("Not defined: " <> name)]
    AST.Accessor {} -> pure tAny
    AST.Call f _ -> case infer env args f of
      Left err -> Left err
      Right (_ :< AST.LambdaType _ t) -> pure t
      Right _ -> throwError [s :< SemanticError "function is expected"]
    AST.Lambda ps body _ ->
      (NoSpan :<) . AST.LambdaType (map inferParameter ps) <$> infer env args body
    AST.FixtureFun _ _ maybeType -> case maybeType of
      Just t -> pure t
      Nothing -> pure tAny
    AST.Error {} -> pure tAny
    where
      inferParameter (_ :< AST.PositionedParameter _ isRest isOption maybeType _) =
        NoSpan :< AST.PositionedParameterType isRest isOption (fromMaybe tAny maybeType)
      inferParameter (_ :< AST.KeywordParameter name isRest isOption maybeType _) =
        NoSpan :< AST.KeywordParameterType name isRest isOption (fromMaybe tAny maybeType)

instance Inferable (AST.Parameter Span) where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    AST.Parameter Span ->
    Either [LanguageError Span] (AST.Type Span)
  infer _ _ _ = pure tAny
