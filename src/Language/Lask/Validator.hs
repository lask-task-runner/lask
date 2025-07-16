{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lask.Validator
  ( validateEnvironment,
    infer,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Data.Maybe (fromMaybe, isJust)
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (..), findExpr, findExprFromEnvironment, findParameter, findStatement)
import Language.Lask.Error (LanguageError, LanguageError' (DuplicateNameError, UndefinedError))
import Language.Lask.Span (Span (NoSpan))

validateEnvironment :: (Eq a) => Environment a -> [LanguageError a]
validateEnvironment env@(Environment _ (_ :< AST.Module statements)) = do
  join $
    map (validateStatementDuplicate statements) statements
      ++ map (validateStatementUndefined env []) statements

validateStatementDuplicate ::
  (Eq a) =>
  [AST.Statement a] ->
  AST.Statement a ->
  [LanguageError a]
validateStatementDuplicate statements s@(sp :< AST.ExprStatement name _) = case findStatement statements name of
  Just s' -> ([sp :< DuplicateNameError name | s /= s'])
  Nothing -> []

validateStatementUndefined ::
  (Eq a) =>
  Environment a ->
  [AST.Parameter a] ->
  AST.Statement a ->
  [LanguageError a]
validateStatementUndefined env args (_ :< AST.ExprStatement _ expr) = validateExpressionUndefined env args expr

validateExpressionUndefined ::
  (Eq a) =>
  Environment a ->
  [AST.Parameter a] ->
  AST.Expr a ->
  [LanguageError a]
validateExpressionUndefined env args (sp :< expr) = case expr of
  AST.Var name -> ([sp :< UndefinedError name | not (referenceable env args name)])
  AST.Accessor target _ -> validateExpressionUndefined env args target
  AST.Call func arguments ->
    validateExpressionUndefined env args func
      ++ concatMap (validateArgumentUndefined env args) arguments
  AST.Lambda params body _ ->
    let newArgs = args ++ params
     in validateExpressionUndefined env newArgs body
  AST.Array elements -> concatMap (validateExpressionUndefined env args) elements
  AST.Object properties -> concatMap (validateObjectPropertyUndefined env args) properties
  AST.FixtureFun {} -> [] -- FixtureFun doesn't contain expressions to validate
  AST.Error _ _ -> [] -- Error message is just a string, not an expression
  _ -> []

referenceable ::
  Environment a ->
  [AST.Parameter a] ->
  String ->
  Bool
referenceable env args name =
  isJust (findExprFromEnvironment env name)
    || isJust (findParameter args name)

validateArgumentUndefined ::
  (Eq a) =>
  Environment a ->
  [AST.Parameter a] ->
  AST.Argument a ->
  [LanguageError a]
validateArgumentUndefined env args (_ :< AST.PositionedArgument _ expr) = validateExpressionUndefined env args expr
validateArgumentUndefined env args (_ :< AST.KeywordArgument _ expr) = validateExpressionUndefined env args expr

validateObjectPropertyUndefined ::
  (Eq a) =>
  Environment a ->
  [AST.Parameter a] ->
  (AST.Expr a, AST.Expr a) ->
  [LanguageError a]
validateObjectPropertyUndefined env args (key, value) =
  validateExpressionUndefined env args key ++ validateExpressionUndefined env args value

class Inferable a where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    a ->
    AST.Type Span

instance Inferable (AST.Expr Span) where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    AST.Expr Span ->
    AST.Type Span
  infer env args (s :< e) = case e of
    AST.Null -> NoSpan :< AST.NullType
    AST.Bool _ -> NoSpan :< AST.BoolType
    AST.Number _ -> NoSpan :< AST.NumberType
    AST.String _ -> NoSpan :< AST.StringType
    AST.Array _ -> NoSpan :< AST.AnyType
    AST.Object _ -> NoSpan :< AST.AnyType
    AST.Image _ -> NoSpan :< AST.ImageType
    AST.Var name -> case findExpr env args name of
      Just a -> infer env args a
      Nothing -> NoSpan :< AST.UnknownType -- undefined variable
    AST.Accessor {} -> NoSpan :< AST.AnyType
    AST.Call f _ -> case infer env args f of
      (_ :< AST.LambdaType _ t) -> t
      _ -> NoSpan :< AST.UnknownType -- not function
    AST.Lambda ps body _ ->
      NoSpan :< AST.LambdaType (map inferParameter ps) (infer env args body)
    AST.FixtureFun _ _ maybeType -> case maybeType of
      Just t -> t
      Nothing -> NoSpan :< AST.AnyType
    AST.Error {} -> NoSpan :< AST.AnyType
    where
      inferParameter (_ :< AST.PositionedParameter _ isRest isOption maybeType _) =
        NoSpan :< AST.PositionedParameterType isRest isOption (fromMaybe (s :< AST.AnyType) maybeType)
      inferParameter (_ :< AST.KeywordParameter name isRest isOption maybeType _) =
        NoSpan :< AST.KeywordParameterType name isRest isOption (fromMaybe (s :< AST.AnyType) maybeType)

instance Inferable (AST.Parameter Span) where
  infer ::
    Environment Span ->
    [AST.PackedArgument Span] ->
    AST.Parameter Span ->
    AST.Type Span
  infer _ _ _ = NoSpan :< AST.AnyType
