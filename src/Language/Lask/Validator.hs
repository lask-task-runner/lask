{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Lask.Validator
  ( validateEnvironment,
    infer,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (..), findExpr, findStatement)
import Language.Lask.Error (LanguageError, LanguageError' (SemanticError))
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
