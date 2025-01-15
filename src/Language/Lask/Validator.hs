module Language.Lask.Validator
  ( validateEnvironment,
    infer,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (..), findExpr, findStatement)
import Language.Lask.Error (LanguageError, LanguageError' (SemanticError))
import Language.Lask.Fixture (tAny, tBool, tImage, tNull, tNumber, tString)
import Language.Lask.Span (Span)

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
  AST.Call a _ -> infer env args a
  AST.Lambda {} -> pure tAny
  AST.FixtureFun {} -> pure tAny
  AST.Error {} -> pure tAny
