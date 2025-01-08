module Language.Lask.ASTSpec (spec) where

import Control.Comonad.Cofree
import qualified Language.Lask.AST as AST
import System.Exit (ExitCode (ExitFailure))
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "module" $ do
      shouldBe
        (show (() :< AST.Module [() :< AST.ExprStatement "a" (() :< AST.Null)] :: AST.Module ()))
        "() :< Module [() :< ExprStatement \"a\" (() :< Null)]"
    it "statement" $ do
      shouldBe
        (show (() :< AST.ExprStatement "a" (() :< AST.Null) :: AST.Statement ()))
        "() :< ExprStatement \"a\" (() :< Null)"
      shouldBe
        (show (() :< AST.TypeStatement "a" (() :< AST.TypeVar "a") :: AST.Statement ()))
        "() :< TypeStatement \"a\" (() :< TypeVar \"a\")"
    it "type" $ do
      shouldBe
        (show (() :< AST.TypeVar "a" :: AST.Type ()))
        "() :< TypeVar \"a\""
      shouldBe
        (show (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "a"] :: AST.Type ()))
        "() :< AssemblyType (() :< TypeVar \"a\") [() :< TypeVar \"a\"]"
      shouldBe
        (show (() :< AST.FuncType (() :< AST.TypeVar "a") (() :< AST.TypeVar "b") :: AST.Type ()))
        "() :< FuncType (() :< TypeVar \"a\") (() :< TypeVar \"b\")"
    it "expr" $ do
      shouldBe
        (show (() :< AST.Null :: AST.Expr ()))
        "() :< Null"
      shouldBe
        (show (() :< AST.Number 1 :: AST.Expr ()))
        "() :< Number 1.0"
      shouldBe
        (show (() :< AST.String "a" :: AST.Expr ()))
        "() :< String \"a\""
      shouldBe
        (show (() :< AST.Bool True :: AST.Expr ()))
        "() :< Bool True"
      shouldBe
        (show (() :< AST.Array [() :< AST.Null, () :< AST.Number 1] :: AST.Expr ()))
        "() :< Array [() :< Null,() :< Number 1.0]"
      shouldBe
        (show (() :< AST.Object [(() :< AST.String "a", () :< AST.Null)] :: AST.Expr ()))
        "() :< Object [(() :< String \"a\",() :< Null)]"
      shouldBe
        (show (() :< AST.Image "a" :: AST.Expr ()))
        "() :< Image \"a\""
      shouldBe
        (show (() :< AST.Var "a" :: AST.Expr ()))
        "() :< Var \"a\""
      shouldBe
        (show (() :< AST.Accessor (() :< AST.Null) (() :< AST.Null) :: AST.Expr ()))
        "() :< Accessor (() :< Null) (() :< Null)"
      shouldBe
        (show (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ()))
        "() :< Call (() :< Null) [() :< PositionedArgument False (() :< Null)]"
      shouldBe
        (show (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ()))
        "() :< Lambda [() :< PositionedParameter \"a\" False False Nothing Nothing] (() :< Null) Nothing"
      shouldBe
        (show (() :< AST.Error "a" (ExitFailure 1) :: AST.Expr ()))
        "() :< Error \"a\" (ExitFailure 1)"
      shouldBe
        (show (() :< AST.FixtureFun undefined undefined undefined :: AST.Expr ()))
        "() :< FixtureFun"
    it "parameter" $ do
      shouldBe
        (show (() :< AST.PositionedParameter "a" False False Nothing (Just $ () :< AST.Null)))
        "() :< PositionedParameter \"a\" False False Nothing (Just (() :< Null))"
      shouldBe
        (show (() :< AST.KeywordParameter "a" False False Nothing (Just $ () :< AST.Null)))
        "() :< KeywordParameter \"a\" False False Nothing (Just (() :< Null))"
    it "argument show" $ do
      shouldBe
        (show $ () :< AST.PositionedArgument False (() :< AST.Null))
        "() :< PositionedArgument False (() :< Null)"
      shouldBe
        (show $ () :< AST.KeywordArgument "a" (() :< AST.Null))
        "() :< KeywordArgument \"a\" (() :< Null)"
  describe "equality" $ do
    it "module" $ do
      shouldBe
        (() :< AST.Module [() :< AST.ExprStatement "a" (() :< AST.Null)] :: AST.Module ())
        (() :< AST.Module [() :< AST.ExprStatement "a" (() :< AST.Null)] :: AST.Module ())
      shouldNotBe
        (() :< AST.Module [() :< AST.ExprStatement "a" (() :< AST.Null)] :: AST.Module ())
        (() :< AST.Module [() :< AST.ExprStatement "b" (() :< AST.Null)] :: AST.Module ())
    it "statement" $ do
      shouldBe
        (() :< AST.ExprStatement "a" (() :< AST.Null) :: AST.Statement ())
        (() :< AST.ExprStatement "a" (() :< AST.Null) :: AST.Statement ())
      shouldNotBe
        (() :< AST.ExprStatement "a" (() :< AST.Null) :: AST.Statement ())
        (() :< AST.ExprStatement "b" (() :< AST.Null) :: AST.Statement ())
      shouldNotBe
        (() :< AST.ExprStatement "a" (() :< AST.Null) :: AST.Statement ())
        (() :< AST.ExprStatement "a" (() :< AST.Number 1) :: AST.Statement ())
      shouldBe
        (() :< AST.TypeStatement "a" (() :< AST.TypeVar "a") :: AST.Statement ())
        (() :< AST.TypeStatement "a" (() :< AST.TypeVar "a") :: AST.Statement ())
      shouldNotBe
        (() :< AST.TypeStatement "a" (() :< AST.TypeVar "a") :: AST.Statement ())
        (() :< AST.TypeStatement "b" (() :< AST.TypeVar "a") :: AST.Statement ())
      shouldNotBe
        (() :< AST.TypeStatement "a" (() :< AST.TypeVar "a") :: AST.Statement ())
        (() :< AST.TypeStatement "a" (() :< AST.TypeVar "b") :: AST.Statement ())
    it "type" $ do
      shouldBe
        (() :< AST.TypeVar "a" :: AST.Type ())
        (() :< AST.TypeVar "a" :: AST.Type ())
      shouldNotBe
        (() :< AST.TypeVar "a" :: AST.Type ())
        (() :< AST.TypeVar "b" :: AST.Type ())
      shouldBe
        (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "a"] :: AST.Type ())
        (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "a"] :: AST.Type ())
      shouldNotBe
        (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "a"] :: AST.Type ())
        (() :< AST.AssemblyType (() :< AST.TypeVar "b") [() :< AST.TypeVar "a"] :: AST.Type ())
      shouldNotBe
        (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "a"] :: AST.Type ())
        (() :< AST.AssemblyType (() :< AST.TypeVar "a") [() :< AST.TypeVar "b"] :: AST.Type ())
      shouldBe
        (() :< AST.FuncType (() :< AST.TypeVar "a") (() :< AST.TypeVar "b") :: AST.Type ())
        (() :< AST.FuncType (() :< AST.TypeVar "a") (() :< AST.TypeVar "b") :: AST.Type ())
      shouldNotBe
        (() :< AST.FuncType (() :< AST.TypeVar "a") (() :< AST.TypeVar "b") :: AST.Type ())
        (() :< AST.FuncType (() :< AST.TypeVar "b") (() :< AST.TypeVar "b") :: AST.Type ())
    it "expr" $ do
      shouldBe
        (() :< AST.Null :: AST.Expr ())
        (() :< AST.Null :: AST.Expr ())
      shouldBe
        (() :< AST.Number 1 :: AST.Expr ())
        (() :< AST.Number 1 :: AST.Expr ())
      shouldBe
        (() :< AST.Bool True :: AST.Expr ())
        (() :< AST.Bool True :: AST.Expr ())
      shouldNotBe
        (() :< AST.Bool True :: AST.Expr ())
        (() :< AST.Bool False :: AST.Expr ())
      shouldNotBe
        (() :< AST.Number 1 :: AST.Expr ())
        (() :< AST.Number 2 :: AST.Expr ())
      shouldBe
        (() :< AST.String "a" :: AST.Expr ())
        (() :< AST.String "a" :: AST.Expr ())
      shouldNotBe
        (() :< AST.String "a" :: AST.Expr ())
        (() :< AST.String "b" :: AST.Expr ())
      shouldBe
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 1] :: AST.Expr ())
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 1] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 1] :: AST.Expr ())
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 2] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 1] :: AST.Expr ())
        (() :< AST.Array [() :< AST.Null, () :< AST.Number 1, () :< AST.Number 2] :: AST.Expr ())
      shouldBe
        (() :< AST.Object [(() :< AST.String "a", () :< AST.Null)] :: AST.Expr ())
        (() :< AST.Object [(() :< AST.String "a", () :< AST.Null)] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Object [(() :< AST.String "a", () :< AST.Null)] :: AST.Expr ())
        (() :< AST.Object [(() :< AST.String "b", () :< AST.Null)] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Object [(() :< AST.String "a", () :< AST.Null)] :: AST.Expr ())
        (() :< AST.Object [(() :< AST.String "a", () :< AST.Number 1)] :: AST.Expr ())
      shouldBe
        (() :< AST.Image "a" :: AST.Expr ())
        (() :< AST.Image "a" :: AST.Expr ())
      shouldNotBe
        (() :< AST.Image "a" :: AST.Expr ())
        (() :< AST.Image "b" :: AST.Expr ())
      shouldBe
        (() :< AST.Var "a" :: AST.Expr ())
        (() :< AST.Var "a" :: AST.Expr ())
      shouldNotBe
        (() :< AST.Var "a" :: AST.Expr ())
        (() :< AST.Var "b" :: AST.Expr ())
      shouldBe
        (() :< AST.Accessor (() :< AST.Null) (() :< AST.Null) :: AST.Expr ())
        (() :< AST.Accessor (() :< AST.Null) (() :< AST.Null) :: AST.Expr ())
      shouldNotBe
        (() :< AST.Accessor (() :< AST.Null) (() :< AST.Null) :: AST.Expr ())
        (() :< AST.Accessor (() :< AST.Null) (() :< AST.Number 1) :: AST.Expr ())
      shouldNotBe
        (() :< AST.Accessor (() :< AST.Null) (() :< AST.Null) :: AST.Expr ())
        (() :< AST.Accessor (() :< AST.Number 1) (() :< AST.Null) :: AST.Expr ())
      shouldBe
        (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ())
        (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ())
        (() :< AST.Call (() :< AST.Number 1) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ())
      shouldNotBe
        (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument False (() :< AST.Null)] :: AST.Expr ())
        (() :< AST.Call (() :< AST.Null) [() :< AST.PositionedArgument True (() :< AST.Number 1)] :: AST.Expr ())
      shouldBe
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
      shouldNotBe
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
        (() :< AST.Lambda [() :< AST.PositionedParameter "b" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
      shouldNotBe
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Number 1) Nothing :: AST.Expr ())
      shouldNotBe
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) Nothing :: AST.Expr ())
        (() :< AST.Lambda [() :< AST.PositionedParameter "a" False False Nothing Nothing] (() :< AST.Null) (Just $ () :< AST.TypeVar "a") :: AST.Expr ())
      shouldBe
        (() :< AST.Error "a" (ExitFailure 1) :: AST.Expr ())
        (() :< AST.Error "a" (ExitFailure 1) :: AST.Expr ())
      shouldNotBe
        (() :< AST.Error "a" (ExitFailure 1) :: AST.Expr ())
        (() :< AST.Error "b" (ExitFailure 1) :: AST.Expr ())
      shouldNotBe
        (() :< AST.Error "a" (ExitFailure 1) :: AST.Expr ())
        (() :< AST.Error "a" (ExitFailure 2) :: AST.Expr ())
      shouldNotBe -- Always False
        (() :< AST.FixtureFun undefined undefined undefined :: AST.Expr ())
        (() :< AST.FixtureFun undefined undefined undefined :: AST.Expr ())
      shouldNotBe
        (() :< AST.Null :: AST.Expr ())
        (() :< AST.Number 1 :: AST.Expr ())
    it "parameter" $ do
      shouldBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.PositionedParameter "a" False False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.PositionedParameter "b" False False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.PositionedParameter "a" True False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.PositionedParameter "a" False True Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing)
        (() :< AST.PositionedParameter "a" False False (Just (() :< AST.TypeVar "Null")) Nothing)
      shouldNotBe
        (() :< AST.PositionedParameter "a" False False Nothing Nothing)
        (() :< AST.PositionedParameter "a" False False Nothing (Just (() :< AST.Null)))
      shouldBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.KeywordParameter "a" False False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.KeywordParameter "b" False False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.KeywordParameter "a" True False Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing :: AST.Parameter ())
        (() :< AST.KeywordParameter "a" False True Nothing Nothing :: AST.Parameter ())
      shouldNotBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing)
        (() :< AST.KeywordParameter "a" False False (Just (() :< AST.TypeVar "Null")) Nothing)
      shouldNotBe
        (() :< AST.KeywordParameter "a" False False Nothing Nothing)
        (() :< AST.KeywordParameter "a" False False Nothing (Just (() :< AST.Null)))
    it "argument" $ do
      shouldBe
        (() :< AST.PositionedArgument False (() :< AST.Null))
        (() :< AST.PositionedArgument False (() :< AST.Null))
      shouldNotBe
        (() :< AST.PositionedArgument False (() :< AST.Null))
        (() :< AST.PositionedArgument True (() :< AST.Null))
      shouldNotBe
        (() :< AST.PositionedArgument False (() :< AST.Null))
        (() :< AST.PositionedArgument False (() :< AST.Number 1))
      shouldBe
        (() :< AST.KeywordArgument "a" (() :< AST.Null))
        (() :< AST.KeywordArgument "a" (() :< AST.Null))
      shouldNotBe
        (() :< AST.KeywordArgument "a" (() :< AST.Null))
        (() :< AST.KeywordArgument "b" (() :< AST.Null))
      shouldNotBe
        (() :< AST.KeywordArgument "a" (() :< AST.Null))
        (() :< AST.KeywordArgument "a" (() :< AST.Number 1))
