module Language.Lask.BundlerSpec (spec) where

import Control.Comonad.Cofree
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler
import Test.Hspec

spec :: Spec
spec = do
  describe "findExpr" $ do
    it "find expr in prelude module" $ do
      findExpr env args "prelude-f1" `shouldBe` Just ("prelude-f1" :< AST.Null)
    it "find expr in current module" $ do
      findExpr env args "current-f1" `shouldBe` Just ("current-f1" :< AST.Null)
    it "find argument" $ do
      findExpr env args "arg1" `shouldBe` Just ("arg1" :< AST.Null)
    it "find nothing" $ do
      findExpr env args "not-found" `shouldBe` Nothing

args :: [AST.PackedArgument String]
args = [("arg1", (Just $ "arg1" :< AST.Null, Just $ "type-arg1" :< AST.TypeVar "Null"))]

env :: Environment String
env =
  Environment
    { preludeModule = preludeStub,
      currentModule = currentStub
    }

preludeStub :: AST.Module String
preludeStub =
  "preludeStub"
    :< AST.Module
      [ "f1" :< AST.ExprStatement "prelude-f1" ("prelude-f1" :< AST.Null)
      ]

currentStub :: AST.Module String
currentStub =
  "currentStub"
    :< AST.Module
      [ "f1" :< AST.ExprStatement "current-f1" ("current-f1" :< AST.Null)
      ]
