module Language.Lask.ValidatorSpec (spec) where

import Control.Comonad.Cofree
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler
import Language.Lask.Error (LanguageError' (SemanticError))
import Language.Lask.Validator (validateEnvironment)
import Test.Hspec

spec :: Spec
spec = do
  describe "validate" $ do
    it "duplicated name" $ do
      validateEnvironment env `shouldBe` ["f2" :< SemanticError "Already defined: current-f1"]

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
      [ "f1" :< AST.ExprStatement "current-f1" ("current-f1" :< AST.Null),
        "f2" :< AST.ExprStatement "current-f1" ("current-f1" :< AST.Null)
      ]
