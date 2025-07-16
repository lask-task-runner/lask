module Language.Lask.ValidatorSpec (spec) where

import Control.Comonad.Cofree
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler
import Language.Lask.Error (LanguageError' (DuplicateNameError, UndefinedError))
import Language.Lask.Validator (validateEnvironment)
import Test.Hspec

spec :: Spec
spec = do
  describe "validate" $ do
    it "duplicated name" $ do
      validateEnvironment env `shouldBe` ["f2" :< DuplicateNameError "current-f1"]
    it "undefined variable" $ do
      validateEnvironment envWithUndefined `shouldBe` ["undefined-var" :< UndefinedError "nonexistent"]
    it "function parameter should not be undefined" $ do
      validateEnvironment envWithFunction `shouldBe` []

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

envWithUndefined :: Environment String
envWithUndefined =
  Environment
    { preludeModule = preludeStub,
      currentModule = currentStubWithUndefined
    }

currentStubWithUndefined :: AST.Module String
currentStubWithUndefined =
  "currentStubWithUndefined"
    :< AST.Module
      [ "f1" :< AST.ExprStatement "defined-f1" ("defined-f1" :< AST.Null),
        "f2" :< AST.ExprStatement "undefined-var" ("undefined-var" :< AST.Var "nonexistent")
      ]

envWithFunction :: Environment String
envWithFunction =
  Environment
    { preludeModule = preludeStub,
      currentModule = currentStubWithFunction
    }

currentStubWithFunction :: AST.Module String
currentStubWithFunction =
  "currentStubWithFunction"
    :< AST.Module
      [ "f1"
          :< AST.ExprStatement
            "test-func"
            ( "test-func"
                :< AST.Lambda
                  ["param" :< AST.PositionedParameter "a" False False Nothing Nothing]
                  ("body" :< AST.Var "a")
                  Nothing
            )
      ]
