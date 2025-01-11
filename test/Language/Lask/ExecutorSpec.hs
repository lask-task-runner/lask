module Language.Lask.ExecutorSpec (spec) where

import Control.Comonad.Cofree
import Control.Monad.Except (runExceptT)
import qualified Language.Lask.AST as AST
import Language.Lask.Bundler (Environment (..))
import Language.Lask.Executor (eval)
import Test.Hspec

spec :: Spec
spec = do
  describe "exec" $ do
    it "null" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.Null)
      result `shouldBe` Right (s 1 :< AST.Null)
    it "bool" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.Bool True)
      result `shouldBe` Right (s 1 :< AST.Bool True)
    it "number" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.Number 42)
      result `shouldBe` Right (s 1 :< AST.Number 42)
    it "string" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.String "hello")
      result `shouldBe` Right (s 1 :< AST.String "hello")
    it "array" $ do
      result <-
        runExceptT $
          eval emptyEnv [] (s 1 :< AST.Array [s 2 :< AST.Number 1, s 3 :< AST.Number 2])
      result `shouldBe` Right (s 1 :< AST.Array [s 2 :< AST.Number 1, s 3 :< AST.Number 2])
    it "object" $ do
      result <-
        runExceptT $
          eval
            emptyEnv
            []
            ( s 1
                :< AST.Object
                  [ (s 2 :< AST.String "a", s 3 :< AST.Number 1),
                    (s 4 :< AST.String "b", s 5 :< AST.Number 2)
                  ]
            )
      result
        `shouldBe` Right
          ( s 1
              :< AST.Object
                [ (s 2 :< AST.String "a", s 3 :< AST.Number 1),
                  (s 4 :< AST.String "b", s 5 :< AST.Number 2)
                ]
          )
    it "image" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.Image "image")
      result `shouldBe` Right (s 1 :< AST.Image "image")
    it "var" $ do
      result <- runExceptT $ eval emptyEnv [] (s 1 :< AST.Var "empty")
      result `shouldBe` Right (s 0 :< AST.Null)

emptyEnv :: Environment SpanStub
emptyEnv =
  Environment
    { preludeModule = emptyModule,
      currentModule = emptyModule
    }

emptyModule :: AST.Module SpanStub
emptyModule =
  s 0
    :< AST.Module
      [ s 0 :< AST.ExprStatement "empty" (s 0 :< AST.Null)
      ]

newtype SpanStub = SpanStub Integer deriving (Eq, Show)

s :: Integer -> SpanStub
s = SpanStub

instance Semigroup SpanStub where
  (SpanStub a) <> (SpanStub b) = SpanStub (a + b)
