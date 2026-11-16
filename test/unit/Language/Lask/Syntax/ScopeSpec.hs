{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Syntax.ScopeSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Position (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseExpr, parseModule)
import Language.Lask.Syntax.Scope
import Test.Hspec

parsed :: [Text] -> Module
parsed ls = either (error . show) id (parseModule "test.lask" (T.unlines ls))

at :: Int -> Int -> Position
at = Position "test.lask"

-- | The children of an expression, without their spans.
children :: Text -> Either String [ExprF]
children src = either (Left . show) (Right . map (exprF . stripSpansExpr) . childExprs) (parseExpr "test.lask" src)

scoped :: Module
scoped =
  parsed
    [ "f(a, --k = 1) = do {",
      "  b = a",
      "  g = \\(c) -> c",
      "  for (x: [b]) { x }",
      "  try { b } catch (e) { e }",
      "}",
      "y = 1",
      "h(a) = \\(a) -> a"
    ]

calls :: Module
calls =
  parsed
    [ "x = tf.init(a, g(b))",
      "y = f(g(1)(2))"
    ]

spec :: Spec
spec = do
  describe "localsAt" $ do
    let locals = localsAt "test.lask" scoped

    it "sees the parameters of the function, and not a binding in its own right-hand side" $
      locals (at 2 7) `shouldBe` ["a", "k"]

    it "lists the innermost bindings first: lambda parameters, then do bindings, then parameters" $
      locals (at 3 15) `shouldBe` ["c", "b", "a", "k"]

    it "sees a do binding only in the statements after it" $ do
      locals (at 4 12) `shouldBe` ["b", "g", "a", "k"]
      locals (at 5 9) `shouldBe` ["b", "g", "a", "k"]

    it "sees a for variable in the loop's block, and not in what it iterates over" $
      locals (at 4 18) `shouldBe` ["x", "b", "g", "a", "k"]

    it "sees a catch variable in its handler" $
      locals (at 5 25) `shouldBe` ["e", "b", "g", "a", "k"]

    it "lists a shadowed name once" $
      locals (at 8 16) `shouldBe` ["a"]

    it "sees nothing outside a function" $
      locals (at 7 5) `shouldBe` []

    it "sees nothing in another file, and matches a path however it is spelt" $ do
      localsAt "other.lask" scoped (at 2 7) `shouldBe` []
      localsAt "./test.lask" scoped (at 2 7) `shouldBe` ["a", "k"]

    it "takes the end of a span as outside it" $ do
      let m = parsed ["f(a) = a"]
      localsAt "test.lask" m (at 1 8) `shouldBe` ["a"]
      localsAt "test.lask" m (at 1 9) `shouldBe` []

  describe "enclosingCall" $ do
    let callAt l c = enclosingCall "test.lask" calls (at l c)

    it "names the callee, dotted, of the call whose arguments the position is in" $ do
      callAt 1 13 `shouldBe` Just ["tf", "init"]
      callAt 1 15 `shouldBe` Just ["tf", "init"]

    it "names the innermost call" $
      callAt 1 18 `shouldBe` Just ["g"]

    it "names no call in its callee" $ do
      callAt 1 8 `shouldBe` Nothing
      callAt 1 1 `shouldBe` Nothing

    it "skips a call whose callee is no dotted name" $
      callAt 2 12 `shouldBe` Just ["f"]

  describe "childExprs" $ do
    it "gives a call its callee and arguments" $
      children "f(1, k = 2)" `shouldBe` Right [EVar "f", ENumber 1, ENumber 2]

    it "gives the expressions of the blocks of if, do and try" $ do
      children "if (c) { a } else { b }" `shouldBe` Right [EVar "c", EVar "a", EVar "b"]
      children "do { a = 1; a }" `shouldBe` Right [ENumber 1, EVar "a"]
      children "try { a } catch (e) { b } finally { c }" `shouldBe` Right [EVar "a", EVar "b", EVar "c"]

    it "gives the holes of a string, and the environment and holes of a command" $ do
      children "\"x#{a}y#{b}\"" `shouldBe` Right [EVar "a", EVar "b"]
      children "$[#local] echo #{a}" `shouldBe` Right [EEnv "local" Nothing Nothing, EVar "a"]

    it "gives the arguments and run options of an environment expression" $
      children "#img(1){user: u}" `shouldBe` Right [ENumber 1, EVar "u"]

    it "gives a case its scrutinee and its value heads and bodies, and no type heads" $
      children "case (s) { 1 -> a; Null -> b; else -> c }" `shouldBe` Right [EVar "s", ENumber 1, EVar "a", EVar "b", EVar "c"]

    it "gives a lambda its parameter defaults, then its body" $
      children "\\(x, --k = d) -> x" `shouldBe` Right [EVar "d", EVar "x"]

    it "gives a leaf nothing" $
      children "x" `shouldBe` Right []

  describe "paramDefaults" $
    it "gives the defaults of the keyword parameters" $
      case moduleDecls (stripSpansModule (parsed ["f(a, --k = 1, --r) = a"])) of
        [Decl _ (DFunction _ _ ps _ _ _)] -> map exprF (paramDefaults ps) `shouldBe` [ENumber 1]
        ds -> expectationFailure (show ds)
