{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.ParserSpec (spec) where

import Control.Comonad.Cofree
import qualified Language.Lask.AST as AST
import Language.Lask.Parser
import Language.Lask.Span (mkSpan)
import Test.Hspec

spec :: Spec
spec = do
  describe "expression" $ do
    it "null" $ do
      parse pExpr "test" "null"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.Null,
            [Token TKNull (mkSpan "test" 1 1 1 5)]
          )
    it "true" $ do
      parse pExpr "test" "true"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.Bool True,
            [Token TKBool (mkSpan "test" 1 1 1 5)]
          )
    it "false" $ do
      parse pExpr "test" "false"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< AST.Bool False,
            [Token TKBool (mkSpan "test" 1 1 1 6)]
          )
    it "positive number" $ do
      parse pExpr "test" "1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 2
              :< AST.Number 1,
            [Token TKNumber (mkSpan "test" 1 1 1 2)]
          )
    it "negative number" $ do
      parse pExpr "test" "-1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< AST.Number (-1),
            [Token TKNumber (mkSpan "test" 1 1 1 3)]
          )
    it "positive float" $ do
      parse pExpr "test" "1.5"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< AST.Number 1.5,
            [Token TKNumber (mkSpan "test" 1 1 1 4)]
          )
    it "negative float" $ do
      parse pExpr "test" "-1.5"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.Number (-1.5),
            [Token TKNumber (mkSpan "test" 1 1 1 5)]
          )
    it "empty string" $ do
      parse pExpr "test" "''"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< AST.String "",
            [Token TKString (mkSpan "test" 1 1 1 3)]
          )
    it "string" $ do
      parse pExpr "test" "'abc'"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< AST.String "abc",
            [Token TKString (mkSpan "test" 1 1 1 6)]
          )
    it "empty array" $ do
      parse pExpr "test" "[]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< AST.Array [],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3)
            ]
          )
    it "array" $ do
      parse pExpr "test" "[1, 2]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 7
              :< AST.Array
                [ mkSpan "test" 1 2 1 3 :< AST.Number 1,
                  mkSpan "test" 1 5 1 6 :< AST.Number 2
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKNumber (mkSpan "test" 1 2 1 3),
              Token TKSep (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6),
              Token TKSep (mkSpan "test" 1 6 1 7)
            ]
          )
    it "empty object" $ do
      parse pExpr "test" "{}"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< AST.Object [],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3)
            ]
          )
    it "object" $ do
      parse pExpr "test" "{'a': 1, 'b': 2}"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 17
              :< AST.Object
                [ ( mkSpan "test" 1 2 1 5 :< AST.String "a",
                    mkSpan "test" 1 7 1 8 :< AST.Number 1
                  ),
                  ( mkSpan "test" 1 10 1 13 :< AST.String "b",
                    mkSpan "test" 1 15 1 16 :< AST.Number 2
                  )
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKString (mkSpan "test" 1 2 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKString (mkSpan "test" 1 10 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14),
              Token TKNumber (mkSpan "test" 1 15 1 16),
              Token TKSep (mkSpan "test" 1 16 1 17)
            ]
          )
    it "image" $ do
      parse pExpr "test" "#alpine:3.12"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13 :< AST.Image "alpine:3.12",
            [Token TKImage (mkSpan "test" 1 1 1 13)]
          )
    it "single alphabet var" $ do
      parse pExpr "test" "a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 2
              :< AST.Var "a",
            [Token TKVar (mkSpan "test" 1 1 1 2)]
          )
    it "multiple alphabet var" $ do
      parse pExpr "test" "abc"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< AST.Var "abc",
            [Token TKVar (mkSpan "test" 1 1 1 4)]
          )
    it "many word var" $ do
      parse pExpr "test" "abc-def"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 8
              :< AST.Var "abc-def",
            [Token TKVar (mkSpan "test" 1 1 1 8)]
          )
    it "lambda" $ do
      parse pExpr "test" "\\(a) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 10
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 4
                    :< AST.PositionedParameter
                      "a"
                      False
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 9 1 10 :< AST.Var "a")
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 6 1 8),
              Token TKVar (mkSpan "test" 1 9 1 10)
            ]
          )
    it "lambda with default value" $ do
      parse pExpr "test" "\\(a = 1) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 14
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 8
                    :< AST.PositionedParameter
                      "a"
                      False
                      False
                      Nothing
                      (Just (mkSpan "test" 1 7 1 8 :< AST.Number 1))
                ]
                (mkSpan "test" 1 13 1 14 :< AST.Var "a")
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKSep (mkSpan "test" 1 10 1 12),
              Token TKVar (mkSpan "test" 1 13 1 14)
            ]
          )
    it "lambda with rest parameter" $ do
      parse pExpr "test" "\\(a...) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 7
                    :< AST.PositionedParameter
                      "a"
                      True
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 12 1 13 :< AST.Var "a")
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 4 1 7),
              Token TKSep (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 9 1 11),
              Token TKVar (mkSpan "test" 1 12 1 13)
            ]
          )
    it "lambda with optional parameter" $ do
      parse pExpr "test" "\\(a?) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 11
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 5
                    :< AST.PositionedParameter
                      "a"
                      False
                      True
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 10 1 11 :< AST.Var "a")
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKSep (mkSpan "test" 1 7 1 9),
              Token TKVar (mkSpan "test" 1 10 1 11)
            ]
          )
    it "lambda with typed parameter" $ do
      parse pExpr "test" "\\(a: Number) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 18
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 12
                    :< AST.PositionedParameter
                      "a"
                      False
                      False
                      (Just (mkSpan "test" 1 6 1 12 :< AST.NumberType))
                      Nothing
                ]
                (mkSpan "test" 1 17 1 18 :< AST.Var "a")
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKTypeVar (mkSpan "test" 1 6 1 12),
              Token TKSep (mkSpan "test" 1 12 1 13),
              Token TKSep (mkSpan "test" 1 14 1 16),
              Token TKVar (mkSpan "test" 1 17 1 18)
            ]
          )
    it "lambda with return type" $ do
      parse pExpr "test" "\\(a): Number -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 18
              :< AST.Lambda
                [ mkSpan "test" 1 3 1 4
                    :< AST.PositionedParameter
                      "a"
                      False
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 17 1 18 :< AST.Var "a")
                (Just (mkSpan "test" 1 7 1 13 :< AST.NumberType)),
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKTypeVar (mkSpan "test" 1 7 1 13),
              Token TKSep (mkSpan "test" 1 14 1 16),
              Token TKVar (mkSpan "test" 1 17 1 18)
            ]
          )
    it "call with positioned argument" $ do
      parse pExpr "test" "f(1)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "f")
                [mkSpan "test" 1 3 1 4 :< AST.PositionedArgument False (mkSpan "test" 1 3 1 4 :< AST.Number 1)],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKNumber (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5)
            ]
          )
    it "call with keyword argument" $ do
      parse pExpr "test" "f(a = 1)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 9
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "f")
                [ mkSpan "test" 1 3 1 8
                    :< AST.KeywordArgument
                      "a"
                      (mkSpan "test" 1 7 1 8 :< AST.Number 1)
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9)
            ]
          )
    it "call with variable positioned argument" $ do
      parse pExpr "test" "f(a)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "f")
                [mkSpan "test" 1 3 1 4 :< AST.PositionedArgument False (mkSpan "test" 1 3 1 4 :< AST.Var "a")],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKVar (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5)
            ]
          )
    it "call with positioned and keyword argument" $ do
      parse pExpr "test" "f(1, a = 2)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 12
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "f")
                [ mkSpan "test" 1 3 1 4
                    :< AST.PositionedArgument False (mkSpan "test" 1 3 1 4 :< AST.Number 1),
                  mkSpan "test" 1 6 1 11
                    :< AST.KeywordArgument
                      "a"
                      (mkSpan "test" 1 10 1 11 :< AST.Number 2)
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKNumber (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKParameter (mkSpan "test" 1 6 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKNumber (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 11 1 12)
            ]
          )
    it "dot accessor" $ do
      parse pExpr "test" "a.b"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< AST.Accessor
                (mkSpan "test" 1 1 1 2 :< AST.Var "a")
                (mkSpan "test" 1 3 1 4 :< AST.String "b"),
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKProperty (mkSpan "test" 1 3 1 4)
            ]
          )
    it "bracket accessor" $ do
      parse pExpr "test" "a['b']"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 7
              :< AST.Accessor
                (mkSpan "test" 1 1 1 2 :< AST.Var "a")
                (mkSpan "test" 1 3 1 6 :< AST.String "b"),
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKString (mkSpan "test" 1 3 1 6),
              Token TKSep (mkSpan "test" 1 6 1 7)
            ]
          )
    it "binary expr" $ do
      parse pExpr "test" "1 + 2"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< AST.Call
                (mkSpan "test" 1 3 1 4 :< AST.Var "+")
                [ mkSpan "test" 1 1 1 2 :< AST.PositionedArgument False (mkSpan "test" 1 1 1 2 :< AST.Number 1),
                  mkSpan "test" 1 5 1 6 :< AST.PositionedArgument False (mkSpan "test" 1 5 1 6 :< AST.Number 2)
                ],
            [ Token TKNumber (mkSpan "test" 1 1 1 2),
              Token TKOp (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6)
            ]
          )
    it "command sugar syntax" $ do
      parse pExpr "test" "$ echo hello;"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 14
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "$")
                [ mkSpan "test" 1 3 1 14
                    :< AST.PositionedArgument False (mkSpan "test" 1 3 1 14 :< AST.String "echo hello")
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKString (mkSpan "test" 1 3 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14)
            ]
          )
    it "command sugar syntax with image tag" $ do
      parse pExpr "test" "$[#alpine:3.12] echo hello;"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 28
              :< AST.Call
                (mkSpan "test" 1 1 1 2 :< AST.Var "$")
                [ mkSpan "test" 1 2 1 16
                    :< AST.KeywordArgument
                      "image"
                      (mkSpan "test" 1 3 1 15 :< AST.Image "alpine:3.12"),
                  mkSpan "test" 1 17 1 28
                    :< AST.PositionedArgument False (mkSpan "test" 1 17 1 28 :< AST.String "echo hello")
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKImage (mkSpan "test" 1 3 1 15),
              Token TKSep (mkSpan "test" 1 15 1 16),
              Token TKString (mkSpan "test" 1 17 1 27),
              Token TKSep (mkSpan "test" 1 27 1 28)
            ]
          )
  describe "type" $ do
    it "type variable" $ do
      parse pType "test" "Void"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< AST.VoidType,
            [Token TKTypeVar (mkSpan "test" 1 1 1 5)]
          )
  describe "expression statement" $ do
    it "variable" $ do
      parse pExprStatement "test" "a = 1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< AST.ExprStatement
                "a"
                (mkSpan "test" 1 5 1 6 :< AST.Number 1),
            [ Token TKFunction (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6)
            ]
          )
    it "function" $ do
      parse pExprStatement "test" "f(a) = a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 9
              :< AST.ExprStatement
                "f"
                ( mkSpan "test" 1 2 1 9
                    :< AST.Lambda
                      [ mkSpan "test" 1 3 1 4
                          :< AST.PositionedParameter
                            "a"
                            False
                            False
                            Nothing
                            Nothing
                      ]
                      (mkSpan "test" 1 8 1 9 :< AST.Var "a")
                      Nothing
                ),
            [ Token TKFunction (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 6 1 7),
              Token TKVar (mkSpan "test" 1 8 1 9)
            ]
          )
  describe "module" $ do
    it "simple module" $ do
      parse pModule "test" "f(a) = h(a)\nh(a) = g(a)\n"
        `shouldBe` Right
          ( mkSpan "test" 1 1 3 1
              :< AST.Module
                [ mkSpan "test" 1 1 1 12
                    :< AST.ExprStatement
                      "f"
                      ( mkSpan "test" 1 2 1 12
                          :< AST.Lambda
                            [ mkSpan "test" 1 3 1 4
                                :< AST.PositionedParameter
                                  "a"
                                  False
                                  False
                                  Nothing
                                  Nothing
                            ]
                            ( mkSpan "test" 1 8 1 12
                                :< AST.Call
                                  (mkSpan "test" 1 8 1 9 :< AST.Var "h")
                                  [mkSpan "test" 1 10 1 11 :< AST.PositionedArgument False (mkSpan "test" 1 10 1 11 :< AST.Var "a")]
                            )
                            Nothing
                      ),
                  mkSpan "test" 2 1 2 12
                    :< AST.ExprStatement
                      "h"
                      ( mkSpan "test" 2 2 2 12
                          :< AST.Lambda
                            [ mkSpan "test" 2 3 2 4
                                :< AST.PositionedParameter
                                  "a"
                                  False
                                  False
                                  Nothing
                                  Nothing
                            ]
                            ( mkSpan "test" 2 8 2 12
                                :< AST.Call
                                  (mkSpan "test" 2 8 2 9 :< AST.Var "g")
                                  [mkSpan "test" 2 10 2 11 :< AST.PositionedArgument False (mkSpan "test" 2 10 2 11 :< AST.Var "a")]
                            )
                            Nothing
                      )
                ],
            [ Token TKFunction (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 6 1 7),
              Token TKVar (mkSpan "test" 1 8 1 9),
              Token TKSep (mkSpan "test" 1 9 1 10),
              Token TKVar (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 11 1 12),
              Token TKFunction (mkSpan "test" 2 1 2 2),
              Token TKSep (mkSpan "test" 2 2 2 3),
              Token TKParameter (mkSpan "test" 2 3 2 4),
              Token TKSep (mkSpan "test" 2 4 2 5),
              Token TKSep (mkSpan "test" 2 6 2 7),
              Token TKVar (mkSpan "test" 2 8 2 9),
              Token TKSep (mkSpan "test" 2 9 2 10),
              Token TKVar (mkSpan "test" 2 10 2 11),
              Token TKSep (mkSpan "test" 2 11 2 12)
            ]
          )
