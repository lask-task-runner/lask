{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Syntax.PrinterSpec (spec) where

import Command.Lask.Fmt (laskFilesUnder)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask.Syntax.Printer (FormatError (..), formatSource)
import Test.Hspec

fmt :: [Text] -> Either FormatError Text
fmt = formatSource "test.lask" . T.unlines

-- | The source is formatted to @expected@, which is formatted already.
formatsTo :: [Text] -> [Text] -> Expectation
formatsTo src expected = do
  fmt src `shouldBe` Right (T.unlines expected)
  fmt expected `shouldBe` Right (T.unlines expected)

-- | The source is formatted already.
unchanged :: [Text] -> Expectation
unchanged src = src `formatsTo` src

spec :: Spec
spec = do
  describe "spacing" $ do
    it "normalizes the space between tokens" $
      [ "f(  x :String,--y:Number=1 ):String=x",
        "z=[1,2 ,3]",
        "r : Record<a:String, b ? :Number> = {a:\"x\" , \"b-c\" : 2}"
      ]
        `formatsTo` [ "f(x: String, --y: Number = 1): String = x",
                      "z = [1, 2, 3]",
                      "r: Record<a: String, b?: Number> = { a: \"x\", \"b-c\": 2 }"
                    ]

    it "pads the braces of record literals, imports and command words, and not of annotation options" $
      [ "import {a,b as c} from \"./lib\"",
        "command {\"go\"} on #golang:1.25{env:{}}",
        "o = {k:1}"
      ]
        `formatsTo` [ "import { a, b as c } from \"./lib\"",
                      "command { \"go\" } on #golang:1.25{env: {}}",
                      "o = { k: 1 }"
                    ]

    it "keeps the export and internal markers" $
      unchanged
        [ "export x = 1",
          "internal y = 2",
          "internal command { \"go\" } on #local",
          "export command { \"npm\" } on #local",
          "export { a } from \"./lib\""
        ]

    it "keeps how literals are spelt" $
      unchanged ["x = ['raw \\n', \"a\\tb #{1.50}\", 1.50, 007]"]

    it "keeps parentheses, and never writes !! for two !" $
      [ "a = (1 + 2) * ((3))",
        "b = ! !true",
        "c = (f)(1).x"
      ]
        `formatsTo` [ "a = (1 + 2) * ((3))",
                      "b = ! !true",
                      "c = (f)(1).x"
                    ]

  describe "line breaks" $ do
    it "puts every statement of a block on its own line" $
      ["f() = do { a = 1; b = 2;  a }"]
        `formatsTo` ["f() = do {", "  a = 1", "  b = 2", "  a", "}"]

    it "keeps a one-statement block on one line when the source does" $
      unchanged ["f(x: Bool) = if (x) { 1 } else { 2 }"]

    it "joins else, catch and finally to the closing brace" $
      [ "f(x: Bool) = if (x) {",
        "  1",
        "}",
        "else {",
        "  2",
        "}"
      ]
        `formatsTo` ["f(x: Bool) = if (x) {", "  1", "} else {", "  2", "}"]

    it "breaks a list where the source breaks it" $
      unchanged
        [ "x = run(image,",
          "  env = {},",
          "  init = true",
          ")",
          "y = [",
          "  1, 2,",
          "  3",
          "]",
          "z = f(1, do {",
          "  2",
          "})"
        ]

    it "indents a list's items" $
      [ "x = [",
        "1,",
        "        2]"
      ]
        `formatsTo` ["x = [", "  1,", "  2]"]

    it "keeps a break after = and around an operator" $
      unchanged
        [ "x =",
          "  1",
          "y = a",
          "  |> b",
          "  |> c",
          "z(): String =",
          "  a",
          "  |> b",
          "w = c",
          "  ? 1",
          "  : 2"
        ]

    it "leaves a pipeline that starts a statement where it starts" $
      unchanged
        [ "f() = do {",
          "  xs",
          "  |> sort",
          "  |> \\(ys: Array<String>) -> map(ys, g)",
          "  |> unique",
          "}"
        ]

    it "keeps one blank line of several" $
      ["a = 1", "", "", "", "b = 2", "c = 3"] `formatsTo` ["a = 1", "", "b = 2", "c = 3"]

    it "keeps the case arms the source lines up lined up, and lines up no others" $ do
      unchanged
        [ "f(x: Any) = case (x) {",
          "  Null   -> 1",
          "  String -> 2",
          "  else   -> 3",
          "}"
        ]
      formatsTo
        [ "f(x: Any) = case (x) {",
          "  Null -> 1",
          "  String  -> 2",
          "}"
        ]
        ["f(x: Any) = case (x) {", "  Null -> 1", "  String -> 2", "}"]

  describe "comments" $ do
    it "keeps comments above and beside declarations, statements and items" $
      unchanged
        [ "// header",
          "",
          "// doc",
          "// @param x  the x",
          "f(x: Number) = do { // the body",
          "  // before",
          "  y = x // beside",
          "  /* block */",
          "  [",
          "    1, // one",
          "    2  // two",
          "    // the end",
          "  ]",
          "}",
          "// trailing"
        ]

    it "keeps the space before a comment that ends a line" $
      unchanged
        [ "f() = do {",
          "  a = 1      // one",
          "  bb = 22    // two",
          "  a",
          "}"
        ]

    it "copies an item whose comment has no place of its own as written" $
      [ "x  = // why",
        "    1",
        "y  =  2"
      ]
        `formatsTo` ["x  = // why", "    1", "y = 2"]

  describe "commands" $ do
    it "copies a command expression as written" $
      unchanged ["f() = $[#local]   echo  'a  b'   #{1}  // not a comment"]

    it "keeps a command's line to itself" $
      unchanged ["xs = [", "  $ ls", "]"]

  describe "errors" $ do
    it "reports a syntax error instead of formatting" $
      fmt ["x = "] `shouldSatisfy` isSyntax

  describe "the repository's own modules" $ do
    files <- runIO ((<>) <$> laskFilesUnder "example" <*> pure ["main.lask"])
    it "are found" $ files `shouldNotBe` []
    forM_ files $ \file ->
      it ("formats " <> file <> " to a fixed point") $ do
        src <- TIO.readFile file
        case formatSource file src of
          Left err -> expectationFailure (show err)
          Right once -> formatSource file once `shouldBe` Right once
  where
    isSyntax (Left (FormatSyntax _)) = True
    isSyntax _ = False
