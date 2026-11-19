{-# LANGUAGE OverloadedStrings #-}

-- | Where the formatter puts a comment it moves (spec 11.11), and that
-- formatting what it wrote changes nothing.
module Language.Lask.Syntax.PrinterCommentsSpec (spec) where

import Data.Text (Text)
import Language.Lask.Syntax.Printer (formatSource)
import Test.Hspec

fmt :: Text -> Either String Text
fmt src = either (const (Left "does not parse")) Right (formatSource "main.lask" src)

-- | A call whose last argument is a lambda opening a block, with a
-- comment after the brace.
lambdaArgument :: Text
lambdaArgument = "f() = g(1, \\(x) -> do {  // note\n  x\n})\n"

spec :: Spec
spec = describe "comments the formatter moves (spec 11.11)" $ do
  it "keeps a trailing comment on the line of the lambda it follows" $ do
    fmt lambdaArgument `shouldBe` Right lambdaArgument
  it "formats that call to a fixed point" $ do
    let once = fmt lambdaArgument
    (once >>= fmt) `shouldBe` once
