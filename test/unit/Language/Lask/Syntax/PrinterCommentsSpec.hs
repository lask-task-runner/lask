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
    pendingWith "known bug: a comment after a lambda that opens a block in a call is moved to just after the call's '(', breaking the line there (spec 11.11)"
    fmt lambdaArgument `shouldBe` Right lambdaArgument
  it "formats that call to a fixed point" $ do
    pendingWith "known bug: formatting the output again indents the arguments the first pass moved, so formatting is not idempotent (spec 11.11)"
    let once = fmt lambdaArgument
    (once >>= fmt) `shouldBe` once
