{-# LANGUAGE OverloadedStrings #-}

-- | The newline-significance pass on its own, over token lists written
-- out by hand. LexerSpec checks the same rules through the lexer.
module Language.Lask.Lexer.LayoutSpec (spec) where

import Language.Lask.Lexer.Layout (layout)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Span (NoSpan))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

lay :: [Token] -> [Token]
lay = map spannedValue . layout . map (Spanned NoSpan)

nl :: Token
nl = TNewline

a, b :: Token
a = TLowerId "a"
b = TLowerId "b"

-- | The newline between @a@ and @b@ is kept when the tokens around it
-- terminate the line, and dropped when they continue it.
kept, dropped :: [Token] -> [Token] -> Expectation
kept xs ys = lay (xs <> [nl] <> ys) `shouldBe` xs <> [nl] <> ys
dropped xs ys = lay (xs <> [nl] <> ys) `shouldBe` xs <> ys

-- | Tokens that layout treats in every way it can: openers, closers,
-- the tokens that continue a line, and others.
genTokens :: Gen [Token]
genTokens =
  listOf . elements $
    [nl, nl, nl, a, b, TUpperId "T", TNumber 1, TAssign, TComma, TColon, TQuestion, TArrow, TSemi]
      <> [TLParen, TRParen, TLBracket, TRBracket, TLBrace, TRBrace]
      <> map TKw [KDo, KTry, KFinally, KElse, KCatch, KCase, KIf]
      <> map TOp [minBound .. maxBound]

spec :: Spec
spec = do
  describe "Layout: terminating newlines" $ do
    it "keeps a newline between two statements" $
      kept [a] [b]

    it "collapses a run of newlines into one" $
      lay [a, nl, nl, nl, b] `shouldBe` [a, nl, b]

    it "drops the newlines at the start and end of the input" $
      lay [nl, nl, a, nl, nl] `shouldBe` [a]

    it "keeps a newline before a ( or [, which start a new statement (spec 6.5)" $ do
      kept [a] [TLParen, b, TRParen]
      kept [a] [TLBracket, b, TRBracket]

    it "keeps a newline before a unary !, which is no binary operator" $
      kept [a] [TOp OpNot, b]

  describe "Layout: continuation (spec 6.5)" $ do
    it "drops a newline after =, ',', :, ?, -> and every operator" $ do
      mapM_ (\t -> dropped [a, t] [b]) [TAssign, TComma, TColon, TQuestion, TArrow]
      mapM_ (\o -> dropped [a, TOp o] [b]) [minBound .. maxBound]

    it "drops a newline before a binary operator, ?, :, else, catch and finally" $ do
      mapM_ (\o -> dropped [a] [TOp o, b]) [o | o <- [minBound .. maxBound], isBinaryOp o]
      mapM_ (\t -> dropped [a] [t, b]) [TQuestion, TColon, TKw KElse, TKw KCatch, TKw KFinally]

    it "drops a newline after ;, after a block's { and before a }" $ do
      dropped [a, TSemi] [b]
      dropped [TKw KDo, TLBrace] [a, TRBrace]
      dropped [TKw KDo, TLBrace, a] [TRBrace]

  describe "Layout: brackets" $ do
    it "drops the newlines inside ( and [" $ do
      lay [TLParen, a, nl, b, TRParen, nl, a] `shouldBe` [TLParen, a, b, TRParen, nl, a]
      lay [TLBracket, a, nl, b, TRBracket, nl, a] `shouldBe` [TLBracket, a, b, TRBracket, nl, a]

    it "drops the newlines inside the braces of an object" $
      lay [a, TAssign, TLBrace, a, nl, b, TRBrace, nl, b]
        `shouldBe` [a, TAssign, TLBrace, a, b, TRBrace, nl, b]

    it "keeps the newlines inside a block, whose { follows do, try, finally, else, case or )" $
      mapM_
        (\opener -> lay (opener <> [TLBrace, a, nl, b, TRBrace]) `shouldBe` opener <> [TLBrace, a, nl, b, TRBrace])
        [[TKw KDo], [TKw KTry], [TKw KFinally], [TKw KElse], [TKw KCase], [TKw KIf, TLParen, a, TRParen]]

    it "keeps the newlines of a block inside parentheses" $
      lay [TLParen, TKw KDo, TLBrace, a, nl, b, TRBrace, TRParen]
        `shouldBe` [TLParen, TKw KDo, TLBrace, a, nl, b, TRBrace, TRParen]

    it "drops the newlines in parentheses inside a block" $
      lay [TKw KDo, TLBrace, TLParen, a, nl, b, TRParen, nl, a, TRBrace]
        `shouldBe` [TKw KDo, TLBrace, TLParen, a, b, TRParen, nl, a, TRBrace]

  describe "Layout: closing angles" $ do
    it "keeps a newline after a > or >> that closes a type argument list" $ do
      kept [a, TColon, TUpperId "Array", TOp OpLt, TUpperId "T", TOp OpGt] [b]
      kept [a, TColon, TUpperId "Array", TOp OpLt, TUpperId "Array", TOp OpLt, TUpperId "T", TOp OpCompR] [b]
      kept [TUpperId "Record", TOp OpLt, TOp OpGt] [b]

    it "drops a newline after a > that compares" $
      dropped [a, TOp OpGt] [b]

    it "drops a newline after >=, whose = half continues the line" $
      dropped [a, TColon, TUpperId "T", TOp OpGe] [b]

  describe "Layout: any token list" $ do
    prop "keeps every token but newlines, in order" $
      forAll genTokens $ \ts -> filter (/= nl) (lay ts) === filter (/= nl) ts

    prop "leaves no newline at either end and none next to another" $
      forAll genTokens $ \ts ->
        let out = lay ts
         in take 1 out =/= [nl] .&&. take 1 (reverse out) =/= [nl] .&&. notElem (nl, nl) (zip out (drop 1 out))

    prop "changes nothing the second time" $
      forAll genTokens $ \ts -> lay (lay ts) === lay ts
