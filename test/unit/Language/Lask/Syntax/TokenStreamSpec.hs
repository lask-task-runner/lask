{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Syntax.TokenStreamSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Lexer (lexTokens)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Position (..), Span (..), toSourcePos)
import Language.Lask.Syntax.Parser (parseModule)
import Language.Lask.Syntax.TokenStream (TokStream (..), renderToken)
import Test.Hspec
import Text.Megaparsec (PosState (..), defaultTabWidth, initialPos)
import qualified Text.Megaparsec as MP

toks :: T.Text -> [Spanned Token]
toks src = either (error . show) id (lexTokens "t.lask" src)

-- | Where each of the tokens the stream starts with would be reported.
posState :: [Spanned Token] -> PosState TokStream
posState ts = PosState (TokStream ts) 0 (initialPos "t.lask") defaultTabWidth ""

pos :: Int -> Int -> MP.SourcePos
pos l c = toSourcePos (Position "t.lask" l c)

-- | The position of the first syntax error in a module.
errorAt :: T.Text -> Either String (Int, Int, T.Text)
errorAt src = case parseModule "t.lask" src of
  Left (d : _) | Span (Position _ l c) _ <- diagSpan d -> Right (l, c, diagMessage d)
  other -> Left (show other)

spec :: Spec
spec = do
  describe "TokStream as a megaparsec stream" $ do
    let ts = toks "ab cd ef"

    it "takes one token, and none from an empty stream" $ do
      fmap (fmap unTokStream) (MP.take1_ (TokStream ts)) `shouldBe` Just (ts !! 0, drop 1 ts)
      fmap (fmap unTokStream) (MP.take1_ (TokStream [])) `shouldBe` Nothing

    it "takes up to n tokens, nothing for n <= 0, and fails only on an empty stream" $ do
      fmap (fmap unTokStream) (MP.takeN_ 2 (TokStream ts)) `shouldBe` Just (take 2 ts, drop 2 ts)
      fmap (fmap unTokStream) (MP.takeN_ 5 (TokStream ts)) `shouldBe` Just (ts, [])
      fmap (fmap unTokStream) (MP.takeN_ 0 (TokStream [])) `shouldBe` Just ([], [])
      fmap (fmap unTokStream) (MP.takeN_ 1 (TokStream [])) `shouldBe` Nothing

    it "takes the tokens that satisfy a predicate" $
      fmap unTokStream (MP.takeWhile_ ((/= TLowerId "ef") . spannedValue) (TokStream ts)) `shouldBe` (take 2 ts, drop 2 ts)

  describe "TokStream positions" $ do
    let ts = toks "ab cd\n  ef"

    it "reports an offset at the start of the token there" $ do
      pstateSourcePos (snd (MP.reachOffset 0 (posState ts))) `shouldBe` pos 1 1
      pstateSourcePos (snd (MP.reachOffset 1 (posState ts))) `shouldBe` pos 1 4
      pstateSourcePos (snd (MP.reachOffset 3 (posState ts))) `shouldBe` pos 2 3

    it "reports an offset past the last token at its end" $
      pstateSourcePos (snd (MP.reachOffset (length ts) (posState ts))) `shouldBe` pos 2 5

    it "reports the starting position for an empty stream" $
      pstateSourcePos (snd (MP.reachOffset 0 (posState []))) `shouldBe` initialPos "t.lask"

    it "moves on from where it was" $ do
      let st = snd (MP.reachOffset 1 (posState ts))
      map spannedValue (unTokStream (pstateInput st)) `shouldBe` drop 1 (map spannedValue ts)
      pstateOffset st `shouldBe` 1
      pstateSourcePos (snd (MP.reachOffset 3 st)) `shouldBe` pos 2 3

    it "gives a syntax error the position of the token it is at" $ do
      errorAt "x = )" `shouldSatisfy` either (const False) (\(l, c, _) -> (l, c) == (1, 5))
      errorAt "x = 1\ny = ]" `shouldSatisfy` either (const False) (\(l, c, _) -> (l, c) == (2, 5))

    it "gives a syntax error at the end of the input the end of the last token" $
      errorAt "x =" `shouldSatisfy` either (const False) (\(l, c, _) -> (l, c) == (1, 4))

  describe "renderToken" $ do
    let fixed =
          map TOp [minBound .. maxBound]
            <> map TKw [minBound .. maxBound]
            <> [ TLParen, TRParen, TLBracket, TRBracket, TLBrace, TRBrace, TComma, TColon, TSemi, TDot,
                 TArrow, TAssign, TBackslash, TDashDash, TBangBang, TEllipsis, TPipe, TQuestion,
                 TBool True, TBool False, TNull, TLowerId "go_2", TUpperId "Pair", TEnvHead "alpine:3.12",
                 TRawString "a b", TNumber 1.5, TNumber 42, TNumber 0.001
               ]

    it "spells a token so that it lexes back to itself" $
      mapM_ (\t -> map spannedValue <$> lexTokens "t.lask" (T.pack (renderToken t)) `shouldBe` Right [t]) fixed

    it "names a token in a syntax error by its spelling" $
      errorAt "x = )" `shouldSatisfy` either (const False) (\(_, _, m) -> "unexpected )" `isInfixOf` T.unpack m)

    it "names a number in a syntax error as a literal writes it, not as a Double" $
      errorAt "42\n" `shouldSatisfy` either (const False) (\(_, _, m) -> "unexpected 42" `isInfixOf` T.unpack m && not ("42.0" `isInfixOf` T.unpack m))
