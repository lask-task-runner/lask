{-# LANGUAGE OverloadedStrings #-}

-- | Properties of the lexer, the parser and the formatter over
-- generated modules ("Language.Lask.Syntax.Gen"), and over text that is
-- not quite a program.
module Language.Lask.Syntax.PropertiesSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.ErrorCode (Stage (StageSyntax))
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Syntax.AST (stripSpansModule)
import Language.Lask.Syntax.Parser (parseModule)
import Language.Lask.Syntax.Printer (FormatError (..), formatSource)
import Support.SyntaxGen
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | How long the lexer, the parser and the formatter may take over one
-- input, in microseconds. Each takes milliseconds; this only bounds a
-- loop.
limit :: Int
limit = 2000000

-- | The lexer and the parser answer, in time: with a module, or with
-- syntax diagnostics. The formatter answers too, and a module that
-- parses it formats, to a fixed point.
answers :: Text -> Property
answers src =
  within limit . counterexample (show src) . label (either (const "a syntax error") (const "a module") (parseModule "noise.lask" src)) $
    lexed .&&. case parseModule "noise.lask" src of
      Left ds ->
        counterexample (show ds) (not (null ds) && all ((== StageSyntax) . diagStage) ds)
          .&&. case formatSource "noise.lask" src of
            Left (FormatSyntax _) -> property True
            other -> counterexample ("formatted a module that does not parse: " <> show other) False
      Right m ->
        forced m .&&. case formatSource "noise.lask" src of
          Right out -> formatSource "noise.lask" out === Right out
          Left err -> counterexample (show err) False
  where
    lexed = either (forced . diagMessage) forced (lexTokensWithComments "noise.lask" src)
    forced :: (Show a) => a -> Property
    forced x = property (length (show x) >= 0)

spec :: Spec
spec = do
  describe "a generated module" $ do
    prop "parses back from its printed source" $
      forAllShrinkShow genModule shrinkModule (T.unpack . renderModule) $ \m ->
        (stripSpansModule <$> parseModule "gen.lask" (renderModule m)) === Right m

    prop "is formatted to a source that parses back to it" $
      forAllShrinkShow genModule shrinkModule (T.unpack . renderModule) $ \m ->
        case formatSource "gen.lask" (renderModule m) of
          Right out -> counterexample (T.unpack out) ((stripSpansModule <$> parseModule "gen.lask" out) === Right m)
          Left err -> counterexample (show err) False

    -- `lask fmt` writes what formatSource returns (Command.Lask.Fmt).
    prop "is formatted to a fixed point" $
      forAllShrinkShow genModule shrinkModule (T.unpack . renderModule) $ \m ->
        case formatSource "gen.lask" (renderModule m) of
          Right out -> counterexample (T.unpack out) (formatSource "gen.lask" out === Right out)
          Left err -> counterexample (show err) False

  describe "text that is not quite a program" $ do
    prop "is answered by the lexer, the parser and the formatter: arbitrary text" $
      forAll genNoise answers

    prop "is answered by the lexer, the parser and the formatter: a generated module with tokens deleted, duplicated, swapped or put in" $
      forAll (genModule >>= genMutation . renderModule) answers

  describe "a unicode escape (spec 3)" $ do
    let rejected src = parseModule "t.lask" src `shouldSatisfy` either (const True) (const False)
    it "answers one whose code point is past any machine integer" $
      answers "x = \"\\u{8000000000000000}\""
    it "rejects one whose code point would wrap around an Int back into range" $
      rejected "x = \"\\u{10000000000000041}\""
    it "rejects one past 10FFFF, and a surrogate" $ do
      rejected "x = \"\\u{110000}\""
      rejected "x = \"\\u{D800}\""
      rejected "x = \"\\u{DFFF}\""
    it "reads the last character before the surrogates and the first after" $ do
      parseModule "t.lask" "x = \"\\u{D7FF}\\u{E000}\\u{10FFFF}\"" `shouldSatisfy` either (const False) (const True)

  -- What the properties found, each with its smallest counterexample.
  -- Pending until fixed; drop the knownBug to run one.
  describe "known bugs" $ do
    -- Shrunk from `internal f = { "Foo": '"😀', "Foo": null }`. The
    -- printer copies a literal by its span, and finds a column in a
    -- line by counting characters, but megaparsec 9.7 counts a wide
    -- character as two columns: everything after one on its line is
    -- sliced in the wrong place. A literal that ends its line is not
    -- affected, nor is a comment or a command, copied to the line end.
    knownBug "Printer.colIndex counts a wide character as one column, megaparsec as two" $
      it "formats a wide character in a literal that more of the line follows" $
        formatSource "t.lask" "x = ['日']\n" `shouldBe` Right "x = ['日']\n"

  where
    knownBug = before_ . pendingWith
