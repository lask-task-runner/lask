{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.LaskSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Lask (hoverAt, lexSemanticTokens, uriPath)
import Language.LSP.Protocol.Types
import Language.Lask (checkText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | (line, startChar, length, type), all 0-based as in the protocol.
type Atom = (Int, Int, Int, SemanticTokenTypes)

toks :: Text -> [Atom]
toks src = case lexSemanticTokens "test.lask" src of
  Right ts ->
    [ (fromIntegral l, fromIntegral c, fromIntegral len, typ)
    | SemanticTokenAbsolute l c len typ _ <- ts
    ]
  Left e -> error (show e)

hasAtom :: Text -> Atom -> Expectation
hasAtom src atom = toks src `shouldSatisfy` elem atom

spec :: Spec
spec = do
  describe "comments" $ do
    it "emits comment tokens for line comments" $
      -- "// hi" occupies columns 0-4 on line 0.
      hasAtom "// hi\na = 1" (0, 0, 5, SemanticTokenTypes_Comment)
    it "emits comment tokens for each line of a block comment" $ do
      let src = "/* a\nb */\nx = 1"
      hasAtom src (0, 0, 4, SemanticTokenTypes_Comment)
      hasAtom src (1, 0, 4, SemanticTokenTypes_Comment)
    it "keeps surrounding code tokens intact" $ do
      let src = "a = 1 // trailing"
      hasAtom src (0, 0, 1, SemanticTokenTypes_Variable)
      hasAtom src (0, 6, 11, SemanticTokenTypes_Comment)

  describe "string interpolation" $ do
    -- s = "Hi, #{name}!"
    let src = "s = \"Hi, #{name}!\""
    it "highlights the interpolated expression" $
      hasAtom src (0, 11, 4, SemanticTokenTypes_Variable)
    it "keeps the string segments around the interpolation" $ do
      -- Opening segment: from the quote up to the nested tokens.
      toks src
        `shouldSatisfy` any
          (\(l, c, _, typ) -> l == 0 && c == 4 && typ == SemanticTokenTypes_String)
      -- Closing segment ends at the closing quote.
      toks src
        `shouldSatisfy` any
          (\(_, c, _, typ) -> c > 11 && typ == SemanticTokenTypes_String)
    it "highlights nested expressions recursively" $
      -- x + 1 inside the interpolation
      hasAtom "s = \"v=#{x + 1}\"" (0, 11, 1, SemanticTokenTypes_Operator)

  describe "command expressions" $ do
    it "marks $ as a function token" $
      -- f() = $ echo hi
      hasAtom "f() = $ echo hi" (0, 6, 1, SemanticTokenTypes_Function)
    it "marks stream selectors as part of the function head" $ do
      hasAtom "f() = $* ls" (0, 6, 2, SemanticTokenTypes_Function)
      hasAtom "f() = $2 ls" (0, 6, 2, SemanticTokenTypes_Function)
    it "renders the command text as string segments" $
      toks "f() = $ echo hi"
        `shouldSatisfy` any
          (\(_, c, _, typ) -> c > 6 && typ == SemanticTokenTypes_String)
    it "highlights interpolation inside commands" $
      -- f() = $ echo #{msg}
      hasAtom "f() = $ echo #{msg}" (0, 15, 3, SemanticTokenTypes_Variable)
    it "highlights the environment expression in commands" $
      -- f() = $[#alpine:3.20] ls   (env head starts at col 8, length 12)
      hasAtom "f() = $[#alpine:3.20] ls" (0, 8, 12, SemanticTokenTypes_Macro)

  describe "plain tokens" $ do
    it "still maps keywords, types, numbers" $ do
      let src = "add(x: Number): Number = x + 1"
      hasAtom src (0, 7, 6, SemanticTokenTypes_Type)
      hasAtom src (0, 29, 1, SemanticTokenTypes_Number)

  describe "hover" $ do
    let src =
          "// Adds one.\n\
          \// Second line.\n\
          \inc(x: Number): Number = x + 1\n\
          \y = inc(2)\n"
        hoverText l c = do
          h <- hoverAt "test.lask" src (Position l c)
          pure $ case h of
            Just (Hover (InL (MarkupContent _ t)) _) -> Just t
            _ -> Nothing
    it "shows the type of a referenced function" $ do
      t <- hoverText 3 4 -- `inc` in `y = inc(2)`
      t `shouldSatisfy` maybe False (T.isInfixOf "inc: Function<Number, Number>")
    it "shows the comment block above the declaration" $ do
      t <- hoverText 3 4
      t `shouldSatisfy` maybe False (T.isInfixOf "Adds one.\nSecond line.")
    it "shows type and docs on the declaration name itself" $ do
      t <- hoverText 2 0 -- `inc` at its definition
      t `shouldSatisfy` maybe False (T.isInfixOf "inc: Function<Number, Number>")
      t `shouldSatisfy` maybe False (T.isInfixOf "Adds one.")
    it "shows the type of local parameter references" $ do
      t <- hoverText 2 25 -- trailing `x` in the body
      t `shouldSatisfy` maybe False (T.isInfixOf "x: Number")
    it "returns nothing on blank positions" $ do
      t <- hoverText 3 1 -- whitespace after `y`
      t `shouldBe` Nothing
    it "shows types of imported symbols with their docs" $
      withSystemTempDirectory "lask-hover" $ \dir -> do
        writeFile (dir </> "lib.lask") "// Doubles a number.\ndouble(n: Number): Number = n * 2\n"
        let docSrc = "import { double } from \"lib.lask\"\nf() = double(3)\n"
        h <- hoverAt (dir </> "main.lask") docSrc (Position 1 6) -- `double` reference
        let t = case h of
              Just (Hover (InL (MarkupContent _ x)) _) -> Just x
              _ -> Nothing
        t `shouldSatisfy` maybe False (T.isInfixOf "double: Function<Number, Number>")
        t `shouldSatisfy` maybe False (T.isInfixOf "Doubles a number.")

  describe "document paths" $ do
    it "strips the file:// scheme from document URIs" $
      uriPath (Uri "file:///tmp/proj/main.lask") `shouldBe` "/tmp/proj/main.lask"
    it "resolves imports relative to the document's directory" $
      withSystemTempDirectory "lask-lsp" $ \dir -> do
        writeFile (dir </> "a.lask") "a = 1\n"
        let docUri = Uri ("file://" <> T.pack (dir </> "main.lask"))
        ds <- checkText (uriPath docUri) "import { a } from \"a.lask\"\nx = a"
        ds `shouldBe` []
