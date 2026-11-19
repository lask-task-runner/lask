{-# LANGUAGE OverloadedStrings #-}

-- | Documentation comments (spec 3.1): the block above a declaration
-- and the tags parsed out of it.
module Language.Lask.DocSpec (spec) where

import Data.Text (Text)
import Language.Lask.Doc
import Language.Lask.Lexer (lexTokensWithComments)
import Test.Hspec

-- | The documentation block for the declaration starting on @line@.
blockAt :: Text -> Int -> Maybe Text
blockAt src line = case lexTokensWithComments "test.lask" src of
  Right (_, comments) -> docBlockAbove src comments line
  Left _ -> Nothing

spec :: Spec
spec = do
  describe "docBlockAbove" $ do
    it "takes the contiguous line comments directly above the declaration" $ do
      let src = "// first\n// second\nbuild() = 1\n"
      blockAt src 3 `shouldBe` Just "first\nsecond"

    it "stops at a blank line" $ do
      let src = "// unrelated\n\n// attached\nbuild() = 1\n"
      blockAt src 4 `shouldBe` Just "attached"

    it "stops at a line of code" $ do
      let src = "// on other\nother() = 1\nbuild() = 1\n"
      blockAt src 3 `shouldBe` Nothing

    it "returns nothing when there is no comment above" $
      blockAt "build() = 1\n" 1 `shouldBe` Nothing

    it "strips block comment markers, including multi-line ones" $ do
      let src = "/* first\n   second */\nbuild() = 1\n"
      blockAt src 3 `shouldBe` Just "first\n   second"

  describe "parseDoc" $ do
    it "takes the first paragraph as the summary, collapsed to one line" $ do
      let doc = parseDoc "Build the\nproject.\n\nMore prose."
      docSummary doc `shouldBe` Just "Build the project."
      docDescription doc `shouldBe` Just "More prose."

    it "has no description when the comment is a summary only" $ do
      let doc = parseDoc "Build the project."
      docSummary doc `shouldBe` Just "Build the project."
      docDescription doc `shouldBe` Nothing

    it "ends the description at the first tag line" $ do
      let doc = parseDoc "Summary.\n\nProse.\n\n@param a first"
      docDescription doc `shouldBe` Just "Prose."
      docParams doc `shouldBe` [("a", "first")]

    it "maps kebab-case parameter names to snake_case (spec 11.2)" $
      docParams (parseDoc "S.\n\n@param out-dir where to write")
        `shouldBe` [("out_dir", "where to write")]

    it "keeps parameters in order of appearance" $
      docParams (parseDoc "S.\n\n@param a one\n@param b two")
        `shouldBe` [("a", "one"), ("b", "two")]

    it "continues a tag on the following unprefixed lines" $
      docParams (parseDoc "S.\n\n@param a first\n  and more\n@param b two")
        `shouldBe` [("a", "first\n  and more"), ("b", "two")]

    it "collects every @example in order" $
      docExamples (parseDoc "S.\n\n@example lask run a\n@example lask run b")
        `shouldBe` ["lask run a", "lask run b"]

    it "reads @return and @hidden" $ do
      let doc = parseDoc "S.\n\n@return the path\n@hidden"
      docReturn doc `shouldBe` Just "the path"
      docHidden doc `shouldBe` True

    it "is not hidden without the tag" $
      docHidden (parseDoc "S.") `shouldBe` False

    it "carries an unknown tag through into the description" $ do
      let doc = parseDoc "Summary.\n\nProse.\n\n@since 0.3.0"
      docDescription doc `shouldBe` Just "Prose.\n\n@since 0.3.0"

    it "yields an empty doc for empty text" $
      parseDoc "" `shouldBe` emptyDoc
