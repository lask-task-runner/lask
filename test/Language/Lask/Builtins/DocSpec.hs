{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Builtins.DocSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Lask.Builtins.Doc
import Language.Lask.Builtins.Names (builtinValueNames)
import Language.Lask.Builtins.Sig (builtinSchemes)
import Test.Hspec

spec :: Spec
spec = do
  describe "builtinDocs" $ do
    it "documents every builtin value" $
      Set.toList (Set.insert "stdin" (Map.keysSet builtinSchemes) `Set.difference` Map.keysSet builtinDocs)
        `shouldBe` []
    it "documents nothing that is not a builtin" $
      Set.toList (Map.keysSet builtinDocs `Set.difference` Set.insert "stdin" builtinValueNames)
        `shouldBe` []
    it "gives every entry a body" $
      [n | (n, d) <- Map.toList builtinDocs, null (bdParagraphs d)] `shouldBe` []
