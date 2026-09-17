{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Builtins.PathSpec (spec) where

import Language.Lask.Builtins.Path
import Test.Hspec

spec :: Spec
spec = do
  describe "path_join (spec 15.10)" $ do
    it "joins with a single separator" $ do
      pathJoin ["a", "b", "c"] `shouldBe` "a/b/c"
      pathJoin ["a/", "b"] `shouldBe` "a/b"
    it "drops empty parts" $ do
      pathJoin ["a", "", "b"] `shouldBe` "a/b"
      pathJoin [] `shouldBe` ""
    it "lets an absolute part discard what came before" $
      pathJoin ["a", "/etc", "hosts"] `shouldBe` "/etc/hosts"
    it "normalizes the result" $
      pathJoin ["a", "b/..", "c"] `shouldBe` "a/c"

  describe "dirname" $ do
    it "returns the parent" $ do
      dirname "a/b/c" `shouldBe` "a/b"
      dirname "/a/b" `shouldBe` "/a"
    it "returns . when there is no separator" $
      dirname "main.lask" `shouldBe` "."
    it "returns / for a child of the root" $
      dirname "/main.lask" `shouldBe` "/"
    it "ignores trailing separators" $
      dirname "a/b/" `shouldBe` "a"

  describe "basename" $ do
    it "returns the final component" $ do
      basename "a/b/c.txt" `shouldBe` "c.txt"
      basename "c.txt" `shouldBe` "c.txt"
    it "ignores trailing separators" $
      basename "a/b/" `shouldBe` "b"

  describe "extname" $ do
    it "returns the final extension with its dot" $ do
      extname "a/b/c.tar.gz" `shouldBe` ".gz"
      extname "main.lask" `shouldBe` ".lask"
    it "is empty without an extension" $ do
      extname "Makefile" `shouldBe` ""
      extname "a/b" `shouldBe` ""
    it "treats a dotfile as having no extension" $
      extname ".env" `shouldBe` ""

  describe "normalize_path" $ do
    it "collapses separators and dot components" $ do
      normalizePath "a//b/./c" `shouldBe` "a/b/c"
      normalizePath "./a" `shouldBe` "a"
    it "resolves .. lexically" $ do
      normalizePath "a/b/../c" `shouldBe` "a/c"
      normalizePath "/a/b/../../c" `shouldBe` "/c"
    it "keeps a leading .. of a relative path" $
      normalizePath "../a" `shouldBe` "../a"
    it "cannot ascend past the root" $
      normalizePath "/../a" `shouldBe` "/a"
    it "keeps the root and the current directory representable" $ do
      normalizePath "/" `shouldBe` "/"
      normalizePath "." `shouldBe` "."

  describe "is_absolute_path" $
    it "is true only for a leading separator" $ do
      isAbsolutePath "/a" `shouldBe` True
      isAbsolutePath "a" `shouldBe` False
