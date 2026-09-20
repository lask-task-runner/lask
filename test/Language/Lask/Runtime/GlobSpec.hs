{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.GlobSpec (spec) where

import Language.Lask.Runtime.Glob
import Test.Hspec

spec :: Spec
spec = do
  describe "component wildcards (spec 15.11)" $ do
    it "matches * within one component" $ do
      matchGlob "*.lask" "main.lask" `shouldBe` True
      matchGlob "src/*.hs" "src/Main.hs" `shouldBe` True
    it "does not let * cross a separator" $ do
      matchGlob "*.hs" "src/Main.hs" `shouldBe` False
      matchGlob "src/*.hs" "src/a/Main.hs" `shouldBe` False
    it "matches ? as exactly one character" $ do
      matchGlob "a?.txt" "ab.txt" `shouldBe` True
      matchGlob "a?.txt" "a.txt" `shouldBe` False
      matchGlob "a?b" "a/b" `shouldBe` False

  describe "character classes" $ do
    it "matches a set and a range" $ do
      matchGlob "[abc].txt" "b.txt" `shouldBe` True
      matchGlob "[a-c].txt" "c.txt" `shouldBe` True
      matchGlob "[a-c].txt" "d.txt" `shouldBe` False
    it "negates with ! or ^" $ do
      matchGlob "[!a].txt" "b.txt" `shouldBe` True
      matchGlob "[^a].txt" "a.txt" `shouldBe` False
    it "treats an unterminated class as a literal bracket" $
      matchGlob "[ab" "[ab" `shouldBe` True

  describe "** across components (spec 15.11)" $ do
    it "matches any number of components, zero included" $ do
      matchGlob "src/**/*.lask" "src/main.lask" `shouldBe` True
      matchGlob "src/**/*.lask" "src/a/b/main.lask" `shouldBe` True
    it "matches nothing outside its root" $
      matchGlob "src/**/*.lask" "test/a/main.lask" `shouldBe` False
    it "is only meaningful as a whole component" $
      matchGlob "**" "a/b/c" `shouldBe` True

  describe "dot entries" $ do
    it "hides them from * and **" $ do
      matchGlob "*" ".env" `shouldBe` False
      matchGlob "**/*.lask" ".hidden/main.lask" `shouldBe` False
    it "reveals them to a pattern that says so" $ do
      matchGlob ".*" ".env" `shouldBe` True
      matchGlob ".git/**" ".git/config" `shouldBe` True

  describe "traversal root (globPrefix)" $ do
    it "is the leading literal components" $ do
      globPrefix "src/lib/*.lask" `shouldBe` "src/lib"
      globPrefix "src/**/*.lask" `shouldBe` "src"
    it "is empty when the first component is a pattern" $ do
      globPrefix "*.lask" `shouldBe` ""
      globPrefix "**/a.lask" `shouldBe` ""
    it "excludes the final component, which is what is being matched" $
      globPrefix "src/main.lask" `shouldBe` "src"
