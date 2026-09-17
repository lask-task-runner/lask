{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Builtins.RegexSpec (spec) where

import Data.Either (isLeft)

import Data.Text (Text)
import Language.Lask.Builtins.Regex
import Test.Hspec

re :: Text -> Regex
re pat = case compile pat of
  Right r -> r
  Left e -> error (show e)

spec :: Spec
spec = do
  describe "regex_test (spec 15.3)" $ do
    it "matches anywhere in the subject" $ do
      test (re "b+") "aabbcc" `shouldBe` True
      test (re "^a") "aabbcc" `shouldBe` True
      test (re "^b") "aabbcc" `shouldBe` False

  describe "regex_match" $ do
    it "returns the whole match followed by each group" $
      matchGroups (re "v([0-9]+)\\.([0-9]+)") "release v1.22.4"
        `shouldBe` ["v1.22", "1", "22"]
    it "returns an empty array when there is no match" $
      matchGroups (re "z+") "abc" `shouldBe` []

  describe "regex_replace" $ do
    it "replaces every match" $
      replaceAll (re "a") "-" "banana" `shouldBe` "b-n-n-"
    it "expands group references" $
      replaceAll (re "([a-z]+)=([a-z]+)") "$2=$1" "k=v" `shouldBe` "v=k"
    it "expands $$ as a literal dollar" $
      replaceAll (re "x") "$$" "x" `shouldBe` "$"
    it "leaves a subject with no match alone" $
      replaceAll (re "z") "-" "abc" `shouldBe` "abc"

  describe "the escapes POSIX lacks (translate)" $ do
    it "rewrites \\d, \\w and \\s into the classes they stand for" $ do
      translate "\\d+" `shouldBe` Right "[0-9]+"
      translate "\\w" `shouldBe` Right "[0-9A-Za-z_]"
      translate "a\\sb" `shouldBe` Right "a[ \t\n\r\f\v]b"
    it "negates them" $
      translate "\\D" `shouldBe` Right "[^0-9]"
    it "matches with them" $ do
      matchGroups (re "(\\d+)") "abc 42" `shouldBe` ["42", "42"]
      test (re "\\s") "a b" `shouldBe` True
    it "expands them inside a character class" $
      translate "[\\d_]" `shouldBe` Right "[0-9_]"
    it "rejects a negated escape inside a character class" $
      translate "[\\D]" `shouldSatisfy` isLeft
    it "passes an escaped metacharacter through" $
      translate "a\\.b" `shouldBe` Right "a\\.b"
    it "keeps a leading ] and ^ literal inside a class" $ do
      translate "[]]" `shouldBe` Right "[]]"
      translate "[^a]" `shouldBe` Right "[^a]"

  describe "malformed patterns" $ do
    it "are rejected rather than matched" $ do
      rejects "["
      rejects "a\\"
      rejects "("
  where
    -- Regex has no Show instance, so a rejection is tested on the tag.
    rejects pat = either (const True) (const False) (compile pat) `shouldBe` True
