{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Builtins.FormatSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Language.Lask.Builtins.Format
import Language.Lask.ErrorCode
import Language.Lask.Runtime.Value
import Test.Hspec

-- | A decoded object: every format uses the representation
-- 'valueFromJson' picks for a JSON object (spec 15.8).
vmap :: [(Text, Value)] -> Value
vmap = VRecord . Map.fromList

varr :: [Value] -> Value
varr = VArray . V.fromList

codeOf :: Either LaskFailure a -> Maybe ErrorCode
codeOf (Left lf) = lfCode lf
codeOf _ = Nothing

spec :: Spec
spec = do
  describe "yaml (spec 15.8)" $ do
    it "decodes scalars by the same rules as JSON" $
      decodeFormat "yaml" "name: lask\nport: 8080\ndebug: true\n"
        `shouldBe` Right (vmap [("name", VString "lask"), ("port", VNumber 8080), ("debug", VBool True)])
    it "decodes nested structure" $
      decodeFormat "yaml" "outer:\n  inner:\n    - 1\n    - 2\n"
        `shouldBe` Right (vmap [("outer", vmap [("inner", varr [VNumber 1, VNumber 2])])])
    it "round-trips through encode" $ do
      let v = vmap [("a", VString "x"), ("b", VNumber 2)]
      (encodeFormat "yaml" v >>= decodeFormat "yaml") `shouldBe` Right v
    it "reports malformed input as E-IO-DATA-DECODE" $
      codeOf (decodeFormat "yaml" "a:\n- b\n  c: d\n") `shouldBe` Just EIoDataDecode

  describe "toml" $ do
    it "decodes a table" $
      decodeFormat "toml" "name = \"lask\"\nport = 8080\n"
        `shouldBe` Right (vmap [("name", VString "lask"), ("port", VNumber 8080)])
    it "decodes a nested table and an array" $
      decodeFormat "toml" "[server]\nhosts = [\"a\", \"b\"]\n"
        `shouldBe` Right (vmap [("server", vmap [("hosts", varr [VString "a", VString "b"])])])
    it "round-trips through encode" $ do
      let v = vmap [("name", VString "lask"), ("port", VNumber 8080)]
      (encodeFormat "toml" v >>= decodeFormat "toml") `shouldBe` Right v
    it "refuses a top level that is not an object" $
      codeOf (encodeFormat "toml" (varr [VNumber 1])) `shouldBe` Just ERuntimeValue
    it "reports malformed input as E-IO-DATA-DECODE" $
      codeOf (decodeFormat "toml" "= 1\n") `shouldBe` Just EIoDataDecode

  describe "csv" $ do
    it "uses the first row as the header and every value as a String" $
      decodeFormat "csv" "name,port\nlask,8080\n"
        `shouldBe` Right (varr [vmap [("name", VString "lask"), ("port", VString "8080")]])
    it "honours quoted cells containing separators and newlines" $
      decodeFormat "csv" "a,b\n\"x,y\",\"line1\nline2\"\n"
        `shouldBe` Right (varr [vmap [("a", VString "x,y"), ("b", VString "line1\nline2")]])
    it "unescapes a doubled quote" $
      decodeFormat "csv" "a\n\"say \"\"hi\"\"\"\n"
        `shouldBe` Right (varr [vmap [("a", VString "say \"hi\"")]])
    it "encodes with a header and quotes what needs it" $
      encodeFormat "csv" (varr [vmap [("a", VString "x,y"), ("b", VNumber 1)]])
        `shouldBe` Right "a,b\n\"x,y\",1\n"
    it "refuses rows whose fields disagree" $
      codeOf (encodeFormat "csv" (varr [vmap [("a", VString "1")], vmap [("b", VString "2")]]))
        `shouldBe` Just ERuntimeValue
    it "decodes empty input as an empty array" $
      decodeFormat "csv" "" `shouldBe` Right (varr [])

  describe "dotenv" $ do
    it "decodes NAME=value lines" $
      decodeFormat "dotenv" "A=1\nB=two\n"
        `shouldBe` Right (vmap [("A", VString "1"), ("B", VString "two")])
    it "ignores blank lines and comments" $
      decodeFormat "dotenv" "\n# a comment\nA=1\n"
        `shouldBe` Right (vmap [("A", VString "1")])
    it "strips one layer of quoting" $
      decodeFormat "dotenv" "A=\"a b\"\nB='c d'\n"
        `shouldBe` Right (vmap [("A", VString "a b"), ("B", VString "c d")])
    it "keeps a value containing = intact" $
      decodeFormat "dotenv" "URL=postgres://h/db?a=b\n"
        `shouldBe` Right (vmap [("URL", VString "postgres://h/db?a=b")])
    it "encodes, quoting a value that needs it" $
      encodeFormat "dotenv" (vmap [("A", VString "a b"), ("B", VString "c")])
        `shouldBe` Right "A='a b'\nB=c\n"
    it "refuses a non-string value" $
      codeOf (encodeFormat "dotenv" (vmap [("A", VNumber 1)])) `shouldBe` Just ERuntimeValue

  describe "unsupported formats" $
    it "are rejected by both directions" $ do
      encodeFormat "xml" (vmap []) `shouldSatisfy` isLeft
      codeOf (decodeFormat "xml" "<a/>") `shouldBe` Just EIoDataDecode
