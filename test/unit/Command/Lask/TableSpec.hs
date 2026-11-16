{-# LANGUAGE OverloadedStrings #-}

-- | The tables of @envs@, @deps@ and @secrets@ (spec 11.4): columns as
-- wide as their widest cell, digests to their first twelve
-- hexadecimal digits, and the content-hash directory of a dependency
-- shown as @…@.
module Command.Lask.TableSpec (spec) where

import Command.Lask.Table
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "renderTable" $ do
    it "pads each column to its widest cell, two spaces apart" $
      renderTable False ["A", "BB"] [[plain "xxx", plain "y"], [plain "z", plain "wwww"]]
        `shouldBe` ["A    BB", "xxx  y", "z    wwww"]
    it "ends no line in spaces, an empty last cell included" $
      renderTable False ["A", "B"] [[plain "x", plain ""]] `shouldSatisfy` all (\l -> T.stripEnd l == l)
    it "counts a character as one column, a dash and an ellipsis included" $
      renderTable False ["PINNED", "X"] [[plain "—", plain "a"], [plain "…/b", plain "c"]]
        `shouldBe` ["PINNED  X", "—       a", "…/b     c"]
    it "colours a toned cell only when asked, without counting the escape in its width" $ do
      renderTable False ["S", "T"] [[toned ToneOk "ok", plain "x"]] `shouldBe` ["S   T", "ok  x"]
      renderTable True ["S", "T"] [[toned ToneOk "ok", plain "x"]] `shouldBe` ["S   T", "\ESC[32mok\ESC[0m  x"]

  describe "shortDigest (spec 11.4)" $ do
    it "cuts a digest, a content hash and a recipe tag to twelve hexadecimal digits" $ do
      shortDigest "sha256:be7c8de0160a0123456789" `shouldBe` "sha256:be7c8de0160a"
      shortDigest "sha256-df8d3acd40bd0123456789" `shouldBe` "sha256-df8d3acd40bd"
      shortDigest "lask/b61fd44bd7eb0123456789" `shouldBe` "lask/b61fd44bd7eb"
    it "shows a pinned reference by its digest" $
      shortDigest "alpine:3.20@sha256:6f3c8a1e09bd0123456789" `shouldBe` "sha256:6f3c8a1e09bd"
    it "leaves anything else as it is" $ do
      shortDigest "—" `shouldBe` "—"
      shortDigest "v1.2.3" `shouldBe` "v1.2.3"

  describe "shortPath (spec 11.4)" $ do
    it "shows the content-hash directory of a cached dependency as …" $
      shortPath ".lask/deps/sha256-0123456789abcdef/lib/Dockerfile" `shouldBe` ".lask/deps/…/lib/Dockerfile"
    it "leaves a path with no such directory alone" $
      shortPath "infra/Dockerfile" `shouldBe` "infra/Dockerfile"

  describe "formatSeconds" $ do
    it "writes seconds to a tenth under a minute, and minutes and seconds from one" $ do
      formatSeconds 0.44 `shouldBe` "0.4s"
      formatSeconds 14.2 `shouldBe` "14.2s"
      formatSeconds 72 `shouldBe` "1m12s"
