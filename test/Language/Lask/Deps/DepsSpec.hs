{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Deps.DepsSpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Lask.Deps.File
import Language.Lask.Deps.Hash
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "dependency definition file (spec chapter 5)" $ do
    it "parses git and url entries" $ do
      let json =
            "{\"dependencies\": {\
            \\"deploy_kit\": {\"git\": \"https://example.com/kit\", \"rev\": \"v1.2.0\", \"hash\": \"sha256-aa\"},\
            \\"notify\": {\"url\": \"https://example.com/notify.lask\", \"hash\": \"sha256-bb\"}}}"
      parseDepsFile (BL8.pack json)
        `shouldBe` Right
          ( DepsFile . Map.fromList $
              [ ("deploy_kit", DepGit "https://example.com/kit" "v1.2.0"),
                ("notify", DepUrl "https://example.com/notify.lask")
              ]
          )
    -- The project file records intent; the hash that pins it lives in
    -- the lock (spec chapter 5).
    it "does not require a hash" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"url\": \"https://x/a.lask\"}}}"
        `shouldBe` Right (DepsFile (Map.fromList [("a", DepUrl "https://x/a.lask")]))
    it "still accepts a hash written by an older project file" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"url\": \"https://x/a.lask\", \"hash\": \"sha256-aa\"}}}"
        `shouldBe` Right (DepsFile (Map.fromList [("a", DepUrl "https://x/a.lask")]))
    it "requires rev with git" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"git\": \"https://x/r\", \"hash\": \"sha256-aa\"}}}"
        `shouldSatisfy` isLeft
    it "rejects rev with url" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"url\": \"https://x/a.lask\", \"rev\": \"v1\", \"hash\": \"sha256-aa\"}}}"
        `shouldSatisfy` isLeft
    it "rejects entries with both git and url" $
      parseDepsFile
        "{\"dependencies\": {\"a\": {\"git\": \"https://x/r\", \"rev\": \"v1\", \"url\": \"https://x/a.lask\", \"hash\": \"sha256-aa\"}}}"
        `shouldSatisfy` isLeft
    it "rejects entries with no source" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"hash\": \"sha256-aa\"}}}"
        `shouldSatisfy` isLeft
    it "rejects unknown keys" $
      parseDepsFile "{\"dependencies\": {\"a\": {\"url\": \"https://x/a.lask\", \"hash\": \"sha256-aa\", \"token\": \"s\"}}}"
        `shouldSatisfy` isLeft
    it "rejects non-identifier dependency names" $
      parseDepsFile "{\"dependencies\": {\"Bad-Name\": {\"url\": \"https://x/a.lask\", \"hash\": \"sha256-aa\"}}}"
        `shouldSatisfy` isLeft
    it "round-trips through render" $ do
      let df =
            DepsFile . Map.fromList $
              [ ("kit", DepGit "https://example.com/kit" "abc123"),
                ("notify", DepUrl "https://example.com/notify.lask")
              ]
      parseDepsFile (renderDepsFile df) `shouldBe` Right df
    it "classifies single-file and tree sources" $ do
      entryIsSingleFile (DepUrl "https://x/notify.lask") `shouldBe` True
      entryIsSingleFile (DepUrl "https://x/kit.tar.gz") `shouldBe` False
      entryIsSingleFile (DepGit "https://x/r" "v1") `shouldBe` False

  describe "content hashes (spec chapter 5)" $ do
    it "hashes bytes in the sha256-hex format" $ do
      let h = hashBytes "hello"
      T.take 7 h `shouldBe` "sha256-"
      T.length h `shouldBe` 7 + 64
    it "is deterministic for files" $
      withSystemTempDirectory "lask-hash" $ \dir -> do
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        h1 <- hashFile (dir </> "a.lask")
        h2 <- hashFile (dir </> "a.lask")
        h1 `shouldBe` h2
        h1 `shouldBe` hashBytes "a = 1\n"
    it "hashes trees independently of creation order" $ do
      h1 <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "b.lask") "b = 2\n"
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        createDirectoryIfMissing True (dir </> "sub")
        BS8.writeFile (dir </> "sub" </> "c.lask") "c = 3\n"
        hashTree dir
      h2 <- withSystemTempDirectory "lask-tree" $ \dir -> do
        createDirectoryIfMissing True (dir </> "sub")
        BS8.writeFile (dir </> "sub" </> "c.lask") "c = 3\n"
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        BS8.writeFile (dir </> "b.lask") "b = 2\n"
        hashTree dir
      h1 `shouldBe` h2
    it "changes when content or paths change" $ do
      base <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        hashTree dir
      changedContent <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "a.lask") "a = 2\n"
        hashTree dir
      changedPath <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "b.lask") "a = 1\n"
        hashTree dir
      base `shouldNotBe` changedContent
      base `shouldNotBe` changedPath
    it "ignores .git directories" $ do
      h1 <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        hashTree dir
      h2 <- withSystemTempDirectory "lask-tree" $ \dir -> do
        BS8.writeFile (dir </> "a.lask") "a = 1\n"
        createDirectoryIfMissing True (dir </> ".git")
        BS8.writeFile (dir </> ".git" </> "HEAD") "ref: refs/heads/main\n"
        hashTree dir
      h1 `shouldBe` h2
