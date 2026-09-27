{-# LANGUAGE OverloadedStrings #-}

-- | Session commands and @:reload@ (spec 11.9).
module Language.Lask.ReplSpec (spec) where

import qualified Data.Text.IO as TIO
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.ErrorCode
import Language.Lask.Repl
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | Run with a fresh module path in a temporary directory.
withModule :: (FilePath -> IO a) -> IO a
withModule action = withSystemTempDirectory "lask-repl" $ \dir -> action (dir </> "main.lask")

spec :: Spec
spec = do
  describe "classifyInput" $ do
    it "recognizes the commands and their short forms" $ do
      map classifyInput [":r", ":reload", " :reload ", ":q", ":quit", ":exit"]
        `shouldBe` [InputReload, InputReload, InputReload, InputQuit, InputQuit, InputQuit]

    it "rejects arguments to a command that takes none" $
      classifyInput ":r main.lask" `shouldBe` InputBadArgs ":r"

    it "rejects an unknown command instead of parsing it as code" $
      classifyInput ":load x" `shouldBe` InputUnknownCommand ":load"

    it "passes anything else through as code" $ do
      classifyInput "  1 + 2  " `shouldBe` InputCode "1 + 2"
      classifyInput "   " `shouldBe` InputBlank

  describe "reloadSession" $ do
    it "picks up the module as edited, re-applying the session's declarations" $
      withModule $ \path -> do
        let old = Session "a(): Number = 1\n" ["b(): Number = a() + 1"]
        TIO.writeFile path "a(): Number = 10\n"
        r <- reloadSession path old
        case r of
          Right s -> s `shouldBe` Session "a(): Number = 10\n" ["b(): Number = a() + 1"]
          Left _ -> expectationFailure "expected the reload to succeed"

    it "fails, naming the declaration, when the module now defines its name" $
      withModule $ \path -> do
        let old = Session "" ["b(): Number = 2", "c(): Number = 3", "d(): Number = b() + c()"]
        TIO.writeFile path "c(): Number = 30\n"
        r <- reloadSession path old
        case r of
          Left (ReloadConflict d ds) -> do
            d `shouldBe` "c(): Number = 3"
            map diagCode ds `shouldBe` [ENameDuplicate]
          _ -> expectationFailure "expected ReloadConflict"

    it "fails when a declaration's dependency no longer exists" $
      withModule $ \path -> do
        let old = Session "a(): Number = 1\n" ["b(): Number = a()"]
        TIO.writeFile path "z(): Number = 1\n"
        r <- reloadSession path old
        case r of
          Left (ReloadConflict d _) -> d `shouldBe` "b(): Number = a()"
          _ -> expectationFailure "expected ReloadConflict"

    it "fails, leaving the caller the old session, when the module does not compile" $
      withModule $ \path -> do
        TIO.writeFile path "a(): Number = \"x\"\n"
        r <- reloadSession path (Session "" [])
        case r of
          Left (ReloadInvalid ds) -> null ds `shouldBe` False
          _ -> expectationFailure "expected ReloadInvalid"

    it "fails when the module is gone" $
      withModule $ \path -> do
        r <- reloadSession path (Session "a(): Number = 1\n" [])
        case r of
          Left ReloadMissing -> pure ()
          _ -> expectationFailure "expected ReloadMissing"
