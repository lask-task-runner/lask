{-# LANGUAGE OverloadedStrings #-}

-- | The files @lask fmt@ takes from a directory, and the formatter on
-- every program the repository holds as test data: a file that parses
-- is formatted to a fixed point, as 11.11 says formatting is
-- idempotent. The modules of example/ are formatted in
-- "Language.Lask.Syntax.PrinterSpec".
module Command.Lask.FmtSpec (spec) where

import Command.Lask.Fmt (laskFilesUnder)
import Control.Monad (forM_)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask.Syntax.Printer (FormatError (..), formatSource)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Support.Pending (knownBug)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | The blocks of doc/spec.md fenced as exactly @```lask@, with the line
-- each starts on.
specBlocks :: IO [(Int, T.Text)]
specBlocks = go 1 . lines <$> readFile "doc/spec.md"
  where
    go _ [] = []
    go n (l : rest)
      | trim l == "```lask" =
          let (body, rest') = break ((== "```") . trim) rest
              indent = length (takeWhile (== ' ') l)
           in (n, T.pack (unlines (map (drop indent) body))) : go (n + length body + 2) (drop 1 rest')
      | otherwise = go (n + 1) rest
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Every file under a directory, recursively.
filesUnder :: FilePath -> IO [FilePath]
filesUnder dir = do
  names <- listDirectory dir
  concat <$> mapM (\n -> let p = dir </> n in doesDirectoryExist p >>= \d -> if d then filesUnder p else pure [p]) names

-- | Formatted twice, the same text: or a syntax error, which leaves
-- the file alone.
fixedPoint :: FilePath -> T.Text -> Expectation
fixedPoint file src = case formatSource file src of
  Left (FormatSyntax _) -> pure ()
  Left err -> expectationFailure (show err)
  Right once -> formatSource file once `shouldBe` Right once

spec :: Spec
spec = do
  describe "the files of a directory (spec 11.11)" $ do
    it "takes the .lask files, recursively, in order, skipping dot directories and links to directories" $
      withSystemTempDirectory "fmt-files" $ \dir -> do
        mapM_ (createDirectoryIfMissing True . (dir </>)) ["b", "a/deep", ".lask/deps", ".hidden", "real"]
        mapM_
          (\f -> writeFile (dir </> f) "x = 1\n")
          ["main.lask", "b/b.lask", "a/a.lask", "a/deep/d.lask", ".lask/deps/dep.lask", ".hidden/h.lask", "real/r.lask", "notes.txt", "a/x.lask.bak"]
        createDirectoryLink "real" (dir </> "link")
        files <- laskFilesUnder dir
        map (drop (length dir + 1)) files `shouldBe` ["a/a.lask", "a/deep/d.lask", "b/b.lask", "main.lask", "real/r.lask"]
    it "takes a .lask file whose own name begins with a dot, which is not a directory" $
      withSystemTempDirectory "fmt-files" $ \dir -> do
        writeFile (dir </> ".dot.lask") "x = 1\n"
        files <- laskFilesUnder dir
        knownBug "laskFilesUnder skips every name that begins with a dot, files included: a file .dot.lask is not formatted, where 11.11 leaves out only directories whose name begins with a dot" $
          map (drop (length dir + 1)) files `shouldBe` [".dot.lask"]

  describe "formatting to a fixed point (spec 11.11)" $ do
    corpus <- runIO (filter (".lask" `isSuffixOf`) <$> ((<>) <$> filesUnder "test/unit/corpus" <*> filesUnder "test/e2e/golden"))
    it "finds the corpus and the golden cases" $ corpus `shouldNotBe` []
    forM_ corpus $ \file ->
      it ("formats " <> file <> " to a fixed point") $ TIO.readFile file >>= fixedPoint file
    blocks <- runIO specBlocks
    it "finds the lask blocks of doc/spec.md" $ blocks `shouldNotBe` []
    forM_ blocks $ \(line, src) ->
      it ("formats the block at doc/spec.md:" <> show line <> " to a fixed point") $ fixedPoint "spec.lask" src
