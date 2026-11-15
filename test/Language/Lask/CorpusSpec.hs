{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Programs under @test/corpus@ whose diagnostics are written in the
-- program itself, checked in process by the front end that
-- @lask check@ runs.
--
-- A line ending in @// expect: E-A, W-B@ expects each code to be
-- reported on that line, and every diagnostic reported must be expected
-- by its line: a missing code and an extra one both fail. A line states
-- which codes it has, not how many times: a use that exceeds several
-- components of one constraint is one diagnostic per component. A program
-- with no expectation must check cleanly.
--
-- A case whose first line is @// pending: <reason>@ states what the
-- checker should do and does not yet: it is pending while it fails,
-- and fails once it passes, so that the mark is removed with the fix.
--
-- A case is a @.lask@ file, the entry module of its own program, or a
-- directory holding a @main.lask@ that may import the other files
-- beside it. Expectations are read from every file of a case.
--
-- This is where a regression of the checker is pinned: a case is a
-- file to add, not code to write, so the cases that break a rule can
-- be as many as there are ways to break it.
module Language.Lask.CorpusSpec (spec) where

import Control.Monad (filterM, forM, forM_)
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), compileWith)
import Language.Lask.Diagnostic (Advisory (..), Diagnostic (..))
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.ErrorCode (advisoryText, codeText)
import Language.Lask.Span (Position (..), Span (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, normalise, takeExtension, (</>))
import Test.Hspec

corpusRoot :: FilePath
corpusRoot = "test/corpus"

-- | A program to check: its directory, the entry module's name in it,
-- and the files whose expectations count.
data Case = Case
  { caseName :: String,
    caseDir :: FilePath,
    caseEntry :: FilePath,
    caseFiles :: [FilePath]
  }

-- | A diagnostic as the corpus states it: file, line and code.
type Finding = (FilePath, Int, T.Text)

spec :: Spec
spec = do
  cases <- runIO (findCases corpusRoot)
  describe "test/corpus" $ do
    it "holds cases" $ length cases `shouldSatisfy` (> 0)
    forM_ cases $ \c ->
      it (caseName c) $ do
        expected <- concat <$> forM (caseFiles c) (expectations (caseDir c))
        found <- findings c
        reason <- pendingReason c
        let got = Diff (nub (sort found))
            want = Diff (nub (sort expected))
        case reason of
          Nothing -> got `shouldBe` want
          Just why
            | got == want -> expectationFailure "passes now: remove its // pending: line"
            | otherwise -> pendingWith (T.unpack why)

-- | Every case under the root, by its path below it.
findCases :: FilePath -> IO [Case]
findCases root = go ""
  where
    go rel = do
      let dir = root </> rel
      entries <- sort <$> listDirectory dir
      fmap concat . forM entries $ \e -> do
        let relE = if null rel then e else rel </> e
            path = root </> relE
        isDir <- doesDirectoryExist path
        hasMain <- if isDir then doesFileExist (path </> "main.lask") else pure False
        if
          | isDir && hasMain -> do
              files <- filterM (doesFileExist . (path </>)) =<< (sort . filter isLask <$> listDirectory path)
              pure [Case relE path "main.lask" files]
          | isDir -> go relE
          | isLask e -> pure [Case relE dir e [e]]
          | otherwise -> pure []
    isLask f = takeExtension f == ".lask"

-- | The findings a file expects, from its @// expect:@ comments.
expectations :: FilePath -> FilePath -> IO [Finding]
expectations dir file = do
  src <- TIO.readFile (dir </> file)
  pure
    [ (file, n, code)
    | (n, l) <- zip [1 ..] (T.lines src),
      (_, rest) <- [T.breakOn marker l],
      not (T.null rest),
      code <- map T.strip (T.splitOn "," (T.drop (T.length marker) rest)),
      not (T.null code)
    ]
  where
    marker = "// expect:"

-- | The reason a case is pending, from the first line of its entry.
pendingReason :: Case -> IO (Maybe T.Text)
pendingReason c = do
  src <- TIO.readFile (caseDir c </> caseEntry c)
  pure $ case T.lines src of
    l : _ | Just why <- T.stripPrefix "// pending:" l -> Just (T.strip why)
    _ -> Nothing

-- | What the front end reports for a case: its errors, or when it has
-- none, its advisories.
findings :: Case -> IO [Finding]
findings c = do
  -- An absolute entry, so that the project and lock files the loader
  -- looks for beside it are the case's, never the repository's.
  dir <- makeAbsolute (caseDir c)
  let at s = case s of
        Span (Position f l _) _ -> (makeRelative dir (normalise f), l)
        NoSpan -> ("?", 0)
      finding s code = let (f, l) = at s in (f, l, code)
  r <- compileWith reader (dir </> caseEntry c)
  pure $ case r of
    Left ds -> [finding (diagSpan d) (codeText (diagCode d)) | d <- ds]
    Right compiled -> [finding (advSpan a) (advisoryText (advCode a)) | a <- cpAdvisories (compiledCore compiled)]
  where
    reader p = do
      exists <- doesFileExist p
      if exists then Right <$> TIO.readFile p else pure (Left (T.pack ("not found: " <> p)))

-- | Findings shown one per line, so a failure reads as a list of what
-- is missing and what is extra.
newtype Diff = Diff [Finding]
  deriving (Eq)

instance Show Diff where
  show (Diff fs) = unlines ("" : [f <> ":" <> show l <> ": " <> T.unpack c | (f, l, c) <- fs])
