-- | Output of the built @lask@ binary compared whole against a file.
--
-- Each directory under @test/golden@ is a project and one command line:
-- its @.lask@ files are copied into a fresh directory, @args@ holds the
-- arguments, one per line, and @expected@ the exit code, standard output
-- and standard error that running them there gives. The directory the
-- project was copied to is written @<dir>@ in @expected@.
--
-- This is for output a reader takes in whole: help, the tables of
-- @envs@, the text of a diagnostic. A test that cares about one part
-- of an output should assert that part instead.
--
-- Set @LASK_ACCEPT=1@ to write what the binary gives into @expected@,
-- then read the diff before committing it.
module Command.Lask.GoldenSpec (spec) where

import Command.Lask.Harness
import Control.Monad (forM_, unless)
import Data.List (isPrefixOf, isSuffixOf, sort)
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

goldenRoot :: FilePath
goldenRoot = "test/golden"

spec :: Spec
spec = do
  cases <- runIO (sort <$> listDirectory goldenRoot)
  accept <- runIO ((== Just "1") <$> lookupEnv "LASK_ACCEPT")
  beforeAll findLask $ describe "test/golden" $ do
    it "holds cases" $ \_ -> length cases `shouldSatisfy` (> 0)
    forM_ cases $ \name ->
      it name $ \lask -> do
        let dir = goldenRoot </> name
        files <- filter (".lask" `isSuffixOf`) <$> listDirectory dir
        sources <- mapM (\f -> (,) f <$> readFile' (dir </> f)) files
        args <- filter (not . null) . lines <$> readFile' (dir </> "args")
        actual <- withProject sources $ \tmp -> do
          real <- canonicalizePath tmp
          render [real, tmp] <$> runLask lask tmp args ""
        let expectedPath = dir </> "expected"
        exists <- doesFileExist expectedPath
        if accept
          then writeFile expectedPath actual
          else do
            unless exists $ expectationFailure ("no expected file: run with LASK_ACCEPT=1 to write " <> expectedPath)
            expected <- readFile' expectedPath
            Golden actual `shouldBe` Golden expected

-- | A run as @expected@ holds it.
render :: [FilePath] -> Result -> String
render dirs r =
  unlines
    [ "exit: " <> show (resExit r),
      "--- stdout",
      hide (resOut r),
      "--- stderr",
      hide (resErr r)
    ]
  where
    -- The real path first: it is the longer where the two differ.
    hide s = foldr (\d -> replace d "<dir>") s (reverse dirs)

-- | A file read whole before it is written again.
readFile' :: FilePath -> IO String
readFile' p = do
  s <- readFile p
  length s `seq` pure s

replace :: String -> String -> String -> String
replace from to = go
  where
    go [] = []
    go s@(c : rest)
      | from `isPrefixOf` s = to <> go (drop (length from) s)
      | otherwise = c : go rest

-- | An output shown as it is, so a failure reads as two texts.
newtype Golden = Golden String
  deriving (Eq)

instance Show Golden where
  show (Golden s) = "\n" <> s
