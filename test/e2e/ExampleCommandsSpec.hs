-- | The commands the language examples tell a reader to run (their
-- header comment and their @\@example@ lines) are commands lask
-- accepts: each binds its arguments and type-checks, so it ends in
-- neither a static error (1) nor a usage error (4). A command that
-- reaches a container may still fail with 3 here, where no image is
-- pulled; one an example calls pure, needing no Docker, succeeds.
--
-- Each is run on a copy of the project, so what it writes stays there.
-- A project with dependencies is synced first, which reaches the
-- network; @LASK_TEST_OFFLINE=1@ leaves it pending.
module ExampleCommandsSpec (spec) where

import Control.Monad (filterM, forM_, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Support.Harness
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Hspec

languageExamples :: IO [FilePath]
languageExamples = do
  let root = "example" </> "02-language"
  names <- sort <$> listDirectory root
  filterM (\p -> doesFileExist (p </> "main.lask")) [root </> n | n <- names]

-- | The commands written in comments as @lask eval ...@ or
-- @lask run ...@, without a trailing shell comment. A line that goes
-- on to other shell commands is left out: it shows a failure on
-- purpose.
documented :: String -> [String]
documented src = nub [c | l <- lines src, Just c <- [command l], ';' `notElem` c]
  where
    command l = case dropWhile isSpace <$> stripPrefix' "//" (dropWhile isSpace l) of
      Just rest
        | Just c <- after "@example " rest -> lask c
        | otherwise -> lask rest
      Nothing -> Nothing
    after p s
      | p `isPrefixOf` s = Just (drop (length p) s)
      | otherwise = case s of
          [] -> Nothing
          _ : t -> after p t
    lask c
      | any (`isPrefixOf` c) ["lask eval ", "lask run "] = Just (trim (takeWhile (/= '#') c))
      | otherwise = Nothing
    trim = reverse . dropWhile isSpace . reverse
    stripPrefix' p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

spec :: Spec
spec = do
  projects <- runIO languageExamples
  cases <- runIO (mapM (\p -> (,) p . documented <$> readFile (p </> "main.lask")) projects)
  offline <- runIO ((== Just "1") <$> lookupEnv "LASK_TEST_OFFLINE")
  beforeAll findLask $ describe "example projects, the commands they document" $ do
    it "are found" $ \_ -> sum (map (length . snd) cases) `shouldSatisfy` (> 20)
    forM_ cases $ \(project, cmds) -> do
      src <- runIO (readFile (project </> "main.lask"))
      let pure' = "needs no Docker" `isInfixOf` src
      forM_ cmds $ \cmd ->
        it (project <> ": " <> cmd) $ \lask -> withFakeDocker $ \_ extra -> withCopy project $ \dir -> do
          let cache = ("LASK_CACHE_DIR", dir </> ".cache")
          hasDeps <- doesFileExist (dir </> "lask.json")
          when (hasDeps && offline) $ pendingWith "declares dependencies (LASK_TEST_OFFLINE=1)"
          when hasDeps $ do
            s <- runLaskEnv lask dir (cache : extra) ["sync"] ""
            resExit s `shouldBe` 0
          (code, out, err) <- shell' lask dir (cache : extra) (drop (length "lask ") cmd)
          let shown = cmd <> "\nstdout: " <> out <> "\nstderr: " <> err
          if pure'
            then (code, shown) `shouldBe` (0, shown)
            else (code `notElem` [1, 4], shown) `shouldBe` (True, shown)
  where
    -- The arguments as the shell splits them, quotes included.
    shell' lask dir extra args = do
      base <- getEnvironment
      let env' = extra <> [(k, v) | (k, v) <- base, k `notElem` map fst extra]
      (c, o, e) <- readCreateProcessWithExitCode (proc "sh" ["-c", "exec \"$0\" " <> args, lask]) {cwd = Just dir, env = Just env', std_in = NoStream} ""
      pure (case c of ExitSuccess -> 0; ExitFailure n -> n :: Int, o, e)

withCopy :: FilePath -> (FilePath -> IO a) -> IO a
withCopy project action =
  withSystemTempDirectory "lask-example" $ \tmp -> do
    let dir = tmp </> "project"
    callProcess "cp" ["-R", project, dir]
    local <- doesDirectoryExist (dir </> ".lask")
    when local $ removeDirectoryRecursive (dir </> ".lask")
    action dir
