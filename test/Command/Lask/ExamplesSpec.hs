-- | Every project under @example/@ passes @lask check@. The examples
-- are what a reader copies first, so one that no longer type-checks is
-- a broken promise even when every other test passes.
--
-- Each project is checked from a copy, with an empty module cache, the
-- way a fresh clone sees it. A project that declares dependencies is
-- synced first, which reaches the network; set @LASK_TEST_OFFLINE=1@
-- to leave those pending instead.
module Command.Lask.ExamplesSpec (spec) where

import Command.Lask.Harness
import Control.Monad (filterM, forM_, when)
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

-- | Directories two levels under @example/@ that hold a @main.lask@.
exampleProjects :: IO [FilePath]
exampleProjects = do
  groups <- subdirs "example"
  projects <- concat <$> mapM subdirs groups
  filterM (\p -> doesFileExist (p </> "main.lask")) projects
  where
    subdirs d = do
      names <- sort <$> listDirectory d
      filterM doesDirectoryExist [d </> n | n <- names]

spec :: Spec
spec = do
  projects <- runIO exampleProjects
  offline <- runIO ((== Just "1") <$> lookupEnv "LASK_TEST_OFFLINE")
  beforeAll findLask $ describe "example projects" $ do
    it "are found" $ \_ -> projects `shouldNotBe` []
    forM_ projects $ \project ->
      it ("check " <> project) $ \lask -> withCopy project $ \dir -> do
        let cache = ("LASK_CACHE_DIR", dir </> ".cache")
        hasDeps <- doesFileExist (dir </> "lask.json")
        when (hasDeps && offline) $ pendingWith "declares dependencies (LASK_TEST_OFFLINE=1)"
        -- Only the modules are wanted here. Sync materializes the
        -- lock's images too, so it runs against the fake docker, and
        -- its verdict is left to the check that follows: a module it
        -- failed to fetch is E-MODULE-UNRESOLVED there.
        synced <-
          if hasDeps
            then withFakeDocker $ \_ extra -> Just <$> runLaskEnv lask dir (cache : extra) ["deps", "sync"] ""
            else pure Nothing
        checked <- runLaskEnv lask dir [cache] ["check"] ""
        (checked, synced) `shouldBe` (Result 0 "the module is valid\n" "", synced)

-- | Run an action on a copy of the project without its local module
-- cache, so the check sees what a fresh clone sees.
withCopy :: FilePath -> (FilePath -> IO a) -> IO a
withCopy project action =
  withSystemTempDirectory "lask-example" $ \tmp -> do
    let dir = tmp </> "project"
    callProcess "cp" ["-R", project, dir]
    local <- doesDirectoryExist (dir </> ".lask")
    when local $ removeDirectoryRecursive (dir </> ".lask")
    action dir
