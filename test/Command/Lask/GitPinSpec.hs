{-# LANGUAGE OverloadedStrings #-}

-- | How a git dependency is pinned in the lock (spec chapter 5, 11.5):
-- @rev@ is the commit the requested reference named when it was
-- fetched, recorded by whichever of @deps add@ and @deps sync@ fetched
-- it, and a reference that later names another commit is
-- @E-MODULE-REV-MOVED@ whether or not the cache still holds the old
-- content.
module Command.Lask.GitPinSpec (spec) where

import Command.Lask.Harness
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A scratch area with a git repository per name, a project, and a
-- runner with an isolated module cache.
data World = World
  { wRoot :: FilePath,
    wProj :: FilePath,
    wRun :: [String] -> IO Result
  }

withWorld :: FilePath -> (World -> IO a) -> IO a
withWorld lask action =
  withSystemTempDirectory "lask-gitpin" $ \root -> do
    let proj = root </> "proj"
    createDirectoryIfMissing True proj
    writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf() = hello()\n"
    action
      World
        { wRoot = root,
          wProj = proj,
          wRun = \args -> runLaskEnv lask proj [("LASK_CACHE_DIR", root </> "cache")] args ""
        }

-- | Create a repository whose @main.lask@ says @greeting@, tagged
-- @v1@ (annotated when asked), and return its URL.
repoAt :: World -> String -> String -> Bool -> IO String
repoAt w name greeting annotated = do
  let repo = wRoot w </> name
  createDirectoryIfMissing True repo
  writeFile (repo </> "main.lask") ("hello(): String = \"" <> greeting <> "\"\n")
  git repo ["init", "--quiet"]
  git repo ["add", "."]
  git repo ["commit", "--quiet", "-m", greeting]
  git repo (["tag"] <> (if annotated then ["-a", "-m", "v1"] else []) <> ["v1"])
  pure ("file://" <> repo)

-- | Commit new content and move tag @v1@ onto it.
moveTag :: World -> String -> IO ()
moveTag w name = do
  let repo = wRoot w </> name
  writeFile (repo </> "main.lask") "hello(): String = \"moved\"\n"
  git repo ["commit", "--quiet", "-am", "move"]
  git repo ["tag", "--force", "v1"]

headOf :: World -> String -> IO String
headOf w name = gitOut (wRoot w </> name) ["rev-parse", "HEAD"]

-- | A string field of a module entry in the project's lock.
lockField :: World -> String -> String -> IO (Maybe String)
lockField w dep field = do
  bytes <- BL.readFile (wProj w </> "lask.lock.json")
  pure $ case A.decode bytes of
    Just (A.Object top)
      | Just (A.Object modules) <- KM.lookup "modules" top,
        Just (A.Object entry) <- KM.lookup (AK.fromString dep) modules,
        Just (A.String s) <- KM.lookup (AK.fromString field) entry ->
          Just (T.unpack s)
    _ -> Nothing

addKit :: World -> String -> IO ()
addKit w url = do
  r <- wRun w ["deps", "add", "kit", "--git", url, "--rev", "v1"]
  resExit r `shouldBe` 0

shouldReportMoved :: Result -> Expectation
shouldReportMoved r = do
  resExit r `shouldBe` 3
  resErr r `shouldSatisfy` isInfixOf "E-MODULE-REV-MOVED"
  resErr r `shouldNotSatisfy` isInfixOf "E-MODULE-HASH-MISMATCH"
  resOut r `shouldSatisfy` isInfixOf "kit NG"

spec :: Spec
spec = beforeAll findLask $ describe "git dependency pinning (spec 5, 11.5)" $ do
  it "deps add pins the commit the tag names" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" False
    addKit w url
    sha <- headOf w "kit"
    lockField w "kit" "rev" `shouldReturn` Just sha

  it "pins the commit, not the tag object, for an annotated tag" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" True
    addKit w url
    sha <- headOf w "kit"
    lockField w "kit" "rev" `shouldReturn` Just sha
    -- An unmoved annotated tag is not reported as moved.
    r <- wRun w ["deps", "sync"]
    resExit r `shouldBe` 0

  -- #56
  it "reports a tag moved between deps add and the first sync" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" False
    addKit w url
    pinned <- headOf w "kit"
    moveTag w "kit"
    r <- wRun w ["deps", "sync"]
    shouldReportMoved r
    lockField w "kit" "rev" `shouldReturn` Just pinned

  -- #57
  it "reports a moved tag as moved when the cache is empty" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" False
    addKit w url
    moveTag w "kit"
    removeDirectoryRecursive (wRoot w </> "cache")
    r <- wRun w ["deps", "sync"]
    shouldReportMoved r

  it "restores the pinned commit into an empty cache while the tag stays" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" False
    addKit w url
    removeDirectoryRecursive (wRoot w </> "cache")
    r <- wRun w ["deps", "sync"]
    resExit r `shouldBe` 0
    e <- wRun w ["eval", "f"]
    resOut e `shouldBe` "\"hi\"\n"

  it "accepts a lock that pinned an annotated tag by its tag object" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" True
    addKit w url
    -- What earlier versions recorded: the SHA ls-remote printed.
    tagObject <- gitOut (wRoot w </> "kit") ["rev-parse", "v1"]
    lock <- readFile (wProj w </> "lask.lock.json")
    sha <- headOf w "kit"
    length lock `seq` writeFile (wProj w </> "lask.lock.json") (replace sha tagObject lock)
    r <- wRun w ["deps", "sync"]
    resExit r `shouldBe` 0
    lockField w "kit" "rev" `shouldReturn` Just sha

  it "deps add keeps the pins of the other dependencies and the images" $ \lask -> withWorld lask $ \w -> do
    url <- repoAt w "kit" "hi" False
    addKit w url
    pinned <- headOf w "kit"
    -- An image pinned by an earlier env build.
    lock <- readFile (wProj w </> "lask.lock.json")
    length lock `seq`
      writeFile
        (wProj w </> "lask.lock.json")
        (replace "\"images\": {}" "\"images\": {\"alpine:3.22.2\": {\"kind\": \"registry\", \"ref\": \"alpine:3.22.2\", \"digest\": \"sha256:aaa\"}}" lock)
    moveTag w "kit"
    other <- repoAt w "other" "o" False
    r <- wRun w ["deps", "add", "other", "--git", other, "--rev", "v1"]
    -- kit's tag has moved: adding another dependency must not re-pin it.
    shouldReportMovedOn r
    lockField w "kit" "rev" `shouldReturn` Just pinned
    readFile (wProj w </> "lask.lock.json") >>= (`shouldContain` "sha256:aaa")
  where
    shouldReportMovedOn r = do
      resExit r `shouldBe` 3
      resErr r `shouldSatisfy` isInfixOf "E-MODULE-REV-MOVED"

-- | Replace every occurrence of a substring.
replace :: String -> String -> String -> String
replace from to = go
  where
    go [] = []
    go s@(c : rest)
      | take (length from) s == from = to <> go (drop (length from) s)
      | otherwise = c : go rest
