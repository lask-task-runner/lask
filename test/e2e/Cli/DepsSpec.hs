{-# LANGUAGE OverloadedStrings #-}

-- | The dependency commands and @sync@ against local git repositories
-- (spec 5, 11.5, 11.7): every status @deps list@ reports, the JSON
-- forms, @add@ and @rm@ at their edges, what reaches the network and
-- what does not, and the exit code of each kind of failure.
module Cli.DepsSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Vector as V
import Support.Harness
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A scratch root with @kit@, which depends on @leaf@, both local
-- repositories tagged @v1@, and a project that imports @kit@ and has
-- added it. The action gets the root, the project and a runner.
data Kit = Kit {kRoot :: FilePath, kProj :: FilePath, kRun :: [String] -> IO Result, kRunEnv :: [(String, String)] -> [String] -> IO Result}

withKit :: FilePath -> (Kit -> IO a) -> IO a
withKit lask action =
  withSystemTempDirectory "lask-deps" $ \root -> do
    leaf <- repo root "leaf" [("main.lask", "leafy(): String = \"leaf\"\n")]
    kit <-
      repo
        root
        "kit"
        [ ("main.lask", "import { leafy } from \"leaf\"\nhello(): String = leafy()\n"),
          ("lask.json", "{\"dependencies\": {\"leaf\": {\"git\": \"" <> leaf <> "\", \"rev\": \"v1\"}}}")
        ]
    let proj = root </> "proj"
        runEnv extra args = runLaskEnv lask proj extra args ""
    createDirectoryIfMissing True proj
    writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
    r <- runEnv [] ["deps", "add", "kit", "--git", kit, "--rev", "v1"]
    resExit r `shouldBe` 0
    action (Kit root proj (runEnv []) runEnv)

-- | A local repository of the files given, tagged @v1@, by its URL.
repo :: FilePath -> String -> [(FilePath, String)] -> IO String
repo root name files = do
  let dir = root </> name
  createDirectoryIfMissing True dir
  mapM_ (\(f, c) -> writeFile (dir </> f) c) files
  git dir ["init", "--quiet"]
  git dir ["add", "."]
  git dir ["commit", "--quiet", "-m", name]
  git dir ["tag", "v1"]
  pure ("file://" <> dir)

-- | The line of @deps list@ that names a dependency.
rowOf :: String -> Result -> String
rowOf name r = case [l | l <- lines (resOut r), take (length name + 1) l == name <> " "] of
  l : _ -> l
  [] -> ""

edit :: FilePath -> (String -> String) -> IO ()
edit path f = do
  s <- readFile path
  length s `seq` writeFile path (f s)

replace :: String -> String -> String -> String
replace old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- | Remove the source repositories, so that only the cache holds them.
offline :: Kit -> IO ()
offline k = mapM_ (\n -> removeDirectoryRecursive (kRoot k </> n)) ["kit", "leaf"]

dropCache :: Kit -> IO ()
dropCache k = removeDirectoryRecursive (kProj k </> ".lask" </> "deps")

cacheEntries :: FilePath -> IO [FilePath]
cacheEntries dir = do
  present <- doesDirectoryExist dir
  if present then listDirectory dir else pure []

decode :: Result -> Maybe A.Value
decode = A.decode . BL.pack . resOut

spec :: Spec
spec = beforeAll findLask $ do
  describe "deps list statuses (spec 11.5)" $ do
    it "reports a declared dependency with no lock entry as not locked" $ \lask -> withKit lask $ \k -> do
      edit (kProj k </> "lask.json") (replace "\"dependencies\": {" ("\"dependencies\": {\"extra\": {\"git\": \"file://" <> kRoot k <> "/kit\", \"rev\": \"v1\"}, "))
      r <- kRun k ["deps", "list"]
      resExit r `shouldBe` 0
      rowOf "extra" r `shouldSatisfy` isInfixOf "not locked (lask sync)"

    it "reports a lock entry nothing declares as not in lask.json" $ \lask -> withKit lask $ \k -> do
      writeFile (kProj k </> "lask.json") "{\"dependencies\": {}}"
      r <- kRun k ["deps", "list"]
      resExit r `shouldBe` 0
      rowOf "kit" r `shouldSatisfy` isInfixOf "not in lask.json (lask sync drops it)"

    it "reports what the cache does not hold as not cached, and its children as unknown" $ \lask -> withKit lask $ \k -> do
      dropCache k
      r <- kRun k ["deps", "list"]
      resExit r `shouldBe` 0
      rowOf "kit" r `shouldSatisfy` isInfixOf "not cached (lask sync)"
      rowOf "kit>leaf" r `shouldSatisfy` isInfixOf "unknown: its parent is not cached (lask sync)"

    it "reaches no network" $ \lask -> withKit lask $ \k -> do
      offline k
      r <- kRun k ["deps", "list"]
      resExit r `shouldBe` 0
      resOut r `shouldSatisfy` isInfixOf "2 ok"

    it "shows the commit to seven digits and the hash to twelve" $ \lask -> withKit lask $ \k -> do
      sha <- gitOut (kRoot k </> "kit") ["rev-parse", "HEAD"]
      r <- kRun k ["deps", "list"]
      let row = words (rowOf "kit" r)
      row `shouldSatisfy` elem (take 7 sha)
      row `shouldSatisfy` any (\w -> take 7 w == "sha256-" && length (drop 7 w) == 12 && all (`elem` ("0123456789abcdef" :: String)) (drop 7 w))

  describe "which dependencies are used (spec 11.5)" $ do
    let unusedWith files = \lask -> withKit lask $ \k -> do
          writeFile (kProj k </> "main.lask") "f(): String = \"x\"\n"
          mapM_
            ( \(f, c) -> do
                createDirectoryIfMissing True (kProj k </> takeDirectory' f)
                writeFile (kProj k </> f) c
            )
            files
          r <- kRun k ["deps", "list"]
          pure (rowOf "kit" r)
        importKit = "import { hello } from \"kit\"\ng(): String = hello()\n"
    it "does not count an import in node_modules" $ \lask ->
      unusedWith [("node_modules/x/a.lask", importKit)] lask >>= (`shouldSatisfy` isInfixOf "unused")
    it "does not count an import in a hidden directory" $ \lask ->
      unusedWith [(".hidden/a.lask", importKit)] lask >>= (`shouldSatisfy` isInfixOf "unused")
    it "does not count an import in another project below this one" $ \lask ->
      unusedWith [("sub/lask.json", "{\"dependencies\": {}}"), ("sub/a.lask", importKit)] lask >>= (`shouldSatisfy` isInfixOf "unused")
    it "counts a file that does not parse by its text" $ \lask ->
      unusedWith [("broken.lask", "import { hello } from \"kit\"\nf( = \n")] lask >>= (`shouldSatisfy` isInfixOf "ok")
    it "counts a namespace import and a deep import" $ \lask -> do
      unusedWith [("a.lask", "import * as k from \"kit\"\n")] lask >>= (`shouldSatisfy` isInfixOf "ok")
      unusedWith [("b.lask", "import { hello } from \"kit/main.lask\"\n")] lask >>= (`shouldSatisfy` isInfixOf "ok")
    it "counts a re-export" $ \lask ->
      unusedWith [("a.lask", "export { hello } from \"kit\"\n")] lask >>= (`shouldSatisfy` isInfixOf "ok")

  describe "JSON output (spec 11.5)" $ do
    it "writes deps list as an array, one object a dependency" $ \lask -> withKit lask $ \k -> do
      r <- kRun k ["deps", "list", "--output", "json"]
      resExit r `shouldBe` 0
      case decode r of
        Just (A.Array xs) -> V.length xs `shouldBe` 2
        other -> expectationFailure ("not an array: " <> show other <> "\n" <> resOut r)
    it "writes deps graph as a nested object" $ \lask -> withKit lask $ \k -> do
      r <- kRun k ["deps", "graph", "--output", "json"]
      resExit r `shouldBe` 0
      case decode r of
        Just (A.Object _) -> resOut r `shouldSatisfy` isInfixOf "leaf"
        other -> expectationFailure ("not an object: " <> show other <> "\n" <> resOut r)
    it "writes the sync summary as one object of modules and images" $ \lask -> withKit lask $ \k -> do
      r <- kRun k ["sync", "--output", "json"]
      resExit r `shouldBe` 0
      case decode r of
        Just (A.Object o) -> do
          KM.member (AK.fromString "modules") o `shouldBe` True
          KM.member (AK.fromString "images") o `shouldBe` True
        other -> expectationFailure ("not an object: " <> show other <> "\n" <> resOut r)

  describe "deps add (spec 11.5)" $ do
    it "records a single-file url dependency" $ \lask -> withSystemTempDirectory "lask-deps" $ \root -> do
      let proj = root </> "proj"
      createDirectoryIfMissing True proj
      writeFile (root </> "one.lask") "one(): String = \"one\"\n"
      writeFile (proj </> "main.lask") "import { one } from \"single\"\nf(): String = one()\n"
      r <- runLask lask proj ["deps", "add", "single", "--url", "file://" <> root </> "one.lask"] ""
      resExit r `shouldBe` 0
      readFile (proj </> "lask.json") >>= (`shouldSatisfy` isInfixOf "\"url\"")
      e <- runLask lask proj ["eval", "f"] ""
      resOut e `shouldBe` "\"one\"\n"
    it "rejects a name that is not a lower_id, writing nothing" $ \lask -> withKit lask $ \k -> do
      before <- readFile (kProj k </> "lask.json")
      length before `seq` pure ()
      r <- kRun k ["deps", "add", "Kit-2", "--git", "file://" <> kRoot k </> "kit", "--rev", "v1"]
      resExit r `shouldBe` 4
      readFile (kProj k </> "lask.json") `shouldReturn` before
    it "replaces an entry of the same name" $ \lask -> withKit lask $ \k -> do
      git (kRoot k </> "kit") ["tag", "v2"]
      r <- kRun k ["deps", "add", "kit", "--git", "file://" <> kRoot k </> "kit", "--rev", "v2"]
      resExit r `shouldBe` 0
      json <- readFile (kProj k </> "lask.json")
      json `shouldSatisfy` isInfixOf "v2"
      json `shouldNotSatisfy` isInfixOf "v1"
    it "fails with 3 when the source cannot be fetched, writing nothing" $ \lask -> withSystemTempDirectory "lask-deps" $ \root -> do
      let proj = root </> "proj"
      createDirectoryIfMissing True proj
      writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
      r <- runLask lask proj ["deps", "add", "gone", "--git", "file://" <> root </> "nowhere", "--rev", "v1"] ""
      resExit r `shouldBe` 3
      doesFileExist (proj </> "lask.json") `shouldReturn` False
    it "fails with 4 given neither --git nor --url, or both" $ \lask -> withKit lask $ \k -> do
      resExit <$> kRun k ["deps", "add", "x"] `shouldReturn` 4
      resExit <$> kRun k ["deps", "add", "x", "--git", "file:///a", "--url", "file:///b"] `shouldReturn` 4

  describe "deps rm (spec 11.5)" $ do
    it "fails with 4 for a dependency that is not declared" $ \lask -> withKit lask $ \k -> do
      r <- kRun k ["deps", "rm", "nope"]
      resExit r `shouldBe` 4
    it "reaches no network" $ \lask -> withKit lask $ \k -> do
      writeFile (kProj k </> "main.lask") "f(): String = \"x\"\n"
      offline k
      resExit <$> kRun k ["deps", "rm", "kit"] `shouldReturn` 0
    it "leaves a shared cache that LASK_CACHE_DIR names alone" $ \lask -> withSystemTempDirectory "lask-deps" $ \root -> do
      leaf <- repo root "leaf" [("main.lask", "leafy(): String = \"leaf\"\n")]
      let proj = root </> "proj"
          shared = [("LASK_CACHE_DIR", root </> "shared")]
      createDirectoryIfMissing True proj
      writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
      resExit <$> runLaskEnv lask proj shared ["deps", "add", "leaf", "--git", leaf, "--rev", "v1"] "" `shouldReturn` 0
      before <- cacheEntries (root </> "shared")
      before `shouldSatisfy` (not . null)
      resExit <$> runLaskEnv lask proj shared ["deps", "rm", "leaf"] "" `shouldReturn` 0
      cacheEntries (root </> "shared") `shouldReturn` before

  describe "sync (spec 11.7)" $ do
    it "reaches no network for what the cache already holds" $ \lask -> withKit lask $ \k -> do
      offline k
      r <- kRun k ["sync"]
      resExit r `shouldBe` 0
    it "restores an emptied cache from the sources" $ \lask -> withKit lask $ \k -> do
      dropCache k
      resExit <$> kRun k ["sync"] `shouldReturn` 0
      e <- kRun k ["eval", "f"]
      resOut e `shouldBe` "\"leaf\"\n"
    it "fails with 3 when a source is gone and the cache does not hold it" $ \lask -> withKit lask $ \k -> do
      dropCache k
      offline k
      r <- kRun k ["sync"]
      resExit r `shouldBe` 3
      resOut r `shouldSatisfy` isInfixOf "failed"
    it "reports content that does not match the locked hash, and does not cache it" $ \lask -> withKit lask $ \k -> do
      dropCache k
      lock <- readFile (kProj k </> "lask.lock.json")
      let bad = rewriteHash "kit" lock
      length lock `seq` writeFile (kProj k </> "lask.lock.json") bad
      r <- kRun k ["sync"]
      resExit r `shouldBe` 3
      resErr r `shouldSatisfy` isInfixOf "E-IO-MODULE-HASH-MISMATCH"
      e <- kRun k ["eval", "f"]
      resExit e `shouldNotBe` 0
    it "leaves an up-to-date lock as it is under --frozen" $ \lask -> withKit lask $ \k -> do
      before <- readFile (kProj k </> "lask.lock.json")
      length before `seq` pure ()
      resExit <$> kRun k ["sync", "--frozen"] `shouldReturn` 0
      readFile (kProj k </> "lask.lock.json") `shouldReturn` before
    it "fails with 1 under --frozen when the lock is stale, writing nothing" $ \lask -> withKit lask $ \k -> do
      git (kRoot k </> "kit") ["tag", "v2"]
      edit (kProj k </> "lask.json") (replace "\"v1\"" "\"v2\"")
      before <- readFile (kProj k </> "lask.lock.json")
      length before `seq` pure ()
      r <- kRun k ["sync", "--frozen"]
      resExit r `shouldBe` 1
      resErr r `shouldSatisfy` isInfixOf "E-MODULE-LOCK-STALE"
      readFile (kProj k </> "lask.lock.json") `shouldReturn` before
    it "writes the Modules and Images tables and a count" $ \lask -> withKit lask $ \k -> do
      r <- kRun k ["sync"]
      resOut r `shouldSatisfy` isInfixOf "NAME"
      resOut r `shouldSatisfy` isInfixOf "Images"
      resOut r `shouldSatisfy` isInfixOf "2 modules (2 present)"
      resOut r `shouldSatisfy` isInfixOf "kit>leaf"
    it "fails with 1 for a malformed project file" $ \lask -> withKit lask $ \k -> do
      writeFile (kProj k </> "lask.json") "{\"dependencies\": "
      resExit <$> kRun k ["sync"] `shouldReturn` 1
      resExit <$> kRun k ["deps", "list"] `shouldReturn` 1
    it "fails with 1 for a project file that breaks the schema" $ \lask -> withKit lask $ \k -> do
      writeFile (kProj k </> "lask.json") "{\"dependencies\": {\"kit\": {\"git\": \"file:///x\"}}}"
      resExit <$> kRun k ["sync"] `shouldReturn` 1

  describe "reading the program (spec 5)" $ do
    it "refuses to run with a dependency the lock does not record" $ \lask -> withKit lask $ \k -> do
      removeFile (kProj k </> "lask.lock.json")
      r <- kRun k ["eval", "f"]
      resExit r `shouldBe` 1
    it "refuses to run with a stale lock entry" $ \lask -> withKit lask $ \k -> do
      git (kRoot k </> "kit") ["tag", "v2"]
      edit (kProj k </> "lask.json") (replace "\"v1\"" "\"v2\"")
      r <- kRun k ["eval", "f"]
      resExit r `shouldBe` 1
      resErr r `shouldSatisfy` isInfixOf "E-MODULE-LOCK-STALE"
    it "runs from the cache with the sources gone" $ \lask -> withKit lask $ \k -> do
      offline k
      e <- kRun k ["eval", "f"]
      resOut e `shouldBe` "\"leaf\"\n"
  where
    takeDirectory' = reverse . drop 1 . dropWhile (/= '/') . reverse
    -- Replace the first sha256 hash after the entry's key with another.
    rewriteHash name s =
      let (pre, post) = T.breakOn (T.pack ("\"" <> name <> "\"")) (T.pack s)
          (mid, rest) = T.breakOn "sha256-" post
       in T.unpack (pre <> mid <> "sha256-" <> T.replicate 64 "0" <> T.drop (7 + 64) rest)
