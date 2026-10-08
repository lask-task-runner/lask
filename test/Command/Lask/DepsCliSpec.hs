{-# LANGUAGE OverloadedStrings #-}

-- | @lask deps list | graph | rm@ and @lask sync --prune@ (spec 11.5,
-- 11.7), against dependencies in local git repositories: what each
-- reports, and what each removes.
module Command.Lask.DepsCliSpec (spec) where

import Command.Lask.Harness
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A project depending on @kit@, which depends on @leaf@; both are
-- local repositories tagged @v1@. The action receives the scratch root,
-- the project directory, and a runner with the project's own cache.
withKit :: FilePath -> (FilePath -> FilePath -> ([String] -> IO Result) -> IO a) -> IO a
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
        run args = runLask lask proj args ""
    createDirectoryIfMissing True proj
    writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
    r <- run ["deps", "add", "kit", "--git", kit, "--rev", "v1"]
    resExit r `shouldBe` 0
    action root proj run
  where
    repo root name files = do
      let dir = root </> name
      createDirectoryIfMissing True dir
      mapM_ (\(f, c) -> writeFile (dir </> f) c) files
      git dir ["init", "--quiet"]
      git dir ["add", "."]
      git dir ["commit", "--quiet", "-m", name]
      git dir ["tag", "v1"]
      pure ("file://" <> dir)

spec :: Spec
spec = beforeAll findLask $ describe "dependency commands (spec 11.5, 11.7)" $ do
  it "lists every dependency, direct and transitive, as ok" $ \lask -> withKit lask $ \_ _ run -> do
    r <- run ["deps", "list"]
    resExit r `shouldBe` 0
    resOut r `shouldSatisfy` isInfixOf "NAME      SOURCE"
    resOut r `shouldSatisfy` isInfixOf "kit>leaf"
    resOut r `shouldSatisfy` isInfixOf "2 dependencies (1 direct, 1 transitive): 2 ok"

  -- The lock disagrees with lask.json exactly when check would call it
  -- stale and sync would pin it afresh.
  it "reports a dependency lask.json bumps as stale" $ \lask -> withKit lask $ \root proj run -> do
    git (root </> "kit") ["tag", "v2"]
    json <- readFile (proj </> "lask.json")
    length json `seq` writeFile (proj </> "lask.json") (replace "\"v1\"" "\"v2\"" json)
    r <- run ["deps", "list"]
    resExit r `shouldBe` 0
    resOut r `shouldSatisfy` isInfixOf "stale: locked v1, declared v2 (lask sync)"

  it "reports a dependency no module imports as unused, and sync warns of it" $ \lask -> withKit lask $ \_ proj run -> do
    writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
    r <- run ["deps", "list"]
    resOut r `shouldSatisfy` isInfixOf "unused (lask sync --prune)"
    s <- run ["sync"]
    resExit s `shouldBe` 0
    resErr s `shouldSatisfy` isInfixOf "warning: 'kit' is declared in lask.json but no module imports it"

  -- Every .lask file of the project counts, not only what main.lask
  -- reaches.
  it "counts an import in any .lask file of the project" $ \lask -> withKit lask $ \_ proj run -> do
    writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
    createDirectoryIfMissing True (proj </> "test")
    writeFile (proj </> "test" </> "self.lask") "import { hello } from \"kit\"\ng(): String = hello()\n"
    r <- run ["deps", "list"]
    resOut r `shouldSatisfy` isInfixOf "2 ok"

  it "shows the graph, the paths to one dependency, and a depth" $ \lask -> withKit lask $ \_ _ run -> do
    whole <- run ["deps", "graph"]
    resExit whole `shouldBe` 0
    resOut whole `shouldSatisfy` isInfixOf "main.lask\n└── kit"
    resOut whole `shouldSatisfy` isInfixOf "    └── leaf"
    toLeaf <- run ["deps", "graph", "leaf"]
    resOut toLeaf `shouldSatisfy` isInfixOf "◀"
    shallow <- run ["deps", "graph", "--depth", "1"]
    resOut shallow `shouldNotSatisfy` isInfixOf "leaf"
    missing <- run ["deps", "graph", "nope"]
    resExit missing `shouldBe` 4

  it "refuses to remove a dependency still imported, naming where" $ \lask -> withKit lask $ \_ proj run -> do
    r <- run ["deps", "rm", "kit"]
    resExit r `shouldBe` 4
    resErr r `shouldSatisfy` isInfixOf "main.lask:1"
    readFile (proj </> "lask.json") >>= (`shouldSatisfy` isInfixOf "kit")

  it "refuses to remove a transitive dependency, naming what needs it" $ \lask -> withKit lask $ \_ _ run -> do
    r <- run ["deps", "rm", "leaf"]
    resExit r `shouldBe` 4
    resErr r `shouldSatisfy` isInfixOf "is a dependency of 'kit'"

  it "removes a dependency, what is under it, and its cache entries" $ \lask -> withKit lask $ \_ proj run -> do
    writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
    r <- run ["deps", "rm", "kit"]
    resExit r `shouldBe` 0
    resOut r `shouldSatisfy` isInfixOf "removed from lask.lock.json: kit, kit>leaf"
    readFile (proj </> "lask.json") >>= (`shouldNotSatisfy` isInfixOf "kit")
    lock <- readFile (proj </> "lask.lock.json")
    length lock `seq` lock `shouldNotSatisfy` isInfixOf "kit"
    cacheEntries proj `shouldReturn` []

  it "prunes unused dependencies with sync --prune, and --frozen refuses to" $ \lask -> withKit lask $ \_ proj run -> do
    writeFile (proj </> "main.lask") "f(): String = \"x\"\n"
    frozen <- run ["sync", "--prune", "--frozen"]
    resExit frozen `shouldBe` 1
    readFile (proj </> "lask.json") >>= (`shouldSatisfy` isInfixOf "kit")
    r <- run ["sync", "--prune"]
    resExit r `shouldBe` 0
    resOut r `shouldSatisfy` isInfixOf "pruned"
    readFile (proj </> "lask.json") >>= (`shouldNotSatisfy` isInfixOf "kit")
    cacheEntries proj `shouldReturn` []
  where
    cacheEntries proj = do
      let cache = proj </> ".lask" </> "deps"
      present <- doesDirectoryExist cache
      if present then listDirectory cache else pure []

-- | Replace every occurrence of a substring.
replace :: String -> String -> String -> String
replace from to = go
  where
    go [] = []
    go s@(c : rest)
      | take (length from) s == from = to <> go (drop (length from) s)
      | otherwise = c : go rest
