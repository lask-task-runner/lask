{-# LANGUAGE OverloadedStrings #-}

-- | Modules and external dependencies from the command line (spec 5, 11.5).
module Cli.ModulesSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  describe "modules (spec 5)" $ do
    it "imports across files" $ \lask ->
      withProject
        [ ("main.lask", "import { add } from \"./lib.lask\"\nsum2(a: Number, b: Number): Number = add(a, b)\n"),
          ("lib.lask", "add(x: Number, y: Number): Number = x + y\n")
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "sum2", "20", "22"] ""
          r `shouldBe` Result 0 "42\n" ""

  describe "external dependencies (spec 5, 11.5)" $ do
    it "adds, resolves, syncs and verifies a single-file url dependency" $ \lask ->
      withSystemTempDirectory "lask-deps" $ \root -> do
        let cache = root </> "cache"
            srcDir = root </> "published"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True proj
        writeFile (srcDir </> "notify.lask") "send(x: String): String = concat(\"sent:\", x)\n"
        writeFile (proj </> "main.lask") "import { send } from \"notify\"\nf(): String = send(\"a\")\n"

        -- Before the dependency is declared: E-MODULE-UNRESOLVED
        -- (check prints diagnostics to stdout, spec 11.3).
        r0 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r0 `shouldBe` 1
        resOut r0 `shouldSatisfy` isInfixOf "E-MODULE-UNRESOLVED"

        -- deps add fetches (file:// URL, no network), records and caches.
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "notify", "--url", "file://" <> srcDir </> "notify.lask"] ""
        resExit r1 `shouldBe` 0
        doesFileExist (proj </> "lask.json") `shouldReturn` True

        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"sent:a\"\n" ""

        -- Wiping the cache: resolution must not touch the network.
        removeDirectoryRecursive cache
        r3 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r3 `shouldBe` 1
        resOut r3 `shouldSatisfy` isInfixOf "E-MODULE-UNRESOLVED"
        resOut r3 `shouldSatisfy` isInfixOf "lask sync"

        -- sync restores the cache and verifies the hash.
        r4 <- runLaskEnv lask proj extraEnv ["sync"] ""
        resExit r4 `shouldBe` 0
        r5 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r5 `shouldBe` Result 0 "\"sent:a\"\n" ""

        -- Tampering with the published source: sync must detect the
        -- mismatch and place nothing in the cache (exit 3).
        removeDirectoryRecursive cache
        writeFile (srcDir </> "notify.lask") "send(x: String): String = concat(\"evil:\", x)\n"
        r6 <- runLaskEnv lask proj extraEnv ["sync"] ""
        resExit r6 `shouldBe` 3
        resErr r6 `shouldSatisfy` isInfixOf "E-IO-MODULE-HASH-MISMATCH"
        r7 <- runLaskEnv lask proj extraEnv ["check"] ""
        resExit r7 `shouldBe` 1

    it "adds and imports a git tree dependency through its entry module" $ \lask ->
      withSystemTempDirectory "lask-deps-git" $ \root -> do
        let cache = root </> "cache"
            repo = root </> "repo"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
        createDirectoryIfMissing True repo
        createDirectoryIfMissing True proj
        -- The re-export binds `u` locally as well as publishing it.
        writeFile (repo </> "main.lask") $
          "export { u } from \"./util.lask\"\n"
            <> "hello(): String = u\n"
        writeFile (repo </> "util.lask") "u: String = \"from-kit\"\n"
        git repo ["init", "--quiet"]
        git repo ["add", "."]
        git repo ["commit", "--quiet", "-m", "init"]
        git repo ["tag", "v1"]
        -- Only the entry module is importable; `u` reaches the
        -- consumer through the re-export in main.lask (spec 5).
        writeFile (proj </> "main.lask") $
          "import { hello, u } from \"kit\"\n"
            <> "f(): String = concat(hello(), u)\n"
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "kit", "--git", "file://" <> repo, "--rev", "v1"] ""
        resExit r1 `shouldBe` 0
        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"from-kitfrom-kit\"\n" ""

    -- A dependency reached through another is locked as parent>child
    -- (spec chapter 5), the key the loader looks it up by.
    it "locks a transitive dependency under its parent, and imports through it" $ \lask ->
      withSystemTempDirectory "lask-deps-transitive" $ \root -> do
        let cache = root </> "cache"
            util = root </> "util"
            kit = root </> "kit"
            proj = root </> "proj"
            extraEnv = [("LASK_CACHE_DIR", cache)]
            repoWith dir files = do
              createDirectoryIfMissing True dir
              mapM_ (\(f, c) -> writeFile (dir </> f) c) files
              git dir ["init", "--quiet"]
              git dir ["add", "."]
              git dir ["commit", "--quiet", "-m", "init"]
              git dir ["tag", "v1"]
        repoWith util [("main.lask", "u(): String = \"from-util\"\n")]
        repoWith
          kit
          [ ("lask.json", "{\"dependencies\": {\"util\": {\"git\": \"file://" <> util <> "\", \"rev\": \"v1\"}}}\n"),
            ("main.lask", "import { u } from \"util\"\nhello(): String = u()\n")
          ]
        createDirectoryIfMissing True proj
        writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
        r1 <- runLaskEnv lask proj extraEnv ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
        resExit r1 `shouldBe` 0
        lock <- readFile (proj </> "lask.lock.json")
        lock `shouldSatisfy` isInfixOf "\"kit>util\""
        r2 <- runLaskEnv lask proj extraEnv ["eval", "f"] ""
        r2 `shouldBe` Result 0 "\"from-util\"\n" ""
        r3 <- runLaskEnv lask proj extraEnv ["sync", "--frozen"] ""
        resExit r3 `shouldBe` 0

    -- A lock is committed and read on other machines, so it must not
    -- record where the machine that wrote it kept its cache (spec 11.7).
    it "writes the same lock for a dependency's recipe wherever LASK_CACHE_DIR puts the cache" $ \lask ->
      withFakeDocker $ \_ extra ->
        withSystemTempDirectory "lask-deps-recipe" $ \root -> do
          let kit = root </> "kit"
              defaultProj = root </> "default"
              movedProj = root </> "moved"
              moved = ("LASK_CACHE_DIR", root </> "cache") : extra
          createDirectoryIfMissing True (kit </> "images")
          writeFile (kit </> "images" </> "Dockerfile") "FROM scratch\n"
          writeFile (kit </> "main.lask") "hello(): String = $[#./images/Dockerfile] cat x\n"
          git kit ["init", "--quiet"]
          git kit ["add", "."]
          git kit ["commit", "--quiet", "-m", "init"]
          git kit ["tag", "v1"]
          let setUp proj env = do
                createDirectoryIfMissing True proj
                writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
                a <- runLaskEnv lask proj env ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
                resExit a `shouldBe` 0
                b <- runLaskEnv lask proj env ["sync"] ""
                resExit b `shouldBe` 0
                readFile (proj </> "lask.lock.json")
          lockDefault <- setUp defaultProj extra
          lockMoved <- setUp movedProj moved
          lockMoved `shouldBe` lockDefault
          lockDefault `shouldContain` "\"#.lask/deps/"
          -- And a lock written with the default cache holds under the
          -- moved one.
          writeFile (movedProj </> "lask.lock.json") lockDefault
          r <- runLaskEnv lask movedProj moved ["sync", "--frozen"] ""
          resExit r `shouldBe` 0

    it "reports a dependency whose own project file cannot be read, and writes nothing" $ \lask ->
      withSystemTempDirectory "lask-deps-transitive" $ \root -> do
        let kit = root </> "kit"
            proj = root </> "proj"
        createDirectoryIfMissing True kit
        writeFile (kit </> "lask.json") "{\"dependencies\": {\"util\": {\"git\": \"https://x/util\"}}}\n"
        writeFile (kit </> "main.lask") "hello(): String = \"hi\"\n"
        git kit ["init", "--quiet"]
        git kit ["add", "."]
        git kit ["commit", "--quiet", "-m", "init"]
        git kit ["tag", "v1"]
        createDirectoryIfMissing True proj
        writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf(): String = hello()\n"
        r <- runLaskEnv lask proj [("LASK_CACHE_DIR", root </> "cache")] ["deps", "add", "kit", "--git", "file://" <> kit, "--rev", "v1"] ""
        resExit r `shouldBe` 3
        resErr r `shouldSatisfy` isInfixOf "of dependency 'kit'"
        doesFileExist (proj </> "lask.json") `shouldReturn` False

    it "requires a source option for deps add (exit 4)" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["deps", "add", "kit"] ""
        resExit r `shouldSatisfy` (/= 0)

    it "refuses a deps add source that git would read as an option, and writes nothing" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        let marker = dir </> "pwned"
        r <- runLask lask dir ["deps", "add", "kit", "--git=--upload-pack=touch " <> marker, "--rev", "v1"] ""
        resExit r `shouldBe` 1
        resErr r `shouldContain` "must not start with '-'"
        doesFileExist marker `shouldReturn` False
        doesFileExist (dir </> "lask.json") `shouldReturn` False

    it "reports malformed dependency files with exit 1" $ \lask ->
      withProject
        [ ("main.lask", "a = 1\n"),
          ("lask.json", "{\"dependencies\": {\"kit\": {\"git\": \"https://x\"}}}")
        ]
        $ \dir -> do
          r <- runLask lask dir ["sync"] ""
          resExit r `shouldBe` 1
