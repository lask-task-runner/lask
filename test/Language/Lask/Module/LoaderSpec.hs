{-# LANGUAGE OverloadedStrings #-}

-- | External import resolution (spec chapter 5) over a fully faked
-- loader environment: no filesystem, no network, no real cache.
module Language.Lask.Module.LoaderSpec (spec) where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.File
import Language.Lask.Deps.File (DepEntry (..), DepsFile (..))
import Language.Lask.Deps.Lock (LockEntry (..), LockFile (..))
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader
import Test.Hspec

-- | A hermetic loader environment over in-memory data.
fakeEnv :: [(FilePath, Text)] -> [(FilePath, DepsFile)] -> LoaderEnv
fakeEnv files depsFiles =
  LoaderEnv
    { leReader = \p -> pure (maybe (Left "not found") Right (lookup p files)),
      leLoadDeps = \dir -> pure (Right (lookup dir depsFiles)),
      -- The fixtures pin hashes directly, so the lock covers whatever
      -- the deps file declares.
      leLoadLock = \dir ->
        pure . Right . Just . LockFile (lockOf (lookup dir depsFiles)) $ Map.empty,
      leLockedHash = lockedHash,
      leCacheDir = "/cache",
      -- A path exists if it is a known file or a directory prefix of
      -- one (cache tree roots).
      leExists = \p -> pure (p `elem` paths || any (\f -> (p <> "/") `isPrefixOf` f) paths)
    }
  where
    paths = map fst files
    -- Fixtures name cache directories directly, so the lock pins the
    -- hash those paths encode. The last segment of a dependency path
    -- names the entry, whichever tree declared it.
    lockedHash p =
      let leaf = last (T.splitOn ">" p)
       in listToMaybe
            [ hashOf e
            | (_, df) <- depsFiles,
              Just e <- [Map.lookup leaf (depsEntries df)]
            ]

    lockOf Nothing = Map.empty
    -- The lock must agree with the deps file, not merely cover it.
    lockOf (Just df) =
      Map.fromList
        [ (n, lockEntryOf e)
        | (n, e) <- Map.toList (depsEntries df)
        ]
    lockEntryOf e = case e of
      DepGit u r -> LockEntry (Just u) Nothing (Just r) (Just r) (hashOf e)
      DepUrl u -> LockEntry Nothing (Just u) Nothing Nothing (hashOf e)
    -- The fixtures name cache directories directly, so the lock pins
    -- the hash those paths encode.
    hashOf e = case e of
      DepGit {} -> "sha256-t"
      DepUrl {} -> "sha256-abc"

load :: [(FilePath, Text)] -> [(FilePath, DepsFile)] -> FilePath -> IO (Either [ErrorCode] [FilePath])
load files depsFiles entry = do
  r <- loadProgramEnv (fakeEnv files depsFiles) entry
  pure $ case r of
    Left ds -> Left (map diagCode ds)
    Right prog -> Right (progOrder prog)

loadsOk :: [(FilePath, Text)] -> [(FilePath, DepsFile)] -> Expectation
loadsOk files depsFiles = do
  r <- load files depsFiles "main.lask"
  case r of
    Right _ -> pure ()
    Left cs -> expectationFailure ("expected success, got " <> show cs)

failsWith :: [(FilePath, Text)] -> [(FilePath, DepsFile)] -> ErrorCode -> Expectation
failsWith files depsFiles code = do
  r <- load files depsFiles "main.lask"
  case r of
    Left cs | code `elem` cs -> pure ()
    other -> expectationFailure ("expected " <> show code <> ", got " <> show other)

singleDep :: Text -> DepsFile
singleDep _hash = DepsFile (Map.fromList [("notify", DepUrl "https://x/notify.lask")])

treeDep :: Text -> DepsFile
treeDep _hash = DepsFile (Map.fromList [("kit", DepGit "https://x/kit" "v1")])

spec :: Spec
spec = do
  describe "local imports (spec chapter 5)" $ do
    it "resolves ./ relative to the importing module" $
      loadsOk
        [ ("main.lask", "import { a } from \"./sub/a.lask\"\nx = a"),
          ("sub/a.lask", "import { b } from \"./b.lask\"\na = b"),
          ("sub/b.lask", "b = 1")
        ]
        []
    it "resolves ../ upwards from the importing module" $
      loadsOk
        [ ("main.lask", "import { a } from \"./sub/a.lask\"\nx = a"),
          ("sub/a.lask", "import { lib } from \"../lib.lask\"\na = lib"),
          ("lib.lask", "lib = 1")
        ]
        []
    it "gives ../ and direct paths the same module identity" $ do
      -- lib.lask reached both directly and via sub/../ must be one
      -- module (no duplicate-symbol errors, stable DAG identity).
      r <-
        load
          [ ("main.lask", "import { lib } from \"./lib.lask\"\nimport { a } from \"./sub/a.lask\"\nx = a + lib"),
            ("sub/a.lask", "import { lib } from \"../lib.lask\"\na = lib"),
            ("lib.lask", "lib = 1")
          ]
          []
          "main.lask"
      case r of
        Right order -> length (filter (== "lib.lask") order) `shouldBe` 1
        Left cs -> expectationFailure (show cs)

  describe "external imports (spec chapter 5)" $ do
    it "resolves a single-file dependency by name" $
      loadsOk
        [ ("main.lask", "import { send } from \"notify\"\nf(): String = send(\"a\")"),
          ("/cache/sha256-abc.lask", "send(x: String): String = concat(\"sent:\", x)")
        ]
        [(".", singleDep "sha256-abc")]
    it "rejects submodule paths into single-file dependencies" $
      failsWith
        [ ("main.lask", "import { send } from \"notify/extra.lask\"\nf(): String = send(\"a\")"),
          ("/cache/sha256-abc.lask", "send(x: String): String = x")
        ]
        [(".", singleDep "sha256-abc")]
        EModuleUnresolved
    it "rejects a path inside a tree dependency (entry module only)" $
      failsWith
        [ ("main.lask", "import { rollout } from \"kit/deploy.lask\"\nf() = rollout()"),
          ("/cache/sha256-t/main.lask", "rollout(): String = \"ok\""),
          ("/cache/sha256-t/deploy.lask", "rollout(): String = \"ok\"")
        ]
        [(".", treeDep "sha256-t")]
        EModuleDeepImport
    it "resolves a bare tree name to main.lask (entry-point convention)" $
      loadsOk
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/main.lask", "hello(): String = \"hi\"")
        ]
        [(".", treeDep "sha256-t")]
    it "reports a bare name without main.lask as E-MODULE-UNRESOLVED" $
      failsWith
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/other.lask", "hello(): String = \"hi\"")
        ]
        [(".", treeDep "sha256-t")]
        EModuleUnresolved
    it "reports undeclared dependency names as E-MODULE-UNRESOLVED" $
      failsWith
        [("main.lask", "import { send } from \"notify\"\nf() = send(\"a\")")]
        []
        EModuleUnresolved
    it "reports uncached dependencies as E-MODULE-UNRESOLVED (no network)" $
      failsWith
        [("main.lask", "import { send } from \"notify\"\nf() = send(\"a\")")]
        [(".", singleDep "sha256-missing")]
        EModuleUnresolved
    it "resolves ./ imports inside an external tree" $
      loadsOk
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/main.lask", "import { u } from \"./util.lask\"\nhello(): Number = u"),
          ("/cache/sha256-t/util.lask", "u = 1")
        ]
        [(".", treeDep "sha256-t")]

  describe "transitive dependencies (spec chapter 5)" $ do
    let notifyEntry = DepUrl "https://x/notify.lask"
    it "resolves bare imports of an external tree against its own dependency file" $
      loadsOk
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/main.lask", "import { send } from \"notify\"\nhello(): String = send(\"x\")"),
          ("/cache/sha256-abc.lask", "send(x: String): String = x")
        ]
        [ (".", treeDep "sha256-t"),
          ("/cache/sha256-t", DepsFile (Map.fromList [("notify", notifyEntry)]))
        ]
    it "does not leak the root scope into external trees" $
      failsWith
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/main.lask", "import { send } from \"notify\"\nhello(): String = send(\"x\")"),
          ("/cache/sha256-abc.lask", "send(x: String): String = x")
        ]
        -- notify is declared at the ROOT only; the tree has no
        -- dependency file, so its bare import must not resolve.
        [(".", DepsFile (Map.fromList [("kit", DepGit "https://x/kit" "v1"), ("notify", notifyEntry)]))]
        EModuleUnresolved
    it "detects cycles inside external trees" $
      failsWith
        [ ("main.lask", "import { hello } from \"kit\"\nf() = hello()"),
          ("/cache/sha256-t/main.lask", "import { a } from \"./a.lask\"\nhello() = a"),
          ("/cache/sha256-t/a.lask", "import { hello } from \"./main.lask\"\na = hello")
        ]
        [(".", treeDep "sha256-t")]
        EModuleCycle
