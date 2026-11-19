{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The language server's compiles keep a module's parse and a
-- buffer's check while their sources are the same, and never past a
-- change.
module Language.Lask.EditorCompileSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Lask (Compiled (..), compileFile, compileText)
import Language.Lask.Deps.Hash (hashTree)
import Language.Lask.Diagnostic (Advisory, Diagnostic (..))
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.ErrorCode
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

codes :: Either [Diagnostic] a -> [ErrorCode]
codes = either (map diagCode) (const [])

-- | What a check found: its errors, or what a valid program's features
-- read from the core program.
data Outcome = Errors [Diagnostic] | Valid [(FilePath, Text)] [Advisory] [String]
  deriving (Show, Eq)

outcome :: Either [Diagnostic] Compiled -> Outcome
outcome (Left ds) = Errors ds
outcome (Right c) =
  let core = compiledCore c
   in Valid (Map.keys (cpDecls core)) (cpAdvisories core) (sort (map show (cpHover core)))

-- | A project whose dependency @kit@ sits in its cache as @lask sync@
-- leaves it, pinned by the lock; returns the entry module's path.
withProject :: Text -> (FilePath -> IO a) -> IO a
withProject kit act =
  withSystemTempDirectory "lask-editor" $ \dir -> do
    let staged = dir </> "staged"
    createDirectoryIfMissing True staged
    T.writeFile (staged </> "main.lask") kit
    hash <- hashTree staged
    let entry = dir </> ".lask" </> "deps" </> T.unpack hash
    createDirectoryIfMissing True entry
    T.writeFile (entry </> "main.lask") kit
    T.writeFile (dir </> "lask.json") "{\"dependencies\": {\"kit\": {\"git\": \"https://example.invalid/kit.git\", \"rev\": \"v1\"}}}\n"
    T.writeFile (dir </> "lask.lock.json") $
      "{\"lock_version\": 1, \"modules\": {\"kit\": {\"git\": \"https://example.invalid/kit.git\", \"hash\": \""
        <> hash
        <> "\", \"requested\": \"v1\", \"rev\": \""
        <> T.replicate 40 "b"
        <> "\"}}}\n"
    act (dir </> "main.lask")

kitSource :: Text
kitSource =
  T.unlines
    [ "export double(n: Number): Number = n * 2",
      "export greet(--name: String = \"World\"): String = \"hi #{name}\"",
      "export type Pair = Record<left: Number, right: Number>",
      "export command { \"go\" } on #golang:1.25 schema { \"go\": { root: { closed: true }, subcommands: { \"test\": {} } } }"
    ]

-- | Buffers as they are typed: valid, broken in each stage, valid again.
edits :: [Text]
edits =
  [ "import { double, greet, Pair } from \"kit\"\nimport command { \"go\" } from \"kit\"\nf(): Number = double(2)\n",
    "import { double, greet, Pair } from \"kit\"\nimport command { \"go\" } from \"kit\"\nf(): Number = doubl(2)\n",
    "import { double, greet, Pair } from \"kit\"\nimport command { \"go\" } from \"kit\"\nf(): String = double(2)\n",
    "import { double, greet, Pair } from \"kit\"\nimport command { \"go\" } from \"kit\"\nf(): String = greet(name = \"you\")\np: Pair = { left: 1, right: 2 }\nt() = $ go test\n",
    "import { double, greet, Pair } from \"kit\"\nimport command { \"go\" } from \"kit\"\nf(): Number = double(2)\n"
  ]

spec :: Spec
spec = describe "compileText" $ do
  -- The language server keeps the resolution and elaboration of a
  -- project's dependencies and checks only the project after each
  -- edit; what it finds must be what the command line finds.
  it "checks a buffer importing a dependency as the command line does, edit after edit" $
    withProject kitSource $ \entry -> do
      forM_ edits $ \buffer -> do
        editor <- outcome <$> compileText entry buffer
        T.writeFile entry buffer
        cli <- outcome <$> compileFile entry
        editor `shouldBe` cli
      -- The edits go from valid to invalid and back.
      found <- mapM (fmap (codes . fmap (const ())) . compileText entry) edits
      found `shouldBe` [[], [ENameUndefined], [ETypeMismatch], [], []]

  it "checks a buffer importing a broken dependency as the command line does" $
    withProject (kitSource <> "export broken(): Number = missing\n") $ \entry ->
      forM_ edits $ \buffer -> do
        editor <- outcome <$> compileText entry buffer
        T.writeFile entry buffer
        cli <- outcome <$> compileFile entry
        editor `shouldBe` cli
        editor `shouldSatisfy` \case
          Errors ds -> ENameUndefined `elem` map diagCode ds
          Valid {} -> False

  it "compiles a buffer again when a module it imports changes on disk" $
    withSystemTempDirectory "lask-editor" $ \dir -> do
      let entry = dir </> "main.lask"
          buffer = "import { greet } from \"./lib.lask\"\nhi(): String = greet()\n"
      T.writeFile (dir </> "lib.lask") "export greet(): String = \"hi\"\n"
      codes <$> compileText entry buffer `shouldReturn` []
      -- The same buffer, with the module it imports no longer exporting
      -- what it uses.
      T.writeFile (dir </> "lib.lask") "export greeting(): String = \"hi\"\n"
      codes <$> compileText entry buffer `shouldReturn` [ENameUndefined]
      T.writeFile (dir </> "lib.lask") "export greet(): String = \"hi\"\n"
      codes <$> compileText entry buffer `shouldReturn` []

  it "compiles the buffer as it is now, not as it was" $
    withSystemTempDirectory "lask-editor" $ \dir -> do
      let entry = dir </> "main.lask"
      codes <$> compileText entry "hi(): String = \"hi\"\n" `shouldReturn` []
      codes <$> compileText entry "hi(): String = 1\n" `shouldReturn` [ETypeMismatch]
      codes <$> compileText entry "hi(): String = \"hi\"\n" `shouldReturn` []
