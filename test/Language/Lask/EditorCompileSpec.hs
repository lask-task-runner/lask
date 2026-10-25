{-# LANGUAGE OverloadedStrings #-}

-- | The language server's compiles keep a module's parse and a
-- buffer's check while their sources are the same, and never past a
-- change.
module Language.Lask.EditorCompileSpec (spec) where

import qualified Data.Text.IO as T
import Language.Lask (compileText)
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.ErrorCode
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

codes :: Either [Diagnostic] a -> [ErrorCode]
codes = either (map diagCode) (const [])

spec :: Spec
spec = describe "compileText" $ do
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
