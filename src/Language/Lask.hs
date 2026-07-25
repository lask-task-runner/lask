-- | Front-end facade: lexing, parsing, module loading, name
-- resolution and elaboration in one call.
module Language.Lask
  ( Compiled (..),
    compileFile,
    compileWith,
    checkText,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic)
import Language.Lask.Elaborate (CoreProgram, elaborateProgram)
import Language.Lask.Module.Loader (ModuleReader, Program, fileReader, loadProgramWith)
import Language.Lask.Module.Resolve (GlobalScope, validateProgram)
import System.FilePath (normalise)

data Compiled = Compiled
  { compiledProgram :: Program,
    compiledScopes :: Map FilePath GlobalScope,
    compiledCore :: CoreProgram
  }

compileFile :: FilePath -> IO (Either [Diagnostic] Compiled)
compileFile = compileWith fileReader

compileWith :: ModuleReader -> FilePath -> IO (Either [Diagnostic] Compiled)
compileWith reader entry = do
  r <- loadProgramWith reader entry
  pure $ do
    prog <- r
    scopes <- validateProgram prog
    core <- elaborateProgram prog scopes
    pure (Compiled prog scopes core)

-- | Diagnostics for an in-editor document: the entry module's text is
-- provided directly; imported modules are read from disk.
checkText :: FilePath -> Text -> IO [Diagnostic]
checkText path txt = either id (const []) <$> compileWith reader path
  where
    reader p
      | normalise p == normalise path = pure (Right txt)
      | otherwise = fileReader p
