-- | Front-end facade: lexing, parsing, module loading, name
-- resolution and elaboration in one call.
module Language.Lask
  ( Compiled (..),
    Partial (..),
    compileFile,
    compileFilePartial,
    compileWith,
    compileText,
    compileTextPartial,
    checkText,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic)
import Language.Lask.Elaborate (CoreProgram, elaborateProgram)
import Language.Lask.Module.Loader (LoadedModule (..), ModuleReader, Program (..), collapseDots, fileReader, loadProgramWith)
import Language.Lask.Module.Resolve (GlobalScope, buildScopes, validateProgram)
import Language.Lask.Syntax.AST (Module)
import Language.Lask.Syntax.Parser (parseModule)
import System.FilePath (normalise)

data Compiled = Compiled
  { compiledProgram :: Program,
    compiledScopes :: Map FilePath GlobalScope,
    compiledCore :: CoreProgram
  }

-- | Whatever the front end managed to produce for a document that
-- does not compile: each stage is kept only if the ones before it
-- succeeded.
data Partial = Partial
  { partialModule :: Maybe Module,
    partialProgram :: Maybe Program,
    partialScopes :: Map FilePath GlobalScope,
    partialCore :: Maybe CoreProgram
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

-- | Compile an in-editor document: the entry module's text is
-- provided directly; imported modules are read from disk.
compileText :: FilePath -> Text -> IO (Either [Diagnostic] Compiled)
compileText path txt = compileWith (textReader path txt) path

-- | Like 'compileText' but keeps the results of the stages that did
-- succeed, for editor features that must work on a buffer being typed.
compileTextPartial :: FilePath -> Text -> IO Partial
compileTextPartial path txt = do
  loaded <- loadProgramWith (textReader path txt) path
  pure $ case loaded of
    Left _ ->
      Partial
        { partialModule = either (const Nothing) Just (parseModule path txt),
          partialProgram = Nothing,
          partialScopes = Map.empty,
          partialCore = Nothing
        }
    Right prog ->
      let scopes = buildScopes prog
       in Partial
            { partialModule = lmModule <$> Map.lookup (progEntry prog) (progModules prog),
              partialProgram = Just prog,
              partialScopes = scopes,
              partialCore = either (const Nothing) Just (elaborateProgram prog scopes)
            }

-- | Like 'compileFile' but keeps whatever the front end managed to
-- produce, together with the diagnostics. CLI help is rendered from
-- the surface syntax even when the module does not type check
-- (spec 11.6), which is exactly when help is most needed.
compileFilePartial :: FilePath -> IO ([Diagnostic], Partial)
compileFilePartial path = do
  loaded <- loadProgramWith fileReader path
  case loaded of
    -- The module graph did not load (syntax error, unresolved import):
    -- parse the entry module on its own so its declarations are still
    -- available.
    Left ds -> do
      src <- fileReader path
      let m = case src of
            Right txt -> either (const Nothing) Just (parseModule path txt)
            Left _ -> Nothing
      pure
        ( ds,
          Partial
            { partialModule = m,
              partialProgram = Nothing,
              partialScopes = Map.empty,
              partialCore = Nothing
            }
        )
    Right prog -> do
      let (ds, scopes, core) = case validateProgram prog of
            Left vds -> (vds, buildScopes prog, Nothing)
            Right sc -> case elaborateProgram prog sc of
              Left eds -> (eds, sc, Nothing)
              Right c -> ([], sc, Just c)
      pure
        ( ds,
          Partial
            { partialModule = lmModule <$> Map.lookup (progEntry prog) (progModules prog),
              partialProgram = Just prog,
              partialScopes = scopes,
              partialCore = core
            }
        )

-- | The loader keys modules by their collapsed, normalised path, so
-- the buffer has to be matched the same way; otherwise a path such as
-- @./main.lask@ misses and the stale file on disk is read instead.
textReader :: FilePath -> Text -> ModuleReader
textReader path txt p
  | collapseDots (normalise p) == collapseDots (normalise path) = pure (Right txt)
  | otherwise = fileReader p

-- | Diagnostics for an in-editor document.
checkText :: FilePath -> Text -> IO [Diagnostic]
checkText path txt = either id (const []) <$> compileText path txt
