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
import Language.Lask.Confirm (validateConfirm)
import Language.Lask.Diagnostic (Diagnostic, settleDiagnostics)
import Language.Lask.Elaborate (CoreProgram, elaborateRecovering)
import Language.Lask.Module.Loader (LoadedModule (..), ModuleReader, Program (..), collapseDots, fileReader, loadProgramWith)
import Language.Lask.Module.Resolve (GlobalScope, resolveProgram)
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
  pure $ case r of
    Left ds -> Left (settleDiagnostics ds)
    Right prog -> case checkProgram prog of
      ([], scopes, core) ->
        -- The project file's confirmations refer to the program; one
        -- that no longer does is an error, never silently dropped (spec 5).
        case validateConfirm prog scopes core of
          [] -> Right (Compiled prog scopes core)
          ds -> Left (settleDiagnostics ds)
      (ds, _, _) -> Left ds

-- | Name resolution and elaboration of a loaded program. Every
-- independent error is reported (spec 14.3): a declaration that holds
-- an error of name resolution is not elaborated, and the others still
-- are. The core program holds the declarations that did elaborate.
checkProgram :: Program -> ([Diagnostic], Map FilePath GlobalScope, CoreProgram)
checkProgram prog =
  let (nameDs, scopes) = resolveProgram prog
      (typeDs, core) = elaborateRecovering prog scopes nameDs
   in (settleDiagnostics (nameDs <> typeDs), scopes, core)

-- | Compile an in-editor document: the entry module's text is
-- provided directly; imported modules are read from disk.
compileText :: FilePath -> Text -> IO (Either [Diagnostic] Compiled)
compileText path txt = compileWith (textReader path txt) path

-- | Like 'compileText' but keeps the results of the stages that did
-- succeed, for editor features that must work on a buffer being typed.
-- The core program holds every declaration that elaborated, so the
-- rest of a module keeps working while one declaration is broken.
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
      let (_, scopes, core) = checkProgram prog
       in Partial
            { partialModule = lmModule <$> Map.lookup (progEntry prog) (progModules prog),
              partialProgram = Just prog,
              partialScopes = scopes,
              partialCore = Just core
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
        ( settleDiagnostics ds,
          Partial
            { partialModule = m,
              partialProgram = Nothing,
              partialScopes = Map.empty,
              partialCore = Nothing
            }
        )
    Right prog -> do
      let (ds, scopes, c) = checkProgram prog
          core = if null ds then Just c else Nothing
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
