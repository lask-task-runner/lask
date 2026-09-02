{-# LANGUAGE OverloadedStrings #-}

-- | Module loading (spec chapter 5): resolves the import graph from
-- an entry module, requiring it to be a DAG (@E-MODULE-CYCLE@) —
-- including across external dependencies.
--
-- Import paths starting with @./@ or @..\/@ are local imports,
-- resolved relative to the directory of the importing module. Any
-- other path is an external import: its first segment must be a
-- dependency name declared in @lask.json@ and present in
-- the cache (@E-MODULE-UNRESOLVED@ otherwise). Module resolution
-- never accesses the network; fetching is done by @lask deps sync@.
module Language.Lask.Module.Loader
  ( Program (..),
    LoadedModule (..),
    ModuleReader,
    LoaderEnv (..),
    loadProgram,
    loadProgramWith,
    loadProgramEnv,
    defaultLoaderEnv,
    fileReader,
    collapseDots,
  )
where

import Control.Exception (IOException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask.Deps.Cache (cacheDirFor, cachePathFor)
import Language.Lask.Deps.File
import Language.Lask.Deps.File (DepsFile (..), defaultDepsFileName, entryIsSingleFile, loadDepsFile)
import Language.Lask.Deps.Lock (LockFile (..), defaultLockFileName, loadLockFile)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic, withNote)
import Language.Lask.ErrorCode (ErrorCode (EModuleCycle, EModuleDeepImport, EModuleLockStale, EModuleUnresolved, ENameUndefined), Stage (StageStatic))
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseModule)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (joinPath, normalise, splitDirectories, takeDirectory, (</>))

-- | How to read module sources; injectable for hermetic tests.
type ModuleReader = FilePath -> IO (Either Text Text)

-- | All external capabilities of the loader, injectable for tests.
data LoaderEnv = LoaderEnv
  { leReader :: ModuleReader,
    -- | Load the dependency definition file of a tree root directory.
    leLoadDeps :: FilePath -> IO (Either Diagnostic (Maybe DepsFile)),
    -- | The lock file of the project (spec chapter 5). Resolution
    -- requires it to cover every declared dependency.
    leLoadLock :: FilePath -> IO (Either Diagnostic (Maybe LockFile)),
    leCacheDir :: FilePath,
    -- | Existence check for cache entries (files and directories).
    leExists :: FilePath -> IO Bool
  }

-- | The per-module resolution context: the module's directory (for
-- local imports) and its dependency scope — the definition file of
-- the tree the module belongs to (spec chapter 5: transitive
-- dependencies resolve independently per dependency).
data ModCtx = ModCtx
  { mcDir :: FilePath,
    mcDeps :: Maybe DepsFile
  }

data Program = Program
  { progEntry :: FilePath,
    progBaseDir :: FilePath,
    progModules :: Map FilePath LoadedModule,
    -- | Topological order, dependencies first; entry module last.
    progOrder :: [FilePath]
  }
  deriving (Show)

data LoadedModule = LoadedModule
  { lmPath :: FilePath,
    lmModule :: Module,
    -- | Import path -> resolved module key, in declaration order.
    lmImportKeys :: Map Text FilePath
  }
  deriving (Show)

fileReader :: ModuleReader
fileReader path = do
  r <- try (TIO.readFile path)
  pure $ case r of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right src -> Right src

-- | The entry path determines the project's base directory, and with
-- it the per-project dependency cache.
defaultLoaderEnv :: ModuleReader -> FilePath -> IO LoaderEnv
defaultLoaderEnv reader entryPath = do
  cacheDir <- cacheDirFor (takeDirectory entryPath)
  pure
    LoaderEnv
      { leReader = reader,
        leLoadDeps = \dir -> loadDepsFile (dir </> defaultDepsFileName),
        leLoadLock = \dir -> loadLockFile (dir </> defaultLockFileName),
        leCacheDir = cacheDir,
        leExists = \p -> (||) <$> doesFileExist p <*> doesDirectoryExist p
      }

loadProgram :: FilePath -> IO (Either [Diagnostic] Program)
loadProgram = loadProgramWith fileReader

loadProgramWith :: ModuleReader -> FilePath -> IO (Either [Diagnostic] Program)
loadProgramWith reader entryPath = do
  env <- defaultLoaderEnv reader entryPath
  loadProgramEnv env entryPath

loadProgramEnv :: LoaderEnv -> FilePath -> IO (Either [Diagnostic] Program)
loadProgramEnv env entryPath = do
  let entry = collapseDots (normalise entryPath)
      baseDir = takeDirectory entry
  lockE <- leLoadLock env baseDir
  rootDepsE <- leLoadDeps env baseDir
  case (rootDepsE, lockE) of
    (Left d, _) -> pure (Left [d])
    (_, Left d) -> pure (Left [d])
    (Right rootDeps, Right lock) -> case lockGap rootDeps lock of
      Just d -> pure (Left [d])
      Nothing -> do
        result <- go (ModCtx baseDir rootDeps) [] Map.empty [] entry
        pure $ case result of
          Left ds -> Left ds
          Right (mods, order) ->
            Right
              Program
                { progEntry = entry,
                  progBaseDir = baseDir,
                  progModules = mods,
                  progOrder = reverse order
                }
  where
    -- Every declared dependency must be covered by the lock file
    -- (spec chapter 5); resolution never falls back to resolving one.
    lockGap rootDeps lock = case rootDeps of
      Nothing -> Nothing
      Just df ->
        let declared = Map.keys (depsEntries df)
            covered = maybe Map.empty lockModules lock
            missing = [n | n <- declared, not (Map.member n covered)]
         in case (lock, missing) of
              (Nothing, (_ : _)) ->
                Just . stale $ "no lock file; run 'lask deps sync'"
              (_, (n : _)) ->
                Just . stale $
                  "the lock file does not cover dependency '" <> n <> "'; run 'lask deps sync'"
              _ -> Nothing

    stale = mkDiagnostic EModuleLockStale StageStatic NoSpan

    -- DFS with an explicit visiting stack for cycle detection.
    -- The accumulated order is reversed topological order.
    go ::
      ModCtx ->
      [FilePath] ->
      Map FilePath LoadedModule ->
      [FilePath] ->
      FilePath ->
      IO (Either [Diagnostic] (Map FilePath LoadedModule, [FilePath]))
    go ctx visiting done order path
      | path `Map.member` done = pure (Right (done, order))
      | path `elem` visiting =
          pure . Left . pure $
            withNote
              (T.pack ("cycle: " <> showCycle path visiting))
              (mkDiagnostic EModuleCycle StageStatic NoSpan "circular module import")
      | otherwise = do
          srcOrErr <- leReader env path
          case srcOrErr of
            Left err ->
              pure . Left . pure $
                withNote err $
                  mkDiagnostic
                    ENameUndefined
                    StageStatic
                    NoSpan
                    (T.pack ("cannot load module: " <> path))
            Right src -> case parseModule path src of
              Left d -> pure (Left [d])
              Right m -> do
                let importPaths = [p | Decl _ f <- moduleDecls m, p <- declImportPath f]
                resolvedE <- resolveAll ctx importPaths
                case resolvedE of
                  Left d -> pure (Left [d])
                  Right resolved ->
                    loadAll (path : visiting) done order resolved >>= \r -> pure $ do
                      (done', order') <- r
                      let lm =
                            LoadedModule
                              { lmPath = path,
                                lmModule = m,
                                lmImportKeys = Map.fromList [(p, k) | (p, k, _) <- resolved]
                              }
                      pure (Map.insert path lm done', path : order')

    resolveAll _ [] = pure (Right [])
    resolveAll ctx (p : rest) = do
      r <- resolveImport env ctx p
      case r of
        Left d -> pure (Left d)
        Right (key, childCtx) -> fmap ((p, key, childCtx) :) <$> resolveAll ctx rest

    loadAll _ done order [] = pure (Right (done, order))
    loadAll visiting done order ((_, key, childCtx) : rest) = do
      r <- go childCtx visiting done order key
      case r of
        Left ds -> pure (Left ds)
        Right (done', order') -> loadAll visiting done' order' rest

    showCycle path visiting =
      let chain = reverse (path : takeWhile (/= path) visiting) <> [path]
       in foldr1 (\a b -> a <> " -> " <> b) chain

-- | Resolve one import path (spec chapter 5): local (@./@\/@..\/@)
-- relative to the importing module's directory, keeping the same
-- dependency scope; otherwise external, against the importer's
-- dependency definition file and the cache.
resolveImport :: LoaderEnv -> ModCtx -> Text -> IO (Either Diagnostic (FilePath, ModCtx))
resolveImport env ctx pathText
  | isLocal = do
      let file = collapseDots (normalise (mcDir ctx </> T.unpack pathText))
      pure (Right (file, ctx {mcDir = takeDirectory file}))
  | otherwise = do
      let (depName, rest) = T.breakOn "/" pathText
      case mcDeps ctx >>= Map.lookup depName . depsEntries of
        Nothing ->
          pure . Left . unresolved $
            "undeclared dependency: '" <> depName <> "' (declare it in " <> T.pack defaultDepsFileName <> ")"
        Just entry -> do
          let base = cachePathFor (leCacheDir env) entry
          present <- leExists env base
          if not present
            then
              pure . Left . unresolved $
                "dependency '" <> depName <> "' is not in the cache; run 'lask deps sync'"
            else
              if entryIsSingleFile entry
                then
                  if T.null rest
                    then pure (Right (base, ModCtx (takeDirectory base) Nothing))
                    else
                      pure . Left . unresolved $
                        "dependency '" <> depName <> "' is a single file and has no submodules"
                else
                  -- Only the entry module of a tree is reachable from
                  -- outside it (spec 5): a public API spanning several
                  -- files is re-exported from main.lask.
                  if not (T.null rest)
                    then
                      pure . Left . deepImport $
                        "'" <> pathText <> "' names a path inside dependency '"
                          <> depName
                          <> "'; only its entry module is importable"
                    else do
                  let relPath = "main.lask"
                      file = collapseDots (normalise (base </> relPath))
                  fileOk <- leExists env file
                  if not fileOk
                    then pure . Left . unresolved $ "dependency '" <> depName <> "' has no main.lask"
                    else do
                      treeDepsE <- leLoadDeps env base
                      case treeDepsE of
                        Left d -> pure (Left d)
                        Right treeDeps -> pure (Right (file, ModCtx (takeDirectory file) treeDeps))
  where
    isLocal = "./" `T.isPrefixOf` pathText || "../" `T.isPrefixOf` pathText
    unresolved = mkDiagnostic EModuleUnresolved StageStatic NoSpan
    deepImport = mkDiagnostic EModuleDeepImport StageStatic NoSpan

-- | Collapse @.@ and @..@ segments so module identities are stable
-- for cycle detection and caching.
collapseDots :: FilePath -> FilePath
collapseDots p =
  let segs = splitDirectories p
      step acc s
        | s == "." = acc
        | s == "..",
          (t : rest) <- acc,
          t /= "..",
          t /= "/",
          t /= "." =
            rest
        | otherwise = s : acc
      collapsed = reverse (foldl step [] segs)
   in case collapsed of
        [] -> "."
        _ -> joinPath collapsed

declImportPath :: DeclF -> [Text]
declImportPath (DImportNamed _ p) = [p]
declImportPath (DImportNamespace _ p) = [p]
declImportPath (DExportFrom _ p) = [p]
declImportPath _ = []
