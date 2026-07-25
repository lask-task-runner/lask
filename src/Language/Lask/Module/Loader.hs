{-# LANGUAGE OverloadedStrings #-}

-- | Module loading (spec chapter 5): resolves the import graph from
-- an entry module, requiring it to be a DAG (@E-MODULE-CYCLE@).
--
-- Import paths are resolved relative to the base directory, i.e. the
-- directory of the entry module (the same directory used for
-- @environments.lask.json@ resolution, spec 10.3).
module Language.Lask.Module.Loader
  ( Program (..),
    LoadedModule (..),
    ModuleReader,
    loadProgram,
    loadProgramWith,
    fileReader,
  )
where

import Control.Exception (IOException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic, withNote)
import Language.Lask.ErrorCode (ErrorCode (EModuleCycle, ENameUndefined), Stage (StageStatic))
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseModule)
import System.FilePath (normalise, takeDirectory, (</>))

-- | How to read module sources; injectable for hermetic tests.
type ModuleReader = FilePath -> IO (Either Text Text)

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

loadProgram :: FilePath -> IO (Either [Diagnostic] Program)
loadProgram = loadProgramWith fileReader

loadProgramWith :: ModuleReader -> FilePath -> IO (Either [Diagnostic] Program)
loadProgramWith reader entryPath = do
  let entry = normalise entryPath
      baseDir = takeDirectory entry
  result <- go baseDir [] Map.empty [] entry
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
    -- DFS with an explicit visiting stack for cycle detection.
    -- The accumulated order is reversed topological order.
    go ::
      FilePath ->
      [FilePath] ->
      Map FilePath LoadedModule ->
      [FilePath] ->
      FilePath ->
      IO (Either [Diagnostic] (Map FilePath LoadedModule, [FilePath]))
    go baseDir visiting done order path
      | path `Map.member` done = pure (Right (done, order))
      | path `elem` visiting =
          pure . Left . pure $
            withNote
              (T.pack ("cycle: " <> showCycle path visiting))
              (mkDiagnostic EModuleCycle StageStatic NoSpan "circular module import")
      | otherwise = do
          srcOrErr <- reader path
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
                    resolved = [(p, normalise (baseDir </> T.unpack p)) | p <- importPaths]
                loadAll baseDir (path : visiting) done order resolved >>= \r -> pure $ do
                  (done', order') <- r
                  let lm =
                        LoadedModule
                          { lmPath = path,
                            lmModule = m,
                            lmImportKeys = Map.fromList resolved
                          }
                  pure (Map.insert path lm done', path : order')

    loadAll _ _ done order [] = pure (Right (done, order))
    loadAll baseDir visiting done order ((_, key) : rest) = do
      r <- go baseDir visiting done order key
      case r of
        Left ds -> pure (Left ds)
        Right (done', order') -> loadAll baseDir visiting done' order' rest

    showCycle path visiting =
      let chain = reverse (path : takeWhile (/= path) visiting) <> [path]
       in foldr1 (\a b -> a <> " -> " <> b) chain

declImportPath :: DeclF -> [Text]
declImportPath (DImportNamed _ p) = [p]
declImportPath (DImportNamespace _ p) = [p]
declImportPath _ = []
