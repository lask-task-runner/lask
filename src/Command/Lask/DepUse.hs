{-# LANGUAGE OverloadedStrings #-}

-- | Where a project imports its dependencies (spec 11.5, 11.7): what
-- @lask sync --prune@ removes, what @lask deps rm@ refuses to remove,
-- and what @lask deps list@ reports as unused.
module Command.Lask.DepUse
  ( ImportSite (..),
    projectImports,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask.Deps.File (defaultDepsFileName)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseModule)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

-- | An import of a dependency: its name, the file, and the line.
data ImportSite = ImportSite
  { isDep :: Text,
    isFile :: FilePath,
    isLine :: Int
  }
  deriving (Show, Eq)

-- | Every import of a dependency in the project's @.lask@ files: every
-- file under the project directory, not only the modules the entry
-- module reaches, so that a module run with @--module@ keeps what it
-- imports. The module cache and other hidden directories,
-- @node_modules@, and a subdirectory that is a project of its own
-- (with its own @lask.json@) are not searched.
--
-- A dependency is imported by @import ... from "name"@ (named or as a
-- namespace), @import command { ... } from "name"@, or a re-export
-- from it. A file that does not parse is searched for @from "name"@ as
-- text instead, so that it still keeps what it names.
projectImports :: FilePath -> IO [ImportSite]
projectImports base = do
  files <- lakFiles base True base
  concat <$> mapM sitesIn files
  where
    sitesIn file = do
      r <- try (TIO.readFile file)
      case r of
        Left e -> const (pure []) (e :: IOException)
        Right src -> pure $ case parseModule file src of
          Right m ->
            [ ImportSite (depName p) rel (lineOf (declSpan d))
            | d <- moduleDecls m,
              Just p <- [importPath (declF d)],
              isExternal p
            ]
          Left _ ->
            [ ImportSite (depName p) rel n
            | (n, l) <- zip [1 ..] (T.lines src),
              p <- quotedAfterFrom l,
              isExternal p
            ]
          where
            rel = makeRelative base file

    importPath f = case f of
      DImportNamed _ p -> Just p
      DImportNamespace _ p -> Just p
      DExportFrom _ p -> Just p
      DImportCommands _ p -> Just p
      DExportCommandsFrom _ p -> Just p
      _ -> Nothing

    lineOf sp = case sp of
      Span (Position _ l _) _ -> l
      NoSpan -> 0

-- | The @.lask@ files under a directory, in order.
lakFiles :: FilePath -> Bool -> FilePath -> IO [FilePath]
lakFiles base isRoot dir = do
  ownProject <- doesFileExist (dir </> defaultDepsFileName)
  if ownProject && not isRoot
    then pure []
    else do
      names <- sort <$> listDirectory dir
      let paths = [dir </> n | n <- names, take 1 n /= ".", n /= "node_modules"]
      dirs <- filterM doesDirectoryExist paths
      files <- filterM doesFileExist [p | p <- paths, takeExtension p == ".lask"]
      nested <- mapM (lakFiles base False) dirs
      pure (files <> concat nested)

-- | An import path naming a dependency: not relative, not absolute.
isExternal :: Text -> Bool
isExternal p = not (any (`T.isPrefixOf` p) ["./", "../", "/"]) && not (T.null p)

-- | The dependency an external import names: its first segment, so
-- that a deep import still counts as a use of it.
depName :: Text -> Text
depName = T.takeWhile (/= '/')

-- | The strings quoted after @from@ on a line.
quotedAfterFrom :: Text -> [Text]
quotedAfterFrom l = case T.breakOn "from \"" l of
  (_, rest)
    | T.null rest -> []
    | otherwise ->
        let after = T.drop 6 rest
            (p, more) = T.breakOn "\"" after
         in p : quotedAfterFrom (T.drop 1 more)
