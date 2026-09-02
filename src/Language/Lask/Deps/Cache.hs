{-# LANGUAGE OverloadedStrings #-}

-- | The dependency cache (spec chapter 5, 11.5): a per-project,
-- content-addressed store keyed by the declared content hash. Only
-- @deps sync@\/@deps add@ write to it (after verification), so presence
-- in the cache implies a verified source; module resolution never
-- touches the network.
--
-- The store lives under @.lask\/deps@ in the project's base directory
-- for module resolution, so a project carries its own dependencies and
-- can be copied (or moved to a machine without network access) whole.
module Language.Lask.Deps.Cache
  ( cacheDirFor,
    cachePathFor,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.File (DepEntry, entryHash, entryIsSingleFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | The cache directory for a project, given its base directory for
-- module resolution: @\<base\>\/.lask\/deps@. @LASK_CACHE_DIR@
-- overrides it; the override exists for hermetic tests and CI.
cacheDirFor :: FilePath -> IO FilePath
cacheDirFor baseDir = do
  override <- lookupEnv "LASK_CACHE_DIR"
  case override of
    Just dir | not (null dir) -> pure dir
    _ -> pure (baseDir </> ".lask" </> "deps")

-- | Content-addressed location of an entry: a @.lask@ file for
-- single-file dependencies, a directory for source trees.
cachePathFor :: FilePath -> DepEntry -> FilePath
cachePathFor cacheDir entry
  | entryIsSingleFile entry = cacheDir </> hashKey <> ".lask"
  | otherwise = cacheDir </> hashKey
  where
    hashKey = T.unpack (sanitize (entryHash entry))
    sanitize :: Text -> Text
    sanitize = T.map (\c -> if c == '/' || c == '\\' then '_' else c)
