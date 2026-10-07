{-# LANGUAGE OverloadedStrings #-}

-- | The dependency cache (spec chapter 5, 11.5): a per-project,
-- content-addressed store keyed by the declared content hash. Only
-- @sync@\/@deps add@ write to it (after verification), and an
-- entry is verified again wherever it is used ('holdsPinned'): being in
-- the cache is not taken as proof of content. Module resolution never
-- touches the network.
--
-- The store lives under @.lask\/deps@ in the project's base directory
-- for module resolution, so a project carries its own dependencies and
-- can be copied (or moved to a machine without network access) whole.
module Language.Lask.Deps.Cache
  ( cacheDirFor,
    cachePathFor,
    holdsPinned,
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.Hash (hashFile, hashTree)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
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

-- | Content-addressed location of a fetched source: a @.lask@ file for
-- single-file dependencies, a directory for source trees. The key is
-- the content hash, which the lock file supplies (spec chapter 5).
cachePathFor :: FilePath -> Text -> Bool -> FilePath
cachePathFor cacheDir hash singleFile
  | singleFile = cacheDir </> hashKey <> ".lask"
  | otherwise = cacheDir </> hashKey
  where
    hashKey = T.unpack (sanitize hash)
    sanitize :: Text -> Text
    sanitize = T.map (\c -> if c == '/' || c == '\\' then '_' else c)

-- | Whether a cache entry holds the content its hash names. The cache
-- may be shared (@LASK_CACHE_DIR@) or written by something other than
-- @sync@, so an entry is checked where it is used rather than
-- trusted for being there. The entry itself must not be a symbolic
-- link: the hash would then describe wherever it points.
holdsPinned :: FilePath -> Text -> Bool -> IO Bool
holdsPinned path hash singleFile = do
  r <- try $ do
    isLink <- pathIsSymbolicLink path
    present <- if singleFile then doesFileExist path else doesDirectoryExist path
    if isLink || not present
      then pure False
      else (== hash) <$> (if singleFile then hashFile path else hashTree path)
  pure (either (\e -> const False (e :: IOException)) id r)
