{-# LANGUAGE OverloadedStrings #-}

-- | The dependency cache (spec chapter 5, 11.5): an
-- implementation-defined, content-addressed store keyed by the
-- declared content hash. Only @deps sync@\/@deps add@ write to it
-- (after verification), so presence in the cache implies a verified
-- source; module resolution never touches the network.
module Language.Lask.Deps.Cache
  ( defaultCacheDir,
    cachePathFor,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.File (DepEntry, entryHash, entryIsSingleFile)
import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | @LASK_CACHE_DIR@ overrides the XDG cache location
-- (@~\/.cache\/lask\/deps@); the override exists for hermetic tests
-- and CI.
defaultCacheDir :: IO FilePath
defaultCacheDir = do
  override <- lookupEnv "LASK_CACHE_DIR"
  case override of
    Just dir | not (null dir) -> pure dir
    _ -> getXdgDirectory XdgCache ("lask" </> "deps")

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
