{-# LANGUAGE OverloadedStrings #-}

-- | Content hashes for external dependencies (spec chapter 5).
--
-- Format: @sha256-\<hex\>@. A single file hashes its bytes; a source
-- tree hashes the sorted sequence of (relative POSIX path, content)
-- pairs — an implementation-defined canonical form (permissions,
-- symlinks and empty directories do not participate).
module Language.Lask.Deps.Hash
  ( hashBytes,
    hashFile,
    hashTree,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

hashBytes :: BS.ByteString -> Text
hashBytes bytes = "sha256-" <> TE.decodeUtf8 (B16.encode (SHA256.hash bytes))

hashFile :: FilePath -> IO Text
hashFile path = hashBytes <$> BS.readFile path

-- | Deterministic hash of a source tree rooted at @dir@. @.git@
-- directories are excluded (they are removed on fetch, but excluding
-- them here keeps the hash stable either way).
hashTree :: FilePath -> IO Text
hashTree dir = do
  files <- sort <$> walk ""
  ctx <-
    foldl
      (\ioCtx rel -> ioCtx >>= \c -> update c rel)
      (pure SHA256.init)
      files
  pure ("sha256-" <> TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))
  where
    walk :: FilePath -> IO [FilePath]
    walk rel = do
      let abs' = if null rel then dir else dir </> rel
      names <- listDirectory abs'
      fmap concat . mapM step $ [n | n <- names, n /= ".git"]
      where
        step name = do
          let relPath = if null rel then name else rel </> name
          isDir <- doesDirectoryExist (dir </> relPath)
          if isDir then walk relPath else pure [relPath]

    -- Each file contributes sha256(path) followed by sha256(content),
    -- making the pair sequence unambiguous.
    update ctx rel = do
      content <- BS.readFile (dir </> rel)
      let pathBytes = TE.encodeUtf8 (T.replace "\\" "/" (T.pack rel))
      pure (SHA256.update (SHA256.update ctx (SHA256.hash pathBytes)) (SHA256.hash content))
