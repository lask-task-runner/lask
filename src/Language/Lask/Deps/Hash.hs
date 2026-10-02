{-# LANGUAGE OverloadedStrings #-}

-- | Content hashes for external dependencies (spec chapter 5).
--
-- Format: @sha256-\<hex\>@. A single file hashes its bytes; a source
-- tree hashes the sorted sequence of (relative POSIX path, content)
-- pairs — an implementation-defined canonical form (permissions and
-- empty directories do not participate). A symbolic link is never
-- followed: it contributes its path and the text it points to, so a
-- tree's hash covers nothing outside the tree.
module Language.Lask.Deps.Hash
  ( hashBytes,
    hashFile,
    hashTree,
    symlinksUnder,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))

hashBytes :: BS.ByteString -> Text
hashBytes bytes = "sha256-" <> TE.decodeUtf8 (B16.encode (SHA256.hash bytes))

hashFile :: FilePath -> IO Text
hashFile path = hashBytes <$> BS.readFile path

-- | Deterministic hash of a source tree rooted at @dir@.
hashTree :: FilePath -> IO Text
hashTree dir = do
  entries <- sort <$> walkTree dir
  ctx <-
    foldl
      (\ioCtx e -> ioCtx >>= \c -> update c e)
      (pure SHA256.init)
      entries
  pure ("sha256-" <> TE.decodeUtf8 (B16.encode (SHA256.finalize ctx)))
  where
    -- Each file contributes sha256(path) followed by sha256(content),
    -- making the pair sequence unambiguous. A link contributes its
    -- target in place of content, marked so that it cannot be taken
    -- for a file holding the same text.
    update ctx (rel, isLink) = do
      content <-
        if isLink
          then ("symlink\0" <>) . TE.encodeUtf8 . T.pack <$> getSymbolicLinkTarget (dir </> rel)
          else BS.readFile (dir </> rel)
      let pathBytes = TE.encodeUtf8 (T.replace "\\" "/" (T.pack rel))
      pure (SHA256.update (SHA256.update ctx (SHA256.hash pathBytes)) (SHA256.hash content))

-- | The symbolic links in a tree, relative to its root.
symlinksUnder :: FilePath -> IO [FilePath]
symlinksUnder dir = map fst . filter snd <$> walkTree dir

-- | Every file and symbolic link under a root, relative to it, with
-- whether it is a link. A link is listed and never followed, so the
-- walk stays inside the tree. @.git@ directories are excluded (they
-- are removed on fetch, but excluding them here keeps the hash stable
-- either way).
walkTree :: FilePath -> IO [(FilePath, Bool)]
walkTree dir = walk ""
  where
    walk rel = do
      let abs' = if null rel then dir else dir </> rel
      names <- listDirectory abs'
      fmap concat . mapM step $ [n | n <- names, n /= ".git"]
      where
        step name = do
          let relPath = if null rel then name else rel </> name
          isLink <- pathIsSymbolicLink (dir </> relPath)
          isDir <- doesDirectoryExist (dir </> relPath)
          if isLink
            then pure [(relPath, True)]
            else if isDir then walk relPath else pure [(relPath, False)]
