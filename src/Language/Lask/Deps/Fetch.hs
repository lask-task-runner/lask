{-# LANGUAGE OverloadedStrings #-}

-- | Fetching and verifying external dependencies (spec chapter 5,
-- 11.5). This module is the only place that accesses the network for
-- module resolution; it is used exclusively by @lask deps sync@ and
-- @lask deps add@.
--
-- Sources are fetched with the @git@ and @curl@ CLIs (the same
-- shell-out policy as SSH and Docker execution); archives are
-- unpacked with @tar@.
module Language.Lask.Deps.Fetch
  ( DepSource (..),
    ensureEntry,
    fetchAndStore,
    syncAll,
    resolveGitRev,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.Cache (cachePathFor)
import Language.Lask.Deps.Lock (childPath)
import Language.Lask.Deps.File
import Language.Lask.Deps.Hash (hashFile, hashTree)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (EIoEnvResolve, EModuleHashMismatch), Stage (StageIo))
import Language.Lask.Span (Span (NoSpan))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
    renameFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

-- | A dependency source without a pinned hash (for @deps add@).
data DepSource = SrcGit Text Text | SrcUrl Text
  deriving (Show, Eq)

sourceOf :: DepEntry -> DepSource
sourceOf (DepGit u rev _) = SrcGit u rev
sourceOf (DepUrl u _) = SrcUrl u

-- | Ensure a declared entry is present and verified in the cache.
-- Already-cached entries are skipped without network access
-- (content-addressed store: presence implies verification, 11.5),
-- unless @recheck@ is set: a declared reference that differs from the
-- one the lock recorded must be fetched again, or a changed @rev@ over
-- an unchanged @hash@ would silently keep the old content.
-- A hash mismatch is @E-MODULE-HASH-MISMATCH@ and nothing is placed
-- in the cache.
ensureEntry :: FilePath -> Bool -> Text -> DepEntry -> IO (Either Diagnostic ())
ensureEntry cacheDir recheck name entry = do
  let target = cachePathFor cacheDir entry
  present <- existsAny target
  if present && not recheck
    then pure (Right ())
    else do
      r <- fetchToTemp cacheDir (sourceOf entry)
      case r of
        Left d -> pure (Left d)
        Right (tmpPath, computedHash)
          | computedHash /= entryHash entry -> do
              cleanup tmpPath
              pure . Left . mkDiagnostic EModuleHashMismatch StageIo NoSpan $
                "dependency '"
                  <> name
                  <> "': content hash mismatch (declared "
                  <> entryHash entry
                  <> ", fetched "
                  <> computedHash
                  <> ")"
          | otherwise -> do
              moveInto tmpPath target
              pure (Right ())

-- | Fetch a source, compute its content hash, and place it in the
-- content-addressed cache (for @deps add@, 11.5: trust on first use).
fetchAndStore :: FilePath -> DepSource -> IO (Either Diagnostic Text)
fetchAndStore cacheDir source = do
  r <- fetchToTemp cacheDir source
  case r of
    Left d -> pure (Left d)
    Right (tmpPath, computedHash) -> do
      let entry = case source of
            SrcGit u rev -> DepGit u rev computedHash
            SrcUrl u -> DepUrl u computedHash
          target = cachePathFor cacheDir entry
      present <- existsAny target
      if present
        then cleanup tmpPath >> pure (Right computedHash)
        else moveInto tmpPath target >> pure (Right computedHash)

-- | Sync all entries of a definition file, following the transitive
-- dependency files of fetched trees (spec chapter 5). Reports every
-- failure instead of stopping at the first. The dependency path of
-- each entry (@name@, @parent>child@) is reported alongside its
-- result so the caller can write the lock file.
syncAll :: FilePath -> (Text -> DepEntry -> Bool) -> DepsFile -> IO [(Text, DepEntry, Either Diagnostic ())]
syncAll cacheDir needsRecheck rootDeps =
  go Set.empty [("" , name, entry) | (name, entry) <- Map.toList (depsEntries rootDeps)]
  where
    go _ [] = pure []
    go seen ((parent, name, entry) : rest)
      | entryHash entry `Set.member` seen = go seen rest
      | otherwise = do
          let seen' = Set.insert (entryHash entry) seen
              path = childPath parent name
          r <- ensureEntry cacheDir (needsRecheck path entry) name entry
          case r of
            Left d -> ((path, entry, Left d) :) <$> go seen' rest
            Right () -> do
              transitive <- transitiveEntries path entry
              ((path, entry, Right ()) :) <$> go seen' (rest <> transitive)

    transitiveEntries :: Text -> DepEntry -> IO [(Text, Text, DepEntry)]
    transitiveEntries path entry
      | entryIsSingleFile entry = pure []
      | otherwise = do
          let root = cachePathFor cacheDir entry
          sub <- loadDepsFile (root </> defaultDepsFileName)
          pure $ case sub of
            Right (Just df) -> [(path, n, e) | (n, e) <- Map.toList (depsEntries df)]
            _ -> []

-- Fetch primitives -----------------------------------------------------------

-- | Fetch a source into a fresh location under the cache directory
-- (same filesystem, so the final move is an atomic rename) and return
-- its computed content hash.
fetchToTemp :: FilePath -> DepSource -> IO (Either Diagnostic (FilePath, Text))
fetchToTemp cacheDir source = do
  createDirectoryIfMissing True cacheDir
  withTempDirectory cacheDir ".fetch" $ \tmp -> case source of
    SrcGit url rev -> do
      let dest = tmp </> "src"
      r1 <- runTool "git" ["clone", "--quiet", T.unpack url, dest]
      case r1 of
        Left e -> pure (Left (fetchErr ("git clone failed for " <> url <> ": " <> e)))
        Right () -> do
          r2 <- runTool "git" ["-C", dest, "checkout", "--quiet", T.unpack rev]
          case r2 of
            Left e -> pure (Left (fetchErr ("git checkout " <> rev <> " failed: " <> e)))
            Right () -> do
              hasGitDir <- doesDirectoryExist (dest </> ".git")
              when hasGitDir (removeDirectoryRecursive (dest </> ".git"))
              h <- hashTree dest
              keep <- promote tmp dest False
              pure (Right (keep, h))
    SrcUrl url
      | ".lask" `T.isSuffixOf` url -> do
          let dest = tmp </> "src.lask"
          r <- runTool "curl" ["-fsSL", T.unpack url, "-o", dest]
          case r of
            Left e -> pure (Left (fetchErr ("download failed for " <> url <> ": " <> e)))
            Right () -> do
              h <- hashFile dest
              keep <- promote tmp dest True
              pure (Right (keep, h))
      | otherwise -> do
          let archive = tmp </> "archive"
              extractDir = tmp </> "extract"
          r <- runTool "curl" ["-fsSL", T.unpack url, "-o", archive]
          case r of
            Left e -> pure (Left (fetchErr ("download failed for " <> url <> ": " <> e)))
            Right () -> do
              createDirectoryIfMissing True extractDir
              r2 <- runTool "tar" ["-xzf", archive, "-C", extractDir]
              case r2 of
                Left e -> pure (Left (fetchErr ("cannot unpack archive from " <> url <> ": " <> e)))
                Right () -> do
                  -- The conventional single top-level directory of an
                  -- archive becomes the tree root.
                  entries <- listDirectory extractDir
                  root <- case entries of
                    [one] -> do
                      isDir <- doesDirectoryExist (extractDir </> one)
                      pure (if isDir then extractDir </> one else extractDir)
                    _ -> pure extractDir
                  h <- hashTree root
                  keep <- promote tmp root False
                  pure (Right (keep, h))
  where
    -- withTempDirectory deletes the temp dir on exit; move the result
    -- out to a sibling location first.
    promote tmp path isFile = do
      let keep = tmp <> ".ready"
      if isFile then renameFile path keep else renameDirectory path keep
      pure keep

    fetchErr = mkDiagnostic EIoEnvResolve StageIo NoSpan

-- | Move a verified fetch result into its content-addressed location.
moveInto :: FilePath -> FilePath -> IO ()
moveInto from target = do
  createDirectoryIfMissing True (takeDirectory target)
  isDir <- doesDirectoryExist from
  present <- existsAny target
  unless present $
    if isDir then renameDirectory from target else renameFile from target
  stillThere <- existsAny from
  when stillThere (cleanup from)

cleanup :: FilePath -> IO ()
cleanup path = do
  r <- try $ do
    isDir <- doesDirectoryExist path
    if isDir
      then removeDirectoryRecursive path
      else do
        fileThere <- doesFileExist path
        when fileThere (removeFile path)
  pure (either (\e -> let _ = (e :: IOException) in ()) id r)

existsAny :: FilePath -> IO Bool
existsAny p = (||) <$> doesFileExist p <*> doesDirectoryExist p

runTool :: String -> [String] -> IO (Either Text ())
runTool tool args = do
  r <- try (readCreateProcessWithExitCode (proc tool args) "")
  pure $ case r of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right (ExitSuccess, _, _) -> Right ()
    Right (ExitFailure n, _, err) ->
      Left (T.pack (show n) <> ": " <> T.strip (T.pack err))

-- | Resolve a git reference to the commit SHA it currently names
-- (spec 11.5). A reference that is already a full SHA resolves to
-- itself; anything the remote does not know resolves to @Nothing@.
resolveGitRev :: Text -> Text -> IO (Maybe Text)
resolveGitRev url rev
  | isFullSha rev = pure (Just rev)
  | otherwise = do
      r <- try (readCreateProcessWithExitCode (proc "git" ["ls-remote", T.unpack url, T.unpack rev]) "")
      pure $ case r of
        Left e -> const Nothing (e :: IOException)
        Right (ExitSuccess, out, _) -> case T.words (T.pack out) of
          (sha : _) | isFullSha sha -> Just sha
          _ -> Nothing
        Right _ -> Nothing
  where
    isFullSha t = T.length t == 40 && T.all (`elem` ("0123456789abcdef" :: String)) t
