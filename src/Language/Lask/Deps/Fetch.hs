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
  ( Pinned (..),
    ensureEntry,
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
import Language.Lask.Deps.Lock (LockEntry (..), childPath)
import Language.Lask.Deps.File
import Language.Lask.Deps.Hash (hashFile, hashTree)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (EIoEnvResolve, EModuleHashMismatch, EModuleRevMoved), Stage (StageIo))
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

-- | A dependency source: a repository at a reference, or a URL.
data DepSource = SrcGit Text Text | SrcUrl Text
  deriving (Show, Eq)

sourceOf :: DepEntry -> DepSource
sourceOf (DepGit u rev) = SrcGit u rev
sourceOf (DepUrl u) = SrcUrl u

-- | What an entry resolved to: the content hash of its source and, for
-- git, the commit that content was taken from.
data Pinned = Pinned
  { pinHash :: Text,
    pinRev :: Maybe Text
  }
  deriving (Show, Eq)

-- | Ensure a declared entry is present and verified in the cache,
-- given what the lock recorded for it (spec 5, 11.5).
--
-- A git entry whose lock pins a commit for the same reference is
-- checked against the remote first: a reference that now names another
-- commit is @E-MODULE-REV-MOVED@, whatever the cache holds. Otherwise
-- the pinned commit, not the reference, is what gets fetched, so the
-- hash check verifies what the lock pins.
--
-- A git entry with no pinned commit is always fetched, so that its
-- @rev@ and @hash@ come from one checkout. An entry already in the
-- content-addressed cache is otherwise not fetched again (presence
-- implies verification, 11.5), unless the declared reference differs
-- from the locked one: a changed @rev@ over an unchanged @hash@ must
-- not silently keep the old content.
--
-- A hash mismatch is @E-MODULE-HASH-MISMATCH@, and nothing is placed
-- in the cache.
ensureEntry :: FilePath -> Maybe LockEntry -> Text -> DepEntry -> IO (Either Diagnostic Pinned)
ensureEntry cacheDir locked name entry = case entry of
  DepUrl {} -> do
    present <- maybe (pure False) isCached expected
    case expected of
      Just h | present -> pure (Right (Pinned h Nothing))
      _ -> fetchVerified (sourceOf entry)
  DepGit url ref -> case pinnedRev of
    Nothing -> fetchVerified (sourceOf entry)
    Just old -> do
      names <- resolveGitRev url ref
      case names of
        Just (commit, aliases)
          | old `notElem` (commit : aliases) -> pure (Left (moved ref commit old))
        _ -> do
          -- An earlier lock may hold the tag object an annotated tag
          -- points through; it is re-pinned to the commit.
          let current = maybe old fst names
          present <- maybe (pure False) isCached expected
          case expected of
            Just h | present -> pure (Right (Pinned h (Just current)))
            _ -> fetchVerified (SrcGit url old)
    where
      sameRef = fmap lkRequested locked == Just (Just ref)
      pinnedRev = if sameRef then locked >>= lkRev else Nothing
  where
    expected = lkHash <$> locked
    isCached h = existsAny (cachePathFor cacheDir h (entryIsSingleFile entry))

    fetchVerified source = do
      r <- fetchToTemp cacheDir source
      case r of
        Left d -> pure (Left d)
        Right (tmpPath, computedHash, commit)
          | Just h <- expected,
            computedHash /= h -> do
              cleanup tmpPath
              pure . Left . mkDiagnostic EModuleHashMismatch StageIo NoSpan $
                "dependency '"
                  <> name
                  <> "': content hash mismatch (locked "
                  <> h
                  <> ", fetched "
                  <> computedHash
                  <> ")"
          | otherwise -> do
              let target = cachePathFor cacheDir computedHash (entryIsSingleFile entry)
              alreadyThere <- existsAny target
              if alreadyThere
                then cleanup tmpPath
                else moveInto tmpPath target
              pure (Right (Pinned computedHash commit))

    moved ref commit old =
      mkDiagnostic EModuleRevMoved StageIo NoSpan $
        "dependency '"
          <> name
          <> "': "
          <> ref
          <> " now resolves to "
          <> commit
          <> " (locked: "
          <> old
          <> ")"

-- | Sync all entries of a definition file, following the transitive
-- dependency files of fetched trees (spec chapter 5). Reports every
-- failure instead of stopping at the first. The dependency path of
-- each entry (@name@, @parent>child@) is reported alongside its
-- result so the caller can write the lock file.
syncAll ::
  FilePath ->
  -- | What the lock records for a dependency path, if anything.
  (Text -> Maybe LockEntry) ->
  DepsFile ->
  IO [(Text, DepEntry, Either Diagnostic Pinned)]
syncAll cacheDir locked rootDeps =
  go Set.empty [("" , name, entry) | (name, entry) <- Map.toList (depsEntries rootDeps)]
  where
    go _ [] = pure []
    go seen ((parent, name, entry) : rest)
      | path `Set.member` seen = go seen rest
      | otherwise = do
          r <- ensureEntry cacheDir (locked path) name entry
          case r of
            Left d -> ((path, entry, Left d) :) <$> go seen' rest
            Right p -> do
              transitive <- transitiveEntries (pinHash p) entry
              ((path, entry, Right p) :) <$> go seen' (rest <> transitive)
      where
        path = childPath parent name
        seen' = Set.insert path seen

    transitiveEntries :: Text -> DepEntry -> IO [(Text, Text, DepEntry)]
    transitiveEntries hash entry
      | entryIsSingleFile entry = pure []
      | otherwise = do
          let root = cachePathFor cacheDir hash False
          sub <- loadDepsFile (root </> defaultDepsFileName)
          pure $ case sub of
            Right (Just df) -> [("", n, e) | (n, e) <- Map.toList (depsEntries df)]
            _ -> []

-- Fetch primitives -----------------------------------------------------------

-- | Fetch a source into a fresh location under the cache directory
-- (same filesystem, so the final move is an atomic rename) and return
-- its computed content hash and, for git, the commit checked out.
fetchToTemp :: FilePath -> DepSource -> IO (Either Diagnostic (FilePath, Text, Maybe Text))
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
              commit <- runToolOut "git" ["-C", dest, "rev-parse", "HEAD"]
              hasGitDir <- doesDirectoryExist (dest </> ".git")
              when hasGitDir (removeDirectoryRecursive (dest </> ".git"))
              h <- hashTree dest
              keep <- promote tmp dest False
              pure (Right (keep, h, either (const Nothing) (Just . T.strip) commit))
    SrcUrl url
      | ".lask" `T.isSuffixOf` url -> do
          let dest = tmp </> "src.lask"
          r <- runTool "curl" ["-fsSL", T.unpack url, "-o", dest]
          case r of
            Left e -> pure (Left (fetchErr ("download failed for " <> url <> ": " <> e)))
            Right () -> do
              h <- hashFile dest
              keep <- promote tmp dest True
              pure (Right (keep, h, Nothing))
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
                  pure (Right (keep, h, Nothing))
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
runTool tool args = fmap (const ()) <$> runToolOut tool args

runToolOut :: String -> [String] -> IO (Either Text Text)
runToolOut tool args = do
  r <- try (readCreateProcessWithExitCode (proc tool args) "")
  pure $ case r of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right (ExitSuccess, out, _) -> Right (T.pack out)
    Right (ExitFailure n, _, err) ->
      Left (T.pack (show n) <> ": " <> T.strip (T.pack err))

-- | Resolve a git reference to the commit it currently names (spec
-- 11.5), with every other SHA the remote lists for it. For an
-- annotated tag the commit is the one the tag points through, and the
-- tag object itself is among the others. A reference that is already
-- a full SHA resolves to itself; one the remote does not know, or a
-- remote that cannot be reached, resolves to @Nothing@.
resolveGitRev :: Text -> Text -> IO (Maybe (Text, [Text]))
resolveGitRev url rev
  | isFullSha rev = pure (Just (rev, []))
  | otherwise = do
      r <- try (readCreateProcessWithExitCode (proc "git" ["ls-remote", T.unpack url, T.unpack rev, T.unpack rev <> "^{}"]) "")
      pure $ case r of
        Left e -> const Nothing (e :: IOException)
        Right (ExitSuccess, out, _) ->
          let listed = [(sha, ref) | (sha : ref : _) <- map T.words (T.lines (T.pack out)), isFullSha sha]
              peeled = [sha | (sha, ref) <- listed, "^{}" `T.isSuffixOf` ref]
              shas = map fst listed
           in case peeled <> shas of
                commit : _ -> Just (commit, filter (/= commit) shas)
                [] -> Nothing
        Right _ -> Nothing
  where
    isFullSha t = T.length t == 40 && T.all (`elem` ("0123456789abcdef" :: String)) t
