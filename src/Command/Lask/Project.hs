{-# LANGUAGE OverloadedStrings #-}

-- | The project's dependencies and images (spec 11.4, 11.5, 11.7):
-- @lask sync@, @lask deps list | graph | add | rm@ and
-- @lask envs list@. Each lists what it is about in the same tables
-- (spec 11.3: the tables on stdout, progress and warnings on stderr).
module Command.Lask.Project
  ( cmdSync,
    cmdDepsAdd,
    cmdDepsList,
    cmdDepsGraph,
    cmdDepsRm,
    cmdEnvsList,
  )
where

import Command.Lask.Common
import Command.Lask.DepUse (ImportSite (..), projectImports)
import Command.Lask.Envs (HeadImage (..), HeadUse (..), Requirer (..), collectHeadUses)
import Command.Lask.Images
import Language.Lask.Runtime.Image (daemonReachable)
import Command.Lask.Options (CommonOpts (..), DepsAddSource (..))
import Command.Lask.Progress (finish, note, section, start, withProgress)
import Command.Lask.Table
import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Either (isRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Deps.Cache (cacheDirFor, cachePathFor, holdsPinned)
import Language.Lask.Deps.Fetch (Pinned (..), syncAll)
import Language.Lask.Deps.File
import Language.Lask.Deps.Lock
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.ErrorCode
import Language.Lask.Utils (kebabToSnake)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import System.IO (stderr, stdout)

-- Paths ------------------------------------------------------------------------

data Paths = Paths
  { pBase :: FilePath,
    pDeps :: FilePath,
    pLock :: FilePath,
    pCache :: FilePath,
    -- | Whether the cache is the project's own (@.lask/deps@) rather
    -- than one @LASK_CACHE_DIR@ names, which other projects may share:
    -- only the project's own is cleaned of what it no longer needs.
    pOwnCache :: Bool
  }

projectPaths :: CommonOpts -> IO Paths
projectPaths opts = do
  let base = takeDirectory (optModule opts)
  cache <- cacheDirFor base
  override <- lookupEnv "LASK_CACHE_DIR"
  pure (Paths base (base </> defaultDepsFileName) (base </> defaultLockFileName) cache (maybe True null override))

loadDepsOrExit :: CommonOpts -> Paths -> IO (Maybe DepsFile)
loadDepsOrExit opts paths = do
  r <- loadDepsFile (pDeps paths)
  case r of
    Left d -> diagExit opts d 1
    Right m -> pure m

loadLockMaybe :: Paths -> IO (Maybe LockFile)
loadLockMaybe paths = either (const Nothing) id <$> loadLockFile (pLock paths)

diagExit :: CommonOpts -> Diagnostic -> Int -> IO a
diagExit opts d code = do
  TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
  exitWith (ExitFailure code)

-- The lock ---------------------------------------------------------------------

-- | What the lock records for a dependency path.
lockedEntry :: Maybe LockFile -> Text -> Maybe LockEntry
lockedEntry lock path = Map.lookup path (maybe Map.empty lockModules lock)

-- | The module section of the lock, from a sync in which every entry
-- resolved.
lockedModules :: [(Text, DepEntry, Either Diagnostic Pinned)] -> Map.Map Text LockEntry
lockedModules results =
  Map.fromList
    [ (p, base {lkRev = pinRev pinned <|> lkRev base})
    | (p, e, Right pinned) <- results,
      let base = lockEntryOf e (pinHash pinned)
    ]

-- | The lock record of a declared entry (spec chapter 5). @requested@
-- keeps the reference that was written; @rev@ is filled in here only
-- when that reference is already a full commit SHA, and otherwise from
-- the commit the fetch checked out.
lockEntryOf :: DepEntry -> Text -> LockEntry
lockEntryOf (DepGit u r) h =
  LockEntry (Just u) Nothing (Just r) (if isFullSha r then Just r else Nothing) h
lockEntryOf (DepUrl u) h = LockEntry Nothing (Just u) Nothing Nothing h

isFullSha :: Text -> Bool
isFullSha r = T.length r == 40 && T.all (`elem` ("0123456789abcdef" :: String)) r

-- | A dependency and every path under it.
underPath :: Text -> Text -> Bool
underPath name path = path == name || (name <> ">") `T.isPrefixOf` path

-- How things are shown ---------------------------------------------------------

-- | The source of an entry: @git github.com/org/repo@, @url example.com/x.lask@.
sourceText :: DepEntry -> Text
sourceText e = case e of
  DepGit u _ -> "git " <> bare u
  DepUrl u -> "url " <> bare u

lockSourceText :: LockEntry -> Text
lockSourceText l = case (lkGit l, lkUrl l) of
  (Just u, _) -> "git " <> bare u
  (_, Just u) -> "url " <> bare u
  _ -> "—"

bare :: Text -> Text
bare u =
  let noScheme = maybe u snd (lookup True [(True, (s, rest)) | s <- ["https://", "http://", "file://"], Just rest <- [T.stripPrefix s u]])
   in fromMaybe noScheme (T.stripSuffix ".git" noScheme)

requestedOf :: DepEntry -> Text
requestedOf e = case e of
  DepGit _ r -> r
  DepUrl _ -> "—"

shortRev :: Maybe Text -> Text
shortRev = maybe "—" (T.take 7)

dash :: Maybe Text -> Text
dash = fromMaybe "—"

-- | Where a head is written, for the REQUIRED BY column: the module —
-- a file of the project by its path, a dependency by its name — the
-- declaration, and the parameter it is the default of.
requirerLabel :: Paths -> Maybe LockFile -> Requirer -> Text
requirerLabel paths lock rq = moduleLabel <> ": " <> rqWhat rq <> maybe "" (\k -> " (default --" <> k <> ")") (rqDefault rq)
  where
    modulePath = rqModule rq
    segments = T.splitOn "/" (T.pack modulePath)
    hashes = [s | s <- segments, "sha256-" `T.isPrefixOf` s]
    byHash = Map.fromList [(lkHash e, p) | (p, e) <- Map.toList (maybe Map.empty lockModules lock)]
    moduleLabel = case [p | h <- hashes, Just p <- [Map.lookup h byHash]] of
      p : _ -> p
      [] -> T.pack (makeRelative (pBase paths) modulePath)

-- | The REQUIRED BY cell: two places, and how many more.
requiredCell :: [Text] -> Text
requiredCell labels = case nub (sort labels) of
  ls | length ls > 2 -> T.intercalate ", " (take 2 ls) <> ", +" <> T.pack (show (length ls - 2)) <> " more"
  ls -> T.intercalate ", " ls

-- | The environment column of an image: its head, and the platforms
-- other than the daemon's own it is used with.
environmentLabel :: ImageRow -> Text
environmentLabel row = case irSource row of
  FromRegistry _ ps | any isJust ps -> irHead row <> " (" <> T.intercalate ", " (map (fromMaybe "native") ps) <> ")"
  _ -> irHead row

printTable :: CommonOpts -> [Text] -> [[Cell]] -> IO ()
printTable opts header rows = do
  color <- colorFor (optNoColor opts) stdout
  mapM_ TIO.putStrLn (renderTable color header rows)

printJson :: A.Value -> IO ()
printJson = TIO.putStrLn . encodeJsonText

plural :: Int -> Text -> Text
plural n w = T.pack (show n) <> " " <> w <> (if n == 1 then "" else "s")

-- | Counts of each status, in the order first seen: @6 pulled, 2 present@.
tally :: [Text] -> Text
tally statuses =
  T.intercalate ", " [T.pack (show n) <> " " <> s | s <- nub statuses, let n = length (filter (== s) statuses)]

-- sync -------------------------------------------------------------------------

-- | How one module ended in @lask sync@.
data ModuleRow = ModuleRow
  { mrPath :: Text,
    mrSource :: Text,
    mrRequested :: Text,
    mrRev :: Maybe Text,
    mrHash :: Maybe Text,
    mrStatus :: Text,
    mrTone :: Tone,
    mrSeconds :: Maybe Double
  }

-- | @lask sync@ (spec 11.7): fetch and verify every declared dependency
-- (including transitive ones) into the cache, then materialize every
-- image the program requires, and record both in the lock, reporting
-- each step as it goes and listing every module and image at the end.
-- The only subcommand allowed to access the network or to start a
-- build. With @--prune@, a dependency no module of the project
-- imports is removed first.
cmdSync :: CommonOpts -> Bool -> Bool -> IO ()
cmdSync opts frozen prune = do
  began <- getCurrentTime
  paths <- projectPaths opts
  mDf <- loadDepsOrExit opts paths
  prior <- loadLockMaybe paths
  sites <- projectImports (pBase paths)
  let declared = maybe Map.empty depsEntries mDf
      imported = Set.fromList (map isDep sites)
      unused = [n | n <- Map.keys declared, not (n `Set.member` imported)]
      removing = if prune then unused else []
  -- Under --frozen nothing is written, so a dependency to prune makes
  -- the files out of date.
  when (frozen && not (null removing)) $ do
    TIO.hPutStrLn stderr $
      codeText EModuleLockStale <> ": unused dependencies would be removed: " <> T.intercalate ", " removing <> " (--frozen)"
    exitWith (ExitFailure 1)
  pcolor <- colorFor (optNoColor opts) stderr
  withProgress (optJsonFormat opts) pcolor $ \pg -> do
    -- Modules.
    section pg "Modules"
    let kept = Map.filterWithKey (\k _ -> k `notElem` removing) declared
        df = maybe emptyDepsFile (\d -> d {depsEntries = kept}) mDf
        pruned =
          [ ModuleRow n (sourceText e) (requestedOf e) (lockedEntry prior n >>= lkRev) (lkHash <$> lockedEntry prior n) "pruned" ToneWarn Nothing
          | (n, e) <- Map.toList declared,
            n `elem` removing
          ]
    mapM_ (\r -> note pg ("  - " <> mrPath r <> "  pruned: no module imports it")) pruned
    timings <- newIORef Map.empty
    let around path entry act = do
          it <- start pg "deps" Nothing path ("resolving " <> sourceText entry <> "@" <> requestedOf entry)
          r <- act
          secs <- case r of
            Right p
              | pinFetched p -> finish it True ("fetched " <> shortRev (pinRev p))
              | otherwise -> finish it True "present"
            Left d -> finish it False ("failed: " <> codeText (diagCode d))
          modifyIORef' timings (Map.insert path secs)
          pure r
    results <-
      if Map.null kept
        then pure []
        else syncAll (pCache paths) (lockedEntry prior) around df
    secsBy <- readIORef timings
    let moduleRows =
          pruned
            <> [ case status of
                   Right p ->
                     ModuleRow path (sourceText e) (requestedOf e) (pinRev p <|> (lockedEntry prior path >>= lkRev)) (Just (pinHash p)) (if pinFetched p then "fetched" else "present") ToneOk (Map.lookup path secsBy)
                   Left d ->
                     ModuleRow path (sourceText e) (requestedOf e) Nothing Nothing ("failed: " <> codeText (diagCode d)) ToneBad (Map.lookup path secsBy)
               | (path, e, status) <- results
               ]
        modulesOk = all (isRight . third) results
        newModules = lockedModules results
        baseLock = LockFile newModules (maybe Map.empty lockImages prior)
        changedModules = Just newModules /= fmap lockModules prior
    unless modulesOk $ do
      summary opts began moduleRows [] "lask.lock.json unchanged"
      mapM_ (\(p, d) -> TIO.hPutStrLn stderr (p <> ": " <> renderDiagsLines (optJsonFormat opts) [d])) [(p, d) | (p, _, Left d) <- results]
      exitWith (ExitFailure 3)
    when (frozen && changedModules) $ do
      summary opts began moduleRows [] "lask.lock.json unchanged"
      TIO.hPutStrLn stderr (codeText EModuleLockStale <> ": the lock file is out of date (--frozen)")
      exitWith (ExitFailure 1)
    -- The modules are written before the images are resolved: reading
    -- the program needs them locked.
    unless frozen $ do
      when (prune && not (null removing)) $
        BL.writeFile (pDeps paths) (renderDepsFile df)
      BL.writeFile (pLock paths) (renderLockFile baseLock)
    -- Images.
    compiledE <- compileFile (optModule opts)
    compiled <- case compiledE of
      Right c -> pure c
      Left ds -> do
        -- A program that does not compile keeps its modules synced; its
        -- images cannot be enumerated.
        summary opts began moduleRows [] "lask.lock.json updated (modules only)"
        TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) ds)
        exitWith (ExitFailure 1)
    let core = compiledCore compiled
    (images, mats) <- materialize pg core (lockImages baseLock)
    let updated = baseLock {lockImages = images}
        changed = Just updated /= prior
    when (frozen && updated /= baseLock) $ do
      imageSummary <- imageSummaryRows paths (Just updated) mats
      summary opts began moduleRows imageSummary "lask.lock.json unchanged"
      TIO.hPutStrLn stderr (codeText EModuleLockStale <> ": the images in the lock file are out of date (--frozen)")
      exitWith (ExitFailure 1)
    unless frozen $ when (updated /= baseLock) $ BL.writeFile (pLock paths) (renderLockFile updated)
    -- The cache entries of the modules pruned, and of those under them.
    when (prune && pOwnCache paths && not frozen) $
      cleanCache paths (Set.fromList (map lkHash (Map.elems (lockModules updated)))) (map lkHash (Map.elems (maybe Map.empty lockModules prior)))
    imageSummary <- imageSummaryRows paths (Just updated) mats
    let written
          | frozen = "lask.lock.json unchanged"
          | prune && not (null removing) = "lask.json and lask.lock.json updated"
          | changed = "lask.lock.json updated"
          | otherwise = "lask.lock.json unchanged"
    summary opts began moduleRows imageSummary written
    let dropped = Map.keys (maybe Map.empty lockImages prior) `minus` Map.keys images
    unless (null dropped) $
      note pg ("images no longer required (kept on the Docker daemon): " <> T.intercalate ", " (map (T.drop 1) dropped))
    unless prune $
      mapM_
        (\n -> note pg ("warning: '" <> n <> "' is declared in lask.json but no module imports it (lask sync --prune removes it)"))
        unused
    let failures = [(irHead (matRow m), e) | m <- mats, Failed e <- [matOutcome m]]
    mapM_ (\(h, e) -> TIO.hPutStrLn stderr (shortPath h <> ": " <> e)) failures
    if null failures then exitSuccess else exitWith (ExitFailure 3)
  where
    third (_, _, c) = c
    minus a b = [x | x <- a, x `notElem` b]

-- | One image of the summary: its row, as the table shows it.
data ImageLine = ImageLine
  { ilEnvironment :: Text,
    ilKind :: Text,
    ilPinned :: Maybe Text,
    ilRequiredBy :: [Text],
    ilStatus :: Text,
    ilTone :: Tone,
    ilSeconds :: Maybe Double
  }

imageSummaryRows :: Paths -> Maybe LockFile -> [Materialized] -> IO [ImageLine]
imageSummaryRows paths lock mats =
  pure
    [ ImageLine
        (environmentLabel (matRow m))
        (irKind (matRow m))
        (matPinned m)
        (map (requirerLabel paths lock) (irRequiredBy (matRow m)))
        status
        tone
        (Just (matSeconds m))
    | m <- mats,
      let (status, tone) = case matOutcome m of
            Pulled -> ("pulled", ToneOk)
            AlreadyPresent -> ("present", ToneOk)
            Built -> ("built", ToneOk)
            Failed e -> ("failed: " <> T.takeWhile (/= ':') e, ToneBad)
    ]

-- | The tables @lask sync@ ends with, and the line under them.
summary :: CommonOpts -> UTCTime -> [ModuleRow] -> [ImageLine] -> Text -> IO ()
summary opts began modules images written = do
  now <- getCurrentTime
  let total = realToFrac (diffUTCTime now began) :: Double
  if optJsonFormat opts
    then
      printJson $
        A.object
          [ ("modules", A.toJSON (map moduleJson modules)),
            ("images", A.toJSON (map imageJson images)),
            ("lock", A.String written),
            ("seconds", A.toJSON total)
          ]
    else do
      TIO.putStrLn "Modules"
      if null modules
        then TIO.putStrLn "  (no dependencies declared)"
        else
          printTable
            opts
            ["NAME", "SOURCE", "REQUESTED", "REV", "HASH", "STATUS", "TIME"]
            [ [plain (mrPath r), plain (mrSource r), plain (mrRequested r), plain (shortRev (mrRev r)), plain (maybe "—" shortDigest (mrHash r)), toned (mrTone r) (mrStatus r), plain (maybe "" formatSeconds (mrSeconds r))]
            | r <- modules
            ]
      TIO.putStrLn ""
      TIO.putStrLn "Images"
      if null images
        then TIO.putStrLn "  (no images required)"
        else
          printTable
            opts
            ["ENVIRONMENT", "KIND", "PINNED", "REQUIRED BY", "STATUS", "TIME"]
            [ [plain (shortPath (ilEnvironment l)), plain (ilKind l), plain (maybe "—" shortDigest (ilPinned l)), plain (requiredCell (ilRequiredBy l)), toned (ilTone l) (ilStatus l), plain (maybe "" formatSeconds (ilSeconds l))]
            | l <- images
            ]
      TIO.putStrLn ""
      let mods = [mrStatus r | r <- modules]
          imgs = [T.takeWhile (/= ':') (ilStatus l) | l <- images]
      TIO.putStrLn $
        plural (length modules) "module"
          <> (if null mods then "" else " (" <> tally (map (T.takeWhile (/= ':')) mods) <> ")")
          <> ", "
          <> plural (length images) "image"
          <> (if null imgs then "" else " (" <> tally imgs <> ")")
          <> " in "
          <> formatSeconds total
          <> " — "
          <> written
  where
    moduleJson r =
      A.object
        [ ("name", A.String (mrPath r)),
          ("source", A.String (mrSource r)),
          ("requested", A.String (mrRequested r)),
          ("rev", maybe A.Null A.String (mrRev r)),
          ("hash", maybe A.Null A.String (mrHash r)),
          ("status", A.String (mrStatus r)),
          ("seconds", maybe A.Null A.toJSON (mrSeconds r))
        ]
    imageJson l =
      A.object
        [ ("environment", A.String (ilEnvironment l)),
          ("kind", A.String (ilKind l)),
          ("pinned", maybe A.Null A.String (ilPinned l)),
          ("required_by", A.toJSON (ilRequiredBy l)),
          ("status", A.String (ilStatus l)),
          ("seconds", maybe A.Null A.toJSON (ilSeconds l))
        ]

-- | Remove cache entries no remaining module uses. Only hashes of
-- modules just removed are considered, so a cache entry the lock never
-- named is left alone.
cleanCache :: Paths -> Set.Set Text -> [Text] -> IO ()
cleanCache paths keep hashes =
  mapM_
    ( \h -> unless (h `Set.member` keep) $ do
        let dir = cachePathFor (pCache paths) h False
            file = cachePathFor (pCache paths) h True
        isDir <- doesDirectoryExist dir
        isFileEntry <- doesFileExist file
        _ <- try (when isDir (removeDirectoryRecursive dir)) :: IO (Either IOException ())
        _ <- try (when isFileEntry (removeFile file)) :: IO (Either IOException ())
        pure ()
    )
    hashes

-- deps add ---------------------------------------------------------------------

-- | @lask deps add@: declare the entry, then resolve the whole project
-- file the way @sync@ does. The new entry is pinned on first use:
-- its content hash and, for git, the commit it came from. Every other
-- entry is verified against what the lock already pins, and the lock's
-- images are kept. Nothing is written unless every entry resolves.
cmdDepsAdd :: CommonOpts -> Text -> DepsAddSource -> IO ()
cmdDepsAdd opts name source = do
  unless (isLowerIdent name) $
    usageError opts ("dependency name must be a lower-case identifier: '" <> name <> "'")
  paths <- projectPaths opts
  let entry = case source of
        AddGit url rev -> DepGit url rev
        AddUrl url -> DepUrl url
  -- The entry reaches git and curl without passing through the project
  -- file's parser, so it is checked the same way here.
  either (\d -> diagExit opts d 1) pure (validateSource name entry)
  existing <- fromMaybe emptyDepsFile <$> loadDepsOrExit opts paths
  prior <- loadLockMaybe paths
  let updated = existing {depsEntries = Map.insert name entry (depsEntries existing)}
      -- The entry being added, and whatever it pulled in before, is
      -- resolved afresh; the rest keeps its pins.
      locked path = if underPath name path then Nothing else lockedEntry prior path
  results <- syncAll (pCache paths) locked (\_ _ act -> act) updated
  let failures = [(p, d) | (p, _, Left d) <- results]
  unless (null failures) $ do
    mapM_ (\(p, d) -> TIO.hPutStrLn stderr (p <> ": " <> renderDiagsLines (optJsonFormat opts) [d])) failures
    exitWith (ExitFailure 3)
  BL.writeFile (pDeps paths) (renderDepsFile updated)
  BL.writeFile (pLock paths) . renderLockFile $
    LockFile (lockedModules results) (maybe Map.empty lockImages prior)
  case [pinHash p | (path, _, Right p) <- results, path == name] of
    hash : _ -> TIO.putStrLn (name <> " " <> hash)
    [] -> pure ()
  exitSuccess
  where
    isLowerIdent t = case T.uncons t of
      Just (c, rest) -> (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest
      Nothing -> False
    identChar c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

-- deps list --------------------------------------------------------------------

-- | One dependency as @lask deps list@ shows it.
data DepRow = DepRow
  { drPath :: Text,
    drSource :: Text,
    drRequested :: Maybe Text,
    drLocked :: Maybe LockEntry,
    drStatus :: Text,
    drTone :: Tone
  }

-- | @lask deps list@ (spec 11.5): every dependency, direct and
-- transitive, with what @lask.json@ requests, what the lock pins, and
-- whether the two agree, the cache holds it, and a module imports it.
-- Reads the project file, the lock and the cache; no network.
cmdDepsList :: CommonOpts -> IO ()
cmdDepsList opts = do
  paths <- projectPaths opts
  mDf <- loadDepsOrExit opts paths
  lockE <- loadLockFile (pLock paths)
  lock <- case lockE of
    Left d -> diagExit opts d 1
    Right l -> pure l
  sites <- projectImports (pBase paths)
  let declared = maybe Map.empty depsEntries mDf
      locked = maybe Map.empty lockModules lock
      imported = Set.fromList (map isDep sites)
      allPaths = Set.toList (Set.fromList (Map.keys declared <> Map.keys locked))
  rows <- forM allPaths $ \path -> do
    let l = Map.lookup path locked
        (parent, name) = case T.breakOnEnd ">" path of
          ("", n) -> (Nothing, n)
          (p, n) -> (Just (T.dropEnd 1 p), n)
    -- What declares it: lask.json for a direct dependency, the
    -- project file of its parent's fetched tree for a transitive one.
    declaredBy <- case parent of
      Nothing -> pure (Right (Map.lookup name declared))
      Just p -> case Map.lookup p locked of
        Nothing -> pure (Left ())
        Just pl -> do
          let root = cachePathFor (pCache paths) (lkHash pl) False
          present <- doesDirectoryExist root
          if not present
            then pure (Left ())
            else do
              sub <- loadDepsFile (root </> defaultDepsFileName)
              pure $ case sub of
                Right m -> Right (m >>= Map.lookup name . depsEntries)
                Left _ -> Left ()
    cached <- case l of
      Just e -> holdsPinned (cachePathFor (pCache paths) (lkHash e) (isSingleFile e)) (lkHash e) (isSingleFile e)
      Nothing -> pure False
    let entry = either (const Nothing) id declaredBy
        (status, tone) = case (declaredBy, l) of
          (Right (Just _), Nothing) -> ("not locked (lask sync)", ToneWarn)
          (Right Nothing, Just _)
            | isNothing parent -> ("not in lask.json (lask sync drops it)", ToneWarn)
            | otherwise -> ("not declared by its parent (lask sync drops it)", ToneWarn)
          (Left (), Just _) -> ("unknown: its parent is not cached (lask sync)", ToneWarn)
          (_, Just e)
            | Just d <- entry, Just why <- lockDisagreement d e -> ("stale: " <> why <> " (lask sync)", ToneWarn)
            | isNothing parent, not (name `Set.member` imported) -> ("unused (lask sync --prune)", ToneWarn)
            | not cached -> ("not cached (lask sync)", ToneWarn)
            | otherwise -> ("ok", ToneOk)
          _ -> ("unknown", ToneWarn)
        source = maybe (maybe "—" lockSourceText l) sourceText entry
    pure (DepRow path source (requestedOf <$> entry) l status tone)
  if optJsonFormat opts
    then
      printJson . A.toJSON $
        [ A.object
            [ ("name", A.String (drPath r)),
              ("source", A.String (drSource r)),
              ("requested", maybe A.Null A.String (drRequested r)),
              ("locked", maybe A.Null A.String (drLocked r >>= lkRequested)),
              ("rev", maybe A.Null A.String (drLocked r >>= lkRev)),
              ("hash", maybe A.Null (A.String . lkHash) (drLocked r)),
              ("status", A.String (drStatus r))
            ]
        | r <- rows
        ]
    else
      if null rows
        then TIO.putStrLn "no dependencies declared"
        else do
          printTable
            opts
            ["NAME", "SOURCE", "REQUESTED", "LOCKED", "REV", "HASH", "STATUS"]
            [ [ plain (drPath r),
                plain (drSource r),
                plain (dash (drRequested r)),
                plain (dash (drLocked r >>= lkRequested)),
                plain (shortRev (drLocked r >>= lkRev)),
                plain (maybe "—" (shortDigest . lkHash) (drLocked r)),
                toned (drTone r) (drStatus r)
              ]
            | r <- rows
            ]
          let direct = length [() | r <- rows, not (">" `T.isInfixOf` drPath r)]
          TIO.putStrLn ""
          TIO.putStrLn $
            T.pack (show (length rows))
              <> (if length rows == 1 then " dependency" else " dependencies")
              <> " ("
              <> T.pack (show direct)
              <> " direct, "
              <> T.pack (show (length rows - direct))
              <> " transitive): "
              <> tally [T.strip (T.takeWhile (\c -> c /= ':' && c /= '(') (drStatus r)) | r <- rows]
  exitSuccess
  where
    isSingleFile e = maybe False (".lask" `T.isSuffixOf`) (lkUrl e)

-- deps graph -------------------------------------------------------------------

-- | @lask deps graph@ (spec 11.5): the dependency graph the lock
-- records, from the entry module down. With a name, only the paths
-- that reach it; with @--depth@, only that many levels.
cmdDepsGraph :: CommonOpts -> Maybe Text -> Maybe Int -> IO ()
cmdDepsGraph opts target depth = do
  paths <- projectPaths opts
  lock <- loadLockOrExit opts
  let locked = lockModules lock
      allPaths = Map.keys locked
      lastName p = last (T.splitOn ">" p)
      childrenOf parent =
        sort
          [ p
          | p <- allPaths,
            case parent of
              Nothing -> not (">" `T.isInfixOf` p)
              Just q -> (q <> ">") `T.isPrefixOf` p && not (">" `T.isInfixOf` T.drop (T.length q + 1) p)
          ]
      reaches p = maybe True (\n -> any (\q -> underPath p q && lastName q == n) allPaths) target
      label p = case Map.lookup p locked of
        Just e ->
          lastName p
            <> "  "
            <> lockSourceText e
            <> maybe "" ("@" <>) (lkRequested e)
            <> maybe "" (\r -> " (" <> T.take 7 r <> ")") (lkRev e)
            <> (if Just (lastName p) == target then "  ◀" else "")
        Nothing -> lastName p
  case target of
    Just n | n `notElem` map lastName allPaths -> usageError opts ("no such dependency in the lock file: '" <> n <> "'")
    _ -> pure ()
  let tree level parent =
        [ Node p (tree (level + 1) (Just p))
        | maybe True (level <) depth,
          p <- childrenOf parent,
          reaches p
        ]
      roots = tree 0 Nothing
      root = T.pack (takeFileName (optModule opts))
  _ <- pure paths
  if optJsonFormat opts
    then
      let node (Node p kids) =
            A.object
              [ ("name", A.String (lastName p)),
                ("path", A.String p),
                ("source", maybe A.Null (A.String . lockSourceText) (Map.lookup p locked)),
                ("requested", maybe A.Null A.String (Map.lookup p locked >>= lkRequested)),
                ("rev", maybe A.Null A.String (Map.lookup p locked >>= lkRev)),
                ("dependencies", A.toJSON (map node kids))
              ]
       in printJson (A.object [("module", A.String root), ("dependencies", A.toJSON (map node roots))])
    else do
      TIO.putStrLn root
      let render prefix nodes =
            concat
              [ (prefix <> (if lastOne then "└── " else "├── ") <> label p)
                  : render (prefix <> (if lastOne then "    " else "│   ")) kids
              | (i, Node p kids) <- zip [1 :: Int ..] nodes,
                let lastOne = i == length nodes
              ]
      mapM_ TIO.putStrLn (render "" roots)
  exitSuccess

-- | A dependency in the graph, by its lock path, and what it depends on.
data Node = Node Text [Node]

-- deps rm ----------------------------------------------------------------------

-- | @lask deps rm@ (spec 11.5): remove a direct dependency from
-- @lask.json@ and the lock, with the dependencies under it, the cache
-- entries nothing else uses, and the images no module requires any
-- more. Refused while a module of the project still imports it. No
-- network.
cmdDepsRm :: CommonOpts -> Text -> IO ()
cmdDepsRm opts name = do
  paths <- projectPaths opts
  df <- fromMaybe emptyDepsFile <$> loadDepsOrExit opts paths
  prior <- loadLockMaybe paths
  unless (name `Map.member` depsEntries df) $ do
    let owners = [T.intercalate ">" (init segs) | p <- maybe [] (Map.keys . lockModules) prior, let segs = T.splitOn ">" p, length segs > 1, last segs == name]
    usageError opts $ case owners of
      o : _ -> "'" <> name <> "' is a dependency of '" <> o <> "', not of this project; remove '" <> T.takeWhile (/= '>') o <> "' instead"
      [] -> "no such dependency in lask.json: '" <> name <> "'"
  sites <- filter ((== name) . isDep) <$> projectImports (pBase paths)
  unless (null sites) $
    usageError opts $
      "'" <> name <> "' is still imported; remove the imports first:\n"
        <> T.intercalate "\n" ["  " <> T.pack (isFile s) <> ":" <> T.pack (show (isLine s)) | s <- sites]
  let df' = df {depsEntries = Map.delete name (depsEntries df)}
      priorModules = maybe Map.empty lockModules prior
      (gone, keptModules) = Map.partitionWithKey (\p _ -> underPath name p) priorModules
  BL.writeFile (pDeps paths) (renderDepsFile df')
  -- The images the program still requires: read the program again,
  -- without the dependency. One that does not compile keeps them all.
  compiledE <- compileFile (optModule opts)
  let priorImages = maybe Map.empty lockImages prior
  keptImages <- case compiledE of
    Right c -> do
      let keys = Set.fromList (map irKey (imageRows (compiledCore c) Nothing))
      pure (Map.filterWithKey (\k _ -> k `Set.member` keys) priorImages)
    Left _ -> pure priorImages
  when (isJust prior) $
    BL.writeFile (pLock paths) (renderLockFile (LockFile keptModules keptImages))
  when (pOwnCache paths) $
    cleanCache paths (Set.fromList (map lkHash (Map.elems keptModules))) (map lkHash (Map.elems gone))
  TIO.putStrLn ("removed " <> name <> " from lask.json")
  unless (Map.null gone) $
    TIO.putStrLn ("removed from lask.lock.json: " <> T.intercalate ", " (Map.keys gone))
  let dropped = Map.keys priorImages `minus` Map.keys keptImages
  unless (null dropped) $
    TIO.putStrLn ("images no longer required (kept on the Docker daemon): " <> T.intercalate ", " (map (T.drop 1) dropped))
  exitSuccess
  where
    minus a b = [x | x <- a, x `notElem` b]

-- envs list --------------------------------------------------------------------

-- | @lask envs list@ (spec 11.4): every environment the program can
-- run in, what the lock resolves it to, what requires it, and whether
-- its image is on the Docker daemon. The daemon is asked once, read
-- only; when it cannot be reached a warning says so and the status is
-- @?@. No network, no pull, no build.
cmdEnvsList :: CommonOpts -> Maybe Text -> IO ()
cmdEnvsList opts function = do
  compiledE <- compileFile (optModule opts)
  compiled <- case compiledE of
    Right c -> pure c
    Left _ -> compileOrExit opts
  paths <- projectPaths opts
  let core = compiledCore compiled
  scope <- case function of
    Nothing -> pure Nothing
    Just fn ->
      case [k | k@(p, n) <- Map.keys (cpDecls core), p == cpEntry core, n == kebabToSnake fn] of
        k : _ -> pure (Just k)
        [] -> usageError opts ("no such function: '" <> fn <> "'")
  lock <- loadLockMaybe paths
  daemon <- daemonReachable
  case daemon of
    Left e -> TIO.hPutStrLn stderr ("warning: cannot reach the Docker daemon (" <> e <> "); image status is not checked")
    Right () -> pure ()
  let uses = collectHeadUses core scope
      locals = sort (nub [huBy u | u@(HeadUse HeadLocal _) <- uses])
      images = maybe Map.empty lockImages lock
  imageLines <- forM (imageRows core scope) $ \row -> do
    pinned <- pinnedOf (pBase paths) images row
    (status, tone) <- case (pinned, daemon) of
      (Nothing, _)
        | irKind row == "registry" -> pure ("not pinned (lask sync)", ToneWarn)
        | otherwise -> pure ("not built (lask sync)", ToneWarn)
      (Just _, Left _) -> pure ("?", ToneWarn)
      (Just p, Right ()) -> do
        ok <- isPresent p
        pure (if ok then ("present", ToneOk) else ("missing (lask sync)", ToneBad))
    pure (ImageLine (environmentLabel row) (irKind row) pinned (map (requirerLabel paths lock) (irRequiredBy row)) status tone Nothing)
  let localLine = [ImageLine "#local" "local" Nothing (map (requirerLabel paths lock) locals) "ok" ToneOk Nothing | not (null locals)]
      lines' = localLine <> imageLines
  if optJsonFormat opts
    then
      printJson . A.toJSON $
        [ A.object
            [ ("environment", A.String (ilEnvironment l)),
              ("kind", A.String (ilKind l)),
              ("pinned", maybe A.Null A.String (ilPinned l)),
              ("required_by", A.toJSON (ilRequiredBy l)),
              ("status", A.String (ilStatus l))
            ]
        | l <- lines'
        ]
    else
      if null lines'
        then TIO.putStrLn "no environments"
        else do
          printTable
            opts
            ["ENVIRONMENT", "KIND", "PINNED", "REQUIRED BY", "STATUS"]
            [ [plain (shortPath (ilEnvironment l)), plain (ilKind l), plain (maybe "—" shortDigest (ilPinned l)), plain (requiredCell (ilRequiredBy l)), toned (ilTone l) (ilStatus l)]
            | l <- lines'
            ]
          let kinds = [ilKind l | l <- lines']
              pinnedCount = length [() | l <- imageLines, isJust (ilPinned l)]
          TIO.putStrLn ""
          TIO.putStrLn $
            plural (length lines') "environment"
              <> " ("
              <> tally kinds
              <> "): "
              <> tally [if ilStatus l == "?" then "unknown" else T.strip (T.takeWhile (/= '(') (ilStatus l)) | l <- lines']
              <> "; "
              <> T.pack (show pinnedCount)
              <> " pinned in lask.lock.json"
  exitSuccess
