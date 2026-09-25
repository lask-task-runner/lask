{-# LANGUAGE OverloadedStrings #-}

-- | The lock file @lask.lock.json@ (spec chapter 5): the resolution of
-- the whole dependency graph, including transitive dependencies.
--
-- The project file records intent (where a dependency comes from);
-- the lock records what that intent resolved to.
-- Keys are dependency paths: a direct dependency is its name, a
-- transitive one is @parent>child@, so the graph a project executes can
-- be read from the project itself.
module Language.Lask.Deps.Lock
  ( LockFile (..),
    LockEntry (..),
    LockImage (..),
    defaultLockFileName,
    emptyLock,
    childPath,
    lookupHash,
    loadLockFile,
    parseLockFile,
    renderLockFile,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (EModuleLockStale), Stage (StageStatic))
import Language.Lask.Span (Span (NoSpan))
import System.Directory (doesFileExist)

defaultLockFileName :: FilePath
defaultLockFileName = "lask.lock.json"

data LockFile = LockFile
  { lockModules :: Map Text LockEntry,
    lockImages :: Map Text LockImage
  }
  deriving (Show, Eq)

-- | Source URL, the reference that was requested (a tag, for @git@),
-- the revision it resolved to, and the content hash of the fetched
-- source.
data LockEntry = LockEntry
  { lkGit :: Maybe Text,
    lkUrl :: Maybe Text,
    lkRequested :: Maybe Text,
    lkRev :: Maybe Text,
    lkHash :: Text
  }
  deriving (Show, Eq)

-- | A materialized container image: a recipe built to a
-- content-addressed tag, or a registry reference (spec 10.3).
data LockImage = LockImage
  { liKind :: Text,
    liRef :: Maybe Text,
    liTag :: Maybe Text
  }
  deriving (Show, Eq)

emptyLock :: LockFile
emptyLock = LockFile Map.empty Map.empty

-- | The lock key of a dependency reached through another one.
childPath :: Text -> Text -> Text
childPath parent child
  | T.null parent = child
  | otherwise = parent <> ">" <> child

lookupHash :: LockFile -> Text -> Maybe Text
lookupHash lf path = lkHash <$> Map.lookup path (lockModules lf)

loadLockFile :: FilePath -> IO (Either Diagnostic (Maybe LockFile))
loadLockFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      r <- try (BL.fromStrict <$> BS.readFile path)
      pure $ case r of
        Left e ->
          Left (err ("cannot read " <> T.pack path <> ": " <> T.pack (show (e :: IOException))))
        Right bytes -> Just <$> parseLockFile bytes

parseLockFile :: BL.ByteString -> Either Diagnostic LockFile
parseLockFile bytes = do
  root <- first ("invalid JSON: " <>) (A.eitherDecode bytes)
  obj <- asObject "the lock file" root
  mods <- section obj "modules" entry
  imgs <- section obj "images" image
  pure (LockFile (Map.fromList mods) (Map.fromList imgs))
  where
    first f = either (Left . err . f . T.pack) Right

    asObject what v = case v of
      A.Object o -> Right o
      _ -> Left (err (what <> " must be a JSON object"))

    section obj key f = case KM.lookup key obj of
      Nothing -> Right []
      Just v -> do
        o <- asObject ("'" <> AK.toText key <> "'") v
        traverse f [(AK.toText k, x) | (k, x) <- KM.toList o]

    str o k = case KM.lookup k o of
      Just (A.String t) | not (T.null t) -> Just t
      _ -> Nothing

    entry (path, v) = do
      o <- asObject ("lock entry '" <> path <> "'") v
      case str o "hash" of
        Nothing -> Left (err ("lock entry '" <> path <> "': missing 'hash'"))
        Just h ->
          Right
            ( path,
              LockEntry (str o "git") (str o "url") (str o "requested") (str o "rev") h
            )

    image (key, v) = do
      o <- asObject ("lock image '" <> key <> "'") v
      let kind = maybe "registry" id (str o "kind")
      Right (key, LockImage kind (str o "ref") (str o "tag"))

renderLockFile :: LockFile -> BL.ByteString
renderLockFile lf =
  AP.encodePretty' cfg . A.object $
    [ "lock_version" A..= (1 :: Int),
      "modules" A..= A.object [AK.fromText p A..= entry e | (p, e) <- Map.toAscList (lockModules lf)],
      "images" A..= A.object [AK.fromText k A..= image i | (k, i) <- Map.toAscList (lockImages lf)]
    ]
  where
    cfg = AP.defConfig {AP.confCompare = compare, AP.confTrailingNewline = True}
    entry e =
      A.object $
        concat
          [ maybe [] (\x -> ["git" A..= x]) (lkGit e),
            maybe [] (\x -> ["url" A..= x]) (lkUrl e),
            maybe [] (\x -> ["requested" A..= x]) (lkRequested e),
            maybe [] (\x -> ["rev" A..= x]) (lkRev e),
            ["hash" A..= lkHash e]
          ]
    image i =
      A.object $
        concat
          [ ["kind" A..= liKind i],
            maybe [] (\x -> ["ref" A..= x]) (liRef i),
            maybe [] (\x -> ["tag" A..= x]) (liTag i)
          ]

err :: Text -> Diagnostic
err = mkDiagnostic EModuleLockStale StageStatic NoSpan
