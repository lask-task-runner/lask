{-# LANGUAGE OverloadedStrings #-}

-- | The project file @lask.json@ (spec chapter 5): external Lask
-- source code fetched over the internet, declared with its source
-- location, version and content hash. Code refers to dependencies by
-- name only.
module Language.Lask.Deps.File
  ( DepsFile (..),
    DepEntry (..),
    emptyDepsFile,
    defaultDepsFileName,
    entryIsSingleFile,
    loadDepsFile,
    parseDepsFile,
    renderDepsFile,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (EModuleUnresolved), Stage (StageStatic))
import Language.Lask.Span (Span (NoSpan))
import System.Directory (doesFileExist)

defaultDepsFileName :: FilePath
defaultDepsFileName = "lask.json"

newtype DepsFile = DepsFile
  { depsEntries :: Map Text DepEntry
  }
  deriving (Show, Eq)

emptyDepsFile :: DepsFile
emptyDepsFile = DepsFile Map.empty

-- | A dependency source: exactly one of @git@ (with a required @rev@)
-- or @url@ (an archive or a single @.lask@ file). Every entry pins a
-- content hash.
-- | A declared source. The project file records intent only; the
-- content hash that pins it lives in the lock file (spec chapter 5).
data DepEntry
  = -- | Repository URL and rev (a tag or a commit).
    DepGit Text Text
  | -- | Source URL.
    DepUrl Text
  deriving (Show, Eq)

-- | A @url@ ending in @.lask@ is a single-file module; anything else
-- (git repositories, archives) is a source tree (spec chapter 5).
entryIsSingleFile :: DepEntry -> Bool
entryIsSingleFile (DepGit {}) = False
entryIsSingleFile (DepUrl u) = ".lask" `T.isSuffixOf` u

-- | Load and validate the dependency definition file. @Nothing@ when
-- the file does not exist (only an error if a bare import is used,
-- which the loader decides).
loadDepsFile :: FilePath -> IO (Either Diagnostic (Maybe DepsFile))
loadDepsFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      r <- try (BL.fromStrict <$> BS.readFile path)
      pure $ case r of
        Left e ->
          Left (err ("cannot read " <> T.pack path <> ": " <> T.pack (show (e :: IOException))))
        Right bytes -> Just <$> parseDepsFile bytes

parseDepsFile :: BL.ByteString -> Either Diagnostic DepsFile
parseDepsFile bytes = do
  root <- first ("invalid JSON: " <>) (A.eitherDecode bytes)
  obj <- asObject "the project file" root
  depsVal <- maybe (Left (err "missing key: 'dependencies'")) Right (KM.lookup "dependencies" obj)
  depsObj <- asObject "'dependencies'" depsVal
  entries <- traverse entry [(AK.toText k, v) | (k, v) <- KM.toList depsObj]
  pure (DepsFile (Map.fromList entries))
  where
    first f = either (Left . err . f . T.pack) Right

    asObject what v = case v of
      A.Object o -> Right o
      _ -> Left (err (what <> " must be a JSON object"))

    entry (name, v) = do
      validateName name
      o <- asObject ("dependency '" <> name <> "'") v
      let str k = case KM.lookup k o of
            Just (A.String s) | not (T.null s) -> Just s
            _ -> Nothing
      parsed <- case (str "git", str "url") of
        (Just g, Nothing) -> case str "rev" of
          Just rev -> Right (DepGit g rev)
          Nothing -> Left (err ("dependency '" <> name <> "': 'git' requires 'rev'"))
        (Nothing, Just u) -> do
          case KM.lookup "rev" o of
            Just _ -> Left (err ("dependency '" <> name <> "': 'rev' is only valid with 'git'"))
            Nothing -> Right ()
          Right (DepUrl u)
        (Just _, Just _) ->
          Left (err ("dependency '" <> name <> "': 'git' and 'url' are mutually exclusive"))
        (Nothing, Nothing) ->
          Left (err ("dependency '" <> name <> "': needs exactly one source ('git' or 'url')"))
      -- `hash` used to live here; it is now the lock's (spec chapter 5).
      -- The key is still tolerated so existing project files load.
      let known = ["git", "rev", "url", "hash"]
      case [AK.toText k | (k, _) <- KM.toList o, AK.toText k `notElem` known] of
        [] -> Right (name, parsed)
        (k : _) -> Left (err ("dependency '" <> name <> "': unknown key '" <> k <> "'"))


    -- Dependency names conform to lower_id (spec chapter 5, 3.2).
    validateName name = case T.uncons name of
      Just (c, rest)
        | (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest -> Right ()
      _ -> Left (err ("dependency name must be a lower-case identifier: '" <> name <> "'"))
    identChar c =
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

-- | Serialize for @lask deps add@ (spec 11.5).
renderDepsFile :: DepsFile -> BL.ByteString
renderDepsFile df =
  AP.encodePretty' (AP.defConfig {AP.confCompare = compare}) $
    A.object
      [ ( "dependencies",
          A.object
            [ (AK.fromText n, entryJson n e)
            | (n, e) <- Map.toList (depsEntries df)
            ]
        )
      ]
  where
    entryJson _ e = case e of
      DepGit g rev -> A.object [("git", A.String g), ("rev", A.String rev)]
      DepUrl u -> A.object [("url", A.String u)]

err :: Text -> Diagnostic
err = mkDiagnostic EModuleUnresolved StageStatic NoSpan
