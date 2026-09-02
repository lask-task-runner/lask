{-# LANGUAGE OverloadedStrings #-}

-- | The project file @lask.json@ (spec chapter 5): external Lask
-- source code fetched over the internet, declared with its source
-- location, version and content hash. Code refers to dependencies by
-- name only.
module Language.Lask.Deps.File
  ( DepsFile (..),
    DepEntry (..),
    Grants (..),
    defaultGrants,
    emptyDepsFile,
    grantsFor,
    defaultDepsFileName,
    entryHash,
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

data DepsFile = DepsFile
  { depsEntries :: Map Text DepEntry,
    -- | Per-dependency capability grants, and the default applied to a
    -- dependency that declares none (spec chapter 16).
    depsGrants :: Map Text Grants,
    depsDefaultGrants :: Grants
  }
  deriving (Show, Eq)

-- | What a dependency may do (spec 16.2): which execution environment
-- kinds it may run commands in, and which environment variables it may
-- read. Absence is denial, not an unspecified state.
data Grants = Grants
  { grantEnvironments :: [Text],
    grantEnvVars :: [Text]
  }
  deriving (Show, Eq)

-- | The default for a dependency that declares none (spec 16.5):
-- containers yes, host execution no, no environment variables.
defaultGrants :: Grants
defaultGrants = Grants ["docker"] []

emptyDepsFile :: DepsFile
emptyDepsFile = DepsFile Map.empty Map.empty defaultGrants

grantsFor :: DepsFile -> Text -> Grants
grantsFor df name =
  Map.findWithDefault (depsDefaultGrants df) name (depsGrants df)

-- | A dependency source: exactly one of @git@ (with a required @rev@)
-- or @url@ (an archive or a single @.lask@ file). Every entry pins a
-- content hash.
data DepEntry
  = -- | Repository URL, rev (tag or commit), content hash.
    DepGit Text Text Text
  | -- | Source URL, content hash.
    DepUrl Text Text
  deriving (Show, Eq)

entryHash :: DepEntry -> Text
entryHash (DepGit _ _ h) = h
entryHash (DepUrl _ h) = h

-- | A @url@ ending in @.lask@ is a single-file module; anything else
-- (git repositories, archives) is a source tree (spec chapter 5).
entryIsSingleFile :: DepEntry -> Bool
entryIsSingleFile (DepGit {}) = False
entryIsSingleFile (DepUrl u _) = ".lask" `T.isSuffixOf` u

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
  root' <- asObject "the project file" root
  let obj = root'
  depsVal <- maybe (Left (err "missing key: 'dependencies'")) Right (KM.lookup "dependencies" obj)
  depsObj <- asObject "'dependencies'" depsVal
  entries <- traverse entry [(AK.toText k, v) | (k, v) <- KM.toList depsObj]
  grantsList <- traverse grantsOf [(AK.toText k, v) | (k, v) <- KM.toList depsObj]
  policy <- case KM.lookup "policy" root' of
    Nothing -> Right defaultGrants
    Just v -> do
      o <- asObject "'policy'" v
      case KM.lookup "default_grants" o of
        Nothing -> Right defaultGrants
        Just g -> parseGrants "policy.default_grants" g
  pure
    ( DepsFile
        (Map.fromList entries)
        (Map.fromList [(n, g) | (n, Just g) <- grantsList])
        policy
    )
  where
    first f = either (Left . err . f . T.pack) Right

    asObject what v = case v of
      A.Object o -> Right o
      _ -> Left (err (what <> " must be a JSON object"))

    entry (name, v) = do
      validateName name
      o <- asObject ("dependency '" <> name <> "'") v
      hash <- case KM.lookup "hash" o of
        Just (A.String h)
          | "sha256-" `T.isPrefixOf` h && T.length h > 7 -> Right h
        Just _ -> Left (err ("dependency '" <> name <> "': 'hash' must be a sha256-... string"))
        Nothing -> Left (err ("dependency '" <> name <> "': missing required 'hash'"))
      let str k = case KM.lookup k o of
            Just (A.String s) | not (T.null s) -> Just s
            _ -> Nothing
      parsed <- case (str "git", str "url") of
        (Just g, Nothing) -> case str "rev" of
          Just rev -> Right (DepGit g rev hash)
          Nothing -> Left (err ("dependency '" <> name <> "': 'git' requires 'rev'"))
        (Nothing, Just u) -> do
          case KM.lookup "rev" o of
            Just _ -> Left (err ("dependency '" <> name <> "': 'rev' is only valid with 'git'"))
            Nothing -> Right ()
          Right (DepUrl u hash)
        (Just _, Just _) ->
          Left (err ("dependency '" <> name <> "': 'git' and 'url' are mutually exclusive"))
        (Nothing, Nothing) ->
          Left (err ("dependency '" <> name <> "': needs exactly one source ('git' or 'url')"))
      let known = ["git", "rev", "url", "hash", "grants"]
      case [AK.toText k | (k, _) <- KM.toList o, AK.toText k `notElem` known] of
        [] -> Right (name, parsed)
        (k : _) -> Left (err ("dependency '" <> name <> "': unknown key '" <> k <> "'"))

    grantsOf (name, v) = do
      o <- asObject ("dependency '" <> name <> "'") v
      case KM.lookup "grants" o of
        Nothing -> Right (name, Nothing)
        Just g -> (,) name . Just <$> parseGrants ("dependency '" <> name <> "'") g

    parseGrants what v = do
      o <- asObject (what <> ": 'grants'") v
      envs <- strings (what <> ": 'environments'") (KM.lookup "environments" o)
      vars <- strings (what <> ": 'env_vars'") (KM.lookup "env_vars" o)
      case [AK.toText k | (k, _) <- KM.toList o, AK.toText k `notElem` ["environments", "env_vars"]] of
        [] -> Right (Grants envs vars)
        (k : _) -> Left (err (what <> ": unknown grant '" <> k <> "'"))

    strings _ Nothing = Right []
    strings what (Just (A.Array xs)) =
      traverse
        (\x -> case x of A.String t -> Right t; _ -> Left (err (what <> " must be strings")))
        (F.toList xs)
    strings what (Just _) = Left (err (what <> " must be an array of strings"))

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
    entryJson n e = case e of
      DepGit g rev h ->
        A.object ([("git", A.String g), ("rev", A.String rev), ("hash", A.String h)] <> grantsJson n)
      DepUrl u h ->
        A.object ([("url", A.String u), ("hash", A.String h)] <> grantsJson n)
    grantsJson n = case Map.lookup n (depsGrants df) of
      Nothing -> []
      Just g ->
        [ ( "grants",
            A.object
              [ ("environments", A.toJSON (grantEnvironments g)),
                ("env_vars", A.toJSON (grantEnvVars g))
              ]
          )
        ]

err :: Text -> Diagnostic
err = mkDiagnostic EModuleUnresolved StageStatic NoSpan
