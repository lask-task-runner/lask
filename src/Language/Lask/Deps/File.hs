{-# LANGUAGE OverloadedStrings #-}

-- | The project file @lask.json@ (spec chapter 5): external Lask
-- source code fetched over the internet, declared with its source
-- location, version and content hash, and the functions that ask for
-- confirmation before they run. Code refers to dependencies by name
-- only, and says nothing about confirmation.
module Language.Lask.Deps.File
  ( DepsFile (..),
    DepEntry (..),
    ConfirmRule (..),
    emptyDepsFile,
    defaultDepsFileName,
    entryIsSingleFile,
    loadDepsFile,
    parseDepsFile,
    renderDepsFile,
    validateSource,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (EModuleUnresolved), Stage (StageStatic))
import Language.Lask.Span (Span (NoSpan))
import System.Directory (doesFileExist)

defaultDepsFileName :: FilePath
defaultDepsFileName = "lask.json"

data DepsFile = DepsFile
  { depsEntries :: Map Text DepEntry,
    -- | The functions that ask for confirmation before they run
    -- (spec chapter 5, 11.2), keyed by function name.
    depsConfirm :: Map Text ConfirmRule
  }
  deriving (Show, Eq)

emptyDepsFile :: DepsFile
emptyDepsFile = DepsFile Map.empty Map.empty

-- | One entry of @confirm@. The references it makes to the module (the
-- function, its parameters, the values' types) are checked against
-- the compiled program, not here.
data ConfirmRule = ConfirmRule
  { -- | Parameter name and the values that require confirmation, in
    -- the order written. Empty: always required.
    crWhen :: [(Text, [A.Value])],
    -- | What has to be typed; 'Nothing' for the default.
    crPhrase :: Maybe Text,
    -- | Where the entry's key is written, as (line, column), 1-based,
    -- so that a diagnostic can point at it.
    crAt :: Maybe (Int, Int)
  }
  deriving (Show, Eq)

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
  -- An unknown key is an error rather than ignored: a misspelt
  -- "confirm" would otherwise switch every confirmation off unnoticed.
  case [AK.toText k | (k, _) <- KM.toList obj, AK.toText k `notElem` ["dependencies", "confirm"]] of
    [] -> Right ()
    (k : _) -> Left (err ("unknown key: '" <> k <> "' (the project file takes 'dependencies' and 'confirm')"))
  entries <- case KM.lookup "dependencies" obj of
    Nothing -> Right []
    Just depsVal -> do
      depsObj <- asObject "'dependencies'" depsVal
      traverse entry [(AK.toText k, v) | (k, v) <- KM.toList depsObj]
  confirms <- case KM.lookup "confirm" obj of
    Nothing -> Right []
    Just confirmVal -> do
      confirmObj <- asObject "'confirm'" confirmVal
      traverse confirmEntry [(AK.toText k, v) | (k, v) <- KM.toList confirmObj]
  pure (DepsFile (Map.fromList entries) (Map.fromList confirms))
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
      validateSource name parsed
      -- `hash` used to live here; it is now the lock's (spec chapter 5).
      -- The key is still tolerated so existing project files load.
      let known = ["git", "rev", "url", "hash"]
      case [AK.toText k | (k, _) <- KM.toList o, AK.toText k `notElem` known] of
        [] -> Right (name, parsed)
        (k : _) -> Left (err ("dependency '" <> name <> "': unknown key '" <> k <> "'"))


    confirmEntry (name, v) = do
      o <- asObject ("confirm '" <> name <> "'") v
      case [AK.toText k | (k, _) <- KM.toList o, AK.toText k `notElem` ["when", "phrase"]] of
        [] -> Right ()
        (k : _) -> Left (err ("confirm '" <> name <> "': unknown key '" <> k <> "' (an entry takes 'when' and 'phrase')"))
      conditions <- case KM.lookup "when" o of
        Nothing -> Right []
        Just w -> do
          wo <- asObject ("confirm '" <> name <> "': 'when'") w
          traverse
            ( \(k, vs) -> case vs of
                A.Array xs
                  | not (null xs) -> Right (AK.toText k, foldr (:) [] xs)
                _ ->
                  Left (err ("confirm '" <> name <> "': 'when' maps a parameter to a non-empty array of values ('" <> AK.toText k <> "')"))
            )
            (KM.toList wo)
      phrase <- case KM.lookup "phrase" o of
        Nothing -> Right Nothing
        Just (A.String t) | not (T.null (T.strip t)) -> Right (Just t)
        Just _ -> Left (err ("confirm '" <> name <> "': 'phrase' must be a non-empty string"))
      Right (name, ConfirmRule conditions phrase (keyPosition name))

    -- Where a confirm entry's key is written. The JSON parser keeps no
    -- positions, so the key is searched for in the text, after the
    -- "confirm" key; a key that cannot be found has no position.
    keyPosition name =
      let txt = TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict bytes)
          (before, fromConfirm) = T.breakOn "\"confirm\"" txt
          (skipped, fromKey) = T.breakOn ("\"" <> name <> "\"") (T.drop 1 fromConfirm)
          prefix = before <> T.take 1 fromConfirm <> skipped
       in if T.null fromConfirm || T.null fromKey
            then Nothing
            else
              let ls = T.splitOn "\n" prefix
               in Just (length ls, T.length (last ls) + 1)

    -- Dependency names conform to lower_id (spec chapter 5, 3.2).
    validateName name = case T.uncons name of
      Just (c, rest)
        | (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest -> Right ()
      _ -> Left (err ("dependency name must be a lower-case identifier: '" <> name <> "'"))
    identChar c =
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

-- | Whether a source can be handed to @git@ and @curl@ (spec chapter
-- 5). Each goes on their command lines as one argument, so a value
-- that starts with @-@ would be read as an option — @git ls-remote
-- --upload-pack=\<command\>@ runs the command — and is refused here,
-- for @deps add@ as for the project file. A @url@ is fetched over
-- @https@, @http@ or from a local @file@, and nothing else.
validateSource :: Text -> DepEntry -> Either Diagnostic ()
validateSource name entry = case entry of
  DepGit g rev -> do
    word "git" g
    word "rev" rev
  DepUrl u -> do
    word "url" u
    if any (`T.isPrefixOf` u) ["https://", "http://", "file://"]
      then Right ()
      else bad "url" u "must be an https://, http:// or file:// URL"
  where
    word key v
      | "-" `T.isPrefixOf` v = bad key v "must not start with '-'"
      | T.any (\c -> isSpace c || isControl c) v = bad key v "must not contain whitespace or control characters"
      | otherwise = Right ()
    bad key v why = Left (err ("dependency '" <> name <> "': '" <> key <> "' " <> why <> ": '" <> v <> "'"))

-- | Serialize for @lask deps add@ (spec 11.5).
renderDepsFile :: DepsFile -> BL.ByteString
renderDepsFile df =
  AP.encodePretty' (AP.defConfig {AP.confCompare = compare}) . A.object $
    [ ( "dependencies",
        A.object
          [ (AK.fromText n, entryJson n e)
          | (n, e) <- Map.toList (depsEntries df)
          ]
      )
    ]
      -- `deps add` rewrites the file, and must not drop what it does
      -- not manage.
      <> [ ( "confirm",
             A.object
               [ (AK.fromText n, confirmJson r)
               | (n, r) <- Map.toList (depsConfirm df)
               ]
           )
         | not (Map.null (depsConfirm df))
         ]
  where
    confirmJson r =
      A.object $
        [("when", A.object [(AK.fromText p, A.toJSON vs) | (p, vs) <- crWhen r]) | not (null (crWhen r))]
          <> [("phrase", A.String t) | Just t <- [crPhrase r]]
    entryJson _ e = case e of
      DepGit g rev -> A.object [("git", A.String g), ("rev", A.String rev)]
      DepUrl u -> A.object [("url", A.String u)]

err :: Text -> Diagnostic
err = mkDiagnostic EModuleUnresolved StageStatic NoSpan
