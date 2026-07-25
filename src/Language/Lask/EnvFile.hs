{-# LANGUAGE OverloadedStrings #-}

-- | The environment definition file @environments.lask.json@
-- (spec 10.3): named environments (in particular @remote@ ones)
-- referenced from code via @#env("name")@.
module Language.Lask.EnvFile
  ( EnvFile (..),
    EnvEntry (..),
    defaultEnvFileName,
    loadEnvFile,
    parseEnvFile,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (ETypeEnvConstruct), Stage (StageStatic))
import Language.Lask.Runtime.Value (Value (..))
import Language.Lask.Serialize (valueFromJson)
import Language.Lask.Span (Span (NoSpan))
import System.Directory (doesFileExist)

defaultEnvFileName :: FilePath
defaultEnvFileName = "environments.lask.json"

newtype EnvFile = EnvFile
  { envFileEntries :: Map Text EnvEntry
  }
  deriving (Show, Eq)

data EnvEntry = EnvEntry
  { entryKind :: Text,
    entryParams :: Map Text Value
  }
  deriving (Show, Eq)

-- | Load and validate an environment file. @Nothing@ when the file
-- does not exist (only an error if the module references @#env@,
-- which the caller decides, spec 10.3).
loadEnvFile :: FilePath -> IO (Either Diagnostic (Maybe EnvFile))
loadEnvFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      r <- try (BL.readFile path)
      pure $ case r of
        Left e ->
          Left (err ("cannot read " <> T.pack path <> ": " <> T.pack (show (e :: IOException))))
        Right bytes -> Just <$> parseEnvFile bytes

parseEnvFile :: BL.ByteString -> Either Diagnostic EnvFile
parseEnvFile bytes = do
  root <- first ("invalid JSON: " <>) (A.eitherDecode bytes)
  obj <- asObject "the environment file" root
  envsVal <- maybe (Left (err "missing key: 'environments'")) Right (KM.lookup "environments" obj)
  envsObj <- asObject "'environments'" envsVal
  entries <- traverse entry [(AK.toText k, v) | (k, v) <- KM.toList envsObj]
  pure (EnvFile (Map.fromList entries))
  where
    first f = either (Left . err . f . T.pack) Right

    asObject what v = case v of
      A.Object o -> Right o
      _ -> Left (err (what <> " must be a JSON object"))

    entry (name, v) = do
      validateName name
      o <- asObject ("environment '" <> name <> "'") v
      kind <- case KM.lookup "kind" o of
        Just (A.String k) -> Right k
        _ -> Left (err ("environment '" <> name <> "' needs a string 'kind'"))
      params <- case KM.lookup "params" o of
        Nothing -> Right Map.empty
        Just (A.Object po) ->
          Right (Map.fromList [(AK.toText k, valueFromJson x) | (k, x) <- KM.toList po])
        Just _ -> Left (err ("environment '" <> name <> "': 'params' must be an object"))
      validated <- validateEntry name kind params
      pure (name, validated)

    -- Environment names must be lower_id identifiers (spec 10.3).
    validateName name = case T.uncons name of
      Just (c, rest)
        | (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest -> Right ()
      _ -> Left (err ("environment name must be a lower-case identifier: '" <> name <> "'"))
    identChar c =
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

    -- Entries hold concrete kinds only; chained 'env' indirection is
    -- an error (spec 10.3). Params are validated against the
    -- constructor signatures of 10.2.
    validateEntry name kind params = case kind of
      "local" -> do
        requireNoExtra name kind params []
        pure (EnvEntry kind params)
      "docker" -> do
        requireString name "image" params
        requireNoExtra name kind params ["image", "memory", "cpus"]
        pure (EnvEntry kind params)
      "remote" -> do
        requireString name "host" params
        mapM_ (checkOptional name params) [("user", isString), ("port", isNumber)]
        requireNoExtra name kind params ["host", "user", "port"]
        pure (EnvEntry kind params)
      "env" -> Left (err ("environment '" <> name <> "': chained 'env' entries are not allowed"))
      other -> Left (err ("environment '" <> name <> "': unknown kind '" <> other <> "'"))

    requireString name key params = case Map.lookup key params of
      Just (VString s) | not (T.null s) -> Right ()
      Just _ -> Left (err ("environment '" <> name <> "': '" <> key <> "' must be a non-empty string"))
      Nothing -> Left (err ("environment '" <> name <> "': missing param '" <> key <> "'"))

    checkOptional name params (key, ok) = case Map.lookup key params of
      Nothing -> Right ()
      Just v
        | ok v -> Right ()
        | otherwise -> Left (err ("environment '" <> name <> "': invalid param '" <> key <> "'"))

    isString (VString _) = True
    isString _ = False
    isNumber (VNumber _) = True
    isNumber _ = False

    requireNoExtra name kind params allowed =
      case [k | k <- Map.keys params, k `notElem` allowed] of
        [] -> Right ()
        (k : _) ->
          Left (err ("environment '" <> name <> "' (" <> kind <> "): unknown param '" <> k <> "'"))

err :: Text -> Diagnostic
err = mkDiagnostic ETypeEnvConstruct StageStatic NoSpan
