{-# LANGUAGE OverloadedStrings #-}

-- | The serialization formats of @encode@ \/ @decode@ (spec 15.8).
--
-- A format selects a syntax, not a schema: every format decodes to the
-- ordinary value kinds of 4.1, and the result is moved to a concrete
-- type with @cast@ exactly as @json@ is.
module Language.Lask.Builtins.Format
  ( encodeFormat,
    decodeFormat,
    formatNames,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Scientific (isInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Language.Lask.ErrorCode
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, valueFromJson, valueToJson)
import qualified Toml
import qualified Toml.Pretty as TomlP
import qualified Toml.Semantics.Types as TomlV

-- | Every format specification string 'encodeFormat' and
-- 'decodeFormat' accept, for diagnostics.
formatNames :: [Text]
formatNames = ["json", "pretty-json", "yaml", "toml", "csv", "dotenv"]

-- | Render a value in the named format.
encodeFormat :: Text -> Value -> Either LaskFailure Text
encodeFormat fmt v = case fmt of
  "json" -> Right (encodeValue v)
  "pretty-json" -> Right (encodeValuePretty v)
  "yaml" -> Right (TE.decodeUtf8 (Y.encode (valueToJson v)))
  "toml" -> encodeToml v
  "csv" -> encodeCsv v
  "dotenv" -> encodeDotenv v
  _ -> Left (unsupported fmt)

-- | Parse text in the named format.
decodeFormat :: Text -> Text -> Either LaskFailure Value
decodeFormat fmt s = case fmt of
  "json" -> decodeJson s
  "pretty-json" -> decodeJson s
  "yaml" -> case Y.decodeEither' (TE.encodeUtf8 s) of
    Left e -> Left (dataError ("invalid YAML: " <> T.pack (Y.prettyPrintParseException e)))
    Right j -> Right (valueFromJson j)
  "toml" -> decodeToml s
  "csv" -> decodeCsv s
  "dotenv" -> decodeDotenv s
  _ -> Left (unsupported fmt)

decodeJson :: Text -> Either LaskFailure Value
decodeJson s = case A.eitherDecode (BL.fromStrict (TE.encodeUtf8 s)) of
  Right j -> Right (valueFromJson j)
  Left e -> Left (dataError ("invalid JSON: " <> T.pack e))

unsupported :: Text -> LaskFailure
unsupported fmt =
  dataError $
    "unsupported format: '" <> fmt <> "' (expected one of " <> T.intercalate ", " formatNames <> ")"

dataError :: Text -> LaskFailure
dataError = ioFailure EIoDataDecode

-- | A value the requested format cannot represent (spec 15.8).
notRepresentable :: Text -> LaskFailure
notRepresentable = runtimeFailure ERuntimeValue

-- TOML ----------------------------------------------------------------------

-- | Only an object can be a TOML document: the format has no
-- top-level scalar or array form.
encodeToml :: Value -> Either LaskFailure Text
encodeToml v = case entriesOf v of
  Nothing -> Left (notRepresentable "toml requires an object at the top level")
  Just kvs -> do
    tbl <- tomlTable kvs
    Right (T.pack (show (TomlP.prettyToml tbl)))

tomlTable :: [(Text, Value)] -> Either LaskFailure TomlV.Table
tomlTable kvs = TomlV.MkTable . Map.fromList <$> mapM entry kvs
  where
    entry (k, v) = (\tv -> (k, ((), tv))) <$> tomlValue v

tomlValue :: Value -> Either LaskFailure TomlV.Value
tomlValue v = case v of
  VString t -> Right (TomlV.Text t)
  VBool b -> Right (TomlV.Bool b)
  VNumber n
    | isInteger n -> Right (TomlV.Integer (truncate (toRealFloat n :: Double)))
    | otherwise -> Right (TomlV.Double (toRealFloat n))
  VArray xs -> TomlV.List <$> mapM tomlValue (V.toList xs)
  _ -> case entriesOf v of
    Just kvs -> TomlV.Table <$> tomlTable kvs
    -- TOML has no null, and no representation of the value kinds that
    -- are not serializable at all (spec 4.5).
    Nothing -> Left (notRepresentable ("toml cannot represent " <> typeNameOf v))

decodeToml :: Text -> Either LaskFailure Value
decodeToml s = case Toml.parse s of
  Left e -> Left (dataError ("invalid TOML: " <> T.strip (T.pack e)))
  Right tbl -> Right (fromTomlTable tbl)

fromTomlTable :: TomlV.Table' a -> Value
fromTomlTable (TomlV.MkTable m) = objectValue [(k, fromTomlValue v) | (k, (_, v)) <- Map.toList m]

-- | A date or a time decodes as a String: the language has no date
-- type (spec 15.8).
fromTomlValue :: TomlV.Value' a -> Value
fromTomlValue v = case v of
  TomlV.Integer' _ n -> VNumber (fromInteger n)
  TomlV.Double' _ d -> VNumber (realToFrac d)
  TomlV.Bool' _ b -> VBool b
  TomlV.Text' _ t -> VString t
  TomlV.List' _ xs -> VArray (V.fromList (map fromTomlValue xs))
  TomlV.Table' _ t -> fromTomlTable t
  TomlV.TimeOfDay' _ t -> VString (T.pack (show t))
  TomlV.ZonedTime' _ t -> VString (T.pack (show t))
  TomlV.LocalTime' _ t -> VString (T.pack (show t))
  TomlV.Day' _ d -> VString (T.pack (show d))

-- CSV (RFC 4180) --------------------------------------------------------------

-- | Decode with the first row as the header. Every value is a
-- @String@: no type is inferred from the text (spec 15.8).
decodeCsv :: Text -> Either LaskFailure Value
decodeCsv s = case csvRows s of
  [] -> Right (VArray V.empty)
  (header : rows) ->
    let row cells = objectValue (zip header (map VString (pad (length header) cells)))
     in Right (VArray (V.fromList (map row (filter (not . blank) rows))))
  where
    blank cells = cells == [""]
    pad n cells = take n (cells <> repeat "")

encodeCsv :: Value -> Either LaskFailure Text
encodeCsv v = case v of
  VArray xs | V.null xs -> Right ""
  VArray xs -> do
    rows <- mapM entriesOrFail (V.toList xs)
    case rows of
      [] -> Right ""
      (firstRow : _) -> do
        let header = map fst firstRow
        cells <- mapM (rowCells header) rows
        Right (T.unlines (map (T.intercalate "," . map csvCell) (header : cells)))
  _ -> Left (notRepresentable "csv requires an array of objects")
  where
    entriesOrFail x = case entriesOf x of
      Just kvs -> Right kvs
      Nothing -> Left (notRepresentable "csv requires an array of objects")

    rowCells header kvs = do
      -- A ragged row would silently drop or invent a column, so the
      -- field sets must agree (spec 15.8).
      if map fst kvs /= header
        then Left (notRepresentable "every csv row must carry the same fields as the first")
        else mapM (scalar . snd) kvs

    scalar x = case x of
      VString t -> Right t
      VNumber n -> Right (formatNumber n)
      VBool b -> Right (if b then "true" else "false")
      _ -> Left (notRepresentable ("csv cannot represent " <> typeNameOf x))

csvCell :: Text -> Text
csvCell t
  | T.any (`elem` (",\"\r\n" :: String)) t = "\"" <> T.replace "\"" "\"\"" t <> "\""
  | otherwise = t

-- | Split into rows of cells, honouring quoted cells that contain
-- separators, newlines, or doubled quotes.
csvRows :: Text -> [[Text]]
csvRows = go [] [] False . T.unpack
  where
    go row cells inQuotes s = case s of
      [] -> finish row cells
      ('"' : '"' : rest) | inQuotes -> go row (push cells '"') True rest
      ('"' : rest) -> go row (openCell cells) (not inQuotes) rest
      (',' : rest) | not inQuotes -> go (cellsDone row cells) [] False rest
      ('\r' : '\n' : rest) | not inQuotes -> newRow row cells rest
      ('\n' : rest) | not inQuotes -> newRow row cells rest
      ('\r' : rest) | not inQuotes -> newRow row cells rest
      (c : rest) -> go row (push cells c) inQuotes rest

    -- Cells accumulate in reverse; an empty cell must still exist.
    push cells c = case cells of
      [] -> [[c]]
      (cell : rest) -> ((c : cell) : rest)
    openCell cells = case cells of
      [] -> [""]
      other -> other
    cellsDone row cells = current cells : row
    current cells = case cells of
      [] -> ""
      (c : _) -> T.pack (reverse c)
    newRow row cells rest = reverse (cellsDone row cells) : go [] [] False rest
    finish row cells = case (row, cells) of
      ([], []) -> []
      _ -> [reverse (cellsDone row cells)]

-- dotenv ----------------------------------------------------------------------

decodeDotenv :: Text -> Either LaskFailure Value
decodeDotenv s = Right (objectValue (concatMap entry (T.lines s)))
  where
    entry raw =
      let line = T.strip raw
       in if T.null line || T.isPrefixOf "#" line
            then []
            else case T.breakOn "=" line of
              (_, "") -> []
              (name, rest) -> [(T.strip name, VString (unquote (T.strip (T.drop 1 rest))))]

    unquote t
      | wrapped "\"" t || wrapped "'" t = T.dropEnd 1 (T.drop 1 t)
      | otherwise = t
    wrapped q t = T.length t >= 2 && T.isPrefixOf q t && T.isSuffixOf q t

encodeDotenv :: Value -> Either LaskFailure Text
encodeDotenv v = case entriesOf v of
  Nothing -> Left (notRepresentable "dotenv requires an object at the top level")
  Just kvs -> T.unlines <$> mapM line kvs
  where
    line (k, VString t) = Right (k <> "=" <> quoted t)
    line (_, other) =
      Left (notRepresentable ("dotenv cannot represent " <> typeNameOf other))
    quoted t
      | T.null t = "''"
      | T.any (`elem` (" \t\"'#=\n\r" :: String)) t = "'" <> T.replace "'" "'\\''" t <> "'"
      | otherwise = t

-- Shared ----------------------------------------------------------------------

-- | Build a decoded object. Every format uses the one representation
-- 'valueFromJson' picks for a JSON object, because spec 15.8 leaves
-- the choice to the implementation but requires it to be the same
-- throughout.
objectValue :: [(Text, Value)] -> Value
objectValue = valueFromJson . A.Object . KM.fromList . map (\(k, v) -> (AK.fromText k, valueToJson v))

-- | The key-value pairs of a record or a map, in key order. Both
-- represent an object (spec 15.8 follows @from_json@ here).
entriesOf :: Value -> Maybe [(Text, Value)]
entriesOf v = case v of
  VRecord m -> Just (Map.toAscList m)
  VMap m -> Just (Map.toAscList m)
  _ -> Nothing
