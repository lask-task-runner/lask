{-# LANGUAGE OverloadedStrings #-}

-- | CLI argument decoding and binding (spec 11.2): kebab-to-snake
-- name mapping, @--arg-decode@ modes and binding against the static
-- declaration parameter info.
module Command.Lask.ArgCodec
  ( ArgDecodeMode (..),
    StdoutEncode (..),
    CliArg (..),
    parseArgDecodeMode,
    parseStdoutEncode,
    parseCliArgs,
    decodeArgValue,
    bindCliArgs,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Elaborate (StaticParams (..))
import Language.Lask.Runtime.Eval (castValueEither, renderCastMismatch)
import Language.Lask.Runtime.Value (Value (VString))
import Language.Lask.Serialize (valueFromJson)
import Language.Lask.Types (Type (TyEnvironment))
import Language.Lask.Utils (kebabToSnake)

data ArgDecodeMode = DecodeText | DecodeJson | DecodeAuto
  deriving (Show, Eq)

data StdoutEncode = EncodeText | EncodeJson | EncodePrettyJson
  deriving (Show, Eq)

parseArgDecodeMode :: String -> Maybe ArgDecodeMode
parseArgDecodeMode s = case s of
  "text" -> Just DecodeText
  "json" -> Just DecodeJson
  "auto" -> Just DecodeAuto
  _ -> Nothing

parseStdoutEncode :: String -> Maybe StdoutEncode
parseStdoutEncode s = case s of
  "text" -> Just EncodeText
  "json" -> Just EncodeJson
  "pretty-json" -> Just EncodePrettyJson
  _ -> Nothing

data CliArg = CliPos Text | CliKw Text Text
  deriving (Show, Eq)

-- | Split raw tokens after the function name into positional and
-- keyword arguments: @--name value@, @--name=value@, and @-c value@
-- for single-character names (spec 11.2).
parseCliArgs :: [Text] -> Either Text [CliArg]
parseCliArgs = go
  where
    go [] = Right []
    go (tok : rest)
      | Just body <- T.stripPrefix "--" tok, not (T.null body) = named body rest
      | Just body <- T.stripPrefix "-" tok,
        T.length body == 1 =
          named body rest
      | otherwise = (CliPos tok :) <$> go rest
    named body rest = case T.breakOn "=" body of
      (name, valueEq)
        | Just v <- T.stripPrefix "=" valueEq -> (CliKw (kebabToSnake name) v :) <$> go rest
      (name, "") -> case rest of
        (v : rest') -> (CliKw (kebabToSnake name) v :) <$> go rest'
        [] -> Left ("keyword argument '--" <> name <> "' needs a value")
      _ -> Left ("malformed keyword argument: '" <> body <> "'")

-- | Decode one raw argument (spec 11.2): @text@ = always String;
-- @json@ = must be JSON; @auto@ = JSON when possible, else String.
decodeArgValue :: ArgDecodeMode -> Text -> Either Text Value
decodeArgValue mode raw = case mode of
  DecodeText -> Right (VString raw)
  DecodeJson -> case decodeJson raw of
    Just v -> Right v
    Nothing -> Left ("argument is not valid JSON: '" <> raw <> "'")
  DecodeAuto -> Right (maybe (VString raw) id (decodeJson raw))
  where
    decodeJson t = valueFromJson <$> A.decode (BL.fromStrict (TE.encodeUtf8 t))

-- | Bind decoded CLI arguments against the declaration parameter info
-- (spec 11.2): positionals in order (excess collected by a variadic
-- parameter), keywords by mapped name, defaults for the rest. Type
-- conformance is checked by the same runtime check as @cast@.
bindCliArgs ::
  StaticParams ->
  ArgDecodeMode ->
  [CliArg] ->
  Either Text ([Value], [(Text, Value)])
bindCliArgs params mode cliArgs =
  let posRaw = [v | CliPos v <- cliArgs]
      kwRaw = [(n, v) | CliKw n v <- cliArgs]
      positional = spPositional params
      nPos = length positional
   in if any (\(_, t) -> t == TyEnvironment) positional
        then Left "functions with Environment positional parameters cannot be called from the CLI"
        else
          if length posRaw < nPos
            then
              Left $
                "missing positional arguments: expected "
                  <> tshow nPos
                  <> ", got "
                  <> tshow (length posRaw)
            else
              if length posRaw > nPos && spVariadic params == Nothing
                then
                  Left $
                    "too many positional arguments: expected "
                      <> tshow nPos
                      <> ", got "
                      <> tshow (length posRaw)
                else
                  let (boundRaw, extraRaw) = splitAt nPos posRaw
                      posTyped =
                        zip boundRaw (map snd positional)
                          <> case spVariadic params of
                            Just (_, elemTy) -> map (\v -> (v, elemTy)) extraRaw
                            Nothing -> []
                   in (,)
                        <$> mapM (decodeAndCheck "argument") posTyped
                        <*> bindKw [] kwRaw
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    kwTypes = spKeywords params

    bindKw acc [] = Right (reverse acc)
    bindKw acc ((n, raw) : rest)
      | n `elem` map fst acc = Left ("duplicate keyword argument: '--" <> n <> "'")
      | otherwise = case lookup n kwTypes of
          Nothing -> Left ("unknown keyword argument: '--" <> n <> "'")
          Just t
            | t == TyEnvironment ->
                Left ("keyword parameter '--" <> n <> "' has type Environment and cannot be set from the CLI")
            | otherwise -> do
                v <- decodeAndCheck ("keyword argument '--" <> n <> "'") (raw, t)
                bindKw ((n, v) : acc) rest

    decodeAndCheck what (raw, ty) = do
      v <- decodeArgValue mode raw
      case castValueEither ty v of
        Right v' -> Right v'
        Left cm
          -- Auto mode decodes JSON without knowing the declared
          -- type, so a value like `true` or an all-digit string
          -- can decode to a non-String even when the parameter
          -- wants `String`. Spec 11.2: "in auto mode, when
          -- ambiguous, String takes precedence" - so retry as the
          -- literal raw text before giving up.
          | mode == DecodeAuto,
            Right v2 <- castValueEither ty (VString raw) ->
              Right v2
          -- The mismatch is worded here rather than reused from
          -- `cast`: the user wrote a command line, not a cast.
          | otherwise ->
              Left (what <> " '" <> raw <> "' does not fit the parameter type" <> renderCastMismatch cm)
