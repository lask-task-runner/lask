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
    instantiateForCli,
    checkCliBounds,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Elaborate (StaticParams (..))
import Language.Lask.Runtime.Eval (castValueEither, renderCastMismatch)
import Language.Lask.Runtime.Value (Value (..))
import Language.Lask.Serialize (valueFromJson)
import Language.Lask.Types
  ( Bound (..),
    Field (..),
    Type (..),
    applySubst,
    renderNamedBound,
    renderType,
    requiredField,
    satisfiesNamed,
  )
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

-- | The parameters of a declaration with type parameters, as the CLI
-- binds them (spec 11.2): a parameter with a type bound at its bound,
-- and every other at @Any@. The CLI has no type to instantiate them
-- from, so it decodes against the widest type each admits, and a
-- named bound is checked afterwards against what was decoded
-- ('checkCliBounds').
instantiateForCli :: [Text] -> Map Text Bound -> StaticParams -> StaticParams
instantiateForCli vs bounds ps
  | null vs = ps
  | otherwise =
      StaticParams
        [(n, at t) | (n, t) <- spPositional ps]
        (fmap (fmap at) (spVariadic ps))
        [(n, at t) | (n, t) <- spKeywords ps]
  where
    at = applySubst (Map.fromList [(v, widest v) | v <- vs])
    widest v = case Map.lookup v bounds of
      Just (BoundType b) -> b
      _ -> TyAny

-- | A type parameter with a named bound is instantiated from the
-- arguments the CLI decoded, as a call instantiates it from the types
-- of its arguments (spec 11.2): the arguments have to agree on one
-- type, and that type has to satisfy the bound.
checkCliBounds :: Map Text Bound -> StaticParams -> [Value] -> [(Text, Value)] -> Either Text ()
checkCliBounds bounds params posVals kwVals =
  mapM_ check [(v, nb) | (v, BoundNamed nb) <- Map.toList bounds]
  where
    (boundVals, extraVals) = splitAt (length (spPositional params)) posVals
    slots =
      zip (map snd (spPositional params)) boundVals
        <> [(elemTy, x) | Just (_, elemTy) <- [spVariadic params], x <- extraVals]
        <> [(t, x) | (n, x) <- kwVals, Just t <- [lookup n (spKeywords params)]]
    check (v, nb) = case nub (concat [evidence v t x | (t, x) <- slots]) of
      [] -> Right ()
      [t]
        | satisfiesNamed (const Nothing) nb t -> Right ()
        | otherwise ->
            Left ("the arguments make " <> v <> " " <> renderType t <> ", which is not " <> renderNamedBound nb)
      ts ->
        Left
          ( "the arguments give "
              <> v
              <> " more than one type ("
              <> T.intercalate ", " (map renderType ts)
              <> "); it has to be one "
              <> renderNamedBound nb
              <> " type"
          )

-- | The types a decoded value gives a type variable, where the
-- parameter's type places the variable.
evidence :: Text -> Type -> Value -> [Type]
evidence v pat val = case (pat, val) of
  (TyVar w, _) | w == v -> [valueType val]
  (TyArray p, VArray xs) -> concatMap (evidence v p) xs
  (TyMap p, VMap m) -> concatMap (evidence v p) (Map.elems m)
  (TyRecord fs, VRecord m) ->
    concat [evidence v (fieldType f) x | (k, f) <- Map.toList fs, Just x <- [Map.lookup k m]]
  _ -> []

-- | The type of a decoded value, as a literal of it would infer (4.3):
-- a heterogeneous array is @Array<Any>@.
valueType :: Value -> Type
valueType val = case val of
  VNumber _ -> TyNumber
  VString _ -> TyString
  VBool _ -> TyBool
  VNull -> TyNull
  VArray xs -> TyArray (common (map valueType (foldr (:) [] xs)))
  VMap m -> TyMap (common (map valueType (Map.elems m)))
  VRecord m -> TyRecord (Map.map (requiredField . valueType) m)
  _ -> TyAny
  where
    common ts = case nub ts of
      [t] -> t
      _ -> TyAny
