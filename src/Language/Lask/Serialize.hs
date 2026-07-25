{-# LANGUAGE OverloadedStrings #-}

-- | Serialization of data values (spec chapter 13).
--
-- JSON is the canonical profile; @Void@\/@Environment@ become tagged
-- metadata objects and function values become 'FunctionRef' objects
-- (13.2). Deserialization ('valueFromJson') reads plain data only and
-- never restores tagged metadata (13.1 round-trip rules).
module Language.Lask.Serialize
  ( valueToJson,
    valueFromJson,
    encodeValue,
    encodeValuePretty,
    functionRefJson,
    renderValueText,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Language.Lask.Core.AST (Lam (..))
import Language.Lask.Runtime.Value
import Language.Lask.Types (renderType)

-- | Convert a runtime value to its canonical JSON representation.
valueToJson :: Value -> A.Value
valueToJson v = case v of
  VNumber n -> A.Number n
  VString t -> A.String t
  VBool b -> A.Bool b
  VNull -> A.Null
  VVoid -> A.object [("$type", A.String "Void")]
  VArray xs -> A.Array (V.map valueToJson xs)
  VMap m -> A.Object (KM.fromList [(AK.fromText k, valueToJson x) | (k, x) <- Map.toList m])
  VRecord m -> A.Object (KM.fromList [(AK.fromText k, valueToJson x) | (k, x) <- Map.toList m])
  VClosure (Closure lam _) -> functionRefJson lam
  VBuiltin name ->
    A.object
      [ ("$type", A.String "FunctionRef"),
        ("module", A.String "<builtin>"),
        ("name", A.String name)
      ]
  VAsync _ -> A.object [("$type", A.String "AsyncHandle")]
  VEnv (EnvValue kind params) ->
    A.object
      [ ("$type", A.String "Environment"),
        ("kind", A.String kind),
        ("params", A.Object (KM.fromList [(AK.fromText k, valueToJson x) | (k, x) <- Map.toList params]))
      ]

-- | 'FunctionRef' metadata (spec 13.2).
functionRefJson :: Lam -> A.Value
functionRefJson lam =
  A.object $
    [ ("$type", A.String "FunctionRef"),
      ("module", A.String (T.pack (lamModule lam))),
      ("name", A.String (lamName lam)),
      ("arity", A.Number (fromIntegral arity)),
      ("type", A.String (renderType (lamType lam)))
    ]
      <> [("variadic", A.Bool True) | variadic]
  where
    arity = length (lamPositional lam) + (if variadic then 1 else 0)
    variadic = maybe False (const True) (lamVariadic lam)

-- | Read plain JSON data as a value: objects become records
-- (consistently, spec 15.8); tagged metadata is not special-cased.
valueFromJson :: A.Value -> Value
valueFromJson v = case v of
  A.Number n -> VNumber n
  A.String t -> VString t
  A.Bool b -> VBool b
  A.Null -> VNull
  A.Array xs -> VArray (V.map valueFromJson xs)
  A.Object o ->
    VRecord (Map.fromList [(AK.toText k, valueFromJson x) | (k, x) <- KM.toList o])

encodeValue :: Value -> Text
encodeValue = TE.decodeUtf8 . BL.toStrict . A.encode . valueToJson

encodeValuePretty :: Value -> Text
encodeValuePretty = TE.decodeUtf8 . BL.toStrict . AP.encodePretty . valueToJson

-- | The @text@ profile (spec 13.1): strings unquoted, other values as
-- canonical JSON.
renderValueText :: Value -> Text
renderValueText v = case v of
  VString t -> t
  VNumber n -> formatNumber n
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  other -> encodeValue other
