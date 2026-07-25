{-# LANGUAGE OverloadedStrings #-}

-- | Semantic types (spec chapter 4): representation after alias
-- expansion, the conformance relation (4.4), comparability (6.2) and
-- well-formedness of @Void@ (4.2).
module Language.Lask.Types
  ( Type (..),
    renderType,
    conformsTo,
    comparable,
    isGround,
    wellFormed,
    errorType,
    commandResultType,
    stringifiable,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data Type
  = TyAny
  | TyNumber
  | TyString
  | TyBool
  | TyNull
  | TyVoid
  | TyEnvironment
  | TyArray Type
  | TyMap Type
  | TyRecord (Map Text Type)
  | TyAsync Type
  | -- | Positional parameter types and return type. Keyword
    -- parameters and variadic-ness are not part of the type (4.4).
    TyFun [Type] Type
  | -- | Type variable; only occurs in builtin schemes (4.4).
    TyVar Text
  deriving (Show, Eq, Ord)

-- | Builtin alias @Error = Record\<code: Number, message: String\>@.
errorType :: Type
errorType = TyRecord (Map.fromList [("code", TyNumber), ("message", TyString)])

-- | Builtin alias
-- @CommandResult = Record\<code: Number, stdout: String, stderr: String\>@.
commandResultType :: Type
commandResultType =
  TyRecord (Map.fromList [("code", TyNumber), ("stdout", TyString), ("stderr", TyString)])

renderType :: Type -> Text
renderType t = case t of
  TyAny -> "Any"
  TyNumber -> "Number"
  TyString -> "String"
  TyBool -> "Bool"
  TyNull -> "Null"
  TyVoid -> "Void"
  TyEnvironment -> "Environment"
  TyArray e -> "Array<" <> renderType e <> ">"
  TyMap e -> "Map<" <> renderType e <> ">"
  TyRecord fs ->
    "Record<"
      <> T.intercalate ", " [renderField k <> ": " <> renderType v | (k, v) <- Map.toList fs]
      <> ">"
  TyAsync e -> "AsyncHandle<" <> renderType e <> ">"
  TyFun ps r -> "Function<" <> T.intercalate ", " (map renderType (ps <> [r])) <> ">"
  TyVar v -> v
  where
    renderField k
      | isLowerId k = k
      | otherwise = "\"" <> k <> "\""
    isLowerId k = case T.uncons k of
      Just (c, rest) ->
        (c >= 'a' && c <= 'z' || c == '_') && T.all identChar rest
      Nothing -> False
    identChar c =
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'

-- | @conformsTo t u@: an expression of type @t@ may be placed where
-- @u@ is required (spec 4.4). Reflexive structural identity, plus
-- @Any@ as the sole top type. No variance.
conformsTo :: Type -> Type -> Bool
conformsTo _ TyAny = True
conformsTo t u = t == u

-- | Comparable types for @==@\/@!=@ (spec 6.2).
comparable :: Type -> Bool
comparable t = case t of
  TyNumber -> True
  TyString -> True
  TyBool -> True
  TyNull -> True
  TyEnvironment -> True
  TyArray e -> comparable e
  TyMap e -> comparable e
  TyRecord fs -> all comparable (Map.elems fs)
  _ -> False

-- | No type variables remain.
isGround :: Type -> Bool
isGround t = case t of
  TyVar _ -> False
  TyArray e -> isGround e
  TyMap e -> isGround e
  TyRecord fs -> all isGround (Map.elems fs)
  TyAsync e -> isGround e
  TyFun ps r -> all isGround ps && isGround r
  _ -> True

-- | Well-formedness (spec 4.2): @Void@ may only appear as a function
-- return type or as the argument of @AsyncHandle@.
wellFormed :: Type -> Bool
wellFormed = go True
  where
    -- The flag says whether Void is allowed at this position.
    go voidOk t = case t of
      TyVoid -> voidOk
      TyArray e -> go False e
      TyMap e -> go False e
      TyRecord fs -> all (go False) (Map.elems fs)
      TyAsync e -> go True e
      TyFun ps r -> all (go False) ps && go True r
      _ -> True

-- | Types accepted inside string\/command interpolation @#{...}@
-- (spec 6.6: a non-stringifiable interpolation is a type error).
stringifiable :: Type -> Bool
stringifiable t = case t of
  TyString -> True
  TyNumber -> True
  TyBool -> True
  TyAny -> True
  _ -> False
