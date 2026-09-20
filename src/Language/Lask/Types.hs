{-# LANGUAGE OverloadedStrings #-}

-- | Semantic types (spec chapter 4): representation after alias
-- expansion, the conformance relation (4.4), comparability (6.2) and
-- well-formedness of @Void@ (4.2).
module Language.Lask.Types
  ( Type (..),
    Field (..),
    requiredField,
    requiredNames,
    mkUnion,
    unionMembers,
    dataType,
    renderType,
    conformsTo,
    applySubst,
    comparable,
    orderable,
    isGround,
    typeVars,
    wellFormed,
    errorType,
    commandResultType,
    stringifiable,
  )
where

import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
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
  | -- | Fields by name, each carrying whether its key may be absent
    -- (spec 4.2). Optionality is a property of the key; whether the
    -- value may be null is a property of the field's type.
    TyRecord (Map Text Field)
  | TyAsync Type
  | -- | Positional parameter types and return type. Keyword
    -- parameters and variadic-ness are not part of the type (4.4).
    TyFun [Type] Type
  | -- | Type variable; only occurs in builtin schemes (4.4).
    TyVar Text
  | -- | Union of two or more members in the canonical order of 4.2.
    -- Construct only through 'mkUnion', which is what establishes
    -- that invariant; pattern matching on it is fine anywhere.
    TyUnion [Type]
  deriving (Show, Eq, Ord)

-- | The union of a member and any number of further members, reduced
-- to the canonical form of 4.2: members are flattened, @Any@ absorbs
-- the whole union, duplicates are dropped, the rest are ordered, and a
-- union of one member is that member.
--
-- Taking the first member separately keeps the function total: there
-- is no empty type for @mkUnion []@ to denote.
mkUnion :: Type -> [Type] -> Type
mkUnion t ts
  | TyAny `elem` flat = TyAny
  | otherwise = case sortOn key (nub flat) of
      [] -> TyAny -- unreachable: flat always holds at least t
      [only] -> only
      members -> TyUnion members
  where
    flat = concatMap unionMembers (t : ts)
    key u = (rank u, renderType u)
    -- The order the type forms are listed in 4.1, except that Null is
    -- always last, so that @String | Null@ reads as it was written.
    rank u = case u of
      TyAny -> 0 :: Int
      TyNumber -> 1
      TyString -> 2
      TyBool -> 3
      TyEnvironment -> 4
      TyArray _ -> 5
      TyMap _ -> 6
      TyRecord _ -> 7
      TyAsync _ -> 8
      TyFun _ _ -> 9
      TyVar _ -> 10
      TyVoid -> 11
      TyUnion _ -> 12
      TyNull -> 13

-- | The members of a union, or the type itself for anything else.
unionMembers :: Type -> [Type]
unionMembers (TyUnion ts) = ts
unionMembers t = [t]

-- | Data types (4.2): the types a union may have as a member and the
-- types @cast@ may check at runtime (15.8). A type variable passes,
-- because it stands for whatever it is instantiated to, and the
-- instantiation is checked by 'wellFormed' in its turn.
dataType :: Type -> Bool
dataType t = case t of
  TyVoid -> False
  TyFun _ _ -> False
  TyAsync _ -> False
  TyArray e -> dataType e
  TyMap e -> dataType e
  TyRecord fs -> all (dataType . fieldType) (Map.elems fs)
  TyUnion ts -> all dataType ts
  _ -> True

-- | One field of a record type (spec 4.2).
data Field = Field
  { -- | @True@ for a field written @name?: T@: the key may be absent.
    fieldOptional :: Bool,
    fieldType :: Type
  }
  deriving (Show, Eq, Ord)

-- | A field whose key must be present.
requiredField :: Type -> Field
requiredField = Field False

-- | The names of the fields whose key must be present.
requiredNames :: Map Text Field -> Set Text
requiredNames fs = Map.keysSet (Map.filter (not . fieldOptional) fs)

-- | Builtin alias @Error = Record\<code: Number, message: String\>@.
errorType :: Type
errorType = TyRecord (Map.fromList [("code", requiredField TyNumber), ("message", requiredField TyString)])

-- | Builtin alias
-- @CommandResult = Record\<code: Number, stdout: String, stderr: String\>@.
commandResultType :: Type
commandResultType =
  TyRecord
    ( Map.fromList
        [ ("code", requiredField TyNumber),
          ("stdout", requiredField TyString),
          ("stderr", requiredField TyString)
        ]
    )

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
      <> T.intercalate
        ", "
        [ renderField k <> (if fieldOptional f then "?" else "") <> ": " <> renderType (fieldType f)
        | (k, f) <- Map.toList fs
        ]
      <> ">"
  TyAsync e -> "AsyncHandle<" <> renderType e <> ">"
  TyFun ps r -> "Function<" <> T.intercalate ", " (map renderType (ps <> [r])) <> ">"
  TyVar v -> v
  TyUnion ts -> T.intercalate " | " (map renderType ts)
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

-- | Substitute type variables (spec 4.4 instantiation, 4.2 alias
-- expansion). Unions are rebuilt through 'mkUnion': a substitution can
-- make two members the same, or bring an @Any@ in.
applySubst :: Map Text Type -> Type -> Type
applySubst s t = case t of
  TyVar v -> Map.findWithDefault t v s
  TyArray e -> TyArray (applySubst s e)
  TyMap e -> TyMap (applySubst s e)
  TyRecord fs -> TyRecord (Map.map (\f -> f {fieldType = applySubst s (fieldType f)}) fs)
  TyAsync e -> TyAsync (applySubst s e)
  TyFun ps r -> TyFun (map (applySubst s) ps) (applySubst s r)
  -- Rebuilt through 'mkUnion': substitution can make two members the
  -- same, or bring an Any in, and the result has to stay canonical.
  TyUnion (u : us) -> mkUnion (applySubst s u) (map (applySubst s) us)
  _ -> t


-- | @conformsTo t u@: an expression of type @t@ may be placed where
-- @u@ is requiredField (spec 4.4). Reflexive structural identity, plus
-- @Any@ as the sole top type. No variance.
conformsTo :: Type -> Type -> Bool
conformsTo _ TyAny = True
-- Union elimination: every member has to fit where the union is used.
conformsTo (TyUnion ts) u = all (`conformsTo` u) ts
-- Union introduction: fitting one member is enough.
conformsTo t (TyUnion us) = any (conformsTo t) us
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
  TyRecord fs -> all (comparable . fieldType) (Map.elems fs)
  TyUnion ts -> all comparable ts
  _ -> False

-- | Types that @sort@ \/ @sort_by@ can order (spec 15.4).
--
-- Narrower than 'comparable': equality is structural and works on
-- every data type, but an order has to be a total one. It is defined
-- here for the sort functions alone and does not extend the ordering
-- operators of 6.2, which stay @Number@-only.
orderable :: Type -> Bool
orderable t = case t of
  TyNumber -> True
  TyString -> True
  _ -> False

-- | The type variables a type mentions, in no particular order.
typeVars :: Type -> [Text]
typeVars t = case t of
  TyVar v -> [v]
  TyArray e -> typeVars e
  TyMap e -> typeVars e
  TyRecord fs -> concatMap (typeVars . fieldType) (Map.elems fs)
  TyAsync e -> typeVars e
  TyFun ps r -> concatMap typeVars ps <> typeVars r
  TyUnion ts -> concatMap typeVars ts
  _ -> []

-- | No type variables remain.
isGround :: Type -> Bool
isGround t = case t of
  TyVar _ -> False
  TyUnion ts -> all isGround ts
  TyArray e -> isGround e
  TyMap e -> isGround e
  TyRecord fs -> all (isGround . fieldType) (Map.elems fs)
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
      TyRecord fs -> all (go False . fieldType) (Map.elems fs)
      TyAsync e -> go True e
      TyFun ps r -> all (go False) ps && go True r
      -- A union admits data types only (4.2), which already excludes
      -- Void, so the members are checked with Void disallowed.
      TyUnion ts -> all dataType ts && all (go False) ts
      _ -> True

-- | Types accepted inside string\/command interpolation @#{...}@
-- (spec 6.6: a non-stringifiable interpolation is a type error).
stringifiable :: Type -> Bool
stringifiable t = case t of
  TyString -> True
  TyNumber -> True
  TyBool -> True
  TyAny -> True
  TyUnion ts -> all stringifiable ts
  _ -> False
