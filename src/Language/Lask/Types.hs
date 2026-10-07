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
    conformsUnder,
    applySubst,
    comparable,
    orderable,
    NamedBound (..),
    Bound (..),
    namedBoundFromText,
    renderNamedBound,
    renderBound,
    satisfiesNamed,
    satisfiesBound,
    boundEntails,
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
  | -- | An environment with its run options (spec 6.7, 15.5).
    -- 'TyEnvironment' conforms to it.
    TyRunnable
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
      TyRunnable -> 4
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
  TyRunnable -> "Runnable"
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
conformsTo = conformsUnder (const Nothing)

-- | 'conformsTo' inside a declaration whose type parameters may carry
-- type bounds (spec 4.4): a parameter with the type bound @B@ also
-- conforms to whatever @B@ conforms to. The first argument gives the
-- type bound of a parameter in scope.
conformsUnder :: (Text -> Maybe Type) -> Type -> Type -> Bool
conformsUnder upper = go
  where
    go _ TyAny = True
    -- Union elimination: every member has to fit where the union is used.
    go (TyUnion ts) u = all (`go` u) ts
    go t u
      | t == u = True
      -- Union introduction: fitting one member is enough.
      | TyUnion us <- u, any (go t) us = True
      -- An environment is a runnable with no run options (spec 4.4).
      | t == TyEnvironment, u == TyRunnable = True
      | TyVar v <- t, Just b <- upper v = go b u
      | otherwise = False

-- | The named bounds of a type parameter (spec 4.2): a closed set of
-- predicates, written in lower case where a bound is expected.
data NamedBound = BComparable | BOrderable | BStringifiable
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | The bound of a type parameter (spec 4.2): a named predicate, or a
-- type that an instantiation has to conform to.
data Bound = BoundNamed NamedBound | BoundType Type
  deriving (Show, Eq)

namedBoundFromText :: Text -> Maybe NamedBound
namedBoundFromText t = lookup t [(renderNamedBound b, b) | b <- [minBound .. maxBound]]

renderNamedBound :: NamedBound -> Text
renderNamedBound b = case b of
  BComparable -> "comparable"
  BOrderable -> "orderable"
  BStringifiable -> "stringifiable"

renderBound :: Bound -> Text
renderBound (BoundNamed b) = renderNamedBound b
renderBound (BoundType t) = renderType t

-- | Whether a type satisfies a named predicate (spec 4.4), given the
-- bounds of the type parameters in scope: a parameter satisfies it
-- when its bound entails it, and otherwise not at all.
satisfiesNamed :: (Text -> Maybe Bound) -> NamedBound -> Type -> Bool
satisfiesNamed boundOf nb = go
  where
    var v = maybe False (\b -> boundEntails b nb) (boundOf v)
    go t = case t of
      TyVar v -> var v
      _ -> case nb of
        BComparable -> comparableWith var t
        BOrderable -> orderable t
        BStringifiable -> stringifiableWith var t

-- | Whether a type satisfies a bound: the predicate for a named one,
-- conformance for a type bound (spec 4.4).
satisfiesBound :: (Text -> Maybe Bound) -> Bound -> Type -> Bool
satisfiesBound boundOf b t = case b of
  BoundNamed nb -> satisfiesNamed boundOf nb t
  BoundType u -> conformsUnder upper t u
  where
    upper v = case boundOf v of
      Just (BoundType u) -> Just u
      _ -> Nothing

-- | Whether every instantiation a bound admits satisfies a named
-- predicate (spec 4.4). @orderable@ admits only Number and String,
-- which are comparable and stringifiable. A type bound entails what it
-- satisfies itself, except @Any@, which every type conforms to. A type
-- bound mentions no type parameter (4.2), so it is checked alone.
boundEntails :: Bound -> NamedBound -> Bool
boundEntails b nb = case b of
  BoundNamed n -> n == nb || n == BOrderable
  BoundType TyAny -> False
  BoundType t -> satisfiesNamed (const Nothing) nb t

-- | Comparable types for @==@\/@!=@ (spec 6.2).
comparable :: Type -> Bool
comparable = comparableWith (const False)

-- | 'comparable', where a type parameter is comparable as the given
-- function says (by its bound, spec 4.4).
comparableWith :: (Text -> Bool) -> Type -> Bool
comparableWith var = go
  where
    go t = case t of
      TyNumber -> True
      TyString -> True
      TyBool -> True
      TyNull -> True
      TyEnvironment -> True
      TyRunnable -> True
      TyArray e -> go e
      TyMap e -> go e
      TyRecord fs -> all (go . fieldType) (Map.elems fs)
      TyUnion ts -> all go ts
      TyVar v -> var v
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
stringifiable = stringifiableWith (const False)

stringifiableWith :: (Text -> Bool) -> Type -> Bool
stringifiableWith var = go
  where
    go t = case t of
      TyString -> True
      TyNumber -> True
      TyBool -> True
      TyAny -> True
      TyUnion ts -> all go ts
      TyVar v -> var v
      _ -> False
