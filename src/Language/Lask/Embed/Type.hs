{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The types of "Language.Lask.Embed", at the type level: the 'Ty'
-- kind, its reflection to "Language.Lask.Types", and the rules of spec
-- chapter 4 computed by GHC.
--
-- * Unions are built in canonical form (spec 4.2), so two spellings of
--   one union are one Haskell type.
-- * Records are sorted by field name, so field order is irrelevant.
-- * Conformance (4.4) is a closed type family, 'Conf'.
-- * The predicates of 4.2 and 4.4 (data type, well-formed, comparable,
--   orderable, stringifiable) are Bool families. Each is required through
--   'Holds', whose method an operation forces, so that a program
--   compiled with @-fdefer-type-errors@ fails where it breaks the rule.
--
-- The canonical order and rendering are those of
-- 'Language.Lask.Types.mkUnion' and 'Language.Lask.Types.renderType'; a
-- property test holds the two together.
module Language.Lask.Embed.Type
  ( -- * Types
    Ty (..),
    Field (..),
    Bound (..),
    Param (..),
    KnownTy (..),
    KnownBound (..),
    KnownFields (..),
    KnownTys (..),

    -- * Canonical forms
    type (:|),
    Rec,
    Canon,
    Render,

    -- * Conformance
    Conf,
    Conforms,
    conforms,

    -- * Predicates
    Holds (..),
    IsData,
    DataTy,
    dataTy,
    IsWF,
    WF,
    wf,
    WFResult,
    wfResult,
    IsComparable,
    Comparable,
    comparable,
    IsOrderable,
    Orderable,
    orderable,
    IsStringifiable,
    Stringify,
    stringify,

    -- * Records
    FieldOf,
    HasField,
    hasField,
    UniqueKeys,
    uniqueKeys,
    Required,
    FitsRecord,
    fitsRecord,

    -- * Operations
    OrNull,
    WhenTy,
    whenTy,
    EqTypes,
    eqTypes,
    Concrete,
    concrete,

    -- * Type parameters
    GenericVars,
    genericVars,
    Subst,
    SubstParams,
    SubstL,
    SubstParamsL,
    Satisfies,
    satisfies,
  )
where

import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeError (Unsatisfiable, unsatisfiable)
import GHC.TypeLits
  ( CmpChar,
    CmpNat,
    CmpSymbol,
    ErrorMessage (..),
    KnownSymbol,
    Nat,
    Symbol,
    TypeError,
    UnconsSymbol,
    symbolVal,
    type AppendSymbol,
  )
import qualified Language.Lask.Types as LT

-- The kind -------------------------------------------------------------------

-- | The Lask types (spec 4.1), used promoted.
data Ty
  = TAny
  | TNumber
  | TString
  | TBool
  | TNull
  | TVoid
  | TEnv
  | TRunnable
  | TArray Ty
  | TMap Ty
  | -- | Build with 'Rec', which sorts the fields.
    TRecord [Field]
  | TAsync Ty
  | -- | Positional parameter types, and the result.
    TFun [Ty] Ty
  | -- | Build with ':|', which puts the union in canonical form.
    TUnion [Ty]
  | -- | A type parameter with its bound, inside the declaration that
    -- binds it (spec 4.2).
    TVar Symbol Bound

-- | A record field: @name: T@ or @name?: T@.
data Field = Symbol := Ty | Symbol :? Ty

infix 4 :=, :?

-- | The bound of a type parameter (spec 4.2).
data Bound = BNone | BComparable | BOrderable | BStringifiable | BType Ty

-- | A positional parameter, as in @fact(n: Number)@:
-- @'[\"n\" ::: 'TNumber]@.
data Param = Symbol ::: Ty

infix 4 :::

-- Reflection -------------------------------------------------------------------

-- | The Lask type a promoted 'Ty' stands for.
class KnownTy (t :: Ty) where
  tyOf :: LT.Type

instance KnownTy 'TAny where tyOf = LT.TyAny

instance KnownTy 'TNumber where tyOf = LT.TyNumber

instance KnownTy 'TString where tyOf = LT.TyString

instance KnownTy 'TBool where tyOf = LT.TyBool

instance KnownTy 'TNull where tyOf = LT.TyNull

instance KnownTy 'TVoid where tyOf = LT.TyVoid

instance KnownTy 'TEnv where tyOf = LT.TyEnvironment

instance KnownTy 'TRunnable where tyOf = LT.TyRunnable

instance (KnownTy t) => KnownTy ('TArray t) where tyOf = LT.TyArray (tyOf @t)

instance (KnownTy t) => KnownTy ('TMap t) where tyOf = LT.TyMap (tyOf @t)

instance (KnownFields fs) => KnownTy ('TRecord fs) where
  tyOf = LT.TyRecord (Map.fromList (fieldsOf @fs))

instance (KnownTy t) => KnownTy ('TAsync t) where tyOf = LT.TyAsync (tyOf @t)

instance (KnownTys ps, KnownTy r) => KnownTy ('TFun ps r) where
  tyOf = LT.TyFun (tysOf @ps) (tyOf @r)

-- A union is reflected as it is, not rebuilt through mkUnion: the
-- canonical form is GHC's to compute (':|'), and the property test holds
-- it to mkUnion's. A union not in canonical form is not a data type.
instance (KnownTys ts) => KnownTy ('TUnion ts) where
  tyOf = LT.TyUnion (tysOf @ts)

instance (KnownSymbol n) => KnownTy ('TVar n b) where
  tyOf = LT.TyVar (T.pack (symbolVal (Proxy @n)))

-- | The Lask types of a list of promoted types.
class KnownTys (ts :: [Ty]) where
  tysOf :: [LT.Type]

instance KnownTys '[] where tysOf = []

instance (KnownTy t, KnownTys ts) => KnownTys (t ': ts) where
  tysOf = tyOf @t : tysOf @ts

-- | The fields of a promoted record type.
class KnownFields (fs :: [Field]) where
  fieldsOf :: [(Text, LT.Field)]

instance KnownFields '[] where fieldsOf = []

instance (KnownSymbol n, KnownTy t, KnownFields fs) => KnownFields ((n ':= t) ': fs) where
  fieldsOf = (T.pack (symbolVal (Proxy @n)), LT.Field False (tyOf @t)) : fieldsOf @fs

instance (KnownSymbol n, KnownTy t, KnownFields fs) => KnownFields ((n ':? t) ': fs) where
  fieldsOf = (T.pack (symbolVal (Proxy @n)), LT.Field True (tyOf @t)) : fieldsOf @fs

-- | The bound recorded for a type parameter, if it has one.
class KnownBound (b :: Bound) where
  boundOf :: Maybe LT.Bound

instance KnownBound 'BNone where boundOf = Nothing

instance KnownBound 'BComparable where boundOf = Just (LT.BoundNamed LT.BComparable)

instance KnownBound 'BOrderable where boundOf = Just (LT.BoundNamed LT.BOrderable)

instance KnownBound 'BStringifiable where boundOf = Just (LT.BoundNamed LT.BStringifiable)

instance (KnownTy t) => KnownBound ('BType t) where boundOf = Just (LT.BoundType (tyOf @t))

-- Rendering, as Language.Lask.Types.renderType -------------------------------------

-- | A type in Lask notation, for ordering unions and for messages.
type family Render (t :: Ty) :: Symbol where
  Render 'TAny = "Any"
  Render 'TNumber = "Number"
  Render 'TString = "String"
  Render 'TBool = "Bool"
  Render 'TNull = "Null"
  Render 'TVoid = "Void"
  Render 'TEnv = "Environment"
  Render 'TRunnable = "Runnable"
  Render ('TArray t) = Wrap "Array<" (Render t) ">"
  Render ('TMap t) = Wrap "Map<" (Render t) ">"
  Render ('TRecord fs) = Wrap "Record<" (RenderFields fs) ">"
  Render ('TAsync t) = Wrap "AsyncHandle<" (Render t) ">"
  Render ('TFun ps r) = Wrap "Function<" (Join ", " (RenderAll (Snoc ps r))) ">"
  Render ('TUnion ts) = Join " | " (RenderAll ts)
  Render ('TVar n _) = n

type Wrap a b c = AppendSymbol a (AppendSymbol b c)

type family RenderAll (ts :: [Ty]) :: [Symbol] where
  RenderAll '[] = '[]
  RenderAll (t ': ts) = Render t ': RenderAll ts

type family RenderFields (fs :: [Field]) :: Symbol where
  RenderFields fs = Join ", " (RenderFieldList fs)

type family RenderFieldList (fs :: [Field]) :: [Symbol] where
  RenderFieldList '[] = '[]
  RenderFieldList ((n ':= t) ': fs) = Wrap (FieldName n) ": " (Render t) ': RenderFieldList fs
  RenderFieldList ((n ':? t) ': fs) = Wrap (FieldName n) "?: " (Render t) ': RenderFieldList fs

-- | A field name as renderType writes it: quoted unless a lower_id.
type family FieldName (n :: Symbol) :: Symbol where
  FieldName n = If (IsLowerId n) n (Wrap "\"" n "\"")

type family Join (sep :: Symbol) (xs :: [Symbol]) :: Symbol where
  Join _ '[] = ""
  Join _ '[x] = x
  Join sep (x ': xs) = AppendSymbol x (AppendSymbol sep (Join sep xs))

type family Snoc (xs :: [k]) (x :: k) :: [k] where
  Snoc '[] x = '[x]
  Snoc (y ': ys) x = y ': Snoc ys x

type family If (c :: Bool) (a :: k) (b :: k) :: k where
  If 'True a _ = a
  If 'False _ b = b

type family IsLowerId (n :: Symbol) :: Bool where
  IsLowerId n = LowerIdChars (UnconsSymbol n) 'True

type family LowerIdChars (m :: Maybe (Char, Symbol)) (first :: Bool) :: Bool where
  LowerIdChars 'Nothing first = Not first
  LowerIdChars ('Just '(c, rest)) 'True = And (Or (InRange c 'a' 'z') (IsChar c '_')) (LowerIdChars (UnconsSymbol rest) 'False)
  LowerIdChars ('Just '(c, rest)) 'False =
    And (Or (Or (InRange c 'a' 'z') (InRange c 'A' 'Z')) (Or (InRange c '0' '9') (IsChar c '_'))) (LowerIdChars (UnconsSymbol rest) 'False)

type family InRange (c :: Char) (lo :: Char) (hi :: Char) :: Bool where
  InRange c lo hi = And (Not (IsLT (CmpChar c lo))) (Not (IsGT (CmpChar c hi)))

type family IsChar (c :: Char) (d :: Char) :: Bool where
  IsChar c c = 'True
  IsChar _ _ = 'False

type family IsLT (o :: Ordering) :: Bool where
  IsLT 'LT = 'True
  IsLT _ = 'False

type family IsGT (o :: Ordering) :: Bool where
  IsGT 'GT = 'True
  IsGT _ = 'False

type family Not (b :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True b = b
  And 'False _ = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False b = b

-- Canonical unions, as Language.Lask.Types.mkUnion ----------------------------

-- | The order of mkUnion: the forms in the order of 4.1, Null last.
type family Rank (t :: Ty) :: Nat where
  Rank 'TAny = 0
  Rank 'TNumber = 1
  Rank 'TString = 2
  Rank 'TBool = 3
  Rank 'TEnv = 4
  Rank 'TRunnable = 4
  Rank ('TArray _) = 5
  Rank ('TMap _) = 6
  Rank ('TRecord _) = 7
  Rank ('TAsync _) = 8
  Rank ('TFun _ _) = 9
  Rank ('TVar _ _) = 10
  Rank 'TVoid = 11
  Rank ('TUnion _) = 12
  Rank 'TNull = 13

type family CmpTy (a :: Ty) (b :: Ty) :: Ordering where
  CmpTy a b = Then (CmpNat (Rank a) (Rank b)) (CmpSymbol (Render a) (Render b))

type family Then (o :: Ordering) (p :: Ordering) :: Ordering where
  Then 'EQ p = p
  Then o _ = o

type family Insert (x :: Ty) (xs :: [Ty]) :: [Ty] where
  Insert x '[] = '[x]
  Insert x (y ': ys) = InsertBy (CmpTy x y) x y ys

type family InsertBy (o :: Ordering) (x :: Ty) (y :: Ty) (ys :: [Ty]) :: [Ty] where
  InsertBy 'LT x y ys = x ': y ': ys
  InsertBy 'EQ _ y ys = y ': ys
  InsertBy 'GT x y ys = y ': Insert x ys

type family Members (t :: Ty) :: [Ty] where
  Members ('TUnion ts) = ts
  Members t = '[t]

type family Collect (ts :: [Ty]) (acc :: [Ty]) :: [Ty] where
  Collect '[] acc = acc
  Collect (t ': ts) acc = Collect ts (InsertAll (Members t) acc)

type family InsertAll (xs :: [Ty]) (acc :: [Ty]) :: [Ty] where
  InsertAll '[] acc = acc
  InsertAll (x ': xs) acc = InsertAll xs (Insert x acc)

type family Close (ts :: [Ty]) :: Ty where
  Close '[] = 'TAny
  Close ('TAny ': _) = 'TAny
  Close '[t] = t
  Close ts = 'TUnion ts

-- | The canonical union of the given types (spec 4.2).
type family Canon (ts :: [Ty]) :: Ty where
  Canon ts = Close (Collect ts '[])

-- | @a | b@, in canonical form.
type a :| b = Canon '[a, b]

infixr 5 :|

-- Records ---------------------------------------------------------------------------

-- | A record type, with its fields sorted by name. Two fields of one
-- name are a type error (@E-TYPE-FIELD-DUPLICATE@).
type family Rec (fs :: [Field]) :: Ty where
  Rec fs = 'TRecord (SortFields fs '[])

type family FieldKey (f :: Field) :: Symbol where
  FieldKey (n ':= _) = n
  FieldKey (n ':? _) = n

type family SortFields (fs :: [Field]) (acc :: [Field]) :: [Field] where
  SortFields '[] acc = acc
  SortFields (f ': fs) acc = SortFields fs (InsertField f acc)

type family InsertField (f :: Field) (fs :: [Field]) :: [Field] where
  InsertField f '[] = '[f]
  InsertField f (g ': gs) = InsertFieldBy (CmpSymbol (FieldKey f) (FieldKey g)) f g gs

type family InsertFieldBy (o :: Ordering) (f :: Field) (g :: Field) (gs :: [Field]) :: [Field] where
  InsertFieldBy 'LT f g gs = f ': g ': gs
  InsertFieldBy 'GT f g gs = g ': InsertField f gs
  InsertFieldBy 'EQ f _ _ =
    TypeError ('Text "the record has two fields named " ':<>: 'ShowType (FieldKey f))

-- | The type of reading a field (spec 6.8): an optional field reads as
-- @T | Null@.
type family FieldOf (n :: Symbol) (t :: Ty) :: Ty where
  FieldOf n ('TRecord fs) = LookupField n fs fs
  FieldOf n t = TypeError ('Text "field access on " ':<>: 'Text (Render t) ':<>: 'Text ", which is not a record")

type family LookupField (n :: Symbol) (fs :: [Field]) (all :: [Field]) :: Ty where
  LookupField n ((n ':= t) ': _) _ = t
  LookupField n ((n ':? t) ': _) _ = t :| 'TNull
  LookupField n (_ ': fs) all = LookupField n fs all
  LookupField n '[] all =
    TypeError ('Text "the record has no field " ':<>: 'ShowType n ':<>: 'Text ": Record<" ':<>: 'Text (RenderFields all) ':<>: 'Text ">")

-- | The fields of a record literal with no expected type: all required
-- (spec 4.3).
type family Required (kvs :: [(Symbol, Ty)]) :: [Field] where
  Required '[] = '[]
  Required ('(k, t) ': kvs) = (k ':= t) ': Required kvs

-- | A record literal checked against an expected record type (spec 4.3):
-- every key it gives is a field whose type its value conforms to, and
-- it gives every required field.
type FitsRecord r kvs = Holds (Fits r kvs) (FitsMsg r kvs)

type FitsMsg r kvs = 'Text "the record literal does not fit " ':<>: 'Text (Render r)

-- | Force the evidence of 'FitsRecord'.
fitsRecord :: forall r kvs. (FitsRecord r kvs) => ()
fitsRecord = holds @(Fits r kvs) @(FitsMsg r kvs)

type family Fits (r :: Ty) (kvs :: [(Symbol, Ty)]) :: Bool where
  Fits ('TRecord fs) kvs = And (GivenFit fs kvs) (RequiredGiven fs kvs)
  -- Against a union, the literal is checked against its one record or
  -- map member, when it has exactly one and that is a record (4.3).
  Fits ('TUnion ts) kvs = FitsOne (ObjectMembers ts) kvs
  Fits _ _ = 'False

type family FitsOne (ms :: [Ty]) (kvs :: [(Symbol, Ty)]) :: Bool where
  FitsOne '[ 'TRecord fs] kvs = Fits ('TRecord fs) kvs
  FitsOne _ _ = 'False

type family ObjectMembers (ts :: [Ty]) :: [Ty] where
  ObjectMembers '[] = '[]
  ObjectMembers ('TRecord fs ': ts) = 'TRecord fs ': ObjectMembers ts
  ObjectMembers ('TMap e ': ts) = 'TMap e ': ObjectMembers ts
  ObjectMembers (_ ': ts) = ObjectMembers ts

type family GivenFit (fs :: [Field]) (kvs :: [(Symbol, Ty)]) :: Bool where
  GivenFit _ '[] = 'True
  GivenFit fs ('(k, t) ': kvs) = And (FieldAccepts (FindField k fs) t) (GivenFit fs kvs)

type family FindField (k :: Symbol) (fs :: [Field]) :: Maybe Ty where
  FindField k ((k ':= u) ': _) = 'Just u
  FindField k ((k ':? u) ': _) = 'Just u
  FindField k (_ ': fs) = FindField k fs
  FindField _ '[] = 'Nothing

type family FieldAccepts (u :: Maybe Ty) (t :: Ty) :: Bool where
  FieldAccepts 'Nothing _ = 'False
  FieldAccepts ('Just u) t = Conf t u

type family RequiredGiven (fs :: [Field]) (kvs :: [(Symbol, Ty)]) :: Bool where
  RequiredGiven '[] _ = 'True
  RequiredGiven ((k ':= _) ': fs) kvs = And (HasKey k kvs) (RequiredGiven fs kvs)
  RequiredGiven ((_ ':? _) ': fs) kvs = RequiredGiven fs kvs

type family HasKey (k :: Symbol) (kvs :: [(Symbol, Ty)]) :: Bool where
  HasKey _ '[] = 'False
  HasKey k ('(k, _) ': _) = 'True
  HasKey k (_ ': kvs) = HasKey k kvs

-- | An @if@ without @else@ whose value is used has a data type or Void
-- for a block (spec 6.4).
type WhenTy t = Holds (Or (IsVoid t) (IsData t)) ('Text "an if without else has the type " ':<>: 'Text (Render t) ':<>: 'Text " | Null, which is not a type; add an else")

-- | Force the evidence of 'WhenTy'.
whenTy :: forall t. (WhenTy t) => ()
whenTy = holds @(Or (IsVoid t) (IsData t)) @('Text "an if without else has the type " ':<>: 'Text (Render t) ':<>: 'Text " | Null, which is not a type; add an else")

type family IsVoid (t :: Ty) :: Bool where
  IsVoid 'TVoid = 'True
  IsVoid _ = 'False

-- | The operands of @==@ (spec 6.2): both comparable, and one conforms
-- to the other.
type EqTypes a b =
  ( Comparable a,
    Comparable b,
    Holds (Or (Conf a b) (Conf b a)) ('Text "== compares " ':<>: 'Text (Render a) ':<>: 'Text " with " ':<>: 'Text (Render b) ':<>: 'Text ", and neither conforms to the other")
  )

-- | Force the evidence of 'EqTypes'.
eqTypes :: forall a b. (EqTypes a b) => ()
eqTypes =
  comparable @a `seq` comparable @b
    `seq` holds @(Or (Conf a b) (Conf b a)) @('Text "== compares " ':<>: 'Text (Render a) ':<>: 'Text " with " ':<>: 'Text (Render b) ':<>: 'Text ", and neither conforms to the other")

-- | A type that mentions no type parameter: what @cast@ may target
-- (spec 15.8).
type Concrete t = Holds (Not (HasVar t)) ('Text "cast cannot target " ':<>: 'Text (Render t) ':<>: 'Text ", which mentions a type parameter")

-- | Force the evidence of 'Concrete'.
concrete :: forall t. (Concrete t) => ()
concrete = holds @(Not (HasVar t)) @('Text "cast cannot target " ':<>: 'Text (Render t) ':<>: 'Text ", which mentions a type parameter")

type family HasVar (t :: Ty) :: Bool where
  HasVar ('TVar _ _) = 'True
  HasVar ('TArray e) = HasVar e
  HasVar ('TMap e) = HasVar e
  HasVar ('TAsync e) = HasVar e
  HasVar ('TRecord fs) = AnyFieldVar fs
  HasVar ('TUnion ts) = AnyVar ts
  HasVar ('TFun ps r) = Or (AnyVar ps) (HasVar r)
  HasVar _ = 'False

type family AnyVar (ts :: [Ty]) :: Bool where
  AnyVar '[] = 'False
  AnyVar (t ': ts) = Or (HasVar t) (AnyVar ts)

type family AnyFieldVar (fs :: [Field]) :: Bool where
  AnyFieldVar '[] = 'False
  AnyFieldVar (f ': fs) = Or (HasVar (FieldType f)) (AnyFieldVar fs)

-- | A record literal gives each key once (@E-TYPE-FIELD-DUPLICATE@).
type UniqueKeys kvs = Holds (NoDupKeys kvs) ('Text "the record literal gives a field twice")

-- | Force the evidence of 'UniqueKeys'.
uniqueKeys :: forall kvs. (UniqueKeys kvs) => ()
uniqueKeys = holds @(NoDupKeys kvs) @('Text "the record literal gives a field twice")

type family NoDupKeys (kvs :: [(Symbol, Ty)]) :: Bool where
  NoDupKeys '[] = 'True
  NoDupKeys ('(k, _) ': kvs) = And (Not (HasKey k kvs)) (NoDupKeys kvs)

-- | The record type @t@ has the field @n@ (@E-TYPE-ACCESS@).
type HasField n t = Holds (FieldExists n t) ('Text "the record has no field " ':<>: 'ShowType n ':<>: 'Text ": " ':<>: 'Text (Render t))

-- | Force the evidence of 'HasField'.
hasField :: forall n t. (HasField n t) => ()
hasField = holds @(FieldExists n t) @('Text "the record has no field " ':<>: 'ShowType n ':<>: 'Text ": " ':<>: 'Text (Render t))

type family FieldExists (n :: Symbol) (t :: Ty) :: Bool where
  FieldExists n ('TRecord fs) = IsJust (FindField n fs)
  FieldExists _ _ = 'False

type family IsJust (m :: Maybe k) :: Bool where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False

-- | The type of an @if@ without @else@ (spec 6.4): @T | Null@, or Void
-- for a Void block.
type family OrNull (t :: Ty) :: Ty where
  OrNull 'TVoid = 'TVoid
  OrNull t = t :| 'TNull

-- Conformance -------------------------------------------------------------------------

-- | The conformance relation of spec 4.4, as a closed family: the
-- equations are tried in order, which is what lets reflexivity and
-- union introduction coexist (as instances they overlap).
type family Conf (a :: Ty) (b :: Ty) :: Bool where
  Conf a a = 'True
  Conf _ 'TAny = 'True
  Conf ('TUnion as) b = AllConf as b
  -- As conformsUnder: a type parameter with a type bound conforms to a
  -- union its bound conforms to, when it is not itself a member.
  Conf a ('TUnion bs) = Or (AnyConf a bs) (ConfBound a ('TUnion bs))
  Conf ('TVar _ ('BType u)) b = Conf u b
  Conf 'TEnv 'TRunnable = 'True
  Conf _ _ = 'False

type family ConfBound (a :: Ty) (b :: Ty) :: Bool where
  ConfBound ('TVar _ ('BType u)) b = Conf u b
  ConfBound _ _ = 'False

type family AllConf (as :: [Ty]) (b :: Ty) :: Bool where
  AllConf '[] _ = 'True
  AllConf (a ': as) b = And (Conf a b) (AllConf as b)

type family AnyConf (a :: Ty) (bs :: [Ty]) :: Bool where
  AnyConf _ '[] = 'False
  AnyConf a (b ': bs) = Or (Conf a b) (AnyConf a bs)

-- Predicates ----------------------------------------------------------------------------

-- | A predicate that holds, or fails to with the given message. The
-- method is forced by the operations that require the predicate.
class Holds (p :: Bool) (msg :: ErrorMessage) where
  holds :: ()

instance Holds 'True msg where holds = ()

instance (Unsatisfiable msg) => Holds 'False msg where holds = unsatisfiable

-- | @a@ conforms to @b@ (spec 4.4).
type Conforms a b = Holds (Conf a b) (ConfMsg a b)

type ConfMsg a b = 'Text (Render a) ':<>: 'Text " does not conform to " ':<>: 'Text (Render b)

-- | Force the evidence that @a@ conforms to @b@.
conforms :: forall a b. (Conforms a b) => ()
conforms = holds @(Conf a b) @(ConfMsg a b)

-- | Data types (spec 4.2), as Language.Lask.Types.dataType: what a union
-- may hold and @cast@ may target.
type family IsData (t :: Ty) :: Bool where
  IsData 'TVoid = 'False
  IsData ('TFun _ _) = 'False
  IsData ('TAsync _) = 'False
  IsData ('TArray e) = IsData e
  IsData ('TMap e) = IsData e
  IsData ('TRecord fs) = And (SortedFields fs) (AllFields IsDataF fs)
  IsData ('TUnion ts) = And (CanonicalUnion ts) (AllTys IsDataT ts)
  IsData _ = 'True

-- | @t@ is a data type ('IsData').
type DataTy t = Holds (IsData t) ('Text (Render t) ':<>: 'Text " is not a data type")

-- | Force the evidence of 'DataTy'.
dataTy :: forall t. (DataTy t) => ()
dataTy = holds @(IsData t) @('Text (Render t) ':<>: 'Text " is not a data type")

-- | Well-formedness (spec 4.2), as Language.Lask.Types.wellFormed, at a
-- position where Void is not allowed: a parameter, an element, a field.
type family IsWF (t :: Ty) :: Bool where
  IsWF t = WFAt 'False t

type family WFAt (voidOk :: Bool) (t :: Ty) :: Bool where
  WFAt v 'TVoid = v
  WFAt _ ('TArray e) = WFAt 'False e
  WFAt _ ('TMap e) = WFAt 'False e
  WFAt _ ('TRecord fs) = And (SortedFields fs) (AllFields WFF fs)
  WFAt _ ('TAsync e) = WFAt 'True e
  WFAt _ ('TFun ps r) = And (AllTys WFT ps) (WFAt 'True r)
  WFAt _ ('TUnion ts) = And (CanonicalUnion ts) (And (AllTys IsDataT ts) (AllTys WFT ts))
  WFAt _ _ = 'True

-- | @t@ is well-formed where Void is not allowed ('IsWF').
type WF t = Holds (IsWF t) ('Text (Render t) ':<>: 'Text " is not well-formed here: Void is only a result")

-- | Force the evidence of 'WF'.
wf :: forall t. (WF t) => ()
wf = holds @(IsWF t) @('Text (Render t) ':<>: 'Text " is not well-formed here: Void is only a result")

-- | A union written with the raw constructor is a type only in canonical
-- form: at least two members, flattened, deduplicated, ordered, and
-- without Any. ':|' always builds it so.
type family CanonicalUnion (ts :: [Ty]) :: Bool where
  CanonicalUnion ts = Equal (Canon ts) ('TUnion ts)

-- | A record written with the raw constructor is a type only with its
-- fields sorted by name, each name once. 'Rec' always builds it so.
type family SortedFields (fs :: [Field]) :: Bool where
  SortedFields '[] = 'True
  SortedFields '[_] = 'True
  SortedFields (f ': g ': fs) = And (IsLT (CmpSymbol (FieldKey f) (FieldKey g))) (SortedFields (g ': fs))

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal _ _ = 'False

-- | A task's result: well-formed, Void allowed.
type WFResult t = Holds (WFAt 'True t) ('Text (Render t) ':<>: 'Text " is not a well-formed type")

-- | Force the evidence of 'WFResult'.
wfResult :: forall t. (WFResult t) => ()
wfResult = holds @(WFAt 'True t) @('Text (Render t) ':<>: 'Text " is not a well-formed type")

-- | Comparable types (spec 6.2), as Language.Lask.Types.comparableWith.
type family IsComparable (t :: Ty) :: Bool where
  IsComparable 'TNumber = 'True
  IsComparable 'TString = 'True
  IsComparable 'TBool = 'True
  IsComparable 'TNull = 'True
  IsComparable 'TEnv = 'True
  IsComparable 'TRunnable = 'True
  IsComparable ('TArray e) = IsComparable e
  IsComparable ('TMap e) = IsComparable e
  IsComparable ('TRecord fs) = AllFields IsComparableF fs
  IsComparable ('TUnion ts) = AllTys IsComparableT ts
  IsComparable ('TVar _ b) = Entails b 'NComparable
  IsComparable _ = 'False

-- | @t@ is comparable ('IsComparable').
type Comparable t = Holds (IsComparable t) ('Text (Render t) ':<>: 'Text " is not comparable")

-- | Force the evidence of 'Comparable'.
comparable :: forall t. (Comparable t) => ()
comparable = holds @(IsComparable t) @('Text (Render t) ':<>: 'Text " is not comparable")

-- | The types the @orderable@ bound admits (spec 4.2): Number and String.
type family IsOrderable (t :: Ty) :: Bool where
  IsOrderable 'TNumber = 'True
  IsOrderable 'TString = 'True
  IsOrderable ('TVar _ b) = Entails b 'NOrderable
  IsOrderable _ = 'False

-- | @t@ satisfies the @orderable@ bound ('IsOrderable').
type Orderable t = Holds (IsOrderable t) ('Text (Render t) ':<>: 'Text " is not orderable")

-- | Force the evidence of 'Orderable'.
orderable :: forall t. (Orderable t) => ()
orderable = holds @(IsOrderable t) @('Text (Render t) ':<>: 'Text " is not orderable")

-- | Stringifiable types (spec 6.6), as Language.Lask.Types.stringifiable.
type family IsStringifiable (t :: Ty) :: Bool where
  IsStringifiable 'TString = 'True
  IsStringifiable 'TNumber = 'True
  IsStringifiable 'TBool = 'True
  IsStringifiable 'TAny = 'True
  IsStringifiable ('TUnion ts) = AllTys IsStringifiableT ts
  IsStringifiable ('TVar _ b) = Entails b 'NStringifiable
  IsStringifiable _ = 'False

-- | @t@ can be interpolated ('IsStringifiable').
type Stringify t = Holds (IsStringifiable t) ('Text (Render t) ':<>: 'Text " cannot be interpolated: only String, Number, Bool and Any can")

-- | Force the evidence of 'Stringify'.
stringify :: forall t. (Stringify t) => ()
stringify = holds @(IsStringifiable t) @('Text (Render t) ':<>: 'Text " cannot be interpolated: only String, Number, Bool and Any can")

-- | The named bounds (spec 4.2).
data Named = NComparable | NOrderable | NStringifiable

-- | What a bound entails (spec 4.4): orderable entails all three; a type
-- bound entails what it satisfies, except Any, which entails nothing.
type family Entails (b :: Bound) (k :: Named) :: Bool where
  Entails 'BNone _ = 'False
  Entails 'BComparable 'NComparable = 'True
  Entails 'BOrderable _ = 'True
  Entails 'BStringifiable 'NStringifiable = 'True
  Entails ('BType 'TAny) _ = 'False
  Entails ('BType u) 'NComparable = IsComparable u
  Entails ('BType u) 'NOrderable = IsOrderable u
  Entails ('BType u) 'NStringifiable = IsStringifiable u
  Entails _ _ = 'False

-- Defunctionalised predicates, for the list walkers.
data PredT = IsDataT | WFT | IsComparableT | IsStringifiableT

data PredF = IsDataF | WFF | IsComparableF

type family ApplyT (p :: PredT) (t :: Ty) :: Bool where
  ApplyT 'IsDataT t = IsData t
  ApplyT 'WFT t = IsWF t
  ApplyT 'IsComparableT t = IsComparable t
  ApplyT 'IsStringifiableT t = IsStringifiable t

type family AllTys (p :: PredT) (ts :: [Ty]) :: Bool where
  AllTys _ '[] = 'True
  AllTys p (t ': ts) = And (ApplyT p t) (AllTys p ts)

type family FieldType (f :: Field) :: Ty where
  FieldType (_ ':= t) = t
  FieldType (_ ':? t) = t

type family ApplyF (p :: PredF) (t :: Ty) :: Bool where
  ApplyF 'IsDataF t = IsData t
  ApplyF 'WFF t = IsWF t
  ApplyF 'IsComparableF t = IsComparable t

type family AllFields (p :: PredF) (fs :: [Field]) :: Bool where
  AllFields _ '[] = 'True
  AllFields p (f ': fs) = And (ApplyF p (FieldType f)) (AllFields p fs)

-- Type parameters ------------------------------------------------------------------------

-- | Instantiate the type parameter @n@ at @t@ (spec 4.4).
type Subst n t u = SubstL '[ '(n, t)] u

-- | Instantiate @n@ at @t@ in parameter types.
type SubstParams n t ps = SubstParamsL '[ '(n, t)] ps

-- | Instantiate several type parameters at once. Substituting one after
-- another would get stuck: after the first, an unknown instantiation
-- might itself be the next parameter. Unions are rebuilt in canonical
-- form, since an instantiation can merge two members or bring Any in.
type family SubstL (s :: [(Symbol, Ty)]) (u :: Ty) :: Ty where
  SubstL s ('TVar n b) = LookupVar n s ('TVar n b)
  SubstL s ('TArray e) = 'TArray (SubstL s e)
  SubstL s ('TMap e) = 'TMap (SubstL s e)
  SubstL s ('TRecord fs) = 'TRecord (SubstFields s fs)
  SubstL s ('TAsync e) = 'TAsync (SubstL s e)
  SubstL s ('TFun ps r) = 'TFun (SubstAll s ps) (SubstL s r)
  SubstL s ('TUnion ts) = Canon (SubstAll s ts)
  SubstL _ u = u

type family LookupVar (n :: Symbol) (s :: [(Symbol, Ty)]) (d :: Ty) :: Ty where
  LookupVar _ '[] d = d
  LookupVar n ('(n, t) ': _) _ = t
  LookupVar n (_ ': s) d = LookupVar n s d

type family SubstAll (s :: [(Symbol, Ty)]) (us :: [Ty]) :: [Ty] where
  SubstAll _ '[] = '[]
  SubstAll s (u ': us) = SubstL s u ': SubstAll s us

type family SubstFields (s :: [(Symbol, Ty)]) (fs :: [Field]) :: [Field] where
  SubstFields _ '[] = '[]
  SubstFields s ((k ':= u) ': fs) = (k ':= SubstL s u) ': SubstFields s fs
  SubstFields s ((k ':? u) ': fs) = (k ':? SubstL s u) ': SubstFields s fs

-- | Instantiate several type parameters at once in parameter types.
type family SubstParamsL (s :: [(Symbol, Ty)]) (ps :: [Param]) :: [Param] where
  SubstParamsL _ '[] = '[]
  SubstParamsL s ((k '::: u) ': ps) = (k '::: SubstL s u) ': SubstParamsL s ps

-- | A generic's type parameters are declared once each, and every type
-- parameter its signature mentions is one of them, with the bound it
-- was declared with (spec 4.2).
type GenericVars vs ps r =
  Holds
    (And (Distinct vs) (AllDeclared (Append (ParamVars ps) (VarsOf r)) vs))
    ('Text "the signature's type parameters must be declared, once each, with the bounds they carry")

-- | Force the evidence of 'GenericVars'.
genericVars :: forall vs ps r. (GenericVars vs ps r) => ()
genericVars =
  holds
    @(And (Distinct vs) (AllDeclared (Append (ParamVars ps) (VarsOf r)) vs))
    @('Text "the signature's type parameters must be declared, once each, with the bounds they carry")

type family VarsOf (t :: Ty) :: [(Symbol, Bound)] where
  VarsOf ('TVar n b) = '[ '(n, b)]
  VarsOf ('TArray e) = VarsOf e
  VarsOf ('TMap e) = VarsOf e
  VarsOf ('TAsync e) = VarsOf e
  VarsOf ('TRecord fs) = FieldVars fs
  VarsOf ('TUnion ts) = TysVars ts
  VarsOf ('TFun ps r) = Append (TysVars ps) (VarsOf r)
  VarsOf _ = '[]

type family TysVars (ts :: [Ty]) :: [(Symbol, Bound)] where
  TysVars '[] = '[]
  TysVars (t ': ts) = Append (VarsOf t) (TysVars ts)

type family FieldVars (fs :: [Field]) :: [(Symbol, Bound)] where
  FieldVars '[] = '[]
  FieldVars (f ': fs) = Append (VarsOf (FieldType f)) (FieldVars fs)

type family ParamVars (ps :: [Param]) :: [(Symbol, Bound)] where
  ParamVars '[] = '[]
  ParamVars ((_ '::: t) ': ps) = Append (VarsOf t) (ParamVars ps)

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type family AllDeclared (xs :: [(Symbol, Bound)]) (vs :: [(Symbol, Bound)]) :: Bool where
  AllDeclared '[] _ = 'True
  AllDeclared (x ': xs) vs = And (ElemOf x vs) (AllDeclared xs vs)

type family ElemOf (x :: k) (xs :: [k]) :: Bool where
  ElemOf _ '[] = 'False
  ElemOf x (x ': _) = 'True
  ElemOf x (_ ': xs) = ElemOf x xs

type family Distinct (vs :: [(Symbol, Bound)]) :: Bool where
  Distinct '[] = 'True
  Distinct ('(n, _) ': vs) = And (Not (NameIn n vs)) (Distinct vs)

type family NameIn (n :: Symbol) (vs :: [(Symbol, Bound)]) :: Bool where
  NameIn _ '[] = 'False
  NameIn n ('(n, _) ': _) = 'True
  NameIn n (_ ': vs) = NameIn n vs

-- | An instantiation satisfies the bound of the parameter (spec 4.4,
-- @E-TYPE-BOUND@).
type family SatisfiesB (b :: Bound) (t :: Ty) :: Bool where
  SatisfiesB 'BNone _ = 'True
  SatisfiesB 'BComparable t = IsComparable t
  SatisfiesB 'BOrderable t = IsOrderable t
  SatisfiesB 'BStringifiable t = IsStringifiable t
  SatisfiesB ('BType u) t = Conf t u

-- | @t@ satisfies the bound @b@ of the type parameter @n@ (@E-TYPE-BOUND@).
type Satisfies n b t = Holds (SatisfiesB b t) (BoundMsg n b t)

type BoundMsg n b t = 'Text (Render t) ':<>: 'Text " does not satisfy the bound of " ':<>: 'Text n

-- | Force the evidence of 'Satisfies'.
satisfies :: forall n b t. (Satisfies n b t) => ()
satisfies = holds @(SatisfiesB b t) @(BoundMsg n b t)
