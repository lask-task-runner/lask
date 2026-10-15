{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The type-level rules of "Language.Lask.Embed.Type", compared with
-- their value-level counterparts in "Language.Lask.Types" over every
-- type, or pair of types, of a sample that has every form.
module Language.Lask.Embed.TypeSpec (spec) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Lask.Embed.Type
import qualified Language.Lask.Types as LT
import Test.Hspec

-- Type parameters with each kind of bound.
type VT = 'TVar "T" 'BNone

type VU = 'TVar "U" ('BType ('TNumber :| 'TString))

type VO = 'TVar "O" 'BOrderable

-- | Their bounds, as Language.Lask.Types sees them.
boundsOf :: Text -> Maybe LT.Bound
boundsOf v = case v of
  "U" -> Just (LT.BoundType (LT.mkUnion LT.TyNumber [LT.TyString]))
  "O" -> Just (LT.BoundNamed LT.BOrderable)
  _ -> Nothing

upper :: Text -> Maybe LT.Type
upper v = case boundsOf v of
  Just (LT.BoundType u) -> Just u
  _ -> Nothing

-- Every form, records with a quoted and an optional key, unions with
-- type parameters, and type parameters with each kind of bound.
type Sample =
  '[ 'TAny,
     'TNumber,
     'TString,
     'TBool,
     'TNull,
     'TEnv,
     'TRunnable,
     'TArray 'TString,
     'TArray 'TNumber,
     'TMap 'TNumber,
     Rec '["name" := 'TString, "X-Api-Key" :? 'TString],
     Rec '["a" := 'TNumber],
     VT,
     VU,
     VO,
     'TArray ('TString :| 'TNull),
     'TNumber :| 'TNull,
     'TNumber :| 'TString,
     'TNumber :| 'TString :| 'TNull,
     VT :| 'TNull
   ]

-- | One row of comparisons: our answer and Types' for one case.
type Check = (String, Bool, Bool)

class KnownBool (b :: Bool) where boolVal :: Bool

instance KnownBool 'True where boolVal = True

instance KnownBool 'False where boolVal = False

render :: LT.Type -> String
render = T.unpack . LT.renderType

-- Unions of each pair, as values: GHC's canonical form, reflected as it
-- is, and mkUnion's.
class Unions (xs :: [Ty]) (ys :: [Ty]) where
  unions :: [(String, LT.Type, LT.Type)]

instance Unions '[] ys where unions = []

instance (UnionRow x ys, Unions xs ys) => Unions (x ': xs) ys where
  unions = unionRow @x @ys <> unions @xs @ys

class UnionRow (x :: Ty) (ys :: [Ty]) where
  unionRow :: [(String, LT.Type, LT.Type)]

instance UnionRow x '[] where unionRow = []

instance (KnownTy x, KnownTy y, KnownTy (x :| y), UnionRow x ys) => UnionRow x (y ': ys) where
  unionRow = (render (tyOf @x) <> " | " <> render (tyOf @y), tyOf @(x :| y), LT.mkUnion (tyOf @x) [tyOf @y]) : unionRow @x @ys

-- Conformance of each pair.
class Confs (xs :: [Ty]) (ys :: [Ty]) where
  confs :: [Check]

instance Confs '[] ys where confs = []

instance (ConfRow x ys, Confs xs ys) => Confs (x ': xs) ys where
  confs = confRow @x @ys <> confs @xs @ys

class ConfRow (x :: Ty) (ys :: [Ty]) where
  confRow :: [Check]

instance ConfRow x '[] where confRow = []

instance (KnownTy x, KnownTy y, KnownBool (Conf x y), ConfRow x ys) => ConfRow x (y ': ys) where
  confRow =
    (render (tyOf @x) <> " conforms to " <> render (tyOf @y), boolVal @(Conf x y), LT.conformsUnder upper (tyOf @x) (tyOf @y))
      : confRow @x @ys

-- The predicates on each type.
class Predicates (xs :: [Ty]) where
  predicates :: [Check]

instance Predicates '[] where predicates = []

instance
  ( KnownTy x,
    KnownSymbol (Render x),
    KnownBool (IsData x),
    KnownBool (IsComparable x),
    KnownBool (IsOrderable x),
    KnownBool (IsStringifiable x),
    Predicates xs
  ) =>
  Predicates (x ': xs)
  where
  predicates =
    [ (t <> " renders", symbolVal (Proxy @(Render x)) == render (tyOf @x), True),
      (t <> " is data", boolVal @(IsData x), LT.dataType (tyOf @x)),
      (t <> " is comparable", boolVal @(IsComparable x), LT.satisfiesNamed boundsOf LT.BComparable (tyOf @x)),
      (t <> " is orderable", boolVal @(IsOrderable x), LT.satisfiesNamed boundsOf LT.BOrderable (tyOf @x)),
      (t <> " is stringifiable", boolVal @(IsStringifiable x), LT.satisfiesNamed boundsOf LT.BStringifiable (tyOf @x))
    ]
      <> predicates @xs
    where
      t = render (tyOf @x)

-- Each bound against each type.
class Bounds (bs :: [Bound]) (xs :: [Ty]) where
  bounds :: [Check]

instance Bounds '[] xs where bounds = []

instance (BoundRow b xs, Bounds bs xs) => Bounds (b ': bs) xs where
  bounds = boundRow @b @xs <> bounds @bs @xs

class BoundRow (b :: Bound) (xs :: [Ty]) where
  boundRow :: [Check]

instance BoundRow b '[] where boundRow = []

instance (KnownBound b, KnownTy x, KnownBool (SatisfiesB' b x), BoundRow b xs) => BoundRow b (x ': xs) where
  boundRow =
    ( render (tyOf @x) <> " satisfies " <> show (boundOf @b),
      boolVal @(SatisfiesB' b x),
      maybe True (\bd -> LT.satisfiesBound boundsOf bd (tyOf @x)) (boundOf @b)
    )
      : boundRow @b @xs

-- | Whether the bound holds, through the constraint the EDSL uses.
type family SatisfiesB' (b :: Bound) (t :: Ty) :: Bool where
  SatisfiesB' 'BNone _ = 'True
  SatisfiesB' 'BComparable t = IsComparable t
  SatisfiesB' 'BOrderable t = IsOrderable t
  SatisfiesB' 'BStringifiable t = IsStringifiable t
  SatisfiesB' ('BType u) t = Conf t u

disagreements :: [Check] -> [String]
disagreements cs = [what <> ": EDSL " <> show ours <> ", Types " <> show theirs | (what, ours, theirs) <- cs, ours /= theirs]

-- Spellings of one union are one type.
_nullLast :: ('TString :| 'TNull) :~: ('TNull :| 'TString)
_nullLast = Refl

_flattened :: ('TNumber :| 'TString :| 'TNull) :~: 'TUnion '[ 'TNumber, 'TString, 'TNull]
_flattened = Refl

_absorbed :: ('TAny :| 'TNull) :~: 'TAny
_absorbed = Refl

_fieldOrder :: Rec '["b" := 'TNumber, "a" := 'TString] :~: Rec '["a" := 'TString, "b" := 'TNumber]
_fieldOrder = Refl

_optionalRead :: FieldOf "tags" (Rec '["tags" :? 'TArray 'TString]) :~: ('TArray 'TString :| 'TNull)
_optionalRead = Refl

_substUnion :: Subst "T" 'TString (VT :| 'TNull) :~: ('TString :| 'TNull)
_substUnion = Refl

_substAny :: Subst "T" 'TAny (VT :| 'TNull) :~: 'TAny
_substAny = Refl

spec :: Spec
spec = describe "Language.Lask.Embed.Type" $ do
  it "orders every union as mkUnion does" $
    [what | (what, ghc, value) <- unions @Sample @Sample, ghc /= value] `shouldBe` []

  it "decides conformance as conformsUnder does, type parameters' bounds included" $
    disagreements (confs @Sample @Sample) `shouldBe` []

  it "renders, and decides data, comparable, orderable and stringifiable, as Types does" $
    disagreements (predicates @Sample) `shouldBe` []

  it "decides every bound as satisfiesBound does" $
    disagreements (bounds @'[ 'BComparable, 'BOrderable, 'BStringifiable, 'BType ('TNumber :| 'TString), 'BType 'TAny] @Sample)
      `shouldBe` []

  it "does not take a union or record in a form other than canonical as a type" $ do
    boolVal @(IsData ('TUnion '[])) `shouldBe` False
    boolVal @(IsData ('TUnion '[ 'TNumber])) `shouldBe` False
    boolVal @(IsData ('TUnion '[ 'TNull, 'TNumber])) `shouldBe` False
    boolVal @(IsData ('TUnion '[ 'TAny, 'TNumber])) `shouldBe` False
    boolVal @(IsData ('TRecord '["b" := 'TNumber, "a" := 'TNumber])) `shouldBe` False
    boolVal @(IsData ('TRecord '["a" := 'TNumber, "a" := 'TString])) `shouldBe` False
    boolVal @(IsData ('TNumber :| 'TNull)) `shouldBe` True
