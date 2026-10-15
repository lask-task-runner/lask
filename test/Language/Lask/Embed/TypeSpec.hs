{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Lask.Embed.TypeSpec (spec) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Lask.Embed.Type
import qualified Language.Lask.Types as LT
import Test.Hspec

-- A sample of every form, records with a quoted and an optional key
-- included, to compare GHC's canonical forms with mkUnion/renderType.
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
     'TVar "T" 'BNone,
     'TArray ('TString :| 'TNull),
     'TNumber :| 'TNull
   ]

-- | Each type, unioned with each type of the sample.
class Pairs (xs :: [Ty]) (ys :: [Ty]) where
  pairs :: [(LT.Type, LT.Type, LT.Type)]

instance Pairs '[] ys where pairs = []

instance (Row x ys, Pairs xs ys) => Pairs (x ': xs) ys where
  pairs = row @x @ys <> pairs @xs @ys

class Row (x :: Ty) (ys :: [Ty]) where
  row :: [(LT.Type, LT.Type, LT.Type)]

instance Row x '[] where row = []

instance (KnownTy x, KnownTy y, KnownTy (x :| y), Row x ys) => Row x (y ': ys) where
  row = (tyOf @x, tyOf @y, tyOf @(x :| y)) : row @x @ys

-- | Each type of the sample with its type-level rendering.
class Renders (xs :: [Ty]) where
  renders :: [(LT.Type, String)]

instance Renders '[] where renders = []

instance (KnownTy x, KnownSymbol (Render x), Renders xs) => Renders (x ': xs) where
  renders = (tyOf @x, symbolVal (Proxy @(Render x))) : renders @xs

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

_varUnion :: ('TVar "T" 'BNone :| 'TNull) :~: ('TNull :| 'TVar "T" 'BNone)
_varUnion = Refl

_substUnion :: Subst "T" 'TString ('TVar "T" 'BNone :| 'TNull) :~: ('TString :| 'TNull)
_substUnion = Refl

_substAny :: Subst "T" 'TAny ('TVar "T" 'BNone :| 'TNull) :~: 'TAny
_substAny = Refl

spec :: Spec
spec = describe "Language.Lask.Embed.Type" $ do
  it "builds every union in the canonical form mkUnion builds" $
    sequence_
      [ (renderT a, renderT b, ghc) `shouldBe` (renderT a, renderT b, LT.mkUnion a [b])
        | (a, b, ghc) <- pairs @Sample @Sample
      ]

  it "renders every type as renderType does" $
    sequence_ [T.pack r `shouldBe` LT.renderType t | (t, r) <- renders @Sample]

  it "decides data types, comparability and well-formedness as Language.Lask.Types does" $ do
    holdsData @('TArray 'TVoid) `shouldBe` LT.dataType (LT.TyArray LT.TyVoid)
    holdsData @('TFun '[ 'TNumber] 'TNumber) `shouldBe` False
    holdsData @'TRunnable `shouldBe` LT.dataType LT.TyRunnable
    holdsData @('TVar "T" 'BNone) `shouldBe` True
    holdsWF @('TArray 'TVoid) `shouldBe` False
    holdsWF @('TFun '[ 'TNumber] 'TVoid) `shouldBe` True
    holdsComparable @('TMap 'TNumber) `shouldBe` LT.comparable (LT.TyMap LT.TyNumber)
    holdsComparable @'TAny `shouldBe` LT.comparable LT.TyAny
    holdsComparable @('TFun '[] 'TNumber) `shouldBe` False
  where
    renderT = T.unpack . LT.renderType

-- Reflected predicates, to compare with their counterparts in Types.
class KnownBool (b :: Bool) where boolVal :: Bool

instance KnownBool 'True where boolVal = True

instance KnownBool 'False where boolVal = False

holdsData :: forall t. (KnownBool (IsData t)) => Bool
holdsData = boolVal @(IsData t)

holdsWF :: forall t. (KnownBool (IsWF t)) => Bool
holdsWF = boolVal @(IsWF t)

holdsComparable :: forall t. (KnownBool (IsComparable t)) => Bool
holdsComparable = boolVal @(IsComparable t)
