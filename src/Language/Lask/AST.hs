{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Lask.AST
  ( Module,
    Module' (..),
    Statement,
    Statement' (..),
    isExpr,
    Type,
    Type' (..),
    PackedArgument,
    Expr,
    Expr' (..),
    Parameter,
    Parameter' (..),
    isPositionedParameter,
    isKeywordParameter,
    IsRest,
    IsOptional,
    Argument,
    Argument' (..),
    isPositionedArgument,
    isKeywordArgument,
    IsExpanded,
  )
where

import Control.Comonad.Cofree
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (bimap)
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Utils (Pretty (pretty), SwitchCofree (..), showWithBrackets)
import System.Exit (ExitCode)

type Module ann = Cofree (Module' ann) ann

newtype Module' ann a = Module [Statement ann] deriving (Show, Eq)

instance (Eq ann) => Eq1 (Module' ann) where
  liftEq _ (Module s1) (Module s2) = s1 == s2

instance (Show ann) => Show1 (Module' ann) where
  liftShowsPrec _ _ _ (Module s) = showString $ "Module " <> show s

type Statement ann = Cofree (Statement' ann) ann

data Statement' ann a
  = ExprStatement String (Expr ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Statement' ann) where
  liftEq _ (ExprStatement i1 d1) (ExprStatement i2 d2) = i1 == i2 && d1 == d2

instance (Show ann) => Show1 (Statement' ann) where
  liftShowsPrec _ _ _ (ExprStatement i d) =
    showString $ unwords ["ExprStatement", show i, showWithBrackets (show d)]

isExpr :: Statement a -> Bool
isExpr (_ :< ExprStatement {}) = True

type Type ann = Cofree (Type' ann) ann

newtype Type' ann self
  = -- | Type variable. ex. String
    TypeVar String
  deriving (Show, Eq)

instance Eq1 (Type' ann) where
  liftEq _ (TypeVar s1) (TypeVar s2) = s1 == s2

instance (Show ann) => Show1 (Type' ann) where
  liftShowsPrec _ _ _ (TypeVar s) = showString $ unwords ["TypeVar", show s]

instance Pretty (Type a) where
  pretty (_ :< TypeVar s) = s

instance SwitchCofree Type' where
  switchCofree f (a :< TypeVar s) = f a :< TypeVar s

type PackedArgument a = (String, (Maybe (Expr a), Maybe (Type a)))

type Expr ann = Cofree (Expr' ann) ann

data Expr' ann self
  = Null
  | Bool Bool
  | Number Scientific
  | String String
  | Array [self]
  | Object [(self, self)]
  | Image String
  | Var String
  | Accessor self self
  | Call self [Argument ann]
  | Lambda [Parameter ann] self (Maybe (Type ann))
  | Error String ExitCode
  | FixtureFun [Parameter ann] ([PackedArgument ann] -> IO self) (Maybe (Type ann))
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Expr' ann) where
  liftEq _ Null Null = True
  liftEq _ (Bool a) (Bool b) = a == b
  liftEq _ (Number a) (Number b) = a == b
  liftEq _ (String a) (String b) = a == b
  liftEq f (Array a) (Array b) = length a == length b && all (uncurry f) (zip a b)
  liftEq f (Object a) (Object b) = length a == length b && all (\((ka, va), (kb, vb)) -> f ka kb && f va vb) (zip a b)
  liftEq _ (Image a) (Image b) = a == b
  liftEq _ (Var a) (Var b) = a == b
  liftEq f (Accessor a1 i1) (Accessor a2 i2) = f a1 a2 && f i1 i2
  liftEq f (Call f1 a1) (Call f2 a2) = f f1 f2 && length a1 == length a2 && all (uncurry (==)) (zip a1 a2)
  liftEq f (Lambda p1 e1 t1) (Lambda p2 e2 t2) = p1 == p2 && f e1 e2 && t1 == t2
  liftEq _ (Error s1 c1) (Error s2 c2) = s1 == s2 && c1 == c2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Expr' ann) where
  liftShowsPrec _ _ _ Null = showString "Null"
  liftShowsPrec _ _ _ (Bool v) = showString $ unwords ["Bool", show v]
  liftShowsPrec _ _ _ (Number v) = showString $ unwords ["Number", show v]
  liftShowsPrec _ _ _ (String v) = showString $ unwords ["String", show v]
  liftShowsPrec _ f _ (Array vs) = showString "Array " <> f vs
  liftShowsPrec f _ n (Object vs) =
    showString "Object ["
      <> foldl1
        (\a b -> a <> showString "," <> b)
        ( map
            ( \(i, a) ->
                showString "("
                  <> f n i
                  <> showString ","
                  <> f n a
                  <> showString ")"
            )
            vs
        )
      <> showString "]"
  liftShowsPrec _ _ _ (Image i) = showString $ unwords ["Image", show i]
  liftShowsPrec _ _ _ (Var i) = showString $ unwords ["Var", show i]
  liftShowsPrec f _ n (Accessor a i) =
    showString "Accessor ("
      <> f n a
      <> showString ") ("
      <> f n i
      <> showString ")"
  liftShowsPrec f _ n (Call a as) =
    showString "Call ("
      <> f n a
      <> showString ") "
      <> showString (show as)
  liftShowsPrec f _ n (Lambda pma e t) =
    showString "Lambda "
      <> showString (show pma)
      <> showString " ("
      <> f n e
      <> showString ") "
      <> showString (show t)
  liftShowsPrec _ _ _ (Error s c) =
    showString $ unwords ["Error", show s, showWithBrackets (show c)]
  liftShowsPrec _ _ _ (FixtureFun {}) = showString "FixtureFun"

instance SwitchCofree Expr' where
  switchCofree f (a :< Null) = f a :< Null
  switchCofree f (a :< Bool b) = f a :< Bool b
  switchCofree f (a :< Number n) = f a :< Number n
  switchCofree f (a :< String s) = f a :< String s
  switchCofree f (a :< Array as) = f a :< Array (map (switchCofree f) as)
  switchCofree f (a :< Object os) =
    f a :< Object (map (bimap (switchCofree f) (switchCofree f)) os)
  switchCofree f (a :< Image i) = f a :< Image i
  switchCofree f (a :< Var v) = f a :< Var v
  switchCofree f (a :< Accessor a1 i1) =
    f a :< Accessor (switchCofree f a1) (switchCofree f i1)
  switchCofree f (a :< Call func as) =
    f a :< Call (switchCofree f func) (map (switchCofree f) as)
  switchCofree f (a :< Lambda ps e t) =
    f a :< Lambda (map (switchCofree f) ps) (switchCofree f e) (switchCofree f <$> t)
  switchCofree f (a :< Error s c) = f a :< Error s c
  switchCofree _ (_ :< FixtureFun {}) = undefined

instance ToJSON (Expr a) where
  toJSON expr = case expr of
    (_ :< Null) -> Aeson.Null
    (_ :< Bool v) -> Aeson.Bool v
    (_ :< Number v) -> Aeson.Number v
    (_ :< String v) -> Aeson.String (T.pack v)
    (_ :< Array vs) -> Aeson.Array $ V.fromList $ map toJSON vs
    (_ :< Object vs) ->
      Aeson.Object $
        KM.fromList $
          map
            ( \case
                (_ :< String i, v) -> (K.fromText (T.pack i), toJSON v)
                (_, v) -> (K.fromText (T.pack "not value"), toJSON v)
            )
            vs
    (_ :< _) -> Aeson.String (T.pack "not value")

instance FromJSON (Expr ()) where
  parseJSON e = case e of
    Aeson.Null -> pure $ () :< Null
    Aeson.Bool v -> pure $ () :< Bool v
    Aeson.Number v -> pure $ () :< Number v
    Aeson.String v -> pure $ () :< String (T.unpack v)
    Aeson.Array vs -> (() :<) . Array <$> mapM parseJSON (V.toList vs)
    Aeson.Object vs ->
      (() :<) . Object
        <$> mapM
          (\(k, v) -> (,) (() :< String (K.toString k)) <$> parseJSON v)
          (KM.toList vs)

instance Show ([PackedArgument ann] -> IO a) where
  show _ = undefined

instance Eq ([PackedArgument ann] -> IO a) where
  _ == _ = undefined

type Parameter ann = Cofree (Parameter' ann) ann

data Parameter' ann self
  = PositionedParameter
      -- | Parameter name
      String
      -- | Rest Parameter
      IsRest
      -- | Optional Parameter
      IsOptional
      -- | Parameter type
      (Maybe (Type ann))
      -- | Default value
      (Maybe (Expr ann))
  | KeywordParameter
      String
      -- | Rest Parameter
      IsRest
      -- | Optional Parameter
      IsOptional
      -- | Parameter type
      (Maybe (Type ann))
      -- | Default value
      (Maybe (Expr ann))
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Parameter' ann) where
  liftEq
    _
    (PositionedParameter p1 r1 o1 t1 e1)
    (PositionedParameter p2 r2 o2 t2 e2) =
      p1 == p2
        && r1 == r2
        && o1 == o2
        && t1 == t2
        && e1 == e2
  liftEq
    _
    (KeywordParameter i1 r1 o1 t1 e1)
    (KeywordParameter i2 r2 o2 t2 e2) =
      i1 == i2
        && r1 == r2
        && o1 == o2
        && t1 == t2
        && e1 == e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Parameter' ann) where
  liftShowsPrec _ _ _ (PositionedParameter p r o t e) =
    showString $
      unwords
        [ "PositionedParameter",
          show p,
          show r,
          show o,
          showWithBrackets (show t),
          showWithBrackets (show e)
        ]
  liftShowsPrec _ _ _ (KeywordParameter i r o t e) =
    showString $
      unwords
        [ "KeywordParameter",
          show i,
          show r,
          show o,
          showWithBrackets (show t),
          showWithBrackets (show e)
        ]

instance SwitchCofree Parameter' where
  switchCofree f (a :< PositionedParameter p r o t e) =
    f a :< PositionedParameter p r o (switchCofree f <$> t) (switchCofree f <$> e)
  switchCofree f (a :< KeywordParameter i r o t e) =
    f a :< KeywordParameter i r o (switchCofree f <$> t) (switchCofree f <$> e)

type IsRest = Bool

type IsOptional = Bool

isPositionedParameter :: Parameter ann -> Bool
isPositionedParameter (_ :< PositionedParameter {}) = True
isPositionedParameter _ = False

isKeywordParameter :: Parameter ann -> Bool
isKeywordParameter (_ :< KeywordParameter {}) = True
isKeywordParameter _ = False

type Argument ann = Cofree (Argument' ann) ann

data Argument' ann self
  = PositionedArgument IsExpanded (Expr ann)
  | KeywordArgument String (Expr ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Argument' ann) where
  liftEq _ (PositionedArgument e1 a1) (PositionedArgument e2 a2) = e1 == e2 && a1 == a2
  liftEq _ (KeywordArgument i1 a1) (KeywordArgument i2 a2) = i1 == i2 && a1 == a2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Argument' ann) where
  liftShowsPrec _ _ _ (PositionedArgument e a) =
    showString $
      unwords
        [ "PositionedArgument",
          show e,
          showWithBrackets (show a)
        ]
  liftShowsPrec _ _ _ (KeywordArgument name a) =
    showString $
      unwords
        [ "KeywordArgument",
          show name,
          showWithBrackets (show a)
        ]

instance SwitchCofree Argument' where
  switchCofree f (a :< PositionedArgument e1 a1) = f a :< PositionedArgument e1 (switchCofree f a1)
  switchCofree f (a :< KeywordArgument i1 a1) = f a :< KeywordArgument i1 (switchCofree f a1)

type IsExpanded = Bool

isPositionedArgument :: Argument ann -> Bool
isPositionedArgument (_ :< PositionedArgument {}) = True
isPositionedArgument _ = False

isKeywordArgument :: Argument ann -> Bool
isKeywordArgument (_ :< KeywordArgument {}) = True
isKeywordArgument _ = False
