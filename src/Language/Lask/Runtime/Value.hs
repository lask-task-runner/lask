{-# LANGUAGE OverloadedStrings #-}

-- | Runtime values (spec 8.2) and the failure exception (8.10).
module Language.Lask.Runtime.Value
  ( Value (..),
    EnvValue (..),
    Closure (..),
    AsyncHandle (..),
    Scope,
    LaskFailure (..),
    pushFrame,
    valueEq,
    stringify,
    formatNumber,
    errorValue,
    runtimeFailure,
    ioFailure,
    userFailure,
    typeNameOf,
  )
where

import Control.Concurrent.Async (Async)
import Control.Exception (Exception)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, isInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Lask.Core.AST (Lam)
import Language.Lask.ErrorCode (ErrorCode)

data Value
  = VNumber !Scientific
  | VString !Text
  | VBool !Bool
  | VNull
  | VVoid
  | VArray !(Vector Value)
  | VMap !(Map Text Value)
  | VRecord !(Map Text Value)
  | VClosure !Closure
  | -- | Builtin function value, applied by name through the
    -- dispatcher.
    VBuiltin !Text
  | VAsync !AsyncHandle
  | VEnv !EnvValue
  deriving (Show)

-- | Structural equality via 'valueEq'; functions and async handles
-- compare unequal (they are never comparable in the language).
instance Eq Value where
  (==) = valueEq

data Closure = Closure
  { cloLam :: Lam,
    cloScope :: Scope
  }
  deriving (Show)

newtype AsyncHandle = AsyncHandle (Async Value)

instance Show AsyncHandle where
  show _ = "<async>"

-- | Environment value: kind and normalized constructor parameters
-- (spec 8.8).
data EnvValue = EnvValue
  { envKind :: Text,
    envParams :: Map Text Value
  }
  deriving (Show)

type Scope = Map Text Value

-- | Structural equality (spec 6.2, 8.8). Static checking guarantees
-- comparable operands; incomparable values compare unequal
-- defensively.
valueEq :: Value -> Value -> Bool
valueEq a b = case (a, b) of
  (VNumber x, VNumber y) -> x == y
  (VString x, VString y) -> x == y
  (VBool x, VBool y) -> x == y
  (VNull, VNull) -> True
  (VArray xs, VArray ys) ->
    V.length xs == V.length ys && V.and (V.zipWith valueEq xs ys)
  (VMap xs, VMap ys) -> mapEq xs ys
  (VRecord xs, VRecord ys) -> mapEq xs ys
  (VEnv (EnvValue k1 p1), VEnv (EnvValue k2 p2)) -> k1 == k2 && mapEq p1 p2
  _ -> False
  where
    mapEq xs ys =
      Map.keysSet xs == Map.keysSet ys
        && and (Map.elems (Map.intersectionWith valueEq xs ys))

-- | Canonical number formatting: integers without a fraction part.
formatNumber :: Scientific -> Text
formatNumber n
  | isInteger n = T.pack (formatScientific Fixed (Just 0) n)
  | otherwise = T.pack (formatScientific Fixed Nothing n)

-- | Text used by string\/command interpolation (spec 6.6).
stringify :: Value -> Text
stringify v = case v of
  VString t -> t
  VNumber n -> formatNumber n
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  other -> T.pack (show other) -- non-stringifiable values are rejected statically

typeNameOf :: Value -> Text
typeNameOf v = case v of
  VNumber _ -> "Number"
  VString _ -> "String"
  VBool _ -> "Bool"
  VNull -> "Null"
  VVoid -> "Void"
  VArray _ -> "Array"
  VMap _ -> "Map"
  VRecord _ -> "Record"
  VClosure _ -> "Function"
  VBuiltin _ -> "Function"
  VAsync _ -> "AsyncHandle"
  VEnv _ -> "Environment"

-- Failures ---------------------------------------------------------------

-- | A Lask failure in flight: the user-visible @Error@ record value
-- (spec 8.10) plus the machine-readable code for diagnostics and
-- events (14 章), and the function-call stack collected while the
-- failure propagates (12.3). @fail(err)@ keeps the user's error value
-- and has no specific code.
data LaskFailure = LaskFailure
  { lfCode :: Maybe ErrorCode,
    lfError :: Value,
    -- | Innermost first: @name (module)@ labels.
    lfFrames :: [Text]
  }
  deriving (Show, Eq)

instance Exception LaskFailure

-- | Record a propagation frame on a failure (spec 12.3).
pushFrame :: Text -> LaskFailure -> LaskFailure
pushFrame frame lf = lf {lfFrames = lfFrames lf <> [frame]}

-- | Construct an @Error@ record value.
errorValue :: Scientific -> Text -> Value
errorValue code msg =
  VRecord (Map.fromList [("code", VNumber code), ("message", VString msg)])

-- | Runtime error: default code 2 (spec 8.10).
runtimeFailure :: ErrorCode -> Text -> LaskFailure
runtimeFailure code msg = LaskFailure (Just code) (errorValue 2 msg) []

-- | External I\/O error: default code 3 (spec 8.10).
ioFailure :: ErrorCode -> Text -> LaskFailure
ioFailure code msg = LaskFailure (Just code) (errorValue 3 msg) []

-- | @fail(err)@: the error value is used as-is.
userFailure :: Value -> LaskFailure
userFailure v = LaskFailure Nothing v []
