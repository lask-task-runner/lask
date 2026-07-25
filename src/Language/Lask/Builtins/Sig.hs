{-# LANGUAGE OverloadedStrings #-}

-- | Type schemes of builtin symbols (spec 15, and the builtin
-- polymorphism rules of 4.4). Only builtins may carry type variables.
module Language.Lask.Builtins.Sig
  ( Scheme (..),
    builtinSchemes,
    schemeType,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.Types

-- | @Scheme vars params ret@: universally quantified over @vars@,
-- instantiated independently per call (4.4).
data Scheme = Scheme
  { schemeVars :: [Text],
    schemeParams :: [Type],
    schemeRet :: Type
  }
  deriving (Show, Eq)

-- | The (possibly polymorphic) function type of a scheme.
schemeType :: Scheme -> Type
schemeType (Scheme _ ps r) = TyFun ps r

mono :: [Type] -> Type -> Scheme
mono = Scheme []

tv :: Text -> Type
tv = TyVar

builtinSchemes :: Map Text Scheme
builtinSchemes =
  Map.fromList
    [ -- 15.2 numeric
      ("add", mono [TyNumber, TyNumber] TyNumber),
      ("sub", mono [TyNumber, TyNumber] TyNumber),
      ("mul", mono [TyNumber, TyNumber] TyNumber),
      ("div", mono [TyNumber, TyNumber] TyNumber),
      ("mod", mono [TyNumber, TyNumber] TyNumber),
      ("abs", mono [TyNumber] TyNumber),
      ("floor", mono [TyNumber] TyNumber),
      ("ceil", mono [TyNumber] TyNumber),
      ("round", mono [TyNumber] TyNumber),
      -- 15.3 string
      ("length", mono [TyString] TyNumber),
      ("concat", mono [TyString, TyString] TyString),
      ("trim", mono [TyString] TyString),
      ("toLower", mono [TyString] TyString),
      ("toUpper", mono [TyString] TyString),
      ("split", mono [TyString, TyString] (TyArray TyString)),
      ("join", mono [TyArray TyString, TyString] TyString),
      ("replace", mono [TyString, TyString, TyString] TyString),
      -- 15.4 array/map/record
      ("map", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] (TyArray (tv "U"))),
      ("filter", Scheme ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] (TyArray (tv "T"))),
      ("reduce", Scheme ["T", "U"] [TyArray (tv "T"), tv "U", TyFun [tv "U", tv "T"] (tv "U")] (tv "U")),
      ("forEach", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] TyVoid),
      ("append", Scheme ["T"] [TyArray (tv "T"), tv "T"] (TyArray (tv "T"))),
      ("concatArray", Scheme ["T"] [TyArray (tv "T"), TyArray (tv "T")] (TyArray (tv "T"))),
      ("get", Scheme ["T"] [TyMap (tv "T"), TyString] (tv "T")),
      ("hasKey", Scheme ["T"] [TyMap (tv "T"), TyString] TyBool),
      ("keys", Scheme ["T"] [TyMap (tv "T")] (TyArray TyString)),
      ("values", Scheme ["T"] [TyMap (tv "T")] (TyArray (tv "T"))),
      -- 15.5 command execution (keyword parameter --env is not part
      -- of the function type, spec 6.6)
      ("runCommand", mono [TyString] commandResultType),
      -- 15.6 parallel/async
      ("spawn", Scheme ["T"] [TyFun [] (tv "T")] (TyAsync (tv "T"))),
      ("all", Scheme ["T"] [TyArray (TyAsync (tv "T"))] (TyArray (tv "T"))),
      ("race", Scheme ["T"] [TyArray (TyAsync (tv "T"))] (tv "T")),
      -- 15.7 error handling
      ("recover", Scheme ["T"] [TyFun [] (tv "T"), TyFun [errorType] (tv "T")] (tv "T")),
      ("fail", Scheme ["T"] [errorType] (tv "T")),
      ("error", mono [TyNumber, TyString] errorType),
      -- 15.8 serialization / cast
      ("toJson", mono [TyAny] TyString),
      ("fromJson", mono [TyString] TyAny),
      ("encode", mono [TyAny, TyString] TyString),
      ("decode", mono [TyString, TyString] TyAny),
      ("cast", Scheme ["T"] [TyAny] (tv "T"))
      -- The reserved identifier stdin (9.3) is a String value, not a
      -- function; the elaborator resolves it specially.
    ]
