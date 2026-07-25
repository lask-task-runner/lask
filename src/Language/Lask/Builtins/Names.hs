{-# LANGUAGE OverloadedStrings #-}

-- | Name sets of core functions and builtin symbols (spec 7.2, 15).
-- Full type signatures live in "Language.Lask.Builtins.Sig".
module Language.Lask.Builtins.Names
  ( coreFunctionNames,
    reservedIdentifiers,
    builtinValueNames,
    builtinTypeAliasNames,
    isUnbindableName,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Core functions that user code must never declare or shadow
-- (spec 7.2): binding one of these at any rank 1-4 position is
-- @E-NAME-DUPLICATE@. @choose@ is additionally never referenceable.
coreFunctionNames :: Set Text
coreFunctionNames =
  Set.fromList
    [ "spawn",
      "choose",
      "map",
      "filter",
      "reduce",
      "forEach",
      "runCommand",
      "recover",
      "fail"
    ]

-- | Reserved identifiers that cannot be bound (spec 3.3, 9.3).
reservedIdentifiers :: Set Text
reservedIdentifiers = Set.singleton "stdin"

-- | True if user code must not bind this name anywhere.
isUnbindableName :: Text -> Bool
isUnbindableName n =
  n `Set.member` coreFunctionNames || n `Set.member` reservedIdentifiers

-- | All builtin value symbols available without import (spec 15).
-- These resolve at the lowest rank and are shadowable, except the
-- core functions above. @await@ is a reserved word, not an
-- identifier, so it is not listed. @choose@ is not exposed.
builtinValueNames :: Set Text
builtinValueNames =
  Set.fromList
    [ -- 15.2 numeric
      "add",
      "sub",
      "mul",
      "div",
      "mod",
      "abs",
      "floor",
      "ceil",
      "round",
      -- 15.3 string
      "length",
      "concat",
      "trim",
      "toLower",
      "toUpper",
      "split",
      "join",
      "replace",
      -- 15.4 array/map/record
      "map",
      "filter",
      "reduce",
      "forEach",
      "append",
      "concatArray",
      "get",
      "hasKey",
      "keys",
      "values",
      -- 15.5 command execution
      "runCommand",
      -- 15.6 parallel/async
      "spawn",
      "all",
      "race",
      -- 15.7 error handling
      "recover",
      "fail",
      "error",
      -- 15.8 serialization / cast
      "toJson",
      "fromJson",
      "encode",
      "decode",
      "cast"
    ]

-- | Builtin type aliases that user code must not redefine
-- (spec 6.6, 6.9): @CommandResult@ and @Error@.
builtinTypeAliasNames :: Set Text
builtinTypeAliasNames = Set.fromList ["CommandResult", "Error"]
