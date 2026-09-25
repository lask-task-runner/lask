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
      "for_each",
      "run_command",
      "recover",
      "fail",
      "get_env",
      "mark_secret"
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
      "min",
      "max",
      "sum",
      "pow",
      "sqrt",
      "clamp",
      -- 15.3 string
      "length",
      "concat",
      "trim",
      "to_lower",
      "to_upper",
      "split",
      "join",
      "replace",
      "contains",
      "starts_with",
      "ends_with",
      "index_of",
      "substring",
      "pad_start",
      "pad_end",
      "repeat",
      "lines",
      "to_string",
      "to_number",
      "regex_test",
      "regex_match",
      "regex_replace",
      -- 15.4 array/map/record
      "map",
      "filter",
      "reduce",
      "for_each",
      "append",
      "concat_array",
      "get",
      "has_key",
      "keys",
      "values",
      "size",
      "is_empty",
      "first",
      "last",
      "slice",
      "take",
      "drop",
      "reverse",
      "sort",
      "sort_by",
      "contains_array",
      "index_of_array",
      "find",
      "find_index",
      "every",
      "any",
      "flatten",
      "flat_map",
      "zip",
      "unique",
      "range",
      "enumerate",
      "set",
      "remove",
      "merge",
      "get_or",
      "entries",
      "from_entries",
      "map_values",
      -- 15.5 command execution
      "run_command",
      "shell_quote",
      -- 15.6 parallel/async
      "spawn",
      "all",
      "race",
      -- 15.7 error handling
      "recover",
      "fail",
      "error",
      -- 15.8 serialization / cast
      "to_json",
      "from_json",
      "encode",
      "decode",
      "cast",
      "base64_encode",
      "base64_decode",
      "sha256",
      "md5",
      -- 15.9 environment access / secret marking
      "get_env",
      "find_env",
      "has_env",
      "get_env_or",
      "mark_secret",
      -- 15.10 path operations
      "path_join",
      "dirname",
      "basename",
      "extname",
      "normalize_path",
      "is_absolute_path",
      -- 15.11 filesystem
      "read_file",
      "write_file",
      "file_exists",
      "remove_file",
      "make_dir",
      "list_dir",
      "glob",
      -- 15.12 diagnostic output
      "log",
      -- 15.13 nondeterministic
      "uuid",
      "random_string"
    ]

-- | Builtin type aliases that user code must not redefine
-- (spec 6.6, 6.9): @CommandResult@ and @Error@.
builtinTypeAliasNames :: Set Text
builtinTypeAliasNames = Set.fromList ["CommandResult", "Error"]
