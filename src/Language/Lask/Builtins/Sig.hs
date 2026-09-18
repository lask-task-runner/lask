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

-- | @Record<first: T, second: U>@, the result element of @zip@.
pairType :: Type -> Type -> Type
pairType a b = TyRecord (Map.fromList [("first", a), ("second", b)])

-- | @Record<index: Number, value: T>@, the result element of
-- @enumerate@.
indexedType :: Type -> Type
indexedType t = TyRecord (Map.fromList [("index", TyNumber), ("value", t)])

-- | @Record<key: String, value: T>@, the element @entries@ produces
-- and @from_entries@ consumes.
entryType :: Type -> Type
entryType t = TyRecord (Map.fromList [("key", TyString), ("value", t)])

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
      ("min", mono [TyNumber, TyNumber] TyNumber),
      ("max", mono [TyNumber, TyNumber] TyNumber),
      ("sum", mono [TyArray TyNumber] TyNumber),
      ("pow", mono [TyNumber, TyNumber] TyNumber),
      ("sqrt", mono [TyNumber] TyNumber),
      ("clamp", mono [TyNumber, TyNumber, TyNumber] TyNumber),
      -- 15.3 string
      ("length", mono [TyString] TyNumber),
      ("concat", mono [TyString, TyString] TyString),
      ("trim", mono [TyString] TyString),
      ("to_lower", mono [TyString] TyString),
      ("to_upper", mono [TyString] TyString),
      ("split", mono [TyString, TyString] (TyArray TyString)),
      ("join", mono [TyArray TyString, TyString] TyString),
      ("replace", mono [TyString, TyString, TyString] TyString),
      ("contains", mono [TyString, TyString] TyBool),
      ("starts_with", mono [TyString, TyString] TyBool),
      ("ends_with", mono [TyString, TyString] TyBool),
      ("index_of", mono [TyString, TyString] TyNumber),
      ("substring", mono [TyString, TyNumber, TyNumber] TyString),
      ("pad_start", mono [TyString, TyNumber, TyString] TyString),
      ("pad_end", mono [TyString, TyNumber, TyString] TyString),
      ("repeat", mono [TyString, TyNumber] TyString),
      ("lines", mono [TyString] (TyArray TyString)),
      ("to_string", mono [TyAny] TyString),
      ("to_number", mono [TyString] TyNumber),
      ("regex_test", mono [TyString, TyString] TyBool),
      ("regex_match", mono [TyString, TyString] (TyArray TyString)),
      ("regex_replace", mono [TyString, TyString, TyString] TyString),
      -- 15.4 array/map/record
      ("map", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] (TyArray (tv "U"))),
      ("filter", Scheme ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] (TyArray (tv "T"))),
      ("reduce", Scheme ["T", "U"] [TyArray (tv "T"), tv "U", TyFun [tv "U", tv "T"] (tv "U")] (tv "U")),
      ("for_each", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] TyVoid),
      ("append", Scheme ["T"] [TyArray (tv "T"), tv "T"] (TyArray (tv "T"))),
      ("concat_array", Scheme ["T"] [TyArray (tv "T"), TyArray (tv "T")] (TyArray (tv "T"))),
      ("get", Scheme ["T"] [TyMap (tv "T"), TyString] (tv "T")),
      ("has_key", Scheme ["T"] [TyMap (tv "T"), TyString] TyBool),
      ("keys", Scheme ["T"] [TyMap (tv "T")] (TyArray TyString)),
      ("values", Scheme ["T"] [TyMap (tv "T")] (TyArray (tv "T"))),
      ("size", Scheme ["T"] [TyArray (tv "T")] TyNumber),
      ("is_empty", Scheme ["T"] [TyArray (tv "T")] TyBool),
      ("first", Scheme ["T"] [TyArray (tv "T")] (tv "T")),
      ("last", Scheme ["T"] [TyArray (tv "T")] (tv "T")),
      ("slice", Scheme ["T"] [TyArray (tv "T"), TyNumber, TyNumber] (TyArray (tv "T"))),
      ("take", Scheme ["T"] [TyArray (tv "T"), TyNumber] (TyArray (tv "T"))),
      ("drop", Scheme ["T"] [TyArray (tv "T"), TyNumber] (TyArray (tv "T"))),
      ("reverse", Scheme ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      -- The element type of sort/sort_by, and of the equality-based
      -- searches below, carries a side condition the signature cannot
      -- state; it is checked at the call site, as for == (6.2).
      ("sort", Scheme ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      ("sort_by", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] (TyArray (tv "T"))),
      ("contains_array", Scheme ["T"] [TyArray (tv "T"), tv "T"] TyBool),
      ("index_of_array", Scheme ["T"] [TyArray (tv "T"), tv "T"] TyNumber),
      ("find_index", Scheme ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyNumber),
      ("every", Scheme ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyBool),
      ("any", Scheme ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyBool),
      ("flatten", Scheme ["T"] [TyArray (TyArray (tv "T"))] (TyArray (tv "T"))),
      ("flat_map", Scheme ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (TyArray (tv "U"))] (TyArray (tv "U"))),
      ("zip", Scheme ["T", "U"] [TyArray (tv "T"), TyArray (tv "U")] (TyArray (pairType (tv "T") (tv "U")))),
      ("unique", Scheme ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      ("range", mono [TyNumber, TyNumber] (TyArray TyNumber)),
      ("enumerate", Scheme ["T"] [TyArray (tv "T")] (TyArray (indexedType (tv "T")))),
      ("set", Scheme ["T"] [TyMap (tv "T"), TyString, tv "T"] (TyMap (tv "T"))),
      ("remove", Scheme ["T"] [TyMap (tv "T"), TyString] (TyMap (tv "T"))),
      ("merge", Scheme ["T"] [TyMap (tv "T"), TyMap (tv "T")] (TyMap (tv "T"))),
      ("get_or", Scheme ["T"] [TyMap (tv "T"), TyString, tv "T"] (tv "T")),
      ("entries", Scheme ["T"] [TyMap (tv "T")] (TyArray (entryType (tv "T")))),
      ("from_entries", Scheme ["T"] [TyArray (entryType (tv "T"))] (TyMap (tv "T"))),
      ("map_values", Scheme ["T", "U"] [TyMap (tv "T"), TyFun [tv "T"] (tv "U")] (TyMap (tv "U"))),
      -- 15.5 command execution. The environment is positional and
      -- required: there is no default execution environment (spec 10.1),
      -- and a keyword parameter must have a default (spec 6.1).
      ("run_command", mono [TyString, TyEnvironment] commandResultType),
      ("shell_quote", mono [TyString] TyString),
      -- 15.6 parallel/async
      ("spawn", Scheme ["T"] [TyFun [] (tv "T")] (TyAsync (tv "T"))),
      ("all", Scheme ["T"] [TyArray (TyAsync (tv "T"))] (TyArray (tv "T"))),
      ("race", Scheme ["T"] [TyArray (TyAsync (tv "T"))] (tv "T")),
      -- 15.7 error handling
      ("recover", Scheme ["T"] [TyFun [] (tv "T"), TyFun [errorType] (tv "T")] (tv "T")),
      ("fail", Scheme ["T"] [errorType] (tv "T")),
      ("error", mono [TyNumber, TyString] errorType),
      -- 15.8 serialization / cast
      ("to_json", mono [TyAny] TyString),
      ("from_json", mono [TyString] TyAny),
      ("encode", mono [TyAny, TyString] TyString),
      ("decode", mono [TyString, TyString] TyAny),
      ("cast", Scheme ["T"] [TyAny] (tv "T")),
      ("base64_encode", mono [TyString] TyString),
      ("base64_decode", mono [TyString] TyString),
      ("sha256", mono [TyString] TyString),
      ("md5", mono [TyString] TyString),
      -- 15.9 environment access / secret marking
      ("get_env", mono [TyString] TyString),
      ("has_env", mono [TyString] TyBool),
      ("get_env_or", mono [TyString, TyString] TyString),
      ("mark_secret", mono [TyString] TyString),
      -- 15.10 path operations: lexical, so no environment is involved.
      ("path_join", mono [TyArray TyString] TyString),
      ("dirname", mono [TyString] TyString),
      ("basename", mono [TyString] TyString),
      ("extname", mono [TyString] TyString),
      ("normalize_path", mono [TyString] TyString),
      ("is_absolute_path", mono [TyString] TyBool),
      -- 15.11 filesystem. As with run_command the environment is
      -- positional and required (spec 10.1, 15.11): there is no
      -- filesystem access that does not name an environment.
      ("read_file", mono [TyString, TyEnvironment] TyString),
      ("write_file", mono [TyString, TyString, TyEnvironment] TyVoid),
      ("file_exists", mono [TyString, TyEnvironment] TyBool),
      ("remove_file", mono [TyString, TyEnvironment] TyVoid),
      ("make_dir", mono [TyString, TyEnvironment] TyVoid),
      ("list_dir", mono [TyString, TyEnvironment] (TyArray TyString)),
      ("glob", mono [TyString, TyEnvironment] (TyArray TyString)),
      -- 15.12 diagnostic output
      ("log", mono [TyString] TyVoid),
      -- 15.13 nondeterministic
      ("uuid", mono [] TyString),
      ("random_string", mono [TyNumber] TyString)
      -- The reserved identifier stdin (9.3) is a String value, not a
      -- function; the elaborator resolves it specially.
    ]
