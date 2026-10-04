{-# LANGUAGE OverloadedStrings #-}

-- | Type schemes of builtin symbols (spec 15, and the polymorphism
-- rules of 4.4, which declarations with type parameters share).
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

-- | @Scheme vars params ret bounds@: universally quantified over
-- @vars@, instantiated independently per call (4.4), each instantiation
-- satisfying the bound of its variable where it has one (4.2).
data Scheme = Scheme
  { schemeVars :: [Text],
    schemeParams :: [Type],
    schemeRet :: Type,
    schemeBounds :: Map Text Bound
  }
  deriving (Show, Eq)

-- | The (possibly polymorphic) function type of a scheme.
schemeType :: Scheme -> Type
schemeType (Scheme _ ps r _) = TyFun ps r

mono :: [Type] -> Type -> Scheme
mono ps r = Scheme [] ps r Map.empty

-- | A scheme whose variables carry no bound.
poly :: [Text] -> [Type] -> Type -> Scheme
poly vs ps r = Scheme vs ps r Map.empty

-- | A scheme with one bounded variable.
bounded :: Text -> Bound -> [Text] -> [Type] -> Type -> Scheme
bounded v b vs ps r = Scheme vs ps r (Map.singleton v b)

-- | @Record<first: T, second: U>@, the result element of @zip@.
pairType :: Type -> Type -> Type
pairType a b = TyRecord (Map.fromList [("first", requiredField a), ("second", requiredField b)])

-- | @Record<index: Number, value: T>@, the result element of
-- @enumerate@.
indexedType :: Type -> Type
indexedType t = TyRecord (Map.fromList [("index", requiredField TyNumber), ("value", requiredField t)])

-- | @Record<key: String, value: T>@, the element @entries@ produces
-- and @from_entries@ consumes.
entryType :: Type -> Type
entryType t = TyRecord (Map.fromList [("key", requiredField TyString), ("value", requiredField t)])

tv :: Text -> Type
tv = TyVar

-- | @T | Null@, the result of a search that may come up empty (15.1).
orNull :: Type -> Type
orNull t = mkUnion t [TyNull]

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
      ("to_string", bounded "T" (BoundNamed BStringifiable) ["T"] [tv "T"] TyString),
      ("to_number", mono [TyString] TyNumber),
      ("regex_test", mono [TyString, TyString] TyBool),
      ("regex_match", mono [TyString, TyString] (TyArray TyString)),
      ("regex_replace", mono [TyString, TyString, TyString] TyString),
      -- 15.4 array/map/record
      ("map", poly ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] (TyArray (tv "U"))),
      ("filter", poly ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] (TyArray (tv "T"))),
      ("reduce", poly ["T", "U"] [TyArray (tv "T"), tv "U", TyFun [tv "U", tv "T"] (tv "U")] (tv "U")),
      ("for_each", poly ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] TyVoid),
      ("append", poly ["T"] [TyArray (tv "T"), tv "T"] (TyArray (tv "T"))),
      ("concat_array", poly ["T"] [TyArray (tv "T"), TyArray (tv "T")] (TyArray (tv "T"))),
      ("get", poly ["T"] [TyMap (tv "T"), TyString] (tv "T")),
      ("has_key", poly ["T"] [TyMap (tv "T"), TyString] TyBool),
      ("keys", poly ["T"] [TyMap (tv "T")] (TyArray TyString)),
      ("values", poly ["T"] [TyMap (tv "T")] (TyArray (tv "T"))),
      ("size", poly ["T"] [TyArray (tv "T")] TyNumber),
      ("is_empty", poly ["T"] [TyArray (tv "T")] TyBool),
      ("first", poly ["T"] [TyArray (tv "T")] (tv "T")),
      ("last", poly ["T"] [TyArray (tv "T")] (tv "T")),
      ("slice", poly ["T"] [TyArray (tv "T"), TyNumber, TyNumber] (TyArray (tv "T"))),
      ("take", poly ["T"] [TyArray (tv "T"), TyNumber] (TyArray (tv "T"))),
      ("drop", poly ["T"] [TyArray (tv "T"), TyNumber] (TyArray (tv "T"))),
      ("reverse", poly ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      -- The element type of sort/sort_by, and of the equality-based
      -- searches below, carries a side condition the signature cannot
      -- state; it is checked at the call site, as for == (6.2).
      -- The conditions of these are bounds (4.2): an order for the
      -- sorts, equality for the searches.
      ("sort", bounded "T" (BoundNamed BOrderable) ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      ("sort_by", bounded "U" (BoundNamed BOrderable) ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (tv "U")] (TyArray (tv "T"))),
      ("contains_array", bounded "T" (BoundNamed BComparable) ["T"] [TyArray (tv "T"), tv "T"] TyBool),
      ("index_of_array", bounded "T" (BoundNamed BComparable) ["T"] [TyArray (tv "T"), tv "T"] TyNumber),
      ("find", poly ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] (orNull (tv "T"))),
      ("find_index", poly ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyNumber),
      ("every", poly ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyBool),
      ("any", poly ["T"] [TyArray (tv "T"), TyFun [tv "T"] TyBool] TyBool),
      ("flatten", poly ["T"] [TyArray (TyArray (tv "T"))] (TyArray (tv "T"))),
      ("flat_map", poly ["T", "U"] [TyArray (tv "T"), TyFun [tv "T"] (TyArray (tv "U"))] (TyArray (tv "U"))),
      ("zip", poly ["T", "U"] [TyArray (tv "T"), TyArray (tv "U")] (TyArray (pairType (tv "T") (tv "U")))),
      ("unique", bounded "T" (BoundNamed BComparable) ["T"] [TyArray (tv "T")] (TyArray (tv "T"))),
      ("range", mono [TyNumber, TyNumber] (TyArray TyNumber)),
      ("enumerate", poly ["T"] [TyArray (tv "T")] (TyArray (indexedType (tv "T")))),
      ("set", poly ["T"] [TyMap (tv "T"), TyString, tv "T"] (TyMap (tv "T"))),
      ("remove", poly ["T"] [TyMap (tv "T"), TyString] (TyMap (tv "T"))),
      ("merge", poly ["T"] [TyMap (tv "T"), TyMap (tv "T")] (TyMap (tv "T"))),
      ("get_or", poly ["T"] [TyMap (tv "T"), TyString, tv "T"] (tv "T")),
      ("entries", poly ["T"] [TyMap (tv "T")] (TyArray (entryType (tv "T")))),
      ("from_entries", poly ["T"] [TyArray (entryType (tv "T"))] (TyMap (tv "T"))),
      ("map_values", poly ["T", "U"] [TyMap (tv "T"), TyFun [tv "T"] (tv "U")] (TyMap (tv "U"))),
      -- 15.5 command execution. The environment is positional and
      -- requiredField: there is no default execution environment (spec 10.1),
      -- and a keyword parameter must have a default (spec 6.1).
      ("run", mono [TyEnvironment, TyString] commandResultType),
      ("shell_quote", mono [TyString] TyString),
      -- 15.6 parallel/async
      ("spawn", poly ["T"] [TyFun [] (tv "T")] (TyAsync (tv "T"))),
      ("all", poly ["T"] [TyArray (TyAsync (tv "T"))] (TyArray (tv "T"))),
      ("race", poly ["T"] [TyArray (TyAsync (tv "T"))] (tv "T")),
      -- 15.7 error handling
      ("recover", poly ["T"] [TyFun [] (tv "T"), TyFun [errorType] (tv "T")] (tv "T")),
      ("fail", poly ["T"] [errorType] (tv "T")),
      ("error", mono [TyNumber, TyString] errorType),
      -- 15.7 retrying and waiting. A strategy is an array of delays.
      ("retry", poly ["T"] [TyArray TyNumber, TyFun [] (tv "T")] (tv "T")),
      ("retry_if", poly ["T"] [TyArray TyNumber, TyFun [errorType] TyBool, TyFun [] (tv "T")] (tv "T")),
      ("until", poly ["T"] [TyArray TyNumber, TyFun [tv "T"] TyBool, TyFun [] (tv "T")] (tv "T")),
      ("timeout", poly ["T"] [TyNumber, TyFun [] (tv "T")] (tv "T")),
      ("backoff_fixed", mono [TyNumber, TyNumber] (TyArray TyNumber)),
      ("backoff_linear", mono [TyNumber, TyNumber, TyNumber] (TyArray TyNumber)),
      ("backoff_exponential", mono [TyNumber, TyNumber, TyNumber] (TyArray TyNumber)),
      -- 15.8 serialization / cast
      ("to_json", mono [TyAny] TyString),
      ("from_json", mono [TyString] TyAny),
      ("encode", mono [TyAny, TyString] TyString),
      ("decode", mono [TyString, TyString] TyAny),
      ("cast", poly ["T"] [TyAny] (tv "T")),
      ("base64_encode", mono [TyString] TyString),
      ("base64_decode", mono [TyString] TyString),
      ("sha256", mono [TyString] TyString),
      ("md5", mono [TyString] TyString),
      -- 15.9 environment access / secret marking
      ("get_env", mono [TyString] TyString),
      ("find_env", mono [TyString] (orNull TyString)),
      ("has_env", mono [TyString] TyBool),
      ("get_env_or", mono [TyString, TyString] TyString),
      -- T is bounded by String | Null (6.10).
      ("mark_secret", bounded "T" (BoundType (orNull TyString)) ["T"] [tv "T"] (tv "T")),
      -- 15.10 path operations: lexical, so no environment is involved.
      ("path_join", mono [TyArray TyString] TyString),
      ("dirname", mono [TyString] TyString),
      ("basename", mono [TyString] TyString),
      ("extname", mono [TyString] TyString),
      ("normalize_path", mono [TyString] TyString),
      ("is_absolute_path", mono [TyString] TyBool),
      -- 15.11 filesystem. As with run the environment is
      -- positional and requiredField (spec 10.1, 15.11): there is no
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
      ("random_string", mono [TyNumber] TyString),
      ("backoff_jitter", mono [TyArray TyNumber] (TyArray TyNumber))
      -- The reserved identifier stdin (9.3) is a String value, not a
      -- function; the elaborator resolves it specially.
    ]
