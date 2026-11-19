{-# LANGUAGE OverloadedStrings #-}

-- | Every built-in of spec chapter 15, evaluated in process, by a table
-- of examples: an expression and what it evaluates to.
--
-- The table is checked against 'builtinValueNames': a built-in with no
-- example fails the first test, so a built-in added to the library
-- without one is noticed here rather than never exercised. An example
-- names the built-in it is for, and must call it.
module Language.Lask.Builtins.ImplSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner, RtHooks (..))
import Language.Lask.Builtins.Names (builtinValueNames)
import Language.Lask.Builtins.Sig (builtinSchemes)
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.Elaborate (elaborateProgram)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue)
import System.Environment (lookupEnv)
import Test.Hspec

-- | What an example evaluates to.
data Outcome
  = -- | This value, as canonical JSON (spec 13.1).
    Is Text
  | -- | @true@: the expression is a check written in Lask.
    Holds
  | -- | A failure with this code.
    Fails ErrorCode

data Case = Case
  { exBuiltin :: Text,
    exExpr :: Text,
    exOutcome :: Outcome
  }

is :: Text -> Text -> Text -> Case
is b e v = Case b e (Is v)

holds :: Text -> Text -> Case
holds b e = Case b e Holds

fails :: Text -> Text -> ErrorCode -> Case
fails b e c = Case b e (Fails c)

-- | Declarations every example may use: values whose types a literal
-- with no expected type would not give.
prelude :: Text
prelude =
  T.unlines
    [ "m: Map<Number> = {b: 2, a: 1}",
      "none: Array<Number> = []",
      "no_strings: Array<String> = []",
      "no_handles: Array<AsyncHandle<Number>> = []",
      "unset = \"LASK_IMPL_SPEC_SURELY_UNSET\""
    ]

examples :: [Case]
examples =
  -- 15.2 numeric operations
  [ is "add" "add(1, 2)" "3",
    holds "add" "add(-1.5, 0.5) == -1",
    is "sub" "sub(1, 3)" "-2",
    is "mul" "mul(-2, 3)" "-6",
    is "div" "div(10, 4)" "2.5",
    fails "div" "div(1, 0)" ERuntimeDivByZero,
    -- add/sub/mul/div/mod mean what the operators of 6.2 do (15.2).
    holds "add" "add(0.1, 0.2) == 0.1 + 0.2",
    holds "mul" "mul(0.1, 3) == 0.1 * 3",
    holds "div" "div(10, 3) == 10 / 3",
    is "div" "div(10, 3)" "3.3333333333333335",
    -- A result beyond the range of Number (4.1) is refused, and one
    -- too small to tell from zero is zero.
    fails "mul" "mul(to_number(\"1e308\"), 10)" ERuntimeValue,
    fails "add" "add(to_number(\"1.7e308\"), to_number(\"1e308\"))" ERuntimeValue,
    fails "sub" "sub(0, to_number(\"1.7e308\")) - to_number(\"1e308\")" ERuntimeValue,
    fails "div" "div(to_number(\"1e308\"), 0.1)" ERuntimeValue,
    fails "sum" "sum([to_number(\"1e308\"), to_number(\"1e308\")])" ERuntimeValue,
    is "mul" "mul(to_number(\"1e-200\"), to_number(\"1e-200\"))" "0",
    is "mul" "mul(to_number(\"1e300\"), 1000)" (T.pack ('1' : replicate 303 '0')),
    is "mod" "mod(7, 3)" "1",
    is "mod" "mod(7.5, 2)" "1.5",
    fails "mod" "mod(1, 0)" ERuntimeDivByZero,
    is "abs" "abs(-3.5)" "3.5",
    is "abs" "abs(0)" "0",
    is "floor" "floor(-1.5)" "-2",
    is "floor" "floor(2)" "2",
    is "ceil" "ceil(-1.5)" "-1",
    is "ceil" "ceil(1.2)" "2",
    is "round" "round(2.4)" "2",
    is "round" "round(-2.6)" "-3",
    is "min" "min(1, -1)" "-1",
    is "max" "max(1, -1)" "1",
    is "sum" "sum([1, 2, 3])" "6",
    is "sum" "sum(none)" "0",
    is "pow" "pow(2, 10)" "1024",
    is "pow" "pow(0, 0)" "1",
    is "pow" "pow(2, -1)" "0.5",
    is "pow" "pow(4, 0.5)" "2",
    is "pow" "pow(-2, 3)" "-8",
    fails "pow" "pow(-8, 0.5)" ERuntimeValue,
    fails "pow" "pow(0, -1)" ERuntimeValue,
    is "sqrt" "sqrt(16)" "4",
    fails "sqrt" "sqrt(-1)" ERuntimeValue,
    is "clamp" "clamp(5, 1, 3)" "3",
    is "clamp" "clamp(0, 1, 3)" "1",
    is "clamp" "clamp(2, 1, 3)" "2",
    fails "clamp" "clamp(2, 3, 1)" ERuntimeValue,
    -- 15.3 string operations: code points, not bytes or graphemes
    is "length" "length(\"é👍\")" "2",
    is "length" "length(\"\")" "0",
    is "concat" "concat(\"a\", \"\")" "\"a\"",
    is "concat" "concat(\"é\", \"👍\")" "\"é👍\"",
    is "trim" "trim(\"\\t a b \\n\")" "\"a b\"",
    is "to_lower" "to_lower(\"ÄbC\")" "\"äbc\"",
    is "to_upper" "to_upper(\"äbc\")" "\"ÄBC\"",
    is "split" "split(\"a,b,\", \",\")" "[\"a\",\"b\",\"\"]",
    is "split" "split(\"abc\", \"\")" "[\"a\",\"b\",\"c\"]",
    is "split" "split(\"\", \"\")" "[]",
    is "split" "split(\"é👍\", \"\")" "[\"é\",\"👍\"]",
    is "join" "join([\"a\", \"b\"], \", \")" "\"a, b\"",
    is "join" "join(no_strings, \",\")" "\"\"",
    is "join" "join([\"a\", \"b\"], \"\")" "\"ab\"",
    is "replace" "replace(\"aaa\", \"a\", \"b\")" "\"bbb\"",
    is "contains" "contains(\"abc\", \"bc\")" "true",
    is "contains" "contains(\"abc\", \"\")" "true",
    is "contains" "contains(\"\", \"a\")" "false",
    is "starts_with" "starts_with(\"abc\", \"ab\")" "true",
    is "starts_with" "starts_with(\"abc\", \"\")" "true",
    is "ends_with" "ends_with(\"abc\", \"bc\")" "true",
    is "ends_with" "ends_with(\"\", \"\")" "true",
    is "index_of" "index_of(\"héllo\", \"l\")" "2",
    is "index_of" "index_of(\"👍a\", \"a\")" "1",
    is "index_of" "index_of(\"abc\", \"\")" "0",
    is "index_of" "index_of(\"abc\", \"x\")" "-1",
    is "substring" "substring(\"héllo\", 1, 3)" "\"él\"",
    is "substring" "substring(\"héllo\", -3, 2)" "\"hé\"",
    is "substring" "substring(\"abc\", 2, 1)" "\"\"",
    is "substring" "substring(\"abc\", 1, 99)" "\"bc\"",
    is "substring" "substring(\"abc\", 0.9, 2.9)" "\"ab\"",
    is "substring" "substring(\"👍a\", 0, 1)" "\"👍\"",
    is "pad_start" "pad_start(\"5\", 3, \"0\")" "\"005\"",
    is "pad_start" "pad_start(\"5\", 4, \"ab\")" "\"aba5\"",
    is "pad_start" "pad_start(\"é\", 3, \"👍\")" "\"👍👍é\"",
    is "pad_start" "pad_start(\"abc\", 2, \"x\")" "\"abc\"",
    is "pad_start" "pad_start(\"a\", 3, \"\")" "\"a\"",
    is "pad_end" "pad_end(\"a\", 4, \"xy\")" "\"axyx\"",
    is "pad_end" "pad_end(\"abc\", -1, \"x\")" "\"abc\"",
    is "repeat" "repeat(\"ab\", 2)" "\"abab\"",
    is "repeat" "repeat(\"ab\", 2.7)" "\"abab\"",
    is "repeat" "repeat(\"ab\", -1)" "\"\"",
    is "repeat" "repeat(\"\", 5)" "\"\"",
    is "lines" "lines(\"a\\r\\nb\\n\")" "[\"a\",\"b\"]",
    is "lines" "lines(\"a\\n\\nb\")" "[\"a\",\"\",\"b\"]",
    is "lines" "lines(\"\")" "[]",
    is "lines" "lines(\"\\n\")" "[\"\"]",
    is "to_string" "to_string(3.0)" "\"3\"",
    is "to_string" "to_string(-0.5)" "\"-0.5\"",
    is "to_string" "to_string(true)" "\"true\"",
    is "to_string" "to_string(\"s\")" "\"s\"",
    fails "to_string" "to_string(from_json(\"null\"))" ERuntimeValue,
    fails "to_string" "to_string(from_json(\"[1]\"))" ERuntimeValue,
    is "to_number" "to_number(\" 1.5e2 \")" "150",
    is "to_number" "to_number(\"-0\")" "0",
    fails "to_number" "to_number(\"\")" EIoDataDecode,
    fails "to_number" "to_number(\"01\")" EIoDataDecode,
    fails "to_number" "to_number(\"+1\")" EIoDataDecode,
    fails "to_number" "to_number(\".5\")" EIoDataDecode,
    fails "to_number" "to_number(\"1.\")" EIoDataDecode,
    fails "to_number" "to_number(\"0x10\")" EIoDataDecode,
    fails "to_number" "to_number(\"NaN\")" EIoDataDecode,
    fails "to_number" "to_number(\"1e1000000000\")" EIoDataDecode,
    fails "to_number" "to_number(\"-1e309\")" EIoDataDecode,
    is "to_number" "to_number(\"1e-1000000000\")" "0",
    is "regex_test" "regex_test(\"a1\", '\\d')" "true",
    is "regex_test" "regex_test(\"a1\", \"[[:digit:]]\")" "true",
    is "regex_test" "regex_test(\"abc\", \"^b\")" "false",
    fails "regex_test" "regex_test(\"a\", \"(\")" ERuntimeRegex,
    -- leftmost-longest, not leftmost-first (15.3)
    is "regex_match" "regex_match(\"ab\", \"a|ab\")" "[\"ab\"]",
    is "regex_match" "regex_match(\"x\", \"(a)?x\")" "[\"x\",\"\"]",
    is "regex_match" "regex_match(\"abc\", \"(x)|(b)\")" "[\"b\",\"\",\"b\"]",
    is "regex_match" "regex_match(\"abc\", \"z\")" "[]",
    is "regex_replace" "regex_replace(\"abc\", \"(b)\", \"[$1$$]\")" "\"a[b$]c\"",
    is "regex_replace" "regex_replace(\"a1b22\", '\\d+', \"#\")" "\"a#b#\"",
    -- an empty match advances one character before the next attempt
    is "regex_replace" "regex_replace(\"abc\", \"x*\", \"-\")" "\"-a-b-c-\"",
    -- 15.4 arrays, maps and records
    is "map" "map([1, 2], \\(x: Number) -> x * 2)" "[2,4]",
    is "map" "map(none, \\(x: Number) -> x * 2)" "[]",
    is "filter" "filter([1, 2, 3], \\(x: Number) -> x > 1)" "[2,3]",
    is "reduce" "reduce([\"a\", \"b\"], \"\", \\(acc: String, x: String) -> concat(acc, x))" "\"ab\"",
    is "reduce" "reduce(none, 7, \\(acc: Number, x: Number) -> acc + x)" "7",
    is "for_each" "for_each([1], \\(x: Number) -> x)" "{\"$type\":\"Void\"}",
    is "append" "append([1], 2)" "[1,2]",
    is "append" "append(none, 2)" "[2]",
    is "concat_array" "concat_array([1], [2, 3])" "[1,2,3]",
    is "get" "get(m, \"a\")" "1",
    fails "get" "get(m, \"z\")" ERuntimeAccess,
    is "has_key" "has_key(m, \"a\")" "true",
    is "has_key" "has_key(m, \"\")" "false",
    -- keys and entries traverse in code point order (15.4)
    is "keys" "keys(m)" "[\"a\",\"b\"]",
    is "values" "values(m)" "[1,2]",
    is "size" "size([1, 2])" "2",
    is "size" "size(none)" "0",
    is "is_empty" "is_empty(none)" "true",
    is "is_empty" "is_empty([0])" "false",
    is "first" "first([1, 2])" "1",
    fails "first" "first(none)" ERuntimeAccess,
    is "last" "last([1, 2])" "2",
    fails "last" "last(none)" ERuntimeAccess,
    is "slice" "slice([1, 2, 3], 1, 2)" "[2]",
    is "slice" "slice([1, 2, 3], -1, 10)" "[1,2,3]",
    is "slice" "slice([1, 2, 3], 2, 1)" "[]",
    is "take" "take([1, 2], 5)" "[1,2]",
    is "take" "take([1, 2], -1)" "[]",
    is "drop" "drop([1, 2], -1)" "[1,2]",
    is "drop" "drop([1, 2], 5)" "[]",
    is "reverse" "reverse([1, 2, 3])" "[3,2,1]",
    is "reverse" "reverse(none)" "[]",
    is "sort" "sort([3, -1, 2])" "[-1,2,3]",
    is "sort" "sort([\"b\", \"a\", \"B\", \"é\"])" "[\"B\",\"a\",\"b\",\"é\"]",
    is "sort" "sort(none)" "[]",
    is "sort_by" "map(sort_by([{n: 2, k: \"a\"}, {n: 1, k: \"b\"}, {n: 2, k: \"c\"}], \\(r: Record<n: Number, k: String>) -> r.n), \\(r: Record<n: Number, k: String>) -> r.k)" "[\"b\",\"a\",\"c\"]",
    is "contains_array" "contains_array([[1], [2]], [2])" "true",
    is "contains_array" "contains_array(none, 1)" "false",
    is "index_of_array" "index_of_array([1, 2, 1], 1)" "0",
    is "index_of_array" "index_of_array([1, 2], 3)" "-1",
    -- p stops being applied once one is true: 1 / 0 is never computed
    is "find" "find([1, 0], \\(x: Number) -> 1 / x > 0)" "1",
    is "find" "find([1, 2], \\(x: Number) -> x > 5)" "null",
    is "find_index" "find_index([5, 6], \\(x: Number) -> x > 5)" "1",
    is "find_index" "find_index([1, 2], \\(x: Number) -> x > 5)" "-1",
    is "every" "every(none, \\(x: Number) -> false)" "true",
    is "every" "every([1, 0], \\(x: Number) -> 1 / x < 0)" "false",
    is "any" "any(none, \\(x: Number) -> true)" "false",
    is "any" "any([1, 0], \\(x: Number) -> 1 / x > 0)" "true",
    is "flatten" "flatten([[1], [2, 3]])" "[1,2,3]",
    is "flatten" "flatten([[[1]]])" "[[1]]",
    is "flat_map" "flat_map([1, 2], \\(x: Number) -> [x, x])" "[1,1,2,2]",
    is "zip" "zip([1, 2, 3], [\"a\"])" "[{\"first\":1,\"second\":\"a\"}]",
    is "zip" "zip(none, [1])" "[]",
    is "unique" "unique([1, 1, 2, 1])" "[1,2]",
    is "unique" "unique([[1], [1]])" "[[1]]",
    is "range" "range(-2, 1)" "[-2,-1,0]",
    is "range" "range(3, 1)" "[]",
    fails "range" "range(0.5, 2)" ERuntimeValue,
    is "enumerate" "enumerate([\"a\"])" "[{\"index\":0,\"value\":\"a\"}]",
    is "set" "set(m, \"a\", 9)" "{\"a\":9,\"b\":2}",
    is "set" "set(m, \"c\", 3)" "{\"a\":1,\"b\":2,\"c\":3}",
    holds "set" "do {\n  n = set(m, \"a\", 9)\n  get(m, \"a\") == 1\n}",
    is "remove" "remove(m, \"a\")" "{\"b\":2}",
    is "remove" "remove(m, \"z\")" "{\"a\":1,\"b\":2}",
    is "merge" "merge(m, set(m, \"a\", 5))" "{\"a\":5,\"b\":2}",
    is "get_or" "get_or(m, \"z\", 0)" "0",
    is "get_or" "get_or(m, \"a\", 0)" "1",
    is "entries" "entries(m)" "[{\"key\":\"a\",\"value\":1},{\"key\":\"b\",\"value\":2}]",
    is "from_entries" "from_entries([{key: \"a\", value: 1}, {key: \"a\", value: 2}])" "{\"a\":2}",
    holds "from_entries" "from_entries(entries(m)) == m",
    is "map_values" "map_values(m, \\(v: Number) -> v * 10)" "{\"a\":10,\"b\":20}",
    -- 15.5 command execution (a mock runner: no process is started)
    is "run" "run(#local, \"echo hi\").stdout" "\"hi\\n\"",
    is "run" "run(#local, \"boom\").code" "7",
    fails "run" "run(#local, \"unreachable\")" EIoEnvResolve,
    holds "runnable" "runnable(#local) == #local",
    is "runnable" "to_json(runnable(#alpine:3.20, memory = \"1g\"))" "\"{\\\"$type\\\":\\\"Runnable\\\",\\\"kind\\\":\\\"docker\\\",\\\"params\\\":{\\\"image\\\":\\\"alpine:3.20\\\",\\\"memory\\\":\\\"1g\\\"}}\"",
    is "shell_quote" "shell_quote(\"it's\")" "\"'it'\\\\''s'\"",
    is "shell_quote" "shell_quote(\"\")" "\"''\"",
    is "with_container" "with_container(#local, \\(s) -> run(s, \"echo a\").stdout)" "\"a\\n\"",
    -- 15.6 parallel and asynchronous helpers
    is "spawn" "await spawn(\\() -> 1)" "1",
    is "all" "all([spawn(\\() -> 1), spawn(\\() -> 2)])" "[1,2]",
    is "all" "all(no_handles)" "[]",
    is "race" "race([spawn(\\() -> 1)])" "1",
    fails "race" "race(no_handles)" ERuntimeValue,
    -- 15.7 error handling, retrying and waiting
    is "recover" "recover(\\(): Number -> fail(error(2, \"x\")), \\(e: Error) -> e.code)" "2",
    is "recover" "recover(\\() -> 1, \\(e: Error) -> 0)" "1",
    is "fail" "try { fail(error(5, \"boom\")) } catch (e) { e.message }" "\"boom\"",
    is "error" "error(1, \"m\")" "{\"code\":1,\"message\":\"m\"}",
    is "retry" "retry([0, 0], \\() -> 1)" "1",
    is "retry" "try { retry([0], \\(): Number -> fail(error(3, \"x\"))) } catch (e) { e.code }" "3",
    fails "retry" "retry([-1], \\() -> 1)" ERuntimeValue,
    is "retry_if" "try { retry_if([0, 0], \\(e: Error) -> false, \\(): Number -> fail(error(4, \"y\"))) } catch (e) { e.code }" "4",
    is "until" "until([0], \\(v: Number) -> v == 1, \\() -> 1)" "1",
    fails "until" "until([0], \\(v: Number) -> v == 2, \\() -> 1)" ERuntimeUntilExhausted,
    is "timeout" "timeout(5, \\() -> 2)" "2",
    fails "timeout" "timeout(0, \\() -> 1)" ERuntimeValue,
    fails "timeout" "timeout(0.05, \\() -> run(#local, \"hang\"))" ERuntimeTimeout,
    is "backoff_fixed" "backoff_fixed(2, 3)" "[2,2,2]",
    is "backoff_fixed" "backoff_fixed(2, 0)" "[]",
    fails "backoff_fixed" "backoff_fixed(-1, 1)" ERuntimeValue,
    fails "backoff_fixed" "backoff_fixed(1, 1.5)" ERuntimeValue,
    is "backoff_linear" "backoff_linear(1, 2, 3)" "[1,3,5]",
    is "backoff_exponential" "backoff_exponential(1, 2, 4)" "[1,2,4,8]",
    fails "backoff_exponential" "backoff_exponential(1, 10, 400)" ERuntimeValue,
    -- 15.8 serialization and type migration
    is "to_json" "to_json({b: 1, a: [true, null]})" "\"{\\\"a\\\":[true,null],\\\"b\\\":1}\"",
    is "to_json" "to_json(\"é\\u{1}\\\"\")" "\"\\\"é\\\\u0001\\\\\\\"\\\"\"",
    is "from_json" "from_json(\" 1 \")" "1",
    is "to_json" "to_json(1.5 - 0.5)" "\"1\"",
    is "encode" "encode(1.5 - 0.5, \"yaml\")" "\"1\\n\"",
    fails "from_json" "from_json(\"\")" EIoDataDecode,
    fails "from_json" "from_json(\"{\\\"a\\\": [1e1000000000]}\")" EIoDataDecode,
    fails "decode" "decode(\"a: 1e400\", \"yaml\")" EIoDataDecode,
    fails "decode" "decode(\"a = inf\", \"toml\")" EIoDataDecode,
    is "decode" "decode(\"a = 0.1\", \"toml\")" "{\"a\":0.1}",
    is "encode" "encode({a: 1}, \"json\")" "\"{\\\"a\\\":1}\"",
    is "encode" "encode({b: [1, true], a: null}, \"yaml\")" "\"a: null\\nb:\\n- 1\\n- true\\n\"",
    is "encode" "encode({a: 1}, \"toml\")" "\"a = 1\\n\"",
    is "encode" "encode({a: \"x y\"}, \"dotenv\")" "\"a='x y'\\n\"",
    fails "encode" "encode(1, \"toml\")" ERuntimeValue,
    fails "encode" "encode({a: 1}, \"dotenv\")" ERuntimeValue,
    fails "encode" "encode([{a: \"1\"}, {b: \"2\"}], \"csv\")" ERuntimeValue,
    fails "encode" "encode({a: 1}, \"xml\")" EIoDataDecode,
    is "decode" "decode(\"a,b\\n1,2\\n\", \"csv\")" "[{\"a\":\"1\",\"b\":\"2\"}]",
    is "decode" "decode(\"x = 1\\nd = 1979-05-27\\n\", \"toml\")" "{\"d\":\"1979-05-27\",\"x\":1}",
    is "decode" "decode(\"- 1\\n- yes\\n- null\\n\", \"yaml\")" "[1,true,null]",
    is "decode" "decode(\"A=b=c\\n# x\\nB='z'\\n\", \"dotenv\")" "{\"A\":\"b=c\",\"B\":\"z\"}",
    fails "decode" "decode(\"{\", \"json\")" EIoDataDecode,
    fails "decode" "decode(\"{}\", \"xml\")" EIoDataDecode,
    holds "cast" "do {\n  r: Map<Number> = cast(from_json(\"{\\\"a\\\": 1}\"))\n  get(r, \"a\") == 1\n}",
    fails "cast" "do {\n  n: Number = cast(from_json(\"\\\"s\\\"\"))\n  n\n}" ERuntimeCast,
    is "base64_encode" "base64_encode(\"é\")" "\"w6k=\"",
    is "base64_encode" "base64_encode(\"\")" "\"\"",
    is "base64_decode" "base64_decode(\"aGk=\")" "\"hi\"",
    is "base64_decode" "base64_decode(\"aGk\")" "\"hi\"",
    is "base64_decode" "base64_decode(\"Pz8-\")" "\"??>\"",
    fails "base64_decode" "base64_decode(\"!!!\")" EIoDataDecode,
    fails "base64_decode" "base64_decode(\"/w==\")" EIoDataDecode,
    is "sha256" "sha256(\"abc\")" "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"",
    is "sha256" "sha256(\"\")" "\"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\"",
    is "md5" "md5(\"abc\")" "\"900150983cd24fb0d6963f7d28e17f72\"",
    is "md5" "md5(\"\")" "\"d41d8cd98f00b204e9800998ecf8427e\"",
    -- 15.9 environment access and secret marking
    holds "get_env" "length(get_env(\"PATH\")) > 0",
    fails "get_env" "get_env(unset)" ERuntimeAccess,
    is "find_env" "find_env(unset)" "null",
    holds "find_env" "find_env(\"PATH\") == get_env(\"PATH\")",
    is "has_env" "has_env(unset)" "false",
    is "has_env" "has_env(\"PATH\")" "true",
    is "get_env_or" "get_env_or(unset, \"fallback\")" "\"fallback\"",
    holds "get_env_or" "get_env_or(\"PATH\", \"\") == get_env(\"PATH\")",
    is "mark_secret" "mark_secret(\"impl-spec-value\")" "\"impl-spec-value\"",
    is "mark_secret" "mark_secret(null)" "null",
    -- 15.10 path operations: lexical and POSIX
    is "path_join" "path_join([\"a\", \"/b\", \"c\"])" "\"/b/c\"",
    is "path_join" "path_join([\"a\", \"\", \"b\"])" "\"a/b\"",
    is "path_join" "path_join(no_strings)" "\"\"",
    is "path_join" "path_join([\"a/\", \"./b\", \"..\"])" "\"a\"",
    is "dirname" "dirname(\"a/b/c\")" "\"a/b\"",
    is "dirname" "dirname(\"abc\")" "\".\"",
    is "dirname" "dirname(\"/a\")" "\"/\"",
    is "dirname" "dirname(\"a/\")" "\".\"",
    is "dirname" "dirname(\"/\")" "\"/\"",
    is "dirname" "dirname(\"//\")" "\"/\"",
    is "basename" "basename(\"a/b/c.txt\")" "\"c.txt\"",
    is "basename" "basename(\"a/b/\")" "\"b\"",
    is "basename" "basename(\"/\")" "\"/\"",
    is "extname" "extname(\"a/b/c.tar.gz\")" "\".gz\"",
    is "extname" "extname(\".env\")" "\"\"",
    is "extname" "extname(\"a.b/c\")" "\"\"",
    is "extname" "extname(\"a/.b.c\")" "\".c\"",
    is "extname" "extname(\"a.\")" "\".\"",
    is "extname" "extname(\"a/..\")" "\"\"",
    is "normalize_path" "normalize_path(\"/../a/./b//c/..\")" "\"/a/b\"",
    is "normalize_path" "normalize_path(\"../a/..\")" "\"..\"",
    is "normalize_path" "normalize_path(\"a/../..\")" "\"..\"",
    is "normalize_path" "normalize_path(\"./a\")" "\"a\"",
    is "normalize_path" "normalize_path(\"/..\")" "\"/\"",
    is "is_absolute_path" "is_absolute_path(\"/a\")" "true",
    is "is_absolute_path" "is_absolute_path(\"a\")" "false",
    is "is_absolute_path" "is_absolute_path(\"\")" "false",
    -- 15.11 filesystem (a mock: "greeting.txt" is the one file there is)
    is "read_file" "read_file(\"greeting.txt\", #local)" "\"hello\\n\"",
    fails "read_file" "read_file(\"missing.txt\", #local)" EIoFs,
    is "write_file" "write_file(\"out.txt\", \"x\", #local)" "{\"$type\":\"Void\"}",
    is "file_exists" "file_exists(\"greeting.txt\", #local)" "true",
    is "file_exists" "file_exists(\"missing.txt\", #local)" "false",
    is "remove_file" "remove_file(\"out.txt\", #local)" "{\"$type\":\"Void\"}",
    is "make_dir" "make_dir(\"d\", #local)" "{\"$type\":\"Void\"}",
    is "list_dir" "list_dir(\".\", #local)" "[\"a.txt\",\"b.txt\"]",
    is "glob" "glob(\"src/*.lask\", #local)" "[\"src/main.lask\"]",
    -- 15.12 diagnostic output
    is "log" "log(\"x\")" "{\"$type\":\"Void\"}",
    -- 15.13 nondeterministic functions
    holds "uuid" "regex_test(uuid(), \"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$\")",
    holds "uuid" "uuid() != uuid()",
    holds "random_string" "regex_test(random_string(16), \"^[0-9a-z]{16}$\")",
    is "random_string" "random_string(0)" "\"\"",
    fails "random_string" "random_string(-1)" ERuntimeValue,
    fails "random_string" "random_string(1.5)" ERuntimeValue,
    holds "backoff_jitter" "every(zip(backoff_jitter([1, 2, 0]), [1, 2, 0]), \\(p) -> p.first >= 0 && p.first <= p.second)",
    is "backoff_jitter" "backoff_jitter(none)" "[]",
    fails "backoff_jitter" "backoff_jitter([-1])" ERuntimeValue
  ]

-- | No real processes: @echo@ prints its arguments, @boom@ exits 7,
-- @hang@ waits until it is stopped, and @unreachable@ has no
-- environment.
mockRunner :: CommandRunner
mockRunner _env cmd
  | cmd == "boom" = pure (Right (7, "", "kaboom"))
  | cmd == "hang" = threadDelay 10000000 >> pure (Right (0, "", ""))
  | cmd == "unreachable" = pure (Left (ioFailure EIoEnvResolve "cannot resolve environment"))
  | Just rest <- T.stripPrefix "echo " cmd = pure (Right (0, rest <> "\n", ""))
  | otherwise = pure (Right (0, "", ""))

-- | No real filesystem: the filesystem functions themselves are
-- covered against a directory by "Language.Lask.Runtime.EnvironmentSpec".
mockFileRunner :: FileRunner
mockFileRunner _env op = pure $ case op of
  FileRead "greeting.txt" -> Right (VString "hello\n")
  FileRead p -> Left (ioFailure EIoFs ("no such file: '" <> p <> "'"))
  FileExists p -> Right (VBool (p == "greeting.txt"))
  FileListDir _ -> Right (VArray (V.fromList [VString "a.txt", VString "b.txt"]))
  FileGlob _ -> Right (VArray (V.fromList [VString "src/main.lask"]))
  FileWrite {} -> Right VVoid
  FileRemove _ -> Right VVoid
  FileMakeDir _ -> Right VVoid

-- | Evaluate an expression after the prelude: its value as canonical
-- JSON, or the code it failed with, or the static diagnostics that
-- stopped it before evaluation.
evaluate :: Text -> IO (Either (Either [ErrorCode] (Maybe ErrorCode)) Text)
evaluate expr = do
  let src = prelude <> "example() = " <> expr <> "\n"
      reader p = pure (if p == "main.lask" then Right src else Left "not found")
  r <- loadProgramWith reader "main.lask"
  case r >>= \prog -> (,) prog <$> validateProgram prog of
    Left ds -> pure (Left (Left (map diagCode ds)))
    Right (prog, scopes) -> case elaborateProgram prog scopes of
      Left ds -> pure (Left (Left (map diagCode ds)))
      Right cp -> do
        ctx <- mkRtCtx cp "" (RtHooks mockRunner mockFileRunner (\_ env body -> body env) noLogSink noAsyncTracker (fmap (fmap T.pack) . lookupEnv . T.unpack))
        out <- try $ do
          v <- topValue ctx ("main.lask", "example")
          case v of
            VClosure _ -> applyValue ctx v [] []
            _ -> pure v
        pure $ case out of
          Right v -> Right (encodeValue v)
          Left lf -> Left (Right (lfCode lf))

-- | Whether the outcome is what the example says; the failure message
-- when it is not.
check :: Outcome -> Either (Either [ErrorCode] (Maybe ErrorCode)) Text -> Maybe String
check want got = case (want, got) of
  (Is v, Right v') | v == v' -> Nothing
  (Holds, Right "true") -> Nothing
  (Fails c, Left (Right (Just c'))) | c == c' -> Nothing
  _ -> Just ("expected " <> shown want <> ", got " <> either (either (("static " <>) . show) (("failure " <>) . show)) T.unpack got)
  where
    shown o = case o of
      Is v -> T.unpack v
      Holds -> "true"
      Fails c -> "failure " <> show c

-- | The identifiers an expression mentions.
identifiers :: Text -> Set.Set Text
identifiers = Set.fromList . filter (not . T.null) . T.split (\c -> not (isAlphaNum c || c == '_'))

spec :: Spec
spec = do
  let covered = Set.fromList (map exBuiltin examples)
  describe "the table of built-ins (spec 15)" $ do
    it "has an example for every built-in" $
      Set.toList (builtinValueNames `Set.difference` covered) `shouldBe` []
    it "names only built-ins" $
      Set.toList (covered `Set.difference` builtinValueNames) `shouldBe` []
    it "lists the same built-ins as the signatures do" $
      Map.keysSet builtinSchemes `shouldBe` builtinValueNames
    it "calls the built-in each example is for" $
      [(exBuiltin e, exExpr e) | e <- examples, exBuiltin e `Set.notMember` identifiers (exExpr e)] `shouldBe` []

  describe "built-ins (spec 15)" $
    forM_ examples $ \e ->
      it (T.unpack (exBuiltin e <> ": " <> T.replace "\n" " " (exExpr e))) $ do
        got <- evaluate (exExpr e)
        maybe (pure ()) expectationFailure (check (exOutcome e) got)
