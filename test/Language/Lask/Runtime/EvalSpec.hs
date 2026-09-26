{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EvalSpec (spec) where

import Control.Exception (try)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner, RtHooks (..))
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.Elaborate (elaborateProgram)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Secrets (maskSecrets, resetSecretRegistryForTests)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue)
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

-- | Mock command runner: no real processes in unit tests.
mockRunner :: CommandRunner
mockRunner _env cmd
  | cmd == "boom" = pure (Right (7, "", "kaboom"))
  | Just rest <- T.stripPrefix "echo " cmd = pure (Right (0, rest <> "\n", ""))
  | cmd == "unreachable" = pure (Left (ioFailure EIoEnvResolve "cannot resolve environment"))
  | otherwise = pure (Right (0, "", ""))

-- | Mock filesystem runner: the unit tests exercise dispatch and
-- typing of the built-ins of 15.11, not the real filesystem, which
-- "Language.Lask.Runtime.EnvironmentSpec" covers against a temp dir.
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

-- | Compile main.lask, evaluate declaration @name@; a closure result
-- is applied to zero arguments. Result is canonical JSON.
run :: Text -> Text -> IO (Either (Maybe ErrorCode, Text) Text)
run src name = do
  r <- loadProgramWith reader "main.lask"
  case r of
    Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
    Right prog -> case validateProgram prog of
      Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
      Right scopes -> case elaborateProgram prog scopes of
        Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
        Right cp -> do
          ctx <- mkRtCtx cp "in-data\n" (RtHooks mockRunner mockFileRunner noLogSink noAsyncTracker)
          out <- try $ do
            v <- topValue ctx ("main.lask", name)
            case v of
              VClosure _ -> applyValue ctx v [] []
              _ -> pure v
          pure $ case out of
            Right v -> Right (encodeValue v)
            Left lf -> Left (lfCode lf, encodeValue (lfError lf))
  where
    reader p = pure (if p == "main.lask" then Right src else Left "not found")

evalsTo :: Text -> Text -> Text -> Expectation
evalsTo src name expected = run src name >>= (`shouldBe` Right expected)

-- | Rejected before evaluation: 'run' reports static diagnostics with
-- no runtime error code.
failsToCompile :: Text -> Text -> Expectation
failsToCompile src name = do
  r <- run src name
  case r of
    Left (Nothing, _) -> pure ()
    other -> expectationFailure ("expected a static rejection, got " <> show other)

failsWith :: Text -> Text -> ErrorCode -> Expectation
failsWith src name code = do
  r <- run src name
  case r of
    Left (Just c, _) | c == code -> pure ()
    other -> expectationFailure ("expected " <> show code <> ", got " <> show other)

spec :: Spec
spec = do
  describe "literals and operators (spec 8.1)" $ do
    it "evaluates arithmetic with precedence" $
      evalsTo "f() = 1 + 2 * 3" "f" "7"
    it "formats integer results without a fraction" $
      evalsTo "x = 10 / 4" "x" "2.5"
    it "evaluates comparisons and logic with short-circuit" $
      evalsTo "f(): Bool = 1 < 2 && !(2 == 3)" "f" "true"
    it "short-circuits && without evaluating the right side" $
      evalsTo "f(): Bool = false && get({\"a\": true}, \"missing\")" "f" "false"
    it "compares structurally" $
      evalsTo "f() = {a: [1, 2]} == {a: [1, 2]}" "f" "true"
    it "compares environments structurally (spec 8.8)" $ do
      evalsTo "f() = #alpine:3.12 == #docker(\"alpine:3.12\")" "f" "true"
      evalsTo "f() = #local == #alpine:3.12" "f" "false"

  describe "strings" $ do
    it "interpolates expressions" $
      evalsTo "f() = \"v=#{1 + 1}!\"" "f" "\"v=2!\""
    it "runs builtins: split/join/replace/to_upper" $
      evalsTo "f() = join(map(split(\"a-b\", \"-\"), \\(s: String) -> to_upper(s)), \"+\")" "f" "\"A+B\""
    it "counts length in characters" $
      evalsTo "f() = length(\"あいう\")" "f" "3"

  describe "functions and scoping (spec 8.2, 8.3)" $ do
    it "captures the definition environment in closures" $
      evalsTo
        "mk(n: Number): Function<Number, Number> = \\(x: Number) -> x + n\nf() = mk(10)(5)"
        "f"
        "15"
    it "fills keyword defaults at call time" $
      evalsTo
        "greet(name: String, --prefix: String = \"hello\"): String = concat(prefix, concat(\", \", name))\nf() = greet(\"alice\")"
        "f"
        "\"hello, alice\""
    it "binds keyword arguments by name" $
      evalsTo
        "greet(name: String, --prefix: String = \"hello\"): String = concat(prefix, name)\nf() = greet(\"a\", prefix = \"hi:\")"
        "f"
        "\"hi:a\""
    it "evaluates defaults in the scope of preceding parameters" $
      evalsTo "g(a: Number, --b: Number = a) = a + b\nf() = g(3)" "f" "6"
    it "collects variadic arguments" $
      evalsTo
        "s(...xs: Array<Number>): Number = reduce(xs, 0, \\(a: Number, x: Number) -> a + x)\nf() = s(1, 2, 3)"
        "f"
        "6"
    it "binds empty variadic to an empty array" $
      evalsTo "s(...xs: Array<Number>): Number = reduce(xs, 100, \\(a: Number, x: Number) -> a + x)\nf() = s()" "f" "100"
    it "pipes and composition" $
      evalsTo "inc(x: Number): Number = x + 1\ndouble(x: Number): Number = x * 2\np = inc >> double\nf() = 3 |> p" "f" "8"

  describe "control (spec 8.4, 8.5)" $ do
    it "evaluates do blocks sequentially" $
      evalsTo "f() = do {\n  a = 1\n  b = a + 1\n  a + b\n}" "f" "3"
    it "evaluates only the selected branch" $
      evalsTo "f() = if (true) { 1 } else { get({\"a\": 2}, \"missing\") }" "f" "1"
    it "evaluates only the selected case arm (spec 6.4)" $
      evalsTo "f() = case (1) {\n  1 -> 1\n  else -> get({\"a\": 2}, \"missing\")\n}" "f" "1"
    it "stops testing heads at the first match" $
      evalsTo "xs = [9]\nf() = case (1) {\n  1 -> \"a\"\n  xs[5] -> \"b\"\n  else -> \"c\"\n}" "f" "\"a\""
    it "matches any of an arm's heads" $
      evalsTo "f(s: String) = case (s) {\n  \"a\", \"b\" -> 1\n  else -> 2\n}\ng() = f(\"b\")" "g" "1"
    it "falls through to the else arm" $
      evalsTo "f(s: String) = case (s) {\n  \"a\" -> 1\n  else -> 2\n}\ng() = f(\"z\")" "g" "2"
    it "dispatches on the value of a command scrutinee" $
      -- The command string runs to the end of the line (spec 6.6), so
      -- the scrutinee is bound first.
      evalsTo
        "f() = do {\n  r = $[#local] echo one\n  case (r) {\n    \"two\\n\" -> 2\n    \"one\\n\" -> 1\n    else -> 0\n  }\n}"
        "f"
        "1"
    it "takes the first true arm of the condition form" $
      evalsTo "f(n: Number) = case {\n  n >= 500 -> \"error\"\n  n >= 400 -> \"warn\"\n  else -> \"info\"\n}\ng() = f(404)" "g" "\"warn\""
    it "maps for expressions in order" $
      evalsTo "f() = for (x : [1, 2, 3]) { x * 2 }" "f" "[2,4,6]"
    it "evaluates early returns" $
      evalsTo
        "p(tag: String): String = do {\n  if (tag == \"\") { return \"skip\" }\n  \"released\"\n}\nf() = p(\"\")"
        "f"
        "\"skip\""
    it "continues past a guard whose condition is false" $
      evalsTo
        "p(tag: String): String = do {\n  if (tag == \"\") { return \"skip\" }\n  \"released\"\n}\nf() = p(\"v1\")"
        "f"
        "\"released\""

  describe "collections" $ do
    it "indexes arrays and records" $
      evalsTo "u = {name: \"a\", tags: [1, 2]}\nf() = u.tags[1]" "f" "2"
    it "fails on out-of-range indexes" $
      failsWith "xs = [1]\nf() = xs[5]" "f" ERuntimeAccess
    it "fails on missing map keys" $
      failsWith "m: Map<Number> = {\"a\": 1}\nf() = m[\"b\"]" "f" ERuntimeAccess
    it "has_key guards get" $
      evalsTo "m: Map<Number> = {\"a\": 1}\nf() = if (has_key(m, \"b\")) { get(m, \"b\") } else { 0 }" "f" "0"
    it "appends and concatenates arrays" $
      evalsTo "f() = concat_array(append([1], 2), [3])" "f" "[1,2,3]"

  describe "numeric edge cases" $ do
    it "fails division by zero" $
      failsWith "f() = 1 / 0" "f" ERuntimeDivByZero
    it "fails div builtin by zero" $
      failsWith "f() = div(1, 0)" "f" ERuntimeDivByZero
    it "computes mod, floor, ceil, abs" $
      evalsTo "f() = [mod(7, 3), floor(1.5), ceil(1.2), abs(0 - 4)]" "f" "[1,1,2,4]"

  describe "errors (spec 8.10, 15.7)" $ do
    it "catches user failures with try/catch" $
      evalsTo "f() = try { fail({code: 7, message: \"x\"}) } catch (e) { e.code }" "f" "7"
    it "does not run the handler on success" $
      evalsTo "f() = try { 1 } catch (e) { 99 }" "f" "1"
    it "propagates uncaught failures" $
      failsWith "f() = 1 / 0" "f" ERuntimeDivByZero
    it "runs finally on both paths and keeps the body value" $
      evalsTo "f() = try { 1 } finally { 2 }" "f" "1"
    it "rethrows after finally" $
      evalsTo
        "f() = try { try { fail(error(5, \"m\")) } finally { 0 } } catch (e) { e.code }"
        "f"
        "5"
    it "constructs errors with error()" $
      evalsTo "f() = error(75, \"later\")" "f" "{\"code\":75,\"message\":\"later\"}"
    it "catches inner failures innermost-first" $
      evalsTo
        "f() = try { try { fail(error(1, \"a\")) } catch (e) { fail(error(2, \"b\")) } } catch (e) { e.code }"
        "f"
        "2"

  describe "commands (spec 8.7, via mock runner)" $ do
    it "returns stdout for $ on success" $
      evalsTo "f() = $[#local] echo hi" "f" "\"hi\\n\""
    it "returns the whole result for $*" $
      evalsTo "f() = $*[#local] boom" "f" "{\"code\":7,\"stderr\":\"kaboom\",\"stdout\":\"\"}"
    it "fails with the exit code and stderr for $ on non-zero" $
      -- The command string runs to the end of the line (spec 6.6), so
      -- the try block must span multiple lines.
      evalsTo
        "f() = try {\n  $[#local] boom\n} catch (e) {\n  e.message\n}"
        "f"
        "\"kaboom\""
    it "exposes the command exit code to catch" $
      evalsTo
        "f() = do {\n  r = try {\n    $[#local] boom\n  } catch (e) {\n    concat(\"code=\", \"?\")\n  }\n  r\n}"
        "f"
        "\"code=?\""
    it "command failures carry the exit code" $
      evalsTo
        "g(): Number = do {\n  r = $*[#local] boom\n  r.code\n}\nf() = g()"
        "f"
        "7"
    it "interpolates into command strings" $
      evalsTo "n = \"world\"\nf() = $[#local] echo hello #{n}" "f" "\"hello world\\n\""
    it "propagates infrastructure failures" $
      failsWith "f() = $[#local] unreachable" "f" EIoEnvResolve

  describe "async (spec 8.6)" $ do
    it "awaits spawned computations" $
      evalsTo "f() = do {\n  h = async 1\n  await h\n}" "f" "1"
    it "runs all and preserves input order" $
      evalsTo "f() = all([async 1, async 2, async 3])" "f" "[1,2,3]"
    it "rethrows failures at await" $
      evalsTo
        "f() = do {\n  h = async (1 / 0)\n  try { await h } catch (e) { e.code }\n}"
        "f"
        "2"
    it "repeated await returns the same result" $
      evalsTo "f() = do {\n  h = async 21\n  (await h) + (await h)\n}" "f" "42"
    it "rethrows the failure unchanged, with its own code (spec 6.3, 15.6)" $ do
      failsWith "f() = do {\n  h = async (1 / 0)\n  await h\n}" "f" ERuntimeDivByZero
      failsWith "f() = all([async 1, async (1 / 0)])" "f" ERuntimeDivByZero
      evalsTo
        "f() = do {\n  h = async $[#local] boom\n  try {\n    out = await h\n    0\n  } catch (e) {\n    e.code\n  }\n}"
        "f"
        "7"
    it "fails race on an empty array as an argument outside its domain" $
      failsWith "hs: Array<AsyncHandle<Number>> = []\nf() = race(hs)" "f" ERuntimeValue

  describe "serialization and cast (spec 13, 15.8)" $ do
    it "encodes records to JSON" $
      evalsTo "f() = to_json({b: 1, a: \"x\"})" "f" "\"{\\\"a\\\":\\\"x\\\",\\\"b\\\":1}\""
    it "decodes and casts JSON" $
      -- do-block bindings cannot carry annotations (spec 6.5), so
      -- cast obtains its context type from a parameter position.
      evalsTo
        "pick(r: Record<a: Number>): Number = r.a\nf() = pick(cast(from_json(\"{\\\"a\\\": 1}\")))"
        "f"
        "1"
    it "fails cast on mismatching data" $
      failsWith
        "pick(r: Record<a: Number>): Number = r.a\nf() = pick(cast(from_json(\"{\\\"a\\\": \\\"s\\\"}\")))"
        "f"
        ERuntimeCast
    -- Issue #28 split this message so the CLI can write its own
    -- lead-in before the same tail (spec 11.2). The `cast failed`
    -- lead-in belongs to this side alone, so it is pinned here.
    it "names the path and the two types in the cast failure message" $ do
      r <-
        run
          "pick(r: Record<a: Number>): Number = r.a\nf() = pick(cast(from_json(\"{\\\"a\\\": \\\"s\\\"}\")))"
          "f"
      r
        `shouldBe` Left
          ( Just ERuntimeCast,
            "{\"code\":2,\"message\":\"cast failed at a: expected Number, got String\"}"
          )
    it "casts records to maps" $
      evalsTo
        "v: Any = {a: 1, b: 2}\nm: Map<Number> = cast(v)\nf() = get(m, \"a\")"
        "f"
        "1"
    it "fails on invalid JSON with E-IO-DATA-DECODE" $
      failsWith "f() = from_json(\"{oops\")" "f" EIoDataDecode
    it "serializes Void as tagged metadata" $
      evalsTo "f() = for (x : []) {}" "f" "{\"$type\":\"Void\"}"
    it "serializes environments as tagged metadata" $
      evalsTo "f() = #alpine:3.20" "f" "{\"$type\":\"Environment\",\"kind\":\"docker\",\"params\":{\"image\":\"alpine:3.20\"}}"

  describe "stdin (spec 9.3)" $ do
    it "exposes stdin as a String" $
      evalsTo "f() = trim(stdin)" "f" "\"in-data\""

  describe "secret bindings (spec 6.10)" $ do
    -- The registry is process-global, so each case clears it and
    -- checks masking through the same path the command log uses.
    let registeredAfter src name = do
          resetSecretRegistryForTests
          r <- run src name
          masked <- maskSecrets "value=s3cr3t-value"
          resetSecretRegistryForTests
          pure (r, masked)
        masksAfterEval src name = do
          (r, masked) <- registeredAfter src name
          r `shouldSatisfy` isRight
          masked `shouldBe` "value=***"

    it "registers a !!-marked value declaration" $
      masksAfterEval "a!!: String = \"s3cr3t-value\"\nf() = a" "f"

    it "registers a !!-marked bind statement" $
      masksAfterEval "f() = do { a!! = \"s3cr3t-value\"\n  a }" "f"

    it "registers a !!-marked positional parameter" $
      masksAfterEval "g(x!!: String): String = x\nf() = g(\"s3cr3t-value\")" "f"

    it "registers a !!-marked keyword parameter's default" $
      masksAfterEval "g(--x!!: String = \"s3cr3t-value\"): String = x\nf() = g()" "f"

    it "registers a !!-marked keyword parameter's caller-supplied value" $
      masksAfterEval "g(--x!!: String = \"unused-default\"): String = x\nf() = g(x = \"s3cr3t-value\")" "f"

    it "leaves the value itself untouched for the running program" $
      evalsTo "g(x!!: String): String = x\nf() = g(\"s3cr3t-value\")" "f" "\"s3cr3t-value\""

    it "does not register anything without the marker" $ do
      (r, masked) <- registeredAfter "a: String = \"s3cr3t-value\"\nf() = a" "f"
      r `shouldSatisfy` isRight
      masked `shouldBe` "value=s3cr3t-value"

    it "registers an explicit mark_secret call" $
      masksAfterEval "f() = mark_secret(\"s3cr3t-value\")" "f"

    it "registers a String | Null secret that is given a value" $
      masksAfterEval
        "g(--x!!: String | Null = null): String | Null = x\nf() = g(x = \"s3cr3t-value\")"
        "f"

    -- An absent secret has no text: in particular the text null prints
    -- as must not become a masked word everywhere in a log.
    it "registers nothing for a secret that is null" $ do
      resetSecretRegistryForTests
      r <- run "g(--x!!: String | Null = null): String | Null = x\nf() = g()" "f"
      masked <- maskSecrets "value=null"
      resetSecretRegistryForTests
      r `shouldSatisfy` isRight
      masked `shouldBe` "value=null"

    it "hands a null secret back as null" $
      evalsTo "f(): String | Null = mark_secret(find_env(\"LASK_TEST_SURELY_UNSET\"))" "f" "null"

    it "does not register values merely because get_env returned them" $ do
      -- Masking is opt-in (spec 12.8): a region or log level read from
      -- the environment must stay readable in logs.
      setEnv "LASK_TEST_SECRET_VAR" "s3cr3t-value"
      (r, masked) <- registeredAfter "f() = get_env(\"LASK_TEST_SECRET_VAR\")" "f"
      unsetEnv "LASK_TEST_SECRET_VAR"
      r `shouldBe` Right "\"s3cr3t-value\""
      masked `shouldBe` "value=s3cr3t-value"

    it "masks a get_env value once it is bound to a !!-marked name" $ do
      setEnv "LASK_TEST_SECRET_VAR" "s3cr3t-value"
      out <- masksAfterEval "f() = do { k!! = get_env(\"LASK_TEST_SECRET_VAR\")\n  k }" "f"
      unsetEnv "LASK_TEST_SECRET_VAR"
      pure out

  describe "filesystem functions (spec 15.11)" $ do
    it "reads a file from the environment it names" $
      evalsTo "f() = read_file(\"greeting.txt\", #local)" "f" "\"hello\\n\""
    it "returns Void from write_file (spec 13.1 tags it)" $
      evalsTo "f() = write_file(\"out.txt\", \"x\", #local)" "f" "{\"$type\":\"Void\"}"
    it "answers file_exists as a Bool" $
      evalsTo "f() = file_exists(\"absent.txt\", #local)" "f" "false"
    it "returns list_dir and glob as arrays of String" $ do
      evalsTo "f() = list_dir(\".\", #local)" "f" "[\"a.txt\",\"b.txt\"]"
      evalsTo "f() = glob(\"src/**/*.lask\", #local)" "f" "[\"src/main.lask\"]"
    it "propagates a filesystem failure as E-IO-FS" $
      failsWith "f() = read_file(\"absent.txt\", #local)" "f" EIoFs
    it "is catchable like any other failure (spec 6.9)" $
      evalsTo
        "f() = try { read_file(\"absent.txt\", #local) } catch (e) { \"fallback\" }"
        "f"
        "\"fallback\""
    it "requires the environment argument (spec 15.11)" $
      failsToCompile "f() = read_file(\"greeting.txt\")" "f"
    it "rejects a non-Environment second argument" $
      failsToCompile "f() = read_file(\"greeting.txt\", \"local\")" "f"

  describe "numeric additions (spec 15.2)" $ do
    it "compares, sums and raises" $ do
      evalsTo "f() = min(3, 1) + max(2, 5)" "f" "6"
      evalsTo "f() = sum([1, 2, 3])" "f" "6"
      evalsTo "f() = sum([])" "f" "0"
      evalsTo "f() = pow(2, 10)" "f" "1024"
      evalsTo "f() = sqrt(9)" "f" "3"
    it "clamps into the given bounds" $ do
      evalsTo "f() = clamp(5, 0, 3)" "f" "3"
      evalsTo "f() = clamp(0 - 1, 0, 3)" "f" "0"
    it "rejects arguments outside the domain with E-RUNTIME-VALUE" $ do
      failsWith "f() = sqrt(0 - 1)" "f" ERuntimeValue
      failsWith "f() = clamp(1, 3, 0)" "f" ERuntimeValue

  describe "string additions (spec 15.3)" $ do
    it "searches without a substring hack" $ do
      evalsTo "f() = contains(\"abc\", \"b\")" "f" "true"
      evalsTo "f() = starts_with(\"abc\", \"ab\")" "f" "true"
      evalsTo "f() = ends_with(\"abc\", \"bc\")" "f" "true"
      evalsTo "f() = index_of(\"abcb\", \"b\")" "f" "1"
    it "reports an absent needle as -1 rather than failing" $
      evalsTo "f() = index_of(\"abc\", \"z\")" "f" "-1"
    it "slices with clamped endpoints and never fails" $ do
      evalsTo "f() = substring(\"abcdef\", 1, 3)" "f" "\"bc\""
      evalsTo "f() = substring(\"abc\", 0, 99)" "f" "\"abc\""
      evalsTo "f() = substring(\"abc\", 2, 1)" "f" "\"\""
    it "pads and repeats" $ do
      evalsTo "f() = pad_start(\"7\", 3, \"0\")" "f" "\"007\""
      evalsTo "f() = pad_end(\"7\", 3, \".\")" "f" "\"7..\""
      evalsTo "f() = repeat(\"ab\", 3)" "f" "\"ababab\""
      evalsTo "f() = repeat(\"ab\", 0)" "f" "\"\""
    it "splits command output into lines, CRLF included" $ do
      evalsTo "f() = lines(\"a\\nb\\n\")" "f" "[\"a\",\"b\"]"
      evalsTo "f() = lines(\"a\\r\\nb\")" "f" "[\"a\",\"b\"]"
      evalsTo "f() = lines(\"\")" "f" "[]"
    it "converts between String and Number" $ do
      evalsTo "f() = to_string(42)" "f" "\"42\""
      evalsTo "f() = to_string(true)" "f" "\"true\""
      evalsTo "f() = to_number(\" 42 \")" "f" "42"
    it "reports unrenderable and unparsable values" $ do
      -- An argument whose type is known not to be stringifiable is
      -- rejected at the call site (spec 15.3); only Any reaches the
      -- runtime check.
      failsToCompile "f() = to_string([1])" "f"
      failsWith "f() = to_string(from_json(\"[1]\"))" "f" ERuntimeValue
      failsWith "f() = to_number(\"seven\")" "f" EIoDataDecode

  describe "regular expressions (spec 15.3)" $ do
    it "tests, captures and replaces" $ do
      evalsTo "f() = regex_test(\"go1.22\", \"[0-9]+\")" "f" "true"
      evalsTo "f() = regex_match(\"v1.22\", \"v([0-9]+)\")" "f" "[\"v1\",\"1\"]"
      evalsTo "f() = regex_replace(\"a1b2\", \"[0-9]\", \"-\")" "f" "\"a-b-\""
    it "expands group references in the replacement" $
      evalsTo "f() = regex_replace(\"k=v\", \"([a-z]+)=([a-z]+)\", \"$2=$1\")" "f" "\"v=k\""
    it "returns an empty array for no match, not a null" $
      evalsTo "f() = regex_match(\"abc\", \"[0-9]+\")" "f" "[]"
    it "supports the escapes POSIX lacks" $
      evalsTo "f() = regex_match(\"x42\", '(\\d+)')" "f" "[\"42\",\"42\"]"
    it "reports a malformed pattern as E-RUNTIME-REGEX" $
      failsWith "f() = regex_test(\"a\", \"(\")" "f" ERuntimeRegex

  describe "array additions (spec 15.4)" $ do
    it "measures without folding by hand" $ do
      evalsTo "f() = size([1, 2, 3])" "f" "3"
      evalsTo "f() = is_empty([])" "f" "true"
    it "takes ends, and fails on an empty array like an index would" $ do
      evalsTo "f() = first([1, 2])" "f" "1"
      evalsTo "f() = last([1, 2])" "f" "2"
      failsWith "f() = first([])" "f" ERuntimeAccess
    it "keeps argument order when one argument was checked out of order" $
      -- append(array, element): the deferred first argument has to end
      -- up first, or the call would receive its arguments swapped.
      evalsTo "f(): Array<Number> = append(cast(from_json(\"[1]\")), 2)" "f" "[1,2]"
    it "returns the element it found, or null (15.1)" $ do
      evalsTo "f(): String | Null = find([\"a\", \"bb\"], \\(s: String) -> length(s) == 2)" "f" "\"bb\""
      evalsTo "f(): String | Null = find([\"a\"], \\(s: String) -> length(s) == 9)" "f" "null"
    it "slices, takes, drops and reverses with clamping" $ do
      evalsTo "f() = slice([1, 2, 3, 4], 1, 3)" "f" "[2,3]"
      evalsTo "f() = take([1, 2, 3], 99)" "f" "[1,2,3]"
      evalsTo "f() = drop([1, 2, 3], 1)" "f" "[2,3]"
      evalsTo "f() = reverse([1, 2, 3])" "f" "[3,2,1]"
    it "sorts numbers and strings" $ do
      evalsTo "f() = sort([3, 1, 2])" "f" "[1,2,3]"
      evalsTo "f() = sort([\"b\", \"a\"])" "f" "[\"a\",\"b\"]"
    it "sorts by a key, stably" $
      evalsTo
        "f() = sort_by([{n: \"b\", k: 1}, {n: \"a\", k: 1}], \\(x: Record<n: String, k: Number>) -> x.k)"
        "f"
        "[{\"k\":1,\"n\":\"b\"},{\"k\":1,\"n\":\"a\"}]"
    it "searches by value and by predicate" $ do
      evalsTo "f() = contains_array([1, 2], 2)" "f" "true"
      evalsTo "f() = index_of_array([1, 2], 3)" "f" "-1"
      evalsTo "f() = find_index([1, 2, 3], \\(x: Number) -> x > 1)" "f" "1"
      evalsTo "f() = find_index([1], \\(x: Number) -> x > 9)" "f" "-1"
    it "quantifies, with the empty-array conventions" $ do
      evalsTo "f() = every([2, 4], \\(x: Number) -> mod(x, 2) == 0)" "f" "true"
      evalsTo "f() = any([1, 3], \\(x: Number) -> mod(x, 2) == 0)" "f" "false"
      evalsTo "xs: Array<Number> = []\nf() = every(xs, \\(x: Number) -> false)" "f" "true"
      evalsTo "xs: Array<Number> = []\nf() = any(xs, \\(x: Number) -> true)" "f" "false"
    it "reshapes" $ do
      evalsTo "f() = flatten([[1], [2, 3]])" "f" "[1,2,3]"
      evalsTo "f() = flat_map([1, 2], \\(x: Number) -> [x, x])" "f" "[1,1,2,2]"
      evalsTo "f() = unique([1, 2, 1])" "f" "[1,2]"
      evalsTo "f() = zip([1], [\"a\"])" "f" "[{\"first\":1,\"second\":\"a\"}]"
    it "counts and indexes, which for has no form for" $ do
      evalsTo "f() = range(0, 3)" "f" "[0,1,2]"
      evalsTo "f() = range(2, 2)" "f" "[]"
      evalsTo "f() = enumerate([\"a\"])" "f" "[{\"index\":0,\"value\":\"a\"}]"
    it "rejects an element type that cannot be ordered, statically" $
      failsToCompile "f() = sort([true, false])" "f"
    it "rejects an element type that cannot be compared, statically" $
      failsToCompile "f() = unique([\\(x: Number) -> x])" "f"

  describe "map additions (spec 15.4)" $ do
    it "updates without mutating" $ do
      evalsTo "m: Map<Number> = {\"a\": 1}\nf() = set(m, \"b\", 2)" "f" "{\"a\":1,\"b\":2}"
      evalsTo "m: Map<Number> = {\"a\": 1}\nf() = remove(m, \"a\")" "f" "{}"
      evalsTo "m: Map<Number> = {\"a\": 1}\nf() = m" "f" "{\"a\":1}"
    it "merges with the right operand winning" $
      evalsTo
        "a: Map<Number> = {\"k\": 1}\nb: Map<Number> = {\"k\": 2}\nf() = merge(a, b)"
        "f"
        "{\"k\":2}"
    it "reads a missing key without failing" $
      evalsTo "m: Map<Number> = {}\nf() = get_or(m, \"absent\", 0)" "f" "0"
    it "moves between a map and an array of entries" $ do
      evalsTo "m: Map<Number> = {\"a\": 1}\nf() = entries(m)" "f" "[{\"key\":\"a\",\"value\":1}]"
      evalsTo
        "es: Array<Record<key: String, value: Number>> = [{key: \"a\", value: 1}]\nf(): Map<Number> = from_entries(es)"
        "f"
        "{\"a\":1}"
    it "maps the values and keeps the keys" $
      evalsTo
        "m: Map<Number> = {\"a\": 1}\nf() = map_values(m, \\(v: Number) -> v + 1)"
        "f"
        "{\"a\":2}"

  describe "shell quoting (spec 15.5)" $
    it "wraps a value as one shell word" $ do
      evalsTo "f() = shell_quote(\"a b\")" "f" "\"'a b'\""
      evalsTo "f() = shell_quote(\"it's\")" "f" "\"'it'\\\\''s'\""

  describe "encodings and digests (spec 15.8)" $ do
    it "round-trips base64" $ do
      evalsTo "f() = base64_encode(\"hello\")" "f" "\"aGVsbG8=\""
      evalsTo "f() = base64_decode(\"aGVsbG8=\")" "f" "\"hello\""
      evalsTo "f() = base64_decode(\"aGVsbG8\")" "f" "\"hello\""
    it "rejects input that is not base64" $
      failsWith "f() = base64_decode(\"!!!\")" "f" EIoDataDecode
    it "digests as lowercase hex" $ do
      evalsTo "f() = sha256(\"abc\")" "f" "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\""
      evalsTo "f() = md5(\"abc\")" "f" "\"900150983cd24fb0d6963f7d28e17f72\""
    it "decodes the added formats" $ do
      evalsTo "f(): Map<String> = cast(decode(\"A=1\\n\", \"dotenv\"))" "f" "{\"A\":\"1\"}"
      evalsTo "f(): Record<name: String> = cast(decode(\"name: lask\\n\", \"yaml\"))" "f" "{\"name\":\"lask\"}"
      evalsTo "f(): Record<name: String> = cast(decode('name = \"lask\"', \"toml\"))" "f" "{\"name\":\"lask\"}"

  describe "environment access (spec 15.9)" $ do
    it "answers whether a variable is set" $ do
      setEnv "LASK_TEST_PRESENT" "x"
      unsetEnv "LASK_TEST_ABSENT"
      evalsTo "f() = has_env(\"LASK_TEST_PRESENT\")" "f" "true"
      evalsTo "f() = has_env(\"LASK_TEST_ABSENT\")" "f" "false"
    it "falls back without ever yielding a null" $ do
      unsetEnv "LASK_TEST_ABSENT"
      evalsTo "f() = get_env_or(\"LASK_TEST_ABSENT\", \"fallback\")" "f" "\"fallback\""
    it "reads a variable it presupposes is set" $ do
      setEnv "LASK_TEST_PRESENT" "x"
      evalsTo "f() = get_env(\"LASK_TEST_PRESENT\")" "f" "\"x\""
    it "fails rather than yielding a null when get_env finds nothing (15.1)" $ do
      unsetEnv "LASK_TEST_ABSENT"
      failsWith "f() = get_env(\"LASK_TEST_ABSENT\")" "f" ERuntimeAccess
    it "returns the absent case as a value from find_env" $ do
      setEnv "LASK_TEST_PRESENT" "x"
      unsetEnv "LASK_TEST_ABSENT"
      evalsTo "f(): String | Null = find_env(\"LASK_TEST_PRESENT\")" "f" "\"x\""
      evalsTo "f(): String | Null = find_env(\"LASK_TEST_ABSENT\")" "f" "null"
      evalsTo
        "f(): String = do {\n  v = find_env(\"LASK_TEST_ABSENT\")\n  case (v) {\n    Null -> \"fallback\"\n    else -> v\n  }\n}"
        "f"
        "\"fallback\""

  describe "union types and type dispatch (spec 4.2, 6.4)" $ do
    it "selects the arm whose type the value has" $ do
      evalsTo "f(v: Any): String = case (v) {\n  String -> \"s\"\n  Number -> \"n\"\n  else -> \"?\"\n}\ng() = f(1)" "g" "\"n\""
      evalsTo "f(v: Any): String = case (v) {\n  String -> \"s\"\n  Number -> \"n\"\n  else -> \"?\"\n}\ng() = f(true)" "g" "\"?\""
    it "tests an element type through the whole array" $
      evalsTo
        "f(v: Any): String = case (v) {\n  Array<String> -> \"strings\"\n  else -> \"other\"\n}\ng() = f(from_json(\"[1]\"))"
        "g"
        "\"other\""
    it "takes the first arm that matches" $
      evalsTo
        "f(v: Any): String = case (v) {\n  Any -> \"first\"\n  Number -> \"second\"\n  else -> \"?\"\n}\ng() = f(1)"
        "g"
        "\"first\""
    it "converts a record narrowed to a map, so the body's type holds" $
      evalsTo
        "f(v: Any): Number = case (v) {\n  Map<Number> -> size(keys(v))\n  else -> 0\n}\ng() = f(from_json(\"{\\\"a\\\": 1, \\\"b\\\": 2}\"))"
        "g"
        "2"
    it "evaluates only the selected arm's body" $
      evalsTo
        "f(v: Any): Number = case (v) {\n  Number -> 1\n  else -> get({\"a\": 2}, \"missing\")\n}\ng() = f(5)"
        "g"
        "1"
    it "casts into and out of a union (spec 15.8)" $ do
      evalsTo "f(): String | Null = cast(from_json(\"null\"))" "f" "null"
      failsWith "f(v: String | Null): String = cast(v)\ng() = f(null)" "g" ERuntimeCast
    it "serializes a union as the member the value is (spec 13.1)" $
      evalsTo "f(): Array<String | Null> = [\"a\", null]" "f" "[\"a\",null]"

  describe "optional record fields (spec 4.2)" $ do
    it "casts a value whose optional key is absent, and not one whose required key is" $ do
      evalsTo
        "f(): String = do {\n  r: Record<a: String, b?: String> = cast(from_json(\"{\\\"a\\\": \\\"x\\\"}\"))\n  r.a\n}"
        "f"
        "\"x\""
      failsWith
        "f(): String = do {\n  r: Record<a: String, b?: String> = cast(from_json(\"{\\\"b\\\": \\\"y\\\"}\"))\n  r.a\n}"
        "f"
        ERuntimeCast
    it "rejects a null in a field that is optional but not nullable" $ do
      failsWith
        "f(): String = do {\n  r: Record<a: String, b?: String> = cast(from_json(\"{\\\"a\\\": \\\"x\\\", \\\"b\\\": null}\"))\n  r.a\n}"
        "f"
        ERuntimeCast
      evalsTo
        "f(): String = do {\n  r: Record<a: String, b?: String | Null> = cast(from_json(\"{\\\"a\\\": \\\"x\\\", \\\"b\\\": null}\"))\n  r.a\n}"
        "f"
        "\"x\""
    it "reads an absent optional key as null" $
      evalsTo
        "f(): String = do {\n  r: Record<a: String, b?: String> = cast(from_json(\"{\\\"a\\\": \\\"x\\\"}\"))\n  b = r.b\n  case (b) {\n    Null -> \"absent\"\n    else -> b\n  }\n}"
        "f"
        "\"absent\""
    it "omits an absent optional field from the output, and writes a null (spec 13.1)" $ do
      evalsTo "f(): Record<a: Number, b?: String> = {a: 1}" "f" "{\"a\":1}"
      evalsTo "f(): Record<a: Number, b?: String | Null> = {a: 1, b: null}" "f" "{\"a\":1,\"b\":null}"

  describe "user type parameters (spec 4.2, 4.4)" $ do
    it "runs one body at every instantiation" $ do
      evalsTo
        "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\ng() = first_or([1, 2], 0)"
        "g"
        "1"
      evalsTo
        "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\ng() = first_or([], \"none\")"
        "g"
        "\"none\""
    it "keeps keyword defaults and variadic collection" $ do
      evalsTo
        "tag<T>(x: T, --label: String = \"v\"): String = concat(label, to_json(x))\ng() = tag(1)"
        "g"
        "\"v1\""
      evalsTo "listy<T>(...xs: Array<T>): Number = size(xs)\ng() = listy(1, 2, 3)" "g" "3"
    it "evaluates a parameterised alias like the type it expands to" $
      evalsTo
        "type Pair<A, B> = Record<first: A, second: B>\nmk(): Pair<Number, String> = {first: 1, second: \"a\"}"
        "mk"
        "{\"first\":1,\"second\":\"a\"}"

  describe "path operations (spec 15.10)" $
    it "are lexical and POSIX" $ do
      evalsTo "f() = path_join([\"a\", \"b\"])" "f" "\"a/b\""
      evalsTo "f() = dirname(\"a/b/c\")" "f" "\"a/b\""
      evalsTo "f() = basename(\"a/b/c.txt\")" "f" "\"c.txt\""
      evalsTo "f() = extname(\"a/c.tar.gz\")" "f" "\".gz\""
      evalsTo "f() = normalize_path(\"a/./b/../c\")" "f" "\"a/c\""
      evalsTo "f() = is_absolute_path(\"/a\")" "f" "true"

  describe "nondeterministic generation (spec 15.13)" $ do
    it "generates a version 4 uuid in canonical form" $ do
      r <- run "f() = uuid()" "f"
      case r of
        Right t -> do
          T.length t `shouldBe` 38 -- 36 characters plus the JSON quotes
          T.isInfixOf "-4" t `shouldBe` True
        Left e -> expectationFailure (show e)
    it "generates a string of the asked-for length" $ do
      r <- run "f() = random_string(12)" "f"
      case r of
        Right t -> T.length t `shouldBe` 14
        Left e -> expectationFailure (show e)
    it "does not repeat itself" $ do
      a <- run "f() = uuid()" "f"
      b <- run "f() = uuid()" "f"
      (a == b) `shouldBe` False
    it "rejects a negative length" $
      failsWith "f() = random_string(0 - 1)" "f" ERuntimeValue
