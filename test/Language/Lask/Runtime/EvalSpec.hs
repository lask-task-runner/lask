{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EvalSpec (spec) where

import Control.Exception (try)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Impl (CommandRunner)
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
          ctx <- mkRtCtx cp "in-data\n" mockRunner
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
      evalsTo "f() = $ echo hi" "f" "\"hi\\n\""
    it "returns the whole result for $*" $
      evalsTo "f() = $* boom" "f" "{\"code\":7,\"stderr\":\"kaboom\",\"stdout\":\"\"}"
    it "fails with the exit code and stderr for $ on non-zero" $
      -- The command string runs to the end of the line (spec 6.6), so
      -- the try block must span multiple lines.
      evalsTo
        "f() = try {\n  $ boom\n} catch (e) {\n  e.message\n}"
        "f"
        "\"kaboom\""
    it "exposes the command exit code to catch" $
      evalsTo
        "f() = do {\n  r = try {\n    $ boom\n  } catch (e) {\n    concat(\"code=\", \"?\")\n  }\n  r\n}"
        "f"
        "\"code=?\""
    it "command failures carry the exit code" $
      evalsTo
        "g(): Number = do {\n  r = $* boom\n  r.code\n}\nf() = g()"
        "f"
        "7"
    it "interpolates into command strings" $
      evalsTo "n = \"world\"\nf() = $ echo hello #{n}" "f" "\"hello world\\n\""
    it "propagates infrastructure failures" $
      failsWith "f() = $ unreachable" "f" EIoEnvResolve

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
