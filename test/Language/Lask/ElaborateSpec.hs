{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.ElaborateSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic, diagCode)
import Language.Lask.Elaborate
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Types (renderType)
import Test.Hspec

-- | Full front-end pipeline over in-memory sources.
elab :: [(FilePath, Text)] -> IO (Either [ErrorCode] (Map (FilePath, Text) CoreDecl))
elab files = do
  r <- loadProgramWith reader "main.lask"
  pure $ case r of
    Left ds -> Left (codes ds)
    Right prog -> case validateProgram prog of
      Left ds -> Left (codes ds)
      Right scopes -> case elaborateProgram prog scopes of
        Left ds -> Left (codes ds)
        Right cp -> Right (cpDecls cp)
  where
    reader p = pure (maybe (Left "not found") Right (lookup p files))
    codes :: [Diagnostic] -> [ErrorCode]
    codes = map diagCode

-- | Type of a declaration in main.lask, rendered.
typeOf :: Text -> Text -> IO (Either [ErrorCode] Text)
typeOf src name = do
  r <- elab [("main.lask", src)]
  pure $ case r of
    Left cs -> Left cs
    Right decls -> case Map.lookup ("main.lask", name) decls of
      Just cd -> Right (renderType (cdType cd))
      Nothing -> Left []

hasType :: Text -> Text -> Text -> Expectation
hasType src name expected = typeOf src name >>= (`shouldBe` Right expected)

accepts :: Text -> Expectation
accepts src = do
  r <- elab [("main.lask", src)]
  case r of
    Right _ -> pure ()
    Left cs -> expectationFailure ("expected success, got " <> show cs)

rejects :: Text -> ErrorCode -> Expectation
rejects src code = do
  r <- elab [("main.lask", src)]
  case r of
    Left cs | code `elem` cs -> pure ()
    other -> expectationFailure ("expected " <> show code <> ", got " <> show other)

spec :: Spec
spec = do
  describe "literal inference (spec 4.3)" $ do
    it "infers numbers" $ hasType "n = 1" "n" "Number"
    it "infers booleans" $ hasType "ok = true" "ok" "Bool"
    it "infers homogeneous arrays" $ hasType "names = [\"a\", \"b\"]" "names" "Array<String>"
    it "widens mixed arrays to Any" $ hasType "mixed = [1, \"a\"]" "mixed" "Array<Any>"
    it "types empty arrays as Array<Any> without expectation" $ hasType "empty = []" "empty" "Array<Any>"
    it "infers records from object literals" $
      hasType "user = {name: \"alice\", age: 20}" "user" "Record<age: Number, name: String>"
    it "types object literals as Map under a Map expectation" $
      hasType "envMap: Map<String> = {\"APP_ENV\": \"prod\"}" "envMap" "Map<String>"
    it "rejects map values not conforming to the expectation" $
      rejects "bad: Map<Number> = {\"a\": \"x\"}" ETypeMismatch
    it "checks array elements against the expected element type" $
      accepts "xs: Array<Any> = [1, \"a\"]"
    it "rejects records with missing expected fields" $
      rejects "u: Record<name: String, age: Number> = {name: \"a\"}" ETypeMismatch

  describe "declarations and calls (spec 4.3, 7.5)" $ do
    it "types annotated functions" $
      hasType "add2(x: Number, y: Number): Number = x + y" "add2" "Function<Number, Number, Number>"
    it "defaults unannotated parameters to Any" $
      hasType "identity(x) = x" "identity" "Function<Any, Any>"
    it "infers keyword parameter types from defaults" $
      hasType "inc(--x = 0) = x + 1" "inc" "Function<Number>"
    it "accepts keyword calls and defaults" $
      accepts "inc(--x = 0) = x + 1\na = inc()\nb = inc(x = 5)"
    it "rejects unknown keyword arguments" $
      rejects "inc(--x = 0) = x + 1\na = inc(y = 1)" ETypeKeyword
    it "rejects binding positional parameters by keyword" $
      rejects "f(a: Number) = a\nx = f(a = 1)" ETypeKeyword
    it "rejects duplicate keyword arguments" $
      rejects "inc(--x = 0) = x + 1\na = inc(x = 1, x = 2)" ETypeKeyword
    it "rejects missing positional arguments" $
      rejects "f(a: Number, b: Number) = a\nx = f(1)" ETypeArity
    it "rejects excess arguments without variadic" $
      rejects "f(a: Number) = a\nx = f(1, 2)" ETypeArity
    it "collects variadic arguments" $
      accepts "s(...xs: Array<Number>): Number = reduce(xs, 0, \\(a: Number, x: Number) -> a + x)\nx = s(1, 2, 3)\ny = s()"
    it "gives variadic functions an array parameter type" $
      hasType "s(...xs: Array<Number>): Number = 0" "s" "Function<Array<Number>, Number>"
    it "function values take the packed array positionally" $
      accepts "s(...xs: Array<Number>): Number = 0\nf: Function<Array<Number>, Number> = s\nr = f([1, 2])"
    it "rejects keyword arguments through function values" $
      rejects "f(--n: Number = 0) = n\ng: Function<Number> = f\nx = g(n = 1)" ETypeKeyword
    it "rejects argument type mismatches" $
      rejects "f(a: Number) = a\nx = f(\"s\")" ETypeMismatch
    it "rejects calling non-functions" $
      rejects "x = 1\ny = x(2)" ETypeCall
    it "supports recursion with a return annotation" $
      accepts "fact(n: Number): Number = if (n == 0) { 1 } else { n * fact(n - 1) }"
    it "rejects recursion without a return annotation" $
      rejects "f(n: Number) = f(n)" ETypeMismatch

  describe "builtin polymorphism (spec 4.4)" $ do
    it "instantiates map per call" $
      hasType "xs = map([1, 2], \\(x: Number) -> x + 1)" "xs" "Array<Number>"
    it "adopts lambda parameter types from instantiation" $
      hasType "xs = map([1, 2], \\(x) -> x)" "xs" "Array<Number>"
    it "instantiates reduce" $
      hasType "n = reduce([1, 2], 0, \\(acc: Number, x: Number) -> acc + x)" "n" "Number"
    it "instantiates from an annotation when referenced as a value" $
      accepts "m: Function<Array<Number>, Function<Number, Number>, Array<Number>> = map"
    it "rejects unannotated references to polymorphic builtins" $
      rejects "m = map" ETypeMismatch
    it "types cast from the expected type" $
      accepts "v: Any = 1\nn: Number = cast(v)"
    it "rejects cast without a contextual type" $
      rejects "v: Any = 1\nn = cast(v)" ETypeMismatch
    it "rejects cast to non-data types" $
      rejects "v: Any = 1\nf: Function<Number> = cast(v)" ETypeIllformed

  describe "operators (spec 6.2)" $ do
    it "types arithmetic as Number" $ hasType "x = 1 + 2 * 3" "x" "Number"
    it "rejects string operands of +" $ rejects "x = \"a\" + \"b\"" ETypeMismatch
    it "types comparisons as Bool" $ hasType "x = 1 < 2" "x" "Bool"
    it "requires equal comparable types for ==" $ rejects "x = 1 == \"a\"" ETypeMismatch
    it "rejects == on functions" $
      rejects "f(x: Number) = x\ng(x: Number) = x\nb = f == g" ETypeMismatch
    it "allows == on environments" $ hasType "b = #local == #alpine:3.12" "b" "Bool"
    it "types pipes as application" $
      hasType "g(x: Number) = x + 1\ny = 3 |> g" "y" "Number"
    it "types composition" $
      hasType "g(x: Number): Number = x + 1\nh = g >> g" "h" "Function<Number, Number>"
    it "composed functions are callable" $
      accepts "g(x: Number): Number = x + 1\nh = g >> g\ny = h(1)"

  describe "control structures (spec 6.4, 6.5)" $ do
    it "requires Bool conditions" $
      rejects "x = if (1) { 2 } else { 3 }" ETypeMismatch
    it "requires matching branch types" $
      rejects "x = if (true) { 2 } else { \"a\" }" ETypeMismatch
    it "types for over arrays as map" $
      hasType "f(xs: Array<String>) = for (x : xs) { concat(\"item:\", x) }" "f" "Function<Array<String>, Array<String>>"
    it "types Void-bodied for as forEach" $
      hasType "f(xs: Array<String>) = for (x : xs) {}" "f" "Function<Array<String>, Void>"
    it "types do blocks by the last statement" $
      hasType "f() = do {\n  a = 1\n  a + 1\n}" "f" "Function<Number>"
    it "types empty blocks as Void" $ hasType "f() = do {}" "f" "Function<Void>"

  describe "early return (spec 6.5)" $ do
    it "accepts guard + return in function bodies" $
      accepts
        "publish(tag: String): String = do {\n  if (tag == \"\") { return \"skip\" }\n  \"released\"\n}"
    it "requires guard blocks to end with return" $
      rejects "f(c: Bool) = do {\n  if (c) { 1 }\n  2\n}" ESyntaxReturnPosition
    it "rejects unreachable statements after return" $
      rejects "f(): Number = do {\n  return 1\n  2\n}" ESyntaxReturnPosition
    it "rejects return in for bodies" $
      rejects "f(xs: Array<Number>) = do {\n  for (x : xs) { return x }\n  1\n}" ESyntaxReturnPosition
    it "rejects return in expression-position do blocks" $
      rejects "f() = do {\n  x = do { return 1 }\n  x\n}" ESyntaxReturnPosition
    it "distributes continuations into both if branches" $
      accepts
        "classify(n: Number): String = do {\n  if (n > 0) { return \"pos\" } else { 0 }\n  \"non-pos\"\n}"

  describe "accessors (spec 6.8)" $ do
    it "types record field access" $
      hasType "u = {name: \"a\"}\nn = u.name" "n" "String"
    it "rejects unknown record fields" $
      rejects "u = {name: \"a\"}\nn = u.nope" ETypeAccess
    it "rejects dot access on maps" $
      rejects "m: Map<String> = {\"a\": \"x\"}\nv = m.a" ETypeAccess
    it "types record access via string-literal index" $
      hasType "h = {\"X-Api-Key\": \"secret\"}\nv = h[\"X-Api-Key\"]" "v" "String"
    it "rejects non-literal record indexes" $
      rejects "u = {name: \"a\"}\nk = \"name\"\nv = u[k]" ETypeAccess
    it "types array indexing" $
      hasType "xs = [1, 2]\nv = xs[0]" "v" "Number"
    it "requires Number indexes for arrays" $
      rejects "xs = [1, 2]\nv = xs[\"0\"]" ETypeMismatch
    it "types map indexing" $
      hasType "m: Map<Number> = {\"a\": 1}\nv = m[\"a\"]" "v" "Number"

  describe "commands and environments (spec 6.6, 6.7)" $ do
    it "types $ as String" $ hasType "v() = $ git --version" "v" "Function<String>"
    it "types $* as CommandResult" $
      hasType "v() = $* ls" "v" "Function<Record<code: Number, stderr: String, stdout: String>>"
    it "accepts Environment-typed command environments" $
      accepts "b(env: Environment) = $[env] ls"
    it "rejects non-Environment command environments" $
      rejects "b(e: Number) = $[e] ls" ETypeCommandEnv
    it "accepts environment constructors" $
      accepts "e1 = #local\ne2 = #docker(\"alpine:3.12\", memory = \"4g\")\ne3 = #env(\"ansible\")\ne4 = #alpine:3.12"
    it "rejects docker without an image" $
      rejects "e = #docker()" ETypeEnvConstruct
    it "rejects unknown docker options" $
      rejects "e = #docker(\"a\", nope = 1)" ETypeEnvConstruct
    it "rejects interpolated env names" $
      rejects "n = \"x\"\ne = #env(\"a#{n}\")" ETypeEnvConstruct
    it "rejects remote constructors in code" $
      rejects "e = #remote(\"h\")" ETypeEnvConstruct
    it "rejects interpolating non-stringifiable values" $
      rejects "u = {a: 1}\ns = \"v=#{u}\"" ETypeMismatch

  describe "async and errors (spec 6.3, 6.9)" $ do
    it "types async as AsyncHandle" $
      hasType "f() = async 1" "f" "Function<AsyncHandle<Number>>"
    it "types await as the resolved type" $
      hasType "f() = do {\n  h = async 1\n  await h\n}" "f" "Function<Number>"
    it "rejects await on non-handles" $
      rejects "f() = await 1" ETypeMismatch
    it "types spawn and all" $
      hasType "f() = all([async 1, async 2])" "f" "Function<Array<Number>>"
    it "types try/catch with matching branches" $
      hasType "f() = try { \"ok\" } catch (e) { e.message }" "f" "Function<String>"
    it "gives catch variables the Error type" $
      hasType "f() = try { 1 } catch (e) { e.code }" "f" "Function<Number>"
    it "rejects mismatched catch types" $
      rejects "f() = try { 1 } catch (e) { \"x\" }" ETypeMismatch
    it "types try/finally by the body" $
      hasType "f() = try { 1 } finally { runCommand(\"true\") }" "f" "Function<Number>"

  describe "misc" $ do
    it "types stdin as String" $ hasType "s = trim(stdin)" "s" "String"
    it "rejects Array<Void> annotations" $
      rejects "xs: Array<Void> = []" ETypeIllformed
    it "resolves imported declarations with types" $ do
      r <-
        elab
          [ ("main.lask", "import { add } from \"lib.lask\"\nx = add(1, 2)"),
            ("lib.lask", "add(a: Number, b: Number): Number = a + b")
          ]
      case r of
        Right _ -> pure ()
        Left cs -> expectationFailure (show cs)
    it "namespace member calls check keyword arguments" $ do
      r <-
        elab
          [ ("main.lask", "import * as m from \"lib.lask\"\nx = m.greet(\"a\", prefix = \"hi\")"),
            ("lib.lask", "greet(name: String, --prefix: String = \"hello\"): String = concat(prefix, name)")
          ]
      case r of
        Right _ -> pure ()
        Left cs -> expectationFailure (show cs)
