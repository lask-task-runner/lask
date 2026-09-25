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
rejects src code = rejectsFiles [("main.lask", src)] code

-- | The type of a declaration in main.lask, over several modules.
hasTypeFiles :: [(FilePath, Text)] -> Text -> Text -> Expectation
hasTypeFiles files name expected = do
  r <- elab files
  fmap (fmap (renderType . cdType) . Map.lookup ("main.lask", name)) r
    `shouldBe` Right (Just expected)

-- | As 'accepts' and 'rejects', over several modules; the program is
-- entered at main.lask.
acceptsFiles :: [(FilePath, Text)] -> Expectation
acceptsFiles files = do
  r <- elab files
  case r of
    Right _ -> pure ()
    Left cs -> expectationFailure ("expected success, got " <> show cs)

rejectsFiles :: [(FilePath, Text)] -> ErrorCode -> Expectation
rejectsFiles files code = do
  r <- elab files
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
    it "instantiates from a later argument, not only from the left" $ do
      hasType
        "f() = filter(cast(from_json(\"[1]\")), \\(s: String) -> length(s) > 0)"
        "f"
        "Function<Array<String>>"
      accepts "f(): Array<Number> = append(cast(from_json(\"[1]\")), 2)"
      accepts "f(m: Map<Number>): Number = get_or(m, \"k\", cast(from_json(\"1\")))"
    it "lets fail stand in an argument a sibling determines (spec 15.7)" $
      accepts
        "f(e: Record<code: Number, message: String>, xs: Array<Number>): Number = reduce(xs, fail(e), \\(a: Number, x: Number) -> a)"
    it "refuses an Any argument in a polymorphic position (spec 4.4)" $ do
      -- The element type came from the lambda, so the array was typed
      -- Array<String> while holding numbers, with no cast in sight.
      rejects
        "f(): String = do {\n  ys = filter(from_json(\"[1,2,3]\"), \\(s: String) -> length(s) > 0)\n  join(ys, \",\")\n}"
        ETypeMismatch
      rejects "f(): Number = size(from_json(\"[1]\"))" ETypeMismatch
      rejects "f(): Array<Any> = do {\n  xs = from_json(\"[1]\")\n  for (x : xs) { x }\n}" ETypeMismatch
    it "takes the same value once it has been cast (spec 15.8)" $ do
      accepts
        "f(): String = join(filter(cast(from_json(\"[1]\")), \\(s: String) -> length(s) > 0), \",\")"
      accepts "f(): Number = do {\n  xs: Array<Any> = cast(from_json(\"[1]\"))\n  size(xs)\n}"
    it "keeps taking Any where Any is what the position requires" $ do
      accepts "f(v: Any): String = to_json(v)"
      accepts "f(v: Any): Number = cast(v)"
      accepts "f(v: Any): Array<Any> = append([], v)"
    it "still reports an argument nothing can determine" $ do
      rejects "f(): Number = size(cast(from_json(\"[1]\")))" ETypeMismatch
      rejects
        "f(): Number = size(concat_array(cast(from_json(\"[1]\")), cast(from_json(\"[2]\"))))"
        ETypeMismatch

  describe "user type parameters (spec 4.2, 4.4)" $ do
    it "instantiates a declaration per call" $ do
      hasType
        "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\ng() = first_or([1], 0)"
        "g"
        "Function<Number>"
      hasType
        "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\ng() = first_or([\"a\"], \"z\")"
        "g"
        "Function<String>"
    it "requires the arguments to agree on the instantiation" $
      rejects
        "first_or<T>(xs: Array<T>, fallback: T): T = if (is_empty(xs)) { fallback } else { xs[0] }\ng(): Number = first_or([1], \"a\")"
        ETypeMismatch
    it "keeps keyword arguments and variadics (spec 7.5)" $ do
      accepts
        "tag<T>(x: T, --label: String = \"v\"): Record<label: String, value: T> = {label: label, value: x}\ng(): Record<label: String, value: Number> = tag(1, label = \"n\")"
      accepts "listy<T>(...xs: Array<T>): Array<T> = xs\ng(): Array<Number> = listy(1, 2, 3)"
      rejects "listy<T>(...xs: Array<T>): Array<T> = xs\ng(): Array<Number> = listy(1, \"a\")" ETypeMismatch
    it "treats a type parameter as opaque in the body (rigidity)" $ do
      rejects "eq<T>(a: T, b: T): Bool = a == b" ETypeMismatch
      rejects "show<T>(x: T): String = \"v=#{x}\"" ETypeMismatch
      rejects "render<T>(x: T): String = to_string(x)" ETypeMismatch
      rejects "sorted<T>(xs: Array<T>): Array<T> = sort(xs)" ETypeMismatch
      rejects "narrow<T>(x: T): Number = case (x) {\n  Number -> 1\n  else -> 0\n}" ETypeMismatch
    it "lets a type parameter be moved around, which is all it can be" $ do
      accepts "ident<T>(x: T): T = x"
      accepts "pair<T>(x: T): Array<T> = [x, x]"
      accepts "wrap<T>(x: T): Record<value: T> = {value: x}"
      accepts "encode<T>(x: T): String = to_json(x)"
      accepts "pick<T>(xs: Array<T>, p: Function<T, Bool>): T | Null = find(xs, p)"
    it "scopes a type parameter over body annotations (spec 6.5)" $
      accepts "head<T>(xs: Array<T>): T = do {\n  x: T = xs[0]\n  x\n}"
    it "instantiates a reference as a value only from the expected type" $ do
      accepts "ident<T>(x: T): T = x\nf: Function<Number, Number> = ident"
      rejects "ident<T>(x: T): T = x\nf = ident" ETypeMismatch
      accepts "ident<T>(x: T): T = x\nf(xs: Array<Number>): Array<Number> = map(xs, ident)"
    it "rejects a type parameter that collides with an alias" $
      rejects "type T = Number\nf<T>(x: T): T = x" ENameDuplicate
    it "allows a type parameter that appears nowhere" $
      accepts "f<T>(x: Number): Number = x"
    it "checks a default against the rigid parameter (spec 6.1)" $ do
      accepts "f<T>(x: T, --y: T = x): T = y"
      accepts "f<T>(--xs: Array<T> = []): Array<T> = xs"
      accepts "f<T>(--y: T | Null = null): T | Null = y"
      rejects "f<T>(--y: T = 1): T = y" ETypeMismatch
    it "supports recursion with a return annotation" $
      accepts
        "countdown<T>(n: Number, x: T): T = if (n > 0) { countdown(n - 1, x) } else { x }\ng(): Number = countdown(3, 7)"

  describe "optional record fields (spec 4.2)" $ do
    it "lets a literal omit an optional key and no other" $ do
      accepts "f(): Record<a: Number, b?: String> = {a: 1}"
      rejects "f(): Record<a: Number, b: String> = {a: 1}" ETypeMismatch
      rejects "f(): Record<a: Number, b?: String> = {a: 1, c: 2}" ETypeMismatch
    it "keeps the key question and the value question apart" $ do
      -- b?: String says the key may be absent, not that the value may
      -- be null; b?: String | Null says both.
      rejects "f(): Record<a: Number, b?: String> = {a: 1, b: null}" ETypeMismatch
      accepts "f(): Record<a: Number, b?: String | Null> = {a: 1, b: null}"
    it "reads an optional field as T | Null (spec 6.8)" $ do
      hasType "f(r: Record<a?: String>) = r.a" "f" "Function<Record<a?: String>, String | Null>"
      hasType "f(r: Record<a: String>) = r.a" "f" "Function<Record<a: String>, String>"
      hasType "f(r: Record<a?: String>) = r[\"a\"]" "f" "Function<Record<a?: String>, String | Null>"
    it "makes optionality part of the type (spec 4.4)" $ do
      rejects "f(r: Record<a: String>): Record<a?: String> = r" ETypeMismatch
      rejects "f(r: Record<a?: String>): Record<a: String> = r" ETypeMismatch
    it "never infers an optional field" $
      rejects "f(): Record<a?: Number> = do {\n  r = {a: 1}\n  r\n}" ETypeMismatch
    it "renders the marker on the key" $
      hasType "f(r: Record<a?: String, b: Number>): Number = 1" "f" "Function<Record<a?: String, b: Number>, Number>"

  describe "parameterised type aliases (spec 4.2)" $ do
    it "expands by substituting the type arguments" $
      hasType
        "type Pair<A, B> = Record<first: A, second: B>\nf(p: Pair<Number, String>): String = p.second"
        "f"
        "Function<Record<first: Number, second: String>, String>"
    it "gives a name to an optional" $
      accepts
        "type Opt<A> = A | Null\nf(x: Opt<String>): String = case (x) {\n  Null -> \"n\"\n  else -> x\n}"
    it "requires the argument count to match" $ do
      rejects "type Pair<A, B> = Record<first: A, second: B>\nf(p: Pair<Number>): Number = p.first" ETypeArity
      rejects "type Strings = Array<String>\nf(x: Strings<Number>): Number = 1" ETypeArity
      rejects "type Pair<A, B> = Record<first: A, second: B>\nf(p: Pair): Number = 1" ETypeArity
    it "checks well-formedness after expansion" $
      rejects "type Bad<A> = Array<A>\nf(x: Bad<Void>): Number = 1" ETypeIllformed

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
    it "types Void-bodied for as for_each" $
      hasType "f(xs: Array<String>) = for (x : xs) {}" "f" "Function<Array<String>, Void>"
    it "types do blocks by the last statement" $
      hasType "f() = do {\n  a = 1\n  a + 1\n}" "f" "Function<Number>"
    it "types empty blocks as Void" $ hasType "f() = do {}" "f" "Function<Void>"

  describe "union types (spec 4.2, 4.4)" $ do
    it "reduces a union to its canonical form" $ do
      hasType "f(x: Null | String): Null | String = x" "f" "Function<String | Null, String | Null>"
      hasType "f(x: String | String): String = x" "f" "Function<String, String>"
      hasType "f(x: Any | Null): Any = x" "f" "Function<Any, Any>"
    it "accepts a member where the union is required" $
      accepts "f(): String | Null = \"a\"\ng(): String | Null = null"
    it "rejects the union where a member is required" $
      rejects "f(x: String | Null): String = x" ETypeMismatch
    it "does not lift the union through a constructor (spec 4.4)" $
      rejects "f(xs: Array<String>): Array<String | Null> = xs" ETypeMismatch
    it "checks a literal element-wise against a union element type" $
      accepts "xs: Array<String | Null> = [\"a\", null]"
    it "propagates an expected union into both branches of an if (spec 4.3)" $
      accepts "f(c: Bool): String | Null = if (c) { \"a\" } else { null }"
    it "still refuses to infer a union from differing branches" $
      rejects "f(c: Bool) = if (c) { \"a\" } else { null }" ETypeMismatch
    it "compares a union with one of its members (spec 6.2)" $ do
      accepts "f(x: String | Null): Bool = x == null"
      rejects "f(x: String | Null): Bool = x == 1" ETypeMismatch
    it "refuses to interpolate a union that may be absent (spec 6.6)" $
      rejects "f(x: String | Null): String = \"v=#{x}\"" ETypeMismatch
    it "refuses to_string of a union that may be absent (spec 15.3)" $
      rejects "f(x: String | Null): String = to_string(x)" ETypeMismatch
    it "interpolates a union all of whose members are stringifiable" $
      accepts "f(x: String | Number): String = \"v=#{x}\""
    it "rejects a member that is not a data type (spec 4.2)" $ do
      rejects "f(x: Function<Number, Number> | Null): Number = 1" ETypeIllformed
      rejects "f(x: AsyncHandle<Number> | Null): Number = 1" ETypeIllformed
      rejects "f(x: Void | Null): Number = 1" ETypeIllformed
    it "determines a union from the expected type (spec 4.4)" $ do
      -- What a signature with a union in a parameter position relies
      -- on: the variable is fixed by whichever position fixes it, and
      -- the arguments are then checked against the concrete union.
      accepts "g(): Array<String | Number> = append([1], \"a\")"
      rejects "g() = append([1], \"a\")" ETypeMismatch
    it "accepts a mixed variadic where the union is written down" $ do
      accepts "f(...xs: Array<String | Number>): Number = size(xs)\ng(): Number = f(1, \"a\")"
      rejects "f(...xs: Array<Number>): Number = size(xs)\ng(): Number = f(1, \"a\")" ETypeMismatch
    it "casts to a union (spec 15.8)" $
      accepts "f(v: Any): String | Null = cast(v)"
    it "instantiates a union return type from an argument (spec 4.4)" $
      hasType
        "f(xs: Array<String>) = find(xs, \\(s: String) -> true)"
        "f"
        "Function<Array<String>, String | Null>"
    it "rejects an instantiation whose result would be ill-formed" $
      rejects
        "f(fs: Array<Function<Number, Number>>) = find(fs, \\(g: Function<Number, Number>) -> true)"
        ETypeIllformed

  describe "case expressions (spec 6.4)" $ do
    it "types a case by its arm bodies" $
      hasType
        "f(x: String) = case (x) {\n  \"a\" -> 1\n  else -> 2\n}"
        "f"
        "Function<String, Number>"
    it "types the condition form by its arm bodies" $
      hasType
        "f(n: Number) = case {\n  n > 1 -> \"big\"\n  else -> \"small\"\n}"
        "f"
        "Function<Number, String>"
    it "requires an else arm" $
      rejects "f(x: String) = case (x) {\n  \"a\" -> 1\n}" ESyntaxCaseElse
    it "requires the else arm to be last" $
      rejects "f(x: String) = case (x) {\n  else -> 1\n  \"a\" -> 2\n}" ESyntaxCaseElse
    it "requires arm heads to have the scrutinee type" $
      rejects "f(x: String) = case (x) {\n  1 -> 1\n  else -> 2\n}" ETypeMismatch
    it "requires the condition form's heads to be Bool" $
      rejects "f(x: String) = case {\n  x -> 1\n  else -> 2\n}" ETypeMismatch
    it "requires the arm bodies to agree" $
      rejects "f(x: String) = case (x) {\n  \"a\" -> 1\n  else -> \"z\"\n}" ETypeMismatch
    it "rejects a scrutinee that cannot be compared (spec 6.2)" $
      rejects "f(x: Any) = case (x) {\n  \"a\" -> 1\n  else -> 2\n}" ETypeMismatch
    it "rejects a literal head an earlier arm already matches" $
      rejects
        "f(x: String) = case (x) {\n  \"a\" -> 1\n  \"b\", \"a\" -> 2\n  else -> 3\n}"
        ETypeCaseDuplicate
    it "allows equal heads that are not literals" $
      accepts "k = \"a\"\nf(x: String) = case (x) {\n  k -> 1\n  k -> 2\n  else -> 3\n}"
    it "takes the arm type from any arm that infers on its own (spec 15.7)" $
      hasType
        "f(x: String): String = case (x) {\n  \"a\" -> fail({ code: 1, message: \"no\" })\n  else -> \"z\"\n}"
        "f"
        "Function<String, String>"
    it "checks every arm against an expected type" $
      rejects
        "f(x: String): String = case (x) {\n  \"a\" -> \"y\"\n  else -> 2\n}"
        ETypeMismatch
    it "narrows a union scrutinee in each arm (spec 6.4)" $
      hasType
        "f(x: Number | String | Null): String = case (x) {\n  Null -> \"none\"\n  Number -> to_string(x)\n  else -> x\n}"
        "f"
        "Function<Number | String | Null, String>"
    it "narrows the else arm by subtracting the matched members" $
      accepts "f(x: String | Null): String = case (x) {\n  Null -> \"none\"\n  else -> x\n}"
    it "subtracts Null for a null value head too" $
      accepts "f(x: String | Null): String = case (x) {\n  null -> \"none\"\n  else -> x\n}"
    it "does not subtract for a value head that does not exhaust its member" $
      rejects "f(x: String | Null): String = case (x) {\n  \"a\" -> \"none\"\n  else -> x\n}" ETypeMismatch
    it "does not narrow a scrutinee that is not a plain name" $
      rejects
        "g(): String | Null = null\nf(): String = case (g()) {\n  Null -> \"none\"\n  else -> g()\n}"
        ETypeMismatch
    it "keeps the narrowing inside the arm" $
      rejects
        "f(x: String | Null): String = do {\n  y = case (x) {\n    Null -> \"none\"\n    else -> x\n  }\n  x\n}"
        ETypeMismatch
    it "dispatches on the type of an Any scrutinee (spec 4.4, 6.4)" $
      hasType
        "f(v: Any): String = case (v) {\n  String -> v\n  Array<String> -> join(v, \",\")\n  else -> to_json(v)\n}"
        "f"
        "Function<Any, String>"
    it "leaves an Any scrutinee Any in the else arm" $
      rejects "f(v: Any): String = case (v) {\n  Number -> \"n\"\n  else -> v\n}" ETypeMismatch
    it "rejects a type head that is not a member of the union" $
      rejects "f(x: String | Null): Number = case (x) {\n  Number -> 1\n  else -> 2\n}" ETypeMismatch
    it "rejects a type head on a scrutinee that is neither a union nor Any" $
      rejects "f(x: Number): Number = case (x) {\n  Number -> 1\n  else -> 2\n}" ETypeMismatch
    it "rejects a type head that is not a data type" $
      rejects "f(v: Any): Number = case (v) {\n  Function<Number> -> 1\n  else -> 2\n}" ETypeIllformed
    it "rejects a type head in the condition form" $
      rejects "f(): Number = case {\n  String -> 1\n  else -> 2\n}" ETypeMismatch
    it "rejects a type head an earlier arm already matches" $
      rejects
        "f(x: String | Null): Number = case (x) {\n  Null -> 1\n  Null -> 2\n  else -> 3\n}"
        ETypeCaseDuplicate
    it "does not require a comparable scrutinee when every head is a type" $
      accepts "f(x: Map<Any> | Null): Number = case (x) {\n  Null -> 0\n  else -> 1\n}"
    it "rejects return inside an arm body (spec 6.5)" $
      rejects
        "f(x: String): String = do {\n  y = case (x) {\n    \"a\" -> do { return \"e\" }\n    else -> \"z\"\n  }\n  y\n}"
        ESyntaxReturnPosition

  describe "local binding annotations (spec 6.5)" $ do
    it "takes the annotation as the declared type" $
      hasType "f() = do {\n  x: Any = 1\n  x\n}" "f" "Function<Any>"
    it "checks the right-hand side against the annotation" $
      rejects "f() = do {\n  x: Number = \"a\"\n  x\n}" ETypeMismatch
    it "gives an expected type to an expression that needs one (spec 15.8)" $
      accepts "f(v: Any): String = do {\n  s: String = cast(v)\n  s\n}"
    it "gives a union to a binding whose right-hand side does not determine it" $
      accepts "f(): String = do {\n  p: String | Null = null\n  case (p) {\n    Null -> \"n\"\n    else -> p\n  }\n}"
    it "checks an annotated last statement against what the block owes" $
      rejects "f(): Number = do {\n  x: String = \"a\"\n}" ETypeMismatch
    it "rejects a Void annotation (spec 4.2)" $
      rejects "f(): Number = do {\n  x: Void = 1\n  2\n}" ETypeIllformed
    it "keeps the !! rule on the declared type (spec 6.10)" $ do
      accepts "f(): Number = do {\n  p!!: String = \"s\"\n  length(p)\n}"
      rejects "f(): Number = do {\n  p!!: Number = 1\n  p\n}" ETypeSecretNonString
    it "resolves a named type in the annotation" $ do
      accepts "type Name = String\nf(): Name = do {\n  x: Name = \"a\"\n  x\n}"
      rejects "f(): Number = do {\n  x: Nope = 1\n  1\n}" ENameUndefined

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
    it "types $ as String" $ hasType "v() = $[#local] git --version" "v" "Function<String>"
    it "types $*[#local] as CommandResult" $
      hasType "v() = $*[#local] ls" "v" "Function<Record<code: Number, stderr: String, stdout: String>>"
    it "accepts Environment-typed command environments" $
      accepts "b(env: Environment) = $[env] ls"
    it "rejects non-Environment command environments" $
      rejects "b(e: Number) = $[e] ls" ETypeCommandEnv
    it "accepts environment constructors" $
      accepts "e1 = #local\ne2 = #docker(\"alpine:3.12\", memory = \"4g\")\ne3 = #docker(dockerfile = \"infra/Dockerfile\", context = \".\")\ne4 = #alpine:3.12"
    it "rejects docker without an image or a recipe" $
      rejects "e = #docker()" ETypeEnvConstruct
    it "rejects unknown docker options" $
      rejects "e = #docker(\"a\", nope = 1)" ETypeEnvConstruct
    it "rejects interpolated env names" $
      rejects "n = \"x\"\ne = #env(\"a#{n}\")" ETypeEnvConstruct
    it "rejects unknown environment kinds" $
      rejects "e = #remote(\"h\")" ETypeEnvConstruct
    it "rejects a docker image reference without a tag or digest" $
      rejects "e = #docker(\"alpine\")" ETypeEnvConstruct
    it "rejects giving both an image reference and a recipe" $
      rejects "e = #docker(\"alpine:3.20\", dockerfile = \"D\")" ETypeEnvConstruct
    it "rejects a recipe path escaping the module tree" $
      rejects "e = #docker(dockerfile = \"../D\")" ETypeEnvConstruct
    it "rejects a non-literal recipe path" $
      rejects "p = \"D\"\ne = #docker(dockerfile = \"#{p}\")" ETypeEnvConstruct
    it "accepts the container options of 10.2" $
      accepts
        "e = #docker(\"alpine:3.20\", cpus = 2, ulimits = [\"nofile=1024:1024\"], env = {\"CI\": \"1\"}, init = true, tmpfs = [\"/tmp\"], publish = [\"8080:80\"], volumes = [\"c:/cache\"])"
    it "accepts build arguments on a recipe" $
      accepts "e = #docker(dockerfile = \"D\", build_args = {\"VERSION\": \"1.2.3\"})"
    it "rejects build arguments that are not literals, since they decide the image" $ do
      rejects "v = \"1.2.3\"\ne = #docker(dockerfile = \"D\", build_args = {\"VERSION\": v})" ETypeEnvConstruct
      rejects "v = \"1.2.3\"\ne = #docker(dockerfile = \"D\", build_args = {\"VERSION\": \"#{v}\"})" ETypeEnvConstruct
    it "rejects build arguments on a registry reference" $
      rejects "e = #docker(\"alpine:3.20\", build_args = {\"V\": \"1\"})" ETypeEnvConstruct
    it "rejects a container option of the wrong type" $
      rejects "e = #docker(\"alpine:3.20\", tmpfs = \"/tmp\")" ETypeMismatch
    it "rejects interpolating non-stringifiable values" $
      rejects "u = {a: 1}\ns = \"v=#{u}\"" ETypeMismatch

  describe "command imports and exports (spec ch. 5)" $ do
    let tools =
          ( "tools.lask",
            "mk(--proxy: String = \"\"): Environment = #docker(\"golang:1.25\", env = {\"GOPROXY\": proxy})\n\
            \node = #node:24-alpine\n\
            \command { \"go\", \"gofmt\" } on mk()\n\
            \export command { \"node\", \"npm\" } on node\n\
            \internal command { \"helper\" } on #local"
          )

    it "dispatches an imported command word" $
      acceptsFiles
        [ ("main.lask", "import command { \"go\", \"npm\" } from \"./tools.lask\"\nt(): String = $ go test ./...\nw(): String = $ npm ci"),
          tools
        ]

    it "keeps the words of one imported declaration on one environment" $
      acceptsFiles
        [ ("main.lask", "import command { \"go\", \"gofmt\" } from \"./tools.lask\"\nv(): String = $ gofmt -l . && go vet"),
          tools
        ]

    it "brings in only the words it names" $
      rejectsFiles
        [ ("main.lask", "import command { \"go\" } from \"./tools.lask\"\nv(): String = $ npm ci"),
          tools
        ]
        ETypeCommandNoEnv

    it "brings in no command word through a namespace import" $
      rejectsFiles
        [("main.lask", "import * as t from \"./tools.lask\"\nv(): String = $ go vet"), tools]
        ETypeCommandNoEnv

    it "rejects a word the module does not export" $ do
      rejectsFiles
        [("main.lask", "import command { \"helper\" } from \"./tools.lask\""), tools]
        ENameUndefined
      rejectsFiles
        [("main.lask", "import command { \"cargo\" } from \"./tools.lask\""), tools]
        ENameUndefined

    it "does not export a word the module itself imported" $
      rejectsFiles
        [ ("main.lask", "import command { \"go\" } from \"./mid.lask\""),
          ("mid.lask", "import command { \"go\" } from \"./tools.lask\""),
          tools
        ]
        ENameUndefined

    it "passes a word on through a re-export" $
      acceptsFiles
        [ ("main.lask", "import command { \"go\" } from \"./mid.lask\"\nv(): String = $ go vet"),
          ("mid.lask", "export command { \"go\" } from \"./tools.lask\""),
          tools
        ]

    it "treats one declaration reached along two paths as one" $
      acceptsFiles
        [ ( "main.lask",
            "import command { \"go\" } from \"./tools.lask\"\nimport command { \"go\" } from \"./mid.lask\"\nv(): String = $ go vet"
          ),
          ("mid.lask", "export command { \"go\" } from \"./tools.lask\""),
          tools
        ]

    it "rejects a word both declared and imported" $
      rejectsFiles
        [ ("main.lask", "import command { \"go\" } from \"./tools.lask\"\ncommand { \"go\" } on #golang:1.25"),
          tools
        ]
        ETypeCommandDuplicate

    it "names an environment through a namespace member" $
      acceptsFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\ncommand { \"node\" } on t.node\nv(): String = $ node -v"),
          tools
        ]

    it "names an environment through a call into another module" $
      acceptsFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\ncommand { \"aws\" } on t.mk(proxy = \"direct\")\nv(): String = $ aws --version"),
          tools
        ]

  -- A re-export publishes a name from the module that declares it
  -- (spec 5); a namespace member has to reach that declaration, not a
  -- declaration of the re-exporting module, which has none.
  describe "namespace members that a module re-exports (spec 5, 7.2)" $ do
    let lib =
          ( "lib/go.lask",
            "go(--proxy: String = \"\"): Environment = #docker(\"golang:1.25\", env = {\"GOPROXY\": proxy})\n\
            \image = \"golang:1.25\"\n\
            \type Pin = Record<image: String>\n\
            \internal hidden = 1"
          )
        tools = ("tools.lask", "export { go, image, Pin } from \"./lib/go.lask\"")

    it "calls a re-exported function, keyword arguments included" $
      hasTypeFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\nb(): Environment = t.go(proxy = \"direct\")"),
          tools,
          lib
        ]
        "b"
        "Function<Environment>"

    it "reads a re-exported value" $
      hasTypeFiles
        [("main.lask", "import * as t from \"./tools.lask\"\nv(): String = t.image"), tools, lib]
        "v"
        "Function<String>"

    it "follows a chain of re-exports" $
      hasTypeFiles
        [ ("main.lask", "import * as t from \"./outer.lask\"\nb(): Environment = t.go(proxy = \"direct\")"),
          ("outer.lask", "export { go } from \"./tools.lask\""),
          tools,
          lib
        ]
        "b"
        "Function<Environment>"

    it "follows a re-export that renames" $
      hasTypeFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\nb(): Environment = t.golang(proxy = \"direct\")"),
          ("tools.lask", "export { go as golang } from \"./lib/go.lask\""),
          lib
        ]
        "b"
        "Function<Environment>"

    it "names a re-exported type through the namespace" $
      acceptsFiles
        [("main.lask", "import * as t from \"./tools.lask\"\np: t.Pin = {image: t.image}"), tools, lib]

    it "backs a command declaration with a re-exported function" $
      acceptsFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\ncommand { \"go\" } on t.go()\nv(): String = $ go vet"),
          tools,
          lib
        ]

    it "still rejects a name the module does not publish" $
      rejectsFiles
        [ ("main.lask", "import * as t from \"./tools.lask\"\nv(): Number = t.hidden"),
          ("tools.lask", "import { hidden } from \"./lib/go.lask\""),
          lib
        ]
        ENameUndefined

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
      hasType "f() = try { 1 } finally { run(#local, \"true\") }" "f" "Function<Number>"

  describe "command declarations and dispatch (spec ch. 5, 10.9)" $ do
    it "takes the environment from the command word" $
      hasType "command { \"go\" } on #golang:1.25\nv() = $ go test ./..." "v" "Function<String>"

    it "accepts a top-level binding of an environment expression" $
      hasType "e = #golang:1.25\ncommand { \"go\" } on e\nv() = $ go test ./..." "v" "Function<String>"

    it "lets neutral words stand alongside a declared command" $
      hasType "command { \"npm\" } on #node:20.20.2-alpine3.23\nv() = $ cd web && npm ci" "v" "Function<String>"

    it "skips assignment words before the command word" $
      hasType "command { \"npm\" } on #node:20.20.2-alpine3.23\nv() = $ FOO=1 npm ci" "v" "Function<String>"

    it "rejects a command string that names no declared command" $
      rejects "command { \"go\" } on #golang:1.25\nv() = $ npm ci" ETypeCommandNoEnv

    it "rejects a bare command with no declarations at all" $
      rejects "v() = $ echo hi" ETypeCommandNoEnv

    it "rejects a command word that cannot be determined statically" $
      rejects "command { \"go\" } on #golang:1.25\nbin = \"go\"\nv() = $ #{bin} test" ETypeCommandNoEnv

    it "rejects a command string that could not be segmented" $
      rejects "command { \"go\" } on #golang:1.25\nv() = $ go test \"unterminated" ETypeCommandNoEnv

    it "rejects two different environments in one command string" $
      rejects
        "command { \"go\" } on #golang:1.25\ncommand { \"npm\" } on #node:20.20.2-alpine3.23\nv() = $ go build && npm ci"
        ETypeCommandConflict

    it "treats #local as an environment like any other" $ do
      hasType "command { \"ls\" } on #local\nv() = $ ls dist" "v" "Function<String>"
      rejects
        "command { \"ls\" } on #local\ncommand { \"go\" } on #golang:1.25\nv() = $ ls dist && go test"
        ETypeCommandConflict

    it "does not conflict when two declarations name the same environment" $
      hasType
        "command { \"go\" } on #golang:1.25\ncommand { \"gofmt\" } on #golang:1.25\nv() = $ gofmt -l . && go vet"
        "v"
        "Function<String>"

    -- Selection compares environment values (10.9), so the order the
    -- options were written in is not part of what is compared.
    it "does not conflict when the same options are written in a different order" $
      hasType
        "command { \"go\" } on #docker(\"golang:1.25\", cpus = 2, memory = \"4g\")\ncommand { \"gofmt\" } on #docker(\"golang:1.25\", memory = \"4g\", cpus = 2)\nv() = $ gofmt -l . && go vet"
        "v"
        "Function<String>"

    -- A list or a table is still known before execution, so an
    -- environment carrying one stays declarable (ch. 5).
    it "accepts an environment whose options are literal lists and tables" $
      hasType
        "command { \"go\" } on #docker(\"golang:1.25\", env = {\"CI\": \"1\"}, tmpfs = [\"/tmp\"])\nv() = $ go vet"
        "v"
        "Function<String>"

    -- The environment of a declaration is an ordinary expression
    -- (ch. 5): what it may not do is have an effect.
    it "accepts an environment computed from other values" $
      hasType
        "d = \"/tmp\"\ncommand { \"go\" } on #docker(\"golang:1.25\", tmpfs = [d])\nv() = $ go vet"
        "v"
        "Function<String>"

    it "accepts an environment produced by a call" $
      hasType
        "mk(--proxy: String = \"\"): Environment = #docker(\"golang:1.25\", env = {\"GOPROXY\": proxy})\ncommand { \"go\" } on mk(proxy = \"direct\")\nv() = $ go vet"
        "v"
        "Function<String>"

    it "accepts an environment that reads a variable of the process" $
      hasType
        "command { \"go\" } on #docker(\"golang:#{get_env(\"GO_TAG\")}\")\nv() = $ go vet"
        "v"
        "Function<String>"

    it "rejects an environment that can run a command" $
      rejects
        "mk(): Environment = do {\n  tag = $[#local] cat tag\n  return #docker(\"golang:#{tag}\")\n}\ncommand { \"go\" } on mk()\nv() = $ go vet"
        ETypeCommandEffect

    it "rejects an environment that reads the standard input" $
      rejects "command { \"go\" } on #docker(\"golang:#{stdin}\")\nv() = $ go vet" ETypeCommandEffect

    it "rejects an environment that touches the filesystem" $
      rejects
        "command { \"go\" } on #docker(\"golang:#{read_file(\"tag\", #local)}\")\nv() = $ go vet"
        ETypeCommandEffect

    -- The command string inside mk() is dispatched against the table
    -- being built: an effect, and one that must not loop.
    it "rejects an environment that dispatches a command of its own module" $
      rejects
        "command { \"ls\" } on mk()\nmk(): Environment = do {\n  x = $ ls\n  return #alpine:3.22.2\n}"
        ETypeCommandEffect

    it "rejects an environment that is not an Environment" $
      rejects "command { \"go\" } on \"golang:1.25\"\nv() = $ go vet" ETypeCommandEnv

    it "leaves an explicit environment specification alone" $
      hasType
        "command { \"npm\" } on #node:20.20.2-alpine3.23\nv() = $[#local] npm ci"
        "v"
        "Function<String>"

    -- Selection compares where environments come from (10.9): two
    -- calls are two values, known equal only at run time.
    it "agrees on two declarations naming one binding" $
      hasType
        "box = #docker(\"golang:1.25\", env = {\"HOME\": get_env(\"HOME\")})\ncommand { \"go\" } on box\ncommand { \"gofmt\" } on box\nv() = $ gofmt -l . && go vet"
        "v"
        "Function<String>"

    it "agrees on the words of one declaration whose environment is a call" $
      hasType
        "mk(): Environment = #golang:1.25\ncommand { \"go\", \"gofmt\" } on mk()\nv() = $ gofmt -l . && go vet"
        "v"
        "Function<String>"

    it "does not take two calls for one environment" $
      rejects
        "mk(): Environment = #golang:1.25\ncommand { \"go\" } on mk()\ncommand { \"gofmt\" } on mk()\nv() = $ gofmt -l . && go vet"
        ETypeCommandConflict

    it "rejects the command words without braces, saying how to write them" $
      rejects "command \"go\" on #golang:1.25\nv() = $ go vet" ESyntaxUnexpectedToken

    it "rejects a name that could never be a command word" $
      rejects "command { \"my prog\" } on #local\nv() = $ ls" ETypeCommandName

    it "rejects a duplicate command word" $
      rejects "command { \"go\", \"go\" } on #golang:1.25\nv() = $ go test" ETypeCommandDuplicate

  describe "misc" $ do
    it "types stdin as String" $ hasType "s = trim(stdin)" "s" "String"
    it "rejects Array<Void> annotations" $
      rejects "xs: Array<Void> = []" ETypeIllformed

  describe "secret bindings (spec 6.10)" $ do
    it "accepts !! on String bindings of every kind" $ do
      accepts "a!!: String = \"s\""
      accepts "a!! = \"s\""
      accepts "f(x!!: String) = x"
      accepts "f(--x!!: String = \"d\") = x"
      accepts "f() = do { x!! = \"s\"\n  x }"

    it "leaves the binding's type unchanged (no distinct secret type)" $ do
      hasType "a!!: String = \"s\"" "a" "String"
      hasType "f(x!!: String): String = x" "f" "Function<String, String>"

    it "rejects !! on a non-String value declaration" $
      rejects "n!!: Number = 1" ETypeSecretNonString

    it "rejects !! on a non-String inferred bind statement" $
      rejects "f() = do { n!! = 1\n  n }" ETypeSecretNonString

    it "rejects !! on non-String parameters" $ do
      rejects "f(n!!: Number) = n" ETypeSecretNonString
      rejects "f(--n!!: Number = 1) = n" ETypeSecretNonString

    it "rejects !! on a function-typed binding" $
      rejects "g!! = \\(x: String) -> x" ETypeSecretNonString
    it "resolves imported declarations with types" $ do
      r <-
        elab
          [ ("main.lask", "import { add } from \"./lib.lask\"\nx = add(1, 2)"),
            ("lib.lask", "add(a: Number, b: Number): Number = a + b")
          ]
      case r of
        Right _ -> pure ()
        Left cs -> expectationFailure (show cs)
    it "namespace member calls check keyword arguments" $ do
      r <-
        elab
          [ ("main.lask", "import * as m from \"./lib.lask\"\nx = m.greet(\"a\", prefix = \"hi\")"),
            ("lib.lask", "greet(name: String, --prefix: String = \"hello\"): String = concat(prefix, name)")
          ]
      case r of
        Right _ -> pure ()
        Left cs -> expectationFailure (show cs)

    it "expands a namespace-qualified type reference (spec 4.2 QualifiedNamedType)" $ do
      r <-
        elab
          [ ("main.lask", "import * as m from \"./lib.lask\"\nxs: m.Strings = [\"a\"]"),
            ("lib.lask", "type Strings = Array<String>")
          ]
      case r of
        Right decls -> case Map.lookup ("main.lask", "xs") decls of
          Just cd -> renderType (cdType cd) `shouldBe` "Array<String>"
          Nothing -> expectationFailure "xs not found"
        Left cs -> expectationFailure (show cs)
