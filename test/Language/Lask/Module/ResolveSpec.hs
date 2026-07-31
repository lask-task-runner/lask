{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Module.ResolveSpec (spec) where

import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic, diagCode)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Test.Hspec

-- | Load a program from an in-memory file set and validate it.
check :: [(FilePath, Text)] -> IO (Either [ErrorCode] ())
check files = do
  r <- loadProgramWith reader "main.lask"
  pure $ case r of
    Left ds -> Left (codes ds)
    Right prog -> case validateProgram prog of
      Left ds -> Left (codes ds)
      Right _ -> Right ()
  where
    reader p = pure (maybe (Left "not found") Right (lookup p files))
    codes :: [Diagnostic] -> [ErrorCode]
    codes = map diagCode

ok :: [(FilePath, Text)] -> Expectation
ok files = check files >>= (`shouldBe` Right ())

failsWith :: [(FilePath, Text)] -> ErrorCode -> Expectation
failsWith files code = do
  r <- check files
  case r of
    Left cs | code `elem` cs -> pure ()
    other -> expectationFailure ("expected " <> show code <> ", got " <> show other)

spec :: Spec
spec = do
  describe "module loading" $ do
    it "loads a two-module program" $
      ok
        [ ("main.lask", "import { add } from \"./lib.lask\"\nsum2(a: Number, b: Number) = add(a, b)"),
          ("lib.lask", "add(x: Number, y: Number): Number = x + y")
        ]

    it "detects circular imports" $
      failsWith
        [ ("main.lask", "import { a } from \"./a.lask\"\nx = a"),
          ("a.lask", "import { b } from \"./b.lask\"\na = b"),
          ("b.lask", "import { a } from \"./a.lask\"\nb = a")
        ]
        EModuleCycle

    it "reports unknown import symbols" $
      failsWith
        [ ("main.lask", "import { missing } from \"./lib.lask\"\nx = missing"),
          ("lib.lask", "a = 1")
        ]
        ENameUndefined

    it "reports unreadable modules" $
      failsWith
        [("main.lask", "import { a } from \"./nope.lask\"\nx = a")]
        ENameUndefined

  describe "top-level collisions" $ do
    it "rejects duplicate top-level declarations" $
      failsWith [("main.lask", "a = 1\na = 2")] ENameDuplicate

    it "rejects binding core function names" $
      failsWith [("main.lask", "map(xs: Array<Any>) = xs")] ENameDuplicate

    it "rejects binding stdin" $
      failsWith [("main.lask", "stdin = \"x\"")] ENameDuplicate

    it "allows shadowing plain builtins" $
      ok [("main.lask", "concat(a: String, b: String) = a")]

    it "rejects import names colliding with top-level declarations" $
      failsWith
        [ ("main.lask", "import { a } from \"./lib.lask\"\na = 1"),
          ("lib.lask", "a = 2")
        ]
        ENameDuplicate

    it "rejects redefining builtin type aliases" $
      failsWith [("main.lask", "type Error = Record<code: Number>")] ENameDuplicate

  describe "namespace imports" $ do
    it "resolves namespace member references" $
      ok
        [ ("main.lask", "import * as m from \"./lib.lask\"\nx = m.f(1)"),
          ("lib.lask", "f(a: Number) = a")
        ]

    it "reports missing namespace members" $
      failsWith
        [ ("main.lask", "import * as m from \"./lib.lask\"\nx = m.g(1)"),
          ("lib.lask", "f(a: Number) = a")
        ]
        ENameUndefined

    it "local bindings shadow namespaces in accessor position" $
      ok
        [ ("main.lask", "import * as m from \"./lib.lask\"\nf(m: Record<g: Number>) = m.g"),
          ("lib.lask", "h(a: Number) = a")
        ]

  describe "references and scopes" $ do
    it "reports undefined names" $
      failsWith [("main.lask", "x = nope")] ENameUndefined

    it "resolves builtins and stdin" $
      ok [("main.lask", "x = trim(stdin)")]

    it "does not treat choose as referenceable" $
      failsWith [("main.lask", "x = choose")] ENameUndefined

    it "allows parameters to shadow top-level names" $
      ok [("main.lask", "a = 1\nf(a: Number) = a")]

    it "rejects duplicate parameters" $
      failsWith [("main.lask", "f(a, a) = a")] ENameDuplicate

    it "rejects core names as parameters" $
      failsWith [("main.lask", "f(fail) = fail")] ENameDuplicate

    it "keyword defaults see preceding parameters" $
      ok [("main.lask", "f(a: Number, --b: Number = a) = b")]

    it "reports use before binding in do blocks" $
      failsWith [("main.lask", "f() = do {\n  x = y\n  y = 1\n  x\n}")] ENameUndefined

    it "rejects rebinding in the same do block" $
      failsWith [("main.lask", "f() = do {\n  x = 1\n  x = 2\n  x\n}")] ENameDuplicate

    it "allows inner blocks to shadow outer bindings" $
      ok [("main.lask", "f(c: Bool) = do {\n  x = 1\n  if (c) { x = 2; x } else { x }\n}")]

    it "scopes for variables to the body" $
      failsWith [("main.lask", "f(xs: Array<Number>) = do {\n  for (x : xs) { x }\n  x\n}")] ENameUndefined

    it "scopes catch variables to the handler" $
      ok [("main.lask", "f() = try { 1 } catch (e) { e.code }")]

  describe "types" $ do
    it "reports undefined type names" $
      failsWith [("main.lask", "x: Missing = 1")] ENameUndefined

    it "resolves builtin type aliases" $
      ok [("main.lask", "f(e: Error): Number = e.code")]

    it "resolves imported type aliases" $
      ok
        [ ("main.lask", "import { Strings } from \"./lib.lask\"\nxs: Strings = [\"a\"]"),
          ("lib.lask", "type Strings = Array<String>")
        ]

    it "rejects recursive type aliases" $
      failsWith [("main.lask", "type T = Array<T>")] ETypeIllformed

    it "rejects mutually recursive type aliases" $
      failsWith
        [("main.lask", "type A = Array<B>\ntype B = Map<A>")]
        ETypeIllformed
