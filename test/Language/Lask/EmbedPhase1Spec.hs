{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The constructs of the EDSL's first phase (#91), each written in Lask
-- and in the EDSL, with the two Cores compared; and the checks that
-- 'assemble' makes of them.
module Language.Lask.EmbedPhase1Spec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.Compare (sameDecl)
import Language.Lask.Core.Pretty (renderDecl)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), elaborateProgram)
import Language.Lask.Embed
import qualified Language.Lask.Embed.Do as L
import Language.Lask.Embed.Internal (programModule)
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Test.Hspec

source :: Text
source =
  T.unlines
    [ "type Opt = Record<a: Number, b?: String>",
      "",
      "pick(c: Bool): String | Null = if (c) { \"x\" }",
      "",
      "side(c: Bool): Number = do {",
      "  if (c) { 1 }",
      "  2",
      "}",
      "",
      "read_b(o: Opt): String | Null = o.b",
      "",
      "mk(): Opt = { a: 1 }",
      "",
      "inferred(): Record<a: Number, b: String> = { a: 1, b: \"x\" }",
      "",
      "same<T: comparable>(a: T, b: T): Bool = a == b",
      "",
      "use_same(): Bool = same(\"x\", \"y\")",
      "",
      "is_null(x: String | Null): Bool = x == null",
      "",
      "widened(): Bool = is_null(\"x\")",
      "",
      "cpu(m: Map<Number>): Number = m[\"cpu\"]",
      "",
      "any_str(v: Any): String = cast(v)",
      "",
      "nums(): Array<Number | Null> = [1, null]",
      "",
      "wrap<T>(x: T, c: Bool): T | Null = if (c) { x }",
      "",
      "wrap_one(): Number | Null = wrap(1, true)",
      "",
      "keep<T>(x: T | Null): T | Null = x",
      "",
      "keep_x(): String | Null = keep(\"x\")",
      "",
      "pair<A, B>(a: A, b: B): Record<first: A, second: B> = { first: a, second: b }",
      "",
      "a_pair(): Record<first: Number, second: String> = pair(1, \"x\")"
    ]

type Opt = Rec '["a" := 'TNumber, "b" :? 'TString]

pick :: Task '["c" ::: 'TBool] ('TString :| 'TNull)
pick = task "pick" $ \c -> when_ c "x"

side :: Task '["c" ::: 'TBool] 'TNumber
side = task "side" $ \c -> L.do
  whenDo c 1
  2

readB :: Task '["o" ::: Opt] ('TString :| 'TNull)
readB = task "read_b" $ \o -> field @"b" o

mk :: Task '[] Opt
mk = task "mk" $ recordAs @Opt (fld @"a" 1 :& RNil)

inferred :: Task '[] (Rec '["a" := 'TNumber, "b" := 'TString])
inferred = task "inferred" $ record (fld @"a" 1 :& fld @"b" "x" :& RNil)

type C = 'TVar "T" 'BComparable

same :: Generic '[ '("T", 'BComparable)] '["a" ::: C, "b" ::: C] 'TBool
same = generic "same" $ \a b -> a ==. b

useSame :: Task '[] 'TBool
useSame = task "use_same" $ call (inst same) "x" "y"

isNull :: Task '["x" ::: 'TString :| 'TNull] 'TBool
isNull = task "is_null" $ \x -> x ==. null_

widened :: Task '[] 'TBool
widened = task "widened" $ call isNull (widen "x")

cpu :: Task '["m" ::: 'TMap 'TNumber] 'TNumber
cpu = task "cpu" $ \m -> atKey m "cpu"

anyStr :: Task '["v" ::: 'TAny] 'TString
anyStr = task "any_str" $ \v -> cast @'TString v

nums :: Task '[] ('TArray ('TNumber :| 'TNull))
nums = task "nums" $ list [widen 1, widen null_]

type W = 'TVar "T" 'BNone

-- A type parameter in a union of the result.
wrap :: Generic '[ '("T", 'BNone)] '["x" ::: W, "c" ::: 'TBool] (W :| 'TNull)
wrap = generic "wrap" $ \x c -> when_ c x

wrapOne :: Task '[] ('TNumber :| 'TNull)
wrapOne = task "wrap_one" $ call (inst wrap) 1 true

-- A type parameter inside a union of a parameter: not inferred from the
-- argument, so given with @.
keep :: Generic '[ '("T", 'BNone)] '["x" ::: W :| 'TNull] (W :| 'TNull)
keep = generic "keep" $ \x -> x

keepX :: Task '[] ('TString :| 'TNull)
keepX = task "keep_x" $ call (inst @'TString keep) (widen "x")

type A = 'TVar "A" 'BNone

type B = 'TVar "B" 'BNone

-- Two type parameters.
pair :: Generic '[ '("A", 'BNone), '("B", 'BNone)] '["a" ::: A, "b" ::: B] (Rec '["first" := A, "second" := B])
pair = generic "pair" $ \a b -> record (fld @"first" a :& fld @"second" b :& RNil)

aPair :: Task '[] (Rec '["first" := 'TNumber, "second" := 'TString])
aPair = task "a_pair" $ call (inst2 pair) 1 "x"

embedded :: Either [Text] Program
embedded =
  assemble
    [ export pick,
      export side,
      export readB,
      export mk,
      export inferred,
      export useSame,
      export widened,
      export cpu,
      export anyStr,
      export nums,
      export wrapOne,
      export keepX,
      export aPair
    ]

elaborated :: IO CoreProgram
elaborated = do
  r <- loadProgramWith (\p -> pure (if p == "main.lask" then Right source else Left "not found")) "main.lask"
  case r >>= \prog -> validateProgram prog >>= elaborateProgram prog of
    Right cp -> pure cp
    Left ds -> fail ("main.lask does not elaborate: " <> show ds)

-- A generic instantiated at its own parameter outside any generic: the
-- parameter escapes into a task that does not declare it.
leaky :: Task '["x" ::: C] 'TBool
leaky = task "leaky" $ \x -> call (inst @C same) x x

spec :: Spec
spec = describe "Language.Lask.Embed, first phase" $ do
  it "reifies each construct to the elaborator's Core" $ do
    cp <- elaborated
    prog <- either (fail . T.unpack . T.unlines) pure embedded
    let source' = Map.fromList [(n, cd) | ((_, n), cd) <- Map.toList (cpDecls cp)]
        mine = Map.elems (cpDecls (progCore prog))
    length mine `shouldBe` 18
    forM_ mine $ \e -> case Map.lookup (cdName e) source' of
      Nothing -> expectationFailure ("not in the source: " <> T.unpack (cdName e))
      Just s -> case sameDecl e s of
        Nothing -> pure ()
        Just why -> expectationFailure (T.unpack (cdName e <> ": " <> why <> "\nembedded:\n" <> renderDecl e <> "\nelaborated:\n" <> renderDecl s))

  it "records a generic's type parameters and bounds" $ do
    prog <- either (fail . T.unpack . T.unlines) pure embedded
    case Map.lookup (programModule, "same") (cpDecls (progCore prog)) of
      Nothing -> expectationFailure "same is not in the program"
      Just cd -> do
        cdTypeVars cd `shouldBe` ["T"]
        Map.keys (cdBounds cd) `shouldBe` ["T"]

  it "rejects a type parameter that escapes its declaration" $
    either id (const []) (assemble [export leaky])
      `shouldSatisfy` any ("mentions the type parameter T, which it does not declare" `T.isInfixOf`)
