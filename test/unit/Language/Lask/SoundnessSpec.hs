{-# LANGUAGE OverloadedStrings #-}

-- | Properties that tie the parts of the language together, over
-- generated programs and values:
--
--   * the checker against the evaluator: an expression the checker
--     gives a type evaluates to a value of that type, by the runtime
--     check of @cast@ (spec 4.3, 15.8);
--   * the serializer against itself: canonical JSON reads back as the
--     value it was written from (spec 13.1);
--   * the decoding of command line arguments against the serializer
--     (spec 11.2).
module Language.Lask.SoundnessSpec (spec) where

import Command.Lask.ArgCodec (ArgDecodeMode (..), CliArg (..), decodeArgValue, parseCliArgs)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), elaborateProgram)
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (castValueEither, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, valueFromJson)
import Language.Lask.Types (Type, renderType)
import Language.Lask.Utils (snakeToKebab)
import Support.Pending (knownBug)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

-- Typed expressions ------------------------------------------------------------

-- | The types the expressions are generated at.
data Ty
  = TNumber
  | TString
  | TBool
  | TArray Ty
  | TRecord [(Text, Ty)]
  | -- | @T | Null@, by a join with a null branch (4.3).
    TOrNull Ty
  | -- | A heterogeneous array literal, @Array<Any>@ (4.3).
    TMixed
  deriving (Show)

-- | The type the checker should infer, as it renders it.
rendered :: Ty -> Text
rendered t = case t of
  TNumber -> "Number"
  TString -> "String"
  TBool -> "Bool"
  TArray e -> "Array<" <> rendered e <> ">"
  TRecord fs -> "Record<" <> T.intercalate ", " [k <> ": " <> rendered v | (k, v) <- fs] <> ">"
  TOrNull e -> rendered e <> " | Null"
  TMixed -> "Array<Any>"

genTy :: Int -> Gen Ty
genTy 0 = elements [TNumber, TString, TBool]
genTy n =
  frequency
    [ (4, genTy 0),
      (2, TArray <$> genTy (n - 1)),
      (2, record),
      (1, TOrNull <$> genTy 0),
      (1, pure TMixed)
    ]
  where
    -- Fields in code point order, as a record type is rendered.
    record = do
      ks <- sublistOf ["a", "count", "name", "x1"] `suchThat` (not . null)
      TRecord <$> mapM (\k -> (,) k <$> genTy (n - 1)) ks

-- | An expression of the type, by the forms that build or keep it.
genExpr :: Int -> Ty -> Gen Text
genExpr n t = case t of
  TNumber ->
    oneof $
      [genNumber]
        <> deeper
          [ binary "+" TNumber TNumber,
            binary "*" TNumber TNumber,
            (\e -> "-(" <> e <> ")") <$> sub TNumber,
            call "abs" [TNumber],
            call "max" [TNumber, TNumber],
            call "length" [TString],
            call "size" [TArray TString],
            (\xs -> "sum([" <> T.intercalate ", " xs <> "])") <$> listOf1 (sub TNumber),
            conditional TNumber
          ]
  TString ->
    oneof $
      [genString]
        <> deeper
          [ call "concat" [TString, TString],
            call "to_upper" [TString],
            call "trim" [TString],
            call "to_string" [TNumber],
            (\e -> "\"n=#{" <> e <> "}\"") <$> sub TNumber,
            (\xs -> "join(" <> xs <> ", \",\")") <$> sub (TArray TString),
            conditional TString
          ]
  TBool ->
    oneof $
      [elements ["true", "false"]]
        <> deeper
          [ binary "<" TNumber TNumber,
            binary "==" TString TString,
            binary "&&" TBool TBool,
            (\e -> "!(" <> e <> ")") <$> sub TBool,
            call "contains" [TString, TString],
            call "is_empty" [TArray TNumber]
          ]
  TArray e ->
    oneof $
      [(\xs -> "[" <> T.intercalate ", " xs <> "]") <$> resize 3 (listOf1 (sub e))]
        <> deeper
          [ call "reverse" [TArray e],
            (\xs x -> "append(" <> xs <> ", " <> x <> ")") <$> sub (TArray e) <*> sub e,
            call "concat_array" [TArray e, TArray e],
            (\xs -> "filter(" <> xs <> ", \\(v) -> true)") <$> sub (TArray e),
            (\xs -> "map(" <> xs <> ", \\(v) -> v)") <$> sub (TArray e)
          ]
  TRecord fs ->
    (\vs -> "{" <> T.intercalate ", " [k <> ": " <> v | ((k, _), v) <- zip fs vs] <> "}") <$> mapM (sub . snd) fs
  TOrNull e ->
    oneof
      [ (\c x -> "(" <> c <> " ? " <> x <> " : null)") <$> sub TBool <*> sub e,
        (\c x -> "(" <> c <> " ? null : " <> x <> ")") <$> sub TBool <*> sub e,
        (\c x -> "if (" <> c <> ") { " <> x <> " }") <$> sub TBool <*> sub e
      ]
  TMixed -> (\a b -> "[" <> a <> ", " <> b <> "]") <$> sub TNumber <*> sub TString
  where
    sub = genExpr (n `div` 2)
    deeper gs = if n <= 0 then [] else gs
    binary op a b = (\x y -> "(" <> x <> " " <> op <> " " <> y <> ")") <$> sub a <*> sub b
    call f args = (\xs -> f <> "(" <> T.intercalate ", " xs <> ")") <$> mapM sub args
    conditional ty = (\c x y -> "(" <> c <> " ? " <> x <> " : " <> y <> ")") <$> sub TBool <*> sub ty <*> sub ty

genNumber :: Gen Text
genNumber =
  oneof
    [ T.pack . show <$> (choose (0, 1000) :: Gen Int),
      (\a b -> T.pack (show a <> "." <> show b)) <$> (choose (0, 99) :: Gen Int) <*> (choose (0, 99) :: Gen Int)
    ]

-- | An interpreted string literal: wide characters, quotes, escapes.
genString :: Gen Text
genString = (\parts -> "\"" <> T.concat parts <> "\"") <$> listOf part
  where
    part =
      frequency
        [ (6, T.singleton <$> elements "abc XYZ09-_.,:;!?'éß字👍"),
          (1, elements ["\\\"", "\\\\", "\\n", "\\t", "\\r", "\\u{1F600}", "\\u{7}"])
        ]

-- | Check and evaluate @x = e@: the type the checker gives @x@, and
-- the value it evaluates to.
checkAndRun :: Text -> IO (Either String (Type, Value))
checkAndRun e = do
  let src = "x = " <> e <> "\n"
      reader p = pure (if p == "main.lask" then Right src else Left "not found")
  r <- loadProgramWith reader "main.lask"
  case r >>= \prog -> (,) prog <$> validateProgram prog of
    Left ds -> pure (Left (show ds))
    Right (prog, scopes) -> case elaborateProgram prog scopes of
      Left ds -> pure (Left (show ds))
      Right cp -> case Map.lookup ("main.lask", "x") (cpDecls cp) of
        Nothing -> pure (Left "no declaration x")
        Just cd -> do
          ctx <- mkRtCtx cp "" (RtHooks (\_ _ -> pure (Right (0, "", ""))) (\_ _ -> pure (Right VVoid)) (\_ env body -> body env) noLogSink noAsyncTracker (\_ -> pure Nothing))
          v <- topValue ctx ("main.lask", "x")
          pure (Right (cdType cd, v))

-- Values ------------------------------------------------------------------------

-- | Data values, as JSON can hold them: no function, handle or
-- environment, and objects as records (spec 15.8).
genValue :: Int -> Gen Value
genValue n =
  frequency $
    [ (2, VNumber <$> genScientific),
      (2, VString . T.pack <$> listOf (elements "ab \"\\\n\t\1éß字👍")),
      (1, VBool <$> arbitrary),
      (1, pure VNull)
    ]
      <> if n <= 0
        then []
        else
          [ (1, VArray . V.fromList <$> resize 4 (listOf (genValue (n `div` 2)))),
            (1, VRecord . Map.fromList <$> resize 4 (listOf ((,) <$> genKey <*> genValue (n `div` 2))))
          ]
  where
    genKey = T.pack <$> listOf (elements "aZ_-.é字 ")

genScientific :: Gen Scientific
genScientific = scientific <$> choose (-100000, 100000) <*> choose (-6, 6)

-- | The value a JSON text reads back as.
readBack :: Text -> Maybe Value
readBack t = valueFromJson <$> A.decode (BL.fromStrict (TE.encodeUtf8 t))

spec :: Spec
spec = do
  describe "the checker and the evaluator agree (spec 4.3, 15.8)" $ do
    modifyMaxSuccess (const 200) $
      prop "an expression evaluates to a value of the type the checker gives it" $
        forAll (sized (\s -> genTy (min 3 (s `div` 20)))) $ \ty ->
          forAll (sized (\s -> genExpr (min 6 (s `div` 10)) ty)) $ \e -> monadicIO $ do
            r <- run (checkAndRun e)
            monitor (counterexample (T.unpack e))
            case r of
              Left problem -> monitor (counterexample problem) >> assert False
              Right (t, v) -> do
                monitor (counterexample ("type " <> T.unpack (renderType t) <> ", value " <> T.unpack (encodeValue v)))
                assert (renderType t == rendered ty)
                assert (either (const False) (const True) (castValueEither t v))

  describe "serialization (spec 13.1)" $ do
    prop "reads canonical JSON back as the value it was written from" $
      forAll (sized (genValue . min 6)) $ \v ->
        counterexample (T.unpack (encodeValue v)) (readBack (encodeValue v) == Just v)
    prop "writes a value it read back as the same text" $
      forAll (sized (genValue . min 6)) $ \v ->
        fmap encodeValue (readBack (encodeValue v)) === Just (encodeValue v)
    it "writes one value as one text, whatever its representation" $
      -- 1.5 - 0.5 is 1 as 1 is, and the two compare equal, but are
      -- written 1.0 and 1.
      knownBug "a Number whose Scientific is not normalized is written with a fraction: 1.0 for the 1 that 1.5 - 0.5 computes" $
        encodeValue (VNumber (scientific 10 (-1))) `shouldBe` encodeValue (VNumber 1)

  describe "command line arguments (spec 11.2)" $ do
    prop "keeps a text argument as it is" $ \s ->
      let t = T.pack s in decodeArgValue DecodeText t === Right (VString t)
    prop "decodes a JSON argument as the value it was written from" $
      forAll (sized (genValue . min 6)) $ \v ->
        decodeArgValue DecodeJson (encodeValue v) == Right v
    prop "decodes an argument that is not JSON as text, in auto mode" $ \s ->
      let t = T.pack s
       in readBack t == Nothing ==> decodeArgValue DecodeAuto t === Right (VString t)
    prop "reads --name value and --name=value as one keyword argument" $
      forAll genName $ \name -> forAll (genArg `suchThat` (not . ("-" `T.isPrefixOf`))) $ \v ->
        let flag = "--" <> snakeToKebab name
         in parseCliArgs [] [flag, v] === Right [CliKw name v]
              .&&. parseCliArgs [] [flag <> "=" <> v] === Right [CliKw name v]
    prop "reads a negative number as a positional argument, not an option" $
      forAll (choose (1, 1000 :: Int)) $ \k ->
        parseCliArgs [] [T.pack ("-" <> show k)] === Right [CliPos (T.pack ("-" <> show k))]
  where
    genName = (T.pack <$> ((:) <$> elements ['a' .. 'z'] <*> resize 8 (listOf (elements ("abcxyz_" :: String))))) `suchThat` (not . ("_" `T.isSuffixOf`))
    genArg = T.pack <$> listOf (elements "ab =-.,é字 ")
