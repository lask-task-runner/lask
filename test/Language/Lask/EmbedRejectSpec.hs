{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- Every declaration below is ill-typed on purpose. Deferring the errors
-- turns each into an exception raised where the program is reified, so
-- the claim that GHC rejects it can be tested like any other.
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Language.Lask.EmbedRejectSpec (spec) where

import Control.Exception (TypeError (..), evaluate)
import Data.List (isInfixOf)
import Language.Lask.Embed
import Test.Hspec

go :: Command
go = command "go" (image "golang:1.22")

fact :: Task '["n" ::: 'TNumber] 'TNumber
fact = task "fact" $ \n -> if_ (n ==. 0) 1 (n * call fact (n - 1))

-- | 'rejected' for a generic declaration.
rejectedG :: Generic vs ps r -> [String] -> Expectation
rejectedG g fragments = evaluate forced `shouldThrow` \(TypeError m) -> all (`isInfixOf` m) fragments
  where
    forced = case assemble [export g] of
      Left errs -> length (show errs)
      Right p -> length (show (progCore p))

-- | Reifying the task raises the deferred type error, whose message
-- must mention each of @fragments@.
rejected :: Task ps r -> [String] -> Expectation
rejected t fragments = evaluate forced `shouldThrow` \(TypeError m) -> all (`isInfixOf` m) fragments
  where
    forced = case assemble [export t] of
      Left errs -> length (show errs)
      Right p -> length (show (progCore p))

wrongArgument :: Task '[] 'TNumber
wrongArgument = task "wrong_argument" $ call fact "five"

missingArgument :: Task '[] 'TNumber
missingArgument = task "missing_argument" $ call fact

notStringifiable :: Task '[] 'TString
notStringifiable = task "not_stringifiable" $ "vet " <> str (lines_ "a")

noSuchField :: Task '[] 'TNumber
noSuchField = task "no_such_field" $ field @"cod" (runAll go "version")

orderedStrings :: Task '[] 'TBool
orderedStrings = task "ordered_strings" $ "a" <. "b"

comparedVoid :: Task '[] 'TBool
comparedVoid = task "compared_void" $ done ==. done

voidParameter :: Task '["x" ::: 'TVoid] 'TNumber
voidParameter = task "void_parameter" $ \_ -> 1

voidElements :: Task '["xs" ::: 'TArray 'TVoid] 'TNumber
voidElements = task "void_elements" $ \_ -> 1

voidKeyword :: Task '[] 'TNumber
voidKeyword = taskWith "void_keyword" $ const 1 <$> kw "x" done

voidField :: Task '["r" ::: Rec '["a" := 'TVoid]] 'TNumber
voidField = task "void_field" $ \_ -> 1

mappedToVoid :: Task '[] ('TArray 'TVoid)
mappedToVoid = task "mapped_to_void" $ mapE (lines_ "a") (const done)

-- First phase ----------------------------------------------------------------

type C = 'TVar "T" 'BComparable

same :: Generic '[ '("T", 'BComparable)] '["a" ::: C, "b" ::: C] 'TBool
same = generic "same" $ \a b -> a ==. b

notConforming :: Task '[] 'TNumber
notConforming = task "not_conforming" $ widen @'TNumber ("x" :: E v 'TString)

unrelatedEquality :: Task '[] 'TBool
unrelatedEquality = task "unrelated_equality" $ ("a" :: E v 'TString) ==. (1 :: E v 'TNumber)

boundViolated :: Task '["x" ::: 'TAny] 'TBool
boundViolated = task "bound_violated" $ \x -> call (inst same) x x

type U = 'TVar "U" 'BNone

castToParameter :: Generic '[ '("U", 'BNone)] '["x" ::: 'TAny] U
castToParameter = generic "cast_to_parameter" $ \x -> cast @U x

rigid :: Generic '[ '("U", 'BNone)] '["x" ::: U] 'TString
rigid = generic "rigid" $ \x -> x <> "a"

type Opt = Rec '["a" := 'TNumber, "b" :? 'TString]

missingField :: Task '[] Opt
missingField = task "missing_field" $ recordAs @Opt RNil

wrongFieldType :: Task '[] Opt
wrongFieldType = task "wrong_field_type" $ recordAs @Opt (fld @"a" "one" :& RNil)

-- Holes found in review of the first phase ---------------------------------------

emptyUnion :: Task '["s" ::: 'TString] 'TString
emptyUnion = task "empty_union" $ \s -> widen @'TString (cast @('TUnion '[]) s)

duplicateRecordType :: Task '["s" ::: 'TAny] 'TNumber
duplicateRecordType = task "duplicate_record_type" $ \s -> field @"a" (cast @('TRecord '["a" := 'TNumber, "a" := 'TString]) s)

-- The signature says comparable, the declaration says no bound.
boundNotDeclared :: Generic '[ '("T", 'BNone)] '["a" ::: C, "b" ::: C] 'TBool
boundNotDeclared = generic "bound_not_declared" $ \a b -> a ==. b

twiceDeclared :: Generic '[ '("U", 'BNone), '("U", 'BOrderable)] '["x" ::: U] U
twiceDeclared = generic "twice_declared" $ \x -> x

duplicateKeyUnderAny :: Task '[] 'TAny
duplicateKeyUnderAny = task "duplicate_key_under_any" $ widen @'TAny (record (fld @"a" 1 :& fld @"a" "x" :& RNil))

missingFieldUnderAny :: Task '["o" ::: Opt] 'TAny
missingFieldUnderAny = task "missing_field_under_any" $ \o -> widen @'TAny (field @"c" o)

whenDoLast :: Task '["c" ::: 'TBool] 'TVoid
whenDoLast = task "when_do_last" $ \c -> whenDo c (1 :: E v 'TNumber)

spec :: Spec
spec = describe "Language.Lask.Embed rejects at compile time" $ do
  it "an argument of the wrong type (E-TYPE-MISMATCH)" $
    rejected wrongArgument ["Couldn't match type", "TNumber", "TString"]
  it "a missing argument (E-TYPE-ARITY)" $
    rejected missingArgument ["Couldn't match type"]
  it "interpolating an array (E-TYPE-BOUND)" $
    rejected notStringifiable ["Array<String> cannot be interpolated"]
  it "a field the record does not have (E-TYPE-ACCESS)" $
    rejected noSuchField ["the record has no field \"cod\""]
  it "ordering strings, which Lask orders only as numbers" $
    rejected orderedStrings ["Couldn't match type", "TNumber", "TString"]
  it "comparing Void" $
    rejected comparedVoid ["Void is not comparable"]
  it "a Void parameter (E-TYPE-ILLFORMED)" $
    rejected voidParameter ["Void is not a data type"]
  it "an array of Void (E-TYPE-ILLFORMED)" $
    rejected voidElements ["Array<Void> is not a data type"]
  it "a Void keyword parameter (E-TYPE-ILLFORMED)" $
    rejected voidKeyword ["Void is not a data type"]
  it "a record field of type Void (E-TYPE-ILLFORMED)" $
    rejected voidField ["Record<a: Void> is not a data type"]
  it "a loop collecting Void (E-TYPE-ILLFORMED)" $
    rejected mappedToVoid ["Array<Void> is not a well-formed type"]
  it "a value where a type it does not conform to is wanted (E-TYPE-MISMATCH)" $
    rejected notConforming ["String does not conform to Number"]
  it "== on types neither of which conforms to the other (E-TYPE-MISMATCH)" $
    rejected unrelatedEquality ["== compares String with Number"]
  it "an instantiation outside the parameter's bound (E-TYPE-BOUND)" $
    rejected boundViolated ["Any does not satisfy the bound of T"]
  it "cast to a type parameter (E-TYPE-ILLFORMED)" $
    rejectedG castToParameter ["cast cannot target U"]
  it "using a type parameter as a concrete type: it is opaque in its body (E-TYPE-MISMATCH)" $
    rejectedG rigid ["Couldn't match type", "TVar"]
  it "a record literal missing a required field (E-TYPE-MISMATCH)" $
    rejected missingField ["the record literal does not fit Record<a: Number, b?: String>"]
  it "a record literal with a field of the wrong type (E-TYPE-MISMATCH)" $
    rejected wrongFieldType ["the record literal does not fit Record<a: Number, b?: String>"]
  it "a union not in canonical form, built with the raw constructor (E-TYPE-ILLFORMED)" $
    rejected emptyUnion ["is not a data type"]
  it "a record type with a field twice, built with the raw constructor (E-TYPE-FIELD-DUPLICATE)" $
    rejected duplicateRecordType ["is not a data type"]
  it "a type parameter used with a bound it was not declared with (E-TYPE-BOUND)" $
    rejectedG boundNotDeclared ["must be declared, once each, with the bounds they carry"]
  it "a type parameter declared twice (E-NAME-DUPLICATE)" $
    rejectedG twiceDeclared ["must be declared, once each, with the bounds they carry"]
  it "a record literal with a key twice, even where nothing reads its type (E-TYPE-FIELD-DUPLICATE)" $
    rejected duplicateKeyUnderAny ["the record literal gives a field twice"]
  it "a field the record does not have, even where nothing reads its type (E-TYPE-ACCESS)" $
    rejected missingFieldUnderAny ["the record has no field \"c\""]
  it "whenDo as a block's value: only a statement before the last discards one (E-TYPE-MISMATCH)" $
    rejected whenDoLast ["Couldn't match", "Stmt"]

