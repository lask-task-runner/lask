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

voidField :: Task '["r" ::: 'TRecord '[ '("a", 'TVoid)]] 'TNumber
voidField = task "void_field" $ \_ -> 1

mappedToVoid :: Task '[] ('TArray 'TVoid)
mappedToVoid = task "mapped_to_void" $ mapE (lines_ "a") (const done)

spec :: Spec
spec = describe "Language.Lask.Embed rejects at compile time" $ do
  it "an argument of the wrong type (E-TYPE-MISMATCH)" $
    rejected wrongArgument ["IsString", "(E v TNumber)"]
  it "a missing argument (E-TYPE-ARITY)" $
    rejected missingArgument ["Couldn't match type"]
  it "interpolating an array (E-TYPE-BOUND)" $
    rejected notStringifiable ["Stringify (TArray TString)"]
  it "a field the record does not have (E-TYPE-ACCESS)" $
    rejected noSuchField ["the record has no field \"cod\""]
  it "ordering strings, which Lask orders only as numbers" $
    rejected orderedStrings ["IsString", "(E v TNumber)"]
  it "comparing Void" $
    rejected comparedVoid ["Comparable TVoid"]
  it "a Void parameter (E-TYPE-ILLFORMED)" $
    rejected voidParameter ["Void is not a data type"]
  it "an array of Void (E-TYPE-ILLFORMED)" $
    rejected voidElements ["Void is not a data type"]
  it "a Void keyword parameter (E-TYPE-ILLFORMED)" $
    rejected voidKeyword ["Void is not a data type"]
  it "a record field of type Void (E-TYPE-ILLFORMED)" $
    rejected voidField ["Void is not a data type"]
  it "a loop collecting Void (E-TYPE-ILLFORMED)" $
    rejected mappedToVoid ["Void is not a data type"]
