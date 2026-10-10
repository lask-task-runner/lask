{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.SuggestSpec (spec) where

import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic (..), mkDiagnostic, withNote, withSuggestions)
import Language.Lask.Elaborate (elaborateProgram)
import Language.Lask.ErrorCode
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Span (Span (..))
import Language.Lask.Suggest (editDistance, suggest)
import Language.Lask.Utils (Pretty (pretty))
import Test.Hspec

-- | The suggestions of every diagnostic the front end reports for an
-- in-memory program, with the code each comes with.
suggestionsFor :: [(FilePath, Text)] -> IO [(ErrorCode, [Text])]
suggestionsFor files = do
  r <- loadProgramWith reader "main.lask"
  pure . map (\d -> (diagCode d, diagSuggestions d)) $ case r of
    Left ds -> ds
    Right prog -> case validateProgram prog of
      Left ds -> ds
      Right scopes -> either id (const []) (elaborateProgram prog scopes)
  where
    reader p = pure (maybe (Left "not found") Right (lookup p files))

suggests :: Text -> ErrorCode -> [Text] -> Expectation
suggests src code expected = suggestionsFor [("main.lask", src)] >>= (`shouldBe` [(code, expected)])

spec :: Spec
spec = do
  describe "edit distance" $ do
    it "counts a swapped pair as one edit" $
      editDistance "hlelo" "hello" `shouldBe` 1
    it "counts insertions, deletions and substitutions" $ do
      editDistance "kitten" "sitting" `shouldBe` 3
      editDistance "" "abc" `shouldBe` 3
      editDistance "abc" "" `shouldBe` 3
      editDistance "same" "same" `shouldBe` 0

  describe "choosing candidates (spec 14.3)" $ do
    it "keeps candidates within max(1, length / 3)" $ do
      suggest "helo" ["hello", "help", "world"] `shouldBe` ["hello", "help"]
      suggest "ab" ["abcd"] `shouldBe` []
    it "sorts by distance, then by name, and shows at most three" $
      suggest "abcdef" ["abcdxx", "abcdex", "abcdeg", "abcdeh", "abcdei"] `shouldBe` ["abcdeg", "abcdeh", "abcdei"]
    it "ignores case and takes '-' and '_' as one" $ do
      suggest "cowsay-hello" ["cowsay_hello"] `shouldBe` ["cowsay_hello"]
      suggest "Hello" ["hello"] `shouldBe` ["hello"]
    it "never suggests the name itself" $
      suggest "hello" ["hello"] `shouldBe` []

  describe "rendering" $ do
    let d = mkDiagnostic ENameUndefined StageStatic NoSpan "undefined name: 'helo'"
    it "adds a note after the others" $
      pretty (withSuggestions ["hello"] (withNote "first" d))
        `shouldBe` "(no location): E-NAME-UNDEFINED [static]: undefined name: 'helo'\n  note: first\n  note: did you mean 'hello'?"
    it "lists several candidates" $
      pretty (withSuggestions ["a", "b", "c"] d)
        `shouldBe` "(no location): E-NAME-UNDEFINED [static]: undefined name: 'helo'\n  note: did you mean 'a', 'b' or 'c'?"

  describe "where suggestions are offered" $ do
    it "an undefined name: top-level declarations" $
      suggests "hello = \"hi\"\nmain() = helo" ENameUndefined ["hello"]
    it "an undefined name: locals and parameters" $
      suggestionsFor [("main.lask", "f(count: Number) = do {\n  total = 1\n  cuont + totl\n}")]
        >>= (`shouldBe` [(ENameUndefined, ["count"]), (ENameUndefined, ["total"])])
    it "an undefined name: builtins" $
      suggests "main() = lenght([1])" ENameUndefined ["length"]
    it "an undefined name: imported names" $
      suggestionsFor
        [ ("main.lask", "import { python_version } from \"./tools.lask\"\nmain() = python_versoin()"),
          ("tools.lask", "python_version() = \"3\"")
        ]
        >>= (`shouldBe` [(ENameUndefined, ["python_version"])])
    it "a member of a namespace import" $
      suggestionsFor
        [ ("main.lask", "import * as tools from \"./tools.lask\"\nmain() = tools.pyhton()"),
          ("tools.lask", "python() = \"3\"\ninternal pythno() = \"2\"")
        ]
        >>= (`shouldBe` [(ENameUndefined, ["python"])])
    it "a name a named import asks for" $
      suggestionsFor
        [ ("main.lask", "import { pyhton } from \"./tools.lask\"\nmain() = 1"),
          ("tools.lask", "python() = \"3\"")
        ]
        >>= (`shouldBe` [(ENameUndefined, ["python"])])
    it "an undefined type: aliases and builtin types" $ do
      suggests "x: Strng = \"a\"" ENameUndefined ["String"]
      suggests "type Config = Record<name: String>\ny: Confg = {name: \"a\"}" ENameUndefined ["Config"]
    it "a command word that is not declared" $
      suggests "command { \"python\" } on #local\nmain() = $ pyhton -V" ETypeCommandNoEnv ["python"]
    it "an unknown keyword argument" $
      suggests "hello(--dry_run: Bool = false) = 1\nmain() = hello(dry_rnu = true)" ETypeKeyword ["dry_run"]
    it "a record field" $ do
      suggests "main() = do {\n  r = {name: \"a\", age: 3}\n  r.nmae\n}" ETypeAccess ["name"]
      suggests "main() = do {\n  r = {name: \"a\", age: 3}\n  r[\"nmae\"]\n}" ETypeAccess ["name"]
    it "an unknown image or run option" $ do
      suggests "main() = #alpine:3.20(platfrom = \"x\")" ETypeEnvConstruct ["platform"]
      suggests "main() = #alpine:3.20{memroy: \"4g\"}" ETypeEnvConstruct ["memory"]
    it "nothing when no name is close" $
      suggests "main() = zzzzzz" ENameUndefined []
