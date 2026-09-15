{-# LANGUAGE OverloadedStrings #-}

-- | The command-word scan of spec 10.9. The cases mirror the worked
-- examples in that section: the table there is this module's
-- specification, so each row has a test.
module Language.Lask.Syntax.CommandWordsSpec (spec) where

import Data.Text (Text)
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST (Expr (..), ExprF (EVar), TextPart (..))
import Language.Lask.Syntax.CommandWords
import Test.Hspec

-- | Scan a command string given as literal text.
words' :: Text -> [Text]
words' t = candidates (commandWords [TPChunk NoSpan t])

-- | Scan a string with one interpolation hole spliced in.
wordsWithHole :: Text -> Text -> [Text]
wordsWithHole pre post =
  candidates $
    commandWords
      [ TPChunk NoSpan pre,
        TPInterp (Expr NoSpan (EVar "x")),
        TPChunk NoSpan post
      ]

candidates :: Analysis -> [Text]
candidates (Analysed ws) = [cwText w | w <- ws, cwCandidate w]
candidates NotAnalysable {} = []

analysable :: Text -> Bool
analysable t = case commandWords [TPChunk NoSpan t] of
  Analysed _ -> True
  NotAnalysable {} -> False

spec :: Spec
spec = do
  describe "command words (spec 10.9)" $ do
    it "takes the leading word of a single segment" $
      words' "go test -v ./..." `shouldBe` ["go"]

    it "takes the leading word of every segment" $
      words' "cd web && npm ci && npm test" `shouldBe` ["cd", "npm", "npm"]

    it "splits on ||, | and ;" $ do
      words' "a || b" `shouldBe` ["a", "b"]
      words' "a | b" `shouldBe` ["a", "b"]
      words' "a ; b" `shouldBe` ["a", "b"]
      words' "a & b" `shouldBe` ["a", "b"]

    it "skips leading assignment words" $
      words' "FOO=1 PATH=/x npm ci" `shouldBe` ["npm"]

    it "does not split a word on whitespace inside quotes" $
      words' "MSG=\"hello world\" npm ci" `shouldBe` ["npm"]

    it "ignores separators inside a quoted region" $
      words' "echo \"npm ci && go test\"" `shouldBe` ["echo"]

    it "ignores separators inside a single-quoted region" $
      words' "echo 'a && b'" `shouldBe` ["echo"]

    it "scans a command substitution and keeps it inside one word" $
      words' "npm ci --prefix $(pwd)/web" `shouldBe` ["npm", "pwd"]

    it "does not invent a command position after a substitution" $
      words' "`which go` test" `shouldBe` ["which"]

    it "scans a grouping" $
      words' "(cd web && npm ci)" `shouldBe` ["cd", "npm"]

    it "treats a wrapper as the command word, not the program it runs" $ do
      words' "sudo go build" `shouldBe` ["sudo"]
      words' "env FOO=1 go test" `shouldBe` ["env"]
      words' "timeout 60 npm ci" `shouldBe` ["timeout"]
      words' "xargs go test" `shouldBe` ["xargs"]

    it "treats shell keywords as ordinary words" $
      words' "for f in *.go; do go build $f; done" `shouldBe` ["for", "do", "done"]

    it "rejects a path as a candidate" $
      words' "/usr/bin/npm ci" `shouldBe` []

    it "rejects a quoted command word as a candidate" $
      words' "\"npm\" ci" `shouldBe` []

    it "rejects a word containing an interpolation hole" $
      wordsWithHole "" " -chdir=infra init" `shouldBe` []

    it "still finds later command words when the first is a hole" $
      wordsWithHole "" " init && npm ci" `shouldBe` ["npm"]

    it "keeps a word plain when a hole is elsewhere in the segment" $
      wordsWithHole "npm ci --prefix " "" `shouldBe` ["npm"]

    it "reports an unterminated quoted region as not analysable" $ do
      analysable "echo \"unterminated" `shouldBe` False
      analysable "echo 'unterminated" `shouldBe` False

    it "reports an unterminated nested region as not analysable" $ do
      analysable "echo $(pwd" `shouldBe` False
      analysable "echo `pwd" `shouldBe` False

    it "treats an escaped character as making the word non-plain" $
      words' "n\\pm ci" `shouldBe` []

    it "finds nothing in an empty command string" $
      words' "" `shouldBe` []

  describe "command name validity (spec chapter 5)" $ do
    it "accepts names that a command string could carry" $
      map validCommandName ["go", "docker-compose", "7z", "g++", "pip3", "mvn.cmd"]
        `shouldBe` [True, True, True, True, True, True]

    it "rejects names that could never be recognized" $
      map validCommandName ["", "my prog", "a=b", "/usr/bin/x", "foo*bar baz", "a&&b", "'q'"]
        `shouldBe` [False, False, False, False, False, False, False]
