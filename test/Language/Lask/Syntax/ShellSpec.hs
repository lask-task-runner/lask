{-# LANGUAGE OverloadedStrings #-}

-- | The shell reading of a command string (spec 6.6, "Shell syntax"):
-- each syntax error the section lists, the constructs it stops at, and
-- the simple commands it gives the command schemas of chapter 5.
module Language.Lask.Syntax.ShellSpec (spec) where

import Data.Text (Text)
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST (Arg (..), ArgF (..), Expr (..), ExprF (..), TextPart (..))
import Language.Lask.Syntax.Shell
import Test.Hspec

reading :: Text -> ShellReading
reading t = readShell [TPChunk NoSpan t]

-- | The problem a command string has, if any.
problem :: Text -> Maybe Text
problem = fmap snd . srProblem . reading

-- | A string with one hole spliced in; @raw@ writes it as @#{raw(x)}@.
withHole :: Bool -> Text -> Text -> ShellReading
withHole raw pre post =
  readShell [TPChunk NoSpan pre, TPInterp hole, TPChunk NoSpan post]
  where
    x = Expr NoSpan (EVar "x")
    hole
      | raw = Expr NoSpan (ECall (Expr NoSpan (EVar "raw")) [Arg NoSpan (APos x)])
      | otherwise = x

-- | Each simple command: its name, and its arguments as the literal
-- text they have, or @?@ when they have none.
commands :: ShellReading -> [(Text, [Text])]
commands r = [(word (scName c), map word (scArgs c)) | c <- srCommands r]
  where
    word w = if swLiteral w then swPrefix w else "?"

spec :: Spec
spec = do
  describe "well-formed command strings" $ do
    it "accepts lists, pipelines and groups" $ do
      mapM_
        (\t -> (t, problem t) `shouldBe` (t, Nothing))
        [ "go test ./...",
          "cd web && npm ci && npm test",
          "a || b; c & d",
          "a | b | c",
          "make;",
          "sleep 1 &",
          "(cd web && npm ci) > log 2>&1",
          "(a; b)",
          "FOO=1 npm ci",
          "FOO=1",
          "> out.txt",
          "cat <in >out 2>/dev/null",
          "exec 3>&- 1>&2",
          "echo $( )",
          "echo `` x",
          "echo $(( ))",
          "echo ${x:-a && b}",
          "echo \"a && b\" 'c | d'",
          "echo a\\;b",
          "a &&\nb",
          "a\n\nb"
        ]

  describe "syntax errors (spec 6.6)" $ do
    it "reports a quoted or nested region left open" $ do
      problem "echo \"abc" `shouldBe` Just "unterminated quoted region"
      problem "echo 'abc" `shouldBe` Just "unterminated quoted region"
      problem "echo $(pwd" `shouldBe` Just "unterminated command substitution"
      problem "echo `pwd" `shouldBe` Just "unterminated command substitution"
      problem "echo ${x" `shouldBe` Just "unterminated parameter expansion"
      problem "echo $((1 + 2)" `shouldBe` Just "unterminated arithmetic expansion"
      problem "(cd web" `shouldBe` Just "unterminated group"

    it "reports a ')' with no '('" $
      problem "echo a)" `shouldBe` Just "')' has no '(' to close"

    it "reports an operator with no command before it" $ do
      problem "| grep x" `shouldBe` Just "'|' has no command before it"
      problem "&& make" `shouldBe` Just "'&&' has no command before it"
      problem "; a" `shouldBe` Just "';' has no command before it"
      problem "a & && b" `shouldBe` Just "'&&' has no command before it"
      problem "a ;; b" `shouldBe` Just "';;' ends a case clause, and there is no case clause here"

    it "reports an operator with no command after it" $ do
      problem "make &&" `shouldBe` Just "'&&' is not followed by a command"
      problem "ls |" `shouldBe` Just "'|' is not followed by a command"
      problem "a || " `shouldBe` Just "'||' is not followed by a command"
      problem "a && && b" `shouldBe` Just "'&&' is not followed by a command"
      problem "a | && b" `shouldBe` Just "'|' is not followed by a command"
      problem "(a &&)" `shouldBe` Just "'&&' is not followed by a command"

    it "reports a redirection with no target" $ do
      problem "cat >" `shouldBe` Just "the redirection '>' has no target"
      problem "cat < | x" `shouldBe` Just "the redirection '<' has no target"
      problem "cat > > f" `shouldBe` Just "the redirection '>' has no target"
      problem "cat 2>&" `shouldBe` Just "the redirection '>&' has no target"

    it "reports an empty group" $
      problem "( )" `shouldBe` Just "a group must contain a command"

    it "reports an error inside a command substitution" $
      problem "echo $(| x)" `shouldBe` Just "'|' has no command before it"

  describe "where reading stops (determinacy, spec 10.9)" $ do
    it "stops at a reserved word in command position" $ do
      problem "for f in *.go; do go build $f; done &&" `shouldBe` Nothing
      problem "case $x in a) b;; esac" `shouldBe` Nothing
      problem "if true; then a; fi |" `shouldBe` Nothing
      problem "! grep -q x f &&" `shouldBe` Nothing

    it "stops at a here-document and at process substitution" $ do
      problem "cat <<EOF &&" `shouldBe` Nothing
      problem "diff <(a) >(b) |" `shouldBe` Nothing

    it "stops at a hole written with raw" $ do
      srProblem (withHole True "echo " " &&") `shouldBe` Nothing
      srProblem (withHole True "echo '" "") `shouldBe` Nothing

    it "does not stop at an escaped hole, which is one word" $ do
      fmap snd (srProblem (withHole False "echo " " &&")) `shouldBe` Just "'&&' is not followed by a command"
      fmap snd (srProblem (withHole False "echo '" "")) `shouldBe` Just "unterminated quoted region"

    it "stops at constructs only some shells have" $ do
      problem "a |& b" `shouldBe` Nothing
      problem "echo $'a\\'b' |" `shouldBe` Nothing
      problem "f() { a; }" `shouldBe` Nothing
      problem "arr=(a b) |" `shouldBe` Nothing

    it "reads a comment to the end of its line" $ do
      problem "echo hi # it's && |" `shouldBe` Nothing
      problem "echo hi # note\n| x" `shouldBe` Just "'|' has no command before it"

    it "reports what comes before the stop" $
      problem "&& if" `shouldBe` Just "'&&' has no command before it"

  describe "simple commands" $ do
    it "gives each command word with its arguments" $
      commands (reading "FOO=1 go test -v ./... 2>&1 | tee log && echo done")
        `shouldBe` [("go", ["test", "-v", "./..."]), ("tee", ["log"]), ("echo", ["done"])]

    it "includes the commands of substitutions" $
      commands (reading "npm ci --prefix $(pwd)/web")
        `shouldBe` [("npm", ["ci", "--prefix", "?"]), ("pwd", [])]

    it "removes quotes from literal words" $
      commands (reading "go test -run 'Test A' \"-v\"")
        `shouldBe` [("go", ["test", "-run", "Test A", "-v"])]

    it "keeps the literal start of a word with a hole in it" $ do
      [(swPrefix w, swLiteral w, swSingle w) | c <- srCommands (withHole False "go test -count=" ""), w <- take 1 (reverse (scArgs c))]
        `shouldBe` [("-count=", False, True)]

    it "marks an unquoted expansion as possibly several fields" $ do
      map (map swSingle . scArgs) (srCommands (reading "go test $FLAGS \"$ONE\""))
        `shouldBe` [[True, False, True]]

    it "marks the command reading stopped in as cut" $ do
      let r = withHole True "go test -run " ""
      map scCut (srCommands r) `shouldBe` [True]
      map scCut (srCommands (reading "go test")) `shouldBe` [False]
