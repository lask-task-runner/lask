{-# LANGUAGE OverloadedStrings #-}

-- | The shell reading of a command string (spec 6.6, "Shell syntax"),
-- at the edges of each construct: quoting and escapes, the four
-- expansions, redirections, comments, and what each word is marked as.
-- "Language.Lask.Syntax.ShellSpec" holds the main cases.
module Language.Lask.Syntax.ShellReadingSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST (Expr (..), ExprF (..), TextPart (..))
import Language.Lask.Syntax.Shell
import Test.Hspec

reading :: Text -> ShellReading
reading t = readShell [TPChunk NoSpan t]

problem :: Text -> Maybe Text
problem = fmap snd . srProblem . reading

commands :: ShellReading -> [(Text, [Text])]
commands r = [(word (scName c), map word (scArgs c)) | c <- srCommands r]
  where
    word w = if swLiteral w then swPrefix w else "?"

-- | The words of the first command, name first.
wordsOf :: Text -> [ShellWord]
wordsOf t = case srCommands (reading t) of
  c : _ -> scName c : scArgs c
  [] -> error ("no command in " <> show t)

-- | Literal, plain, single, of the n-th word.
flags :: Int -> Text -> (Bool, Bool, Bool)
flags n t = let w = wordsOf t !! n in (swLiteral w, swPlain w, swSingle w)

hole :: Bool -> TextPart
hole raw = (if raw then TPRaw else TPInterp) (Expr NoSpan (EVar "x"))

spec :: Spec
spec = do
  describe "quoting (spec 6.6)" $ do
    it "removes single quotes, keeping everything inside them as it is" $
      commands (reading "echo 'a $b `c` \\d'") `shouldBe` [("echo", ["a $b `c` \\d"])]
    it "removes double quotes, and the backslash before $, `, \", \\ and a newline" $
      commands (reading "echo \"a\\$b\\`c\\\"d\\\\e\"") `shouldBe` [("echo", ["a$b`c\"d\\e"])]
    it "keeps a backslash in double quotes before any other character" $
      commands (reading "echo \"a\\nb\"") `shouldBe` [("echo", ["a\\nb"])]
    it "removes a backslash outside quotes, keeping the character after it" $
      commands (reading "echo a\\ b \\|c") `shouldBe` [("echo", ["a b", "|c"])]
    it "keeps a lone backslash at the end as itself" $
      commands (reading "echo a\\") `shouldBe` [("echo", ["a\\"])]
    it "joins quoted and unquoted parts into one word" $
      commands (reading "echo a'b'\"c\"d") `shouldBe` [("echo", ["abcd"])]
    it "reports a single or double quote left open" $ do
      problem "echo 'a" `shouldBe` Just "unterminated quoted region"
      problem "echo \"a" `shouldBe` Just "unterminated quoted region"
      problem "echo \"a\\\"" `shouldBe` Just "unterminated quoted region"

  describe "what each word is marked as (spec 6.6)" $ do
    it "marks a word with nothing special as literal, plain and single" $
      flags 1 "echo abc" `shouldBe` (True, True, True)
    it "marks a quoted or escaped word as literal and single, not plain" $ do
      flags 1 "echo 'a b'" `shouldBe` (True, False, True)
      flags 1 "echo a\\ b" `shouldBe` (True, False, True)
    it "marks an unquoted parameter expansion as possibly several fields" $ do
      flags 1 "echo $x" `shouldBe` (False, False, False)
      flags 1 "echo ${x}" `shouldBe` (False, False, False)
      flags 1 "echo $(date)" `shouldBe` (False, False, False)
    it "marks a quoted expansion as one field" $ do
      flags 1 "echo \"$x\"" `shouldBe` (False, False, True)
      flags 1 "echo \"$(date)\"" `shouldBe` (False, False, True)
    it "marks an arithmetic expansion as one field" $
      flags 1 "echo $((1 + 2))" `shouldBe` (False, False, True)
    it "marks a pattern as possibly several fields, keeping its literal start" $ do
      let w = wordsOf "ls src/*.c" !! 1
      (swPrefix w, swLiteral w, swSingle w) `shouldBe` ("src/", False, False)
    it "marks a tilde at the start of a word as an expansion, and not one inside it" $ do
      let (lit1, _, _) = flags 1 "ls ~/x"
          (lit2, _, _) = flags 1 "ls a~b"
      (lit1, lit2) `shouldBe` (False, True)
    it "marks a hole as one field, not literal" $ do
      let r = readShell [TPChunk NoSpan "echo pre", hole False, TPChunk NoSpan "post"]
      [(swPrefix w, swLiteral w, swSingle w) | c <- srCommands r, w <- scArgs c] `shouldBe` [("pre", False, True)]
    it "reads special parameters as expansions" $
      mapM_ (\p -> let (l, _, _) = flags 1 ("echo " <> p) in (p, l) `shouldBe` (p, False)) ["$@", "$*", "$#", "$?", "$$", "$!", "$-", "$0", "$1"]
    it "reads a $ that starts nothing as itself" $
      commands (reading "echo a$ $ $%") `shouldBe` [("echo", ["a$", "$", "$%"])]

  describe "expansions read to their end (spec 6.6)" $ do
    it "reads ${...} with braces, quotes and escapes inside it" $ do
      problem "echo ${x:-{a}} ok" `shouldBe` Nothing
      problem "echo ${x:-'}'} ok" `shouldBe` Nothing
      problem "echo ${x:-\"}\"} ok" `shouldBe` Nothing
      problem "echo ${x:-\\}} ok" `shouldBe` Nothing
      commands (reading "echo ${x:-'}'} ok") `shouldBe` [("echo", ["?", "ok"])]
    it "reports ${ left open" $
      problem "echo ${x" `shouldBe` Just "unterminated parameter expansion"
    it "reads $((...)) with nested parentheses" $ do
      problem "echo $(( (1 + 2) * 3 )) ok" `shouldBe` Nothing
      problem "echo $(( (1 + 2) * 3 )" `shouldBe` Just "unterminated arithmetic expansion"
    it "reads the commands of a backquoted substitution, with its escapes" $ do
      commands (reading "echo `date \\`id\\``") `shouldSatisfy` any ((== "date") . fst)
      problem "echo `date" `shouldBe` Just "unterminated command substitution"
    it "reads a substitution inside double quotes" $
      commands (reading "echo \"$(git rev-parse HEAD)\"") `shouldSatisfy` any ((== "git") . fst)
    it "reports a substitution left open" $
      problem "echo $(date" `shouldBe` Just "unterminated command substitution"
    it "reports an error inside a backquoted substitution" $
      problem "echo `a &&`" `shouldSatisfy` isJust
    it "stops at $'...', which only some shells have" $
      srCommands (reading "echo $'a' && rm x") `shouldSatisfy` (\cs -> all ((/= "rm") . swPrefix . scName) cs)

  describe "redirections (spec 6.6)" $ do
    it "reads every POSIX redirection operator, leaving its target out of the words" $
      mapM_
        (\op -> shouldRead op ("cat", ["x"]))
        [">", ">>", "<", "<>", ">|", ">&", "<&", "2>", "10>"]
    it "reads digits right before an operator as its descriptor, not a word" $
      commands (reading "cmd 2>/dev/null 1>&2") `shouldBe` [("cmd", [])]
    it "reads digits that are not right before it as a word" $
      commands (reading "cmd 2 >/dev/null") `shouldBe` [("cmd", ["2"])]
    it "reports a redirection followed by another, or by an operator" $ do
      problem "cmd > > x" `shouldSatisfy` isJust
      problem "cmd > && x" `shouldSatisfy` isJust
    it "stops at a here-document and at process substitution" $ do
      srProblem (reading "cat <<EOF") `shouldSatisfy` isNothing
      srProblem (reading "diff <(a) <(b)") `shouldSatisfy` isNothing
      srProblem (reading "tee >(a)") `shouldSatisfy` isNothing
    it "reads a redirection with no command as one" $
      problem "> out" `shouldBe` Nothing

  describe "comments (spec 6.6)" $ do
    it "reads # at the start of a word as a comment, and inside one as text" $ do
      commands (reading "echo a # b && c") `shouldBe` [("echo", ["a"])]
      commands (reading "echo a#b") `shouldBe` [("echo", ["a#b"])]
    it "ends a comment at a newline" $
      commands (reading "# a && b\necho c") `shouldBe` [("echo", ["c"])]
    it "stops at a hole in a comment, which may hold a newline" $ do
      let r = readShell [TPChunk NoSpan "echo a # ", hole False, TPChunk NoSpan " && rm x"]
      srProblem r `shouldBe` Nothing
      map (swPrefix . scName) (srCommands r) `shouldBe` ["echo"]

  describe "holes (spec 6.6)" $ do
    it "stops at a raw hole in quotes, in an expansion and after an escape" $
      mapM_
        ( \(pre, post) ->
            map (swPrefix . scName) (srCommands (readShell [TPChunk NoSpan pre, hole True, TPChunk NoSpan post]))
              `shouldNotContain` ["rm"]
        )
        [("echo '", "' && rm x"), ("echo \"", "\" && rm x"), ("echo ${y:-", "} && rm x"), ("echo $((", ")) && rm x"), ("echo \\", " && rm x"), ("echo `", "` && rm x")]
    it "reads an escaped hole as part of a word" $ do
      let r = readShell [TPChunk NoSpan "echo \\", hole False, TPChunk NoSpan " && ls"]
      srProblem r `shouldBe` Nothing
      map (swPrefix . scName) (srCommands r) `shouldBe` ["echo", "ls"]

  describe "lists and groups (spec 6.6)" $ do
    it "reads ;; as an operator, which only case takes" $
      problem "a ;; b" `shouldSatisfy` isJust
    it "stops at |&, which only some shells have" $
      srProblem (reading "a |& b") `shouldBe` Nothing
    it "reads a group followed by a redirection, and stops at a word after it" $ do
      problem "(a) > out" `shouldBe` Nothing
      srProblem (reading "(a) b") `shouldBe` Nothing
    it "stops at a ( inside a word, as in a function definition" $
      srProblem (reading "f() { a; }") `shouldBe` Nothing
    it "reads a carriage return as a newline" $
      problem "a\r\nb" `shouldSatisfy` isJust
  where
    shouldRead op expected =
      let t = "cat " <> op <> " t x"
       in (op, commands (reading t), problem t) `shouldBe` (op, [expected], Nothing)
