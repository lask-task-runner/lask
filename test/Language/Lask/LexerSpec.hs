{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.LexerSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Language.Lask.Lexer (lexLayout, lexTokens)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Span (NoSpan))
import Test.Hspec

-- | Tokenize (no layout) and strip spans.
lexed :: Text -> Either String [Token]
lexed src = either (const (Left "lex error")) (Right . stripTokens) (lexTokens "test.lask" src)

-- | Tokenize, apply layout, strip spans.
laid :: Text -> Either String [Token]
laid src = either (const (Left "lex error")) (Right . stripTokens) (lexLayout "test.lask" src)

num :: Double -> Token
num = TNumber . realToFrac

lid :: Text -> Token
lid = TLowerId

chunk1 :: Text -> [StrPart]
chunk1 t = [Chunk t]

-- | Nested tokens inside 'Interp' / command environments come back
-- from 'stripTokens' wrapped with 'NoSpan'.
nt :: Token -> Spanned Token
nt = Spanned NoSpan

spec :: Spec
spec = do
  describe "basic tokens" $ do
    it "lexes a simple declaration" $
      lexed "a = 1" `shouldBe` Right [lid "a", TAssign, num 1]

    it "lexes numbers with fraction" $
      lexed "1.5" `shouldBe` Right [num 1.5]

    it "does not take an accessor dot into a number" $
      lexed "1.foo" `shouldBe` Right [num 1, TDot, lid "foo"]

    it "has no signed numbers: leading minus is an operator" $
      lexed "-1" `shouldBe` Right [TOp OpSub, num 1]

    it "lexes identifiers with underscores and digits" $
      lexed "show_version2" `shouldBe` Right [lid "show_version2"]

    it "distinguishes upper identifiers" $
      lexed "Number x" `shouldBe` Right [TUpperId "Number", lid "x"]

    it "lexes keywords, not identifiers" $
      lexed "if else do type import from as async await for return try catch finally"
        `shouldBe` Right
          ( map
              TKw
              [KIf, KElse, KDo, KType, KImport, KFrom, KAs, KAsync, KAwait, KFor, KReturn, KTry, KCatch, KFinally]
          )

    it "keywords are matched whole-word only" $
      lexed "iffy forx" `shouldBe` Right [lid "iffy", lid "forx"]

    it "lexes literal keywords" $
      lexed "true false null" `shouldBe` Right [TBool True, TBool False, TNull]

  describe "comments" $ do
    it "skips line comments up to the newline" $
      laid "a = 1 // comment\nb = 2"
        `shouldBe` Right [lid "a", TAssign, num 1, TNewline, lid "b", TAssign, num 2]

    it "skips nested block comments" $
      lexed "a /* x /* y */ z */ b" `shouldBe` Right [lid "a", lid "b"]

    it "fails on unterminated block comments" $
      lexed "/* /* */" `shouldSatisfy` isLeft

  describe "operators and punctuation" $ do
    it "lexes all binary operators" $
      lexed "* / + - == != < <= > >= && || |> <| >> <<"
        `shouldBe` Right
          ( map
              TOp
              [OpMul, OpDiv, OpAdd, OpSub, OpEq, OpNe, OpLt, OpLe, OpGt, OpGe, OpAnd, OpOr, OpPipeR, OpPipeL, OpCompR, OpCompL]
          )

    it "lexes a-b as subtraction" $
      lexed "a-b" `shouldBe` Right [lid "a", TOp OpSub, lid "b"]

    it "lexes -- as the keyword parameter marker" $
      lexed "--name" `shouldBe` Right [TDashDash, lid "name"]

    it "lexes ... arrow and lambda tokens" $
      lexed "\\(...xs) -> xs"
        `shouldBe` Right [TBackslash, TLParen, TEllipsis, lid "xs", TRParen, TArrow, lid "xs"]

  describe "strings" $ do
    it "lexes raw strings without escape processing" $
      lexed "'a\\nb'" `shouldBe` Right [TRawString "a\\nb"]

    it "allows newlines inside raw strings" $
      lexed "'a\nb'" `shouldBe` Right [TRawString "a\nb"]

    it "lexes interpreted strings with escapes" $
      lexed "\"a\\n\\t\\\"b\\\\\"" `shouldBe` Right [TString (chunk1 "a\n\t\"b\\")]

    it "supports unicode escapes" $
      lexed "\"\\u{41}\"" `shouldBe` Right [TString (chunk1 "A")]

    it "rejects unknown escapes" $
      lexed "\"\\q\"" `shouldSatisfy` isLeft

    it "lexes interpolation as a nested token stream" $
      lexed "\"x#{a + 1}y\""
        `shouldBe` Right
          [TString [Chunk "x", Interp (map nt [lid "a", TOp OpAdd, num 1]), Chunk "y"]]

    it "handles nested braces inside interpolation" $
      lexed "\"#{ {a: 1}.a }\""
        `shouldBe` Right
          [TString [Interp (map nt [TLBrace, lid "a", TColon, num 1, TRBrace, TDot, lid "a"])]]

    it "treats a lone # in a string as literal" $
      lexed "\"a#b\"" `shouldBe` Right [TString (chunk1 "a#b")]

    it "rejects raw newline in interpreted strings" $
      lexed "\"a\nb\"" `shouldSatisfy` isLeft

  describe "environment heads" $ do
    it "lexes simple environment heads" $
      lexed "#local" `shouldBe` Right [TEnvHead "local"]

    it "lexes docker image references" $
      lexed "#alpine:3.12" `shouldBe` Right [TEnvHead "alpine:3.12"]

    it "lexes digest references with @" $
      lexed "#ubuntu@sha256:abc" `shouldBe` Right [TEnvHead "ubuntu@sha256:abc"]

    it "terminates at a non-env character (longest match)" $
      lexed "[#alpine:3.12]" `shouldBe` Right [TLBracket, TEnvHead "alpine:3.12", TRBracket]

    it "rejects a bare #" $
      lexed "# " `shouldSatisfy` isLeft

  describe "command expressions" $ do
    it "lexes $ with stdout stream by default" $
      lexed "$ echo hi" `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "echo hi")]

    it "lexes explicit stream selectors" $ do
      lexed "$1 ls" `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "ls")]
      lexed "$2 ls" `shouldBe` Right [TCommand StreamErr Nothing (chunk1 "ls")]
      lexed "$* ls" `shouldBe` Right [TCommand StreamAll Nothing (chunk1 "ls")]

    it "captures the environment token stream" $
      lexed "$[#alpine:3.12] uname -a"
        `shouldBe` Right
          [TCommand StreamOut (Just [nt (TEnvHead "alpine:3.12")]) (chunk1 "uname -a")]

    it "captures interpolation inside command strings" $
      lexed "$ echo #{name} done"
        `shouldBe` Right
          [TCommand StreamOut Nothing [Chunk "echo ", Interp [nt (lid "name")], Chunk " done"]]

    it "escapes literal \\#{ in command strings" $
      lexed "$ echo \\#{x}"
        `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "echo #{x}")]

    it "treats a lone # in commands as literal" $
      lexed "$ echo a#b" `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "echo a#b")]

    it "joins continuation lines with backslash-newline" $
      lexed "$ echo a\\\nb" `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "echo ab")]

    it "trims outer whitespace but keeps inner whitespace" $
      lexed "$   echo   a   " `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "echo   a")]

    it "terminates the command string at the newline" $
      laid "v = $ ls\nv"
        `shouldBe` Right
          [lid "v", TAssign, TCommand StreamOut Nothing (chunk1 "ls"), TNewline, lid "v"]

    it "keeps shell syntax unescaped" $
      lexed "$ ls | grep 'x; y'"
        `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "ls | grep 'x; y'")]

    it "does not take a spaced [ as environment" $
      lexed "$ [ls]" `shouldBe` Right [TCommand StreamOut Nothing (chunk1 "[ls]")]

  describe "layout: continuation rules (spec 6.5)" $ do
    it "keeps newlines between top-level declarations" $
      laid "a = 1\nb = 2"
        `shouldBe` Right [lid "a", TAssign, num 1, TNewline, lid "b", TAssign, num 2]

    it "collapses blank lines" $
      laid "a = 1\n\n\nb = 2"
        `shouldBe` Right [lid "a", TAssign, num 1, TNewline, lid "b", TAssign, num 2]

    it "continues after a trailing =" $
      laid "x =\n  1" `shouldBe` Right [lid "x", TAssign, num 1]

    it "continues after a trailing binary operator" $
      laid "x = 1 +\n  2" `shouldBe` Right [lid "x", TAssign, num 1, TOp OpAdd, num 2]

    it "continues after a trailing comma inside parens" $
      laid "f(1,\n  2)" `shouldBe` Right [lid "f", TLParen, num 1, TComma, num 2, TRParen]

    it "continues when the next line starts with a binary operator" $
      laid "xs\n  |> f" `shouldBe` Right [lid "xs", TOp OpPipeR, lid "f"]

    it "continues before else" $
      laid "if (a) { 1 }\nelse { 2 }"
        `shouldBe` Right
          [TKw KIf, TLParen, lid "a", TRParen, TLBrace, num 1, TRBrace, TKw KElse, TLBrace, num 2, TRBrace]

    it "continues before catch and finally" $
      laid "try { a }\ncatch (e) { b }\nfinally { c }"
        `shouldBe` Right
          [ TKw KTry, TLBrace, lid "a", TRBrace,
            TKw KCatch, TLParen, lid "e", TRParen, TLBrace, lid "b", TRBrace,
            TKw KFinally, TLBrace, lid "c", TRBrace
          ]

    it "drops newlines inside parens brackets and object braces" $
      laid "h = {\n  a: 1,\n  b: [\n    2\n  ]\n}"
        `shouldBe` Right
          [lid "h", TAssign, TLBrace, lid "a", TColon, num 1, TComma, lid "b", TColon, TLBracket, num 2, TRBracket, TRBrace]

    it "keeps newlines between statements inside do blocks" $
      laid "f() = do {\n  a = 1\n  a\n}"
        `shouldBe` Right
          [ lid "f", TLParen, TRParen, TAssign, TKw KDo, TLBrace,
            lid "a", TAssign, num 1, TNewline, lid "a", TRBrace
          ]

    it "keeps newlines inside if blocks (block braces, not object braces)" $
      laid "if (c) {\n  a = 1\n  a\n} else { 2 }"
        `shouldBe` Right
          [ TKw KIf, TLParen, lid "c", TRParen, TLBrace,
            lid "a", TAssign, num 1, TNewline, lid "a", TRBrace,
            TKw KElse, TLBrace, num 2, TRBrace
          ]

    it "does not continue onto a leading ( — new statement instead" $
      laid "a = f\n(1)"
        `shouldBe` Right [lid "a", TAssign, lid "f", TNewline, TLParen, num 1, TRParen]

    it "does not continue onto a leading [ — new statement instead" $
      laid "a = f\n[1]"
        `shouldBe` Right [lid "a", TAssign, lid "f", TNewline, TLBracket, num 1, TRBracket]

    it "drops leading and trailing newlines of the file" $
      laid "\n\na = 1\n\n" `shouldBe` Right [lid "a", TAssign, num 1]

    it "command-terminating newline also terminates the statement" $
      laid "f() = do {\n  v = $ git --version\n  v\n}"
        `shouldBe` Right
          [ lid "f", TLParen, TRParen, TAssign, TKw KDo, TLBrace,
            lid "v", TAssign, TCommand StreamOut Nothing (chunk1 "git --version"), TNewline,
            lid "v", TRBrace
          ]

    it "a command can be continued by a next-line pipe" $
      laid "v = $ git --version\n  |> trim"
        `shouldBe` Right
          [lid "v", TAssign, TCommand StreamOut Nothing (chunk1 "git --version"), TOp OpPipeR, lid "trim"]

    it "keyword import braces are continuation regions" $
      laid "import {\n  a,\n  b\n} from \"lib.lask\""
        `shouldBe` Right
          [ TKw KImport, TLBrace, lid "a", TComma, lid "b", TRBrace,
            TKw KFrom, TString (chunk1 "lib.lask")
          ]
