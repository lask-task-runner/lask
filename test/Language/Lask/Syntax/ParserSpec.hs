{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Syntax.ParserSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Language.Lask.Lexer.Token (CmdStream (..), Op (..), Spanned (..))
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseExpr, parseModule)
import Test.Hspec hiding (Arg)

-- Span-free construction helpers -------------------------------------------

pModule :: Text -> Either String [DeclF]
pModule src = case parseModule "test.lask" src of
  Left d -> Left (show d)
  Right m -> Right [f | Decl _ f <- moduleDecls (stripSpansModule m)]

pExpr :: Text -> Either String ExprF
pExpr src = case parseExpr "test.lask" src of
  Left d -> Left (show d)
  Right e -> Right (exprF (stripSpansExpr e))

ex :: ExprF -> Expr
ex = Expr NoSpan

sp :: a -> Spanned a
sp = Spanned NoSpan

num :: Double -> Expr
num = ex . ENumber . realToFrac

str :: Text -> Expr
str t = ex (EString [TPChunk t])

var :: Text -> Expr
var = ex . EVar

posArg :: Expr -> Arg
posArg = Arg NoSpan . APos

kwArg :: Text -> Expr -> Arg
kwArg n = Arg NoSpan . AKw n

ty :: STypeF -> SType
ty = SType NoSpan

stmt :: StmtF -> Stmt
stmt = Stmt NoSpan

block :: [StmtF] -> Block
block = Block NoSpan . map stmt

spec :: Spec
spec = do
  describe "declarations" $ do
    it "parses a value declaration" $
      pModule "a = 1" `shouldBe` Right [DValue "a" Public Nothing (num 1)]

    it "parses a typed value declaration" $
      pModule "a: Number = 1"
        `shouldBe` Right [DValue "a" Public (Just (ty SNumber)) (num 1)]

    it "parses a function declaration" $
      pModule "add(x: Number, y: Number): Number = x + y"
        `shouldBe` Right
          [ DFunction
              "add"
              [ Param NoSpan (PPositional "x" Public (Just (ty SNumber))),
                Param NoSpan (PPositional "y" Public (Just (ty SNumber)))
              ]
              (Just (ty SNumber))
              (ex (EBin OpAdd (var "x") (var "y")))
          ]

    it "parses variadic and keyword parameters" $
      pModule "f(a, ...xs: Array<Number>, --n: Number = 3) = a"
        `shouldBe` Right
          [ DFunction
              "f"
              [ Param NoSpan (PPositional "a" Public Nothing),
                Param NoSpan (PVariadic "xs" (Just (ty (SArray (ty SNumber))))),
                Param NoSpan (PKeyword "n" Public (Just (ty SNumber)) (num 3))
              ]
              Nothing
              (var "a")
          ]

    it "rejects keyword parameters before positional ones" $
      pModule "f(--n = 1, a) = a" `shouldSatisfy` isLeft

    it "rejects variadic parameters with non-array annotation" $
      pModule "f(...xs: Number) = xs" `shouldSatisfy` isLeft

    it "parses the !! secret marker on a value declaration (spec 6.10)" $
      pModule "a!!: String = \"s\""
        `shouldBe` Right [DValue "a" Secret (Just (ty SString)) (str "s")]

    it "parses !! on positional and keyword parameters (spec 6.10)" $
      pModule "f(a!!: String, --n!!: String = \"d\") = a"
        `shouldBe` Right
          [ DFunction
              "f"
              [ Param NoSpan (PPositional "a" Secret (Just (ty SString))),
                Param NoSpan (PKeyword "n" Secret (Just (ty SString)) (str "d"))
              ]
              Nothing
              (var "a")
          ]

    it "rejects !! on a variadic parameter (spec 6.1)" $
      pModule "f(...xs!!: Array<String>) = xs" `shouldSatisfy` isLeft

    it "parses a type alias" $
      pModule "type Strings = Array<String>"
        `shouldBe` Right [DTypeAlias "Strings" (ty (SArray (ty SString)))]

    it "parses named imports with rename" $
      pModule "import { add, mul as times } from \"lib/math.lask\""
        `shouldBe` Right
          [ DImportNamed
              [ImportSpec NoSpan "add" Nothing, ImportSpec NoSpan "mul" (Just "times")]
              "lib/math.lask"
          ]

    it "parses namespace imports" $
      pModule "import * as m from \"lib.lask\""
        `shouldBe` Right [DImportNamespace "m" "lib.lask"]

    it "rejects renamed imports that change identifier kind" $
      pModule "import { add as Strings } from \"lib.lask\"" `shouldSatisfy` isLeft

    it "parses multiple declarations separated by newlines and semicolons" $
      pModule "a = 1; b = 2\nc = 3"
        `shouldBe` Right
          [DValue "a" Public Nothing (num 1), DValue "b" Public Nothing (num 2), DValue "c" Public Nothing (num 3)]

  describe "types" $ do
    it "parses nested generics with >> splitting" $
      pModule "xs: Array<Array<Number>> = []"
        `shouldBe` Right
          [DValue "xs" Public (Just (ty (SArray (ty (SArray (ty SNumber)))))) (ex (EArray []))]

    it "parses record types with identifier and string field names" $
      pModule "u: Record<name: String, \"X-Api-Key\": String> = u2"
        `shouldBe` Right
          [ DValue
              "u"
              Public
              (Just (ty (SRecord [(sp "name", ty SString), (sp "X-Api-Key", ty SString)])))
              (var "u2")
          ]

    it "parses function types" $
      pModule "f: Function<Number, Number, Number> = add"
        `shouldBe` Right
          [DValue "f" Public (Just (ty (SFunction [ty SNumber, ty SNumber] (ty SNumber)))) (var "add")]

    it "parses Function<R> as a nullary function type" $
      pModule "f: Function<Number> = g"
        `shouldBe` Right [DValue "f" Public (Just (ty (SFunction [] (ty SNumber)))) (var "g")]

    it "parses >= splitting after a generic type" $
      pModule "m: Map<String>= x" `shouldBe` Right [DValue "m" Public (Just (ty (SMap (ty SString)))) (var "x")]

  describe "expressions" $ do
    it "parses operator precedence: * over +" $
      pExpr "1 + 2 * 3"
        `shouldBe` Right (EBin OpAdd (num 1) (ex (EBin OpMul (num 2) (num 3))))

    it "parses comparison below arithmetic" $
      pExpr "1 + 2 == 3"
        `shouldBe` Right (EBin OpEq (ex (EBin OpAdd (num 1) (num 2))) (num 3))

    it "parses pipes at the lowest precedence" $
      pExpr "a && b |> f"
        `shouldBe` Right (EBin OpPipeR (ex (EBin OpAnd (var "a") (var "b"))) (var "f"))

    it "parses unary not tightest" $
      pExpr "!a && b"
        `shouldBe` Right (EBin OpAnd (ex (ENot (var "a"))) (var "b"))

    it "parses calls with positional and keyword arguments" $
      pExpr "f(1, c = 3)"
        `shouldBe` Right (ECall (var "f") [posArg (num 1), kwArg "c" (num 3)])

    it "parses accessor chains" $
      pExpr "a.b[0].c"
        `shouldBe` Right
          (EDot (ex (EIndex (ex (EDot (var "a") (sp "b"))) (num 0))) (sp "c"))

    it "parses lambdas" $
      pExpr "\\(x: Number): Number -> x + 1"
        `shouldBe` Right
          ( ELambda
              [Param NoSpan (PPositional "x" Public (Just (ty SNumber)))]
              (Just (ty SNumber))
              (ex (EBin OpAdd (var "x") (num 1)))
          )

    it "parses string interpolation" $
      pExpr "\"v=#{x + 1}\""
        `shouldBe` Right
          (EString [TPChunk "v=", TPInterp (ex (EBin OpAdd (var "x") (num 1)))])

    it "parses object literals" $
      pExpr "{name: \"alice\", age: 20}"
        `shouldBe` Right (EObject [(sp "name", str "alice"), (sp "age", num 20)])

    it "parses async and await" $
      pExpr "await f(1)"
        `shouldBe` Right (EAwait (ex (ECall (var "f") [posArg (num 1)])))

    it "await binds tighter than pipes" $
      pExpr "await h |> f"
        `shouldBe` Right (EBin OpPipeR (ex (EAwait (var "h"))) (var "f"))

  describe "environment expressions" $ do
    it "parses bare environment heads" $
      pExpr "#local" `shouldBe` Right (EEnv "local" Nothing)

    it "parses docker image sugar heads" $
      pExpr "#alpine:3.12" `shouldBe` Right (EEnv "alpine:3.12" Nothing)

    it "parses constructor arguments" $
      pExpr "#docker(\"alpine:3.12\", memory = \"4g\")"
        `shouldBe` Right
          (EEnv "docker" (Just [posArg (str "alpine:3.12"), kwArg "memory" (str "4g")]))

    it "does not attach a spaced ( as constructor arguments" $
      pExpr "#local (1)" `shouldSatisfy` isRight
    -- parsed as a call of the environment value; rejected later by the type checker

  describe "command expressions" $ do
    it "parses commands with environment and interpolation" $
      pExpr "$[#alpine:3.12] echo #{msg}"
        `shouldBe` Right
          ( ECommand
              StreamOut
              (Just (ex (EEnv "alpine:3.12" Nothing)))
              [TPChunk "echo ", TPInterp (var "msg")]
          )

    it "parses stream selectors" $
      pExpr "$* ls" `shouldBe` Right (ECommand StreamAll Nothing [TPChunk "ls"])

  describe "do blocks and statements" $ do
    it "parses do blocks with binds and trailing expression" $
      pExpr "do {\n  a = 1\n  a\n}"
        `shouldBe` Right (EDo (block [SBind "a" Public (num 1), SExpr (var "a")]))

    it "parses semicolon-separated statements" $
      pExpr "do { a = 1; a }"
        `shouldBe` Right (EDo (block [SBind "a" Public (num 1), SExpr (var "a")]))

    it "parses the !! secret marker on a bind statement (spec 6.10)" $
      pExpr "do { a!! = \"s\"; a }"
        `shouldBe` Right (EDo (block [SBind "a" Secret (str "s"), SExpr (var "a")]))

    it "parses empty do blocks" $
      pExpr "do {}" `shouldBe` Right (EDo (block []))

    it "parses return statements" $
      pExpr "do { return 1 }" `shouldBe` Right (EDo (block [SReturn (num 1)]))

    it "parses guard statements (if without else)" $
      pExpr "do { if (c) { return 1 }; 2 }"
        `shouldBe` Right
          ( EDo
              ( block
                  [ SGuard (var "c") (block [SReturn (num 1)]),
                    SExpr (num 2)
                  ]
              )
          )

    it "parses statement-position if-else as an expression statement" $
      pExpr "do { if (c) { 1 } else { 2 } }"
        `shouldBe` Right
          (EDo (block [SExpr (ex (EIf (var "c") (block [SExpr (num 1)]) (Just (block [SExpr (num 2)]))))]))

    it "requires else for if in expression position" $
      pModule "x = if (c) { 1 }" `shouldSatisfy` isLeft

    it "parses if-else chains applied in larger expressions" $
      pExpr "do { if (c) { 1 } else { 2 } |> f }" `shouldSatisfy` isRight

  describe "control expressions" $ do
    it "parses for expressions" $
      pExpr "for (x : xs) { concat(\"item:\", x) }"
        `shouldBe` Right
          ( EFor
              (sp "x")
              (var "xs")
              (block [SExpr (ex (ECall (var "concat") [posArg (str "item:"), posArg (var "x")]))])
          )

    it "parses try-catch-finally" $
      pExpr "try { a } catch (e) { b } finally { c }"
        `shouldBe` Right
          ( ETry
              (block [SExpr (var "a")])
              (Just (sp "e", block [SExpr (var "b")]))
              (Just (block [SExpr (var "c")]))
          )

    it "parses try-finally without catch" $
      pExpr "try { a } finally { c }"
        `shouldBe` Right (ETry (block [SExpr (var "a")]) Nothing (Just (block [SExpr (var "c")])))

    it "rejects try without catch or finally" $
      pModule "x = try { a }" `shouldSatisfy` isLeft

  describe "spec 16 style programs" $ do
    it "parses the minimal program (16.1)" $
      pModule "hello() = \"hello, lask\""
        `shouldBe` Right [DFunction "hello" [] Nothing (str "hello, lask")]

    it "parses a multi-line procedural function (16.5 style)" $
      pModule
        ( "publish(tag: String): String = do {\n"
            <> "  if (tag == \"\") { return \"skip: no tag\" }\n"
            <> "  r = $* ./release.sh #{tag}\n"
            <> "  if (r.code != 0) { return r.stderr }\n"
            <> "  \"released\"\n"
            <> "}"
        )
        `shouldSatisfy` isRight
