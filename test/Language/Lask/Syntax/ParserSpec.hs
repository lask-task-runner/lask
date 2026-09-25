{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Syntax.ParserSpec (spec) where

import Data.Either (isLeft, isRight)
import qualified Data.Set as Set
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

-- | The top-level names carrying the @internal@ marker (spec 5).
pInternal :: Text -> Either String [Text]
pInternal src = case parseModule "test.lask" src of
  Left d -> Left (show d)
  Right m -> Right (Set.toAscList (moduleInternal m))

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
str t = ex (EString [TPChunk NoSpan t])

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

carm :: [Expr] -> Expr -> CaseArm
carm hs = CaseArm NoSpan (Just (ValueHeads hs))

celse :: Expr -> CaseArm
celse = CaseArm NoSpan Nothing

-- | A function declaration with no type parameters, and a plain type
-- alias: the shapes almost every test wants (spec 4.2).
dfun :: Text -> [Param] -> Maybe SType -> Expr -> DeclF
dfun n = DFunction n []

dalias :: Text -> SType -> DeclF
dalias n = DTypeAlias n []

named :: Maybe Text -> Text -> STypeF
named q n = SNamed q n []

-- | A record field whose key must be present (spec 4.2).
fld :: Text -> SType -> (Spanned Text, Bool, SType)
fld n t = (sp n, False, t)

tarm :: [STypeF] -> Expr -> CaseArm
tarm hs = CaseArm NoSpan (Just (TypeHeads (map ty hs)))

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
          [ dfun
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
          [ dfun
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
          [ dfun
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
        `shouldBe` Right [dalias "Strings" (ty (SArray (ty SString)))]

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

    it "parses the visibility markers (spec 5)" $ do
      pModule "export a = 1" `shouldBe` Right [DValue "a" Public Nothing (num 1)]
      pModule "internal a = 1" `shouldBe` Right [DValue "a" Public Nothing (num 1)]
      pModule "internal type Strings = Array<String>"
        `shouldBe` Right [dalias "Strings" (ty (SArray (ty SString)))]

    it "records which names are marked internal (spec 5)" $ do
      pInternal "internal a = 1\nb = 2\ninternal c() = 3" `shouldBe` Right ["a", "c"]
      pInternal "export a = 1" `shouldBe` Right []

    it "parses a command declaration with its words in braces (spec 5)" $
      pModule "command { \"go\", \"gofmt\" } on e"
        `shouldBe` Right [DCommand [sp "go", sp "gofmt"] (var "e")]

    it "parses an import of command words (spec 5)" $
      pModule "import command { \"node\", \"python\" } from \"tools\""
        `shouldBe` Right [DImportCommands [sp "node", sp "python"] "tools"]

    it "parses the export forms of a command declaration (spec 5)" $ do
      pModule "export command { \"go\" } on e"
        `shouldBe` Right [DCommand [sp "go"] (var "e")]
      pModule "export command { \"go\" } from \"./lib.lask\""
        `shouldBe` Right [DExportCommandsFrom [sp "go"] "./lib.lask"]

    it "keeps internal command words apart from internal names" $ do
      let m = parseModule "t.lask" "internal command { \"helper\" } on e\ninternal x = 1"
      fmap moduleInternalCommands m `shouldBe` Right (Set.fromList ["helper"])
      fmap moduleInternal m `shouldBe` Right (Set.fromList ["x"])

    it "requires the braces around command words" $ do
      pModule "command \"go\" on e" `shouldSatisfy` isLeft
      pModule "import command \"go\" from \"tools\"" `shouldSatisfy` isLeft

    it "parses a re-export declaration (spec 5)" $
      pModule "export { a, b as c } from \"./lib.lask\""
        `shouldBe` Right
          [ DExportFrom
              [ImportSpec NoSpan "a" Nothing, ImportSpec NoSpan "b" (Just "c")]
              "./lib.lask"
          ]

    it "rejects the markers as ordinary names (spec 3.3)" $ do
      -- Reserved words since they became `Visibility` markers, so
      -- none of these is a declaration of a name any more.
      pModule "export = 1" `shouldSatisfy` isLeft
      pModule "internal(x) = x" `shouldSatisfy` isLeft
      pModule "internal: Number = 1" `shouldSatisfy` isLeft
      pModule "y = export" `shouldSatisfy` isLeft

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
              (Just (ty (SRecord [fld "name" (ty SString), fld "X-Api-Key" (ty SString)])))
              (var "u2")
          ]

    it "parses the optional marker on a field name (spec 4.2)" $
      pModule "u: Record<a?: String, \"X-Key\"?: Number, c: Bool> = u2"
        `shouldBe` Right
          [ DValue
              "u"
              Public
              ( Just
                  ( ty
                      ( SRecord
                          [ (sp "a", True, ty SString),
                            (sp "X-Key", True, ty SNumber),
                            fld "c" (ty SBool)
                          ]
                      )
                  )
              )
              (var "u2")
          ]

    it "parses function types" $
      pModule "f: Function<Number, Number, Number> = add"
        `shouldBe` Right
          [DValue "f" Public (Just (ty (SFunction [ty SNumber, ty SNumber] (ty SNumber)))) (var "add")]

    it "parses Function<R> as a nullary function type" $
      pModule "f: Function<Number> = g"
        `shouldBe` Right [DValue "f" Public (Just (ty (SFunction [] (ty SNumber)))) (var "g")]

    it "parses type parameters on a function declaration (spec 4.2)" $
      pModule "first<T>(xs: Array<T>): T = xs[0]"
        `shouldBe` Right
          [ DFunction
              "first"
              [Spanned NoSpan "T"]
              [Param NoSpan (PPositional "xs" Public (Just (ty (SArray (ty (named Nothing "T"))))))]
              (Just (ty (named Nothing "T")))
              (ex (EIndex (var "xs") (num 0)))
          ]

    it "parses several type parameters" $
      pModule "pair<A, B>(a: A, b: B): A = a"
        `shouldBe` Right
          [ DFunction
              "pair"
              [Spanned NoSpan "A", Spanned NoSpan "B"]
              [ Param NoSpan (PPositional "a" Public (Just (ty (named Nothing "A")))),
                Param NoSpan (PPositional "b" Public (Just (ty (named Nothing "B"))))
              ]
              (Just (ty (named Nothing "A")))
              (var "a")
          ]

    it "parses type parameters on a type alias, and arguments on a reference" $ do
      pModule "type Pair<A, B> = Record<first: A, second: B>"
        `shouldBe` Right
          [ DTypeAlias
              "Pair"
              [Spanned NoSpan "A", Spanned NoSpan "B"]
              (ty (SRecord [fld "first" (ty (named Nothing "A")), fld "second" (ty (named Nothing "B"))]))
          ]
      pModule "p: Pair<Number, String> = x"
        `shouldBe` Right
          [ DValue
              "p"
              Public
              (Just (ty (SNamed Nothing "Pair" [ty SNumber, ty SString])))
              (var "x")
          ]

    it "parses a union type (spec 4.2)" $
      pModule "f: String | Null = g"
        `shouldBe` Right
          [DValue "f" Public (Just (ty (SUnion [ty SString, ty SNull]))) (var "g")]

    it "parses a union inside a type argument" $
      pModule "xs: Array<String | Null> = g"
        `shouldBe` Right
          [ DValue
              "xs"
              Public
              (Just (ty (SArray (ty (SUnion [ty SString, ty SNull])))))
              (var "g")
          ]

    it "parses a union as the last argument of a function type" $
      pModule "f: Function<String, String | Null> = g"
        `shouldBe` Right
          [ DValue
              "f"
              Public
              (Just (ty (SFunction [ty SString] (ty (SUnion [ty SString, ty SNull])))))
              (var "g")
          ]

    it "keeps | and || apart" $
      pModule "b = x || y"
        `shouldBe` Right [DValue "b" Public Nothing (ex (EBin OpOr (var "x") (var "y")))]

    it "parses >= splitting after a generic type" $
      pModule "m: Map<String>= x" `shouldBe` Right [DValue "m" Public (Just (ty (SMap (ty SString)))) (var "x")]

    it "parses a bare named type" $
      pModule "u: Config = 1" `shouldBe` Right [DValue "u" Public (Just (ty (named Nothing "Config"))) (num 1)]

    it "parses a namespace-qualified named type (spec 4.2 QualifiedNamedType)" $
      pModule "u: tf.TfOutputs = 1"
        `shouldBe` Right [DValue "u" Public (Just (ty (named (Just "tf") "TfOutputs"))) (num 1)]

    it "parses a namespace-qualified type nested inside a generic" $
      pModule "xs: Array<tf.TfOutputs> = []"
        `shouldBe` Right
          [DValue "xs" Public (Just (ty (SArray (ty (named (Just "tf") "TfOutputs"))))) (ex (EArray []))]

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
          (EString [TPChunk NoSpan "v=", TPInterp (ex (EBin OpAdd (var "x") (num 1)))])

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
              [TPChunk NoSpan "echo ", TPInterp (var "msg")]
          )

    it "parses stream selectors" $
      pExpr "$*[#local] ls"
        `shouldBe` Right (ECommand StreamAll (Just (ex (EEnv "local" Nothing))) [TPChunk NoSpan "ls"])

  describe "do blocks and statements" $ do
    it "parses do blocks with binds and trailing expression" $
      pExpr "do {\n  a = 1\n  a\n}"
        `shouldBe` Right (EDo (block [SBind "a" Public Nothing (num 1), SExpr (var "a")]))

    it "parses semicolon-separated statements" $
      pExpr "do { a = 1; a }"
        `shouldBe` Right (EDo (block [SBind "a" Public Nothing (num 1), SExpr (var "a")]))

    it "parses the !! secret marker on a bind statement (spec 6.10)" $
      pExpr "do { a!! = \"s\"; a }"
        `shouldBe` Right (EDo (block [SBind "a" Secret Nothing (str "s"), SExpr (var "a")]))

    it "parses a type annotation on a bind statement (spec 6.5)" $
      pExpr "do { a: Number = 1; a }"
        `shouldBe` Right
          (EDo (block [SBind "a" Public (Just (ty SNumber)) (num 1), SExpr (var "a")]))

    it "parses a union annotation on a bind statement" $
      pExpr "do { a: String | Null = null; a }"
        `shouldBe` Right
          ( EDo
              ( block
                  [ SBind "a" Public (Just (ty (SUnion [ty SString, ty SNull]))) (ex ENull),
                    SExpr (var "a")
                  ]
              )
          )

    it "parses the !! marker before the annotation, as a ValueDecl does" $
      pExpr "do { a!!: String = \"s\"; a }"
        `shouldBe` Right
          (EDo (block [SBind "a" Secret (Just (ty SString)) (str "s"), SExpr (var "a")]))

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

    it "parses case expressions with a scrutinee (spec 6.4)" $
      pExpr "case (x) {\n  \"a\" -> 1\n  else -> 2\n}"
        `shouldBe` Right (ECase (Just (var "x")) [carm [str "a"] (num 1), celse (num 2)])

    it "parses several heads in one arm" $
      pExpr "case (x) {\n  \"a\", \"b\" -> 1\n  else -> 2\n}"
        `shouldBe` Right (ECase (Just (var "x")) [carm [str "a", str "b"] (num 1), celse (num 2)])

    it "parses type heads (spec 6.4)" $
      pExpr "case (x) {\n  Null -> 1\n  else -> 2\n}"
        `shouldBe` Right (ECase (Just (var "x")) [tarm [SNull] (num 1), celse (num 2)])

    it "parses several type heads in one arm" $
      pExpr "case (x) {\n  Number, String -> 1\n  else -> 2\n}"
        `shouldBe` Right
          (ECase (Just (var "x")) [tarm [SNumber, SString] (num 1), celse (num 2)])

    it "parses a composite type head" $
      pExpr "case (x) {\n  Array<String> -> 1\n  else -> 2\n}"
        `shouldBe` Right
          (ECase (Just (var "x")) [tarm [SArray (ty SString)] (num 1), celse (num 2)])

    it "parses the condition form without a scrutinee" $
      pExpr "case {\n  c -> 1\n  else -> 2\n}"
        `shouldBe` Right (ECase Nothing [carm [var "c"] (num 1), celse (num 2)])

    it "parses semicolon-separated arms" $
      pExpr "case (x) { 1 -> \"a\"; else -> \"b\" }"
        `shouldBe` Right (ECase (Just (var "x")) [carm [num 1] (str "a"), celse (str "b")])

    it "parses a do block as an arm body" $
      pExpr "case (x) {\n  1 -> do { a = 1; a }\n  else -> 2\n}"
        `shouldBe` Right
          ( ECase
              (Just (var "x"))
              [ carm [num 1] (ex (EDo (block [SBind "a" Public Nothing (num 1), SExpr (var "a")]))),
                celse (num 2)
              ]
          )

    it "reads an else on its own line as the else arm, not as an if branch" $
      pExpr "case (x) {\n  1 -> if (c) { 1 } else { 2 }\n  else -> 3\n}"
        `shouldBe` Right
          ( ECase
              (Just (var "x"))
              [ carm [num 1] (ex (EIf (var "c") (block [SExpr (num 1)]) (Just (block [SExpr (num 2)])))),
                celse (num 3)
              ]
          )

    it "lets an arm body's if put its else block on the next line" $
      pExpr "case (x) {\n  1 -> if (c) { 1 }\n  else { 2 }\n  else -> 3\n}"
        `shouldBe` Right
          ( ECase
              (Just (var "x"))
              [ carm [num 1] (ex (EIf (var "c") (block [SExpr (num 1)]) (Just (block [SExpr (num 2)])))),
                celse (num 3)
              ]
          )

    it "parses an empty case (the else arm is required later, in elaboration)" $
      pExpr "case (x) { }" `shouldBe` Right (ECase (Just (var "x")) [])

    it "rejects an arm without a body" $
      pModule "x = case (a) { 1 -> }" `shouldSatisfy` isLeft

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
        `shouldBe` Right [dfun "hello" [] Nothing (str "hello, lask")]

    it "parses a multi-line procedural function (16.5 style)" $
      pModule
        ( "publish(tag: String): String = do {\n"
            <> "  if (tag == \"\") { return \"skip: no tag\" }\n"
            <> "  r = $*[#local] ./release.sh #{tag}\n"
            <> "  if (r.code != 0) { return r.stderr }\n"
            <> "  \"released\"\n"
            <> "}"
        )
        `shouldSatisfy` isRight
