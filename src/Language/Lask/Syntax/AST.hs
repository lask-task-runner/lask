{-# LANGUAGE OverloadedStrings #-}

-- | Surface AST (spec chapters 4-6). All sugar is preserved as
-- dedicated nodes; normalization to the core language happens in a
-- later elaboration pass (spec 7.6).
--
-- Every node carries a 'Span' as a plain record field. 'stripSpans*'
-- helpers erase them for structural comparison in tests.
module Language.Lask.Syntax.AST
  ( Module (..),
    Decl (..),
    DeclF (..),
    TypeParam (..),
    SBound (..),
    typeParamNames,
    renderTypeParams,
    renderSType,
    builtinTypeNames,
    ImportSpec (..),
    Secrecy (..),
    Param (..),
    ParamF (..),
    SType (..),
    STypeF (..),
    Expr (..),
    ExprF (..),
    TextPart (..),
    Arg (..),
    ArgF (..),
    Block (..),
    CaseArm (..),
    CaseHeads (..),
    Stmt (..),
    StmtF (..),
    stripSpansModule,
    stripSpansDecl,
    stripSpansExpr,
    stripSpansType,
  )
where

import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Lexer.Token (CmdStream, Op, Spanned (..))
import Language.Lask.Span (Span (NoSpan))

data Module = Module
  { moduleDecls :: [Decl],
    -- | Top-level names carrying the @internal@ marker (spec 5). They
    -- are not public symbols of the module.
    moduleInternal :: Set Text,
    -- | Command words declared by an @internal@ command declaration
    -- (spec 5). Command words occupy a namespace of their own, so they
    -- are kept apart from the value and type names above.
    moduleInternalCommands :: Set Text
  }
  deriving (Show, Eq)

data Decl = Decl {declSpan :: Span, declF :: DeclF}
  deriving (Show, Eq)

data DeclF
  = -- | @import { a, b as c } from "path"@
    DImportNamed [ImportSpec] Text
  | -- | @import * as m from "path"@
    DImportNamespace Text Text
  | -- | @type Name = Type@, or @type Name\<A, B\> = Type@ with type
    -- parameters (spec 4.2).
    DTypeAlias Text [TypeParam] SType
  | -- | @name[!!] [: Type] = expr@
    DValue Text Secrecy (Maybe SType) Expr
  | -- | @name(params) [: Type] = expr@ (sugar for a lambda binding),
    -- with the type parameters it declares (spec 4.2).
    DFunction Text [TypeParam] [Param] (Maybe SType) Expr
  | -- | @export { a, b as c } from "path"@ (spec 5): a named import
    -- whose bound names are also public symbols of this module.
    DExportFrom [ImportSpec] Text
  | -- | @command { "go", "gofmt" } on #golang:1.25@ (spec 5):
    -- registers each name as a command word of this module. Binds no
    -- value name.
    DCommand [Spanned Text] Expr
  | -- | @import command { "go", "gofmt" } from "path"@ (spec 5): makes
    -- command words the target module exports command words of this
    -- module.
    DImportCommands [Spanned Text] Text
  | -- | @export command { "go" } from "path"@ (spec 5): an import of
    -- command words that also exports them.
    DExportCommandsFrom [Spanned Text] Text
  deriving (Show, Eq)

-- | Whether a binding carries the @!!@ secret marker (spec 6.10).
-- Only the binding is marked; the bound type is unaffected.
data Secrecy = Public | Secret
  deriving (Show, Eq)

data ImportSpec = ImportSpec
  { importSpecSpan :: Span,
    importSpecName :: Text,
    importSpecAlias :: Maybe Text
  }
  deriving (Show, Eq)

data Param = Param {paramSpan :: Span, paramF :: ParamF}
  deriving (Show, Eq)

data ParamF
  = -- | @name[!!] : T@
    PPositional Text Secrecy (Maybe SType)
  | -- | @...name : Array\<T\>@. Cannot be marked @!!@ (spec 6.1).
    PVariadic Text (Maybe SType)
  | -- | @--name[!!] : T = default@
    PKeyword Text Secrecy (Maybe SType) Expr
  deriving (Show, Eq)

-- | A type parameter as declared, with its bound if it has one
-- (spec 4.2): @T@, @T: orderable@, @T: Number | String@.
data TypeParam = TypeParam {tpName :: Spanned Text, tpBound :: Maybe SBound}
  deriving (Show, Eq)

-- | A bound as written: a lower-case named bound, or a type.
data SBound = SBoundNamed (Spanned Text) | SBoundType SType
  deriving (Show, Eq)

typeParamNames :: [TypeParam] -> [Text]
typeParamNames tps = [v | TypeParam (Spanned _ v) _ <- tps]

-- | The binder as it is written on a declaration, @\<T: orderable\>@,
-- or empty when there are no type parameters (spec 11.6).
renderTypeParams :: [TypeParam] -> Text
renderTypeParams [] = ""
renderTypeParams tps = "<" <> T.intercalate ", " (map one tps) <> ">"
  where
    one (TypeParam (Spanned _ v) b) = v <> maybe "" ((": " <>) . bound) b
    bound (SBoundNamed (Spanned _ n)) = n
    bound (SBoundType t) = renderSType t

-- | The type names the grammar itself recognises (spec 4.2), which no
-- alias declares.
builtinTypeNames :: [Text]
builtinTypeNames =
  [ "Any", "Number", "String", "Bool", "Null", "Void", "Environment", "Runnable",
    "Array", "Map", "AsyncHandle", "Record", "Function"
  ]

-- | A type as written, in the notation of 4.2.
renderSType :: SType -> Text
renderSType (SType _ f) = case f of
  SAny -> "Any"
  SNumber -> "Number"
  SString -> "String"
  SBool -> "Bool"
  SNull -> "Null"
  SVoid -> "Void"
  SEnvironment -> "Environment"
  SRunnable -> "Runnable"
  SArray t -> "Array<" <> renderSType t <> ">"
  SMap t -> "Map<" <> renderSType t <> ">"
  SRecord fs ->
    "Record<"
      <> T.intercalate ", " [k <> (if opt then "?" else "") <> ": " <> renderSType t | (Spanned _ k, opt, t) <- fs]
      <> ">"
  SAsyncHandle t -> "AsyncHandle<" <> renderSType t <> ">"
  SFunction ps r -> "Function<" <> T.intercalate ", " (map renderSType (ps <> [r])) <> ">"
  SNamed q n as ->
    maybe n (\ns -> ns <> "." <> n) q
      <> (if null as then "" else "<" <> T.intercalate ", " (map renderSType as) <> ">")
  SUnion ts -> T.intercalate " | " (map renderSType ts)

data SType = SType {stypeSpan :: Span, stypeF :: STypeF}
  deriving (Show, Eq)

data STypeF
  = SAny
  | SNumber
  | SString
  | SBool
  | SNull
  | SVoid
  | SEnvironment
  | SRunnable
  | SArray SType
  | SMap SType
  | -- | Fields as written: name, whether it carries the optional
    -- marker @?@ (spec 4.2), and its type.
    SRecord [(Spanned Text, Bool, SType)]
  | SAsyncHandle SType
  | -- | Parameter types and return type.
    SFunction [SType] SType
  | -- | @Nothing@: bare @upper_id@. @Just ns@: qualified @ns.TypeName@,
    -- a reference to a public type alias of the module the namespace
    -- import @ns@ refers to (spec 4.2 QualifiedNamedType).
    -- Type arguments are given where the alias takes parameters
    -- (@Pair\<Number, String\>@); the list is empty otherwise.
    SNamed (Maybe Text) Text [SType]
  | -- | @T1 | T2 | ...@ as written, with at least two members (spec
    -- 4.2). Canonicalization happens when it becomes a semantic type.
    SUnion [SType]
  deriving (Show, Eq)

data Expr = Expr {exprSpan :: Span, exprF :: ExprF}
  deriving (Show, Eq)

data ExprF
  = ENull
  | EBool Bool
  | ENumber Scientific
  | -- | Interpreted or raw string; raw strings become a single chunk.
    EString [TextPart]
  | EVar Text
  | EArray [Expr]
  | EObject [(Spanned Text, Expr)]
  | ELambda [Param] (Maybe SType) Expr
  | ECall Expr [Arg]
  | EDot Expr (Spanned Text)
  | EIndex Expr Expr
  | EBin Op Expr Expr
  | ENot Expr
  | EDo Block
  | -- | @else@ is mandatory in expression position; 'Nothing' only
    -- occurs for the statement-position guard form (spec 6.4/6.5).
    EIf Expr Block (Maybe Block)
  | EFor (Spanned Text) Expr Block
  | -- | @case (e) { p -> b ... else -> b }@ (spec 6.4). 'Nothing' as
    -- the scrutinee is the condition form, whose arm heads are @Bool@
    -- conditions rather than values compared with the scrutinee.
    ECase (Maybe Expr) [CaseArm]
  | -- | try body, optional catch (name, handler), optional finally.
    ETry Block (Maybe (Spanned Text, Block)) (Maybe Block)
  | EAsync Expr
  | EAwait Expr
  | -- | Stream selector, optional environment expression, command parts.
    ECommand CmdStream (Maybe Expr) [TextPart]
  | -- | Environment head text, its image options in parentheses, and
    -- the run options in braces that make it a runnable (spec 6.7);
    -- 'Nothing' = not written (e.g. @#local@, @#alpine:3.12@). A brace
    -- entry @k: v@ is carried as the keyword argument @k = v@ of the
    -- @runnable@ call it stands for.
    EEnv Text (Maybe [Arg]) (Maybe [Arg])
  deriving (Show, Eq)

-- | A piece of a string or command string. 'TPChunk' carries the
-- source span it was lexed from, so a position inside it can be
-- recovered by walking its text (spec 10.9 command words).
data TextPart = TPChunk Span Text | TPInterp Expr
  deriving (Show, Eq)

-- | One arm of an 'ECase'. 'Nothing' as the heads marks the @else@
-- arm, which the elaborator requires to be present exactly once and
-- last (spec 6.4). An arm with several heads matches any of them.
data CaseArm = CaseArm
  { caseArmSpan :: Span,
    caseArmHeads :: Maybe CaseHeads,
    caseArmBody :: Expr
  }
  deriving (Show, Eq)

-- | The heads of one arm (spec 6.4). A value head is compared with the
-- scrutinee by equality; a type head dispatches on its runtime type.
-- The two are never mixed within one arm: a head beginning with an
-- @upper_id@ is a type, and an @upper_id@ cannot begin an expression.
data CaseHeads = ValueHeads [Expr] | TypeHeads [SType]
  deriving (Show, Eq)

data Arg = Arg {argSpan :: Span, argF :: ArgF}
  deriving (Show, Eq)

data ArgF = APos Expr | AKw Text Expr
  deriving (Show, Eq)

data Block = Block {blockSpan :: Span, blockStmts :: [Stmt]}
  deriving (Show, Eq)

data Stmt = Stmt {stmtSpan :: Span, stmtF :: StmtF}
  deriving (Show, Eq)

data StmtF
  = -- | @name[!!] [: Type] = expr@
    SBind Text Secrecy (Maybe SType) Expr
  | SExpr Expr
  | SReturn Expr
  | -- | @if (cond) { ... }@ without @else@ in statement position.
    SGuard Expr Block
  deriving (Show, Eq)

-- Span stripping (test helpers) -------------------------------------------

stripSpansModule :: Module -> Module
stripSpansModule (Module ds ints cmds) = Module (map stripSpansDecl ds) ints cmds

stripTypeParam :: TypeParam -> TypeParam
stripTypeParam (TypeParam (Spanned _ v) b) = TypeParam (Spanned NoSpan v) (fmap stripBound b)
  where
    stripBound (SBoundNamed (Spanned _ n)) = SBoundNamed (Spanned NoSpan n)
    stripBound (SBoundType t) = SBoundType (stripSpansType t)

stripSpansDecl :: Decl -> Decl
stripSpansDecl (Decl _ f) = Decl NoSpan $ case f of
  DImportNamed specs path -> DImportNamed (map stripSpec specs) path
  DImportNamespace a p -> DImportNamespace a p
  DExportFrom specs path -> DExportFrom (map stripSpec specs) path
  DTypeAlias n ps t -> DTypeAlias n (map stripTypeParam ps) (stripSpansType t)
  DValue n sec t e -> DValue n sec (fmap stripSpansType t) (stripSpansExpr e)
  DFunction n tps ps t e ->
    DFunction
      n
      (map stripTypeParam tps)
      (map stripParam ps)
      (fmap stripSpansType t)
      (stripSpansExpr e)
  DCommand ns e -> DCommand (map stripWord ns) (stripSpansExpr e)
  DImportCommands ns path -> DImportCommands (map stripWord ns) path
  DExportCommandsFrom ns path -> DExportCommandsFrom (map stripWord ns) path
  where
    stripWord (Spanned _ n) = Spanned NoSpan n
    stripSpec (ImportSpec _ n a) = ImportSpec NoSpan n a

stripParam :: Param -> Param
stripParam (Param _ f) = Param NoSpan $ case f of
  PPositional n sec t -> PPositional n sec (fmap stripSpansType t)
  PVariadic n t -> PVariadic n (fmap stripSpansType t)
  PKeyword n sec t d -> PKeyword n sec (fmap stripSpansType t) (stripSpansExpr d)

stripSpansType :: SType -> SType
stripSpansType (SType _ f) = SType NoSpan $ case f of
  SArray t -> SArray (stripSpansType t)
  SMap t -> SMap (stripSpansType t)
  SRecord fs -> SRecord [(Spanned NoSpan n, opt, stripSpansType t) | (Spanned _ n, opt, t) <- fs]
  SAsyncHandle t -> SAsyncHandle (stripSpansType t)
  SFunction ps r -> SFunction (map stripSpansType ps) (stripSpansType r)
  SUnion ts -> SUnion (map stripSpansType ts)
  SNamed q n as -> SNamed q n (map stripSpansType as)
  other -> other

stripSpansExpr :: Expr -> Expr
stripSpansExpr (Expr _ f) = Expr NoSpan $ case f of
  EString ps -> EString (map stripPart ps)
  EArray es -> EArray (map stripSpansExpr es)
  EObject kvs -> EObject [(Spanned NoSpan k, stripSpansExpr v) | (Spanned _ k, v) <- kvs]
  ELambda ps t b -> ELambda (map stripParam ps) (fmap stripSpansType t) (stripSpansExpr b)
  ECall fn as -> ECall (stripSpansExpr fn) (map stripArg as)
  EDot e (Spanned _ n) -> EDot (stripSpansExpr e) (Spanned NoSpan n)
  EIndex e i -> EIndex (stripSpansExpr e) (stripSpansExpr i)
  EBin o a b -> EBin o (stripSpansExpr a) (stripSpansExpr b)
  ENot e -> ENot (stripSpansExpr e)
  EDo b -> EDo (stripBlock b)
  EIf c t e -> EIf (stripSpansExpr c) (stripBlock t) (fmap stripBlock e)
  EFor (Spanned _ x) xs b -> EFor (Spanned NoSpan x) (stripSpansExpr xs) (stripBlock b)
  ECase scrut arms -> ECase (fmap stripSpansExpr scrut) (map stripArm arms)
  ETry b c fin ->
    ETry
      (stripBlock b)
      (fmap (\(Spanned _ n, h) -> (Spanned NoSpan n, stripBlock h)) c)
      (fmap stripBlock fin)
  EAsync e -> EAsync (stripSpansExpr e)
  EAwait e -> EAwait (stripSpansExpr e)
  ECommand s env ps -> ECommand s (fmap stripSpansExpr env) (map stripPart ps)
  EEnv h as os -> EEnv h (fmap (map stripArg) as) (fmap (map stripArg) os)
  other -> other
  where
    stripArm (CaseArm _ hs b) = CaseArm NoSpan (fmap stripHeads hs) (stripSpansExpr b)
    stripHeads (ValueHeads es) = ValueHeads (map stripSpansExpr es)
    stripHeads (TypeHeads ts) = TypeHeads (map stripSpansType ts)
    stripPart (TPChunk _ c) = TPChunk NoSpan c
    stripPart (TPInterp e) = TPInterp (stripSpansExpr e)
    stripArg (Arg _ (APos e)) = Arg NoSpan (APos (stripSpansExpr e))
    stripArg (Arg _ (AKw n e)) = Arg NoSpan (AKw n (stripSpansExpr e))

stripBlock :: Block -> Block
stripBlock (Block _ ss) = Block NoSpan (map stripStmt ss)

stripStmt :: Stmt -> Stmt
stripStmt (Stmt _ f) = Stmt NoSpan $ case f of
  SBind n sec t e -> SBind n sec (fmap stripSpansType t) (stripSpansExpr e)
  SExpr e -> SExpr (stripSpansExpr e)
  SReturn e -> SReturn (stripSpansExpr e)
  SGuard c b -> SGuard (stripSpansExpr c) (stripBlock b)
