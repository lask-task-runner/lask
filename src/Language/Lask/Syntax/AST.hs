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
    Stmt (..),
    StmtF (..),
    stripSpansModule,
    stripSpansDecl,
    stripSpansExpr,
    stripSpansType,
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Language.Lask.Lexer.Token (CmdStream, Op, Spanned (..))
import Language.Lask.Span (Span (NoSpan))

newtype Module = Module {moduleDecls :: [Decl]}
  deriving (Show, Eq)

data Decl = Decl {declSpan :: Span, declF :: DeclF}
  deriving (Show, Eq)

data DeclF
  = -- | @import { a, b as c } from "path"@
    DImportNamed [ImportSpec] Text
  | -- | @import * as m from "path"@
    DImportNamespace Text Text
  | -- | @type Name = Type@
    DTypeAlias Text SType
  | -- | @name[!!] [: Type] = expr@
    DValue Text Secrecy (Maybe SType) Expr
  | -- | @name(params) [: Type] = expr@ (sugar for a lambda binding)
    DFunction Text [Param] (Maybe SType) Expr
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
  | SArray SType
  | SMap SType
  | SRecord [(Spanned Text, SType)]
  | SAsyncHandle SType
  | -- | Parameter types and return type.
    SFunction [SType] SType
  | SNamed Text
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
  | -- | try body, optional catch (name, handler), optional finally.
    ETry Block (Maybe (Spanned Text, Block)) (Maybe Block)
  | EAsync Expr
  | EAwait Expr
  | -- | Stream selector, optional environment expression, command parts.
    ECommand CmdStream (Maybe Expr) [TextPart]
  | -- | Environment head text and constructor arguments;
    -- 'Nothing' = no argument list written (e.g. @#local@, @#alpine:3.12@).
    EEnv Text (Maybe [Arg])
  deriving (Show, Eq)

data TextPart = TPChunk Text | TPInterp Expr
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
  = -- | @name[!!] = expr@
    SBind Text Secrecy Expr
  | SExpr Expr
  | SReturn Expr
  | -- | @if (cond) { ... }@ without @else@ in statement position.
    SGuard Expr Block
  deriving (Show, Eq)

-- Span stripping (test helpers) -------------------------------------------

stripSpansModule :: Module -> Module
stripSpansModule (Module ds) = Module (map stripSpansDecl ds)

stripSpansDecl :: Decl -> Decl
stripSpansDecl (Decl _ f) = Decl NoSpan $ case f of
  DImportNamed specs path -> DImportNamed (map stripSpec specs) path
  DImportNamespace a p -> DImportNamespace a p
  DTypeAlias n t -> DTypeAlias n (stripSpansType t)
  DValue n sec t e -> DValue n sec (fmap stripSpansType t) (stripSpansExpr e)
  DFunction n ps t e ->
    DFunction n (map stripParam ps) (fmap stripSpansType t) (stripSpansExpr e)
  where
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
  SRecord fs -> SRecord [(Spanned NoSpan n, stripSpansType t) | (Spanned _ n, t) <- fs]
  SAsyncHandle t -> SAsyncHandle (stripSpansType t)
  SFunction ps r -> SFunction (map stripSpansType ps) (stripSpansType r)
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
  ETry b c fin ->
    ETry
      (stripBlock b)
      (fmap (\(Spanned _ n, h) -> (Spanned NoSpan n, stripBlock h)) c)
      (fmap stripBlock fin)
  EAsync e -> EAsync (stripSpansExpr e)
  EAwait e -> EAwait (stripSpansExpr e)
  ECommand s env ps -> ECommand s (fmap stripSpansExpr env) (map stripPart ps)
  EEnv h as -> EEnv h (fmap (map stripArg) as)
  other -> other
  where
    stripPart (TPChunk c) = TPChunk c
    stripPart (TPInterp e) = TPInterp (stripSpansExpr e)
    stripArg (Arg _ (APos e)) = Arg NoSpan (APos (stripSpansExpr e))
    stripArg (Arg _ (AKw n e)) = Arg NoSpan (AKw n (stripSpansExpr e))

stripBlock :: Block -> Block
stripBlock (Block _ ss) = Block NoSpan (map stripStmt ss)

stripStmt :: Stmt -> Stmt
stripStmt (Stmt _ f) = Stmt NoSpan $ case f of
  SBind n sec e -> SBind n sec (stripSpansExpr e)
  SExpr e -> SExpr (stripSpansExpr e)
  SReturn e -> SReturn (stripSpansExpr e)
  SGuard c b -> SGuard (stripSpansExpr c) (stripBlock b)
