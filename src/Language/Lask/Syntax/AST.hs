{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Surface AST (spec chapters 4-6). All sugar is preserved as
-- dedicated nodes; normalization to the core language happens in a
-- later elaboration pass (spec 7.6).
--
-- Every node carries a 'Span' as a plain record field. 'stripSpans*'
-- helpers erase them for structural comparison in tests.
module Language.Lask.Syntax.AST
  ( Module (..),
    Decl (..),
    DeclF (.., DValue),
    AccessSpec (..),
    TypeParam (..),
    SBound (..),
    ClassMethod (..),
    methodParamName,
    valueMethod,
    functionOfValue,
    methodDefaultWritten,
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
    stmtBinding,
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
  | -- | @name[!!] [: Type] [within { ... }] = expr@, with its access
    -- constraint (spec 5, 10.11). 'DValue' matches one whatever its
    -- constraint, and builds one without.
    DValueW Text Secrecy (Maybe SType) (Maybe AccessSpec) Expr
  | -- | @name(params) [: Type] [within { ... }] = expr@ (sugar for a
    -- lambda binding), with the type parameters it declares (spec 4.2)
    -- and its access constraint (spec 5, 10.11).
    DFunction Text [TypeParam] [Param] (Maybe SType) (Maybe AccessSpec) Expr
  | -- | @within { ... }@ or @within Name@ (spec 5): the module
    -- constraint.
    DWithin AccessSpec
  | -- | @access Name = { ... }@ (spec 5): a named access constraint.
    -- The parser gives it braces, never a name.
    DAccess Text AccessSpec
  | -- | @export { a, b as c } from "path"@ (spec 5): a named import
    -- whose bound names are also public symbols of this module.
    DExportFrom [ImportSpec] Text
  | -- | @export * as m from "path"@ (spec 5): a namespace import that
    -- is also a public namespace of this module.
    DExportNamespace Text Text
  | -- | @command { "go", "gofmt" } on #golang:1.25 [schema e]@ (spec
    -- 5): registers each name as a command word of this module, with
    -- the command schemas @e@ gives them. Binds no value name.
    DCommand [Spanned Text] Expr (Maybe Expr)
  | -- | @import command { "go", "gofmt" } from "path"@ (spec 5): makes
    -- command words the target module exports command words of this
    -- module.
    DImportCommands [Spanned Text] Text
  | -- | @export command { "go" } from "path"@ (spec 5): an import of
    -- command words that also exports them.
    DExportCommandsFrom [Spanned Text] Text
  | -- | @class name\<T: bound\> { methods }@ (spec 4.6): a class over
    -- one type parameter, with the signatures of its methods and the
    -- bodies of those that have a default.
    DClass Text TypeParam [ClassMethod]
  | -- | @instance[\<T: bound\>] [ns.]name\<Type\> { methods }@ (spec
    -- 4.6): the type parameters of a parametric instance, the class,
    -- with the namespace it is qualified by, the type it is an
    -- instance for, and its methods, each a function declaration
    -- without type parameters.
    DInstance [TypeParam] (Maybe (Spanned Text)) (Spanned Text) SType [Decl]
  deriving (Show, Eq)

-- | A value declaration with its access constraint left out: what
-- most readers of the AST need, which a constraint does not change.
pattern DValue :: Text -> Secrecy -> Maybe SType -> Expr -> DeclF
pattern DValue n sec t e <- DValueW n sec t _ e
  where
    DValue n sec t e = DValueW n sec t Nothing e

{-# COMPLETE DImportNamed, DImportNamespace, DTypeAlias, DValue, DFunction, DWithin, DAccess, DExportFrom, DExportNamespace, DCommand, DImportCommands, DExportCommandsFrom, DClass, DInstance #-}

-- | A method of a class as declared (spec 4.6): its signature, written
-- in full, and its default body when it has one.
--
-- A method written as a value, @m: Function\<A, R\> [= e]@, keeps its
-- type and default as written in 'cmValueForm', and has here the
-- parameters and result of that function type, and as its default
-- the body of @e@ when @e@ is a lambda taking those parameters, or
-- otherwise @e@ applied to them, the parameters then named so that no
-- name in the source can be one ('methodParamName'). One whose type
-- is no function type has no parameters and that type as its result,
-- which the check of the class rejects.
data ClassMethod = ClassMethod
  { cmSpan :: Span,
    cmName :: Spanned Text,
    cmParams :: [Param],
    cmReturn :: SType,
    cmDefault :: Maybe Expr,
    cmValueForm :: Maybe (SType, Maybe Expr)
  }
  deriving (Show, Eq)

-- | The name of the @i@-th parameter of a method written as a value:
-- not an identifier, so no name the source writes captures it.
methodParamName :: Int -> Text
methodParamName i = "%" <> T.pack (show i)

-- | A method written as a value, as the signature and default it
-- stands for (spec 4.6).
valueMethod :: Span -> Spanned Text -> SType -> Maybe Expr -> ClassMethod
valueMethod sp n t d = case stypeF t of
  SFunction pts r -> case valueFunction pts d of
    Just (ps, body) -> ClassMethod sp n ps r body (Just (t, d))
    Nothing -> ClassMethod sp n [] t d (Just (t, d))
  _ -> ClassMethod sp n [] t d (Just (t, d))
  where
    valueFunction pts md = case md of
      Nothing -> Just (hidden pts, Nothing)
      Just e -> do
        (ps, body) <- functionOfValue (length pts) e
        let typed = [Param psp (PPositional v Public (Just pt)) | (Param psp (PPositional v _ _), pt) <- zip ps pts]
        pure (typed, Just body)
    hidden pts = [Param (stypeSpan pt) (PPositional (methodParamName i) Public (Just pt)) | (i, pt) <- zip [1 ..] pts]

-- | A function value as the parameters and body of a function
-- declaration of @arity@ positional parameters (spec 4.6): a lambda
-- with exactly those, unannotated and with no type parameters or
-- return type of its own, as its own parameters and body; anything
-- else applied to parameters no name in the source can be.
functionOfValue :: Int -> Expr -> Maybe ([Param], Expr)
functionOfValue arity e = case exprF e of
  ELambda [] ps Nothing body
    | length ps == arity,
      length [() | Param _ (PPositional _ Public Nothing) <- ps] == arity ->
        Just (ps, body)
  _ ->
    let ps = [Param (exprSpan e) (PPositional (methodParamName i) Public Nothing) | i <- [1 .. arity]]
     in Just (ps, Expr (exprSpan e) (ECall e [Arg (exprSpan e) (APos (Expr (exprSpan e) (EVar v))) | Param _ (PPositional v _ _) <- ps]))

-- | The default of a method as written: for one written as a value,
-- the value its default is.
methodDefaultWritten :: ClassMethod -> Maybe Expr
methodDefaultWritten cm = maybe (cmDefault cm) snd (cmValueForm cm)

-- | An access constraint as written (spec 5, 10.11): the span of its
-- braces, and each field with its value, or the name of an @access@
-- declaration, with the namespace it is qualified by, or the
-- conjunction of two or more of those, @A & { ... }@, as written. The
-- braces build no value, and the name is no type.
data AccessSpec
  = AccessSpec Span [(Spanned Text, Expr)]
  | AccessRef Span (Maybe Text) Text
  | -- | Two or more terms, none of them a conjunction.
    AccessAnd [AccessSpec]
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
  | -- | @--name[!!] : T = default@, or @--name[!!] : T@ for a
    -- required keyword parameter, which has no default (spec 7.5).
    PKeyword Text Secrecy (Maybe SType) (Maybe Expr)
  deriving (Show, Eq)

-- | A type parameter as declared, with its bound if it has one
-- (spec 4.2): @T@, @T: orderable@, @T: Number | String@.
data TypeParam = TypeParam {tpName :: Spanned Text, tpBound :: Maybe SBound}
  deriving (Show, Eq)

-- | A bound as written: a lower-case named bound or class, one
-- qualified by a namespace (@m.ord@), or a type. 'SBoundClass' is
-- never written: it is the class an inferred type parameter is bound
-- by (spec 4.3), named by its declaring module and name.
data SBound
  = SBoundNamed (Spanned Text)
  | SBoundQualified (Spanned Text) (Spanned Text)
  | SBoundType SType
  | SBoundClass FilePath Text
  | -- | @a + b@: several named bounds and classes at once (spec 4.2).
    SBoundAll [SBound]
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
    bound (SBoundQualified (Spanned _ q) (Spanned _ n)) = q <> "." <> n
    bound (SBoundType t) = renderSType t
    bound (SBoundClass _ n) = n
    bound (SBoundAll bs) = T.intercalate " + " (map bound bs)

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
  | -- | Type parameters (spec 4.2, 6.1), parameters, return type, body.
    ELambda [TypeParam] [Param] (Maybe SType) Expr
  | ECall Expr [Arg]
  | EDot Expr (Spanned Text)
  | EIndex Expr Expr
  | EBin Op Expr Expr
  | ENot Expr
  | -- | Unary minus, @-e@ (spec 6.2).
    ENeg Expr
  | EDo Block
  | -- | 'Nothing' is an @if@ without @else@, whose value is @null@
    -- (or nothing, for a @Void@ block) when the condition is false
    -- (spec 6.4).
    EIf Expr Block (Maybe Block)
  | -- | @c ? a : b@ (spec 6.2): an 'EIf' whose branches are single
    -- expressions. Kept apart from 'EIf' because its branches are
    -- expression positions, where no @return@ may appear (6.5).
    ECond Expr Expr Expr
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
-- 'TPRaw' is a hole written @#!{e}@, which only a command string has
-- (spec 6.6): the shell parses its value instead of receiving it
-- escaped.
data TextPart = TPChunk Span Text | TPInterp Expr | TPRaw Expr
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
  | -- | @name[\<T\>](params) [: Type] = expr@ (spec 6.5): a function
    -- declared in a block, which binds what @name = \\(params) -> expr@
    -- binds ('stmtBinding').
    SFunDecl Text [TypeParam] [Param] (Maybe SType) Expr
  | SExpr Expr
  deriving (Show, Eq)

-- | A statement as the binding it stands for: a function declared in
-- a block as the binding of its lambda (spec 6.5).
stmtBinding :: Stmt -> Stmt
stmtBinding st@(Stmt sp f) = case f of
  SFunDecl n tps ps rt body -> Stmt sp (SBind n Public Nothing (Expr sp (ELambda tps ps rt body)))
  _ -> st

-- Span stripping (test helpers) -------------------------------------------

stripSpansModule :: Module -> Module
stripSpansModule (Module ds ints cmds) = Module (map stripSpansDecl ds) ints cmds

stripTypeParam :: TypeParam -> TypeParam
stripTypeParam (TypeParam (Spanned _ v) b) = TypeParam (Spanned NoSpan v) (fmap stripBound b)
  where
    stripBound (SBoundNamed (Spanned _ n)) = SBoundNamed (Spanned NoSpan n)
    stripBound (SBoundQualified (Spanned _ q) (Spanned _ n)) = SBoundQualified (Spanned NoSpan q) (Spanned NoSpan n)
    stripBound (SBoundType t) = SBoundType (stripSpansType t)
    stripBound (SBoundClass p n) = SBoundClass p n
    stripBound (SBoundAll bs) = SBoundAll (map stripBound bs)

stripSpansDecl :: Decl -> Decl
stripSpansDecl (Decl _ f) = Decl NoSpan $ case f of
  DImportNamed specs path -> DImportNamed (map stripSpec specs) path
  DImportNamespace a p -> DImportNamespace a p
  DExportFrom specs path -> DExportFrom (map stripSpec specs) path
  DExportNamespace a p -> DExportNamespace a p
  DTypeAlias n ps t -> DTypeAlias n (map stripTypeParam ps) (stripSpansType t)
  DValueW n sec t w e -> DValueW n sec (fmap stripSpansType t) (fmap stripAccess w) (stripSpansExpr e)
  DFunction n tps ps t w e ->
    DFunction
      n
      (map stripTypeParam tps)
      (map stripParam ps)
      (fmap stripSpansType t)
      (fmap stripAccess w)
      (stripSpansExpr e)
  DWithin w -> DWithin (stripAccess w)
  DAccess n w -> DAccess n (stripAccess w)
  DCommand ns e sch -> DCommand (map stripWord ns) (stripSpansExpr e) (fmap stripSpansExpr sch)
  DImportCommands ns path -> DImportCommands (map stripWord ns) path
  DExportCommandsFrom ns path -> DExportCommandsFrom (map stripWord ns) path
  DClass n tp ms -> DClass n (stripTypeParam tp) (map stripMethod ms)
  DInstance tps q c t ds -> DInstance (map stripTypeParam tps) (fmap stripWord q) (stripWord c) (stripSpansType t) (map stripSpansDecl ds)
  where
    stripMethod (ClassMethod _ n ps r d vf) =
      ClassMethod NoSpan (stripWord n) (map stripParam ps) (stripSpansType r) (fmap stripSpansExpr d) (fmap (\(t, e) -> (stripSpansType t, fmap stripSpansExpr e)) vf)
    stripAccess w = case w of
      AccessSpec _ fs -> AccessSpec NoSpan [(Spanned NoSpan k, stripSpansExpr v) | (Spanned _ k, v) <- fs]
      AccessRef _ q n -> AccessRef NoSpan q n
      AccessAnd ws -> AccessAnd (map stripAccess ws)
    stripWord (Spanned _ n) = Spanned NoSpan n
    stripSpec (ImportSpec _ n a) = ImportSpec NoSpan n a

stripParam :: Param -> Param
stripParam (Param _ f) = Param NoSpan $ case f of
  PPositional n sec t -> PPositional n sec (fmap stripSpansType t)
  PVariadic n t -> PVariadic n (fmap stripSpansType t)
  PKeyword n sec t d -> PKeyword n sec (fmap stripSpansType t) (fmap stripSpansExpr d)

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
  ELambda tps ps t b -> ELambda (map stripTypeParam tps) (map stripParam ps) (fmap stripSpansType t) (stripSpansExpr b)
  ECall fn as -> ECall (stripSpansExpr fn) (map stripArg as)
  EDot e (Spanned _ n) -> EDot (stripSpansExpr e) (Spanned NoSpan n)
  EIndex e i -> EIndex (stripSpansExpr e) (stripSpansExpr i)
  EBin o a b -> EBin o (stripSpansExpr a) (stripSpansExpr b)
  ENot e -> ENot (stripSpansExpr e)
  ENeg e -> ENeg (stripSpansExpr e)
  EDo b -> EDo (stripBlock b)
  EIf c t e -> EIf (stripSpansExpr c) (stripBlock t) (fmap stripBlock e)
  ECond c a b -> ECond (stripSpansExpr c) (stripSpansExpr a) (stripSpansExpr b)
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
    stripPart (TPRaw e) = TPRaw (stripSpansExpr e)
    stripArg (Arg _ (APos e)) = Arg NoSpan (APos (stripSpansExpr e))
    stripArg (Arg _ (AKw n e)) = Arg NoSpan (AKw n (stripSpansExpr e))

stripBlock :: Block -> Block
stripBlock (Block _ ss) = Block NoSpan (map stripStmt ss)

stripStmt :: Stmt -> Stmt
stripStmt (Stmt _ f) = Stmt NoSpan $ case f of
  SBind n sec t e -> SBind n sec (fmap stripSpansType t) (stripSpansExpr e)
  SFunDecl n tps ps t e -> SFunDecl n (map stripTypeParam tps) (map stripParam ps) (fmap stripSpansType t) (stripSpansExpr e)
  SExpr e -> SExpr (stripSpansExpr e)
