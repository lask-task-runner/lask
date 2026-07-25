-- | Core AST: the target of static desugaring (spec 7.6). All sugar
-- (@if@\/@for@\/@try@\/@do@ sequencing of @return@\/commands\/@async@)
-- has been normalized; applications are positional plus statically
-- validated keyword arguments; every callable carries the information
-- needed for runtime binding (8.3) and 'FunctionRef' serialization
-- (13.2).
module Language.Lask.Core.AST
  ( Core (..),
    CoreF (..),
    CorePart (..),
    CoreStmt (..),
    VarRef (..),
    IndexKind (..),
    PrimOp (..),
    Lam (..),
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Language.Lask.Span (Span)
import Language.Lask.Types (Type)

data Core = Core {coreSpan :: Span, coreF :: CoreF}
  deriving (Show, Eq)

data VarRef
  = LocalRef Text
  | -- | Top-level declaration: module key and name.
    TopRef FilePath Text
  | -- | Builtin symbol (including the reserved @stdin@).
    BuiltinRef Text
  deriving (Show, Eq)

data CoreF
  = CNull
  | CBool Bool
  | CNumber Scientific
  | CStrLit Text
  | -- | Interpolated string: concatenation of parts.
    CStr [CorePart]
  | CVar VarRef
  | CArray [Core]
  | -- | Object literal typed as @Map\<T\>@.
    CMapLit [(Text, Core)]
  | -- | Object literal typed as @Record\<...\>@.
    CRecordLit [(Text, Core)]
  | CLam Lam
  | -- | Application: function, positional args, keyword args
    -- (statically validated; missing keywords are filled from the
    -- callee's defaults at call time, spec 8.3).
    CApp Core [Core] [(Text, Core)]
  | -- | Record field access (statically guaranteed, spec 8.9).
    CDot Core Text
  | -- | Index access; the container kind is statically known.
    CIndex IndexKind Core Core
  | -- | Normalized @choose@: only the selected branch is evaluated.
    CIf Core Core Core
  | CAnd Core Core
  | COr Core Core
  | CNot Core
  | CBin PrimOp Core Core
  | CDo [CoreStmt]
  | -- | Core @await@ application (spec 6.3).
    CAwait Core
  | -- | Environment construction (core expression, spec 8.8):
    -- kind (@local@\/@docker@\/@env@) and normalized named arguments.
    CEnv Text [(Text, Core)]
  | -- | @cast@ with its statically determined target type (15.8).
    CCast Core Type
  deriving (Show, Eq)

data CorePart = CPText Text | CPExpr Core
  deriving (Show, Eq)

data CoreStmt = CSBind Text Core | CSExpr Core
  deriving (Show, Eq)

data IndexKind = IdxArray | IdxMap | IdxRecord
  deriving (Show, Eq)

data PrimOp
  = PAdd
  | PSub
  | PMul
  | PDiv
  | PEq
  | PNe
  | PLt
  | PLe
  | PGt
  | PGe
  deriving (Show, Eq)

data Lam = Lam
  { -- | Declaration name, or @\<lambda\@line:col\>@ for anonymous
    -- lambdas (13.2).
    lamName :: Text,
    lamModule :: FilePath,
    lamPositional :: [Text],
    lamVariadic :: Maybe Text,
    -- | Keyword parameters with their default expressions, evaluated
    -- at call time when unbound (8.3).
    lamKeywords :: [(Text, Core)],
    lamBody :: Core,
    -- | The function type ('Language.Lask.Types.TyFun').
    lamType :: Type
  }
  deriving (Show, Eq)
