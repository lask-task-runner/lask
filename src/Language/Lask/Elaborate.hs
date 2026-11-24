{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Elaboration: bidirectional type checking (spec 4, 7) combined
-- with static desugaring to the core language (7.6).
--
-- Top-level declarations are typed on demand with memoization;
-- recursion is permitted when the recursive declaration carries a
-- return-type annotation (its header type is then computable without
-- its body).
module Language.Lask.Elaborate
  ( CoreProgram (..),
    SchemaUse (..),
    CommandUse (..),
    CoreDecl (..),
    readFieldType,
    dockerRunOptions,
    imageOptionNames,
    Key,
    StaticParams (..),
    HoverInfo (..),
    elaborateProgram,
    elaborateRecovering,
    Elaborated,
    elaborateModules,
    elaborateRecoveringFrom,
    specName,
    isTemplate,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when, zipWithM)
import Control.Monad.State.Strict (StateT (runStateT), get, gets, lift, modify, put)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Bifunctor (first)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Access (Mode (..), accessTable, parseEgressEntry, parseMode, validateBaseMount)
import Language.Lask.Builtins.Names (effectfulBuiltinNames)
import Language.Lask.Builtins.Shell (escapeCommand, rawBuiltin)
import Language.Lask.CommandSchema (Finding (..), Schema, Static (..), StaticF (..), checkCommand, fromStatic)
import Language.Lask.Builtins.Sig
import qualified Language.Lask.Builtins.Regex as Re
import Language.Lask.Core.AST
import Language.Lask.Diagnostic
import Language.Lask.ErrorCode
import Language.Lask.Lexer.Token (CmdStream (..), Op (..), Spanned (..))
import Language.Lask.Module.Loader (LoadedModule (..), Program (..), collapseDots)
import Language.Lask.Module.Resolve (GlobalScope (..), TypeTarget (..), ValueTarget (..), namespaceMember)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.CommandWords (Analysis (..), CommandWord (..), commandWords, validCommandName)
import Language.Lask.Syntax.Shell (ShellReading (..), ShellWord (..), SimpleCommand (..), readShell)
import Language.Lask.Deps.Cache (inCache)
import Language.Lask.Runtime.Image (recipeSource)
import Language.Lask.Types
import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories, takeDirectory, (</>))

-- Program-level results ------------------------------------------------------

type Key = (FilePath, Text)

data CoreDecl = CoreDecl
  { cdModule :: FilePath,
    cdName :: Text,
    -- | Type parameters the declaration binds (spec 4.2); empty for a
    -- monomorphic declaration, which is every declaration that does
    -- not write @\<...\>@.
    cdTypeVars :: [Text],
    cdType :: Type,
    cdCore :: Core,
    -- | Declaration parameter info (spec 7.5) when the declaration is
    -- a function or a directly lambda-valued binding.
    cdParams :: Maybe StaticParams,
    -- | Bounds of the type parameters that have one (spec 4.2).
    cdBounds :: Map Text Bound
  }
  deriving (Show, Eq)

-- | Static declaration parameter information (spec 7.5): what a call
-- site may use when the callee resolves to a declaration.
data StaticParams = StaticParams
  { spPositional :: [(Text, Type)],
    -- | Variadic parameter with its element type.
    spVariadic :: Maybe (Text, Type),
    spKeywords :: [(Text, Type)],
    -- | The keyword parameters without a default, which every call
    -- has to bind (7.5).
    spRequired :: [Text]
  }
  deriving (Show, Eq)

data CoreProgram = CoreProgram
  { cpEntry :: FilePath,
    cpBaseDir :: FilePath,
    cpDecls :: Map Key CoreDecl,
    -- | Names of the entry module marked @internal@ (spec 5): not
    -- reachable from the CLI or from help listings.
    cpInternal :: Set Text,
    -- | Each module's command words and the environments they name
    -- (spec ch. 5). Kept for enumeration (11.4) and for @lask cmd@
    -- (11.8), which must resolve a command the module declares even
    -- when no task uses it.
    cpCommands :: Map FilePath (Map Text Core),
    -- | Every command execution expression, with the environment it
    -- resolved to and the command words that selected it. Recorded for
    -- editor tooling: the environment dispatch derived is not present
    -- in the text, and the words that carry it must be visible as the
    -- references they are (spec 10.9).
    cpCommandUses :: [CommandUse],
    -- | Name references with their types, recorded during
    -- elaboration for editor tooling (hover).
    cpHover :: [HoverInfo],
    -- | Advisory diagnostics (spec 14.2), in source order.
    cpAdvisories :: [Advisory],
    -- | For each declaration, the command words through which its own
    -- command strings take their environment by dispatch (10.9), with
    -- the module whose table holds them, for the check before a run
    -- (spec ch. 5).
    cpDispatched :: Map Key [(FilePath, Text)],
    -- | Each module's command words that have a schema (spec ch. 5).
    cpSchemas :: Map FilePath (Map Text Schema),
    -- | Every simple command checked against a schema, for editor
    -- tooling.
    cpSchemaUses :: [SchemaUse],
    -- | The types each class has an instance for (spec 4.6), by the
    -- class's module and name: what the CLI instantiates a type
    -- parameter with a class bound at (11.2).
    cpInstances :: Map Key [Type]
  }
  deriving (Show)

-- | One simple command of a dispatched command string, with the
-- schema of its command word (spec ch. 5).
data SchemaUse = SchemaUse
  { suWord :: Text,
    suSchema :: Schema,
    suCommand :: SimpleCommand
  }
  deriving (Show)

-- | One command execution expression as elaboration resolved it.
data CommandUse = CommandUse
  { -- | Span of the whole expression, anchored at its @$@.
    cuSpan :: Span,
    -- | The environment dispatch derived, in the notation of
    -- environment expressions. Nothing when the expression carries an
    -- environment specification of its own: that is already written at
    -- the site, and nothing needs to stand in for it.
    cuEnv :: Maybe Text,
    -- | The command words that selected it, empty when the expression
    -- carried an explicit environment specification.
    cuWords :: [Spanned Text],
    -- | The command string as written, for listing the programs it
    -- runs (spec 11.4).
    cuParts :: [TextPart]
  }
  deriving (Show, Eq)

-- | A resolved name occurrence: where it was written, what it is, and
-- (for top-level targets) which declaration it refers to.
data HoverInfo = HoverInfo
  { hiSpan :: Span,
    hiName :: Text,
    hiType :: Type,
    hiDecl :: Maybe Key,
    -- | The builtin it refers to, when it refers to one: builtins
    -- have no declaration to take documentation from.
    hiBuiltin :: Maybe Text
  }
  deriving (Show, Eq)

-- Elaboration monad ----------------------------------------------------------

data Ctx = Ctx
  { ctxProg :: Program,
    ctxScopes :: Map FilePath GlobalScope,
    -- | Type parameters of the declaration being elaborated (spec
    -- 4.2). An @upper_id@ in this set is that parameter; anything else
    -- is a type alias reference.
    ctxTypeVars :: Set Text,
    -- | Bounds of those type parameters that have one (spec 4.2).
    ctxBounds :: Map Text Bound,
    -- | Whether name resolution reported errors (spec 14.3). The
    -- declarations they are in are not elaborated, and a reference to
    -- something they left out follows from them.
    ctxNamesFailed :: Bool,
    -- | Declarations as checked after their parameters' types were
    -- inferred (spec 4.3): the written declaration with those types
    -- written in. A recursive call reads its header from here.
    ctxDeclOverride :: Map Key Decl,
    -- | The program's classes and instances (spec 4.6).
    ctxEnv :: ClassEnv,
    -- | The type parameters in scope that a local function declares
    -- (spec 6.1): never specialized, so they are ordered as Number and
    -- String are, by the language (4.6).
    ctxLocalVars :: Set Text
  }

-- | The classes and instances of a program (spec 4.6), read before any
-- declaration is elaborated.
data ClassEnv = ClassEnv
  { clsClasses :: Map Key ClassInfo,
    clsInstances :: [InstanceInfo],
    -- | Each method an instance writes, as the declaration it is
    -- elaborated as: named for the instance, with the types the
    -- class's signature gives it written in where it leaves them out.
    clsSynthetic :: Map Key Decl
  }

emptyClassEnv :: ClassEnv
emptyClassEnv = ClassEnv Map.empty [] Map.empty

data ClassInfo = ClassInfo
  { ciParam :: TypeParam,
    ciMethods :: [(Text, MethodInfo)]
  }

data MethodInfo = MethodInfo
  { miParams :: [Param],
    miDefault :: Maybe Expr,
    -- | The method's type, in terms of the class's type parameter;
    -- Nothing where its signature does not elaborate, which the check
    -- of the class reports.
    miType :: Maybe Type
  }

data InstanceInfo = InstanceInfo
  { iiModule :: FilePath,
    iiSpan :: Span,
    iiClass :: Key,
    -- | The type it is for, which mentions its type parameters when it
    -- is parametric.
    iiHead :: Type,
    iiDecls :: [Decl],
    -- | The type parameters of a parametric instance, with their bounds
    -- as written and as read (spec 4.6).
    iiTypeParams :: [TypeParam],
    iiParams :: [(Text, Maybe Bound)]
  }

-- | A type is settled at this point when every variable it still
-- mentions is a type parameter of the declaration being elaborated
-- (spec 4.4): those stand for themselves and nothing can instantiate
-- them further, so a pattern holding one is as concrete as it gets.
settled :: Ctx -> Type -> Bool
settled ctx t = all (\v -> isInferVar v || v `Set.member` ctxTypeVars ctx) (typeVars t)

-- | Elaborate @act@ with @vs@ as the type parameters in scope.
withTypeVars :: [(Text, Maybe Bound)] -> Ctx -> Ctx
withTypeVars vs ctx = ctx {ctxTypeVars = Set.fromList (map fst vs), ctxBounds = boundMap vs}

boundMap :: [(Text, Maybe Bound)] -> Map Text Bound
boundMap vs = Map.fromList [(v, b) | (v, Just b) <- vs]

-- | 'withTypeVars' for a lambda's own type parameters (spec 6.1),
-- which add to those of the declaration it is written in.
addTypeVars :: [(Text, Maybe Bound)] -> Ctx -> Ctx
addTypeVars vs ctx =
  ctx
    { ctxTypeVars = ctxTypeVars ctx <> Set.fromList (map fst vs),
      ctxLocalVars = ctxLocalVars ctx <> Set.fromList (map fst vs),
      ctxBounds = ctxBounds ctx <> boundMap vs
    }

-- | The type variables a generic local binding generalized (spec 6.1):
-- its type parameters, renamed apart by where the lambda is written as
-- @T\@line:column@. An @\@@ cannot occur in an identifier, so no source
-- names one, and 'renderType' shows the part before it.
generalizedVars :: Type -> [Text]
generalizedVars t = nub (filter (T.isInfixOf "@") (typeVars t))

-- | A lambda with type parameters is generic, and a function value has
-- no type variables of its own (spec 4.4): it is bound to a name, by
-- a top-level declaration or a binding of a block, which instantiates
-- it at each reference. Anywhere else it is an error.
monoLambda :: Span -> [TypeParam] -> TC ()
monoLambda _ [] = pure ()
monoLambda sp _ =
  abort . diag ETypeIllformed sp $
    "a lambda with type parameters has to be bound to a name: a function value has no type parameters of its own; bind it first, or write it without them"

-- | The bound of a type parameter in scope, if it has one.
boundOf :: Ctx -> Text -> Maybe Bound
boundOf ctx v = Map.lookup v (ctxBounds ctx)

-- | Conformance (spec 4.4) where the type parameters in scope may
-- carry type bounds: one conforms to what its bound conforms to.
conforms :: Ctx -> Type -> Type -> Bool
conforms ctx = conformsUnder upper
  where
    upper v = case boundOf ctx v of
      Just (BoundType u) -> Just u
      _ -> Nothing

-- | A named predicate (spec 4.4), where a type parameter in scope
-- satisfies what its bound entails.
holds :: Ctx -> NamedBound -> Type -> Bool
holds ctx = satisfiesNamed (boundOf ctx)

-- | 'holds', seen from a module: a type is orderable by an instance in
-- effect there as well (spec 4.6).
holdsAt :: Ctx -> FilePath -> NamedBound -> Type -> Bool
holdsAt ctx path nb t =
  holds ctx nb t || (nb == BOrderable && isJust (instanceFor ctx (Just path) orderableKey t))

-- | A use instantiated a bounded type variable at a type outside its
-- bound (spec 4.4).
boundViolation :: Ctx -> Span -> Text -> Text -> Bound -> Type -> Diagnostic
boundViolation ctx sp callee v b t =
  diag ETypeBound sp $
    "'" <> callee <> "' requires " <> varDisplay v <> " to " <> wanted <> ", but here it is " <> renderType t <> hint
  where
    wanted = case b of
      BoundNamed nb -> "be " <> renderNamedBound nb
      BoundType u -> "conform to " <> renderType u
      BoundClass c -> "have an instance of " <> crName c
      BoundAll _ -> "be " <> renderBound b
    hint = case t of
      TyVar w
        | w `Set.member` ctxTypeVars ctx ->
            " (a type parameter has only what its bound gives it; declare it as <"
              <> w <> ": " <> renderBound b <> ">)"
      _ -> case b of
        BoundNamed BOrderable -> " (Number and String are orderable, and a type with an instance orderable<...> in effect here)"
        BoundNamed BStringifiable -> " (only String, Number, Bool and Any are stringifiable; use to_json for structured values)"
        BoundNamed BComparable -> " (functions, handles, Void and Any cannot be compared)"
        BoundType _ -> ""
        BoundClass c -> " (no instance " <> crName c <> "<" <> renderType t <> "> is in effect here)"
        BoundAll _ -> ""

data St = St
  { stDecls :: Map Key CoreDecl,
    -- | Elaborated alias bodies, with the type parameters the alias
    -- binds (spec 4.2); a reference substitutes its arguments in.
    stAliases :: Map Key ([(Text, Maybe Bound)], Type),
    stActive :: Set Key,
    stHover :: [HoverInfo],
    stCommandUses :: [CommandUse],
    -- | Command tables, built once per module on first use.
    stCommands :: Map FilePath (Map Text CommandEntry),
    -- | Modules whose command table is being built. A command string
    -- elaborated meanwhile was reached from the environment of a
    -- command declaration, which may run nothing (spec ch. 5).
    stCommandsBuilding :: Set FilePath,
    -- | Advisories found so far, most recent first.
    stAdvisories :: [Advisory],
    -- | The declarations being elaborated, innermost first.
    stCurrent :: [Key],
    -- | The probed words each declaration dispatches through.
    stDispatched :: Map Key (Set (FilePath, Text)),
    stSchemaUses :: [SchemaUse],
    -- | Bounds of the type variables generic local bindings generalized
    -- (spec 6.1), by their renamed names ('generalize').
    stLocalBounds :: Map Text Bound,
    -- | The inference variables of the parameters whose types are
    -- being inferred (spec 4.3), by name ('newInferVar').
    stInfer :: Map Text IVar,
    stInferNext :: Int,
    -- | Set while a body is checked only to find what its parameters
    -- are used at ('inferParams'): what that check would record for
    -- editor tooling and advisories is recorded by the check that
    -- follows it instead.
    stQuiet :: Bool,
    stFailures :: Failures,
    stInstanceHeads :: Map Key [Type]
  }

-- | The top-level units (declarations, type aliases, command
-- declarations and imports) that failed. Each contributes its first
-- error, and the others are still checked (spec 14.3). This part of
-- the state outlives an attempt that fails ('recoverTC'): a unit that
-- failed while another was being elaborated failed for good, and the
-- context it failed in may not come again.
data Failures = Failures
  { -- | Errors found so far, most recent first.
    fsErrors :: [Diagnostic],
    -- | Declarations that failed, and are not elaborated again.
    fsDecls :: Set Key,
    -- | Type aliases that failed.
    fsAliases :: Set Key,
    -- | Command words whose declaration or import failed, with the
    -- module whose table they belong to.
    fsWords :: Set (FilePath, Text)
  }

modifyFailures :: (Failures -> Failures) -> TC ()
modifyFailures f = modify (\s -> s {stFailures = f (stFailures s)})

-- | One command word of a module's table (spec ch. 5, 10.9).
data CommandEntry = CommandEntry
  { -- | The word where it was declared, which for an imported word is
    -- in another module.
    ceSpan :: Span,
    -- | The environment, elaborated in the module that declared it.
    -- References in it are resolved, so it can stand at a command
    -- string in any module.
    ceEnv :: Core,
    -- | What selection compares (10.9).
    ceSource :: EnvSource,
    -- | The declaring module and the declaration's position in it. A
    -- declaration reached through two imports is one entry, not two.
    ceOrigin :: (FilePath, Int),
    -- | The command schema the declaration gives the word, if any.
    ceSchema :: Maybe Schema
  }

-- | The identity selection compares (spec 10.9). The environment of a
-- command declaration is an ordinary expression, evaluated when a
-- command runs, so two declarations are known to name one environment
-- only where the text shows it: the same literal environment
-- expression, the same top-level binding, or the same declaration.
data EnvSource
  = SrcLiteral Text
  | SrcBinding Key
  | SrcDecl FilePath Int
  deriving (Show, Eq)

-- | Why elaboration stopped.
data Failure
  = Failed Diagnostic
  | -- | Something it needs failed, and was reported where it failed:
    -- an error that only follows from another is not reported (spec
    -- 14.3).
    DependencyFailed

type TC = StateT St (Either (Failure, Failures))

-- | Record how one command execution expression got its environment.
recordCommandUse :: Span -> Maybe Text -> [Spanned Text] -> [TextPart] -> TC ()
recordCommandUse sp env ws parts =
  unlessQuiet $ modify (\s -> s {stCommandUses = CommandUse sp env ws parts : stCommandUses s})

-- | An environment in the notation of environment expressions (6.7),
-- for display.
renderEnvCore :: Core -> Text
renderEnvCore c = case coreF c of
  CEnv "local" _ -> "#local"
  CEnv "docker" args -> case (lookup "image" args, lookup "dockerfile" args) of
    (Just (Core _ (CStrLit img)), _) -> "#" <> img
    (_, Just (Core _ (CStrLit df))) -> "#" <> recipeSource df
    _ -> "#?"
  CEnv kind _ -> "#" <> kind
  CRunnable base _ -> renderEnvCore base <> "{...}"
  -- An environment named by a binding, or produced by a call, is
  -- shown as what was written: its value exists only at run time.
  CVar (TopRef _ n) -> n
  CApp (Core _ (CVar (TopRef _ n))) _ _ -> n <> "(...)"
  _ -> "#?"

-- | Record a resolved name occurrence for hover (editor tooling).
recordVar :: Span -> Text -> Type -> Maybe Key -> TC ()
recordVar sp n t d =
  unlessQuiet $ modify (\s -> s {stHover = HoverInfo sp n t d Nothing : stHover s})

-- | Record a reference to the builtin @bn@ for hover.
recordBuiltin :: Span -> Text -> Type -> Text -> TC ()
recordBuiltin sp n t bn =
  unlessQuiet $ modify (\s -> s {stHover = HoverInfo sp n t Nothing (Just bn) : stHover s})

-- | Local value bindings with their types.
type Locals = Map Text Type

abort :: Diagnostic -> TC a
abort = failWith . Failed

dependencyFailed :: TC a
dependencyFailed = failWith DependencyFailed

failWith :: Failure -> TC a
failWith f = gets stFailures >>= \fs -> lift (Left (f, fs))

-- | Record an advisory diagnostic (spec 14.2); elaboration continues.
advise :: AdvisoryCode -> Span -> Text -> TC ()
advise code sp msg = unlessQuiet $ modify (\s -> s {stAdvisories = Advisory code sp msg : stAdvisories s})

-- | Record something only outside a check made to infer parameter
-- types ('stQuiet').
unlessQuiet :: TC () -> TC ()
unlessQuiet act = gets stQuiet >>= \q -> unless q act

-- | Run @act@ as a check of its own, recording what it finds: a
-- declaration or a command table elaborated on demand from inside a
-- quiet check is elaborated once, for good.
loud :: TC a -> TC a
loud act = do
  q <- gets stQuiet
  modify (\s -> s {stQuiet = False})
  r <- act
  modify (\s -> s {stQuiet = q})
  pure r

-- | Attempt an elaboration, recovering from its diagnostic. Used for
-- bidirectional fallbacks (e.g. inferring one if branch and checking
-- the other, so context-typed calls like @fail(e)@ work in either
-- branch, spec 15.7). A failed dependency is not recovered from: no
-- other way of reading the expression can do without it.
tryTC :: TC a -> TC (Either Diagnostic a)
tryTC action = do
  r <- recoverTC action
  case r of
    Left (Failed d) -> pure (Left d)
    Left DependencyFailed -> dependencyFailed
    Right a -> pure (Right a)

-- | Attempt an elaboration, recovering from any failure. The state
-- goes back to what it was before the attempt, all but the units
-- found to fail meanwhile ('Failures').
recoverTC :: TC a -> TC (Either Failure a)
recoverTC action = do
  st <- get
  case runStateT action st of
    Left (f, fs) -> put st {stFailures = fs} >> pure (Left f)
    Right (a, st') -> put st' >> pure (Right a)

-- | Record the failure of one top-level unit: its error, unless it
-- only followed from another.
recordFailure :: Failure -> TC ()
recordFailure (Failed d) = modifyFailures (\fs -> fs {fsErrors = d : fsErrors fs})
recordFailure DependencyFailed = pure ()

-- | A declaration the scopes name but the program does not hold. Once
-- name resolution has failed (spec 14.3) that follows from an error
-- already reported; otherwise it is an internal error.
missingDecl :: Ctx -> Text -> TC a
missingDecl ctx what
  | ctxNamesFailed ctx = dependencyFailed
  | otherwise = abort (diag ENameUndefined NoSpan ("internal: missing " <> what))

diag :: ErrorCode -> Span -> Text -> Diagnostic
diag code sp = mkDiagnostic code StageStatic sp

mismatch :: Span -> Type -> Type -> TC a
mismatch sp expected actual =
  abort $
    withExpectedActual (renderType expected) (renderType actual) $
      diag ETypeMismatch sp "type mismatch"

-- Entry point -----------------------------------------------------------------

-- | Elaborate a program whose names all resolved: the core program,
-- or every independent error found in it (spec 14.3).
elaborateProgram :: Program -> Map FilePath GlobalScope -> Either [Diagnostic] CoreProgram
elaborateProgram prog scopes = case elaborateRecovering prog scopes [] of
  ([], core) -> Right core
  (ds, _) -> Left ds

-- | Elaborate every top-level unit that does not depend on one that
-- failed, given the errors name resolution reported. A unit holding
-- one of those is not elaborated (spec 14.3): its names do not all
-- mean something, and checking it would only report what follows from
-- them. Returns the errors found, first in source order within each
-- unit, and the core program of what did elaborate.
elaborateRecovering :: Program -> Map FilePath GlobalScope -> [Diagnostic] -> ([Diagnostic], CoreProgram)
elaborateRecovering = elaborateRecoveringFrom Nothing

-- | The elaboration of some of a program's modules, which the rest of
-- it can resume from ('elaborateRecoveringFrom'): the language server
-- keeps the one of a project's dependencies, which do not change while
-- the project is edited.
data Elaborated = Elaborated (Set FilePath) St

-- | Elaborate the units of the given modules alone. They must import
-- no module outside them, so that everything elaborating them reaches
-- is among them: the result then depends on nothing but their sources
-- and the program's directories ('recipePath'). The diagnostics are
-- those name resolution reported in these modules. 'Nothing' for an
-- internal error.
elaborateModules :: Program -> Map FilePath GlobalScope -> [Diagnostic] -> [FilePath] -> Maybe Elaborated
elaborateModules prog scopes nameErrors paths =
  case runStateT (elabUnits ctx inBase) (initialSt (restrict (unitsHolding prog nameErrors))) of
    Left _ -> Nothing
    Right ((), st) -> Just (Elaborated base st)
  where
    base = Set.fromList paths
    inBase p = p `Set.member` base
    ctx = Ctx prog scopes Set.empty Map.empty (not (null nameErrors)) Map.empty emptyClassEnv Set.empty
    -- The units holding an error elsewhere are not these modules' to
    -- record: the result must not depend on the rest of the program.
    restrict (ds, as, ws) =
      (Set.filter (inBase . fst) ds, Set.filter (inBase . fst) as, Set.filter (inBase . fst) ws)

-- | 'elaborateRecovering', resuming from the elaboration of some of the
-- program's modules when one is given: only the other modules' units
-- are elaborated, and what they reach in the given ones is already
-- there.
elaborateRecoveringFrom :: Maybe Elaborated -> Program -> Map FilePath GlobalScope -> [Diagnostic] -> ([Diagnostic], CoreProgram)
elaborateRecoveringFrom resumed prog scopes nameErrors =
  case runStateT (elabUnits ctx (`Set.notMember` done)) start of
    -- Every unit recovers on its own, so only an internal error in
    -- the recovery itself can get here.
    Left (f, fs) -> (reverse (fsErrors fs) <> [d | Failed d <- [f]], emptyCore)
    Right ((), st) ->
      ( reverse (fsErrors (stFailures st)),
        CoreProgram
          { cpEntry = progEntry prog,
            cpBaseDir = progBaseDir prog,
            cpDecls = stDecls st,
            cpInternal = internal,
            cpCommands = Map.map (Map.map ceEnv) (stCommands st),
            cpCommandUses = stCommandUses st,
            cpHover = stHover st,
            -- An expression elaborated twice, by a bidirectional
            -- fallback, reports once.
            cpAdvisories = sortOn advSpan (nub (stAdvisories st)),
            cpDispatched = Map.map Set.toList (stDispatched st),
            cpSchemas = Map.map (Map.mapMaybe ceSchema) (stCommands st),
            cpSchemaUses = stSchemaUses st,
            cpInstances = stInstanceHeads st
          }
      )
  where
    ctx = Ctx prog scopes Set.empty Map.empty (not (null nameErrors)) Map.empty emptyClassEnv Set.empty
    failedUnits@(failedDecls, failedAliases, failedWords) = unitsHolding prog nameErrors
    (done, start) = case resumed of
      Nothing -> (Set.empty, initialSt failedUnits)
      Just (Elaborated base st) ->
        let fs = stFailures st
         in ( base,
              st
                { stFailures =
                    fs
                      { fsDecls = Set.union failedDecls (fsDecls fs),
                        fsAliases = Set.union failedAliases (fsAliases fs),
                        fsWords = Set.union failedWords (fsWords fs)
                      }
                }
            )
    internal =
      maybe Set.empty (moduleInternal . lmModule) $
        Map.lookup (progEntry prog) (progModules prog)
    emptyCore =
      CoreProgram (progEntry prog) (progBaseDir prog) Map.empty internal Map.empty [] [] [] Map.empty Map.empty [] Map.empty

-- | The state no unit has been elaborated in, given the units that
-- hold an error of name resolution.
initialSt :: (Set Key, Set Key, Set (FilePath, Text)) -> St
initialSt (failedDecls, failedAliases, failedWords) =
  St
    { stDecls = Map.empty,
      stAliases = Map.empty,
      stActive = Set.empty,
      stHover = [],
      stCommandUses = [],
      stCommands = Map.empty,
      stCommandsBuilding = Set.empty,
      stAdvisories = [],
      stCurrent = [],
      stDispatched = Map.empty,
      stSchemaUses = [],
      stLocalBounds = Map.empty,
      stInfer = Map.empty,
      stInferNext = 0,
      stQuiet = False,
      stFailures = Failures [] failedDecls failedAliases failedWords,
      stInstanceHeads = Map.empty
    }

-- | Elaborate every unit of the modules the predicate selects.
-- Every module's command declarations are checked, whether or not a
-- command string uses them (spec ch. 5), and so is every type alias,
-- whether or not a type refers to it (spec 4.2).
elabUnits :: Ctx -> (FilePath -> Bool) -> TC ()
elabUnits ctx0 selected = do
  env <- buildClassEnv ctx0
  let ctx = ctx0 {ctxEnv = env}
      modules = filter (selected . lmPath) (Map.elems (progModules (ctxProg ctx)))
  modify (\s -> s {stInstanceHeads = instanceHeads ctx})
  elabUnitsWith ctx modules
  mapM_ (unit . checkClassDecl ctx) [(lmPath lm, d) | lm <- modules, d@(Decl _ DClass {}) <- moduleDecls (lmModule lm)]
  mapM_ (unit . checkInstanceDecl ctx) [(lmPath lm, d) | lm <- modules, d@(Decl _ DInstance {}) <- moduleDecls (lmModule lm)]
  specializeEntry ctx (filter (selected . lmPath) [lm | lm <- Map.elems (progModules (ctxProg ctx)), lmPath lm == progEntry (ctxProg ctx)])
  specializeAll ctx
  where
    unit act = recoverTC act >>= either recordFailure pure

elabUnitsWith :: Ctx -> [LoadedModule] -> TC ()
elabUnitsWith ctx modules = do
  mapM_ (commandTable ctx . lmPath) modules
  mapM_
    (tryAliasBody ctx)
    [ (lmPath lm, a)
    | lm <- modules,
      Decl _ (DTypeAlias a _ _) <- moduleDecls (lmModule lm)
    ]
  mapM_
    (tryDemandDecl ctx)
    [ (lmPath lm, n)
    | lm <- modules,
      Decl _ f <- moduleDecls (lmModule lm),
      n <- declValueName f
    ]
  where
    declValueName (DValue n _ _ _) = [n]
    declValueName (DFunction n _ _ _ _ _) = [n]
    declValueName _ = []

-- | The top-level units that hold an error of name resolution: the
-- declarations, the type aliases and the command words declared or
-- imported where one is reported. A name an import binds by mistake
-- (a duplicate) takes the module's own declaration of that name with
-- it; a name it binds to nothing is caught where it is used
-- ('missingDecl').
unitsHolding :: Program -> [Diagnostic] -> (Set Key, Set Key, Set (FilePath, Text))
unitsHolding prog ds =
  ( Set.fromList (concat [vs | (vs, _, _) <- hits]),
    Set.fromList (concat [ts | (_, ts, _) <- hits]),
    Set.fromList (concat [ws | (_, _, ws) <- hits])
  )
  where
    spans = [sp | d <- ds, sp@Span {} <- [diagSpan d]]
    holdsError outer = any (outer `contains`) spans
    hits =
      [ unit (lmPath lm) d
      | lm <- Map.elems (progModules prog),
        d <- moduleDecls (lmModule lm),
        holdsError (declSpan d)
      ]
    unit path (Decl _ f) = case f of
      DValue n _ _ _ -> ([(path, n)], [], [])
      DFunction n _ _ _ _ _ -> ([(path, n)], [], [])
      DTypeAlias n _ _ -> ([], [(path, n)], [])
      DCommand ns _ _ -> ([], [], [(path, n) | Spanned _ n <- ns])
      DImportCommands ns _ -> ([], [], [(path, n) | Spanned _ n <- ns])
      DExportCommandsFrom ns _ -> ([], [], [(path, n) | Spanned _ n <- ns])
      DImportNamed specs _ -> imported path specs
      DExportFrom specs _ -> imported path specs
      DImportNamespace alias _ -> ([(path, alias)], [], [])
      DExportNamespace alias _ -> ([(path, alias)], [], [])
      DWithin _ -> ([], [], [])
      DAccess n _ -> ([], [(path, n)], [])
      DClass n _ ms -> ([(path, m) | m <- n : [mn | ClassMethod _ (Spanned _ mn) _ _ _ <- ms]], [], [])
      DInstance {} -> ([], [], [])
    imported path specs =
      ([(path, v) | (v, False) <- bad], [(path, v) | (v, True) <- bad], [])
      where
        bad = [(v, isType v) | ImportSpec sp n a <- specs, holdsError sp, let v = maybe n id a]
    isType t = maybe False (\(c, _) -> c >= 'A' && c <= 'Z') (T.uncons t)

-- | Whether a span lies within another one, in the same file.
contains :: Span -> Span -> Bool
contains (Span s e) (Span s' e') =
  fileName s == fileName s' && (line s, column s) <= (line s', column s') && (line e', column e') <= (line e, column e)
contains _ _ = False

-- Top-level declarations -------------------------------------------------------

lookupDeclAst :: Ctx -> Key -> Maybe Decl
lookupDeclAst ctx key@(path, name)
  | Just d <- Map.lookup key (ctxDeclOverride ctx) = Just d
  | Just d <- Map.lookup key (clsSynthetic (ctxEnv ctx)) = Just d
  | Just (_, d) <- methodDeclAst ctx key = Just d
  | otherwise = do
      lm <- Map.lookup path (progModules (ctxProg ctx))
      let match d = case declF d of
            DValue n _ _ _ -> n == name
            DFunction n _ _ _ _ _ -> n == name
            _ -> False
      case filter match (moduleDecls (lmModule lm)) of
        (d : _) -> Just d
        [] -> Nothing

-- | Type of a top-level declaration, elaborating it if needed. For
-- recursive references, falls back to the annotation-derived header
-- type.
declType :: Ctx -> Key -> TC Type
declType ctx key = snd <$> declTypeBounds ctx key

-- | 'declType', with the bounds of the declaration's type parameters.
declTypeBounds :: Ctx -> Key -> TC (Map Text Bound, Type)
declTypeBounds ctx key = do
  done <- gets stDecls
  case Map.lookup key done of
    Just cd -> pure (cdBounds cd, cdType cd)
    Nothing -> do
      active <- gets stActive
      if key `Set.member` active
        then (\(vs, t) -> (boundMap vs, t)) <$> headerType ctx key
        else (\cd -> (cdBounds cd, cdType cd)) <$> demandOrStandIn ctx key

-- | 'show' into 'Text', for counts inside diagnostics.
tshowInt :: Int -> Text
tshowInt = T.pack . show

-- | The type parameters a declaration binds (spec 4.2). A name that
-- is also a type alias visible here is a duplicate definition: a type
-- name means one thing throughout a declaration.
--
-- Each comes with its bound, where it has one (spec 4.2): a named
-- bound is one of a closed set, and a type bound is a type that
-- mentions no type parameter.
typeVarsOf :: Ctx -> FilePath -> [TypeParam] -> TC [(Text, Maybe Bound)]
typeVarsOf ctx path tps = do
  mapM_ distinct tps
  mapM param tps
  where
    names = typeParamNames tps
    distinct (TypeParam (Spanned vsp v) _)
      | Just _ <- Map.lookup path (ctxScopes ctx) >>= Map.lookup v . gsTypes =
          abort . diag ENameDuplicate vsp $
            "type parameter '" <> v <> "' has the name of a type alias in scope"
      -- The grammar reads these as its own types, so a parameter of
      -- that name would never be referred to (spec 4.2).
      | v `elem` builtinTypeNames =
          abort . diag ENameDuplicate vsp $
            "type parameter '" <> v <> "' has the name of a built-in type"
      | length [() | w <- names, w == v] > 1 =
          abort (diag ENameDuplicate vsp ("duplicate type parameter: '" <> v <> "'"))
      | otherwise = pure ()
    param (TypeParam (Spanned _ v) mb) = (,) v <$> traverse bound mb
    bound (SBoundNamed (Spanned bsp n)) = case namedBoundFromText n of
      Just b -> pure (BoundNamed b)
      Nothing -> case classNamed ctx path Nothing n of
        Just key -> pure (BoundClass (classRefOf ctx key))
        Nothing ->
          abort . diag ENameUndefined bsp $
            "unknown bound '" <> n <> "': a named bound is comparable, orderable or stringifiable, or a class in scope"
    bound (SBoundQualified (Spanned _ q) (Spanned bsp n)) = case classNamed ctx path (Just q) n of
      Just key -> pure (BoundClass (classRefOf ctx key))
      Nothing -> abort (diag ENameUndefined bsp ("'" <> q <> "." <> n <> "' is not a class"))
    bound (SBoundClass p n) = pure (BoundClass (classRefOf ctx (p, n)))
    bound (SBoundAll sbs) = do
      bs <- mapM part sbs
      pure $ case nub (concatMap boundParts bs) of
        [b] -> b
        bs' -> BoundAll bs'
    bound (SBoundType st) = do
      case filter (`elem` names) (sTypeNames st) of
        (v : _) ->
          () <$ abort (diag ETypeIllformed (stypeSpan st) ("a bound cannot mention the type parameter '" <> v <> "'"))
        [] -> pure ()
      t <- typeFromS (withTypeVars [] ctx) path st
      when (t == TyVoid || not (wellFormed t)) $
        () <$ abort (diag ETypeIllformed (stypeSpan st) ("'" <> renderType t <> "' cannot be a bound"))
      pure (BoundType t)
    -- A part of @a + b@ is a named bound or a class (spec 4.2).
    part sb = case sb of
      SBoundType st -> abort (diag ETypeIllformed (stypeSpan st) "a type bound stands alone: only named bounds and classes are joined with +")
      _ -> bound sb

-- | The unqualified type names a type mentions.
sTypeNames :: SType -> [Text]
sTypeNames (SType _ f) = case f of
  SArray t -> sTypeNames t
  SMap t -> sTypeNames t
  SRecord fs -> concat [sTypeNames t | (_, _, t) <- fs]
  SAsyncHandle t -> sTypeNames t
  SFunction ps r -> concatMap sTypeNames (ps <> [r])
  SNamed Nothing n as -> n : concatMap sTypeNames as
  SNamed (Just _) _ as -> concatMap sTypeNames as
  SUnion ts -> concatMap sTypeNames ts
  _ -> []

-- | Header type from annotations only (recursion support).
headerType :: Ctx -> Key -> TC ([(Text, Maybe Bound)], Type)
headerType ctx key@(path, name) = case lookupDeclAst ctx key of
  Just (Decl sp f) -> do
   cycleOf <- gets (takeWhile (/= key) . stCurrent)
   -- Reached again through other declarations: every declaration of
   -- the cycle has its parameters written (spec 4.3), since what one
   -- of them is called with there would not be checked against the
   -- types its body is still to give them.
   case [k | k <- cycleOf <> [key | not (null cycleOf)], Just ps <- [lookupDeclAst ctx k >>= writtenParams], any needsInference ps] of
    (_, k) : _ ->
      abort . diag ETypeMismatch sp $
        "'"
          <> k
          <> "' is in a cycle of calls ("
          <> T.intercalate " -> " (map snd (key : reverse cycleOf <> [key]))
          <> "), so its parameters' types cannot be inferred from its body; annotate them"
    [] -> pure ()
   case f of
    DFunction _ tps ps (Just rt) _ _ -> do
      vs <- typeVarsOf ctx path tps
      let ctx' = withTypeVars vs ctx
      posTys <- paramHeaderTypes ctx' path ps
      (,) vs . TyFun posTys <$> typeFromS ctx' path rt
    DValue _ _ (Just t) _ -> (,) [] <$> typeFromS ctx path t
    -- A lambda with its return type written gives the type as a
    -- function declaration does (spec 6.1).
    DValue _ _ Nothing (Expr _ (ELambda tps ps (Just rt) _)) -> do
      vs <- typeVarsOf ctx path tps
      let ctx' = withTypeVars vs ctx
      posTys <- paramHeaderTypes ctx' path ps
      (,) vs . TyFun posTys <$> typeFromS ctx' path rt
    _ ->
      abort . diag ETypeMismatch sp $
        "recursive declaration '" <> name <> "' needs a return type annotation"
  Nothing -> missingDecl ctx ("declaration " <> name)

-- | The parameters a declaration writes: a function declaration's, or
-- those of the lambda a binding is (spec 6.1).
writtenParams :: Decl -> Maybe [Param]
writtenParams (Decl _ df) = case df of
  DFunction _ _ ps _ _ _ -> Just ps
  DValue _ _ _ (Expr _ (ELambda _ ps _ _)) -> Just ps
  _ -> Nothing

paramHeaderTypes :: Ctx -> FilePath -> [Param] -> TC [Type]
paramHeaderTypes ctx path ps =
  sequence
    [ t
    | Param _ f <- ps,
      Just t <- [positionalOf f]
    ]
  where
    positionalOf (PPositional _ _ ann) = Just (maybe (pure TyAny) (typeFromS ctx path) ann)
    positionalOf (PVariadic _ ann) =
      Just (maybe (pure (TyArray TyAny)) (typeFromS ctx path) ann)
    positionalOf (PKeyword {}) = Nothing

-- | The elaborated declaration. One that failed fails what needs it,
-- without reporting anything more.
demandDecl :: Ctx -> Key -> TC CoreDecl
demandDecl ctx key = tryDemandDecl ctx key >>= maybe dependencyFailed pure

-- | 'demandDecl', or for a declaration that failed, what its
-- annotations alone say it is (spec 14.3): the declarations that use
-- it are still checked, so their own errors are reported too.
demandOrStandIn :: Ctx -> Key -> TC CoreDecl
demandOrStandIn ctx key = do
  r <- tryDemandDecl ctx key
  case r of
    Just cd -> pure cd
    Nothing -> recoverTC (standIn ctx key) >>= either (const dependencyFailed) (maybe dependencyFailed pure)

-- | Elaborate a declaration once, recording its error if it fails.
-- Nothing when it failed, now or before.
tryDemandDecl :: Ctx -> Key -> TC (Maybe CoreDecl)
tryDemandDecl ctx key@(path, name) = do
  done <- gets stDecls
  failed <- gets (fsDecls . stFailures)
  case Map.lookup key done of
    Just cd -> pure (Just cd)
    Nothing
      | key `Set.member` failed -> pure Nothing
      | otherwise -> do
          d <- maybe (missingDecl ctx ("declaration " <> name)) pure (lookupDeclAst ctx key)
          outer <- gets stCurrent
          r <- recoverTC $ do
            modify (\s -> s {stActive = Set.insert key (stActive s), stCurrent = key : outer})
            loud $ case methodDeclAst ctx key of
              Just (True, _) -> elabMethod ctx path d
              _ -> elabDecl ctx path d
          modify (\s -> s {stCurrent = outer})
          case r of
            Right cd -> do
              modify $ \s ->
                s
                  { stActive = Set.delete key (stActive s),
                    stDecls = Map.insert key cd (stDecls s)
                  }
              pure (Just cd)
            Left f -> do
              recordFailure f
              modifyFailures (\fs -> fs {fsDecls = Set.insert key (fsDecls fs)})
              pure Nothing

-- | What a declaration's annotations alone say it is, when they say
-- all of it: a function with its return type and the type of every
-- keyword parameter written, or a binding with its type written. Its
-- core is only a reference to it, never evaluated: a program with a
-- failed declaration does not run.
standIn :: Ctx -> Key -> TC (Maybe CoreDecl)
standIn ctx key@(path, name) = case lookupDeclAst ctx key of
  Just (Decl sp f) -> case f of
    DFunction _ tps ps (Just rt) _ _
      | annotated ps -> do
          vs <- typeVarsOf ctx path tps
          let ctx' = withTypeVars vs ctx
          params <- headerParams ctx' ps
          ret <- typeFromS ctx' path rt
          pure (Just (stub sp (map fst vs) (funType params ret) (Just params) (boundMap vs)))
    DValue _ _ Nothing (Expr _ (ELambda tps ps (Just rt) _))
      | annotated ps -> do
          vs <- typeVarsOf ctx path tps
          let ctx' = withTypeVars vs ctx
          params <- headerParams ctx' ps
          ret <- typeFromS ctx' path rt
          pure (Just (stub sp (map fst vs) (funType params ret) (Just params) (boundMap vs)))
    DValue _ _ (Just ann) rhs -> do
      t <- typeFromS ctx path ann
      case exprF rhs of
        ELambda _ ps _ _
          | annotated ps -> do
              params <- headerParams ctx ps
              pure (Just (stub sp [] t (Just params) Map.empty))
          | otherwise -> pure Nothing
        _ -> pure (Just (stub sp [] t Nothing Map.empty))
    _ -> pure Nothing
  Nothing -> pure Nothing
  where
    stub sp vs t params bounds = CoreDecl path name vs t (Core sp (CVar (TopRef path name))) params bounds
    funType params ret =
      TyFun (map snd (spPositional params) <> [TyArray e | Just (_, e) <- [spVariadic params]]) ret
    headerParams c = headerStaticParams c path

-- | Whether the annotations give the type of every parameter. One
-- without an annotation is typed by its default or by its uses (spec
-- 4.3), which only a body that checked can say (14.3).
annotated :: [Param] -> Bool
annotated ps = and [hasAnn f | Param _ f <- ps]
  where
    hasAnn f = case f of
      PPositional _ _ ann -> isJust ann
      PVariadic _ ann -> isJust ann
      PKeyword _ _ ann _ -> isJust ann

-- | The declaration parameter information the annotations give, for a
-- parameter list that is 'annotated'.
headerStaticParams :: Ctx -> FilePath -> [Param] -> TC StaticParams
headerStaticParams c path ps = do
  pos <- sequence [(,) n <$> maybe (pure TyAny) (typeFromS c path) ann | Param _ (PPositional n _ ann) <- ps]
  var <- sequence [(,) n <$> variadicElem ann | Param _ (PVariadic n ann) <- ps]
  kws <- sequence [(,) n <$> maybe (pure TyAny) (typeFromS c path) ann | Param _ (PKeyword n _ ann _) <- ps]
  pure (StaticParams pos (listToMaybe var) kws [n | Param _ (PKeyword n _ _ Nothing) <- ps])
  where
    variadicElem ann = do
      t <- maybe (pure (TyArray TyAny)) (typeFromS c path) ann
      case t of
        TyArray e -> pure e
        _ -> dependencyFailed

elabDecl :: Ctx -> FilePath -> Decl -> TC CoreDecl
elabDecl ctx0 path (Decl sp f) = case f of
  -- Parameters written without annotations have the types their uses
  -- give them (spec 4.3): the declaration is checked as if they were
  -- written, and a recursive call sees them so ('ctxDeclOverride').
  DFunction name tps ps rt w body
    | any needsInference ps -> do
        vs <- typeVarsOf ctx0 path tps
        (ps', extra) <- inferParams (withTypeVars vs ctx0) path Map.empty True ps rt body
        let d' = Decl sp (DFunction name (tps <> extra) ps' rt w body)
        elabDecl (ctx0 {ctxDeclOverride = Map.insert (path, name) d' (ctxDeclOverride ctx0)}) path d'
  DValue name sec Nothing rhs@(Expr rsp (ELambda tps ps rt body))
    | any needsInference ps -> do
        vs <- typeVarsOf ctx0 path tps
        (ps', extra) <- inferParams (withTypeVars vs ctx0) path Map.empty True ps rt body
        let d' = Decl sp (DValue name sec Nothing (rhs {exprF = ELambda (tps <> extra) ps' rt body}))
        _ <- pure rsp
        elabDecl (ctx0 {ctxDeclOverride = Map.insert (path, name) d' (ctxDeclOverride ctx0)}) path d'
  DFunction name tps ps rt _ body -> do
    vs <- typeVarsOf ctx0 path tps
    let ctx = withTypeVars vs ctx0
    (lam, ty, params) <- elabLambda ctx path Map.empty sp (Just name) ps rt body
    pure (CoreDecl path name (map fst vs) ty lam (Just params) (boundMap vs))
  DValue name sec ann rhs -> do
    let ctx = ctx0
    case (exprF rhs, ann) of
      (ELambda (_ : _) _ _ _, Just t) ->
        abort . diag ETypeIllformed (stypeSpan t) $
          "a binding of a lambda with type parameters takes no type annotation: the lambda gives its types"
      _ -> pure ()
    annTy <- traverse (typeFromS ctx path) ann
    case exprF rhs of
      -- A directly lambda-valued binding keeps declaration parameter
      -- info (callable with keyword arguments, spec 7.5), and its type
      -- parameters, as a function declaration does (6.1).
      ELambda tps ps rt body -> do
        vs <- typeVarsOf ctx path tps
        -- The annotation's result type is the body's expected type.
        let expectedRet = case annTy of
              Just (TyFun _ r) -> Just r
              _ -> Nothing
        (lam, ty, params) <- elabLambdaExpecting (withTypeVars vs ctx) path Map.empty (exprSpan rhs) (Just name) expectedRet ps rt body
        -- A function-typed binding can never be a secret (6.10 allows
        -- only String); report that before the annotation check.
        _ <- applySecrecy sp name sec ty lam
        case annTy of
          Just t | not (conforms ctx ty t) -> mismatch sp t ty
          Just t -> pure (CoreDecl path name [] t lam (Just params) Map.empty)
          Nothing -> pure (CoreDecl path name (map fst vs) ty lam (Just params) (boundMap vs))
      _ -> do
        (core, ty) <- case annTy of
          Just t -> (,) <$> check ctx path Map.empty rhs t <*> pure t
          Nothing -> infer ctx path Map.empty rhs
        when (ty == TyVoid) $
          abort (diag ETypeIllformed sp "a Void value cannot be bound at top level")
        core' <- applySecrecy sp name sec ty core
        pure (CoreDecl path name [] ty core' Nothing Map.empty)
  _ -> abort (diag ENameUndefined sp "internal: not a value declaration")

-- Secret bindings (spec 6.10) ---------------------------------------------------

-- | Wrap a core expression in a @mark_secret@ application, the
-- desugaring target of @!!@ (spec 6.10). The value itself is
-- unchanged; this only registers it for log masking (12.8).
markSecretCall :: Span -> Core -> Core
markSecretCall sp inner =
  Core sp (CApp (Core sp (CVar (BuiltinRef "mark_secret"))) [inner] [])

-- | Apply the @!!@ marker to a binding whose type is now known.
-- @!!@ is permitted only on @String@ and @String | Null@ bindings
-- (spec 6.10).
applySecrecy :: Span -> Text -> Secrecy -> Type -> Core -> TC Core
applySecrecy _ _ Public _ core = pure core
applySecrecy sp name Secret ty core = do
  checkSecretType sp name ty
  pure (markSecretCall sp core)

checkSecretType :: Span -> Text -> Type -> TC ()
checkSecretType sp name ty = do
  vs <- gets stInfer
  case openVar vs ty of
    -- The marker is a use at String | Null (spec 4.3, 6.10).
    Just v -> addUse sp v (mkUnion TyString [TyNull])
    Nothing ->
      unless (secretType ty) $
        abort . diag ETypeSecretNonString sp $
          "'" <> name <> "!!' marks a secret binding, which must be String or String | Null, but its type is "
            <> renderType ty

-- | The types a secret can have (spec 6.10): text, or text that may be
-- absent. An absent secret has no text, and registers nothing.
secretType :: Type -> Bool
secretType ty = ty == TyString || ty == mkUnion TyString [TyNull]

-- | Rebind each @!!@-marked parameter as a @!!@-marked local binding
-- of itself at the top of the function body (spec 6.10). Done on the
-- surface body, before it is elaborated, so the rebinding goes through
-- the ordinary block and shadowing rules; the binding registers the
-- value through the built-in @mark_secret@, whatever that name is
-- bound to in scope (7.6).
wrapSecretParams :: [Text] -> Expr -> Expr
wrapSecretParams [] body = body
wrapSecretParams names body =
  Expr sp (EDo (Block sp (map bind names <> [Stmt sp (SExpr body)])))
  where
    sp = exprSpan body
    bind n = Stmt sp (SBind n Secret Nothing (Expr sp (EVar n)))

-- Types from surface syntax -----------------------------------------------------

typeFromS :: Ctx -> FilePath -> SType -> TC Type
typeFromS ctx path st = do
  t <- go st
  unless (wellFormed t) $
    abort (diag ETypeIllformed (stypeSpan st) ("ill-formed type: " <> renderType t))
  pure t
  where
    go (SType sp f) = case f of
      SAny -> pure TyAny
      SNumber -> pure TyNumber
      SString -> pure TyString
      SBool -> pure TyBool
      SNull -> pure TyNull
      SVoid -> pure TyVoid
      SEnvironment -> pure TyEnvironment
      SRunnable -> pure TyRunnable
      SArray t -> TyArray <$> go t
      SMap t -> TyMap <$> go t
      SAsyncHandle t -> TyAsync <$> go t
      SFunction ps r -> TyFun <$> mapM go ps <*> go r
      SRecord fields -> do
        tys <- foldM addField Map.empty fields
        pure (TyRecord tys)
        where
          addField acc (Spanned fsp k, opt, t) = do
            when (Map.member k acc) $
              abort (diag ETypeFieldDuplicate fsp ("duplicate field: '" <> k <> "'"))
            t' <- go t
            pure (Map.insert k (Field opt t') acc)
      -- An upper_id that is a type parameter of the enclosing
      -- declaration is that parameter; anything else is an alias
      -- reference (spec 4.2, 4.4).
      SNamed Nothing n []
        | n `Set.member` ctxTypeVars ctx -> pure (TyVar n)
        -- The type of a parameter of an enclosing function still
        -- being inferred (spec 4.3), written in by 'inferParams'.
        | isInferVar n -> pure (TyVar n)
      SNamed Nothing n args@(_ : _)
        | n `Set.member` ctxTypeVars ctx ->
            abort . diag ETypeArity sp $
              "type parameter '"
                <> n
                <> "' takes no type arguments, but "
                <> tshowInt (length args)
                <> " were given"
      SNamed q n args -> do
        argTys <- mapM go args
        aliasType ctx path sp q n argTys
      SUnion (u : us) -> mkUnion <$> go u <*> mapM go us
      SUnion [] -> pure TyAny -- unreachable: the parser builds at least two

aliasType :: Ctx -> FilePath -> Span -> Maybe Text -> Text -> [Type] -> TC Type
aliasType ctx path sp qualifier n args = do
  -- A qualified reference (ns.TypeName) is resolved by first following
  -- the namespace import to its target module, then searching that
  -- module's own type-alias table instead of the current module's
  -- (mirrors 'elabDot's namespace member resolution). Resolve.checkType
  -- (spec 7 phase 3) has already rejected any reference to a non-public
  -- type alias by the time this runs, so no visibility check is needed
  -- here.
  searchPath <- case qualifier of
    Nothing -> pure path
    Just ns ->
      case Map.lookup path (ctxScopes ctx) >>= Map.lookup ns . gsNamespaces of
        Just key -> pure key
        Nothing ->
          abort . suggesting ns (maybe [] (Map.keys . gsNamespaces) (Map.lookup path (ctxScopes ctx))) $
            diag ENameUndefined sp ("undefined namespace: '" <> ns <> "'")
  case Map.lookup searchPath (ctxScopes ctx) >>= Map.lookup n . gsTypes of
    Just (TBuiltinAlias "Error") -> noArgs >> pure errorType
    Just (TBuiltinAlias "CommandResult") -> noArgs >> pure commandResultType
    Just (TBuiltinAlias "CommandOption") -> noArgs >> pure commandOptionType
    Just (TBuiltinAlias "CommandSpec") -> noArgs >> pure commandSpecType
    Just (TBuiltinAlias "CommandSchema") -> noArgs >> pure commandSchemaType
    Just (TBuiltinAlias other) ->
      abort (diag ENameUndefined sp ("internal: unknown builtin alias " <> other))
    -- A parameterised alias is elaborated once with its parameters
    -- standing for themselves, and each reference substitutes its type
    -- arguments into that body (spec 4.2).
    -- An access declaration shares the namespace of type aliases, and
    -- is no type (spec 5).
    Just (TAlias defPath defName)
      | declaresAccess defPath defName ->
          abort (diag ENameUndefined sp ("'" <> n <> "' is an access constraint, not a type; it is written after within"))
    Just (TAlias defPath defName) -> do
      (params, body) <- aliasBody ctx (defPath, defName)
      unless (length args == length params) $
        abort . diag ETypeArity sp $
          "'"
            <> n
            <> "' takes "
            <> tshowInt (length params)
            <> " type arguments, but "
            <> tshowInt (length args)
            <> " were given"
      -- A bounded parameter takes only arguments within its bound
      -- (spec 4.2), checked here, where the alias is applied.
      sequence_
        [ abort (boundViolation ctx sp n v b t)
        | ((v, Just b), t) <- zip params args,
          not (boundHolds ctx path b t)
        ]
      pure (applySubst (Map.fromList (zip (map fst params) args)) body)
    Nothing ->
      abort . suggesting n ([t | isNothing qualifier, t <- builtinTypeNames] <> maybe [] (Map.keys . gsTypes) (Map.lookup searchPath (ctxScopes ctx))) $
        diag ENameUndefined sp ("undefined type: '" <> n <> "'")
  where
    noArgs =
      unless (null args) $
        abort (diag ETypeArity sp ("'" <> n <> "' takes no type arguments"))
    declaresAccess defPath defName = case Map.lookup defPath (progModules (ctxProg ctx)) of
      Just lm -> not (null [() | Decl _ (DAccess a _) <- moduleDecls (lmModule lm), a == defName])
      Nothing -> False

-- | The body of a declared type alias, with the type parameters it
-- binds, elaborated once and cached. A parameterised alias is
-- elaborated with its parameters standing for themselves. An alias
-- that failed fails the types that refer to it, without reporting
-- anything more.
aliasBody :: Ctx -> Key -> TC ([(Text, Maybe Bound)], Type)
aliasBody ctx key = tryAliasBody ctx key >>= maybe dependencyFailed pure

-- | 'aliasBody', recording the alias's error if it fails. Nothing when
-- it failed, now or before.
tryAliasBody :: Ctx -> Key -> TC (Maybe ([(Text, Maybe Bound)], Type))
tryAliasBody ctx key@(defPath, defName) = do
  cached <- gets stAliases
  failed <- gets (fsAliases . stFailures)
  case Map.lookup key cached of
    Just pb -> pure (Just pb)
    Nothing
      | key `Set.member` failed -> pure Nothing
      | otherwise -> do
          (tps, rhs) <- aliasRhs
          r <- recoverTC $ do
            vs <- typeVarsOf ctx defPath tps
            (,) vs <$> typeFromS (withTypeVars vs ctx) defPath rhs
          case r of
            Right pb -> do
              modify (\s -> s {stAliases = Map.insert key pb (stAliases s)})
              pure (Just pb)
            Left f -> do
              recordFailure f
              modifyFailures (\fs -> fs {fsAliases = Set.insert key (fsAliases fs)})
              pure Nothing
  where
    aliasRhs =
      case Map.lookup defPath (progModules (ctxProg ctx)) of
        Just lm
          | (r : _) <- [(tps, t) | Decl _ (DTypeAlias a tps t) <- moduleDecls (lmModule lm), a == defName] ->
              pure r
        _ -> missingDecl ctx ("type alias " <> defName)

-- Lambdas and parameters ----------------------------------------------------------

-- | Elaborate a lambda or function declaration: computes static
-- parameter info, and infers or checks the return type.
elabLambda ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  Maybe Text ->
  [Param] ->
  Maybe SType ->
  Expr ->
  TC (Core, Type, StaticParams)
elabLambda ctx path locals sp mName = elabLambdaExpecting ctx path locals sp mName Nothing

-- | 'elabLambda', given the result type the lambda's expected type
-- gives it, if any. A body without a result annotation is checked
-- against it, as an annotation would have it checked (spec 4.3), so
-- that what needs an expected type in its last expression (fail, cast,
-- a literal [] or {...}) has one.
elabLambdaExpecting ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  Maybe Text ->
  Maybe Type ->
  [Param] ->
  Maybe SType ->
  Expr ->
  TC (Core, Type, StaticParams)
elabLambdaExpecting ctx path locals sp mName expectedRet ps0 retAnn body = do
  -- A lambda that is not bound to a name has no type parameters
  -- (4.4); one bound to a name has had its parameters inferred
  -- already, with them ('inferParams').
  (ps, _) <- inferParams ctx path locals False ps0 retAnn body
  (params, kwDefaults, bodyLocals, secretParams) <- elabParams ctx path locals ps
  retTy <- maybe (pure expectedRet) (fmap Just . typeFromS ctx path) retAnn
  let body' = wrapSecretParams secretParams body
  (bodyCore, bodyTy) <- case retTy of
    Just t -> (,) <$> check ctx path bodyLocals body' t <*> pure t
    Nothing -> infer ctx path bodyLocals body'
  let posTys = map snd (spPositional params) <> variadicTys
      variadicTys = case spVariadic params of
        Just (_, elemTy) -> [TyArray elemTy]
        Nothing -> []
      funTy = TyFun posTys bodyTy
      name = maybe (lambdaName sp) id mName
      lam =
        Lam
          { lamName = name,
            lamModule = path,
            lamPositional = map fst (spPositional params),
            lamVariadic = fmap fst (spVariadic params),
            lamKeywords = kwDefaults,
            lamBody = bodyCore,
            lamType = funTy
          }
  pure (Core sp (CLam lam), funTy, params)

lambdaName :: Span -> Text
lambdaName = generatedName "lambda"

-- | The name of a function the language makes rather than the user
-- writes, after what made it and where: @<async\@3:7>@ (13.2). A stack
-- trace shows it by what made it (12.4).
generatedName :: Text -> Span -> Text
generatedName what (Span (Position _ l c) _) =
  "<" <> what <> "@" <> T.pack (show l) <> ":" <> T.pack (show c) <> ">"
generatedName what NoSpan = "<" <> what <> ">"

-- | Elaborate the parameter list: types, keyword defaults (evaluated
-- in the scope of preceding parameters, spec 8.3), the body scope, and
-- the names of @!!@-marked parameters (spec 6.10), whose types have
-- been validated here and which the caller rebinds in the body.
elabParams ::
  Ctx ->
  FilePath ->
  Locals ->
  [Param] ->
  TC (StaticParams, [(Text, Maybe Core)], Locals, [Text])
elabParams = elabParamsWith False

-- | 'elabParams'. While @inferring@, a parameter whose type is inferred
-- (spec 4.3) has an inference variable for its type ('inferParams').
elabParamsWith ::
  Bool ->
  Ctx ->
  FilePath ->
  Locals ->
  [Param] ->
  TC (StaticParams, [(Text, Maybe Core)], Locals, [Text])
elabParamsWith inferring ctx path outer = go [] Nothing [] [] [] outer
  where
    go pos var kws defaults secrets locals [] =
      pure
        ( StaticParams (reverse pos) var (reverse kws) [n | (n, Nothing) <- reverse defaults],
          reverse defaults,
          Map.union locals outer,
          reverse secrets
        )
    go pos var kws defaults secrets locals (Param psp f : rest) = case f of
      PPositional n sec ann -> do
        t <- maybe (unannotatedOf n) (typeFromS ctx path) ann
        checkParamType psp t
        secrets' <- addSecret psp n sec t secrets
        go ((n, t) : pos) var kws defaults secrets' (Map.insert n t locals) rest
      PVariadic n ann -> do
        t <- maybe (if inferring then TyArray <$> newInferVar n else pure (TyArray TyAny)) (typeFromS ctx path) ann
        elemTy <- case t of
          TyArray e -> pure e
          _ -> abort (diag ETypeIllformed psp "variadic parameter type must be Array<T>")
        go pos (Just (n, elemTy)) kws defaults secrets (Map.insert n t locals) rest
      PKeyword n sec ann dflt -> do
        (dCore, dTy) <- case (ann, dflt) of
          (Just a, _) -> do
            t <- typeFromS ctx path a
            checkParamType psp t
            c <- traverse (\d -> check ctx path (Map.union locals outer) d t) dflt
            pure (c, t)
          (Nothing, Just d) -> first Just <$> infer ctx path (Map.union locals outer) d
          -- Required and unannotated: inferred, as a positional
          -- parameter is.
          (Nothing, Nothing) -> (,) Nothing <$> unannotatedOf n
        secrets' <- addSecret psp n sec dTy secrets
        go pos var ((n, dTy) : kws) ((n, dCore) : defaults) secrets' (Map.insert n dTy locals) rest

    unannotatedOf n = if inferring then newInferVar n else pure TyAny

    addSecret _ _ Public _ secrets = pure secrets
    addSecret psp n Secret t secrets = do
      vs <- gets stInfer
      case openVar vs t of
        -- The marker is a use at String | Null (spec 4.3, 6.10).
        Just v -> addUse psp v (mkUnion TyString [TyNull])
        Nothing -> checkSecretType psp n t
      pure (n : secrets)

    checkParamType psp t =
      when (t == TyVoid) $
        abort (diag ETypeIllformed psp "Void cannot be a parameter type")

-- Inference of parameter types (spec 4.3) ------------------------------------------

-- | An inference variable: the type, or a part of the type, of a
-- parameter written without an annotation, while its function's body
-- is checked to find what it is used at.
data IVar = IVar
  { -- | The parameter it belongs to, for diagnostics.
    ivOrigin :: Text,
    -- | The order it was made in: a check of a body solves only the
    -- variables made since it began ('inferParams').
    ivSerial :: Int,
    -- | The shape a generic use gave it, with variables of its own.
    -- Once it has one, its uses are passed on to those.
    ivShape :: Maybe Type,
    -- | The types it is used at, with where.
    ivUppers :: [(Span, Type)],
    -- | The bounds its uses need of it: named bounds and classes.
    ivBounds :: [Bound],
    -- | The types of the values the body puts where it is, as the
    -- other branches of a choice: what it is when it is used at none.
    ivLowers :: [Type],
    -- | Where a case dispatches on it by type (spec 4.3, 6.4): it is
    -- then to be Any or a union, which only can be dispatched on.
    ivDispatch :: Maybe Span
  }

-- | Inference variables are named @?x%n@: a @?@ cannot begin a type
-- name, so no source writes one.
isInferVar :: Text -> Bool
isInferVar v = "?" `T.isPrefixOf` v

-- | Whether a check is finding the types of parameters right now.
inferActive :: TC Bool
inferActive = gets (not . Map.null . stInfer)

-- | A fresh inference variable for a part of the type of @origin@.
newInferVar :: Text -> TC Type
newInferVar origin = do
  n <- gets stInferNext
  let v = "?" <> origin <> "%" <> tshowInt n
  modify $ \s ->
    s
      { stInferNext = n + 1,
        stInfer = Map.insert v (IVar origin n Nothing [] [] [] Nothing) (stInfer s)
      }
  pure (TyVar v)

-- | A type with the shapes found so far put in for their variables.
zonk :: Type -> TC Type
zonk t = do
  vs <- gets stInfer
  pure (zonkWith vs t)

zonkWith :: Map Text IVar -> Type -> Type
zonkWith vs = go (0 :: Int)
  where
    go d t
      | d > 64 = t
      | otherwise = case t of
          TyVar v
            | Just iv <- Map.lookup v vs, Just sh <- ivShape iv -> go (d + 1) sh
          TyArray e -> TyArray (go d e)
          TyMap e -> TyMap (go d e)
          TyAsync e -> TyAsync (go d e)
          TyRecord fs -> TyRecord (Map.map (\f -> f {fieldType = go d (fieldType f)}) fs)
          TyFun ps r -> TyFun (map (go d) ps) (go d r)
          TyUnion ts -> case map (go d) ts of
            (u : us) -> mkUnion u us
            [] -> t
          _ -> t

-- | The inference variable a type is, when it is one with no shape.
openVar :: Map Text IVar -> Type -> Maybe Text
openVar vs t = case zonkWith vs t of
  TyVar v | isInferVar v, Just iv <- Map.lookup v vs, isNothing (ivShape iv) -> Just v
  _ -> Nothing

hasInfer :: Type -> Bool
hasInfer = any isInferVar . typeVars

modifyVar :: Text -> (IVar -> IVar) -> TC ()
modifyVar v f = modify (\s -> s {stInfer = Map.adjust f v (stInfer s)})

addUse :: Span -> Text -> Type -> TC ()
addUse sp v u = modifyVar v (\iv -> iv {ivUppers = ivUppers iv <> [(sp, u)]})

-- | A value of a type put where the variable is. Null joins with any
-- type (spec 4.3) and says nothing of it.
addLower :: Text -> Type -> TC ()
addLower v t
  | t == TyNull || hasInfer t = pure ()
  | otherwise = modifyVar v (\iv -> iv {ivLowers = nub (ivLowers iv <> [t])})

addVarBound :: Text -> Bound -> TC ()
addVarBound v nb = modifyVar v (\iv -> iv {ivBounds = nub (ivBounds iv <> [nb])})

-- | The scrutinee of a case with type heads (spec 4.3): a use at Any,
-- and the variable is to be a type that can be dispatched on.
addDispatch :: Span -> Text -> TC ()
addDispatch sp v = do
  addUse sp v TyAny
  modifyVar v (\iv -> iv {ivDispatch = ivDispatch iv <|> Just sp})

-- | Give an open variable a shape. The uses it had are uses of the
-- shape now, and a variable it is made the same as takes its bounds.
refineVar :: Ctx -> Text -> Type -> TC ()
refineVar ctx v sh = do
  vs <- gets stInfer
  let sh' = zonkWith vs sh
  case Map.lookup v vs of
    Just iv
      | isNothing (ivShape iv),
        v `notElem` typeVars sh' -> do
          modifyVar v (\i -> i {ivShape = Just sh', ivUppers = [], ivBounds = [], ivLowers = []})
          mapM_ (\(sp, u) -> constrain ctx sp sh' u) (ivUppers iv)
          case openVar vs sh' of
            Just w -> do
              mapM_ (addVarBound w) (ivBounds iv)
              mapM_ (addLower w) (ivLowers iv)
              mapM_ (\dsp -> modifyVar w (\i -> i {ivDispatch = ivDispatch i <|> Just dsp})) (ivDispatch iv)
            Nothing -> pure ()
    _ -> pure ()

-- | Conformance (spec 4.4) that, while parameter types are inferred,
-- takes a value whose type is still to be found as used at the type
-- it is placed at. Where the answer depends on what is not known yet,
-- it is yes: the check made with the inferred types decides.
conformsM :: Ctx -> Span -> Type -> Type -> TC Bool
conformsM ctx sp t u = do
  active <- inferActive
  if active then constrain ctx sp t u else pure (conforms ctx t u)

constrain :: Ctx -> Span -> Type -> Type -> TC Bool
constrain ctx sp a0 e0 = do
  vs <- gets stInfer
  let a = zonkWith vs a0
      e = zonkWith vs e0
  if conforms ctx a e
    then pure True
    else
      if not (hasInfer a || hasInfer e)
        then pure False
        else case (openVar vs a, openVar vs e) of
          (Just v, Just w)
            | v == w -> pure True
            | otherwise -> True <$ refineVar ctx v (TyVar w)
          (Just v, Nothing)
            | hasInfer e -> True <$ refineVar ctx v e
            | otherwise -> True <$ addUse sp v e
          -- A value placed where a parameter's type is: that says what
          -- may be passed, not what the parameter is, unless nothing
          -- else does.
          (Nothing, Just w) -> True <$ addLower w a
          _ -> structural a e
  where
    both x y = (&&) <$> constrain ctx sp x y <*> constrain ctx sp y x
    structural a e = case (a, e) of
      (TyUnion ms, _) -> and <$> mapM (\m -> constrain ctx sp m e) ms
      (TyArray x, TyArray y) -> both x y
      (TyMap x, TyMap y) -> both x y
      (TyAsync x, TyAsync y) -> both x y
      (TyRecord fa, TyRecord fe)
        | Map.keysSet fa == Map.keysSet fe,
          map fieldOptional (Map.elems fa) == map fieldOptional (Map.elems fe) ->
            and <$> zipWithM both (map fieldType (Map.elems fa)) (map fieldType (Map.elems fe))
      (TyFun ps r, TyFun qs r')
        | length ps == length qs ->
            and <$> sequence (zipWith both ps qs <> [both r r'])
      (_, TyUnion us) -> case filter (sameHead a) us of
        [u] -> constrain ctx sp a u
        _ -> pure (hasInfer a)
      _ -> pure False
    sameHead x y = case (x, y) of
      (TyArray _, TyArray _) -> True
      (TyMap _, TyMap _) -> True
      (TyAsync _, TyAsync _) -> True
      (TyRecord _, TyRecord _) -> True
      (TyFun _ _, TyFun _ _) -> True
      _ -> False

-- | A named predicate (spec 4.4), where a value whose type is still to
-- be found is taken as needing it.
holdsM :: Ctx -> NamedBound -> Type -> TC Bool
holdsM ctx nb t0 = do
  active <- inferActive
  path <- currentModule ctx
  if not active
    then pure (holdsAt ctx path nb t0)
    else do
      vs <- gets stInfer
      let t = zonkWith vs t0
          open = [v | v <- typeVars t, Just _ <- [openVar vs (TyVar v)]]
          ok =
            satisfiesNamed (\v -> if v `elem` open then Just (BoundNamed nb) else boundOf ctx v) nb t
              || holdsAt ctx path nb t
      when ok (mapM_ (`addVarBound` BoundNamed nb) open)
      pure ok

-- | 'satisfiesBound', with 'holdsM' and 'conformsM'.
satisfiesBoundM :: Ctx -> Span -> Bound -> Type -> TC Bool
satisfiesBoundM ctx sp b t = case b of
  BoundNamed nb -> holdsM ctx nb t
  BoundType u -> conformsM ctx sp t u
  BoundClass c -> classHoldsM ctx c t
  BoundAll bs -> and <$> mapM (\b' -> satisfiesBoundM ctx sp b' t) bs

-- | 'classHolds', where a value whose type is still to be found is
-- taken as needing the class (spec 4.3), and one whose type is found
-- only in part is left to the check made with the inferred types.
classHoldsM :: Ctx -> ClassRef -> Type -> TC Bool
classHoldsM ctx c t0 = do
  active <- inferActive
  vs <- gets stInfer
  path <- currentModule ctx
  let t = zonkWith vs t0
  case openVar vs t of
    Just v | active -> True <$ addVarBound v (BoundClass c)
    _
      | active && hasInfer t -> pure True
      | otherwise -> pure (classHolds ctx path c t)

-- | The module of the declaration being elaborated.
currentModule :: Ctx -> TC FilePath
currentModule ctx = gets (maybe (progEntry (ctxProg ctx)) fst . listToMaybe . stCurrent)

-- | 'unifyOrFail', where an argument whose type is still to be found
-- takes the shape the callee's parameter gives it (spec 4.3): an
-- @Array<T>@ parameter makes it an array of a variable still to be
-- found in turn.
unifyM :: Ctx -> Span -> Type -> Type -> Subst -> TC Subst
unifyM ctx sp pat0 actual0 s0 = do
  active <- inferActive
  if not active || not (hasInfer pat0 || hasInfer actual0)
    then unifyOrFail sp pat0 actual0 s0
    else go pat0 actual0 s0
  where
    calleeVars p = [v | v <- typeVars p, not (isInferVar v), not (v `Set.member` ctxTypeVars ctx)]
    go pat actual s = do
      vs <- gets stInfer
      let p = zonkWith vs pat
          a = zonkWith vs actual
      case p of
        TyVar v
          | isInferVar v -> pure s
          | otherwise -> case Map.lookup v s of
              Nothing -> pure (Map.insert v a s)
              Just b -> do
                b' <- zonk b
                if b' == a
                  then pure s
                  else do
                    ok <- constrain ctx sp a b'
                    if ok then pure s else unifyOrFail sp p a s
        _
          | Just w <- openVar vs a ->
              if null (calleeVars p)
                then s <$ constrain ctx sp a p
                else case p of
                  TyUnion ms ->
                    pure (foldl (\acc m -> case m of TyVar u | not (Map.member u acc) -> Map.insert u a acc; _ -> acc) s ms)
                  _ -> do
                    sh <- freshShape (maybe w ivOrigin (Map.lookup w vs)) p
                    refineVar ctx w sh
                    go p sh s
          | otherwise -> case (p, a) of
              (TyArray x, TyArray y) -> go x y s
              (TyMap x, TyMap y) -> go x y s
              (TyAsync x, TyAsync y) -> go x y s
              (TyRecord fp, TyRecord fa)
                | Map.keysSet fp == Map.keysSet fa ->
                    foldM (\acc (x, y) -> go (fieldType x) (fieldType y) acc) s (zip (Map.elems fp) (Map.elems fa))
              (TyFun ps r, TyFun qs r')
                | length ps == length qs -> do
                    s' <- foldM (\acc (x, y) -> go x y acc) s (zip ps qs)
                    go r r' s'
              _
                | hasInfer p || hasInfer a -> pure s
                | otherwise -> unifyOrFail sp p a s
    -- The shape of a pattern, with a fresh variable for each of its
    -- variables.
    freshShape origin t = case t of
      TyVar _ -> newInferVar origin
      TyArray e -> TyArray <$> freshShape origin e
      TyMap e -> TyMap <$> freshShape origin e
      TyAsync e -> TyAsync <$> freshShape origin e
      TyRecord fs -> TyRecord <$> traverse (\f -> (\ft -> f {fieldType = ft}) <$> freshShape origin (fieldType f)) fs
      TyFun ps r -> TyFun <$> mapM (freshShape origin) ps <*> freshShape origin r
      _ -> pure t

-- | Give a value whose type is still to be found the shape its use
-- needs (an @Array@ iterated over, an @AsyncHandle@ awaited).
shapeAs :: Ctx -> Type -> (Type -> Type) -> TC Type
shapeAs ctx t ctor = do
  vs <- gets stInfer
  case openVar vs t of
    Just v -> do
      e <- newInferVar (maybe v ivOrigin (Map.lookup v vs))
      refineVar ctx v (ctor e)
      pure (ctor e)
    Nothing -> pure (zonkWith vs t)

-- | A field access, an index or a call on a value whose type is still
-- to be found does not say what the whole type is (spec 4.3).
cannotTell :: ErrorCode -> Span -> Text -> Type -> TC ()
cannotTell code sp what t = do
  vs <- gets stInfer
  case openVar vs t of
    Just v ->
      abort . diag code sp $
        what
          <> " does not say what the whole type of '"
          <> maybe v ivOrigin (Map.lookup v vs)
          <> "' is; annotate the parameter"
    Nothing -> pure ()

-- | The greatest type that conforms to both (spec 4.3): the one a
-- caller can pass the most values of.
meetType :: Ctx -> Type -> Type -> Maybe Type
meetType ctx a b
  | conforms ctx a b = Just a
  | conforms ctx b a = Just b
  | TyUnion ms <- a = members [m' | m <- ms, Just m' <- [meetType ctx m b]]
  | TyUnion ms <- b = members [m' | m <- ms, Just m' <- [meetType ctx a m]]
  | otherwise = Nothing
  where
    members [] = Nothing
    members (m : ms) = Just (mkUnion m ms)

-- | Whether a parameter's type is inferred (spec 4.3): a keyword
-- parameter with a default takes the default's type.
needsInference :: Param -> Bool
needsInference (Param _ f) = case f of
  PPositional _ _ Nothing -> True
  PVariadic _ Nothing -> True
  PKeyword _ _ Nothing Nothing -> True
  _ -> False

-- | The types of the parameters a function writes without annotations
-- (spec 4.3), found from how its body uses them, and written into its
-- parameter list. The body is checked once to find them, quietly, and
-- the caller checks it again with them: that check is the one that
-- reports, and it holds the body to the types as to written ones.
--
-- A parameter used at no type becomes a type parameter, returned with
-- its bound, when the function is bound to a name (@generalize@).
-- Otherwise it is @Any@, as a function value has no type parameters.
inferParams ::
  Ctx ->
  FilePath ->
  Locals ->
  Bool ->
  [Param] ->
  Maybe SType ->
  Expr ->
  TC ([Param], [TypeParam])
inferParams ctx path locals generalize ps retAnn body
  | not (any needsInference ps) = pure (ps, [])
  | otherwise = do
      st0 <- get
      let n0 = stInferNext st0
      modify (\s -> s {stQuiet = True})
      (params, _, bodyLocals, secretParams) <- elabParamsWith True ctx path locals ps
      retTy <- traverse (typeFromS ctx path) retAnn
      let body' = wrapSecretParams secretParams body
      case retTy of
        Just t -> () <$ check ctx path bodyLocals body' t
        Nothing -> () <$ infer ctx path bodyLocals body'
      modify (\s -> s {stQuiet = stQuiet st0})
      vs <- gets stInfer
      let paramTys =
            Map.fromList $
              [(n, zonkWith vs t) | (n, t) <- spPositional params]
                <> [(n, zonkWith vs (TyArray e)) | Just (n, e) <- [spVariadic params]]
                <> [(n, zonkWith vs t) | (n, t) <- spKeywords params]
          inferred = [(psp, n, paramTys Map.! n) | p@(Param psp f) <- ps, needsInference p, let n = paramName f]
          mine v = maybe False ((>= n0) . ivSerial) (Map.lookup v vs)
          open = nub [v | (_, _, t) <- inferred, v <- typeVars t, mine v, isJust (openVar vs (TyVar v))]
      let spanOf o = fromMaybe NoSpan (listToMaybe [psp | (psp, n, _) <- inferred, n == o])
      solved <- mapM (solveVar spanOf vs) open
      let taken = Set.map varDisplay (ctxTypeVars ctx) <> aliasNames <> Set.fromList builtinTypeNames
          names = filter (`Set.notMember` taken) candidates
          generic = [(v, b) | (v, Left b) <- solved]
          named = zip generic names
          subst =
            Map.fromList $
              [(v, t) | (v, Right t) <- solved]
                <> [(v, if generalize then TyVar (nm <> "%") else TyAny) | ((v, _), nm) <- named]
          tparams =
            [ TypeParam (Spanned NoSpan (nm <> "%")) (fmap writtenBound b)
            | generalize,
              ((_, b), nm) <- named
            ]
          annotate p@(Param psp f)
            | needsInference p =
                let t = applySubst subst (paramTys Map.! paramName f)
                    ann = Just (typeToS psp t)
                 in Param psp $ case f of
                      PPositional n sec _ -> PPositional n sec ann
                      PVariadic n _ -> PVariadic n ann
                      PKeyword n sec _ dflt -> PKeyword n sec ann dflt
            | otherwise = p
      modify (\s -> s {stInfer = Map.filter ((< n0) . ivSerial) (stInfer s)})
      pure (map annotate ps, tparams)
  where
    paramName f = case f of
      PPositional n _ _ -> n
      PVariadic n _ -> n
      PKeyword n _ _ _ -> n
    aliasNames = maybe Set.empty (Set.fromList . Map.keys . gsTypes) (Map.lookup path (ctxScopes ctx))
    candidates = ["T", "U", "V", "W", "X", "Y", "Z"] <> ["T" <> tshowInt i | i <- [1 :: Int ..]]
    -- A variable used at types is the greatest type that conforms to
    -- all of them; one used at none is a type parameter, with the
    -- bound its uses need.
    solveVar spanOf vs v = do
      let iv = vs Map.! v
      case ivUppers iv of
        [] -> case ivLowers iv of
          [l] -> pure (v, Right l)
          _ -> (,) v . Left <$> boundFor (spanOf (ivOrigin iv)) iv
        (sp0, u0) : rest -> do
          let step (_, acc) (sp, u) = case meetType ctx acc u of
                Just m -> pure (sp, m)
                Nothing ->
                  abort . diag ETypeMismatch sp $
                    "'"
                      <> ivOrigin iv
                      <> "' is used as "
                      <> renderType acc
                      <> " and here as "
                      <> renderType u
                      <> ", and no type is both; annotate the parameter"
          (_, m) <- foldM step (sp0, u0) rest
          checkDispatch iv m
          pure (v, Right m)
    -- A type is dispatched on only in Any or a union (spec 4.3, 6.4):
    -- a use outside the case that narrows the parameter to one type
    -- leaves nothing to dispatch on.
    checkDispatch iv m = case ivDispatch iv of
      Just dsp
        | not (dispatchable m),
          (sp, u) : _ <- [use | use@(_, u') <- ivUppers iv, not (dispatchable u')] ->
            abort . withNote ("the case that dispatches on it is at " <> renderAt dsp) . diag ETypeMismatch sp $
              "'"
                <> ivOrigin iv
                <> "' is dispatched on by type in a case, and used here as "
                <> renderType u
                <> "; a type is dispatched on only in Any or a union, so annotate the parameter"
      _ -> pure ()
    dispatchable t = case t of
      TyAny -> True
      TyUnion _ -> True
      _ -> False
    renderAt (Span (Position f l _) _) = T.pack f <> ":" <> T.pack (show l)
    renderAt NoSpan = "its case"
    -- The bound as it would be written, for the declaration checked
    -- with the inferred parameters written in.
    writtenBound b = case b of
      BoundClass c -> SBoundClass (crModule c) (crName c)
      BoundAll bs -> SBoundAll (map writtenBound bs)
      _ -> SBoundNamed (Spanned NoSpan (renderBound b))
    -- What the uses need, less what another of them entails: one
    -- bound, or several joined with + (spec 4.2).
    boundFor _ iv = case nub (concatMap boundParts (ivBounds iv)) of
      [] -> pure Nothing
      bs -> case [b | b <- bs, not (any (\b' -> b' /= b && entailsBound b' b) bs)] of
        [b] -> pure (Just b)
        bs' -> pure (Just (BoundAll bs'))

-- Bidirectional elaboration --------------------------------------------------------

infer :: Ctx -> FilePath -> Locals -> Expr -> TC (Core, Type)
infer ctx path locals (Expr sp f) = case f of
  ENull -> pure (Core sp CNull, TyNull)
  EBool b -> pure (Core sp (CBool b), TyBool)
  ENumber n -> pure (Core sp (CNumber n), TyNumber)
  EString parts -> elabString ctx path locals sp parts
  EVar n -> inferVar ctx path locals sp n
  EArray es -> do
    elems <- mapM (infer ctx path locals) es
    let tys = map snd elems
        elemTy = case tys of
          [] -> TyAny
          (t : ts) | all (== t) ts -> t
          _ -> TyAny
    pure (Core sp (CArray (map fst elems)), TyArray elemTy)
  EObject kvs -> do
    fields <- objectFields ctx path locals kvs
    let recTy = TyRecord (Map.fromList [(k, requiredField t) | (k, _, t) <- fields])
    pure (Core sp (CRecordLit [(k, c) | (k, c, _) <- fields]), recTy)
  ELambda tps ps rt body -> do
    monoLambda sp tps
    requiredAsValue sp "a lambda" ps
    (lam, ty, _) <- elabLambda ctx path locals sp Nothing ps rt body
    pure (lam, ty)
  ECall fn args -> elabCall ctx path locals sp fn args Nothing
  EDot inner (Spanned fsp fld) -> elabDot ctx path locals sp inner fsp fld
  EIndex inner idx -> elabIndex ctx path locals sp inner idx
  EBin op a b -> elabBin ctx path locals sp op a b Nothing
  -- -e is 0 - e (spec 6.2).
  ENeg inner -> do
    c <- check ctx path locals inner TyNumber
    pure (Core sp (CBin PSub (Core sp (CNumber 0)) c), TyNumber)
  ENot inner -> do
    c <- check ctx path locals inner TyBool
    pure (Core sp (CNot c), TyBool)
  EDo block -> do
    (stmts, ty) <- elabBlock ctx path locals block Nothing
    pure (Core sp (CDo stmts), ty)
  EIf c t (Just el) -> elabIfElse ctx path locals sp c t el False
  EIf c t Nothing -> elabIfNoElse ctx path locals sp c t Nothing False
  ECond c a b -> infer ctx path locals (condAsIf sp c a b)
  EFor (Spanned xsp x) xs body -> do
    (xsCore, xsTy0) <- infer ctx path locals xs
    xsTy <- shapeAs ctx xsTy0 TyArray
    elemTy <- case xsTy of
      TyArray t -> pure t
      other -> mismatch (exprSpan xs) (TyArray TyAny) other
    (bodyStmts, bodyTy) <- elabBlock ctx path (Map.insert x elemTy locals) body Nothing
    let bodyLam =
          Core (blockSpan body) . CLam $
            Lam
              { lamName = lambdaName (Span (spanStart xsp) (spanStart xsp)),
                lamModule = path,
                lamPositional = [x],
                lamVariadic = Nothing,
                lamKeywords = [],
                lamBody = Core (blockSpan body) (CDo bodyStmts),
                lamType = TyFun [elemTy] bodyTy
              }
        (fnName, resTy) =
          if bodyTy == TyVoid
            then ("for_each", TyVoid)
            else ("map", TyArray bodyTy)
    pure (Core sp (CApp (Core sp (CVar (BuiltinRef fnName))) [xsCore, bodyLam] []), resTy)
  ECase scrut arms -> elabCase ctx path locals sp scrut arms Nothing False
  ETry body mCatch mFin -> elabTry ctx path locals sp body mCatch mFin Nothing False
  EAsync inner -> do
    (c, t) <- infer ctx path locals inner
    let thunk = mkThunk "async" path sp c t
    pure (Core sp (CApp (Core sp (CVar (BuiltinRef "spawn"))) [thunk] []), TyAsync t)
  EAwait inner -> do
    (c, t0) <- infer ctx path locals inner
    t <- shapeAs ctx t0 TyAsync
    case t of
      TyAsync r -> pure (Core sp (CAwait c), r)
      other -> mismatch (exprSpan inner) (TyAsync TyAny) other
  ECommand stream env parts -> elabCommand ctx path locals sp stream env parts
  EEnv h args opts -> do
    (env, envTy) <- elabEnv ctx path locals sp h args
    -- #head{k: v} stands for runnable(#head, k = v) (spec 6.7).
    case opts of
      Nothing -> pure (env, envTy)
      Just os -> elabRunnable ctx path locals sp env os

spanStart :: Span -> Position
spanStart (Span s _) = s
spanStart NoSpan = Position "" 0 0

mkThunk :: Text -> FilePath -> Span -> Core -> Type -> Core
mkThunk what path sp body ty =
  Core sp . CLam $
    Lam
      { lamName = generatedName what sp,
        lamModule = path,
        lamPositional = [],
        lamVariadic = Nothing,
        lamKeywords = [],
        lamBody = body,
        lamType = TyFun [] ty
      }

-- | @if (c) { t }@ without @else@ (spec 6.4). When the condition is
-- false its value is @null@, so it is @T | Null@ for a block of type
-- @T@; a @Void@ block makes it @Void@, as an empty @else@ would. With
-- an expected type the block is checked against it, and @Null@ must
-- conform to it too. A statement whose value is discarded (the last
-- argument) may have a block of any type.
elabIfNoElse :: Ctx -> FilePath -> Locals -> Span -> Expr -> Block -> Maybe Type -> Bool -> TC (Core, Type)
elabIfNoElse ctx path locals sp c t mExpected discarded = do
  condCore <- check ctx path locals c TyBool
  (thenStmts, thenTy) <- case mExpected of
    Just TyVoid -> elabBlock ctx path locals t (Just TyVoid)
    Just e -> do
      nullOk <- conformsM ctx sp TyNull e
      unless nullOk $
        abort . diag ETypeMismatch sp $
          "an if without else is null when its condition is false, and Null does not conform to "
            <> renderType e
            <> "; add an else block"
      elabBlock ctx path locals t (Just e)
    Nothing -> do
      r <- tryTC (elabBlock ctx path locals t Nothing)
      case r of
        Right ok -> pure ok
        -- A block typed by its context, such as fail(e), has none
        -- here; it is checked as the Void an empty else would give it.
        Left err -> either (const (abort err)) pure =<< tryTC (elabBlock ctx path locals t (Just TyVoid))
  let thenCore = Core (blockSpan t) (CDo thenStmts)
      orElse ty elseF = pure (Core sp (CIf condCore thenCore (Core sp elseF)), ty)
  case mExpected of
    Just e -> orElse e (if e == TyVoid then CDo [] else CNull)
    Nothing
      | thenTy == TyVoid -> orElse TyVoid (CDo [])
      | discarded -> orElse thenTy CNull
      | dataType thenTy -> orElse (mkUnion thenTy [TyNull]) CNull
      | otherwise ->
          abort . diag ETypeIllformed sp $
            "an if without else has the type "
              <> renderType thenTy
              <> " | Null, which is not a type: only a data type can be a member of a union; add an else block"

-- | One branch of an expression that chooses between several (spec
-- 4.3): how it is elaborated with no expected type, and how it is
-- checked against one.
data Branch a = Branch
  { branchInfer :: TC (a, Type),
    branchCheck :: Type -> TC a
  }

-- | The branches of an @if@, a conditional, a @case@ or a @try@ with
-- no expected type (spec 4.3, 6.2, 6.4, 6.9). They share one type,
-- which comes from the first branch that infers on its own as other
-- than @Null@; every other branch is checked against it, so a
-- context-typed call such as @fail(e)@ or @cast@ may be any of them
-- (15.7). A branch that is @Null@ instead joins the result with
-- @Null@: @T | Null@, which must be a type. Branches that fail to
-- infer commit nothing ('tryTC'). When none infers, the first
-- branch's diagnostic is the report. A statement whose value is
-- discarded (the first argument) is not held to @T | Null@ being a
-- type, as an if without else is not ('elabIfNoElse'), and keeps @T@.
-- A branch of a wider type, one the shared type conforms to, widens it
-- (4.3): which branch comes first does not decide the type.
joinBranches :: Ctx -> Bool -> Span -> [Branch a] -> TC ([a], Type)
joinBranches ctx discarded sp branches = go [] Nothing Nothing (zip [0 :: Int ..] branches)
  where
    -- @done@ holds the branches elaborated so far, by position;
    -- @pending@ those that did not infer on their own, still to be
    -- checked against the shared type once there is one.
    go done mAnchor mErr [] = finish done mAnchor mErr
    go done mAnchor mErr ((i, b) : rest) = case mAnchor of
      Just t -> do
        r <- tryTC (branchCheck b t)
        case r of
          Right a -> go ((i, Right (a, False)) : done) mAnchor mErr rest
          Left err -> do
            -- A branch that is null where the others are T joins them
            -- with Null rather than conflicting with them.
            r2 <- tryTC (branchInfer b)
            case r2 of
              Right (a, TyNull) -> go ((i, Right (a, True)) : done) mAnchor mErr rest
              -- A wider branch: the branches so far conform to it too.
              Right (a, u) | conforms ctx t u -> go ((i, Right (a, False)) : done) (Just u) mErr rest
              _ -> abort err
      Nothing -> do
        r <- tryTC (branchInfer b)
        case r of
          Right (a, TyNull) -> go ((i, Right (a, True)) : done) Nothing mErr rest
          Right (a, t) -> go ((i, Right (a, False)) : done) (Just t) mErr rest
          Left d -> go ((i, Left b) : done) Nothing (maybe (Just d) Just mErr) rest

    finish done mAnchor mErr = do
      let sawNull = or [n | (_, Right (_, n)) <- done]
      shared <- case mAnchor of
        Just t -> pure t
        -- Every branch that inferred is null: the others are checked
        -- as Null, which is then the type of the whole.
        Nothing
          | sawNull -> pure TyNull
          | otherwise -> abort (fromMaybe (diag ETypeMismatch sp "these branches have no type") mErr)
      results <- mapM (elabPending shared) (sortOn fst done)
      ty <- joined sawNull shared
      pure (results, ty)

    joined sawNull shared
      | not sawNull || shared == TyNull = pure shared
      | dataType shared = pure (mkUnion shared [TyNull])
      | discarded = pure shared
      | otherwise =
          abort . diag ETypeIllformed sp $
            "one branch is null and another is "
              <> renderType shared
              <> ", and "
              <> renderType shared
              <> " | Null is not a type: only a data type can be a member of a union"

    elabPending _ (_, Right (a, _)) = pure a
    elabPending t (_, Left b) = branchCheck b t

-- | @if (c) { t } else { el }@ with no expected type (spec 6.4): the
-- two branches share a type, or join it with Null (4.3). The last
-- argument says whether the value is discarded ('joinBranches').
elabIfElse :: Ctx -> FilePath -> Locals -> Span -> Expr -> Block -> Block -> Bool -> TC (Core, Type)
elabIfElse ctx path locals sp c t el discarded = do
  condCore <- check ctx path locals c TyBool
  (stmtss, ty) <- joinBranches ctx discarded sp [blockBranch ctx path locals t, blockBranch ctx path locals el]
  (thenStmts, elseStmts) <- case stmtss of
    [a, b] -> pure (a, b)
    _ -> abort (diag ETypeMismatch sp "an if has two branches")
  pure
    ( Core sp (CIf condCore (Core (blockSpan t) (CDo thenStmts)) (Core (blockSpan el) (CDo elseStmts))),
      ty
    )

-- | A block as one of the branches of 'joinBranches'.
blockBranch :: Ctx -> FilePath -> Locals -> Block -> Branch [CoreStmt]
blockBranch ctx path locals blk =
  Branch
    { branchInfer = elabBlock ctx path locals blk Nothing,
      branchCheck = \t -> fst <$> elabBlock ctx path locals blk (Just t)
    }

-- | @c ? a : b@ is @if (c) { a } else { b }@ (spec 6.2), and is
-- typed and evaluated as that @IfExpr@ is (6.4).
condAsIf :: Span -> Expr -> Expr -> Expr -> Expr
condAsIf sp c a b = Expr sp (EIf c (branch a) (Just (branch b)))
  where
    branch e = Block (exprSpan e) [Stmt (exprSpan e) (SExpr e)]

check :: Ctx -> FilePath -> Locals -> Expr -> Type -> TC Core
check ctx path locals e@(Expr sp f) expected = case f of
  EArray es | TyArray t <- expected -> do
    cs <- mapM (\el -> check ctx path locals el t) es
    pure (Core sp (CArray cs))
  -- Against a union with one array among its members, as a parameter
  -- that is a list or null, a list written out is checked as that
  -- array, so that `[]` is a list of its elements rather than of Any.
  EArray es
    | TyUnion ts <- expected,
      [TyArray t] <- [x | x@(TyArray _) <- ts] -> do
        cs <- mapM (\el -> check ctx path locals el t) es
        pure (Core sp (CArray cs))
  EObject kvs -> checkObject ctx path locals sp kvs expected
  ELambda tps ps rt body
    | TyFun expPs expRet <- expected -> do
        monoLambda sp tps
        (lam, ty, _) <- elabLambdaAgainst ctx path locals sp ps rt body expPs (Just expRet)
        conformsM ctx sp ty expected >>= \ok -> unless ok (mismatch sp expected ty)
        pure lam
  ECond c a b -> check ctx path locals (condAsIf sp c a b) expected
  EIf c t Nothing | expected /= TyAny -> fst <$> elabIfNoElse ctx path locals sp c t (Just expected) False
  EIf c t (Just el) | expected /= TyAny -> do
    condCore <- check ctx path locals c TyBool
    (thenStmts, _) <- elabBlock ctx path locals t (Just expected)
    (elseStmts, _) <- elabBlock ctx path locals el (Just expected)
    pure (Core sp (CIf condCore (Core (blockSpan t) (CDo thenStmts)) (Core (blockSpan el) (CDo elseStmts))))
  EDo block | expected /= TyAny -> do
    (stmts, _) <- elabBlock ctx path locals block (Just expected)
    pure (Core sp (CDo stmts))
  ECase scrut arms | expected /= TyAny -> do
    (c, _) <- elabCase ctx path locals sp scrut arms (Just expected) False
    pure c
  ETry body mCatch mFin | expected /= TyAny -> do
    (c, _) <- elabTry ctx path locals sp body mCatch mFin (Just expected) False
    pure c
  ECall fn args -> do
    (c, t) <- elabCall ctx path locals sp fn args (Just expected)
    conformsM ctx sp t expected >>= \ok -> unless ok (mismatch sp expected t)
    pure c
  -- A generic local function referenced as a value: the expected type
  -- has to determine every one of its type variables (spec 4.4, 6.1).
  EVar n
    | Just t <- Map.lookup n locals,
      gvs@(_ : _) <- generalizedVars t -> do
        subst <- unifyM ctx sp t expected Map.empty
        bounds <- gets stLocalBounds
        refBounds ctx sp n (Map.restrictKeys bounds (Set.fromList gvs)) subst
        let t' = applySubst subst t
        ok <- conformsM ctx sp t' expected
        unless (null (generalizedVars t') && ok) (mismatch sp expected t')
        recordVar sp n t' Nothing
        pure (Core sp (CVar (LocalRef n)))
  EVar n
    | not (Map.member n locals),
      Just (VTopLevel defPath defName) <- lookupValueTarget ctx path n -> do
        declAsValue ctx sp n (defPath, defName)
        -- A declaration with type parameters, referenced as a value:
        -- the expected type has to determine every one of them (4.4).
        (bounds, t) <- declTypeBounds ctx (defPath, defName)
        (t', inst) <-
          if settled ctx t
            then pure (t, [(v, TyVar v) | (v, b) <- Map.toList bounds, dictBound b])
            else do
              subst <- unifyM ctx sp t expected Map.empty
              refBounds ctx sp n bounds subst
              pure (applySubst subst t, [(v, applySubst subst (TyVar v)) | (v, b) <- Map.toList bounds, dictBound b])
        ok <- conformsM ctx sp t' expected
        unless (settled ctx t' && ok) (mismatch sp expected t')
        recordVar sp n t' (Just (defPath, defName))
        pure (instantiateRef (Core sp (CVar (TopRef defPath defName))) inst)
  EVar n
    | not (Map.member n locals),
      Just (VBuiltin bn) <- lookupValueTarget ctx path n,
      Just scheme <- Map.lookup bn builtinSchemes,
      not (null (schemeVars scheme)) -> do
        -- Builtin polymorphic function as a value: instantiate from
        -- the expected type (spec 4.4).
        subst <- unifyM ctx sp (schemeType scheme) expected Map.empty
        let t = applySubst subst (schemeType scheme)
        ok <- conformsM ctx sp t expected
        unless (all (`Map.member` subst) (schemeVars scheme) && ok) $
          mismatch sp expected t
        refBounds ctx sp n (schemeBounds scheme) subst
        recordBuiltin sp n t bn
        builtinRef ctx sp bn (schemeBounds scheme) subst
  EBin op a b | isEqOp op || expected == TyBool -> do
    (c, t) <- elabBin ctx path locals sp op a b (Just expected)
    conformsM ctx sp t expected >>= \ok -> unless ok (mismatch sp expected t)
    pure c
  _ -> do
    (c, t) <- infer ctx path locals e
    conformsM ctx sp t expected >>= \ok -> unless ok (mismatch sp expected t)
    pure c
  where
    isEqOp OpEq = True
    isEqOp OpNe = True
    isEqOp _ = False

-- | The bounds of a function referenced as a value, against the
-- instantiation its expected type determined (spec 4.4): a reference
-- is held to them as a call is.
refBounds :: Ctx -> Span -> Text -> Map Text Bound -> Subst -> TC ()
refBounds ctx sp name bounds subst =
  sequence_
    [ do
        ok <- satisfiesBoundM ctx sp b t
        unless ok (abort (boundViolation ctx sp name v b t))
    | (v, b) <- Map.toList bounds,
      Just t <- [Map.lookup v subst]
    ]

-- | Check a lambda against expected positional parameter types:
-- unannotated parameters adopt the expected types (spec 4.3).
elabLambdaAgainst ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  [Param] ->
  Maybe SType ->
  Expr ->
  [Type] ->
  Maybe Type ->
  TC (Core, Type, StaticParams)
elabLambdaAgainst ctx path locals sp ps rt body expPs expRet = do
  requiredAsValue sp "a lambda" ps
  let positionals = [p | p@(Param _ (PPositional _ _ _)) <- ps]
  ps' <-
    if length positionals == length expPs && length positionals == length ps
      then pure (zipWith adopt ps expPs)
      else pure ps
  elabLambdaExpecting ctx path locals sp Nothing expRet ps' rt body
  where
    adopt (Param psp (PPositional n sec Nothing)) expTy =
      Param psp (PPositional n sec (Just (typeToS psp expTy)))
    adopt p _ = p

-- | Encode a semantic type back into surface syntax for adoption.
typeToS :: Span -> Type -> SType
typeToS sp t = SType sp $ case t of
  TyAny -> SAny
  TyNumber -> SNumber
  TyString -> SString
  TyBool -> SBool
  TyNull -> SNull
  TyVoid -> SVoid
  TyEnvironment -> SEnvironment
  TyRunnable -> SRunnable
  TyArray e -> SArray (typeToS sp e)
  TyMap e -> SMap (typeToS sp e)
  TyRecord fs -> SRecord [(Spanned sp k, fieldOptional f, typeToS sp (fieldType f)) | (k, f) <- Map.toList fs]
  TyAsync e -> SAsyncHandle (typeToS sp e)
  TyFun psL r -> SFunction (map (typeToS sp) psL) (typeToS sp r)
  TyUnion ts -> SUnion (map (typeToS sp) ts)
  TyVar v -> SNamed Nothing v []

-- Variables ---------------------------------------------------------------------

lookupValueTarget :: Ctx -> FilePath -> Text -> Maybe ValueTarget
lookupValueTarget ctx path n =
  Map.lookup path (ctxScopes ctx) >>= Map.lookup n . gsValues

-- | A declaration referenced other than as the callee of a call is a
-- function value, which cannot have a required keyword parameter
-- (7.5): a call through a value binds no names.
declAsValue :: Ctx -> Span -> Text -> Key -> TC ()
declAsValue ctx sp n key = notAClass ctx sp n key >> case lookupDeclAst ctx key of
  Just (Decl _ (DFunction _ _ ps _ _ _)) -> requiredAsValue sp ("'" <> n <> "'") ps
  Just (Decl _ (DValue _ _ _ (Expr _ (ELambda _ ps _ _)))) -> requiredAsValue sp ("'" <> n <> "'") ps
  _ -> pure ()

requiredAsValue :: Span -> Text -> [Param] -> TC ()
requiredAsValue sp what ps = case [k | Param _ (PKeyword k _ _ Nothing) <- ps] of
  k : _ ->
    () <$ abort (diag ETypeKeyword sp (what <> " has the required keyword parameter '" <> k <> "', so it can only be called, not used as a value"))
  [] -> pure ()

inferVar :: Ctx -> FilePath -> Locals -> Span -> Text -> TC (Core, Type)
inferVar ctx path locals sp n = case Map.lookup n locals of
  Just t
    | not (null (generalizedVars t)) ->
        abort . diag ETypeMismatch sp $
          "cannot infer the type of '" <> n <> "' without an expected type: it declares type parameters"
  Just t0 -> do
    t <- zonk t0
    recordVar sp n t Nothing
    pure (Core sp (CVar (LocalRef n)), t)
  Nothing -> case lookupValueTarget ctx path n of
    Just (VTopLevel defPath defName) -> do
      declAsValue ctx sp n (defPath, defName)
      t <- declType ctx (defPath, defName)
      -- A function value carries no type variables of its own (spec
      -- 4.4), so a polymorphic declaration referenced as one has to be
      -- instantiated by the expected type -- which infer mode has not
      -- got.
      unless (settled ctx t) $
        abort . diag ETypeMismatch sp $
          "cannot infer the type of '" <> n <> "' without an expected type: it declares type parameters"
      recordVar sp n t (Just (defPath, defName))
      pure (Core sp (CVar (TopRef defPath defName)), t)
    Just (VBuiltin "stdin") -> do
      recordBuiltin sp n TyString "stdin"
      pure (Core sp (CVar (BuiltinRef "stdin")), TyString)
    Just (VBuiltin bn) -> case Map.lookup bn builtinSchemes of
      Just scheme
        | null (schemeVars scheme) -> do
            recordBuiltin sp n (schemeType scheme) bn
            pure (Core sp (CVar (BuiltinRef bn)), schemeType scheme)
        | otherwise ->
            abort . diag ETypeMismatch sp $
              "cannot infer the type of polymorphic builtin '" <> bn <> "' without an expected type"
      Nothing -> abort (undefinedName ctx path locals sp n)
    Nothing -> abort (undefinedName ctx path locals sp n)

-- | A name that resolves at no rank (spec 7.2), with the names in scope
-- that lie close to it: locals, top-level declarations, imported names,
-- builtins and namespaces.
undefinedName :: Ctx -> FilePath -> Locals -> Span -> Text -> Diagnostic
undefinedName ctx path locals sp n =
  suggesting n (Map.keys locals <> maybe [] scopeNames (Map.lookup path (ctxScopes ctx))) $
    diag ENameUndefined sp ("undefined name: '" <> n <> "'")
  where
    scopeNames gs = Map.keys (gsValues gs) <> Map.keys (gsNamespaces gs)

-- Strings and interpolation --------------------------------------------------------

elabString :: Ctx -> FilePath -> Locals -> Span -> [TextPart] -> TC (Core, Type)
elabString ctx path locals sp parts = do
  cs <- mapM part parts
  let core = case cs of
        [CPText t] -> CStrLit t
        [] -> CStrLit ""
        _ -> CStr cs
  pure (Core sp core, TyString)
  where
    part (TPChunk _ t) = pure (CPText t)
    part (TPInterp e) = CPExpr <$> hole e
    -- Only a command string has one; its mark keeps escaping away
    -- from it (6.6).
    part (TPRaw e) = do
      c <- hole e
      let sp' = exprSpan e
      pure (CPExpr (Core sp' (CApp (Core sp' (CVar (BuiltinRef rawBuiltin))) [Core sp' (CStr [CPExpr c])] [])))

    hole e = do
      (c, t) <- infer ctx path locals e
      ok <- holdsM ctx BStringifiable t
      unless ok $
        abort (diag ETypeBound (exprSpan e) ("cannot interpolate a value of type " <> renderType t <> ": it is not stringifiable"))
      pure c

-- Object literals ------------------------------------------------------------------

objectFields :: Ctx -> FilePath -> Locals -> [(Spanned Text, Expr)] -> TC [(Text, Core, Type)]
objectFields ctx path locals kvs = do
  checkDuplicateKeys kvs
  mapM
    ( \(Spanned _ k, v) -> do
        (c, t) <- infer ctx path locals v
        pure (k, c, t)
    )
    kvs

checkDuplicateKeys :: [(Spanned Text, Expr)] -> TC ()
checkDuplicateKeys kvs = go Set.empty kvs
  where
    go _ [] = pure ()
    go seen ((Spanned ksp k, _) : rest)
      | k `Set.member` seen =
          abort (diag ETypeFieldDuplicate ksp ("duplicate field: '" <> k <> "'"))
      | otherwise = go (Set.insert k seen) rest

-- | Expected-type-directed object literal checking (spec 4.3).
checkObject :: Ctx -> FilePath -> Locals -> Span -> [(Spanned Text, Expr)] -> Type -> TC Core
checkObject ctx path locals sp kvs expected = case expected of
  TyRecord fieldTys -> do
    checkDuplicateKeys kvs
    let litKeys = Set.fromList [k | (Spanned _ k, _) <- kvs]
        expKeys = Map.keysSet fieldTys
    -- Every required field has to be given, any optional one may be,
    -- and nothing outside the field set (spec 4.3, 4.2).
    unless (requiredNames fieldTys `Set.isSubsetOf` litKeys && litKeys `Set.isSubsetOf` expKeys) $
      abort . withExpectedActual (renderType expected) (renderKeys litKeys) $
        diag ETypeMismatch sp "object literal keys do not match the expected record fields"
    fields <-
      mapM
        ( \(Spanned _ k, v) -> do
            c <- check ctx path locals v (fieldType (fieldTys Map.! k))
            pure (k, c)
        )
        kvs
    pure (Core sp (CRecordLit fields))
  TyMap valTy -> do
    checkDuplicateKeys kvs
    fields <-
      mapM
        ( \(Spanned _ k, v) -> do
            c <- check ctx path locals v valTy
            pure (k, c)
        )
        kvs
    pure (Core sp (CMapLit fields))
  TyAny -> do
    (c, _) <- infer ctx path locals (Expr sp (EObject kvs))
    pure c
  -- A union with exactly one member an object literal can be checked
  -- against directs the literal at that member; otherwise the literal
  -- is inferred and has to conform to the union (spec 4.3).
  TyUnion members
    | [m] <- filter objectTarget members -> checkObject ctx path locals sp kvs m
    | otherwise -> do
        (c, t) <- infer ctx path locals (Expr sp (EObject kvs))
        conformsM ctx sp t expected >>= \ok -> unless ok (mismatch sp expected t)
        pure c
  other ->
    abort . withExpectedActual (renderType other) "object literal" $
      diag ETypeMismatch sp "an object literal cannot have this expected type"
  where
    renderKeys ks = "{" <> T.intercalate ", " (Set.toList ks) <> "}"
    objectTarget (TyRecord _) = True
    objectTarget (TyMap _) = True
    objectTarget _ = False

-- Accessors -------------------------------------------------------------------------

elabDot :: Ctx -> FilePath -> Locals -> Span -> Expr -> Span -> Text -> TC (Core, Type)
elabDot ctx path locals sp inner fsp fld = case namespaceExpr ctx path locals inner of
  Just key -> do
        -- Namespace member (resolution rank 4, spec 7.2), followed to
        -- its declaration when the module re-exports it.
        let target@(defPath, defName) = namespaceMember (ctxScopes ctx) key fld
        declAsValue ctx sp fld target
        t <- declType ctx target
        recordVar fsp fld t (Just target)
        pure (Core sp (CVar (TopRef defPath defName)), t)
  _ -> do
    (c, t) <- infer ctx path locals inner
    cannotTell ETypeAccess fsp "a field access" t
    case t of
      TyRecord fields -> case Map.lookup fld fields of
        Just f -> do
          -- Recorded so hovering the field shows its own type rather
          -- than a top-level declaration that shares its name.
          recordVar fsp fld (readFieldType f) Nothing
          pure (Core sp (CDot c fld), readFieldType f)
        Nothing ->
          abort . suggesting fld (Map.keys fields) $
            diag ETypeAccess fsp ("record has no field '" <> fld <> "': " <> renderType t)
      other ->
        abort (diag ETypeAccess fsp ("field access requires a Record type, got " <> renderType other))

-- | The module a namespace expression names (spec 5): a namespace in
-- scope, or a namespace that one re-exports, @tools.python@. Name
-- resolution has already rejected a namespace that is not public.
namespaceExpr :: Ctx -> FilePath -> Locals -> Expr -> Maybe FilePath
namespaceExpr ctx path locals (Expr _ ex) = case ex of
  EVar m
    | not (Map.member m locals),
      Nothing <- lookupValueTarget ctx path m ->
        Map.lookup path (ctxScopes ctx) >>= Map.lookup m . gsNamespaces
  EDot inner (Spanned _ n) -> do
    key <- namespaceExpr ctx path locals inner
    Map.lookup key (ctxScopes ctx) >>= Map.lookup n . gsNamespaces
  _ -> Nothing

elabIndex :: Ctx -> FilePath -> Locals -> Span -> Expr -> Expr -> TC (Core, Type)
elabIndex ctx path locals sp inner idx = do
  (c, t) <- infer ctx path locals inner
  cannotTell ETypeAccess sp "an index" t
  case t of
    TyArray elemTy -> do
      i <- index "an Array" TyNumber
      pure (Core sp (CIndex IdxArray c i), elemTy)
    TyMap valTy -> do
      i <- index "a Map" TyString
      pure (Core sp (CIndex IdxMap c i), valTy)
    TyRecord fields -> case literalString idx of
      Just k -> case Map.lookup k fields of
        Just f ->
          pure (Core sp (CDot c k), readFieldType f)
        Nothing ->
          abort . suggesting k (Map.keys fields) $
            diag ETypeAccess (exprSpan idx) ("record has no field '" <> k <> "'")
      Nothing ->
        abort (diag ETypeAccess (exprSpan idx) "record index must be a string literal")
    other ->
      abort (diag ETypeAccess sp ("index access requires Array, Map or Record, got " <> renderType other))
  where
    literalString (Expr _ (EString [TPChunk _ t])) = Just t
    literalString (Expr _ (EString [])) = Just ""
    literalString _ = Nothing

    -- An index of the wrong type is a combination of target and index
    -- type 6.8 does not admit (E-TYPE-ACCESS). A mismatch inside the
    -- index expression stays what it is.
    index what want = do
      r <- tryTC (check ctx path locals idx want)
      case r of
        Right i -> pure i
        Left d
          | diagCode d == ETypeMismatch && diagSpan d == exprSpan idx ->
              abort d {diagCode = ETypeAccess, diagMessage = what <> " is indexed by a " <> renderType want}
          | otherwise -> abort d

-- Binary operators ---------------------------------------------------------------------

elabBin :: Ctx -> FilePath -> Locals -> Span -> Op -> Expr -> Expr -> Maybe Type -> TC (Core, Type)
elabBin ctx path locals sp op a b mExpected = case op of
  OpMul -> arith PMul
  OpDiv -> arith PDiv
  OpAdd -> arith PAdd
  OpSub -> arith PSub
  OpLt -> compare' PLt
  OpLe -> compare' PLe
  OpGt -> compare' PGt
  OpGe -> compare' PGe
  OpEq -> equality PEq
  OpNe -> equality PNe
  OpAnd -> logical CAnd
  OpOr -> logical COr
  -- e |> f  ==  f(e)   /   f <| e  ==  f(e)   (spec 6.2)
  OpPipeR -> pipedInto b (elabCall ctx path locals sp b [Arg (exprSpan a) (APos a)] mExpected)
  OpPipeL -> pipedInto a (elabCall ctx path locals sp a [Arg (exprSpan b) (APos b)] mExpected)
  -- f >> g  ==  \(x) -> g(f(x))   /   f << g  ==  \(x) -> f(g(x))
  OpCompR -> compose a b
  OpCompL -> compose b a
  OpNot -> abort (diag ETypeMismatch sp "internal: '!' is not a binary operator")
  where
    -- `xs |> map(f)` reads as passing xs to map, but the call is made
    -- first and its result is what xs is given to (spec 6.2): say so
    -- where the call comes up short.
    pipedInto fn act = do
      r <- recoverTC act
      case (r, exprF fn) of
        (Right x, _) -> pure x
        (Left (Failed d), ECall {})
          | diagCode d == ETypeArity ->
              abort . withNote "the operand of |> is not added to this call's arguments: the call is made first, and the operand is given to what it returns; write `x |> \\(v) -> f(v, a)`, or `f(x, a)`" $ d
        (Left f, _) -> failWith f
    arith p = do
      ca <- check ctx path locals a TyNumber
      cb <- check ctx path locals b TyNumber
      pure (Core sp (CBin p ca cb), TyNumber)
    -- Both operands have one orderable type (spec 6.2): Number or
    -- String. An operand that needs an expected type to be typed, as
    -- a cast, takes the other's.
    compare' p = do
      ra <- tryTC (infer ctx path locals a)
      rb <- tryTC (infer ctx path locals b)
      (ca, cb, t) <- case (ra, rb) of
        (Right (ca, ta), Right (cb, tb)) -> do
          used <- orderUse ta tb
          unless used $ do
            ab <- conformsM ctx sp ta tb
            ba <- conformsM ctx sp tb ta
            unless (ab && ba) (mismatch sp ta tb)
          pure (ca, cb, ta)
        (Right (ca, ta), Left _) -> (\cb -> (ca, cb, ta)) <$> check ctx path locals b ta
        (Left _, Right (cb, tb)) -> (\ca -> (ca, cb, tb)) <$> check ctx path locals a tb
        (Left d, Left _) -> abort d
      ok <- holdsM ctx BOrderable t
      unless ok $
        abort (diag ETypeBound sp ("values of type " <> renderType t <> " cannot be ordered with <, <=, > or >=: Number and String are orderable, and a type with an instance orderable<...>"))
      vs <- gets stInfer
      let t' = zonkWith vs t
      pure $
        if orderNative ctx t'
          then (Core sp (CBin p ca cb), TyBool)
          else (Core sp (CApp (Core sp (CVar (InstRef builtinClassModule (orderOpName p) [("T", t')]))) [ca, cb] []), TyBool)
    -- A parameter whose type is still to be found, ordered (spec 4.3):
    -- against a value of a type, it is used at that type; against
    -- another such parameter, the two have one type. Either way it is
    -- orderable.
    orderUse ta tb = do
      vs <- gets stInfer
      case (openVar vs ta, openVar vs tb) of
        (Just v, Just w) -> do
          when (v /= w) (refineVar ctx v (TyVar w))
          True <$ addVarBound w (BoundNamed BOrderable)
        (Just v, Nothing) -> True <$ constrain ctx sp (TyVar v) (zonkWith vs tb)
        (Nothing, Just w) -> True <$ constrain ctx sp (TyVar w) (zonkWith vs ta)
        _ -> pure False
    -- One side has to fit where the other is, and the wider of the two
    -- -- the one conformed to -- has to be comparable (spec 6.2). For
    -- two non-union types that is the same as requiring them equal.
    equality p = do
      (ca, ta) <- infer ctx path locals a
      (cb, tb) <- infer ctx path locals b
      used <- equalityUse ta tb
      if used
        then pure (Core sp (CBin p ca cb), TyBool)
        else do
          wider <- do
            ab <- conformsM ctx sp ta tb
            if ab
              then pure tb
              else do
                ba <- conformsM ctx sp tb ta
                if ba then pure ta else mismatch sp ta tb
          ok <- holdsM ctx BComparable wider
          unless ok $
            abort (diag ETypeBound sp ("values of type " <> renderType wider <> " cannot be compared with ==/!=: it is not comparable"))
          pure (Core sp (CBin p ca cb), TyBool)
    -- A parameter whose type is still to be found, compared (spec
    -- 4.3): compared with a value of a type, it is used at that type;
    -- with another such parameter, the two have one type. Either way
    -- it is comparable. Compared with null, it says nothing more.
    equalityUse ta tb = do
      vs <- gets stInfer
      let use v other
            | other == TyNull = True <$ addVarBound v (BoundNamed BComparable)
            | otherwise = do
                _ <- constrain ctx sp (TyVar v) other
                vs' <- gets stInfer
                mapM_ (`addVarBound` BoundNamed BComparable) (openVar vs' (TyVar v))
                pure True
      case (openVar vs ta, openVar vs tb) of
        (Just v, Just w) -> do
          when (v /= w) (refineVar ctx v (TyVar w))
          True <$ addVarBound w (BoundNamed BComparable)
        (Just v, Nothing) -> use v (zonkWith vs tb)
        (Nothing, Just w) -> use w (zonkWith vs ta)
        _ -> pure False
    logical ctor = do
      ca <- check ctx path locals a TyBool
      cb <- check ctx path locals b TyBool
      pure (Core sp (ctor ca cb), TyBool)
    compose f g = do
      (cf, tf) <- infer ctx path locals f
      (cg, tg) <- infer ctx path locals g
      -- The result of the first is passed to the second, so it has to
      -- conform to what the second takes (spec 6.2, 4.4).
      fits <- case (tf, tg) of
        (TyFun [_] tb, TyFun [tb'] _) -> conformsM ctx sp tb tb'
        _ -> pure False
      case (tf, tg) of
        (TyFun [ta] _, TyFun [_] tc) | fits -> do
          let x = "%x"
              bodyCore =
                Core sp (CApp cg [Core sp (CApp cf [Core sp (CVar (LocalRef x))] [])] [])
              lam =
                Lam
                  { lamName = lambdaName sp,
                    lamModule = path,
                    lamPositional = [x],
                    lamVariadic = Nothing,
                    lamKeywords = [],
                    lamBody = bodyCore,
                    lamType = TyFun [ta] tc
                  }
          pure (Core sp (CLam lam), TyFun [ta] tc)
        _ ->
          abort . diag ETypeMismatch sp $
            "function composition requires unary functions with matching types, got "
              <> renderType tf
              <> " and "
              <> renderType tg

-- Blocks -----------------------------------------------------------------------------

-- | Elaborate a do\/control block. The expected type (if any) applies
-- to the last statement. Bindings are visible after their statement.
elabBlock :: Ctx -> FilePath -> Locals -> Block -> Maybe Type -> TC ([CoreStmt], Type)
elabBlock ctx path locals0 (Block bsp stmts0) mExpected = go locals0 stmts0
  where
    go _ []
      | Just t <- mExpected, t /= TyVoid && t /= TyAny =
          abort (diag ETypeMismatch bsp ("an empty block has type Void, expected " <> renderType t))
      | otherwise = pure ([], TyVoid)
    go locals [Stmt ssp f] = case f of
      SBind n sec ann e -> do
        (c, t) <- elabBind locals ssp n sec ann e (inferOrCheck locals e)
        -- The value of the block is the value of its last statement
        -- (6.5), so an annotation there has to fit what the block owes
        -- its context; without one, 'inferOrCheck' has already checked.
        case mExpected of
          Just u | not (conforms ctx t u) -> mismatch ssp u t
          _ -> pure ([CSBind n c], t)
      SExpr e -> do
        (c, t) <- inferOrCheck locals e
        pure ([CSExpr c], t)
    go locals (Stmt ssp f : rest) = case f of
      SBind n sec ann e -> do
        (c, t) <- elabBind locals ssp n sec ann e (infer ctx path locals e)
        (cs, ty) <- go (Map.insert n t locals) rest
        -- A later binding of the same name, or a parameter, counts as
        -- a reference: the check never reports a handle that is used.
        when (holdsHandles t && not (any (mentions n) cs)) $
          advise WAsyncUnused ssp $
            "'" <> n <> "' holds " <> handlesOf t <> " that nothing refers to, so " <> neverAwaited t
        pure (CSBind n c : cs, ty)
      SExpr e -> do
        (c, t) <- case e of
          -- Its value is discarded, so a choice between branches is
          -- not held to having a type that can be unioned with Null
          -- (spec 4.3, 6.4).
          Expr isp (EIf cond thenB Nothing) -> elabIfNoElse ctx path locals isp cond thenB Nothing True
          Expr isp (EIf cond thenB (Just elseB)) -> elabIfElse ctx path locals isp cond thenB elseB True
          Expr isp (ECond cond a b) -> case condAsIf isp cond a b of
            Expr _ (EIf c' t' (Just e')) -> elabIfElse ctx path locals isp c' t' e' True
            other -> infer ctx path locals other
          Expr isp (ECase scrut arms) -> elabCase ctx path locals isp scrut arms Nothing True
          Expr isp (ETry body mCatch mFin) -> elabTry ctx path locals isp body mCatch mFin Nothing True
          _ -> infer ctx path locals e
        -- Only the last statement gives the block its value (6.5).
        when (holdsHandles t) $
          advise WAsyncUnused (exprSpan e) $
            "this statement discards " <> handlesOf t <> ", so " <> neverAwaited t
        (cs, ty) <- go locals rest
        pure (CSExpr c : cs, ty)

    -- An annotation is the expected type for the right-hand side and
    -- the declared type of the binding (spec 4.3, 6.5); without one,
    -- the binding adopts the type of its right-hand side.
    elabBind locals ssp n sec ann e unannotated
      -- A lambda bound to a name has its parameters' types inferred
      -- with type parameters of its own (spec 4.3, 6.1).
      | Nothing <- ann,
        ELambda tps ps rt body <- exprF e,
        any needsInference ps = do
          vs <- typeVarsOf ctx path tps
          (ps', extra) <- inferParams (addTypeVars vs ctx) path locals True ps rt body
          let e' = e {exprF = ELambda (tps <> extra) ps' rt body}
          elabBind locals ssp n sec ann e' (infer ctx path locals e')
      | ELambda tps@(_ : _) ps rt body <- exprF e = do
          case ann of
            Just t ->
              () <$ abort (diag ETypeIllformed (stypeSpan t) "a binding of a lambda with type parameters takes no type annotation: the lambda gives its types")
            Nothing -> pure ()
          vs <- typeVarsOf ctx path tps
          -- A local function is not specialized (spec 4.6): a class
          -- bound is a top-level declaration's.
          case [tsp | (TypeParam (Spanned tsp _) _, (_, Just (BoundClass _))) <- zip tps vs] of
            tsp : _ ->
              () <$ abort (diag ETypeIllformed (if tsp == NoSpan then exprSpan e else tsp) "a class bound is written on a type parameter of a top-level declaration, not of a local function; declare the function at the top level")
            [] -> pure ()
          -- A type name means one thing throughout a declaration (4.2).
          mapM_
            ( \(TypeParam (Spanned vsp v) _) ->
                when (v `Set.member` ctxTypeVars ctx) $
                  abort . diag ENameDuplicate vsp $
                    "type parameter '" <> v <> "' is already a type parameter of the declaration it is written in"
            )
            tps
          requiredAsValue (exprSpan e) "a lambda" ps
          (lam, ty, _) <- elabLambda (addTypeVars vs ctx) path locals (exprSpan e) Nothing ps rt body
          -- Generalized: each reference instantiates it (6.1).
          let tag = case spanStart (exprSpan e) of
                Position _ l c -> "@" <> T.pack (show l) <> ":" <> T.pack (show c)
              ren = Map.fromList [(v, TyVar (v <> tag)) | (v, _) <- vs]
              ty' = applySubst ren ty
          modify (\st -> st {stLocalBounds = stLocalBounds st <> Map.fromList [(v <> tag, b) | (v, Just b) <- vs]})
          c' <- applySecrecy ssp n sec ty' lam
          pure (c', ty')
      | otherwise = do
      annTy <- traverse (typeFromS ctx path) ann
      (c, t) <- case annTy of
        Just t -> do
          when (t == TyVoid) $
            abort . diag ETypeIllformed ssp $
              "'" <> n <> "' cannot be annotated Void: a Void value cannot be bound"
          c <- check ctx path locals e t
          pure (c, t)
        Nothing -> unannotated
      c' <- applySecrecy ssp n sec t c
      pure (c', t)

    inferOrCheck locals e = case mExpected of
      Just t -> do
        c <- check ctx path locals e t
        pure (c, t)
      Nothing -> infer ctx path locals e

    -- The value of an @async@ (6.3), or an array of them, as @for@
    -- over an @async@ body produces.
    holdsHandles t = case t of
      TyAsync _ -> True
      TyArray (TyAsync _) -> True
      _ -> False
    handlesOf t = case t of
      TyArray _ -> "async handles"
      _ -> "an async handle"
    neverAwaited t = case t of
      TyArray _ -> "they are never awaited"
      _ -> "it is never awaited"
    mentions n stmt = any refersTo (concatMap stmtCores [stmt])
      where
        refersTo c = case coreF c of
          CVar (LocalRef m) | m == n -> True
          _ -> any refersTo (coreChildren c)
        stmtCores (CSBind _ c) = [c]
        stmtCores (CSExpr c) = [c]

-- case (spec 6.4) -----------------------------------------------------------------------

-- | @case@ is a chain of 'CIf': it adds no core function and no
-- evaluation rule of its own (spec 6.4). The scrutinee form binds the
-- scrutinee first, so it is evaluated once however many arms are
-- tested; the condition form takes the arm heads as conditions.
elabCase ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  Maybe Expr ->
  [CaseArm] ->
  Maybe Type ->
  -- | Whether the value is discarded ('joinBranches').
  Bool ->
  TC (Core, Type)
elabCase ctx path locals sp mScrut arms mExpected discarded = do
  locals' <- dispatchedParam
  elabCaseIn ctx path locals' sp mScrut arms mExpected discarded
  where
    -- A parameter whose type is still to be found (spec 4.3), which a
    -- case dispatches on by type, is Any throughout the case: each arm
    -- narrows it as it narrows an Any, so what an arm does with it is
    -- done with the narrowed value and is no use of the parameter.
    dispatchedParam = case mScrut of
      Just (Expr ssp (EVar n))
        | any typeHeaded arms,
          Just t <- Map.lookup n locals -> do
            vs <- gets stInfer
            case openVar vs t of
              Just v -> Map.insert n TyAny locals <$ addDispatch ssp v
              Nothing -> pure locals
      _ -> pure locals
    typeHeaded (CaseArm _ (Just (TypeHeads _)) _) = True
    typeHeaded _ = False

elabCaseIn ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  Maybe Expr ->
  [CaseArm] ->
  Maybe Type ->
  Bool ->
  TC (Core, Type)
elabCaseIn ctx path locals sp mScrut arms mExpected discarded = do
  (matchArms, elseBody) <- splitCaseArms sp arms
  duplicateHeads [h | (ValueHeads hs, _) <- matchArms, h <- hs]
  (mBind, conds, narrows, taken) <- case mScrut of
    Nothing -> do
      cs <- mapM (boolCond . fst) matchArms
      pure (Nothing, cs, map (const plain) matchArms, [])
    Just scrut -> do
      (scrutCore, scrutTy0) <- infer ctx path locals scrut
      scrutTy <- scrutineeUse matchArms scrut scrutTy0
      let name = caseScrutName sp
          scrutVar = Core (exprSpan scrut) (CVar (LocalRef name))
      -- Only an equality head needs the scrutinee to be comparable
      -- (spec 6.2, 6.4); a case that only dispatches on types does not.
      comparableScrut <- holdsM ctx BComparable scrutTy
      when (any (isValueHeads . fst) matchArms && not comparableScrut) $
        abort . diag ETypeBound (exprSpan scrut) $
          "values of type " <> renderType scrutTy <> " cannot be matched by case: it is not comparable"
      parts <- mapM (armPart scrutTy name scrutVar) matchArms
      duplicateTypeHeads [h | (_, _, _, hs) <- parts, h <- hs]
      pure
        ( Just (name, scrutCore),
          [c | (c, _, _, _) <- parts],
          [n | (_, n, _, _) <- parts],
          [m | (_, _, ms, _) <- parts, m <- ms]
        )
  let elseNarrow = case (mScrut, taken) of
        (Just (Expr _ (EVar n)), _ : _)
          | Just declared <- Map.lookup n locals -> narrowBind n declared (subtractAll declared taken)
        _ -> plain
  (bodies, ty) <-
    elabBodies (zip narrows (map snd matchArms) <> [(elseNarrow, elseBody)])
  (matchBodies, elseCore) <- case reverse bodies of
    (lastCore : revInit) -> pure (reverse revInit, lastCore)
    [] -> abort (caseElse sp "a case expression requires an else arm")
  let chain = foldr branch elseCore (zip conds matchBodies)
      branch (cond, body) rest = Core (coreSpan cond <> coreSpan body) (CIf cond body rest)
  pure $ case mBind of
    Nothing -> (chain, ty)
    Just (name, scrutCore) -> (Core sp (CDo [CSBind name scrutCore, CSExpr chain]), ty)
  where
    isValueHeads (ValueHeads _) = True
    isValueHeads _ = False

    -- A scrutinee whose type is still to be found (spec 4.3): a case
    -- with type heads dispatches on an Any, and one with value heads
    -- uses it at the type of its heads.
    scrutineeUse matchArms scrut t = do
      vs <- gets stInfer
      case openVar vs t of
        Nothing -> pure t
        Just v
          | any (not . isValueHeads . fst) matchArms -> TyAny <$ addDispatch (exprSpan scrut) v
          | otherwise -> do
              tys <- mapM (headType t) [h | (ValueHeads hs, _) <- matchArms, h <- hs]
              case [u | Just u <- tys] of
                u : us -> do
                  let ht = mkUnion u us
                  addUse (exprSpan scrut) v ht
                  pure ht
                [] -> pure t

    -- How an arm body is elaborated: under which locals, and wrapped
    -- in which rebinding of the narrowed scrutinee (spec 6.4).
    plain :: (Locals, Core -> Core)
    plain = (locals, id)

    -- The condition form (spec 6.4): every head is a Bool, and a type
    -- head has no scrutinee to dispatch on.
    boolCond (TypeHeads (t : _)) =
      abort . diag ETypeMismatch (stypeSpan t) $
        "a type can only be the head of a case that has a scrutinee"
    boolCond (TypeHeads []) = pure (Core sp (CBool False))
    boolCond (ValueHeads hs) =
      anyOf <$> mapM (\h -> check ctx path locals h TyBool) hs

    -- One matching arm of the scrutinee form: its condition, how its
    -- body is elaborated, and the members it takes out of the union.
    armPart scrutTy name scrutVar (heads, _) = case heads of
      ValueHeads hs -> do
        tys <- mapM (headType scrutTy) hs
        conds <- mapM (equalsHead scrutTy name) hs
        let members = [t | Just t <- tys, t `elem` unionMembers scrutTy]
            -- Only Null is exhausted by matching one of its values.
            takenHere = filter (== TyNull) members
            -- Where each type taken here is written, for
            -- 'duplicateTypeHeads'.
            written = [(t, exprSpan h) | (h, Just t) <- zip hs tys, t == TyNull, t `elem` unionMembers scrutTy]
        pure (anyOf conds, narrowTo scrutTy members, takenHere, written)
      TypeHeads sts -> do
        ms <- mapM (typeHead scrutTy) sts
        let conds = [Core (stypeSpan st) (CIsType scrutVar m) | (st, m) <- zip sts ms]
        pure (anyOf conds, narrowTo scrutTy ms, ms, zip ms (map stypeSpan sts))

    -- A type head is admissible for the two types whose runtime kind
    -- is not settled statically (spec 6.4).
    typeHead scrutTy st = do
      m <- typeFromS ctx path st
      case scrutTy of
        TyUnion members
          | m `elem` members -> pure m
          | otherwise ->
              abort . withExpectedActual (renderType scrutTy) (renderType m) $
                diag ETypeMismatch (stypeSpan st) $
                  renderType m <> " is not a member of " <> renderType scrutTy
        TyAny
          | dataType m && isGround m -> pure m
          | otherwise ->
              abort . diag ETypeIllformed (stypeSpan st) $
                "a case type head must be a data type, got " <> renderType m
        other ->
          abort . diag ETypeMismatch (stypeSpan st) $
            "dispatching on a type requires a union or Any scrutinee, got "
              <> renderType other

    -- The type a value head stands for, where it is knowable. The head
    -- is elaborated again by 'equalsHead'; this pass commits nothing.
    headType _ h = do
      r <- tryTC (infer ctx path locals h)
      pure $ case r of
        Right (_, t) | isGround t -> Just t
        _ -> Nothing

    equalsHead scrutTy name h = do
      hc <- check ctx path locals h scrutTy
      let hsp = exprSpan h
      pure (Core hsp (CBin PEq (Core hsp (CVar (LocalRef name))) hc))

    -- Positive narrowing: inside the arm, the scrutinee has the type
    -- its heads selected (spec 6.4). Only a plain local name narrows.
    narrowTo scrutTy ms = case (mScrut, ms) of
      (Just (Expr _ (EVar n)), m : rest)
        | Map.lookup n locals == Just scrutTy -> narrowBind n scrutTy (mkUnion m rest)
      _ -> plain

    -- Rebind the name at the narrowed type for the arm body. A single
    -- narrowed type goes through the conversion of cast (15.8), so a
    -- record narrowed to Map<T> reaches the body as a map. A type
    -- parameter is opaque in the body (4.4) and has no run-time check,
    -- so its positions are converted as Any is: passed as they are.
    narrowBind n declared narrowed
      | narrowed == declared = plain
      | otherwise = (Map.insert n narrowed locals, wrap)
      where
        wrap body = Core (coreSpan body) (CDo [CSBind n rhs, CSExpr body])
        scrutVar = Core sp (CVar (LocalRef (caseScrutName sp)))
        rhs = case narrowed of
          TyUnion _ -> scrutVar
          _ -> case applySubst (Map.fromList [(v, TyAny) | v <- typeVars narrowed]) narrowed of
            TyAny -> scrutVar
            target -> Core sp (CCast scrutVar target)

    -- Subtractive narrowing for the else arm (spec 6.4). Removing
    -- every member leaves the declared type: there is no empty type.
    subtractAll declared takenTys = case filter (`notElem` takenTys) (unionMembers declared) of
      (m : rest) | isUnion declared -> mkUnion m rest
      _ -> declared
      where
        isUnion (TyUnion _) = True
        isUnion _ = False

    -- An arm with several heads matches any of them (spec 6.4).
    anyOf [] = Core sp (CBool False)
    anyOf (c : cs) = foldl (\acc x -> Core (coreSpan acc <> coreSpan x) (COr acc x)) c cs

    -- All arm bodies share one type, or join it with Null (spec 4.3,
    -- 6.4). Without an expected type, 'joinBranches' finds it, so a
    -- context-typed call such as @fail(e)@ or @cast@ may sit in any
    -- arm (15.7).
    elabBodies bodies = case mExpected of
      Just t | t /= TyAny -> do
        cs <- mapM (`checkArm` t) bodies
        pure (cs, t)
      _ -> joinBranches ctx discarded sp (map armBranch bodies)

    checkArm ((lcls, wrap), b) t = wrap <$> check ctx path lcls b t

    armBranch arm@((lcls, wrap), b) =
      Branch
        { branchInfer = (\(c, t) -> (wrap c, t)) <$> infer ctx path lcls b,
          branchCheck = checkArm arm
        }

    -- Two type heads denoting the same type make the later arm
    -- unreachable (spec 6.4); it is reported at the later head, as a
    -- duplicate value head is.
    duplicateTypeHeads = goTy Set.empty
      where
        goTy _ [] = pure ()
        goTy seen ((m, at) : rest)
          | Set.member m seen =
              abort . diag ETypeCaseDuplicate at $
                "this case arm can never be selected: an earlier arm already matches "
                  <> renderType m
          | otherwise = goTy (Set.insert m seen) rest

    -- Two literal heads of the same value make the later arm
    -- unreachable (spec 6.4). Heads that are not literals are not
    -- compared with one another.
    duplicateHeads = go Set.empty
      where
        go _ [] = pure ()
        go seen (h : rest) = case literalKey h of
          Just k
            | Set.member k seen ->
                abort . diag ETypeCaseDuplicate (exprSpan h) $
                  "this case arm can never be selected: an earlier arm already matches " <> k
            | otherwise -> go (Set.insert k seen) rest
          Nothing -> go seen rest

-- | A key identifying a literal arm head, or 'Nothing' for a head
-- whose value is not known statically.
literalKey :: Expr -> Maybe Text
literalKey (Expr _ f) = case f of
  ENull -> Just "null"
  EBool b -> Just (if b then "true" else "false")
  ENumber n -> Just (T.pack (show n))
  EString [] -> Just "\"\""
  EString [TPChunk _ t] -> Just (T.pack (show t))
  _ -> Nothing

-- | The name an elaborated @case@ binds its scrutinee to (spec 6.4).
-- Angle brackets cannot occur in a @lower_id@, so it shadows nothing
-- and no expression written in the source can refer to it.
caseScrutName :: Span -> Text
caseScrutName (Span (Position _ l c) _) =
  "<case@" <> T.pack (show l) <> ":" <> T.pack (show c) <> ">"
caseScrutName NoSpan = "<case>"

-- | Split the arms into the matching arms and the body of the @else@
-- arm, which must be present exactly once and last (spec 6.4).
splitCaseArms :: Span -> [CaseArm] -> TC ([(CaseHeads, Expr)], Expr)
splitCaseArms sp arms = case reverse arms of
  [] -> abort (caseElse sp "a case expression requires an else arm")
  (CaseArm lsp lastHeads lastBody : revInit) -> do
    let initArms = reverse revInit
    case [asp | CaseArm asp Nothing _ <- initArms] of
      (asp : _) -> abort (caseElse asp "the else arm must be the last arm of a case expression")
      [] -> pure ()
    case lastHeads of
      Just _ -> abort (caseElse lsp "a case expression requires an else arm")
      Nothing -> pure ([(hs, b) | CaseArm _ (Just hs) b <- initArms], lastBody)

caseElse :: Span -> Text -> Diagnostic
caseElse = mkDiagnostic ESyntaxCaseElse StageSyntax

-- try/catch/finally (spec 6.9) ----------------------------------------------------------

elabTry ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  Block ->
  Maybe (Spanned Text, Block) ->
  Maybe Block ->
  Maybe Type ->
  -- | Whether the value is discarded ('joinBranches').
  Bool ->
  TC (Core, Type)
elabTry ctx path locals sp body mCatch mFin mExpected discarded = do
  (caught, caughtTy) <- case mCatch of
    Just (Spanned _ ename, handler) -> do
      let handlerLocals = Map.insert ename errorType locals
      (bodyCore, handlerCore, ty) <- case mExpected of
        -- An expected type from context applies to both blocks.
        Just t -> do
          (bodyStmts, _) <- elabBlock ctx path locals body (Just t)
          (hStmts, _) <- elabBlock ctx path handlerLocals handler (Just t)
          pure (coreOf body bodyStmts, coreOf handler hStmts, t)
        Nothing -> do
          -- The body and the handler share a type, or join it with
          -- Null (spec 4.3, 6.9); context-typed calls like fail may
          -- appear in either (15.7).
          (stmtss, ty) <-
            joinBranches ctx discarded sp [blockBranch ctx path locals body, blockBranch ctx path handlerLocals handler]
          case stmtss of
            [bodyStmts, hStmts] -> pure (coreOf body bodyStmts, coreOf handler hStmts, ty)
            _ -> abort (diag ETypeMismatch sp "a try with catch has two branches")
      pure (recoverCall bodyCore (Just ename) handlerCore ty, ty)
    Nothing -> do
      (bodyStmts, bodyTy) <- elabBlock ctx path locals body mExpected
      pure (coreOf body bodyStmts, bodyTy)
  case mFin of
    Nothing -> pure (caught, caughtTy)
    Just fin -> do
      (finStmts, finTy) <- elabBlock ctx path locals fin Nothing
      let finCore = Core (blockSpan fin) (CDo finStmts)
          -- try X finally F is %ensure(\() -> X, \() -> F), an internal
          -- builtin rather than an expansion through recover: F also
          -- runs when X is abandoned, which is not a failure recover
          -- could catch (spec 6.9, 8.10).
          whole =
            Core sp $
              CApp
                (builtin "%ensure")
                [mkThunk "try" path sp caught caughtTy, mkThunk "finally" path (blockSpan fin) finCore finTy]
                []
      pure (whole, caughtTy)
  where
    builtin n = Core sp (CVar (BuiltinRef n))
    coreOf blk stmts = Core (blockSpan blk) (CDo stmts)
    recoverCall bodyCore ename handlerCore ty = recoverCallWith bodyCore ename handlerCore ty
    recoverCallWith bodyCore ename handlerCore ty =
      let thunk = mkThunk "try" path sp bodyCore ty
          handlerLam =
            Core sp . CLam $
              Lam
                { lamName = lambdaName sp,
                  lamModule = path,
                  lamPositional = [maybe "%e" id ename],
                  lamVariadic = Nothing,
                  lamKeywords = [],
                  lamBody = handlerCore,
                  lamType = TyFun [errorType] ty
                }
       in Core sp (CApp (builtin "recover") [thunk, handlerLam] [])

-- Commands (spec 6.6) ---------------------------------------------------------------------

elabCommand ::
  Ctx ->
  FilePath ->
  Locals ->
  Span ->
  CmdStream ->
  Maybe Expr ->
  [TextPart] ->
  TC (Core, Type)
elabCommand ctx path locals sp stream mEnv parts = do
  (cmdCore, _) <- first escapeCommand <$> elabString ctx path locals sp parts
  -- The shell syntax is checked before dispatch (6.6), whose own
  -- reading cannot segment a string with an open region.
  let reading = readShell parts
  checkShellSyntax sp reading
  (envCore, shownEnv, viaWords) <- case mEnv of
    Just envExpr -> do
      (c, t) <- infer ctx path locals envExpr
      envOk <- conformsM ctx (exprSpan envExpr) t TyRunnable
      unless envOk $
        abort . withExpectedActual "Runnable" (renderType t) $
          diag ETypeCommandEnv (exprSpan envExpr) "command environment must be an Environment or a Runnable"
      pure (c, Nothing, [])
    Nothing -> do
      (c, ws) <- dispatchEnv ctx path sp parts
      tbl <- commandTable ctx path
      case ws of
        Spanned _ w : _
          | Just entry <- Map.lookup w tbl -> checkSchemas ctx path sp reading (ceSource entry)
        _ -> pure ()
      recordDispatched [(path, w) | Spanned _ w <- ws]
      pure (c, Just (renderEnvCore c), ws)
  recordCommandUse sp shownEnv viaWords parts
  let call = Core sp (CApp (Core sp (CVar (BuiltinRef "run"))) [envCore, cmdCore] [])
  case stream of
    StreamAll -> pure (call, commandResultType)
    StreamOut -> pure (streamSelect call "stdout", TyString)
    StreamErr -> pure (streamSelect call "stderr", TyString)
  where
    -- do { r = run(...);
    --      if (r.code == 0) { r.<stream> } else { fail({code: r.code, message: r.stderr}) } }
    streamSelect call field =
      let r = "%r"
          rv = Core sp (CVar (LocalRef r))
          cond = Core sp (CBin PEq (Core sp (CDot rv "code")) (Core sp (CNumber 0)))
          okBranch = Core sp (CDot rv field)
          errRecord =
            Core sp $
              CRecordLit
                [ ("code", Core sp (CDot rv "code")),
                  ("message", Core sp (CDot rv "stderr"))
                ]
          -- %commandFail is an internal builtin: like fail, but
          -- tagged E-RUNTIME-COMMAND-NONZERO for diagnostics (14.5).
          failCall = Core sp (CApp (Core sp (CVar (BuiltinRef "%commandFail"))) [errRecord] [])
       in Core sp (CDo [CSBind r call, CSExpr (Core sp (CIf cond okBranch failCall))])

-- | A command string must be well formed as POSIX shell syntax, as far
-- as its reading goes (6.6).
checkShellSyntax :: Span -> ShellReading -> TC ()
checkShellSyntax sp reading = case srProblem reading of
  Nothing -> pure ()
  Just (psp, why) ->
    abort . diag ETypeCommandSyntax (if psp == NoSpan then sp else psp) $
      "this command is not valid shell syntax: " <> why

-- Command declarations and dispatch (spec ch. 5, 10.9) -----------------------

-- | Where a module's command words come from: its own declarations,
-- with their positions, and its imports and re-exports of command
-- words, with the module they name. In source order.
data CommandSite
  = SiteDecl Int [Spanned Text] Expr (Maybe Expr)
  | SiteImport [Spanned Text] FilePath

moduleCommandSites :: Ctx -> FilePath -> [CommandSite]
moduleCommandSites ctx path = case Map.lookup path (progModules (ctxProg ctx)) of
  Nothing -> []
  Just lm ->
    [ site
    | (i, Decl _ f) <- zip [0 ..] (moduleDecls (lmModule lm)),
      site <- case f of
        DCommand ns e sch -> [SiteDecl i ns e sch]
        DImportCommands ns p -> [SiteImport ns (importKey lm p)]
        DExportCommandsFrom ns p -> [SiteImport ns (importKey lm p)]
        _ -> []
    ]
  where
    importKey lm p = Map.findWithDefault (T.unpack p) p (lmImportKeys lm)

-- | The environment of a command declaration (spec ch. 5): any
-- expression of type Environment, provided it can reach no effect.
-- The restriction is what lets @lask cmd@ evaluate it without running
-- anything, and what keeps it meaning the same wherever it is used.
declaredEnv :: Ctx -> FilePath -> Expr -> TC Core
declaredEnv ctx path e = do
  (c, t) <- infer ctx path Map.empty e
  unless (conforms ctx t TyRunnable) $
    abort . withExpectedActual "Runnable" (renderType t) $
      diag ETypeCommandEnv (exprSpan e) "the environment of a command declaration must be an Environment or a Runnable"
  effect <- reachableEffect ctx c
  case effect of
    Nothing -> pure c
    Just (via, n) ->
      abort . diag ETypeCommandEffect (exprSpan e) $
        "the environment of a command declaration must not have effects, but "
          <> maybe "it" (\v -> "'" <> v <> "'") via
          <> " can reach '"
          <> n
          <> "'; compute the environment from values alone, or give it at the command with $[...]"

-- | The first effectful builtin (15.1, 9.3) an expression can reach,
-- directly or through the top-level declarations it references, with
-- the first of those declarations on the way. Reachability
-- over-approximates as enumeration does (11.4): a reference counts
-- whether or not it is ever called.
reachableEffect :: Ctx -> Core -> TC (Maybe (Maybe Text, Text))
reachableEffect ctx root = go Set.empty [(Nothing, root)]
  where
    go _ [] = pure Nothing
    go seen ((via, c) : rest) = case coreF c of
      CVar (BuiltinRef n)
        | n `Set.member` effectfulBuiltinNames -> pure (Just (via, n))
      CVar (TopRef p n)
        | not (Set.member (p, n) seen) -> do
            cd <- demandDecl ctx (p, n)
            go (Set.insert (p, n) seen) ((via <|> Just n, cdCore cd) : rest)
      _ -> go seen ([(via, x) | x <- coreChildren c] <> rest)

-- | What selection compares for a declaration's environment (10.9):
-- a literal environment expression, reached directly or through
-- bindings, by its value; otherwise the last binding on the way; and
-- failing that, the declaration itself.
envSource :: Ctx -> FilePath -> Int -> Core -> TC EnvSource
envSource ctx path idx = walk Set.empty Nothing
  where
    walk seen lastBinding c = case coreF c of
      CEnv {}
        | literalEnv c -> pure (SrcLiteral (envKey c))
      CRunnable {}
        | literalEnv c -> pure (SrcLiteral (envKey c))
      CVar (TopRef p n)
        | not (Set.member (p, n) seen) -> do
            cd <- demandDecl ctx (p, n)
            walk (Set.insert (p, n) seen) (Just (p, n)) (cdCore cd)
      _ -> pure (maybe (SrcDecl path idx) SrcBinding lastBinding)

    literalEnv c = case coreF c of
      CEnv _ args -> all (literal . snd) args
      CRunnable base args -> literalEnv base && all (literal . snd) args
      _ -> False
    literal c = case coreF c of
      CStrLit _ -> True
      CNumber _ -> True
      CBool _ -> True
      CNull -> True
      CArray es -> all literal es
      CMapLit kvs -> all (literal . snd) kvs
      _ -> False

-- | A canonical rendering of a literal environment, for the structural
-- equality selection compares (spec 10.9). Spans are not part of it,
-- so two declarations naming the same environment agree.
envKey :: Core -> Text
envKey c = case coreF c of
  -- Arguments and table entries are keyed in name order, not in the
  -- order they were written: two declarations that pass the same
  -- options in a different order denote the same environment, and
  -- selection compares environment values (10.9).
  CEnv kind args -> kind <> "(" <> T.intercalate "," [k <> "=" <> envKey v | (k, v) <- sortOn fst args] <> ")"
  -- A runnable runs as its environment with the options merged in,
  -- so it is keyed as one argument list: #a{m: 1} and the same
  -- environment written with that option compare equal (spec 8.8).
  CRunnable base args -> case coreF base of
    CEnv kind eargs -> envKey (base {coreF = CEnv kind (eargs <> args)})
    _ -> T.pack (show (coreF base))
  CStrLit t -> "\"" <> t <> "\""
  CNumber n -> T.pack (show n)
  CBool b -> if b then "true" else "false"
  CNull -> "null"
  CArray es -> "[" <> T.intercalate "," (map envKey es) <> "]"
  CMapLit kvs -> "{" <> T.intercalate "," [k <> ":" <> envKey v | (k, v) <- sortOn fst kvs] <> "}"
  other -> T.pack (show other)

-- | The command schemas of a declaration (spec appendix A): a
-- @Map\<CommandSchema\>@ keyed by words of the declaration, known
-- without evaluation.
declaredSchemas :: Ctx -> FilePath -> [Spanned Text] -> Expr -> TC (Map Text Schema)
declaredSchemas ctx path names e = do
  c <- check ctx path Map.empty e (TyMap commandSchemaType)
  Static sp v <- staticValue ctx c
  entries <- case v of
    VObject kvs -> pure kvs
    _ -> abort (diag ETypeCommandSchema sp "the schemas of a command declaration must be a map keyed by its command words")
  let declared = [n | Spanned _ n <- names]
      -- A map written here names words of this declaration; one a
      -- top-level value holds may give other words too (spec ch. 5).
      written = case coreF c of
        CMapLit _ -> True
        _ -> False
  Map.fromList
    <$> mapM
      ( \(k, s@(Static ssp _)) -> do
          unless (k `elem` declared || not written) $
            abort . suggesting k declared $
              diag ETypeCommandSchema (orSpan ssp) ("'" <> k <> "' is not a command word of this declaration")
          case fromStatic s of
            Left (esp, why) ->
              abort (diag ETypeCommandSchema (orSpan (if esp == NoSpan then ssp else esp)) ("invalid command schema for '" <> k <> "': " <> why))
            Right schema -> pure (k, schema)
      )
      [entry | entry@(k, _) <- entries, k `elem` declared || written]
  where
    orSpan s = if s == NoSpan then exprSpan e else s

-- | A value written with literals and references to top-level values
-- alone (spec ch. 5), read without evaluating anything.
staticValue :: Ctx -> Core -> TC Static
staticValue ctx = go Set.empty
  where
    go seen c =
      let sp = coreSpan c
          here = pure . Static sp
       in case coreF c of
            CNull -> here VNull
            CBool b -> here (VBool b)
            CNumber n -> here (VNumber (realToFrac n))
            CStrLit t -> here (VString t)
            CStr ps | Just ts <- traverse textPart ps -> here (VString (T.concat ts))
            CArray es -> Static sp . VArray <$> mapM (go seen) es
            CMapLit kvs -> Static sp . VObject <$> mapM (traverse (go seen)) kvs
            CRecordLit kvs -> Static sp . VObject <$> mapM (traverse (go seen)) kvs
            CVar (TopRef p n)
              | not (Set.member (p, n) seen) -> do
                  cd <- demandDecl ctx (p, n)
                  go (Set.insert (p, n) seen) (cdCore cd)
            _ ->
              abort . diag ETypeCommandSchema sp $
                "a command schema must be written with literals and references to top-level values, and this part is computed"
    textPart (CPText t) = Just t
    textPart (CPExpr _) = Nothing

-- | Record the command words a command string of the declaration
-- being elaborated dispatches through, for the check before a run
-- (spec ch. 5).
recordDispatched :: [(FilePath, Text)] -> TC ()
recordDispatched [] = pure ()
recordDispatched ws = do
  current <- gets stCurrent
  case current of
    key : _ ->
      modify $ \s ->
        s {stDispatched = Map.insertWith Set.union key (Set.fromList ws) (stDispatched s)}
    [] -> pure ()

-- | Check the simple commands of a dispatched command string against
-- the schemas of the declarations that selected its environment (spec
-- ch. 5). A command word declared elsewhere, or not plainly written,
-- is not checked.
checkSchemas :: Ctx -> FilePath -> Span -> ShellReading -> EnvSource -> TC ()
checkSchemas ctx path sp reading src = do
  tbl <- commandTable ctx path
  mapM_ (one tbl) (srCommands reading)
  where
    one tbl cmd =
      let n = scName cmd
          word = swPrefix n
       in case Map.lookup word tbl of
            Just entry
              | swLiteral n,
                swPlain n,
                not (T.any (== '/') word),
                ceSource entry == src,
                Just schema <- ceSchema entry -> do
                  unlessQuiet $ modify (\s -> s {stSchemaUses = SchemaUse word schema cmd : stSchemaUses s})
                  case checkCommand schema word cmd of
                    f : _
                      | fdSure f -> abort (diag ETypeCommandOption (at (fdSpan f)) (fdMessage f))
                      | otherwise -> advise WCommandOption (at (fdSpan f)) (fdMessage f)
                    [] -> pure ()
            _ -> pure ()
    at s = if s == NoSpan then sp else s

-- | The module's command words and the environments they name, built
-- once per module and then cached.
commandTable :: Ctx -> FilePath -> TC (Map Text CommandEntry)
commandTable ctx path = do
  cached <- gets stCommands
  case Map.lookup path cached of
    Just table -> pure table
    Nothing -> do
      modify (\st -> st {stCommandsBuilding = Set.insert path (stCommandsBuilding st)})
      table <- loud (buildCommandTable ctx path)
      modify $ \st ->
        st
          { stCommands = Map.insert path table (stCommands st),
            stCommandsBuilding = Set.delete path (stCommandsBuilding st)
          }
      pure table

-- Each declaration and import is a unit of its own (spec 14.3): one
-- that fails leaves its words out of the table, and marks them failed
-- so that a command string using one reports nothing more.
buildCommandTable :: Ctx -> FilePath -> TC (Map Text CommandEntry)
buildCommandTable ctx path = foldM recoverSite Map.empty (moduleCommandSites ctx path)
  where
    recoverSite tbl site = do
      failed <- gets (fsWords . stFailures)
      let names = siteNames site
      if any (\(Spanned _ n) -> (path, n) `Set.member` failed) names
        then pure tbl
        else do
          r <- recoverTC (addSite tbl site)
          case r of
            Right tbl' -> pure tbl'
            Left f -> do
              recordFailure f
              -- A word the table already has keeps its entry: the
              -- earlier declaration of a duplicate is not the one at
              -- fault.
              let lost = [(path, n) | Spanned _ n <- names, not (Map.member n tbl)]
              modifyFailures (\fs -> fs {fsWords = Set.union (Set.fromList lost) (fsWords fs)})
              pure tbl

    siteNames (SiteDecl _ names _ _) = names
    siteNames (SiteImport names _) = names

    addSite tbl (SiteDecl i names envExpr schemaExpr) = do
      env <- declaredEnv ctx path envExpr
      src <- envSource ctx path i env
      schemas <- maybe (pure Map.empty) (declaredSchemas ctx path names) schemaExpr
      foldM (addDeclared env src i schemas) tbl names
    addSite tbl (SiteImport names key) = do
      target <- commandTable ctx key
      foldM (addImported key target) tbl names

    addDeclared env src i schemas tbl (Spanned sp n) = do
      unless (validCommandName n) $
        abort . diag ETypeCommandName sp $
          "'" <> n <> "' could never be recognized as a command word in a command string"
      when (Map.member n tbl) $ abort (duplicate sp n)
      pure (Map.insert n (CommandEntry sp env src (path, i) (Map.lookup n schemas)) tbl)

    -- The resolver has already checked that the target exports the
    -- word. The same declaration reached along two import paths is
    -- one entry; anything else under the same word is a duplicate.
    addImported key target tbl (Spanned sp n) = case Map.lookup n target of
      Nothing -> do
        -- A word whose declaration failed in the module it comes from
        -- has been reported there.
        failed <- gets (fsWords . stFailures)
        when ((key, n) `Set.member` failed) dependencyFailed
        abort . suggesting n (Map.keys target) $
          diag ENameUndefined sp ("module exports no command '" <> n <> "'")
      Just entry -> case Map.lookup n tbl of
        Just prev
          | ceOrigin prev == ceOrigin entry -> pure tbl
          | otherwise -> abort (duplicate sp n)
        Nothing -> pure (Map.insert n entry tbl)

    duplicate sp n =
      diag ETypeCommandDuplicate sp ("command '" <> n <> "' is declared or imported more than once")

-- | The environment of a command execution expression that carries no
-- environment specification, with the command words that selected it
-- (spec 10.9).
dispatchEnv :: Ctx -> FilePath -> Span -> [TextPart] -> TC (Core, [Spanned Text])
dispatchEnv ctx path sp parts = do
  building <- gets stCommandsBuilding
  when (path `Set.member` building) $
    abort . diag ETypeCommandEffect sp $
      "this command is reached from the environment of a command declaration, "
        <> "which must not run commands"
  tbl <- commandTable ctx path
  failed <- gets (fsWords . stFailures)
  case commandWords parts of
    NotAnalysable _ why ->
      abort . diag ETypeCommandNoEnv sp $
        "the command string could not be segmented (" <> why <> "), so no command word could be read; "
          <> "give the environment explicitly with $[...]"
    -- A word whose declaration failed could have selected the
    -- environment: what else is wrong here follows from that.
    Analysed ws
      | any (\w -> cwCandidate w && (path, cwText w) `Set.member` failed) ws -> dependencyFailed
    Analysed ws -> case [(w, e) | w <- ws, cwCandidate w, Just e <- [Map.lookup (cwText w) tbl]] of
      [] ->
        abort . suggestingAny [cwText w | w <- ws, cwCandidate w] (Map.keys tbl) $
          diag ETypeCommandNoEnv sp (noneMessage ws)
      matched@((w0, e0) : more) -> case [(w, e) | (w, e) <- more, ceSource e /= ceSource e0] of
        [] -> pure (ceEnv e0, [Spanned (cwSpan w) (cwText w) | (w, _) <- matched])
        ((w1, e1) : _) ->
          abort . diag ETypeCommandConflict sp $
            "this command runs both "
              <> describe w0 e0
              <> " and "
              <> describe w1 e1
              <> ", which are not known to be one environment; a command string is one process in one environment, "
              <> "so split the command, declare both words on one environment, or give the environment explicitly with $[...]"
  where
    describe w e = "'" <> cwText w <> "' (" <> sourceText e <> ")"
    sourceText e = case ceSource e of
      SrcLiteral _ -> renderEnvCore (ceEnv e)
      SrcBinding (_, n) -> n
      SrcDecl _ _ -> "declared at " <> renderSpan (ceSpan e)
    renderSpan (Span (Position f l _) _) = T.pack f <> ":" <> T.pack (show l)
    renderSpan NoSpan = "its declaration"

    noneMessage ws =
      let seen = [cwText w | w <- ws, cwCandidate w]
          quoted' ns = T.intercalate ", " ["'" <> n <> "'" | n <- ns]
          named = case seen of
            [] -> "no command word could be read"
            [n] -> "'" <> n <> "' is not a declared command"
            ns -> "none of " <> quoted' ns <> " is a declared command"
       in named
            <> "; declare it with `command { \"<name>\" } on <environment>`, "
            <> "import it with `import command { \"<name>\" } from \"<module>\"`, "
            <> "or give the environment explicitly with $[...]"

-- Environment expressions (spec 6.7, 10.2) ---------------------------------------------------

-- | A recipe path written in the module at @modulePath@, as the path
-- relative to the program's base directory @base@ that names the same
-- file. Module paths and the base directory are both relative to where
-- lask runs, or both absolute; where they are not alike, the
-- module-relative path is kept whole, which the base directory joins to
-- unchanged.
--
-- A recipe in a dependency is written as it would be under the default
-- cache, @.lask/deps/...@, wherever @LASK_CACHE_DIR@ puts the cache
-- (@cacheDir@): the path is part of the recipe's lock key and hash, and
-- a lock must not depend on where the machine that wrote it kept its
-- cache. Reading the file maps it back ('resolveInBase').
recipePath :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
recipePath base cacheDir modulePath written
  | Just canonical <- inCache cacheDir target = canonical
  | isAbsolute base /= isAbsolute target = target
  | otherwise =
      let b = parts base
          t = parts target
          common = length (takeWhile id (zipWith (==) b t))
          rel = replicate (length b - common) ".." <> drop common t
       in if null rel then "." else joinPath rel
  where
    target = collapseDots (normalise (takeDirectory modulePath </> written))
    parts = filter (/= ".") . splitDirectories . collapseDots . normalise

-- | The heads of an environment expression (spec 6.7): the host, a
-- recipe path, or a registry reference. The head is the only place an
-- image is named.
data EnvHead = HeadLocal | HeadRecipe Text | HeadRegistry Text

classifyHead :: Text -> EnvHead
classifyHead h
  | h == "local" = HeadLocal
  | "." `T.isPrefixOf` h = HeadRecipe h
  | otherwise = HeadRegistry h

-- | The run options of the @docker@ profile (spec 10.2, 15.5): how an
-- image runs, never which image it is. They are the keyword parameters
-- of @runnable@, and each takes any expression of its type.
--
-- A container option given null is left out: the one way an argument
-- can say "not given" without giving up a value a caller might mean,
-- such as "" (10.2). A list or a table says it by being empty, and
-- leaves out its null elements and null values the same way.
dockerRunOptions :: [(Text, Type)]
dockerRunOptions =
  [ -- Resource limits.
    ("memory", text),
    ("memory_swap", text),
    ("memory_reservation", text),
    ("cpus", number),
    ("cpu_shares", number),
    ("cpuset_cpus", text),
    ("cpuset_mems", text),
    ("pids_limit", number),
    ("shm_size", text),
    ("blkio_weight", number),
    ("ulimits", list),
    -- Execution context.
    ("workdir", text),
    ("user", text),
    ("env", table),
    ("hostname", text),
    ("init", switch),
    -- Confinement: these narrow the boundary of 10.7.
    ("read_only", switch),
    ("tmpfs", list),
    ("cap_drop", list),
    -- Network.
    ("network", text),
    -- The destinations a container may reach, through a proxy (10.12).
    -- Null as a whole, unlike the other lists: an empty list admits
    -- nothing, so "not given" is null, which a library passes on.
    ("egress", mkUnion list [TyNull]),
    ("dns", list),
    ("dns_search", list),
    ("add_hosts", table),
    ("publish", list),
    -- Host filesystem: where the base directory is mounted and what
    -- the mount permits (10.5), and what is mounted beyond it.
    ("base_mount", text),
    ("base_access", mkUnion TyString [TyMap TyString, TyNull]),
    ("volumes", list)
  ]
  where
    nullable t = mkUnion t [TyNull]
    text = nullable TyString
    number = nullable TyNumber
    switch = nullable TyBool
    list = TyArray (nullable TyString)
    table = TyMap (nullable TyString)

-- | The options that decide which image is used (spec 6.7, 10.2). They
-- are given only in a head's own argument list, as literals, so that
-- the images a program uses are known by reading it.
imageOptionNames :: [Text]
imageOptionNames = ["platform", "context", "build_args", "build_network"]

-- | The type of a runnable's options, for a diagnostic that points at
-- the braces.
runOptionNames :: [Text]
runOptionNames = map fst dockerRunOptions

elabEnv :: Ctx -> FilePath -> Locals -> Span -> Text -> Maybe [Arg] -> TC (Core, Type)
elabEnv ctx path locals sp h mArgs = case classifyHead h of
  HeadLocal -> case mArgs of
    Nothing -> pure (Core sp (CEnv "local" []), TyEnvironment)
    Just _ -> envErr "#local takes no argument list"
  HeadRegistry ref -> do
    when (h == "docker" && any positional (maybe [] id mArgs)) $
      () <$ envErr removedDocker
    opts <- options [("platform", TyString)]
    _ <- requireLiteral opts "platform"
    pure (Core sp (CEnv "docker" (("image", Core sp (CStrLit ref)) : opts)), TyEnvironment)
  HeadRecipe written -> do
    dockerfile <- treePath "the recipe path" written
    opts <- options [("platform", TyString), ("context", TyString), ("build_args", TyMap TyString), ("build_network", TyString)]
    _ <- requireLiteral opts "platform"
    mapM_ (treePath "'context'") =<< requireLiteral opts "context"
    requireLiteralTable opts "build_args"
    -- What the build reaches (10.3): "none", or the engine's default.
    network <- requireLiteral opts "build_network"
    case network of
      Just n | n `notElem` ["none", "default"] -> envErr ("'build_network' takes \"none\" or \"default\", not '" <> n <> "'")
      _ -> pure ()
    pure
      ( Core sp (CEnv "docker" (map recipeArg (("dockerfile", Core sp (CStrLit dockerfile)) : opts))),
        TyEnvironment
      )
  where
    envErr :: Text -> TC a
    envErr = abort . diag ETypeEnvConstruct sp

    positional (Arg _ af) = case af of
      APos _ -> True
      AKw _ _ -> False

    options imageOpts = bindEnvOptions ctx path locals imageOpts unknown (maybe [] id mArgs)

    unknown n
      | h == "docker" || n `elem` ["image", "dockerfile"] = removedDocker
      | n `elem` imageOptionNames = "'" <> n <> "' is only valid on a recipe head, #./path/Dockerfile(" <> n <> " = ...)"
      | n `elem` runOptionNames =
          "'" <> n <> "' is a run option: give it in braces, as #" <> h <> "{" <> n <> ": ...}, or to runnable(...)"
      | otherwise = "unknown image option: '" <> n <> "'"

    -- A recipe path is written relative to the directory of the module
    -- that declares it (10.2), and is read — by the runtime, by
    -- `lask sync`, in the lock — relative to the program's base
    -- directory, since the value it ends up in no longer knows its
    -- module. It is rewritten here, where the module is known. A recipe
    -- beside the entry module keeps the path it was written with.
    recipeArg (k, c)
      | k `elem` ["dockerfile", "context"],
        CStrLit p <- coreF c =
          (k, c {coreF = CStrLit (T.pack (recipePath (progBaseDir (ctxProg ctx)) (progCacheDir (ctxProg ctx)) path (T.unpack p)))})
      | otherwise = (k, c)

    -- An image option is read before anything runs, by `lask env
    -- build`, which has no evaluator to compute it with (10.3).
    requireLiteral :: [(Text, Core)] -> Text -> TC (Maybe Text)
    requireLiteral named key = case lookup key named of
      Nothing -> pure Nothing
      Just (Core _ (CStrLit t))
        | T.null t -> envErr ("'" <> key <> "' must not be empty")
        | otherwise -> pure (Just t)
      Just _ -> envErr ("'" <> key <> "' must be a string literal without interpolation")

    requireLiteralTable :: [(Text, Core)] -> Text -> TC ()
    requireLiteralTable named key = case lookup key named of
      Nothing -> pure ()
      Just (Core _ (CMapLit kvs))
        | all (isLiteralString . snd) kvs -> pure ()
      Just _ -> envErr ("'" <> key <> "' must be a table of string literals without interpolation")
      where
        isLiteralString c = case coreF c of
          CStrLit _ -> True
          _ -> False

    -- A recipe resolves inside the tree of the module it is written in
    -- (10.2), so the module's content hash covers it.
    treePath :: Text -> Text -> TC Text
    treePath what p
      | isAbsolute (T.unpack p) = envErr (what <> " must be a relative path inside the module tree")
      | ".." `elem` splitDirectories (normalise (T.unpack p)) = envErr (what <> " must not escape the module tree")
      | otherwise = pure (T.pack (normalise (T.unpack p)))

-- | A runnable (spec 6.7, 15.5): an environment with run options,
-- from @runnable(env, k = v, ...)@ or its sugar @#head{k: v, ...}@. It
-- takes an 'Environment' only, so options are given once and never
-- applied over a runnable's.
elabRunnable :: Ctx -> FilePath -> Locals -> Span -> Core -> [Arg] -> TC (Core, Type)
elabRunnable ctx path locals sp env args = do
  opts <- bindEnvOptions ctx path locals dockerRunOptions unknown args
  case coreF env of
    CEnv "local" _
      | not (null opts) -> abort (diag ETypeEnvConstruct sp "#local takes no run options")
    _ -> pure ()
  checkBaseOptions opts
  pure (Core sp (CRunnable env opts), TyRunnable)
  where
    -- What the mount of the base directory is given as a literal is
    -- checked here (10.2); anything else, when the command runs.
    checkBaseOptions opts = do
      mount <- case lookup "base_mount" opts of
        Just c@(Core _ (CStrLit t)) -> Just c <$ either (optionErr c) pure (validateBaseMount t)
        Just c | not (isNullLit c) -> pure (Just c)
        _ -> pure Nothing
      case lookup "base_access" opts of
        Just c@(Core _ (CStrLit t)) -> do
          m <- either (optionErr c) pure (parseMode t)
          case mount of
            Just mc
              | m == ModeNone ->
                  optionErr mc "'base_mount' is given with base_access \"none\", which mounts nothing"
            _ -> pure ()
        Just c@(Core _ (CMapLit kvs))
          | Just pairs <- mapM literalValue kvs -> () <$ either (optionErr c) pure (accessTable pairs)
        _ -> pure ()
      -- An entry of egress written as a literal, and egress with
      -- network or publish, both written as literals (10.12). A
      -- library passes its own parameters on, null when not given, and
      -- is held to it when the command runs.
      case lookup "egress" opts of
        Just (Core _ (CArray es)) -> do
          mapM_ (\e -> case coreF e of CStrLit t -> () <$ either (optionErr e) pure (parseEgressEntry t); _ -> pure ()) es
          case [(k, o) | k <- ["network", "publish"], Just o <- [lookup k opts], givenLiteral o] of
            (k, o) : _ -> optionErr o ("'" <> k <> "' is given with egress, which provides the network: a container given egress reaches only what it lists, and publishes no port")
            [] -> pure ()
        _ -> pure ()

    literalValue (k, v) = case coreF v of
      CStrLit t -> Just (k, t)
      _ -> Nothing

    isNullLit c = case coreF c of
      CNull -> True
      _ -> False

    -- A literal that gives the option: not null, and not a list of
    -- nothing but nulls, which leaves it out (10.2).
    givenLiteral c = case coreF c of
      CStrLit _ -> True
      CArray xs -> any (not . isNullLit) xs
      _ -> False

    optionErr :: Core -> Text -> TC a
    optionErr c = abort . diag ETypeEnvConstruct (coreSpan c)

    unknown n
      | n `elem` ["image", "dockerfile"] = "a runnable cannot change the image; name it by the head, as #alpine:3.20"
      | n `elem` imageOptionNames =
          "'" <> n <> "' decides the image, and is given on the head, as #alpine:3.20(" <> n <> " = ...)"
      | otherwise = "unknown run option: '" <> n <> "'"

-- | An option an environment expression or a runnable does not take,
-- with the image and run options close to it when it is none of them.
-- One that is an option, given in the wrong place, is explained by the
-- message instead.
unknownEnvOption :: Text -> Diagnostic -> Diagnostic
unknownEnvOption n
  | n `elem` known || n `elem` ["image", "dockerfile"] = id
  | otherwise = suggesting n known
  where
    known = imageOptionNames <> runOptionNames

-- | Migration hint for the removed @#docker(...)@ form.
removedDocker :: Text
removedDocker =
  "#docker(...) was removed: name the image by the head, as #alpine:3.20(memory = \"4g\"), "
    <> "or a recipe by its path, as #./images/build/Dockerfile(context = \".\")"

-- | Bind the named arguments of an environment expression or an option
-- application against the options it accepts (spec 6.7, 7.5).
bindEnvOptions :: Ctx -> FilePath -> Locals -> [(Text, Type)] -> (Text -> Text) -> [Arg] -> TC [(Text, Core)]
bindEnvOptions ctx path locals sig unknown = foldM step []
  where
    step acc (Arg asp af) = case af of
      APos _ ->
        abort . diag ETypeEnvConstruct asp $
          "environment options are named, as memory = \"4g\"; the image is named by the head, as #alpine:3.20"
      AKw n e -> do
        when (n `elem` map fst acc) $
          () <$ abort (diag ETypeEnvConstruct asp ("duplicate environment option: '" <> n <> "'"))
        case lookup n sig of
          Just ty -> do
            c <- checkEnvArg e ty
            pure (acc <> [(n, c)])
          Nothing -> abort (unknownEnvOption n (diag ETypeEnvConstruct asp (unknown n)))

    -- A list or table option is declared with nullable elements, so
    -- that a literal can hold a null to leave out (10.2). A value
    -- already typed as a list or table of strings conforms to that as
    -- it is, containers being covariant (4.4).
    checkEnvArg e ty = check ctx path locals e ty

-- Calls (spec 7.5) ------------------------------------------------------------------------

data Callee
  = -- | Statically resolved declaration or direct lambda: keyword
    -- arguments allowed, variadic collection applies. The type
    -- variables are those the declaration binds (spec 4.2), empty for
    -- a monomorphic one.
    CalleeStatic Core [Text] (Map Text Bound) Type StaticParams
  | -- | Builtin with a type scheme.
    CalleeBuiltin Text Scheme
  | -- | Any other function-typed value: positional-only, exact arity.
    CalleeValue Core Type

elabCall :: Ctx -> FilePath -> Locals -> Span -> Expr -> [Arg] -> Maybe Type -> TC (Core, Type)
elabCall ctx path locals sp fn args mExpected = do
  validateArgOrder
  callee <- resolveCallee
  case callee of
    CalleeStatic fnCore tvs bounds fnTy params -> do
      ret <- case fnTy of
        TyFun _ r -> pure r
        other -> abort (diag ETypeCall (exprSpan fn) ("cannot call a value of type " <> renderType other))
      (posCores, kwCores, retTy, inst) <- bindStatic tvs bounds ret params
      case fnCore of
        Core _ (CVar (LocalRef ln))
          | (v, t) : _ <- [(v, t) | (v, t) <- inst, not (orderNative ctx t)] ->
              abort . diag ETypeBound sp $
                "'" <> ln <> "' is a local function, so its " <> varDisplay (T.takeWhile (/= '@') v) <> " is ordered as Number and String are, and " <> renderType t <> " is neither; declare the function at the top level to order it by its instance"
        _ -> pure ()
      pure (Core sp (CApp (instantiateRef fnCore inst) posCores kwCores), retTy)
    -- runnable's keyword parameters are the run options (spec 15.5).
    CalleeBuiltin "runnable" _ -> case args of
      Arg _ (APos e) : rest -> do
        env <- check ctx path locals e TyEnvironment
        elabRunnable ctx path locals sp env rest
      _ -> abort (diag ETypeArity sp "runnable(...) takes an environment, then its run options by name")
    CalleeBuiltin name scheme -> elabBuiltinCall name scheme
    CalleeValue fnCore fnTy -> cannotTell ETypeCall (exprSpan fn) "a call" fnTy >> case fnTy of
      TyFun paramTys retTy -> do
        unless (null kwArgs) $
          abort (diag ETypeKeyword sp "keyword arguments require a statically resolved function declaration")
        unless (length posArgs == length paramTys) $
          abort . diag ETypeArity sp $
            "expected " <> tshow (length paramTys) <> " arguments, got " <> tshow (length posArgs)
        cores <- mapM (\(e, t) -> check ctx path locals e t) (zip posExprs paramTys)
        pure (Core sp (CApp fnCore cores []), retTy)
      other ->
        abort (diag ETypeCall (exprSpan fn) ("cannot call a value of type " <> renderType other))
  where
    tshow = T.pack . show
    (posArgs, kwArgs) = span (\(Arg _ af) -> case af of APos _ -> True; _ -> False) args
    posExprs = [e | Arg _ (APos e) <- posArgs]

    validateArgOrder =
      when (any (\(Arg _ af) -> case af of APos _ -> True; _ -> False) kwArgs) $
        abort (diag ETypeKeyword sp "positional arguments must precede keyword arguments")

    resolveCallee = case exprF fn of
      EVar n
        | Just t <- Map.lookup n locals,
          gvs@(_ : _) <- generalizedVars t,
          TyFun ps _ <- t -> do
            -- A generic local function, instantiated at this call as a
            -- generic declaration is (spec 4.4, 6.1). It is a value, so
            -- it takes positional arguments only (7.5).
            bounds <- gets stLocalBounds
            recordVar (exprSpan fn) n t Nothing
            pure $
              CalleeStatic
                (Core (exprSpan fn) (CVar (LocalRef n)))
                gvs
                (Map.restrictKeys bounds (Set.fromList gvs))
                t
                (StaticParams [("#" <> T.pack (show i), p) | (i, p) <- zip [1 :: Int ..] ps] Nothing [] [])
        | Map.member n locals -> valueCallee
        | otherwise -> case lookupValueTarget ctx path n of
            Just (VTopLevel p dn) -> staticFromDecl (p, dn)
            Just (VBuiltin bn) -> case Map.lookup bn builtinSchemes of
              Just scheme -> do
                recordBuiltin (exprSpan fn) bn (schemeType scheme) bn
                pure (CalleeBuiltin bn scheme)
              Nothing -> abort (undefinedName ctx path locals (exprSpan fn) n)
            Nothing -> abort (undefinedName ctx path locals (exprSpan fn) n)
      EDot inner (Spanned _ fld)
        | Just key <- namespaceExpr ctx path locals inner ->
            staticFromDecl (namespaceMember (ctxScopes ctx) key fld)
      ELambda tps ps rt body -> do
        monoLambda (exprSpan fn) tps
        (lam, ty, params) <- elabLambda ctx path locals (exprSpan fn) Nothing ps rt body
        pure (CalleeStatic lam [] Map.empty ty params)
      _ -> valueCallee

    valueCallee = do
      (c, t) <- infer ctx path locals fn
      pure (CalleeValue c t)

    staticFromDecl key = do
      notAClass ctx (exprSpan fn) calleeName key
      cd <- demandOrHeader key
      case cd of
        Just (core, tvs, bounds, ty, Just params) -> pure (CalleeStatic core tvs bounds ty params)
        Just (core, _, _, ty, Nothing) -> pure (CalleeValue core ty)
        Nothing -> valueCallee

    -- For recursive calls the declaration is still being elaborated;
    -- fall back to its header type as a plain function value.
    demandOrHeader key@(p, n) = do
      active <- gets stActive
      if key `Set.member` active
        then do
          (vs, t) <- headerType ctx key
          recordVar (exprSpan fn) n t (Just key)
          -- Still being elaborated: its parameter information is not
          -- available yet, so the call is checked against the header
          -- type as a value. A recursive call of a generic keeps its
          -- variables, which the header does carry. A function with a
          -- required keyword parameter cannot be called as a value
          -- (7.5), so its annotations have to give the parameters.
          params <- case lookupDeclAst ctx key >>= declaredParams of
            Just ps
              | not (null [k | Param _ (PKeyword k _ _ Nothing) <- ps]) ->
                  if annotated ps
                    then Just <$> headerStaticParams (withTypeVars vs ctx) p ps
                    else
                      abort . diag ETypeKeyword (exprSpan fn) $
                        "a recursive call of '" <> n <> "', which has a required keyword parameter, needs every keyword parameter annotated"
            _ -> pure Nothing
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), map fst vs, boundMap vs, t, params))
        else do
          cd <- demandOrStandIn ctx key
          recordVar (exprSpan fn) n (cdType cd) (Just key)
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), cdTypeVars cd, cdBounds cd, cdType cd, cdParams cd))

    -- The parameters a declaration writes: a function declaration's,
    -- or those of the lambda a binding is (spec 6.1).
    declaredParams (Decl _ df) = case df of
      DFunction _ _ ps _ _ _ -> Just ps
      DValue _ _ _ (Expr _ (ELambda _ ps _ _)) -> Just ps
      _ -> Nothing

    -- Static binding per 7.5 for declarations. A declaration that
    -- binds type variables (spec 4.2) is instantiated here, through
    -- the same two passes a built-in call uses, so a call of a generic
    -- declaration keeps keyword arguments and variadic collection.
    bindStatic tvs bounds ret (StaticParams positional variadic keywords required) = do
      -- Keyword-name violations (binding positional/variadic
      -- parameters by name) report E-TYPE-KEYWORD before arity.
      let nonKeywordNames =
            Set.fromList (map fst positional) <> maybe Set.empty (Set.singleton . fst) variadic
      mapM_
        ( \(Arg asp af) -> case af of
            AKw n _
              | n `Set.member` nonKeywordNames ->
                  () <$ abort (diag ETypeKeyword asp ("parameter '" <> n <> "' cannot be bound by keyword"))
            _ -> pure ()
        )
        kwArgs
      let nPos = length positional
      -- What the call should have looked like (spec 14.3).
      let takes =
            withNote $
              "'" <> calleeName <> "' takes "
                <> if null positional
                  then "no positional arguments"
                  else "(" <> T.intercalate ", " [pn <> ": " <> renderType pt | (pn, pt) <- positional] <> ")"
      when (length posExprs < nPos) $
        () <$ abort (takes (diag ETypeArity sp ("missing positional arguments: expected " <> tshow nPos <> ", got " <> tshow (length posExprs))))
      let (bound, extra) = splitAt nPos posExprs
      when (isNothing variadic && not (null extra)) $
        () <$ abort (takes (diag ETypeArity sp ("too many positional arguments: expected " <> tshow nPos <> ", got " <> tshow (length posExprs))))
      kwSlots <- keywordSlots keywords
      case [n | n <- required, n `notElem` [k | (k, _, _) <- kwSlots]] of
        n : _ -> () <$ abort (diag ETypeKeyword sp ("missing keyword argument: '" <> n <> "'"))
        [] -> pure ()
      let (declVs, ren) = freshen tvs
          posSlots =
            zip bound (map (ren . snd) positional)
              <> [(e, ren elemTy) | Just (_, elemTy) <- [variadic], e <- extra]
      if null tvs
        then do
          posCores <- mapM (\(e, t) -> check ctx path locals e t) posSlots
          kwCores <- mapM (\(n, e, t) -> (,) n <$> check ctx path locals e t) kwSlots
          pure (posCores, kwCores, ret, [])
        else do
          -- Pre-bind from the expected return type, then let every
          -- argument -- positional, variadic and keyword alike --
          -- contribute, in no particular order (spec 4.4).
          let retPat = ren ret
              subst0 = case mExpected of
                Just expT -> either (const Map.empty) id (unifyE retPat expT Map.empty)
                Nothing -> Map.empty
          (cores, subst) <-
            goArgs retPat subst0 (posSlots <> [(e, ren t) | (_, e, t) <- kwSlots])
          retTy <- instantiateRet calleeName declVs retPat subst
          checkBounds calleeName bounds subst
          vs <- gets stInfer
          let (posCores, kwCores) = splitAt (length posSlots) cores
              declared = Map.mapKeys (T.takeWhile (/= '#')) subst
              inst = [(v, zonkWith vs t) | (v, b) <- Map.toList bounds, dictBound b, Just t <- [Map.lookup v declared]]
          pure (posCores, zip [n | (n, _, _) <- kwSlots] kwCores, retTy, inst)

    -- The keyword arguments a call gives, validated against the
    -- declaration's keyword parameters (spec 7.5).
    keywordSlots keywords = go Set.empty kwArgs
      where
        kwTypes = Map.fromList keywords
        go _ [] = pure []
        go seen (Arg asp (AKw n e) : rest) = do
          when (n `Set.member` seen) $
            () <$ abort (diag ETypeKeyword asp ("duplicate keyword argument: '" <> n <> "'"))
          case Map.lookup n kwTypes of
            Just t -> ((n, e, t) :) <$> go (Set.insert n seen) rest
            Nothing ->
              abort . suggesting n (map fst keywords) $
                diag ETypeKeyword asp ("unknown keyword argument: '" <> n <> "'")
        go seen (Arg _ (APos _) : rest) = go seen rest -- unreachable (validated)

    -- The arguments, then the variables only the parameters of
    -- function arguments bound from above (spec 4.4, Variance): each
    -- is the greatest type those parameters all take, and is checked
    -- against them once every argument has spoken, so the order of
    -- the arguments decides nothing.
    goArgs retPat subst argSlots = do
      (done, deferred, subst', ups) <- firstPass subst [] Map.empty [] (zip [0 :: Int ..] argSlots)
      subst'' <- settleUppers subst' ups
      (done', subst''', ups') <- retryPass subst'' ups done deferred
      subst4 <- settleUppers subst''' ups'
      mapM_ (checkUpper subst4) ups'
      -- One bounded only by Any, and absent from the result, can be
      -- nothing but Any; one in the result is left to the expected
      -- type, or is the ordinary inference failure.
      let anyOnly =
            [ (v, TyAny)
            | (v, _, _, _, _) <- ups',
              not (Map.member v subst4),
              v `notElem` typeVars retPat
            ]
      pure (Map.elems done', Map.union subst4 (Map.fromList anyOnly))

    firstPass subst ups done deferred [] = pure (done, reverse deferred, subst, ups)
    firstPass subst ups done deferred (slot@(i, (argExpr, pat)) : rest) = do
      let p = applySubst subst pat
      if settled ctx p
        then do
          c <- checkArg argExpr p
          firstPass subst ups (Map.insert i c done) deferred rest
        else do
          r <- tryTC $ do
            (c, t) <- inferWithHint argExpr p
            (subst', new) <- matchArg (exprSpan argExpr) p t subst
            pure (c, subst', new)
          case r of
            Right (c, subst', new) -> firstPass subst' (ups <> new) (Map.insert i c done) deferred rest
            -- Deferred rather than reported: another argument may
            -- yet make this position concrete, and if none does,
            -- the retry reports it against the type it ended with.
            Left _ -> firstPass subst ups done (slot : deferred) rest

    -- A retried argument whose position is now concrete is checked
    -- against it, which is what gives @cast@ its target. One still
    -- undetermined is elaborated as before, so its own diagnostic
    -- is the report.
    retryPass subst ups done [] = pure (done, subst, ups)
    retryPass subst ups done ((i, (argExpr, pat)) : rest) = do
      let p = applySubst subst pat
      if settled ctx p
        then do
          c <- checkArg argExpr p
          retryPass subst ups (Map.insert i c done) rest
        else do
          (c, t) <- inferWithHint argExpr p
          when (t == TyAny) (anyArgument (exprSpan argExpr) p)
          (subst', new) <- matchArg (exprSpan argExpr) p t subst
          retryPass subst' (ups <> new) (Map.insert i c done) rest

    -- An argument at a concrete position. A function argument that
    -- does not conform is reported as 'matchArg' reports it, in the
    -- terms of the call, where that finds the parameter or the result
    -- at fault; anything else keeps the diagnostic of the check.
    checkArg argExpr p = do
      r <- tryTC (check ctx path locals argExpr p)
      case r of
        Right c -> pure c
        Left d
          | hasFun p ->
              tryTC (infer ctx path locals argExpr) >>= \case
                Right (_, t) -> varianceOf (exprSpan argExpr) p t >> abort d
                Left _ -> abort d
          | otherwise -> abort d
      where
        hasFun t = case t of
          TyFun _ _ -> True
          TyArray e -> hasFun e
          TyMap e -> hasFun e
          TyAsync e -> hasFun e
          TyRecord fs -> any (hasFun . fieldType) fs
          _ -> False

    -- Matching an argument against its parameter (spec 4.4). Outside
    -- a function type it is exact. Inside one, a parameter conforms
    -- the other way round and a result as it is (Variance): a type
    -- already known is checked by conformance, and a variable in a
    -- parameter position of the argument is bounded from above by
    -- what the argument takes rather than bound to it. What is left
    -- is matched as before, so the result of a function argument
    -- still determines its variable.
    matchArg asp p t subst = do
      (p', ups) <- varianceOf asp p t
      subst' <- unifyM ctx asp p' t subst
      pure (subst', [(v, a, asp, p, t) | (v, a) <- ups])

    -- The variance part of 'matchArg': the pattern with what was
    -- checked by conformance put in place, and the upper bounds found.
    varianceOf asp p t = variance False True p t
      where
        variance inFun co pt at = case (pt, at) of
          _ | inFun && hasInfer at -> pure (pt, [])
          (TyVar v, _)
            | inFun,
              not co,
              v `elem` calleeVars pt ->
                pure (at, [(v, at)])
          _
            | inFun,
              pt /= at,
              null (calleeVars pt) -> do
                ok <- if co then conformsM ctx asp at pt else conformsM ctx asp pt at
                unless ok (varianceMismatch asp co pt at p t)
                pure (at, [])
          (TyFun ps r, TyFun qs r')
            | length ps == length qs -> do
                params <- zipWithM (variance True (not co)) ps qs
                (r'', ur) <- variance True co r r'
                pure (TyFun (map fst params) r'', concatMap snd params <> ur)
          (TyArray x, TyArray y) -> first TyArray <$> variance inFun co x y
          (TyMap x, TyMap y) -> first TyMap <$> variance inFun co x y
          (TyAsync x, TyAsync y) -> first TyAsync <$> variance inFun co x y
          (TyRecord fp, TyRecord fa)
            | Map.keysSet fp == Map.keysSet fa,
              map fieldOptional (Map.elems fp) == map fieldOptional (Map.elems fa) -> do
                fields <-
                  traverse
                    (\(f, g) -> first (\ft -> f {fieldType = ft}) <$> variance inFun co (fieldType f) (fieldType g))
                    (Map.intersectionWith (,) fp fa)
                pure (TyRecord (fmap fst fields), concatMap snd (Map.elems fields))
          _ -> pure (pt, [])

    -- The callee's own variables in a pattern, as 'unifyM' finds them.
    calleeVars pt = [v | v <- typeVars pt, not (isInferVar v), not (v `Set.member` ctxTypeVars ctx)]

    -- A variable only parameter positions bounded is the greatest type
    -- they all take. Where that is Any, it stays open until the end:
    -- Any is never chosen where it would become a result (spec 4.4).
    settleUppers subst ups = foldM settle subst (Map.toList grouped)
      where
        grouped = Map.fromListWith (flip (<>)) [(v, [u]) | u@(v, _, _, _, _) <- ups, not (Map.member v subst)]
        settle s (v, us@(u0 : _)) = case foldM (meetType ctx) (upperOf u0) (map upperOf us) of
          Just m
            | m /= TyAny -> pure (Map.insert v m s)
          Just _ -> pure s
          Nothing -> case [u | u <- us, isNothing (meetType ctx (upperOf u0) (upperOf u))] of
            u : _ -> abort (noCommonUpper u0 u)
            [] -> pure s
        settle s (_, []) = pure s
        upperOf (_, a, _, _, _) = a

    -- Each upper bound holds of what its variable became.
    checkUpper subst (v, a, asp, p, t) = case Map.lookup v subst of
      Just b -> do
        ok <- conformsM ctx asp b a
        unless ok (varianceMismatch asp False b a p t)
      Nothing -> pure ()

    -- Said in the terms of the call (spec 4.4): what the callee passes
    -- a function argument, or needs back from it, against what the
    -- function takes or returns.
    varianceMismatch asp co want got p t =
      abort . withExpectedActual (renderSig p) (renderType t) . diag ETypeMismatch asp $
        if co
          then
            "this function returns " <> renderType got <> ", but '" <> calleeName <> "' needs "
              <> renderSig want
          else
            "this function takes " <> renderType got <> ", but '" <> calleeName <> "' passes it "
              <> renderSig want

    noCommonUpper (_, a, _, _, _) (_, b, bsp, p, t) =
      withExpectedActual (renderSig p) (renderType t) . diag ETypeMismatch bsp $
        "'" <> calleeName <> "' passes one type to functions that take " <> renderType a
          <> " and " <> renderType b <> ", and no type is both"

    -- An Any value may be placed only where Any is required (spec
    -- 4.4). Said in the terms of the call, since the position comes
    -- from a signature and not from something the source names.
    anyArgument asp p =
      abort . withExpectedActual (renderSig p) "Any" . diag ETypeMismatch asp $
        "'"
          <> calleeName
          <> "' expects "
          <> renderSig p
          <> " here, and an Any value cannot be placed there; move it to a"
          <> " concrete type first with cast (15.8) or case (6.4)"

    -- A lambda argument adopts concrete parameter types from the
    -- (partially instantiated) pattern.
    inferWithHint argExpr p = case (exprF argExpr, p) of
      (ELambda [] ps rt body, TyFun expPs expRet)
        | all (settled ctx) expPs -> do
            -- The result too, once it is concrete: until then the
            -- body is what determines it.
            let ret = if settled ctx expRet then Just expRet else Nothing
            (lam, ty, _) <- elabLambdaAgainst ctx path locals (exprSpan argExpr) ps rt body expPs ret
            pure (lam, ty)
      _ -> infer ctx path locals argExpr

    -- A callee's type variables and the enclosing declaration's are
    -- both written T in practice, and instantiation has to tell them
    -- apart, so the callee's are renamed to names no source can write
    -- (spec 4.4). Outside a generic declaration there is nothing to
    -- collide with, and nothing is renamed.
    freshen vs
      | Set.null (ctxTypeVars ctx) || null vs = (vs, id)
      | otherwise =
          ( map fresh vs,
            applySubst (Map.fromList [(v, TyVar (fresh v)) | v <- vs])
          )
      where
        fresh v = v <> "#"

    -- The result of instantiating a signature at this call: the
    -- substitution applied, checked for well-formedness (4.2), and
    -- reported as an inference failure where a variable is left over.
    instantiateRet name' vs ret subst
      | all (`Map.member` subst) vs = done subst
      | otherwise = case mExpected of
          Just expT -> do
            s' <- unifyM ctx sp (applySubst subst ret) expT subst
            if all (`Map.member` s') vs then done s' else cannotInstantiate
          Nothing -> cannotInstantiate
      where
        done sub = let t = applySubst sub ret in wellFormedRet t >> pure t
        wellFormedRet t =
          unless (wellFormed t) $
            abort . diag ETypeIllformed sp $
              "'" <> name' <> "' would have the ill-formed result type " <> renderSig t
        cannotInstantiate =
          abort . diag ETypeMismatch sp $
            "cannot instantiate the type of '" <> name' <> "'; add a type annotation"

    -- Every bounded variable has to be instantiated within its bound
    -- (spec 4.4). A variable renamed apart by 'freshen' is found under
    -- its declared name.
    checkBounds name' bounds subst =
      sequence_
        [ do
            ok <- satisfiesBoundM ctx sp b t
            unless ok (abort (boundViolation ctx sp name' v b t))
        | (v, b) <- Map.toList bounds,
          Just t <- [Map.lookup v (Map.mapKeys (T.takeWhile (/= '#')) subst)]
        ]

    -- The name of the callee, for diagnostics about its instantiation.
    calleeName = case exprF fn of
      EVar n -> n
      EDot _ (Spanned _ fld) -> fld
      _ -> "function"

    -- Builtin calls: scheme instantiation (spec 4.4), plus the
    -- special cases of cast (15.8) and to_string.
    elabBuiltinCall name scheme
      | name == "cast" = do
          expected <- maybe castNeedsType pure mExpected
          unless (castable expected) $
            abort (diag ETypeIllformed sp ("cast target must be a data type, got " <> renderType expected))
          case (posExprs, kwArgs) of
            ([arg], []) -> do
              (c, _) <- infer ctx path locals arg
              pure (Core sp (CCast c expected), expected)
            _ -> abort (diag ETypeArity sp "cast takes exactly one argument")
      | otherwise = do
          regexAdvisory name
          unless (null kwArgs) $
            () <$ abort (diag ETypeKeyword sp ("'" <> name <> "' takes no keyword arguments"))
          let (schemeVs, ren) = freshen (schemeVars scheme)
              params = map ren (schemeParams scheme)
              retPat = ren (schemeRet scheme)
          unless (length posExprs == length params) $
            () <$ abort
              ( withNote ("its signature is " <> name <> "(" <> T.intercalate ", " (map renderType (schemeParams scheme)) <> "): " <> renderType (schemeRet scheme)) $
                  diag ETypeArity sp ("'" <> name <> "' expects " <> tshow (length params) <> " arguments, got " <> tshow (length posExprs))
              )
          -- Pre-bind type variables from the expected return type.
          let subst0 = case mExpected of
                Just expT -> either (const Map.empty) id (unifyE retPat expT Map.empty)
                Nothing -> Map.empty
          (cores, subst) <- goArgs retPat subst0 (zip posExprs params)
          retTy <- instantiateRet name schemeVs retPat subst
          checkBounds name (schemeBounds scheme) subst
          fnRef <- builtinRef ctx sp name (schemeBounds scheme) (Map.mapKeys (T.takeWhile (/= '#')) subst)
          pure (Core sp (CApp fnRef cores []), retTy)
      where
        castNeedsType =
          abort (diag ETypeMismatch sp "cast requires an expected type from context")

        -- A pattern written as a literal is known before the call is
        -- reached: one that does not compile fails every call, which
        -- is said now (spec 15.3, 14.2).
        regexAdvisory n
          | n `elem` ["regex_test", "regex_match", "regex_replace"],
            _ : Expr psp (EString [TPChunk _ pat]) : _ <- posExprs,
            Left err <- Re.compile pat =
              advise WRegexPattern psp ("this pattern does not compile, so every call of " <> n <> " fails: " <> err)
          | otherwise = pure ()

        -- Arguments are elaborated in source order, except that one
        -- which cannot be typed on its own is set aside and retried
        -- once the others have determined the variables of its
        -- position. Instantiation comes from the argument types and
        -- the expected type in no particular order (spec 4.4), so a
        -- context-typed call such as @cast@ (15.8) or @fail@ (15.7)
        -- may sit in any argument whose type another one fixes. Each
        -- argument is still elaborated exactly once, because a
        -- deferred attempt commits nothing ('tryTC'), and each keeps
        -- its place in the result, so evaluation order is untouched
        -- (8.3).
-- | The type a field read yields (spec 6.8): an optional field may be
-- absent, and an absent key reads as null, so it is @T | Null@.
readFieldType :: Field -> Type
readFieldType f
  | fieldOptional f = mkUnion (fieldType f) [TyNull]
  | otherwise = fieldType f

-- | Legal target of @cast@ (spec 15.8): a data type, fully
-- instantiated. A union is one when all of its members are, which
-- 'dataType' already requires of every union (4.2).
castable :: Type -> Bool
castable t = dataType t && isGround t

-- Type variable substitution / matching -------------------------------------------------------

type Subst = Map Text Type

-- | First-order matching of a scheme pattern against a concrete type.
unifyE :: Type -> Type -> Subst -> Either Text Subst
unifyE pat actual s = case (pat, actual) of
  (TyVar v, t) -> case Map.lookup v s of
    Just bound
      | bound == t -> Right s
      | otherwise ->
          Left ("type variable " <> v <> " bound to both " <> renderType bound <> " and " <> renderType t)
    Nothing -> Right (Map.insert v t s)
  (TyArray a, TyArray b) -> unifyE a b s
  (TyMap a, TyMap b) -> unifyE a b s
  (TyAsync a, TyAsync b) -> unifyE a b s
  -- Instantiating a union return type against an expected union (spec
  -- 4.4): drop the members the two share, and bind if that leaves one
  -- variable facing one type.
  (TyUnion ps, TyUnion as) ->
    let common = filter (`elem` as) ps
        ps' = filter (`notElem` common) ps
        as' = filter (`notElem` common) as
     in case (ps', as') of
          ([], []) -> Right s
          ([p], [a]) -> unifyE p a s
          _ -> Left (renderType pat <> " does not match " <> renderType actual)
  (TyRecord as, TyRecord bs)
    | Map.keysSet as == Map.keysSet bs,
      map fieldOptional (Map.elems as) == map fieldOptional (Map.elems bs) ->
        foldM
          (\acc (a, b) -> unifyE (fieldType a) (fieldType b) acc)
          s
          (zip (Map.elems as) (Map.elems bs))
  (TyFun aps ar, TyFun bps br)
    | length aps == length bps -> do
        s' <- foldM (\acc (a, b) -> unifyE a b acc) s (zip aps bps)
        unifyE ar br s'
  (a, b)
    | a == b -> Right s
    | otherwise -> Left (renderType a <> " does not match " <> renderType b)

unifyOrFail :: Span -> Type -> Type -> Subst -> TC Subst
unifyOrFail sp pat actual s = case unifyE pat actual s of
  Right s' -> pure s'
  Left msg ->
    abort $
      withExpectedActual (renderSig (applySubst s pat)) (renderType actual) $
        diag ETypeMismatch sp ("type mismatch: " <> asWritten msg)

-- | A signature's type as the source writes it: the marker that keeps
-- a callee's type variables apart from the caller's (spec 4.4) is an
-- implementation device and never appears in a diagnostic.
renderSig :: Type -> Text
renderSig = asWritten . renderType

asWritten :: Text -> Text
asWritten = T.filter (/= '#')

-- Classes and instances (spec 4.6) --------------------------------------------

-- | The class a bound names, by its module and name, where it names
-- one: a name in scope, or a member of a namespace.
classNamed :: Ctx -> FilePath -> Maybe Text -> Text -> Maybe Key
classNamed ctx path q n = do
  gs <- Map.lookup path (ctxScopes ctx)
  key <- case q of
    Nothing -> case Map.lookup n (gsValues gs) of
      Just (VTopLevel p n') -> Just (p, n')
      _ -> Nothing
    Just ns -> do
      m <- Map.lookup ns (gsNamespaces gs)
      pure (namespaceMember (ctxScopes ctx) m n)
  key <$ classDeclAst ctx key

-- | A class is written where a bound is (spec 4.6): it is no value.
notAClass :: Ctx -> Span -> Text -> Key -> TC ()
notAClass ctx sp n key = case classDeclAst ctx key of
  Just _ -> abort (diag ETypeMismatch sp ("'" <> n <> "' is a class, which is written as a bound (<T: " <> n <> ">), not as a value"))
  Nothing -> pure ()

-- | A class declaration as written.
classDeclAst :: Ctx -> Key -> Maybe (Span, TypeParam, [ClassMethod])
classDeclAst ctx (path, name) = do
  lm <- Map.lookup path (progModules (ctxProg ctx))
  listToMaybe [(sp, tp, ms) | Decl sp (DClass n tp ms) <- moduleDecls (lmModule lm), n == name]

-- | What a class's type parameter is bounded by, where that is a named
-- bound or a class.
superBound :: Ctx -> FilePath -> SBound -> Maybe (Either NamedBound Key)
superBound ctx p b = case b of
  SBoundNamed (Spanned _ n) -> case namedBoundFromText n of
    Just nb -> Just (Left nb)
    Nothing -> Right <$> classNamed ctx p Nothing n
  SBoundQualified (Spanned _ q) (Spanned _ n) -> Right <$> classNamed ctx p (Just q) n
  SBoundClass cp cn -> Just (Right (cp, cn))
  SBoundType _ -> Nothing
  SBoundAll _ -> Nothing

-- | The superclass of a class, as its type parameter's bound names it.
classSuper :: Ctx -> Key -> Maybe (Either NamedBound Key)
classSuper _ key | key == orderableKey = Just (Left BComparable)
classSuper ctx key@(p, _) = do
  (_, TypeParam _ mb, _) <- classDeclAst ctx key
  mb >>= superBound ctx p

-- | A class where it is a bound (spec 4.6), with what it entails along
-- its chain of superclasses. A chain that comes back to a class is cut
-- there; the check of the class reports it.
classRefOf :: Ctx -> Key -> ClassRef
classRefOf ctx = go []
  where
    go seen key@(p, n) = case classSuper ctx key of
      Just (Left nb) -> ClassRef p n (namedClosure nb) []
      Just (Right k)
        | k `notElem` (key : seen) ->
            let r = go (key : seen) k
             in ClassRef p n (crEntailsNamed r) (k : crSupers r)
      _ -> ClassRef p n [] []
    namedClosure nb = [b | b <- [minBound .. maxBound], boundEntails (BoundNamed nb) b]

-- | Whether a type has an instance of a class (spec 4.6), seen from a
-- module: a type parameter in scope by what its bound entails, and a
-- concrete type by an instance in effect there.
classHolds :: Ctx -> FilePath -> ClassRef -> Type -> Bool
classHolds ctx path c t = case t of
  TyVar v -> maybe False (`entailsBound` BoundClass c) (boundOf ctx v)
  _ -> isJust (instanceFor ctx (Just path) (classKey c) t)

-- | 'satisfiesBound', with the program's instances.
boundHolds :: Ctx -> FilePath -> Bound -> Type -> Bool
boundHolds ctx path b t = case b of
  BoundClass c -> classHolds ctx path c t
  BoundNamed nb -> holdsAt ctx path nb t
  BoundAll bs -> all (\b' -> boundHolds ctx path b' t) bs
  _ -> satisfiesBound (boundOf ctx) b t

-- | The instance of a class for a type: one in effect in the given
-- module -- declared there or in a module it imports, directly or not
-- -- or, without one, anywhere in the program.
instanceFor :: Ctx -> Maybe FilePath -> Key -> Type -> Maybe InstanceInfo
instanceFor ctx from cls t = fst <$> instanceMatch ctx from cls t

-- | 'instanceFor', with the types the type parameters of a parametric
-- instance take for the type (spec 4.6). Its type is matched against
-- the instance's, and each type a parameter takes has to satisfy the
-- parameter's bound in turn: a part of the type, so the search ends.
instanceMatch :: Ctx -> Maybe FilePath -> Key -> Type -> Maybe (InstanceInfo, Map Text Type)
instanceMatch ctx from cls t =
  listToMaybe
    [ (ii, sub)
    | ii <- clsInstances (ctxEnv ctx),
      iiClass ii == cls,
      maybe True (\p -> iiModule ii `Set.member` importClosure (ctxProg ctx) p) from,
      Just sub <- [matchHead (map fst (iiParams ii)) (iiHead ii) t],
      and
        [ boundHolds ctx (fromMaybe (iiModule ii) from) b (Map.findWithDefault TyAny v sub)
        | (v, Just b) <- iiParams ii
        ]
    ]

-- | Match a type against the type of an instance, whose variables
-- among the given ones stand for any type: the types they stand for.
matchHead :: [Text] -> Type -> Type -> Maybe (Map Text Type)
matchHead vars pat0 t0 = go pat0 t0 Map.empty
  where
    go pat t sub = case pat of
      TyVar v
        | v `elem` vars -> case Map.lookup v sub of
            Just u -> if u == t then Just sub else Nothing
            Nothing -> Just (Map.insert v t sub)
      _ -> case (pat, t) of
        (TyArray a, TyArray b) -> go a b sub
        (TyMap a, TyMap b) -> go a b sub
        (TyAsync a, TyAsync b) -> go a b sub
        (TyRecord fa, TyRecord fb)
          | Map.keysSet fa == Map.keysSet fb,
            map fieldOptional (Map.elems fa) == map fieldOptional (Map.elems fb) ->
              foldM (\s' (x, y) -> go (fieldType x) (fieldType y) s') sub (zip (Map.elems fa) (Map.elems fb))
        (TyFun ps r, TyFun qs r')
          | length ps == length qs -> foldM (\s' (x, y) -> go x y s') sub (zip (ps <> [r]) (qs <> [r']))
        _
          | pat == t -> Just sub
          | otherwise -> Nothing

-- | Whether two instances' types have a type in common (spec 4.6):
-- each one's variables among the given ones may stand for any type.
headsOverlap :: [Text] -> Type -> [Text] -> Type -> Bool
headsOverlap vs a ws b = isJust (unify (applySubst ren a) b Map.empty)
  where
    ren = Map.fromList [(v, TyVar (v <> "'")) | v <- vs]
    free = Set.fromList (map (<> "'") vs <> ws)
    walk sub t = case t of
      TyVar v | Just u <- Map.lookup v sub -> walk sub u
      _ -> t
    unify x0 y0 sub =
      let x = walk sub x0
          y = walk sub y0
       in case (x, y) of
            (TyVar v, _) | v `Set.member` free -> if x == y then Just sub else Just (Map.insert v y sub)
            (_, TyVar w) | w `Set.member` free -> Just (Map.insert w x sub)
            (TyArray p, TyArray q) -> unify p q sub
            (TyMap p, TyMap q) -> unify p q sub
            (TyAsync p, TyAsync q) -> unify p q sub
            (TyRecord fa, TyRecord fb)
              | Map.keysSet fa == Map.keysSet fb,
                map fieldOptional (Map.elems fa) == map fieldOptional (Map.elems fb) ->
                  foldM (\s' (f1, f2) -> unify (fieldType f1) (fieldType f2) s') sub (zip (Map.elems fa) (Map.elems fb))
            (TyFun ps r, TyFun qs r')
              | length ps == length qs -> foldM (\s' (p', q') -> unify p' q' s') sub (zip (ps <> [r]) (qs <> [r']))
            _
              | x == y -> Just sub
              | otherwise -> Nothing

-- | A module and every module it imports, directly or not.
importClosure :: Program -> FilePath -> Set FilePath
importClosure prog = go Set.empty . pure
  where
    go seen [] = seen
    go seen (p : rest)
      | p `Set.member` seen = go seen rest
      | otherwise =
          go (Set.insert p seen) (maybe [] (Map.elems . lmImportKeys) (Map.lookup p (progModules prog)) <> rest)

-- | The declaration a method's key names (spec 4.6): the method itself,
-- whose signature is all there is to it (True), or, under its name with
-- @#default@, its default body (False).
methodDeclAst :: Ctx -> Key -> Maybe (Bool, Decl)
methodDeclAst ctx (path, name) = do
  let (m, isDefault) = maybe (name, False) (\b -> (b, True)) (T.stripSuffix "#default" name)
  lm <- Map.lookup path (progModules (ctxProg ctx))
  (cn, TypeParam tv _, ClassMethod sp _ ps rt d) <-
    listToMaybe
      [ (cn, tp, cm)
      | Decl _ (DClass cn tp ms) <- moduleDecls (lmModule lm),
        cm@(ClassMethod _ (Spanned _ mn) _ _ _) <- ms,
        mn == m
      ]
  body <- if isDefault then d else Just (fromMaybe (Expr sp ENull) d)
  pure (not isDefault, Decl sp (DFunction m [TypeParam tv (Just (SBoundClass path cn))] ps (Just rt) Nothing body))

-- | A method: its signature alone (spec 4.6). Its core is never run:
-- every reference to it is to the method of an instance.
elabMethod :: Ctx -> FilePath -> Decl -> TC CoreDecl
elabMethod ctx path (Decl sp f) = case f of
  DFunction m tps ps (Just rt) _ _ -> do
    vs <- typeVarsOf ctx path tps
    let ctx' = withTypeVars vs ctx
    params <- headerStaticParams ctx' path ps
    ret <- typeFromS ctx' path rt
    pure (CoreDecl path m (map fst vs) (TyFun (map snd (spPositional params)) ret) (Core sp CNull) (Just params) (boundMap vs))
  _ -> abort (diag ENameUndefined sp "internal: not a method")

-- | A declaration with a type parameter bounded by a class: generic,
-- and run only as specialized (spec 4.6).
--
-- One bounded by @orderable@ alone is generic only where its body
-- orders a value of its type parameter's type, which a value of an
-- instance type is ordered by its instance to do.
isTemplate :: CoreDecl -> Bool
isTemplate cd =
  any (any isClass . boundParts) (Map.elems (cdBounds cd))
    || or
      [ v `elem` cdTypeVars cd
      | CVar (InstRef _ _ inst) <- map coreF (universeCore (cdCore cd)),
        (_, t) <- inst,
        v <- typeVars t
      ]
  where
    isClass b = case b of
      BoundClass _ -> True
      _ -> False

-- | A bound an instantiation has to be followed through: one with a
-- class, or @orderable@, which an instance may give a type (spec 4.6).
dictBound :: Bound -> Bool
dictBound = any one . boundParts
  where
    one b = case b of
      BoundClass _ -> True
      BoundNamed BOrderable -> True
      _ -> False

-- | Whether values of a type are ordered by the language (spec 15.4)
-- rather than by an instance: Number and String, a type still to be
-- found while parameters are inferred (that check is not the one that
-- runs), and a type parameter of a local function, which is never
-- specialized.
orderNative :: Ctx -> Type -> Bool
orderNative ctx t = case t of
  TyVar v -> isInferVar v || v `Set.member` ctxLocalVars ctx || T.isInfixOf "@" v
  _ -> nativeOrderable t || hasInfer t

-- | The name a built-in order operation is specialized under.
orderOpName :: PrimOp -> Text
orderOpName p = case p of
  PLt -> "<"
  PLe -> "<="
  PGt -> ">"
  _ -> ">="

-- | A built-in at the types a call or reference instantiates it at:
-- itself, unless an @orderable@ type parameter of it is at a type the
-- language does not order, where it is the built-in specialized to the
-- instance's order (spec 4.6).
builtinRef :: Ctx -> Span -> Text -> Map Text Bound -> Subst -> TC Core
builtinRef ctx sp name bounds subst = do
  vs <- gets stInfer
  let inst = [(v, zonkWith vs t) | (v, BoundNamed BOrderable) <- Map.toList bounds, Just t <- [Map.lookup v subst]]
  pure $
    if all (orderNative ctx . snd) inst
      then Core sp (CVar (BuiltinRef name))
      else Core sp (CVar (InstRef builtinClassModule name inst))

-- | The name of a declaration specialized at the types of its type
-- parameters with a class bound, in the order of their names.
specName :: Text -> [(Text, Type)] -> Text
specName n inst = n <> "<" <> T.intercalate ", " (map (renderType . snd) (sortOn fst inst)) <> ">"

-- | A reference to a declaration, at the types its class-bounded type
-- parameters are instantiated at.
instantiateRef :: Core -> [(Text, Type)] -> Core
instantiateRef c [] = c
instantiateRef (Core sp (CVar (TopRef p n))) inst = Core sp (CVar (InstRef p n (sortOn fst inst)))
instantiateRef c _ = c

-- | The key of a method an instance writes.
implKey :: InstanceInfo -> Text -> Key
implKey ii m = (iiModule ii, snd (iiClass ii) <> "<" <> renderType (iiHead ii) <> ">." <> m)

-- | Read the program's classes and instances. Nothing is reported
-- here: what does not read is left out, and reported by the check of
-- the class or instance it is in.
buildClassEnv :: Ctx -> TC ClassEnv
buildClassEnv ctx = do
  q <- gets stQuiet
  modify (\s -> s {stQuiet = True})
  classes <-
    Map.fromList
      <$> sequence
        [ do
            let ref = classRefOf ctx key
                ctx' = withTypeVars [(v, Just (BoundClass ref))] ctx
            methods <-
              sequence
                [ do
                    r <- recoverTC (TyFun <$> paramHeaderTypes ctx' path ps <*> typeFromS ctx' path rt)
                    pure (mn, MethodInfo ps d (either (const Nothing) Just r))
                | ClassMethod _ (Spanned _ mn) ps rt d <- ms
                ]
            pure (key, ClassInfo tp methods)
        | lm <- Map.elems (progModules (ctxProg ctx)),
          let path = lmPath lm,
          Decl _ (DClass n tp@(TypeParam (Spanned _ v) _) ms) <- moduleDecls (lmModule lm),
          let key = (path, n)
        ]
  instances <-
    catMaybes
      <$> sequence
        [ case either (const Nothing) Just (instanceClass ctx path qual c) of
            Nothing -> pure Nothing
            Just ck -> do
              r <- recoverTC $ do
                vs <- typeVarsOf ctx path tps
                (,) vs <$> typeFromS (withTypeVars vs ctx) path t
              pure (either (const Nothing) (\(vs, ht) -> Just (InstanceInfo path sp ck ht ds tps vs)) r)
        | -- Dependencies first (progOrder), so that of two instances
          -- for one type, the later is the one reported.
          path <- progOrder (ctxProg ctx),
          Just lm <- [Map.lookup path (progModules (ctxProg ctx))],
          Decl sp (DInstance tps qual (Spanned _ c) t ds) <- moduleDecls (lmModule lm)
        ]
  modify (\s -> s {stQuiet = q})
  let classes' = Map.insert orderableKey orderableInfo classes
      synthetic =
        Map.fromList
          [ (implKey ii mn, implDecl ci ii d)
          | ii <- instances,
            Just ci <- [Map.lookup (iiClass ii) classes'],
            d@(Decl _ (DFunction mn [] _ _ Nothing _)) <- iiDecls ii
          ]
  pure (ClassEnv classes' instances synthetic)
  where
    -- A method written in an instance, with the types the class gives
    -- it written in where it leaves them out.
    implDecl ci ii d@(Decl dsp df) = case df of
      DFunction mn _ ps rt w body
        | Just mi <- lookup mn (ciMethods ci),
          Just (TyFun pts r) <- miType mi,
          length pts == length ps ->
            let TypeParam (Spanned _ v) _ = ciParam ci
                sub = applySubst (Map.singleton v (iiHead ii))
                fill (Param psp pf) pt = Param psp $ case pf of
                  PPositional n sec Nothing -> PPositional n sec (Just (typeToS psp (sub pt)))
                  other -> other
             in Decl dsp (DFunction mn (iiTypeParams ii) (zipWith fill ps pts) (rt <|> Just (typeToS dsp (sub r))) w body)
      _ -> d

-- | The built-in class @orderable@ (spec 4.6): its method @compare@,
-- whose built-in instances are Number and String.
orderableInfo :: ClassInfo
orderableInfo =
  ClassInfo
    (TypeParam (Spanned NoSpan "T") (Just (SBoundNamed (Spanned NoSpan "comparable"))))
    [ ( "compare",
        MethodInfo
          [Param NoSpan (PPositional n Public (Just (SType NoSpan (SNamed Nothing "T" [])))) | n <- ["a", "b"]]
          Nothing
          (Just (TyFun [TyVar "T", TyVar "T"] TyNumber))
      )
    ]

-- | The class an instance declaration is of: a class in scope, or the
-- built-in @orderable@. The other named bounds are closed classes,
-- whose instances are the types the language gives them.
instanceClass :: Ctx -> FilePath -> Maybe (Spanned Text) -> Text -> Either Text Key
instanceClass ctx path qual c = case (qual, namedBoundFromText c) of
  (Nothing, Just BOrderable) -> Right orderableKey
  (Nothing, Just nb) ->
    Left ("'" <> renderNamedBound nb <> "' is a closed class: its instances are the types the language makes " <> renderNamedBound nb <> ", and no others are declared")
  _ -> maybe (Left ("'" <> maybe "" ((<> ".") . spannedValue) qual <> c <> "' is not a class")) Right (classNamed ctx path (spannedValue <$> qual) c)

-- | Check a class declaration (spec 4.6), and the signatures and
-- default bodies of its methods.
checkClassDecl :: Ctx -> (FilePath, Decl) -> TC ()
checkClassDecl ctx (path, Decl sp f) = case f of
  DClass n tp@(TypeParam _ mb) ms -> do
    when (isJust (namedBoundFromText n)) $
      abort (diag ENameDuplicate sp ("'" <> n <> "' is a named bound, which no class can be called"))
    case mb of
      Just (SBoundType st) ->
        abort (diag ETypeIllformed (stypeSpan st) "a class's type parameter is bounded by a named bound or by another class, not by a type")
      _ -> pure ()
    _ <- typeVarsOf ctx path [tp]
    when ((path, n) `elem` superChain (path, n)) $
      abort (diag ETypeIllformed sp ("class '" <> n <> "' is its own superclass"))
    sequence_
      [ abort (diag ETypeIllformed psp "a method takes positional parameters, each with its type written")
      | ClassMethod _ _ ps _ _ <- ms,
        Param psp pf <- ps,
        case pf of
          PPositional _ Public (Just _) -> False
          _ -> True
      ]
    sequence_
      [ do
          _ <- tryDemandDecl ctx (path, m)
          when (isJust d) (() <$ tryDemandDecl ctx (path, m <> "#default"))
      | ClassMethod _ (Spanned _ m) _ _ d <- ms
      ]
  _ -> pure ()
  where
    superChain = go (16 :: Int)
      where
        go 0 _ = []
        go k key = case classSuper ctx key of
          Just (Right s) -> s : go (k - 1) s
          _ -> []

-- | Check an instance declaration (spec 4.6): its class and type, where
-- it may be declared, that it is the only one, its superclass, and its
-- methods against the class's signatures.
checkInstanceDecl :: Ctx -> (FilePath, Decl) -> TC ()
checkInstanceDecl ctx (path, Decl sp f) = case f of
  DInstance tps qual (Spanned csp c) st ds -> do
    let written = maybe "" ((<> ".") . spannedValue) qual <> c
        display = written <> "<" <> renderSType st <> ">"
    ck <- case instanceClass ctx path qual c of
      Right k -> pure k
      Left msg
        | isJust (namedBoundFromText c) && isNothing qual -> abort (diag ETypeInstance csp msg)
        | otherwise -> abort (diag ENameUndefined csp msg)
    ci <- maybe dependencyFailed pure (Map.lookup ck (clsClasses (ctxEnv ctx)))
    vs <- typeVarsOf ctx path tps
    let ictx = withTypeVars vs ctx
    ht <- typeFromS ictx path st
    when (ht == TyAny || ht == TyVoid || not (wellFormed ht)) $
      abort (diag ETypeIllformed (stypeSpan st) ("there is no instance for " <> renderType ht <> ": an instance is for a type other than Any and Void"))
    -- A parametric instance is for the types its type has, each of its
    -- type parameters standing for a part of them (spec 4.6).
    case ht of
      TyVar _ -> abort (diag ETypeIllformed (stypeSpan st) "an instance is for a type that is more than a type parameter")
      _ -> pure ()
    case [v | (v, _) <- vs, v `notElem` typeVars ht] of
      v : _ -> abort (diag ETypeIllformed (stypeSpan st) ("the type parameter '" <> v <> "' of the instance is not in its type, so nothing says what it is"))
      [] -> pure ()
    when (varInUnion ht) $
      abort (diag ETypeIllformed (stypeSpan st) "a type parameter of an instance is not a member of a union: its type matches a type part by part")
    -- Instances are of the class's module, or of the project: a
    -- dependency cannot declare one for another's class (spec 4.6).
    let prog = ctxProg ctx
    unless (path == fst ck || isNothing (inCache (progCacheDir prog) path)) $
      abort (diag ETypeInstance sp ("an instance of '" <> written <> "' is declared in the module of the class or in the project, not in a dependency"))
    when (ck == orderableKey && nativeOrderable ht) $
      abort (diag ENameDuplicate sp (renderType ht <> " is orderable by the language, which is its one instance of orderable"))
    -- Instances whose types have one in common, after this one in the
    -- order of the modules, are reported where they are.
    let others = [ii | ii <- clsInstances (ctxEnv ctx), iiClass ii == ck, headsOverlap (map fst (iiParams ii)) (iiHead ii) (map fst vs) ht]
    case others of
      earlier : _
        | (iiModule earlier, iiSpan earlier) /= (path, sp) ->
            abort . withNote ("the other is at " <> renderAt (iiSpan earlier)) $
              diag ENameDuplicate sp ("duplicate instance " <> display <> ": a type has one instance of a class, and this one is for a type that another is for")
      _ -> pure ()
    case classSuper ctx ck of
      Just (Left nb)
        | not (holdsAt ictx path nb ht) ->
            abort (diag ETypeInstance sp ("'" <> written <> "' is a class of " <> renderNamedBound nb <> " types, and " <> renderType ht <> " is not " <> renderNamedBound nb))
      Just (Right sk)
        | isNothing (instanceFor ictx (Just path) sk ht) ->
            abort (diag ETypeInstance sp ("an instance " <> display <> " needs an instance " <> snd sk <> "<" <> renderSType st <> "> in effect here"))
      _ -> pure ()
    let methods = ciMethods ci
        names = [n | Decl _ (DFunction n _ _ _ _ _) <- ds] <> [n | Decl _ (DValue n _ _ _) <- ds]
    sequence_ [methodShape (map fst methods) d | d <- ds]
    case [n | (n : _ : _) <- group' names] of
      n : _ -> abort (diag ENameDuplicate sp ("the instance " <> display <> " defines '" <> n <> "' more than once"))
      [] -> pure ()
    case [m | (m, mi) <- methods, isNothing (miDefault mi), m `notElem` names] of
      [] -> pure ()
      missing ->
        abort (diag ETypeInstance sp ("the instance " <> display <> " does not define " <> T.intercalate ", " ["'" <> m <> "'" | m <- missing] <> ", which " <> written <> " has no default for"))
    let ii = InstanceInfo path sp ck ht ds tps vs
        TypeParam (Spanned _ v) _ = ciParam ci
    sequence_
      [ do
          r <- tryDemandDecl ctx (implKey ii m)
          case (r, miType mi) of
            (Just cd, Just expected)
              | let want = applySubst (Map.singleton v ht) expected,
                cdType cd /= want ->
                  abort . withExpectedActual (renderType want) (renderType (cdType cd)) $
                    diag ETypeMismatch dsp ("'" <> m <> "' of " <> display <> " has to have the type the class gives it")
            _ -> pure ()
      | Decl dsp (DFunction m _ _ _ _ _) <- ds,
        Just mi <- [lookup m methods]
      ]
  _ -> pure ()
  where
    group' xs = [[x | x' <- xs, x' == x] | x <- nub xs]
    varInUnion t = case t of
      TyUnion ms -> any (not . null . typeVars) ms
      TyArray e -> varInUnion e
      TyMap e -> varInUnion e
      TyAsync e -> varInUnion e
      TyRecord fs -> any (varInUnion . fieldType) (Map.elems fs)
      TyFun ps r -> any varInUnion (r : ps)
      _ -> False
    renderAt (Span (Position fl l _) _) = T.pack fl <> ":" <> T.pack (show l)
    renderAt NoSpan = "another module"
    methodShape known (Decl dsp df) = case df of
      DValue n _ _ _ -> abort (diag ETypeIllformed dsp ("'" <> n <> "' is written as a method is: " <> n <> "(...) = ..."))
      DFunction n tps ps _ w _
        | n `notElem` known ->
            abort . suggesting n known $ diag ENameUndefined dsp ("the class has no method '" <> n <> "'")
        | not (null tps) -> abort (diag ETypeIllformed dsp "a method of an instance declares no type parameters: the class gives its types")
        | isJust w -> abort (diag ETypeIllformed dsp "a method of an instance takes the access constraint of its module; it writes no within of its own")
        | Just mi <- lookup' n,
          length [() | Param _ (PPositional {}) <- ps] /= length ps || length ps /= length (miParams mi) ->
            abort (diag ETypeArity dsp ("'" <> n <> "' takes the " <> T.pack (show (length (miParams mi))) <> " positional parameters the class gives it"))
        | otherwise -> pure ()
      _ -> pure ()
      where
        lookup' n = case f of
          DInstance _ qual (Spanned _ c) _ _ -> do
            ck <- either (const Nothing) Just (instanceClass ctx path qual c)
            ci <- Map.lookup ck (clsClasses (ctxEnv ctx))
            lookup n (ciMethods ci)
          _ -> Nothing

-- Specialization (spec 4.6) ---------------------------------------------------

-- | Make every declaration that is not generic refer, wherever it
-- refers to a generic one at concrete types, to that declaration
-- specialized at them, making the specializations it needs.
specializeAll :: Ctx -> TC ()
specializeAll ctx = do
  decls <- gets stDecls
  mapM_ (\(k, cd) -> unless (isTemplate cd) (rewriteDecl ctx 0 k cd)) (Map.toList decls)

-- | The entry module's public declarations with a class bound, at each
-- choice of instances for their bounded type parameters: what the CLI
-- may invoke them at (spec 11.2).
specializeEntry :: Ctx -> [LoadedModule] -> TC ()
specializeEntry ctx lms =
  sequence_
    [ do
        r <- tryDemandDecl ctx key
        case r of
          Just cd
            | isTemplate cd ->
                sequence_
                  [ () <$ recoverTC (ensureSpec ctx 0 key inst)
                  | inst <- take 64 (choices [(v, candidates b) | (v, b) <- Map.toList (cdBounds cd), dictBound b])
                  ]
          _ -> pure ()
    | lm <- lms,
      Decl _ df <- moduleDecls (lmModule lm),
      n <- case df of
        DFunction n' _ _ _ _ _ -> [n']
        DValue n' _ _ _ -> [n']
        _ -> [],
      n `Set.notMember` moduleInternal (lmModule lm),
      let key = (lmPath lm, n)
    ]
  where
    heads ck = Map.findWithDefault [] ck (instanceHeads ctx)
    -- The types the CLI may instantiate a variable with the bound at:
    -- those with an instance of each of its classes, orderable or not.
    candidates b =
      [ t
      | t <- nub (concat [partHeads p' | p' <- boundParts b]),
        all (\p' -> boundHolds ctx (progEntry (ctxProg ctx)) p' t) (boundParts b)
      ]
    partHeads p' = case p' of
      BoundClass c -> heads (classKey c)
      BoundNamed BOrderable -> [TyNumber, TyString] <> heads orderableKey
      _ -> []
    choices [] = [[]]
    choices ((v, ts) : rest) = [(v, t) : more | t <- ts, more <- choices rest]

-- | Rewrite a declaration's references to generic declarations.
rewriteDecl :: Ctx -> Int -> Key -> CoreDecl -> TC ()
rewriteDecl ctx depth key cd = do
  let refs = nub [(p, n, inst) | CVar (InstRef p n inst) <- map coreF (universeCore (cdCore cd))]
  targets <-
    mapM
      ( \(p, n, inst) ->
          (,) (p, n, inst)
            <$> if p == builtinClassModule
              then resolveBuiltinOp ctx depth n inst
              else uncurry TopRef <$> resolveInst ctx depth (p, n) inst
      )
      refs
  let replace c@(Core sp cf) = case cf of
        CVar (InstRef p n inst) | Just r <- lookup (p, n, inst) targets -> Core sp (CVar r)
        _ -> c
  modify (\s -> s {stDecls = Map.insert key cd {cdCore = transformCore replace (cdCore cd)} (stDecls s)})

-- | The declaration a reference to a generic declaration at concrete
-- types runs: a method's, that of the instance for the type, or its
-- default specialized; any other's, itself specialized.
resolveInst :: Ctx -> Int -> Key -> [(Text, Type)] -> TC Key
resolveInst ctx depth key@(p, n) inst
  | not (all (isGround . snd) inst) =
      abort (diag ETypeBound NoSpan ("internal: '" <> n <> "' is referred to at a type that is not concrete"))
  | Nothing <- methodDeclAst ctx key = do
      -- A declaration that orders nothing by an instance runs as it is.
      cd <- demandDecl ctx key
      if isTemplate cd then ensureSpec ctx depth key inst else pure key
  | Just (True, _) <- methodDeclAst ctx key,
    Just (ck, ci) <- classOfMethod,
    [(_, t)] <- inst =
      case instanceMatch ctx Nothing ck t of
        Nothing -> abort (diag ETypeBound NoSpan ("there is no instance " <> snd ck <> "<" <> renderType t <> ">"))
        Just (ii, sub)
          | n `elem` [m | Decl _ (DFunction m _ _ _ _ _) <- iiDecls ii] -> instanceMethod ctx depth ii sub n
          | isJust (lookup n (ciMethods ci) >>= miDefault) -> ensureSpec ctx depth (p, n <> "#default") inst
          | otherwise -> abort (diag ETypeInstance (iiSpan ii) ("the instance does not define '" <> n <> "'"))
  | otherwise = ensureSpec ctx depth key inst
  where
    classOfMethod =
      listToMaybe
        [ (ck, ci)
        | (ck@(cp, _), ci) <- Map.toList (clsClasses (ctxEnv ctx)),
          cp == p,
          isJust (lookup n (ciMethods ci))
        ]

-- | The key of a generic declaration specialized at concrete types,
-- making the specialization the first time it is asked for.
ensureSpec :: Ctx -> Int -> Key -> [(Text, Type)] -> TC Key
ensureSpec ctx depth key@(p, n) inst = do
  let key' = (p, specName n inst)
  done <- gets (Map.member key' . stDecls)
  if done
    then pure key'
    else do
      cd <- demandDecl ctx key
      when (depth > 32) $
        abort (diag ETypeBound (coreSpan (cdCore cd)) ("'" <> n <> "' calls itself, through its instances, at ever larger types, so it is never specialized at all of them; call it at the types it is given"))
      let vs = map fst inst
          sub = applySubst (Map.fromList inst)
          -- A recursive call refers to the declaration itself, at the
          -- types it is specialized at (spec 4.3: a recursive call is
          -- not generalized).
          self c@(Core sp cf) = case cf of
            CVar (TopRef p' n') | (p', n') == key -> Core sp (CVar (TopRef p (specName n inst)))
            _ -> c
          cd' =
            cd
              { cdTypeVars = filter (`notElem` vs) (cdTypeVars cd),
                cdType = sub (cdType cd),
                cdCore = transformCore self (substCoreTypes inst (cdCore cd)),
                cdParams = fmap (substParams sub) (cdParams cd),
                cdBounds = Map.withoutKeys (cdBounds cd) (Set.fromList vs)
              }
      -- Recorded before its references are followed, so that one that
      -- comes back to it finds it.
      modify (\s -> s {stDecls = Map.insert key' cd' (stDecls s)})
      rewriteDecl ctx (depth + 1) key' cd'
      pure key'
  where
    substParams sub sp =
      sp
        { spPositional = [(x, sub t) | (x, t) <- spPositional sp],
          spVariadic = fmap (fmap sub) (spVariadic sp),
          spKeywords = [(x, sub t) | (x, t) <- spKeywords sp]
        }

-- | A core expression and everything in it.
universeCore :: Core -> [Core]
universeCore c = c : concatMap universeCore (coreChildren c)

-- | A built-in order operation -- @sort@, @sort_by@, @compare@ or an
-- ordering operator -- at a concrete type (spec 4.6): the built-in
-- itself for a type the language orders, and otherwise the operation
-- ordering by the @compare@ of the type's instance.
resolveBuiltinOp :: Ctx -> Int -> Text -> [(Text, Type)] -> TC VarRef
resolveBuiltinOp ctx depth op inst = case inst of
  [(_, t)]
    | nativeOrderable t -> case op of
        _ | op `elem` ["sort", "sort_by", "compare"] -> pure (BuiltinRef op)
        _ -> synthesize builtinClassModule t (\a b -> Core NoSpan (CBin (primOf op) a b))
    | Just (ii, sub) <- instanceMatch ctx Nothing orderableKey t -> do
        cmpKey <- instanceMethod ctx depth ii sub "compare"
        let cmp = Core NoSpan (CVar (uncurry TopRef cmpKey))
            call f args = Core NoSpan (CApp f args [])
        case op of
          "compare" -> pure (uncurry TopRef cmpKey)
          "sort" -> synthesizeWith (iiModule ii) ["xs"] (TyFun [TyArray t] (TyArray t)) (\args -> call (builtin "%sort_with") (args <> [cmp]))
          "sort_by" -> synthesizeWith (iiModule ii) ["xs", "key"] (TyFun [TyArray TyAny, TyFun [TyAny] t] (TyArray TyAny)) (\args -> call (builtin "%sort_by_with") (args <> [cmp]))
          _ -> synthesize (iiModule ii) t (\a b -> Core NoSpan (CBin (primOf op) (call cmp [a, b]) (Core NoSpan (CNumber 0))))
    | otherwise -> abort (diag ETypeBound NoSpan ("there is no instance orderable<" <> renderType t <> ">"))
  _ -> abort (diag ETypeBound NoSpan ("internal: '" <> op <> "' is ordered at no one type"))
  where
    builtin b = Core NoSpan (CVar (BuiltinRef b))
    local x = Core NoSpan (CVar (LocalRef x))
    primOf o = case o of
      "<" -> PLt
      "<=" -> PLe
      ">" -> PGt
      _ -> PGe
    synthesize :: FilePath -> Type -> (Core -> Core -> Core) -> TC VarRef
    synthesize path t mk =
      synthesizeWith path ["a", "b"] (TyFun [t, t] TyBool) (\args -> case args of
        [a, b] -> mk a b
        _ -> Core NoSpan CNull)
    -- A declaration made for the operation at the type, under a name
    -- no source can write, once.
    synthesizeWith :: FilePath -> [Text] -> Type -> ([Core] -> Core) -> TC VarRef
    synthesizeWith path ps ty mk = do
      let t = maybe TyAny snd (listToMaybe inst)
          key = (path, "%" <> op <> "<" <> renderType t <> ">")
          params = case ty of
            TyFun pts _ -> StaticParams (zip ps pts) Nothing [] []
            _ -> StaticParams [] Nothing [] []
          lam = Lam op path ps Nothing [] (mk (map local ps)) ty
      done <- gets (Map.member key . stDecls)
      unless done $
        modify (\s -> s {stDecls = Map.insert key (CoreDecl path (snd key) [] ty (Core NoSpan (CLam lam)) (Just params) Map.empty) (stDecls s)})
      pure (uncurry TopRef key)

-- | The declaration a method of an instance runs as, at the types its
-- type parameters take: the method as written, or, for a parametric
-- instance whose method orders or calls by them, specialized there.
instanceMethod :: Ctx -> Int -> InstanceInfo -> Map Text Type -> Text -> TC Key
instanceMethod ctx depth ii sub n = do
  let key = implKey ii n
  cd <- demandDecl ctx key
  if isTemplate cd
    then ensureSpec ctx depth key [(v, t) | (v, Just b) <- iiParams ii, dictBound b, Just t <- [Map.lookup v sub]]
    else pure key

-- | The types each class has an instance for that the CLI can name
-- (spec 11.2): those of the instances that are not parametric, and
-- each a parametric one gives with one of those, a built-in base type
-- or Number or String put in for each of its type parameters -- one
-- level deep, since a parametric instance gives types without end.
instanceHeads :: Ctx -> Map Key [Type]
instanceHeads ctx =
  Map.map nub . Map.fromListWith (flip (<>)) $
    [(iiClass ii, [iiHead ii]) | ii <- plain]
      <> [ (iiClass ii, [t])
         | ii <- parametric,
           sub <- take 32 (choices [(v, base) | (v, _) <- iiParams ii]),
           let t = applySubst (Map.fromList sub) (iiHead ii),
           isJust (instanceMatch ctx Nothing (iiClass ii) t)
         ]
  where
    instances = clsInstances (ctxEnv ctx)
    plain = [ii | ii <- instances, null (iiParams ii)]
    parametric = [ii | ii <- instances, not (null (iiParams ii))]
    base = nub ([TyNumber, TyString, TyBool] <> map iiHead plain)
    choices [] = [[]]
    choices ((v, ts) : rest) = [(v, t) : more | t <- ts, more <- choices rest]
