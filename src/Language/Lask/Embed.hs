{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An embedding of Lask in Haskell (#91). Experimental: the API may
-- change in any release.
--
-- A program written with this module reifies to the same Core
-- ("Language.Lask.Core.AST") the elaborator produces from @.lask@
-- source, so everything that works on Core works on it unchanged: the
-- reachability analyses behind @lask envs@ and @lask secrets@, help,
-- and the evaluator.
--
-- Terms are parametric higher-order abstract syntax (PHOAS). A binder
-- is a Haskell function, so code reads like monadic code under
-- @QualifiedDo@ ("Language.Lask.Embed.Do"); but what the function
-- receives is an abstract 'E', which it can only hand back to the term
-- language, never inspect. Instantiating the variable type with names
-- therefore reifies the whole body without running anything: every
-- command, environment and call is visible before execution, however
-- much the program depends on run-time values.
--
-- Three rules follow from the representation:
--
-- * A result is shared only when it is bound with @<-@ in an @L.do@
--   block. A Haskell @let@ or @where@ copies the term, so a command in
--   it runs once per use. This never depends on what GHC optimises.
--
-- * A 'task' is a named declaration, and 'call' refers to it by name.
--   That is what lets a task recurse on a run-time value, and what the
--   CLI resolves. A Haskell function over 'E' is a macro: it is
--   inlined, has no name, and cannot recurse on a run-time value.
--
-- * Haskell types check what they check cheaply: arity, positional
--   argument and result types, record fields, which types can be
--   compared or interpolated, and that @Void@ is only returned. The rest
--   is checked by 'assemble': one declaration per name, keyword
--   arguments the callee declares at their types, one environment per
--   command word, and names Lask can write.
--
-- Why not an applicative or selective functor, which would track
-- effects statically by construction? Because Lask is monadic: a
-- command's arguments, and which commands run at all, may depend on
-- what an earlier command printed. An applicative cannot express that,
-- and a selective functor only for a fixed set of branches. What Lask
-- needs before running is the program itself, and an over-approximation
-- of what it can reach is what its analyses already compute; PHOAS
-- gives the whole term, under every binder, without restricting what
-- can be written. Applicatives are used where the structure really is
-- static: keyword parameters ('Kw') and parallel composition ('Par').
--
-- Scope. This is a second front end to Core, not yet the one the parser
-- is built on. It covers a subset of the language: no unions, optional
-- record fields, @Any@, @Map@, @null@, @case@, @try@, environment
-- values, run options, modules or polymorphic declarations. 'runTask'
-- evaluates a program with the hooks it is given, which decide where
-- commands run; the hooks that run them in Docker are still built
-- inside the CLI, and the CLI (@lask run@, @--help@, @envs@) does not
-- take a program yet. Today a program can be analysed, and run against
-- hooks of your own, such as a scripted runner in tests.
--
-- Every 'task' needs a type signature: the parameters and the result
-- are read from it.
module Language.Lask.Embed
  ( -- * Types
    Ty (..),
    Param (..),
    CommandResult,
    KnownTy (..),
    E,
    Fn,
    KnownParams,

    -- * Declarations
    Task,
    task,
    taskWith,
    doc,
    call,
    callWith,
    KwArg,
    (.=),

    -- * Keyword parameters
    Kw,
    kw,
    help,

    -- * Parallel composition
    Par,
    par,
    parallel,

    -- * Environments and commands
    Env,
    image,
    local,
    Command,
    command,
    run,
    runAll,

    -- * Expressions
    true,
    false,
    done,
    Stringify,
    str,
    field,
    Comparable,
    DataTy,
    (==.),
    (/=.),
    (<.),
    (<=.),
    (>.),
    (>=.),
    (&&.),
    (||.),
    not_,
    if_,
    forEach,
    mapE,
    lines_,
    trim,
    getEnvOr,

    -- * Programs
    Export,
    export,
    internal,
    Program (..),
    Decl (..),
    KwInfo (..),
    assemble,
    programModule,

    -- * Running
    runTask,

    -- * For "Language.Lask.Embed.Do"
    bindE,
    thenE,
    callSpan,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Kind (Type)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (fromFloatDigits, fromRationalRepetend)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack)
import GHC.TypeError (Unsatisfiable, unsatisfiable)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Language.Lask.Core.AST
import Language.Lask.Core.Pretty (renderCore)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.Builtins.Impl (RtHooks)
import Language.Lask.Lexer.Token (keywordFromText)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value (Value)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Types as LT

-- Types ----------------------------------------------------------------------

-- | The Lask types the embedding covers (spec 4.1), used promoted.
-- Unions, optional record fields and @Any@ are not covered yet.
data Ty
  = TNumber
  | TString
  | TBool
  | TVoid
  | TArray Ty
  | -- | Fields by name, all required.
    TRecord [(Symbol, Ty)]

-- | A positional parameter, as in @fact(n: Number)@:
-- @'[\"n\" ::: 'TNumber]@.
data Param = Symbol ::: Ty

infix 6 :::

-- | The built-in alias @CommandResult@ (spec 6.6).
type CommandResult = 'TRecord '[ '("code", 'TNumber), '("stdout", 'TString), '("stderr", 'TString)]

-- | The Lask type a promoted 'Ty' stands for.
class KnownTy (t :: Ty) where
  tyOf :: LT.Type

instance KnownTy 'TNumber where tyOf = LT.TyNumber

instance KnownTy 'TString where tyOf = LT.TyString

instance KnownTy 'TBool where tyOf = LT.TyBool

instance KnownTy 'TVoid where tyOf = LT.TyVoid

instance (KnownTy t) => KnownTy ('TArray t) where tyOf = LT.TyArray (tyOf @t)

instance (KnownFields fs) => KnownTy ('TRecord fs) where
  tyOf = LT.TyRecord (Map.fromList [(n, LT.requiredField t) | (n, t) <- fieldsOf @fs])

class KnownFields (fs :: [(Symbol, Ty)]) where
  fieldsOf :: [(Text, LT.Type)]

instance KnownFields '[] where fieldsOf = []

instance (KnownSymbol n, KnownTy t, KnownFields fs) => KnownFields ('(n, t) ': fs) where
  fieldsOf = (T.pack (symbolVal (Proxy @n)), tyOf @t) : fieldsOf @fs

type family Lookup (f :: Symbol) (fs :: [(Symbol, Ty)]) :: Ty where
  Lookup f ('(f, t) ': _) = t
  Lookup f (_ ': fs) = Lookup f fs
  Lookup f '[] = TypeError ('Text "the record has no field " ':<>: 'ShowType f)

-- The predicates below each have one method, which the operations that
-- require them force: a program compiled with -fdefer-type-errors then
-- fails where it breaks the rule, which is how the tests check them.

-- | Types whose values @==@ compares (spec 6.2).
class Comparable (t :: Ty) where
  comparable :: ()

instance Comparable 'TNumber where comparable = ()

instance Comparable 'TString where comparable = ()

instance Comparable 'TBool where comparable = ()

instance (Comparable t) => Comparable ('TArray t) where comparable = comparable @t

instance (ComparableFields fs) => Comparable ('TRecord fs) where comparable = comparableFields @fs

class ComparableFields (fs :: [(Symbol, Ty)]) where
  comparableFields :: ()

instance ComparableFields '[] where comparableFields = ()

instance (Comparable t, ComparableFields fs) => ComparableFields ('(n, t) ': fs) where
  comparableFields = comparable @t `seq` comparableFields @fs

-- | Data types (spec 4.2): everything but @Void@, which only a function
-- may return. Array elements, record fields, parameters and keyword
-- parameters must be data.
class DataTy (t :: Ty) where
  dataTy :: ()

instance DataTy 'TNumber where dataTy = ()

instance DataTy 'TString where dataTy = ()

instance DataTy 'TBool where dataTy = ()

instance (DataTy t) => DataTy ('TArray t) where dataTy = dataTy @t

instance (DataFields fs) => DataTy ('TRecord fs) where dataTy = dataFields @fs

instance (Unsatisfiable ('Text "Void is not a data type: only a task may return it")) => DataTy 'TVoid where
  dataTy = unsatisfiable

class DataFields (fs :: [(Symbol, Ty)]) where
  dataFields :: ()

instance DataFields '[] where dataFields = ()

instance (DataTy t, DataFields fs) => DataFields ('(n, t) ': fs) where
  dataFields = dataTy @t `seq` dataFields @fs

-- | Types interpolation accepts (spec 6.6, @E-TYPE-BOUND@).
class Stringify (t :: Ty) where
  stringify :: ()

instance Stringify 'TString where stringify = ()

instance Stringify 'TNumber where stringify = ()

instance Stringify 'TBool where stringify = ()

-- Terms ----------------------------------------------------------------------

-- | Untyped PHOAS terms. 'TNode' covers every Core node without a
-- binder, building it from its reified children; the smart
-- constructors below fix how many children each node has.
data Tm v
  = TVar v
  | TNode Span ([Core] -> CoreF) [Tm v]
  | -- | A reference to a declaration: a name in Core, and an edge of
    -- the call graph for 'assemble'.
    TRef Span Decl
  | -- | An anonymous lambda: its function type and how many positional
    -- parameters it binds.
    TLam Span LT.Type Int ([v] -> Tm v)
  | -- | @x = e@ and the rest of a @do@ block ('bindE').
    TBind Span (Tm v) (v -> Tm v)
  | -- | A statement and the rest of a @do@ block ('thenE').
    TSeq Span (Tm v) (Tm v)
  | -- | A @do@ block of its own: a binding and a body that stay one
    -- expression wherever they appear, as the lowering of @$ cmd@ does.
    TLet Span (Tm v) (v -> Tm v)
  | -- | A branch or loop body: the statements of a block, as the
    -- elaborator builds it. A @do@ block written there gives its
    -- statements; any other expression is a block of one.
    TBlock Span (Tm v)
  | -- | Something 'assemble' needs to know about the term inside.
    TNote Note (Tm v)

data Note
  = -- | A command word used here, recorded for @lask cmd@ (spec 11.8).
    NoteCommand Command
  | -- | A call with keyword arguments: the callee and each argument's
    -- name and type, checked by 'assemble'.
    NoteKeywords Text [(Text, LT.Type)]

-- | A Lask expression of type @t@. Abstract, so that a binder's body
-- cannot inspect the variable it is given.
newtype E v (t :: Ty) = E (Tm v)

unE :: E v t -> Tm v
unE (E t) = t

leaf :: Span -> CoreF -> E v t
leaf sp f = E (TNode sp (const f) [])

node1 :: Span -> (Core -> CoreF) -> Tm v -> E v t
node1 sp f a = E (TNode sp (\cs -> case cs of [x] -> f x; _ -> arity) [a])

node2 :: Span -> (Core -> Core -> CoreF) -> Tm v -> Tm v -> E v t
node2 sp f a b = E (TNode sp (\cs -> case cs of [x, y] -> f x y; _ -> arity) [a, b])

node3 :: Span -> (Core -> Core -> Core -> CoreF) -> Tm v -> Tm v -> Tm v -> E v t
node3 sp f a b c = E (TNode sp (\cs -> case cs of [x, y, z] -> f x y z; _ -> arity) [a, b, c])

-- | 'reifyTm' gives a node as many children as it was built with.
arity :: a
arity = error "Language.Lask.Embed: internal error: node arity"

builtin :: Span -> Text -> [Tm v] -> E v t
builtin sp name args = E (TNode sp (\cs -> CApp (Core sp (CVar (BuiltinRef name))) cs []) args)

-- | The span of the caller of an API function, so that diagnostics,
-- stack traces and command logs point into the Haskell source.
callSpan :: CallStack -> Span
callSpan cs = case getCallStack cs of
  (_, l) : _ ->
    Span
      (Position (srcLocFile l) (srcLocStartLine l) (srcLocStartCol l))
      (Position (srcLocFile l) (srcLocEndLine l) (srcLocEndCol l))
  [] -> NoSpan

-- Literals and operators ---------------------------------------------------------

instance Num (E v 'TNumber) where
  fromInteger n = leaf NoSpan (CNumber (fromInteger n))
  E a + E b = node2 NoSpan (CBin PAdd) a b
  E a - E b = node2 NoSpan (CBin PSub) a b
  E a * E b = node2 NoSpan (CBin PMul) a b
  abs (E a) = builtin NoSpan "abs" [a]
  -- The argument is bound once: it may run commands.
  signum (E x) = E (TLet NoSpan x (unE . sign . E . TVar))
    where
      sign :: E v 'TNumber -> E v 'TNumber
      sign n = if_ (n >. 0) 1 (if_ (n <. 0) (-1) 0)

instance Fractional (E v 'TNumber) where
  -- A literal is kept exactly, as the parser keeps it. Only a rational
  -- with a repeating decimal, which Scientific cannot hold, is rounded
  -- to the nearest Double.
  fromRational r = leaf NoSpan . CNumber $ case fromRationalRepetend Nothing r of
    Right (exact, Nothing) -> exact
    _ -> fromFloatDigits (fromRational r :: Double)
  E a / E b = node2 NoSpan (CBin PDiv) a b

instance IsString (E v 'TString) where
  fromString = leaf NoSpan . CStrLit . T.pack

-- | Concatenation is interpolation: @a <> b@ lowers to the one @CStr@
-- that @\"#{a}#{b}\"@ does.
instance Semigroup (E v 'TString) where
  E a <> E b = node2 NoSpan (\x y -> interpolation (parts x <> parts y)) a b
    where
      parts c = case coreF c of
        CStrLit t -> [CPText t]
        CStr ps -> ps
        _ -> [CPExpr c]

instance Monoid (E v 'TString) where
  mempty = ""

-- | A string literal stays a literal; anything else is an
-- interpolation, with adjacent text merged.
interpolation :: [CorePart] -> CoreF
interpolation ps = case merge ps of
  [] -> CStrLit ""
  [CPText t] -> CStrLit t
  merged -> CStr merged
  where
    merge (CPText s : CPText t : rest) = merge (CPText (s <> t) : rest)
    merge (CPText "" : rest) = merge rest
    merge (p : rest) = p : merge rest
    merge [] = []

-- | @{}@, the empty block: the @Void@ a task can end with.
done :: E v 'TVoid
done = leaf NoSpan (CDo [])

true, false :: E v 'TBool
true = leaf NoSpan (CBool True)
false = leaf NoSpan (CBool False)

-- | @\"#{e}\"@.
str :: forall t v. (Stringify t) => E v t -> E v 'TString
str (E a) = stringify @t `seq` node1 NoSpan (\x -> CStr [CPExpr x]) a

-- | @r.name@.
field :: forall f fs v. (KnownSymbol f) => E v ('TRecord fs) -> E v (Lookup f fs)
field (E r) = node1 NoSpan (\x -> CDot x (T.pack (symbolVal (Proxy @f)))) r

(==.), (/=.) :: forall t v. (Comparable t) => E v t -> E v t -> E v 'TBool
E a ==. E b = comparable @t `seq` node2 NoSpan (CBin PEq) a b
E a /=. E b = comparable @t `seq` node2 NoSpan (CBin PNe) a b

-- | The ordering operators, which Lask defines on @Number@ only (spec
-- 6.2).
(<.), (<=.), (>.), (>=.) :: E v 'TNumber -> E v 'TNumber -> E v 'TBool
E a <. E b = node2 NoSpan (CBin PLt) a b
E a <=. E b = node2 NoSpan (CBin PLe) a b
E a >. E b = node2 NoSpan (CBin PGt) a b
E a >=. E b = node2 NoSpan (CBin PGe) a b

infix 4 ==., /=., <., <=., >., >=.

-- | Short-circuiting, as in Lask.
(&&.), (||.) :: E v 'TBool -> E v 'TBool -> E v 'TBool
E a &&. E b = node2 NoSpan CAnd a b
E a ||. E b = node2 NoSpan COr a b

infixr 3 &&.

infixr 2 ||.

not_ :: E v 'TBool -> E v 'TBool
not_ (E a) = node1 NoSpan CNot a

-- | @if (c) { t } else { e }@: only the selected branch is evaluated.
-- Lask has no @if@ without @else@, and neither does the embedding.
if_ :: (HasCallStack) => E v 'TBool -> E v t -> E v t -> E v t
if_ (E c) (E t) (E e) = node3 sp CIf c (TBlock sp t) (TBlock sp e)
  where
    sp = callSpan callStack

lambda1 :: forall a b v. (KnownTy a, KnownTy b) => Span -> (E v a -> E v b) -> Tm v
lambda1 sp f = TLam sp (LT.TyFun [tyOf @a] (tyOf @b)) 1 (\xs -> TBlock sp (unE (f (E (TVar (only xs))))))
  where
    only [x] = x
    only _ = arity

-- | @for (x : xs) { body }@ run for its effects.
forEach :: forall a b v. (KnownTy a, KnownTy b, HasCallStack) => E v ('TArray a) -> (E v a -> E v b) -> E v 'TVoid
forEach (E xs) f = builtin sp "for_each" [xs, lambda1 @a @b sp f]
  where
    sp = callSpan callStack

-- | @for (x : xs) { body }@ collecting the results. A body that
-- returns @Void@ is a 'forEach', as Lask's @for@ is (spec 6.4).
mapE :: forall a b v. (KnownTy a, KnownTy b, DataTy b, HasCallStack) => E v ('TArray a) -> (E v a -> E v b) -> E v ('TArray b)
mapE (E xs) f = dataTy @b `seq` builtin sp "map" [xs, lambda1 @a @b sp f]
  where
    sp = callSpan callStack

lines_ :: E v 'TString -> E v ('TArray 'TString)
lines_ (E s) = builtin NoSpan "lines" [s]

trim :: E v 'TString -> E v 'TString
trim (E s) = builtin NoSpan "trim" [s]

-- | @get_env_or(name, default)@. A literal name is what @lask secrets@
-- reports a task as reading (spec 11.10).
getEnvOr :: E v 'TString -> E v 'TString -> E v 'TString
getEnvOr (E n) (E d) = builtin NoSpan "get_env_or" [n, d]

-- Binding ------------------------------------------------------------------------

-- | @x = e@ followed by the rest of a @do@ block.
bindE :: Span -> E v a -> (E v a -> E v b) -> E v b
bindE sp (E e) k = E (TBind sp e (unE . k . E . TVar))

-- | A statement run for its effects, followed by the rest of a @do@
-- block.
thenE :: Span -> E v a -> E v b -> E v b
thenE sp (E a) (E b) = E (TSeq sp a b)

-- Environments and commands ---------------------------------------------------------

-- | A closed environment expression (spec 6.7).
newtype Env = Env Core

-- | A registry image, @#golang:1.22@.
image :: (HasCallStack) => Text -> Env
image ref = Env (Core sp (CEnv "docker" [("image", Core sp (CStrLit ref))]))
  where
    sp = callSpan callStack

-- | @#local@.
local :: Env
local = Env (Core NoSpan (CEnv "local" []))

-- | A command word bound to its environment, as declared by
-- @command { \"go\" } on #golang:1.22@ (spec ch. 5).
data Command = Command {cmdWord :: Text, cmdEnv :: Core}

command :: Text -> Env -> Command
command w (Env e) = Command w e

-- | @$* go args@: the whole 'CommandResult', whatever the exit code.
runAll :: (HasCallStack) => Command -> E v 'TString -> E v CommandResult
runAll c = rawRun (callSpan callStack) c

-- | @$ go args@: standard output, failing on a non-zero exit code. It
-- lowers exactly as the elaborator lowers @$ go args@ (spec 6.6).
run :: (HasCallStack) => Command -> E v 'TString -> E v 'TString
run c args = E (TLet sp (unE (rawRun sp c args)) check)
  where
    sp = callSpan callStack
    check r = unE (node1 sp (\rv -> CIf (cond rv) (dot rv "stdout") (failCall rv)) (TVar r))
    dot rv f = Core sp (CDot rv f)
    cond rv = Core sp (CBin PEq (dot rv "code") (Core sp (CNumber 0)))
    failCall rv =
      Core sp $
        CApp
          (Core sp (CVar (BuiltinRef "%commandFail")))
          [Core sp (CRecordLit [("code", dot rv "code"), ("message", dot rv "stderr")])]
          []

rawRun :: Span -> Command -> E v 'TString -> E v CommandResult
rawRun sp c (E args) =
  E . TNote (NoteCommand c) . unE $
    node1 sp (\s -> CApp (Core sp (CVar (BuiltinRef "run"))) [cmdEnv c, Core sp (commandString s)] []) args
  where
    -- The word, then the arguments after a space; @run go ""@ is the
    -- command @go@, as @$ go@ is.
    commandString s = case coreF s of
      CStrLit "" -> CStrLit (cmdWord c)
      CStrLit t -> CStrLit (cmdWord c <> " " <> t)
      CStr ps -> interpolation (CPText (cmdWord c <> " ") : ps)
      _ -> interpolation [CPText (cmdWord c <> " "), CPExpr s]

-- Parallel composition ----------------------------------------------------------------

-- | Computations to run concurrently with 'parallel'. The 'Applicative'
-- interface is what makes them independent: no job sees another's
-- result.
data Par v a = Par [(LT.Type, Tm v)] ([Tm v] -> a)

instance Functor (Par v) where
  fmap f (Par js k) = Par js (f . k)

instance Applicative (Par v) where
  pure a = Par [] (const a)
  Par js1 f <*> Par js2 g = Par (js1 <> js2) $ \vs ->
    let (a, b) = splitAt (length js1) vs in f a (g b)

par :: forall t v. (KnownTy t) => E v t -> Par v (E v t)
par (E e) = Par [(tyOf @t, e)] (\vs -> case vs of [x] -> E x; _ -> arity)

-- | @async@ for every job, then @await@ for each in order, then the
-- result (spec 6.3).
parallel :: (HasCallStack) => Par v (E v r) -> E v r
parallel (Par jobs k) = E (spawnAll jobs [])
  where
    sp = callSpan callStack
    spawnAll ((t, j) : js) hs =
      TBind sp (unE (builtin sp "spawn" [TLam sp (LT.TyFun [] t) 0 (const j)])) $ \h ->
        spawnAll js (hs <> [h])
    spawnAll [] hs = awaitAll hs []
    awaitAll (h : hs) as =
      TBind sp (unE (node1 sp CAwait (TVar h))) $ \a -> awaitAll hs (as <> [TVar a])
    awaitAll [] as = unE (k as)

-- Keyword parameters ----------------------------------------------------------------

data KwSpec v = KwSpec {ksName :: Text, ksType :: LT.Type, ksDefault :: Tm v, ksHelp :: Maybe Text}

-- | Keyword parameters with their defaults and docs. As an applicative
-- its structure is static: names, types, defaults and docs are known
-- without running the task.
data Kw v a = Kw [KwSpec v] ([Tm v] -> a)

instance Functor (Kw v) where
  fmap f (Kw s k) = Kw s (f . k)

instance Applicative (Kw v) where
  pure a = Kw [] (const a)
  Kw s1 f <*> Kw s2 g = Kw (s1 <> s2) $ \vs ->
    let (a, b) = splitAt (length s1) vs in f a (g b)

-- | @--name: T = default@.
kw :: forall t v. (KnownTy t, DataTy t) => Text -> E v t -> Kw v (E v t)
kw name (E d) = dataTy @t `seq` Kw [KwSpec name (tyOf @t) d Nothing] (\vs -> case vs of [x] -> E x; _ -> arity)

-- | The description @--help@ shows for the keyword parameters given.
help :: Text -> Kw v a -> Kw v a
help h (Kw s k) = Kw [x {ksHelp = Just h} | x <- s] k

-- Declarations ------------------------------------------------------------------------

-- | Positional parameters as a curried Haskell function.
type family Fn v (ps :: [Param]) (r :: Ty) :: Type where
  Fn v '[] r = E v r
  Fn v ((n ::: t) ': ps) r = E v t -> Fn v ps r

class KnownParams (ps :: [Param]) where
  paramsOf :: [(Text, LT.Type)]
  applyVars :: [v] -> Fn v ps r -> E v r
  collect :: ([Tm v] -> E v r) -> Fn v ps r

instance KnownParams '[] where
  paramsOf = []
  applyVars _ e = e
  collect k = k []

instance (KnownSymbol n, KnownTy t, DataTy t, KnownParams ps) => KnownParams ((n ::: t) ': ps) where
  paramsOf = dataTy @t `seq` (T.pack (symbolVal (Proxy @n)), tyOf @t) : paramsOf @ps
  applyVars (x : xs) f = applyVars @ps xs (f (E (TVar x)))
  applyVars [] _ = arity
  collect k = \(E a) -> collect @ps (\as -> k (a : as))

-- | A keyword parameter as 'assemble' and help see it.
data KwInfo = KwInfo {kiName :: Text, kiType :: LT.Type, kiDefault :: Core, kiHelp :: Maybe Text}

-- | A reified declaration. Its key is computed without touching the
-- body, so referring to a declaration never forces it: that is what
-- keeps Haskell-level recursion finite.
data Decl = Decl
  { declKey :: (FilePath, Text),
    declDoc :: Maybe Text,
    declCore :: CoreDecl,
    declKeywords :: [KwInfo],
    declDeps :: [Decl],
    declCommands :: [(Text, Core)],
    -- | Calls with keyword arguments: the callee, and each argument's
    -- name and type.
    declKeywordUses :: [(Text, [(Text, LT.Type)])]
  }

-- | A task with positional parameters @ps@ returning @r@. Keyword
-- parameters are not part of its type, as they are not part of a Lask
-- function type (spec 4.4).
newtype Task (ps :: [Param]) (r :: Ty) = Task Decl

-- | A declaration without keyword parameters:
--
-- > fact :: Task '["n" ::: 'TNumber] 'TNumber
-- > fact = task "fact" $ \n -> if_ (n ==. 0) 1 (n * call fact (n - 1))
task :: forall ps r. (KnownParams ps, KnownTy r, HasCallStack) => Text -> (forall v. Fn v ps r) -> Task ps r
task name body = Task (reifyDecl @ps @r (callSpan callStack) name def)
  where
    def :: forall v. Kw v (Fn v ps r)
    def = pure (body @v)

-- | A declaration with keyword parameters, given before the positional
-- ones:
--
-- > release :: Task '["target" ::: 'TString] 'TString
-- > release = taskWith "release" $ body <$> kw "dry_run" true
-- >   where body dryRun target = ...
taskWith :: forall ps r. (KnownParams ps, KnownTy r, HasCallStack) => Text -> (forall v. Kw v (Fn v ps r)) -> Task ps r
taskWith name def = Task (reifyDecl @ps @r (callSpan callStack) name def)

-- | The summary @--help@ shows.
doc :: Text -> Task ps r -> Task ps r
doc d (Task decl) = Task decl {declDoc = Just d}

-- | A call: a reference to the callee's name, never a copy of its body.
call :: forall ps r v. (KnownParams ps, HasCallStack) => Task ps r -> Fn v ps r
call t = callAt @ps @r @v (callSpan callStack) t []

-- | A keyword argument, @--name = value@.
data KwArg v = KwArg Text LT.Type (Tm v)

(.=) :: forall t v. (KnownTy t) => Text -> E v t -> KwArg v
n .= E e = KwArg n (tyOf @t) e

infix 1 .=

-- | A call with keyword arguments. 'assemble' checks that the callee
-- declares each of them, at its type, once.
callWith :: forall ps r v. (KnownParams ps, HasCallStack) => Task ps r -> [KwArg v] -> Fn v ps r
callWith t kws = callAt @ps @r @v (callSpan callStack) t kws

callAt :: forall ps r v. (KnownParams ps) => Span -> Task ps r -> [KwArg v] -> Fn v ps r
callAt sp (Task d) kws = collect @ps @v @r $ \args ->
  E . note $
    TNode
      sp
      ( \cs -> case cs of
          f : rest ->
            let (pos, kwVals) = splitAt (length args) rest
             in CApp f pos (zip [n | KwArg n _ _ <- kws] kwVals)
          [] -> arity
      )
      (TRef sp d : args <> [e | KwArg _ _ e <- kws])
  where
    note
      | null kws = id
      | otherwise = TNote (NoteKeywords (snd (declKey d)) [(n, t) | KwArg n t _ <- kws])

-- Reification ----------------------------------------------------------------------------

data R = R
  { rFresh :: Int,
    rDeps :: [Decl],
    rCmds :: [(Text, Core)],
    rKwUses :: [(Text, [(Text, LT.Type)])],
    rFile :: FilePath
  }

-- | Names Lask code cannot write, so they never capture a parameter.
fresh :: State R Text
fresh = do
  n <- gets rFresh
  modify (\r -> r {rFresh = n + 1})
  pure ("%" <> T.pack (show n))

reifyTm :: Tm Text -> State R Core
reifyTm tm = case tm of
  TVar n -> pure (Core NoSpan (CVar (LocalRef n)))
  TNode sp f ts -> Core sp . f <$> mapM reifyTm ts
  TRef sp d -> do
    modify (\r -> r {rDeps = d : rDeps r})
    pure (Core sp (CVar (uncurry TopRef (declKey d))))
  TLam sp ty n body -> do
    xs <- mapM (const fresh) [1 .. n]
    b <- reifyTm (body xs)
    file <- gets rFile
    pure (Core sp (CLam (Lam (lambdaName sp) file xs Nothing [] b ty)))
  TBind sp _ _ -> Core sp . CDo <$> reifyStmts tm
  TSeq sp _ _ -> Core sp . CDo <$> reifyStmts tm
  TBlock sp t -> Core sp . CDo <$> reifyStmts t
  TLet sp e k -> do
    x <- fresh
    e' <- reifyTm e
    body <- reifyTm (k x)
    pure (Core sp (CDo [CSBind x e', CSExpr body]))
  TNote (NoteCommand c) t -> do
    modify (\r -> r {rCmds = (cmdWord c, cmdEnv c) : rCmds r})
    reifyTm t
  TNote (NoteKeywords callee kws) t -> do
    modify (\r -> r {rKwUses = (callee, kws) : rKwUses r})
    reifyTm t

-- | The statements of a block. Only what 'bindE' and 'thenE' built is
-- spliced in, so a command's own block stays one statement. A nested
-- @L.do@ in tail position is spliced too: after desugaring it cannot be
-- told from the rest of the outer block, and the result is the same.
reifyStmts :: Tm Text -> State R [CoreStmt]
reifyStmts tm = case tm of
  TBind _ e k -> do
    x <- fresh
    e' <- reifyTm e
    (CSBind x e' :) <$> reifyStmts (k x)
  TSeq _ a b -> do
    a' <- reifyTm a
    (CSExpr a' :) <$> reifyStmts b
  _ -> (: []) . CSExpr <$> reifyTm tm

lambdaName :: Span -> Text
lambdaName (Span (Position _ l c) _) = "<lambda@" <> T.pack (show l) <> ":" <> T.pack (show c) <> ">"
lambdaName NoSpan = "<lambda>"

reifyDecl :: forall ps r. (KnownParams ps, KnownTy r) => Span -> Text -> (forall v. Kw v (Fn v ps r)) -> Decl
reifyDecl sp name def =
  Decl
    { declKey = (programModule, name),
      declDoc = Nothing,
      declCore = coreDecl,
      declKeywords = [KwInfo (ksName s) (ksType s) d (ksHelp s) | (s, (_, d)) <- zip specs kwCores],
      declDeps = reverse (rDeps st),
      declCommands = rCmds st,
      declKeywordUses = rKwUses st
    }
  where
    file = case sp of
      Span p _ -> fileName p
      NoSpan -> programModule
    Kw specs build = def @Text
    pos = paramsOf @ps
    body = applyVars @ps @Text @r (map fst pos) (build [TVar (ksName s) | s <- specs])
    ((kwCores, bodyCore), st) = flip runState (R 0 [] [] [] file) $ do
      ks <- forM specs $ \s -> (,) (ksName s) <$> reifyTm (ksDefault s)
      b <- reifyTm (unE body)
      pure (ks, b)
    funTy = LT.TyFun (map snd pos) (tyOf @r)
    coreDecl =
      CoreDecl
        { cdModule = programModule,
          cdName = name,
          cdTypeVars = [],
          cdType = funTy,
          cdCore = Core sp (CLam (Lam name file (map fst pos) Nothing kwCores bodyCore funTy)),
          cdParams = Just (StaticParams pos Nothing [(ksName s, ksType s) | s <- specs]),
          cdBounds = Map.empty
        }

-- Programs ----------------------------------------------------------------------------

-- | The module key every declaration of a program shares. A program is
-- one namespace, as one Lask module is, whichever Haskell modules its
-- tasks are written in; spans still name the Haskell file.
programModule :: FilePath
programModule = "main"

data Export = Export Bool Decl

-- | Reachable from the CLI and listed in help.
export :: Task ps r -> Export
export (Task d) = Export True d

-- | Reachable only through other tasks, like Lask's @internal@.
internal :: Task ps r -> Export
internal (Task d) = Export False d

data Program = Program
  { progCore :: CoreProgram,
    -- | Every declaration reached, exports first.
    progDecls :: [Decl],
    progExports :: [Text]
  }

-- | Collect every declaration the exports reach, following calls with a
-- visited set so that recursion terminates, and check what the Haskell
-- types do not.
assemble :: [Export] -> Either [Text] Program
assemble exports = case nameErrors <> dupErrors <> docErrors <> kwErrors <> commandErrors of
  [] -> Right (Program core documented exported)
  errs -> Left errs
  where
    roots = [d | Export _ d <- exports]
    exported = [snd (declKey d) | Export True d <- exports]

    decls = go Set.empty roots
    go _ [] = []
    go seen (d : rest)
      | declKey d `Set.member` seen = go seen rest
      | otherwise = d : go (Set.insert (declKey d) seen) (declDeps d <> rest)

    byKey = Map.fromList [(declKey d, d) | d <- decls]

    nameErrors =
      concat
        [ [ "'" <> name <> "' is not a valid declaration name"
            | not (validName name)
          ]
            <> [ "'" <> name <> "': '" <> p <> "' is not a valid parameter name"
                 | p <- params,
                   not (validName p)
               ]
            <> [ "'" <> name <> "' has two parameters named '" <> p <> "'"
                 | p <- nub params,
                   length (filter (== p) params) > 1
               ]
          | d <- decls,
            let name = snd (declKey d)
                params = maybe [] (\sp -> map fst (spPositional sp) <> map fst (spKeywords sp)) (cdParams (declCore d))
        ]

    -- The same declaration reached along two paths is the same value;
    -- two declarations given one name differ somewhere in what they
    -- declare. Comparing is linear in their size, and done once per
    -- reference.
    dupErrors =
      nub
        [ "two different declarations are named '" <> snd (declKey d) <> "'"
          | d <- reached,
            Just d' <- [Map.lookup (declKey d) byKey],
            identity d /= identity d'
        ]
    -- A doc is not part of a declaration: 'doc' may be applied where a
    -- task is exported and not where it is called.
    identity d =
      ( declCore d,
        [(kiName k, kiType k, kiDefault k, kiHelp k) | k <- declKeywords d],
        declCommands d
      )
    reached = roots <> concatMap declDeps decls
    docs = Map.map nub (Map.fromListWith (flip (<>)) [(declKey d, [t]) | d <- reached, Just t <- [declDoc d]])
    docErrors =
      [ "'" <> name <> "' is given two different docs"
        | ((_, name), ts) <- Map.toList docs,
          length ts > 1
      ]
    -- Every declaration with the doc any of its references carries.
    documented = [d {declDoc = declDoc d <|> listToMaybe (Map.findWithDefault [] (declKey d) docs)} | d <- decls]

    kwErrors =
      concat
        [ [ "'" <> caller <> "' calls '" <> callee <> "' with --" <> k <> ", which '" <> callee <> "' does not declare"
            | (k, _) <- kws,
              k `notElem` map kiName declared
          ]
            <> [ "'" <> caller <> "' calls '" <> callee <> "' with --" <> k <> " of type " <> LT.renderType t
                   <> ", but it is declared " <> LT.renderType (kiType ki)
                 | (k, t) <- kws,
                   ki <- declared,
                   kiName ki == k,
                   not (t `LT.conformsTo` kiType ki)
               ]
            <> [ "'" <> caller <> "' calls '" <> callee <> "' with --" <> k <> " more than once"
                 | k <- nub (map fst kws),
                   length (filter ((== k) . fst) kws) > 1
               ]
          | d <- decls,
            let caller = snd (declKey d),
            (callee, kws) <- declKeywordUses d,
            Just target <- [Map.lookup (programModule, callee) byKey],
            let declared = declKeywords target
        ]

    -- A command word names one environment (spec ch. 5).
    commandErrors =
      [ "the command '" <> w <> "' is bound to more than one environment: " <> T.intercalate ", " envs
        | (w, envs) <- Map.toList commandEnvs,
          length envs > 1
      ]
    commandEnvs =
      Map.map nub (Map.fromListWith (flip (<>)) [(w, [renderCore 0 e]) | d <- decls, (w, e) <- declCommands d])

    core =
      CoreProgram
        { cpEntry = programModule,
          cpBaseDir = ".",
          cpDecls = Map.fromList [(declKey d, declCore d) | d <- decls],
          -- Whatever is not exported is internal, whether it was named
          -- with 'internal' or only reached through a call.
          cpInternal = Set.fromList (map (snd . declKey) decls) `Set.difference` Set.fromList exported,
          cpCommands = Map.singleton programModule (Map.fromList (concatMap declCommands decls)),
          cpCommandUses = [],
          cpHover = [],
          cpAdvisories = []
        }

-- | Run one task of a program with the given positional and keyword
-- arguments. The hooks decide where commands run; see the module
-- header for which are available. A failure is thrown as
-- 'Language.Lask.Runtime.Value.LaskFailure'.
runTask :: Program -> RtHooks -> Text -> [Value] -> [(Text, Value)] -> IO Value
runTask prog hooks name args kws = do
  ctx <- mkRtCtx (progCore prog) "" hooks
  f <- topValue ctx (programModule, name)
  applyValue ctx f args kws

-- | A @lower_id@ (spec 3.2) that is not a reserved word.
validName :: Text -> Bool
validName t = case T.uncons t of
  Just (c, rest) ->
    (isAsciiLower c || c == '_')
      && T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_') rest
      && not (isJust (keywordFromText t))
      && t `notElem` ["true", "false", "null"]
  Nothing -> False
