{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The implementation of "Language.Lask.Embed": the term
-- representation, reification and 'assemble'. Everything here is
-- exported for "Language.Lask.Embed.Do", "Language.Lask.Embed.Run" and
-- the tests; it is not part of the EDSL's API and changes without
-- notice. See "Language.Lask.Embed" for the design.
module Language.Lask.Embed.Internal
  ( -- * Types
    module Language.Lask.Embed.Type,
    CommandResult,
    Error,
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
    ValueDecl,
    value,
    ref,
    Generic,
    generic,
    inst,
    inst2,
    KnownVars,
    Declared (..),
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
    null_,
    done,
    str,
    widen,
    cast,
    list,
    mapOf,
    Fields (..),
    FieldVal,
    fld,
    record,
    recordAs,
    field,
    at,
    atKey,
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
    when_,
    whenDo,
    forEach,
    mapE,
    lines_,
    trim,
    getEnvOr,

    -- * Built-ins (Phase 1)
    isEmpty,
    getOr,

    -- * Programs
    Export,
    export,
    internal,
    Program (..),
    Decl (..),
    KwInfo (..),
    assemble,
    programModule,

    -- * For "Language.Lask.Embed.Do"
    Stmt,
    Statement,
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
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Lask.Core.AST
import Language.Lask.Core.Lint (lintProgram)
import Language.Lask.Core.Pretty (renderCore)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))
import Language.Lask.Embed.Type
import Language.Lask.Lexer.Token (keywordFromText)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Types as LT

-- Types ----------------------------------------------------------------------

-- | The built-in alias @CommandResult@ (spec 6.6).
type CommandResult = Rec '["code" := 'TNumber, "stdout" := 'TString, "stderr" := 'TString]

-- | The built-in alias @Error@ (spec 6.9).
type Error = Rec '["code" := 'TNumber, "message" := 'TString]

-- Terms ----------------------------------------------------------------------

-- | Untyped PHOAS terms. 'TNode' covers every Core node without a
-- binder, building it from its reified children; the smart
-- constructors below fix how many children each node has.
data Tm v
  = TmVar v
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

-- Literals take their type from where they are used: @1@ is a Number
-- wherever a Number is wanted, and unifies a type nothing else fixes.
instance (t ~ 'TNumber) => Num (E v t) where
  fromInteger n = leaf NoSpan (CNumber (fromInteger n))
  E a + E b = node2 NoSpan (CBin PAdd) a b
  E a - E b = node2 NoSpan (CBin PSub) a b
  E a * E b = node2 NoSpan (CBin PMul) a b
  abs (E a) = builtin NoSpan "abs" [a]
  -- The argument is bound once: it may run commands.
  signum (E x) = E (TLet NoSpan x (unE . sign . E . TmVar))
    where
      sign :: E v 'TNumber -> E v 'TNumber
      sign n = if_ (n >. 0) 1 (if_ (n <. 0) (-1) 0)

instance (t ~ 'TNumber) => Fractional (E v t) where
  -- A literal is kept exactly, as the parser keeps it. Only a rational
  -- with a repeating decimal, which Scientific cannot hold, is rounded
  -- to the nearest Double.
  fromRational r = leaf NoSpan . CNumber $ case fromRationalRepetend Nothing r of
    Right (exact, Nothing) -> exact
    _ -> fromFloatDigits (fromRational r :: Double)
  E a / E b = node2 NoSpan (CBin PDiv) a b

instance (t ~ 'TString) => IsString (E v t) where
  fromString = leaf NoSpan . CStrLit . T.pack

-- | Concatenation is interpolation: @a <> b@ lowers to the one @CStr@
-- that @\"#{a}#{b}\"@ does.
instance (t ~ 'TString) => Semigroup (E v t) where
  E a <> E b = node2 NoSpan (\x y -> interpolation (parts x <> parts y)) a b
    where
      parts c = case coreF c of
        CStrLit t -> [CPText t]
        CStr ps -> ps
        _ -> [CPExpr c]

instance (t ~ 'TString) => Monoid (E v t) where
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

-- | @true@ and @false@.
true, false :: E v 'TBool
true = leaf NoSpan (CBool True)
false = leaf NoSpan (CBool False)

-- | @\"#{e}\"@.
str :: forall t v. (Stringify t) => E v t -> E v 'TString
str (E a) = stringify @t `seq` node1 NoSpan (\x -> CStr [CPExpr x]) a

-- | @r.name@, or @r[\"X-Api-Key\"]@ (spec 6.8). An optional field reads
-- as @T | Null@.
field :: forall f t v. (KnownSymbol f, HasField f t) => E v t -> E v (FieldOf f t)
field (E r) = hasField @f @t `seq` node1 NoSpan (\x -> CDot x (T.pack (symbolVal (Proxy @f)))) r

-- | @xs[i]@: fails at run time when @i@ is out of range (spec 6.8).
at :: E v ('TArray t) -> E v 'TNumber -> E v t
at (E xs) (E i) = node2 NoSpan (CIndex IdxArray) xs i

-- | @m[k]@: fails at run time when the key is missing (spec 6.8).
atKey :: E v ('TMap t) -> E v 'TString -> E v t
atKey (E m) (E k) = node2 NoSpan (CIndex IdxMap) m k

-- | @==@ and @!=@ (spec 6.2): both operands comparable, and one
-- conforming to the other.
(==.), (/=.) :: forall a b v. (EqTypes a b) => E v a -> E v b -> E v 'TBool
E a ==. E b = eqTypes @a @b `seq` node2 NoSpan (CBin PEq) a b
E a /=. E b = eqTypes @a @b `seq` node2 NoSpan (CBin PNe) a b

-- | @null@.
null_ :: E v 'TNull
null_ = leaf NoSpan CNull

-- | Use a value where a type it conforms to is wanted (spec 4.4). Lask
-- does this implicitly, and the Core is the same: nothing.
widen :: forall b a v. (Conforms a b) => E v a -> E v b
widen (E x) = conforms @a @b `seq` E x

-- | @cast(e)@ at the type @t@ (spec 15.8): checked at run time, failing
-- when the value is of another kind.
cast :: forall t a v. (KnownTy t, DataTy t, Concrete t) => E v a -> E v t
cast (E x) = dataTy @t `seq` concrete @t `seq` node1 NoSpan (\c -> CCast c (tyOf @t)) x

-- | An array literal of element type @t@ (spec 4.3). Elements of a type
-- that conforms to @t@ are given with 'widen'.
list :: forall t v. (DataTy t) => [E v t] -> E v ('TArray t)
list es = dataTy @t `seq` E (TNode NoSpan CArray (map unE es))

-- | A map literal, @{\"k\": v}@ at @Map<T>@ (spec 4.3).
mapOf :: forall t v. (DataTy t) => [(Text, E v t)] -> E v ('TMap t)
mapOf kvs = dataTy @t `seq` E (TNode NoSpan (CMapLit . zip (map fst kvs)) (map (unE . snd) kvs))

-- | One field of a record literal: @fld \@\"name\" e@.
newtype FieldVal v (k :: Symbol) (t :: Ty) = FieldVal (Tm v)

-- | The field @k@ of a record literal, with its value.
fld :: forall k t v. E v t -> FieldVal v k t
fld (E e) = FieldVal e

-- | The fields of a record literal, in the order written.
data Fields v (kvs :: [(Symbol, Ty)]) where
  RNil :: Fields v '[]
  (:&) :: (KnownSymbol k) => FieldVal v k t -> Fields v kvs -> Fields v ('(k, t) ': kvs)

infixr 5 :&

fieldList :: Fields v kvs -> [(Text, Tm v)]
fieldList RNil = []
fieldList ((fv :: FieldVal v k t) :& rest) = case fv of
  FieldVal e -> (T.pack (symbolVal (Proxy @k)), e) : fieldList rest

recordTm :: Fields v kvs -> Tm v
recordTm fs = let kvs = fieldList fs in TNode NoSpan (CRecordLit . zip (map fst kvs)) (map snd kvs)

-- | A record literal with no expected type: every field it gives is
-- required, at the type of its value (spec 4.3).
record :: forall kvs v. (UniqueKeys kvs) => Fields v kvs -> E v (Rec (Required kvs))
record fs = uniqueKeys @kvs `seq` E (recordTm fs)

-- | A record literal checked against the record type @r@ (spec 4.3): it
-- may leave out optional fields, and give a value of any type that
-- conforms to its field's.
recordAs :: forall r kvs v. (UniqueKeys kvs, FitsRecord r kvs) => Fields v kvs -> E v r
recordAs fs = uniqueKeys @kvs `seq` fitsRecord @r @kvs `seq` E (recordTm fs)

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

-- | @!b@.
not_ :: E v 'TBool -> E v 'TBool
not_ (E a) = node1 NoSpan CNot a

-- | @if (c) { t } else { e }@: only the selected branch is evaluated.
-- The embedding has no @Null@ type to give an @if@ without @else@, so
-- it always takes both branches; for an effect under a condition, the
-- else branch is 'done'.
if_ :: (HasCallStack) => E v 'TBool -> E v t -> E v t -> E v t
if_ (E c) (E t) (E e) = node3 sp CIf c (TBlock sp t) (TBlock sp e)
  where
    sp = callSpan callStack

-- | @if (c) { t }@ as an expression (spec 6.4): @T | Null@, null when the
-- condition is false, or Void for a Void block.
when_ :: forall t v. (KnownTy t, WhenTy t, HasCallStack) => E v 'TBool -> E v t -> E v (OrNull t)
when_ (E c) (E t) = whenTy @t `seq` node2 sp (\x y -> CIf x y (Core sp (otherwise' (tyOf @t)))) c (TBlock sp t)
  where
    sp = callSpan callStack
    otherwise' ty = if ty == LT.TyVoid then CDo [] else CNull

-- | @if (c) { t }@ as a statement whose value is discarded (spec 6.4): the
-- block may have any type. It is a 'Stmt', which an @L.do@ block takes
-- before its last statement and nowhere else, as Lask discards a value
-- only there.
whenDo :: forall t v. (KnownTy t, HasCallStack) => E v 'TBool -> E v t -> Stmt v
whenDo (E c) (E t) = Stmt . unE $ node2 sp (\x y -> CIf x y (Core sp (otherwise' (tyOf @t)))) c (TBlock sp t)
  where
    sp = callSpan callStack
    otherwise' ty = if ty == LT.TyVoid then CDo [] else CNull

lambda1 :: forall a b v. (KnownTy a, KnownTy b) => Span -> (E v a -> E v b) -> Tm v
lambda1 sp f = TLam sp (LT.TyFun [tyOf @a] (tyOf @b)) 1 (\xs -> TBlock sp (unE (f (E (TmVar (only xs))))))
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

-- | @lines(s)@: the lines of a command's output, without a trailing
-- empty one (spec 15.3).
lines_ :: E v 'TString -> E v ('TArray 'TString)
lines_ (E s) = builtin NoSpan "lines" [s]

-- | @trim(s)@. A command's standard output keeps its trailing newline,
-- so this is how a one-line result is usually read.
trim :: E v 'TString -> E v 'TString
trim (E s) = builtin NoSpan "trim" [s]

-- | @is_empty(xs)@.
isEmpty :: E v ('TArray t) -> E v 'TBool
isEmpty (E xs) = builtin NoSpan "is_empty" [xs]

-- | @get_or(m, key, fallback)@: the value at a key, or the fallback.
getOr :: E v ('TMap t) -> E v 'TString -> E v t -> E v t
getOr (E m) (E k) (E d) = builtin NoSpan "get_or" [m, k, d]

-- | @get_env_or(name, default)@. A literal name is what @lask secrets@
-- reports a task as reading (spec 11.10).
getEnvOr :: E v 'TString -> E v 'TString -> E v 'TString
getEnvOr (E n) (E d) = builtin NoSpan "get_env_or" [n, d]

-- Binding ------------------------------------------------------------------------

-- | @x = e@ followed by the rest of a @do@ block.
bindE :: Span -> E v a -> (E v a -> E v b) -> E v b
bindE sp (E e) k = E (TBind sp e (unE . k . E . TmVar))

-- | A statement run for its effects, followed by the rest of a @do@
-- block.
thenE :: (Statement x v) => Span -> x -> E v b -> E v b
thenE sp a (E b) = E (TSeq sp (stmtTm a) b)

-- | A statement that only an @L.do@ block takes, before its last
-- statement: it has no value to give.
newtype Stmt v = Stmt (Tm v)

-- | What can stand before the last statement of an @L.do@ block: an
-- expression, whose value is discarded, or a 'Stmt'.
class Statement x v | x -> v where
  stmtTm :: x -> Tm v

instance Statement (E v a) v where stmtTm = unE

instance Statement (Stmt v) v where stmtTm (Stmt t) = t

-- Environments and commands ---------------------------------------------------------

-- | A closed environment expression (spec 6.7).
newtype Env = Env Core

-- | A registry image, @#golang:1.22@.
image :: (HasCallStack) => Text -> Env
image reference = Env (Core sp (CEnv "docker" [("image", Core sp (CStrLit reference))]))
  where
    sp = callSpan callStack

-- | @#local@.
local :: Env
local = Env (Core NoSpan (CEnv "local" []))

-- | A command word bound to its environment, as declared by
-- @command { \"go\" } on #golang:1.22@ (spec ch. 5).
data Command = Command {cmdWord :: Text, cmdEnv :: Core}

-- | Bind a command word to the environment its commands run in.
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
    check r = unE (node1 sp (\rv -> CIf (cond rv) (dot rv "stdout") (failCall rv)) (TmVar r))
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

-- | One job of a 'parallel' composition.
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
      TBind sp (unE (node1 sp CAwait (TmVar h))) $ \a -> awaitAll hs (as <> [TmVar a])
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

-- | The positional parameters of a t'Task', from its type: their names
-- and Lask types, and how to build the curried function over them.
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
  applyVars (x : xs) f = applyVars @ps xs (f (E (TmVar x)))
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
task :: forall ps r. (KnownParams ps, KnownTy r, WFResult r, HasCallStack) => Text -> (forall v. Fn v ps r) -> Task ps r
task name body = wfResult @r `seq` Task (reifyDecl @ps @r (callSpan callStack) name def)
  where
    def :: forall v. Kw v (Fn v ps r)
    def = pure (body @v)

-- | A declaration with keyword parameters, given before the positional
-- ones:
--
-- > release :: Task '["target" ::: 'TString] 'TString
-- > release = taskWith "release" $ body <$> kw "dry_run" true
-- >   where body dryRun target = ...
taskWith :: forall ps r. (KnownParams ps, KnownTy r, WFResult r, HasCallStack) => Text -> (forall v. Kw v (Fn v ps r)) -> Task ps r
taskWith name def = wfResult @r `seq` Task (reifyDecl @ps @r (callSpan callStack) name def)

-- | The summary @--help@ shows.
doc :: Text -> Task ps r -> Task ps r
doc d (Task decl) = Task decl {declDoc = Just d}

-- | A top-level value (spec 5), evaluated once, the first time it is
-- referenced: @port: Number = 8080@.
newtype ValueDecl (t :: Ty) = ValueDecl Decl

-- | Declare a value: its name, and its expression.
value :: forall t. (KnownTy t, DataTy t, HasCallStack) => Text -> (forall v. E v t) -> ValueDecl t
value name e = dataTy @t `seq` ValueDecl (reifyValue @t (callSpan callStack) name e)

-- | A reference to a value or to a task, as an expression.
ref :: (HasCallStack) => ValueDecl t -> E v t
ref (ValueDecl d) = E (TRef (callSpan callStack) d)

-- | A declaration with type parameters (spec 4.2), each with its bound:
--
-- > -- first_or<T>(xs: Array<T>, fallback: T): T
-- > type T = 'TVar "T" 'BNone
-- > firstOr :: Generic '[ '("T", 'BNone)] '["xs" ::: 'TArray T, "fallback" ::: T] T
-- > firstOr = generic "first_or" $ \xs fallback -> if_ (isEmpty xs) fallback (at xs 0)
--
-- The body is checked once, at the type parameters themselves: inside
-- it a @T@ is opaque, and has what its bound gives it and nothing else
-- (spec 4.4). It is reified once.
newtype Generic (vs :: [(Symbol, Bound)]) (ps :: [Param]) (r :: Ty) = Generic Decl

-- | Declare a generic: its name, and its body, a curried function of
-- its positional parameters at the type parameters.
generic :: forall vs ps r. (KnownVars vs, KnownParams ps, KnownTy r, WFResult r, GenericVars vs ps r, HasCallStack) => Text -> (forall v. Fn v ps r) -> Generic vs ps r
generic name body = wfResult @r `seq` genericVars @vs @ps @r `seq` Generic d {declCore = (declCore d) {cdTypeVars = map fst vars, cdBounds = Map.fromList [(n, b) | (n, Just b) <- vars]}}
  where
    vars = varsOf @vs
    d = reifyDecl @ps @r (callSpan callStack) name def
    def :: forall v. Kw v (Fn v ps r)
    def = pure (body @v)

-- | A generic declaration at an instantiation of its one parameter. The
-- instantiation must satisfy the parameter's bound (@E-TYPE-BOUND@), and
-- is usually inferred from the arguments of the call.
inst :: forall t n b ps r. (Satisfies n b t) => Generic '[ '(n, b)] ps r -> Task (SubstParams n t ps) (Subst n t r)
inst (Generic d) = satisfies @n @b @t `seq` Task d

-- | 'inst' for two parameters.
inst2 ::
  forall t1 t2 n1 b1 n2 b2 ps r.
  (Satisfies n1 b1 t1, Satisfies n2 b2 t2) =>
  Generic '[ '(n1, b1), '(n2, b2)] ps r ->
  Task (SubstParamsL '[ '(n1, t1), '(n2, t2)] ps) (SubstL '[ '(n1, t1), '(n2, t2)] r)
inst2 (Generic d) = satisfies @n1 @b1 @t1 `seq` satisfies @n2 @b2 @t2 `seq` Task d

-- | The type parameters of a generic declaration, and their bounds.
class KnownVars (vs :: [(Symbol, Bound)]) where
  varsOf :: [(Text, Maybe LT.Bound)]

instance KnownVars '[] where varsOf = []

instance (KnownSymbol n, KnownBound b, KnownVars vs) => KnownVars ('(n, b) ': vs) where
  varsOf = (T.pack (symbolVal (Proxy @n)), boundOf @b) : varsOf @vs

-- | Anything that is a declaration: a task, a value, a generic.
class Declared d where
  declOf :: d -> Decl

instance Declared (Task ps r) where declOf (Task d) = d

instance Declared (ValueDecl t) where declOf (ValueDecl d) = d

instance Declared (Generic vs ps r) where declOf (Generic d) = d

-- | A call: a reference to the callee's name, never a copy of its body.
call :: forall ps r v. (KnownParams ps, HasCallStack) => Task ps r -> Fn v ps r
call t = callAt @ps @r @v (callSpan callStack) t []

-- | A keyword argument, @--name = value@.
data KwArg v = KwArg Text LT.Type (Tm v)

-- | @name = value@ at a call: @callWith release [\"dry_run\" .= false]@.
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
  TmVar n -> pure (Core NoSpan (CVar (LocalRef n)))
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
  TBlock sp t -> Core sp . CDo . emptied <$> reifyStmts t
    where
      -- A block that is just 'done' is the empty block @{ }@, as Lask
      -- writes it: no statements, rather than one empty block.
      emptied [CSExpr (Core _ (CDo []))] = []
      emptied stmts = stmts
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
    body = applyVars @ps @Text @r (map fst pos) (build [TmVar (ksName s) | s <- specs])
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

reifyValue :: forall t. (KnownTy t) => Span -> Text -> (forall v. E v t) -> Decl
reifyValue sp name e =
  Decl
    { declKey = (programModule, name),
      declDoc = Nothing,
      declCore =
        CoreDecl
          { cdModule = programModule,
            cdName = name,
            cdTypeVars = [],
            cdType = tyOf @t,
            cdCore = core,
            cdParams = Nothing,
            cdBounds = Map.empty
          },
      declKeywords = [],
      declDeps = reverse (rDeps st),
      declCommands = rCmds st,
      declKeywordUses = rKwUses st
    }
  where
    file = case sp of
      Span p _ -> fileName p
      NoSpan -> programModule
    (core, st) = runState (reifyTm (unE (e @Text))) (R 0 [] [] [] file)

-- Programs ----------------------------------------------------------------------------

-- | The module key every declaration of a program shares. A program is
-- one namespace, as one Lask module is, whichever Haskell modules its
-- tasks are written in; spans still name the Haskell file.
programModule :: FilePath
programModule = "main"

-- | A task as 'assemble' takes it: exported or 'internal'.
data Export = Export Bool Decl

-- | Reachable from the CLI and listed in help.
export :: (Declared d) => d -> Export
export = Export True . declOf

-- | Reachable only through other declarations, like Lask's @internal@.
internal :: (Declared d) => d -> Export
internal = Export False . declOf

-- | A program 'assemble' accepted: its Core, and every declaration its
-- exports reach.
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
assemble exports = case nameErrors <> dupErrors <> docErrors <> kwErrors <> commandErrors <> varErrors of
  -- Core.Lint is the backstop: it runs on what the checks above let
  -- through, so a problem they report is reported once.
  [] -> case lintProgram core of
    [] -> Right (Program core documented exported)
    problems -> Left problems
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

    -- A type parameter is in scope only in the declaration that binds
    -- it (spec 4.2), so one in any other declaration's types escaped
    -- from a generic body: instantiated at the parameter itself, outside.
    varErrors =
      [ "'" <> snd (declKey d) <> "' mentions the type parameter " <> v <> ", which it does not declare"
        | d <- decls,
          let cd = declCore d,
          v <- nub (concatMap LT.typeVars (cdType cd : typesIn (cdCore cd))),
          v `notElem` cdTypeVars cd
      ]
    typesIn c =
      [t | CLam lam <- map coreF (universe c), t <- [lamType lam]]
        <> [t | CCast _ t <- map coreF (universe c)]
        <> [t | CIsType _ t <- map coreF (universe c)]
    universe c = c : concatMap universe (coreChildren c)

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

-- | A @lower_id@ (spec 3.2) that is not a reserved word.
validName :: Text -> Bool
validName t = case T.uncons t of
  Just (c, rest) ->
    (isAsciiLower c || c == '_')
      && T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_') rest
      && not (isJust (keywordFromText t))
      && t `notElem` ["true", "false", "null"]
  Nothing -> False
