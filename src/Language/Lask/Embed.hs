{-# LANGUAGE ExplicitNamespaces #-}

-- | An embedding of Lask in Haskell (#91).
--
-- __Experimental.__ This module, "Language.Lask.Embed.Do" and
-- "Language.Lask.Embed.Run" are the EDSL's API, and it may change in
-- any release. Every other module of the package is internal to lask.
--
-- > {-# LANGUAGE DataKinds, OverloadedStrings, QualifiedDo, TypeOperators #-}
-- > import Data.Text (Text)
-- > import Language.Lask.Embed
-- > import qualified Language.Lask.Embed.Do as L
-- >
-- > go :: Command
-- > go = command "go" (image "golang:1.22")
-- >
-- > -- fact(n: Number): Number = if (n == 0) { 1 } else { n * fact(n - 1) }
-- > fact :: Task '["n" ::: 'TNumber] 'TNumber
-- > fact = task "fact" $ \n -> if_ (n ==. 0) 1 (n * call fact (n - 1))
-- >
-- > -- build(--tags: String = ""): String = do {
-- > --   $ go vet ./...
-- > --   $ go build -tags=#{tags} ./...
-- > -- }
-- > build :: Task '[] 'TString
-- > build = taskWith "build" $ body <$> kw "tags" ""
-- >   where
-- >     body tags = L.do
-- >       run go "vet ./..."
-- >       run go ("build -tags=" <> tags <> " ./...")
-- >
-- > program :: Either [Text] Program
-- > program = assemble [export build, internal fact]
--
-- "Language.Lask.Embed.Run" runs a task of the program and answers what
-- it can reach before it runs.
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
-- receives is an abstract t'E', which it can only hand back to the term
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
--   CLI resolves. A Haskell function over t'E' is a macro: it is
--   inlined, has no name, and cannot recurse on a run-time value.
--
-- * Haskell types check the types of chapter 4, computed at the type
--   level ("Language.Lask.Embed.Type"): unions in canonical form, so
--   @'TString :| 'TNull@ and @'TNull :| 'TString@ are one type; records
--   with optional fields; conformance, which Lask applies implicitly and
--   the embedding asks for with 'widen'; what is comparable, data,
--   stringifiable; and type parameters with their bounds. The rest is
--   checked by 'assemble': one declaration per name, keyword arguments
--   the callee declares at their types, one environment per command
--   word, names Lask can write, type parameters that do not escape
--   their declaration, and finally Core.Lint ("Language.Lask.Core.Lint").
--
-- Literals take their type from where they are used. A value of a type
-- that conforms to the one wanted is passed with 'widen', as in
-- @call isNull (widen \"x\")@ for a @String | Null@ parameter.
--
-- A declaration with type parameters is a 'generic', checked once at the
-- parameters themselves, which are opaque in its body, and called
-- through 'inst', which checks the bound:
--
-- > -- first_or<T>(xs: Array<T>, fallback: T): T
-- > type T = 'TVar "T" 'BNone
-- > firstOr :: Generic '[ '("T", 'BNone)] '["xs" ::: 'TArray T, "fallback" ::: T] T
-- > firstOr = generic "first_or" $ \xs fallback -> if_ (isEmpty xs) fallback (at xs 0)
-- >
-- > headName :: Task '[] 'TString
-- > headName = task "head_name" $ call (inst firstOr) (list ["lask"]) "none"
--
-- The instantiation is inferred from the arguments, except where a
-- parameter appears only inside a union; there it is given with @\@@:
-- @inst \@'TString keep@.
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
-- static: keyword parameters (t'Kw') and parallel composition (t'Par').
--
-- Scope. This is a second front end to Core, not yet the one the parser
-- is built on. It covers the types of chapter 4 and the expressions of
-- the first phase of the plan on #91; not yet @case@, lambdas and
-- function values, @try@, @async@, most built-ins, environment values,
-- run options, or modules. A program
-- runs with hooks you write, which decide how a command runs (see
-- "Language.Lask.Embed.Run"); the hooks the lask CLI uses, with pinned
-- images and command logs, are not available to it, and the CLI does
-- not take a program.
--
-- Every 'task' needs a type signature: the parameters and the result
-- are read from it.
module Language.Lask.Embed
  ( -- * Types
    Ty (..),
    Field (..),
    Bound (..),
    Param (..),
    type (:|),
    Rec,
    CommandResult,
    Error,
    E,
    Fn,
    KnownTy (..),
    KnownParams,
    KnownVars,
    FieldOf,
    OrNull,
    Subst,
    SubstParams,

    -- ** Predicates
    Conforms,
    DataTy,
    WF,
    Comparable,
    Orderable,
    Stringify,
    Satisfies,
    EqTypes,
    WhenTy,
    Concrete,
    FitsRecord,

    -- * Declarations
    Task,
    task,
    taskWith,
    doc,
    call,
    callWith,
    KwArg,
    (.=),
    ValueDecl,
    value,
    ref,
    Generic,
    generic,
    inst,
    inst2,
    Declared,

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

    -- ** Literals
    true,
    false,
    null_,
    done,
    str,
    list,
    mapOf,

    -- ** Conformance and narrowing
    widen,
    cast,

    -- ** Operators
    (==.),
    (/=.),
    (<.),
    (<=.),
    (>.),
    (>=.),
    (&&.),
    (||.),
    not_,

    -- ** Records, arrays and maps
    record,
    recordAs,
    Fields (..),
    FieldVal,
    fld,
    field,
    at,
    atKey,

    -- ** Control
    if_,
    when_,
    whenDo,
    forEach,
    mapE,

    -- ** Built-in functions
    lines_,
    trim,
    getEnvOr,
    isEmpty,
    getOr,

    -- * Programs
    Export,
    export,
    internal,
    assemble,
    Program (..),
    Decl (..),
    KwInfo (..),
  )
where

import Language.Lask.Embed.Internal
