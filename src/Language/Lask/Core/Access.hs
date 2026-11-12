{-# LANGUAGE OverloadedStrings #-}

-- | What the uses of environments may reach past their containers,
-- read without evaluating anything (spec 10.11, 11.4), and the static
-- check of the access constraints written with @within@.
--
-- A use is a command, a filesystem function or a @with_container@
-- (10.11). Its environment is read from its expression, as
-- @envs commands@ reads it, and so are the run options that decide
-- what it reaches, where they are static. A call of a top-level
-- function is walked again with what the call gives its parameters, so
-- an environment a caller passes is resolved at that caller; what a
-- declaration walked on its own leaves to its parameters is
-- /parametric/, and is checked at each call that resolves it.
module Language.Lask.Core.Access
  ( -- * Heads
    HeadImage (..),
    headImage,
    holdsEnv,
    literalArg,
    defaultContext,

    -- * Uses
    SVal (..),
    OptVal (..),
    Via (..),
    Target (..),
    UseKind (..),
    UseSite (..),
    collectUseSites,
    listedOptions,
    targetAccess,
    targetParametric,

    -- * Secrets
    Secret (..),
    SecretKind (..),
    Flow (..),
    Carried (..),
    channelFlows,
    secretFindings,
    secretAdvisories,
    programMarksSecrets,

    -- * Constraints
    Covering (..),
    renderCovering,
    Constraints,
    withinConstraints,
    coveringsOf,
    checkWithin,
    checkInvocation,
    checkCommandEnv,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Scientific (FPFormat (Generic), formatScientific)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Access
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Builtins.Names (effectfulBuiltinNames)
import Language.Lask.Builtins.Sig (Scheme (..), builtinSchemes)
import Language.Lask.Core.AST
import Language.Lask.Diagnostic (Advisory (..), Diagnostic, mkDiagnostic, withNote)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), Key, StaticParams (..))
import Language.Lask.ErrorCode (AdvisoryCode (..), ErrorCode (..), Stage (..))
import Language.Lask.Lexer.Token (Spanned (..))
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import Language.Lask.Module.Resolve (GlobalScope (..), TypeTarget (..), buildScopes)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (evalCore, mkRtCtx)
import Language.Lask.Runtime.Image (Recipe (..))
import Language.Lask.Runtime.Value (Value (..), ioFailure)
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (Field (..), Type (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import qualified Data.Vector as V

-- Heads ---------------------------------------------------------------

-- | The image a head names (spec 6.7, 10.2): the host, a registry
-- reference for a platform ('Nothing' is the daemon's own), or a
-- recipe.
data HeadImage
  = HeadLocal
  | HeadRegistry Text (Maybe Text)
  | HeadRecipe Recipe
  deriving (Show, Eq, Ord)

headImage :: [(Text, Core)] -> Maybe HeadImage
headImage args = case (lookup "image" args, lookup "dockerfile" args) of
  (Just (Core _ (CStrLit ref)), _) -> Just (HeadRegistry ref (literalArg "platform" args))
  (_, Just (Core _ (CStrLit df))) ->
    Just . HeadRecipe $
      Recipe
        { rcDockerfile = df,
          rcContext = maybe (defaultContext df) id (literalArg "context" args),
          rcBuildArgs = case lookup "build_args" args of
            Just (Core _ (CMapLit kvs)) -> sortOn fst [(k, v) | (k, Core _ (CStrLit v)) <- kvs]
            _ -> [],
          rcPlatform = literalArg "platform" args,
          rcNetwork = literalArg "build_network" args
        }
  _ -> Nothing

-- | An image option, which is always a literal (spec 6.7).
literalArg :: Text -> [(Text, Core)] -> Maybe Text
literalArg k args = case lookup k args of
  Just (Core _ (CStrLit v)) -> Just v
  _ -> Nothing

-- | The context of a recipe that names none: the Dockerfile's
-- directory (spec 10.2).
defaultContext :: Text -> Text
defaultContext df =
  let parts = T.splitOn "/" df
   in if length parts <= 1 then "." else T.intercalate "/" (init parts)

-- | Whether a value of a type can be, or contain, an environment.
holdsEnv :: Type -> Bool
holdsEnv t = case t of
  TyAny -> True
  TyEnvironment -> True
  TyRunnable -> True
  TyVar _ -> True
  TyArray e -> holdsEnv e
  TyMap e -> holdsEnv e
  TyAsync e -> holdsEnv e
  TyRecord fs -> any (holdsEnv . fieldType) (Map.elems fs)
  TyFun _ r -> holdsEnv r
  TyUnion ts -> any holdsEnv ts
  _ -> False

-- Uses ----------------------------------------------------------------

-- | A value known without evaluating the program.
data SVal
  = SStr Text
  | SBool Bool
  | SNum Text
  | SNull
  | SArr [SVal]
  | SMap [(Text, SVal)]
  | SRec [(Text, SVal)]
  deriving (Show, Eq, Ord)

-- | A run option as the reading finds it: its static value, a value
-- only the run knows, or one a parameter of the declaration walked on
-- its own supplies, which each call resolves.
data OptVal = Known SVal | Computed | FromParam
  deriving (Show, Eq, Ord)

-- | The call that gave a use the environment or the option values it
-- has: the declaration it is written in, and where.
data Via = Via
  { viaModule :: FilePath,
    viaDecl :: Text,
    viaSpan :: Span
  }
  deriving (Show, Eq, Ord)

-- | An environment a use may run in: its head, the run options the
-- listings show, and the call that resolved it, if a parameter did.
data Target = Target
  { tHead :: HeadImage,
    tOpts :: Map Text OptVal,
    tVia :: Maybe Via,
    -- | The secrets its run options are given, by channel (spec 10.11).
    tSecrets :: Map Channel Flow
  }
  deriving (Show, Eq, Ord)

-- | Where a secret originates (spec 10.11, Secret flow): a secret
-- binding, named by the declaration it is in and its name, or a call
-- of @mark_secret@.
data Secret = Secret
  { scModule :: FilePath,
    scDecl :: Text,
    scName :: Text,
    scKind :: SecretKind,
    scSpan :: Span
  }
  deriving (Show, Eq, Ord)

data SecretKind = SecretValue | SecretKeyword | SecretPositional | SecretBinding | SecretCall
  deriving (Show, Eq, Ord)

-- | What a value carries: the secrets, each with the call that gave it
-- when a parameter did, and whether a parameter of the declaration
-- walked on its own may add to them, which each call resolves.
data Flow = Flow
  { flSecrets :: Map Secret Carried,
    flParam :: Bool
  }
  deriving (Show, Eq, Ord)

-- | A secret as a value carries it: the call that gave it, when a
-- parameter did, and every name it has been bound to with @!!@ on its
-- way (spec 10.11), each as the module, the declaration and the name.
data Carried = Carried
  { crVia :: Maybe Via,
    crNames :: Set (FilePath, Text, Text)
  }
  deriving (Show, Eq, Ord)

noFlow :: Flow
noFlow = Flow Map.empty False

joinFlow :: Flow -> Flow -> Flow
joinFlow a b = Flow (Map.unionWith joinCarried (flSecrets a) (flSecrets b)) (flParam a || flParam b)
  where
    joinCarried x y = Carried (crVia x <|> crVia y) (Set.union (crNames x) (crNames y))

carries :: Flow -> Bool
carries f = not (Map.null (flSecrets f)) || flParam f

-- | The run options @envs files@, @envs network@ and @envs access@
-- show (spec 11.4); those of 'accessOptions' decide what a use
-- reaches.
listedOptions :: [Text]
listedOptions =
  [ "base_mount",
    "base_access",
    "volumes",
    "read_only",
    "tmpfs",
    "network",
    "egress",
    "publish",
    "dns",
    "dns_search",
    "add_hosts"
  ]

-- | The run options that enter an access set (spec 10.11).
accessOptions :: [Text]
accessOptions = ["base_access", "volumes", "network", "egress", "publish"]

data UseKind
  = UseCommand
  | -- | A filesystem function, and the path when it is a literal.
    UseFile Text (Maybe Text)
  | UseSession
  | -- | A top-level function named other than by a call, through
    -- which the uses it leaves to its parameters are not followed.
    UseEscape Key
  deriving (Show, Eq, Ord)

-- | One use of an environment (spec 10.11): where it is written, what
-- it may run in, and the constraints covering the declarations it is
-- reached through.
data UseSite = UseSite
  { usModule :: FilePath,
    usDecl :: Text,
    usSpan :: Span,
    usKind :: UseKind,
    usTargets :: Set Target,
    -- | Whether a value the program does not show may be the
    -- environment.
    usDynamic :: Bool,
    -- | Whether a parameter of the declaration walked on its own may
    -- be the environment.
    usParam :: Bool,
    usUnder :: [Covering],
    -- | The declarations it is reached through, outermost first.
    usPath :: [Text],
    -- | The declaration the walk that found it started from.
    usRoot :: Key,
    -- | The calls it is reached through, outermost first: each in the
    -- declaration it is written in.
    usCalls :: [(Key, Span)],
    -- | The secrets its command string or its arguments carry
    -- (@command@, @file@); those of its run options are its targets'.
    usFlows :: Map Channel Flow
  }
  deriving (Show, Eq, Ord)

-- | A function as a value: a lambda with what it closes over and the
-- declaration it is written in, or a top-level function.
data FunRef
  = FunLam Lam (Map Text AVal) Key
  | FunTop Key
  deriving (Show)

funKey :: FunRef -> Either (FilePath, Text, Key) Key
funKey f = case f of
  FunLam lam _ origin -> Left (lamModule lam, lamName lam, origin)
  FunTop k -> Right k

instance Eq FunRef where
  a == b = funKey a == funKey b

instance Ord FunRef where
  compare a b = compare (funKey a) (funKey b)

-- | What an expression can be: the environments it reaches, whether
-- an unknown one, or a parameter, may be among them, the value it has
-- when that is static, with the call that gave it, and the functions
-- it may be.
data AVal = AVal
  { avTargets :: Set Target,
    avDyn :: Bool,
    avParam :: Bool,
    avStatic :: Maybe SVal,
    avVia :: Maybe Via,
    avFuns :: [FunRef],
    -- | What each field of a record or map literal can be, so that a
    -- static field is static even where the rest is not.
    avFields :: Maybe (Map Text AVal),
    -- | The secrets it carries (spec 10.11, Secret flow).
    avFlow :: Flow
  }
  deriving (Show, Eq, Ord)

unknownV :: AVal
unknownV = AVal Set.empty False False Nothing Nothing [] Nothing noFlow

dynV :: AVal
dynV = unknownV {avDyn = True}

paramV :: AVal
paramV = unknownV {avParam = True, avFlow = noFlow {flParam = True}}

staticV :: SVal -> AVal
staticV v = unknownV {avStatic = Just v}

-- | What either of two values can be.
joinV :: AVal -> AVal -> AVal
joinV a b =
  AVal
    (Set.union (avTargets a) (avTargets b))
    (avDyn a || avDyn b)
    (avParam a || avParam b)
    (if avStatic a == avStatic b then avStatic a else Nothing)
    (avVia a <|> avVia b)
    (nub (avFuns a <> avFuns b))
    (if avFields a == avFields b then avFields a else Nothing)
    (joinFlow (avFlow a) (avFlow b))

-- | The environments of several values, none of their static values.
unionV :: [AVal] -> AVal
unionV vs =
  unknownV
    { avTargets = Set.unions (map avTargets vs),
      avDyn = any avDyn vs,
      avParam = any avParam vs,
      avFuns = nub (concatMap avFuns vs),
      avFlow = foldr (joinFlow . avFlow) noFlow vs
    }

-- | A value as a parameter of a call gives it: what it reaches was
-- resolved at that call, unless a call further out did.
passedFrom :: Via -> AVal -> AVal
passedFrom call v =
  v
    { avTargets = Set.map (\t -> t {tVia = tVia t <|> Just call}) (avTargets v),
      avVia = avVia v <|> (call <$ avStatic v),
      avFields = Map.map (passedFrom call) <$> avFields v,
      avFlow = (avFlow v) {flSecrets = Map.map (\cr -> cr {crVia = crVia cr <|> Just call}) (flSecrets (avFlow v))}
    }

fsFunctions :: [Text]
fsFunctions = ["read_file", "write_file", "file_exists", "remove_file", "make_dir", "list_dir", "glob"]

type Walk = State (Map (Key, Maybe [(Text, AVal)]) [UseSite])

-- | Every use of an environment reachable from the entry module, or
-- with a declaration, from it (spec 11.4), with the constraints that
-- cover what it is reached through.
collectUseSites :: Constraints -> CoreProgram -> Maybe Key -> [UseSite]
collectUseSites cons core scope = walkRoots cons core roots
  where
    roots = case scope of
      Just key -> [key]
      Nothing -> [k | k@(p, _) <- Map.keys (cpDecls core), p == cpEntry core]

-- | The uses found walking each declaration on its own.
walkRoots :: Constraints -> CoreProgram -> [Key] -> [UseSite]
walkRoots cons core roots =
  Set.toList . Set.fromList $
    evalState (concat <$> mapM (\k -> maybe (pure []) (\cd -> walkDecl cons core [] [] Nothing cd Nothing) (Map.lookup k (cpDecls core))) roots) Map.empty

-- | A declaration, walked with what a call binds its parameters to, or
-- on its own. Walks are remembered by what they were given, except a
-- function value, which is not compared by what it closes over; what
-- is remembered is what the declaration reaches from itself, and the
-- declarations it is reached through (@outer@, innermost first) are
-- added to each use as it is returned. @guard@ is every declaration
-- being walked, which a call of one does not walk again.
walkDecl :: Constraints -> CoreProgram -> [Key] -> [Key] -> Maybe (Key, Span) -> CoreDecl -> Maybe [(Text, AVal)] -> Walk [UseSite]
walkDecl cons core guard outer at cd given = map (liftSite cons outer at) <$> do
  let key = (cdModule cd, cdName cd)
      memoKey = (key, given)
      remembered = all (null . avFuns . snd) (maybe [] id given)
  memo <- if remembered then gets (Map.lookup memoKey) else pure Nothing
  case memo of
    Just found -> pure found
    Nothing -> do
      if remembered then modify' (Map.insert memoKey []) else pure ()
      found <- case coreF (cdCore cd) of
        CLam lam -> do
          let binds = maybe (rootBinds core cd lam) Map.fromList given
          defaults <- concat <$> mapM (walk cons core (key : guard) [key] key binds) [d | (k, Just d) <- lamKeywords lam, maybe True (notElem k . map fst) given]
          body <- walk cons core (key : guard) [key] key binds (lamBody lam)
          pure (defaults <> body)
        _ -> walk cons core (key : guard) [key] key Map.empty (cdCore cd)
      if remembered then modify' (Map.insert memoKey found) else pure ()
      pure found

-- | A use a declaration reaches, as reached through the declarations
-- that lead to it.
--
-- @at@ is the call it is reached by, in the declaration that makes it.
liftSite :: Constraints -> [Key] -> Maybe (Key, Span) -> UseSite -> UseSite
liftSite cons outer at u0 =
  let u = u0 {usCalls = maybe [] pure at <> usCalls u0}
   in case outer of
        [] -> u
        _ ->
          u
            { usUnder = nub (usUnder u <> concatMap (coveringsOf cons) outer),
              usPath = reverse (map snd outer) <> usPath u,
              usRoot = last outer
            }

-- | What a declaration walked on its own binds its parameters to: a
-- parameter, which each call resolves, except a keyword parameter of a
-- type the command line cannot give, which an invocation leaves at its
-- default (11.2).
rootBinds :: CoreProgram -> CoreDecl -> Lam -> Map Text AVal
rootBinds core cd lam =
  Map.fromList $
    [(n, paramV) | n <- lamPositional lam]
      <> [(n, paramV) | Just n <- [lamVariadic lam]]
      <> [(k, kwRoot k d) | (k, d) <- lamKeywords lam]
  where
    here = (cdModule cd, cdName cd)
    keywordTypes = maybe [] spKeywords (cdParams cd)
    kwRoot k d = case (lookup k keywordTypes, d) of
      (Just t, Just dc) | holdsEnv t -> reach core here Set.empty Map.empty dc
      _ -> paramV

-- | The uses an expression reaches. @scope@ is the declarations the
-- walk of the remembered declaration has entered, innermost first,
-- which a lambda a parameter holds adds its own to.
walk :: Constraints -> CoreProgram -> [Key] -> [Key] -> Key -> Map Text AVal -> Core -> Walk [UseSite]
walk cons core guard scope here binds c = case coreF c of
  CApp (Core _ (CVar (BuiltinRef "run"))) (envC : rest) kws -> do
    inner <- concat <$> mapM go (envC : rest <> map snd kws)
    pure (use UseCommand (value envC) (carriedBy ChCommand (rest <> map snd kws)) : inner)
  CApp (Core _ (CVar (BuiltinRef fn))) args@(_ : _) []
    | fn `elem` fsFunctions -> do
        inner <- concat <$> mapM go args
        let path = case args of
              Core _ (CStrLit p) : _ -> Just p
              _ -> Nothing
        pure (use (UseFile fn path) (value (last args)) (carriedBy ChFile (init args)) : inner)
  -- The session a with_container body is given runs in the
  -- environment the call opens it on (spec 15.5).
  CApp (Core _ (CVar (BuiltinRef "with_container"))) [envC, Core _ (CLam lam)] []
    | box : _ <- lamPositional lam -> do
        let env = value envC
        outer <- go envC
        inner <- walk cons core guard scope here (Map.insert box env binds) (lamBody lam)
        pure (use UseSession env Map.empty : outer <> inner)
  -- A call of a top-level function: its body, with what this call
  -- gives its parameters. A function given as an argument is bound,
  -- not named as a value.
  -- A lambda given to a built-in function is called with what the
  -- other arguments carry (spec 10.11, Secret flow).
  CApp (Core _ (CVar (BuiltinRef _))) pos kws
    | any isLam (pos <> map snd kws) -> do
        let args = pos <> map snd kws
            others = avFlow (unionV [value a | a <- args, not (isLam a)])
        concat
          <$> mapM
            ( \a -> case coreF a of
                CLam lam -> walkLam others lam
                _ -> go a
            )
            args
  CApp (Core _ (CVar (TopRef p n))) pos kws
    | Just callee <- Map.lookup (p, n) (cpDecls core),
      CLam lam <- coreF (cdCore callee) -> do
        args <- concat <$> mapM go [a | a <- pos <> map snd kws, not (namesFunction a)]
        inner <-
          if (p, n) `elem` guard
            then pure []
            else walkDecl cons core guard scope (Just (here, coreSpan c)) callee (Just (Map.toAscList (callBinds core here (call c) Set.empty binds lam pos kws)))
        pure (args <> inner)
  -- A call of a function a parameter or a binding holds: the body of
  -- each function it may be, with what this call gives it.
  CApp (Core _ (CVar (LocalRef f))) pos kws
    | Just v <- Map.lookup f binds,
      not (null (avFuns v)) -> do
        args <- concat <$> mapM go (pos <> map snd kws)
        inner <- concat <$> mapM (callFun pos kws) (avFuns v)
        pure (args <> inner)
  -- A value binding is walked on its own; so is a function named
  -- other than by a call, which is marked, since what it leaves to
  -- its parameters is not followed from here.
  CVar (TopRef p n)
    | Just callee <- Map.lookup (p, n) (cpDecls core) -> do
        let escape = case coreF (cdCore callee) of
              CLam _ -> [use (UseEscape (p, n)) unknownV Map.empty]
              _ -> []
        inner <- if (p, n) `elem` guard then pure [] else walkDecl cons core guard scope (Just (here, coreSpan c)) callee Nothing
        pure (escape <> inner)
  CLam lam -> walkLam noFlow lam
  CDo stmts -> walkStmts binds stmts
  _ -> concat <$> mapM go (coreChildren c)
  where
    go = walk cons core guard scope here binds
    value = reach core here Set.empty binds
    call e = Via (fst here) (snd here) (coreSpan e)
    isLam a = case coreF a of
      CLam _ -> True
      _ -> False
    walkLam extra lam = do
      let params = case lamType lam of
            TyFun ts _ -> [(x, (if holdsEnv t then dynV else unknownV) {avFlow = extra}) | (x, t) <- zip (lamPositional lam) ts]
            _ -> [(x, unknownV {avFlow = extra}) | x <- lamPositional lam]
          binds' = Map.union (Map.fromList params) binds
      defaults <- concat <$> mapM go [d | (_, Just d) <- lamKeywords lam]
      body <- walk cons core guard scope here binds' (lamBody lam)
      pure (defaults <> body)
    -- What the expressions a use is given carry, by the channel they
    -- give it by.
    carriedBy ch es =
      let f = avFlow (unionV (map value es))
       in if carries f then Map.singleton ch f else Map.empty
    use kind v flows =
      UseSite
        (fst here)
        (snd here)
        (coreSpan c)
        kind
        (avTargets v)
        (avDyn v)
        (avParam v)
        (nub (concatMap (coveringsOf cons) (here : scope)))
        (reverse (map snd scope))
        (last scope)
        []
        flows
    namesFunction a = case coreF a of
      CVar (TopRef p n) | Just cd <- Map.lookup (p, n) (cpDecls core), CLam _ <- coreF (cdCore cd) -> True
      _ -> False
    callFun pos kws f = case f of
      FunTop k
        | Just callee <- Map.lookup k (cpDecls core),
          CLam lam <- coreF (cdCore callee),
          k `notElem` guard ->
            walkDecl cons core guard scope (Just (here, coreSpan c)) callee (Just (Map.toAscList (callBinds core here (call c) Set.empty binds lam pos kws)))
      FunTop _ -> pure []
      FunLam lam closure origin -> do
        let given = callBinds core here (call c) Set.empty binds lam pos kws
        walk cons core guard (if origin `elem` scope then scope else origin : scope) origin (Map.union given closure) (lamBody lam)
    walkStmts _ [] = pure []
    walkStmts bs (CSBind x e : more) = do
      a <- walk cons core guard scope here bs e
      b <- walkStmts (Map.insert x (boundValue core here Set.empty bs x e) bs) more
      pure (a <> b)
    walkStmts bs (CSExpr e : more) = (<>) <$> walk cons core guard scope here bs e <*> walkStmts bs more

-- | What a call binds the parameters of the function it calls to: the
-- arguments it gives, and the defaults of those it leaves.
callBinds :: CoreProgram -> Key -> Via -> Set Key -> Map Text AVal -> Lam -> [Core] -> [(Text, Core)] -> Map Text AVal
callBinds core here call seen binds lam pos kws =
  let arg = passedFrom call . reach core here seen binds
      positional = zip (lamPositional lam) (map arg pos)
      variadic = [(v, unionV (map arg (drop (length (lamPositional lam)) pos))) | Just v <- [lamVariadic lam]]
      given = Map.fromList (positional <> variadic <> [(k, arg e) | (k, e) <- kws])
   in foldl
        (\acc (k, d) -> if Map.member k acc then acc else Map.insert k (maybe unknownV (reach core (lamModule lam, lamName lam) seen acc) d) acc)
        given
        (lamKeywords lam)

-- | What an expression can be (spec 10.11, Resolution), and what it
-- carries: nothing, where its type is one that can tell nothing of a
-- secret (Secret flow).
reach :: CoreProgram -> Key -> Set Key -> Map Text AVal -> Core -> AVal
reach core here seen binds c
  | scalarResult core c = v {avFlow = noFlow}
  | otherwise = v
  where
    v = reachRaw core here seen binds c

-- | What a binding of @x@ to @e@ binds it to: a binding through
-- @mark_secret@ originates a secret named by it (spec 10.11, Secret
-- flow), as a parameter when it rebinds one (6.10).
boundValue :: CoreProgram -> Key -> Set Key -> Map Text AVal -> Text -> Core -> AVal
boundValue core here seen binds x e = case coreF e of
  CApp (Core _ (CVar (BuiltinRef "mark_secret"))) [inner] [] ->
    let kind = case (coreF inner, coreF . cdCore <$> Map.lookup here (cpDecls core)) of
          (CVar (LocalRef y), Just (CLam lam))
            | y == x, x `elem` map fst (lamKeywords lam) -> SecretKeyword
            | y == x, x `elem` (lamPositional lam <> maybe [] pure (lamVariadic lam)) -> SecretPositional
          _ -> SecretBinding
        -- A parameter is placed at its declaration, not at the
        -- rebinding the marker desugars to.
        at = case kind of
          SecretBinding -> coreSpan e
          _ -> maybe (coreSpan e) (coreSpan . cdCore) (Map.lookup here (cpDecls core))
     in originates (Secret (fst here) (snd here) x kind at) (reach core here seen binds inner)
  _ -> reach core here seen binds e

-- | A value bound as a secret. One known to be @null@ originates
-- nothing, since an absent secret has no text (spec 6.10), and nor
-- does one that carries a secret already: it is that secret, marked
-- again, as a library marks what it is given, and is bound to this
-- name as well (spec 10.11).
originates :: Secret -> AVal -> AVal
originates sc v
  | avStatic v == Just SNull = v
  | not (Map.null (flSecrets (avFlow v))) = v {avFlow = (avFlow v) {flSecrets = Map.map (\cr -> cr {crNames = Set.union named (crNames cr)}) (flSecrets (avFlow v))}}
  | otherwise = v {avFlow = (avFlow v) {flSecrets = Map.insert sc (Carried Nothing named) (flSecrets (avFlow v))}}
  where
    named = case scKind sc of
      SecretCall -> Set.empty
      _ -> Set.singleton (scModule sc, scDecl sc, scName sc)

-- | Whether an expression's type is @Bool@, @Number@, @Null@ or @Void@,
-- as far as it is known without the type checker's annotations.
scalarResult :: CoreProgram -> Core -> Bool
scalarResult core c = case coreF c of
  CNull -> True
  CBool _ -> True
  CNumber _ -> True
  CAnd _ _ -> True
  COr _ _ -> True
  CNot _ -> True
  CIsType _ _ -> True
  CBin PAdd a b -> scalarResult core a || scalarResult core b
  CBin _ _ _ -> True
  CCast _ t -> scalar t
  CApp (Core _ (CVar (BuiltinRef f))) _ _ -> maybe False (scalar . schemeRet) (Map.lookup f builtinSchemes)
  CApp (Core _ (CVar (TopRef p n))) _ _
    | Just cd <- Map.lookup (p, n) (cpDecls core),
      TyFun _ r <- cdType cd ->
        scalar r
  _ -> False
  where
    scalar t = t `elem` [TyBool, TyNumber, TyNull, TyVoid]

reachRaw :: CoreProgram -> Key -> Set Key -> Map Text AVal -> Core -> AVal
reachRaw core here seen binds c = case coreF c of
  CNull -> staticV SNull
  CBool b -> staticV (SBool b)
  CNumber x -> staticV (SNum (T.pack (formatScientific Generic Nothing x)))
  CStrLit t -> staticV (SStr t)
  CArray es ->
    let vs = map go es
     in (unionV vs) {avStatic = SArr <$> mapM avStatic vs, avVia = firstVia vs}
  CMapLit kvs -> literal SMap kvs
  CRecordLit kvs -> literal SRec kvs
  CEnv "local" _ -> unknownV {avTargets = Set.singleton (Target HeadLocal Map.empty Nothing Map.empty)}
  CEnv "docker" args -> unknownV {avTargets = maybe Set.empty (\h -> Set.singleton (Target h Map.empty Nothing Map.empty)) (headImage args)}
  CRunnable envC opts ->
    let env = go envC
        vals = [(k, evaluated o (go o)) | (k, o) <- opts, k `elem` listedOptions]
        shown = Map.fromList [(k, optVal v) | (k, v) <- vals]
        via = listToMaybe [w | (_, v) <- vals, isJust (avStatic v), Just w <- [avVia v]]
        flows = Map.filter carries (Map.fromListWith joinFlow (concat [optionFlows k (go o) | (k, o) <- opts]))
     in env {avTargets = Set.map (\t -> t {tOpts = shown, tVia = tVia t <|> via, tSecrets = Map.unionWith joinFlow (tSecrets t) flows}) (avTargets env), avStatic = Nothing}
  CApp (Core _ (CVar (BuiltinRef "mark_secret"))) [inner] [] ->
    originates (Secret (fst here) (snd here) "" SecretCall (coreSpan c)) (go inner)
  -- The tables a library builds a variable set with keep their keys,
  -- so that what each variable carries is still known (spec 10.11,
  -- Secret flow): a merge is both tables, the later winning, an entry
  -- filtered out is kept, and a reduction over no element is its
  -- initial value.
  CApp (Core _ (CVar (BuiltinRef "merge"))) [a, b] [] ->
    let va = go a
        vb = go b
     in (unionV [va, vb]) {avFields = Map.union <$> fieldsOf vb <*> fieldsOf va}
  CApp (Core _ (CVar (BuiltinRef "from_entries"))) [xs] []
    | Just m <- entriesOf xs -> (go m) {avStatic = Nothing}
  CApp (Core _ (CVar (BuiltinRef "reduce"))) [xs, z, _] []
    | avStatic (go xs) == Just (SArr []) -> go z
  CCast a _ -> (go a) {avStatic = Nothing}
  CVar (LocalRef x) -> Map.findWithDefault unknownV x binds
  CVar (TopRef p n)
    | Just cd <- Map.lookup (p, n) (cpDecls core) -> case coreF (cdCore cd) of
        -- A function as a value carries what its body does, which a
        -- call of it may return.
        CLam lam
          | (p, n) `Set.member` seen -> unknownV {avFuns = [FunTop (p, n)]}
          | otherwise -> unknownV {avFuns = [FunTop (p, n)], avFlow = avFlow (reach core (p, n) (Set.insert (p, n) seen) Map.empty (lamBody lam))}
        _
          | (p, n) `Set.member` seen -> unknownV
          | otherwise -> topValue (p, n) cd
  CLam lam -> unknownV {avFuns = [FunLam lam binds here], avFlow = avFlow (reach core here seen (foldr Map.delete binds (lamPositional lam <> map fst (lamKeywords lam))) (lamBody lam))}
  CApp (Core _ (CVar (TopRef p n))) pos kws
    | Just cd <- Map.lookup (p, n) (cpDecls core),
      CLam lam <- coreF (cdCore cd),
      (p, n) `Set.notMember` seen ->
        let seen' = Set.insert (p, n) seen
            call = Via (fst here) (snd here) (coreSpan c)
         in evaluated c (reach core (p, n) seen' (callBinds core here call seen binds lam pos kws) (lamBody lam))
  CIndex _ (Core _ (CVar (TopRef p n))) (Core _ (CStrLit k))
    | Just e <- entry (p, n) k -> go e
  CDot (Core _ (CVar (TopRef p n))) k
    | Just e <- entry (p, n) k -> go e
  CDot e k
    | Just v <- Map.lookup k =<< avFields (go e) -> v
  CIf _ a b -> joinV (go a) (go b)
  CDo stmts -> doStmts binds unknownV stmts
  _ -> unionV (map go (coreChildren c))
  where
    go = reach core here seen binds
    -- An option the rules above leave unresolved is still static when
    -- evaluating it can do nothing but compute (spec 10.11).
    evaluated o v
      | isJust (avStatic v) || avParam v = v
      | otherwise = maybe v (\sv -> v {avStatic = Just sv}) (staticEval core binds o)
    optVal v
      | Just s <- avStatic v = Known s
      | avParam v = FromParam
      | otherwise = Computed
    literal mk kvs =
      let vs = map (go . snd) kvs
       in (unionV vs) {avStatic = mk . zip (map fst kvs) <$> mapM avStatic vs, avVia = firstVia vs, avFields = Just (Map.fromList (zip (map fst kvs) vs))}
    firstVia vs = listToMaybe [w | v <- vs, Just w <- [avVia v]]
    doStmts _ acc [] = acc
    doStmts bs acc (CSBind x e : more) =
      let v = boundValue core here seen bs x e
       in doStmts (Map.insert x v bs) (accumulate acc v) more
    doStmts bs acc (CSExpr e : more) =
      let v = reach core here seen bs e
       in doStmts bs (if null more then (accumulate acc v) {avStatic = avStatic v, avVia = avVia v, avFields = avFields v} else accumulate acc v) more
    accumulate acc v = acc {avTargets = Set.union (avTargets acc) (avTargets v), avDyn = avDyn acc || avDyn v, avParam = avParam acc || avParam v}
    entriesOf xs = case coreF xs of
      CApp (Core _ (CVar (BuiltinRef "entries"))) [m] [] -> Just m
      CApp (Core _ (CVar (BuiltinRef "filter"))) [inner, _] [] -> entriesOf inner
      _ -> Nothing
    -- A top-level value; one bound through @mark_secret@ originates a
    -- secret named by it.
    topValue key cd = case coreF (cdCore cd) of
      CApp (Core _ (CVar (BuiltinRef "mark_secret"))) [inner] [] ->
        originates (Secret (fst key) (snd key) (snd key) SecretValue (coreSpan (cdCore cd))) (reach core key (Set.insert key seen) Map.empty inner)
      _ -> reach core key (Set.insert key seen) Map.empty (cdCore cd)
    -- What a run option carries, by channel: a value of @env@ by the
    -- name of its variable, when the table is a literal.
    optionFlows k v
      | k == "env" = case fieldsOf v of
          Just fs -> [(ChEnv name, avFlow fv) | (name, fv) <- Map.toList fs]
          Nothing -> [(ChEnvComputed, avFlow v)]
      | otherwise = [(ChOption k, avFlow v)]
    entry key k = case coreF . cdCore <$> Map.lookup key (cpDecls core) of
      Just (CMapLit kvs) -> lookup k kvs
      Just (CRecordLit kvs) -> lookup k kvs
      _ -> Nothing

-- | The value of an expression that can only compute (spec 10.11):
-- it reaches no effect, no variable of the process and no @await@,
-- and every local it reads is static. It is evaluated as a run would
-- evaluate it, with hooks that refuse anything else; one that fails,
-- or does not finish in a second, is not static.
staticEval :: CoreProgram -> Map Text AVal -> Core -> Maybe SVal
staticEval core binds c
  | not (computesOnly core c) = Nothing
  | otherwise = do
      scope <- traverse (\x -> knownValue =<< Map.lookup x binds) (Map.fromSet id locals)
      unsafePerformIO $ do
        ctx <- mkRtCtx core "" (RtHooks refuseCommand refuseFile refuseSession (const (pure ())) noAsyncTracker refuseEnv)
        r <- timeout 1000000 (try (evaluate . forceValue =<< evalCore ctx scope c))
        pure $ case r of
          Just (Right v) -> fromValue v
          Just (Left e) -> const Nothing (e :: SomeException)
          Nothing -> Nothing
  where
    -- The locals the expression reads that the walk has a value for;
    -- any other is bound inside it, by a lambda.
    locals = Set.fromList [x | CVar (LocalRef x) <- map coreF (c : coreDescendants c), Map.member x binds]
    refuseCommand _ _ = pure (Left refusal)
    refuseFile _ _ = pure (Left refusal)
    refuseSession _ _ _ = throwIO refusal
    refuseEnv _ = throwIO refusal
    refusal = ioFailure EIoEnvResolve "not evaluated statically"
    forceValue v = length (show (fromValue v)) `seq` v

-- | Whether an expression, and every top-level declaration it reaches,
-- can only compute: no effect (Chapter 5), no variable of the process,
-- no @await@.
computesOnly :: CoreProgram -> Core -> Bool
computesOnly core = go Set.empty . pure
  where
    go _ [] = True
    go seen (e : rest) = case coreF e of
      CVar (BuiltinRef n) | n `Set.member` forbidden -> False
      CAwait _ -> False
      CVar (TopRef p n)
        | (p, n) `Set.member` seen -> go seen rest
        | Just cd <- Map.lookup (p, n) (cpDecls core) -> go (Set.insert (p, n) seen) (cdCore cd : rest)
      _ -> go seen (coreChildren e <> rest)
    forbidden = effectfulBuiltinNames <> Set.fromList ["get_env", "find_env", "get_env_or"]

coreDescendants :: Core -> [Core]
coreDescendants e = let cs = coreChildren e in cs <> concatMap coreDescendants cs

-- | A value as far as it is static: a record or map of which only some
-- fields are holds those, and reading another fails.
knownValue :: AVal -> Maybe Value
knownValue v = case (avStatic v, avFields v) of
  (Just sv, _) -> toValue sv
  (Nothing, Just fs) -> Just (VRecord (Map.mapMaybe knownValue fs))
  _ -> Nothing

toValue :: SVal -> Maybe Value
toValue v = case v of
  SStr t -> Just (VString t)
  SBool b -> Just (VBool b)
  SNull -> Just VNull
  SNum t -> VNumber <$> readMaybe (T.unpack t)
  SArr xs -> VArray . V.fromList <$> mapM toValue xs
  SMap kvs -> VMap . Map.fromList <$> mapM (traverse toValue) kvs
  SRec kvs -> VRecord . Map.fromList <$> mapM (traverse toValue) kvs

fromValue :: Value -> Maybe SVal
fromValue v = case v of
  VString t -> Just (SStr t)
  VBool b -> Just (SBool b)
  VNull -> Just SNull
  VNumber n -> Just (SNum (T.pack (formatScientific Generic Nothing n)))
  VArray xs -> SArr <$> mapM fromValue (V.toList xs)
  VMap m -> SMap <$> mapM (traverse fromValue) (Map.toAscList m)
  VRecord m -> SRec <$> mapM (traverse fromValue) (Map.toAscList m)
  _ -> Nothing

-- | Whether what a target reaches waits on a parameter of the
-- declaration walked on its own.
targetParametric :: Target -> Bool
targetParametric t = any (== FromParam) [v | (k, v) <- Map.toList (tOpts t), k `elem` accessOptions]

-- | The access set of a target (spec 10.11): a component whose option
-- is computed is 'Nothing'.
targetAccess :: Target -> AccessSet
targetAccess t = case tHead t of
  HeadLocal -> HostAccess "#local reaches the whole host"
  _
    | Just vs <- volumes,
      v : _ <- filter (hostEquivalent . volSource) vs ->
        HostAccess ("the volume " <> q (sourceOf (volSource v)) <> " gives what the host has")
    | otherwise -> AccessSet files volumes network publish
  where
    q x = "\"" <> x <> "\""
    sourceOf s' = case s' of
      VolumeHost h -> h
      VolumeNamed n -> n
    opt k = case Map.lookup k (tOpts t) of
      Just (Known SNull) -> Nothing
      Nothing -> Nothing
      Just v -> Just v
    computed k = case Map.lookup k (tOpts t) of
      Just Computed -> True
      Just FromParam -> True
      _ -> False
    strings v = case v of
      SArr xs -> Just [s | SStr s <- xs]
      SStr s -> Just [s]
      _ -> Nothing
    files
      | computed "base_access" = Nothing
      | otherwise = case opt "base_access" of
          Nothing -> Just (accessScalar ModeRw)
          Just (Known (SStr m)) -> either (const Nothing) (Just . accessScalar) (parseMode m)
          Just (Known (SMap kvs)) -> either (const Nothing) Just (accessTable [(k, v) | (k, SStr v) <- kvs])
          _ -> Nothing
    volumes
      | computed "volumes" = Nothing
      | otherwise = case opt "volumes" of
          Nothing -> Just []
          Just (Known v) -> mapMaybe parseVolume <$> strings v
          _ -> Nothing
    -- An egress given leaves the network to what it lists (spec 10.12).
    network
      | computed "egress" = Nothing
      | Just (Known v) <- opt "egress" = case v of
          SArr xs -> NetEgress <$> mapM entry [x | x <- xs, x /= SNull]
          _ -> Nothing
      | computed "network" = Nothing
      | otherwise = case opt "network" of
          Nothing -> Just (NetReach NetDefault Nothing)
          Just (Known (SStr v)) -> Just (NetReach (classifyNetwork (Just v)) (Just v))
          _ -> Nothing
    entry x = case x of
      SStr e -> either (const Nothing) Just (parseEgressEntry e)
      _ -> Nothing
    publish
      | computed "publish" = Nothing
      | otherwise = case opt "publish" of
          Nothing -> Just []
          Just (Known v) -> map parsePublish <$> strings v
          _ -> Nothing

-- Constraints -----------------------------------------------------------

-- | A constraint as it covers what it is written on (spec 5): the
-- function it is written on, or the module, and where.
data Covering = Covering
  { cvModule :: FilePath,
    cvFunction :: Maybe Text,
    cvSpan :: Span,
    cvConstraint :: Constraint,
    -- | The @access@ declaration it is written by, as it is named.
    cvName :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | A constraint as what it covers writes it: by its name, or by its
-- fields.
renderCovering :: Covering -> Text
renderCovering cv = maybe (renderConstraint (cvConstraint cv)) ("within " <>) (cvName cv)

-- | The constraints of each module: its own, and those of its
-- functions.
type Constraints = Map FilePath (Maybe Covering, Map Text Covering)

-- | The constraints a declaration is checked against (spec 10.11): its
-- module's and its own.
coveringsOf :: Constraints -> Key -> [Covering]
coveringsOf cons (p, n) = case Map.lookup p cons of
  Nothing -> []
  Just (m, fs) -> filter (bounds . cvConstraint) (maybe [] pure m <> maybe [] pure (Map.lookup n fs))

-- | Read the access constraints of a program (spec 5, 10.11): what
-- each module and function is written with, and @E-TYPE-WITHIN-SPEC@
-- for one that is not a constraint. A constraint written by name is
-- that of the @access@ declaration it names, which is reported once,
-- where it is declared.
withinConstraints :: Program -> ([Diagnostic], Constraints)
withinConstraints prog =
  let perModule = [(lmPath lm, moduleConstraints (lmPath lm) (AST.moduleDecls (lmModule lm))) | lm <- Map.elems (progModules prog)]
   in ( aliasDiags <> concat [ds | (_, (ds, _)) <- perModule],
        Map.fromList [(p, c) | (p, (_, c)) <- perModule]
      )
  where
    scopes = buildScopes prog
    -- Each access declaration, read once.
    declared =
      Map.fromList
        [ ((lmPath lm, n), readAlias w)
        | lm <- Map.elems (progModules prog),
          AST.Decl _ (AST.DAccess n w) <- AST.moduleDecls (lmModule lm)
        ]
    aliasDiags = concat [ds | Left ds <- Map.elems declared]
    readAlias w = do
      c <- parseSpec w
      case conSecrets c of
        Just (SecretsNamed ((_, sp) : _)) ->
          Left [mkDiagnostic ETypeWithinSpec StageStatic sp "an access declaration gives 'secrets' only as false: the secrets a constraint names are bindings of what it covers"]
        _ -> Right c

    -- The constraint a name gives, where it is written: 'Nothing' when
    -- the name resolves to nothing, which name resolution reports.
    resolveRef path q n = do
      gs <- Map.lookup path scopes
      gs' <- case q of
        Nothing -> pure gs
        Just ns -> Map.lookup ns (gsNamespaces gs) >>= (`Map.lookup` scopes)
      Map.lookup n (gsTypes gs')

    moduleConstraints path decls =
      let modules = [(sp, w) | AST.Decl sp (AST.DWithin w) <- decls]
          functions = [(n, w) | AST.Decl _ (AST.DFunction n _ _ _ (Just w) _) <- decls]
          second = [mkDiagnostic ETypeWithinSpec StageStatic sp "a module has at most one module constraint" | (sp, _) <- drop 1 modules]
          read' fn w = case w of
            AST.AccessSpec sp _ -> case parseSpec w of
              Left ds -> (ds, Nothing)
              Right c -> ([], Just (Covering path fn sp c Nothing))
            AST.AccessRef sp q n ->
              let named = maybe "" (<> ".") q <> n
               in case resolveRef path q n of
                    Just (TAlias defPath defName)
                      | Just r <- Map.lookup (defPath, defName) declared ->
                          -- A declaration that does not read is reported
                          -- where it is declared.
                          ([], either (const Nothing) (\c -> Just (Covering path fn sp c (Just named))) r)
                    Just _ -> ([mkDiagnostic ETypeWithinSpec StageStatic sp ("'" <> named <> "' is a type, not an access constraint")], Nothing)
                    Nothing -> ([], Nothing)
          (mds, mc) = case modules of
            (_, w) : _ -> read' Nothing w
            [] -> ([], Nothing)
          fs = [(n, read' (Just n) w) | (n, w) <- functions]
       in ( second <> mds <> concat [ds | (_, (ds, _)) <- fs],
            (mc, Map.fromList [(n, c) | (n, (_, Just c)) <- fs])
          )

-- | An access constraint from its fields (spec 10.11), each a literal.
parseSpec :: AST.AccessSpec -> Either [Diagnostic] Constraint
parseSpec w = case w of
  AST.AccessRef sp _ _ -> Left [mkDiagnostic ETypeWithinSpec StageStatic sp "an access declaration is written with braces"]
  AST.AccessSpec _ fields ->
    let results = map field fields
        duplicates =
          [ err sp ("duplicate field '" <> k <> "'")
          | (i, (Spanned sp k, _)) <- zip [0 :: Int ..] fields,
            k `elem` [k' | (Spanned _ k', _) <- take i fields]
          ]
     in case [d | Left d <- results] <> duplicates of
          [] -> Right (foldl (\c f -> f c) noConstraint [f | Right f <- results])
          ds -> Left ds
  where
    err = mkDiagnostic ETypeWithinSpec StageStatic
    field (Spanned ksp k, v) = case k of
      "files" -> case literal v of
        Just (SStr m) -> either (Left . err (AST.exprSpan v)) (\x -> Right (\c -> c {conFiles = Just (accessScalar x)})) (parseMode m)
        Just (SMap kvs)
          | Just pairs <- mapM str kvs ->
              either (Left . err (AST.exprSpan v)) (\t -> Right (\c -> c {conFiles = Just t})) (accessTable pairs)
        _ -> Left (err (AST.exprSpan v) "'files' takes \"rw\", \"ro\", \"none\", or a table of them with \".\"")
      "volumes" -> case literal v of
        Just (SBool False) -> Right (\c -> c {conVolumes = Just (VolumesListed [])})
        Just (SBool True) -> Right (\c -> c {conVolumes = Just VolumesAny})
        Just (SMap kvs)
          | Just pairs <- mapM str kvs,
            Right vs <- mapM volume pairs ->
              Right (\c -> c {conVolumes = Just (VolumesListed vs)})
          | Just pairs <- mapM str kvs,
            Left e <- mapM volume pairs ->
              Left (err (AST.exprSpan v) e)
        _ -> Left (err (AST.exprSpan v) "'volumes' takes false, true, or a table of host paths and volume names to \"rw\" or \"ro\"")
      "outbound" -> case literal v of
        Just (SBool False) -> Right (\c -> c {conOutbound = Just (BoundClass NetNone)})
        Just (SBool True) -> Right (\c -> c {conOutbound = Just (BoundClass NetDefault)})
        Just (SArr xs)
          | Just ts <- strings xs ->
              either (Left . err (AST.exprSpan v)) (\es -> Right (\c -> c {conOutbound = Just (BoundEgress es)})) (mapM parseEgressEntry ts)
        _ -> Left (err (AST.exprSpan v) "'outbound' takes false, true, or an array of egress entries such as [\"registry.npmjs.org:443\"]")
      "inbound" -> case literal v of
        Just (SBool False) -> Right (\c -> c {conInbound = Just (InboundListed [])})
        Just (SBool True) -> Right (\c -> c {conInbound = Just InboundAny})
        Just (SArr xs)
          | Just ts <- strings xs ->
              either (Left . err (AST.exprSpan v)) (\es -> Right (\c -> c {conInbound = Just (InboundListed es)})) (mapM parseInboundEntry ts)
        _ -> Left (err (AST.exprSpan v) "'inbound' takes false, true, or an array of host addresses, each with a port where it is limited to one, such as [\"127.0.0.1:8080\"]")
      "runs" -> case literal v of
        Just (SBool False) -> Right (\c -> c {conRuns = Just []})
        Just (SArr xs)
          | Just ts <- strings xs,
            not (null ts),
            Just ps <- mapM parseProfile ts ->
              Right (\c -> c {conRuns = Just (nub ps)})
        _ -> Left (err (AST.exprSpan v) "'runs' takes false, or an array of \"local\" and \"container\"")
      "secrets" -> case AST.exprF v of
        AST.EBool False -> Right (\c -> c {conSecrets = Just SecretsNone})
        AST.EArray xs
          | Just names <- mapM (\x -> case AST.exprF x of AST.EVar n -> Just (n, AST.exprSpan x); _ -> Nothing) xs ->
              Right (\c -> c {conSecrets = Just (SecretsNamed names)})
        _ -> Left (err (AST.exprSpan v) "'secrets' takes false, or an array of the names of secret bindings, such as [token]")
      _ -> Left (err ksp ("unknown field '" <> k <> "': a constraint takes files, volumes, outbound, inbound, secrets and runs"))
    str (k, SStr s') = Just (k, s')
    str _ = Nothing
    strings = mapM (\x -> case x of SStr t -> Just t; _ -> Nothing)
    volume (k, m) = do
      mode <- case m of
        "rw" -> Right ModeRw
        "ro" -> Right ModeRo
        _ -> Left ("the mode of '" <> k <> "' must be \"rw\" or \"ro\"")
      src <-
        if "/" `T.isPrefixOf` k
          then
            if k == "/" || all (`notElem` ["", ".", ".."]) (T.splitOn "/" (T.drop 1 k))
              then Right (VolumeHost k)
              else Left ("'" <> k <> "' must be a normalized absolute path")
          else
            if not (T.null k) && not ("/" `T.isInfixOf` k)
              then Right (VolumeNamed k)
              else Left ("'" <> k <> "' must be an absolute host path or a volume name")
      pure (src, mode)
    -- A value as written, when it is a literal containing no
    -- interpolation.
    literal e = case AST.exprF e of
      AST.ENull -> Just SNull
      AST.EBool b -> Just (SBool b)
      AST.EString parts -> SStr . T.concat <$> mapM chunk parts
      AST.EObject kvs -> SMap <$> mapM (\(Spanned _ k, x) -> (,) k <$> literal x) kvs
      AST.EArray xs -> SArr <$> mapM literal xs
      _ -> Nothing
    chunk p = case p of
      AST.TPChunk _ t -> Just t
      _ -> Nothing

-- | What the constraints of a program find (spec 10.11): every use a
-- covered declaration reaches, checked against each constraint that
-- covers it; a parametric one at each call that resolves it.
checkWithin :: Constraints -> CoreProgram -> [Diagnostic]
checkWithin cons core
  | Map.null (Map.filter covered cons) = []
  | otherwise = settle (concatMap (siteFindings core) uses <> escapes) <> unnamed
  where
    covered (m, fs) = isJust m || not (Map.null fs)
    unnamed = concat [fst (secretNames core cv) | (m, fs) <- Map.elems cons, cv <- maybe [] pure m <> Map.elems fs]
    uses = walkRoots cons core (Map.keys (cpDecls core))
    -- A function whose covered uses wait on its parameters, named
    -- other than by a call: its constraint cannot be checked there.
    owes u = not (null (usUnder u)) && (usParam u || any targetParametric (Set.toList (usTargets u)) || any (\(_, f, _) -> flParam f) (channelFlows u))
    owing = Set.fromList [usRoot u | u <- uses, owes u]
    escapes =
      [ ( (Nothing, usSpan u, "escape"),
          mkDiagnostic ETypeWithinUnproven StageStatic (usSpan u) $
            "'" <> n <> "' is named as a value, so what its access constraint requires of its arguments cannot be checked at the calls it reaches; call it by name"
        )
      | u <- uses,
        UseEscape k@(_, n) <- [usKind u],
        k `Set.member` owing
      ]

-- | The findings of one use against each constraint covering it.
siteFindings :: CoreProgram -> UseSite -> [((Maybe Span, Span, Text), Diagnostic)]
siteFindings core u = case usKind u of
  UseEscape _ -> []
  _ ->
    [ ((Just (cvSpan cv), sp, findingField f), finding cv sp f)
    | cv <- usUnder u,
      (sp, f) <-
        [ (maybe (inCovered cv) viaSpan (tVia t), f)
        | t <- Set.toList (usTargets u),
          not (targetParametric t),
          f <- checkAccess (cvConstraint cv) (targetAccess t)
        ]
          <> [(maybe (maybe (inCovered cv) viaSpan tvia) viaSpan via, f) | (via, tvia, _, _, f) <- secretFindings core cv u]
          <> [(inCovered cv, unknownEnv cv) | usDynamic u]
          -- A constraint that admits no use is broken by any, whatever
          -- its environment, including one a parameter gives.
          <> [(inCovered cv, Exceeds "runs" (usedHere (usKind u))) | usParam u, conRuns (cvConstraint cv) == Just []]
    ]
  where
    unknownEnv cv
      | conRuns (cvConstraint cv) == Just [] = Exceeds "runs" (usedHere (usKind u))
      | otherwise = Unproven "environment"
    usedHere k = case k of
      UseFile _ _ -> "a filesystem function uses an environment here"
      UseSession -> "with_container starts a container here"
      _ -> "a command runs here"
    -- Where a constraint is broken: in what it covers. A use written
    -- there is reported where it is written; one reached through a
    -- call, at the call that leads out of what the constraint covers,
    -- since what the callee does is within its own constraints.
    inCovered cv = case cvFunction cv of
      Just n
        | (usModule u, usDecl u) == (cvModule cv, n) -> usSpan u
        | (_, sp) : _ <- [x | x@(k, _) <- usCalls u, k == (cvModule cv, n)] -> sp
      Nothing
        | usModule u == cvModule cv -> usSpan u
        | (_, sp) : _ <- reverse [x | x@((m, _), _) <- usCalls u, m == cvModule cv] -> sp
      _ -> usSpan u
    finding cv sp f =
      let (code, msg) = case f of
            Exceeds k what -> (ETypeWithinExceeded, what <> ", which " <> renderCovering cv <> " of " <> coveredLabel cv <> " does not allow (" <> k <> ")")
            Unproven "environment" -> (ETypeWithinUnproven, "the environment here is not known without running the program, so " <> renderCovering cv <> " of " <> coveredLabel cv <> " cannot be shown to hold")
            Unproven k -> (ETypeWithinUnproven, "'" <> option k <> "' is not known here without running the program, so " <> renderCovering cv <> " of " <> coveredLabel cv <> " cannot be shown to hold")
       in withNote ("reached through " <> T.intercalate " → " (usPath u <> [usDecl u | lastMay (usPath u) /= Just (usDecl u)]) <> ", at " <> location (usSpan u)) $
            withNote ("the constraint is at " <> location (cvSpan cv)) $
              mkDiagnostic code StageStatic sp msg
    -- The run option a component is read from.
    option k = case k of
      "files" -> "base_access"
      "outbound" -> "network"
      "inbound" -> "publish"
      other -> other
    lastMay xs = if null xs then Nothing else Just (last xs)

coveredLabel :: Covering -> Text
coveredLabel cv = maybe "the module" (\n -> "'" <> n <> "'") (cvFunction cv)

location :: Span -> Text
location sp = case sp of
  Span (Position f l _) _ -> T.pack f <> ":" <> T.pack (show l)
  NoSpan -> "?"

-- | One diagnostic for each constraint, place and component.
settle :: [((Maybe Span, Span, Text), Diagnostic)] -> [Diagnostic]
settle = Map.elems . Map.fromListWith (\_ first -> first)

-- | What an invocation by @run@ or @eval@ resolves (spec 10.11): the
-- uses the function leaves to its parameters, with the values the
-- command line gives them and the defaults of the keyword parameters
-- it does not.
checkInvocation :: Constraints -> CoreProgram -> Key -> [(Text, SVal)] -> [Diagnostic]
checkInvocation cons core key args = case Map.lookup key (cpDecls core) of
  Just cd
    | CLam lam <- coreF (cdCore cd) ->
        let given = Map.fromList [(n, staticV v) | (n, v) <- args]
            binds =
              foldl
                (\acc (k, d) -> if Map.member k acc then acc else Map.insert k (maybe unknownV (reach core key Set.empty acc) d) acc)
                given
                (lamKeywords lam)
            found = evalState (walkDecl cons core [] [] Nothing cd (Just (Map.toAscList binds))) Map.empty
         in map (withNote "with the arguments the command line gives, and the defaults of those it does not") (settle (concatMap (siteFindings core) found))
  _ -> []

-- | What @cmd@ runs a command word in (spec 5, 11.8), checked against
-- the constraint of the module that declares it.
checkCommandEnv :: Constraints -> CoreProgram -> FilePath -> Text -> Core -> [Diagnostic]
checkCommandEnv cons core declaring word env =
  case Map.lookup declaring cons of
    Just (Just cv, _)
      | bounds (cvConstraint cv) ->
          let v = reach core (declaring, "") Set.empty Map.empty env
              site = UseSite declaring ("command \"" <> word <> "\"") (coreSpan env) UseCommand (avTargets v) (avDyn v) (avParam v) [cv] [] (declaring, "") [] Map.empty
           in settle (siteFindings core site)
    _ -> []

-- Secrets ----------------------------------------------------------------

-- | The secrets a use is given, by channel: those of the run options of
-- each environment it may run in, with the call that resolved that
-- environment, and those of its command string or arguments.
channelFlows :: UseSite -> [(Channel, Flow, Maybe Via)]
channelFlows u =
  [(ch, f, tVia t) | t <- Set.toList (usTargets u), (ch, f) <- Map.toList (tSecrets t)]
    <> [(ch, f, Nothing) | (ch, f) <- Map.toList (usFlows u)]

-- | What a constraint finds of the secrets a use is given (spec
-- 10.11): each with the call that gave the secret, and the one that
-- resolved the environment, where a parameter did.
secretFindings :: CoreProgram -> Covering -> UseSite -> [(Maybe Via, Maybe Via, Channel, Secret, Finding)]
secretFindings core cv u =
  [ (crVia cr, tvia, ch, sc, f)
  | (ch, fl, tvia) <- channelFlows u,
    (sc, cr) <- Map.toList (flSecrets fl),
    f <- checkSecrets (cvConstraint cv) ch (label sc cr) (not (Set.null (Set.intersection names (crNames cr))))
  ]
  where
    names = snd (secretNames core cv)
    -- A secret bound to no name cannot be one a constraint gives.
    label sc cr =
      "'" <> secretLabel (secretFile sc) sc <> "'"
        <> (if Set.null (crNames cr) then ", bound to no name a constraint can give (bind it with !!)," else "")

-- | The secret bindings the names of a @secrets@ field are (spec
-- 10.11): for a function's constraint, a @!!@ parameter or a binding
-- of its body through @mark_secret@, which @!!@ is; otherwise, or when
-- it has none of the name, a top-level value of the module bound so.
-- A name that is neither is @E-TYPE-WITHIN-SPEC@.
secretNames :: CoreProgram -> Covering -> ([Diagnostic], Set (FilePath, Text, Text))
secretNames core cv = case conSecrets (cvConstraint cv) of
  Just (SecretsNamed ns) ->
    let resolved = [(n, sp, resolve n) | (n, sp) <- ns]
     in ( [ mkDiagnostic ETypeWithinSpec StageStatic sp ("'" <> n <> "' is not a secret binding of " <> coveredLabel cv <> ": a constraint names its !! parameters and bindings, and the !! values of its module")
          | (n, sp, Nothing) <- resolved
          ],
          Set.fromList [k | (_, _, Just k) <- resolved]
        )
  _ -> ([], Set.empty)
  where
    path = cvModule cv
    resolve n = case cvFunction cv of
      Just f | n `Set.member` localSecrets f -> Just (path, f, n)
      _ | n `Set.member` topSecrets -> Just (path, n, n)
      _ -> Nothing
    localSecrets f = case Map.lookup (path, f) (cpDecls core) of
      Just cd -> Set.fromList [x | CDo stmts <- map coreF (cdCore cd : coreDescendants (cdCore cd)), CSBind x e <- stmts, isMark e]
      Nothing -> Set.empty
    topSecrets = Set.fromList [n | ((p, n), cd) <- Map.toList (cpDecls core), p == path, isMark (cdCore cd)]
    isMark e = case coreF e of
      CApp (Core _ (CVar (BuiltinRef "mark_secret"))) [_] [] -> True
      _ -> False

-- | Where a secret originates, as 10.11 names it, with its module
-- shown as given.
secretLabel :: Text -> Secret -> Text
secretLabel file sc = case scKind sc of
  SecretValue -> file <> ": " <> scName sc
  SecretKeyword -> file <> ": " <> scDecl sc <> " --" <> scName sc
  SecretPositional -> file <> ": " <> scDecl sc <> " " <> scName sc
  SecretBinding -> file <> ": " <> scDecl sc <> " " <> scName sc
  SecretCall -> location (scSpan sc) <> " mark_secret"

secretFile :: Secret -> Text
secretFile sc = case scSpan sc of
  Span (Position f _ _) _ -> T.pack f
  NoSpan -> T.pack (scModule sc)

-- | Whether a program has any secret to follow.
programMarksSecrets :: CoreProgram -> Bool
programMarksSecrets core = any (marks . cdCore) (Map.elems (cpDecls core))
  where
    marks e = any isMark (e : coreDescendants e)
    isMark e = case coreF e of
      CVar (BuiltinRef "mark_secret") -> True
      _ -> False

-- | @W-SECRET-COMMAND-LINE@ (spec 10.11, 14.2): a secret that reaches a
-- use in its command string, or in a run option other than a value of
-- @env@, where it is on a command line of the host. Placed where it
-- enters, or at the call that gives it to a parameter.
secretAdvisories :: CoreProgram -> [Advisory]
secretAdvisories core
  | not (programMarksSecrets core) = []
  | otherwise = sortOn advSpan (Map.elems (Map.fromListWith (\_ a -> a) found))
  where
    uses = walkRoots Map.empty core (Map.keys (cpDecls core))
    found =
      [ ((place, sc), Advisory WSecretCommandLine place (message sc ch))
      | u <- uses,
        (ch, fl, tvia) <- channelFlows u,
        exposed ch,
        (sc, cr) <- Map.toList (flSecrets fl),
        let place = maybe (maybe (usSpan u) viaSpan tvia) viaSpan (crVia cr)
      ]
    exposed ch = case ch of
      ChCommand -> True
      ChOption _ -> True
      _ -> False
    message sc ch =
      "the secret '"
        <> secretLabel (secretFile sc) sc
        <> "' is "
        <> ( case ch of
               ChOption k -> "in the run option '" <> k <> "', which is given to the engine client on a command line of the host; give it in 'env'"
               _ -> "in the command string, from which the programs the command runs are given it as arguments, which the host's process list shows; give it in 'env' to a program that reads the variable itself"
           )

-- | What each entry of a table or record can be: those of a literal,
-- or of a static value.
fieldsOf :: AVal -> Maybe (Map Text AVal)
fieldsOf v = avFields v <|> (static =<< avStatic v)
  where
    static sv = case sv of
      SMap kvs -> Just (Map.fromList [(k, staticV x) | (k, x) <- kvs])
      SRec kvs -> Just (Map.fromList [(k, staticV x) | (k, x) <- kvs])
      _ -> Nothing
