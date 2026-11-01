{-# LANGUAGE OverloadedStrings #-}

-- | What the uses of environments may reach past their containers
-- (spec 10.11, 11.4): the static reading behind @lask envs files@,
-- @lask envs network@ and @lask envs access@.
--
-- A use is a command, a filesystem function or a @with_container@
-- (10.11). Its environment is read from its expression without
-- evaluating anything, as @envs commands@ reads it, and so are the run
-- options the listings show, where they are static. A call of a
-- top-level function is followed into its body with what the call
-- gives its parameters, so an environment a caller passes is listed at
-- that caller, not as @(dynamic)@.
module Command.Lask.EnvAccess
  ( Target (..),
    OptVal (..),
    SVal (..),
    UseKind (..),
    UseSite (..),
    Via,
    collectUseSites,
    listedOptions,
  )
where

import Command.Lask.Envs (HeadImage (..), headImage, holdsEnv)
import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, evalState, gets, modify')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, listToMaybe)
import Data.Scientific (formatScientific, FPFormat (Generic))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), Key, StaticParams (..))
import Language.Lask.Span (Span)
import Language.Lask.Types (Type (..))

-- | A value known without evaluating the program.
data SVal
  = SStr Text
  | SBool Bool
  | SNum Text
  | SNull
  | SArr [SVal]
  | SMap [(Text, SVal)]
  deriving (Show, Eq, Ord)

-- | A run option as the listings read it: its static value, or a value
-- only the run knows.
data OptVal = Known SVal | Computed
  deriving (Show, Eq, Ord)

-- | The declaration whose call gave a use the environment or the
-- option values it has (spec 11.4, REQUIRED BY): its module and name.
type Via = (FilePath, Text)

-- | An environment a use may run in: its head, the run options the
-- listings show, and the call that resolved it, if a parameter did.
data Target = Target
  { tHead :: HeadImage,
    tOpts :: Map Text OptVal,
    tVia :: Maybe Via
  }
  deriving (Show, Eq, Ord)

-- | The run options @envs files@, @envs network@ and @envs access@
-- show (spec 11.4).
listedOptions :: [Text]
listedOptions =
  [ "base_mount",
    "base_access",
    "volumes",
    "read_only",
    "tmpfs",
    "network",
    "publish",
    "dns",
    "dns_search",
    "add_hosts"
  ]

data UseKind
  = UseCommand
  | -- | A filesystem function, and the path when it is a literal.
    UseFile Text (Maybe Text)
  | UseSession
  deriving (Show, Eq, Ord)

-- | One use of an environment (spec 10.11), where it is written, and
-- what it may run in.
data UseSite = UseSite
  { usModule :: FilePath,
    usDecl :: Text,
    usSpan :: Span,
    usKind :: UseKind,
    usTargets :: Set Target,
    -- | Whether a value the program does not show may be the
    -- environment: listed under @(dynamic)@.
    usDynamic :: Bool
  }
  deriving (Show, Eq, Ord)

-- | What an expression can be, as far as the listings go: the
-- environments it reaches, whether an unknown one may be among them,
-- and the value it has when that is static, with the call that gave
-- it.
data AVal = AVal
  { avTargets :: Set Target,
    avDyn :: Bool,
    avStatic :: Maybe SVal,
    avVia :: Maybe Via
  }
  deriving (Show, Eq, Ord)

unknownV :: AVal
unknownV = AVal Set.empty False Nothing Nothing

dynV :: AVal
dynV = unknownV {avDyn = True}

staticV :: SVal -> AVal
staticV v = unknownV {avStatic = Just v}

-- | What either of two values can be.
joinV :: AVal -> AVal -> AVal
joinV a b =
  AVal
    (Set.union (avTargets a) (avTargets b))
    (avDyn a || avDyn b)
    (if avStatic a == avStatic b then avStatic a else Nothing)
    (avVia a <|> avVia b)

-- | The environments of several values, none of their static values.
unionV :: [AVal] -> AVal
unionV vs = unknownV {avTargets = Set.unions (map avTargets vs), avDyn = any avDyn vs}

-- | A value as a parameter of a call made in @caller@ gives it: what
-- it reaches was resolved there, unless a call further out did.
passedFrom :: Via -> AVal -> AVal
passedFrom caller v =
  v
    { avTargets = Set.map (\t -> t {tVia = tVia t <|> Just caller}) (avTargets v),
      avVia = avVia v <|> (caller <$ avStatic v)
    }

fsFunctions :: [Text]
fsFunctions = ["read_file", "write_file", "file_exists", "remove_file", "make_dir", "list_dir", "glob"]

-- | Every use of an environment reachable from the entry module, or
-- with a declaration, from it (spec 11.4).
--
-- Each root is walked as the command line would invoke it: a
-- parameter that can hold an environment is one no invocation gives,
-- so it is @(dynamic)@ unless it has a default; any other parameter
-- may be given, so an option it supplies is computed. A call of a
-- top-level function is walked again for each distinct set of values
-- it is given, which is what resolves a parametric use (10.11).
collectUseSites :: CoreProgram -> Maybe Key -> [UseSite]
collectUseSites core scope =
  Set.toList . Set.fromList $ evalState (concat <$> mapM root roots) Map.empty
  where
    roots = case scope of
      Just key -> [key]
      Nothing -> [k | k@(p, _) <- Map.keys (cpDecls core), p == cpEntry core]

    root key = case Map.lookup key (cpDecls core) of
      Nothing -> pure []
      Just cd -> walkDecl [key] cd Nothing

    -- A declaration, walked with what a call binds its parameters to,
    -- or as a root. Walks are remembered by what they were given.
    walkDecl :: [Key] -> CoreDecl -> Maybe [(Text, AVal)] -> State (Map (Key, Maybe [(Text, AVal)]) [UseSite]) [UseSite]
    walkDecl stack cd given = do
      let key = (cdModule cd, cdName cd)
      memo <- gets (Map.lookup (key, given))
      case memo of
        Just found -> pure found
        Nothing -> do
          modify' (Map.insert (key, given) [])
          found <- case coreF (cdCore cd) of
            CLam lam -> do
              let binds = maybe (rootBinds cd lam) Map.fromList given
              defaults <- concat <$> mapM (walk stack cd binds) [d | (_, Just d) <- lamKeywords lam]
              body <- walk stack cd binds (lamBody lam)
              pure (defaults <> body)
            _ -> walk stack cd Map.empty (cdCore cd)
          modify' (Map.insert (key, given) found)
          pure found

    rootBinds cd lam =
      Map.fromList $
        [(n, if holdsEnv t then dynV else unknownV) | (n, t) <- positional]
          <> [(n, if holdsEnv t then dynV else unknownV) | Just (n, t) <- [variadic]]
          <> [ (k, kwRoot k d)
             | (k, d) <- lamKeywords lam
             ]
      where
        here = (cdModule cd, cdName cd)
        (positional, variadic, keywordTypes) = case cdParams cd of
          Just sp -> (spPositional sp, fmap (fmap TyArray) (spVariadic sp), spKeywords sp)
          Nothing -> case lamType lam of
            TyFun ts _ -> (zip (lamPositional lam) ts, Nothing, [])
            _ -> ([], Nothing, [])
        -- A keyword parameter of a type the command line cannot give
        -- takes its default (11.2); any other may be given.
        kwRoot k d = case (lookup k keywordTypes, d) of
          (Just t, Just dc) | holdsEnv t -> reach here Set.empty Map.empty dc
          (Just t, Nothing) | holdsEnv t -> dynV
          _ -> unknownV

    walk :: [Key] -> CoreDecl -> Map Text AVal -> Core -> State (Map (Key, Maybe [(Text, AVal)]) [UseSite]) [UseSite]
    walk stack cd binds c = case coreF c of
      CApp (Core _ (CVar (BuiltinRef "run"))) (envC : rest) kws -> do
        inner <- concat <$> mapM (walk stack cd binds) (envC : rest <> map snd kws)
        pure (use UseCommand (reach here Set.empty binds envC) : inner)
      CApp (Core _ (CVar (BuiltinRef fn))) args@(_ : _) []
        | fn `elem` fsFunctions -> do
            inner <- concat <$> mapM (walk stack cd binds) args
            let path = case args of
                  Core _ (CStrLit p) : _ -> Just p
                  _ -> Nothing
            pure (use (UseFile fn path) (reach here Set.empty binds (last args)) : inner)
      -- The session a with_container body is given runs in the
      -- environment the call opens it on (spec 15.5).
      CApp (Core _ (CVar (BuiltinRef "with_container"))) [envC, Core _ (CLam lam)] []
        | box : _ <- lamPositional lam -> do
            let env = reach here Set.empty binds envC
            outer <- walk stack cd binds envC
            inner <- walk stack cd (Map.insert box env binds) (lamBody lam)
            pure (use UseSession env : outer <> inner)
      -- A call of a top-level function: its body, with what this call
      -- gives its parameters.
      CApp (Core _ (CVar (TopRef p n))) pos kws
        | Just callee <- Map.lookup (p, n) (cpDecls core),
          CLam lam <- coreF (cdCore callee) -> do
            args <- concat <$> mapM (walk stack cd binds) (pos <> map snd kws)
            inner <-
              if (p, n) `elem` stack
                then pure []
                else walkDecl ((p, n) : stack) callee (Just (Map.toAscList (callBinds here Set.empty binds lam pos kws)))
            pure (args <> inner)
      -- A value binding, and a function named as a value: what the
      -- function does when called, by whatever calls it, is not
      -- followed, so it is walked as a root.
      CVar (TopRef p n)
        | Just callee <- Map.lookup (p, n) (cpDecls core),
          (p, n) `notElem` stack ->
            walkDecl ((p, n) : stack) callee Nothing
      CLam lam -> do
        let params = case lamType lam of
              TyFun ts _ -> [(x, if holdsEnv t then dynV else unknownV) | (x, t) <- zip (lamPositional lam) ts]
              _ -> [(x, unknownV) | x <- lamPositional lam]
            binds' = Map.union (Map.fromList params) binds
        defaults <- concat <$> mapM (walk stack cd binds) [d | (_, Just d) <- lamKeywords lam]
        body <- walk stack cd binds' (lamBody lam)
        pure (defaults <> body)
      CDo stmts -> walkStmts binds stmts
      _ -> concat <$> mapM (walk stack cd binds) (coreChildren c)
      where
        here = (cdModule cd, cdName cd)
        use kind v = UseSite (cdModule cd) (cdName cd) (coreSpan c) kind (avTargets v) (avDyn v)
        walkStmts _ [] = pure []
        walkStmts bs (CSBind x e : more) = do
          a <- walk stack cd bs e
          b <- walkStmts (Map.insert x (reach here Set.empty bs e) bs) more
          pure (a <> b)
        walkStmts bs (CSExpr e : more) = (<>) <$> walk stack cd bs e <*> walkStmts bs more

    -- What a call binds the parameters of the function it calls to:
    -- the arguments it gives, and the defaults of those it leaves.
    callBinds caller seen binds lam pos kws =
      let positional = zip (lamPositional lam) (map (passedFrom caller . reach caller seen binds) pos)
          given = Map.fromList (positional <> [(k, passedFrom caller (reach caller seen binds e)) | (k, e) <- kws])
          defaulted = foldl (\acc (k, d) -> if Map.member k acc then acc else Map.insert k (maybe unknownV (reach caller seen acc) d) acc) given (lamKeywords lam)
       in defaulted

    -- What an expression can be (spec 10.11, Resolution).
    reach :: Via -> Set Key -> Map Text AVal -> Core -> AVal
    reach here seen binds c = case coreF c of
      CNull -> staticV SNull
      CBool b -> staticV (SBool b)
      CNumber x -> staticV (SNum (T.pack (formatScientific Generic Nothing x)))
      CStrLit t -> staticV (SStr t)
      CArray es ->
        let vs = map go es
         in (unionV vs) {avStatic = SArr <$> mapM avStatic vs, avVia = firstVia vs}
      CMapLit kvs -> literal kvs
      CRecordLit kvs -> literal kvs
      CEnv "local" _ -> unknownV {avTargets = Set.singleton (Target HeadLocal Map.empty Nothing)}
      CEnv "docker" args -> unknownV {avTargets = maybe Set.empty (\h -> Set.singleton (Target h Map.empty Nothing)) (headImage args)}
      CRunnable envC opts ->
        let env = go envC
            vals = [(k, go o) | (k, o) <- opts, k `elem` listedOptions]
            shown = Map.fromList [(k, maybe Computed Known (avStatic v)) | (k, v) <- vals]
            via = listToMaybe [w | (_, v) <- vals, isJust (avStatic v), Just w <- [avVia v]]
         in env {avTargets = Set.map (\t -> t {tOpts = shown, tVia = tVia t <|> via}) (avTargets env), avStatic = Nothing}
      CVar (LocalRef x) -> Map.findWithDefault unknownV x binds
      CVar (TopRef p n)
        | Just cd <- Map.lookup (p, n) (cpDecls core) -> case coreF (cdCore cd) of
            CLam _ -> dynV
            _
              | (p, n) `Set.member` seen -> unknownV
              | otherwise -> reach here (Set.insert (p, n) seen) Map.empty (cdCore cd)
      CApp (Core _ (CVar (TopRef p n))) pos kws
        | Just cd <- Map.lookup (p, n) (cpDecls core),
          CLam lam <- coreF (cdCore cd),
          (p, n) `Set.notMember` seen ->
            let seen' = Set.insert (p, n) seen
             in reach here seen' (callBinds here seen binds lam pos kws) (lamBody lam)
      CIndex _ (Core _ (CVar (TopRef p n))) (Core _ (CStrLit k))
        | Just e <- entry (p, n) k -> go e
      CDot (Core _ (CVar (TopRef p n))) k
        | Just e <- entry (p, n) k -> go e
      CIf _ a b -> joinV (go a) (go b)
      CDo stmts -> doStmts binds unknownV stmts
      _ -> unionV (map go (coreChildren c))
      where
        go = reach here seen binds
        literal kvs =
          let vs = map (go . snd) kvs
           in (unionV vs) {avStatic = SMap . zip (map fst kvs) <$> mapM avStatic vs, avVia = firstVia vs}
        firstVia vs = listToMaybe [w | v <- vs, Just w <- [avVia v]]
        doStmts _ acc [] = acc
        doStmts bs acc (CSBind x e : more) =
          let v = reach here seen bs e
           in doStmts (Map.insert x v bs) (accumulate acc v) more
        doStmts bs acc (CSExpr e : more) =
          let v = reach here seen bs e
           in doStmts bs (if null more then (accumulate acc v) {avStatic = avStatic v, avVia = avVia v} else accumulate acc v) more
        accumulate acc v = acc {avTargets = Set.union (avTargets acc) (avTargets v), avDyn = avDyn acc || avDyn v}

    entry key k = case coreF . cdCore <$> Map.lookup key (cpDecls core) of
      Just (CMapLit kvs) -> lookup k kvs
      Just (CRecordLit kvs) -> lookup k kvs
      _ -> Nothing

