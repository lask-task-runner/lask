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
    CommandUse (..),
    CoreDecl (..),
    readFieldType,
    Key,
    StaticParams (..),
    HoverInfo (..),
    elaborateProgram,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT (runStateT), evalStateT, get, gets, lift, modify, put)
import Data.Maybe (isNothing)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Names (effectfulBuiltinNames)
import Language.Lask.Builtins.Sig
import Language.Lask.Core.AST
import Language.Lask.Diagnostic
import Language.Lask.Desugar.Return (transformFunctionBody)
import Language.Lask.ErrorCode
import Language.Lask.Lexer.Token (CmdStream (..), Op (..), Spanned (..))
import Language.Lask.Module.Loader (LoadedModule (..), Program (..), collapseDots)
import Language.Lask.Module.Resolve (GlobalScope (..), TypeTarget (..), ValueTarget (..), namespaceMember)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.CommandWords (Analysis (..), CommandWord (..), commandWords, validCommandName)
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
    cdParams :: Maybe StaticParams
  }
  deriving (Show, Eq)

-- | Static declaration parameter information (spec 7.5): what a call
-- site may use when the callee resolves to a declaration.
data StaticParams = StaticParams
  { spPositional :: [(Text, Type)],
    -- | Variadic parameter with its element type.
    spVariadic :: Maybe (Text, Type),
    spKeywords :: [(Text, Type)]
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
    cpHover :: [HoverInfo]
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
    cuWords :: [Spanned Text]
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
    ctxTypeVars :: Set Text
  }

-- | A type is settled at this point when every variable it still
-- mentions is a type parameter of the declaration being elaborated
-- (spec 4.4): those stand for themselves and nothing can instantiate
-- them further, so a pattern holding one is as concrete as it gets.
settled :: Ctx -> Type -> Bool
settled ctx t = all (`Set.member` ctxTypeVars ctx) (typeVars t)

-- | Elaborate @act@ with @vs@ as the type parameters in scope.
withTypeVars :: Set Text -> Ctx -> Ctx
withTypeVars vs ctx = ctx {ctxTypeVars = vs}

data St = St
  { stDecls :: Map Key CoreDecl,
    -- | Elaborated alias bodies, with the type parameters the alias
    -- binds (spec 4.2); a reference substitutes its arguments in.
    stAliases :: Map Key ([Text], Type),
    stActive :: Set Key,
    stHover :: [HoverInfo],
    stCommandUses :: [CommandUse],
    -- | Command tables, built once per module on first use.
    stCommands :: Map FilePath (Map Text CommandEntry),
    -- | Modules whose command table is being built. A command string
    -- elaborated meanwhile was reached from the environment of a
    -- command declaration, which may run nothing (spec ch. 5).
    stCommandsBuilding :: Set FilePath
  }

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
    ceOrigin :: (FilePath, Int)
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

type TC = StateT St (Either Diagnostic)

-- | Record how one command execution expression got its environment.
recordCommandUse :: Span -> Maybe Text -> [Spanned Text] -> TC ()
recordCommandUse sp env ws =
  modify (\s -> s {stCommandUses = CommandUse sp env ws : stCommandUses s})

-- | An environment in the notation of environment expressions (6.7),
-- for display.
renderEnvCore :: Core -> Text
renderEnvCore c = case coreF c of
  CEnv "local" _ -> "#local"
  CEnv "docker" args -> case (lookup "image" args, lookup "dockerfile" args) of
    (Just (Core _ (CStrLit img)), _) -> "#" <> img
    (_, Just (Core _ (CStrLit df))) -> "#docker(dockerfile = \"" <> df <> "\")"
    _ -> "#docker(...)"
  CEnv kind _ -> "#" <> kind
  -- An environment named by a binding, or produced by a call, is
  -- shown as what was written: its value exists only at run time.
  CVar (TopRef _ n) -> n
  CApp (Core _ (CVar (TopRef _ n))) _ _ -> n <> "(...)"
  _ -> "#?"

-- | Record a resolved name occurrence for hover (editor tooling).
recordVar :: Span -> Text -> Type -> Maybe Key -> TC ()
recordVar sp n t d =
  modify (\s -> s {stHover = HoverInfo sp n t d Nothing : stHover s})

-- | Record a reference to the builtin @bn@ for hover.
recordBuiltin :: Span -> Text -> Type -> Text -> TC ()
recordBuiltin sp n t bn =
  modify (\s -> s {stHover = HoverInfo sp n t Nothing (Just bn) : stHover s})

-- | Local value bindings with their types.
type Locals = Map Text Type

abort :: Diagnostic -> TC a
abort = lift . Left

-- | Attempt an elaboration, recovering from its diagnostic. Used for
-- bidirectional fallbacks (e.g. inferring one if branch and checking
-- the other, so context-typed calls like @fail(e)@ work in either
-- branch, spec 15.7).
tryTC :: TC a -> TC (Either Diagnostic a)
tryTC action = do
  st <- get
  case runStateT action st of
    Left d -> pure (Left d)
    Right (a, st') -> put st' >> pure (Right a)

diag :: ErrorCode -> Span -> Text -> Diagnostic
diag code sp = mkDiagnostic code StageStatic sp

mismatch :: Span -> Type -> Type -> TC a
mismatch sp expected actual =
  abort $
    withExpectedActual (renderType expected) (renderType actual) $
      diag ETypeMismatch sp "type mismatch"

-- Entry point -----------------------------------------------------------------

elaborateProgram :: Program -> Map FilePath GlobalScope -> Either [Diagnostic] CoreProgram
elaborateProgram prog scopes =
  case evalStateT (elabAll >> gets (\s -> (stDecls s, stHover s, stCommands s, stCommandUses s))) (St Map.empty Map.empty Set.empty [] [] Map.empty Set.empty) of
    Left d -> Left [d]
    Right (decls, hover, commands, uses) ->
      Right
        CoreProgram
          { cpEntry = progEntry prog,
            cpBaseDir = progBaseDir prog,
            cpDecls = decls,
            cpInternal =
              maybe Set.empty (moduleInternal . lmModule) $
                Map.lookup (progEntry prog) (progModules prog),
            cpCommands = Map.map (Map.map ceEnv) commands,
            cpCommandUses = uses,
            cpHover = hover
          }
  where
    ctx = Ctx prog scopes Set.empty
    -- Every module's command declarations are checked, whether or not
    -- a command string uses them (spec ch. 5).
    elabAll = do
      mapM_ (commandTable ctx . lmPath) (Map.elems (progModules prog))
      mapM_
        (demandDecl ctx)
        [ (lmPath lm, n)
        | lm <- Map.elems (progModules prog),
          Decl _ f <- moduleDecls (lmModule lm),
          n <- declValueName f
        ]
    declValueName (DValue n _ _ _) = [n]
    declValueName (DFunction n _ _ _ _) = [n]
    declValueName _ = []

-- Top-level declarations -------------------------------------------------------

lookupDeclAst :: Ctx -> Key -> Maybe Decl
lookupDeclAst ctx (path, name) = do
  lm <- Map.lookup path (progModules (ctxProg ctx))
  let match d = case declF d of
        DValue n _ _ _ -> n == name
        DFunction n _ _ _ _ -> n == name
        _ -> False
  case filter match (moduleDecls (lmModule lm)) of
    (d : _) -> Just d
    [] -> Nothing

-- | Type of a top-level declaration, elaborating it if needed. For
-- recursive references, falls back to the annotation-derived header
-- type.
declType :: Ctx -> Key -> TC Type
declType ctx key = do
  done <- gets stDecls
  case Map.lookup key done of
    Just cd -> pure (cdType cd)
    Nothing -> do
      active <- gets stActive
      if key `Set.member` active
        then snd <$> headerType ctx key
        else cdType <$> demandDecl ctx key

-- | 'show' into 'Text', for counts inside diagnostics.
tshowInt :: Int -> Text
tshowInt = T.pack . show

-- | The type parameters a declaration binds (spec 4.2). A name that
-- is also a type alias visible here is a duplicate definition: a type
-- name means one thing throughout a declaration.
typeVarsOf :: Ctx -> FilePath -> [Spanned Text] -> TC [Text]
typeVarsOf ctx path tps = do
  mapM_ distinct tps
  pure [v | Spanned _ v <- tps]
  where
    distinct (Spanned vsp v)
      | Just _ <- Map.lookup path (ctxScopes ctx) >>= Map.lookup v . gsTypes =
          abort . diag ENameDuplicate vsp $
            "type parameter '" <> v <> "' has the name of a type alias in scope"
      | length [() | Spanned _ w <- tps, w == v] > 1 =
          abort (diag ENameDuplicate vsp ("duplicate type parameter: '" <> v <> "'"))
      | otherwise = pure ()

-- | Header type from annotations only (recursion support).
headerType :: Ctx -> Key -> TC ([Text], Type)
headerType ctx key@(path, name) = case lookupDeclAst ctx key of
  Just (Decl sp f) -> case f of
    DFunction _ tps ps (Just rt) _ -> do
      vs <- typeVarsOf ctx path tps
      let ctx' = withTypeVars (Set.fromList vs) ctx
      posTys <- paramHeaderTypes ctx' path ps
      (,) vs . TyFun posTys <$> typeFromS ctx' path rt
    DValue _ _ (Just t) _ -> (,) [] <$> typeFromS ctx path t
    _ ->
      abort . diag ETypeMismatch sp $
        "recursive declaration '" <> name <> "' needs a return type annotation"
  Nothing -> abort (diag ENameUndefined NoSpan ("internal: missing declaration " <> name))

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

demandDecl :: Ctx -> Key -> TC CoreDecl
demandDecl ctx key@(path, name) = do
  done <- gets stDecls
  case Map.lookup key done of
    Just cd -> pure cd
    Nothing -> do
      d <- maybe (abort (diag ENameUndefined NoSpan ("internal: missing declaration " <> name))) pure (lookupDeclAst ctx key)
      modify (\s -> s {stActive = Set.insert key (stActive s)})
      cd <- elabDecl ctx path d
      modify $ \s ->
        s
          { stActive = Set.delete key (stActive s),
            stDecls = Map.insert key cd (stDecls s)
          }
      pure cd

elabDecl :: Ctx -> FilePath -> Decl -> TC CoreDecl
elabDecl ctx0 path (Decl sp f) = case f of
  DFunction name tps ps rt body -> do
    vs <- typeVarsOf ctx0 path tps
    let ctx = withTypeVars (Set.fromList vs) ctx0
    (lam, ty, params) <- elabLambda ctx path Map.empty sp (Just name) ps rt body
    pure (CoreDecl path name vs ty lam (Just params))
  DValue name sec ann rhs -> do
    let ctx = ctx0
    annTy <- traverse (typeFromS ctx path) ann
    case exprF rhs of
      -- A directly lambda-valued binding keeps declaration parameter
      -- info (callable with keyword arguments, spec 7.5).
      ELambda ps rt body -> do
        (lam, ty, params) <- elabLambda ctx path Map.empty (exprSpan rhs) (Just name) ps rt body
        -- A function-typed binding can never be a secret (6.10 allows
        -- only String); report that before the annotation check.
        _ <- applySecrecy sp name sec ty lam
        case annTy of
          Just t | not (conformsTo ty t) -> mismatch sp t ty
          Just t -> pure (CoreDecl path name [] t lam (Just params))
          Nothing -> pure (CoreDecl path name [] ty lam (Just params))
      _ -> do
        (core, ty) <- case annTy of
          Just t -> (,) <$> check ctx path Map.empty rhs t <*> pure t
          Nothing -> infer ctx path Map.empty rhs
        when (ty == TyVoid) $
          abort (diag ETypeIllformed sp "a Void value cannot be bound at top level")
        core' <- applySecrecy sp name sec ty core
        pure (CoreDecl path name [] ty core' Nothing)
  _ -> abort (diag ENameUndefined sp "internal: not a value declaration")

-- Secret bindings (spec 6.10) ---------------------------------------------------

-- | Wrap a core expression in a @mark_secret@ application, the
-- desugaring target of @!!@ (spec 6.10). The value itself is
-- unchanged; this only registers it for log masking (12.8).
markSecretCall :: Span -> Core -> Core
markSecretCall sp inner =
  Core sp (CApp (Core sp (CVar (BuiltinRef "mark_secret"))) [inner] [])

-- | Apply the @!!@ marker to a binding whose type is now known.
-- @!!@ is permitted only on @String@ bindings (spec 6.10).
applySecrecy :: Span -> Text -> Secrecy -> Type -> Core -> TC Core
applySecrecy _ _ Public _ core = pure core
applySecrecy sp name Secret ty core = do
  checkSecretType sp name ty
  pure (markSecretCall sp core)

checkSecretType :: Span -> Text -> Type -> TC ()
checkSecretType sp name ty =
  unless (ty == TyString) $
    abort . diag ETypeSecretNonString sp $
      "'" <> name <> "!!' marks a secret binding, which must be String, but its type is "
        <> renderType ty

-- | Rebind each @!!@-marked parameter through @mark_secret@ at the top
-- of the function body (spec 6.10). Done on the surface body, before
-- it is elaborated, so the rebinding goes through the ordinary block
-- and shadowing rules.
wrapSecretParams :: [Text] -> Expr -> Expr
wrapSecretParams [] body = body
wrapSecretParams names body =
  Expr sp (EDo (Block sp (map bind names <> [Stmt sp (SExpr body)])))
  where
    sp = exprSpan body
    bind n = Stmt sp (SBind n Public Nothing (call n))
    call n =
      Expr sp (ECall (Expr sp (EVar "mark_secret")) [Arg sp (APos (Expr sp (EVar n)))])

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
        Nothing -> abort (diag ENameUndefined sp ("undefined namespace: '" <> ns <> "'"))
  case Map.lookup searchPath (ctxScopes ctx) >>= Map.lookup n . gsTypes of
    Just (TBuiltinAlias "Error") -> noArgs >> pure errorType
    Just (TBuiltinAlias "CommandResult") -> noArgs >> pure commandResultType
    Just (TBuiltinAlias other) ->
      abort (diag ENameUndefined sp ("internal: unknown builtin alias " <> other))
    -- A parameterised alias is elaborated once with its parameters
    -- standing for themselves, and each reference substitutes its type
    -- arguments into that body (spec 4.2).
    Just (TAlias defPath defName) -> do
      (params, body) <- do
        cached <- gets stAliases
        case Map.lookup (defPath, defName) cached of
          Just pb -> pure pb
          Nothing -> do
            (tps, rhs) <- aliasRhs defPath defName
            vs <- typeVarsOf ctx defPath tps
            t <- typeFromS (withTypeVars (Set.fromList vs) ctx) defPath rhs
            modify (\s -> s {stAliases = Map.insert (defPath, defName) (vs, t) (stAliases s)})
            pure (vs, t)
      unless (length args == length params) $
        abort . diag ETypeArity sp $
          "'"
            <> n
            <> "' takes "
            <> tshowInt (length params)
            <> " type arguments, but "
            <> tshowInt (length args)
            <> " were given"
      pure (applySubst (Map.fromList (zip params args)) body)
    Nothing -> abort (diag ENameUndefined sp ("undefined type: '" <> n <> "'"))
  where
    noArgs =
      unless (null args) $
        abort (diag ETypeArity sp ("'" <> n <> "' takes no type arguments"))
    aliasRhs defPath defName =
      case Map.lookup defPath (progModules (ctxProg ctx)) of
        Just lm ->
          case [(tps, t) | Decl _ (DTypeAlias a tps t) <- moduleDecls (lmModule lm), a == defName] of
            (r : _) -> pure r
            [] -> abort (diag ENameUndefined sp ("undefined type: '" <> n <> "'"))
        Nothing -> abort (diag ENameUndefined sp ("undefined type: '" <> n <> "'"))

-- Lambdas and parameters ----------------------------------------------------------

-- | Elaborate a lambda or function declaration: computes static
-- parameter info, transforms early returns in the body, and infers or
-- checks the return type.
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
elabLambda ctx path locals sp mName ps retAnn body = do
  (params, kwDefaults, bodyLocals, secretParams) <- elabParams ctx path locals ps
  retTy <- traverse (typeFromS ctx path) retAnn
  -- Wrapped after the early-return transform so the rebindings sit in
  -- a plain, return-free block (spec 6.10 desugaring).
  body' <- wrapSecretParams secretParams <$> lift (transformFunctionBody body)
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
lambdaName (Span (Position _ l c) _) =
  "<lambda@" <> T.pack (show l) <> ":" <> T.pack (show c) <> ">"
lambdaName NoSpan = "<lambda>"

-- | Elaborate the parameter list: types, keyword defaults (evaluated
-- in the scope of preceding parameters, spec 8.3), the body scope, and
-- the names of @!!@-marked parameters (spec 6.10), whose types have
-- been validated here and which the caller rebinds in the body.
elabParams ::
  Ctx ->
  FilePath ->
  Locals ->
  [Param] ->
  TC (StaticParams, [(Text, Core)], Locals, [Text])
elabParams ctx path outer = go [] Nothing [] [] [] outer
  where
    go pos var kws defaults secrets locals [] =
      pure
        ( StaticParams (reverse pos) var (reverse kws),
          reverse defaults,
          Map.union locals outer,
          reverse secrets
        )
    go pos var kws defaults secrets locals (Param psp f : rest) = case f of
      PPositional n sec ann -> do
        t <- maybe (pure TyAny) (typeFromS ctx path) ann
        checkParamType psp t
        secrets' <- addSecret psp n sec t secrets
        go ((n, t) : pos) var kws defaults secrets' (Map.insert n t locals) rest
      PVariadic n ann -> do
        t <- maybe (pure (TyArray TyAny)) (typeFromS ctx path) ann
        elemTy <- case t of
          TyArray e -> pure e
          _ -> abort (diag ETypeIllformed psp "variadic parameter type must be Array<T>")
        go pos (Just (n, elemTy)) kws defaults secrets (Map.insert n t locals) rest
      PKeyword n sec ann dflt -> do
        (dCore, dTy) <- case ann of
          Just a -> do
            t <- typeFromS ctx path a
            checkParamType psp t
            c <- check ctx path (Map.union locals outer) dflt t
            pure (c, t)
          Nothing -> infer ctx path (Map.union locals outer) dflt
        secrets' <- addSecret psp n sec dTy secrets
        go pos var ((n, dTy) : kws) ((n, dCore) : defaults) secrets' (Map.insert n dTy locals) rest

    addSecret _ _ Public _ secrets = pure secrets
    addSecret psp n Secret t secrets = do
      checkSecretType psp n t
      pure (n : secrets)

    checkParamType psp t =
      when (t == TyVoid) $
        abort (diag ETypeIllformed psp "Void cannot be a parameter type")

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
  ELambda ps rt body -> do
    (lam, ty, _) <- elabLambda ctx path locals sp Nothing ps rt body
    pure (lam, ty)
  ECall fn args -> elabCall ctx path locals sp fn args Nothing
  EDot inner (Spanned fsp fld) -> elabDot ctx path locals sp inner fsp fld
  EIndex inner idx -> elabIndex ctx path locals sp inner idx
  EBin op a b -> elabBin ctx path locals sp op a b Nothing
  ENot inner -> do
    c <- check ctx path locals inner TyBool
    pure (Core sp (CNot c), TyBool)
  EDo block -> do
    (stmts, ty) <- elabBlock ctx path locals block Nothing
    pure (Core sp (CDo stmts), ty)
  EIf c t (Just el) -> do
    condCore <- check ctx path locals c TyBool
    -- Infer one branch and check the other against it, in either
    -- order: this lets context-typed calls (fail(e), cast) appear in
    -- one of the two branches (spec 6.4, 15.7).
    r <- tryTC (elabBlock ctx path locals t Nothing)
    (thenStmts, elseStmts, ty) <- case r of
      Right (thenStmts, thenTy) -> do
        (elseStmts, _) <- elabBlock ctx path locals el (Just thenTy)
        pure (thenStmts, elseStmts, thenTy)
      Left thenErr -> do
        r2 <- tryTC (elabBlock ctx path locals el Nothing)
        case r2 of
          Right (elseStmts, elseTy) -> do
            (thenStmts, _) <- elabBlock ctx path locals t (Just elseTy)
            pure (thenStmts, elseStmts, elseTy)
          Left _ -> abort thenErr
    pure
      ( Core sp (CIf condCore (Core (blockSpan t) (CDo thenStmts)) (Core (blockSpan el) (CDo elseStmts))),
        ty
      )
  EIf _ _ Nothing ->
    abort (diag ESyntaxReturnPosition sp "if without else is not allowed here")
  EFor (Spanned xsp x) xs body -> do
    (xsCore, xsTy) <- infer ctx path locals xs
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
  ECase scrut arms -> elabCase ctx path locals sp scrut arms Nothing
  ETry body mCatch mFin -> elabTry ctx path locals sp body mCatch mFin Nothing
  EAsync inner -> do
    (c, t) <- infer ctx path locals inner
    let thunk = mkThunk path sp c t
    pure (Core sp (CApp (Core sp (CVar (BuiltinRef "spawn"))) [thunk] []), TyAsync t)
  EAwait inner -> do
    (c, t) <- infer ctx path locals inner
    case t of
      TyAsync r -> pure (Core sp (CAwait c), r)
      other -> mismatch (exprSpan inner) (TyAsync TyAny) other
  ECommand stream env parts -> elabCommand ctx path locals sp stream env parts
  EEnv h args -> elabEnv ctx path locals sp h args

spanStart :: Span -> Position
spanStart (Span s _) = s
spanStart NoSpan = Position "" 0 0

mkThunk :: FilePath -> Span -> Core -> Type -> Core
mkThunk path sp body ty =
  Core sp . CLam $
    Lam
      { lamName = lambdaName sp,
        lamModule = path,
        lamPositional = [],
        lamVariadic = Nothing,
        lamKeywords = [],
        lamBody = body,
        lamType = TyFun [] ty
      }

check :: Ctx -> FilePath -> Locals -> Expr -> Type -> TC Core
check ctx path locals e@(Expr sp f) expected = case f of
  EArray es | TyArray t <- expected -> do
    cs <- mapM (\el -> check ctx path locals el t) es
    pure (Core sp (CArray cs))
  EObject kvs -> checkObject ctx path locals sp kvs expected
  ELambda ps rt body
    | TyFun expPs _ <- expected -> do
        (lam, ty, _) <- elabLambdaAgainst ctx path locals sp ps rt body expPs
        unless (conformsTo ty expected) (mismatch sp expected ty)
        pure lam
  EIf c t (Just el) | expected /= TyAny -> do
    condCore <- check ctx path locals c TyBool
    (thenStmts, _) <- elabBlock ctx path locals t (Just expected)
    (elseStmts, _) <- elabBlock ctx path locals el (Just expected)
    pure (Core sp (CIf condCore (Core (blockSpan t) (CDo thenStmts)) (Core (blockSpan el) (CDo elseStmts))))
  EDo block | expected /= TyAny -> do
    (stmts, _) <- elabBlock ctx path locals block (Just expected)
    pure (Core sp (CDo stmts))
  ECase scrut arms | expected /= TyAny -> do
    (c, _) <- elabCase ctx path locals sp scrut arms (Just expected)
    pure c
  ETry body mCatch mFin | expected /= TyAny -> do
    (c, _) <- elabTry ctx path locals sp body mCatch mFin (Just expected)
    pure c
  ECall fn args -> do
    (c, t) <- elabCall ctx path locals sp fn args (Just expected)
    unless (conformsTo t expected) (mismatch sp expected t)
    pure c
  EVar n
    | not (Map.member n locals),
      Just (VTopLevel defPath defName) <- lookupValueTarget ctx path n -> do
        -- A declaration with type parameters, referenced as a value:
        -- the expected type has to determine every one of them (4.4).
        t <- declType ctx (defPath, defName)
        t' <-
          if settled ctx t
            then pure t
            else do
              subst <- unifyOrFail sp t expected Map.empty
              pure (applySubst subst t)
        unless (settled ctx t' && conformsTo t' expected) (mismatch sp expected t')
        recordVar sp n t' (Just (defPath, defName))
        pure (Core sp (CVar (TopRef defPath defName)))
  EVar n
    | not (Map.member n locals),
      Just (VBuiltin bn) <- lookupValueTarget ctx path n,
      Just scheme <- Map.lookup bn builtinSchemes,
      not (null (schemeVars scheme)) -> do
        -- Builtin polymorphic function as a value: instantiate from
        -- the expected type (spec 4.4).
        subst <- unifyOrFail sp (schemeType scheme) expected Map.empty
        let t = applySubst subst (schemeType scheme)
        unless (all (`Map.member` subst) (schemeVars scheme) && conformsTo t expected) $
          mismatch sp expected t
        recordBuiltin sp n t bn
        pure (Core sp (CVar (BuiltinRef bn)))
  EBin op a b | isEqOp op || expected == TyBool -> do
    (c, t) <- elabBin ctx path locals sp op a b (Just expected)
    unless (conformsTo t expected) (mismatch sp expected t)
    pure c
  _ -> do
    (c, t) <- infer ctx path locals e
    unless (conformsTo t expected) (mismatch sp expected t)
    pure c
  where
    isEqOp OpEq = True
    isEqOp OpNe = True
    isEqOp _ = False

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
  TC (Core, Type, StaticParams)
elabLambdaAgainst ctx path locals sp ps rt body expPs = do
  let positionals = [p | p@(Param _ (PPositional _ _ _)) <- ps]
  ps' <-
    if length positionals == length expPs && length positionals == length ps
      then pure (zipWith adopt ps expPs)
      else pure ps
  elabLambda ctx path locals sp Nothing ps' rt body
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

inferVar :: Ctx -> FilePath -> Locals -> Span -> Text -> TC (Core, Type)
inferVar ctx path locals sp n = case Map.lookup n locals of
  Just t -> do
    recordVar sp n t Nothing
    pure (Core sp (CVar (LocalRef n)), t)
  Nothing -> case lookupValueTarget ctx path n of
    Just (VTopLevel defPath defName) -> do
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
      Nothing -> abort (diag ENameUndefined sp ("undefined name: '" <> n <> "'"))
    Nothing -> abort (diag ENameUndefined sp ("undefined name: '" <> n <> "'"))

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
    part (TPInterp e) = do
      (c, t) <- infer ctx path locals e
      unless (stringifiable t) $
        abort (diag ETypeMismatch (exprSpan e) ("cannot interpolate a value of type " <> renderType t))
      pure (CPExpr c)

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
  other ->
    abort . withExpectedActual (renderType other) "object literal" $
      diag ETypeMismatch sp "an object literal cannot have this expected type"
  where
    renderKeys ks = "{" <> T.intercalate ", " (Set.toList ks) <> "}"

-- Accessors -------------------------------------------------------------------------

elabDot :: Ctx -> FilePath -> Locals -> Span -> Expr -> Span -> Text -> TC (Core, Type)
elabDot ctx path locals sp inner fsp fld = case exprF inner of
  EVar m
    | not (Map.member m locals),
      Nothing <- lookupValueTarget ctx path m,
      Just key <- namespaceTarget m -> do
        -- Namespace member (resolution rank 4, spec 7.2), followed to
        -- its declaration when the module re-exports it.
        let target@(defPath, defName) = namespaceMember (ctxScopes ctx) key fld
        t <- declType ctx target
        recordVar fsp fld t (Just target)
        pure (Core sp (CVar (TopRef defPath defName)), t)
  _ -> do
    (c, t) <- infer ctx path locals inner
    case t of
      TyRecord fields -> case Map.lookup fld fields of
        Just f -> pure (Core sp (CDot c fld), readFieldType f)
        Nothing ->
          abort (diag ETypeAccess fsp ("record has no field '" <> fld <> "': " <> renderType t))
      other ->
        abort (diag ETypeAccess fsp ("field access requires a Record type, got " <> renderType other))
  where
    namespaceTarget m =
      Map.lookup path (ctxScopes ctx) >>= Map.lookup m . gsNamespaces

elabIndex :: Ctx -> FilePath -> Locals -> Span -> Expr -> Expr -> TC (Core, Type)
elabIndex ctx path locals sp inner idx = do
  (c, t) <- infer ctx path locals inner
  case t of
    TyArray elemTy -> do
      i <- check ctx path locals idx TyNumber
      pure (Core sp (CIndex IdxArray c i), elemTy)
    TyMap valTy -> do
      i <- check ctx path locals idx TyString
      pure (Core sp (CIndex IdxMap c i), valTy)
    TyRecord fields -> case literalString idx of
      Just k -> case Map.lookup k fields of
        Just f ->
          pure (Core sp (CDot c k), readFieldType f)
        Nothing ->
          abort (diag ETypeAccess (exprSpan idx) ("record has no field '" <> k <> "'"))
      Nothing ->
        abort (diag ETypeAccess (exprSpan idx) "record index must be a string literal")
    other ->
      abort (diag ETypeAccess sp ("index access requires Array, Map or Record, got " <> renderType other))
  where
    literalString (Expr _ (EString [TPChunk _ t])) = Just t
    literalString (Expr _ (EString [])) = Just ""
    literalString _ = Nothing

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
  OpPipeR -> elabCall ctx path locals sp b [Arg (exprSpan a) (APos a)] mExpected
  OpPipeL -> elabCall ctx path locals sp a [Arg (exprSpan b) (APos b)] mExpected
  -- f >> g  ==  \(x) -> g(f(x))   /   f << g  ==  \(x) -> f(g(x))
  OpCompR -> compose a b
  OpCompL -> compose b a
  OpNot -> abort (diag ETypeMismatch sp "internal: '!' is not a binary operator")
  where
    arith p = do
      ca <- check ctx path locals a TyNumber
      cb <- check ctx path locals b TyNumber
      pure (Core sp (CBin p ca cb), TyNumber)
    compare' p = do
      ca <- check ctx path locals a TyNumber
      cb <- check ctx path locals b TyNumber
      pure (Core sp (CBin p ca cb), TyBool)
    -- One side has to fit where the other is, and the wider of the two
    -- -- the one conformed to -- has to be comparable (spec 6.2). For
    -- two non-union types that is the same as requiring them equal.
    equality p = do
      (ca, ta) <- infer ctx path locals a
      (cb, tb) <- infer ctx path locals b
      wider <-
        if conformsTo ta tb
          then pure tb
          else
            if conformsTo tb ta
              then pure ta
              else mismatch sp ta tb
      unless (comparable wider) $
        abort (diag ETypeMismatch sp ("values of type " <> renderType wider <> " cannot be compared with ==/!="))
      pure (Core sp (CBin p ca cb), TyBool)
    logical ctor = do
      ca <- check ctx path locals a TyBool
      cb <- check ctx path locals b TyBool
      pure (Core sp (ctor ca cb), TyBool)
    compose f g = do
      (cf, tf) <- infer ctx path locals f
      (cg, tg) <- infer ctx path locals g
      case (tf, tg) of
        (TyFun [ta] tb, TyFun [tb'] tc) | tb == tb' -> do
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
          Just u | not (conformsTo t u) -> mismatch ssp u t
          _ -> pure ([CSBind n c], t)
      SExpr e -> do
        (c, t) <- inferOrCheck locals e
        pure ([CSExpr c], t)
      SReturn _ -> returnErr
      SGuard _ _ -> returnErr
    go locals (Stmt ssp f : rest) = case f of
      SBind n sec ann e -> do
        (c, t) <- elabBind locals ssp n sec ann e (infer ctx path locals e)
        (cs, ty) <- go (Map.insert n t locals) rest
        pure (CSBind n c : cs, ty)
      SExpr e -> do
        (c, _) <- infer ctx path locals e
        (cs, ty) <- go locals rest
        pure (CSExpr c : cs, ty)
      SReturn _ -> returnErr
      SGuard _ _ -> returnErr

    -- An annotation is the expected type for the right-hand side and
    -- the declared type of the binding (spec 4.3, 6.5); without one,
    -- the binding adopts the type of its right-hand side.
    elabBind locals ssp n sec ann e unannotated = do
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

    returnErr =
      abort (diag ESyntaxReturnPosition bsp "return is not allowed in this position")

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
  TC (Core, Type)
elabCase ctx path locals sp mScrut arms mExpected = do
  (matchArms, elseBody) <- splitCaseArms sp arms
  duplicateHeads [h | (ValueHeads hs, _) <- matchArms, h <- hs]
  (mBind, conds, narrows, taken) <- case mScrut of
    Nothing -> do
      cs <- mapM (boolCond . fst) matchArms
      pure (Nothing, cs, map (const plain) matchArms, [])
    Just scrut -> do
      (scrutCore, scrutTy) <- infer ctx path locals scrut
      let name = caseScrutName sp
          scrutVar = Core (exprSpan scrut) (CVar (LocalRef name))
      -- Only an equality head needs the scrutinee to be comparable
      -- (spec 6.2, 6.4); a case that only dispatches on types does not.
      when (any (isValueHeads . fst) matchArms && not (comparable scrutTy)) $
        abort . diag ETypeMismatch (exprSpan scrut) $
          "values of type " <> renderType scrutTy <> " cannot be matched by case"
      parts <- mapM (armPart scrutTy name scrutVar) matchArms
      duplicateTypeHeads [m | (_, _, ms) <- parts, m <- ms]
      pure
        ( Just (name, scrutCore),
          [c | (c, _, _) <- parts],
          [n | (_, n, _) <- parts],
          [m | (_, _, ms) <- parts, m <- ms]
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
        pure (anyOf conds, narrowTo scrutTy members, takenHere)
      TypeHeads sts -> do
        ms <- mapM (typeHead scrutTy) sts
        let conds = [Core (stypeSpan st) (CIsType scrutVar m) | (st, m) <- zip sts ms]
        pure (anyOf conds, narrowTo scrutTy ms, ms)

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
    -- record narrowed to Map<T> reaches the body as a map.
    narrowBind n declared narrowed
      | narrowed == declared = plain
      | otherwise = (Map.insert n narrowed locals, wrap)
      where
        wrap body = Core (coreSpan body) (CDo [CSBind n rhs, CSExpr body])
        scrutVar = Core sp (CVar (LocalRef (caseScrutName sp)))
        rhs = case narrowed of
          TyUnion _ -> scrutVar
          _ -> Core sp (CCast scrutVar narrowed)

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

    -- All arm bodies share one type. Without an expected type, it
    -- comes from the first arm that infers on its own, and every other
    -- arm is checked against it, so a context-typed call such as
    -- @fail(e)@ or @cast@ may sit in any arm (spec 6.4, 15.7). Arms
    -- that fail to infer commit nothing ('tryTC'), so checking them
    -- afterwards elaborates each arm exactly once.
    elabBodies bodies = case mExpected of
      Just t | t /= TyAny -> do
        cs <- mapM (`checkArm` t) bodies
        pure (cs, t)
      _ -> inferFirst [] Nothing bodies

    checkArm ((lcls, wrap), b) t = wrap <$> check ctx path lcls b t

    -- When no arm infers, the first arm's diagnostic is the report.
    inferFirst _ mFirstErr [] = case mFirstErr of
      Just d -> abort d
      Nothing -> abort (caseElse sp "a case expression requires an else arm")
    inferFirst pending mFirstErr (a@((lcls, wrap), b) : rest) = do
      r <- tryTC (infer ctx path lcls b)
      case r of
        Right (c, t) -> do
          before <- mapM (`checkArm` t) (reverse pending)
          after <- mapM (`checkArm` t) rest
          pure (before <> (wrap c : after), t)
        Left d -> inferFirst (a : pending) (maybe (Just d) Just mFirstErr) rest

    -- Two type heads denoting the same type make the later arm
    -- unreachable (spec 6.4).
    duplicateTypeHeads = goTy Set.empty
      where
        goTy _ [] = pure ()
        goTy seen (m : rest)
          | Set.member m seen =
              abort . diag ETypeCaseDuplicate sp $
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
  TC (Core, Type)
elabTry ctx path locals sp body mCatch mFin mExpected = do
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
          -- Infer the body and check the handler against it, or the
          -- other way round (context-typed calls like fail may appear
          -- in either, 15.7).
          r <- tryTC (elabBlock ctx path locals body Nothing)
          case r of
            Right (bodyStmts, bodyTy) -> do
              (hStmts, _) <- elabBlock ctx path handlerLocals handler (Just bodyTy)
              pure (coreOf body bodyStmts, coreOf handler hStmts, bodyTy)
            Left bodyErr -> do
              r2 <- tryTC (elabBlock ctx path handlerLocals handler Nothing)
              case r2 of
                Right (hStmts, hTy) -> do
                  (bodyStmts, _) <- elabBlock ctx path locals body (Just hTy)
                  pure (coreOf body bodyStmts, coreOf handler hStmts, hTy)
                Left _ -> abort bodyErr
      pure (recoverCall bodyCore (Just ename) handlerCore ty, ty)
    Nothing -> do
      (bodyStmts, bodyTy) <- elabBlock ctx path locals body mExpected
      pure (coreOf body bodyStmts, bodyTy)
  case mFin of
    Nothing -> pure (caught, caughtTy)
    Just fin -> do
      (finStmts, _) <- elabBlock ctx path locals fin Nothing
      let finCore = Core (blockSpan fin) (CDo finStmts)
          -- try X finally F ==
          --   do { v = recover(\() -> X, \(e) -> do { F; fail(e) }); F; v }
          e = "%e"
          v = "%v"
          rethrow =
            Core sp $
              CDo
                [ CSExpr finCore,
                  CSExpr (Core sp (CApp (builtin "fail") [Core sp (CVar (LocalRef e))] []))
                ]
          protected = recoverCallWith caught (Just e) rethrow caughtTy
          whole =
            Core sp $
              CDo
                [ CSBind v protected,
                  CSExpr finCore,
                  CSExpr (Core sp (CVar (LocalRef v)))
                ]
      pure (whole, caughtTy)
  where
    builtin n = Core sp (CVar (BuiltinRef n))
    coreOf blk stmts = Core (blockSpan blk) (CDo stmts)
    recoverCall bodyCore ename handlerCore ty = recoverCallWith bodyCore ename handlerCore ty
    recoverCallWith bodyCore ename handlerCore ty =
      let thunk = mkThunk path sp bodyCore ty
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
  (cmdCore, _) <- elabString ctx path locals sp parts
  (envCore, shownEnv, viaWords) <- case mEnv of
    Just envExpr -> do
      (c, t) <- infer ctx path locals envExpr
      unless (conformsTo t TyEnvironment) $
        abort . withExpectedActual "Environment" (renderType t) $
          diag ETypeCommandEnv (exprSpan envExpr) "command environment must be an Environment"
      pure (c, Nothing, [])
    Nothing -> do
      (c, ws) <- dispatchEnv ctx path sp parts
      pure (c, Just (renderEnvCore c), ws)
  recordCommandUse sp shownEnv viaWords
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

-- Command declarations and dispatch (spec ch. 5, 10.9) -----------------------

-- | Where a module's command words come from: its own declarations,
-- with their positions, and its imports and re-exports of command
-- words, with the module they name. In source order.
data CommandSite
  = SiteDecl Int [Spanned Text] Expr
  | SiteImport [Spanned Text] FilePath

moduleCommandSites :: Ctx -> FilePath -> [CommandSite]
moduleCommandSites ctx path = case Map.lookup path (progModules (ctxProg ctx)) of
  Nothing -> []
  Just lm ->
    [ site
    | (i, Decl _ f) <- zip [0 ..] (moduleDecls (lmModule lm)),
      site <- case f of
        DCommand ns e -> [SiteDecl i ns e]
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
  unless (conformsTo t TyEnvironment) $
    abort . withExpectedActual "Environment" (renderType t) $
      diag ETypeCommandEnv (exprSpan e) "the environment of a command declaration must be an Environment"
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
      CVar (TopRef p n)
        | not (Set.member (p, n) seen) -> do
            cd <- demandDecl ctx (p, n)
            walk (Set.insert (p, n) seen) (Just (p, n)) (cdCore cd)
      _ -> pure (maybe (SrcDecl path idx) SrcBinding lastBinding)

    literalEnv c = case coreF c of
      CEnv _ args -> all (literal . snd) args
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
  CStrLit t -> "\"" <> t <> "\""
  CNumber n -> T.pack (show n)
  CBool b -> if b then "true" else "false"
  CNull -> "null"
  CArray es -> "[" <> T.intercalate "," (map envKey es) <> "]"
  CMapLit kvs -> "{" <> T.intercalate "," [k <> ":" <> envKey v | (k, v) <- sortOn fst kvs] <> "}"
  other -> T.pack (show other)

-- | The module's command words and the environments they name, built
-- once per module and then cached.
commandTable :: Ctx -> FilePath -> TC (Map Text CommandEntry)
commandTable ctx path = do
  cached <- gets stCommands
  case Map.lookup path cached of
    Just table -> pure table
    Nothing -> do
      modify (\st -> st {stCommandsBuilding = Set.insert path (stCommandsBuilding st)})
      table <- buildCommandTable ctx path
      modify $ \st ->
        st
          { stCommands = Map.insert path table (stCommands st),
            stCommandsBuilding = Set.delete path (stCommandsBuilding st)
          }
      pure table

buildCommandTable :: Ctx -> FilePath -> TC (Map Text CommandEntry)
buildCommandTable ctx path = foldM addSite Map.empty (moduleCommandSites ctx path)
  where
    addSite tbl (SiteDecl i names envExpr) = do
      env <- declaredEnv ctx path envExpr
      src <- envSource ctx path i env
      foldM (addDeclared env src i) tbl names
    addSite tbl (SiteImport names key) = do
      target <- commandTable ctx key
      foldM (addImported target) tbl names

    addDeclared env src i tbl (Spanned sp n) = do
      unless (validCommandName n) $
        abort . diag ETypeCommandName sp $
          "'" <> n <> "' could never be recognized as a command word in a command string"
      when (Map.member n tbl) $ abort (duplicate sp n)
      pure (Map.insert n (CommandEntry sp env src (path, i)) tbl)

    -- The resolver has already checked that the target exports the
    -- word. The same declaration reached along two import paths is
    -- one entry; anything else under the same word is a duplicate.
    addImported target tbl (Spanned sp n) = case Map.lookup n target of
      Nothing -> abort (diag ENameUndefined sp ("module exports no command '" <> n <> "'"))
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
  case commandWords parts of
    NotAnalysable _ why ->
      abort . diag ETypeCommandNoEnv sp $
        "the command string could not be segmented (" <> why <> "), so no command word could be read; "
          <> "give the environment explicitly with $[...]"
    Analysed ws -> case [(w, e) | w <- ws, cwCandidate w, Just e <- [Map.lookup (cwText w) tbl]] of
      [] -> abort (diag ETypeCommandNoEnv sp (noneMessage ws))
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
-- lask runs, or both absolute; where they are not alike (a dependency
-- cache moved elsewhere by LASK_CACHE_DIR), the module-relative path is
-- kept whole, which the base directory joins to unchanged.
recipePath :: FilePath -> FilePath -> FilePath -> FilePath
recipePath base modulePath written
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

elabEnv :: Ctx -> FilePath -> Locals -> Span -> Text -> Maybe [Arg] -> TC (Core, Type)
elabEnv ctx path locals sp h mArgs = do
  argsOrdered <- traverse validateOrder mArgs
  case h of
    "local" -> do
      case argsOrdered of
        Just (_ : _) -> envErr "local() takes no arguments"
        _ -> pure ()
      pure (Core sp (CEnv "local" []), TyEnvironment)
    "docker" -> do
      args <- maybe (envErr "docker(...) requires an image reference or a recipe") pure argsOrdered
      let hasPositional = any (\(Arg _ af) -> case af of APos _ -> True; _ -> False) args
          -- A container option given null is left out: the one way an
          -- argument can say "not given" without giving up a value a
          -- caller might mean, such as "" (10.2). A list or a table
          -- says it by being empty, and leaves out its null elements
          -- and null values the same way.
          nullable t = mkUnion t [TyNull]
          text = nullable TyString
          number = nullable TyNumber
          switch = nullable TyBool
          list = TyArray (nullable TyString)
          table = TyMap (nullable TyString)
          optionals =
            [ ("image", TyString),
              ("dockerfile", TyString),
              ("context", TyString),
              ("build_args", TyMap TyString),
              -- Resource limits.
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
              ("platform", text),
              ("hostname", text),
              ("init", switch),
              -- Confinement: these narrow the boundary of 10.7.
              ("read_only", switch),
              ("tmpfs", list),
              ("cap_drop", list),
              -- Network.
              ("network", text),
              ("dns", list),
              ("dns_search", list),
              ("add_hosts", table),
              ("publish", list),
              -- Host filesystem beyond the base directory mount (10.5).
              ("volumes", list)
            ]
      named <-
        if hasPositional
          then bindEnvArgs "docker" [("image", TyString)] optionals args
          else bindEnvArgs "docker" [] optionals args
      validateDockerEnv named
      pure (Core sp (CEnv "docker" (map recipeArg named)), TyEnvironment)
    imageName -> case mArgs of
      -- #image-name sugar: docker("image-name") (spec 6.7).
      Nothing ->
        pure
          ( Core sp (CEnv "docker" [("image", Core sp (CStrLit imageName))]),
            TyEnvironment
          )
      -- An unknown environment kind is a static error (spec 10.4).
      Just _ -> envErr ("unknown environment kind: '" <> imageName <> "'")
  where
    envErr :: Text -> TC a
    envErr = abort . diag ETypeEnvConstruct sp

    -- A recipe path is written relative to the directory of the module
    -- that declares it (10.2), and is read — by the runtime, by
    -- `lask env build`, in the lock — relative to the program's base
    -- directory, since the value it ends up in no longer knows its
    -- module. It is rewritten here, where the module is known. A recipe
    -- beside the entry module keeps the path it was written with.
    recipeArg (k, c)
      | k `elem` ["dockerfile", "context"],
        CStrLit p <- coreF c =
          (k, c {coreF = CStrLit (T.pack (recipePath (progBaseDir (ctxProg ctx)) path (T.unpack p)))})
      | otherwise = (k, c)

    -- Positional arguments must precede named ones (spec 6.7).
    validateOrder args = do
      let go _ [] = pure ()
          go sawNamed (Arg _ af : rest) = case af of
            AKw _ _ -> go True rest
            APos _
              | sawNamed -> () <$ envErr "positional arguments must precede named arguments"
              | otherwise -> go False rest
      go False args
      pure args

    -- Bind positional-then-named args against a constructor signature.
    bindEnvArgs :: Text -> [(Text, Type)] -> [(Text, Type)] -> [Arg] -> TC [(Text, Core)]
    bindEnvArgs kind required optional args = do
      let (posArgs, kwArgs) = span (\(Arg _ af) -> case af of APos _ -> True; _ -> False) args
      when (length posArgs > length required) $
        () <$ envErr (kind <> "(...) has too many positional arguments")
      posBound <-
        mapM
          ( \((pname, pty), arg) -> case arg of
              Arg _ (APos e) -> do
                c <- checkEnvArg e pty
                pure (pname, c)
              Arg asp (AKw _ _) ->
                abort (diag ETypeEnvConstruct asp "positional arguments must precede named arguments")
          )
          (zip required posArgs)
      let boundNames = Set.fromList (map fst posBound)
          sigNamed = Map.fromList (required <> optional)
      kwBound <-
        foldM
          ( \acc arg -> case arg of
              Arg asp (AKw n e) -> do
                when (n `Set.member` Set.union boundNames (Set.fromList (map fst acc))) $
                  () <$ abort (diag ETypeEnvConstruct asp ("duplicate environment argument: '" <> n <> "'"))
                case Map.lookup n sigNamed of
                  Just pty -> do
                    c <- checkEnvArg e pty
                    pure (acc <> [(n, c)])
                  Nothing ->
                    abort (diag ETypeEnvConstruct asp ("unknown environment argument: '" <> n <> "'"))
              Arg asp (APos _) ->
                abort (diag ETypeEnvConstruct asp "positional arguments must precede named arguments")
          )
          []
          kwArgs
      let bound = posBound <> kwBound
          missing = [n | (n, _) <- required, n `notElem` map fst bound]
      unless (null missing) $
        () <$ envErr (kind <> "(...) is missing required argument: " <> T.intercalate ", " missing)
      pure bound

    -- A list or table option is declared with nullable elements, so
    -- that a literal can hold a null to leave out (10.2). Containers
    -- are invariant (4.4), so a value already typed as a list or table
    -- of strings would not conform to that; it holds no null to leave
    -- out, and is accepted as it is.
    checkEnvArg e ty = case ty of
      TyArray el | el == nullableText -> orPlain (TyArray TyString)
      TyMap el | el == nullableText -> orPlain (TyMap TyString)
      _ -> check ctx path locals e ty
      where
        nullableText = mkUnion TyString [TyNull]
        orPlain plain = do
          r <- tryTC (check ctx path locals e ty)
          case r of
            Right c -> pure c
            Left d -> either (const (abort d)) pure =<< tryTC (check ctx path locals e plain)

    coreStrLit (Core _ (CStrLit t)) = Just t
    coreStrLit _ = Nothing

    -- The image is given either as a registry reference or as a recipe
    -- (spec 10.2). Exactly one form; a registry reference carries a tag
    -- or digest; a recipe is a literal path inside the module tree.
    validateDockerEnv :: [(Text, Core)] -> TC ()
    validateDockerEnv named = do
      let present k = maybe False (const True) (lookup k named)
      case (present "image", present "dockerfile") of
        (True, True) ->
          () <$ envErr "docker(...) takes either an image reference or a 'dockerfile' recipe, not both"
        (False, False) ->
          () <$ envErr "docker(...) requires an image reference or a 'dockerfile' recipe"
        (True, False) -> do
          when (present "context") $
            () <$ envErr "'context' is only valid together with 'dockerfile'"
          when (present "build_args") $
            () <$ envErr "'build_args' is only valid together with 'dockerfile'"
          case lookup "image" named >>= coreStrLit of
            -- A runtime image value stays permitted here; whether the
            -- owning module may use one is a trust-domain rule (16.1).
            Nothing -> pure ()
            -- A reference without a tag names the repository's
            -- `latest`; the lock pins whatever that resolved to when it
            -- was materialized (10.3), so it cannot move under a run.
            Just img
              | T.null img -> () <$ envErr "the image reference must not be empty"
              | otherwise -> pure ()
        (False, True) -> do
          _ <- requireTreePath named "dockerfile"
          _ <- requireTreePath named "context"
          requireLiteralTable named "build_args"

    -- Build arguments decide which image a recipe builds, and are part
    -- of its hash (10.3), so they are read before anything runs: by
    -- `lask env build`, which has no evaluator to compute them with.
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

    requireTreePath :: [(Text, Core)] -> Text -> TC (Maybe Text)
    requireTreePath named key = case lookup key named of
      Nothing -> pure Nothing
      Just c -> case coreStrLit c of
        Nothing ->
          envErr ("'" <> key <> "' must be a string literal without interpolation")
        Just p
          | T.null p -> envErr ("'" <> key <> "' must not be empty")
          | isAbsolute (T.unpack p) ->
              envErr ("'" <> key <> "' must be a relative path inside the module tree")
          | ".." `elem` splitDirectories (normalise (T.unpack p)) ->
              envErr ("'" <> key <> "' must not escape the module tree")
          | otherwise -> pure (Just p)

-- Calls (spec 7.5) ------------------------------------------------------------------------

data Callee
  = -- | Statically resolved declaration or direct lambda: keyword
    -- arguments allowed, variadic collection applies. The type
    -- variables are those the declaration binds (spec 4.2), empty for
    -- a monomorphic one.
    CalleeStatic Core [Text] Type StaticParams
  | -- | Builtin with a type scheme.
    CalleeBuiltin Text Scheme
  | -- | Any other function-typed value: positional-only, exact arity.
    CalleeValue Core Type

elabCall :: Ctx -> FilePath -> Locals -> Span -> Expr -> [Arg] -> Maybe Type -> TC (Core, Type)
elabCall ctx path locals sp fn args mExpected = do
  validateArgOrder
  callee <- resolveCallee
  case callee of
    CalleeStatic fnCore tvs fnTy params -> do
      ret <- case fnTy of
        TyFun _ r -> pure r
        other -> abort (diag ETypeCall (exprSpan fn) ("cannot call a value of type " <> renderType other))
      (posCores, kwCores, retTy) <- bindStatic tvs ret params
      pure (Core sp (CApp fnCore posCores kwCores), retTy)
    CalleeBuiltin name scheme -> elabBuiltinCall name scheme
    CalleeValue fnCore fnTy -> case fnTy of
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
        | Map.member n locals -> valueCallee
        | otherwise -> case lookupValueTarget ctx path n of
            Just (VTopLevel p dn) -> staticFromDecl (p, dn)
            Just (VBuiltin bn) -> case Map.lookup bn builtinSchemes of
              Just scheme -> do
                recordBuiltin (exprSpan fn) bn (schemeType scheme) bn
                pure (CalleeBuiltin bn scheme)
              Nothing -> abort (diag ENameUndefined (exprSpan fn) ("undefined name: '" <> n <> "'"))
            Nothing -> abort (diag ENameUndefined (exprSpan fn) ("undefined name: '" <> n <> "'"))
      EDot (Expr _ (EVar m)) (Spanned _ fld)
        | not (Map.member m locals),
          Nothing <- lookupValueTarget ctx path m,
          Just key <- Map.lookup path (ctxScopes ctx) >>= Map.lookup m . gsNamespaces ->
            staticFromDecl (namespaceMember (ctxScopes ctx) key fld)
      ELambda ps rt body -> do
        (lam, ty, params) <- elabLambda ctx path locals (exprSpan fn) Nothing ps rt body
        pure (CalleeStatic lam [] ty params)
      _ -> valueCallee

    valueCallee = do
      (c, t) <- infer ctx path locals fn
      pure (CalleeValue c t)

    staticFromDecl key = do
      cd <- demandOrHeader key
      case cd of
        Just (core, tvs, ty, Just params) -> pure (CalleeStatic core tvs ty params)
        Just (core, _, ty, Nothing) -> pure (CalleeValue core ty)
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
          -- variables, which the header does carry.
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), vs, t, Nothing))
        else do
          cd <- demandDecl ctx key
          recordVar (exprSpan fn) n (cdType cd) (Just key)
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), cdTypeVars cd, cdType cd, cdParams cd))

    -- Static binding per 7.5 for declarations. A declaration that
    -- binds type variables (spec 4.2) is instantiated here, through
    -- the same two passes a built-in call uses, so a call of a generic
    -- declaration keeps keyword arguments and variadic collection.
    bindStatic tvs ret (StaticParams positional variadic keywords) = do
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
      when (length posExprs < nPos) $
        () <$ abort (diag ETypeArity sp ("missing positional arguments: expected " <> tshow nPos <> ", got " <> tshow (length posExprs)))
      let (bound, extra) = splitAt nPos posExprs
      when (isNothing variadic && not (null extra)) $
        () <$ abort (diag ETypeArity sp ("too many positional arguments: expected " <> tshow nPos <> ", got " <> tshow (length posExprs)))
      kwSlots <- keywordSlots keywords
      let (declVs, ren) = freshen tvs
          posSlots =
            zip bound (map (ren . snd) positional)
              <> [(e, ren elemTy) | Just (_, elemTy) <- [variadic], e <- extra]
      if null tvs
        then do
          posCores <- mapM (\(e, t) -> check ctx path locals e t) posSlots
          kwCores <- mapM (\(n, e, t) -> (,) n <$> check ctx path locals e t) kwSlots
          pure (posCores, kwCores, ret)
        else do
          -- Pre-bind from the expected return type, then let every
          -- argument -- positional, variadic and keyword alike --
          -- contribute, in no particular order (spec 4.4).
          let retPat = ren ret
              subst0 = case mExpected of
                Just expT -> either (const Map.empty) id (unifyE retPat expT Map.empty)
                Nothing -> Map.empty
          (cores, subst) <-
            goArgs subst0 (posSlots <> [(e, ren t) | (_, e, t) <- kwSlots])
          retTy <- instantiateRet calleeName declVs retPat subst
          let (posCores, kwCores) = splitAt (length posSlots) cores
          pure (posCores, zip [n | (n, _, _) <- kwSlots] kwCores, retTy)

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
              abort (diag ETypeKeyword asp ("unknown keyword argument: '" <> n <> "'"))
        go seen (Arg _ (APos _) : rest) = go seen rest -- unreachable (validated)

    goArgs subst argSlots = do
      (done, deferred, subst') <- firstPass subst Map.empty [] (zip [0 :: Int ..] argSlots)
      (done', subst'') <- retryPass subst' done deferred
      pure (Map.elems done', subst'')

    firstPass subst done deferred [] = pure (done, reverse deferred, subst)
    firstPass subst done deferred (slot@(i, (argExpr, pat)) : rest) = do
      let p = applySubst subst pat
      if settled ctx p
        then do
          c <- check ctx path locals argExpr p
          firstPass subst (Map.insert i c done) deferred rest
        else do
          r <- tryTC $ do
            (c, t) <- inferWithHint argExpr p
            subst' <- unifyOrFail (exprSpan argExpr) p t subst
            pure (c, subst')
          case r of
            Right (c, subst') -> firstPass subst' (Map.insert i c done) deferred rest
            -- Deferred rather than reported: another argument may
            -- yet make this position concrete, and if none does,
            -- the retry reports it against the type it ended with.
            Left _ -> firstPass subst done (slot : deferred) rest

    -- A retried argument whose position is now concrete is checked
    -- against it, which is what gives @cast@ its target. One still
    -- undetermined is elaborated as before, so its own diagnostic
    -- is the report.
    retryPass subst done [] = pure (done, subst)
    retryPass subst done ((i, (argExpr, pat)) : rest) = do
      let p = applySubst subst pat
      if settled ctx p
        then do
          c <- check ctx path locals argExpr p
          retryPass subst (Map.insert i c done) rest
        else do
          (c, t) <- inferWithHint argExpr p
          when (t == TyAny) (anyArgument (exprSpan argExpr) p)
          subst' <- unifyOrFail (exprSpan argExpr) p t subst
          retryPass subst' (Map.insert i c done) rest

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
      (ELambda ps rt body, TyFun expPs _)
        | all (settled ctx) expPs -> do
            (lam, ty, _) <- elabLambdaAgainst ctx path locals (exprSpan argExpr) ps rt body expPs
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
            s' <- unifyOrFail sp (applySubst subst ret) expT subst
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
      | name == "to_string" = case (posExprs, kwArgs) of
          ([arg], []) -> do
            (c, t) <- infer ctx path locals arg
            unless (stringifiable t) $
              abort . diag ETypeMismatch (exprSpan arg) $
                "'to_string' cannot render a value of type " <> renderType t
            pure (Core sp (CApp (Core sp (CVar (BuiltinRef name))) [c] []), TyString)
          _ -> abort (diag ETypeArity sp "to_string takes exactly one argument")
      | otherwise = do
          unless (null kwArgs) $
            () <$ abort (diag ETypeKeyword sp ("'" <> name <> "' takes no keyword arguments"))
          let (schemeVs, ren) = freshen (schemeVars scheme)
              params = map ren (schemeParams scheme)
              retPat = ren (schemeRet scheme)
          unless (length posExprs == length params) $
            () <$ abort (diag ETypeArity sp ("'" <> name <> "' expects " <> tshow (length params) <> " arguments, got " <> tshow (length posExprs)))
          -- Pre-bind type variables from the expected return type.
          let subst0 = case mExpected of
                Just expT -> either (const Map.empty) id (unifyE retPat expT Map.empty)
                Nothing -> Map.empty
          (cores, subst) <- goArgs subst0 (zip posExprs params)
          builtinSideCondition name sp (Map.mapKeys (T.takeWhile (/= '#')) subst)
          retTy <- instantiateRet name schemeVs retPat subst
          pure (Core sp (CApp (Core sp (CVar (BuiltinRef name))) cores []), retTy)
      where
        castNeedsType =
          abort (diag ETypeMismatch sp "cast requires an expected type from context")

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

-- | Restrictions a built-in's signature cannot state, checked against
-- the instantiated type variables at the call site (spec 15.4).
--
-- This is the same shape of rule as @==@ (6.2): the type system has
-- no constraints, so a polymorphic built-in that only works for some
-- element types has that condition checked where it is called.
builtinSideCondition :: Text -> Span -> Subst -> TC ()
builtinSideCondition name sp subst = case name of
  "sort" -> needs orderable "T" "ordered"
  "sort_by" -> needs orderable "U" "ordered"
  "contains_array" -> needs comparable "T" "compared"
  "index_of_array" -> needs comparable "T" "compared"
  "unique" -> needs comparable "T" "compared"
  _ -> pure ()
  where
    needs ok var verb = case Map.lookup var subst of
      Just t
        | not (ok t) ->
            () <$ abort (diag ETypeMismatch sp (message t verb))
      _ -> pure ()
    message t verb =
      "'" <> name <> "' cannot be used here: values of type "
        <> renderType t
        <> " cannot be "
        <> verb
        <> (if verb == "ordered" then " (only Number and String can)" else "")

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
