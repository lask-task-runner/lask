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
    CoreDecl (..),
    StaticParams (..),
    HoverInfo (..),
    elaborateProgram,
  )
where

import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT (runStateT), evalStateT, get, gets, lift, modify, put)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Sig
import Language.Lask.Core.AST
import Language.Lask.Diagnostic
import Language.Lask.Desugar.Return (transformFunctionBody)
import Language.Lask.ErrorCode
import Language.Lask.Lexer.Token (CmdStream (..), Op (..), Spanned (..))
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import Language.Lask.Module.Resolve (GlobalScope (..), TypeTarget (..), ValueTarget (..))
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Types

-- Program-level results ------------------------------------------------------

type Key = (FilePath, Text)

data CoreDecl = CoreDecl
  { cdModule :: FilePath,
    cdName :: Text,
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
    -- | Name references with their types, recorded during
    -- elaboration for editor tooling (hover).
    cpHover :: [HoverInfo]
  }
  deriving (Show)

-- | A resolved name occurrence: where it was written, what it is, and
-- (for top-level targets) which declaration it refers to.
data HoverInfo = HoverInfo
  { hiSpan :: Span,
    hiName :: Text,
    hiType :: Type,
    hiDecl :: Maybe Key
  }
  deriving (Show, Eq)

-- Elaboration monad ----------------------------------------------------------

data Ctx = Ctx
  { ctxProg :: Program,
    ctxScopes :: Map FilePath GlobalScope
  }

data St = St
  { stDecls :: Map Key CoreDecl,
    stAliases :: Map Key Type,
    stActive :: Set Key,
    stHover :: [HoverInfo]
  }

type TC = StateT St (Either Diagnostic)

-- | Record a resolved name occurrence for hover (editor tooling).
recordVar :: Span -> Text -> Type -> Maybe Key -> TC ()
recordVar sp n t d =
  modify (\s -> s {stHover = HoverInfo sp n t d : stHover s})

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
  case evalStateT (elabAll >> gets (\s -> (stDecls s, stHover s))) (St Map.empty Map.empty Set.empty []) of
    Left d -> Left [d]
    Right (decls, hover) ->
      Right
        CoreProgram
          { cpEntry = progEntry prog,
            cpBaseDir = progBaseDir prog,
            cpDecls = decls,
            cpHover = hover
          }
  where
    ctx = Ctx prog scopes
    elabAll =
      mapM_
        (demandDecl ctx)
        [ (lmPath lm, n)
        | lm <- Map.elems (progModules prog),
          Decl _ f <- moduleDecls (lmModule lm),
          n <- declValueName f
        ]
    declValueName (DValue n _ _) = [n]
    declValueName (DFunction n _ _ _) = [n]
    declValueName _ = []

-- Top-level declarations -------------------------------------------------------

lookupDeclAst :: Ctx -> Key -> Maybe Decl
lookupDeclAst ctx (path, name) = do
  lm <- Map.lookup path (progModules (ctxProg ctx))
  let match d = case declF d of
        DValue n _ _ -> n == name
        DFunction n _ _ _ -> n == name
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
        then headerType ctx key
        else cdType <$> demandDecl ctx key

-- | Header type from annotations only (recursion support).
headerType :: Ctx -> Key -> TC Type
headerType ctx key@(path, name) = case lookupDeclAst ctx key of
  Just (Decl sp f) -> case f of
    DFunction _ ps (Just rt) _ -> do
      posTys <- paramHeaderTypes ctx path ps
      TyFun posTys <$> typeFromS ctx path rt
    DValue _ (Just t) _ -> typeFromS ctx path t
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
    positionalOf (PPositional _ ann) = Just (maybe (pure TyAny) (typeFromS ctx path) ann)
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
elabDecl ctx path (Decl sp f) = case f of
  DFunction name ps rt body -> do
    (lam, ty, params) <- elabLambda ctx path Map.empty sp (Just name) ps rt body
    pure (CoreDecl path name ty lam (Just params))
  DValue name ann rhs -> do
    annTy <- traverse (typeFromS ctx path) ann
    case exprF rhs of
      -- A directly lambda-valued binding keeps declaration parameter
      -- info (callable with keyword arguments, spec 7.5).
      ELambda ps rt body -> do
        (lam, ty, params) <- elabLambda ctx path Map.empty (exprSpan rhs) (Just name) ps rt body
        case annTy of
          Just t | not (conformsTo ty t) -> mismatch sp t ty
          Just t -> pure (CoreDecl path name t lam (Just params))
          Nothing -> pure (CoreDecl path name ty lam (Just params))
      _ -> do
        (core, ty) <- case annTy of
          Just t -> (,) <$> check ctx path Map.empty rhs t <*> pure t
          Nothing -> infer ctx path Map.empty rhs
        when (ty == TyVoid) $
          abort (diag ETypeIllformed sp "a Void value cannot be bound at top level")
        pure (CoreDecl path name ty core Nothing)
  _ -> abort (diag ENameUndefined sp "internal: not a value declaration")

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
          addField acc (Spanned fsp k, t) = do
            when (Map.member k acc) $
              abort (diag ETypeFieldDuplicate fsp ("duplicate field: '" <> k <> "'"))
            t' <- go t
            pure (Map.insert k t' acc)
      SNamed n -> aliasType ctx path sp n

aliasType :: Ctx -> FilePath -> Span -> Text -> TC Type
aliasType ctx path sp n =
  case Map.lookup path (ctxScopes ctx) >>= Map.lookup n . gsTypes of
    Just (TBuiltinAlias "Error") -> pure errorType
    Just (TBuiltinAlias "CommandResult") -> pure commandResultType
    Just (TBuiltinAlias other) ->
      abort (diag ENameUndefined sp ("internal: unknown builtin alias " <> other))
    Just (TAlias defPath defName) -> do
      cached <- gets stAliases
      case Map.lookup (defPath, defName) cached of
        Just t -> pure t
        Nothing -> do
          rhs <- aliasRhs defPath defName
          t <- typeFromS ctx defPath rhs
          modify (\s -> s {stAliases = Map.insert (defPath, defName) t (stAliases s)})
          pure t
    Nothing -> abort (diag ENameUndefined sp ("undefined type: '" <> n <> "'"))
  where
    aliasRhs defPath defName =
      case Map.lookup defPath (progModules (ctxProg ctx)) of
        Just lm ->
          case [t | Decl _ (DTypeAlias a t) <- moduleDecls (lmModule lm), a == defName] of
            (t : _) -> pure t
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
  (params, kwDefaults, bodyLocals) <- elabParams ctx path locals ps
  retTy <- traverse (typeFromS ctx path) retAnn
  body' <- lift (transformFunctionBody body)
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
-- in the scope of preceding parameters, spec 8.3), and the body scope.
elabParams ::
  Ctx ->
  FilePath ->
  Locals ->
  [Param] ->
  TC (StaticParams, [(Text, Core)], Locals)
elabParams ctx path outer = go [] Nothing [] [] outer
  where
    go pos var kws defaults locals [] =
      pure
        ( StaticParams (reverse pos) var (reverse kws),
          reverse defaults,
          Map.union locals outer
        )
    go pos var kws defaults locals (Param psp f : rest) = case f of
      PPositional n ann -> do
        t <- maybe (pure TyAny) (typeFromS ctx path) ann
        checkParamType psp t
        go ((n, t) : pos) var kws defaults (Map.insert n t locals) rest
      PVariadic n ann -> do
        t <- maybe (pure (TyArray TyAny)) (typeFromS ctx path) ann
        elemTy <- case t of
          TyArray e -> pure e
          _ -> abort (diag ETypeIllformed psp "variadic parameter type must be Array<T>")
        go pos (Just (n, elemTy)) kws defaults (Map.insert n t locals) rest
      PKeyword n ann dflt -> do
        (dCore, dTy) <- case ann of
          Just a -> do
            t <- typeFromS ctx path a
            checkParamType psp t
            c <- check ctx path (Map.union locals outer) dflt t
            pure (c, t)
          Nothing -> infer ctx path (Map.union locals outer) dflt
        go pos var ((n, dTy) : kws) ((n, dCore) : defaults) (Map.insert n dTy locals) rest

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
    let recTy = TyRecord (Map.fromList [(k, t) | (k, _, t) <- fields])
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
            then ("forEach", TyVoid)
            else ("map", TyArray bodyTy)
    pure (Core sp (CApp (Core sp (CVar (BuiltinRef fnName))) [xsCore, bodyLam] []), resTy)
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
  ETry body mCatch mFin | expected /= TyAny -> do
    (c, _) <- elabTry ctx path locals sp body mCatch mFin (Just expected)
    pure c
  ECall fn args -> do
    (c, t) <- elabCall ctx path locals sp fn args (Just expected)
    unless (conformsTo t expected) (mismatch sp expected t)
    pure c
  EVar n
    | not (Map.member n locals),
      Just (VBuiltin bn) <- lookupValueTarget ctx path n,
      Just scheme <- Map.lookup bn builtinSchemes,
      not (null (schemeVars scheme)) -> do
        -- Builtin polymorphic function as a value: instantiate from
        -- the expected type (spec 4.4).
        subst <- unifyOrFail sp (schemeType scheme) expected Map.empty
        let t = applySubst subst (schemeType scheme)
        unless (isGround t && conformsTo t expected) (mismatch sp expected t)
        recordVar sp n t Nothing
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
  let positionals = [p | p@(Param _ (PPositional _ _)) <- ps]
  ps' <-
    if length positionals == length expPs && length positionals == length ps
      then pure (zipWith adopt ps expPs)
      else pure ps
  elabLambda ctx path locals sp Nothing ps' rt body
  where
    adopt (Param psp (PPositional n Nothing)) expTy =
      Param psp (PPositional n (Just (typeToS psp expTy)))
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
  TyRecord fs -> SRecord [(Spanned sp k, typeToS sp v) | (k, v) <- Map.toList fs]
  TyAsync e -> SAsyncHandle (typeToS sp e)
  TyFun psL r -> SFunction (map (typeToS sp) psL) (typeToS sp r)
  TyVar v -> SNamed v

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
      recordVar sp n t (Just (defPath, defName))
      pure (Core sp (CVar (TopRef defPath defName)), t)
    Just (VBuiltin "stdin") -> do
      recordVar sp n TyString Nothing
      pure (Core sp (CVar (BuiltinRef "stdin")), TyString)
    Just (VBuiltin bn) -> case Map.lookup bn builtinSchemes of
      Just scheme
        | null (schemeVars scheme) -> do
            recordVar sp n (schemeType scheme) Nothing
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
    part (TPChunk t) = pure (CPText t)
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
    unless (litKeys == expKeys) $
      abort . withExpectedActual (renderType expected) (renderKeys litKeys) $
        diag ETypeMismatch sp "object literal keys do not match the expected record fields"
    fields <-
      mapM
        ( \(Spanned _ k, v) -> do
            c <- check ctx path locals v (fieldTys Map.! k)
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
        -- Namespace member (resolution rank 4, spec 7.2).
        t <- declType ctx (key, fld)
        recordVar fsp fld t (Just (key, fld))
        pure (Core sp (CVar (TopRef key fld)), t)
  _ -> do
    (c, t) <- infer ctx path locals inner
    case t of
      TyRecord fields -> case Map.lookup fld fields of
        Just fieldTy -> pure (Core sp (CDot c fld), fieldTy)
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
        Just fieldTy ->
          pure (Core sp (CDot c k), fieldTy)
        Nothing ->
          abort (diag ETypeAccess (exprSpan idx) ("record has no field '" <> k <> "'"))
      Nothing ->
        abort (diag ETypeAccess (exprSpan idx) "record index must be a string literal")
    other ->
      abort (diag ETypeAccess sp ("index access requires Array, Map or Record, got " <> renderType other))
  where
    literalString (Expr _ (EString [TPChunk t])) = Just t
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
    equality p = do
      (ca, ta) <- infer ctx path locals a
      (cb, tb) <- infer ctx path locals b
      unless (ta == tb) (mismatch sp ta tb)
      unless (comparable ta) $
        abort (diag ETypeMismatch sp ("values of type " <> renderType ta <> " cannot be compared with ==/!="))
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
    go locals [Stmt _ f] = case f of
      SBind n e -> do
        (c, t) <- inferOrCheck locals e
        pure ([CSBind n c], t)
      SExpr e -> do
        (c, t) <- inferOrCheck locals e
        pure ([CSExpr c], t)
      SReturn _ -> returnErr
      SGuard _ _ -> returnErr
    go locals (Stmt _ f : rest) = case f of
      SBind n e -> do
        (c, t) <- infer ctx path locals e
        (cs, ty) <- go (Map.insert n t locals) rest
        pure (CSBind n c : cs, ty)
      SExpr e -> do
        (c, _) <- infer ctx path locals e
        (cs, ty) <- go locals rest
        pure (CSExpr c : cs, ty)
      SReturn _ -> returnErr
      SGuard _ _ -> returnErr

    inferOrCheck locals e = case mExpected of
      Just t -> do
        c <- check ctx path locals e t
        pure (c, t)
      Nothing -> infer ctx path locals e

    returnErr =
      abort (diag ESyntaxReturnPosition bsp "return is not allowed in this position")

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
  envArg <- case mEnv of
    Nothing -> pure []
    Just envExpr -> do
      (c, t) <- infer ctx path locals envExpr
      unless (conformsTo t TyEnvironment) $
        abort . withExpectedActual "Environment" (renderType t) $
          diag ETypeCommandEnv (exprSpan envExpr) "command environment must be an Environment"
      pure [("env", c)]
  let call = Core sp (CApp (Core sp (CVar (BuiltinRef "runCommand"))) [cmdCore] envArg)
  case stream of
    StreamAll -> pure (call, commandResultType)
    StreamOut -> pure (streamSelect call "stdout", TyString)
    StreamErr -> pure (streamSelect call "stderr", TyString)
  where
    -- do { r = runCommand(...);
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

-- Environment expressions (spec 6.7, 10.2) ---------------------------------------------------

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
      args <- maybe (envErr "docker(...) requires an image argument") pure argsOrdered
      named <- bindEnvArgs "docker" [("image", TyString)] [("memory", TyString), ("cpus", TyNumber)] args
      pure (Core sp (CEnv "docker" named), TyEnvironment)
    "env" -> do
      args <- maybe (envErr "env(...) requires a name argument") pure argsOrdered
      case args of
        [Arg _ (APos nameExpr)] -> case literalString nameExpr of
          Just name ->
            pure (Core sp (CEnv "env" [("name", Core (exprSpan nameExpr) (CStrLit name))]), TyEnvironment)
          Nothing -> envErr "the environment name must be a string literal without interpolation"
        _ -> envErr "env(...) takes exactly one name argument"
    "remote" -> envErr "remote environments can only be defined in the environment definition file"
    imageName -> case mArgs of
      -- #image-name sugar: docker("image-name") (spec 6.7).
      Nothing ->
        pure
          ( Core sp (CEnv "docker" [("image", Core sp (CStrLit imageName))]),
            TyEnvironment
          )
      Just _ ->
        abort . mkDiagnostic ESyntaxUnexpectedToken StageSyntax sp $
          "an environment argument list requires a kind name (local/docker/env)"
  where
    envErr :: Text -> TC a
    envErr = abort . diag ETypeEnvConstruct sp

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

    literalString (Expr _ (EString [TPChunk t])) = Just t
    literalString (Expr _ (EString [])) = Just ""
    literalString _ = Nothing

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

    checkEnvArg e ty = check ctx path locals e ty

-- Calls (spec 7.5) ------------------------------------------------------------------------

data Callee
  = -- | Statically resolved declaration or direct lambda: keyword
    -- arguments allowed, variadic collection applies.
    CalleeStatic Core Type StaticParams
  | -- | Builtin with a type scheme.
    CalleeBuiltin Text Scheme
  | -- | Any other function-typed value: positional-only, exact arity.
    CalleeValue Core Type

elabCall :: Ctx -> FilePath -> Locals -> Span -> Expr -> [Arg] -> Maybe Type -> TC (Core, Type)
elabCall ctx path locals sp fn args mExpected = do
  validateArgOrder
  callee <- resolveCallee
  case callee of
    CalleeStatic fnCore fnTy params -> do
      retTy <- case fnTy of
        TyFun _ r -> pure r
        other -> abort (diag ETypeCall (exprSpan fn) ("cannot call a value of type " <> renderType other))
      (posCores, kwCores) <- bindStatic params
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
                recordVar (exprSpan fn) bn (schemeType scheme) Nothing
                pure (CalleeBuiltin bn scheme)
              Nothing -> abort (diag ENameUndefined (exprSpan fn) ("undefined name: '" <> n <> "'"))
            Nothing -> abort (diag ENameUndefined (exprSpan fn) ("undefined name: '" <> n <> "'"))
      EDot (Expr _ (EVar m)) (Spanned _ fld)
        | not (Map.member m locals),
          Nothing <- lookupValueTarget ctx path m,
          Just key <- Map.lookup path (ctxScopes ctx) >>= Map.lookup m . gsNamespaces ->
            staticFromDecl (key, fld)
      ELambda ps rt body -> do
        (lam, ty, params) <- elabLambda ctx path locals (exprSpan fn) Nothing ps rt body
        pure (CalleeStatic lam ty params)
      _ -> valueCallee

    valueCallee = do
      (c, t) <- infer ctx path locals fn
      pure (CalleeValue c t)

    staticFromDecl key = do
      cd <- demandOrHeader key
      case cd of
        Just (core, ty, Just params) -> pure (CalleeStatic core ty params)
        Just (core, ty, Nothing) -> pure (CalleeValue core ty)
        Nothing -> valueCallee

    -- For recursive calls the declaration is still being elaborated;
    -- fall back to its header type as a plain function value.
    demandOrHeader key@(p, n) = do
      active <- gets stActive
      if key `Set.member` active
        then do
          t <- headerType ctx key
          recordVar (exprSpan fn) n t (Just key)
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), t, Nothing))
        else do
          cd <- demandDecl ctx key
          recordVar (exprSpan fn) n (cdType cd) (Just key)
          pure (Just (Core (exprSpan fn) (CVar (TopRef p n)), cdType cd, cdParams cd))

    -- Static binding per 7.5 for declarations.
    bindStatic (StaticParams positional variadic keywords) = do
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
      posCores <- mapM (\(e, (_, t)) -> check ctx path locals e t) (zip bound positional)
      extraCores <- case variadic of
        Just (_, elemTy) -> mapM (\e -> check ctx path locals e elemTy) extra
        Nothing -> do
          unless (null extra) $
            () <$ abort (diag ETypeArity sp ("too many positional arguments: expected " <> tshow nPos <> ", got " <> tshow (length posExprs)))
          pure []
      kwCores <- bindKeywords keywords
      pure (posCores <> extraCores, kwCores)

    bindKeywords keywords = go Set.empty kwArgs
      where
        kwTypes = Map.fromList keywords
        go _ [] = pure []
        go seen (Arg asp (AKw n e) : rest) = do
          when (n `Set.member` seen) $
            () <$ abort (diag ETypeKeyword asp ("duplicate keyword argument: '" <> n <> "'"))
          case Map.lookup n kwTypes of
            Just t -> do
              c <- check ctx path locals e t
              ((n, c) :) <$> go (Set.insert n seen) rest
            Nothing ->
              abort (diag ETypeKeyword asp ("unknown keyword argument: '" <> n <> "'"))
        go seen (Arg _ (APos _) : rest) = go seen rest -- unreachable (validated)

    -- Builtin calls: scheme instantiation (spec 4.4), plus the
    -- special cases of cast (15.8) and runCommand's --env (6.6).
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
          kwCores <- case (name, kwArgs) of
            (_, []) -> pure []
            ("runCommand", _) -> bindKeywords [("env", TyEnvironment)]
            _ -> abort (diag ETypeKeyword sp ("'" <> name <> "' takes no keyword arguments"))
          when (name == "runCommand") $
            mapM_
              ( \(kn, _) ->
                  when (kn /= "env") (() <$ abort (diag ETypeKeyword sp ("unknown keyword argument: '" <> kn <> "'")))
              )
              kwCores
          let params = schemeParams scheme
          unless (length posExprs == length params) $
            () <$ abort (diag ETypeArity sp ("'" <> name <> "' expects " <> tshow (length params) <> " arguments, got " <> tshow (length posExprs)))
          -- Pre-bind type variables from the expected return type.
          let subst0 = case mExpected of
                Just expT -> either (const Map.empty) id (unifyE (schemeRet scheme) expT Map.empty)
                Nothing -> Map.empty
          (cores, subst) <- goArgs subst0 [] (zip posExprs params)
          retTy <- case applySubst subst (schemeRet scheme) of
            t | isGround t -> pure t
            t -> case mExpected of
              Just expT -> do
                s' <- unifyOrFail sp t expT subst
                let t' = applySubst s' t
                if isGround t'
                  then pure t'
                  else inferenceFailure
              Nothing -> inferenceFailure
          pure (Core sp (CApp (Core sp (CVar (BuiltinRef name))) (reverse cores) (kwEnvOf kwCores)), retTy)
      where
        kwEnvOf = id
        castNeedsType =
          abort (diag ETypeMismatch sp "cast requires an expected type from context")
        inferenceFailure =
          abort . diag ETypeMismatch sp $
            "cannot instantiate the type of builtin '" <> name <> "'; add a type annotation"

        goArgs subst acc [] = pure (acc, subst)
        goArgs subst acc ((argExpr, pat) : rest) = do
          let p = applySubst subst pat
          (c, subst') <-
            if isGround p
              then do
                c <- check ctx path locals argExpr p
                pure (c, subst)
              else do
                (c, t) <- inferWithHint argExpr p
                s <- unifyOrFail (exprSpan argExpr) p t subst
                pure (c, s)
          goArgs subst' (c : acc) rest

        -- A lambda argument adopts concrete parameter types from the
        -- (partially instantiated) pattern.
        inferWithHint argExpr p = case (exprF argExpr, p) of
          (ELambda ps rt body, TyFun expPs _)
            | all isGround expPs -> do
                (lam, ty, _) <- elabLambdaAgainst ctx path locals (exprSpan argExpr) ps rt body expPs
                pure (lam, ty)
          _ -> infer ctx path locals argExpr

castable :: Type -> Bool
castable t = case t of
  TyVoid -> False
  TyFun _ _ -> False
  TyAsync _ -> False
  TyVar _ -> False
  TyArray e -> castable e
  TyMap e -> castable e
  TyRecord fs -> all castable (Map.elems fs)
  _ -> True

-- Type variable substitution / matching -------------------------------------------------------

type Subst = Map Text Type

applySubst :: Subst -> Type -> Type
applySubst s t = case t of
  TyVar v -> Map.findWithDefault t v s
  TyArray e -> TyArray (applySubst s e)
  TyMap e -> TyMap (applySubst s e)
  TyRecord fs -> TyRecord (Map.map (applySubst s) fs)
  TyAsync e -> TyAsync (applySubst s e)
  TyFun ps r -> TyFun (map (applySubst s) ps) (applySubst s r)
  _ -> t

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
  (TyRecord as, TyRecord bs)
    | Map.keysSet as == Map.keysSet bs ->
        foldM (\acc (a, b) -> unifyE a b acc) s (zip (Map.elems as) (Map.elems bs))
  (TyFun aps ar, TyFun bps br)
    | length aps == length bps -> do
        s' <- foldM (\acc (a, b) -> unifyE a b acc) s (zip aps bps)
        unifyE ar br s'
  (a, b)
    | a == b -> Right s
    | b == TyAny -> Right s -- an Any value may flow into any pattern position
    | otherwise -> Left (renderType a <> " does not match " <> renderType b)

unifyOrFail :: Span -> Type -> Type -> Subst -> TC Subst
unifyOrFail sp pat actual s = case unifyE pat actual s of
  Right s' -> pure s'
  Left msg ->
    abort $
      withExpectedActual (renderType (applySubst s pat)) (renderType actual) $
        diag ETypeMismatch sp ("type mismatch: " <> msg)
