{-# LANGUAGE OverloadedStrings #-}

-- | Tree-walking evaluator over the core language (spec chapter 8).
module Language.Lask.Runtime.Eval
  ( RtCtx (..),
    mkRtCtx,
    evalCore,
    applyValue,
    topValue,
    castValue,
  )
where

import Control.Concurrent.Async (waitCatch)
import Control.Exception (catch, fromException, throwIO)
import Control.Monad (foldM, when)
import Data.Time.Clock (getCurrentTime)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (CommandRunner, callBuiltin)
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..))
import Language.Lask.ErrorCode
import Language.Lask.Obs.Events
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (functionRefJson)
import Language.Lask.Types (Type (..), renderType)

data RtCtx = RtCtx
  { rtProgram :: CoreProgram,
    rtTopCache :: IORef (Map (FilePath, Text) Value),
    rtStdin :: Text,
    rtRunCommand :: CommandRunner,
    rtTraceId :: TraceId,
    rtEmit :: EventSink
  }

mkRtCtx :: CoreProgram -> Text -> CommandRunner -> IO RtCtx
mkRtCtx prog stdinText runner = do
  cache <- newIORef Map.empty
  traceId <- newTraceId
  pure (RtCtx prog cache stdinText runner traceId noSink)

-- | Top-level values are evaluated once, on first reference.
topValue :: RtCtx -> (FilePath, Text) -> IO Value
topValue ctx key = do
  cache <- readIORef (rtTopCache ctx)
  case Map.lookup key cache of
    Just v -> pure v
    Nothing -> case Map.lookup key (cpDecls (rtProgram ctx)) of
      Just cd -> do
        v <- evalCore ctx Map.empty (cdCore cd)
        modifyIORef' (rtTopCache ctx) (Map.insert key v)
        pure v
      Nothing ->
        internal ("missing top-level declaration: " <> T.pack (show key))

evalCore :: RtCtx -> Scope -> Core -> IO Value
evalCore ctx scope (Core _ f) = case f of
  CNull -> pure VNull
  CBool b -> pure (VBool b)
  CNumber n -> pure (VNumber n)
  CStrLit t -> pure (VString t)
  CStr parts -> VString . T.concat <$> mapM part parts
    where
      part (CPText t) = pure t
      part (CPExpr c) = stringify <$> evalCore ctx scope c
  CVar (LocalRef n) -> case Map.lookup n scope of
    Just v -> pure v
    Nothing -> internal ("unbound local: " <> n)
  CVar (TopRef p n) -> topValue ctx (p, n)
  CVar (BuiltinRef "stdin") -> pure (VString (rtStdin ctx))
  CVar (BuiltinRef b) -> pure (VBuiltin b)
  CArray es -> VArray . V.fromList <$> mapM (evalCore ctx scope) es
  CMapLit kvs -> VMap . Map.fromList <$> mapM evalKv kvs
  CRecordLit kvs -> VRecord . Map.fromList <$> mapM evalKv kvs
  CLam lam -> pure (VClosure (Closure lam scope))
  CApp fn pos kw -> do
    fv <- evalCore ctx scope fn
    posVs <- mapM (evalCore ctx scope) pos
    kwVs <- mapM (\(n, c) -> (,) n <$> evalCore ctx scope c) kw
    applyValue ctx fv posVs kwVs
  CDot c fld -> do
    v <- evalCore ctx scope c
    case v of
      VRecord m -> case Map.lookup fld m of
        Just x -> pure x
        Nothing -> internal ("missing record field: " <> fld)
      _ -> internal "field access on a non-record"
  CIndex kind c i -> do
    container <- evalCore ctx scope c
    idx <- evalCore ctx scope i
    case (kind, container, idx) of
      (IdxArray, VArray xs, VNumber n) ->
        let len = V.length xs
            asInt = round (realToFrac n :: Double) :: Int
         in if fromIntegral asInt == n && asInt >= 0 && asInt < len
              then pure (xs V.! asInt)
              else
                throwIO . runtimeFailure ERuntimeAccess $
                  "array index out of range: " <> formatNumber n <> " (length " <> T.pack (show len) <> ")"
      (IdxMap, VMap m, VString k) -> case Map.lookup k m of
        Just v -> pure v
        Nothing -> throwIO (runtimeFailure ERuntimeAccess ("key not found: '" <> k <> "'"))
      (IdxRecord, VRecord m, VString k) -> case Map.lookup k m of
        Just v -> pure v
        Nothing -> internal ("missing record field: " <> k)
      _ -> internal "malformed index access"
  CIf c t e -> do
    cond <- evalCore ctx scope c
    case cond of
      VBool True -> evalCore ctx scope t
      VBool False -> evalCore ctx scope e
      _ -> internal "non-boolean condition"
  CAnd a b -> do
    av <- evalCore ctx scope a
    case av of
      VBool False -> pure (VBool False)
      VBool True -> evalCore ctx scope b
      _ -> internal "non-boolean operand of &&"
  COr a b -> do
    av <- evalCore ctx scope a
    case av of
      VBool True -> pure (VBool True)
      VBool False -> evalCore ctx scope b
      _ -> internal "non-boolean operand of ||"
  CNot a -> do
    av <- evalCore ctx scope a
    case av of
      VBool b -> pure (VBool (not b))
      _ -> internal "non-boolean operand of !"
  CBin op a b -> do
    av <- evalCore ctx scope a
    bv <- evalCore ctx scope b
    binOp ctx op av bv
  CDo stmts -> go scope VVoid stmts
    where
      go _ acc [] = pure acc
      go sc _ (CSBind n c : rest) = do
        v <- evalCore ctx sc c
        go (Map.insert n v sc) v rest
      go sc _ (CSExpr c : rest) = do
        v <- evalCore ctx sc c
        go sc v rest
  CAwait c -> do
    v <- evalCore ctx scope c
    case v of
      VAsync (AsyncHandle a) -> do
        r <- waitCatch a
        case r of
          Right x -> pure x
          Left ex -> case fromException ex of
            Just failure -> throwIO (failure :: LaskFailure)
            Nothing ->
              throwIO (runtimeFailure ERuntimeAwaitFailed ("async computation failed: " <> T.pack (show ex)))
      _ -> internal "await on a non-handle"
  CEnv kind args -> do
    params <- mapM evalKv args
    pure (VEnv (EnvValue kind (Map.fromList params)))
  CCast c ty -> do
    v <- evalCore ctx scope c
    castValue ty v
  where
    evalKv (n, c) = (,) n <$> evalCore ctx scope c

-- | Function application (spec 8.3): bind positionals, collect the
-- variadic tail, then bind keyword parameters in declaration order —
-- unbound ones evaluate their defaults in an environment containing
-- the parameters bound so far.
applyValue :: RtCtx -> Value -> [Value] -> [(Text, Value)] -> IO Value
applyValue ctx fv pos kw = case fv of
  VClosure (Closure lam captured) -> instrument ctx lam pos $ do
    let nPos = length (lamPositional lam)
    when (length pos < nPos) $
      throwIO . runtimeFailure ERuntimeAccess $
        "not enough arguments for " <> lamName lam
    let (bound, extra) = splitAt nPos pos
        scope1 = foldr (uncurry Map.insert) captured (zip (lamPositional lam) bound)
    scope2 <- case lamVariadic lam of
      Just v -> pure (Map.insert v (VArray (V.fromList extra)) scope1)
      Nothing -> do
        when (not (null extra)) $
          throwIO . runtimeFailure ERuntimeAccess $
            "too many arguments for " <> lamName lam
        pure scope1
    scope3 <-
      foldM
        ( \sc (name, defCore) -> case lookup name kw of
            Just v -> pure (Map.insert name v sc)
            Nothing -> do
              v <- evalCore ctx sc defCore
              pure (Map.insert name v sc)
        )
        scope2
        (lamKeywords lam)
    evalCore ctx scope3 (lamBody lam)
  VBuiltin name -> callBuiltin (applyValue ctx) (rtRunCommand ctx) name pos kw
  other ->
    throwIO . runtimeFailure ERuntimeAccess $
      "cannot call a value of type " <> typeNameOf other

-- | Call-boundary instrumentation (spec 12.3, 12.5): emit
-- CallEvent\/ReturnEvent\/FailEvent for declared functions and push a
-- stack frame onto propagating failures. A FailEvent is emitted even
-- when the failure is caught further out (12.5). Anonymous internal
-- lambdas (names in angle brackets) are not observed.
instrument :: RtCtx -> Lam -> [Value] -> IO Value -> IO Value
instrument ctx lam args action
  | isAnonymous = action `catch` (throwIO . pushFrame frameLabel)
  | otherwise = do
      emit EvCall [("args", summarizeArgs)]
      r <-
        action `catch` \lf -> do
          emit EvFail [("error", summarizeValue (lfError lf))]
          throwIO (pushFrame frameLabel lf)
      emit EvReturn [("result", summarizeValue r)]
      pure r
  where
    isAnonymous = T.isPrefixOf "<" (lamName lam)
    frameLabel = lamName lam <> " (" <> T.pack (lamModule lam) <> ")"
    summarizeArgs = summarizeValue (VArray (V.fromList args))
    emit kind payload = do
      now <- getCurrentTime
      rtEmit ctx (Event kind (rtTraceId ctx) now (functionRefJson lam) payload)

binOp :: RtCtx -> PrimOp -> Value -> Value -> IO Value
binOp _ op a b = case op of
  PEq -> pure (VBool (valueEq a b))
  PNe -> pure (VBool (not (valueEq a b)))
  _ -> case (a, b) of
    (VNumber x, VNumber y) -> case op of
      PAdd -> pure (VNumber (x + y))
      PSub -> pure (VNumber (x - y))
      PMul -> pure (VNumber (x * y))
      PDiv
        | y == 0 -> throwIO (runtimeFailure ERuntimeDivByZero "division by zero")
        | otherwise -> pure (VNumber (divS x y))
      PLt -> pure (VBool (x < y))
      PLe -> pure (VBool (x <= y))
      PGt -> pure (VBool (x > y))
      PGe -> pure (VBool (x >= y))
    _ -> internal "non-numeric operands"
  where
    divS x y = realToFrac (realToFrac x / realToFrac y :: Double)

-- | Runtime type check for @cast@ (spec 15.8). Records and maps are
-- mutually accepted (structural conversion); @Any@ positions are
-- unchecked; failure is @E-RUNTIME-CAST@ with a path.
castValue :: Type -> Value -> IO Value
castValue = go []
  where
    go :: [Text] -> Type -> Value -> IO Value
    go path ty v = case (ty, v) of
      (TyAny, _) -> pure v
      (TyNumber, VNumber _) -> pure v
      (TyString, VString _) -> pure v
      (TyBool, VBool _) -> pure v
      (TyNull, VNull) -> pure v
      (TyEnvironment, VEnv _) -> pure v
      (TyArray t, VArray xs) ->
        VArray <$> V.imapM (\i x -> go (path <> [T.pack ("[" <> show i <> "]")]) t x) xs
      (TyMap t, VMap m) -> VMap <$> Map.traverseWithKey (\k x -> go (path <> [k]) t x) m
      (TyMap t, VRecord m) -> VMap <$> Map.traverseWithKey (\k x -> go (path <> [k]) t x) m
      (TyRecord fields, VRecord m) -> castRecord path fields m
      (TyRecord fields, VMap m) -> castRecord path fields m
      _ -> castFail path ty v

    castRecord path fields m
      | Map.keysSet fields == Map.keysSet m =
          VRecord
            <$> Map.traverseWithKey
              (\k x -> go (path <> [k]) (fields Map.! k) x)
              m
      | otherwise =
          castFail path (TyRecord fields) (VRecord m)

    castFail :: [Text] -> Type -> Value -> IO a
    castFail path ty v =
      throwIO . runtimeFailure ERuntimeCast $
        "cast failed"
          <> (if null path then "" else " at " <> T.intercalate "." path)
          <> ": expected "
          <> renderType ty
          <> ", got "
          <> typeNameOf v

internal :: Text -> IO a
internal msg =
  throwIO (runtimeFailure ERuntimeAccess ("internal error: " <> msg))
