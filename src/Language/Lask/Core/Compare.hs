{-# LANGUAGE OverloadedStrings #-}

-- | Comparing the Core of two programs up to what may legitimately
-- differ between them: spans, module paths, and the names of locals. The
-- EDSL and the elaborator are checked against each other with this, and
-- it is what "reifies to the same Core" means (#91).
module Language.Lask.Core.Compare
  ( normalise,
    sameDecl,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..))
import Language.Lask.Span (Span (..))

-- | Spans and module paths erased, anonymous lambdas unnamed, and
-- locals renamed in binding order. The names the elaborator derives
-- from spans (lambdas' @<lambda\@l:c>@, a @case@ scrutinee's name, a
-- command's result @%r@) are all locals or anonymous lambdas, so they
-- are covered. Parameter names are kept: they are part of a
-- declaration's interface.
normalise :: Core -> Core
normalise c0 = evalState (norm Map.empty c0) (0 :: Int)
  where
    norm env (Core _ f) = Core NoSpan <$> case f of
      CVar (LocalRef n) -> pure (CVar (LocalRef (Map.findWithDefault n n env)))
      CVar (TopRef _ n) -> pure (CVar (TopRef "" n))
      CLam lam -> do
        let named = not ("<" `T.isPrefixOf` lamName lam)
        ps <- if named then pure (lamPositional lam) else mapM (const freshName) (lamPositional lam)
        let env' = Map.union (Map.fromList (zip (lamPositional lam) ps)) env
        kws <- mapM (\(k, d) -> (,) k <$> traverse (norm env') d) (lamKeywords lam)
        b <- norm env' (lamBody lam)
        pure
          ( CLam
              lam
                { lamName = if named then lamName lam else "<lambda>",
                  lamModule = "",
                  lamPositional = ps,
                  lamKeywords = kws,
                  lamBody = b
                }
          )
      CDo ss -> CDo <$> stmts env ss
      CEnv k args -> CEnv k <$> mapM (\(n, a) -> (,) n <$> norm env a) args
      _ -> rebuild env f
    stmts _ [] = pure []
    stmts env (CSExpr e : rest) = (:) <$> (CSExpr <$> norm env e) <*> stmts env rest
    stmts env (CSBind n e : rest) = do
      e' <- norm env e
      n' <- freshName
      (CSBind n' e' :) <$> stmts (Map.insert n n' env) rest
    freshName :: State Int Text
    freshName = do
      i <- gets id
      modify (+ 1)
      pure ("v" <> T.pack (show i))
    -- Every other node: normalise the children in place.
    rebuild env f = case f of
      CStr ps -> CStr <$> mapM (part env) ps
      CArray es -> CArray <$> mapM (norm env) es
      CMapLit kvs -> CMapLit <$> mapM (\(k, v) -> (,) k <$> norm env v) kvs
      CRecordLit kvs -> CRecordLit <$> mapM (\(k, v) -> (,) k <$> norm env v) kvs
      CApp fn pos kws -> CApp <$> norm env fn <*> mapM (norm env) pos <*> mapM (\(k, v) -> (,) k <$> norm env v) kws
      CDot e n -> (`CDot` n) <$> norm env e
      CIndex k a b -> CIndex k <$> norm env a <*> norm env b
      CIf a b e -> CIf <$> norm env a <*> norm env b <*> norm env e
      CAnd a b -> CAnd <$> norm env a <*> norm env b
      COr a b -> COr <$> norm env a <*> norm env b
      CNot a -> CNot <$> norm env a
      CBin op a b -> CBin op <$> norm env a <*> norm env b
      CAwait a -> CAwait <$> norm env a
      CRunnable e args -> CRunnable <$> norm env e <*> mapM (\(n, a) -> (,) n <$> norm env a) args
      CCast a t -> (`CCast` t) <$> norm env a
      CIsType a t -> (`CIsType` t) <$> norm env a
      other -> pure other
    part env (CPExpr e) = CPExpr <$> norm env e
    part _ p = pure p

-- | Whether two declarations agree on everything but spans, module
-- paths and the names of locals; the difference, when they do not.
sameDecl :: CoreDecl -> CoreDecl -> Maybe Text
sameDecl a b
  | cdName a /= cdName b = Just ("names differ: " <> cdName a <> ", " <> cdName b)
  | cdType a /= cdType b = Just "types differ"
  | cdParams a /= cdParams b = Just "parameters differ"
  | cdTypeVars a /= cdTypeVars b = Just "type parameters differ"
  | cdBounds a /= cdBounds b = Just "bounds differ"
  | normalise (cdCore a) /= normalise (cdCore b) = Just "bodies differ"
  | otherwise = Nothing
