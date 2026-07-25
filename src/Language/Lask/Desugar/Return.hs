{-# LANGUAGE OverloadedStrings #-}

-- | Early-return normalization (spec 6.5): the continuation
-- distribution transform.
--
-- Applied to each function body before elaboration (desugar step 3 of
-- 7.6). Validates return positions and unreachable statements
-- (@E-SYNTAX-RETURN-POSITION@) and produces a return-free surface
-- block:
--
-- > do { A...; return e }                  == do { A...; e }
-- > do { A...; if (c) { B...; return e }; C... }
-- >   -> do { A...; if (c) { B...; e } else { C... } }
--
-- For statement-position @if\/else@ containing returns, the
-- continuation is injected into both branches (a branch ending in
-- @return@ drops it, spec 6.5). Nested lambda bodies are transformed
-- separately when the elaborator reaches them; any 'SReturn' or
-- 'SGuard' remaining after this transform is in an illegal position.
module Language.Lask.Desugar.Return
  ( transformFunctionBody,
  )
where

import Data.Text (Text)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (ESyntaxReturnPosition), Stage (StageSyntax))
import Language.Lask.Span (Span)
import Language.Lask.Syntax.AST

-- | Transform a function body. Only @do@-block bodies contain
-- statements; any other expression is returned unchanged.
transformFunctionBody :: Expr -> Either Diagnostic Expr
transformFunctionBody e = case exprF e of
  EDo b -> do
    b' <- transformBlock b
    pure e {exprF = EDo b'}
  _ -> pure e

transformBlock :: Block -> Either Diagnostic Block
transformBlock (Block sp stmts) = Block sp <$> go stmts
  where
    go :: [Stmt] -> Either Diagnostic [Stmt]
    go [] = pure []
    go (Stmt ssp (SReturn e) : rest)
      | null rest = pure [Stmt ssp (SExpr e)]
      | otherwise = Left (unreachable ssp)
    go (Stmt ssp (SGuard c body) : rest) = do
      endsWithReturn ssp body
      thenB <- transformBlock body
      elseB <- Block ssp <$> go rest
      pure [Stmt ssp (SExpr (Expr ssp (EIf c thenB (Just elseB))))]
    go (s@(Stmt ssp (SExpr ife@(Expr _ (EIf c thenB (Just elseB))))) : rest)
      | blockHasReturn thenB || blockHasReturn elseB = do
          thenB' <- injectContinuation thenB rest
          elseB' <- injectContinuation elseB rest
          pure [Stmt ssp (SExpr (Expr (exprSpan ife) (EIf c thenB' (Just elseB'))))]
      | otherwise = (s :) <$> go rest
    go (s : rest) = (s :) <$> go rest

    -- A branch ending in return drops the continuation; a branch not
    -- ending in return has it appended (spec 6.5).
    injectContinuation :: Block -> [Stmt] -> Either Diagnostic Block
    injectContinuation b@(Block bsp bstmts) cont
      | blockEndsWithReturn b = transformBlock b
      | otherwise = transformBlock (Block bsp (bstmts <> cont))

    endsWithReturn :: Span -> Block -> Either Diagnostic ()
    endsWithReturn ssp b
      | blockEndsWithReturn b = pure ()
      | otherwise =
          Left . mkDiag ssp $
            "an if statement without else is allowed only when its block ends with return"

    unreachable ssp =
      mkDiag ssp "unreachable statements after return"

    mkDiag :: Span -> Text -> Diagnostic
    mkDiag ssp = mkDiagnostic ESyntaxReturnPosition StageSyntax ssp

blockEndsWithReturn :: Block -> Bool
blockEndsWithReturn (Block _ stmts) = case reverse stmts of
  (Stmt _ (SReturn _) : _) -> True
  _ -> False

-- | Statement-level return anywhere in the block (through
-- statement-position if branches and guards, per the recursive
-- return-position rule of 6.5).
blockHasReturn :: Block -> Bool
blockHasReturn (Block _ stmts) = any stmtHasReturn stmts
  where
    stmtHasReturn (Stmt _ f) = case f of
      SReturn _ -> True
      SGuard _ _ -> True
      SExpr (Expr _ (EIf _ t (Just e))) -> blockHasReturn t || blockHasReturn e
      _ -> False
