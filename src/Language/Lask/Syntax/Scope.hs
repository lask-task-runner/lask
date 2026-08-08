-- | Cursor-directed queries over the surface AST for editor tooling:
-- which local bindings are visible at a position, and which call the
-- position sits inside.
module Language.Lask.Syntax.Scope
  ( localsAt,
    enclosingCall,
  )
where

import Data.List (nub)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Language.Lask.Lexer.Token (Spanned (..))
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import System.FilePath (normalise)

-- | Local bindings visible at a position, innermost first: function
-- and lambda parameters, @do@ bindings preceding the position, and
-- @for@\/@catch@ variables.
localsAt :: FilePath -> Module -> Position -> [Text]
localsAt path (Module ds) pos = nub (concat (reverse (concatMap goDecl ds)))
  where
    at :: Span -> Bool
    at = spanContains path pos

    goDecl (Decl sp f)
      | not (at sp) = []
      | otherwise = case f of
          DValue _ _ e -> goExpr e
          DFunction _ ps _ body ->
            [map paramName ps] <> concatMap goExpr (paramDefaults ps) <> goExpr body
          _ -> []

    goExpr e@(Expr sp f)
      | not (at sp) = []
      | otherwise = case f of
          ELambda ps _ b ->
            [map paramName ps] <> concatMap goExpr (paramDefaults ps) <> goExpr b
          EFor (Spanned _ x) xs b
            | at (blockSpan b) -> [x] : goBlock b
            | otherwise -> goExpr xs
          ETry b mCatch mFin ->
            goBlock b
              <> concat
                [ if at (blockSpan h) then [c] : goBlock h else []
                | (Spanned _ c, h) <- maybeToList mCatch
                ]
              <> maybe [] goBlock mFin
          EDo b -> goBlock b
          EIf c t mElse -> goExpr c <> goBlock t <> maybe [] goBlock mElse
          _ -> concatMap goExpr (childExprs e)

    -- A @do@ binding is visible only to the statements after it.
    goBlock (Block sp ss)
      | not (at sp) = []
      | otherwise = go [] ss
      where
        go acc [] = [acc]
        go acc (Stmt ssp sf : rest)
          | at ssp = acc : goStmt sf
          | otherwise = go (acc <> bound sf) rest
        bound (SBind n _) = [n]
        bound _ = []

    goStmt sf = case sf of
      SBind _ e -> goExpr e
      SExpr e -> goExpr e
      SReturn e -> goExpr e
      SGuard c b -> goExpr c <> goBlock b

-- | The dotted name of the callee of the innermost call whose
-- argument list the position sits in, e.g. @["tf", "init"]@.
enclosingCall :: FilePath -> Module -> Position -> Maybe [Text]
enclosingCall path m pos =
  listToMaybe
    [ ns
    | Expr _ (ECall fn _) <- reverse (enclosingExprs path m pos),
      afterCallee (exprSpan fn),
      ns <- maybeToList (calleePath fn)
    ]
  where
    afterCallee (Span _ (Position _ l c)) = (line pos, column pos) >= (l, c)
    afterCallee NoSpan = False

    calleePath (Expr _ (EVar n)) = Just [n]
    calleePath (Expr _ (EDot inner (Spanned _ fld))) = (<> [fld]) <$> calleePath inner
    calleePath _ = Nothing

-- | Expressions containing the position, outermost first.
enclosingExprs :: FilePath -> Module -> Position -> [Expr]
enclosingExprs path (Module ds) pos = concatMap goDecl ds
  where
    at = spanContains path pos

    goDecl (Decl sp f)
      | not (at sp) = []
      | otherwise = case f of
          DValue _ _ e -> descend e
          DFunction _ ps _ body -> concatMap descend (paramDefaults ps <> [body])
          _ -> []

    descend e@(Expr sp _)
      | at sp = e : concatMap descend (childExprs e)
      | otherwise = []

childExprs :: Expr -> [Expr]
childExprs (Expr _ f) = case f of
  EString ps -> partExprs ps
  EArray es -> es
  EObject kvs -> map snd kvs
  ELambda ps _ b -> paramDefaults ps <> [b]
  ECall fn as -> fn : map argExpr as
  EDot e _ -> [e]
  EIndex e i -> [e, i]
  EBin _ a b -> [a, b]
  ENot e -> [e]
  EDo b -> blockExprs b
  EIf c t mElse -> c : blockExprs t <> maybe [] blockExprs mElse
  EFor _ xs b -> xs : blockExprs b
  ETry b mCatch mFin ->
    blockExprs b
      <> maybe [] (blockExprs . snd) mCatch
      <> maybe [] blockExprs mFin
  EAsync e -> [e]
  EAwait e -> [e]
  ECommand _ env ps -> maybeToList env <> partExprs ps
  EEnv _ as -> maybe [] (map argExpr) as
  _ -> []

blockExprs :: Block -> [Expr]
blockExprs (Block _ ss) = concatMap stmtExprs ss
  where
    stmtExprs (Stmt _ sf) = case sf of
      SBind _ e -> [e]
      SExpr e -> [e]
      SReturn e -> [e]
      SGuard c b -> c : blockExprs b

partExprs :: [TextPart] -> [Expr]
partExprs ps = [e | TPInterp e <- ps]

argExpr :: Arg -> Expr
argExpr (Arg _ (APos e)) = e
argExpr (Arg _ (AKw _ e)) = e

paramName :: Param -> Text
paramName (Param _ f) = case f of
  PPositional n _ -> n
  PVariadic n _ -> n
  PKeyword n _ _ -> n

paramDefaults :: [Param] -> [Expr]
paramDefaults ps = [d | Param _ (PKeyword _ _ d) <- ps]

-- | Whether a span covers a position. The end is exclusive (spans end
-- just past their last character), matching hover; an inclusive end
-- would make two adjacent nodes both match at their boundary.
spanContains :: FilePath -> Position -> Span -> Bool
spanContains path pos (Span (Position f l1 c1) (Position _ l2 c2)) =
  normalise f == normalise path
    && (l1, c1) <= (line pos, column pos)
    && (line pos, column pos) < (l2, c2)
spanContains _ _ NoSpan = False
