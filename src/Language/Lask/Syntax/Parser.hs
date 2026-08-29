{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the surface language (spec chapters 4-6).
--
-- Operates on the layouted token stream from "Language.Lask.Lexer":
-- every remaining 'TNewline' is a statement\/declaration terminator.
--
-- A small push-back buffer (carried in a 'State' layer) lets the type
-- parser split @>>@ into two closing angles (@Array\<Array\<Number>>@)
-- and @>=@ into a closing angle plus @=@. Push-backs are created only
-- by 'closeAngle' and are always consumed by the directly following
-- token match, so they never interact with backtracking.
module Language.Lask.Syntax.Parser
  ( parseModule,
    parseExpr,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.State.Strict (State, evalState, get, lift, modify, put)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (ESyntaxUnexpectedToken), Stage (StageSyntax))
import Language.Lask.Lexer (lexLayout)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Span (..), fromSourcePos)
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.TokenStream
import Text.Megaparsec hiding (State, Token, Tokens, token)
import qualified Text.Megaparsec as MP

type P = ParsecT Void TokStream (State [Spanned Token])

-- Entry points -------------------------------------------------------------

parseModule :: FilePath -> Text -> Either Diagnostic Module
parseModule file src = do
  toks <- lexLayout file src
  runP pModule file toks

parseExpr :: FilePath -> Text -> Either Diagnostic Expr
parseExpr file src = do
  toks <- lexLayout file src
  runP (pExpr <* pEnd) file toks

runP :: P a -> FilePath -> [Spanned Token] -> Either Diagnostic a
runP p file toks =
  case evalState (runParserT p file (TokStream toks)) [] of
    Left bundle -> Left (bundleToDiagnostic bundle)
    Right a -> Right a

bundleToDiagnostic :: ParseErrorBundle TokStream Void -> Diagnostic
bundleToDiagnostic bundle =
  let err = NE.head (bundleErrors bundle)
      (_, posState) = reachOffset (errorOffset err) (bundlePosState bundle)
      pos = fromSourcePos (pstateSourcePos posState)
      msg = T.strip (T.pack (parseErrorTextPretty err))
   in mkDiagnostic ESyntaxUnexpectedToken StageSyntax (Span pos pos) msg

-- Token primitives ----------------------------------------------------------

-- | Match a token through the push-back buffer.
matchTok :: String -> (Token -> Maybe a) -> P (Spanned a)
matchTok lbl f = do
  pb <- lift get
  case pb of
    (Spanned sp t : ts) -> case f t of
      Just a -> lift (put ts) >> pure (Spanned sp a)
      Nothing -> failure Nothing (expectedLabel lbl)
    [] ->
      MP.token
        (\s -> Spanned (spannedSpan s) <$> f (spannedValue s))
        (expectedLabel lbl)
  where
    expectedLabel l = Set.singleton (Label (NE.fromList l))

-- | Peek without consuming; safe with the push-back buffer.
peekTok :: P (Maybe (Spanned Token))
peekTok = do
  pb <- lift get
  case pb of
    (t : _) -> pure (Just t)
    [] -> optional (lookAhead (MP.token Just Set.empty))

sym :: Token -> P Span
sym t = spannedSpan <$> matchTok (renderToken t) (\x -> if x == t then Just () else Nothing)

kw :: Keyword -> P Span
kw k = sym (TKw k)

op :: Op -> P Span
op o = sym (TOp o)

lowerId :: P (Spanned Text)
lowerId = matchTok "identifier" $ \t -> case t of
  TLowerId n -> Just n
  _ -> Nothing

upperId :: P (Spanned Text)
upperId = matchTok "type name" $ \t -> case t of
  TUpperId n -> Just n
  _ -> Nothing

-- | Identifier of either kind, for import specifiers.
identAny :: P (Spanned Text)
identAny = matchTok "identifier" $ \t -> case t of
  TLowerId n -> Just n
  TUpperId n -> Just n
  _ -> Nothing

-- | String literal without interpolation (import paths, field keys).
stringLit :: String -> P (Spanned Text)
stringLit lbl = matchTok lbl $ \t -> case t of
  TRawString s -> Just s
  TString ps -> chunksOnly ps
  _ -> Nothing
  where
    chunksOnly ps = T.concat <$> traverse chunkOf ps
    chunkOf (Chunk c) = Just c
    chunkOf (Interp _) = Nothing

-- | Close a type argument list: @>@, or split @>>@\/@>=@ by pushing
-- the remainder back.
closeAngle :: P Span
closeAngle = do
  Spanned sp r <- matchTok "'>'" $ \t -> case t of
    TOp OpGt -> Just Nothing
    TOp OpCompR -> Just (Just (TOp OpGt))
    TOp OpGe -> Just (Just TAssign)
    _ -> Nothing
  case r of
    Nothing -> pure sp
    Just t' -> lift (modify (Spanned sp t' :)) >> pure sp

-- | End of input, ensuring no push-back is pending.
pEnd :: P ()
pEnd = do
  pb <- lift get
  unless (null pb) (fail "unexpected trailing token")
  eof

terminator :: P ()
terminator = void (sym TNewline) <|> void (sym TSemi)

-- Module and declarations ----------------------------------------------------

pModule :: P Module
pModule = do
  skipMany terminator
  decls <- sepEndBy pDecl (skipSome terminator)
  pEnd
  pure (Module decls)

pDecl :: P Decl
pDecl = choice [pImport, pTypeAliasDecl, pValueOrFunction]

pImport :: P Decl
pImport = do
  s <- kw KImport
  choice
    [ do
        _ <- op OpMul
        _ <- kw KAs
        Spanned _ alias <- lowerId
        Spanned e path <- pFromPath
        pure (Decl (s <> e) (DImportNamespace alias path)),
      do
        _ <- sym TLBrace
        specs <- sepBy1 pImportSpec (sym TComma)
        _ <- sym TRBrace
        Spanned e path <- pFromPath
        pure (Decl (s <> e) (DImportNamed specs path))
    ]
  where
    pFromPath = kw KFrom *> stringLit "import path"

pImportSpec :: P ImportSpec
pImportSpec = do
  Spanned sp n <- identAny
  alias <- optional (kw KAs *> identAny)
  case alias of
    Just (Spanned sp2 a) -> do
      when (identKind n /= identKind a) $
        fail "renamed import must keep the identifier kind"
      pure (ImportSpec (sp <> sp2) n (Just a))
    Nothing -> pure (ImportSpec sp n Nothing)
  where
    identKind t = maybe False (\(c, _) -> c >= 'A' && c <= 'Z') (T.uncons t)

pTypeAliasDecl :: P Decl
pTypeAliasDecl = do
  s <- kw KType
  Spanned _ name <- upperId
  _ <- sym TAssign
  t <- pType
  pure (Decl (s <> stypeSpan t) (DTypeAlias name t))

pValueOrFunction :: P Decl
pValueOrFunction = do
  Spanned sp name <- lowerId
  choice
    [ do
        _ <- sym TLParen
        ps <- pParamList
        _ <- sym TRParen
        rt <- optional (sym TColon *> pType)
        _ <- sym TAssign
        e <- pExpr
        pure (Decl (sp <> exprSpan e) (DFunction name ps rt e)),
      do
        sec <- pSecrecy
        t <- optional (sym TColon *> pType)
        _ <- sym TAssign
        e <- pExpr
        pure (Decl (sp <> exprSpan e) (DValue name sec t e))
    ]

-- | The optional @!!@ secret marker following a bound name (spec 6.10).
pSecrecy :: P Secrecy
pSecrecy = maybe Public (const Secret) <$> optional (sym TBangBang)

-- Parameters ------------------------------------------------------------------

pParamList :: P [Param]
pParamList = do
  ps <- sepBy pParam (sym TComma)
  validateParamOrder ps
  pure ps

pParam :: P Param
pParam =
  choice
    [ do
        s <- sym TDashDash
        Spanned _ n <- lowerId
        sec <- pSecrecy
        t <- optional (sym TColon *> pType)
        _ <- sym TAssign
        d <- pExpr
        pure (Param (s <> exprSpan d) (PKeyword n sec t d)),
      do
        s <- sym TEllipsis
        Spanned sp n <- lowerId
        -- A variadic parameter cannot be marked !! (spec 6.1); reject
        -- it here rather than letting the marker parse and be ignored.
        marked <- optional (sym TBangBang)
        case marked of
          Just _ -> fail "a variadic parameter cannot be marked '!!'"
          Nothing -> pure ()
        t <- optional (sym TColon *> pType)
        case t of
          Just ty
            | not (isArrayForm ty) ->
                fail "variadic parameter type must have the form Array<T>"
          _ -> pure ()
        pure (Param (s <> maybe sp stypeSpan t) (PVariadic n t)),
      do
        Spanned sp n <- lowerId
        sec <- pSecrecy
        t <- optional (sym TColon *> pType)
        pure (Param (sp <> maybe sp stypeSpan t) (PPositional n sec t))
    ]
  where
    isArrayForm (SType _ (SArray _)) = True
    isArrayForm _ = False

-- | Enforce declaration order: positional*, variadic?, keyword*
-- (spec 6.1, by grammar).
validateParamOrder :: [Param] -> P ()
validateParamOrder = go (0 :: Int)
  where
    -- 0 = positionals, 1 = after variadic, 2 = keywords
    go _ [] = pure ()
    go st (Param _ f : rest) = case f of
      PPositional {}
        | st == 0 -> go 0 rest
        | otherwise -> fail "positional parameters must come first"
      PVariadic {}
        | st == 0 -> go 1 rest
        | otherwise -> fail "at most one variadic parameter, before keyword parameters"
      PKeyword {} -> go 2 rest

-- Types -------------------------------------------------------------------------

pType :: P SType
pType = do
  Spanned sp name <- upperId
  case name of
    "Any" -> pure (SType sp SAny)
    "Number" -> pure (SType sp SNumber)
    "String" -> pure (SType sp SString)
    "Bool" -> pure (SType sp SBool)
    "Null" -> pure (SType sp SNull)
    "Void" -> pure (SType sp SVoid)
    "Environment" -> pure (SType sp SEnvironment)
    "Array" -> pGeneric1 sp SArray
    "Map" -> pGeneric1 sp SMap
    "AsyncHandle" -> pGeneric1 sp SAsyncHandle
    "Record" -> do
      _ <- op OpLt
      fields <- sepBy pRecordField (sym TComma)
      e <- closeAngle
      pure (SType (sp <> e) (SRecord fields))
    "Function" -> do
      _ <- op OpLt
      ts <- sepBy1 pType (sym TComma)
      e <- closeAngle
      pure (SType (sp <> e) (SFunction (init ts) (last ts)))
    _ -> pure (SType sp (SNamed name))
  where
    pGeneric1 sp f = do
      _ <- op OpLt
      t <- pType
      e <- closeAngle
      pure (SType (sp <> e) (f t))

pRecordField :: P (Spanned Text, SType)
pRecordField = do
  key <- lowerId <|> stringLit "field name"
  _ <- sym TColon
  t <- pType
  pure (key, t)

-- Expressions ---------------------------------------------------------------------

pExpr :: P Expr
pExpr = makeExprParser pUnary operatorTable

operatorTable :: [[Operator P Expr]]
operatorTable =
  [ [binL OpMul, binL OpDiv],
    [binL OpAdd, binL OpSub],
    [binL OpEq, binL OpNe, binL OpLt, binL OpLe, binL OpGt, binL OpGe],
    [binL OpAnd, binL OpOr],
    [binL OpPipeR, binL OpPipeL, binL OpCompR, binL OpCompL]
  ]
  where
    binL o = InfixL (mkBin o <$ op o)
    mkBin o a b = Expr (exprSpan a <> exprSpan b) (EBin o a b)

pUnary :: P Expr
pUnary =
  choice
    [ do
        s <- op OpNot
        e <- pUnary
        pure (Expr (s <> exprSpan e) (ENot e)),
      do
        s <- kw KAsync
        e <- pUnary
        pure (Expr (s <> exprSpan e) (EAsync e)),
      do
        s <- kw KAwait
        e <- pUnary
        pure (Expr (s <> exprSpan e) (EAwait e)),
      pPostfix
    ]

pPostfix :: P Expr
pPostfix = pPrimary >>= loop
  where
    loop e =
      choice
        [ do
            _ <- sym TDot
            Spanned sp n <- lowerId
            loop (Expr (exprSpan e <> sp) (EDot e (Spanned sp n))),
          do
            _ <- sym TLBracket
            i <- pExpr
            s2 <- sym TRBracket
            loop (Expr (exprSpan e <> s2) (EIndex e i)),
          do
            _ <- sym TLParen
            as <- sepBy pArg (sym TComma)
            s2 <- sym TRParen
            loop (Expr (exprSpan e <> s2) (ECall e as)),
          pure e
        ]

pArg :: P Arg
pArg =
  choice
    [ try $ do
        Spanned sp n <- lowerId
        _ <- sym TAssign
        e <- pExpr
        pure (Arg (sp <> exprSpan e) (AKw n e)),
      (\e -> Arg (exprSpan e) (APos e)) <$> pExpr
    ]

pPrimary :: P Expr
pPrimary =
  choice
    [ pLiteral,
      pString,
      pEnvExpr,
      pCommandExpr,
      pVar,
      pParen,
      pArrayLit,
      pObjectLit,
      pLambda,
      pDoExpr,
      pIfExpr,
      pForExpr,
      pTryExpr
    ]

pLiteral :: P Expr
pLiteral = do
  Spanned sp f <- matchTok "literal" $ \t -> case t of
    TNull -> Just ENull
    TBool b -> Just (EBool b)
    TNumber n -> Just (ENumber n)
    _ -> Nothing
  pure (Expr sp f)

pString :: P Expr
pString = do
  Spanned sp raw <- matchTok "string" $ \t -> case t of
    TRawString s -> Just (Left s)
    TString ps -> Just (Right ps)
    _ -> Nothing
  f <- case raw of
    Left s -> pure (EString [TPChunk s])
    Right ps -> EString <$> traverse convertPart ps
  pure (Expr sp f)

convertPart :: StrPart -> P TextPart
convertPart (Chunk c) = pure (TPChunk c)
convertPart (Interp toks) = TPInterp <$> subExpr "<interpolation>" toks

-- | Parse a captured nested token stream as a full expression.
subExpr :: String -> [Spanned Token] -> P Expr
subExpr name toks =
  case evalState (runParserT (pExpr <* pEnd) name (TokStream toks)) [] of
    Right e -> pure e
    Left bundle ->
      fail (T.unpack (T.strip (T.pack (parseErrorTextPretty (NE.head (bundleErrors bundle))))))

pVar :: P Expr
pVar = do
  Spanned sp n <- lowerId
  pure (Expr sp (EVar n))

pParen :: P Expr
pParen = do
  s1 <- sym TLParen
  e <- pExpr
  s2 <- sym TRParen
  pure (Expr (s1 <> s2) (exprF e))

pArrayLit :: P Expr
pArrayLit = do
  s1 <- sym TLBracket
  es <- sepBy pExpr (sym TComma)
  s2 <- sym TRBracket
  pure (Expr (s1 <> s2) (EArray es))

pObjectLit :: P Expr
pObjectLit = do
  s1 <- sym TLBrace
  kvs <- sepBy pKeyValue (sym TComma)
  s2 <- sym TRBrace
  pure (Expr (s1 <> s2) (EObject kvs))
  where
    pKeyValue = do
      key <- lowerId <|> stringLit "object key"
      _ <- sym TColon
      v <- pExpr
      pure (key, v)

pLambda :: P Expr
pLambda = do
  s <- sym TBackslash
  _ <- sym TLParen
  ps <- pParamList
  _ <- sym TRParen
  rt <- optional (sym TColon *> pType)
  _ <- sym TArrow
  body <- pExpr
  pure (Expr (s <> exprSpan body) (ELambda ps rt body))

pEnvExpr :: P Expr
pEnvExpr = do
  Spanned sp h <- matchTok "environment expression" $ \t -> case t of
    TEnvHead n -> Just n
    _ -> Nothing
  nxt <- peekTok
  case nxt of
    Just (Spanned sp2 TLParen)
      | adjacent sp sp2 -> do
          _ <- sym TLParen
          as <- sepBy pArg (sym TComma)
          e <- sym TRParen
          pure (Expr (sp <> e) (EEnv h (Just as)))
    _ -> pure (Expr sp (EEnv h Nothing))
  where
    adjacent (Span _ e) (Span s _) = e == s
    adjacent _ _ = False

pCommandExpr :: P Expr
pCommandExpr = do
  Spanned sp (stream, envToks, parts) <- matchTok "command" $ \t -> case t of
    TCommand s env ps -> Just (s, env, ps)
    _ -> Nothing
  env <- traverse (subExpr "<command environment>") envToks
  ps <- traverse convertPart parts
  pure (Expr sp (ECommand stream env ps))

pDoExpr :: P Expr
pDoExpr = do
  s <- kw KDo
  b <- pBlock
  pure (Expr (s <> blockSpan b) (EDo b))

-- | @if@ in expression position: @else@ is mandatory (spec 6.4). The
-- statement parser handles the guard form separately.
pIfExpr :: P Expr
pIfExpr = do
  (s, c, thenB, elseB) <- pIfHead
  case elseB of
    Just eb -> pure (Expr (s <> blockSpan eb) (EIf c thenB (Just eb)))
    Nothing -> fail "if expression requires an else block"

pIfHead :: P (Span, Expr, Block, Maybe Block)
pIfHead = do
  s <- kw KIf
  _ <- sym TLParen
  c <- pExpr
  _ <- sym TRParen
  thenB <- pBlock
  elseB <- optional (kw KElse *> pBlock)
  pure (s, c, thenB, elseB)

pForExpr :: P Expr
pForExpr = do
  s <- kw KFor
  _ <- sym TLParen
  x <- lowerId
  _ <- sym TColon
  xs <- pExpr
  _ <- sym TRParen
  b <- pBlock
  pure (Expr (s <> blockSpan b) (EFor x xs b))

pTryExpr :: P Expr
pTryExpr = do
  s <- kw KTry
  body <- pBlock
  choice
    [ do
        _ <- kw KCatch
        _ <- sym TLParen
        n <- lowerId
        _ <- sym TRParen
        h <- pBlock
        fin <- optional (kw KFinally *> pBlock)
        let end = maybe (blockSpan h) blockSpan fin
        pure (Expr (s <> end) (ETry body (Just (n, h)) fin)),
      do
        _ <- kw KFinally
        fin <- pBlock
        pure (Expr (s <> blockSpan fin) (ETry body Nothing (Just fin)))
    ]

-- Blocks and statements --------------------------------------------------------------

pBlock :: P Block
pBlock = do
  s1 <- sym TLBrace
  skipMany terminator
  stmts <- sepEndBy pStmt (skipSome terminator)
  s2 <- sym TRBrace
  pure (Block (s1 <> s2) stmts)

pStmt :: P Stmt
pStmt =
  choice
    [ do
        s <- kw KReturn
        e <- pExpr
        pure (Stmt (s <> exprSpan e) (SReturn e)),
      try pBindStmt,
      try ((\e -> Stmt (exprSpan e) (SExpr e)) <$> pExpr),
      pGuardStmt
    ]

pBindStmt :: P Stmt
pBindStmt = do
  Spanned sp n <- lowerId
  sec <- pSecrecy
  _ <- sym TAssign
  e <- pExpr
  pure (Stmt (sp <> exprSpan e) (SBind n sec e))

-- | Statement-position @if@ without @else@ (spec 6.5 GuardStmt). Only
-- reached when the expression parser failed, i.e. there is no @else@.
pGuardStmt :: P Stmt
pGuardStmt = do
  (s, c, thenB, elseB) <- pIfHead
  case elseB of
    Just eb -> pure (Stmt (s <> blockSpan eb) (SExpr (Expr (s <> blockSpan eb) (EIf c thenB (Just eb)))))
    Nothing -> pure (Stmt (s <> blockSpan thenB) (SGuard c thenB))
