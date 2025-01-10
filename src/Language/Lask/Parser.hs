{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Parser
  ( parse,
    pModule,
    pExprStatement,
    pExpr,
    pType,
    Token (..),
    TokenKind (..),
  )
where

import Control.Comonad.Cofree
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (StateT (runStateT), get, put)
import Data.Functor (($>))
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Language.Lask.AST as AST
import Language.Lask.Error (LanguageError, fromParseErrorBundle)
import Language.Lask.Span (Span (Span))
import qualified Language.Lask.Span as S
import Language.Lask.Utils (tupleToCofree)
import Text.Megaparsec hiding (Token, parse, token, tokens)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = StateT [Token] (Parsec Void Text)

data Token = Token
  { tokenKind :: TokenKind,
    tokenSpan :: Span
  }
  deriving (Show, Eq)

data TokenKind
  = TKTypeVar
  | TKOp
  | TKVar
  | TKFunction
  | TKKeyword
  | TKNull
  | TKBool
  | TKNumber
  | TKString
  | TKSep
  | TKComment
  | TKParameter
  | TKTypeParameter
  | TKProperty
  | TKImage
  deriving (Show, Eq)

parse :: Parser a -> FilePath -> Text -> Either [LanguageError Span] (a, [Token])
parse parser filePath src = case runParser (runStateT parser []) filePath src of
  Left e -> Left [fromParseErrorBundle e]
  Right (a, tokens) -> Right (a, tokens)

pModule :: Parser (AST.Module Span)
pModule = do
  sc
  p1 <- getSourcePos
  statements <- many pExprStatement
  eof
  p2 <- getSourcePos
  pure $
    Span (S.fromSourcePos p1) (S.fromSourcePos p2) :< AST.Module statements

pExprStatement :: Parser (AST.Statement Span)
pExprStatement = cofreeSpanned $ do
  name <- lexeme $ tLowerIdentifier TKFunction
  maybeParams <-
    optional
      $ spanned
      $ between
        (lexeme $ token TKSep $ char '(')
        (lexeme $ token TKSep $ char ')')
      $ pParameter `sepBy` lexeme (token TKSep (char ','))
  maybeExprType <- optional $ lexeme (token TKSep $ char ':') *> pType
  _ <- lexeme $ token TKSep $ char '='
  expr@(expr' :< _) <- pExpr
  case maybeParams of
    Just (params', params) ->
      pure $
        AST.ExprStatement
          name
          (params' <> expr' :< AST.Lambda params expr maybeExprType)
    Nothing -> pure $ AST.ExprStatement name expr

pType :: Parser (AST.Type Span)
pType = do
  v <- pTPrimary
  choice
    [ pAssemblyType v,
      pure v
    ]
  where
    pAssemblyType :: AST.Type Span -> Parser (AST.Type Span)
    pAssemblyType f@(s1 :< _) = do
      (s2, ts) <-
        spanned $
          between
            (lexeme $ token TKSep $ char '[')
            (lexeme $ token TKSep $ char ']')
            (pType `sepBy` lexeme (token TKSep (char ',')))
      pure $ s1 <> s2 :< AST.AssemblyType f ts

pTPrimary :: Parser (AST.Type Span)
pTPrimary =
  choice
    [ pTypeVar,
      pArrayOrTupleType
    ]

pTypeVar :: Parser (AST.Type Span)
pTypeVar = cofreeSpanned $ AST.TypeVar <$> lexeme (tUpperIdentifier TKTypeVar)

-- array and tuple type sugar syntax
pArrayOrTupleType :: Parser (AST.Type Span)
pArrayOrTupleType = do
  (s, ts) <-
    spanned $
      between
        (lexeme $ token TKSep $ char '[')
        (lexeme $ token TKSep $ char ']')
        (pType `sepBy` lexeme (token TKSep (char ',')))
  case ts of
    [] -> fail "type"
    [t] -> pure $ s :< AST.AssemblyType (s :< AST.TypeVar "Array") [t]
    _ -> pure $ s :< AST.AssemblyType (s :< AST.TypeVar "Tuple") ts

pExpr :: Parser (AST.Expr Span)
pExpr = makeExprParser pTerm opTable

opTable :: [[Operator Parser (AST.Expr Span)]]
opTable =
  [ [ unary $ string "!"
    ],
    [ binary $ string "*",
      binary $ string "/"
    ],
    [ binary $ string "+",
      binary $ string "-"
    ],
    [ binary $ string "==",
      binary $ string "!=",
      binary $ try $ string "<" <* notFollowedBy (char '<' <|> char '|'),
      binary $ string "<=",
      binary $ try $ string ">" <* notFollowedBy (char '>'),
      binary $ string ">="
    ],
    [ binary $ string "&&",
      binary $ string "||"
    ],
    [ binary $ string "|>",
      binary $ string "<|",
      binary $ string ">>",
      binary $ string "<<"
    ]
  ]

unary :: Parser Text -> Operator Parser (AST.Expr Span)
unary name = Prefix (f <$> pOp name)
  where
    f :: AST.Expr Span -> AST.Expr Span -> AST.Expr Span
    f op@(op' :< _) v@(v' :< _) =
      (op' <> v')
        :< AST.Call op [v' :< AST.PositionedArgument False v]

binary :: Parser Text -> Operator Parser (AST.Expr Span)
binary name = InfixL (f <$> pOp name)
  where
    f :: AST.Expr Span -> AST.Expr Span -> AST.Expr Span -> AST.Expr Span
    f op a@(a' :< _) b@(b' :< _) =
      (a' <> b')
        :< AST.Call
          op
          [ a' :< AST.PositionedArgument False a,
            b' :< AST.PositionedArgument False b
          ]

pOp :: Parser Text -> Parser (AST.Expr Span)
pOp name = cofreeSpanned $ AST.Var <$> pOp'
  where
    pOp' = lexeme $ token TKOp (T.unpack <$> name)

pTerm :: Parser (AST.Expr Span)
pTerm = pPrimary >>= pChain
  where
    pChain :: AST.Expr Span -> Parser (AST.Expr Span)
    pChain v =
      choice
        [ pCall v >>= pChain,
          pBracketAccessor v >>= pChain,
          pDotAccessor v >>= pChain,
          pure v
        ]
    pCall :: AST.Expr Span -> Parser (AST.Expr Span)
    pCall f@(s1 :< _) = do
      (s2, es) <-
        spanned
          $ between
            (lexeme $ token TKSep $ char '(')
            (lexeme $ token TKSep $ char ')')
          $ pArgument `sepBy` lexeme (token TKSep (char ','))
      pure $ s1 <> s2 :< AST.Call f es
    pDotAccessor :: AST.Expr Span -> Parser (AST.Expr Span)
    pDotAccessor v@(s1 :< _) = do
      _ <- lexeme $ token TKSep $ try $ char '.' <* notFollowedBy (char '.')
      key@(s2 :< _) <- cofreeSpanned $ AST.String <$> lexeme (tIdentifier TKProperty)
      pure $ s1 <> s2 :< AST.Accessor v key
    pBracketAccessor :: AST.Expr Span -> Parser (AST.Expr Span)
    pBracketAccessor v@(s1 :< _) = do
      (s2, key) <-
        spanned $
          between
            (lexeme $ token TKSep $ char '[')
            (lexeme $ token TKSep $ char ']')
            pExpr
      pure $ s1 <> s2 :< AST.Accessor v key

pArgument :: Parser (AST.Argument Span)
pArgument =
  choice
    [ try keywordArgument,
      positionedArgument
    ]
  where
    positionedArgument :: Parser (AST.Argument Span)
    positionedArgument = cofreeSpanned $ do
      v <- pExpr
      isExpanded <- isJust <$> optional (lexeme $ token TKOp $ string "...")
      pure $ AST.PositionedArgument isExpanded v
    keywordArgument :: Parser (AST.Argument Span)
    keywordArgument = cofreeSpanned $ do
      key <- lexeme $ tLowerIdentifier TKParameter
      _ <- lexeme $ token TKSep $ char '='
      AST.KeywordArgument key <$> pExpr

pParameter :: Parser (AST.Parameter Span)
pParameter =
  choice
    [ positionedParameter,
      keywordParameter
    ]
  where
    positionedParameter :: Parser (AST.Parameter Span)
    positionedParameter = cofreeSpanned $ do
      name <- tLowerIdentifier TKParameter
      isOptional <- isJust <$> optional (token TKOp $ char '?')
      isRest <- isJust <$> optional (lexeme $ token TKOp $ string "...")
      paramType <- optional $ lexeme (token TKSep $ char ':') *> pType
      sc
      defaultExpr <- optional $ lexeme (token TKOp $ char '=') *> pExpr
      pure $ AST.PositionedParameter name isRest isOptional paramType defaultExpr
    keywordParameter :: Parser (AST.Parameter Span)
    keywordParameter = cofreeSpanned $ do
      _ <- token TKOp $ char '-'
      name <- tLowerIdentifier TKParameter
      isOptional <- isJust <$> optional (token TKOp $ char '?')
      isRest <- isJust <$> optional (lexeme $ token TKOp $ string "...")
      paramType <- optional $ lexeme (token TKSep $ char ':') *> pType
      sc
      defaultExpr <- optional $ lexeme (token TKOp $ char '=') *> pExpr
      pure $ AST.KeywordParameter name isOptional isRest paramType defaultExpr

pPrimary :: Parser (AST.Expr Span)
pPrimary =
  choice
    [ pLambda,
      pCommand,
      pImage,
      pObject,
      pArray,
      pString,
      pStringWithExprExpansion,
      pNumber,
      pBool,
      pNull,
      pVar,
      lexeme (token TKSep (char '(')) *> pExpr <* lexeme (token TKSep (char ')'))
    ]

-- command sugar syntax
pCommand :: Parser (AST.Expr Span)
pCommand = cofreeSpanned $ do
  name <- cofreeSpanned $ AST.Var <$> tCommandVar
  maybeImage <- optional $ cofreeSpanned $ AST.KeywordArgument "image" <$> pRunOnImage
  cmd <- cofreeSpanned $ AST.PositionedArgument False <$> pCommandString
  pure $ AST.Call name $ catMaybes [maybeImage, Just cmd]
  where
    tCommandVar = lexeme $ token TKVar $ ("$" ++) <$> (char '$' *> many (alphaNumChar <|> char '_'))
    pRunOnImage =
      between
        (lexeme $ token TKSep $ char '[')
        (lexeme $ token TKSep $ char ']')
        pExpr
    pCommandString = pNakedStringWithExprExpansion (string "") (lexeme $ token TKSep $ string ";")

pLambda :: Parser (AST.Expr Span)
pLambda = cofreeSpanned $ do
  _ <- lexeme $ token TKSep $ string' "\\"
  params <-
    between
      (lexeme $ token TKSep $ char '(')
      (lexeme $ token TKSep $ char ')')
      $ pParameter `sepBy` lexeme (token TKSep (char ','))
  exprType <- optional $ lexeme (token TKSep $ char ':') *> pType
  _ <- lexeme $ token TKSep $ string' "->"
  body <- pExpr
  pure $ AST.Lambda params body exprType

pVar :: Parser (AST.Expr Span)
pVar = cofreeSpanned (AST.Var <$> lexeme (tLowerIdentifier TKVar))

pImage :: Parser (AST.Expr Span)
pImage = cofreeSpanned $ AST.Image <$> tImage
  where
    tImage =
      lexeme $
        token TKImage $
          char '#'
            *> many
              ( choice
                  [ alphaNumChar,
                    char '/',
                    char ':',
                    char '.',
                    char '-',
                    char '_'
                  ]
              )

pObject :: Parser (AST.Expr Span)
pObject =
  cofreeSpanned $
    AST.Object
      <$> between
        (lexeme $ token TKSep $ char '{')
        (lexeme $ token TKSep $ char '}')
        (pPair `sepBy` lexeme (token TKSep (char ',')))
  where
    pPair :: Parser (AST.Expr Span, AST.Expr Span)
    pPair = do
      key <- pObjectKey <|> pExpr
      _ <- lexeme $ token TKSep $ char ':'
      value <- pExpr
      pure (key, value)
    pObjectKey :: Parser (AST.Expr Span)
    pObjectKey =
      tupleToCofree <$> lexeme (spanned $ AST.String <$> tLowerIdentifier TKVar)

pArray :: Parser (AST.Expr Span)
pArray =
  cofreeSpanned $
    AST.Array
      <$> between
        (lexeme $ token TKSep $ char '[')
        (lexeme $ token TKSep $ char ']')
        (pExpr `sepBy` lexeme (token TKSep (char ','))) -- TODO: Add trailing comma support.

pString :: Parser (AST.Expr Span)
pString = cofreeSpanned $ AST.String <$> pString'
  where
    pString' = lexeme $ token TKString (char '\'' *> manyTill L.charLiteral (char '\''))

pStringWithExprExpansion :: Parser (AST.Expr Span)
pStringWithExprExpansion =
  pNakedStringWithExprExpansion
    (token TKString $ char '"')
    (token TKString $ char '"')

-- | String literal parser.
-- It can include expression expansion. e.g. "hello #{ "world" }".
pNakedStringWithExprExpansion :: Parser a -> Parser b -> Parser (AST.Expr Span)
pNakedStringWithExprExpansion startSymbol endSymbol = do
  (s, es) <- lexeme $ spanned $ startSymbol *> manyTill (pExprInString <|> pSomeChar) endSymbol
  case es of
    [] -> pure $ s :< AST.String ""
    [_ :< e] -> pure (s :< e)
    _ ->
      pure $
        s
          :< AST.Call
            (s :< AST.Var "concat")
            [s :< AST.PositionedArgument False (s :< AST.Array es)]
  where
    pSomeChar :: Parser (AST.Expr Span)
    pSomeChar = do
      (s, str) <-
        spanned $
          token TKString $
            someTillWithoutEnd L.charLiteral (endSymbol $> () <|> string "#{" $> ())
      pure $ s :< AST.String str
    pExprInString :: Parser (AST.Expr Span)
    pExprInString = between (lexeme $ token TKSep $ string "#{") (token TKSep $ string "}") pExpr
    someTillWithoutEnd :: Parser a -> Parser b -> Parser [a]
    someTillWithoutEnd p end = someTill p (lookAhead end)

pNumber :: Parser (AST.Expr Span)
pNumber = cofreeSpanned $ AST.Number <$> tNumber
  where
    tNumber = lexeme (token TKNumber (L.signed sc L.scientific))

pBool :: Parser (AST.Expr Span)
pBool = cofreeSpanned $ AST.Bool <$> tBool
  where
    tBool = lexeme (token TKBool (keyword "true" $> True <|> keyword "false" $> False))

pNull :: Parser (AST.Expr Span)
pNull = cofreeSpanned $ AST.Null <$ tNull
  where
    tNull = lexeme (token TKNull (keyword "null"))

tIdentifier :: TokenKind -> Parser String
tIdentifier kind =
  token
    kind
    ( (:)
        <$> (lowerChar <|> upperChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
    )

tLowerIdentifier :: TokenKind -> Parser String
tLowerIdentifier kind =
  token
    kind
    ( (:)
        <$> (lowerChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
    )

tUpperIdentifier :: TokenKind -> Parser String
tUpperIdentifier kind =
  token
    kind
    ( (:)
        <$> (upperChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
    )

getTokens :: Parser [Token]
getTokens = get

addTokens :: [Token] -> Parser ()
addTokens ts = get >>= put . (++ ts)

cofreeSpanned :: Parser (f (Cofree f Span)) -> Parser (Cofree f Span)
cofreeSpanned a = tupleToCofree <$> spanned a

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- length <$> getTokens
  x <- p
  end <- length <$> getTokens
  tokens <- take (end - start) . drop start <$> getTokens
  pure (tokenSpan (head tokens) <> tokenSpan (last tokens), x)

keyword :: Text -> Parser ()
keyword k = try $ void (string k) <* notFollowedBy alphaNumChar

token :: TokenKind -> Parser a -> Parser a
token kind parser = do
  (s, v) <- tokenSpanned parser
  addTokens [Token kind s]
  pure v
  where
    tokenSpanned :: Parser a -> Parser (Span, a)
    tokenSpanned p = do
      start <- getSourcePos
      x <- p
      end <- getSourcePos
      pure (S.Span (S.fromSourcePos start) (S.fromSourcePos end), x)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineComment blockComment

lineComment :: Parser ()
lineComment = token TKComment $ L.skipLineComment "//"

blockComment :: Parser ()
blockComment = token TKComment $ L.skipBlockComment "/*" "*/"
