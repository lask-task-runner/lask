{-# LANGUAGE OverloadedStrings #-}

module Command.Lask.Parser.Argument
  ( parseArguments,
    replaceArgumentVar,
  )
where

import Control.Applicative ((<|>))
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Language.Lask.AST as AST
import Language.Lask.Error (LanguageError)
import qualified Language.Lask.Parser as P
import Language.Lask.Span
import Text.Megaparsec
  ( MonadParsec (try),
    choice,
    many,
    optional,
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseArguments :: [Text] -> Either [LanguageError Span] [AST.Argument Span]
parseArguments args =
  traverse
    (\i -> parseArgument ("arg@" <> show i) (args !! i))
    [0 .. (length args - 1)]

parseArgument :: String -> Text -> Either [LanguageError Span] (AST.Argument Span)
parseArgument name arg =
  case P.parse pArgument name arg of
    Right (e, _) -> pure e
    Left e -> Left e

replaceArgumentVar :: [(String, AST.Expr Span)] -> AST.Argument Span -> AST.Argument Span
replaceArgumentVar kvs (pos :< AST.PositionedArgument isExpanded v) =
  pos :< AST.PositionedArgument isExpanded (replaceVar kvs v)
replaceArgumentVar kvs (pos :< AST.KeywordArgument key v) =
  pos :< AST.KeywordArgument key (replaceVar kvs v)

replaceVar :: [(String, AST.Expr Span)] -> AST.Expr Span -> AST.Expr Span
replaceVar kvs v@(_ :< AST.Var name) = fromMaybe v (lookup name kvs)
replaceVar kvs (pos :< AST.Array elements) =
  pos :< AST.Array (replaceVar kvs <$> elements)
replaceVar kvs (pos :< AST.Object pairs) =
  pos
    :< AST.Object
      ( bimap
          (replaceVar kvs)
          (replaceVar kvs)
          <$> pairs
      )
replaceVar kvs (pos :< AST.Accessor obj prop) =
  pos :< AST.Accessor (replaceVar kvs obj) (replaceVar kvs prop)
replaceVar kvs (pos :< AST.Call func args) =
  pos :< AST.Call (replaceVar kvs func) args
replaceVar kvs (pos :< AST.Lambda params body t) =
  pos :< AST.Lambda params (replaceVar kvs body) t
replaceVar _ e = e

pArgument :: P.Parser (AST.Argument Span)
pArgument =
  choice
    [ try keywordArgument,
      positionedArgument
    ]
  where
    positionedArgument :: P.Parser (AST.Argument Span)
    positionedArgument =
      P.cofreeSpanned $ do
        v <- pExpr
        isExpanded <- isJust <$> optional (P.lexeme $ P.token P.TKOp $ string "...")
        pure $ AST.PositionedArgument isExpanded v
    keywordArgument :: P.Parser (AST.Argument Span)
    keywordArgument =
      P.cofreeSpanned $ do
        key <- P.lexeme $ P.tLowerIdentifier P.TKParameter
        _ <- P.lexeme $ P.token P.TKSep $ char '='
        AST.KeywordArgument key <$> pExpr

pExpr :: P.Parser (AST.Expr Span)
pExpr =
  P.lexeme $
    choice
      [ P.pNull,
        P.pBool,
        P.pNumber,
        P.pImage,
        pVar,
        P.pString,
        pImplicitString
      ]

pVar :: P.Parser (AST.Expr Span)
pVar =
  P.cofreeSpanned $
    AST.Var
      <$> P.token
        P.TKVar
        ( (:)
            <$> char '@'
            <*> many (alphaNumChar <|> char '_' <|> char '-')
        )

pImplicitString :: P.Parser (AST.Expr Span)
pImplicitString = P.cofreeSpanned (AST.String <$> many L.charLiteral)
