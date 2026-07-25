{-# LANGUAGE TypeFamilies #-}

-- | A megaparsec 'MP.Stream' instance over the lexer's token list, so
-- the parser can consume 'Spanned' 'Token's with correct source
-- positions in error messages.
module Language.Lask.Syntax.TokenStream
  ( TokStream (..),
    renderToken,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Lask.Lexer.Token
import Language.Lask.Span (Span (..), toSourcePos)
import qualified Text.Megaparsec as MP

newtype TokStream = TokStream {unTokStream :: [Spanned Token]}
  deriving (Show)

instance MP.Stream TokStream where
  type Token TokStream = Spanned Token
  type Tokens TokStream = [Spanned Token]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokStream []) = Nothing
  take1_ (TokStream (t : ts)) = Just (t, TokStream ts)
  takeN_ n (TokStream s)
    | n <= 0 = Just ([], TokStream s)
    | null s = Nothing
    | otherwise = let (a, b) = splitAt n s in Just (a, TokStream b)
  takeWhile_ f (TokStream s) = let (a, b) = span f s in (a, TokStream b)

instance MP.VisualStream TokStream where
  showTokens _ ts = unwords (map (renderToken . spannedValue) (NE.toList ts))
  tokensLength _ = length . NE.toList

instance MP.TraversableStream TokStream where
  reachOffset o pst =
    let delta = o - MP.pstateOffset pst
        TokStream input = MP.pstateInput pst
        (skipped, rest) = splitAt delta input
        pos = case rest of
          (Spanned (Span s _) _ : _) -> toSourcePos s
          _ -> case reverse skipped of
            (Spanned (Span _ e) _ : _) -> toSourcePos e
            _ -> MP.pstateSourcePos pst
     in ( Nothing,
          pst
            { MP.pstateInput = TokStream rest,
              MP.pstateOffset = max o (MP.pstateOffset pst),
              MP.pstateSourcePos = pos
            }
        )

renderToken :: Token -> String
renderToken t = case t of
  TNumber n -> show n
  TRawString s -> "'" <> T.unpack s <> "'"
  TString _ -> "string"
  TBool True -> "true"
  TBool False -> "false"
  TNull -> "null"
  TLowerId n -> T.unpack n
  TUpperId n -> T.unpack n
  TEnvHead n -> "#" <> T.unpack n
  TCommand {} -> "command"
  TKw k -> renderKeyword k
  TOp o -> renderOp o
  TLParen -> "("
  TRParen -> ")"
  TLBracket -> "["
  TRBracket -> "]"
  TLBrace -> "{"
  TRBrace -> "}"
  TComma -> ","
  TColon -> ":"
  TSemi -> ";"
  TDot -> "."
  TArrow -> "->"
  TAssign -> "="
  TBackslash -> "\\"
  TDashDash -> "--"
  TEllipsis -> "..."
  TNewline -> "newline"

renderKeyword :: Keyword -> String
renderKeyword k = case k of
  KImport -> "import"
  KFrom -> "from"
  KAs -> "as"
  KType -> "type"
  KDo -> "do"
  KAsync -> "async"
  KAwait -> "await"
  KIf -> "if"
  KElse -> "else"
  KFor -> "for"
  KReturn -> "return"
  KTry -> "try"
  KCatch -> "catch"
  KFinally -> "finally"

renderOp :: Op -> String
renderOp o = case o of
  OpNot -> "!"
  OpMul -> "*"
  OpDiv -> "/"
  OpAdd -> "+"
  OpSub -> "-"
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpAnd -> "&&"
  OpOr -> "||"
  OpPipeR -> "|>"
  OpPipeL -> "<|"
  OpCompR -> ">>"
  OpCompL -> "<<"
