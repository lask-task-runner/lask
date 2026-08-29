{-# LANGUAGE OverloadedStrings #-}

-- | Token definitions for the Lask lexer (spec chapter 3, plus the
-- lexical parts of 6.6 command expressions and 6.7 environment heads).
module Language.Lask.Lexer.Token
  ( Token (..),
    Keyword (..),
    Op (..),
    StrPart (..),
    CmdStream (..),
    Spanned (..),
    isBinaryOp,
    isContinuationEnd,
    startsContinuation,
    keywordFromText,
    keywordText,
    stripToken,
    stripTokens,
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Language.Lask.Span (Span (NoSpan))

-- | A value paired with its source span.
data Spanned a = Spanned
  { spannedSpan :: Span,
    spannedValue :: a
  }
  deriving (Show, Eq, Ord)

-- | Reserved words (spec 3.3). @true@/@false@/@null@ are lexed as
-- literal tokens instead.
data Keyword
  = KImport
  | KFrom
  | KAs
  | KType
  | KDo
  | KAsync
  | KAwait
  | KIf
  | KElse
  | KFor
  | KReturn
  | KTry
  | KCatch
  | KFinally
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Operators (spec 6.2). 'OpNot' is the only unary operator.
data Op
  = OpNot
  | OpMul
  | OpDiv
  | OpAdd
  | OpSub
  | OpEq
  | OpNe
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpAnd
  | OpOr
  | OpPipeR -- ^ @|>@
  | OpPipeL -- ^ @<|@
  | OpCompR -- ^ @>>@
  | OpCompL -- ^ @<<@
  deriving (Show, Eq, Ord, Enum, Bounded)

isBinaryOp :: Op -> Bool
isBinaryOp OpNot = False
isBinaryOp _ = True

-- | A piece of an interpreted string or command string: either a
-- literal chunk or an interpolation holding its own token stream.
data StrPart
  = Chunk Text
  | Interp [Spanned Token]
  deriving (Show, Eq, Ord)

-- | Which part of a command result a command expression selects
-- (spec 6.6): @$@\/@$1@ = stdout, @$2@ = stderr, @$*@ = whole result.
data CmdStream = StreamOut | StreamErr | StreamAll
  deriving (Show, Eq, Ord)

data Token
  = TNumber Scientific
  | TRawString Text
  | TString [StrPart]
  | TBool Bool
  | TNull
  | TLowerId Text
  | TUpperId Text
  | TEnvHead Text
  -- ^ The text after @#@ (e.g. @local@, @alpine:3.12@).
  | TCommand CmdStream (Maybe [Spanned Token]) [StrPart]
  -- ^ Stream selector, optional @[env]@ token stream, command string.
  | TKw Keyword
  | TOp Op
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TComma
  | TColon
  | TSemi
  | TDot
  | TArrow
  | TAssign
  | TBackslash
  | TDashDash
  | -- | @!!@, the secret-binding marker (spec 6.10). Like 'TDashDash',
    -- this is its own lexeme and is never read as two 'OpNot's.
    TBangBang
  | TEllipsis
  | TNewline
  deriving (Show, Eq, Ord)

-- | Line-end tokens after which a newline continues the statement
-- (spec 6.5): @=@, @,@, @:@, @->@, any binary operator, @!@.
isContinuationEnd :: Token -> Bool
isContinuationEnd t = case t of
  TAssign -> True
  TComma -> True
  TColon -> True
  TArrow -> True
  TOp _ -> True -- all binary operators and '!'
  _ -> False

-- | Line-start tokens that continue the previous statement
-- (spec 6.5): binary operators, @else@, @catch@, @finally@.
startsContinuation :: Token -> Bool
startsContinuation t = case t of
  TOp op -> isBinaryOp op
  TKw KElse -> True
  TKw KCatch -> True
  TKw KFinally -> True
  _ -> False

keywordFromText :: Text -> Maybe Keyword
keywordFromText t = case t of
  "import" -> Just KImport
  "from" -> Just KFrom
  "as" -> Just KAs
  "type" -> Just KType
  "do" -> Just KDo
  "async" -> Just KAsync
  "await" -> Just KAwait
  "if" -> Just KIf
  "else" -> Just KElse
  "for" -> Just KFor
  "return" -> Just KReturn
  "try" -> Just KTry
  "catch" -> Just KCatch
  "finally" -> Just KFinally
  _ -> Nothing

-- | The spelling of a reserved word.
--
-- >>> keywordText KImport
-- "import"
keywordText :: Keyword -> Text
keywordText k = case k of
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

-- | Erase spans recursively (nested streams included); test helper.
stripToken :: Token -> Token
stripToken t = case t of
  TString ps -> TString (map stripPart ps)
  TCommand s env ps -> TCommand s (fmap stripTokens' env) (map stripPart ps)
  _ -> t
  where
    stripPart (Chunk c) = Chunk c
    stripPart (Interp ts) = Interp (stripTokens' ts)
    stripTokens' = map (Spanned NoSpan . stripToken . spannedValue)

stripTokens :: [Spanned Token] -> [Token]
stripTokens = map (stripToken . spannedValue)
