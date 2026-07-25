{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for Lask (spec chapter 3, plus the lexical rules of 6.6
-- command expressions and 6.7 environment expressions).
--
-- Produces a flat stream of 'Spanned' 'Token's in which newlines are
-- explicit ('TNewline'). Statement-continuation rules (spec 6.5) are
-- applied afterwards by "Language.Lask.Lexer.Layout".
--
-- Interpolations (@#{...}@) and the optional @[env]@ part of command
-- expressions are captured as nested token streams inside the token,
-- so the parser never needs lexer modes.
module Language.Lask.Lexer
  ( lexTokens,
    lexLayout,
  )
where

import Data.Char (chr, isDigit, isHexDigit)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode (ErrorCode (ESyntaxUnexpectedToken), Stage (StageSyntax))
import Language.Lask.Lexer.Layout (layout)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Span (Span), fromSourcePos)
import Text.Megaparsec hiding (Token, Tokens)
import Text.Megaparsec.Char (char)

type Lexer = Parsec Void Text

-- | Tokenize a module source. Newlines are kept as 'TNewline'.
lexTokens :: FilePath -> Text -> Either Diagnostic [Spanned Token]
lexTokens file src =
  case runParser pModuleTokens file src of
    Left bundle -> Left (bundleToDiagnostic bundle)
    Right ts -> Right ts

-- | Tokenize and apply the newline-significance rules (spec 6.5).
lexLayout :: FilePath -> Text -> Either Diagnostic [Spanned Token]
lexLayout file src = layout <$> lexTokens file src

pModuleTokens :: Lexer [Spanned Token]
pModuleTokens = sc *> manyTill (spanned pToken <* sc) eof

-- | Skip spaces, tabs and comments (but not newlines).
sc :: Lexer ()
sc =
  skipMany $
    choice
      [ skipSome (satisfy (\c -> c == ' ' || c == '\t')),
        lineComment,
        blockComment
      ]

lineComment :: Lexer ()
lineComment = do
  _ <- chunk "//"
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  pure ()

-- | Block comments nest (spec 3.1).
blockComment :: Lexer ()
blockComment = do
  _ <- chunk "/*"
  go 1
  where
    go :: Int -> Lexer ()
    go 0 = pure ()
    go depth =
      choice
        [ chunk "*/" *> go (depth - 1),
          chunk "/*" *> go (depth + 1),
          anySingle *> go depth,
          fail "unterminated block comment"
        ]

spanned :: Lexer Token -> Lexer (Spanned Token)
spanned p = do
  start <- getSourcePos
  t <- p
  end <- getSourcePos
  pure (Spanned (Span (fromSourcePos start) (fromSourcePos end)) t)

pToken :: Lexer Token
pToken =
  choice
    [ TNewline <$ pNewline,
      pCommand,
      pEnvHead,
      pNumber,
      pRawString,
      pInterpString,
      pIdentOrKeyword,
      pPunct
    ]

pNewline :: Lexer ()
pNewline = () <$ chunk "\r\n" <|> () <$ char '\n'

-- Numbers ---------------------------------------------------------------

-- | Unsigned decimal with optional fraction (spec 3.3). @1.foo@ lexes
-- as the number @1@ followed by an accessor dot.
pNumber :: Lexer Token
pNumber = do
  intPart <- takeWhile1P (Just "digit") isDigit
  fracPart <- optional $ try $ do
    _ <- char '.'
    takeWhile1P (Just "digit") isDigit
  let txt = intPart <> maybe "" ("." <>) fracPart
  pure (TNumber (read (T.unpack txt)))

-- Strings ---------------------------------------------------------------

-- | Raw string @'...'@: no escapes, may contain newlines, cannot
-- contain @'@ (spec 3.3).
pRawString :: Lexer Token
pRawString = do
  _ <- char '\''
  content <- takeWhileP Nothing (/= '\'')
  _ <- char '\'' <?> "closing '"
  pure (TRawString content)

-- | Interpreted string @"..."@ with escapes and @#{...}@ interpolation.
pInterpString :: Lexer Token
pInterpString = do
  _ <- char '"'
  parts <- many pStrPart
  _ <- char '"' <?> "closing \""
  pure (TString (mergeChunks parts))
  where
    pStrPart =
      choice
        [ Interp <$> (chunk "#{" *> pNestedUntil TLBrace TRBrace),
          Chunk <$> pEscape,
          Chunk <$> takeWhile1P Nothing plainChar,
          Chunk "#" <$ char '#' -- '#' not followed by '{'
        ]
    plainChar c =
      c /= '"' && c /= '\\' && c /= '\n' && c /= '\r' && c /= '#'

pEscape :: Lexer Text
pEscape = do
  _ <- char '\\'
  c <- anySingle <?> "escape character"
  case c of
    '\\' -> pure "\\"
    '"' -> pure "\""
    'n' -> pure "\n"
    'r' -> pure "\r"
    't' -> pure "\t"
    'u' -> do
      _ <- char '{'
      hexDigits <- takeWhile1P (Just "hex digit") isHexDigit
      _ <- char '}'
      let n = T.foldl' (\acc d -> acc * 16 + Char.digitToInt d) 0 hexDigits
      if n > 0x10FFFF
        then fail "unicode escape out of range"
        else pure (T.singleton (chr n))
    _ -> fail ("invalid escape sequence: \\" <> [c])

-- Environment heads -----------------------------------------------------

-- | @#@ followed by at least one @env_char@ (spec 3.3). A bare @#@ is
-- a lexical error outside interpolation contexts.
pEnvHead :: Lexer Token
pEnvHead = do
  _ <- char '#'
  body <- takeWhile1P (Just "environment character") isEnvChar
  pure (TEnvHead body)

isEnvChar :: Char -> Bool
isEnvChar c =
  Char.isAsciiLower c
    || Char.isAsciiUpper c
    || isDigit c
    || c `elem` ("/:.-_@" :: String)

-- Identifiers and keywords ----------------------------------------------

pIdentOrKeyword :: Lexer Token
pIdentOrKeyword = do
  first <- satisfy (\c -> Char.isAsciiLower c || Char.isAsciiUpper c || c == '_')
  rest <- takeWhileP Nothing isIdentChar
  let word = T.cons first rest
  pure $ case keywordFromText word of
    Just k -> TKw k
    Nothing
      | word == "true" -> TBool True
      | word == "false" -> TBool False
      | word == "null" -> TNull
      | Char.isAsciiUpper first -> TUpperId word
      | otherwise -> TLowerId word

isIdentChar :: Char -> Bool
isIdentChar c =
  Char.isAsciiLower c || Char.isAsciiUpper c || isDigit c || c == '_'

-- Command expressions ----------------------------------------------------

-- | @$[stream][ \[env\] ] shell-string@ up to (not including) the
-- terminating newline (spec 6.6). Stream selector and @[env]@ must
-- directly follow @$@ without whitespace; otherwise they are part of
-- the command string.
pCommand :: Lexer Token
pCommand = do
  _ <- char '$'
  stream <-
    option
      StreamOut
      ( StreamOut <$ char '1'
          <|> StreamErr <$ char '2'
          <|> StreamAll <$ char '*'
      )
  envToks <- optional (char '[' *> pNestedUntil TLBracket TRBracket)
  parts <- pCommandString
  pure (TCommand stream envToks parts)

pCommandString :: Lexer [StrPart]
pCommandString = do
  _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t') -- leading trim
  parts <- many pCmdPart
  pure (trimTrailing (mergeChunks parts))
  where
    pCmdPart =
      choice
        [ Chunk "#{" <$ chunk "\\#{",
          Chunk "" <$ try (char '\\' *> pNewline), -- line continuation
          Interp <$> (chunk "#{" *> pNestedUntil TLBrace TRBrace),
          Chunk <$> takeWhile1P Nothing plainChar,
          Chunk . T.singleton <$> satisfy (\c -> c /= '\n' && c /= '\r')
        ]
    plainChar c =
      c /= '\n' && c /= '\r' && c /= '\\' && c /= '#'

-- | Lex a nested token stream until the matching closer (exclusive).
-- Newlines inside the capture are dropped: the capture is by
-- definition enclosed in brackets/braces, where newlines are not
-- significant (spec 6.5).
pNestedUntil :: Token -> Token -> Lexer [Spanned Token]
pNestedUntil openTok closeTok = go (1 :: Int) []
  where
    go depth acc = do
      sc
      t <- spanned pToken <?> "token before closing delimiter"
      let v = spannedValue t
      if v == closeTok && depth == 1
        then pure (reverse acc)
        else
          if v == closeTok
            then go (depth - 1) (t : acc)
            else
              if v == openTok
                then go (depth + 1) (t : acc)
                else
                  if v == TNewline
                    then go depth acc
                    else go depth (t : acc)

-- String part helpers ----------------------------------------------------

mergeChunks :: [StrPart] -> [StrPart]
mergeChunks = filter notEmpty . foldr step []
  where
    step (Chunk a) (Chunk b : rest) = Chunk (a <> b) : rest
    step p rest = p : rest
    notEmpty (Chunk "") = False
    notEmpty _ = True

trimTrailing :: [StrPart] -> [StrPart]
trimTrailing ps = case reverse ps of
  (Chunk c : rest) ->
    let c' = T.dropWhileEnd (\x -> x == ' ' || x == '\t') c
     in reverse (if T.null c' then rest else Chunk c' : rest)
  _ -> ps

-- Punctuation and operators ----------------------------------------------

pPunct :: Lexer Token
pPunct =
  choice
    [ TEllipsis <$ chunk "...",
      TArrow <$ chunk "->",
      TDashDash <$ chunk "--",
      TOp OpPipeR <$ chunk "|>",
      TOp OpOr <$ chunk "||",
      TOp OpPipeL <$ chunk "<|",
      TOp OpCompL <$ chunk "<<",
      TOp OpLe <$ chunk "<=",
      TOp OpCompR <$ chunk ">>",
      TOp OpGe <$ chunk ">=",
      TOp OpEq <$ chunk "==",
      TOp OpNe <$ chunk "!=",
      TOp OpAnd <$ chunk "&&",
      TOp OpNot <$ char '!',
      TOp OpLt <$ char '<',
      TOp OpGt <$ char '>',
      TAssign <$ char '=',
      TOp OpAdd <$ char '+',
      TOp OpSub <$ char '-',
      TOp OpMul <$ char '*',
      TOp OpDiv <$ char '/',
      TLParen <$ char '(',
      TRParen <$ char ')',
      TLBracket <$ char '[',
      TRBracket <$ char ']',
      TLBrace <$ char '{',
      TRBrace <$ char '}',
      TComma <$ char ',',
      TColon <$ char ':',
      TSemi <$ char ';',
      TDot <$ char '.',
      TBackslash <$ char '\\'
    ]

-- Diagnostics -------------------------------------------------------------

bundleToDiagnostic :: ParseErrorBundle Text Void -> Diagnostic
bundleToDiagnostic bundle =
  let err = head (bagToList (bundleErrors bundle))
      (_, posState) = reachOffset (errorOffset err) (bundlePosState bundle)
      pos = fromSourcePos (pstateSourcePos posState)
      msg = T.pack (parseErrorTextPretty err)
   in mkDiagnostic
        ESyntaxUnexpectedToken
        StageSyntax
        (Span pos pos)
        (T.strip msg)
  where
    bagToList = foldr (:) []
