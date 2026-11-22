{-# LANGUAGE OverloadedStrings #-}


-- | What may be written at a position, read from the tokens before it.
--
-- Completion asks this before it gathers candidates, so that a type
-- annotation offers types, @within@ offers access names, and the
-- braces of an environment or an access constraint offer their fields,
-- instead of every name in scope. It reads tokens rather than the
-- syntax tree because the buffer is half-written whenever completion
-- is asked for: the tokens before the cursor are there even when the
-- declaration they belong to does not parse yet.
module Language.LSP.Lask.Context
  ( Where (..),
    Fields (..),
    whereAt,
    whereAfter,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Lexer (lexTokens)
import qualified Language.Lask.Lexer.Token as Tok

-- | The kind of thing that may be written at a position.
data Where
  = -- | An expression: the names in scope.
    InExpr
  | -- | A type (spec 4.2).
    InType
  | -- | The head of an arm of a @case@ on a value: a type, or @else@
    -- (spec 6.9).
    InCaseHead
  | -- | The name of an access constraint, after @within@ (spec 10.11).
    InWithin
  | -- | A field of a literal whose fields are fixed, and the fields
    -- already written in it.
    InFields Fields [Text]
  deriving (Show, Eq)

-- | Which fixed set of fields.
data Fields
  = -- | An access constraint, @{ files: "ro", ... }@ (spec 10.11).
    AccessFields
  | -- | The run options of an environment, @#alpine{ network: "none" }@
    -- (spec 10.2).
    RunOptionFields
  | -- | The image options of an environment head, @#alpine(platform = ...)@
    -- (spec 6.7).
    ImageOptionFields
  deriving (Show, Eq)

-- | What may be written at a 0-based line and column of a source: the
-- column where the word being written starts.
whereAt :: FilePath -> Text -> Int -> Int -> Where
whereAt path src line col =
  whereAfter path (T.intercalate "\n" (take line ls <> [T.take col (mconcat (take 1 (drop line ls)))]))
  where
    ls = T.splitOn "\n" src

-- | What may be written right after a source, which ends where the
-- word being written starts. A source the lexer does not get through
-- (the cursor inside a string, say) is an expression, as everything
-- was before this module.
whereAfter :: FilePath -> Text -> Where
whereAfter path src = case lexTokens path src of
  Left _ -> InExpr
  Right ts -> decide (foldl step start (map Tok.spannedValue ts))

-- | An open bracket.
data Frame
  = Paren Opened
  | Bracket
  | Angle
  | Brace Braced
  deriving (Show, Eq)

-- | What came before a parenthesis.
data Opened
  = -- | The scrutinee of a @case@: the braces after it hold arms with
    -- type heads.
    CaseScrutinee
  | -- | The image options of an environment head.
    EnvHeadArgs
  | -- | The head of a block: @if (...) {@, @catch (e) {@.
    BlockHead
  | -- | The head of a loop, @for (x: xs) {@: a value follows its colon.
    ForHead
  | OtherParen
  deriving (Show, Eq)

-- | What a pair of braces holds.
data Braced
  = Block
  | CaseArms
  | FixedFields Fields [Text]
  | -- | A record or map literal, whose keys are free.
    Literal
  deriving (Show, Eq)

data St = St
  { stStack :: [Frame],
    -- | The tokens since the last newline outside any bracket, newest
    -- first: enough to tell a type alias, an access declaration and a
    -- @within@ apart.
    stLine :: [Tok.Token],
    -- | The tokens just read, newest first.
    stRecent :: [Tok.Token],
    -- | A type may come next.
    stTypeNext :: Bool,
    -- | A type is being written: an upper-case name, a qualified one,
    -- or a closed type argument list, which @|@ or @<@ may continue.
    stInType :: Bool,
    -- | The parenthesis just closed was opened as this.
    stClosed :: Maybe Opened
  }

start :: St
start = St [] [] [] False False Nothing

step :: St -> Tok.Token -> St
step st t =
  st'
    { stRecent = t : take 3 (stRecent st),
      stLine = case (t, stStack st') of
        (Tok.TNewline, []) -> []
        _ -> t : stLine st
    }
  where
    st' = advance st t

advance :: St -> Tok.Token -> St
advance st t = case t of
  Tok.TLParen ->
    reset {stStack = Paren opened : stack}
    where
      opened = case prev of
        Just (Tok.TKw Tok.KCase) -> CaseScrutinee
        Just (Tok.TEnvHead _) -> EnvHeadArgs
        Just (Tok.TKw Tok.KFor) -> ForHead
        Just (Tok.TKw k) | k `elem` [Tok.KIf, Tok.KCatch] -> BlockHead
        _ -> OtherParen
  Tok.TRParen -> case stack of
    Paren o : rest -> reset {stStack = rest, stClosed = Just o}
    _ -> reset {stStack = drop 1 stack}
  Tok.TLBracket -> reset {stStack = Bracket : stack}
  Tok.TRBracket -> reset {stStack = drop 1 stack}
  Tok.TLBrace -> reset {stStack = Brace braced : stack}
    where
      braced = case prev of
        Just (Tok.TEnvHead _) -> FixedFields RunOptionFields []
        Just (Tok.TLowerId "within") -> FixedFields AccessFields []
        Just Tok.TAssign | accessDecl -> FixedFields AccessFields []
        Just (Tok.TKw k) | k `elem` [Tok.KDo, Tok.KTry, Tok.KFinally, Tok.KElse] -> Block
        Just (Tok.TKw Tok.KCase) -> Block
        Just Tok.TRParen -> case stClosed st of
          Just CaseScrutinee -> CaseArms
          Just BlockHead -> Block
          Just ForHead -> Block
          -- The run options after the image options: #alpine(...){...}.
          Just EnvHeadArgs -> FixedFields RunOptionFields []
          _ -> Literal
        _ -> Literal
      -- `access Name = {`, at the top level.
      accessDecl = case reverse (stLine st) of
        (Tok.TLowerId "access" : _) -> True
        (Tok.TKw k : Tok.TLowerId "access" : _) | k `elem` [Tok.KExport, Tok.KInternal] -> True
        _ -> False
  Tok.TRBrace -> reset {stStack = drop 1 stack}
  -- The name of a field written in fixed braces.
  Tok.TLowerId n
    | Brace (FixedFields f seen) : rest <- stack,
      keyPosition ->
        reset {stStack = Brace (FixedFields f (n : seen)) : rest}
    | Paren EnvHeadArgs : _ <- stack,
      keyPosition ->
        reset
    -- The namespace of a qualified type, `m.Type`.
    | stTypeNext st -> typing
  Tok.TUpperId _ | stTypeNext st -> typing
  Tok.TDot | stInType st -> expectType
  Tok.TPipe | stInType st -> expectType
  Tok.TQuestion | stInType st -> st
  Tok.TOp Tok.OpLt
    | stInType st -> expectType {stStack = Angle : stack}
  Tok.TOp Tok.OpGt
    | Angle : rest <- stack -> typing {stStack = rest}
  Tok.TComma
    | Angle : _ <- stack -> expectType
  Tok.TColon
    -- A field of a literal: a value follows.
    | Brace b : _ <- stack, b /= Block -> reset
    | Paren ForHead : _ <- stack -> reset
    | otherwise -> expectType
  -- `type Name = ...` and `type Name<T> = ...`.
  Tok.TAssign
    | null stack, (Tok.TKw Tok.KType : _) <- reverse (stLine st) -> expectType
    | (Tok.TKw k : Tok.TKw Tok.KType : _) <- reverse (stLine st), null stack, k `elem` [Tok.KExport, Tok.KInternal] -> expectType
  _ -> reset
  where
    stack = stStack st
    prev = case stRecent st of
      (p : _) -> Just p
      [] -> Nothing
    reset = st {stTypeNext = False, stInType = False, stClosed = Nothing}
    expectType = reset {stTypeNext = True}
    typing = reset {stInType = True}
    -- Right after the opening brace, or after a comma or newline.
    keyPosition = case prev of
      Just p -> p `elem` [Tok.TLBrace, Tok.TLParen, Tok.TComma, Tok.TNewline]
      Nothing -> False

decide :: St -> Where
decide st
  | stTypeNext st = InType
  | otherwise = case (stRecent st, stStack st) of
      (Tok.TLowerId "within" : _, _) -> InWithin
      (p : _, Brace (FixedFields f seen) : _) | opensKey p -> InFields f seen
      (p : _, Paren EnvHeadArgs : _) | opensKey p -> InFields ImageOptionFields []
      -- An arm whose body goes on to the next line is not a new arm.
      (Tok.TNewline : Tok.TArrow : _, Brace CaseArms : _) -> InExpr
      (p : _, Brace CaseArms : _) | p `elem` [Tok.TLBrace, Tok.TNewline] -> InCaseHead
      _ -> InExpr
  where
    opensKey p = p `elem` [Tok.TLBrace, Tok.TLParen, Tok.TComma, Tok.TNewline]
