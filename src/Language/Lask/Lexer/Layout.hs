-- | Newline-significance pass (spec 3.3 and 6.5).
--
-- The lexer emits every newline as a 'TNewline' token. This pass
-- removes the newlines that are /continuations/ rather than statement
-- or declaration terminators, and collapses runs of newlines, so the
-- parser can treat every remaining 'TNewline' as a terminator.
--
-- A newline is a continuation (dropped) when any of these holds
-- (spec 6.5):
--
--   * it occurs while a @(@, @[@ or object-literal @{@ is still open
--     (do\/if\/for\/try\/catch\/finally __block__ braces do /not/
--     suppress newlines: statements inside blocks are
--     newline-terminated);
--   * the previous token is a continuation token:
--     @=@, @,@, @:@, @->@, a binary operator, or @!@;
--   * the next token is a binary operator, @else@, @catch@ or
--     @finally@;
--   * additionally, as normalization: at start of input and end of
--     input, right after a block @{@, right before @}@, and right
--     after @;@ (terminators there are optional per spec 5\/6.5).
--
-- A leading @(@ or @[@ on the next line never continues the previous
-- statement (spec 6.5), which is satisfied here simply because those
-- tokens are not continuation tokens.
--
-- Block braces are distinguished from object-literal braces by the
-- token immediately before @{@: @do@, @try@, @finally@, @else@ and
-- @)@ (the header close of @if@\/@for@\/@catch@) open blocks; braces
-- in any other position are object literals. The two can never occur
-- in the same position in valid programs, because an expression can
-- never directly follow another expression.
module Language.Lask.Lexer.Layout
  ( layout,
  )
where

import Language.Lask.Lexer.Token

data Delim = DParen | DBracket | DObj | DBlock
  deriving (Show, Eq)

layout :: [Spanned Token] -> [Spanned Token]
layout = go [] Nothing Nothing
  where
    go :: [Delim] -> Maybe Token -> Maybe Token -> [Spanned Token] -> [Spanned Token]
    go _ _ _ [] = []
    go stack prev2 prev (t : rest)
      | TNewline <- spannedValue t =
          let rest' = dropWhile ((== TNewline) . spannedValue) rest
           in if keepNewline stack prev2 prev rest'
                then t : go stack prev (Just TNewline) rest'
                else go stack prev2 prev rest'
      | otherwise =
          let v = spannedValue t
              stack' = updateStack stack prev v
           in t : go stack' prev (Just v) rest

    keepNewline :: [Delim] -> Maybe Token -> Maybe Token -> [Spanned Token] -> Bool
    keepNewline stack prev2 prev next
      | d : _ <- stack, d /= DBlock = False
      | Nothing <- prev = False -- start of input
      | Just p <- prev, isContinuationEnd p, not (isTypeClose p prev2) = False
      | Just TSemi <- prev = False
      | Just TLBrace <- prev = False -- right after a block opener
      | n : _ <- next, startsContinuation (spannedValue n) = False
      | n : _ <- next, spannedValue n == TRBrace = False
      | [] <- next = False -- end of input
      | otherwise = True

    -- A line-end @>@ or @>>@ directly after a type name (or another
    -- closing angle) closes a generic type argument list, not a
    -- comparison\/composition: it terminates the declaration instead
    -- of continuing it. Type names ('TUpperId') cannot appear as
    -- expression operands, so this is unambiguous. @>=@ stays a
    -- continuation: its @=@ half starts the declaration body.
    isTypeClose :: Token -> Maybe Token -> Bool
    isTypeClose p prev2 = closingAngle p && maybe False typeCloseContext prev2
      where
        closingAngle (TOp OpGt) = True
        closingAngle (TOp OpCompR) = True
        closingAngle _ = False
        typeCloseContext (TUpperId _) = True
        typeCloseContext (TOp OpGt) = True
        typeCloseContext (TOp OpCompR) = True
        typeCloseContext (TOp OpLt) = True -- empty Record<>
        typeCloseContext _ = False

    updateStack :: [Delim] -> Maybe Token -> Token -> [Delim]
    updateStack stack prev v = case v of
      TLParen -> DParen : stack
      TLBracket -> DBracket : stack
      TLBrace -> classifyBrace prev : stack
      TRParen -> popIf DParen stack
      TRBracket -> popIf DBracket stack
      TRBrace -> popBrace stack
      _ -> stack

    classifyBrace :: Maybe Token -> Delim
    classifyBrace prev = case prev of
      Just (TKw KDo) -> DBlock
      Just (TKw KTry) -> DBlock
      Just (TKw KFinally) -> DBlock
      Just (TKw KElse) -> DBlock
      Just TRParen -> DBlock -- if (...) { / for (...) { / catch (e) {
      _ -> DObj

    popIf :: Delim -> [Delim] -> [Delim]
    popIf d (x : xs) | x == d = xs
    popIf _ s = s

    popBrace :: [Delim] -> [Delim]
    popBrace (x : xs) | x == DObj || x == DBlock = xs
    popBrace s = s
