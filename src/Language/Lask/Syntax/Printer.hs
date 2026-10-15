{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The source formatter behind @lask fmt@ (spec 11.11).
--
-- The surface AST drops what a formatter has to keep — comments, blank
-- lines, how a literal was spelt — so a module is printed from its AST
-- with the source beside it:
--
--   * literals, command expressions and import paths are copied from
--     the source by their spans, and so are the parentheses around an
--     expression, which the parser keeps only in the span;
--   * a comment, which the lexer reports by its span, is claimed by the
--     declaration, statement, case arm or list item it sits before or
--     after. An item with a comment inside it that nothing claimed is
--     copied from the source as written, so no comment is ever lost or
--     moved inside a line;
--   * whether a list is written one item per line, and whether a line
--     breaks around an operator, @=@ or @->@, follows the source. Every
--     break is kept at the token boundary the source had it at, so the
--     newline rules of spec 6.5 read the result the way they read the
--     source.
--
-- The result is checked before it is returned: it must lex to the same
-- tokens and comments, and parse to the same module, as the source.
module Language.Lask.Syntax.Printer
  ( FormatError (..),
    formatSource,
  )
where

import Control.Monad (forM, unless)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.List (intersperse, partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Lexer.Token
import Language.Lask.Span (Position (Position), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Parser (parseModule)
import Language.Lask.Syntax.TokenStream (renderToken)

data FormatError
  = -- | The source does not parse, and is left as it is.
    FormatSyntax [Diagnostic]
  | -- | What the formatter produced is not the program it was given:
    -- a bug in the formatter, reported instead of written.
    FormatInternal Text
  deriving (Show, Eq)

-- | Format a module. Formatting a formatted module changes nothing.
formatSource :: FilePath -> Text -> Either FormatError Text
formatSource file src = do
  m <- either (Left . FormatSyntax) Right (parseModule file src)
  (toks, comments) <- either (Left . FormatSyntax . pure) Right (lexTokensWithComments file src)
  let ctx = mkCtx src toks
      cs = sortOn cStart [Comment (startOf sp) (endOf sp) (slice (ctxLines ctx) (startOf sp) (endOf sp)) | sp <- dedupe comments]
      out = renderDoc (evalState (runReaderT (moduleDoc m) ctx) cs)
  verify file src m out
  pure out

-- | Comment spans in source order, each once: the lexer records a
-- comment where it skips it, which a backtracking alternative can do
-- twice.
dedupe :: [Span] -> [Span]
dedupe = map fst . Map.toAscList . Map.fromList . map (\s -> (s, ()))

-- Verification -----------------------------------------------------------------

-- | The formatted source must be the same program, token for token and
-- comment for comment. Newlines and @;@ are the only tokens it may add
-- or drop, and @>>@ \/ @>=@ may be spelt apart where they close a type.
verify :: FilePath -> Text -> Module -> Text -> Either FormatError ()
verify file src m out = do
  m' <- either (internal . ("the result does not parse: " <>) . firstMessage) Right (parseModule file out)
  unless (stripSpansModule m' == stripSpansModule m) (internal "the result parses to a different module")
  (t1, c1) <- lexed src
  (t2, c2) <- lexed out
  unless (tokens t1 == tokens t2) (internal "the result has different tokens")
  unless (commentTexts src c1 == commentTexts out c2) (internal "the result has different comments")
  where
    internal = Left . FormatInternal
    firstMessage ds = maybe "" diagMessage (listToMaybe ds)
    lexed s = either (internal . ("the result does not lex: " <>) . diagMessage) Right (lexTokensWithComments file s)
    tokens = concatMap apart . filter (`notElem` [TNewline, TSemi]) . stripTokens
    apart t = case t of
      TOp OpCompR -> [TOp OpGt, TOp OpGt]
      TOp OpGe -> [TOp OpGt, TAssign]
      _ -> [t]
    commentTexts s spans =
      let ls = sourceLines s in [slice ls (startOf sp) (endOf sp) | sp <- dedupe spans]

-- Documents ----------------------------------------------------------------------

-- | A layout with breaks fixed in advance: lines break where 'DLine'
-- says, and indentation grows by two spaces inside 'DNest'.
data Doc
  = DText Text
  | DLine
  | DNest Doc
  | DCat [Doc]
  | -- | What follows must start a new line: the end of a line comment
    -- or of a command expression, which run to the end of their line.
    DEol

instance Semigroup Doc where
  a <> b = DCat [a, b]

instance Monoid Doc where
  mempty = DCat []

instance IsString Doc where
  fromString = DText . T.pack

text :: Text -> Doc
text = DText

line :: Doc
line = DLine

nest :: Doc -> Doc
nest = DNest

sepBy :: Text -> [Doc] -> Doc
sepBy s = mconcat . intersperse (text s)

data R = R
  { rOut :: [Text],
    rPending :: Int,
    rEol :: Bool,
    rStarted :: Bool
  }

-- | Render a document. Runs of line breaks collapse to at most one
-- blank line, a line never ends in indentation, and the output ends
-- with a newline unless it is empty.
renderDoc :: Doc -> Text
renderDoc d =
  let body = T.concat (reverse (rOut (go 0 d (R [] 0 False False))))
   in if T.null body then "" else body <> "\n"
  where
    go :: Int -> Doc -> R -> R
    go ind doc r = case doc of
      DCat ds -> foldl (flip (go ind)) r ds
      DNest x -> go (ind + 2) x r
      DLine -> r {rPending = rPending r + 1}
      DEol -> r {rEol = True}
      DText t
        | T.null t -> r
        | otherwise ->
            let pending = if rEol r then max 1 (rPending r) else rPending r
                out
                  | pending > 0 && rStarted r =
                      T.stripStart t : T.replicate ind " " : T.replicate (min 2 pending) "\n" : rOut r
                  | otherwise = t : rOut r
             in R out 0 False True

-- Source and tokens ------------------------------------------------------------------

-- | A source position as megaparsec reports it: line, then column.
type Pos = (Int, Int)

data Comment = Comment {cStart :: Pos, cEnd :: Pos, cText :: Text}

data Ctx = Ctx
  { ctxLines :: V.Vector Text,
    -- | The top-level tokens by where they start, with where they end.
    ctxToks :: Map.Map Pos (Pos, Token)
  }

type P = ReaderT Ctx (State [Comment])

mkCtx :: Text -> [Spanned Token] -> Ctx
mkCtx src toks =
  Ctx
    (sourceLines src)
    (Map.fromList [(startOf sp, (endOf sp, t)) | Spanned sp t <- toks, t /= TNewline])

sourceLines :: Text -> V.Vector Text
sourceLines = V.fromList . T.splitOn "\n"

startOf, endOf :: Span -> Pos
startOf (Span (Position _ l c) _) = (l, c)
startOf NoSpan = (0, 0)
endOf (Span _ (Position _ l c)) = (l, c)
endOf NoSpan = (0, 0)

-- | The index in a line of the character at a column. Megaparsec
-- advances the column of a tab to the next multiple of eight, plus one.
colIndex :: Text -> Int -> Int
colIndex ln target = go 0 1 (T.unpack ln)
  where
    go i col _ | col >= target = i
    go i col (c : cs) = go (i + 1) (if c == '\t' then col + 8 - ((col - 1) `rem` 8) else col + 1) cs
    go i _ [] = i

slice :: V.Vector Text -> Pos -> Pos -> Text
slice ls (l1, c1) (l2, c2)
  | l1 == l2 = T.take (colIndex t1 c2 - colIndex t1 c1) (T.drop (colIndex t1 c1) t1)
  | otherwise =
      T.intercalate "\n" $
        [T.drop (colIndex t1 c1) t1] <> [lineAt l | l <- [l1 + 1 .. l2 - 1]] <> [T.take (colIndex t2 c2) t2]
  where
    lineAt l = fromMaybe "" (ls V.!? (l - 1))
    t1 = lineAt l1
    t2 = lineAt l2

srcText :: Pos -> Pos -> P Text
srcText a b = asks (\c -> slice (ctxLines c) a b)

-- | The token that starts at a position.
tokAt :: Pos -> P (Maybe Token)
tokAt p = asks (fmap snd . Map.lookup p . ctxToks)

-- | Where the first token of a kind at or after a position starts and
-- ends.
tokFrom :: Token -> Pos -> P (Pos, Pos)
tokFrom t p = do
  toks <- asks ctxToks
  pure $ case [(s, e) | (s, (e, t')) <- Map.toAscList (Map.dropWhileAntitone (< p) toks), t' == t] of
    (x : _) -> x
    [] -> (p, p)

-- | The token before a position, with where it starts.
tokBefore :: Pos -> P (Maybe (Pos, Pos, Token))
tokBefore p = asks (fmap (\(s, (e, t)) -> (s, e, t)) . Map.lookupLT p . ctxToks)

-- | Where the last token before a position starts.
lastTokStart :: Pos -> P Pos
lastTokStart p = maybe p (\(s, _, _) -> s) <$> tokBefore p

-- | Whether the source breaks the line just before the token at a
-- position.
brokenBefore :: Pos -> P Bool
brokenBefore p = maybe False (\(_, e, _) -> fst e < fst p) <$> tokBefore p

-- Comments --------------------------------------------------------------------------

inRange :: Pos -> Pos -> Comment -> Bool
inRange a b c = cStart c >= a && cEnd c <= b

claimWhere :: (Comment -> Bool) -> P [Comment]
claimWhere f = do
  cs <- get
  let (mine, rest) = partition f cs
  put rest
  pure mine

claim :: Pos -> Pos -> P [Comment]
claim a b = claimWhere (inRange a b)

anyIn :: Pos -> Pos -> P Bool
anyIn a b = any (inRange a b) <$> get

commentDoc :: Comment -> Doc
commentDoc c
  | "//" `T.isPrefixOf` cText c = text (cText c) <> DEol
  | otherwise = text (cText c)

-- | The source from one position to another, as written, with the
-- comments in it.
verbatim :: Pos -> Pos -> P Doc
verbatim a b = do
  _ <- claim a b
  t <- T.dropWhileEnd (`elem` [' ', '\t']) <$> srcText a b
  lastTok <- tokBefore b
  pure $ case lastTok of
    Just (_, _, TCommand {}) -> text t <> DEol
    _ -> text t

-- | Print an item, or copy it from the source when it holds a comment
-- the printing did not place.
withFallback :: Pos -> Pos -> P Doc -> P Doc
withFallback a b m = do
  before <- get
  d <- m
  stray <- anyIn a b
  if stray then put before >> verbatim a b else pure d

-- Sequences and lists ------------------------------------------------------------

data Item = Item {itemStart :: Pos, itemEnd :: Pos, itemDoc :: P Doc}

-- | Items one per line, between two positions: the declarations of a
-- module, the statements of a block, the arms of a @case@.
seqLines :: Pos -> Pos -> [Item] -> P Doc
seqLines a b items = (\(d, _, _) -> d) <$> entries True "" "" a b items

-- | Items between two positions, in order, each after the comments
-- above it and before the comments that end its line, and then the
-- comments after the last one. An item starts a line of its own when
-- @always@ says so, when a comment is above it, or when the source
-- starts it on a new line; otherwise it follows the item before it, or
-- @lead@ for the first. Every item but the last is followed by @sep@.
-- A blank line in the source stays one blank line.
--
-- Returns the document, whether it breaks a line, and where in the
-- source the last thing it placed ends.
entries :: Bool -> Text -> Text -> Pos -> Pos -> [Item] -> P (Doc, Bool, Pos)
entries always lead sep a b items = do
  -- Comments on the line that opens the sequence stay on it.
  opening <- claimWhere (\c -> fst (cStart c) == fst a && cStart c >= a && cEnd c <= b)
  let gaps = zipWith (\p c -> snd (cStart c) - snd p) (a : map cEnd opening) opening
      start = maybe a cEnd (lastMay opening)
  (d, broken, end) <- go (null opening) start items
  pure (mconcat [text (T.replicate (max 1 g) " ") <> commentDoc c | (g, c) <- zip gaps opening] <> d, broken, end)
  where
    go placed from [] = do
      cs <- claim from b
      let (d, end) = commentLines placed from cs
      pure (d, not (null cs), end)
    go placed from (it : rest) = do
      leading <- claim from (itemStart it)
      d <- withFallback (itemStart it) (itemEnd it) (itemDoc it)
      let next = maybe b itemStart (listToMaybe rest)
          endLine = fst (itemEnd it)
      trailing <-
        claimWhere (\c -> cStart c >= itemEnd it && fst (cStart c) == endLine && cEnd c <= next)
      let (dl, from') = commentLines placed from leading
          nothingYet = placed && null leading
          broken = always || not (null leading) || fst (itemStart it) > fst from'
          before
            | broken = blank nothingYet from' (itemStart it) <> line
            | nothingYet = text lead
            | otherwise = " "
          sep' = if null rest then "" else sep
          -- A comment that ends the line keeps the space the source
          -- puts before it, which keeps a column of them aligned.
          gaps = zipWith (\p c -> snd (cStart c) - snd p) (itemEnd it : map cEnd trailing) trailing
          gaps' = zipWith (-) gaps (T.length sep' : repeat 0)
          after = mconcat [text (T.replicate (max 1 g) " ") <> commentDoc c | (g, c) <- zip gaps' trailing]
          end = maybe (itemEnd it) cEnd (lastMay trailing)
      (more, brokenLater, lastEnd) <- go False end rest
      pure (dl <> before <> d <> text sep' <> after <> more, broken || brokenLater, lastEnd)

    -- Comments on lines of their own.
    commentLines placed from cs = foldl step (mempty, from) (zip (placed : repeat False) cs)
      where
        step (acc, p) (nothingYet, c) = (acc <> blank nothingYet p (cStart c) <> line <> commentDoc c, cEnd c)

    blank nothingYet from p = if not nothingYet && fst p - fst from >= 2 then line else mempty

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

-- | A bracketed list, given its opening token and the kind of its
-- closing one. The lines break where the source breaks them, between
-- items and before the closing bracket, and the items of a list that
-- breaks are indented.
list :: Text -> Text -> Bool -> (Pos, Pos) -> Token -> [Item] -> P Doc
list open close padded (_, openEnd) closeTok items = do
  (closeStart, _) <- tokFrom closeTok (maybe openEnd itemEnd (lastMay items))
  let pad = if padded && not (null items) then " " else ""
  (body, broken, end) <- entries False pad "," openEnd closeStart items
  let closeBroken = fst closeStart > fst end
      closing = (if closeBroken then line else text pad) <> text close
  pure $
    if broken || closeBroken
      then text open <> nest body <> closing
      else text open <> body <> closing

-- | @lhs sep rhs@, breaking after @sep@ where the source does. The
-- right-hand side is told whether it starts a line of its own.
joinAfter :: Doc -> Text -> Pos -> (Bool -> P Doc) -> P Doc
joinAfter lhs sep rhsStart rhs = do
  broken <- brokenBefore rhsStart
  d <- rhs broken
  pure $
    if broken
      then lhs <> text (" " <> sep) <> nest (line <> d)
      else lhs <> text (" " <> sep <> " ") <> d

-- | @lhs op rhs@, breaking before and after @op@ where the source does.
-- A line that begins with @op@ is indented, unless the expression
-- starts a line of its own (@flat@), whose indentation it then keeps:
--
-- > ask(prompt) =
-- >   with_server(prompt)
-- >   |> trim
-- >
-- > x = with_server(prompt)
-- >   |> trim
infixJoin :: Bool -> Doc -> Text -> Pos -> Expr -> P Doc
infixJoin flat lhs op opStart r = do
  before <- brokenBefore opStart
  after <- brokenBefore (startOf (exprSpan r))
  -- An operand on a line of its own starts that line. So does a lambda
  -- after a line-leading @op@: @|> \\(x) -> f(x)@ continues its body
  -- at the indentation of the @|>@ lines.
  dr <- exprOn r (after || (before && isLambda r))
  let hang = if flat then id else nest
  pure $ case (before, after) of
    (False, False) -> lhs <> text (" " <> op <> " ") <> dr
    (True, False) -> lhs <> hang (line <> text (op <> " ") <> dr)
    (False, True) -> lhs <> text (" " <> op) <> nest (line <> dr)
    (True, True) -> lhs <> hang (line <> text op <> nest (line <> dr))

-- Declarations ----------------------------------------------------------------------

moduleDoc :: Module -> P Doc
moduleDoc m = do
  items <- mapM declItem (moduleDecls m)
  end <- asks (\c -> (V.length (ctxLines c) + 1, 1))
  -- Nothing opens a module, so no comment is on its opening line.
  seqLines (0, 0) end items

-- | A declaration, with the @export@ or @internal@ marker before it,
-- which the parser keeps out of its span.
declItem :: Decl -> P Item
declItem (Decl sp f) = do
  marker <- tokBefore (startOf sp)
  let (start, prefix) = case marker of
        Just (s, _, TKw KExport) -> (s, "export ")
        Just (s, _, TKw KInternal) -> (s, "internal ")
        _ -> (startOf sp, "")
  pure (Item start (endOf sp) ((text prefix <>) <$> decl sp f))

decl :: Span -> DeclF -> P Doc
decl sp f = case f of
  DImportNamed specs _ -> do
    ds <- importSpecs specs
    p <- path
    pure ("import " <> ds <> " from " <> p)
  DImportNamespace alias _ -> do
    p <- path
    pure ("import * as " <> text alias <> " from " <> p)
  DExportFrom specs _ -> do
    ds <- importSpecs specs
    p <- path
    pure ("export " <> ds <> " from " <> p)
  DTypeAlias n tps t -> do
    dtp <- typeParams tps
    joinAfter ("type " <> text n <> dtp) "=" (startOf (stypeSpan t)) (const (ty t))
  DValue n sec t e -> do
    dt <- traverse ty t
    joinAfter (text n <> secrecy sec <> annot dt) "=" (startOf (exprSpan e)) (exprOn e)
  DFunction n tps ps rt e -> do
    dtp <- typeParams tps
    open <- tokFrom TLParen start
    dps <- params open ps
    drt <- traverse ty rt
    joinAfter (text n <> dtp <> dps <> annot drt) "=" (startOf (exprSpan e)) (exprOn e)
  DCommand ns e -> do
    -- @export command { ... } on e@ is the only declaration whose span
    -- starts at its @export@.
    exported <- tokAt start
    dn <- commandNames ns
    de <- expr e
    let prefix = if exported == Just (TKw KExport) then "export " else ""
    pure (prefix <> "command " <> dn <> " on " <> de)
  DImportCommands ns _ -> do
    dn <- commandNames ns
    p <- path
    pure ("import command " <> dn <> " from " <> p)
  DExportCommandsFrom ns _ -> do
    dn <- commandNames ns
    p <- path
    pure ("export command " <> dn <> " from " <> p)
  where
    start = startOf sp
    -- The path ends the declaration, spelt as the source spells it.
    path = do
      s <- lastTokStart (endOf sp)
      text <$> srcText s (endOf sp)
    importSpecs specs = do
      open <- tokFrom TLBrace start
      list "{" "}" True open TRBrace [Item (startOf isp) (endOf isp) (pure (spec n a)) | ImportSpec isp n a <- specs]
    spec n a = text n <> maybe mempty (\x -> text (" as " <> x)) a
    commandNames ns = do
      open <- tokFrom TLBrace start
      list "{" "}" True open TRBrace [Item (startOf s) (endOf s) (text <$> srcText (startOf s) (endOf s)) | Spanned s _ <- ns]

secrecy :: Secrecy -> Doc
secrecy Public = mempty
secrecy Secret = "!!"

annot :: Maybe Doc -> Doc
annot = maybe mempty (": " <>)

params :: (Pos, Pos) -> [Param] -> P Doc
params open ps = list "(" ")" False open TRParen (map item ps)
  where
    item (Param sp f) = Item (startOf sp) (endOf sp) $ case f of
      PPositional n sec t -> (\dt -> text n <> secrecy sec <> annot dt) <$> traverse ty t
      PVariadic n t -> (\dt -> "..." <> text n <> annot dt) <$> traverse ty t
      PKeyword n sec t d -> do
        dt <- traverse ty t
        joinAfter ("--" <> text n <> secrecy sec <> annot dt) "=" (startOf (exprSpan d)) (exprOn d)

typeParams :: [TypeParam] -> P Doc
typeParams [] = pure mempty
typeParams tps = do
  ds <- forM tps $ \(TypeParam (Spanned _ v) b) -> case b of
    Nothing -> pure (text v)
    Just (SBoundNamed (Spanned _ n)) -> pure (text (v <> ": " <> n))
    Just (SBoundType t) -> (\dt -> text v <> ": " <> dt) <$> ty t
  pure ("<" <> sepBy ", " ds <> ">")

-- Types ---------------------------------------------------------------------------------

-- | A type on one line, or as written when the source spreads it over
-- several or puts a comment in it.
ty :: SType -> P Doc
ty t = do
  let a = startOf (stypeSpan t)
      b = endOf (stypeSpan t)
  commented <- anyIn a b
  if fst a /= fst b || commented then verbatim a b else text <$> tyText t

tyText :: SType -> P Text
tyText (SType _ f) = case f of
  SAny -> pure "Any"
  SNumber -> pure "Number"
  SString -> pure "String"
  SBool -> pure "Bool"
  SNull -> pure "Null"
  SVoid -> pure "Void"
  SEnvironment -> pure "Environment"
  SRunnable -> pure "Runnable"
  SArray t -> generic "Array" [t]
  SMap t -> generic "Map" [t]
  SAsyncHandle t -> generic "AsyncHandle" [t]
  SFunction ps r -> generic "Function" (ps <> [r])
  SNamed q n as -> do
    xs <- mapM tyText as
    pure (maybe n (\ns -> ns <> "." <> n) q <> (if null xs then "" else "<" <> T.intercalate ", " xs <> ">"))
  SRecord fs -> do
    xs <- forM fs $ \(Spanned ksp _, opt, t) -> do
      -- A key is spelt as written: a quoted key stays quoted.
      k <- srcText (startOf ksp) (endOf ksp)
      tt <- tyText t
      pure (k <> (if opt then "?" else "") <> ": " <> tt)
    pure ("Record<" <> T.intercalate ", " xs <> ">")
  SUnion ts -> T.intercalate " | " <$> mapM tyText ts
  where
    generic n ts = (\xs -> n <> "<" <> T.intercalate ", " xs <> ">") <$> mapM tyText ts

-- Expressions --------------------------------------------------------------------------

isLambda :: Expr -> Bool
isLambda (Expr _ ELambda {}) = True
isLambda _ = False

-- | The operand an expression begins with, which shares its start.
leftChild :: ExprF -> Maybe Expr
leftChild f = case f of
  EBin _ a _ -> Just a
  ECond c _ _ -> Just c
  ECall g _ -> Just g
  EDot e _ -> Just e
  EIndex e _ -> Just e
  _ -> Nothing

-- | How many pairs of parentheses enclose an expression, and the span
-- inside them. The parser keeps parentheses only by widening the span,
-- so a span that starts at a @(@ its first operand does not start at
-- is a parenthesized expression.
parenthesized :: Expr -> P (Int, Pos, Pos)
parenthesized (Expr sp f) = go 0 (startOf sp) (endOf sp)
  where
    leftStart = startOf . exprSpan <$> leftChild f
    go :: Int -> Pos -> Pos -> P (Int, Pos, Pos)
    go n a b = do
      t <- asks (Map.lookup a . ctxToks)
      case t of
        Just (openEnd, TLParen) | Just a /= leftStart -> do
          inner <- asks (fmap fst . Map.lookupGE openEnd . ctxToks)
          close <- lastTokStart b
          innerEnd <- maybe close (\(_, e, _) -> e) <$> tokBefore close
          go (n + 1) (fromMaybe openEnd inner) innerEnd
        _ -> pure (n, a, b)

expr :: Expr -> P Doc
expr e = exprOn e False

-- | An expression, told whether it starts a line of its own, which
-- 'infixJoin' needs to know. Inside parentheses it does not.
exprOn :: Expr -> Bool -> P Doc
exprOn e@(Expr _ f) flat = do
  (n, a, b) <- parenthesized e
  d <- exprBody (flat && n == 0) a b f
  pure (text (T.replicate n "(") <> d <> text (T.replicate n ")"))

exprItem :: Expr -> Item
exprItem e = Item (startOf (exprSpan e)) (endOf (exprSpan e)) (expr e)

argItem :: Arg -> Item
argItem (Arg sp f) = Item (startOf sp) (endOf sp) $ case f of
  APos e -> expr e
  AKw k e -> joinAfter (text k) "=" (startOf (exprSpan e)) (exprOn e)

-- | An expression without its parentheses, which span from @a@ to @b@.
exprBody :: Bool -> Pos -> Pos -> ExprF -> P Doc
exprBody flat a b f = case f of
  ENull -> pure "null"
  EBool True -> pure "true"
  EBool False -> pure "false"
  ENumber _ -> verbatim a b
  EString _ -> verbatim a b
  ECommand {} -> verbatim a b
  EVar n -> pure (text n)
  EArray es -> do
    open <- tokFrom TLBracket a
    list "[" "]" False open TRBracket (map exprItem es)
  EObject kvs -> do
    open <- tokFrom TLBrace a
    list "{" "}" True open TRBrace [Item (startOf ksp) (endOf (exprSpan v)) (entry ksp v) | (Spanned ksp _, v) <- kvs]
  ELambda ps rt body -> do
    open <- tokFrom TLParen a
    dps <- params open ps
    drt <- traverse ty rt
    joinAfter ("\\" <> dps <> annot drt) "->" (startOf (exprSpan body)) (\broken -> exprOn body (broken || flat))
  ECall g as -> do
    dg <- expr g
    open <- tokFrom TLParen (endOf (exprSpan g))
    (dg <>) <$> list "(" ")" False open TRParen (map argItem as)
  EDot e (Spanned _ n) -> (<> text ("." <> n)) <$> expr e
  EIndex e i -> do
    de <- expr e
    di <- expr i
    pure (de <> "[" <> di <> "]")
  EBin o l r -> do
    dl <- exprOn l flat
    (opStart, _) <- tokFrom (TOp o) (endOf (exprSpan l))
    infixJoin flat dl (T.pack (renderToken (TOp o))) opStart r
  ENot x -> do
    dx <- expr x
    -- @!!@ would lex as the secret marker.
    c <- srcText (startOf (exprSpan x)) (second (+ 1) (startOf (exprSpan x)))
    pure ("!" <> (if c == "!" then " " else mempty) <> dx)
  EDo body -> ("do " <>) <$> block body
  EIf c t me -> do
    dc <- expr c
    dt <- block t
    de <- traverse block me
    pure ("if (" <> dc <> ") " <> dt <> maybe mempty (" else " <>) de)
  ECond c x y -> do
    dc <- exprOn c flat
    (q, _) <- tokFrom TQuestion (endOf (exprSpan c))
    -- Unlike an operator, a @?@ or @:@ that begins a line is always
    -- indented.
    dx <- infixJoin False dc "?" q x
    (colon, _) <- tokFrom TColon (endOf (exprSpan x))
    infixJoin False dx ":" colon y
  EFor (Spanned _ x) xs body -> do
    dxs <- expr xs
    dbody <- block body
    pure ("for (" <> text x <> ": " <> dxs <> ") " <> dbody)
  ECase scrut arms -> caseExpr a b scrut arms
  ETry body c fin -> do
    dbody <- block body
    dc <- forM c $ \(Spanned _ n, h) -> (\dh -> " catch (" <> text n <> ") " <> dh) <$> block h
    dfin <- traverse block fin
    pure ("try " <> dbody <> fromMaybe mempty dc <> maybe mempty (" finally " <>) dfin)
  EAsync x -> ("async " <>) <$> expr x
  EAwait x -> ("await " <>) <$> expr x
  EEnv h as os -> envExpr a h as os
  where
    second g (x, y) = (x, g y)
    entry ksp v = do
      -- A key is spelt as written: a quoted key stays quoted.
      k <- srcText (startOf ksp) (endOf ksp)
      dv <- expr v
      pure (text k <> ": " <> dv)

-- | @#head(args){options}@: the lists hug the head, as they must
-- (spec 6.7).
envExpr :: Pos -> Text -> Maybe [Arg] -> Maybe [Arg] -> P Doc
envExpr a h as os = do
  (_, headEnd) <- tokFrom (TEnvHead h) a
  (dargs, argsEnd) <- case as of
    Nothing -> pure (mempty, headEnd)
    Just xs -> do
      open@(_, openEnd) <- tokFrom TLParen headEnd
      d <- list "(" ")" False open TRParen (map argItem xs)
      (_, closeEnd) <- tokFrom TRParen (maybe openEnd (endOf . argSpan) (lastMay xs))
      pure (d, closeEnd)
  dopts <- case os of
    Nothing -> pure mempty
    Just xs -> do
      open <- tokFrom TLBrace argsEnd
      list "{" "}" False open TRBrace (map option xs)
  pure ("#" <> text h <> dargs <> dopts)
  where
    option (Arg sp f) = Item (startOf sp) (endOf sp) $ case f of
      AKw k v -> (\dv -> text k <> ": " <> dv) <$> expr v
      APos v -> expr v

-- | @case (s) { ... }@: arms one per line, or on the line of @case@,
-- separated by @;@, when the source writes them there. Arms the source
-- writes with their @->@ in one column keep them in one column.
caseExpr :: Pos -> Pos -> Maybe Expr -> [CaseArm] -> P Doc
caseExpr a b scrut arms = do
  ds <- traverse expr scrut
  (openStart, openEnd) <- tokFrom TLBrace (maybe a (endOf . exprSpan) scrut)
  closeStart <- lastTokStart b
  commented <- anyIn openEnd closeStart
  let header = "case " <> maybe mempty (\d -> "(" <> d <> ") ") ds
      items w = [Item (startOf sp) (endOf sp) (arm w x) | x@(CaseArm sp _ _) <- arms]
  if
    | null arms && not commented -> pure (header <> "{}")
    | fst openStart == fst closeStart && not commented -> do
        das <- mapM (\it -> withFallback (itemStart it) (itemEnd it) (itemDoc it)) (items 0)
        pure (header <> "{ " <> sepBy "; " das <> " }")
    | otherwise -> do
        -- Measured without claiming the comments in them, which the
        -- arms claim when they are printed.
        widths <- forM arms $ \(CaseArm _ hs _) -> do
          saved <- get
          w <- flatWidth <$> heads hs
          put saved
          pure w
        arrows <- mapM arrowColumn arms
        let aligned = case arrows of
              (c : more@(_ : _)) -> all (== c) more
              _ -> False
            width = if aligned then maximum (0 : catMaybes widths) else 0
        body <- seqLines openEnd closeStart (items width)
        pure (header <> "{" <> nest body <> line <> "}")
  where
    -- Where the source puts an arm's @->@. Arms whose arrows the source
    -- lines up stay lined up.
    arrowColumn (CaseArm sp hs _) = do
      headEnd <- case hs of
        Nothing -> snd <$> tokFrom (TKw KElse) (startOf sp)
        Just (ValueHeads es) -> pure (endOf (exprSpan (last es)))
        Just (TypeHeads ts) -> pure (endOf (stypeSpan (last ts)))
      snd . fst <$> tokFrom TArrow headEnd
    heads hs = case hs of
      Nothing -> pure "else"
      Just (ValueHeads es) -> sepBy ", " <$> mapM expr es
      Just (TypeHeads ts) -> sepBy ", " <$> mapM ty ts
    -- The heads, padded to @width@ when they fit on one line.
    arm width (CaseArm _ hs body) = do
      dh <- heads hs
      let pad = maybe mempty (\w -> text (T.replicate (width - w) " ")) (flatWidth dh)
      joinAfter (dh <> pad) "->" (startOf (exprSpan body)) (exprOn body)

-- | How wide a document is, if it has no line break.
flatWidth :: Doc -> Maybe Int
flatWidth d = case d of
  DText t
    | T.any (== '\n') t -> Nothing
    | otherwise -> Just (T.length t)
  DCat ds -> sum <$> mapM flatWidth ds
  DNest x -> flatWidth x
  DLine -> Nothing
  DEol -> Nothing

-- | A block: statements one per line, or @{ s }@ when the source writes
-- its only statement on the line of its braces.
block :: Block -> P Doc
block (Block sp stmts) = do
  let (l, c) = startOf sp
      open = (l, c + 1)
  close <- lastTokStart (endOf sp)
  commented <- anyIn open close
  case stmts of
    [] | not commented -> pure "{}"
    [s] | not commented && fst open == fst close -> do
      d <- stmt s
      pure ("{ " <> d <> " }")
    _ -> do
      body <- seqLines open close [Item (startOf (stmtSpan s)) (endOf (stmtSpan s)) (stmt s) | s <- stmts]
      pure ("{" <> nest body <> line <> "}")

stmt :: Stmt -> P Doc
stmt (Stmt _ f) = case f of
  SBind n sec t e -> do
    dt <- traverse ty t
    joinAfter (text n <> secrecy sec <> annot dt) "=" (startOf (exprSpan e)) (exprOn e)
  SExpr e -> exprOn e True
