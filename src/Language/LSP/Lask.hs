{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Language server: diagnostics from the full front-end pipeline and
-- semantic tokens straight from the lexer token stream (including
-- comments, interpolation contents and command heads).
module Language.LSP.Lask
  ( serve,
    lexSemanticTokens,
    uriPath,
    hoverAt,
    hoverMarkdown,
    completionAt,
  )
where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding (Iso)
import Control.Monad (forever, join)
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Message ()
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Control.Exception (IOException)
import qualified Control.Exception as E
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), Partial (..), checkText, compileText, compileTextPartial)
import Language.Lask.Builtins.Sig (builtinSchemes, schemeType)
import qualified Language.Lask.Diagnostic as D
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), HoverInfo (..))
import Language.Lask.ErrorCode (codeText)
import Language.Lask.Lexer (lexTokens, lexTokensWithComments)
import qualified Language.Lask.Lexer.Token as Tok
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import Language.Lask.Module.Resolve (GlobalScope (..), Publics (..), ValueTarget (..), modulePublics)
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Syntax.Scope (enclosingCall, localsAt)
import Language.Lask.Types (Type (..), renderType)
import System.FilePath (normalise)
import qualified Language.Lask.Span as S
import Language.Lask.Utils (Pretty (pretty))

data Config = Config
  deriving (Generic, Show)

instance J.ToJSON Config where
  toJSON _ = J.object []

instance J.FromJSON Config where
  parseJSON _ = pure Config

newtype ReactorInput = ReactorAction (IO ())

serve :: IO Int
serve = do
  let stderrLogger :: LogAction IO (WithSeverity T.Text)
      stderrLogger = L.cmap show L.logStringStderr
      clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
      clientLogger = defaultClientLogger
      dualLogger :: LogAction (LspM Config) (WithSeverity T.Text)
      dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

  rin <- atomically newTChan :: IO (TChan ReactorInput)
  runServer $
    ServerDefinition
      { defaultConfig = Config,
        parseConfig = \_old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right cfg,
        onConfigChange = const $ pure (),
        configSection = "lask",
        doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env),
        staticHandlers = \_caps -> lspHandlers dualLogger rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = lspOptions
      }

reactor :: L.LogAction IO (WithSeverity T.Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

lspHandlers :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> TChan ReactorInput -> Handlers m
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
  where
    goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). Handler (LspM Config) a -> Handler (LspM Config) a
    goReq f msg k = do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)
    goNot :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
    goNot f msg = do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      optCompletionTriggerCharacters = Just ['.']
    }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True,
      LSP._change = Just LSP.TextDocumentSyncKind_Incremental,
      LSP._willSave = Just False,
      LSP._willSaveWaitUntil = Just False,
      LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

handle :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        logger <& "Processing the Initialized notification" `WithSeverity` Info,
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ sendDocumentDiagnostics logger,
      notificationHandler LSP.SMethod_TextDocumentDidChange $ sendDocumentDiagnostics logger,
      notificationHandler LSP.SMethod_TextDocumentDidSave $ sendDocumentDiagnostics logger,
      requestHandler LSP.SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
            doc = LSP.toNormalizedUri uri
            path = uriPath uri
        mdoc <- getVirtualFile doc
        case mdoc of
          Just file -> case lexSemanticTokens path (virtualFileText file) of
            Right ts -> case makeSemanticTokens defaultSemanticTokensLegend ts of
              Right ts' -> responder $ Right $ LSP.InL ts'
              Left t -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) t Nothing
            Left _ -> case makeSemanticTokens defaultSemanticTokensLegend [] of
              Right ts -> responder $ Right $ LSP.InL ts
              Left t -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) t Nothing
          Nothing -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) "cannot get virtual file" Nothing,
      requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
        let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
            doc = LSP.toNormalizedUri uri
            path = uriPath uri
            pos = req ^. LSP.params . LSP.position
        mdoc <- getVirtualFile doc
        case mdoc of
          Nothing -> responder $ Right $ LSP.InR LSP.Null
          Just file -> do
            h <- liftIO $ hoverAt path (virtualFileText file) pos
            responder $ Right $ maybe (LSP.InR LSP.Null) LSP.InL h,
      requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
        let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
            doc = LSP.toNormalizedUri uri
            path = uriPath uri
            pos = req ^. LSP.params . LSP.position
        mdoc <- getVirtualFile doc
        case mdoc of
          Nothing -> responder $ Right $ LSP.InL []
          Just file -> do
            items <- liftIO $ completionAt path (virtualFileText file) pos
            responder $ Right $ LSP.InL items,
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure (),
      notificationHandler LSP.SMethod_TextDocumentDidClose $ \_ -> pure ()
    ]

sendDocumentDiagnostics ::
  (m ~ LspM Config, LSP.HasUri a1 Uri, LSP.HasTextDocument a2 a1, LSP.HasParams s a2) =>
  L.LogAction m (WithSeverity T.Text) ->
  s ->
  LspT Config IO ()
sendDocumentDiagnostics logger msg = do
  let uri = msg ^. LSP.params . LSP.textDocument . LSP.uri
      doc = LSP.toNormalizedUri uri
      path = uriPath uri
  logger <& ("Publishing diagnostics for: " <> T.pack (show doc)) `WithSeverity` Info
  mdoc <- getVirtualFile doc
  case mdoc of
    Just file -> do
      ds <- liftIO $ checkText path (virtualFileText file)
      sendDiagnostics doc (Just $ virtualFileVersion file) ds
    Nothing -> sendDiagnostics doc Nothing []

-- | The filesystem path of a document URI. Imports and the
-- environment definition file resolve relative to this path, so the
-- @file://@ scheme must be stripped; the raw URI text is only a
-- fallback for non-file schemes.
uriPath :: Uri -> FilePath
uriPath uri = maybe (T.unpack (getUri uri)) id (uriToFilePath uri)

sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> [D.Diagnostic] -> LspM Config ()
sendDiagnostics fileUri version ds = do
  let diags =
        map
          ( \d ->
              LSP.Diagnostic
                (toRange (D.diagSpan d))
                (Just LSP.DiagnosticSeverity_Error)
                (Just (LSP.InR (codeText (D.diagCode d))))
                Nothing
                (Just "lask")
                (T.pack $ pretty d)
                Nothing
                (Just [])
                Nothing
          )
          ds
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- | Semantic token atoms from the lexer: comments (collected on the
-- side), interpolation contents (nested token streams inside string
-- and command tokens, recursively), and command heads (@$@\/@$1@\/
-- @$2@\/@$*@) as function tokens.
lexSemanticTokens :: String -> Text -> Either Text [SemanticTokenAbsolute]
lexSemanticTokens fileName src =
  case lexTokensWithComments fileName src of
    Left e -> Left $ T.pack $ pretty e
    Right (ts, comments) ->
      let atoms =
            concatMap flattenToken ts
              <> [(c, SemanticTokenTypes_Comment) | c <- comments]
          sorted = sortOn (spanStart . fst) atoms
       in Right (join (map toAbsolutes sorted))
  where
    spanStart (S.Span s _) = Just s
    spanStart S.NoSpan = Nothing

    -- The protocol has no multi-line tokens; split them per line.
    toAbsolutes :: (S.Span, SemanticTokenTypes) -> [SemanticTokenAbsolute]
    toAbsolutes (sp, typ) =
      [ SemanticTokenAbsolute
          (fromIntegral $ l - 1)
          (fromIntegral $ c1 - 1)
          (fromIntegral $ c2 - c1)
          typ
          []
      | (l, c1, c2) <- splitSpanLines sp,
        c2 > c1
      ]

    splitSpanLines (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2))
      | l1 == l2 = [(l1, c1, c2)]
      | otherwise =
          [ (l, s, e)
          | l <- [l1 .. l2],
            let s = if l == l1 then c1 else 1,
            let e = if l == l2 then c2 else lineLength l + 1
          ]
    splitSpanLines S.NoSpan = []

    lineLength l = case drop (l - 1) srcLines of
      (x : _) -> T.length x
      [] -> 0

    srcLines = T.splitOn "\n" src

    charAt l c = case drop (l - 1) srcLines of
      (x : _) | c >= 1 && c <= T.length x -> Just (T.index x (c - 1))
      _ -> Nothing

    flattenToken :: Tok.Spanned Tok.Token -> [(S.Span, SemanticTokenTypes)]
    flattenToken (Tok.Spanned sp t) = case t of
      Tok.TString strParts ->
        carve sp SemanticTokenTypes_String (partAtoms strParts)
      Tok.TCommand _ env cmdParts ->
        let headLen = case sp of
              S.Span (S.Position _ l c) _
                | maybe False (`elem` ("12*" :: String)) (charAt l (c + 1)) -> 2
              _ -> 1
            (headSpan, restSpan) = splitSpanAt sp headLen
            nested = maybe [] (concatMap flattenToken) env <> partAtoms cmdParts
         in (headSpan, SemanticTokenTypes_Function) : carve restSpan SemanticTokenTypes_String nested
      _ -> case toSemanticTokenTypes t of
        Just typ -> [(sp, typ)]
        Nothing -> []

    partAtoms = concatMap partAtom
    partAtom (Tok.Chunk _) = []
    partAtom (Tok.Interp toks) = concatMap flattenToken toks

    splitSpanAt (S.Span s@(S.Position f l c) e) n =
      let mid = S.Position f l (c + n)
       in (S.Span s mid, S.Span mid e)
    splitSpanAt S.NoSpan _ = (S.NoSpan, S.NoSpan)

    -- Fill the outer span with background segments of the given type
    -- around the (non-overlapping) nested atoms.
    carve :: S.Span -> SemanticTokenTypes -> [(S.Span, SemanticTokenTypes)] -> [(S.Span, SemanticTokenTypes)]
    carve S.NoSpan _ atoms = atoms
    carve (S.Span outerS outerE) typ atoms =
      let positioned = sortOn fst [(s, a) | a@(S.Span s _, _) <- atoms]
          go cur [] = [(S.Span cur outerE, typ) | cur < outerE]
          go cur ((as, (S.Span _ ae, _)) : rest) =
            [(S.Span cur as, typ) | cur < as] <> go (max cur ae) rest
          go cur ((_, (S.NoSpan, _)) : rest) = go cur rest
       in go outerS positioned <> map snd positioned

    toSemanticTokenTypes :: Tok.Token -> Maybe SemanticTokenTypes
    toSemanticTokenTypes t = case t of
      Tok.TUpperId _ -> Just SemanticTokenTypes_Type
      Tok.TOp _ -> Just SemanticTokenTypes_Operator
      Tok.TLowerId _ -> Just SemanticTokenTypes_Variable
      Tok.TKw _ -> Just SemanticTokenTypes_Keyword
      Tok.TNull -> Just SemanticTokenTypes_Macro
      Tok.TBool _ -> Just SemanticTokenTypes_Macro
      Tok.TNumber _ -> Just SemanticTokenTypes_Number
      Tok.TRawString _ -> Just SemanticTokenTypes_String
      Tok.TEnvHead _ -> Just SemanticTokenTypes_Macro
      _ -> Nothing

toRange :: S.Span -> Range
toRange (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2)) =
  mkRange
    (fromIntegral l1 - 1)
    (fromIntegral c1 - 1)
    (fromIntegral l2 - 1)
    (fromIntegral c2 - 1)
toRange S.NoSpan = Range (Position 0 0) (Position 0 0)

-- Hover -----------------------------------------------------------------------

-- | Hover information for a document position: the type of the name
-- under the cursor (name references recorded during elaboration, or
-- the declaration name itself), plus the comment block directly above
-- the referenced declaration as documentation.
hoverAt :: FilePath -> Text -> Position -> IO (Maybe Hover)
hoverAt path src (Position pl pc) = do
  r <- compileText path src
  case r of
    Left _ -> pure Nothing
    Right compiled -> do
      let line = fromIntegral pl + 1
          col = fromIntegral pc + 1
          core = compiledCore compiled
          hits =
            [ hi
            | hi <- cpHover core,
              spanContains (hiSpan hi) line col
            ]
      case hits of
        [] -> declNameHover compiled path src line col
        _ -> do
          let hi = minimumBy (comparing (spanSize . hiSpan)) hits
          docs <- declDocs compiled path src (hiDecl hi)
          pure (Just (mkHover (hiName hi) (renderType (hiType hi)) docs (hiSpan hi)))
  where
    spanContains (S.Span (S.Position f l1 c1) (S.Position _ l2 c2)) line col =
      normalise f == normalise path
        && (line, col) >= (l1, c1)
        && (line, col) < (l2, c2)
    spanContains S.NoSpan _ _ = False

    spanSize (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2)) = (l2 - l1, c2 - c1)
    spanSize S.NoSpan = (maxBound, maxBound)

    -- Hovering the declaration name itself (the definition site is
    -- not a reference, so it is not in the hover records).
    declNameHover compiled docPath docSrc line col =
      case lexTokens docPath docSrc of
        Left _ -> pure Nothing
        Right ts -> do
          let core = compiledCore compiled
              entry = cpEntry core
              named =
                [ (sp, n)
                | Tok.Spanned sp (Tok.TLowerId n) <- ts,
                  spanContains sp line col
                ]
          case named of
            ((sp, n) : _) -> case Map.lookup (entry, n) (cpDecls core) of
              Just cd -> do
                docs <- declDocs compiled docPath docSrc (Just (entry, n))
                pure (Just (mkHover n (renderType (cdType cd)) docs sp))
              Nothing -> pure Nothing
            [] -> pure Nothing

mkHover :: Text -> Text -> Maybe Text -> S.Span -> Hover
mkHover name typeText docs sp =
  Hover
    (LSP.InL (LSP.MarkupContent LSP.MarkupKind_Markdown (hoverMarkdown name typeText docs)))
    (Just (toRange sp))

-- | The markdown body of a hover: a code block with @name: Type@,
-- followed by the declaration's documentation comment when present.
hoverMarkdown :: Text -> Text -> Maybe Text -> Text
hoverMarkdown name typeText docs =
  "```lask\n"
    <> name
    <> ": "
    <> typeText
    <> "\n```"
    <> maybe "" ("\n\n---\n\n" <>) docs

-- | The documentation of a declaration: the contiguous block of
-- comments directly above it, with comment markers stripped.
declDocs :: Compiled -> FilePath -> Text -> Maybe (FilePath, Text) -> IO (Maybe Text)
declDocs _ _ _ Nothing = pure Nothing
declDocs compiled docPath docSrc (Just (declPath, name)) = do
  mSrc <-
    if normalise declPath == normalise docPath
      then pure (Just docSrc)
      else do
        r <- E.try (TIO.readFile declPath)
        pure (either (\e -> let _ = (e :: IOException) in Nothing) Just r)
  pure $ do
    srcText <- mSrc
    declLine <- declStartLine
    comments <- case lexTokensWithComments declPath srcText of
      Right (_, cs) -> Just cs
      Left _ -> Nothing
    let block = commentBlockEndingAt comments (declLine - 1)
        texts = map (commentText srcText) block
    if null block then Nothing else Just (T.intercalate "\n" texts)
  where
    declStartLine = do
      lm <- Map.lookup (normalisedKey declPath) modules
      let matches d = case AST.declF d of
            AST.DValue n _ _ _ -> n == name
            AST.DFunction n _ _ _ -> n == name
            _ -> False
      case [AST.declSpan d | d <- AST.moduleDecls (lmModule lm), matches d] of
        (S.Span (S.Position _ l _) _ : _) -> Just l
        _ -> Nothing

    modules = progModules (compiledProgram compiled)
    normalisedKey p =
      case [k | k <- Map.keys modules, normalise k == normalise p] of
        (k : _) -> k
        [] -> p

    -- The contiguous run of comments whose last line is `endLine`,
    -- chaining upwards line by line.
    commentBlockEndingAt comments endLine =
      case [c | c@(S.Span _ (S.Position _ el _)) <- comments, el == endLine] of
        (c@(S.Span (S.Position _ sl _) _) : _) ->
          commentBlockEndingAt comments (sl - 1) <> [c]
        _ -> []

    commentText srcText (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2)) =
      let srcLines = T.splitOn "\n" srcText
          lineAt l = case drop (l - 1) srcLines of
            (x : _) -> x
            [] -> ""
          raw
            | l1 == l2 = T.take (c2 - c1) (T.drop (c1 - 1) (lineAt l1))
            | otherwise =
                T.intercalate "\n" $
                  [T.drop (c1 - 1) (lineAt l1)]
                    <> [lineAt l | l <- [l1 + 1 .. l2 - 1]]
                    <> [T.take (c2 - 1) (lineAt l2)]
       in stripMarkers raw
    commentText _ S.NoSpan = ""

    stripMarkers t =
      let noLine = maybe t T.stripStart (T.stripPrefix "//" t)
          noBlock = case T.stripPrefix "/*" noLine of
            Just rest -> T.strip (maybe rest id (T.stripSuffix "*/" rest))
            Nothing -> noLine
       in T.stripEnd noBlock

-- Completion ------------------------------------------------------------------

-- | A candidate before it is turned into a protocol item. 'candRank'
-- follows the name resolution ranks (spec 7.2) and drives the sort
-- order; keyword parameters of the enclosing call come first.
data Cand = Cand
  { candLabel :: Text,
    candKind :: CompletionItemKind,
    candDetail :: Maybe Text,
    candInsert :: Maybe Text,
    candRank :: Int
  }

-- | Completion candidates for a document position: names in scope
-- (locals, module top level, imports, builtins), reserved words, the
-- public members of a namespace or the fields of a record after a
-- dot, and the keyword parameters of the call being written.
completionAt :: FilePath -> Text -> Position -> IO [CompletionItem]
completionAt path src (Position pl pc)
  | afterDot = do
      p <- compileTextPartial path healedDot
      pure (items (dotCands p))
  | otherwise = do
      -- The word being typed is replaced by a placeholder identifier;
      -- if that still does not parse (a half-written declaration, say)
      -- the whole line is replaced by a trivial binding so that the
      -- rest of the buffer is still analysed.
      first <- compileTextPartial path healedWord
      p <-
        if hasSyntax first
          then pure first
          else compileTextPartial path healedLine
      let scope = scopeFor p
      pure . items $
        localCands p
          <> globalCands p scope
          <> tokenCands
          <> keywordCands
          <> kwParamCands p scope
  where
    -- Narrowing is left to the client: it matches case-insensitively
    -- and fuzzily, so filtering here only ever removes candidates the
    -- user expects. It also keeps the result independent of how much
    -- of the word has been typed, which is what lets the client reuse
    -- one answer for the whole word.
    items cs =
      [ toItem c
      | c <- dedup cs,
        -- The healed buffers introduce `_` as a placeholder name.
        candLabel c /= "_"
      ]
    hasSyntax p = isJust (partialProgram p) || isJust (partialModule p)

    srcLines = T.splitOn "\n" src
    lineIdx = fromIntegral pl :: Int
    col = fromIntegral pc :: Int
    lineText = case drop lineIdx srcLines of
      (x : _) -> x
      [] -> ""
    before = T.take col lineText
    prefix = T.takeWhileEnd isIdentChar before
    startCol = col - T.length prefix

    -- Everything left of the cursor with the partial name removed; a
    -- trailing dot means a member of whatever precedes it is wanted.
    beforeName = T.dropWhileEnd isIdentChar before
    afterDot = "." `T.isSuffixOf` beforeName
    dotCol = T.length beforeName - 1
    receiver = T.dropEnd 1 beforeName
    qualifier =
      let q = T.takeWhileEnd isIdentChar receiver
       in if T.null q then Nothing else Just q
    -- A chained receiver (`a.b.`, `f().`, `xs[0].`) is not something
    -- the single name lookups below can resolve.
    chained =
      case T.unsnoc (T.dropWhileEnd isIdentChar receiver) of
        Just (_, c) -> c `elem` ['.', ')', ']']
        Nothing -> False

    replaceLine f = T.intercalate "\n" (zipWith apply [0 ..] srcLines)
      where
        apply i l
          | i == (lineIdx :: Int) = f l
          | otherwise = l

    healedWord = replaceLine (\l -> T.take startCol l <> "_" <> T.drop col l)
    healedLine = replaceLine (const (T.replicate startCol " " <> "_ = null"))
    -- The dot and the partial member name are dropped entirely, so the
    -- receiver keeps its own type instead of becoming a bad selection.
    healedDot = replaceLine (\l -> T.take dotCol l <> T.drop col l)

    cursor = S.Position path (lineIdx + 1) (startCol + 1)
    -- The last character of the receiver, for the hover lookup.
    receiverEnd = S.Position path (lineIdx + 1) dotCol

    isIdentChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

    scopeFor p =
      listToMaybe
        [gs | (k, gs) <- Map.toList (partialScopes p), normalise k == normalise path]

    entryPath p = maybe path progEntry (partialProgram p)

    localCands p =
      [ Cand n CompletionItemKind_Variable Nothing Nothing 1
      | m <- maybeToList (partialModule p),
        n <- localsAt path m cursor
      ]

    globalCands p (Just gs) =
      [valueCand p n t | (n, t) <- Map.toList (gsValues gs)]
        <> [ Cand n CompletionItemKind_Module Nothing Nothing 4
           | n <- Map.keys (gsNamespaces gs)
           ]
    globalCands p Nothing =
      [ Cand n CompletionItemKind_Function Nothing Nothing 2
      | m <- maybeToList (partialModule p),
        n <- topLevelNames m
      ]
        <> [builtinCand n n | n <- Map.keys builtinSchemes]

    -- A floor under everything above: names the lexer can see even
    -- when neither the loader nor the parser gets through the buffer.
    -- Ranked with the module's own top level, and only reached by
    -- 'dedup' when the analysed candidates do not already have it.
    tokenCands =
      [ Cand n CompletionItemKind_Function Nothing Nothing 2
      | n <- tokenNames path src
      ]

    valueCand p n (VTopLevel modPath declName) =
      Cand n (kindFor ty) (renderType <$> ty) Nothing rank
      where
        ty = declType p modPath declName
        rank = if normalise modPath == normalise (entryPath p) then 2 else 3
    valueCand _ n (VBuiltin b) = builtinCand n b

    builtinCand label n = case Map.lookup n builtinSchemes of
      Just sch ->
        let ty = schemeType sch
         in Cand label (kindFor (Just ty)) (Just (renderType ty)) Nothing 5
      Nothing -> Cand label CompletionItemKind_Variable Nothing Nothing 5

    keywordCands =
      [ Cand w CompletionItemKind_Keyword Nothing Nothing 6
      | w <-
          map Tok.keywordText [minBound .. maxBound :: Tok.Keyword]
            <> ["true", "false", "null"]
      ]

    -- After a dot: the public members of a namespace, else the fields
    -- of the receiver's record type. Nothing at all beats guessing.
    dotCands p
      | chained = []
      | otherwise = case qualifier of
          Nothing -> []
          Just q -> case namespaceCands p (scopeFor p) q of
            [] -> recordCands p
            cs -> cs

    namespaceCands p scope q =
      [ c
      | gs <- maybeToList scope,
        key <- maybeToList (Map.lookup q (gsNamespaces gs)),
        prog <- maybeToList (partialProgram p),
        lm <- maybeToList (Map.lookup key (progModules prog)),
        let pub = modulePublics lm,
        c <-
          [ let ty = declType p key n
             in Cand n (kindFor ty) (renderType <$> ty) Nothing 4
          | n <- Set.toList (pubValues pub)
          ]
            <> [ Cand n CompletionItemKind_Interface Nothing Nothing 4
               | n <- Set.toList (pubTypes pub)
               ]
      ]

    recordCands p =
      [ Cand n CompletionItemKind_Field (Just (renderType t)) Nothing 4
      | TyRecord fields <- maybeToList (receiverType p),
        (n, t) <- Map.toList fields
      ]

    -- The type elaboration recorded for the innermost expression
    -- ending at the dot.
    receiverType p = do
      core <- partialCore p
      let hits = [hi | hi <- cpHover core, covers (hiSpan hi)]
      hi <- listToMaybe (sortOn (spanSize . hiSpan) hits)
      pure (hiType hi)

    covers (S.Span (S.Position f l1 c1) (S.Position _ l2 c2)) =
      normalise f == normalise path
        && (l1, c1) <= (S.line receiverEnd, S.column receiverEnd)
        && (S.line receiverEnd, S.column receiverEnd) < (l2, c2)
    covers S.NoSpan = False

    spanSize (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2)) = (l2 - l1, c2 - c1)
    spanSize S.NoSpan = (maxBound, maxBound)

    -- Keyword parameters are written `name = value` at the call site.
    kwParamCands p scope =
      [ Cand n CompletionItemKind_Property Nothing (Just (n <> " = ")) 0
      | m <- maybeToList (partialModule p),
        ns <- maybeToList (enclosingCall path m cursor),
        n <- calleeKeywords p scope ns
      ]

    calleeKeywords p scope ns = case ns of
      [n] -> case scope >>= Map.lookup n . gsValues of
        Just (VTopLevel modPath declName) -> keywordsOf p modPath declName
        Just (VBuiltin _) -> []
        Nothing -> keywordsOf p (entryPath p) n
      [q, n] -> case scope >>= Map.lookup q . gsNamespaces of
        Just key -> keywordsOf p key n
        Nothing -> []
      _ -> []

    keywordsOf p modPath declName =
      [ n
      | m <- maybeToList (moduleAt p modPath),
        ps <- maybeToList (declParams m declName),
        AST.Param _ (AST.PKeyword n _ _ _) <- ps
      ]

    moduleAt p modPath
      | normalise modPath == normalise path = partialModule p
      | otherwise = do
          prog <- partialProgram p
          lm <-
            listToMaybe
              [ l
              | (k, l) <- Map.toList (progModules prog),
                normalise k == normalise modPath
              ]
          pure (lmModule lm)

    declParams m declName =
      listToMaybe $
        [ps | AST.Decl _ (AST.DFunction n ps _ _) <- AST.moduleDecls m, n == declName]
          <> [ ps
             | AST.Decl _ (AST.DValue n _ _ (AST.Expr _ (AST.ELambda ps _ _))) <- AST.moduleDecls m,
               n == declName
             ]

    topLevelNames m =
      [ n
      | AST.Decl _ f <- AST.moduleDecls m,
        n <- case f of
          AST.DValue n' _ _ _ -> [n']
          AST.DFunction n' _ _ _ -> [n']
          _ -> []
      ]

    declType p modPath declName = do
      core <- partialCore p
      cd <- Map.lookup (modPath, declName) (cpDecls core)
      pure (cdType cd)

    kindFor (Just (TyFun _ _)) = CompletionItemKind_Function
    kindFor _ = CompletionItemKind_Variable

    -- The same name can be reached at several ranks; keep the nearest.
    dedup cs =
      Map.elems $
        Map.fromListWith
          (\new old -> if candRank new < candRank old then new else old)
          [(candLabel c, c) | c <- cs]

    toItem c =
      CompletionItem
        (candLabel c)
        Nothing
        (Just (candKind c))
        Nothing
        (candDetail c)
        Nothing
        Nothing
        Nothing
        (Just (T.pack (show (candRank c)) <> "-" <> candLabel c))
        Nothing
        (candInsert c)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

-- | Top level and imported names read straight off the token stream.
--
-- The lexer accepts a great deal that the parser rejects, so this
-- keeps the names a user has already written available while the
-- buffer as a whole is still half-written: a declaration is any
-- identifier in column one that is followed by @(@, @=@ or @:@.
tokenNames :: FilePath -> Text -> [Text]
tokenNames path src = case lexTokens path src of
  Left _ -> []
  Right ts -> declNames ts <> importNames ts
  where
    declNames ts =
      [ n
      | (Tok.Spanned sp t, next) <- zip ts (drop 1 ts),
        inColumnOne sp,
        n <- identText t,
        opensDecl (Tok.spannedValue next)
      ]

    opensDecl Tok.TLParen = True
    opensDecl Tok.TAssign = True
    opensDecl Tok.TColon = True
    opensDecl _ = False

    inColumnOne (S.Span (S.Position _ _ c) _) = c == 1
    inColumnOne S.NoSpan = False

    identText (Tok.TLowerId n) = [n]
    identText (Tok.TUpperId n) = [n]
    identText _ = []

    importNames (Tok.Spanned _ (Tok.TKw Tok.KImport) : rest) =
      boundByImport rest <> importNames rest
    importNames (_ : rest) = importNames rest
    importNames [] = []

    -- `import * as m from "..."`
    boundByImport
      ( Tok.Spanned _ (Tok.TOp Tok.OpMul)
          : Tok.Spanned _ (Tok.TKw Tok.KAs)
          : Tok.Spanned _ alias
          : _
        ) = identText alias
    -- `import { a, b as c } from "..."`
    boundByImport (Tok.Spanned _ Tok.TLBrace : rest) = specNames rest
    boundByImport _ = []

    specNames
      ( Tok.Spanned _ _
          : Tok.Spanned _ (Tok.TKw Tok.KAs)
          : Tok.Spanned _ alias
          : rest
        ) = identText alias <> nextSpec rest
    specNames (Tok.Spanned _ t : rest) = identText t <> nextSpec rest
    specNames [] = []

    nextSpec (Tok.Spanned _ Tok.TComma : rest) = specNames rest
    nextSpec _ = []
