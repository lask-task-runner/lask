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
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), checkText, compileText)
import qualified Language.Lask.Diagnostic as D
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), HoverInfo (..))
import Language.Lask.ErrorCode (codeText)
import Language.Lask.Lexer (lexTokens, lexTokensWithComments)
import qualified Language.Lask.Lexer.Token as Tok
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (renderType)
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
    { optTextDocumentSync = Just syncOptions
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
