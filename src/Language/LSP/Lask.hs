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
-- semantic tokens straight from the lexer token stream.
module Language.LSP.Lask (serve) where

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
import Language.Lask (checkText)
import qualified Language.Lask.Diagnostic as D
import Language.Lask.ErrorCode (codeText)
import Language.Lask.Lexer (lexTokens)
import qualified Language.Lask.Lexer.Token as Tok
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
        let doc =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        mdoc <- getVirtualFile doc
        let (NormalizedUri _ path) = doc
        case mdoc of
          Just file -> case lexSemanticTokens (T.unpack path) (virtualFileText file) of
            Right ts -> case makeSemanticTokens defaultSemanticTokensLegend ts of
              Right ts' -> responder $ Right $ LSP.InL ts'
              Left t -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) t Nothing
            Left _ -> case makeSemanticTokens defaultSemanticTokensLegend [] of
              Right ts -> responder $ Right $ LSP.InL ts
              Left t -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) t Nothing
          Nothing -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) "cannot get virtual file" Nothing,
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure (),
      notificationHandler LSP.SMethod_TextDocumentDidClose $ \_ -> pure ()
    ]

sendDocumentDiagnostics ::
  (m ~ LspM Config, LSP.HasUri a1 Uri, LSP.HasTextDocument a2 a1, LSP.HasParams s a2) =>
  L.LogAction m (WithSeverity T.Text) ->
  s ->
  LspT Config IO ()
sendDocumentDiagnostics logger msg = do
  let doc =
        msg
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to LSP.toNormalizedUri
  let (NormalizedUri _ path) = doc
  logger <& ("Publishing diagnostics for: " <> T.pack (show doc)) `WithSeverity` Info
  mdoc <- getVirtualFile doc
  case mdoc of
    Just file -> do
      ds <- liftIO $ checkText (T.unpack path) (virtualFileText file)
      sendDiagnostics doc (Just $ virtualFileVersion file) ds
    Nothing -> sendDiagnostics doc Nothing []

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

lexSemanticTokens :: String -> Text -> Either Text [SemanticTokenAbsolute]
lexSemanticTokens fileName src =
  case lexTokens fileName src of
    Left e -> Left $ T.pack $ pretty e
    -- The protocol has no multi-line tokens; split them per line.
    Right ts -> Right $ join $ map toAbsolutes ts
  where
    toAbsolutes :: Tok.Spanned Tok.Token -> [SemanticTokenAbsolute]
    toAbsolutes (Tok.Spanned sp t) = case toSemanticTokenTypes t of
      Just typ ->
        [ SemanticTokenAbsolute
            (fromIntegral $ l - 1)
            (fromIntegral $ c1 - 1)
            (fromIntegral $ c2 - c1)
            typ
            []
        | (l, c1, c2) <- splitSpanLines sp
        ]
      Nothing -> []

    splitSpanLines (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2))
      | l1 == l2 = [(l1, c1, c2)]
      | otherwise =
          [ (l, s, e)
          | l <- [l1 .. l2],
            let s = if l == l1 then c1 else 1,
            let e = if l == l2 then c2 else lineLength l + 1
          ]
    splitSpanLines S.NoSpan = []

    lineLength l = case drop (l - 1) (T.splitOn "\n" src) of
      (x : _) -> T.length x
      [] -> 0

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
      Tok.TString _ -> Just SemanticTokenTypes_String
      Tok.TEnvHead _ -> Just SemanticTokenTypes_Macro
      Tok.TCommand {} -> Just SemanticTokenTypes_String
      _ -> Nothing

toRange :: S.Span -> Range
toRange (S.Span (S.Position _ l1 c1) (S.Position _ l2 c2)) =
  mkRange
    (fromIntegral l1 - 1)
    (fromIntegral c1 - 1)
    (fromIntegral l2 - 1)
    (fromIntegral c2 - 1)
toRange S.NoSpan = Range (Position 0 0) (Position 0 0)
