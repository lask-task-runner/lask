{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Lask (serve) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens hiding (Iso, (:<))
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
import Language.Lask (tokenize, validate)
import Language.Lask.Error (LanguageError)
import Language.Lask.Parser (Token (Token), TokenKind (..))
import qualified Language.Lask.Span as S
import Language.Lask.Utils (Pretty (pretty))

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, Show)

instance J.ToJSON Config where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Config

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
      { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0},
        parseConfig = \_old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right cfg,
        onConfigChange = const $ pure (),
        configSection = "lask",
        doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env),
        -- Handlers log to both the client and stderr
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
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ sendSyntaxError logger,
      notificationHandler LSP.SMethod_TextDocumentDidChange $ sendSyntaxError logger,
      notificationHandler LSP.SMethod_TextDocumentDidSave $ sendSyntaxError logger,
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
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure (), -- Nothing to do
      notificationHandler LSP.SMethod_TextDocumentDidClose $ \_ -> pure () -- Nothing to do
    ]

sendSyntaxError ::
  (m ~ LspM Config, LSP.HasUri a1 Uri, LSP.HasTextDocument a2 a1, LSP.HasParams s a2) =>
  L.LogAction m (WithSeverity T.Text) ->
  s ->
  LspT Config IO ()
sendSyntaxError logger msg = do
  let doc =
        msg
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to LSP.toNormalizedUri
  let (NormalizedUri _ path) = doc
  logger <& ("Processing DidSaveTextDocument for: " <> T.pack (show doc)) `WithSeverity` Info
  mdoc <- getVirtualFile doc
  case mdoc of
    Just file -> do
      let vs = validate (T.unpack path) (virtualFileText file)
      sendDiagnostics doc (Just $ virtualFileVersion file) vs
    Nothing -> sendDiagnostics doc Nothing []

sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> [LanguageError S.Span] -> LspM Config ()
sendDiagnostics fileUri version es = do
  let diags =
        map
          ( \e@(s :< _) -> do
              LSP.Diagnostic
                (toRange s)
                (Just LSP.DiagnosticSeverity_Error) -- severity
                Nothing -- code
                Nothing
                (Just "lask") -- source
                (T.pack $ pretty e)
                Nothing -- tags
                (Just [])
                Nothing
          )
          es
  publishDiagnostics 100 fileUri version (partitionBySource diags)

lexSemanticTokens :: String -> Text -> Either Text [SemanticTokenAbsolute]
lexSemanticTokens fileName src =
  case tokenize fileName src of
    Left e -> Left $ T.pack $ pretty e
    -- split input into lines because language server protocol is not supporting multi-line token.
    Right ts -> Right $ makeSemanticTokenAbsolutes $ join $ map (splitMultiLineToken src) ts
  where
    makeSemanticTokenAbsolutes :: [Token] -> [SemanticTokenAbsolute]
    makeSemanticTokenAbsolutes [] = []
    makeSemanticTokenAbsolutes (Token x (S.Span (S.Position _ l1 c1) (S.Position _ _ c2)) : ts) =
      case toSemanticTokenTypes x of
        Just t ->
          SemanticTokenAbsolute
            (fromIntegral $ l1 - 1)
            (fromIntegral $ c1 - 1)
            (fromIntegral $ c2 - c1)
            t
            []
            : makeSemanticTokenAbsolutes ts
        Nothing -> makeSemanticTokenAbsolutes ts
    makeSemanticTokenAbsolutes (_ : ts) = makeSemanticTokenAbsolutes ts
    toSemanticTokenTypes :: TokenKind -> Maybe SemanticTokenTypes
    toSemanticTokenTypes t = case t of
      TKTypeVar -> Just SemanticTokenTypes_Type
      TKOp -> Just SemanticTokenTypes_Operator
      TKVar -> Just SemanticTokenTypes_Variable
      TKFunction -> Just SemanticTokenTypes_Function
      TKKeyword -> Just SemanticTokenTypes_Keyword
      TKNull -> Just SemanticTokenTypes_Macro
      TKBool -> Just SemanticTokenTypes_Macro
      TKNumber -> Just SemanticTokenTypes_Number
      TKString -> Just SemanticTokenTypes_String
      TKComment -> Just SemanticTokenTypes_Comment
      TKParameter -> Just SemanticTokenTypes_Parameter
      TKTypeParameter -> Just SemanticTokenTypes_TypeParameter
      TKProperty -> Just SemanticTokenTypes_Property
      TKImage -> Just SemanticTokenTypes_Macro
      _ -> Nothing

splitMultiLineToken :: Text -> Token -> [Token]
splitMultiLineToken _ (Token _ S.NoSpan) = []
splitMultiLineToken src (Token kind (S.Span (S.Position f1 l1 c1) (S.Position f2 l2 c2))) =
  map
    ( \l -> do
        let c1' = if l == l1 then c1 else 1
        let c2' = if l == l2 then c2 else T.length (getLineAt l src) + 1
        Token
          kind
          (S.Span (S.Position f1 l c1') (S.Position f2 l c2'))
    )
    [l1 .. l2]
  where
    getLineAt :: Int -> Text -> Text
    getLineAt line input = T.splitOn "\n" input !! (line - 1)

toRange :: S.Span -> Range
toRange (S.Span p1@(S.Position _ l1 c1) p2@(S.Position _ l2 c2)) =
  if p1 == p2
    then
      mkRange
        (fromIntegral l1 - 1)
        (fromIntegral c1 - 1)
        (fromIntegral l2 - 1)
        (fromIntegral c2)
    else
      mkRange
        (fromIntegral l1 - 1)
        (fromIntegral c1 - 1)
        (fromIntegral l2 - 1)
        (fromIntegral c2 - 1)
toRange S.NoSpan = Range (Position 0 0) (Position 0 0)
