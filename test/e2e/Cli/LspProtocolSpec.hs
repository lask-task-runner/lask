{-# LANGUAGE OverloadedStrings #-}

-- | The language server spoken to over its real protocol (@lask serve@):
-- JSON-RPC on stdin and stdout, as an editor speaks it. The handlers
-- themselves are tested in "Language.LSP.LaskSpec"; this checks what
-- only the running server shows: its capabilities, documents kept in
-- step with incremental edits, positions in UTF-16 code units, and how
-- it ends.
module Cli.LspProtocolSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Support.Harness
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

data Server = Server
  { sIn :: Handle,
    sOut :: Handle,
    sProc :: ProcessHandle,
    sNext :: IORef Int,
    -- | Messages read while waiting for another, oldest first.
    sSeen :: IORef [A.Value],
    sDir :: FilePath
  }

withServer :: FilePath -> (Server -> IO a) -> IO a
withServer lask action = withSystemTempDirectory "lask-lsp" $ \dir ->
  bracket
    (createProcess (proc lask ["serve"]) {cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
    (\(_, _, _, ph) -> terminateProcess ph >> void (waitForProcess ph))
    $ \handles -> case handles of
      (Just i, Just o, Just e, ph) -> do
        mapM_ (`hSetBinaryMode` True) [i, o]
        -- Drain stderr so that the server never blocks on it.
        _ <- forkIO (BC.hGetContents e >>= \c -> BC.length c `seq` pure ())
        s <- Server i o ph <$> newIORef 1 <*> newIORef [] <*> pure dir
        action s
      _ -> fail "the server's streams were not piped"

send :: Server -> A.Value -> IO ()
send s v = do
  let body = BL.toStrict (A.encode v)
  BC.hPut (sIn s) ("Content-Length: " <> BC.pack (show (BC.length body)) <> "\r\n\r\n" <> body)
  hFlush (sIn s)

readMessage :: Server -> IO A.Value
readMessage s = do
  len <- headers Nothing
  body <- BC.hGet (sOut s) len
  maybe (fail ("not JSON: " <> BC.unpack body)) pure (A.decodeStrict body)
  where
    headers acc = do
      l <- BC.hGetLine (sOut s)
      let line = BC.takeWhile (/= '\r') l
      if BC.null line
        then maybe (fail "no Content-Length") pure acc
        else case BC.stripPrefix "Content-Length: " line of
          Just n -> headers (fst <$> BC.readInt n)
          Nothing -> headers acc

-- | The first message the predicate accepts, among those already read
-- and those that follow, within five seconds.
await :: Server -> (A.Value -> Bool) -> IO A.Value
await s p = do
  seen <- readIORef (sSeen s)
  case break p seen of
    (earlier, m : later) -> writeIORef (sSeen s) (earlier <> later) >> pure m
    _ -> do
      r <- timeout 10000000 loop
      maybe (expectationFailure "no such message from the server" >> fail "unreachable") pure r
  where
    loop = do
      m <- readMessage s
      if p m then pure m else modifyIORef (sSeen s) (<> [m]) >> loop

request :: Server -> Text -> A.Value -> IO A.Value
request s method params = do
  n <- atomicModifyIORef' (sNext s) (\k -> (k + 1, k))
  send s (A.object ["jsonrpc" A..= ("2.0" :: Text), "id" A..= n, "method" A..= method, "params" A..= params])
  await s (\m -> field "id" m == Just (A.toJSON n))

notify :: Server -> Text -> A.Value -> IO ()
notify s method params = send s (A.object ["jsonrpc" A..= ("2.0" :: Text), "method" A..= method, "params" A..= params])

field :: Text -> A.Value -> Maybe A.Value
field k (A.Object o) = KM.lookup (AK.fromText k) o
field _ _ = Nothing

path :: [Text] -> A.Value -> Maybe A.Value
path ks v = foldl (\acc k -> acc >>= field k) (Just v) ks

initialize :: Server -> IO A.Value
initialize s = do
  r <- request s "initialize" (A.object ["processId" A..= A.Null, "rootUri" A..= ("file://" <> T.pack (sDir s)), "capabilities" A..= A.object []])
  notify s "initialized" (A.object [])
  pure r

uriOf :: Server -> FilePath -> Text
uriOf s f = "file://" <> T.pack (sDir s </> f)

open :: Server -> FilePath -> Text -> IO ()
open s f text = do
  writeFile (sDir s </> f) (T.unpack text)
  notify s "textDocument/didOpen" (A.object ["textDocument" A..= A.object ["uri" A..= uriOf s f, "languageId" A..= ("lask" :: Text), "version" A..= (1 :: Int), "text" A..= text]])

pos :: Int -> Int -> A.Value
pos l c = A.object ["line" A..= l, "character" A..= c]

-- | The diagnostics next published for a document.
diagnosticsOf :: Server -> FilePath -> IO [A.Value]
diagnosticsOf s f = do
  m <- await s (\m -> field "method" m == Just "textDocument/publishDiagnostics" && path ["params", "uri"] m == Just (A.toJSON (uriOf s f)))
  pure $ case path ["params", "diagnostics"] m of
    Just (A.Array xs) -> V.toList xs
    _ -> []

textDoc :: Server -> FilePath -> A.Value
textDoc s f = A.object ["uri" A..= uriOf s f]

spec :: Spec
spec = beforeAll findLask $ describe "the language server over its protocol" $ do
  it "announces what it provides" $ \lask -> withServer lask $ \s -> do
    r <- initialize s
    let cap k = path ["result", "capabilities", k] r
    mapM_
      (\k -> cap k `shouldSatisfy` maybe False (/= A.Null))
      ["textDocumentSync", "hoverProvider", "completionProvider", "documentFormattingProvider", "codeActionProvider", "semanticTokensProvider", "inlayHintProvider"]
    path ["result", "capabilities", "textDocumentSync", "change"] r `shouldBe` Just (A.Number 2)
    path ["result", "capabilities", "completionProvider", "triggerCharacters"] r `shouldBe` Just (A.toJSON ([".", "-", "=", "(", ",", " "] :: [Text]))

  it "publishes the errors of a document it opens, with their code" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "f(): Number = nope\n"
    ds <- diagnosticsOf s "main.lask"
    case ds of
      [d] -> do
        fmap show (field "code" d) `shouldSatisfy` maybe False (T.isInfixOf "E-NAME-UNDEFINED" . T.pack)
        path ["range", "start"] d `shouldBe` Just (pos 0 14)
      _ -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  it "keeps the document in step with incremental edits" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "f(): Number = nope\n"
    _ <- diagnosticsOf s "main.lask"
    notify s "textDocument/didChange" $
      A.object
        [ "textDocument" A..= A.object ["uri" A..= uriOf s "main.lask", "version" A..= (2 :: Int)],
          "contentChanges" A..= [A.object ["range" A..= A.object ["start" A..= pos 0 14, "end" A..= pos 0 18], "text" A..= ("1" :: Text)]]
        ]
    diagnosticsOf s "main.lask" `shouldReturn` []

  it "counts positions in UTF-16 code units, both ways" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    -- The emoji is two code units: the name after it starts at 11.
    open s "main.lask" "a = [\"\128512\", nope]\n"
    ds <- diagnosticsOf s "main.lask"
    map (path ["range", "start"]) ds `shouldBe` [Just (pos 0 11)]
    notify s "textDocument/didChange" $
      A.object
        [ "textDocument" A..= A.object ["uri" A..= uriOf s "main.lask", "version" A..= (2 :: Int)],
          "contentChanges" A..= [A.object ["range" A..= A.object ["start" A..= pos 0 11, "end" A..= pos 0 15], "text" A..= ("\"x\"" :: Text)]]
        ]
    diagnosticsOf s "main.lask" `shouldReturn` []

  it "formats a document, and edits nothing in one already formatted" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "f( ):Number=1\n"
    _ <- diagnosticsOf s "main.lask"
    r <- request s "textDocument/formatting" (A.object ["textDocument" A..= textDoc s "main.lask", "options" A..= A.object ["tabSize" A..= (2 :: Int), "insertSpaces" A..= True]])
    case path ["result"] r of
      Just (A.Array edits) | [e] <- V.toList edits -> field "newText" e `shouldBe` Just "f(): Number = 1\n"
      other -> expectationFailure ("expected one edit, got " <> show other)
    open s "ok.lask" "f(): Number = 1\n"
    _ <- diagnosticsOf s "ok.lask"
    r2 <- request s "textDocument/formatting" (A.object ["textDocument" A..= textDoc s "ok.lask", "options" A..= A.object ["tabSize" A..= (2 :: Int), "insertSpaces" A..= True]])
    path ["result"] r2 `shouldBe` Just (A.Array V.empty)

  it "answers hover with the type of what is under the cursor" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "greet(name: String): String = \"hi #{name}\"\nf(): String = greet(\"a\")\n"
    _ <- diagnosticsOf s "main.lask"
    r <- request s "textDocument/hover" (A.object ["textDocument" A..= textDoc s "main.lask", "position" A..= pos 1 15])
    fmap show (path ["result", "contents"] r) `shouldSatisfy` maybe False (T.isInfixOf "String" . T.pack)

  it "completes the names in scope and the built-ins" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "greet(): String = \"hi\"\nf(): String = gr\n"
    _ <- diagnosticsOf s "main.lask"
    r <- request s "textDocument/completion" (A.object ["textDocument" A..= textDoc s "main.lask", "position" A..= pos 1 16])
    let labels = case path ["result"] r of
          Just (A.Array xs) -> [l | x <- V.toList xs, Just (A.String l) <- [field "label" x]]
          Just o@(A.Object _) | Just (A.Array xs) <- field "items" o -> [l | x <- V.toList xs, Just (A.String l) <- [field "label" x]]
          _ -> []
    labels `shouldContain` ["greet"]

  it "offers the correction of a misspelt name as a quick fix" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "greet(): String = \"hi\"\nf(): String = gret()\n"
    ds <- diagnosticsOf s "main.lask"
    r <- request s "textDocument/codeAction" (A.object ["textDocument" A..= textDoc s "main.lask", "range" A..= A.object ["start" A..= pos 1 14, "end" A..= pos 1 18], "context" A..= A.object ["diagnostics" A..= ds]])
    show (path ["result"] r) `shouldContain` "greet"

  it "answers semantic tokens for a document" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    open s "main.lask" "f(): String = $[#local] echo hi\n"
    _ <- diagnosticsOf s "main.lask"
    r <- request s "textDocument/semanticTokens/full" (A.object ["textDocument" A..= textDoc s "main.lask"])
    case path ["result", "data"] r of
      Just (A.Array xs) -> do
        V.length xs `shouldSatisfy` (> 0)
        V.length xs `mod` 5 `shouldBe` 0
      other -> expectationFailure ("no tokens: " <> show other)

  it "answers for a document it was not given, without failing" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    h <- request s "textDocument/hover" (A.object ["textDocument" A..= textDoc s "unknown.lask", "position" A..= pos 0 0])
    field "error" h `shouldBe` Nothing
    c <- request s "textDocument/completion" (A.object ["textDocument" A..= textDoc s "unknown.lask", "position" A..= pos 0 0])
    field "error" c `shouldBe` Nothing

  it "answers a method it does not know with MethodNotFound, and goes on" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    r <- request s "lask/nonsense" (A.object [])
    path ["error", "code"] r `shouldBe` Just (A.Number (-32601))
    open s "main.lask" "f(): Number = nope\n"
    ds <- diagnosticsOf s "main.lask"
    length ds `shouldBe` 1

  it "exits 0 after shutdown and exit" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    r <- request s "shutdown" A.Null
    field "error" r `shouldBe` Nothing
    notify s "exit" A.Null
    code <- timeout 5000000 (waitForProcess (sProc s))
    code `shouldBe` Just ExitSuccess

  it "exits 1 on exit without shutdown" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    notify s "exit" A.Null
    code <- timeout 5000000 (waitForProcess (sProc s))
    code `shouldBe` Just (ExitFailure 1)

  it "ends when the editor closes its input" $ \lask -> withServer lask $ \s -> do
    _ <- initialize s
    hClose (sIn s)
    code <- timeout 5000000 (waitForProcess (sProc s))
    code `shouldSatisfy` maybe False (const True)
