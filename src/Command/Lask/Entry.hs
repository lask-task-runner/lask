{-# LANGUAGE TupleSections #-}

module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.Parser (RootCommand (..), parseArguments, replaceArgumentVar)
import Control.Monad (void)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Maybe (catMaybes)
import qualified Data.Text.IO as T
import Language.LSP.Lask (serve)
import Language.Lask
import qualified Language.Lask.AST as AST
import Language.Lask.Span (Span (NoSpan))
import Language.Lask.Utils (Pretty (pretty), SwitchCofree (switchCofree))
import System.IO (hPutStrLn, hReady, stderr, stdin)

runRootCommand :: RootCommand -> IO ()
runRootCommand cmd = case cmd of
  Serve -> void serve
  Check filePath -> do
    src <- T.readFile filePath
    case validate filePath src of
      [] -> hPutStrLn stderr "The module is valid!"
      errors -> hPutStrLn stderr $ pretty errors
  Run filePath functionName rawArgs -> do
    result <- runExceptT $ do
      src <- liftIO $ T.readFile filePath
      env <- liftIO loadCommandLineEnvironment
      planeArgs <- liftEither $ parseArguments rawArgs
      let args = replaceArgumentVar env <$> planeArgs
      evaluate filePath src functionName args
    case result of
      Left e -> hPutStrLn stderr $ pretty e
      Right _ -> pure ()
  Eval filePath functionName rawArgs -> do
    result <- runExceptT $ do
      src <- liftIO $ T.readFile filePath
      env <- liftIO loadCommandLineEnvironment
      planeArgs <- liftEither $ parseArguments rawArgs
      let args = replaceArgumentVar env <$> planeArgs
      evaluate filePath src functionName args
    case result of
      Left e -> hPutStrLn stderr $ pretty e
      Right e -> putStrLn (unpack $ encode e)
  Infer filePath functionName -> do
    result <- runExceptT $ do
      src <- liftIO $ T.readFile filePath
      liftEither $ infer filePath src functionName
    case result of
      Left e -> hPutStrLn stderr $ pretty e
      Right e -> putStrLn $ pretty e

loadCommandLineEnvironment :: IO [(String, AST.Expr Span)]
loadCommandLineEnvironment = do
  maybeStdinRaw <- getStdin
  let decodedExpr = maybeStdinRaw >>= decodeExpr
  let maybeStdin = ("@stdin",) <$> decodedExpr
  pure $ catMaybes [maybeStdin]

decodeExpr :: ByteString -> Maybe (AST.Expr Span)
decodeExpr raw = do
  e <- decode raw :: Maybe (AST.Expr ())
  pure $ switchCofree (const NoSpan) e

getStdin :: IO (Maybe ByteString)
getStdin = do
  hasStdin <- hReady stdin
  if hasStdin then Just <$> B.getContents else pure Nothing
