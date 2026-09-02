{-# LANGUAGE OverloadedStrings #-}

-- | Interactive session (spec 11.1).
--
-- Each input line is either a top-level declaration (accumulated for
-- the rest of the session) or an expression (evaluated and printed).
-- The whole session source is recompiled per input through the same
-- pipeline as @run@\/@eval@, so static errors are reported the same
-- way. Note: @stdin@ is not provided in the REPL (spec 9.3); here it
-- is bound to the empty string.
module Language.Lask.Repl
  ( runRepl,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), compileWith)
import Language.Lask.Diagnostic (Diagnostic)
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.Module.Loader (fileReader)
import Language.Lask.Obs.CommandLog (newLineWriter, textCommandLog)
import Language.Lask.Runtime.Environment (mkCommandRunner)
import Language.Lask.Runtime.Eval (mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue)
import Language.Lask.Syntax.Parser (parseExpr)
import Language.Lask.Utils (Pretty (pretty))
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeDirectory, (</>))
import System.IO (stderr)

-- | The synthetic binding used to evaluate expression inputs.
resultName :: Text
resultName = "repl_result__"

runRepl :: FilePath -> IO ()
runRepl modulePath = do
  exists <- doesFileExist modulePath
  baseSource <-
    if exists
      then TIO.readFile modulePath
      else pure ""
  putStrLn "lask repl — :quit to exit"
  runInputT defaultSettings (loop modulePath baseSource)

loop :: FilePath -> Text -> InputT IO ()
loop modulePath source = do
  minput <- getInputLine "lask> "
  case minput of
    Nothing -> pure ()
    Just input
      | trimmed `elem` [":q", ":quit", ":exit"] -> pure ()
      | T.null trimmed -> loop modulePath source
      | otherwise -> case parseExpr "<repl>" trimmed of
          Right _ -> do
            -- Expression: bind it to a synthetic name and evaluate.
            let source' = source <> "\n" <> resultName <> " = " <> trimmed <> "\n"
            r <- liftIO (evalSession modulePath source')
            case r of
              Left ds -> mapM_ (outputStrLn . pretty) ds
              Right (Left lf) -> outputStrLn (renderFailure lf)
              Right (Right v) -> case v of
                VVoid -> pure ()
                _ -> outputStrLn (T.unpack (encodeValue v))
            loop modulePath source
          Left _ -> do
            -- Declaration: accumulate it if the session still compiles.
            let source' = source <> "\n" <> trimmed <> "\n"
            r <- liftIO (compileSession modulePath source')
            case r of
              Left ds -> do
                mapM_ (outputStrLn . pretty) ds
                loop modulePath source
              Right _ -> loop modulePath source'
      where
        trimmed = T.strip (T.pack input)

compileSession :: FilePath -> Text -> IO (Either [Diagnostic] Compiled)
compileSession modulePath source = compileWith reader modulePath
  where
    reader p
      | normalise p == normalise modulePath = pure (Right source)
      | otherwise = fileReader p

evalSession :: FilePath -> Text -> IO (Either [Diagnostic] (Either LaskFailure Value))
evalSession modulePath source = do
  r <- compileSession modulePath source
  case r of
    Left ds -> pure (Left ds)
    Right compiled -> do
      let core = compiledCore compiled
          baseDir = takeDirectory (normalise modulePath)
      writeErr <- newLineWriter stderr
      runner <- mkCommandRunner baseDir (textCommandLog writeErr)
      ctx <- mkRtCtx core "" runner
      result <- try (topValue ctx (cpEntry core, resultName))
      pure (Right result)

renderFailure :: LaskFailure -> String
renderFailure lf = case lfError lf of
  VRecord m
    | Just (VString s) <- Map.lookup "message" m ->
        "error: " <> T.unpack s
  other -> "error: " <> T.unpack (encodeValue other)
