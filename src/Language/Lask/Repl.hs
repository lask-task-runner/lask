{-# LANGUAGE OverloadedStrings #-}

-- | Interactive session (spec 11.9).
--
-- Each input line is a session command (@:reload@, @:quit@), a
-- top-level declaration (accumulated for the rest of the session) or
-- an expression (evaluated and printed). The whole session source is
-- recompiled per input through the same pipeline as @run@\/@eval@, so
-- static errors are reported the same way. Note: @stdin@ is not
-- provided in the REPL (spec 9.3); here it is bound to the empty
-- string.
module Language.Lask.Repl
  ( runRepl,
    Session (..),
    sessionSource,
    Input (..),
    classifyInput,
    ReloadFailure (..),
    reloadSession,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), compileWith)
import Language.Lask.Diagnostic (Diagnostic)
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.Module.Loader (fileReader)
import Language.Lask.Obs.CommandLog (newLineWriter, textCommandLog)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Obs.ExecLog (textLogSink)
import Language.Lask.Deps.Lock (LockFile (..), defaultLockFileName, loadLockFile)
import Language.Lask.Runtime.Environment (mkCommandRunner, mkFileRunner)
import Language.Lask.Runtime.Image (lockPins, unlockedPins)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, failureMessage)
import Language.Lask.Syntax.Parser (parseExpr)
import Language.Lask.Utils (Pretty (pretty))
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.FilePath (normalise, takeDirectory, takeFileName, (</>))
import System.IO (stderr)

-- | The synthetic binding used to evaluate expression inputs.
resultName :: Text
resultName = "repl_result__"

-- | The module text as last loaded from disk, and the declarations
-- typed at the prompt since, oldest first. They are kept apart so
-- that @:reload@ can replace the one and re-apply the other.
data Session = Session
  { sessionBase :: Text,
    sessionDecls :: [Text]
  }
  deriving (Eq, Show)

-- | The entry module's text as the session compiles it.
sessionSource :: Session -> Text
sessionSource (Session base decls) = base <> T.concat ["\n" <> d <> "\n" | d <- decls]

-- | One line of input, classified.
data Input
  = InputBlank
  | InputQuit
  | InputReload
  | -- | A known command given arguments it does not take.
    InputBadArgs Text
  | InputUnknownCommand Text
  | -- | A declaration or an expression.
    InputCode Text
  deriving (Eq, Show)

-- | Anything starting with @:@ is a session command; no declaration
-- or expression does.
classifyInput :: Text -> Input
classifyInput raw = case T.uncons trimmed of
  Nothing -> InputBlank
  Just (':', rest) ->
    let (name, args) = T.break (== ' ') rest
        noArgs i
          | T.null (T.strip args) = i
          | otherwise = InputBadArgs (":" <> name)
     in case name of
          _ | name `elem` ["q", "quit", "exit"] -> noArgs InputQuit
          _ | name `elem` ["r", "reload"] -> noArgs InputReload
          _ -> InputUnknownCommand (":" <> name)
  Just _ -> InputCode trimmed
  where
    trimmed = T.strip raw

data ReloadFailure
  = ReloadMissing
  | -- | The module does not compile on its own.
    ReloadInvalid [Diagnostic]
  | -- | The module compiles, but this declaration typed at the prompt
    -- no longer compiles on top of it.
    ReloadConflict Text [Diagnostic]

-- | Read the module again and re-apply the session's declarations on
-- top of it, in the order they were typed. Declarations are compiled,
-- not evaluated, and expressions are not replayed, so a reload runs
-- nothing. A reload is all or nothing: on any failure, including a
-- declaration that no longer compiles, the caller keeps the old
-- session.
reloadSession :: FilePath -> Session -> IO (Either ReloadFailure Session)
reloadSession modulePath old = do
  exists <- doesFileExist modulePath
  if not exists
    then pure (Left ReloadMissing)
    else do
      base <- TIO.readFile modulePath
      r <- compileSession modulePath base
      case r of
        Left ds -> pure (Left (ReloadInvalid ds))
        Right _ -> reapply (Session base []) (sessionDecls old)
  where
    -- One at a time, so that a failure names the declaration.
    reapply s [] = pure (Right s)
    reapply s (d : ds) = do
      let s' = s {sessionDecls = sessionDecls s <> [d]}
      r <- compileSession modulePath (sessionSource s')
      case r of
        Left diags -> pure (Left (ReloadConflict d diags))
        Right _ -> reapply s' ds

runRepl :: FilePath -> IO ()
runRepl modulePath = do
  exists <- doesFileExist modulePath
  baseSource <-
    if exists
      then TIO.readFile modulePath
      else pure ""
  putStrLn "lask repl — :reload to reload, :quit to exit"
  runInputT defaultSettings (loop modulePath (Session baseSource []))

loop :: FilePath -> Session -> InputT IO ()
loop modulePath session = do
  minput <- getInputLine "lask> "
  case classifyInput . T.pack <$> minput of
    Nothing -> pure ()
    Just InputQuit -> pure ()
    Just InputBlank -> loop modulePath session
    Just (InputBadArgs name) -> do
      outputStrLn ("error: " <> T.unpack name <> " takes no arguments")
      loop modulePath session
    Just (InputUnknownCommand name) -> do
      outputStrLn ("error: unknown command '" <> T.unpack name <> "'")
      loop modulePath session
    Just InputReload -> do
      r <- liftIO (reloadSession modulePath session)
      case r of
        Left ReloadMissing -> do
          outputStrLn ("error: " <> modulePath <> " not found; the previous session is kept.")
          loop modulePath session
        Left (ReloadInvalid ds) -> do
          mapM_ (outputStrLn . pretty) ds
          outputStrLn "reload failed; the previous session is kept."
          loop modulePath session
        Left (ReloadConflict d ds) -> do
          mapM_ (outputStrLn . pretty) ds
          outputStrLn ("reload failed: the REPL declaration '" <> T.unpack d <> "' no longer compiles; the previous session is kept.")
          loop modulePath session
        Right session' -> do
          outputStrLn ("Ok, reloaded " <> takeFileName modulePath <> "." <> reappliedNote (length (sessionDecls session')))
          loop modulePath session'
    Just (InputCode code) -> case parseExpr "<repl>" code of
      Right _ -> do
        -- Expression: bind it to a synthetic name and evaluate. It is
        -- not kept, so a reload never runs it again.
        let source' = sessionSource session <> "\n" <> resultName <> " = " <> code <> "\n"
        r <- liftIO (evalSession modulePath source')
        case r of
          Left ds -> mapM_ (outputStrLn . pretty) ds
          Right (Left lf) -> outputStrLn (renderFailure lf)
          Right (Right v) -> case v of
            VVoid -> pure ()
            _ -> outputStrLn (T.unpack (encodeValue v))
        loop modulePath session
      Left _ -> do
        -- Declaration: accumulate it if the session still compiles.
        let session' = session {sessionDecls = sessionDecls session <> [code]}
        r <- liftIO (compileSession modulePath (sessionSource session'))
        case r of
          Left ds -> do
            mapM_ (outputStrLn . pretty) ds
            loop modulePath session
          Right _ -> loop modulePath session'
  where
    reappliedNote 0 = ""
    reappliedNote 1 = " (1 REPL declaration re-applied)"
    reappliedNote n = " (" <> show n <> " REPL declarations re-applied)"

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
      -- The REPL is not among the subcommands that may not pull (spec
      -- 10.3): a reference the lock pins runs as pinned, and any other
      -- as written, the daemon pulling it if it has to.
      lock <- either (const Nothing) id <$> loadLockFile (baseDir </> defaultLockFileName)
      let pins = unlockedPins (lockPins (maybe mempty lockImages lock))
      runner <- mkCommandRunner pins baseDir (textCommandLog writeErr)
      fileRunner <- mkFileRunner pins baseDir
      ctx <- mkRtCtx core "" (RtHooks runner fileRunner (textLogSink writeErr) noAsyncTracker)
      result <- try (topValue ctx (cpEntry core, resultName))
      pure (Right result)

renderFailure :: LaskFailure -> String
renderFailure lf = "error: " <> T.unpack (failureMessage lf)
