{-# LANGUAGE OverloadedStrings #-}

-- | Evaluating a one-file program in process, against a command runner
-- the test gives, and a scripted runner whose commands say how long
-- they take and record what happened to them.
module Support.Eval
  ( evalWith,
    Script,
    scripted,
    events,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (onException, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner, RtHooks (..))
import Language.Lask.Diagnostic (diagCode)
import Language.Lask.Elaborate (elaborateProgram)
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Module.Loader (loadProgramWith)
import Language.Lask.Module.Resolve (validateProgram)
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Compile main.lask and evaluate declaration @name@, applying a
-- closure to no arguments. The result is canonical JSON; a failure is
-- its code and its error value as JSON, and a static rejection has no
-- code.
evalWith :: CommandRunner -> Text -> Text -> IO (Either (Maybe ErrorCode, Text) Text)
evalWith runner src name = do
  r <- loadProgramWith reader "main.lask"
  case r of
    Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
    Right prog -> case validateProgram prog of
      Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
      Right scopes -> case elaborateProgram prog scopes of
        Left ds -> pure (Left (Nothing, T.pack (show (map diagCode ds))))
        Right cp -> do
          ctx <- mkRtCtx cp "" (RtHooks runner noFiles (\_ env body -> body env) noLogSink noAsyncTracker (fmap (fmap T.pack) . lookupEnv . T.unpack))
          out <- try $ do
            v <- topValue ctx ("main.lask", name)
            case v of
              VClosure _ -> applyValue ctx v [] []
              _ -> pure v
          pure $ case out of
            Right v -> Right (encodeValue v)
            Left lf -> Left (lfCode lf, encodeValue (lfError lf))
  where
    reader p = pure (if p == "main.lask" then Right src else Left "not found")
    noFiles :: FileRunner
    noFiles _ op = pure $ case op of
      FileExists _ -> Right (VBool False)
      FileListDir _ -> Right (VArray V.empty)
      FileGlob _ -> Right (VArray V.empty)
      _ -> Left (ioFailure EIoFs "no files in this test")

-- | What the scripted runner records, in the order it happened.
type Script = IORef [Text]

-- | A runner whose commands are a script:
--
-- * @wait S TAG@ records @start TAG@, waits S seconds and records
--   @end TAG@, printing TAG; stopped before then, it records
--   @stop TAG@.
-- * @exit N TAG@ records @TAG@ and exits N, with TAG on stderr.
-- * @note TAG@ records @TAG@ and prints it.
scripted :: IO (CommandRunner, Script)
scripted = do
  seen <- newIORef []
  let record t = atomicModifyIORef' seen (\xs -> (xs <> [t], ()))
      runner _ cmd = case T.words cmd of
        ["wait", s, tag] | Just secs <- (readMaybe (T.unpack s) :: Maybe Double) -> do
          record ("start " <> tag)
          threadDelay (round (secs * 1000000)) `onException` record ("stop " <> tag)
          record ("end " <> tag)
          pure (Right (0, tag, ""))
        ["exit", n, tag] | Just code <- readMaybe (T.unpack n) -> do
          record tag
          pure (Right (code, "", tag))
        ["note", tag] -> record tag >> pure (Right (0, tag, ""))
        _ -> pure (Right (0, "", ""))
  pure (runner, seen)

events :: Script -> IO [Text]
events = readIORef
