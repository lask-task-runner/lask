{-# LANGUAGE OverloadedStrings #-}

-- | What the subcommands share: compiling the entry module or exiting,
-- reporting usage errors, rendering diagnostics, and reading the lock
-- (spec 11.3, 14.3).
module Command.Lask.Common
  ( compileOrExit,
    usageError,
    usageErrorSuggesting,
    noSuchFunction,
    renderDiags,
    renderDiagsLines,
    diagJson,
    loadLockOrExit,
    encodeJsonText,
  )
where

import Command.Lask.Options (CommonOpts (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Deps.Lock
import Language.Lask.Diagnostic (Diagnostic (..), didYouMean)
import Language.Lask.ErrorCode
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Suggest (suggest)
import Language.Lask.Utils (Pretty (pretty), kebabToSnake, snakeToKebab)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

compileOrExit :: CommonOpts -> IO Compiled
compileOrExit opts = do
  r <- compileFile (optModule opts)
  case r of
    Right compiled -> pure compiled
    Left ds -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) ds)
      exitWith (ExitFailure 1)


usageError :: CommonOpts -> Text -> IO a
usageError opts msg = usageErrorSuggesting opts msg []

-- | A usage error about a name the command line misspelt, with the
-- names close to it (spec 14.3).
usageErrorSuggesting :: CommonOpts -> Text -> [Text] -> IO a
usageErrorSuggesting opts msg suggestions = do
  if optJsonFormat opts
    then
      TIO.hPutStrLn stderr . TE.decodeUtf8 . BL.toStrict . A.encode . A.object $
        [("code", A.String (codeText ECliUsage)), ("message", A.String msg)]
          <> [("suggestions", A.toJSON suggestions) | not (null suggestions)]
    else
      TIO.hPutStrLn stderr . T.intercalate "\n  note: " $
        (codeText ECliUsage <> ": " <> msg) : didYouMean suggestions
  exitWith (ExitFailure 4)

-- | A function name the command line gave that the module does not
-- have, with the names of @functions@ close to it, in the form the
-- CLI writes them (spec 11.2).
noSuchFunction :: CommonOpts -> Text -> [Text] -> IO a
noSuchFunction opts wanted functions =
  usageErrorSuggesting opts ("no such function: '" <> wanted <> "'") $
    map snakeToKebab (suggest (kebabToSnake wanted) functions)

-- | Diagnostics for stdout (@check@): a JSON array in json mode.
renderDiags :: Bool -> [Diagnostic] -> Text
renderDiags jsonFormat ds
  | jsonFormat = TE.decodeUtf8 (BL.toStrict (A.encode (map diagJson ds)))
  | otherwise = renderDiagsText ds

-- | Diagnostics for stderr: JSON Lines, one object per line
-- (spec 12.2 canonical form; discriminated by code + stage).
renderDiagsLines :: Bool -> [Diagnostic] -> Text
renderDiagsLines jsonFormat ds
  | jsonFormat =
      T.intercalate "\n" (map (TE.decodeUtf8 . BL.toStrict . A.encode . diagJson) ds)
  | otherwise = renderDiagsText ds

-- | Diagnostics for a reader: at most 'diagLimit' of them, then how
-- many more there are (spec 14.3). JSON output is read by a program,
-- and carries every one.
renderDiagsText :: [Diagnostic] -> Text
renderDiagsText ds =
  T.intercalate "\n" $
    map (T.pack . pretty) shown
      <> ["... and " <> T.pack (show (length hidden)) <> " more " <> plural hidden | not (null hidden)]
  where
    (shown, hidden) = splitAt diagLimit ds
    plural [_] = "error"
    plural _ = "errors"

diagLimit :: Int
diagLimit = 50

diagJson :: Diagnostic -> A.Value
diagJson d =
  A.object $
    [ ("code", A.String (codeText (diagCode d))),
      ("stage", A.String (stageText (diagStage d))),
      ("message", A.String (diagMessage d))
    ]
      <> location (diagSpan d)
      <> maybe [] (\e -> [("expected", A.String e)]) (diagExpected d)
      <> maybe [] (\a -> [("actual", A.String a)]) (diagActual d)
      <> [("notes", A.toJSON (diagNotes d)) | not (null (diagNotes d))]
      <> [("suggestions", A.toJSON (diagSuggestions d)) | not (null (diagSuggestions d))]
  where
    location (Span (Position file l c) _) =
      [ ( "location",
          A.object
            [ ("file", A.String (T.pack file)),
              ("line", A.Number (fromIntegral l)),
              ("column", A.Number (fromIntegral c))
            ]
        )
      ]
    location NoSpan = []

loadLockOrExit :: CommonOpts -> IO LockFile
loadLockOrExit opts = do
  let baseDir = takeDirectory (optModule opts)
  r <- loadLockFile (baseDir </> defaultLockFileName)
  case r of
    Left d -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) [d])
      exitWith (ExitFailure 1)
    Right Nothing -> do
      TIO.hPutStrLn stderr (codeText EModuleLockStale <> ": no lock file; run 'lask sync'")
      exitWith (ExitFailure 1)
    Right (Just lf) -> pure lf

-- | A JSON value as one line of text.
encodeJsonText :: A.Value -> Text
encodeJsonText = TE.decodeUtf8 . BL.toStrict . A.encode
