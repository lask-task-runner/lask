{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @lask fmt@ (spec 11.11): format the .lask files of a project in
-- place, check that they are formatted, or format standard input.
module Command.Lask.Fmt
  ( cmdFmt,
    laskFilesUnder,
  )
where

import Command.Lask.Common (renderDiagsLines, usageError)
import Command.Lask.Options (CommonOpts (..), FmtOpts (..))
import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Language.Lask.ErrorCode (ErrorCode (EIoFs), codeText)
import Language.Lask.Syntax.Printer (FormatError (..), formatSource)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (normalise, takeDirectory, takeExtension, (</>))
import System.IO (stderr)

-- | What became of one file.
data Outcome
  = Unchanged
  | -- | Formatted, or with @--check@, not formatted.
    Changed
  | -- | Left as it is, with the exit code it calls for.
    Failed Int

cmdFmt :: FmtOpts -> IO ()
cmdFmt o
  | fmtStdin o = do
      unless (null (fmtPaths o)) $
        usageError opts "--stdin formats standard input and takes no PATH"
      fmtStdinOnly o
  | otherwise = do
      let roots = if null (fmtPaths o) then [takeDirectory (optModule opts)] else fmtPaths o
      files <- concat <$> mapM expand roots
      outcomes <- forM files $ \f -> (,) f <$> fmtFile o f
      let changed = [f | (f, Changed) <- outcomes]
          failures = [c | (_, Failed c) <- outcomes]
      if optJsonFormat opts
        then BL.putStr (A.encode changed <> "\n")
        else mapM_ putStrLn changed
      case failures of
        (c : _) -> exitWith (ExitFailure c)
        [] -> when (fmtCheck o && not (null changed)) (exitWith (ExitFailure 1))
  where
    opts = fmtCommon o
    expand p = do
      isDir <- doesDirectoryExist p
      isFile <- doesFileExist p
      if
        | isDir -> laskFilesUnder p
        | isFile -> pure [normalise p]
        | otherwise -> usageError opts ("no such file or directory: " <> T.pack p)

-- | The .lask files under a directory, in order. Directories whose name
-- starts with a dot are skipped — among them @.lask@, where the
-- dependencies are cached, and @.git@ — and so are symbolic links to
-- directories.
laskFilesUnder :: FilePath -> IO [FilePath]
laskFilesUnder dir = do
  names <- sort <$> listDirectory dir
  fmap concat . forM [n | n <- names, take 1 n /= "."] $ \n -> do
    let p = normalise (dir </> n)
    isDir <- doesDirectoryExist p
    link <- pathIsSymbolicLink p
    if
      | isDir && link -> pure []
      | isDir -> laskFilesUnder p
      | takeExtension n == ".lask" -> pure [p]
      | otherwise -> pure []

fmtFile :: FmtOpts -> FilePath -> IO Outcome
fmtFile o path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case r of
    Left e -> ioFailure (T.pack (show e))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _ -> ioFailure "not UTF-8 text"
      Right src -> case formatSource path src of
        Left err -> reportError (fmtCommon o) path err
        Right out
          | out == src -> pure Unchanged
          | fmtCheck o -> pure Changed
          | otherwise -> do
              w <- try (BS.writeFile path (TE.encodeUtf8 out)) :: IO (Either IOException ())
              either (ioFailure . T.pack . show) (const (pure Changed)) w
  where
    ioFailure msg = do
      TIO.hPutStrLn stderr $
        if optJsonFormat (fmtCommon o)
          then jsonLine [("code", A.String (codeText EIoFs)), ("file", A.String (T.pack path)), ("message", A.String msg)]
          else codeText EIoFs <> ": " <> T.pack path <> ": " <> msg
      pure (Failed 3)

fmtStdinOnly :: FmtOpts -> IO ()
fmtStdinOnly o = do
  bytes <- BS.getContents
  src <- case TE.decodeUtf8' bytes of
    Right t -> pure t
    Left _ -> do
      TIO.hPutStrLn stderr (codeText EIoFs <> ": standard input is not UTF-8 text")
      exitWith (ExitFailure 3)
  case formatSource "<stdin>" src of
    Left err -> reportError (fmtCommon o) "<stdin>" err >>= \outcome -> case outcome of
      Failed c -> exitWith (ExitFailure c)
      _ -> pure ()
    Right out
      | fmtCheck o -> when (out /= src) (exitWith (ExitFailure 1))
      | otherwise -> BS.putStr (TE.encodeUtf8 out)

-- | A file that does not parse is reported like @lask check@ reports
-- it; one the formatter got wrong is reported as the bug it is.
reportError :: CommonOpts -> FilePath -> FormatError -> IO Outcome
reportError opts path err = case err of
  FormatSyntax ds -> do
    TIO.hPutStrLn stderr (renderDiagsLines (optJsonFormat opts) ds)
    pure (Failed 1)
  FormatInternal msg -> do
    TIO.hPutStrLn stderr $
      if optJsonFormat opts
        then jsonLine [("file", A.String (T.pack path)), ("message", A.String ("internal error: " <> msg))]
        else T.pack path <> ": internal error: " <> msg <> "; the file is left as it is (please report this)"
    pure (Failed 1)

jsonLine :: [(A.Key, A.Value)] -> Text
jsonLine = TE.decodeUtf8 . BL.toStrict . A.encode . A.object

