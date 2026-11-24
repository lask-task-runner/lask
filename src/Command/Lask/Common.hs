{-# LANGUAGE OverloadedStrings #-}

-- | What the subcommands share: compiling the entry module or exiting,
-- reporting usage errors, rendering diagnostics, and reading the lock
-- (spec 11.3, 14.3).
module Command.Lask.Common
  ( compileOrExit,
    usageError,
    usageErrorSuggesting,
    noSuchFunction,
    cliCallable,
    cliFunctionNames,
    cliFunction,
    cliFunctionNamed,
    cliNamespaces,
    FunctionPath (..),
    cliFunctionPath,
    notAFunction,
    internalFunction,
    renderDiags,
    renderDiagsLines,
    diagJson,
    loadLockOrExit,
    encodeJsonText,
    outputFormat,
    outputJson,
    jsonOut,
  )
where

import Command.Lask.Options (CommonOpts (..))
import Command.Lask.ArgCodec (OutputFormat (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..))
import Language.Lask.Module.Resolve (entryPublicValues)
import Language.Lask.Types (Type (TyFun))
import Language.Lask.Deps.Lock
import Language.Lask.Diagnostic (Diagnostic (..), didYouMean)
import Language.Lask.ErrorCode
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Suggest (suggest)
import Language.Lask.Utils (Pretty (pretty), cliPath, functionPath, kebabToSnake, namespacesOf)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

compileOrExit :: CommonOpts -> IO Compiled
compileOrExit opts = do
  r <- compileFile (optModule opts)
  case r of
    Right compiled -> pure compiled
    Left ds -> do
      TIO.hPutStrLn stderr (renderDiagsLines (optLogJson opts) ds)
      exitWith (ExitFailure 1)


usageError :: CommonOpts -> Text -> IO a
usageError opts msg = usageErrorSuggesting opts msg []

-- | A usage error about a name the command line misspelt, with the
-- names close to it (spec 14.3).
usageErrorSuggesting :: CommonOpts -> Text -> [Text] -> IO a
usageErrorSuggesting opts msg suggestions = do
  if optLogJson opts
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
    map cliPath (suggest (kebabToSnake (T.replace " " "." wanted)) functions)

-- | Whether the CLI can call a declaration (spec 11.2): its type is a
-- function type. That is a function declaration, or a value binding
-- whose value is a function; a plain value is not called.
cliCallable :: CoreDecl -> Bool
cliCallable cd = isJust (cdParams cd) || isFun (cdType cd)
  where
    isFun (TyFun _ _) = True
    isFun _ = False

-- | The public symbols of the entry module the CLI can call, re-exported
-- ones included, with the declaration each names.
cliFunctions :: Compiled -> [(Text, ((FilePath, Text), CoreDecl))]
cliFunctions compiled =
  [ (n, (key, cd))
  | (n, key) <- entryPublicValues (compiledProgram compiled) (compiledScopes compiled),
    Just cd <- [Map.lookup key (cpDecls (compiledCore compiled))],
    cliCallable cd
  ]

-- | The names 'cliFunction' accepts, as declared.
cliFunctionNames :: Compiled -> [Text]
cliFunctionNames = map fst . cliFunctions

-- | The namespaces through which the entry module publishes values
-- (spec 5, "Namespace re-export"), as declared: the names a function
-- path on the command line may go through (11.2).
cliNamespaces :: Compiled -> Set Text
cliNamespaces compiled =
  namespacesOf (map fst (entryPublicValues (compiledProgram compiled) (compiledScopes compiled)))

-- | The function path at the head of the words that follow a
-- subcommand (spec 11.2): its name as declared, the words it took, and
-- the words after it, which are the function's arguments.
data FunctionPath = FunctionPath
  { fpName :: Text,
    fpWords :: [Text],
    fpRest :: [Text]
  }

cliFunctionPath :: Set Text -> [Text] -> FunctionPath
cliFunctionPath namespaces ws = case functionPath namespaces ws of
  (n, taken, rest) -> FunctionPath n taken rest

-- | The function a path on the command line gives (spec 11.2, 11.4,
-- 11.10), after the @-@ to @_@ mapping: a public function of the entry
-- module, or a usage error. A name of a plain value is told so, and
-- one of a namespace is told which functions it holds.
cliFunction :: CommonOpts -> Compiled -> FunctionPath -> IO ((FilePath, Text), CoreDecl)
cliFunction opts compiled fp = do
  let n = fpName fp
  case lookup n (cliFunctions compiled) of
    Just found -> pure found
    Nothing
      | n `Set.member` cliNamespaces compiled -> groupNeeded opts raw n (cliFunctionNames compiled)
      | n `elem` map fst (entryPublicValues (compiledProgram compiled) (compiledScopes compiled)) ->
          notAFunction opts raw
      -- Declared, but not public: say so rather than that it is not
      -- there (spec 11.2).
      | Map.member (cpEntry core, n) (cpDecls core) -> internalFunction opts raw (cpEntry core)
      | otherwise -> noSuchFunction opts raw (cliFunctionNames compiled)
  where
    core = compiledCore compiled
    raw = T.unwords (fpWords fp)

-- | The function the words given in place of one name, where nothing
-- follows it: the @<function>@ of @envs@ (11.4) and @secrets@ (11.10).
cliFunctionNamed :: CommonOpts -> Compiled -> [Text] -> IO ((FilePath, Text), CoreDecl)
cliFunctionNamed opts compiled ws = do
  let fp = cliFunctionPath (cliNamespaces compiled) ws
  found <- cliFunction opts compiled fp
  case fpRest fp of
    [] -> pure found
    (w : _) -> usageError opts ("unexpected argument '" <> w <> "' after the function '" <> T.unwords (fpWords fp) <> "'")

-- | A path that stops at a namespace (spec 11.2): it names a group of
-- functions rather than one, and the message lists the names that may
-- follow it, as the command line writes them.
groupNeeded :: CommonOpts -> Text -> Text -> [Text] -> IO a
groupNeeded opts raw ns functions =
  usageError opts $
    "'" <> raw <> "' is a group of functions: name one of them" <> case members of
      [] -> ""
      ms -> " (" <> T.intercalate ", " (map cliPath ms) <> ")"
  where
    members = nub [T.takeWhile (/= '.') rest | f <- functions, Just rest <- [T.stripPrefix (ns <> ".") f]]

-- | A name on the command line that names an @internal@ declaration
-- of the entry module, which the CLI does not call (spec 11.2).
internalFunction :: CommonOpts -> Text -> FilePath -> IO a
internalFunction opts raw entry =
  usageError opts $
    "'" <> raw <> "' is internal: the command line calls the public functions of " <> T.pack entry <> "; remove 'internal' from its declaration to call it"

-- | A name on the command line that names a plain value binding, which
-- the CLI does not call (spec 11.2).
notAFunction :: CommonOpts -> Text -> IO a
notAFunction opts raw =
  usageError opts $
    "'" <> raw <> "' is a value, not a function: the command line calls functions; declare it as '"
      <> kebabToSnake raw
      <> "()' to call it"

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
      TIO.hPutStrLn stderr (renderDiagsLines (optLogJson opts) [d])
      exitWith (ExitFailure 1)
    Right Nothing -> do
      TIO.hPutStrLn stderr (codeText EModuleLockStale <> ": no lock file; run 'lask sync'")
      exitWith (ExitFailure 1)
    Right (Just lf) -> pure lf

-- | A JSON value as one line of text.
-- | What stdout is written in (spec 11.3): what @--output@ says, or
-- the command's own default.
outputFormat :: OutputFormat -> CommonOpts -> OutputFormat
outputFormat dflt opts = maybe dflt id (optOutput opts)

-- | Whether a command whose report defaults to text writes it as JSON.
outputJson :: CommonOpts -> Bool
outputJson opts = outputFormat OutputText opts /= OutputText

-- | JSON written to stdout, formatted when @--output pretty-json@ asks
-- for it.
jsonOut :: CommonOpts -> Text -> Text
jsonOut opts t = case optOutput opts of
  Just OutputPrettyJson
    | Just v <- A.decodeStrict (TE.encodeUtf8 t) -> TE.decodeUtf8 (BL.toStrict (AP.encodePretty (v :: A.Value)))
  _ -> t

encodeJsonText :: A.Value -> Text
encodeJsonText = TE.decodeUtf8 . BL.toStrict . A.encode
