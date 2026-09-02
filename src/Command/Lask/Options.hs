{-# LANGUAGE OverloadedStrings #-}

-- | CLI option definitions (spec 11.1).
module Command.Lask.Options
  ( RootCommand (..),
    CommonOpts (..),
    RunOpts (..),
    EnvsOpts (..),
    DepsAddSource (..),
    pRootCommand,
    runOptionsHelp,
    argSeparator,
    protectArgSeparator,
  )
where

import Command.Lask.ArgCodec
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

data CommonOpts = CommonOpts
  { optModule :: FilePath,
    optJsonFormat :: Bool,
    optTraceId :: Maybe Text,
    optNoColor :: Bool
  }

data RunOpts = RunOpts
  { runCommon :: CommonOpts,
    runArgDecode :: ArgDecodeMode,
    runStdoutEncode :: StdoutEncode,
    -- | @--help@ / @-h@ before the function name. After the function
    -- name it reaches 'runArgs' instead and is handled there
    -- (spec 11.2).
    runHelp :: Bool,
    -- | Absent for @lask run --help@, which lists the module's
    -- functions instead of calling one.
    runFunction :: Maybe Text,
    runArgs :: [Text]
  }

data EnvsOpts = EnvsOpts
  { envsCommon :: CommonOpts,
    envsFunction :: Maybe Text,
    envsCheck :: Bool
  }

data RootCommand
  = CmdServe
  | CmdCheck CommonOpts
  | CmdRun RunOpts
  | CmdEval RunOpts
  | CmdInfer CommonOpts (Maybe Text)
  | CmdRepl CommonOpts
  | CmdEnvs EnvsOpts
  | CmdDepsSync CommonOpts Bool
  | CmdDepsAdd CommonOpts Text DepsAddSource
  | CmdDepsWhy CommonOpts Text
  | CmdDepsDiff CommonOpts Text
  | CmdEnvBuild CommonOpts
  | CmdEnvList CommonOpts
  | CmdVersion

-- | The source of a @deps add@ entry (spec 11.5).
data DepsAddSource = AddGit Text Text | AddUrl Text

-- | optparse-applicative consumes a bare @--@ itself, which would
-- erase the boundary spec 11.2 gives it (everything after @--@ is an
-- argument of the function, @--help@ included). The first one is
-- swapped for this marker before parsing, so the argument scan in
-- @run@ \/ @eval@ can still see where it was.
argSeparator :: Text
argSeparator = "\SOH--"

protectArgSeparator :: [String] -> [String]
protectArgSeparator args = case break (== "--") args of
  (before, _ : after) -> before <> [T.unpack argSeparator] <> after
  _ -> args

pCommon :: Parser CommonOpts
pCommon =
  build
    <$> strOption
      ( long "module"
          <> metavar "PATH"
          <> value "main.lask"
          <> showDefault
          <> help "Path to the entry module"
      )
    <*> strOption
      ( long "format"
          <> metavar "text|json"
          <> value "text"
          <> help "Diagnostics output format"
      )
    <*> optional (T.pack <$> strOption (long "trace-id" <> metavar "ID" <> help "Trace identifier for this execution"))
    <*> switch (long "no-color" <> help "Disable colored output")
  where
    build m fmt tid nc = CommonOpts m (fmt == ("json" :: String)) tid nc

pRunOpts :: Parser RunOpts
pRunOpts =
  RunOpts
    <$> pCommon
    <*> option
      (maybeReader parseArgDecodeMode)
      ( long "arg-decode"
          <> metavar "text|json|auto"
          <> value DecodeAuto
          <> help "How to decode function arguments (default: auto)"
      )
    <*> option
      (maybeReader parseStdoutEncode)
      ( long "stdout-encode"
          <> metavar "text|json|pretty-json"
          <> value EncodeJson
          <> help "How to encode the eval result (default: json)"
      )
    <*> switch
      ( long "help"
          <> short 'h'
          <> help "Show the help of FUNCTION, or list the module's functions"
      )
    <*> optional (T.pack <$> argument str (metavar "FUNCTION"))
    <*> many (T.pack <$> argument str (metavar "ARGS..."))

pEnvsOpts :: Parser EnvsOpts
pEnvsOpts =
  EnvsOpts
    <$> pCommon
    <*> optional (T.pack <$> argument str (metavar "FUNCTION"))
    <*> switch (long "check" <> help "Check accessibility of each environment")

-- | Two deviations from the obvious parser for @run@ \/ @eval@.
--
-- @subparser@ rather than @hsubparser@: the latter installs its own
-- @--help@ in every subcommand, which would swallow the @--help@ that
-- spec 11.6 gives to @run@ \/ @eval@. Every other subcommand gets one
-- explicitly.
--
-- @noIntersperse@ rather than @forwardOptions@: both set the same
-- policy and the last one wins, and @forwardOptions@ keeps parsing
-- /known/ options after the function name, so @lask run f --module x@
-- bound @--module@ to @lask@ instead of to @f@. @noIntersperse@ turns
-- everything after the first positional into an argument, which is
-- the boundary rule of spec 11.2.
pRootCommand :: Parser RootCommand
pRootCommand =
  subparser
    ( command "serve" (withHelp (pure CmdServe) (progDesc "Start the language server"))
        <> command "check" (withHelp (CmdCheck <$> pCommon) (progDesc "Statically validate the module"))
        <> command
          "run"
          ( info
              (CmdRun <$> pRunOpts)
              (progDesc "Run a function (result is not printed)" <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              (CmdEval <$> pRunOpts)
              (progDesc "Run a function and print its result" <> noIntersperse)
          )
        <> command
          "infer"
          ( withHelp
              (CmdInfer <$> pCommon <*> optional (T.pack <$> strOption (long "symbol" <> metavar "NAME")))
              (progDesc "Print inferred types")
          )
        <> command "repl" (withHelp (CmdRepl <$> pCommon) (progDesc "Interactive session"))
        <> command "envs" (withHelp (CmdEnvs <$> pEnvsOpts) (progDesc "List and check environments"))
        <> command "deps" (withHelp pDepsCommand (progDesc "Manage external dependencies"))
        <> command "env" (withHelp pEnvCommand (progDesc "Materialize and inspect container images"))
        <> command "version" (withHelp (pure CmdVersion) (progDesc "Print the lask version"))
    )
  where
    withHelp p = info (p <**> helper)

-- | The option help of @run@ / @eval@, printed by @lask run --help@
-- before the module's function list (spec 11.6).
runOptionsHelp :: String -> String
runOptionsHelp subcommand =
  fst (renderFailure failure ("lask " <> subcommand))
  where
    failure =
      parserFailure
        defaultPrefs
        (info (CmdRun <$> pRunOpts) (progDesc desc <> noIntersperse))
        (ShowHelpText Nothing)
        []
    desc
      | subcommand == "eval" = "Run a function and print its result"
      | otherwise = "Run a function (result is not printed)"

pEnvCommand :: Parser RootCommand
pEnvCommand =
  hsubparser
    ( command
        "build"
        (info (CmdEnvBuild <$> pCommon) (progDesc "Materialize every image the program requires"))
        <> command
          "list"
          (info (CmdEnvList <$> pCommon) (progDesc "Report every image reference and whether it is present"))
    )

pDepsCommand :: Parser RootCommand
pDepsCommand =
  hsubparser
    ( command
        "sync"
        ( info
            ( CmdDepsSync
                <$> pCommon
                <*> switch (long "frozen" <> help "Fail instead of updating the lock file")
            )
            (progDesc "Fetch and verify all declared dependencies")
        )
        <> command
          "add"
          ( info
              ( CmdDepsAdd
                  <$> pCommon
                  <*> (T.pack <$> argument str (metavar "NAME"))
                  <*> pAddSource
              )
              (progDesc "Fetch a source, record it with its content hash, and cache it")
          )
        <> command
          "why"
          ( info
              (CmdDepsWhy <$> pCommon <*> (T.pack <$> argument str (metavar "NAME")))
              (progDesc "Report the graph paths through which a dependency is reached")
          )
        <> command
          "diff"
          ( info
              (CmdDepsDiff <$> pCommon <*> (T.pack <$> argument str (metavar "NAME")))
              (progDesc "Report what a dependency bump would change")
          )
    )
  where
    pAddSource =
      ( AddGit
          <$> (T.pack <$> strOption (long "git" <> metavar "URL" <> help "Git repository URL"))
          <*> (T.pack <$> strOption (long "rev" <> metavar "REV" <> help "Tag or commit"))
      )
        <|> (AddUrl . T.pack <$> strOption (long "url" <> metavar "URL" <> help "Archive or single .lask file URL"))
