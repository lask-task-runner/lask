{-# LANGUAGE OverloadedStrings #-}

-- | CLI option definitions (spec 11.1).
module Command.Lask.Options
  ( RootCommand (..),
    CommonOpts (..),
    RunOpts (..),
    EnvsOpts (..),
    SecretsOpts (..),
    CmdOpts (..),
    DepsAddSource (..),
    pRootCommand,
    runOptionsHelp,
    cmdOptionsHelp,
    argSeparator,
    protectArgSeparator,
    retiredCommand,
  )
where

import Command.Lask.ArgCodec
import Command.Lask.Complete (Shell, parseShell)
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
    -- | @--confirm@: approve the confirmation the project file asks
    -- for (spec 11.2).
    runConfirm :: Bool,
    -- | Absent for @lask run --help@, which lists the module's
    -- functions instead of calling one.
    runFunction :: Maybe Text,
    runArgs :: [Text]
  }

-- | @lask envs list@ (spec 11.4).
data EnvsOpts = EnvsOpts
  { envsCommon :: CommonOpts,
    envsFunction :: Maybe Text
  }

-- | @lask secrets list@ \/ @check@ (spec 11.10).
data SecretsOpts = SecretsOpts
  { secretsCommon :: CommonOpts,
    secretsFunction :: Maybe Text,
    secretsRead :: Bool
  }

-- | @lask cmd@ (spec 11.8): a declared command and everything after
-- its name, or @--help@.
data CmdOpts = CmdOpts
  { cmdCommon :: CommonOpts,
    -- | @--help@ / @-h@ before the command name, which also lists the
    -- module's commands. After the name it reaches 'cmdArgs'.
    cmdShowHelp :: Bool,
    cmdName :: Maybe Text,
    cmdArgs :: [Text]
  }

data RootCommand
  = CmdServe
  | CmdCheck CommonOpts
  | CmdRun RunOpts
  | CmdEval RunOpts
  | CmdRepl CommonOpts
  | CmdEnvsList EnvsOpts
  | -- | @--frozen@, @--prune@.
    CmdSync CommonOpts Bool Bool
  | CmdDepsList CommonOpts
  | -- | A dependency to show the paths to, and a depth.
    CmdDepsGraph CommonOpts (Maybe Text) (Maybe Int)
  | CmdDepsAdd CommonOpts Text DepsAddSource
  | CmdDepsRm CommonOpts Text
  | CmdSecretsList SecretsOpts
  | CmdSecretsCheck SecretsOpts
  | CmdCmd CmdOpts
  | CmdCompletion Shell
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

-- | Everything after the command name belongs to the program, with no
-- interception at all (spec 11.8) — not even @--help@, which a program
-- may define itself. @noIntersperse@ gives exactly that boundary, as it
-- does for @run@ / @eval@.
pCmdOpts :: Parser CmdOpts
pCmdOpts =
  CmdOpts
    <$> pCommon
    <*> switch (long "help" <> short 'h' <> help "Show this help text and list the module's commands")
    <*> optional (T.pack <$> argument str (metavar "COMMAND"))
    <*> many (T.pack <$> argument str (metavar "ARGS..."))

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
    <*> switch (long "confirm" <> help "Approve the confirmation lask.json asks for")
    <*> optional (T.pack <$> argument str (metavar "FUNCTION"))
    <*> many (T.pack <$> argument str (metavar "ARGS..."))

pEnvsOpts :: Parser EnvsOpts
pEnvsOpts =
  EnvsOpts
    <$> pCommon
    <*> optional (T.pack <$> argument str (metavar "FUNCTION"))

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
    ( commandGroup "Run tasks:"
        <> command
          "run"
          ( info
              (CmdRun <$> pRunOpts)
              (progDesc "Run a function (its result is not printed)" <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              (CmdEval <$> pRunOpts)
              (progDesc "Run a function and print its result" <> noIntersperse)
          )
        <> command
          "cmd"
          ( info
              (CmdCmd <$> pCmdOpts)
              (progDesc cmdDesc <> noIntersperse)
          )
        <> command "repl" (withHelp (CmdRepl <$> pCommon) (progDesc "Start an interactive session"))
        <> metavar "COMMAND"
    )
    <|> subparser
      ( commandGroup "Set up the project:"
          <> command
            "sync"
            ( withHelp
                ( CmdSync
                    <$> pCommon
                    <*> switch (long "frozen" <> help "Fail instead of updating lask.json or the lock file")
                    <*> switch (long "prune" <> help "Remove the dependencies no .lask file of the project imports")
                )
                (progDesc "Fetch dependencies, pull and build images, and write the lock file")
            )
          <> command "deps" (withHelp pDepsCommand (progDesc "List, graph, add and remove dependencies"))
          <> command "envs" (withHelp pEnvsCommand (progDesc "List environments, their images, and whether each is present"))
          <> command "secrets" (withHelp pSecretsCommand (progDesc "List and check secret references"))
          <> hidden
      )
    <|> subparser
      ( commandGroup "Develop:"
          <> command "check" (withHelp (CmdCheck <$> pCommon) (progDesc "Statically validate the module"))
          <> command "serve" (withHelp (pure CmdServe) (progDesc "Start the language server"))
          <> command
            "completion"
            ( withHelp
                (CmdCompletion <$> argument (maybeReader parseShell) (metavar "bash|zsh|fish"))
                (progDesc "Print the shell completion script")
            )
          <> command "version" (withHelp (pure CmdVersion) (progDesc "Print the lask version"))
          <> hidden
      )
  where
    withHelp p = info (p <**> helper)

-- | The option help of @lask cmd --help@, printed before the module's
-- command list, as 'runOptionsHelp' is for @run@ / @eval@.
cmdOptionsHelp :: String
cmdOptionsHelp =
  fst (renderFailure failure "lask cmd")
  where
    failure =
      parserFailure
        defaultPrefs
        (info (CmdCmd <$> pCmdOpts) (progDesc cmdDesc <> noIntersperse))
        (ShowHelpText Nothing)
        []

cmdDesc :: String
cmdDesc = "Run a declared command in its declared environment"

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

pEnvsCommand :: Parser RootCommand
pEnvsCommand =
  hsubparser
    ( command
        "list"
        ( info
            (CmdEnvsList <$> pEnvsOpts)
            (progDesc "List the environments, what the lock resolves them to, what requires them, and whether each image is on the Docker daemon")
        )
    )

-- | The spelling that replaced a retired command (spec 11.1), for the
-- arguments that invoke one. Answered before parsing, so the old
-- spelling is told where to go rather than only that it is not a
-- command.
retiredCommand :: [String] -> Maybe (String, String)
retiredCommand args = case args of
  "env" : "build" : _ -> Just ("env build", "sync")
  "env" : "list" : _ -> Just ("env list", "envs list")
  "deps" : "sync" : _ -> Just ("deps sync", "sync")
  "deps" : "why" : _ -> Just ("deps why", "deps graph")
  "deps" : "diff" : _ -> Just ("deps diff", "deps list")
  "cmd" : rest | "--list" `elem` cmdOptions rest -> Just ("cmd --list", "cmd --help")
  "envs" : rest
    | "--check" `elem` rest -> Just ("envs --check", "envs list")
    | otherwise -> case dropOptions rest of
        [] | all (`notElem` ["--help", "-h"]) rest -> Just ("envs", "envs list")
        "check" : _ -> Just ("envs check", "envs list")
        w : _ | w /= "list" -> Just ("envs " <> w, "envs list " <> w)
        _ -> Nothing
  _ -> Nothing
  where
    -- The options before the command name; after it, @--list@ is
    -- the program's own argument (spec 11.8).
    cmdOptions ws = case ws of
      o : _ : more | o `elem` ["--module", "--format", "--trace-id"] -> cmdOptions more
      o : more | take 1 o == "-" -> o : cmdOptions more
      _ -> []
    -- The old @envs@ took the common options before its function.
    dropOptions ws = case ws of
      o : _ : more | o `elem` ["--module", "--format", "--trace-id"] -> dropOptions more
      o : more | take 1 o == "-" -> dropOptions more
      _ -> ws

pSecretsCommand :: Parser RootCommand
pSecretsCommand =
  hsubparser
    ( command
        "list"
        ( info
            (CmdSecretsList <$> pSecretsOpts (pure False))
            (progDesc "List the secret references in the environment, without reaching any store")
        )
        <> command
          "check"
          ( info
              ( CmdSecretsCheck
                  <$> pSecretsOpts (switch (long "read" <> help "Also read each value (issues a dynamic secret, then revokes it)"))
              )
              (progDesc "Check that each store can be reached, logged in to and read from")
          )
    )
  where
    pSecretsOpts pRead =
      SecretsOpts
        <$> pCommon
        <*> optional (T.pack <$> argument str (metavar "FUNCTION"))
        <*> pRead

pDepsCommand :: Parser RootCommand
pDepsCommand =
  hsubparser
    ( command
        "list"
        ( info
            (CmdDepsList <$> pCommon)
            (progDesc "List the dependencies, what lask.json requests and the lock pins, and whether each is in use")
        )
        <> command
          "graph"
          ( info
              ( CmdDepsGraph
                  <$> pCommon
                  <*> optional (T.pack <$> argument str (metavar "NAME" <> help "Show only the paths that reach this dependency"))
                  <*> optional (option auto (long "depth" <> metavar "N" <> help "Show N levels of dependencies"))
              )
              (progDesc "Show the dependency graph the lock records")
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
          "rm"
          ( info
              (CmdDepsRm <$> pCommon <*> (T.pack <$> argument str (metavar "NAME")))
              (progDesc "Remove a dependency no module imports, with what only it needed")
          )
    )
  where
    pAddSource =
      ( AddGit
          <$> (T.pack <$> strOption (long "git" <> metavar "URL" <> help "Git repository URL"))
          <*> (T.pack <$> strOption (long "rev" <> metavar "REV" <> help "Tag or commit"))
      )
        <|> (AddUrl . T.pack <$> strOption (long "url" <> metavar "URL" <> help "Archive or single .lask file URL"))
