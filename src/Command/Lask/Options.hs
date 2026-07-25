{-# LANGUAGE OverloadedStrings #-}

-- | CLI option definitions (spec 11.1).
module Command.Lask.Options
  ( RootCommand (..),
    CommonOpts (..),
    RunOpts (..),
    EnvsOpts (..),
    pRootCommand,
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
    runEnvFile :: Maybe FilePath,
    runSshKnownHosts :: Maybe FilePath,
    runSshStrictHostKey :: Maybe Text,
    runSshConnectTimeout :: Maybe Int,
    runFunction :: Text,
    runArgs :: [Text]
  }

data EnvsOpts = EnvsOpts
  { envsCommon :: CommonOpts,
    envsEnvFile :: Maybe FilePath,
    envsFunction :: Maybe Text,
    envsCheck :: Bool,
    envsSshKnownHosts :: Maybe FilePath,
    envsSshStrictHostKey :: Maybe Text,
    envsSshConnectTimeout :: Maybe Int
  }

data RootCommand
  = CmdServe
  | CmdCheck CommonOpts
  | CmdRun RunOpts
  | CmdEval RunOpts
  | CmdInfer CommonOpts (Maybe Text)
  | CmdRepl CommonOpts
  | CmdEnvs EnvsOpts

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
    <*> optional (strOption (long "env-file" <> metavar "PATH" <> help "Environment definition file"))
    <*> optional (strOption (long "ssh-known-hosts" <> metavar "PATH"))
    <*> optional (T.pack <$> strOption (long "ssh-strict-host-key-checking" <> metavar "yes|accept-new|no"))
    <*> optional (option auto (long "ssh-connect-timeout" <> metavar "SECONDS"))
    <*> (T.pack <$> argument str (metavar "FUNCTION"))
    <*> many (T.pack <$> argument str (metavar "ARGS..."))

pEnvsOpts :: Parser EnvsOpts
pEnvsOpts =
  EnvsOpts
    <$> pCommon
    <*> optional (strOption (long "env-file" <> metavar "PATH" <> help "Environment definition file"))
    <*> optional (T.pack <$> argument str (metavar "FUNCTION"))
    <*> switch (long "check" <> help "Check accessibility of each environment")
    <*> optional (strOption (long "ssh-known-hosts" <> metavar "PATH"))
    <*> optional (T.pack <$> strOption (long "ssh-strict-host-key-checking" <> metavar "yes|accept-new|no"))
    <*> optional (option auto (long "ssh-connect-timeout" <> metavar "SECONDS"))

pRootCommand :: Parser RootCommand
pRootCommand =
  hsubparser
    ( command "serve" (info (pure CmdServe) (progDesc "Start the language server"))
        <> command "check" (info (CmdCheck <$> pCommon) (progDesc "Statically validate the module"))
        <> command
          "run"
          ( info
              (CmdRun <$> pRunOpts)
              (progDesc "Run a function (result is not printed)" <> noIntersperse <> forwardOptions)
          )
        <> command
          "eval"
          ( info
              (CmdEval <$> pRunOpts)
              (progDesc "Run a function and print its result" <> noIntersperse <> forwardOptions)
          )
        <> command
          "infer"
          ( info
              (CmdInfer <$> pCommon <*> optional (T.pack <$> strOption (long "symbol" <> metavar "NAME")))
              (progDesc "Print inferred types")
          )
        <> command "repl" (info (CmdRepl <$> pCommon) (progDesc "Interactive session"))
        <> command "envs" (info (CmdEnvs <$> pEnvsOpts) (progDesc "List and check environments"))
    )
