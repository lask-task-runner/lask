module Command.Lask.Parser.CommandLine
  ( pRootCommand,
    RootCommand (..),
  )
where

import Data.Text (Text)
import Options.Applicative

data RootCommand
  = Serve
  | Check String
  | Run String String [Text]
  | Eval String String [Text]
  | Infer String String
  deriving (Show, Eq)

pRootCommand :: Parser RootCommand
pRootCommand =
  subparser
    ( command "serve" (info (pure Serve) (progDesc "Runs lask language server."))
        <> command
          "check"
          ( info
              (Check <$> pFileOption)
              (progDesc "Validates lask file")
          )
        <> command
          "run"
          ( info
              ( Run
                  <$> pFileOption
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Runs the specific function." <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              ( Eval
                  <$> pFileOption
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Evaluates the specific function and prints the return value to stdout." <> noIntersperse)
          )
        <> command
          "infer"
          ( info
              (Infer <$> pFileOption <*> argument str (metavar "FUNCTION"))
              (progDesc "Infers the type of the specific function.")
          )
    )

pFileOption :: Parser String
pFileOption =
  strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> value "main.lask"
    )
