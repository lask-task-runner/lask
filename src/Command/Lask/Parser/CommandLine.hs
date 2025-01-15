module Command.Lask.Parser.CommandLine
  ( pRootCommand,
    RootCommand (..),
  )
where

import Options.Applicative

data RootCommand
  = Serve
  deriving (Show, Eq)

pRootCommand :: Parser RootCommand
pRootCommand =
  subparser
    ( command "serve" (info (pure Serve) (progDesc "Runs lask language server."))
    )
