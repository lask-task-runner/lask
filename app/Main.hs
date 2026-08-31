module Main (main) where

import Command.Lask.Entry (runRootCommand)
import Command.Lask.Options (pRootCommand, protectArgSeparator)
import Options.Applicative
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cmd <-
    handleParseResult $
      execParserPure defaultPrefs (info (pRootCommand <**> helper) idm) (protectArgSeparator args)
  runRootCommand cmd
