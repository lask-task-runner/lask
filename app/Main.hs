module Main (main) where

import Command.Lask.Complete (runComplete)
import Command.Lask.Entry (runRootCommand)
import Command.Lask.Options (pRootCommand, protectArgSeparator)
import Options.Applicative
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- The completion protocol (spec 11.7) is answered before the
    -- parser runs: it has its own contract — always exit 0, never
    -- write to stderr — which an option parsing failure would break.
    ("__complete" : rest) -> runComplete (dropRequestSeparator rest)
    _ -> runCli args

dropRequestSeparator :: [String] -> [String]
dropRequestSeparator args = case break (== "--") args of
  (_, _ : after) -> after
  _ -> args

runCli :: [String] -> IO ()
runCli args = do
  cmd <-
    handleParseResult $
      execParserPure defaultPrefs (info (pRootCommand <**> helper) idm) (protectArgSeparator args)
  runRootCommand cmd
