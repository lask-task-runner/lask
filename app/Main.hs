module Main (main) where

import Command.Lask.Complete (runComplete)
import Command.Lask.Entry (runRootCommand)
import Command.Lask.Options (pRootCommand, protectArgSeparator, retiredCommand)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- The completion protocol (spec 11.7) is answered before the
    -- parser runs: it has its own contract — always exit 0, never
    -- write to stderr — which an option parsing failure would break.
    ("__complete" : rest) -> runComplete (dropRequestSeparator rest)
    _ | Just (old, new) <- retiredCommand args -> do
      hPutStrLn stderr ("E-CLI-USAGE: 'lask " <> old <> "' was replaced by 'lask " <> new <> "'")
      exitWith (ExitFailure 4)
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
