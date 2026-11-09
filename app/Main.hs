module Main (main) where

import Command.Lask.Complete (runComplete)
import Command.Lask.Entry (runRootCommand)
import Command.Lask.Options (pRootCommand, protectArgSeparator)
import Control.Exception (finally)
import Language.Lask.Runtime.Egress (removeEgress)
import Language.Lask.Runtime.Reaper (enableReaper, reaperMain, releaseReaper)
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
    -- The reaper an invocation starts before its first container
    -- (spec 11.12). It is lask itself, so that it needs nothing else
    -- installed.
    ("__reap" : rest) -> reaperMain rest
    _ -> runCli args

dropRequestSeparator :: [String] -> [String]
dropRequestSeparator args = case break (== "--") args of
  (_, _ : after) -> after
  _ -> args

runCli :: [String] -> IO ()
runCli args = do
  enableReaper
  cmd <-
    handleParseResult $
      execParserPure defaultPrefs (info (pRootCommand <**> helper) idm) (protectArgSeparator args)
  -- The networks and proxies of egress live as long as the invocation
  -- (spec 10.12), however it ends. An invocation that gets here has
  -- removed what it started, so the reaper is let go (11.12).
  runRootCommand cmd `finally` (removeEgress >> releaseReaper)
