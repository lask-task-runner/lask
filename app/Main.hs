module Main (main) where

import Command.Lask.Entry (runRootCommand)
import Command.Lask.Options (pRootCommand)
import Options.Applicative

main :: IO ()
main = runRootCommand =<< execParser (info (pRootCommand <**> helper) idm)
