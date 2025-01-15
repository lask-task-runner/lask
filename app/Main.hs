module Main (main) where

import Command.Lask.Entry (runRootCommand)
import Command.Lask.Parser (pRootCommand)
import Control.Monad (join)
import Options.Applicative

main :: IO ()
main = join $ execParser (info (runRootCommand <$> pRootCommand <**> helper) idm)
