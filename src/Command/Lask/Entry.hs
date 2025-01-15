module Command.Lask.Entry
  ( runRootCommand,
  )
where

import Command.Lask.Parser (RootCommand (..))
import Control.Monad (void)
import Language.LSP.Lask (serve)

runRootCommand :: RootCommand -> IO ()
runRootCommand cmd = case cmd of
  Serve -> void serve
