module Command.Lask.Parser
  ( parseArguments,
    replaceArgumentVar,
    pRootCommand,
    RootCommand (..),
  )
where

import Command.Lask.Parser.Argument
  ( parseArguments,
    replaceArgumentVar,
  )
import Command.Lask.Parser.CommandLine
  ( RootCommand (..),
    pRootCommand,
  )
