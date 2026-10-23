{-# LANGUAGE OverloadedStrings #-}

-- | Escaping of the holes of a command string (spec 6.6) and
-- @shell_quote@ (15.5).
module Language.Lask.Builtins.Shell
  ( ShellQuoting (..),
    escapeCommand,
    shellEscape,
    shellEscapeBuiltin,
    shellEscapeBuiltins,
    shellQuote,
    rawBuiltin,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.AST

-- | Escape every hole of a command string (spec 6.6) for the shell
-- quoting it sits in, so that its value reaches the command as text:
-- one word outside quotes, the inside of the quotes it is written in.
-- A hole written @#!{e}@, which the elaborator marks with 'rawBuiltin',
-- is substituted as it is.
--
-- The quoting is that of the literal text alone (backslash, @'...'@
-- and @"..."@); a hole in @$(...)@ or backquotes counts as being where
-- the substitution is.
escapeCommand :: Core -> Core
escapeCommand (Core sp (CStr ps)) = Core sp (CStr (go QuoteNone ps))
  where
    go _ [] = []
    go q (CPText t : rest) = CPText t : go (scan q (T.unpack t)) rest
    go q (CPExpr c : rest) = CPExpr (escapeHole q c) : go q rest
    scan q [] = q
    scan QuoteNone ('\\' : _ : cs) = scan QuoteNone cs
    scan QuoteNone ('\'' : cs) = scan QuoteSingle cs
    scan QuoteNone ('"' : cs) = scan QuoteDouble cs
    scan QuoteSingle ('\'' : cs) = scan QuoteNone cs
    scan QuoteDouble ('\\' : _ : cs) = scan QuoteDouble cs
    scan QuoteDouble ('"' : cs) = scan QuoteNone cs
    scan q (_ : cs) = scan q cs
escapeCommand c = c

escapeHole :: ShellQuoting -> Core -> Core
escapeHole q c = case coreF c of
  CApp (Core _ (CVar (BuiltinRef n))) _ _ | n == rawBuiltin -> c
  _ -> Core sp (CApp (Core sp (CVar (BuiltinRef (shellEscapeBuiltin q)))) [Core sp (CStr [CPExpr c])] [])
  where
    sp = coreSpan c

-- | The internal builtin that marks a hole written @#!{e}@ (spec 6.6):
-- the identity on its string, which escaping leaves alone. No program
-- can write its name.
rawBuiltin :: Text
rawBuiltin = "%raw"

-- | The shell quoting a hole of a command string sits in.
data ShellQuoting = QuoteNone | QuoteSingle | QuoteDouble
  deriving (Show, Eq, Enum, Bounded)

-- | The text that expands to exactly the value where the hole is: one
-- word outside quotes, the inside of the quotes otherwise.
shellEscape :: ShellQuoting -> Text -> Text
shellEscape QuoteNone = shellQuote
shellEscape QuoteSingle = T.replace "'" "'\\''"
shellEscape QuoteDouble = T.concatMap (\c -> if c `elem` ("\\\"$`" :: String) then T.pack ['\\', c] else T.singleton c)

-- | One POSIX shell word that expands to exactly the value.
shellQuote :: Text -> Text
shellQuote v = "'" <> shellEscape QuoteSingle v <> "'"

-- | The internal builtin the elaborator applies to a hole in this
-- quoting. No program can write its name.
shellEscapeBuiltin :: ShellQuoting -> Text
shellEscapeBuiltin q = case q of
  QuoteNone -> "%shellWord"
  QuoteSingle -> "%shellSingle"
  QuoteDouble -> "%shellDouble"

-- | Every internal escaping builtin, with the quoting it escapes for.
shellEscapeBuiltins :: [(Text, ShellQuoting)]
shellEscapeBuiltins = [(shellEscapeBuiltin q, q) | q <- [minBound .. maxBound]]
