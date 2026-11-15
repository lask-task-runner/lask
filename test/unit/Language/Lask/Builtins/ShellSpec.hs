{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Builtins.ShellSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Shell
import System.Process (readProcess)
import Test.Hspec

-- | What @sh@ prints for @printf %s <word>@.
viaShell :: Text -> IO Text
viaShell word = T.pack <$> readProcess "sh" ["-c", "printf %s " <> T.unpack word] ""

-- | Values with every character the shell treats specially.
nasty :: [Text]
nasty =
  [ "",
    "plain",
    "a b",
    "it's",
    "say \"hi\"",
    "$HOME $(echo pwned) `echo pwned`",
    "back\\slash\\",
    "semi; rm -rf / && echo | cat > x",
    "*?[a]~#!{a,b}",
    "line\nbreak\ttab",
    "'\"'\"\\'"
  ]

spec :: Spec
spec = describe "Language.Lask.Builtins.Shell (spec 6.6, 15.5)" $ do
  it "quotes a value outside quotes as one word that expands to it" $
    mapM_ (\v -> viaShell (shellEscape QuoteNone v) `shouldReturn` v) nasty
  it "escapes a value inside single quotes so that they expand to it" $
    mapM_ (\v -> viaShell ("'" <> shellEscape QuoteSingle v <> "'") `shouldReturn` v) nasty
  it "escapes a value inside double quotes so that they expand to it" $
    mapM_ (\v -> viaShell ("\"" <> shellEscape QuoteDouble v <> "\"") `shouldReturn` v) nasty
  it "names a distinct internal builtin for each quoting" $
    map fst shellEscapeBuiltins `shouldBe` ["%shellWord", "%shellSingle", "%shellDouble"]
