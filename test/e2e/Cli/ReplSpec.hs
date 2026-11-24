{-# LANGUAGE OverloadedStrings #-}

-- | @lask repl@ driven through its standard input (spec 11.9): what an
-- expression, a declaration and a session command each do, and how a
-- session ends. Reloading is covered in process by
-- "Language.Lask.ReplSpec"; here once, with the module edited while the
-- session runs.
module Cli.ReplSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Hspec

program :: String
program = "size(): Number = 3\n"

-- | The session given these lines, and what it wrote to each stream.
session :: FilePath -> [String] -> IO Result
session lask input =
  withProject [("main.lask", program)] $ \dir -> runLask lask dir ["repl"] (unlines input)

-- | The values the session printed, one per input that has one: the
-- prompt is taken off each line of stdout.
printed :: Result -> [String]
printed r = filter (not . null) (concatMap (splitOn "lask> ") (drop 1 (lines (resOut r))))
  where
    splitOn sep s = case breakOn sep s of
      (a, Nothing) -> [a]
      (a, Just rest) -> a : splitOn sep rest
    breakOn sep s
      | take (length sep) s == sep = ("", Just (drop (length sep) s))
      | otherwise = case s of
          [] -> ("", Nothing)
          c : cs -> let (a, r') = breakOn sep cs in (c : a, r')

-- | Everything the session wrote, both streams.
said :: Result -> String
said r = resOut r <> resErr r

spec :: Spec
spec = beforeAll findLask $ describe "repl (spec 11.9)" $ do
  it "prints the value of an expression and keeps a declaration" $ \lask -> do
    r <- session lask ["size() + 1", "double(n: Number): Number = n * 2", "double(size())"]
    resExit r `shouldBe` 0
    printed r `shouldBe` ["4", "6"]
  it "reports a declaration that does not compile, and leaves the session as it was" $ \lask -> do
    r <- session lask ["bad(): Number = \"s\"", "bad()", "bad(): Number = 7", "bad()"]
    said r `shouldSatisfy` isInfixOf "E-TYPE-MISMATCH"
    said r `shouldSatisfy` isInfixOf "E-NAME-UNDEFINED"
    printed r `shouldSatisfy` elem "7"
  it "reports a declaration of a name the module has, and keeps the module's" $ \lask -> do
    r <- session lask ["size(): Number = 9", "size()"]
    said r `shouldSatisfy` isInfixOf "E-NAME-DUPLICATE"
    printed r `shouldSatisfy` elem "3"
  it "reports an unknown session command as such, never as code" $ \lask -> do
    r <- session lask [":foo", "size()"]
    said r `shouldSatisfy` isInfixOf "unknown command ':foo'"
    said r `shouldNotSatisfy` isInfixOf "E-SYNTAX"
    printed r `shouldSatisfy` elem "3"
  it "lists its commands at :help" $ \lask -> do
    r <- session lask [":help", "size()"]
    said r `shouldSatisfy` isInfixOf ":reload, :r"
    printed r `shouldSatisfy` elem "3"
  it "places an error in what was typed by the input, not the module" $ \lask -> do
    r <- session lask ["twice(n: Number): Number = n + \"x\"", "size() + \"y\""]
    said r `shouldSatisfy` isInfixOf "<repl>:1:32-1:35: E-TYPE-MISMATCH"
    said r `shouldSatisfy` isInfixOf "<repl>:1:10-1:13: E-TYPE-MISMATCH"
    said r `shouldNotSatisfy` isInfixOf "main.lask:"
  it "refuses arguments to a session command, and goes on" $ \lask -> do
    r <- session lask [":q now", "size()"]
    said r `shouldSatisfy` isInfixOf "takes no arguments"
    printed r `shouldSatisfy` elem "3"
  it "ends the session at :quit, :q and :exit, reading nothing after it" $ \lask ->
    mapM_
      ( \quit -> do
          r <- session lask [quit, "size() + 100"]
          resExit r `shouldBe` 0
          said r `shouldNotSatisfy` isInfixOf "103"
      )
      [":quit", ":q", ":exit"]
  it "ends the session at the end of its input, exiting 0" $ \lask -> do
    r <- session lask []
    resExit r `shouldBe` 0
  it "runs a command, logging it on stderr" $ \lask -> do
    r <- session lask ["$[#local] echo hi"]
    printed r `shouldSatisfy` elem "\"hi\\n\""
    resErr r `shouldSatisfy` isInfixOf "] $ echo hi"
  it "picks up the module as edited at :reload, re-applying what was typed" $ \lask ->
    withProject [("main.lask", program)] $ \dir -> do
      let feed =
            "{ printf 'double(n: Number): Number = n * 2\\ndouble(size())\\n'; sleep 1; "
              <> "echo 'size(): Number = 5' > main.lask; printf ':r\\ndouble(size())\\n'; } | '"
              <> lask
              <> "' repl"
      (_, out, err) <- readCreateProcessWithExitCode ((shell feed) {cwd = Just dir}) ""
      let r = Result 0 out err
      out `shouldSatisfy` isInfixOf "1 REPL declaration re-applied"
      printed r `shouldSatisfy` (\ps -> "6" `elem` ps && "10" `elem` ps)
  it "gives no stdin: a reference typed at the prompt is an undefined name (spec 9.3)" $ \lask -> do
    r <- session lask ["length(stdin)", "echo(): String = stdin", "echo()", "size()"]
    said r `shouldSatisfy` isInfixOf "E-NAME-UNDEFINED"
    printed r `shouldSatisfy` notElem "0"
    printed r `shouldSatisfy` elem "3"
  it "loads a module that names stdin, which reads the empty string there (spec 9.3)" $ \lask ->
    withProject [("main.lask", "lines(): Number = length(stdin)\n")] $ \dir -> do
      r <- runLask lask dir ["repl"] (unlines ["lines() + 1"])
      said r `shouldNotSatisfy` isInfixOf "E-NAME-UNDEFINED"
      printed r `shouldBe` ["1"]
  it "names a number it did not expect as it was typed" $ \lask -> do
    r <- session lask ["42 +"]
    said r `shouldSatisfy` isInfixOf "E-SYNTAX-UNEXPECTED-TOKEN"
    said r `shouldNotSatisfy` isInfixOf "42.0"
