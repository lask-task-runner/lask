{-# LANGUAGE OverloadedStrings #-}

-- | @lask fmt@ on a project (spec 11.11): which files it takes, how it
-- lists them, and its exit codes. Formatting itself is covered in
-- process by "Language.Lask.Syntax.PrinterSpec" and
-- "Command.Lask.FmtSpec"; the basic runs by "Cli.CheckFmtSpec".
module Cli.FmtSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import System.Directory (createDirectoryLink, getPermissions, readable, setOwnerReadable, setPermissions)
import System.FilePath ((</>))
import Test.Hspec

-- | A project with a file to format in each kind of place.
project :: [(FilePath, String)]
project =
  [ ("main.lask", "x=1\n"),
    ("sub/s.lask", "y=2\n"),
    (".hidden/h.lask", "z=3\n"),
    ("real/r.lask", "w=4\n")
  ]

spec :: Spec
spec = beforeAll findLask $ describe "fmt (spec 11.11)" $ do
  it "enters neither a directory whose name begins with a dot nor a link to a directory" $ \lask ->
    withProject project $ \dir -> do
      createDirectoryLink "real" (dir </> "link")
      r <- runLask lask dir ["fmt"] ""
      (resExit r, lines (resOut r)) `shouldBe` (0, ["main.lask", "real/r.lask", "sub/s.lask"])
      readFile (dir </> ".hidden/h.lask") `shouldReturn` "z=3\n"
  it "lists the files as a JSON array of paths with --output json" $ \lask ->
    withProject project $ \dir -> do
      r <- runLask lask dir ["fmt", "--check", "--output", "json"] ""
      (resExit r, resOut r) `shouldBe` (1, "[\"main.lask\",\"real/r.lask\",\"sub/s.lask\"]\n")
  it "lists nothing, and exits 0, for a project that is formatted, also after formatting it" $ \lask ->
    withProject project $ \dir -> do
      _ <- runLask lask dir ["fmt"] ""
      runLask lask dir ["fmt", "--check"] "" `shouldReturn` Result 0 "" ""
      j <- runLask lask dir ["fmt", "--output", "json"] ""
      (resExit j, resOut j) `shouldBe` (0, "[]\n")
  it "reports a path that does not exist as a usage error, and formats nothing" $ \lask ->
    withProject project $ \dir -> do
      r <- runLask lask dir ["fmt", "main.lask", "nope.lask"] ""
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` isInfixOf "E-CLI-USAGE"
      readFile (dir </> "main.lask") `shouldReturn` "x=1\n"
  it "takes no path with --stdin" $ \lask ->
    withProject project $ \dir -> do
      r <- runLask lask dir ["fmt", "--stdin", "main.lask"] "x=1\n"
      resExit r `shouldBe` 4
      resOut r `shouldBe` ""
  it "checks standard input with --stdin --check, writing nothing" $ \lask ->
    withProject project $ \dir -> do
      runLask lask dir ["fmt", "--stdin", "--check"] "x = 1\n" `shouldReturn` Result 0 "" ""
      r <- runLask lask dir ["fmt", "--stdin", "--check"] "x=1\n"
      (resExit r, resOut r) `shouldBe` (1, "")
  it "reports a syntax error on standard input, writing nothing to stdout" $ \lask ->
    withProject project $ \dir -> do
      r <- runLask lask dir ["fmt", "--stdin"] "x = (\n"
      (resExit r, resOut r) `shouldBe` (1, "")
      resErr r `shouldSatisfy` isInfixOf "E-SYNTAX"
  it "exits 3 for a file it cannot read, and formats the others" $ \lask ->
    withProject project $ \dir -> do
      writeFile (dir </> "locked.lask") "q=5\n"
      p <- getPermissions (dir </> "locked.lask")
      setPermissions (dir </> "locked.lask") (setOwnerReadable False p)
      stillReadable <- readable <$> getPermissions (dir </> "locked.lask")
      if stillReadable
        then pendingWith "the file stays readable (running as root?)"
        else do
          r <- runLask lask dir ["fmt"] ""
          resExit r `shouldBe` 3
          resErr r `shouldSatisfy` isInfixOf "E-IO-FS"
          readFile (dir </> "main.lask") `shouldReturn` "x = 1\n"
