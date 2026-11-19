{-# LANGUAGE OverloadedStrings #-}

-- | lask check and lask fmt (spec 11.1, 11.11).
module Cli.CheckFmtSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import Support.Harness
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  describe "check (spec 11.1)" $ do
    it "check reports validity" $ \lask ->
      withProject [("main.lask", "a = 1\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        r `shouldBe` Result 0 "the module is valid\n" ""
    it "check reports advisories and still exits 0 (spec 14.2)" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  async \"x\"\n  \"done\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        r
          `shouldBe` Result
            0
            "main.lask:2:3-2:12: W-ASYNC-UNUSED [static]: this statement discards an async handle, so it is never awaited\nthe module is valid\n"
            ""
        j <- runLask lask dir ["check", "--output", "json"] ""
        resExit j `shouldBe` 0
        mapM_ (resOut j `shouldContain`) ["\"code\":\"W-ASYNC-UNUSED\"", "\"severity\":\"warning\"", "\"stage\":\"static\""]
    it "check reports diagnostics as JSON with --output json" $ \lask ->
      withProject [("main.lask", "x: Number = \"s\"\n")] $ \dir -> do
        r <- runLask lask dir ["check", "--output", "json"] ""
        resExit r `shouldBe` 1
        resOut r `shouldSatisfy` isInfixOf "E-TYPE-MISMATCH"
    it "check shows at most 50 errors, then how many more there are (spec 14.3)" $ \lask -> do
      let src = concat ["f" <> show i <> "(): Number = \"s\"\n" | i <- [1 .. 53 :: Int]]
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resExit r `shouldBe` 1
        length (filter (isInfixOf "E-TYPE-MISMATCH") (lines (resOut r))) `shouldBe` 50
        last (lines (resOut r)) `shouldBe` "... and 3 more errors"
        j <- runLask lask dir ["check", "--output", "json"] ""
        T.count "\"code\":\"E-TYPE-MISMATCH\"" (T.pack (resOut j)) `shouldBe` 53
    it "check suggests a correction for a misspelt name (spec 14.3)" $ \lask ->
      withProject [("main.lask", "hello = \"hi\"\nmain() = helo\n")] $ \dir -> do
        r <- runLask lask dir ["check"] ""
        resExit r `shouldBe` 1
        resOut r `shouldContain` "undefined name: 'helo'\n  note: did you mean 'hello'?"
        j <- runLask lask dir ["check", "--output", "json"] ""
        resOut j `shouldContain` "\"suggestions\":[\"hello\"]"
    it "check keeps notes in JSON (spec 14.3)" $ \lask ->
      withProject [("main.lask", "import { a } from \"./missing.lask\"\nb = a\n")] $ \dir -> do
        j <- runLask lask dir ["check", "--output", "json"] ""
        resExit j `shouldBe` 1
        resOut j `shouldContain` "\"notes\":["

  describe "fmt (spec 11.11)" $ do
    let messy = "a  =  1\nb = [1,2]\n"
        tidy = "a = 1\nb = [1, 2]\n"
        project =
          [ ("main.lask", messy),
            ("lib/util.lask", messy),
            ("ok.lask", tidy),
            (".lask/deps/x/main.lask", messy),
            ("notes.txt", messy)
          ]
    it "formats every .lask file of the project in place, and lists those it changed" $ \lask ->
      withProject project $ \dir -> do
        r <- runLask lask dir ["fmt"] ""
        r `shouldBe` Result 0 "lib/util.lask\nmain.lask\n" ""
        mapM_ (\f -> readFile (dir </> f) `shouldReturn` tidy) ["main.lask", "lib/util.lask", "ok.lask"]
        -- The dependency cache and other files are not the project's
        -- sources.
        readFile (dir </> ".lask/deps/x/main.lask") `shouldReturn` messy
        readFile (dir </> "notes.txt") `shouldReturn` messy
        again <- runLask lask dir ["fmt"] ""
        again `shouldBe` Result 0 "" ""
    it "--check lists the files that are not formatted, changes none, and exits 1" $ \lask ->
      withProject project $ \dir -> do
        r <- runLask lask dir ["fmt", "--check", "--output", "json"] ""
        r `shouldBe` Result 1 "[\"lib/util.lask\",\"main.lask\"]\n" ""
        readFile (dir </> "main.lask") `shouldReturn` messy
        clean <- runLask lask dir ["fmt", "--check", "ok.lask"] ""
        clean `shouldBe` Result 0 "" ""
    it "formats the files and directories it is given" $ \lask ->
      withProject project $ \dir -> do
        r <- runLask lask dir ["fmt", "lib"] ""
        r `shouldBe` Result 0 "lib/util.lask\n" ""
        readFile (dir </> "main.lask") `shouldReturn` messy
        missing <- runLask lask dir ["fmt", "nope.lask"] ""
        resExit missing `shouldBe` 4
    it "--stdin formats standard input to standard output" $ \lask ->
      withProject [] $ \dir -> do
        r <- runLask lask dir ["fmt", "--stdin"] messy
        r `shouldBe` Result 0 tidy ""
        c <- runLask lask dir ["fmt", "--stdin", "--check"] messy
        resExit c `shouldBe` 1
    it "leaves a file with a syntax error as it is, reports it and exits 1" $ \lask ->
      withProject [("main.lask", "a = \n"), ("b.lask", messy)] $ \dir -> do
        r <- runLask lask dir ["fmt"] ""
        resExit r `shouldBe` 1
        resOut r `shouldBe` "b.lask\n"
        resErr r `shouldContain` "main.lask:"
        resErr r `shouldContain` "E-SYNTAX-UNEXPECTED-TOKEN"
        readFile (dir </> "main.lask") `shouldReturn` "a = \n"
