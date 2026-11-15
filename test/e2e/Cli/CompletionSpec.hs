{-# LANGUAGE OverloadedStrings #-}

-- | Shell completion through the binary (spec 11.7).
module Cli.CompletionSpec (spec) where

import Command.Lask.Complete (Opt (..), Plan (..), classify)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Text as T
import Support.Harness
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  describe "shell completion (spec 11.7)" $ do
    let src =
          "// Build it.\n\
          \build(target: String, --out_dir: String = \"dist\") = $ echo #{target}\n\
          \// @hidden\n\
          \scratch() = $ echo x\n"

    it "completes the module's functions, and only the callable ones" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["__complete", "--", "run", ""] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "build\tBuild it.\n:4\n"

    it "hands the function's own parameters over after the function name (spec 11.2)" $ \lask ->
      withProject [("main.lask", src)] $ \dir -> do
        r <- runLask lask dir ["__complete", "--", "run", "build", "--"] ""
        resOut r `shouldContain` "--out-dir"
        resOut r `shouldNotContain` "--module"

    -- The contract that lets a script call this on every keystroke.
    it "always exits 0 and stays silent, whatever the module is in" $ \lask -> do
      let requests =
            [ ["__complete", "--", "run", ""],
              ["__complete", "--", "check", "--module", ""],
              ["__complete", "--"],
              ["__complete"],
              ["__complete", "--", "no-such-command", "--", "-"]
            ]
          projects =
            [ [("main.lask", src)],
              [("main.lask", "build( = oops\n")],
              [("other.lask", "x() = 1\n")]
            ]
      sequence_
        [ withProject files $ \dir -> do
            r <- runLask lask dir args ""
            (args, resExit r) `shouldBe` (args, 0)
            (args, resErr r) `shouldBe` (args, "")
        | files <- projects,
          args <- requests
        ]

    it "prints a script for each shell" $ \lask ->
      withProject [] $ \dir ->
        sequence_
          [ do
              r <- runLask lask dir ["completion", shell] ""
              resExit r `shouldBe` 0
              resOut r `shouldContain` "__complete"
          | shell <- ["bash", "zsh", "fish"]
          ]

    it "rejects a shell it has no script for" $ \lask ->
      withProject [] $ \dir -> do
        r <- runLask lask dir ["completion", "tcsh"] ""
        -- An argument the command line does not accept is a usage error
        -- (spec 11.3).
        resExit r `shouldBe` 4
        resErr r `shouldSatisfy` isInfixOf "E-CLI-USAGE"

    -- The completion grammar is written by hand beside the parser, so
    -- it can drift from it. optparse-applicative's built-in completer
    -- is wrong about context (it does not know spec 11.2's boundary
    -- rule) but authoritative about which options a parser has, which
    -- is exactly the part that drifts.
    it "offers the same options the parser accepts" $ \lask ->
      withProject [] $ \dir ->
        sequence_
          [ do
              let request =
                    -- One past the last word: the position after the
                    -- subcommand, where its options are offered.
                    ["--bash-completion-index", show (length path + 1)]
                      <> concat [["--bash-completion-word", w] | w <- "lask" : path]
              r <- runLask lask dir request ""
              let fromParser = sort (nub (filter ("--" `isPrefixOf`) (lines (resOut r))))
              (path, fromGrammar path) `shouldBe` (path, fromParser)
          | path <-
              [ ["serve"],
                ["check"],
                ["fmt"],
                ["run"],
                ["eval"],
                ["repl"],
                ["envs", "list"],
                ["sync"],
                ["version"],
                ["completion"],
                ["deps", "list"],
                ["deps", "graph"],
                ["deps", "add"],
                ["deps", "rm"],
                ["cmd"]
              ]
          ]

-- | The long options the completion grammar offers for a subcommand.
fromGrammar :: [String] -> [String]
fromGrammar path =
  case classify (map T.pack path <> ["--"]) of
    POptions opts _ _ -> sort (nub ["--" <> T.unpack (optLong o) | o <- opts])
    _ -> []
