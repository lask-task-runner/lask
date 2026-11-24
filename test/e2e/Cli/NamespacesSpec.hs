{-# LANGUAGE OverloadedStrings #-}

-- | The function path on the command line (spec 11.2): a function a
-- namespace the entry module re-exports holds is named by its
-- namespaces and its name, as words or joined by @.@, wherever the CLI
-- takes a function — @run@ \/ @eval@, their help (11.6), @envs@ (11.4),
-- @secrets@ (11.10) and completion (11.7).
module Cli.NamespacesSpec (spec) where

import Data.List (isInfixOf)
import Support.Harness
import Test.Hspec

project :: [(FilePath, String)]
project =
  [ ( "main.lask",
      unlines
        [ "export * as db from \"./db.lask\"",
          "// Build it.",
          "build(): String = \"build\""
        ]
    ),
    ( "db.lask",
      unlines
        [ "export * as tools from \"./tools.lask\"",
          "// Apply pending migrations.",
          "migrate(--dry_run: Bool = false): String = dry_run ? \"dry\" : \"migrated\"",
          "// Load seed data.",
          "seed(): String = \"seeded\"",
          "version = \"1\""
        ]
    ),
    ( "tools.lask",
      unlines
        [ "// Run the tool.",
          "run_it(name: String): String = \"tool #{name}\""
        ]
    )
  ]

lask' :: FilePath -> [String] -> IO Result
lask' lask args = withProject project $ \dir -> runLask lask dir args ""

spec :: Spec
spec = beforeAll findLask $ do
  describe "a function in a namespace (spec 11.2)" $ do
    it "is named by its namespace and its name, as words" $ \lask -> do
      r <- lask' lask ["eval", "--output", "text", "db", "migrate", "--dry-run"]
      (resExit r, resOut r) `shouldBe` (0, "dry\n")

    it "is still named by its qualified name in one word" $ \lask -> do
      r <- lask' lask ["eval", "--output", "text", "db.migrate"]
      (resExit r, resOut r) `shouldBe` (0, "migrated\n")

    it "is named through any number of namespaces, in either form or both" $ \lask ->
      mapM_
        ( \path -> do
            r <- lask' lask (["eval", "--output", "text"] <> path <> ["x"])
            (path, resExit r, resOut r) `shouldBe` (path, 0, "tool x\n")
        )
        [ ["db", "tools", "run-it"],
          ["db.tools", "run-it"],
          ["db", "tools.run-it"],
          ["db.tools.run-it"]
        ]

    it "takes the words after the name as its arguments" $ \lask -> do
      -- `seed` is a name in `db`, but after the function it is an argument.
      r <- lask' lask ["eval", "--output", "text", "db", "tools", "run-it", "seed"]
      (resExit r, resOut r) `shouldBe` (0, "tool seed\n")

    it "is a usage error that lists the group when the path stops at a namespace" $ \lask -> do
      r <- lask' lask ["run", "db"]
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` isInfixOf "'db' is a group of functions: name one of them (migrate, seed, tools)"
      r' <- lask' lask ["run", "db", "tools"]
      resErr r' `shouldSatisfy` isInfixOf "'db tools' is a group of functions: name one of them (run-it)"

    it "suggests the function path as words" $ \lask -> do
      r <- lask' lask ["run", "db", "migrat"]
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` isInfixOf "no such function: 'db migrat'"
      resErr r `shouldSatisfy` isInfixOf "did you mean 'db migrate'?"

    it "says a value in a namespace is not a function" $ \lask -> do
      r <- lask' lask ["run", "db", "version"]
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` isInfixOf "'db version' is a value, not a function"

  describe "the help (spec 11.6)" $ do
    it "lists the functions of a namespace, as words" $ \lask -> do
      r <- lask' lask ["run", "db", "--help"]
      resExit r `shouldBe` 0
      lines (resOut r)
        `shouldBe` [ "Functions in db:",
                     "  db migrate       Apply pending migrations.",
                     "  db seed          Load seed data.",
                     "  db tools run-it  Run the tool."
                   ]

    it "describes a function in a namespace, with its path as words" $ \lask -> do
      r <- lask' lask ["run", "db", "migrate", "--help"]
      resExit r `shouldBe` 0
      lines (resOut r) `shouldContain` ["db migrate - Apply pending migrations."]
      lines (resOut r) `shouldContain` ["  lask run db migrate [--dry-run]"]

    it "lists them among the module's functions" $ \lask -> do
      r <- lask' lask ["run", "--help"]
      lines (resOut r) `shouldContain` ["  db tools run-it  Run the tool."]

    it "keeps the name as declared in JSON" $ \lask -> do
      r <- lask' lask ["run", "--output", "json", "db", "migrate", "--help"]
      resOut r `shouldSatisfy` isInfixOf "\"function\":\"db.migrate\""

  describe "envs and secrets (spec 11.4, 11.10)" $ do
    it "take the function path as words" $ \lask -> do
      r <- lask' lask ["envs", "list", "db", "migrate"]
      resExit r `shouldBe` 0
      r' <- lask' lask ["secrets", "list", "db", "tools", "run-it"]
      resExit r' `shouldBe` 0

    it "refuse a word after the function" $ \lask -> do
      r <- lask' lask ["envs", "list", "db", "migrate", "extra"]
      resExit r `shouldBe` 4
      resErr r `shouldSatisfy` isInfixOf "unexpected argument 'extra'"

  describe "completion (spec 11.7)" $ do
    let complete args = \lask -> withProject project $ \dir -> resOut <$> runLask lask dir (["__complete", "--"] <> args) ""
    it "offers a namespace among the functions, then the names in it" $ \lask -> do
      out <- complete ["run", ""] lask
      lines out `shouldBe` ["build\tBuild it.", "db", ":4"]
      out' <- complete ["run", "db", ""] lask
      lines out' `shouldBe` ["migrate\tApply pending migrations.", "seed\tLoad seed data.", "tools", ":4"]
      out'' <- complete ["envs", "list", "db", "tools", ""] lask
      lines out'' `shouldBe` ["run-it\tRun the tool.", ":4"]

    it "offers the qualified names once a . is typed" $ \lask -> do
      out <- complete ["run", "db.t"] lask
      lines out `shouldBe` ["db.tools.run-it\tRun the tool.", ":4"]

    it "offers the parameters of the function the path names" $ \lask -> do
      out <- complete ["run", "db", "migrate", "--"] lask
      out `shouldSatisfy` isInfixOf "--dry-run"
