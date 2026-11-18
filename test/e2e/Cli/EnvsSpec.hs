{-# LANGUAGE OverloadedStrings #-}

-- | The fields of the @envs@ listings as JSON, their ordering and
-- counting lines, and the exit codes of @envs@ (spec 11.4). The
-- container engine is the fake one of "Support.Harness".
--
-- The text tables are compared whole in test/e2e/golden; the statuses
-- of @envs list@ against a daemon are in "Cli.ImagesSpec".
module Cli.EnvsSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Support.Harness
import Test.Hspec

program :: String
program =
  unlines
    [ "alpine = #alpine:3.22.2",
      "deploy(): String = $[#local] echo deploy",
      "build(): String = $[alpine] make build",
      "again(): String = $[#alpine:3.22.2] make again",
      "multi(--image: Environment = #alpine:3.22.2): String = $[image] npm test",
      "show(e: Environment): String = $[e] cat x",
      "both(): String = do {",
      "  deploy()",
      "  build()",
      "}",
      "v = \"1\""
    ]

-- | The objects of a JSON array on stdout.
objects :: Result -> IO [A.Object]
objects r = case A.decode (utf8 (resOut r)) of
  Just (A.Array xs) -> pure [o | A.Object o <- V.toList xs]
  _ -> expectationFailure ("not a JSON array of objects: " <> resOut r) >> pure []

field :: Text -> A.Object -> Maybe A.Value
field k = KM.lookup (K.fromText k)

keysOf :: A.Object -> [Text]
keysOf = sort . map K.toText . KM.keys

envsJson :: FilePath -> [String] -> IO [A.Object]
envsJson lask args =
  withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
    r <- runLaskEnv lask dir extra ("envs" : args <> ["--output", "json"]) ""
    resExit r `shouldBe` 0
    objects r

-- | What a process wrote, as the bytes it wrote.
utf8 :: String -> BL.ByteString
utf8 = BL.fromStrict . TE.encodeUtf8 . T.pack

spec :: Spec
spec = beforeAll findLask $ do
  describe "envs list (spec 11.4)" $ do
    it "gives each environment as an object of environment, kind, pinned, required_by and status" $ \lask -> do
      os <- envsJson lask ["list"]
      map keysOf os `shouldSatisfy` all (== ["environment", "kind", "pinned", "required_by", "status"])
      map (field "environment") os `shouldBe` [Just "#local", Just "#alpine:3.22.2"]
      map (field "kind") os `shouldBe` [Just "local", Just "registry"]
    it "gives every place that requires an environment, in full, where the table shows two" $ \lask ->
      withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
        j <- runLaskEnv lask dir extra ["envs", "list", "--output", "json"] ""
        os <- objects j
        [field "required_by" o | o <- os, field "kind" o == Just "registry"]
          `shouldBe` [Just (A.toJSON ["main.lask: again", "main.lask: alpine", "main.lask: multi (default --image)" :: Text])]
        t <- runLaskEnv lask dir extra ["envs", "list"] ""
        [l | l <- lines (resOut t), "#alpine:3.22.2" `isPrefixOf` l] `shouldSatisfy` all (isInfixOf "1 more")
    it "gives the pinned digest in full, and null before the lock pins it" $ \lask ->
      withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
        unsynced <- runLaskEnv lask dir extra ["envs", "list", "--output", "json"] "" >>= objects
        [field "pinned" o | o <- unsynced, field "kind" o == Just "registry"] `shouldBe` [Just A.Null]
        _ <- runLaskEnv lask dir extra ["sync"] ""
        synced <- runLaskEnv lask dir extra ["envs", "list", "--output", "json"] "" >>= objects
        -- In full: the pinned reference, whose digest the table shortens.
        [p | o <- synced, field "kind" o == Just "registry", Just (A.String p) <- [field "pinned" o]]
          `shouldSatisfy` (\ps -> length ps == 1 && all ("@sha256:aaa" `T.isSuffixOf`) ps)
        [field "status" o | o <- synced] `shouldBe` [Just "ok", Just "present"]
    it "ends the table with a line counting the environments by kind and by status" $ \lask ->
      withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
        r <- runLaskEnv lask dir extra ["envs", "list"] ""
        last (lines (resOut r)) `shouldSatisfy` ("2 environments (1 local, 1 registry): 1 ok, 1 not pinned" `isPrefixOf`)
    it "limits the list to the call graph of the function, by its command-line name" $ \lask -> do
      os <- envsJson lask ["list", "both"]
      map (field "environment") os `shouldBe` [Just "#local", Just "#alpine:3.22.2"]
      [field "required_by" o | o <- os] `shouldBe` [Just (A.toJSON ["main.lask: deploy" :: Text]), Just (A.toJSON ["main.lask: alpine" :: Text])]
    it "exits 4 for a name that is not a function, and 1 for a module that does not compile" $ \lask ->
      withFakeDocker $ \_ extra -> do
        withProject [("main.lask", program)] $ \dir -> do
          (resExit <$> runLaskEnv lask dir extra ["envs", "list", "nope"] "") `shouldReturn` 4
          (resExit <$> runLaskEnv lask dir extra ["envs", "list", "v"] "") `shouldReturn` 4
          (resExit <$> runLaskEnv lask dir extra ["envs", "commands", "nope"] "") `shouldReturn` 4
        withProject [("main.lask", "x: Number = \"s\"\n")] $ \dir ->
          (resExit <$> runLaskEnv lask dir extra ["envs", "list"] "") `shouldReturn` 1

  describe "envs commands (spec 11.4)" $ do
    it "gives each environment as environment, kind, programs and commands, (dynamic) last" $ \lask -> do
      os <- envsJson lask ["commands"]
      map keysOf os `shouldSatisfy` all (== ["commands", "environment", "kind", "programs"])
      map (field "environment") os `shouldBe` [Just "#local", Just "#alpine:3.22.2", Just "(dynamic)"]
      map (field "kind") os `shouldBe` [Just "local", Just "registry", Just "dynamic"]
    it "gives each command its text, programs, place, declaration and the expression it runs in" $ \lask -> do
      os <- envsJson lask ["commands"]
      let commands = [c | o <- os, Just (A.Array cs) <- [field "commands" o], A.Object c <- V.toList cs]
      map keysOf commands `shouldSatisfy` all (== ["command", "declaration", "line", "module", "programs", "via"])
      [(field "declaration" c, field "via" c) | c <- commands, field "command" c == Just "make build"]
        `shouldBe` [(Just "build", Just "alpine")]
      [field "via" c | c <- commands, field "command" c == Just "echo deploy"] `shouldBe` [Just A.Null]
      [(field "module" c, field "line" c) | c <- commands, field "command" c == Just "cat x"]
        `shouldBe` [(Just "main.lask", Just (A.Number 6))]
    it "lists a command whose environment a parameter gives under (dynamic), and its default under the head" $ \lask -> do
      os <- envsJson lask ["commands"]
      let programsOf env = [p | o <- os, field "environment" o == Just env, Just (A.Array ps) <- [field "programs" o], A.Object po <- V.toList ps, Just (A.String p) <- [field "program" po]]
      programsOf "(dynamic)" `shouldBe` ["cat", "npm"]
      programsOf "#alpine:3.22.2" `shouldBe` ["make", "npm"]
    it "gives the same JSON with --verbose" $ \lask -> do
      plain' <- envsJson lask ["commands"]
      verbose <- envsJson lask ["commands", "--verbose"]
      verbose `shouldBe` plain'
    it "contacts no container engine" $ \lask ->
      withFakeDocker $ \state extra -> withProject [("main.lask", program)] $ \dir -> do
        writeFile (state <> "/calls") ""
        _ <- runLaskEnv lask dir extra ["envs", "commands"] ""
        calls state `shouldReturn` []

  describe "envs files, network, access and secrets (spec 11.4)" $ do
    it "gives each row of files as environment, kind, check, required_by and files" $ \lask -> do
      os <- envsJson lask ["files"]
      map keysOf os `shouldSatisfy` all (== ["check", "environment", "files", "kind", "required_by"])
      [field "files" o | o <- os, field "environment" o == Just "#local"]
        `shouldBe` [Just (A.toJSON [A.object [("access", "rw"), ("source", "(host)"), ("target", "/")]])]
    it "gives each row of network as environment, kind, check, required_by and network" $ \lask -> do
      os <- envsJson lask ["network"]
      map keysOf os `shouldSatisfy` all (== ["check", "environment", "kind", "network", "required_by"])
      let network env = [n | o <- os, field "environment" o == Just env, Just (A.Object n) <- [field "network" o]]
      map keysOf (network "#local") `shouldBe` [["class", "egress", "inbound", "name", "name_resolution"]]
      map (field "class") (network "#local") `shouldBe` [Just "host"]
      map (field "class") (network "#alpine:3.22.2") `shouldBe` [Just "default"]
      -- inbound and name_resolution are null for (dynamic).
      [(field "class" n, field "inbound" n, field "name_resolution" n) | n <- network "(dynamic)"]
        `shouldBe` [(Just "dynamic", Just A.Null, Just A.Null)]
    it "gives each row of access as files and network both" $ \lask -> do
      os <- envsJson lask ["access"]
      map keysOf os `shouldSatisfy` all (== ["check", "environment", "files", "kind", "network", "required_by"])
      -- No constraint covers any use: check is null, for the — of the table.
      map (field "check") os `shouldSatisfy` all (== Just A.Null)
    it "states, for a function, the constraint it is checked against before the table" $ \lask ->
      withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
        r <- runLaskEnv lask dir extra ["envs", "access", "both"] ""
        resExit r `shouldBe` 0
        take 1 (lines (resOut r)) `shouldBe` ["within: none"]
    it "says so when no secret is given to an environment, and gives an empty array as JSON" $ \lask ->
      withFakeDocker $ \_ extra -> withProject [("main.lask", program)] $ \dir -> do
        r <- runLaskEnv lask dir extra ["envs", "secrets"] ""
        resExit r `shouldBe` 0
        resOut r `shouldSatisfy` isInfixOf "no secrets"
        j <- runLaskEnv lask dir extra ["envs", "secrets", "--output", "json"] ""
        (resExit j, resOut j) `shouldBe` (0, "[]\n")
