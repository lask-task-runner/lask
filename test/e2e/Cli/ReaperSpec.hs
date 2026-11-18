-- | The reaper on its own (spec 11.12): @lask __reap <client> <owner>@
-- run as an invocation runs it, with its stdin the pipe the invocation
-- holds, against a client that answers as it is told. "Cli.SignalSpec"
-- has an invocation start it and be killed.
module Cli.ReaperSpec (spec) where

import Support.FakeClient (clientCalls, withClient)
import Support.Harness
import System.FilePath ((</>))
import Test.Hspec

-- | A client with two containers and a proxy labelled with the owner
-- @h:1@, one container under both labels, and its two networks.
leftovers :: String
leftovers =
  unlines
    [ "case \"$*\" in",
      "  'ps -aq --filter label=io.lask.owner=h:1') echo c1; echo shared ;;",
      "  'ps -aq --filter label=io.lask.egress.owner=h:1') echo shared; echo proxy1 ;;",
      "  'network ls -q --filter label=io.lask.egress.owner=h:1') echo n-in; echo n-out ;;",
      "esac"
    ]

-- | Run the reaper with the client of 'withClient', the owner @h:1@ and
-- what the invocation said on the pipe before it closed.
reap :: FilePath -> FilePath -> String -> IO Result
reap lask dir said = runLask lask dir ["__reap", dir </> "bin" </> "docker", "h:1"] said

spec :: Spec
spec = beforeAll findLask $ describe "the reaper (spec 11.12)" $ do
  it "removes the containers, the proxy and the networks of an owner whose pipe closed without a word" $ \lask ->
    withClient "docker" leftovers $ \dir -> do
      r <- reap lask dir ""
      (resExit r, resOut r, resErr r) `shouldBe` (0, "", "")
      clientCalls dir
        `shouldReturn` [ "ps -aq --filter label=io.lask.owner=h:1",
                         "ps -aq --filter label=io.lask.egress.owner=h:1",
                         "rm -f c1 shared proxy1",
                         "network ls -q --filter label=io.lask.egress.owner=h:1",
                         "network rm n-in n-out"
                       ]

  it "removes nothing when the invocation said it was done, whatever else it wrote" $ \lask ->
    withClient "docker" leftovers $ \dir -> do
      resExit <$> reap lask dir "partial\ndone\n" `shouldReturn` 0
      clientCalls dir `shouldReturn` []

  it "asks for the owner's containers and networks, and removes nothing when there are none" $ \lask ->
    withClient "docker" "" $ \dir -> do
      resExit <$> reap lask dir "not done\n" `shouldReturn` 0
      clientCalls dir
        `shouldReturn` [ "ps -aq --filter label=io.lask.owner=h:1",
                         "ps -aq --filter label=io.lask.egress.owner=h:1",
                         "network ls -q --filter label=io.lask.egress.owner=h:1"
                       ]

  it "takes a client that fails, or that is not there, as having nothing to remove" $ \lask -> do
    withClient "docker" "echo c1; exit 1" $ \dir -> do
      resExit <$> reap lask dir "" `shouldReturn` 0
      length <$> clientCalls dir `shouldReturn` 3
    withClient "docker" "" $ \dir -> do
      r <- runLask lask dir ["__reap", dir </> "no-such-client", "h:1"] ""
      (resExit r, resErr r) `shouldBe` (0, "")

  it "does nothing given other than a client and an owner" $ \lask ->
    withClient "docker" leftovers $ \dir -> do
      r <- runLask lask dir ["__reap", dir </> "bin" </> "docker"] ""
      (resExit r, resOut r, resErr r) `shouldBe` (0, "", "")
      clientCalls dir `shouldReturn` []
