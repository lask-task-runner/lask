-- | What @lask sync@ reports while it runs (spec 11.7, "Progress and
-- the summary"): JSON Lines events under @--log-format json@,
-- timestamped lines when stderr is not a terminal, and a line redrawn
-- in place on one.
module Cli.ProgressSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Support.Harness
import Test.Hspec

project :: [(FilePath, String)]
project = [("main.lask", "f(): String = $[#alpine:3.22.2] true\n")]

-- | The JSON objects among the lines of stderr.
events :: String -> [A.Object]
events = mapMaybe (A.decode . BL.pack) . lines

field :: String -> A.Object -> Maybe A.Value
field k = KM.lookup (read (show k))

str :: String -> A.Object -> Maybe String
str k o = case field k o of
  Just (A.String t) -> Just (show t)
  _ -> Nothing

spec :: Spec
spec = beforeAll findLask $ describe "sync progress (spec 11.7)" $ do
  it "writes JSON Lines events: sections, and each item's start and end" $ \lask ->
    withFakeDocker $ \_ extra -> withProject project $ \dir -> do
      r <- runLaskEnv lask dir extra ["sync", "--log-format", "json"] ""
      resExit r `shouldBe` 0
      let es = events (resErr r)
          kinds = mapMaybe (str "event") es
      kinds `shouldSatisfy` elem (show "section")
      kinds `shouldSatisfy` elem (show "start")
      kinds `shouldSatisfy` elem (show "done")
      es `shouldSatisfy` all (\o -> field "timestamp" o /= Nothing)
      [o | o <- es, str "event" o == Just (show "done")] `shouldSatisfy` all (\o -> field "ok" o == Just (A.Bool True))
    -- Every line of stderr is an event: nothing else is mixed in.
  it "writes nothing but events to stderr under --log-format json" $ \lask ->
    withFakeDocker $ \_ extra -> withProject project $ \dir -> do
      r <- runLaskEnv lask dir extra ["sync", "--log-format", "json"] ""
      length (events (resErr r)) `shouldBe` length (lines (resErr r))

  it "writes timestamped lines when stderr is not a terminal" $ \lask ->
    withFakeDocker $ \_ extra -> withProject project $ \dir -> do
      r <- runLaskEnv lask dir extra ["sync"] ""
      resExit r `shouldBe` 0
      resErr r `shouldSatisfy` isInfixOf "Images (1)"
      resErr r `shouldSatisfy` isInfixOf "alpine:3.22.2"
      -- Each item line starts with its time.
      [l | l <- lines (resErr r), "alpine:3.22.2" `isInfixOf` l] `shouldSatisfy` all (\l -> take 2 l == "20")
      resErr r `shouldNotSatisfy` isInfixOf "\ESC[2K"

  it "redraws the item in progress in place on a terminal" $ \lask ->
    withFakeDocker $ \_ extra -> withProject project $ \dir -> do
      r <- runLaskInTerminal "/usr/bin/env" dir ([k <> "=" <> v | (k, v) <- extra] <> [lask, "sync"]) ""
      resExit r `shouldBe` 0
      resOut r `shouldSatisfy` isInfixOf "\ESC[2K"
      resOut r `shouldSatisfy` isInfixOf "alpine:3.22.2"

  it "reports a failed item with its error code" $ \lask ->
    withFakeDocker $ \_ extra -> withProject [("main.lask", "f(): String = $[#nosuchimage-lask-test:1] true\n")] $ \dir -> do
      r <- runLaskEnv lask dir extra ["sync", "--log-format", "json"] ""
      resExit r `shouldBe` 3
      [o | o <- events (resErr r), str "event" o == Just (show "done")] `shouldSatisfy` any (\o -> field "ok" o == Just (A.Bool False))
