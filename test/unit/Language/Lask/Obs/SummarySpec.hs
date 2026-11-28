{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Obs.SummarySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Language.Lask.Obs.Summary
import Test.Hspec

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 7 23) 36000

row :: Int -> String -> Double -> Maybe CommandEnd -> CommandRow
row n cmd secs end =
  CommandRow n "#local" (A.object []) (T.pack cmd) t0 ((\e -> (addUTCTime (realToFrac secs) t0, e)) <$> end)

spec :: Spec
spec = describe "run summary (spec 12.9)" $ do
  it "formats durations by their size" $ do
    map formatDuration [0.04, 12.44, 185, 3725] `shouldBe` ["0.0s", "12.4s", "3m05s", "1h02m"]
  it "aligns the rows and counts how the commands ended" $ do
    let sm =
          Summary
            (SubjectFunction "deploy")
            "main.lask"
            (Failed 1)
            12.4
            [ row 1 "make build" 3.1 (Just (EndExit 0)),
              row 2 "npm test" 8.9 (Just (EndExit 1)),
              row 3 "sleep 9" 0.2 (Just EndKilled),
              row 4 "true" 0 Nothing
            ]
    renderSummaryText sm
      `shouldBe` [ "summary: deploy (main.lask) failed with exit code 1 after 12.4s",
                   "  #1  #local  make build  3.1s  exit 0",
                   "  #2  #local  npm test    8.9s  exit 1",
                   "  #3  #local  sleep 9     0.2s  killed",
                   "  #4  #local  true        -     unfinished",
                   "  4 commands: 1 succeeded, 1 failed, 1 killed, 1 unfinished"
                 ]
  it "has the first line alone when no command ran" $ do
    renderSummaryText (Summary (SubjectCommand "npm") "main.lask" (Interrupted 130) 1 [])
      `shouldBe` ["summary: cmd npm (main.lask) was interrupted (exit code 130) after 1.0s"]
  it "shortens a long or multi-line command to its first line" $ do
    let sm = Summary (SubjectFunction "f") "m.lask" Succeeded 1 [row 1 "echo a\necho b" 0.1 (Just (EndExit 0))]
    renderSummaryText sm !! 1 `shouldBe` "  #1  #local  echo a ...  0.1s  exit 0"
