{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Obs.CommandLogSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), picosecondsToDiffTime)
import Language.Lask.Obs.CommandLog
import Test.Hspec

ts :: UTCTime
ts =
  UTCTime
    (fromGregorian 2026 7 23)
    (picosecondsToDiffTime (((10 * 3600 + 15 * 60) * 1000 + 4123) * 1000000000))

envJson :: A.Value
envJson =
  A.object
    [ ("$type", A.String "Environment"),
      ("kind", A.String "docker"),
      ("params", A.object [("image", A.String "golang:1.22")])
    ]

mkLog :: CommandLogKind -> CommandLog
mkLog = CommandLog ts "#golang:1.22" envJson 1 "go build ./..."

decodeLine :: Text -> Maybe A.Object
decodeLine t = case A.decode (BL.fromStrict (TE.encodeUtf8 t)) of
  Just (A.Object o) -> Just o
  _ -> Nothing

fieldOf :: Text -> Text -> Maybe A.Value
fieldOf line k = decodeLine line >>= KM.lookup (AK.fromText k)

hasField :: Text -> Text -> Bool
hasField line k = fieldOf line k /= Nothing

spec :: Spec
spec = do
  describe "text format (spec 12.3)" $ do
    it "renders the start line with the command string" $
      renderCommandLogText (mkLog ClStart)
        `shouldBe` "2026-07-23T10:15:04.123Z [#golang:1.22:1] $ go build ./..."
    it "renders relayed stdout lines with the fd prefix" $
      renderCommandLogText (mkLog (ClLine 1 "compiling module..."))
        `shouldBe` "2026-07-23T10:15:04.123Z [#golang:1.22:1] 1| compiling module..."
    it "renders relayed stderr lines with the fd prefix" $
      renderCommandLogText (mkLog (ClLine 2 "warning: unused variable"))
        `shouldBe` "2026-07-23T10:15:04.123Z [#golang:1.22:1] 2| warning: unused variable"
    it "renders exit lines with the code" $
      renderCommandLogText (mkLog (ClExit 0))
        `shouldBe` "2026-07-23T10:15:04.123Z [#golang:1.22:1] exit 0"
    it "carries the execution number in the environment summary" $
      renderCommandLogText ((mkLog (ClExit 0)) {clExec = 42})
        `shouldBe` "2026-07-23T10:15:04.123Z [#golang:1.22:42] exit 0"
    it "does not repeat the command on relay lines" $
      renderCommandLogText (mkLog (ClLine 1 "x"))
        `shouldSatisfy` (not . T.isInfixOf "go build")
    it "truncates long command summaries" $ do
      let long = T.replicate 300 "x"
      T.length (summarizeCommand long) `shouldBe` 256

  describe "JSON Lines format (spec 12.2, 12.3)" $ do
    it "carries the common required fields on every line" $ do
      let checkCommon line = do
            fieldOf line "timestamp" `shouldBe` Just (A.String "2026-07-23T10:15:04.123Z")
            fieldOf line "traceId" `shouldBe` Just (A.String "t-1")
            fieldOf line "exec" `shouldBe` Just (A.Number 1)
            hasField line "level" `shouldBe` True
      checkCommon (renderCommandLogJson "t-1" (mkLog ClStart))
      checkCommon (renderCommandLogJson "t-1" (mkLog (ClLine 1 "hello")))
      checkCommon (renderCommandLogJson "t-1" (mkLog (ClExit 0)))
    it "puts command and env on the start line" $ do
      let line = renderCommandLogJson "t-1" (mkLog ClStart)
      fieldOf line "event" `shouldBe` Just (A.String "start")
      fieldOf line "command" `shouldBe` Just (A.String "go build ./...")
      fieldOf line "env" `shouldBe` Just envJson
      fieldOf line "level" `shouldBe` Just (A.String "info")
    it "omits command and env on relay lines (correlated via exec)" $ do
      let line = renderCommandLogJson "t-1" (mkLog (ClLine 1 "hello"))
      fieldOf line "stream" `shouldBe` Just (A.String "1")
      fieldOf line "message" `shouldBe` Just (A.String "hello")
      hasField line "command" `shouldBe` False
      hasField line "env" `shouldBe` False
    it "marks exit with event=exit and the code" $ do
      let line = renderCommandLogJson "t-1" (mkLog (ClExit 0))
      fieldOf line "event" `shouldBe` Just (A.String "exit")
      fieldOf line "code" `shouldBe` Just (A.Number 0)
      fieldOf line "level" `shouldBe` Just (A.String "info")
    it "raises exit level to warn on non-zero codes" $ do
      let line = renderCommandLogJson "t-1" (mkLog (ClExit 7))
      fieldOf line "code" `shouldBe` Just (A.Number 7)
      fieldOf line "level" `shouldBe` Just (A.String "warn")
    it "is one JSON object per line" $ do
      let line = renderCommandLogJson "t-1" (mkLog (ClLine 2 "e"))
      T.count "\n" line `shouldBe` 0
      decodeLine line `shouldSatisfy` (/= Nothing)
