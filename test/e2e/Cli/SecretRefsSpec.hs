{-# LANGUAGE OverloadedStrings #-}

-- | @lask secrets@ without a store to reach (spec 9.8, 11.10): which
-- variables it considers, how it reports a reference it cannot use,
-- and its exit codes. The stages against a store are checked against
-- the fake Vault in "Cli.SecretsSpec".
module Cli.SecretRefsSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Support.Harness
import Test.Hspec

-- | Variables holding references, malformed ones and a plain value.
variables :: [(String, String)]
variables =
  [ ("SR_TOKEN", "{vault://secret/app#token}"),
    ("SR_OTHER", "{zzz://a/b#c}"),
    ("SR_OPEN", "{vault://secret/app#token"),
    ("SR_EMBEDDED", "x-{vault://secret/app#token}"),
    ("SR_PLAIN", "a-plain-value-sr"),
    ("LASK_SECRETS", "")
  ]

program :: String
program =
  unlines
    [ "token(): String = get_env(\"SR_TOKEN\")",
      "plain(): String = get_env(\"SR_PLAIN\")",
      "any(name: String): String = get_env(name)"
    ]

secrets :: FilePath -> [String] -> IO Result
secrets lask args = withProject [("main.lask", program)] $ \dir -> runLaskEnv lask dir variables ("secrets" : args) ""

-- | The variables a report names, of those this spec sets.
named :: Result -> [String]
named r = sort [v | (v, _) <- variables, v /= "LASK_SECRETS", (v <> " ") `isInfixOf` resOut r || ("\"" <> v <> "\"") `isInfixOf` resOut r]

json :: Result -> Maybe A.Value
json = A.decode . BL.fromStrict . TE.encodeUtf8 . T.pack . resOut

keysOf :: A.Object -> [Text]
keysOf = sort . map K.toText . KM.keys

spec :: Spec
spec = beforeAll findLask $ describe "secrets (spec 9.8, 11.10)" $ do
  it "works outside a project" $ \lask ->
    withProject [] $ \dir -> do
      r <- runLaskEnv lask dir variables ["secrets", "list"] ""
      resExit r `shouldBe` 0
      named r `shouldBe` ["SR_OPEN", "SR_OTHER", "SR_TOKEN"]
  it "lists the references, a malformed one included, and neither an embedded one nor a value" $ \lask -> do
    r <- secrets lask ["list"]
    resExit r `shouldBe` 0
    named r `shouldBe` ["SR_OPEN", "SR_OTHER", "SR_TOKEN"]
    resOut r `shouldNotSatisfy` isInfixOf "a-plain-value-sr"
  it "reports a scheme no store provides, one not enabled and a malformed reference as statuses" $ \lask -> do
    r <- secrets lask ["list"]
    let line v = [l | l <- lines (resOut r), (v <> " ") `isInfixOf` l]
    line "SR_OTHER" `shouldSatisfy` all (isInfixOf "E-IO-SECRET-PROVIDER")
    line "SR_TOKEN" `shouldSatisfy` all (\l -> "E-IO-SECRET-PROVIDER" `isInfixOf` l && "LASK_SECRETS" `isInfixOf` l)
    line "SR_OPEN" `shouldSatisfy` all (isInfixOf "E-IO-SECRET-REF")
  it "gives each reference as JSON with its variable, scheme, reference, status, code and message" $ \lask -> do
    r <- secrets lask ["list", "--output", "json"]
    case json r of
      Just (A.Array xs) -> do
        let os = [o | A.Object o <- V.toList xs, KM.lookup "variable" o `elem` map (Just . A.String) ["SR_TOKEN", "SR_OTHER", "SR_OPEN"]]
        length os `shouldBe` 3
        -- A malformed reference has no scheme to give.
        [(KM.lookup "variable" o, keysOf o) | o <- os]
          `shouldMatchList` [ (Just "SR_TOKEN", ["code", "message", "reference", "scheme", "status", "variable"]),
                              (Just "SR_OTHER", ["code", "message", "reference", "scheme", "status", "variable"]),
                              (Just "SR_OPEN", ["code", "message", "reference", "status", "variable"])
                            ]
        map (KM.lookup "status") os `shouldSatisfy` all (== Just "NG")
      other -> expectationFailure ("not a JSON array: " <> show other)
  it "considers, for a function, only the variables it names" $ \lask -> do
    r <- secrets lask ["list", "token"]
    named r `shouldBe` ["SR_TOKEN"]
    p <- secrets lask ["list", "plain"]
    (resExit p, named p) `shouldBe` (0, [])
  it "considers every variable for a function that computes the name it reads" $ \lask -> do
    r <- secrets lask ["list", "any"]
    named r `shouldBe` ["SR_OPEN", "SR_OTHER", "SR_TOKEN"]
  it "exits 4 for a name that is not a function of the module" $ \lask -> do
    r <- secrets lask ["list", "nope"]
    resExit r `shouldBe` 4
  it "exits 3 from check when a store is not available, skipping its references" $ \lask -> do
    r <- secrets lask ["check"]
    resExit r `shouldBe` 3
    [l | l <- lines (resOut r), "SR_TOKEN" `isInfixOf` l] `shouldSatisfy` all (isInfixOf "skipped")
  it "reports a malformed reference apart, as JSON stores and malformed" $ \lask -> do
    r <- secrets lask ["check", "--output", "json"]
    resExit r `shouldBe` 3
    case json r of
      Just (A.Object o) -> do
        keysOf o `shouldBe` ["malformed", "stores"]
        case KM.lookup "malformed" o of
          Just (A.Array ms) -> [(KM.lookup "variable" m, KM.lookup "code" m) | A.Object m <- V.toList ms] `shouldBe` [(Just "SR_OPEN", Just "E-IO-SECRET-REF")]
          other -> expectationFailure ("no malformed: " <> show other)
      other -> expectationFailure ("not a JSON object: " <> show other)
  it "exits 0 from check when there is nothing to check" $ \lask ->
    withProject [] $ \dir -> do
      r <- runLaskEnv lask dir [("LASK_SECRETS", "")] ["secrets", "check"] ""
      resExit r `shouldBe` 0
