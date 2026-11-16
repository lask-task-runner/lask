{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EgressSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Language.Lask.Access (renderEgressEntry)
import Language.Lask.ErrorCode
import Language.Lask.Runtime.Container (Engine (..), cliBackend)
import Language.Lask.Runtime.Egress
import Language.Lask.Runtime.Value
import Test.Hspec

-- | Run options with @egress@ set to these strings.
withEgress :: [Text] -> [(Text, Value)] -> Map Text Value
withEgress es rest = Map.fromList (("egress", VArray (V.fromList (map VString es))) : rest)

-- | The entries as written back, or the code of the failure.
entries :: Map Text Value -> Either (Maybe ErrorCode) (Maybe [Text])
entries opts = either (Left . lfCode) (Right . fmap (map renderEgressEntry)) (egressOf opts)

-- | A placement whose engine none of these tests reaches.
placement :: Placement
placement = Placement "lask-in" "lask-out" "lask-proxy" (cliBackend Docker)

envOf :: Map Text Value -> Map Text Value
envOf opts = case Map.lookup "env" opts of
  Just (VMap m) -> m
  _ -> Map.empty

proxyUrl :: Value
proxyUrl = VString "http://lask-egress-proxy:3128"

spec :: Spec
spec = do
  describe "egressOf (spec 10.12)" $ do
    it "is Nothing when egress is not given, or is null" $ do
      entries Map.empty `shouldBe` Right Nothing
      entries (Map.fromList [("egress", VNull)]) `shouldBe` Right Nothing
    it "reads each entry as <host>:<port>" $
      entries (withEgress ["registry.npmjs.org:443", "*.github.com:443"] [])
        `shouldBe` Right (Just ["registry.npmjs.org:443", "*.github.com:443"])
    it "is an empty list, reaching nothing, when egress is []" $
      entries (withEgress [] []) `shouldBe` Right (Just [])
    it "drops null entries, as an array literal with a false if does" $
      entries (Map.fromList [("egress", VArray (V.fromList [VNull, VString "a.example:443"]))])
        `shouldBe` Right (Just ["a.example:443"])
    it "is E-RUNTIME-VALUE for an entry without a port" $
      entries (withEgress ["registry.npmjs.org"] []) `shouldBe` Left (Just ERuntimeValue)
    it "is E-RUNTIME-VALUE for an entry that is not a string" $
      entries (Map.fromList [("egress", VArray (V.fromList [VNumber 443]))]) `shouldBe` Left (Just ERuntimeValue)
    it "is E-RUNTIME-VALUE when egress is not an array" $
      entries (Map.fromList [("egress", VString "a.example:443")]) `shouldBe` Left (Just ERuntimeValue)
    it "is E-RUNTIME-VALUE when network is given beside it" $
      entries (withEgress ["a.example:443"] [("network", VString "host")]) `shouldBe` Left (Just ERuntimeValue)
    it "is E-RUNTIME-VALUE when publish is given beside it" $
      entries (withEgress ["a.example:443"] [("publish", VArray (V.fromList [VString "8080:80"]))])
        `shouldBe` Left (Just ERuntimeValue)
    it "allows network and publish that are null or hold only nulls" $
      entries (withEgress ["a.example:443"] [("network", VNull), ("publish", VArray (V.fromList [VNull]))])
        `shouldBe` Right (Just ["a.example:443"])

  describe "placedOptions (spec 10.12)" $ do
    it "puts the container on the inside network, in place of egress" $ do
      let opts = placedOptions placement (withEgress ["a.example:443"] [])
      Map.lookup "network" opts `shouldBe` Just (VString "lask-in")
      Map.member "egress" opts `shouldBe` False
    it "names the proxy in both spellings of the proxy variables" $ do
      let env = envOf (placedOptions placement (withEgress ["a.example:443"] []))
      mapM_ (\k -> Map.lookup k env `shouldBe` Just proxyUrl) ["HTTP_PROXY", "HTTPS_PROXY", "http_proxy", "https_proxy"]
      Map.lookup "NO_PROXY" env `shouldBe` Just (VString "localhost,127.0.0.1,::1")
      Map.lookup "no_proxy" env `shouldBe` Just (VString "localhost,127.0.0.1,::1")
      Map.lookup "NODE_USE_ENV_PROXY" env `shouldBe` Just (VString "1")
    it "keeps a variable the program's own env sets" $ do
      let own = VMap (Map.fromList [("NO_PROXY", VString "internal"), ("FOO", VString "bar")])
          env = envOf (placedOptions placement (withEgress ["a.example:443"] [("env", own)]))
      Map.lookup "NO_PROXY" env `shouldBe` Just (VString "internal")
      Map.lookup "FOO" env `shouldBe` Just (VString "bar")
      Map.lookup "HTTPS_PROXY" env `shouldBe` Just proxyUrl
    it "leaves the other options as they are" $ do
      let opts = placedOptions placement (withEgress ["a.example:443"] [("workdir", VString "/src")])
      Map.lookup "workdir" opts `shouldBe` Just (VString "/src")
    it "reaches the proxy by the host and port every container uses" $ do
      proxyHost `shouldBe` "lask-egress-proxy"
      proxyPort `shouldBe` 3128
