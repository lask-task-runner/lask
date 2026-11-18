{-# LANGUAGE OverloadedStrings #-}

-- | How @lask envs commands@ finds the environment of each command
-- (spec 11.4): through local bindings, keyword defaults, sessions,
-- values bound at the top level and choices, and @(dynamic)@ where a
-- caller supplies it.
{-# LANGUAGE OverloadedStrings #-}

module Cli.EnvsReachSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import Support.Harness
import Test.Hspec

-- | Each command @envs commands --output json@ lists, as the function
-- it is in and the environment it is listed under.
commandEnvs :: FilePath -> String -> IO [(String, String)]
commandEnvs lask src = withFakeDocker $ \_ extra -> withProject [("main.lask", src)] $ \dir -> do
  r <- runLaskEnv lask dir extra ["envs", "commands", "--output", "json"] ""
  resExit r `shouldBe` 0
  case A.decode (BL.pack (resOut r)) of
    Just (A.Array envs) ->
      pure
        [ (text "declaration" c, text "environment" e)
        | A.Object e <- V.toList envs,
          Just (A.Array cs) <- [KM.lookup "commands" e],
          A.Object c <- V.toList cs
        ]
    other -> expectationFailure ("not an array: " <> show other <> "\n" <> resOut r) >> pure []
  where
    text k o = case KM.lookup k o of
      Just (A.String t) -> T.unpack t
      _ -> "?"

-- | Whether the command in a function is listed with a text in it.
listedWith :: [(String, String)] -> String -> String -> Bool
listedWith cs f t = any (\(g, e) -> g == f && T.pack t `T.isInfixOf` T.pack e) cs

spec :: Spec
spec = beforeAll findLask $ describe "envs commands, where the environment comes from (spec 11.4)" $ do
  it "follows a local binding" $ \lask -> do
    cs <- commandEnvs lask "f(): String = do {\n  e = #alpine:3.22.2\n  $[e] true\n}\n"
    listedWith cs "f" "alpine:3.22.2" `shouldBe` True
  it "follows a value bound at the top level" $ \lask -> do
    cs <- commandEnvs lask "box = #alpine:3.22.2\nf(): String = $[box] true\n"
    listedWith cs "f" "alpine:3.22.2" `shouldBe` True
  it "follows both branches of a choice" $ \lask -> do
    cs <- commandEnvs lask "f(c: Bool): String = $[c ? #alpine:3.22.2 : #busybox:1.37] true\n"
    (listedWith cs "f" "alpine:3.22.2", listedWith cs "f" "busybox:1.37") `shouldBe` (True, True)
  it "follows the default of a keyword parameter" $ \lask -> do
    cs <- commandEnvs lask "f(--env: Runnable = runnable(#alpine:3.22.2)): String = $[env] true\n"
    listedWith cs "f" "alpine:3.22.2" `shouldBe` True
  it "follows the session with_container gives its body" $ \lask -> do
    cs <- commandEnvs lask "f(): String = with_container(#alpine:3.22.2, \\(box: Runnable): String -> $[box]{ true })\n"
    listedWith cs "f" "alpine:3.22.2" `shouldBe` True
  it "lists a command whose environment a caller supplies as dynamic" $ \lask -> do
    cs <- commandEnvs lask "f(env: Runnable): String = $[env] true\n"
    listedWith cs "f" "dynamic" `shouldBe` True
  it "lists a command inside a lambda" $ \lask -> do
    cs <- commandEnvs lask "f(): Array<String> = map([1], \\(x: Number): String -> $[#alpine:3.22.2]{ true })\n"
    listedWith cs "f" "alpine:3.22.2" `shouldBe` True
