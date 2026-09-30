{-# LANGUAGE OverloadedStrings #-}

-- | Confirmation declared in the project file (spec 5, 11.2): the
-- checks @lask check@ runs on it, and what is asked before a call.
module Language.Lask.ConfirmSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Confirm
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Module.Resolve (entryPublicValues)
import Language.Lask.Runtime.Value (Value (..))
import Language.Lask.Span (Position (..), Span (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

tasks :: String
tasks =
  "deploy(--env: String = \"staging\"): String = env\n\
  \dynamic(--env: String = get_env_or(\"TARGET\", \"staging\")): String = env\n\
  \destroy(): String = \"gone\"\n\
  \reset_db(db: String, --force: Bool = false): String = db\n\
  \scale(--count: Number = 1): Number = count\n\
  \release(): String = deploy(env = \"prod\")\n"

spec :: Spec
spec = do
  describe "lask check on confirm" $ do
    it "accepts entries that refer to the program" $
      withCompiled tasks confirmOk $ \r -> fmap (const ()) r `shouldBe` Right ()

    it "reports a function that does not exist, where the key is written" $
      withCompiled tasks "{\n  \"confirm\": {\n    \"deploi\": {}\n  }\n}" $ \r -> do
        let ds = either id (const []) r
        map diagCode ds `shouldBe` [EModuleConfirmTarget]
        map (spanLine . diagSpan) ds `shouldBe` [Just 3]
        map (T.unpack . diagMessage) ds `shouldSatisfy` all ("did you mean 'deploy'" `isInfixOf`)

    it "reports a parameter, a value or an interpolation that does not fit" $
      withCompiled tasks badRefs $ \r ->
        map diagCode (either id (const []) r) `shouldBe` replicate 3 EModuleConfirmTarget

    it "checks every entry, whether or not anything calls the function" $
      withCompiled "a(): String = \"a\"\n" "{\"confirm\": {\"b\": {}}}" $ \r ->
        map diagCode (either id (const []) r) `shouldBe` [EModuleConfirmTarget]

  describe "what is asked before a call" $ do
    it "always asks when there is no condition, for the function's name" $
      withCompiled tasks confirmOk $ \r ->
        promptFor r "destroy" [] [] `shouldBe` Just (Prompt "destroy" [] "destroy")

    it "asks only when a condition holds, for the matched value" $
      withCompiled tasks confirmOk $ \r -> do
        promptFor r "deploy" [] [("env", VString "prod")] `shouldBe` Just (Prompt "deploy" ["env=prod"] "prod")
        promptFor r "deploy" [] [("env", VString "dev")] `shouldBe` Nothing

    it "takes a literal default into account" $
      withCompiled tasks "{\"confirm\": {\"deploy\": {\"when\": {\"env\": [\"staging\"]}}}}" $ \r ->
        promptFor r "deploy" [] [] `shouldBe` Just (Prompt "deploy" ["env=staging"] "staging")

    it "asks when a condition is on a default only known once the run starts" $
      withCompiled tasks "{\"confirm\": {\"dynamic\": {\"when\": {\"env\": [\"prod\"]}}}}" $ \r ->
        promptFor r "dynamic" [] [] `shouldBe` Just (Prompt "dynamic" ["env=<default>"] "dynamic")

    it "fills a phrase from the arguments, and compares values of any type" $
      withCompiled tasks confirmOk $ \r -> do
        promptFor r "reset_db" [VString "main"] [] `shouldBe` Just (Prompt "reset_db" [] "reset main")
        promptFor r "scale" [] [("count", VNumber 0)] `shouldBe` Just (Prompt "scale" ["count=0"] "0")

    it "applies to the function the CLI calls, and not to what it calls" $
      withCompiled tasks confirmOk $ \r ->
        promptFor r "release" [] [] `shouldBe` Nothing

    it "follows a re-export to the declaration" $
      withSystemTempDirectory "lask-confirm" $ \dir -> do
        writeFile (dir </> "lib.lask") "deploy(--env: String = \"staging\"): String = env\n"
        writeFile (dir </> "main.lask") "export { deploy as ship } from \"./lib.lask\"\n"
        writeFile (dir </> "lask.json") "{\"confirm\": {\"ship\": {}}}"
        r <- compileFile (dir </> "main.lask")
        promptFor r "ship" [] [] `shouldBe` Just (Prompt "ship" [] "ship")

  describe "describing a rule for help" $
    it "names the parameters and values" $
      withCompiled tasks confirmOk $ \r -> do
        describeFor r "deploy" `shouldBe` Just "requires confirmation when env is prod or production"
        describeFor r "destroy" `shouldBe` Just "requires confirmation"
        describeFor r "release" `shouldBe` Nothing
  where
    confirmOk =
      "{\"confirm\": {\
      \\"deploy\": {\"when\": {\"env\": [\"prod\", \"production\"]}},\
      \\"destroy\": {},\
      \\"reset_db\": {\"phrase\": \"reset #{db}\"},\
      \\"scale\": {\"when\": {\"count\": [0]}}}}"
    badRefs =
      "{\"confirm\": {\
      \\"deploy\": {\"when\": {\"environment\": [\"prod\"]}},\
      \\"scale\": {\"when\": {\"count\": [\"zero\"]}},\
      \\"reset_db\": {\"phrase\": \"reset #{database}\"}}}"

withCompiled :: String -> String -> (Either [Diagnostic] Compiled -> IO a) -> IO a
withCompiled source project k =
  withSystemTempDirectory "lask-confirm" $ \dir -> do
    writeFile (dir </> "main.lask") source
    writeFile (dir </> "lask.json") project
    compileFile (dir </> "main.lask") >>= k

-- | What the CLI would ask before calling @name@ with these arguments.
promptFor :: Either [Diagnostic] Compiled -> Text -> [Value] -> [(Text, Value)] -> Maybe Prompt
promptFor r name posVals kwVals = do
  c <- either (const Nothing) Just r
  key <- lookup name (entryPublicValues (compiledProgram c) (compiledScopes c))
  cd <- Map.lookup key (cpDecls (compiledCore c))
  (_, rule) <- ruleFor (compiledProgram c) (compiledScopes c) key
  confirmationFor name rule cd posVals kwVals

describeFor :: Either [Diagnostic] Compiled -> Text -> Maybe Text
describeFor r name = do
  c <- either (const Nothing) Just r
  key <- lookup name (entryPublicValues (compiledProgram c) (compiledScopes c))
  describeRule . snd <$> ruleFor (compiledProgram c) (compiledScopes c) key

spanLine :: Span -> Maybe Int
spanLine (Span p _) = Just (line p)
spanLine NoSpan = Nothing
