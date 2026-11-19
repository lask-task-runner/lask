{-# LANGUAGE OverloadedStrings #-}

-- | Concurrency seen from the command line: cancellation and failures nobody awaited (spec 6.3, 8.7, 15.6).
module Cli.ConcurrencySpec (spec) where

import Control.Concurrent (threadDelay)
import Data.List (isInfixOf, isPrefixOf)
import Support.Harness
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = beforeAll findLask $ do
  -- A computation nothing awaits is waited for at the end of the run,
  -- and reported as an advisory that leaves the outcome alone.
  describe "stopping the commands race cancels (spec 8.7, 15.6)" $ do
    it "stops the losing command and what it started, and logs it as killed" $ \lask ->
      withProject
        [ ( "main.lask",
            "slow(): String = $[#local] echo $$ > pids; sleep 30 & echo $! >> pids; wait\n\
            \fast(): String = $[#local] sleep 1; echo fast\n\
            \f(): String = race([async slow(), async fast()])\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          resExit r `shouldBe` 0
          resOut r `shouldBe` "\"fast\\n\"\n"
          resErr r `shouldContain` "killed"
          pids <- lines <$> readFile (dir </> "pids")
          length pids `shouldBe` 2
          alive <- mapM (\p -> readCreateProcessWithExitCode (proc "kill" ["-0", p]) "") pids
          [c | (c, _, _) <- alive] `shouldSatisfy` notElem ExitSuccess

    it "stops the losing container through the daemon, by its name" $ \lask ->
      withFakeDocker $ \state extra ->
        withProject
          [ ( "main.lask",
              "slow(): String = $[#alpine:3.22.2] sleep 30\n\
              \fast(): String = $[#local] sleep 1; echo fast\n\
              \f(): String = race([async slow(), async fast()])\n"
            )
          ]
          $ \dir -> do
            _ <- runLaskEnv lask dir extra ["sync"] ""
            r <- runLaskEnv lask dir extra ["eval", "f"] ""
            resExit r `shouldBe` 0
            cs <- calls state
            let named = [w | c <- cs, "run " `isPrefixOf` c, (flag, w) <- zip (words c) (drop 1 (words c)), flag == "--name"]
            case named of
              [name] -> do
                cs `shouldContain` ["stop -t 3 " <> name]
                cs `shouldContain` ["rm --force " <> name]
              _ -> expectationFailure ("expected one named run, got " <> show named)

  describe "stopping the commands all cancels when one fails (spec 8.7, 15.6)" $ do
    it "fails at the first failure, wherever it is, and stops the rest" $ \lask ->
      withProject
        [ ( "main.lask",
            "slow() = $[#local] sleep 3; touch marker\n\
            \quick() = $[#local] sleep 0.5; exit 7\n\
            \f() = all([async slow(), async quick()])\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["run", "f"] ""
          resExit r `shouldBe` 7
          resErr r `shouldContain` "killed"
          resErr r `shouldNotContain` "W-ASYNC-UNAWAITED"
          threadDelay 4000000
          doesFileExist (dir </> "marker") `shouldReturn` False

  describe "never-awaited async (spec 6.3, 14.2)" $ do
    it "runs it to completion and reports it, keeping the exit code" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] sh -c 'sleep 1; echo done > side.txt'\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"ok\"\n"
        resErr r `shouldContain` "W-ASYNC-UNAWAITED: the async at main.lask:2:7 was never awaited"
        resErr r `shouldContain` "and completed"
        readFile (dir </> "side.txt") `shouldReturn` "done\n"
    it "fails the run with its failure, and prints no result (spec 6.3, 8.6)" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] exit 3\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 3
        resOut r `shouldBe` ""
        resErr r `shouldContain` "failed with E-RUNTIME-COMMAND-NONZERO (exit code 3)"
        resErr r `shouldContain` "\nE-RUNTIME-COMMAND-NONZERO: "
    it "fails the run with the first failure in the order the computations started" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  a = async $[#local] sh -c 'sleep 1; exit 4'\n  b = async $[#local] exit 6\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 4
    it "leaves the run as it is when the failure is caught where it starts" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async try { $[#local] { exit 3 } } catch (e) { null }\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 0
        resOut r `shouldBe` "\"ok\"\n"
    it "reports it when the function itself fails, which keeps its own exit code" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] echo side\n  $[#local] exit 5\n}\n")] $ \dir -> do
        r <- runLask lask dir ["run", "f"] ""
        resExit r `shouldBe` 5
        resErr r `shouldContain` "W-ASYNC-UNAWAITED: the async at main.lask:2:7"
    it "does not report a handle consumed by await, all, race, or another function" $ \lask ->
      withProject
        [ ( "main.lask",
            "wait(h: AsyncHandle<String>): String = await h\n\
            \f(): String = do {\n\
            \  a = async \"a\"\n\
            \  b = async \"b\"\n\
            \  c = async \"c\"\n\
            \  d = async \"d\"\n\
            \  e = async \"e\"\n\
            \  x = await a\n\
            \  ys = all([b, c])\n\
            \  z = race([d])\n\
            \  w = wait(e)\n\
            \  \"ok\"\n\
            \}\n"
          )
        ]
        $ \dir -> do
          r <- runLask lask dir ["eval", "f"] ""
          r `shouldBe` Result 0 "\"ok\"\n" ""
    it "does not report the rest of an all whose first handle failed" $ \lask ->
      withProject [("main.lask", "f(): Array<String> = do {\n  a = async $[#local] exit 4\n  b = async $[#local] echo b\n  all([a, b])\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "f"] ""
        resExit r `shouldBe` 4
        resErr r `shouldNotContain` "W-ASYNC-UNAWAITED"
    it "reports it as a JSON Lines warning under --log-format json" $ \lask ->
      withProject [("main.lask", "f(): String = do {\n  h = async $[#local] exit 3\n  \"ok\"\n}\n")] $ \dir -> do
        r <- runLask lask dir ["eval", "--log-format", "json", "f"] ""
        resExit r `shouldBe` 3
        let warnings = [l | l <- lines (resErr r), "W-ASYNC-UNAWAITED" `isInfixOf` l]
        length warnings `shouldBe` 1
        mapM_
          (\field -> concat warnings `shouldContain` field)
          [ "\"severity\":\"warning\"",
            "\"stage\":\"runtime\"",
            "\"location\":{\"column\":7,\"file\":\"main.lask\",\"line\":2}",
            "\"failure\":{\"code\":\"E-RUNTIME-COMMAND-NONZERO\""
          ]
