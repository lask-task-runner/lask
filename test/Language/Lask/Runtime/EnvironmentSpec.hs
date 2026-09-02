{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EnvironmentSpec (spec) where

import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Secrets (registerSecret, resetSecretRegistryForTests)
import Language.Lask.Runtime.Value
import Test.Hspec

env :: Text -> [(Text, Value)] -> EnvValue
env k ps = EnvValue k (Map.fromList ps)

spec :: Spec
spec = do
  describe "environment resolution (spec 10.4)" $ do
    it "resolves local" $
      resolveEnv (env "local" []) `shouldBe` Right ResolvedLocal
    it "resolves a registry reference" $
      resolveEnv (env "docker" [("image", VString "alpine:3.20")])
        `shouldBe` Right (ResolvedDocker "alpine:3.20" Map.empty)
    it "resolves a recipe, defaulting the context to the Dockerfile's directory" $
      resolveEnv (env "docker" [("dockerfile", VString "infra/Dockerfile")])
        `shouldBe` Right (ResolvedRecipe "infra/Dockerfile" "infra" Map.empty)
    it "keeps an explicit context" $
      resolveEnv (env "docker" [("dockerfile", VString "infra/Dockerfile"), ("context", VString ".")])
        `shouldBe` Right (ResolvedRecipe "infra/Dockerfile" "." Map.empty)
    it "fails on docker without an image or a recipe" $
      case resolveEnv (env "docker" []) of
        Left lf -> lfCode lf `shouldBe` Just EIoEnvResolve
        Right r -> expectationFailure (show r)
    it "fails on unknown kinds" $
      resolveEnv (env "remote" [("host", VString "h")]) `shouldSatisfy` isLeft

  describe "launch argument construction (spec 10.5)" $ do
    it "builds docker run arguments with mounted workdir" $
      dockerArgs "/proj" "alpine:3.20" (Map.fromList [("memory", VString "4g")]) "uname -a"
        `shouldBe` [ "run", "--rm",
                     "-v", "/proj:/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "--memory", "4g",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

  describe "local execution (spec 8.7, real process)" $ do
    it "runs a local command and captures streams and exit code" $ do
      runner <- mkCommandRunner "/tmp" noCommandLog
      r <- runner (env "local" []) "echo out; echo err 1>&2; exit 3"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 3
          out `shouldBe` "out\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
    it "uses the base directory as the working directory" $ do
      runner <- mkCommandRunner "/tmp" noCommandLog
      r <- runner (env "local" []) "pwd"
      case r of
        Right (0, out, _) -> out `shouldSatisfy` (\o -> o == "/tmp\n" || o == "/private/tmp\n")
        other -> expectationFailure (show other)

  describe "command execution log relay (spec 12.3)" $ do
    let mkLoggedRunner = do
          logRef <- newIORef []
          let sink cl = modifyIORef' logRef (<> [cl])
          runner <- mkCommandRunner "/tmp" sink
          pure (runner, readIORef logRef)
        runWithLog cmd = do
          (runner, readLog) <- mkLoggedRunner
          r <- runner (env "local" []) cmd
          entries <- readLog
          pure (r, entries)
        kinds = map clKind

    it "relays lines in order between start and exit" $ do
      (r, entries) <- runWithLog "echo one; echo two; echo err 1>&2; exit 5"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 5
          out `shouldBe` "one\ntwo\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
      head (kinds entries) `shouldBe` ClStart
      last (kinds entries) `shouldBe` ClExit 5
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["one", "two"]
      [l | ClLine 2 l <- kinds entries] `shouldBe` ["err"]

    it "always emits start and exit even without output" $ do
      (_, entries) <- runWithLog "true"
      kinds entries `shouldBe` [ClStart, ClExit 0]

    it "keeps CommandResult faithful for unterminated output" $ do
      (r, entries) <- runWithLog "printf 'no-newline'"
      case r of
        Right (0, out, _) -> out `shouldBe` "no-newline"
        other -> expectationFailure (show other)
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["no-newline"]

    it "stamps log entries with the environment summary and command" $ do
      (_, entries) <- runWithLog "true"
      map clEnvSummary entries `shouldSatisfy` all (== "#local")
      map clCommand entries `shouldSatisfy` all (== "true")

    it "assigns unique 1-based execution numbers per command" $ do
      (runner, readLog) <- mkLoggedRunner
      _ <- runner (env "local" []) "echo a"
      _ <- runner (env "local" []) "echo b"
      entries <- readLog
      let execsOf cmd = [clExec cl | cl <- entries, clCommand cl == cmd]
      execsOf "echo a" `shouldSatisfy` all (== 1)
      execsOf "echo b" `shouldSatisfy` all (== 2)

    it "masks a registered secret out of the log without touching the captured result (spec 12.8)" $ do
      resetSecretRegistryForTests
      registerSecret "sup3rsecret"
      (r, entries) <- runWithLog "echo sup3rsecret; echo sup3rsecret 1>&2"
      resetSecretRegistryForTests
      case r of
        Right (0, out, errOut) -> do
          -- The value returned to the running program is the real one.
          out `shouldBe` "sup3rsecret\n"
          errOut `shouldBe` "sup3rsecret\n"
        other -> expectationFailure (show other)
      -- The observed log copy is masked, on both the relayed lines...
      [l | ClLine 1 l <- kinds entries] `shouldBe` ["***"]
      [l | ClLine 2 l <- kinds entries] `shouldBe` ["***"]
      -- ...and the command string on the start line.
      map clCommand entries `shouldSatisfy` (not . any (T.isInfixOf "sup3rsecret"))

  describe "environment log info (spec 12.3)" $ do
    it "summarizes environments in environment-expression notation" $ do
      fst (envLogInfo (env "local" []) ResolvedLocal) `shouldBe` "#local"
      fst (envLogInfo (env "docker" [("image", VString "alpine:3.20")]) (ResolvedDocker "alpine:3.20" Map.empty))
        `shouldBe` "#alpine:3.20"
    it "summarizes a recipe by its Dockerfile" $
      fst
        ( envLogInfo
            (env "docker" [("dockerfile", VString "infra/Dockerfile")])
            (ResolvedRecipe "infra/Dockerfile" "infra" Map.empty)
        )
        `shouldBe` "#docker(dockerfile = \"infra/Dockerfile\")"
