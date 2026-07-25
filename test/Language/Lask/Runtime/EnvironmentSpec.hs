{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EnvironmentSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.EnvFile
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Value
import Test.Hspec

envFile :: EnvFile
envFile =
  EnvFile . Map.fromList $
    [ ("ansible", EnvEntry "remote" (Map.fromList [("host", VString "203.0.113.10"), ("user", VString "automation")])),
      ("builder", EnvEntry "docker" (Map.fromList [("image", VString "golang:1.22")]))
    ]

env :: Text -> [(Text, Value)] -> EnvValue
env k ps = EnvValue k (Map.fromList ps)

spec :: Spec
spec = do
  describe "environment file parsing (spec 10.3)" $ do
    it "parses valid files" $ do
      let json =
            "{\"environments\": {\"ansible\": {\"kind\": \"remote\", \"params\": {\"host\": \"203.0.113.10\", \"user\": \"automation\"}},\
            \ \"builder\": {\"kind\": \"docker\", \"params\": {\"image\": \"golang:1.22\"}}}}"
      parseEnvFile (BL8.pack json) `shouldBe` Right envFile
    it "rejects chained env kinds" $
      parseEnvFile "{\"environments\": {\"a\": {\"kind\": \"env\", \"params\": {\"name\": \"b\"}}}}"
        `shouldSatisfy` isLeft
    it "rejects unknown kinds" $
      parseEnvFile "{\"environments\": {\"a\": {\"kind\": \"cloud\", \"params\": {}}}}"
        `shouldSatisfy` isLeft
    it "rejects remote entries without host" $
      parseEnvFile "{\"environments\": {\"a\": {\"kind\": \"remote\", \"params\": {\"user\": \"u\"}}}}"
        `shouldSatisfy` isLeft
    it "rejects docker entries without image" $
      parseEnvFile "{\"environments\": {\"a\": {\"kind\": \"docker\", \"params\": {}}}}"
        `shouldSatisfy` isLeft
    it "rejects unknown params" $
      parseEnvFile "{\"environments\": {\"a\": {\"kind\": \"remote\", \"params\": {\"host\": \"h\", \"password\": \"nope\"}}}}"
        `shouldSatisfy` isLeft
    it "rejects non-identifier names" $
      parseEnvFile "{\"environments\": {\"Bad-Name\": {\"kind\": \"local\", \"params\": {}}}}"
        `shouldSatisfy` isLeft

  describe "environment resolution (spec 10.4)" $ do
    it "resolves local" $
      resolveEnv Nothing (env "local" []) `shouldBe` Right ResolvedLocal
    it "resolves docker with image" $
      resolveEnv Nothing (env "docker" [("image", VString "alpine:3.20")])
        `shouldBe` Right (ResolvedDocker "alpine:3.20" Map.empty)
    it "substitutes named environments from the file" $
      resolveEnv (Just envFile) (env "env" [("name", VString "ansible")])
        `shouldBe` Right (ResolvedRemote "203.0.113.10" (Just "automation") Nothing)
    it "fails on undefined names with E-IO-ENV-RESOLVE" $
      case resolveEnv (Just envFile) (env "env" [("name", VString "nope")]) of
        Left lf -> lfCode lf `shouldBe` Just EIoEnvResolve
        Right r -> expectationFailure (show r)
    it "fails without a file when env is referenced" $
      resolveEnv Nothing (env "env" [("name", VString "x")]) `shouldSatisfy` isLeft

  describe "launch argument construction (spec 10.5, 10.9)" $ do
    it "builds docker run arguments with mounted workdir" $
      dockerArgs "/proj" "alpine:3.20" (Map.fromList [("memory", VString "4g")]) "uname -a"
        `shouldBe` [ "run", "--rm", "-v", "/proj:/work", "-w", "/work",
                     "--memory", "4g", "alpine:3.20", "/bin/sh", "-c", "uname -a"
                   ]
    it "builds ssh arguments with strict host key checking by default" $
      sshArgs defaultSshSettings "h.example" (Just "u") (Just 2222) "ls"
        `shouldBe` [ "-o", "BatchMode=yes",
                     "-o", "StrictHostKeyChecking=yes",
                     "-p", "2222", "u@h.example", "ls"
                   ]
    it "applies ssh settings" $
      sshArgs (SshSettings (Just "/kh") (Just "accept-new") (Just 10)) "h" Nothing Nothing "ls"
        `shouldBe` [ "-o", "BatchMode=yes",
                     "-o", "StrictHostKeyChecking=accept-new",
                     "-o", "UserKnownHostsFile=/kh",
                     "-o", "ConnectTimeout=10", "h", "ls"
                   ]

  describe "local execution (spec 8.7, real process)" $ do
    it "runs a local command and captures streams and exit code" $ do
      runner <- mkCommandRunner "/tmp" Nothing defaultSshSettings noCommandLog
      r <- runner (env "local" []) "echo out; echo err 1>&2; exit 3"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 3
          out `shouldBe` "out\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
    it "uses the base directory as the working directory" $ do
      runner <- mkCommandRunner "/tmp" Nothing defaultSshSettings noCommandLog
      r <- runner (env "local" []) "pwd"
      case r of
        Right (0, out, _) -> out `shouldSatisfy` (\o -> o == "/tmp\n" || o == "/private/tmp\n")
        other -> expectationFailure (show other)

  describe "command execution log relay (spec 12.3)" $ do
    let mkLoggedRunner = do
          logRef <- newIORef []
          let sink cl = modifyIORef' logRef (<> [cl])
          runner <- mkCommandRunner "/tmp" Nothing defaultSshSettings sink
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

  describe "environment log info (spec 12.3)" $ do
    it "summarizes environments in environment-expression notation" $ do
      fst (envLogInfo (env "local" []) ResolvedLocal) `shouldBe` "#local"
      fst (envLogInfo (env "docker" [("image", VString "alpine:3.20")]) (ResolvedDocker "alpine:3.20" Map.empty))
        `shouldBe` "#alpine:3.20"
    it "uses the reference for named environments" $
      fst
        ( envLogInfo
            (env "env" [("name", VString "ansible")])
            (ResolvedRemote "203.0.113.10" (Just "automation") Nothing)
        )
        `shouldBe` "#env(\"ansible\")"
