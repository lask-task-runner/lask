{-# LANGUAGE OverloadedStrings #-}

-- | Every error code of spec chapter 14, raised end to end through the
-- built @lask@ binary with the exit code of spec 11.3.
--
-- 'trigger' is a total function over 'ErrorCode', so a code added to
-- the enumeration without a way to raise it here is an
-- incomplete-pattern warning rather than a code no test ever sees.
module Command.Lask.ErrorCodeSpec (spec) where

import Command.Lask.Harness
import Control.Monad (void)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Language.Lask.ErrorCode (ErrorCode (..), codeText)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)
import Test.Hspec

-- | How to make the binary report one error code.
data Trigger
  = -- | @lask check@ on these files: exit 1, the code on stdout.
    Check [(FilePath, String)]
  | -- | These arguments against @main.lask@: the code on stderr.
    Invoke String [String] Int
  | -- | A scenario that needs more than one step. It receives the
    -- binary and returns the final run; the code may be on either
    -- stream.
    Scripted Int (FilePath -> IO Result)

trigger :: ErrorCode -> Trigger
trigger c = case c of
  -- Syntax and static errors (spec 14.4) stop before evaluation.
  ESyntaxUnexpectedToken -> check "x = = 1\n"
  ESyntaxReturnPosition ->
    check "f(x: String): String = do {\n  y = case (x) {\n    \"a\" -> do { return \"e\" }\n    else -> \"z\"\n  }\n  y\n}\n"
  ESyntaxCaseElse -> check "f(x: String) = case (x) {\n  \"a\" -> 1\n}\n"
  ENameUndefined -> check "x = nope\n"
  ENameDuplicate -> check "a = 1\na = 2\n"
  ETypeMismatch -> check "x: Number = \"s\"\n"
  ETypeArity -> check "f(a: Number) = a\nx = f(1, 2)\n"
  ETypeCall -> check "x = 1\ny = x(2)\n"
  ETypeCommandEnv -> check "b(e: Number) = $[e] ls\n"
  ETypeCommandNoEnv -> check "v(): String = $ ls\n"
  ETypeCommandConflict ->
    check "command { \"go\" } on #golang:1.25\ncommand { \"npm\" } on #node:20\nv() = $ go build && npm ci\n"
  ETypeCommandEffect -> check "command { \"go\" } on #docker(\"golang:#{stdin}\")\nv() = $ go vet\n"
  ETypeCommandName -> check "command { \"my prog\" } on #local\nv() = $ ls\n"
  ETypeCommandDuplicate -> check "command { \"go\" } on #local\ncommand { \"go\" } on #golang:1.25\n"
  ETypeEnvConstruct -> check "e = #docker()\n"
  ETypeAccess -> check "u = {name: \"a\"}\nn = u.nope\n"
  ETypeFieldDuplicate -> check "r = {a: 1, a: 2}\n"
  ETypeCaseDuplicate -> check "f(x: String) = case (x) {\n  \"a\" -> 1\n  \"b\", \"a\" -> 2\n  else -> 3\n}\n"
  ETypeKeyword -> check "inc(--x = 0) = x + 1\na = inc(y = 1)\n"
  ETypeIllformed -> check "type T = Array<T>\n"
  ETypeSecretNonString -> check "f(): Number = do {\n  p!!: Number = 1\n  p\n}\n"
  EModuleCycle ->
    Check
      [ ("main.lask", "import { a } from \"./a.lask\"\nx = a\n"),
        ("a.lask", "import { x } from \"./main.lask\"\na = x\n")
      ]
  EModuleUnresolved -> check "import { x } from \"kit\"\ny = x\n"
  EModuleDeepImport -> Scripted 1 $ \lask ->
    withGitDep lask $ \_ proj run -> do
      writeFile (proj </> "main.lask") "import { x } from \"kit/sub.lask\"\nf() = x\n"
      run ["check"]
  EModuleLockStale -> Scripted 1 $ \lask ->
    withUrlDep lask $ \_ proj run -> do
      -- The project file now asks for a source the lock never saw.
      writeFile (proj </> "lask.json") "{\"dependencies\": {\"notify\": {\"url\": \"file:///elsewhere.lask\"}}}"
      run ["check"]
  EModuleRevMoved -> Scripted 3 $ \lask ->
    withGitDep lask $ \repo _ run -> do
      -- The first sync pins the commit the tag names.
      void (run ["deps", "sync"])
      writeFile (repo </> "main.lask") "hello(): String = \"moved\"\n"
      git repo ["commit", "--quiet", "-am", "move"]
      git repo ["tag", "--force", "v1"]
      run ["deps", "sync"]
  EModuleHashMismatch -> Scripted 3 $ \lask ->
    withUrlDep lask $ \published _ run -> do
      writeFile published "send(x: String): String = concat(\"evil:\", x)\n"
      run ["deps", "sync"]
  -- Runtime errors (spec 14.5) default to exit 2; a failed command
  -- passes its own exit code through.
  ERuntimeDivByZero -> Invoke "f() = 1 / 0\n" ["eval", "f"] 2
  ERuntimeCommandNonzero -> Invoke "f() = $[#local] exit 42\n" ["run", "f"] 42
  ERuntimeAccess -> Invoke "xs = [1]\nf() = xs[5]\n" ["eval", "f"] 2
  ERuntimeCast ->
    Invoke "pick(r: Record<a: Number>): Number = r.a\nf() = pick(cast(from_json(\"{\\\"a\\\": \\\"s\\\"}\")))\n" ["eval", "f"] 2
  ERuntimeValue -> Invoke "f() = sqrt(0 - 1)\n" ["eval", "f"] 2
  ERuntimeRegex -> Invoke "f() = regex_test(\"a\", \"(\")\n" ["eval", "f"] 2
  -- External I/O errors (spec 14.6) default to exit 3.
  EIoStdinRead -> Scripted 3 $ \lask ->
    withProject [("main.lask", "f(): String = stdin\n")] $ \dir -> do
      -- Bytes that are not UTF-8, which a String input cannot carry.
      (code, out, err) <-
        readCreateProcessWithExitCode
          ((proc "sh" ["-c", "printf '\\377\\376' | \"$0\" eval f", lask]) {cwd = Just dir})
          ""
      pure (Result (exitOf code) out err)
  EIoEnvResolve -> Invoke "f(--img: String = \"\") = $[#docker(img)] ls\n" ["eval", "f"] 3
  EIoImageMissing -> Scripted 3 $ \lask ->
    withFakeDocker $ \_ extra -> withProject pinned $ \dir ->
      runLaskEnv lask dir extra ["eval", "hi"] ""
  EIoImageDigest -> Scripted 3 $ \lask ->
    withFakeDocker $ \state extra -> withProject pinned $ \dir -> do
      void (runLaskEnv lask dir extra ["env", "build"] "")
      writeFile (state </> "present" </> "alpine_sha256_aaa") "[\"alpine@sha256:ccc\"]"
      runLaskEnv lask dir extra ["env", "build"] ""
  EIoFs -> Invoke "f() = read_file(\"nope.txt\", #local)\n" ["eval", "f"] 3
  EIoDataDecode -> Invoke "f() = from_json(\"{oops\")\n" ["eval", "f"] 3
  -- CLI usage errors (spec 11.3).
  ECliUsage -> Invoke "f(n: Number) = n\n" ["eval", "f", "abc"] 4
  where
    check src = Check [("main.lask", src)]
    pinned = [("main.lask", "command { \"cat\" } on #alpine:3.22.2\nhi(): String = $ cat x\n")]

-- | A project depending on a single-file module published at a
-- @file://@ URL. The action receives the published file, the project
-- directory, and a runner with an isolated module cache.
withUrlDep :: FilePath -> (FilePath -> FilePath -> ([String] -> IO Result) -> IO a) -> IO a
withUrlDep lask action =
  withSystemTempDirectory "lask-errcode" $ \root -> do
    let published = root </> "notify.lask"
        proj = root </> "proj"
        run args = runLaskEnv lask proj [("LASK_CACHE_DIR", root </> "cache")] args ""
    createDirectoryIfMissing True proj
    writeFile published "send(x: String): String = x\n"
    writeFile (proj </> "main.lask") "import { send } from \"notify\"\nf(): String = send(\"a\")\n"
    added <- run ["deps", "add", "notify", "--url", "file://" <> published]
    resExit added `shouldBe` 0
    -- Resolution reads the cache, so an empty one forces a refetch.
    removeDirectoryRecursive (root </> "cache")
    action published proj run

-- | A project depending on tag @v1@ of a local git repository. The
-- action receives the repository, the project directory, and a runner
-- with an isolated module cache.
withGitDep :: FilePath -> (FilePath -> FilePath -> ([String] -> IO Result) -> IO a) -> IO a
withGitDep lask action =
  withSystemTempDirectory "lask-errcode-git" $ \root -> do
    let repo = root </> "repo"
        proj = root </> "proj"
        run args = runLaskEnv lask proj [("LASK_CACHE_DIR", root </> "cache")] args ""
    createDirectoryIfMissing True repo
    createDirectoryIfMissing True proj
    writeFile (repo </> "main.lask") "hello(): String = \"hi\"\n"
    writeFile (repo </> "sub.lask") "x: String = \"x\"\n"
    git repo ["init", "--quiet"]
    git repo ["add", "."]
    git repo ["commit", "--quiet", "-m", "init"]
    git repo ["tag", "v1"]
    writeFile (proj </> "main.lask") "import { hello } from \"kit\"\nf() = hello()\n"
    added <- run ["deps", "add", "kit", "--git", "file://" <> repo, "--rev", "v1"]
    resExit added `shouldBe` 0
    action repo proj run

exitOf :: ExitCode -> Int
exitOf ExitSuccess = 0
exitOf (ExitFailure n) = n

spec :: Spec
spec = beforeAll findLask $
  describe "error codes (spec 14)" $
    mapM_ (\c -> it (T.unpack (codeText c)) (raises c)) [minBound .. maxBound]

raises :: ErrorCode -> FilePath -> Expectation
raises c lask = case trigger c of
  Check files -> withProject files $ \dir -> do
    r <- runLask lask dir ["check"] ""
    resExit r `shouldBe` 1
    resOut r `shouldSatisfy` isInfixOf code
  Invoke src args expected -> withProject [("main.lask", src)] $ \dir -> do
    r <- runLask lask dir args ""
    resExit r `shouldBe` expected
    resErr r `shouldSatisfy` isInfixOf code
  Scripted expected scenario -> do
    r <- scenario lask
    resExit r `shouldBe` expected
    (resOut r <> resErr r) `shouldSatisfy` isInfixOf code
  where
    code = T.unpack (codeText c)
