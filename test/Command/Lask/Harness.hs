-- | Shared plumbing for tests that drive the built @lask@ binary as a
-- black box: locating it, laying out a throwaway project, running it,
-- and standing in for the docker CLI.
module Command.Lask.Harness
  ( findLask,
    Result (..),
    runLask,
    runLaskEnv,
    withProject,
    fakeDocker,
    calls,
    withFakeDocker,
    git,
    gitOut,
  )
where

import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode, readProcess)

-- | Run git in a repository as a throwaway identity, failing the test
-- if it fails.
git :: FilePath -> [String] -> IO ()
git repo args = () <$ gitOut repo args

-- | 'git', returning its standard output without the final newline.
gitOut :: FilePath -> [String] -> IO String
gitOut repo args = do
  (code, out, err) <-
    readCreateProcessWithExitCode
      ((proc "git" (["-c", "user.email=t@example.com", "-c", "user.name=t"] <> args)) {cwd = Just repo})
      ""
  case code of
    ExitSuccess -> pure (takeWhile (/= '\n') out)
    ExitFailure _ -> fail ("git " <> unwords args <> ": " <> err)

-- | Locate the freshly built binary via stack.
findLask :: IO FilePath
findLask = do
  stackBin <- findExecutable "stack"
  case stackBin of
    Nothing -> fail "stack not found"
    Just _ -> do
      root <- readProcess "stack" ["path", "--local-install-root"] ""
      pure (takeWhile (/= '\n') root </> "bin" </> "lask")

data Result = Result
  { resExit :: Int,
    resOut :: String,
    resErr :: String
  }
  deriving (Show, Eq)

runLask :: FilePath -> FilePath -> [String] -> String -> IO Result
runLask lask dir = runLaskEnv lask dir []

-- | Run the binary with extra environment variables (e.g.
-- @LASK_CACHE_DIR@ for hermetic dependency tests).
runLaskEnv :: FilePath -> FilePath -> [(String, String)] -> [String] -> String -> IO Result
runLaskEnv lask dir extraEnv args input = do
  baseEnv <- getEnvironment
  let fullEnv = extraEnv <> [(k, v) | (k, v) <- baseEnv, k `notElem` map fst extraEnv]
  (code, out, err) <-
    readCreateProcessWithExitCode ((proc lask args) {cwd = Just dir, env = Just fullEnv}) input
  pure (Result (exitOf code) out err)
  where
    exitOf ExitSuccess = 0
    exitOf (ExitFailure n) = n

withProject :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withProject files action =
  withSystemTempDirectory "lask-e2e" $ \dir -> do
    mapM_
      ( \(name, content) -> do
          createDirectoryIfMissing True (takeDirectory (dir </> name))
          writeFile (dir </> name) content
      )
      files
    action dir

-- | A stand-in for the docker CLI, for tests of image pinning that must
-- not need a daemon. Its state is files under @$FAKE_DOCKER_STATE@:
-- @registry/<ref>@ holds the digest a tag resolves to upstream,
-- @present/<name>@ an image on the daemon with its repository digests,
-- and @calls@ every invocation.
fakeDocker :: String
fakeDocker =
  unlines
    [ "#!/bin/sh",
      "S=\"$FAKE_DOCKER_STATE\"",
      "mkdir -p \"$S/present\" \"$S/registry\"",
      "echo \"$*\" >> \"$S/calls\"",
      "key() { printf '%s' \"$1\" | tr '/:@' '___'; }",
      "repo() { r=\"${1%@*}\"; last=\"${r##*/}\"; case \"$last\" in *:*) r=\"${r%:*}\";; esac; printf '%s' \"$r\"; }",
      "case \"$1\" in",
      "  version) echo 27.0.0 ;;",
      "  pull)",
      "    ref=\"$3\"",
      "    case \"$ref\" in",
      "      *@*) digest=\"${ref#*@}\" ;;",
      "      *) [ -f \"$S/registry/$(key \"$ref\")\" ] || { echo \"manifest unknown: $ref\" >&2; exit 1; }",
      "         digest=$(cat \"$S/registry/$(key \"$ref\")\") ;;",
      "    esac",
      "    name=\"$(repo \"$ref\")@$digest\"",
      "    printf '[\"%s\"]' \"$name\" > \"$S/present/$(key \"$ref\")\"",
      "    printf '[\"%s\"]' \"$name\" > \"$S/present/$(key \"$name\")\"",
      "    echo \"$name\" ;;",
      "  image)",
      "    ref=\"$5\"; [ \"$3\" = \"--format\" ] || ref=\"$3\"",
      "    f=\"$S/present/$(key \"$ref\")\"",
      "    [ -f \"$f\" ] || { echo \"Error: No such image: $ref\" >&2; exit 1; }",
      "    [ \"$3\" = \"--format\" ] && cat \"$f\"; exit 0 ;;",
      "  build) tag=''; while [ $# -gt 0 ]; do [ \"$1\" = -t ] && tag=\"$2\"; shift; done",
      "    printf '[]' > \"$S/present/$(key \"$tag\")\" ;;",
      "  run) echo ran ;;",
      "  *) echo \"fake docker: unsupported: $*\" >&2; exit 2 ;;",
      "esac"
    ]

-- | The invocations the fake docker has recorded, one per line.
calls :: FilePath -> IO [String]
calls state = lines <$> readFile (state </> "calls")

-- | Run an action with the fake docker first on PATH and upstream
-- @alpine:3.22.2@ resolving to @sha256:aaa@. The action receives the
-- state directory and the extra environment to run lask with.
withFakeDocker :: (FilePath -> [(String, String)] -> IO a) -> IO a
withFakeDocker action =
  withSystemTempDirectory "fake-docker" $ \root -> do
    let bin = root </> "bin"
        state = root </> "state"
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (state </> "registry")
    writeFile (bin </> "docker") fakeDocker
    _ <- readProcess "chmod" ["+x", bin </> "docker"] ""
    writeFile (state </> "registry" </> "alpine_3.22.2") "sha256:aaa"
    path <- maybe "" id . lookup "PATH" <$> getEnvironment
    action state [("PATH", bin <> ":" <> path), ("FAKE_DOCKER_STATE", state)]
