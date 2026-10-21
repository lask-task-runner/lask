-- | Shared plumbing for tests that drive the built @lask@ binary as a
-- black box: locating it, laying out a throwaway project, running it,
-- and standing in for a container engine's client.
module Command.Lask.Harness
  ( findLask,
    Result (..),
    runLask,
    runLaskEnv,
    runLaskInTerminal,
    withProject,
    fakeDocker,
    calls,
    withFakeDocker,
    withFakeEngine,
    git,
    gitOut,
  )
where

import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
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

-- | Run the binary on a pseudo-terminal, as a person at a terminal
-- would, typing one line once it has started. @script(1)@ provides the
-- terminal; its options differ between BSD (macOS) and util-linux.
-- The line is sent after a pause, and the input is held open a while
-- longer: sent at once, the end of input can reach the terminal before
-- the line does. What the terminal shows, stdout and stderr alike,
-- comes back as 'resOut'.
runLaskInTerminal :: FilePath -> FilePath -> [String] -> String -> IO Result
runLaskInTerminal lask dir args typed = do
  let command = unwords (map shellQuote (lask : args))
      onTerminal
        | os == "darwin" = "script -q /dev/null " <> command
        | otherwise = "script -qec " <> shellQuote command <> " /dev/null"
      feed = "(sleep 1; printf '%s\\n' " <> shellQuote typed <> "; sleep 2) | " <> onTerminal
  (code, out, err) <- readCreateProcessWithExitCode ((proc "sh" ["-c", feed]) {cwd = Just dir}) ""
  pure (Result (exitOf code) out err)
  where
    shellQuote a = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) a <> "'"
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

-- | A stand-in for a container engine's client — docker, podman or
-- nerdctl, which take the same arguments — for tests that must not need
-- an engine. Its state is files under @$FAKE_DOCKER_STATE@:
-- @registry/<ref>@ holds the digest a tag resolves to upstream,
-- @present/<name>@ an image on the daemon with its repository digests,
-- @calls@ every invocation, and @env@ each variable a @run@ named by
-- @--env NAME@ with the value the client's environment gave it. A file
-- @daemon-down@ makes it answer as a client that reaches no daemon: every
-- command fails. A file @run-fails@ makes a @run@ write its contents to
-- stderr and exit with the code in @run-exit@.
--
-- A reference pulled by its Docker Hub name, as podman is made to pull
-- one, is also found by its short name, as podman's store finds it.
-- An image inspected with @--mode=native@ is answered as nerdctl
-- answers it.
fakeDocker :: String
fakeDocker =
  unlines
    [ "#!/bin/sh",
      "S=\"$FAKE_DOCKER_STATE\"",
      "mkdir -p \"$S/present\" \"$S/registry\"",
      "echo \"$*\" >> \"$S/calls\"",
      "key() { printf '%s' \"$1\" | tr '/:@' '___'; }",
      "repo() { r=\"${1%@*}\"; last=\"${r##*/}\"; case \"$last\" in *:*) r=\"${r%:*}\";; esac; printf '%s' \"$r\"; }",
      "[ -f \"$S/daemon-down\" ] && { echo 'Cannot connect to the Docker daemon' >&2; exit 1; }",
      "case \"$1\" in",
      "  version) echo 27.0.0 ;;",
      "  info) echo amd64 ;;",
      "  pull)",
      "    for a in \"$@\"; do ref=\"$a\"; done",
      "    short=$(printf '%s' \"$ref\" | sed -e 's#^docker.io/library/##' -e 's#^docker.io/##')",
      "    case \"$ref\" in",
      "      *@*) digest=\"${ref#*@}\" ;;",
      "      *) [ -f \"$S/registry/$(key \"$short\")\" ] || { echo \"manifest unknown: $ref\" >&2; exit 1; }",
      "         digest=$(cat \"$S/registry/$(key \"$short\")\") ;;",
      "    esac",
      "    name=\"$(repo \"$ref\")@$digest\"",
      "    for r in \"$ref\" \"$short\" \"$name\" \"$(repo \"$short\")@$digest\"; do printf '[\"%s\"]' \"$name\" > \"$S/present/$(key \"$r\")\"; done",
      "    echo \"$name\" ;;",
      "  image)",
      "    for a in \"$@\"; do ref=\"$a\"; done",
      -- nerdctl's own form: a line per image, @<name> <digest>@, and
      -- nothing for a reference that names none.
      "    if [ \"$3\" = --mode=native ]; then",
      "      f=\"$S/present/$(key \"$ref\")\"; [ -f \"$f\" ] || exit 0",
      "      { tr -d '[]\"' < \"$f\" | tr ',' '\\n'; echo; } | while read -r e; do [ -n \"$e\" ] && echo \"$e ${e#*@}\"; done; exit 0",
      "    fi",
      "    f=\"$S/present/$(key \"$ref\")\"",
      "    [ -f \"$f\" ] || { echo \"Error: No such image: $ref\" >&2; exit 1; }",
      "    [ \"$3\" = \"--format\" ] && cat \"$f\"; exit 0 ;;",
      "  build) tag=''; while [ $# -gt 0 ]; do [ \"$1\" = -t ] && tag=\"$2\"; shift; done",
      "    printf '[]' > \"$S/present/$(key \"$tag\")\" ;;",
      -- A variable named by `--env NAME` is taken from the client's
      -- environment, as docker takes it; @env@ records what it found.
      "  run) prev=''",
      "    [ -f \"$S/run-fails\" ] && { cat \"$S/run-fails\" >&2; exit \"$(cat \"$S/run-exit\")\"; }",
      "    for a in \"$@\"; do",
      "      if [ \"$prev\" = --env ]; then case \"$a\" in *=*) ;; *) eval \"v=\\${$a-UNSET}\"; echo \"$a=$v\" >> \"$S/env\" ;; esac; fi",
      "      prev=\"$a\"",
      "    done",
      "    case \"$*\" in *'sleep 30'*) sleep 30 ;; esac; echo ran ;;",
      "  stop|rm) ;;",
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
withFakeDocker = withFakeEngine "docker"

-- | As 'withFakeDocker', with the fake standing in for the client of
-- the engine named, and @LASK_CONTAINER_ENGINE@ selecting it.
withFakeEngine :: String -> (FilePath -> [(String, String)] -> IO a) -> IO a
withFakeEngine engine action =
  withSystemTempDirectory "fake-engine" $ \root -> do
    let bin = root </> "bin"
        state = root </> "state"
    createDirectoryIfMissing True bin
    createDirectoryIfMissing True (state </> "registry")
    writeFile (bin </> engine) fakeDocker
    _ <- readProcess "chmod" ["+x", bin </> engine] ""
    writeFile (state </> "registry" </> "alpine_3.22.2") "sha256:aaa"
    path <- maybe "" id . lookup "PATH" <$> getEnvironment
    action state [("PATH", bin <> ":" <> path), ("FAKE_DOCKER_STATE", state), ("LASK_CONTAINER_ENGINE", engine)]
