{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Runtime.EnvironmentSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Lask.Builtins.Impl (FileOp (..))
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Image (ImagePins, recipeTag, unlockedPins)
import Language.Lask.Runtime.Secrets (registerSecret, resetSecretRegistryForTests)
import Language.Lask.Runtime.Value
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | These runners run on the host, where no image is involved.
noPins :: ImagePins
noPins = unlockedPins Map.empty

env :: Text -> [(Text, Value)] -> EnvValue
env k ps = EnvValue k (Map.fromList ps)

-- | A successful operation returning Void. It cannot be compared with
-- '==': Void is not a comparable type (spec 6.2), so 'Eq' 'Value'
-- reports even two Voids unequal.
returnsVoid :: Either LaskFailure Value -> Expectation
returnsVoid r = case r of
  Right VVoid -> pure ()
  other -> expectationFailure ("expected Void, got " <> show other)

failsWithFs :: Either LaskFailure Value -> Expectation
failsWithFs r = case r of
  Left lf -> lfCode lf `shouldBe` Just EIoFs
  Right v -> expectationFailure ("expected E-IO-FS, got " <> show v)

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
                     "--mount", "type=bind,source=/proj,target=/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "--memory", "4g",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

    it "attaches stdin for a container write, so no content rides on the command line" $
      dockerShellArgs "/proj" "alpine:3.20" Map.empty True "cat > 'out.txt'"
        `shouldBe` [ "run", "--rm",
                     "-i",
                     "--mount", "type=bind,source=/proj,target=/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "alpine:3.20",
                     "-c", "cat > 'out.txt'"
                   ]

    -- A Windows base directory always carries a drive letter, so the
    -- colon-separated `-v` form read `C` as the source and `/work` as
    -- the mode, and the daemon answered `invalid mode: /work`.
    it "keeps a Windows drive letter in the source, not in the mount separator" $
      dockerArgs "C:\\proj" "alpine:3.20" Map.empty "uname -a"
        `shouldBe` [ "run", "--rm",
                     "--mount", "type=bind,source=C:\\proj,target=/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

    -- The same defect on a POSIX host: a colon is legal in a directory
    -- name there, so this is reachable without Windows at all.
    it "keeps a colon inside a POSIX base directory out of the mount separator" $
      dockerArgs "/tmp/a:b" "alpine:3.20" Map.empty "uname -a"
        `shouldBe` [ "run", "--rm",
                     "--mount", "type=bind,source=/tmp/a:b,target=/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

  describe "container options (spec 10.2)" $ do
    let opts ps = dockerArgs "/proj" "alpine:3.20" (Map.fromList ps) "uname -a"
        -- Just the part between the fixed prologue (run, the mount,
        -- the default -w and the entrypoint) and the image.
        optionArgs ps = takeWhile (/= "alpine:3.20") (drop 8 (opts ps))

    it "passes scalar options as one flag each" $
      optionArgs
        [ ("memory", VString "4g"),
          ("cpus", VNumber 1.5),
          ("pids_limit", VNumber 256),
          ("user", VString "1000:1000"),
          ("platform", VString "linux/amd64"),
          ("network", VString "none")
        ]
        `shouldBe` [ "--cpus", "1.5",
                     "--memory", "4g",
                     "--network", "none",
                     "--pids-limit", "256",
                     "--platform", "linux/amd64",
                     "--user", "1000:1000"
                   ]

    -- One environment value has to produce one argument vector, or a
    -- container could differ between two runs of the same task.
    it "emits options in name order, not in the order they were given" $
      optionArgs [("memory", VString "4g"), ("cpus", VNumber 2)]
        `shouldBe` optionArgs [("cpus", VNumber 2), ("memory", VString "4g")]

    it "repeats the flag for a list option" $
      optionArgs [("tmpfs", VArray (V.fromList [VString "/tmp", VString "/run"]))]
        `shouldBe` ["--tmpfs", "/tmp", "--tmpfs", "/run"]

    it "joins a table option into the form its flag expects" $ do
      optionArgs [("env", VMap (Map.fromList [("CI", VString "1"), ("LANG", VString "C")]))]
        `shouldBe` ["--env", "CI=1", "--env", "LANG=C"]
      optionArgs [("add_hosts", VMap (Map.fromList [("api", VString "10.0.0.2")]))]
        `shouldBe` ["--add-host", "api:10.0.0.2"]

    -- Null is how an argument says "not given" (spec 10.2); "" is a
    -- value the caller means, and is passed on.
    it "leaves out an option given null, and the null elements and values of a list or table" $ do
      optionArgs [("user", VNull), ("init", VNull), ("cpus", VNull)] `shouldBe` []
      optionArgs
        [ ("env", VMap (Map.fromList [("A", VNull), ("B", VString "")])),
          ("tmpfs", VArray (V.fromList [VNull, VString "/t"]))
        ]
        `shouldBe` ["--env", "B=", "--tmpfs", "/t"]

    it "says nothing for a switch left false, which is the daemon's own default" $ do
      optionArgs [("init", VBool True), ("read_only", VBool True)]
        `shouldBe` ["--init", "--read-only"]
      optionArgs [("init", VBool False), ("read_only", VBool False)]
        `shouldBe` []

    -- 10.5 gives an explicit working directory precedence over the
    -- default, and the daemon takes the last -w it is given.
    it "puts an explicit workdir after the mounted default so it wins" $
      opts [("workdir", VString "/work/web")]
        `shouldBe` [ "run", "--rm",
                     "--mount", "type=bind,source=/proj,target=/work",
                     "-w", "/work",
                     "--entrypoint", "/bin/sh",
                     "-w", "/work/web",
                     "alpine:3.20",
                     "-c", "uname -a"
                   ]

    -- Build arguments belong to the recipe hash (10.3), not to the
    -- container the image is then run in.
    it "does not pass the image reference or build arguments as run options" $
      optionArgs
        [ ("image", VString "alpine:3.20"),
          ("build_args", VMap (Map.fromList [("VERSION", VString "1.2.3")]))
        ]
        `shouldBe` []

  describe "recipe hashing (spec 10.3)" $ do
    let tagFor buildArgs = withSystemTempDirectory "lask-recipe" $ \dir -> do
          writeFile (dir <> "/Dockerfile") "FROM alpine:3.20\n"
          recipeTag dir "Dockerfile" "." buildArgs

    it "covers the declared build arguments" $ do
      a <- tagFor []
      b <- tagFor [("VERSION", "1.2.3")]
      a `shouldSatisfy` isRight
      b `shouldNotBe` a

    it "does not depend on the order the build arguments were written in" $ do
      a <- tagFor [("VERSION", "1.2.3"), ("FLAVOUR", "slim")]
      b <- tagFor [("FLAVOUR", "slim"), ("VERSION", "1.2.3")]
      b `shouldBe` a

  describe "local execution (spec 8.7, real process)" $ do
    it "runs a local command and captures streams and exit code" $ do
      runner <- mkCommandRunner noPins "/tmp" noCommandLog
      r <- runner (env "local" []) "echo out; echo err 1>&2; exit 3"
      case r of
        Right (code, out, errOut) -> do
          code `shouldBe` 3
          out `shouldBe` "out\n"
          errOut `shouldBe` "err\n"
        Left lf -> expectationFailure (show lf)
    it "uses the base directory as the working directory" $ do
      runner <- mkCommandRunner noPins "/tmp" noCommandLog
      r <- runner (env "local" []) "pwd"
      case r of
        Right (0, out, _) -> out `shouldSatisfy` (\o -> o == "/tmp\n" || o == "/private/tmp\n")
        other -> expectationFailure (show other)

  describe "command execution log relay (spec 12.3)" $ do
    let mkLoggedRunner = do
          logRef <- newIORef []
          -- The runner relays stdout and stderr from two concurrent
          -- threads, so a sink must be atomic: a plain read-modify-write
          -- here loses entries.
          let sink cl = atomicModifyIORef' logRef (\ls -> (ls <> [cl], ()))
          runner <- mkCommandRunner noPins "/tmp" sink
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
      listToMaybe (kinds entries) `shouldBe` Just ClStart
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

  describe "filesystem functions (spec 15.11, real filesystem)" $ do
    let withProject act = withSystemTempDirectory "lask-fs" $ \dir -> do
          runner <- mkFileRunner noPins dir
          act dir (\op -> runner (env "local" []) op)

    it "writes a file and reads it back" $ withProject $ \_ run -> do
      run (FileWrite "note.txt" "hello\n") >>= returnsVoid
      run (FileRead "note.txt") `shouldReturn` Right (VString "hello\n")

    it "resolves a relative path against the base directory" $ withProject $ \dir run -> do
      _ <- run (FileWrite "note.txt" "x")
      doesFileExist (dir <> "/note.txt") `shouldReturn` True

    it "reports a read of a missing file as E-IO-FS" $ withProject $ \_ run -> do
      run (FileRead "absent.txt") >>= failsWithFs

    it "answers file_exists without failing on a missing path" $ withProject $ \_ run -> do
      run (FileExists "absent.txt") `shouldReturn` Right (VBool False)
      _ <- run (FileWrite "there.txt" "x")
      run (FileExists "there.txt") `shouldReturn` Right (VBool True)

    it "creates parents with make_dir and refuses them in write_file" $ withProject $ \_ run -> do
      run (FileWrite "sub/deep/note.txt" "x") >>= failsWithFs
      run (FileMakeDir "sub/deep") >>= returnsVoid
      run (FileWrite "sub/deep/note.txt" "x") >>= returnsVoid

    it "removes a file, and removing what is absent succeeds" $ withProject $ \_ run -> do
      _ <- run (FileWrite "gone.txt" "x")
      run (FileRemove "gone.txt") >>= returnsVoid
      run (FileRemove "gone.txt") >>= returnsVoid
      run (FileExists "gone.txt") `shouldReturn` Right (VBool False)

    it "refuses to remove a directory" $ withProject $ \_ run -> do
      _ <- run (FileMakeDir "adir")
      run (FileRemove "adir") >>= failsWithFs

    it "lists a directory in a deterministic order" $ withProject $ \_ run -> do
      _ <- run (FileMakeDir "d")
      mapM_ (\n -> run (FileWrite ("d/" <> n) "x")) ["c.txt", "a.txt", "b.txt"]
      run (FileListDir "d")
        `shouldReturn` Right (VArray (V.fromList (map VString ["a.txt", "b.txt", "c.txt"])))

    it "reports list_dir on a non-directory as E-IO-FS" $ withProject $ \_ run -> do
      _ <- run (FileWrite "afile" "x")
      run (FileListDir "afile") >>= failsWithFs

    it "globs across components and hides dot entries" $ withProject $ \dir run -> do
      createDirectoryIfMissing True (dir <> "/src/lib")
      createDirectoryIfMissing True (dir <> "/.git")
      mapM_ (\n -> run (FileWrite n "x"))
        ["src/main.lask", "src/lib/util.lask", "src/notes.md", ".git/config.lask"]
      run (FileGlob "src/**/*.lask")
        `shouldReturn` Right (VArray (V.fromList (map VString ["src/lib/util.lask", "src/main.lask"])))
      run (FileGlob "**/*.lask")
        `shouldReturn` Right (VArray (V.fromList (map VString ["src/lib/util.lask", "src/main.lask"])))

    it "returns no match as an empty array rather than a failure" $ withProject $ \_ run -> do
      run (FileGlob "nowhere/*.lask") `shouldReturn` Right (VArray V.empty)
      run (FileGlob "*.absent") `shouldReturn` Right (VArray V.empty)
