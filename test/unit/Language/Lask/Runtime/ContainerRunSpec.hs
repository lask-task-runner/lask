{-# LANGUAGE OverloadedStrings #-}

-- | Running in a container environment (spec 10, 15.5, 15.11), with a
-- backend that runs what it is asked to run in a container on the host
-- instead, in the base directory: a container run is its entrypoint
-- and command, started there, and an @exec@ is its command, with the
-- variables it is given. What reaches the engine is then real
-- processes, so the filesystem functions, the sessions of
-- @with_container@ and @lask cmd@ are exercised whole, with no engine.
module Language.Lask.Runtime.ContainerRunSpec (spec) where

import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString as BS
import Control.Exception (throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import Language.Lask.Builtins.Impl (FileOp (..))
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog (noCommandLog)
import Language.Lask.Runtime.Container
import Language.Lask.Runtime.Environment
import Language.Lask.Runtime.Image (lockedPins, unlockedPins)
import Language.Lask.Runtime.Value
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe)
import System.Process (CreateProcess (..), proc)
import System.Timeout (timeout)
import Test.Hspec

-- | The containers the host backend was asked to stop.
newtype Seen = Seen {seenStops :: IORef [Text]}

-- | A backend that runs a container's command, and what is run in a
-- session's container, on the host in the base directory; every image
-- is in its store.
hostBackend :: FilePath -> IO (ContainerBackend, Seen)
hostBackend dir = do
  stops <- newIORef []
  inherited <- getEnvironment
  let b =
        (cliBackend Docker)
          { cbImageExists = const (pure True),
            cbReachable = pure (Right ()),
            cbRunProcess = \s ->
              pure ((proc (T.unpack (runEntrypoint s)) (map T.unpack (runCommand s))) {cwd = Just (runBaseDir s)}, pure ()),
            cbExecProcess = \e ->
              (proc (T.unpack (T.concat (take 1 (execCommand e)))) (map T.unpack (drop 1 (execCommand e))))
                { cwd = Just dir,
                  env = Just ([(T.unpack k, T.unpack v) | (k, v) <- execEnv e] <> inherited)
                },
            cbStop = \_ name -> atomicModifyIORef' stops (\ns -> (ns <> [name], ())),
            cbRunFailure = \code err -> if code == 125 then Just (T.strip err) else Nothing
          }
  pure (b, Seen stops)

alpine :: EnvValue
alpine = EnvValue "docker" (Map.fromList [("image", VString "alpine:3.20")])

withOptions :: [(Text, Value)] -> EnvValue
withOptions os = EnvValue "docker" (Map.fromList (("image", VString "alpine:3.20") : os))

failsWith :: ErrorCode -> Either LaskFailure a -> Expectation
failsWith c r = either (\lf -> lfCode lf `shouldBe` Just c) (const (expectationFailure ("expected " <> show c))) r

messageOf :: LaskFailure -> Text
messageOf lf = case lfError lf of
  VRecord m | Just (VString t) <- Map.lookup "message" m -> t
  _ -> ""

isVoid :: Either LaskFailure Value -> Bool
isVoid r = case r of
  Right VVoid -> True
  _ -> False

strings :: [Text] -> Either LaskFailure Value
strings = Right . VArray . V.fromList . map VString

-- | Run an action in a fresh base directory with the file runner and
-- the command runner of the host backend.
inBase :: (FilePath -> ContainerBackend -> Seen -> IO a) -> IO a
inBase act = withSystemTempDirectory "container-run" $ \dir -> do
  (b, seen) <- hostBackend dir
  act dir b seen

-- | Open a session on the environment and run the body with it.
session :: ContainerBackend -> FilePath -> EnvValue -> (EnvValue -> IO Value) -> IO Value
session b dir e body = do
  opener <- mkSessionOpener b (unlockedPins Map.empty) dir
  opener (const (pure ())) e body

spec :: Spec
spec = do
  describe "the filesystem functions in a container (spec 15.11)" $ do
    it "reads, writes, tests, lists, globs, makes and removes through the container's shell" $ inBase $ \dir b _ -> do
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      r1 <- files alpine (FileWrite "out.txt" "héllo\n")
      r1 `shouldSatisfy` isVoid
      files alpine (FileRead "out.txt") `shouldReturn` Right (VString "héllo\n")
      files alpine (FileExists "out.txt") `shouldReturn` Right (VBool True)
      files alpine (FileExists "no.txt") `shouldReturn` Right (VBool False)
      files alpine (FileMakeDir "src/deep") >>= (`shouldSatisfy` isVoid)
      mapM_ (\f -> files alpine (FileWrite f "")) ["src/b.txt", "src/a.txt", "src/deep/c.txt", "src/x.md"]
      files alpine (FileListDir "src") `shouldReturn` strings ["a.txt", "b.txt", "deep", "x.md"]
      files alpine (FileGlob "src/*.txt") `shouldReturn` strings ["src/a.txt", "src/b.txt"]
      files alpine (FileGlob "src/**/*.txt") `shouldReturn` strings ["src/a.txt", "src/b.txt", "src/deep/c.txt"]
      files alpine (FileGlob "*.txt") `shouldReturn` strings ["out.txt"]
      files alpine (FileGlob "nowhere/*.txt") `shouldReturn` strings []
      files alpine (FileRemove "out.txt") >>= (`shouldSatisfy` isVoid)
      doesFileExist (dir </> "out.txt") `shouldReturn` False
      -- Removing what is not there is not a failure, as rm -f says.
      files alpine (FileRemove "out.txt") >>= (`shouldSatisfy` isVoid)
    it "keeps a quote in a path one word" $ inBase $ \dir b _ -> do
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      files alpine (FileWrite "it's here.txt" "x") >>= (`shouldSatisfy` isVoid)
      doesFileExist (dir </> "it's here.txt") `shouldReturn` True
    it "reports a file it cannot read as E-IO-FS, naming the path, the environment and the shell's reason" $ inBase $ \dir b _ -> do
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      r <- files alpine (FileRead "missing.txt")
      failsWith EIoFs r
      either (\lf -> messageOf lf `shouldSatisfy` (\m -> "cannot access 'missing.txt' in #alpine:3.20: " `T.isPrefixOf` m && "No such file" `T.isInfixOf` m)) (const (pure ())) r
      files alpine (FileListDir "missing") >>= failsWith EIoFs
    it "reports a file that is not UTF-8 as E-IO-DATA-DECODE" $ inBase $ \dir b _ -> do
      BS.writeFile (dir </> "bin.dat") "\xff\xfe"
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      r <- files alpine (FileRead "bin.dat")
      failsWith EIoDataDecode r
      either (\lf -> messageOf lf `shouldBe` "file is not valid UTF-8: 'bin.dat' in #alpine:3.20") (const (pure ())) r
    it "reports the engine's own failure, and a client it cannot launch, as E-IO-ENV-RESOLVE" $ inBase $ \dir b _ -> do
      let refusing = b {cbRunProcess = \_ -> pure (proc "sh" ["-c", "echo 'no such image' >&2; exit 125"], pure ())}
      files <- mkFileRunner refusing (unlockedPins Map.empty) dir
      r <- files alpine (FileExists "x")
      failsWith EIoEnvResolve r
      either (\lf -> messageOf lf `shouldBe` "no such image") (const (pure ())) r
      let unlaunchable = b {cbRunProcess = \_ -> throwIO (userError "no client")}
      files' <- mkFileRunner unlaunchable (unlockedPins Map.empty) dir
      r' <- files' alpine (FileRead "x")
      failsWith EIoEnvResolve r'
      either (\lf -> messageOf lf `shouldSatisfy` T.isPrefixOf "cannot launch docker: ") (const (pure ())) r'
    it "stops the container of a filesystem function that is abandoned (spec 8.7)" $ inBase $ \dir b seen -> do
      -- Reading a FIFO no one writes to blocks until the read is stopped.
      createNamedPipe (dir </> "fifo") 0o600
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      r <- timeout 200000 (files alpine (FileRead "fifo"))
      maybe "abandoned" (const "a result") r `shouldBe` ("abandoned" :: String)
      length <$> readIORef (seenStops seen) `shouldReturn` 1
    it "refuses an environment whose image is not pinned, or whose mount is not one, before running anything" $ inBase $ \dir b _ -> do
      locked <- lockedPins Map.empty
      files <- mkFileRunner b locked dir
      files alpine (FileRead "x") >>= failsWith EIoImageMissing
      files' <- mkFileRunner b (unlockedPins Map.empty) dir
      files' (withOptions [("base_access", VMap (Map.fromList [(".", VString "rw"), ("nope", VString "ro")]))]) (FileRead "x") >>= failsWith EIoEnvResolve
      files' (withOptions [("base_access", VString "sometimes")]) (FileRead "x") >>= failsWith ERuntimeValue
      files' (withOptions [("egress", VString "x")]) (FileRead "x") >>= failsWith ERuntimeValue
      files' (EnvValue "docker" Map.empty) (FileRead "x") >>= failsWith EIoEnvResolve
      files' (EnvValue "vm" Map.empty) (FileRead "x") >>= failsWith EIoEnvResolve

  describe "sessions of with_container (spec 15.5)" $ do
    it "runs commands and filesystem functions in the session's container, and removes it after the body" $ inBase $ \dir b seen -> do
      runner <- mkCommandRunner b (unlockedPins Map.empty) dir noCommandLog
      files <- mkFileRunner b (unlockedPins Map.empty) dir
      said <- newIORef []
      opener <- mkSessionOpener b (unlockedPins Map.empty) dir
      saved <- newIORef Nothing
      v <- opener (\t -> atomicModifyIORef' said (\ts -> (ts <> [t], ()))) alpine $ \s -> do
        atomicModifyIORef' saved (const (Just s, ()))
        r <- runner s "echo in-session; echo $LASK_SESSION_EXEC | cut -c1-5"
        _ <- files s (FileWrite "s.txt" "from the session")
        content <- files s (FileRead "s.txt")
        pure (VArray (V.fromList [either (VString . messageOf) (\(c, out, _) -> VString (T.pack (show c) <> " " <> out)) r, either (VString . messageOf) id content]))
      v `shouldBe` VArray (V.fromList [VString "0 in-session\nlask-\n", VString "from the session"])
      readFile (dir </> "s.txt") `shouldReturn` "from the session"
      ts <- readIORef said
      ts `shouldSatisfy` (\xs -> case xs of [opened, closed] -> "started on #alpine:3.20 for with_container" `T.isInfixOf` opened && "of with_container removed" `T.isSuffixOf` closed; _ -> False)
      length <$> readIORef (seenStops seen) `shouldReturn` 1
      -- After the body, the session is closed.
      Just s <- readIORef saved
      runner s "echo late" >>= failsWith ERuntimeSessionClosed
      files s (FileRead "s.txt") >>= failsWith ERuntimeSessionClosed
    it "stops a command abandoned in a session, and only that command" $ inBase $ \dir b _ -> do
      runner <- mkCommandRunner b (unlockedPins Map.empty) dir noCommandLog
      v <- session b dir alpine $ \s -> do
        start <- getMonotonicTime
        (other, abandoned) <-
          concurrently
            (runner s "sleep 1; echo other-done")
            (timeout 200000 (runner s "sleep 30"))
        end <- getMonotonicTime
        abandoned `shouldBe` Nothing
        (end - start) `shouldSatisfy` (< 20)
        after' <- runner s "echo still-open"
        pure (VArray (V.fromList [either (VString . messageOf) (\(_, out, _) -> VString out) r | r <- [other, after']]))
      v `shouldBe` VArray (V.fromList [VString "other-done\n", VString "still-open\n"])
    it "passes a local environment and a session through to the body as they are" $ inBase $ \dir b _ -> do
      local' <- session b dir (EnvValue "local" Map.empty) (\e -> pure (VEnv e))
      local' `shouldBe` VEnv (EnvValue "local" Map.empty)
      nested <- session b dir alpine $ \s -> session b dir s (\e -> pure (VBool (VEnv e == VEnv s)))
      nested `shouldBe` VBool True
    it "reports a container that does not start, with the engine's reason" $ inBase $ \dir b _ -> do
      let refusing = b {cbRunProcess = \_ -> pure (proc "sh" ["-c", "echo 'no space left' >&2; exit 125"], pure ())}
          exiting = b {cbRunProcess = \_ -> pure (proc "sh" ["-c", "echo 'went away' >&2; exit 1"], pure ())}
          unlaunchable = b {cbRunProcess = \_ -> pure (proc "/nonexistent/client" [], pure ())}
          throwing = b {cbRunProcess = \_ -> throwIO (userError "no client")}
          body = const (pure VNull)
          message act = do
            r <- try act
            case r of
              Left lf -> pure (lfCode lf, messageOf lf)
              Right v -> pure (Nothing, T.pack (show v))
      message (session refusing dir alpine body) `shouldReturn` (Just EIoEnvResolve, "no space left")
      message (session exiting dir alpine body) `shouldReturn` (Just EIoEnvResolve, "the container of with_container did not start: went away")
      fst <$> message (session unlaunchable dir alpine body) `shouldReturn` Just EIoEnvResolve
      snd <$> message (session throwing dir alpine body) >>= (`shouldSatisfy` T.isPrefixOf "cannot launch docker: ")
    it "refuses an environment it cannot resolve, or whose image is not pinned" $ inBase $ \dir b _ -> do
      r <- try (session b dir (EnvValue "vm" Map.empty) (const (pure VNull)))
      either (\lf -> lfCode lf `shouldBe` Just EIoEnvResolve) (const (expectationFailure "opened")) r
      locked <- lockedPins Map.empty
      opener <- mkSessionOpener b locked dir
      r' <- try (opener (const (pure ())) alpine (const (pure VNull)))
      either (\lf -> lfCode lf `shouldBe` Just EIoImageMissing) (const (expectationFailure "opened")) r'
    it "removes the container when the body fails" $ inBase $ \dir b seen -> do
      r <- try (session b dir alpine (\_ -> throwIO (runtimeFailure ERuntimeValue "body failed")))
      either (\lf -> messageOf lf `shouldBe` "body failed") (const (expectationFailure "succeeded")) r
      length <$> readIORef (seenStops seen) `shouldReturn` 1

  describe "commands in a container (spec 8.7, 10.4)" $ do
    it "runs a command string the container's shell reads from its stdin" $ inBase $ \dir b _ -> do
      runner <- mkCommandRunner b (unlockedPins Map.empty) dir noCommandLog
      r <- runner alpine "printf '%s' \"$(pwd)\" | wc -c >/dev/null; echo 'quoted \"words\"'; exit 4"
      fmap (\(c, out, _) -> (c, out)) r `shouldBe` Right (4, "quoted \"words\"\n")
    it "reports the engine's own failure and a client it cannot launch as E-IO-ENV-RESOLVE" $ inBase $ \dir b _ -> do
      let refusing = b {cbRunProcess = \_ -> pure (proc "sh" ["-c", "cat >/dev/null; echo 'pull access denied' >&2; exit 125"], pure ())}
      runner <- mkCommandRunner refusing (unlockedPins Map.empty) dir noCommandLog
      r <- runner alpine "true"
      failsWith EIoEnvResolve r
      either (\lf -> messageOf lf `shouldBe` "pull access denied") (const (pure ())) r
      let throwing = b {cbRunProcess = \_ -> ioError (userError "no client")}
      runner' <- mkCommandRunner throwing (unlockedPins Map.empty) dir noCommandLog
      runner' alpine "true" >>= failsWith EIoEnvResolve

  describe "lask cmd in a container (spec 11.8)" $ do
    let declared b pins dir e prog args = runDeclaredCommand b pins dir noCommandLog True e prog args
    it "runs the program with its argument vector, and gives back its exit code" $ inBase $ \dir b _ -> do
      declared b (unlockedPins Map.empty) dir alpine "sh" ["-c", "exit 3"] `shouldReturn` Right 3
      declared b (unlockedPins Map.empty) dir (EnvValue "local" Map.empty) "sh" ["-c", "exit 5"] `shouldReturn` Right 5
    it "fails before the program starts for an environment it cannot resolve, an image that is not pinned, or a mount that is not one" $ inBase $ \dir b _ -> do
      locked <- lockedPins Map.empty
      declared b (unlockedPins Map.empty) dir (EnvValue "vm" Map.empty) "true" [] >>= failsWith EIoEnvResolve
      declared b locked dir alpine "true" [] >>= failsWith EIoImageMissing
      declared b (unlockedPins Map.empty) dir (withOptions [("base_access", VString "sometimes")]) "true" [] >>= failsWith ERuntimeValue
    it "reports a program it cannot launch as E-IO-ENV-RESOLVE" $ inBase $ \dir b _ -> do
      declared b (unlockedPins Map.empty) dir (EnvValue "local" Map.empty) "/nonexistent/program" [] >>= failsWith EIoEnvResolve
      let throwing = b {cbRunProcess = \_ -> ioError (userError "no client")}
      declared throwing (unlockedPins Map.empty) dir alpine "true" [] >>= failsWith EIoEnvResolve
