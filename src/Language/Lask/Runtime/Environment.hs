{-# LANGUAGE OverloadedStrings #-}

-- | Environment resolution and command launching (spec 10, 12.3).
--
-- An 'EnvValue' is resolved to a concrete runtime configuration just
-- before command launch (10.4): @env@ entries substitute their
-- definition from the environment file; @docker@\/@local@ are used
-- directly. Commands run via the host shell, or in a container through
-- the container engine's client (10.10; "Language.Lask.Runtime.Container").
--
-- Child process stdout\/stderr are relayed line by line, in real
-- time, to the injected 'CommandLogSink' while also being captured
-- verbatim for the 'CommandResult' contract (spec 12.3: relaying must
-- not affect value semantics).
module Language.Lask.Runtime.Environment
  ( ResolvedEnv (..),
    resolveEnv,
    mkCommandRunner,
    mkFileRunner,
    runLoggedProcess,
    runDeclaredCommand,
    envLogInfo,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Language.Lask.Runtime.Glob (globPrefix, matchGlob)
import Language.Lask.Runtime.Image (ImagePins, Recipe (..), absentImage, imageExists, recipeSource, recipeTag, resolveRegistry)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
    pathIsSymbolicLink,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO.Error
  ( ioeGetErrorString,
    isAlreadyExistsError,
    isDoesNotExistError,
    isFullError,
    isPermissionError,
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner)
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Container (ContainerBackend (..), RunSpec (..), engineName)
import Language.Lask.Runtime.ProcessStop
  ( newContainerName,
    stopGraceSeconds,
    stopProcessTree,
    withStoppableProcess,
  )
import Language.Lask.Runtime.Secrets (maskSecrets, maskSecretsJson)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (valueToJson)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsTerminalDevice, stderr, stdin, stdout)
import System.Process
  ( CreateProcess (cwd, delegate_ctlc, std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe, Inherit),
    createProcess,
    getPid,
    proc,
    shell,
    waitForProcess,
  )

data ResolvedEnv
  = ResolvedLocal
  | -- | Image and remaining options.
    ResolvedDocker Text (Map Text Value)
  | -- | The recipe and the remaining options (spec 10.2 recipe form).
    ResolvedRecipe Recipe (Map Text Value)
  deriving (Show, Eq)

-- | Resolve an environment value to a concrete configuration
-- (spec 10.4). The kinds are @local@ and @docker@; a @docker@ image is
-- either a registry reference or a recipe (10.2).
resolveEnv :: EnvValue -> Either LaskFailure ResolvedEnv
resolveEnv (EnvValue kind params) = case kind of
  "local" -> Right ResolvedLocal
  "docker" -> case (Map.lookup "image" params, Map.lookup "dockerfile" params) of
    (Just (VString img), _)
      | not (T.null img) -> Right (ResolvedDocker img (Map.delete "image" params))
    (_, Just (VString df))
      | not (T.null df) ->
          let ctx = case Map.lookup "context" params of
                Just (VString c) | not (T.null c) -> c
                _ -> T.pack (takeDirectory (T.unpack df))
              recipe = Recipe df ctx (recipeBuildArgs params) (platformOf params)
           in Right (ResolvedRecipe recipe (Map.delete "dockerfile" (Map.delete "context" params)))
    _ -> Left (ioFailure EIoEnvResolve "docker environment requires an image reference or a recipe")
  other -> Left (ioFailure EIoEnvResolve ("unknown environment kind: '" <> other <> "'"))

-- | The build arguments a recipe environment declares (spec 10.2),
-- in name order. They are part of what the recipe hash covers (10.3),
-- so a changed argument is a different image.
recipeBuildArgs :: Map Text Value -> [(Text, Text)]
recipeBuildArgs opts = case Map.lookup "build_args" opts of
  Just (VMap m) -> [(k, t) | (k, VString t) <- Map.toAscList m]
  _ -> []

-- | The platform an environment names (spec 10.2). It is an image
-- option, which the recipe hash covers, and is also passed to @docker
-- run@, so the variant materialized is the one that runs.
platformOf :: Map Text Value -> Maybe Text
platformOf opts = case Map.lookup "platform" opts of
  Just (VString p) | not (T.null p) -> Just p
  _ -> Nothing

-- | The environment summary and 13.1 metadata JSON used by command
-- execution logs (spec 12.3). The summary follows environment
-- expression notation: @#local@, @#\<image\>@ for a registry
-- reference, and @#.\/\<path\>@ for a recipe.
envLogInfo :: EnvValue -> ResolvedEnv -> (Text, A.Value)
envLogInfo _ resolved = (summary, json)
  where
    summary = case resolved of
      ResolvedLocal -> "#local"
      ResolvedDocker img _ -> "#" <> img
      ResolvedRecipe r _ -> "#" <> recipeSource (rcDockerfile r)
    resolvedEnvValue = case resolved of
      ResolvedLocal -> EnvValue "local" Map.empty
      ResolvedDocker img opts -> EnvValue "docker" (Map.insert "image" (VString img) opts)
      ResolvedRecipe r opts -> EnvValue "docker" (Map.insert "dockerfile" (VString (rcDockerfile r)) opts)
    json = valueToJson (VEnv resolvedEnvValue)

-- | Run one declared command with its argument vector (spec 11.8).
--
-- Unlike a command execution expression this creates no shell, so an
-- argument containing whitespace cannot be re-split.
--
-- The program's stdout is always Lask's stdout, so @lask cmd@ can be
-- piped like the program it runs (spec 11.3: its stdout belongs to the
-- program). What varies is stderr. When all three standard streams are
-- terminals it is attached directly, because a prefixed line-buffered
-- relay cannot carry a prompt, a pager, or a progress display, and
-- interactivity takes precedence over the relay; otherwise it is
-- relayed as the command execution log (12.3). The start and exit
-- lines are written either way.
runDeclaredCommand ::
  ContainerBackend ->
  ImagePins ->
  FilePath ->
  CommandLogSink ->
  -- | Force the relay even on a terminal (@--format json@).
  Bool ->
  EnvValue ->
  -- | Program name (the declared command word).
  Text ->
  -- | Its arguments, each preserved as one word.
  [Text] ->
  IO (Either LaskFailure Int)
runDeclaredCommand backend pins baseDir0 sink forceRelay envValue prog argv = do
  baseDir <- makeAbsolute baseDir0
  tty <- allTerminals
  let interactive = tty && not forceRelay
  case resolveEnv envValue of
    Left failure -> pure (Left failure)
    Right resolved -> do
      let (sm, ej) = envLogInfo envValue resolved
          rendered = T.unwords (prog : argv)
          launch cp = do
            r <- try (runAttachedProcess sink sm ej rendered interactive cp)
            pure (mapLaunchFailure r)
      case resolved of
        ResolvedLocal ->
          launch ((proc (T.unpack prog) (map T.unpack argv)) {cwd = Just baseDir})
        _ -> do
          img <- materializedImage backend pins baseDir resolved
          case img of
            Left failure -> pure (Left failure)
            Right Nothing -> pure (Left (ioFailure EIoEnvResolve "internal: a container environment resolved to the host"))
            -- No shell is created, so the program name becomes the
            -- entrypoint and the remaining words are passed as they
            -- stand. Stdin is always attached, since the program's
            -- stdin is that of @lask cmd@ whether or not it is a
            -- terminal; a terminal is allocated only when all three
            -- streams are one.
            Right (Just (image, opts)) ->
              launch
                =<< cbRunProcess
                  backend
                  RunSpec
                    { runBaseDir = baseDir,
                      runName = Nothing,
                      runImage = image,
                      runOptions = opts,
                      runStdin = True,
                      runTty = interactive,
                      runEntrypoint = prog,
                      runCommand = argv
                    }
  where
    allTerminals =
      and <$> mapM hIsTerminalDevice [stdin, stdout, stderr]
    mapLaunchFailure r = case r of
      Left e -> Left (ioFailure EIoEnvResolve ("cannot launch command: " <> T.pack (show (e :: IOException))))
      Right code -> Right code

-- | Run a process with the caller's stdin and stdout attached,
-- emitting the start and exit lines of the command execution log. When
-- not interactive, stderr is relayed line by line as @2|@ entries
-- instead of being attached (spec 11.8).
runAttachedProcess ::
  CommandLogSink ->
  Text ->
  A.Value ->
  Text ->
  -- | Attach stderr directly rather than relaying it.
  Bool ->
  CreateProcess ->
  IO Int
runAttachedProcess sink summary envJson0 rendered interactive cp = do
  maskedCmd <- maskSecrets rendered
  emit maskedCmd ClStart
  (_, _, mErr, ph) <-
    createProcess
      cp
        { std_in = Inherit,
          std_out = Inherit,
          std_err = if interactive then Inherit else CreatePipe,
          delegate_ctlc = True
        }
  mapM_ (relayErr maskedCmd) mErr
  exitCode <- waitForProcess ph
  let code = case exitCode of
        ExitSuccess -> 0
        ExitFailure n -> n
  emit maskedCmd (ClExit code)
  pure code
  where
    emit maskedCmd kind = do
      now <- getCurrentTime
      envJson <- maskSecretsJson envJson0
      sink (CommandLog now summary envJson 1 maskedCmd kind)

    relayErr maskedCmd h = go ""
      where
        go pending = do
          chunkT <- TIO.hGetChunk h
          if T.null chunkT
            then unless (T.null pending) (emitLine maskedCmd pending)
            else do
              let (ls, rest) = splitLines (pending <> chunkT)
              mapM_ (emitLine maskedCmd) ls
              go rest
    emitLine maskedCmd l = do
      now <- getCurrentTime
      masked <- maskSecrets l
      envJson <- maskSecretsJson envJson0
      sink (CommandLog now summary envJson 1 maskedCmd (ClLine 2 masked))
    splitLines t = case T.breakOn "\n" t of
      (_, rest) | T.null rest -> ([], t)
      (l, rest) -> let (ls, r) = splitLines (T.drop 1 rest) in (T.stripEnd l : ls, r)

-- | Real command runner over the three environment families
-- (spec 10.2, 10.8): a unified result contract regardless of the
-- environment, with infrastructure failures mapped to external I\/O
-- errors, and child output relayed to the command execution log
-- (spec 12.3). Allocated in IO: it carries the execution-number
-- counter, unique within the top-level execution even across
-- concurrent commands (12.3).
mkCommandRunner :: ContainerBackend -> ImagePins -> FilePath -> CommandLogSink -> IO CommandRunner
mkCommandRunner backend pins baseDir0 sink = do
  -- The base directory is mounted into containers (spec 10.5), and a
  -- bind mount requires an absolute path.
  baseDir <- makeAbsolute baseDir0
  counter <- newIORef 0
  pure $ \envValue cmd ->
    case resolveEnv envValue of
      Left failure -> pure (Left failure)
      Right resolved -> do
        execNo <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
        let (summary, envJson) = envLogInfo envValue resolved
            run stopC cp engineFailure = do
              r <- try (runLoggedProcess sink summary envJson execNo cmd stopC cp)
              pure $ case r of
                Left e ->
                  Left (ioFailure EIoEnvResolve ("cannot launch command: " <> T.pack (show (e :: IOException))))
                Right (code, out, errOut)
                  | Just msg <- engineFailure code errOut -> Left (ioFailure EIoEnvResolve msg)
                  | otherwise -> Right (code, out, errOut)
        img <- materializedImage backend pins baseDir resolved
        case img of
          Left failure -> pure (Left failure)
          Right Nothing ->
            run (pure ()) ((shell (T.unpack cmd)) {cwd = Just baseDir}) (\_ _ -> Nothing)
          Right (Just (image, opts)) -> do
            name <- newContainerName
            cp <- cbRunProcess backend (shellRun baseDir name image opts False cmd)
            run (cbStop backend stopGraceSeconds name) cp (cbRunFailure backend)

-- | A container run of a command string through @/bin/sh@, named so
-- that it can be stopped if the command is abandoned (8.7); with stdin
-- kept open when the shell is to be fed on it (the filesystem runner
-- writes a file that way, so no file content has to fit on a command
-- line).
shellRun :: FilePath -> Text -> Text -> Map Text Value -> Bool -> Text -> RunSpec
shellRun baseDir name image opts wantStdin cmd =
  RunSpec
    { runBaseDir = baseDir,
      runName = Just name,
      runImage = image,
      runOptions = opts,
      runStdin = wantStdin,
      runTty = False,
      runEntrypoint = "/bin/sh",
      runCommand = ["-c", cmd]
    }

-- | The image a resolved environment runs in, or 'Nothing' for the
-- local one (spec 10.4). A registry reference resolves to the image the
-- lock pins it to, a recipe to its content-addressed tag. Neither is
-- pulled or built here (10.3): an image that is not on the daemon is a
-- failure that names the command that materializes it.
materializedImage ::
  ContainerBackend ->
  ImagePins ->
  FilePath ->
  ResolvedEnv ->
  IO (Either LaskFailure (Maybe (Text, Map Text Value)))
materializedImage backend pins baseDir resolved = case resolved of
  ResolvedLocal -> pure (Right Nothing)
  ResolvedDocker ref opts -> fmap (\image -> Just (image, opts)) <$> resolveRegistry backend pins ref
  ResolvedRecipe r opts -> do
    let df = rcDockerfile r
    tagE <- recipeTag baseDir r
    case tagE of
      Left e -> pure (Left (ioFailure EIoImageMissing e))
      Right tag -> do
        ok <- imageExists backend tag
        if not ok
          then
            Left
              <$> absentImage backend ("image for recipe '" <> df <> "' is not materialized; run 'lask sync'")
          else pure (Right (Just (tag, opts)))

-- | Run a process, relaying its output line by line to the command
-- execution log (spec 12.3) while capturing both streams verbatim.
-- Always emits the @start@ log entry, then @exit \<code\>@, or
-- @killed@ when the run is abandoned before the process exits and the
-- process is stopped (8.7).
runLoggedProcess ::
  CommandLogSink ->
  -- | Environment summary for log lines.
  Text ->
  -- | Environment metadata JSON (13.1).
  A.Value ->
  -- | Execution number (spec 12.3).
  Int ->
  -- | Command string (rendered on the start line).
  Text ->
  -- | Stop the container the process runs, if it runs one.
  IO () ->
  CreateProcess ->
  IO (Int, Text, Text)
runLoggedProcess sink summary envJson0 execNo cmd stopC cp = do
  -- Masked against the registry as it stands now (spec 12.8), before
  -- the command runs and before any sink can retain the log record.
  maskedCmd <- maskSecrets cmd
  let stop ph = do
        -- A process already reaped exited on its own, and its exit
        -- line may already be written.
        running <- getPid ph
        stopProcess stopC ph
        mapM_ (const (emit maskedCmd ClKilled)) running
  withStoppableProcess
    cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    stop
    $ \(mIn, mOut, mErr, ph) -> do
      mapM_ hClose mIn
      emit maskedCmd ClStart
      (out, errOut) <- case (mOut, mErr) of
        (Just hOut, Just hErr) ->
          concurrently (relayStream maskedCmd 1 hOut) (relayStream maskedCmd 2 hErr)
        _ -> pure ("", "")
      exitCode <- waitForProcess ph
      let code = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n
      emit maskedCmd (ClExit code)
      pure (code, out, errOut)
  where
    -- `cmd` (unmasked) already drove `cp` before this function was
    -- even called, so passing the masked copy here only affects what
    -- gets logged, never execution.
    emit maskedCmd kind = do
      now <- getCurrentTime
      envJson <- maskSecretsJson envJson0
      sink (CommandLog now summary envJson execNo maskedCmd kind)

    -- Read a stream in chunks: accumulate the raw text verbatim for
    -- the CommandResult (trailing newlines and unterminated final
    -- lines preserved; never masked — the language must see the real
    -- value, spec 8.7), and relay completed lines as they arrive
    -- (masked, since only the log copy is observation data).
    relayStream :: Text -> Int -> Handle -> IO Text
    relayStream maskedCmd fd h = go [] ""
      where
        go rawAcc pending = do
          chunk <- TIO.hGetChunk h
          if T.null chunk
            then do
              unless (T.null pending) (relayLine pending)
              hClose h
              pure (T.concat (reverse rawAcc))
            else do
              let combined = pending <> chunk
                  pieces = T.splitOn "\n" combined
                  completeLines = init pieces
                  pending' = last pieces
              mapM_ relayLine completeLines
              go (chunk : rawAcc) pending'
        relayLine line = maskSecrets (stripCR line) >>= emit maskedCmd . ClLine fd
        stripCR = T.dropWhileEnd (== '\r')

-- | Build the filesystem runner behind the built-ins of spec 15.11.
--
-- Every operation acts on the filesystem of the environment it is
-- given: the host for @local@, and the container's own filesystem for
-- a container environment, where the base directory is mounted at
-- @\/work@ exactly as it is for commands (10.5). No path reaches a
-- filesystem the program did not name.
--
-- No command execution log is emitted (15.11): nothing here is a
-- command execution expression, and a read is not something the user
-- wrote a command for.
mkFileRunner :: ContainerBackend -> ImagePins -> FilePath -> IO FileRunner
mkFileRunner backend pins baseDir0 = do
  baseDir <- makeAbsolute baseDir0
  pure $ \envValue op ->
    case resolveEnv envValue of
      Left failure -> pure (Left failure)
      Right resolved -> do
        img <- materializedImage backend pins baseDir resolved
        -- The environment belongs in the diagnostic (spec 15.11): a
        -- path that is absent in a container is often present on the
        -- host, and the message has to say which filesystem was read.
        let (summary, _) = envLogInfo envValue resolved
        case img of
          Left failure -> pure (Left failure)
          Right Nothing -> localFileOp summary baseDir op
          Right (Just (image, opts)) -> containerFileOp backend summary baseDir image opts op

-- | Resolve a path written by the program against the base directory
-- (spec 10.5). An absolute path is taken as it stands.
localPath :: FilePath -> Text -> FilePath
localPath baseDir p
  | T.isPrefixOf "/" p = T.unpack p
  | otherwise = baseDir </> T.unpack p

-- | Perform an operation on the host filesystem.
localFileOp :: Text -> FilePath -> FileOp -> IO (Either LaskFailure Value)
localFileOp summary baseDir op = case op of
  FileRead p -> guarded p $ \fp -> do
    bs <- BS.readFile fp
    pure $ case TE.decodeUtf8' bs of
      Left _ -> Left (ioFailure EIoDataDecode (notUtf8 summary p))
      Right t -> Right (VString t)
  FileWrite p contents -> guarded p $ \fp -> do
    BS.writeFile fp (TE.encodeUtf8 contents)
    pure (Right VVoid)
  -- A missing component is false, not a failure (spec 15.11).
  FileExists p -> do
    let fp = localPath baseDir p
    isFile <- doesFileExist fp
    isDir <- doesDirectoryExist fp
    pure (Right (VBool (isFile || isDir)))
  FileRemove p -> guarded p $ \fp -> do
    isFile <- doesFileExist fp
    if isFile
      then removeFile fp >> pure (Right VVoid)
      else do
        isDir <- doesDirectoryExist fp
        pure $
          if isDir
            then Left (fsFailure summary p "path is a directory")
            else -- Removing what is not there succeeds (spec 15.11),
            -- so a cleanup path needs no prior test.
              Right VVoid
  FileMakeDir p -> guarded p $ \fp -> do
    createDirectoryIfMissing True fp
    pure (Right VVoid)
  FileListDir p -> guarded p $ \fp -> do
    isDir <- doesDirectoryExist fp
    if not isDir
      then pure (Left (fsFailure summary p "not a directory"))
      else do
        entries <- listDirectory fp
        pure (Right (textArray (sort (map T.pack entries))))
  FileGlob pat -> do
    let root = localPath baseDir (globRoot pat)
    isDir <- doesDirectoryExist root
    if not isDir
      then -- A pattern rooted at a directory that does not exist
      -- matches nothing; that is not a failure (spec 15.11).
        pure (Right (textArray []))
      else do
        r <- try (walkTree root)
        pure $ case r of
          Left e -> Left (fsFailure summary pat (ioMessage e))
          Right rels -> Right (globResult pat rels)
  where
    guarded p act = do
      r <- try (act (localPath baseDir p))
      pure $ case r of
        Left e -> Left (fsFailure summary p (ioMessage e))
        Right v -> v

-- | Every path under a root, relative to it, directories included.
-- A symbolic link is reported but never descended into, so a link
-- cycle cannot make the traversal diverge.
walkTree :: FilePath -> IO [Text]
walkTree root = go ""
  where
    go rel = do
      let dir = if T.null rel then root else root </> T.unpack rel
      entries <- listDirectory dir
      fmap concat . forM (sort entries) $ \e -> do
        let child = if T.null rel then T.pack e else rel <> "/" <> T.pack e
            fp = dir </> e
        isDir <- doesDirectoryExist fp
        link <- pathIsSymbolicLink fp
        rest <- if isDir && not link then go child else pure []
        pure (child : rest)

-- | Perform an operation inside a container, through one short shell
-- command. The base directory is mounted at @\/work@ and is the
-- working directory, so a relative path means the same thing it does
-- for a command in the same environment (spec 10.5).
containerFileOp ::
  ContainerBackend ->
  Text ->
  FilePath ->
  Text ->
  Map Text Value ->
  FileOp ->
  IO (Either LaskFailure Value)
containerFileOp backend summary baseDir image opts op = case op of
  FileRead p ->
    interpret p (shCmd ["cat", "--", shQuote p]) Nothing $ \_ out ->
      case TE.decodeUtf8' out of
        Left _ -> Left (ioFailure EIoDataDecode (notUtf8 summary p))
        Right t -> Right (VString t)
  -- The content travels on stdin, so no file has to fit on a command
  -- line and no quoting of the content is involved.
  FileWrite p contents ->
    interpret p (shCmd ["cat", ">", shQuote p]) (Just contents) $ \_ _ ->
      Right VVoid
  FileExists p ->
    run (shCmd ["test", "-e", shQuote p]) Nothing >>= \r -> pure $ case r of
      Left failure -> Left failure
      Right (code, _, _) -> Right (VBool (code == 0))
  FileRemove p ->
    interpret p (shCmd ["rm", "-f", "--", shQuote p]) Nothing $ \_ _ -> Right VVoid
  FileMakeDir p ->
    interpret p (shCmd ["mkdir", "-p", "--", shQuote p]) Nothing $ \_ _ -> Right VVoid
  FileListDir p ->
    let q = shQuote p
        cmd = "if [ -d " <> q <> " ]; then ls -A -- " <> q <> "; else exit 2; fi"
     in interpret p cmd Nothing $ \_ out ->
          Right (textArray (sort (lines' out)))
  FileGlob pat ->
    let root = globRoot pat
        q = shQuote (if T.null root then "." else root)
        -- A root that is not there matches nothing, so the command
        -- succeeds with no output rather than failing.
        cmd = "if [ -e " <> q <> " ]; then find " <> q <> " -print; fi"
     in interpret pat cmd Nothing $ \_ out ->
          Right (globResult pat (map stripDot (lines' out)))
  where
    run cmd mStdin = do
      name <- newContainerName
      cp <- cbRunProcess backend (shellRun baseDir name image opts (mStdin /= Nothing) cmd)
      r <- try (runQuietProcess (cbStop backend stopGraceSeconds name) cp mStdin)
      pure $ case r of
        Left e ->
          Left (ioFailure EIoEnvResolve ("cannot launch " <> engineName (cbEngine backend) <> ": " <> T.pack (show (e :: IOException))))
        Right (code, _, err)
          | Just msg <- cbRunFailure backend code err -> Left (ioFailure EIoEnvResolve msg)
        Right ok -> Right ok

    -- Run, and turn a non-zero exit into E-IO-FS carrying the
    -- command's own diagnosis of the path (spec 15.11). The contents
    -- of a file never reach a diagnostic: only stderr does.
    interpret p cmd mStdin f = do
      r <- run cmd mStdin
      pure $ case r of
        Left failure -> Left failure
        Right (code, out, err)
          | code == 0 -> f code out
          | otherwise -> Left (fsFailure summary p (T.strip err))

    shCmd = T.unwords

    lines' = filter (not . T.null) . T.lines . TE.decodeUtf8With TEE.lenientDecode

    stripDot t = maybe t id (T.stripPrefix "./" t)

-- | Run a process without emitting a command execution log, capturing
-- stdout as bytes so the caller decides how to decode it. Like
-- 'runLoggedProcess', it stops the process, and the named container,
-- when the run is abandoned (spec 8.7).
runQuietProcess :: IO () -> CreateProcess -> Maybe Text -> IO (Int, BS.ByteString, Text)
runQuietProcess stopC cp mStdin =
  withStoppableProcess
    cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    (stopProcess stopC)
    $ \(mIn, mOut, mErr, ph) -> do
      -- Feeding stdin runs alongside the reads: a child that writes
      -- while it is still being written to would otherwise deadlock.
      (_, (out, err)) <- concurrently (feed mIn) (concurrently (readAll mOut) (readAll mErr))
      exitCode <- waitForProcess ph
      let code = case exitCode of
            ExitSuccess -> 0
            ExitFailure n -> n
      pure (code, out, TE.decodeUtf8With TEE.lenientDecode err)
  where
    feed Nothing = pure ()
    feed (Just h) = do
      case mStdin of
        Just t -> BS.hPut h (TE.encodeUtf8 t)
        Nothing -> pure ()
      hClose h
    readAll Nothing = pure BS.empty
    readAll (Just h) = BS.hGetContents h

-- | Stop an abandoned command (spec 8.7): its container through the
-- engine first, which also ends the client that runs it, then the
-- process tree on the host.
stopProcess :: IO () -> ProcessHandle -> IO ()
stopProcess stopC ph = do
  stopC
  stopProcessTree stopGraceSeconds ph

-- | Quote one value as a single POSIX shell word.
shQuote :: Text -> Text
shQuote s = "'" <> T.replace "'" "'\\''" s <> "'"

-- | The directory a glob traversal starts from: the leading literal
-- components of the pattern, keeping it rooted where the pattern is.
globRoot :: Text -> Text
globRoot pat = (if T.isPrefixOf "/" pat then "/" else "") <> globPrefix pat

-- | Select the paths matching the pattern out of a traversal, sorted
-- so that the same pattern yields the same order in every environment
-- (spec 15.11).
globResult :: Text -> [Text] -> Value
globResult pat rels = textArray (sort (filter (matchGlob pat) (map logical rels)))
  where
    root = globRoot pat
    logical rel
      | T.null root = rel
      | T.isPrefixOf (root <> "/") rel || rel == root = rel
      | otherwise = root <> "/" <> rel

textArray :: [Text] -> Value
textArray = VArray . V.fromList . map VString

notUtf8 :: Text -> Text -> Text
notUtf8 summary p = "file is not valid UTF-8: '" <> p <> "' in " <> summary

-- | A filesystem failure naming the path and the environment it was
-- read in (spec 14.3, 15.11). The file's contents never appear here;
-- only the path and the cause do.
fsFailure :: Text -> Text -> Text -> LaskFailure
fsFailure summary p detail =
  ioFailure EIoFs $
    "cannot access '" <> p <> "' in " <> summary <> ": " <> cause
  where
    cause
      | T.null (T.strip detail) = "filesystem access failed"
      | otherwise = T.strip detail

-- | An IO exception as a short cause, without the GHC call detail
-- that means nothing to someone reading a task's diagnostics.
ioMessage :: IOException -> Text
ioMessage e
  | isDoesNotExistError e = "no such file or directory"
  | isPermissionError e = "permission denied"
  | isAlreadyExistsError e = "already exists"
  | isFullError e = "no space left on device"
  | otherwise = T.pack (ioeGetErrorString e)
