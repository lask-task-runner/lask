{-# LANGUAGE OverloadedStrings #-}

-- | Environment resolution and command launching (spec 10, 12.3).
--
-- An 'EnvValue' is resolved to a concrete runtime configuration just
-- before command launch (10.4): @env@ entries substitute their
-- definition from the environment file; @docker@\/@local@ are used
-- directly. Commands run via the host shell, the @docker@ CLI, or the
-- @ssh@ CLI (10.9; implementation choice permitted by the spec).
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
    dockerArgs,
    dockerShellArgs,
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
import Language.Lask.Runtime.Image (imageExists, recipeTag)
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
import Data.Scientific (formatScientific, isInteger)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner)
import Language.Lask.ErrorCode
import Language.Lask.Obs.CommandLog
import Language.Lask.Runtime.Secrets (maskSecrets)
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (valueToJson)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsTerminalDevice, stderr, stdin, stdout)
import System.Process
  ( CreateProcess (cwd, delegate_ctlc, std_err, std_in, std_out),
    StdStream (CreatePipe, Inherit),
    createProcess,
    proc,
    shell,
    waitForProcess,
  )

data ResolvedEnv
  = ResolvedLocal
  | -- | Image and remaining options.
    ResolvedDocker Text (Map Text Value)
  | -- | Dockerfile, context, and remaining options (spec 10.2 recipe form).
    ResolvedRecipe Text Text (Map Text Value)
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
           in Right (ResolvedRecipe df ctx (Map.delete "dockerfile" (Map.delete "context" params)))
    _ -> Left (ioFailure EIoEnvResolve "docker environment requires an image reference or a recipe")
  other -> Left (ioFailure EIoEnvResolve ("unknown environment kind: '" <> other <> "'"))

-- | The environment summary and 13.1 metadata JSON used by command
-- execution logs (spec 12.3). The summary follows environment
-- expression notation: @#local@, @#\<image\>@ for a registry
-- reference, and @#docker(dockerfile = ...)@ for a recipe.
envLogInfo :: EnvValue -> ResolvedEnv -> (Text, A.Value)
envLogInfo _ resolved = (summary, json)
  where
    summary = case resolved of
      ResolvedLocal -> "#local"
      ResolvedDocker img _ -> "#" <> img
      ResolvedRecipe df _ _ -> "#docker(dockerfile = \"" <> df <> "\")"
    resolvedEnvValue = case resolved of
      ResolvedLocal -> EnvValue "local" Map.empty
      ResolvedDocker img opts -> EnvValue "docker" (Map.insert "image" (VString img) opts)
      ResolvedRecipe df _ opts -> EnvValue "docker" (Map.insert "dockerfile" (VString df) opts)
    json = valueToJson (VEnv resolvedEnvValue)

-- | Arguments for @docker run@ (spec 10.5: base directory mounted as
-- the working directory inside the container).
dockerArgs :: FilePath -> Text -> Map Text Value -> Text -> [String]
dockerArgs baseDir image opts = dockerShellArgs baseDir image opts False

-- | As 'dockerArgs', with @-i@ when the shell is to be fed on stdin
-- (the filesystem runner writes a file that way, so no file content
-- has to fit on a command line).
dockerShellArgs :: FilePath -> Text -> Map Text Value -> Bool -> Text -> [String]
dockerShellArgs baseDir image opts wantStdin cmd =
  ["run", "--rm"]
    <> (if wantStdin then ["-i"] else [])
    <> ["-v", baseDir <> ":/work", "-w", "/work", "--entrypoint", "/bin/sh"]
    <> dockerOptArgs opts
    <> [T.unpack image, "-c", T.unpack cmd]

-- | Implementation-defined environment options (spec 10.2).
dockerOptArgs :: Map Text Value -> [String]
dockerOptArgs opts =
  concat
    [ case (k, v) of
        ("memory", VString m) -> ["--memory", T.unpack m]
        ("cpus", VNumber n) -> ["--cpus", formatNum n]
        _ -> []
    | (k, v) <- Map.toList opts
    ]
  where
    formatNum n
      | isInteger n = formatScientific Sci.Fixed (Just 0) n
      | otherwise = formatScientific Sci.Fixed Nothing n

-- | Arguments for @docker run@ of one program with its argument
-- vector (spec 11.8): no shell is created, so the program name becomes
-- the entrypoint and the remaining words are passed as they stand.
dockerExecArgs :: FilePath -> Text -> Map Text Value -> Bool -> Text -> [Text] -> [String]
dockerExecArgs baseDir image opts interactive prog argv =
  ["run", "--rm"]
    <> (if interactive then ["-i", "-t"] else [])
    <> ["-v", baseDir <> ":/work", "-w", "/work", "--entrypoint", T.unpack prog]
    <> dockerOptArgs opts
    <> [T.unpack image]
    <> map T.unpack argv

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
runDeclaredCommand baseDir0 sink forceRelay envValue prog argv = do
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
        ResolvedDocker image opts ->
          launch (proc "docker" (dockerExecArgs baseDir image opts interactive prog argv))
        ResolvedRecipe df ctx opts -> do
          tagE <- recipeTag baseDir df ctx
          case tagE of
            Left e -> pure (Left (ioFailure EIoImageMissing e))
            Right tag -> do
              ok <- imageExists tag
              if not ok
                then
                  pure . Left . ioFailure EIoImageMissing $
                    "image for recipe '" <> df <> "' is not materialized; run 'lask env build'"
                else launch (proc "docker" (dockerExecArgs baseDir tag opts interactive prog argv))
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
runAttachedProcess sink summary envJson rendered interactive cp = do
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
mkCommandRunner :: FilePath -> CommandLogSink -> IO CommandRunner
mkCommandRunner baseDir0 sink = do
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
            run infraCode cp infraExit = do
              r <- try (runLoggedProcess sink summary envJson execNo cmd cp)
              pure $ case r of
                Left e ->
                  Left (ioFailure infraCode ("cannot launch command: " <> T.pack (show (e :: IOException))))
                Right (code, out, errOut)
                  | Just code == infraExit -> Left (ioFailure infraCode (T.strip errOut))
                  | otherwise -> Right (code, out, errOut)
        img <- materializedImage baseDir resolved
        case img of
          Left failure -> pure (Left failure)
          Right Nothing ->
            run EIoEnvResolve ((shell (T.unpack cmd)) {cwd = Just baseDir}) Nothing
          Right (Just (image, opts)) ->
            -- docker exit code 125 = daemon/run infrastructure error.
            run EIoEnvResolve (proc "docker" (dockerArgs baseDir image opts cmd)) (Just 125)

-- | The image a resolved environment runs in, or 'Nothing' for the
-- local one. A recipe resolves to its content-addressed tag; building
-- is never implicit (spec 10.3), so an unmaterialized recipe is a
-- failure rather than a silent build.
materializedImage ::
  FilePath ->
  ResolvedEnv ->
  IO (Either LaskFailure (Maybe (Text, Map Text Value)))
materializedImage baseDir resolved = case resolved of
  ResolvedLocal -> pure (Right Nothing)
  ResolvedDocker image opts -> pure (Right (Just (image, opts)))
  ResolvedRecipe df ctx opts -> do
    tagE <- recipeTag baseDir df ctx
    case tagE of
      Left e -> pure (Left (ioFailure EIoImageMissing e))
      Right tag -> do
        ok <- imageExists tag
        if not ok
          then
            pure . Left . ioFailure EIoImageMissing $
              "image for recipe '" <> df <> "' is not materialized; run 'lask env build'"
          else pure (Right (Just (tag, opts)))

-- | Run a process, relaying its output line by line to the command
-- execution log (spec 12.3) while capturing both streams verbatim.
-- Always emits the @start@ and @exit \<code\>@ log entries.
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
  CreateProcess ->
  IO (Int, Text, Text)
runLoggedProcess sink summary envJson execNo cmd cp = do
  -- Masked against the registry as it stands now (spec 12.8), before
  -- the command runs and before any sink can retain the log record.
  maskedCmd <- maskSecrets cmd
  (mIn, mOut, mErr, ph) <-
    createProcess cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
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
mkFileRunner :: FilePath -> IO FileRunner
mkFileRunner baseDir0 = do
  baseDir <- makeAbsolute baseDir0
  pure $ \envValue op ->
    case resolveEnv envValue of
      Left failure -> pure (Left failure)
      Right resolved -> do
        img <- materializedImage baseDir resolved
        -- The environment belongs in the diagnostic (spec 15.11): a
        -- path that is absent in a container is often present on the
        -- host, and the message has to say which filesystem was read.
        let (summary, _) = envLogInfo envValue resolved
        case img of
          Left failure -> pure (Left failure)
          Right Nothing -> localFileOp summary baseDir op
          Right (Just (image, opts)) -> containerFileOp summary baseDir image opts op

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
  Text ->
  FilePath ->
  Text ->
  Map Text Value ->
  FileOp ->
  IO (Either LaskFailure Value)
containerFileOp summary baseDir image opts op = case op of
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
      let args = dockerShellArgs baseDir image opts (mStdin /= Nothing) cmd
      r <- try (runQuietProcess (proc "docker" args) mStdin)
      pure $ case r of
        Left e ->
          Left (ioFailure EIoEnvResolve ("cannot launch docker: " <> T.pack (show (e :: IOException))))
        -- docker exit code 125 = daemon/run infrastructure error.
        Right (125, _, err) -> Left (ioFailure EIoEnvResolve (T.strip err))
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
-- stdout as bytes so the caller decides how to decode it.
runQuietProcess :: CreateProcess -> Maybe Text -> IO (Int, BS.ByteString, Text)
runQuietProcess cp mStdin = do
  (mIn, mOut, mErr, ph) <-
    createProcess cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  -- Feeding stdin runs alongside the reads: a child that writes while
  -- it is still being written to would otherwise deadlock.
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
