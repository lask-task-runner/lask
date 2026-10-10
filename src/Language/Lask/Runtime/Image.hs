{-# LANGUAGE OverloadedStrings #-}

-- | Container images built from a recipe (spec 10.2, 10.3).
--
-- A recipe environment names a Dockerfile and a build context inside
-- the module tree. The image it denotes is content-addressed by a
-- recipe hash, so a changed recipe is a different image and cannot
-- reuse a cached one. Building is never implicit: @check@ \/ @run@ \/
-- @eval@ \/ @envs@ resolve the tag and report @E-IO-IMAGE-MISSING@
-- when it is absent, and only @sync@ materializes it (spec 10.3).
module Language.Lask.Runtime.Image
  ( Recipe (..),
    recipeSource,
    recipeTag,
    imageExists,
    daemonReachable,
    absentImage,
    buildRecipe,
    repositoryOf,
    writtenDigest,
    pinnedRef,
    pullImage,
    registryDigest,
    ImagePins,
    lockPins,
    lockedPins,
    unlockedPins,
    resolveRegistry,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Deps.Cache (resolveInBase)
import Language.Lask.Deps.Hash (hashBytes)
import Language.Lask.Deps.Lock (LockImage (..))
import Language.Lask.ErrorCode (ErrorCode (EIoEnvResolve, EIoImageMissing))
import Language.Lask.Runtime.Value (LaskFailure, ioFailure)
import System.Exit (ExitCode (..))
import Control.Concurrent.Async (concurrently, concurrently_)
import System.IO (hGetLine, hIsEOF)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readCreateProcessWithExitCode, waitForProcess)

-- | A recipe environment (spec 10.2): the Dockerfile and the image
-- options that decide what it builds, every one of them a literal of
-- the head it was written with (6.7). Paths are relative to the
-- program's base directory.
data Recipe = Recipe
  { rcDockerfile :: Text,
    rcContext :: Text,
    -- | In name order.
    rcBuildArgs :: [(Text, Text)],
    rcPlatform :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | A recipe path as a head writes it (spec 6.7): relative paths with a
-- leading @./@. A recipe of a dependency cached outside the project is
-- shown by its absolute path, which no head could name.
recipeSource :: Text -> Text
recipeSource df
  | "/" `T.isPrefixOf` df = df
  | otherwise = "./" <> df

-- | The content-addressed tag of a recipe: @lask\/\<recipe hash\>@.
-- The hash covers the Dockerfile's contents, the context path, the
-- declared build arguments and the platform (spec 10.3), so changing
-- any of them yields a different tag and cannot reuse a cached image.
-- A recipe with no platform hashes as it did before platforms were
-- covered, so its tag does not move.
recipeTag :: FilePath -> Recipe -> IO (Either Text Text)
recipeTag baseDir (Recipe dockerfile context buildArgs platform) = do
  file <- resolveInBase baseDir (T.unpack dockerfile)
  r <- try (BS.readFile file)
  pure $ case r of
    Left e ->
      Left ("cannot read Dockerfile '" <> dockerfile <> "': " <> T.pack (show (e :: IOException)))
    Right bytes ->
      let platformKey = maybe "" ("\0platform=" <>) platform
          key = hashBytes (bytes <> TE.encodeUtf8 ("\0" <> context <> buildArgKey buildArgs <> platformKey))
       in Right ("lask/" <> T.replace "sha256-" "" key)

-- | Build arguments in name order, so the hash does not depend on the
-- order they were written in.
buildArgKey :: [(Text, Text)] -> Text
buildArgKey buildArgs = T.concat ["\0" <> k <> "=" <> v | (k, v) <- sortOn fst buildArgs]

-- | Whether the Docker daemon answers, asked once and read only; the
-- reason when it does not.
daemonReachable :: IO (Either Text ())
daemonReachable = do
  r <- try (readCreateProcessWithExitCode (proc "docker" ["version", "--format", "{{.Server.Version}}"]) "")
  pure $ case r of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right (ExitSuccess, _, _) -> Right ()
    Right (_, _, err) -> Left (firstLine (T.pack err))
  where
    firstLine t = case filter (not . T.null) (map T.strip (T.lines t)) of
      l : _ -> l
      [] -> "docker version failed"

-- | The failure for an image the daemon was not found to hold: the
-- image is missing, unless the daemon itself cannot be reached, which
-- @lask sync@ would not mend (spec 10.4, 14.6).
absentImage :: Text -> IO LaskFailure
absentImage missing = do
  reach <- daemonReachable
  pure $ case reach of
    Left e -> ioFailure EIoEnvResolve ("cannot reach the Docker daemon: " <> e)
    Right () -> ioFailure EIoImageMissing missing

-- | Whether the tag is present on the target Docker daemon.
imageExists :: Text -> IO Bool
imageExists tag = do
  r <- try (readCreateProcessWithExitCode (proc "docker" ["image", "inspect", T.unpack tag]) "")
  pure $ case r of
    Left e -> const False (e :: IOException)
    Right (ExitSuccess, _, _) -> True
    Right _ -> False

-- | Build a recipe into its content-addressed tag. No host mount other
-- than the declared context, no privileged mode, no host networking
-- (spec 10.3).
--
-- The build's steps are reported as they run: @[3/6] RUN apk add ...@.
buildRecipe :: (Text -> IO ()) -> FilePath -> Recipe -> Text -> IO (Either Text ())
buildRecipe report baseDir (Recipe dockerfile context buildArgs platform) tag = do
  file <- resolveInBase baseDir (T.unpack dockerfile)
  dir <- resolveInBase baseDir (T.unpack context)
  let args =
        [ "build",
          "--progress=plain",
          "-f",
          file,
          "-t",
          T.unpack tag
        ]
          <> maybe [] (\p -> ["--platform", T.unpack p]) platform
          <> concat [["--build-arg", T.unpack (k <> "=" <> v)] | (k, v) <- sortOn fst buildArgs]
          <> [dir]
  -- BuildKit writes its plain progress to stderr: a step is a line
  -- such as @#7 [3/6] RUN apk add ...@.
  r <- streamProcess "docker build" (proc "docker" args) (const (pure ())) $ \line ->
    case T.breakOn " [" line of
      (n, step) | "#" `T.isPrefixOf` n, not (T.null step) -> report (T.strip step)
      _ -> pure ()
  pure (() <$ r)

-- Registry references (spec 10.3) ---------------------------------------------

-- | The repository a registry reference names: the reference without
-- its tag or digest. The tag separator is the last @:@ of the last path
-- segment, so a registry port — @localhost:5000/app:1@ — is kept.
repositoryOf :: Text -> Text
repositoryOf ref =
  let (dir, lastSeg) = T.breakOnEnd "/" ref
      base = T.takeWhile (/= '@') lastSeg
      name = case T.breakOnEnd ":" base of
        ("", _) -> base
        (pre, _) -> T.dropEnd 1 pre
   in dir <> name

-- | The digest a reference is written with, as in
-- @alpine\@sha256:...@; a reference written with a tag has none.
writtenDigest :: Text -> Maybe Text
writtenDigest ref = case T.breakOn "@" ref of
  (_, rest) | not (T.null rest) -> Just (T.drop 1 rest)
  _ -> Nothing

-- | The reference that names exactly one image: the repository at a
-- digest. The daemon resolves it by content, whatever the tag names
-- today.
pinnedRef :: Text -> Text -> Text
pinnedRef ref digest = repositoryOf ref <> "@" <> digest

-- | Pull a reference. The one network access a registry image needs,
-- made only by @sync@ (spec 10.3, 11.7).
--
-- With a platform, that platform's variant of the image is pulled.
--
-- Progress is reported as layers complete, @layers 4/9@, from the
-- status lines docker writes for each layer when it is not on a
-- terminal (@<id>: Pulling fs layer@, @<id>: Pull complete@).
pullImage :: (Text -> IO ()) -> Maybe Text -> Text -> IO (Either Text ())
pullImage report platform ref = do
  let args = ["pull"] <> maybe [] (\p -> ["--platform", T.unpack p]) platform <> [T.unpack ref]
  layers <- newIORef (Map.empty :: Map Text Bool)
  r <- streamProcess "docker pull" (proc "docker" args) (onLine layers) (const (pure ()))
  pure (() <$ r)
  where
    onLine layers line = case T.breakOn ": " line of
      (layer, status)
        | T.length layer == 12,
          T.all (`elem` ("0123456789abcdef" :: String)) layer -> do
            let done = T.drop 2 status `elem` ["Pull complete", "Already exists"]
            counts <- atomicModifyIORef' layers $ \m ->
              let m' = Map.insertWith (||) layer done m
               in (m', (length (filter id (Map.elems m')), Map.size m'))
            report ("layers " <> T.pack (show (fst counts)) <> "/" <> T.pack (show (snd counts)))
      _ -> pure ()

-- | Run a process, handing each line of its stdout and its stderr to
-- the given actions as it arrives. A failure is the tail of stderr.
streamProcess :: Text -> CreateProcess -> (Text -> IO ()) -> (Text -> IO ()) -> IO (Either Text ())
streamProcess what cp onOut onErr = do
  started <-
    try $
      createProcess cp {std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe}
  case started of
    Left e -> pure (Left ("cannot run " <> what <> ": " <> T.pack (show (e :: IOException))))
    Right (_, Just out, Just err, ph) -> do
      tailRef <- newIORef []
      let readLines h act = do
            eof <- hIsEOF h
            if eof
              then pure ()
              else do
                line <- T.pack <$> hGetLine h
                _ <- act line
                readLines h act
          keep line = do
            atomicModifyIORef' tailRef (\ls -> (take 20 (line : ls), ()))
            onErr line
      (_, code) <- concurrently (concurrently_ (readLines out onOut) (readLines err keep)) (waitForProcess ph)
      errTail <- reverse <$> readIORef tailRef
      pure $ case code of
        ExitSuccess -> Right ()
        _ -> Left (T.strip (T.unlines (filter (not . T.null . T.strip) errTail)))
    Right _ -> pure (Left ("cannot run " <> what))

-- | The registry digest of a local image, read from the repository
-- digests the daemon records for it. The one recorded for the
-- reference's own repository is taken; the familiar and the qualified
-- spelling of a Docker Hub name (@alpine@, @docker.io/library/alpine@)
-- count as one.
registryDigest :: Text -> IO (Either Text Text)
registryDigest ref = do
  r <-
    try
      ( readCreateProcessWithExitCode
          (proc "docker" ["image", "inspect", "--format", "{{json .RepoDigests}}", T.unpack ref])
          ""
      )
  pure $ case r of
    Left e -> Left ("cannot run docker image inspect: " <> T.pack (show (e :: IOException)))
    Right (ExitSuccess, out, _) ->
      case A.decode (BL.fromStrict (TE.encodeUtf8 (T.strip (T.pack out)))) :: Maybe [Text] of
        Nothing -> Left ("unreadable repository digests for '" <> ref <> "'")
        Just ds ->
          case [T.drop 1 d | rd <- ds, let (repo, d) = T.breakOn "@" rd, normal repo == normal (repositoryOf ref)] of
            (d : _) -> Right d
            [] -> Left ("'" <> ref <> "' has no registry digest; it was not pulled from a registry")
    Right (_, _, err) -> Left (T.strip (T.pack err))
  where
    normal r = stripPrefix "library/" (stripPrefix "docker.io/" r)
    stripPrefix p r = maybe r id (T.stripPrefix p r)

-- | How the registry references of a running program resolve (spec
-- 10.4).
data ImagePins
  = -- | Through the lock only. Every reference a program can carry was
    -- written as a head (6.7), so one the lock does not pin means the
    -- lock is behind the program. Nothing is pulled: an image that is
    -- not on the daemon is @E-IO-IMAGE-MISSING@. The images found
    -- present are remembered, so a loop does not ask the daemon again.
    Locked (Map Text Text) (IORef (Set Text))
  | -- | Through the lock where it pins the reference, and otherwise as
    -- written, leaving the daemon to pull. The REPL resolves this way:
    -- it is not one of the subcommands 10.3 forbids to pull.
    Unlocked (Map Text Text)

-- | Each registry reference a lock pins, to the image it names.
lockPins :: Map Text LockImage -> Map Text Text
lockPins images =
  Map.fromList
    [ (ref, pinnedRef ref digest)
    | LockImage "registry" (Just ref) _ (Just digest) <- Map.elems images
    ]

-- | Pins for @run@, @eval@, @cmd@ and @envs@: each pinned reference to
-- the image it names.
lockedPins :: Map Text Text -> IO ImagePins
lockedPins pinned = Locked pinned <$> newIORef Set.empty

unlockedPins :: Map Text Text -> ImagePins
unlockedPins = Unlocked

-- | The image a registry reference runs as (spec 10.4).
--
-- A reference the lock pins runs as the pinned image, and one written
-- with a digest pins itself. Any other means that @lask sync@ has
-- not run since the reference was written.
resolveRegistry :: ImagePins -> Text -> IO (Either LaskFailure Text)
resolveRegistry pins ref = case pins of
  Unlocked pinned -> pure (Right (Map.findWithDefault ref ref pinned))
  Locked pinned seen -> case Map.lookup ref pinned <|> (ref <$ writtenDigest ref) of
    Just image -> do
      known <- Set.member image <$> readIORef seen
      ok <- if known then pure True else imageExists image
      if ok
        then do
          atomicModifyIORef' seen (\s -> (Set.insert image s, ()))
          pure (Right image)
        else
          Left
            <$> absentImage
              ("image '" <> ref <> "' (pinned as " <> image <> ") is not on the Docker daemon; run 'lask sync'")
    Nothing ->
      pure . Left . ioFailure EIoImageMissing $
        "image '" <> ref <> "' is not pinned in lask.lock.json; run 'lask sync'"
