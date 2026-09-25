{-# LANGUAGE OverloadedStrings #-}

-- | Container images built from a recipe (spec 10.2, 10.3).
--
-- A recipe environment names a Dockerfile and a build context inside
-- the module tree. The image it denotes is content-addressed by a
-- recipe hash, so a changed recipe is a different image and cannot
-- reuse a cached one. Building is never implicit: @check@ \/ @run@ \/
-- @eval@ \/ @envs@ resolve the tag and report @E-IO-IMAGE-MISSING@
-- when it is absent, and only @deps sync@ \/ @env build@ materialize
-- it (spec 10.3).
module Language.Lask.Runtime.Image
  ( recipeTag,
    imageExists,
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
import Language.Lask.Deps.Hash (hashBytes)
import Language.Lask.Deps.Lock (LockImage (..))
import Language.Lask.ErrorCode (ErrorCode (EIoImageMissing))
import Language.Lask.Runtime.Value (LaskFailure, ioFailure)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- | The content-addressed tag of a recipe: @lask\/\<recipe hash\>@.
-- The hash covers the Dockerfile's contents, the context path and the
-- declared build arguments (spec 10.3), so changing any of the three
-- yields a different tag and cannot reuse a cached image.
recipeTag :: FilePath -> Text -> Text -> [(Text, Text)] -> IO (Either Text Text)
recipeTag baseDir dockerfile context buildArgs = do
  r <- try (BS.readFile (baseDir </> T.unpack dockerfile))
  pure $ case r of
    Left e ->
      Left ("cannot read Dockerfile '" <> dockerfile <> "': " <> T.pack (show (e :: IOException)))
    Right bytes ->
      let key = hashBytes (bytes <> TE.encodeUtf8 ("\0" <> context <> buildArgKey buildArgs))
       in Right ("lask/" <> T.replace "sha256-" "" key)

-- | Build arguments in name order, so the hash does not depend on the
-- order they were written in.
buildArgKey :: [(Text, Text)] -> Text
buildArgKey buildArgs = T.concat ["\0" <> k <> "=" <> v | (k, v) <- sortOn fst buildArgs]

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
buildRecipe :: FilePath -> Text -> Text -> [(Text, Text)] -> Text -> IO (Either Text ())
buildRecipe baseDir dockerfile context buildArgs tag = do
  let args =
        [ "build",
          "-f",
          baseDir </> T.unpack dockerfile,
          "-t",
          T.unpack tag
        ]
          <> concat [["--build-arg", T.unpack (k <> "=" <> v)] | (k, v) <- sortOn fst buildArgs]
          <> [baseDir </> T.unpack context]
  r <- try (readCreateProcessWithExitCode (proc "docker" args) "")
  pure $ case r of
    Left e -> Left ("cannot run docker build: " <> T.pack (show (e :: IOException)))
    Right (ExitSuccess, _, _) -> Right ()
    Right (_, _, err) -> Left (T.strip (T.pack err))

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
-- made only by @deps sync@ and @env build@ (spec 10.3, 11.5, 11.7).
pullImage :: Text -> IO (Either Text ())
pullImage ref = do
  r <- try (readCreateProcessWithExitCode (proc "docker" ["pull", "--quiet", T.unpack ref]) "")
  pure $ case r of
    Left e -> Left ("cannot run docker pull: " <> T.pack (show (e :: IOException)))
    Right (ExitSuccess, _, _) -> Right ()
    Right (_, _, err) -> Left (T.strip (T.pack err))

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
  = -- | Through the lock, and only the lock may resolve a reference the
    -- program writes as a literal. Nothing is pulled: an image that is
    -- not on the daemon is @E-IO-IMAGE-MISSING@. The images found
    -- present are remembered, so a loop does not ask the daemon again.
    Locked (Map Text Text) (Set Text) (IORef (Set Text))
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
-- the image it names, and the references the program writes as
-- literals.
lockedPins :: Map Text Text -> Set Text -> IO ImagePins
lockedPins pinned literals = Locked pinned literals <$> newIORef Set.empty

unlockedPins :: Map Text Text -> ImagePins
unlockedPins = Unlocked

-- | The image a registry reference runs as (spec 10.4).
--
-- A reference the lock pins runs as the pinned image. A literal the
-- lock does not pin means the lock is behind the program, and that
-- @lask env build@ has not run since the reference was written. A
-- reference computed at run time cannot be enumerated or pinned (10.3),
-- so it runs as written — but is never pulled for it either.
resolveRegistry :: ImagePins -> Text -> IO (Either LaskFailure Text)
resolveRegistry pins ref = case pins of
  Unlocked pinned -> pure (Right (Map.findWithDefault ref ref pinned))
  -- A reference written with a digest pins itself.
  Locked pinned literals seen -> case Map.lookup ref pinned <|> (ref <$ writtenDigest ref) of
    Just image -> present seen image $ \() ->
      "image '" <> ref <> "' (pinned as " <> image <> ") is not on the Docker daemon; run 'lask env build'"
    Nothing
      | ref `Set.member` literals ->
          pure . Left . ioFailure EIoImageMissing $
            "image '" <> ref <> "' is not pinned in lask.lock.json; run 'lask env build'"
      | otherwise -> present seen ref $ \() ->
          "image '"
            <> ref
            <> "' is computed at run time, so lask cannot materialize it; pull it with 'docker pull "
            <> ref
            <> "'"
  where
    present seen image message = do
      known <- Set.member image <$> readIORef seen
      ok <- if known then pure True else imageExists image
      if ok
        then do
          atomicModifyIORef' seen (\s -> (Set.insert image s, ()))
          pure (Right image)
        else pure (Left (ioFailure EIoImageMissing (message ())))
