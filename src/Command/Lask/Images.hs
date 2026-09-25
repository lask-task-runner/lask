{-# LANGUAGE OverloadedStrings #-}

-- | The container images a program requires (spec 10.3, 11.5, 11.7):
-- pinning registry references in the lock, materializing images, and
-- the pins commands resolve through when they run (10.4).
module Command.Lask.Images
  ( loadPins,
    lockedImages,
    Materialized (..),
    materialize,
    ImageRow (..),
    imageRows,
  )
where

import Command.Lask.Envs (collectRecipes, collectRegistryRefs)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Lask.Deps.Lock
import Language.Lask.Elaborate (CoreProgram (..))
import Language.Lask.ErrorCode (ErrorCode (..), codeText)
import Language.Lask.Runtime.Image
import System.FilePath ((</>))

-- | The images the lock of a program's project records; none when it
-- has no lock file yet.
lockedImages :: CoreProgram -> IO (Map Text LockImage)
lockedImages core =
  maybe Map.empty lockImages . either (const Nothing) id
    <$> loadLockFile (cpBaseDir core </> defaultLockFileName)

-- | The pins the commands of a program resolve through (spec 10.4):
-- each registry reference the lock pins, and the references the
-- program writes as literals, which only the lock may resolve.
loadPins :: CoreProgram -> IO ImagePins
loadPins core = do
  images <- lockedImages core
  lockedPins (lockPins images) (Set.fromList (collectRegistryRefs core))

-- | A registry reference is recorded once for the whole graph, under
-- the root project's empty path: one reference names one image
-- wherever it is written, and resolving it at run time needs nothing
-- but the reference (spec ch. 5, 10.4).
registryKey :: Text -> Text
registryKey ref = "#" <> ref

recipeKey :: Text -> Text
recipeKey dockerfile = "#" <> dockerfile

-- | What materializing a program's images produced.
data Materialized = Materialized
  { -- | The lock entries of every image the program references, and
    -- of no other: an image nothing references any more is dropped.
    -- An image that failed keeps the entry it had.
    matImages :: Map Text LockImage,
    -- | One line per image materialized: the source and what it
    -- resolved to.
    matReport :: [Text],
    -- | One diagnostic line per image that failed.
    matFailures :: [Text]
  }

-- | Materialize every image the program references (spec 10.3):
-- registry references are pulled and pinned, recipes built. The only
-- place besides the module fetch where lask reaches the network.
--
-- A reference the lock already pins is pulled by its digest, so a
-- fresh machine gets the very image the lock names, whatever the tag
-- names today; the image is then checked to carry that digest
-- (@E-IO-IMAGE-DIGEST@). A reference the lock does not pin is pulled by
-- what is written, and the digest it resolved to is recorded. An image
-- already on the daemon is not fetched again, nor a recipe rebuilt:
-- both are content-addressed (11.7).
materialize :: CoreProgram -> Map Text LockImage -> IO Materialized
materialize core prior = do
  registries <- mapM registry (collectRegistryRefs core)
  recipes <- mapM recipe (uniqueRecipes core)
  let results = registries <> recipes
  pure
    Materialized
      { matImages =
          Map.fromList
            ( [(key, entry) | Right (key, entry, _) <- results]
                <> [(key, entry) | Left (key, Just entry, _) <- results]
            ),
        matReport = [line | Right (_, _, line) <- results],
        matFailures = [line | Left (_, _, line) <- results]
      }
  where
    baseDir = cpBaseDir core

    registry ref = do
      let key = registryKey ref
          before = Map.lookup key prior
          failed code msg = Left (key, before, codeText code <> ": " <> ref <> ": " <> msg)
          pinned digest = LockImage "registry" (Just ref) Nothing (Just digest)
      case writtenDigest ref <|> (before >>= liDigest) of
        Just digest -> do
          let image = pinnedRef ref digest
          present <- imageExists image
          fetched <- if present then pure (Right ()) else pullImage image
          case fetched of
            Left e -> pure (failed EIoImageMissing e)
            Right () -> do
              actual <- registryDigest image
              pure $ case actual of
                Right got
                  | got == digest -> Right (key, pinned digest, ref <> " -> " <> image)
                  | otherwise -> failed EIoImageDigest ("pinned to " <> digest <> ", but the image carries " <> got)
                Left e -> failed EIoImageDigest e
        Nothing -> do
          fetched <- pullImage ref
          case fetched of
            Left e -> pure (failed EIoImageMissing e)
            Right () -> do
              actual <- registryDigest ref
              case actual of
                Left e -> pure (failed EIoImageDigest e)
                Right digest -> do
                  -- Pulled once more by its digest: the content is
                  -- already here, but the daemon now holds a name for
                  -- the pinned image of its own. An image store that
                  -- reaches images only through names (containerd's)
                  -- would otherwise lose it the moment the tag is
                  -- pulled again and moves.
                  let image = pinnedRef ref digest
                  named <- pullImage image
                  pure $ case named of
                    Left e -> failed EIoImageMissing e
                    Right () -> Right (key, pinned digest, ref <> " -> " <> image)

    recipe (dockerfile, context, buildArgs) = do
      let key = recipeKey dockerfile
          failed msg = Left (key, Map.lookup key prior, codeText EIoImageMissing <> ": " <> dockerfile <> ": " <> msg)
      tagE <- recipeTag baseDir dockerfile context buildArgs
      case tagE of
        Left e -> pure (failed e)
        Right tag -> do
          present <- imageExists tag
          built <- if present then pure (Right ()) else buildRecipe baseDir dockerfile context buildArgs tag
          pure $ case built of
            Left e -> failed e
            Right () ->
              Right (key, LockImage "recipe" Nothing (Just tag) Nothing, dockerfile <> " -> " <> tag)

-- | One image of @lask env list@ (spec 11.7).
data ImageRow = ImageRow
  { irSource :: Text,
    irKind :: Text,
    -- | The pinned reference or the content-addressed tag; 'Nothing'
    -- for a registry reference the lock does not pin yet.
    irResolved :: Maybe Text,
    irPresent :: Bool
  }

-- | Every image the program references, as the lock resolves it. No
-- network access and no build.
imageRows :: CoreProgram -> IO [ImageRow]
imageRows core = do
  images <- lockedImages core
  let pins = lockPins images
  registries <-
    mapM
      ( \ref -> case writtenDigest ref of
          Just _ -> ImageRow ref "registry" (Just ref) <$> imageExists ref
          Nothing -> case Map.lookup ref pins of
            Just image -> ImageRow ref "registry" (Just image) <$> imageExists image
            Nothing -> pure (ImageRow ref "registry" Nothing False)
      )
      (collectRegistryRefs core)
  recipes <-
    mapM
      ( \(dockerfile, context, buildArgs) -> do
          tagE <- recipeTag (cpBaseDir core) dockerfile context buildArgs
          case tagE of
            Left _ -> pure (ImageRow dockerfile "recipe" Nothing False)
            Right tag -> ImageRow dockerfile "recipe" (Just tag) <$> imageExists tag
      )
      (uniqueRecipes core)
  pure (registries <> recipes)

-- | A recipe the program writes more than once is one image.
uniqueRecipes :: CoreProgram -> [(Text, Text, [(Text, Text)])]
uniqueRecipes = Set.toList . Set.fromList . collectRecipes
