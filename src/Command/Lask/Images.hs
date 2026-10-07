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

import Command.Lask.Envs (collectRecipes, collectRegistryImages, collectRegistryRefs)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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
-- each registry reference the lock pins.
loadPins :: CoreProgram -> IO ImagePins
loadPins core = lockedPins . lockPins =<< lockedImages core

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
  registries <- mapM registry (Map.toList platformsByRef)
  recipes <- mapM recipe (collectRecipes core)
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

    -- One reference used for several platforms is one lock entry,
    -- pinned once and pulled for each (spec 10.3). The daemon's own
    -- platform, 'Nothing', sorts first and pins when it is among them.
    platformsByRef :: Map Text [Maybe Text]
    platformsByRef = Map.fromListWith (flip (<>)) [(ref, [p]) | (ref, p) <- collectRegistryImages core]

    registry (ref, platforms) = do
      let key = registryKey ref
          before = Map.lookup key prior
          failed code msg = Left (key, before, codeText code <> ": " <> ref <> ": " <> msg)
          (primary, others) = case platforms of
            p : ps -> (p, ps)
            [] -> (Nothing, [])
          suffix p = maybe "" (\x -> " (" <> x <> ")") p
      pinnedE <- pin ref primary (writtenDigest ref <|> (before >>= liDigest))
      case pinnedE of
        Left (code, msg) -> pure (failed code msg)
        Right (digest, image) -> do
          -- Every other platform the reference is used with: pulled by
          -- the pinned digest, so each runs the very image the lock
          -- names.
          rest <- mapM (\p -> (,) p <$> pullImage (Just p) image) [p | Just p <- others]
          pure $ case [(p, e) | (p, Left e) <- rest] of
            (p, e) : _ -> failed EIoImageMissing (p <> ": " <> e)
            [] ->
              Right
                ( key,
                  LockImage "registry" (Just ref) Nothing (Just digest),
                  ref <> suffix primary <> " -> " <> image <> T.concat [", " <> p | Just p <- others]
                )

    -- Materialize one reference for one platform, and the digest it is
    -- pinned to. A platform's variant cannot be told present by its
    -- name, so with a platform the image is always pulled; the daemon
    -- fetches nothing it already holds.
    pin ref platform known = case known of
      Just digest -> do
        let image = pinnedRef ref digest
        present <- if platform == Nothing then imageExists image else pure False
        fetched <- if present then pure (Right ()) else pullImage platform image
        case fetched of
          Left e -> pure (Left (EIoImageMissing, e))
          Right () -> do
            actual <- registryDigest image
            pure $ case actual of
              Right got
                | got == digest -> Right (digest, image)
                | otherwise -> Left (EIoImageDigest, "pinned to " <> digest <> ", but the image carries " <> got)
              Left e -> Left (EIoImageDigest, e)
      Nothing -> do
        fetched <- pullImage platform ref
        case fetched of
          Left e -> pure (Left (EIoImageMissing, e))
          Right () -> do
            actual <- registryDigest ref
            case actual of
              Left e -> pure (Left (EIoImageDigest, e))
              Right digest -> do
                -- Pulled once more by its digest: the content is
                -- already here, but the daemon now holds a name for
                -- the pinned image of its own. An image store that
                -- reaches images only through names (containerd's)
                -- would otherwise lose it the moment the tag is
                -- pulled again and moves.
                let image = pinnedRef ref digest
                named <- pullImage platform image
                pure $ case named of
                  Left e -> Left (EIoImageMissing, e)
                  Right () -> Right (digest, image)

    recipe r = do
      let dockerfile = rcDockerfile r
          key = recipeKey dockerfile
          failed msg = Left (key, Map.lookup key prior, codeText EIoImageMissing <> ": " <> dockerfile <> ": " <> msg)
      tagE <- recipeTag baseDir r
      case tagE of
        Left e -> pure (failed e)
        Right tag -> do
          present <- imageExists tag
          built <- if present then pure (Right ()) else buildRecipe baseDir r tag
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
      ( \r -> do
          let source = recipeSource (rcDockerfile r)
          tagE <- recipeTag (cpBaseDir core) r
          case tagE of
            Left _ -> pure (ImageRow source "recipe" Nothing False)
            Right tag -> ImageRow source "recipe" (Just tag) <$> imageExists tag
      )
      (collectRecipes core)
  pure (registries <> recipes)
