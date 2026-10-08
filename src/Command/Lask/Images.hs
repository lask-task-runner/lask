{-# LANGUAGE OverloadedStrings #-}

-- | The container images a program requires (spec 10.3, 11.5, 11.7):
-- pinning registry references in the lock, materializing images, and
-- the pins commands resolve through when they run (10.4).
module Command.Lask.Images
  ( loadPins,
    lockedImages,
    ImageRow (..),
    ImageSource (..),
    imageRows,
    pinnedOf,
    isPresent,
    Outcome (..),
    Materialized (..),
    materialize,
  )
where

import Command.Lask.Envs (HeadImage (..), HeadUse (..), Requirer (..), collectHeadUses)
import Command.Lask.Progress (Progress, finish, section, start, update)
import Command.Lask.Table (shortPath)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Deps.Lock
import Language.Lask.Elaborate (CoreProgram (..), Key)
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

-- | One image the program requires, as the listings show it (spec
-- 11.4, 11.7): a registry reference with every platform a head names
-- it for, or a recipe, and where each head is written.
data ImageRow = ImageRow
  { -- | The head, in the notation of 6.7: @#node:24@, @#./infra/Dockerfile@.
    irHead :: Text,
    irKind :: Text,
    -- | The key of its lock entry.
    irKey :: Text,
    irSource :: ImageSource,
    irRequiredBy :: [Requirer]
  }

data ImageSource = FromRegistry Text [Maybe Text] | FromRecipe Recipe

-- | The images the program requires (spec 10.3), with what requires
-- them. With a declaration, only what it reaches.
imageRows :: CoreProgram -> Maybe Key -> [ImageRow]
imageRows core scope = registries <> recipes
  where
    uses = collectHeadUses core scope
    registries =
      [ ImageRow ("#" <> ref) "registry" (registryKey ref) (FromRegistry ref (Set.toList ps)) (Set.toList by)
      | (ref, (ps, by)) <-
          Map.toList $
            Map.fromListWith
              (\(a, b) (c, d) -> (Set.union a c, Set.union b d))
              [(ref, (Set.singleton p, Set.singleton (huBy u))) | u@(HeadUse (HeadRegistry ref p) _) <- uses]
      ]
    recipes =
      [ ImageRow ("#" <> recipeSource (rcDockerfile r)) "recipe" (recipeKey (rcDockerfile r)) (FromRecipe r) (Set.toList by)
      | (r, by) <- Map.toList (Map.fromListWith Set.union [(r, Set.singleton (huBy u)) | u@(HeadUse (HeadRecipe r) _) <- uses])
      ]

-- | What the lock resolves an image to, read without Docker: the
-- pinned reference of a registry reference (or the reference itself
-- when written with a digest), the content-addressed tag of a recipe.
pinnedOf :: FilePath -> Map Text LockImage -> ImageRow -> IO (Maybe Text)
pinnedOf baseDir images row = case irSource row of
  FromRegistry ref _ ->
    pure (maybe (Map.lookup ref (lockPins images)) (const (Just ref)) (writtenDigest ref))
  FromRecipe r -> either (const Nothing) Just <$> recipeTag baseDir r

-- | Whether the pinned image is on the daemon. Read only: nothing is
-- pulled or built.
isPresent :: Text -> IO Bool
isPresent = imageExists

-- | How materializing one image ended.
data Outcome = Pulled | AlreadyPresent | Built | Failed Text
  deriving (Show, Eq)

-- | One image after @lask sync@: its row, what the lock now resolves it
-- to, how it ended, and how long it took.
data Materialized = Materialized
  { matRow :: ImageRow,
    matPinned :: Maybe Text,
    matOutcome :: Outcome,
    matSeconds :: Double
  }

-- | Materialize every image the program references (spec 10.3):
-- registry references are pulled and pinned, recipes built, each
-- reported on the progress as it goes (11.7). The only place besides
-- the module fetch where lask reaches the network. Returns the image
-- section of the lock — every image the program references, and no
-- other, an image that failed keeping the entry it had — and one
-- result per image.
--
-- A reference the lock already pins is pulled by its digest, so a
-- fresh machine gets the very image the lock names, whatever the tag
-- names today; the image is then checked to carry that digest
-- (@E-IO-IMAGE-DIGEST@). A reference the lock does not pin is pulled by
-- what is written, and the digest it resolved to is recorded. An image
-- already on the daemon is not fetched again, nor a recipe rebuilt:
-- both are content-addressed (11.7).
materialize :: Progress -> CoreProgram -> Map Text LockImage -> IO (Map Text LockImage, [Materialized])
materialize pg core prior = do
  let rows = imageRows core Nothing
      total = length rows
  section pg ("Images (" <> T.pack (show total) <> ")")
  results <- mapM (\(i, row) -> one (i, total) row) (zip [1 ..] rows)
  let entries =
        Map.fromList
          [ (irKey (matRow m), e)
          | (m, Just e) <- results
          ]
  pure (entries, map fst results)
  where
    baseDir = cpBaseDir core

    one counter row = case irSource row of
      FromRegistry ref platforms -> registry counter row ref platforms
      FromRecipe r -> recipe counter row r

    done it row pinned outcome entry = do
      secs <- case outcome of
        Failed e -> finish it False ("failed: " <> e)
        Pulled -> finish it True "pulled"
        AlreadyPresent -> finish it True "present"
        Built -> finish it True "built"
      pure (Materialized row pinned outcome secs, entry)

    registry counter row ref platforms = do
      let key = irKey row
          before = Map.lookup key prior
          -- The daemon's own platform, 'Nothing', sorts first and pins
          -- when it is among them; every other platform is pulled by
          -- the pinned digest.
          (primary, others) = case platforms of
            p : ps -> (p, ps)
            [] -> (Nothing, [])
          known = writtenDigest ref <|> (before >>= liDigest)
      it <- start pg "image" (Just counter) (shortPath (irHead row)) (maybe "pulling" (const "checking") known)
      pinnedE <- pin it ref primary known
      case pinnedE of
        Left (code, msg) -> done it row Nothing (Failed (codeText code <> ": " <> msg)) before
        Right (digest, image, outcome) -> do
          rest <- mapM (\p -> (,) p <$> pullImage (update it) (Just p) image) [p | Just p <- others]
          case [(p, e) | (p, Left e) <- rest] of
            (p, e) : _ -> done it row Nothing (Failed (codeText EIoImageMissing <> ": " <> p <> ": " <> e)) before
            [] ->
              done it row (Just image) (if null rest then outcome else Pulled) (Just (LockImage "registry" (Just ref) Nothing (Just digest)))

    -- Materialize one reference for one platform, and the digest it is
    -- pinned to. A platform's variant cannot be told present by its
    -- name, so with a platform the image is always pulled; the daemon
    -- fetches nothing it already holds.
    pin it ref platform known = case known of
      Just digest -> do
        let image = pinnedRef ref digest
        present <- if platform == Nothing then imageExists image else pure False
        fetched <- if present then pure (Right ()) else pullImage (update it) platform image
        case fetched of
          Left e -> pure (Left (EIoImageMissing, e))
          Right () -> do
            actual <- registryDigest image
            pure $ case actual of
              Right got
                | got == digest -> Right (digest, image, if present then AlreadyPresent else Pulled)
                | otherwise -> Left (EIoImageDigest, "pinned to " <> digest <> ", but the image carries " <> got)
              Left e -> Left (EIoImageDigest, e)
      Nothing -> do
        fetched <- pullImage (update it) platform ref
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
                named <- pullImage (update it) platform image
                pure $ case named of
                  Left e -> Left (EIoImageMissing, e)
                  Right () -> Right (digest, image, Pulled)

    recipe counter row r = do
      let key = irKey row
          before = Map.lookup key prior
      tagE <- recipeTag baseDir r
      case tagE of
        Left e -> do
          it <- start pg "image" (Just counter) (shortPath (irHead row)) "building"
          done it row Nothing (Failed (codeText EIoImageMissing <> ": " <> e)) before
        Right tag -> do
          present <- imageExists tag
          it <- start pg "image" (Just counter) (shortPath (irHead row)) (if present then "checking" else "building")
          built <- if present then pure (Right ()) else buildRecipe (update it) baseDir r tag
          case built of
            Left e -> done it row Nothing (Failed (codeText EIoImageMissing <> ": " <> e)) before
            Right () ->
              done it row (Just tag) (if present then AlreadyPresent else Built) (Just (LockImage "recipe" Nothing (Just tag) Nothing))
