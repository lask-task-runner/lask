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
    Probed (..),
    probeCommands,
    missingCommands,
    evalCommandEnv,
    commandImage,
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
import Control.Exception (try)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Core.AST (Core)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Container (ContainerBackend (..), RunSpec (..))
import Language.Lask.Runtime.Environment (ResolvedEnv (..), resolveEnv)
import Language.Lask.Runtime.Eval (evalCore, mkRtCtx)
import Language.Lask.Runtime.Image
import Language.Lask.Runtime.Value (EnvValue, LaskFailure, Value (..), ioFailure)
import Language.Lask.SecretStore.Resolve (readEnvVarUnresolved)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readCreateProcessWithExitCode)

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
isPresent :: ContainerBackend -> Text -> IO Bool
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
materialize :: ContainerBackend -> Progress -> CoreProgram -> Map Text LockImage -> IO (Map Text LockImage, [Materialized])
materialize backend pg core prior = do
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
          rest <- mapM (\p -> (,) p <$> pullImage backend (update it) (Just p) image) [p | Just p <- others]
          case [(p, e) | (p, Left e) <- rest] of
            (p, e) : _ -> done it row Nothing (Failed (codeText EIoImageMissing <> ": " <> p <> ": " <> e)) before
            [] ->
              done it row (Just image) (if null rest then outcome else Pulled) (Just (LockImage "registry" (Just ref) Nothing (Just digest) Map.empty))

    -- Materialize one reference for one platform, and the digest it is
    -- pinned to. A platform's variant cannot be told present by its
    -- name, so with a platform the image is always pulled; the daemon
    -- fetches nothing it already holds.
    pin it ref platform known = case known of
      Just digest -> do
        let image = pinnedRef ref digest
        present <- if platform == Nothing then imageExists backend image else pure False
        fetched <- if present then pure (Right ()) else pullImage backend (update it) platform image
        case fetched of
          Left e -> pure (Left (EIoImageMissing, e))
          Right () -> do
            actual <- registryDigest backend image
            pure $ case actual of
              Right got
                | got == digest -> Right (digest, image, if present then AlreadyPresent else Pulled)
                | otherwise -> Left (EIoImageDigest, "pinned to " <> digest <> ", but the image carries " <> got)
              Left e -> Left (EIoImageDigest, e)
      Nothing -> do
        fetched <- pullImage backend (update it) platform ref
        case fetched of
          Left e -> pure (Left (EIoImageMissing, e))
          Right () -> do
            actual <- registryDigest backend ref
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
                named <- pullImage backend (update it) platform image
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
          present <- imageExists backend tag
          it <- start pg "image" (Just counter) (shortPath (irHead row)) (if present then "checking" else "building")
          built <- if present then pure (Right ()) else buildRecipe backend (update it) baseDir r tag
          case built of
            Left e -> done it row Nothing (Failed (codeText EIoImageMissing <> ": " <> e)) before
            Right () ->
              done it row (Just tag) (if present then AlreadyPresent else Built) (Just (LockImage "recipe" Nothing (Just tag) Nothing Map.empty))

-- | The outcome of probing the command words of one image (spec 11.7).
data Probed
  = -- | The words, each with whether the image provides it.
    Probed Text [(Text, Bool)]
  | -- | The image's @/bin/sh@ could not be started; why.
    NoShell Text [Text] Text
  | -- | The probe could not be run; why.
    ProbeFailed Text Text

-- | Evaluate the environment of a command declaration as @lask cmd@
-- does (spec 11.8): nothing runs, no file is touched, and a secret
-- reference stays unresolved.
evalCommandEnv :: (Text -> IO (Maybe Text)) -> CoreProgram -> Core -> IO (Either LaskFailure EnvValue)
evalCommandEnv readEnv core c = do
  ctx <- mkRtCtx core "" (RtHooks refuseCommand refuseFile (const (pure ())) noAsyncTracker readEnv)
  r <- try (evalCore ctx Map.empty c)
  pure $ case r of
    Left lf -> Left lf
    Right (VEnv ev) -> Right ev
    Right _ -> Left (refusal "the command's environment did not evaluate to an Environment")
  where
    refuseCommand _ _ = pure (Left (refusal "the environment of a command declaration tried to run a command"))
    refuseFile _ _ = pure (Left (refusal "the environment of a command declaration tried to access a file"))
    refusal = ioFailure EIoEnvResolve

-- | The image a declared command word runs in (spec ch. 5): the lock
-- key of the image its environment evaluates to, with the run options.
-- 'Nothing' for the local environment, one that names a platform, and
-- one that does not evaluate: those words are not probed.
commandImage :: CoreProgram -> Core -> IO (Maybe (Text, Map Text Value))
commandImage core c = do
  r <- evalCommandEnv readEnvVarUnresolved core c
  pure $ case r >>= resolveEnv of
    Right (ResolvedDocker ref opts)
      | not (Map.member "platform" opts) -> Just (registryKey ref, opts)
    Right (ResolvedRecipe rc opts)
      | rcPlatform rc == Nothing -> Just (recipeKey (rcDockerfile rc), opts)
    _ -> Nothing

-- | Probe the command words of every module's declarations whose image
-- materialized (spec ch. 5, 11.7): in one container of the image, run
-- with the environment's run options, @/bin/sh@ reports whether
-- @command -v@ finds each word. Returns the image section of the lock
-- with the results recorded, and one outcome per image probed. A word
-- found under any of the run options it is declared with is present.
-- An image whose probe could not run keeps no results. An image the
-- prior lock pins to the same content, with results for the same
-- words, is not probed again: its results are those.
probeCommands :: ContainerBackend -> CoreProgram -> [Materialized] -> Map Text LockImage -> Map Text LockImage -> IO (Map Text LockImage, [Probed])
probeCommands backend core mats prior images = do
  declared <-
    fmap concat . mapM (\(w, c) -> maybe [] (\(k, o) -> [(k, o, w)]) <$> commandImage core c) $
      [(w, c) | tbl <- Map.elems (cpCommands core), (w, c) <- Map.toList tbl]
  let pinned = Map.fromList [(irKey (matRow m), image) | m <- mats, notFailed (matOutcome m), Just image <- [matPinned m]]
      byImage = Map.fromListWith (<>) [(k, [w]) | (k, _, w) <- declared]
  outcomes <-
    mapM
      (\(key, image) -> probeImage key image (Set.toList (Set.fromList (byImage Map.! key))) [(o, w) | (k, o, w) <- declared, k == key])
      [(k, image) | (k, image) <- Map.toList pinned, Map.member k byImage]
  pure (foldr record images outcomes, outcomes)
  where
    notFailed (Failed _) = False
    notFailed _ = True

    probeImage key image ws uses = case (Map.lookup key prior, Map.lookup key images) of
      (Just before, Just now)
        | before {liCommands = Map.empty} == now,
          Map.keys (liCommands before) == ws ->
            pure (Probed key (Map.toList (liCommands before)))
      _ -> do
        -- One run per distinct set of run options the words come with.
        let groups = Map.elems (Map.fromListWith (\(o, a) (_, b) -> (o, a <> b)) [(T.pack (show (Map.toAscList o)), (o, [w])) | (o, w) <- uses])
        results <- mapM (\(o, gws) -> probeWith image o (Set.toList (Set.fromList gws))) groups
        pure $ case [r | Left r <- results] of
          Left why : _ -> ProbeFailed key why
          Right (gws, why) : _ -> NoShell key gws why
          [] -> Probed key (Map.toList (Map.fromListWith (||) (concat [rs | Right rs <- results])))

    -- Left (Left why): the probe could not run. Left (Right (words, why)):
    -- the image's shell could not start.
    probeWith image opts ws = do
      cp <-
        cbRunProcess backend $
          RunSpec
            { runBaseDir = cpBaseDir core,
              runName = Nothing,
              runImage = image,
              runOptions = opts,
              runStdin = False,
              runTty = False,
              runEntrypoint = "/bin/sh",
              runCommand = ["-c", script, "sh"] <> ws
            }
      (code, out, err) <- readCreateProcessWithExitCode cp ""
      let errText = T.strip (T.pack err)
          exit = case code of
            ExitSuccess -> 0
            ExitFailure n -> n
      pure $ case cbRunFailure backend exit errText of
        Just why -> Left (Left why)
        Nothing
          | exit == 0 ->
              let seen = Map.fromList [(T.drop 1 l, T.take 1 l == "+") | l <- T.lines (T.pack out), T.take 1 l `elem` ["+", "-"]]
               in Right [(w, Map.findWithDefault False w seen) | w <- ws]
          | otherwise -> Left (Right (ws, if T.null errText then "/bin/sh exited with " <> T.pack (show exit) else errText))

    script = "for w in \"$@\"; do if command -v \"$w\" >/dev/null 2>&1; then echo \"+$w\"; else echo \"-$w\"; fi; done"

    record outcome m = case outcome of
      Probed key rs -> Map.adjust (\li -> li {liCommands = Map.fromList rs}) key m
      NoShell key ws _ -> Map.adjust (\li -> li {liCommands = Map.fromList [(w, False) | w <- ws]}) key m
      ProbeFailed _ _ -> m

-- | The command words recorded as missing from their image (spec ch.
-- 5) that the given declarations dispatch through, with the image:
-- each word's environment is evaluated, as the probe evaluated it, to
-- find the image it runs in.
missingCommands :: CoreProgram -> Map Text LockImage -> [Key] -> IO [(Text, Text)]
missingCommands core images keys =
  Set.toList . Set.fromList . concat
    <$> mapM
      missingOf
      (Set.toList (Set.fromList [u | k <- keys, u <- Map.findWithDefault [] k (cpDispatched core)]))
  where
    missingOf (m, w) = case Map.lookup m (cpCommands core) >>= Map.lookup w of
      Nothing -> pure []
      Just c -> do
        img <- commandImage core c
        pure
          [ (w, key)
          | Just (key, _) <- [img],
            Just li <- [Map.lookup key images],
            Map.lookup w (liCommands li) == Just False
          ]
