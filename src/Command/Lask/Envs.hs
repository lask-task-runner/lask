{-# LANGUAGE OverloadedStrings #-}

-- | Static enumeration of the execution environments a program
-- constructs (spec 11.4), shared by @lask envs@, @lask sync@ and the
-- environment section of function help (spec 11.6).
module Command.Lask.Envs
  ( EnvRef (..),
    HeadImage (..),
    Requirer (..),
    HeadUse (..),
    collectHeadUses,
    collectEnvRefs,
    collectEnvRefsFrom,
    collectRecipes,
    collectRegistryRefs,
    collectRegistryImages,
    collectEnvReadsFrom,
    readsStdin,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), Key)
import Language.Lask.Runtime.Image (Recipe (..), recipeSource)
import Language.Lask.Span (Span (..))

data EnvRef = EnvRef
  { refLabel :: Text,
    refKind :: Text,
    refTarget :: Text
  }
  deriving (Show, Eq, Ord)

-- | All environment constructions reachable from the entry module
-- (spec 11.4), including the environments of its command words,
-- declared or imported (spec ch. 5). A command word must be enumerable
-- and materializable even when no task uses it, because @lask cmd@ can
-- invoke it (spec 11.8).
collectEnvRefs :: CoreProgram -> [EnvRef]
collectEnvRefs core = concatMap envRefsIn (reachableCores core)

-- | The environments of the entry module's command words, declared or
-- imported.
commandEnvs :: CoreProgram -> [Core]
commandEnvs core = Map.elems (Map.findWithDefault Map.empty (cpEntry core) (cpCommands core))

-- | Every expression the program can evaluate (spec 10.3, 11.4): the
-- entry module's declarations and the environments of its command
-- words, with every top-level declaration they reach, in any module.
-- A declaration of an imported module that nothing reachable
-- references is left out, so the images it names are not required.
reachableCores :: CoreProgram -> [Core]
reachableCores core = map cdCore (reachableFrom core roots) <> envs
  where
    envs = commandEnvs core
    roots =
      [k | k@(p, _) <- Map.keys (cpDecls core), p == cpEntry core]
        <> [(p, n) | c <- envs, CVar (TopRef p n) <- map coreF (c : descendants c)]

-- | The environments reachable from one declaration: its own
-- environment expressions plus those of every top-level declaration
-- it can reach. Reachability is an over-approximation (spec 11.4) —
-- every referenced declaration counts, whether or not it is actually
-- called.
collectEnvRefsFrom :: CoreProgram -> Key -> [EnvRef]
collectEnvRefsFrom core start = concatMap declEnvRefs (reachableDecls core start)

-- | The declarations reachable from one: itself and every top-level
-- declaration it refers to, transitively.
reachableDecls :: CoreProgram -> Key -> [CoreDecl]
reachableDecls core start = reachableFrom core [start]

-- | The declarations reachable from several: themselves and every
-- top-level declaration they refer to, transitively.
reachableFrom :: CoreProgram -> [Key] -> [CoreDecl]
reachableFrom core starts = go Set.empty starts
  where
    go :: Set Key -> [Key] -> [CoreDecl]
    go _ [] = []
    go seen (k : rest)
      | k `Set.member` seen = go seen rest
      | otherwise = case Map.lookup k (cpDecls core) of
          Nothing -> go (Set.insert k seen) rest
          Just cd -> cd : go (Set.insert k seen) (topRefs (cdCore cd) <> rest)

    topRefs c =
      [(p, n) | CVar (TopRef p n) <- map coreF (c : descendants c)]

-- | Whether a declaration can reach the @stdin@ reference (spec 9.3):
-- whether it, or anything it refers to, names @stdin@, here or in the
-- environments of the program's command declarations.
readsStdin :: CoreProgram -> Key -> Bool
readsStdin core start =
  any names (map cdCore (reachableFrom core (start : envRefs)) <> envs)
  where
    envs = commandEnvs core
    envRefs = [(p, n) | c <- envs, CVar (TopRef p n) <- map coreF (c : descendants c)]
    names c = or [True | CVar (BuiltinRef "stdin") <- map coreF (c : descendants c)]

-- | The environment variables a declaration can read by name (spec
-- 11.10): the literal names given to @get_env@, @find_env@ and
-- @get_env_or@ in everything it reaches, and in the environments of
-- the program's command declarations, which its commands may run in.
-- 'Nothing' when a name is computed, or one of these built-ins is
-- passed as a value: then any variable may be read.
collectEnvReadsFrom :: CoreProgram -> Key -> Maybe (Set Text)
collectEnvReadsFrom core start =
  fmap Set.fromList . sequence $
    concatMap (readsIn . cdCore) (reachableDecls core start) <> concatMap readsIn (commandEnvs core)
  where
    readers = ["get_env", "find_env", "get_env_or"] :: [Text]
    readsIn c = case coreF c of
      CApp (Core _ (CVar (BuiltinRef n))) args kws
        | n `elem` readers ->
            ( case args of
                Core _ (CStrLit name) : _ -> Just name
                _ -> Nothing
            )
              : concatMap readsIn (args <> map snd kws)
      CVar (BuiltinRef n) | n `elem` readers -> [Nothing]
      _ -> concatMap readsIn (children c)

declEnvRefs :: CoreDecl -> [EnvRef]
declEnvRefs cd = envRefsIn (cdCore cd)

-- | Every environment expression within a core expression, keyword
-- defaults of its lambdas included.
envRefsIn :: Core -> [EnvRef]
envRefsIn c = case coreF c of
  CEnv kind args -> mkRef kind args : concatMap (envRefsIn . snd) args
  _ -> concatMap envRefsIn (children c)

mkRef :: Text -> [(Text, Core)] -> EnvRef
mkRef kind args = case kind of
  "docker" -> case (lookup "image" args, lookup "dockerfile" args) of
    (Just (Core _ (CStrLit img)), _) -> EnvRef img "docker" img
    (_, Just (Core _ (CStrLit df))) -> EnvRef (recipeSource df) "docker" ("recipe " <> recipeSource df)
    _ -> EnvRef "docker" "docker" "?"
  _ -> EnvRef kind kind kind

-- | Every sub-expression of a core node, transitively.
descendants :: Core -> [Core]
descendants c = let cs = children c in cs <> concatMap descendants cs

children :: Core -> [Core]
children = coreChildren

-- | The image a head names (spec 6.7, 10.2): the host, a registry
-- reference for a platform ('Nothing' is the daemon's own), or a
-- recipe.
data HeadImage
  = HeadLocal
  | HeadRegistry Text (Maybe Text)
  | HeadRecipe Recipe
  deriving (Show, Eq, Ord)

-- | Where a head is written, for the REQUIRED BY column of
-- @lask envs list@ and @lask sync@: the module, the declaration (or
-- the command words of the entry module whose environment it is), and
-- the keyword parameter whose default it is, if any.
data Requirer = Requirer
  { rqModule :: FilePath,
    rqWhat :: Text,
    rqDefault :: Maybe Text
  }
  deriving (Show, Eq, Ord)

-- | One head, and where it is written.
data HeadUse = HeadUse
  { huImage :: HeadImage,
    huBy :: Requirer
  }
  deriving (Show, Eq, Ord)

-- | A head and the span it is written at, before deduplication.
data Found = Found Span HeadUse

-- | Every head the program can evaluate (spec 10.3, 11.4), with where
-- it is written. With a declaration, only what it reaches.
--
-- A head is counted where it is written. Dispatch (10.9) copies the
-- environment of a command word into every command string that uses
-- it, so the same head is found again in each declaration that runs
-- the word; those copies carry the span of the head in the command
-- declaration, and are attributed to it.
collectHeadUses :: CoreProgram -> Maybe Key -> [HeadUse]
collectHeadUses core scope = dedupe $ case scope of
  Just key -> concatMap declUses (reachableDecls core key)
  Nothing ->
    concatMap declUses (reachableFrom core roots)
      <> concat
        [ usesIn (Requirer (cpEntry core) ("command " <> w) Nothing) c
        | (w, c) <- Map.toList commands
        ]
  where
    dedupe found =
      Set.toList . Set.fromList $
        [ pick group
        | group <- Map.elems (Map.fromListWith (<>) [((sp, huImage u), [u]) | Found sp u <- found, sp /= NoSpan])
        ]
          <> [u | Found NoSpan u <- found]
    -- The command declaration a copy came from, when it is among them.
    pick group = case [u | u <- group, "command " `T.isPrefixOf` rqWhat (huBy u)] of
      u : _ -> u
      [] -> minimum group
    commands = Map.findWithDefault Map.empty (cpEntry core) (cpCommands core)
    roots =
      [k | k@(p, _) <- Map.keys (cpDecls core), p == cpEntry core]
        <> [(p, n) | c <- Map.elems commands, CVar (TopRef p n) <- map coreF (c : descendants c)]
    declUses cd = usesIn (Requirer (cdModule cd) (cdName cd) Nothing) (cdCore cd)

-- | The heads within a core expression. A head in the default of a
-- keyword parameter is marked with the parameter, since it is required
-- whether or not a caller passes another (spec 10.3).
usesIn :: Requirer -> Core -> [Found]
usesIn by c = case coreF c of
  CEnv "local" _ -> [Found (coreSpan c) (HeadUse HeadLocal by)]
  CEnv "docker" args -> maybe [] (\i -> [Found (coreSpan c) (HeadUse i by)]) (headImage args) <> concatMap (usesIn by . snd) args
  CLam lam ->
    concat [usesIn by {rqDefault = Just k} d | (k, d) <- lamKeywords lam]
      <> usesIn by (lamBody lam)
  _ -> concatMap (usesIn by) (children c)

headImage :: [(Text, Core)] -> Maybe HeadImage
headImage args = case (lookup "image" args, lookup "dockerfile" args) of
  (Just (Core _ (CStrLit ref)), _) -> Just (HeadRegistry ref (literalArg "platform" args))
  (_, Just (Core _ (CStrLit df))) ->
    Just . HeadRecipe $
      Recipe
        { rcDockerfile = df,
          rcContext = maybe (defaultContext df) id (literalArg "context" args),
          rcBuildArgs = case lookup "build_args" args of
            Just (Core _ (CMapLit kvs)) -> sortOn fst [(k, v) | (k, Core _ (CStrLit v)) <- kvs]
            _ -> [],
          rcPlatform = literalArg "platform" args
        }
  _ -> Nothing

-- | Every registry reference the program requires (spec 10.3): those
-- named by the heads reachable from its entry module. Every reference
-- is written as a head (6.7), so none is left out.
collectRegistryRefs :: CoreProgram -> [Text]
collectRegistryRefs = Set.toList . Set.fromList . map fst . collectRegistryImages

-- | Every registry reference the program requires, with the platform
-- each head names it for (spec 10.3). 'Nothing' is the daemon's own.
collectRegistryImages :: CoreProgram -> [(Text, Maybe Text)]
collectRegistryImages core =
  Set.toList (Set.fromList [(ref, p) | HeadUse (HeadRegistry ref p) _ <- collectHeadUses core Nothing])

-- | Every recipe environment the program requires (spec 10.2, 10.3).
-- The context defaults to the Dockerfile's directory, and the build
-- arguments and the platform are carried because the recipe hash
-- covers them (10.3).
collectRecipes :: CoreProgram -> [Recipe]
collectRecipes core =
  Set.toList (Set.fromList [r | HeadUse (HeadRecipe r) _ <- collectHeadUses core Nothing])

-- | An image option, which is always a literal (spec 6.7).
literalArg :: Text -> [(Text, Core)] -> Maybe Text
literalArg k args = case lookup k args of
  Just (Core _ (CStrLit v)) -> Just v
  _ -> Nothing

-- | The context of a recipe that names none: the Dockerfile's
-- directory (spec 10.2).
defaultContext :: Text -> Text
defaultContext df =
  let parts = T.splitOn "/" df
   in if length parts <= 1 then "." else T.intercalate "/" (init parts)

