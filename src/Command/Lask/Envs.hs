{-# LANGUAGE OverloadedStrings #-}

-- | Static enumeration of the execution environments a program
-- constructs (spec 11.4), shared by @lask envs@ and by the
-- environment section of function help (spec 11.6).
module Command.Lask.Envs
  ( EnvRef (..),
    collectEnvRefs,
    collectEnvRefsFrom,
    collectRecipes,
    collectRegistryRefs,
    collectRegistryImages,
    collectEnvReadsFrom,
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

-- | Every registry reference the program requires (spec 10.3): those
-- named by the heads reachable from its entry module. Every reference
-- is written as a head (6.7), so none is left out.
collectRegistryRefs :: CoreProgram -> [Text]
collectRegistryRefs = Set.toList . Set.fromList . map fst . collectRegistryImages

-- | Every registry reference the program requires, with the platform
-- each head names it for (spec 10.3). 'Nothing' is the daemon's own.
collectRegistryImages :: CoreProgram -> [(Text, Maybe Text)]
collectRegistryImages core = Set.toList . Set.fromList $ concatMap go (reachableCores core)
  where
    go c = case coreF c of
      CEnv "docker" args
        | Just (Core _ (CStrLit ref)) <- lookup "image" args ->
            (ref, literalArg "platform" args) : concatMap (go . snd) args
      _ -> concatMap go (children c)

-- | Every recipe environment the program requires (spec 10.2, 10.3).
-- The context defaults to the Dockerfile's directory, and the build
-- arguments and the platform are carried because the recipe hash
-- covers them (10.3).
collectRecipes :: CoreProgram -> [Recipe]
collectRecipes core = Set.toList . Set.fromList $ concatMap go (reachableCores core)
  where
    go c = case coreF c of
      CEnv "docker" args -> case lookup "dockerfile" args of
        Just (Core _ (CStrLit df)) ->
          [ Recipe
              { rcDockerfile = df,
                rcContext = maybe (defaultContext df) id (literalArg "context" args),
                rcBuildArgs = buildArgs args,
                rcPlatform = literalArg "platform" args
              }
          ]
        _ -> []
      _ -> concatMap go (children c)

    buildArgs args = case lookup "build_args" args of
      Just (Core _ (CMapLit kvs)) -> sortOn fst [(k, v) | (k, Core _ (CStrLit v)) <- kvs]
      _ -> []

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

