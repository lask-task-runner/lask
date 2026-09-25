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

data EnvRef = EnvRef
  { refLabel :: Text,
    refKind :: Text,
    refTarget :: Text
  }
  deriving (Show, Eq, Ord)

-- | All environment constructions in the core program, including the
-- environments named by command declarations (spec ch. 5). A declared
-- command must be enumerable and materializable even when no task uses
-- it, because @lask cmd@ can invoke it (spec 11.8). A declaration's
-- environment may be any expression, so it is walked whole; the
-- declarations it calls are among those walked already.
collectEnvRefs :: CoreProgram -> [EnvRef]
collectEnvRefs core =
  concatMap declEnvRefs (Map.elems (cpDecls core))
    <> concatMap envRefsIn (commandEnvs core)

-- | The environments of every command declaration in the program.
commandEnvs :: CoreProgram -> [Core]
commandEnvs core = concatMap Map.elems (Map.elems (cpCommands core))

-- | The environments reachable from one declaration: its own
-- environment expressions plus those of every top-level declaration
-- it can reach. Reachability is an over-approximation (spec 11.4) —
-- every referenced declaration counts, whether or not it is actually
-- called.
collectEnvRefsFrom :: CoreProgram -> Key -> [EnvRef]
collectEnvRefsFrom core start = go Set.empty [start]
  where
    go :: Set Key -> [Key] -> [EnvRef]
    go _ [] = []
    go seen (k : rest)
      | k `Set.member` seen = go seen rest
      | otherwise = case Map.lookup k (cpDecls core) of
          Nothing -> go (Set.insert k seen) rest
          Just cd ->
            declEnvRefs cd
              <> go (Set.insert k seen) (topRefs (cdCore cd) <> rest)

    topRefs c =
      [(p, n) | CVar (TopRef p n) <- map coreF (c : descendants c)]

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
    (_, Just (Core _ (CStrLit df))) -> EnvRef df "docker" ("recipe " <> df)
    _ -> EnvRef "<dynamic>" "docker" "<dynamic image>"
  _ -> EnvRef kind kind kind

-- | Every sub-expression of a core node, transitively.
descendants :: Core -> [Core]
descendants c = let cs = children c in cs <> concatMap descendants cs

children :: Core -> [Core]
children = coreChildren

-- | Every registry reference the program writes as a literal (spec
-- 10.3), in all its modules and command declarations: the references
-- the lock pins. One computed at run time is not among them; it cannot
-- be pinned.
collectRegistryRefs :: CoreProgram -> [Text]
collectRegistryRefs core =
  Set.toList . Set.fromList $
    concatMap (go . cdCore) (Map.elems (cpDecls core)) <> concatMap go (commandEnvs core)
  where
    go c = case coreF c of
      CEnv "docker" args
        | Just (Core _ (CStrLit ref)) <- lookup "image" args -> ref : concatMap (go . snd) args
      _ -> concatMap go (children c)

-- | Every recipe environment the program constructs, as
-- (dockerfile, context, build arguments) triples (spec 10.2). The
-- context defaults to the Dockerfile's directory, and the build
-- arguments are carried because the recipe hash covers them (10.3).
collectRecipes :: CoreProgram -> [(Text, Text, [(Text, Text)])]
collectRecipes core = concatMap fromDecl (Map.elems (cpDecls core)) <> concatMap go (commandEnvs core)
  where
    fromDecl cd = go (cdCore cd)
    go c = case coreF c of
      CEnv "docker" args -> case lookup "dockerfile" args of
        Just (Core _ (CStrLit df)) ->
          let ctx = case lookup "context" args of
                Just (Core _ (CStrLit x)) -> x
                _ -> defaultContext df
           in [(df, ctx, buildArgs args)]
        _ -> []
      CLam lam -> concatMap (go . snd) (lamKeywords lam) <> concatMap go (children c)
      _ -> concatMap go (children c)

    buildArgs args = case lookup "build_args" args of
      Just (Core _ (CMapLit kvs)) -> sortOn fst [(k, v) | (k, Core _ (CStrLit v)) <- kvs]
      _ -> []

    defaultContext df =
      let parts = T.splitOn "/" df
       in if length parts <= 1 then "." else T.intercalate "/" (init parts)

