{-# LANGUAGE OverloadedStrings #-}

-- | Static enumeration of the execution environments a program
-- constructs (spec 11.4), shared by @lask envs@ and by the
-- environment section of function help (spec 11.6).
module Command.Lask.Envs
  ( EnvRef (..),
    collectEnvRefs,
    collectEnvRefsFrom,
    collectRecipes,
  )
where

import Data.Map.Strict (Map)
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

-- | All environment constructions in the core program.
collectEnvRefs :: CoreProgram -> [EnvRef]
collectEnvRefs core =
  concatMap declEnvRefs (Map.elems (cpDecls core))

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
declEnvRefs cd = concatMap fromCore (cdCore cd : keywordDefaults (cdCore cd))
  where
    -- A lambda's keyword defaults are not part of its body, so they
    -- have to be walked separately.
    keywordDefaults c = case coreF c of
      CLam lam -> map snd (lamKeywords lam)
      _ -> []

    fromCore c = case coreF c of
      CEnv kind args -> mkRef kind args : concatMap (fromCore . snd) args
      _ -> concatMap fromCore (children c)

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
children c = case coreF c of
  CStr ps -> [e | CPExpr e <- ps]
  CArray es -> es
  CMapLit kvs -> map snd kvs
  CRecordLit kvs -> map snd kvs
  CLam lam -> map snd (lamKeywords lam) <> [lamBody lam]
  CApp fn pos kw -> fn : pos <> map snd kw
  CDot e _ -> [e]
  CIndex _ a b -> [a, b]
  CIf a b c' -> [a, b, c']
  CAnd a b -> [a, b]
  COr a b -> [a, b]
  CNot a -> [a]
  CBin _ a b -> [a, b]
  CDo stmts -> concatMap stmtExpr stmts
  CAwait a -> [a]
  CCast a _ -> [a]
  CEnv _ args -> map snd args
  _ -> []
  where
    stmtExpr (CSBind _ e) = [e]
    stmtExpr (CSExpr e) = [e]

-- | Every recipe environment the program constructs, as
-- (dockerfile, context) pairs (spec 10.2). The context defaults to the
-- Dockerfile's directory.
collectRecipes :: CoreProgram -> [(Text, Text)]
collectRecipes core = concatMap fromDecl (Map.elems (cpDecls core))
  where
    fromDecl cd = go (cdCore cd)
    go c = case coreF c of
      CEnv "docker" args -> case lookup "dockerfile" args of
        Just (Core _ (CStrLit df)) ->
          let ctx = case lookup "context" args of
                Just (Core _ (CStrLit x)) -> x
                _ -> defaultContext df
           in [(df, ctx)]
        _ -> []
      CLam lam -> concatMap (go . snd) (lamKeywords lam) <> concatMap go (children c)
      _ -> concatMap go (children c)

    defaultContext df =
      let parts = T.splitOn "/" df
       in if length parts <= 1 then "." else T.intercalate "/" (init parts)

