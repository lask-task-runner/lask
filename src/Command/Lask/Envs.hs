{-# LANGUAGE OverloadedStrings #-}

-- | Static enumeration of the execution environments a program
-- constructs (spec 11.4), shared by @lask envs@ and by the
-- environment section of function help (spec 11.6).
module Command.Lask.Envs
  ( EnvRef (..),
    collectEnvRefs,
    collectEnvRefsFrom,
  )
where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), Key)
import Language.Lask.EnvFile
import Language.Lask.Runtime.Value (Value (..))

data EnvRef = EnvRef
  { refLabel :: Text,
    refKind :: Text,
    refTarget :: Text
  }
  deriving (Show, Eq, Ord)

-- | All environment constructions in the core program.
collectEnvRefs :: CoreProgram -> Maybe EnvFile -> [EnvRef]
collectEnvRefs core envFile =
  concatMap (declEnvRefs envFile) (Map.elems (cpDecls core))

-- | The environments reachable from one declaration: its own
-- environment expressions plus those of every top-level declaration
-- it can reach. Reachability is an over-approximation (spec 11.4) —
-- every referenced declaration counts, whether or not it is actually
-- called.
collectEnvRefsFrom :: CoreProgram -> Maybe EnvFile -> Key -> [EnvRef]
collectEnvRefsFrom core envFile start = go Set.empty [start]
  where
    go :: Set Key -> [Key] -> [EnvRef]
    go _ [] = []
    go seen (k : rest)
      | k `Set.member` seen = go seen rest
      | otherwise = case Map.lookup k (cpDecls core) of
          Nothing -> go (Set.insert k seen) rest
          Just cd ->
            declEnvRefs envFile cd
              <> go (Set.insert k seen) (topRefs (cdCore cd) <> rest)

    topRefs c =
      [(p, n) | CVar (TopRef p n) <- map coreF (c : descendants c)]

declEnvRefs :: Maybe EnvFile -> CoreDecl -> [EnvRef]
declEnvRefs envFile cd = concatMap fromCore (cdCore cd : keywordDefaults (cdCore cd))
  where
    -- A lambda's keyword defaults are not part of its body, so they
    -- have to be walked separately.
    keywordDefaults c = case coreF c of
      CLam lam -> map snd (lamKeywords lam)
      _ -> []

    fromCore c = case coreF c of
      CEnv kind args -> mkRef envFile kind args : concatMap (fromCore . snd) args
      _ -> concatMap fromCore (children c)

mkRef :: Maybe EnvFile -> Text -> [(Text, Core)] -> EnvRef
mkRef envFile kind args = case kind of
  "docker" -> case lookup "image" args of
    Just (Core _ (CStrLit img)) -> EnvRef img "docker" img
    _ -> EnvRef "<dynamic>" "docker" "<dynamic image>"
  "env" -> case lookup "name" args of
    Just (Core _ (CStrLit name)) ->
      case envFile >>= Map.lookup name . envFileEntries of
        Just (EnvEntry k ps) -> EnvRef name k (entryTarget k ps)
        Nothing -> EnvRef name "env" "<undefined>"
    _ -> EnvRef "<env>" "env" "<unknown>"
  _ -> EnvRef kind kind kind
  where
    entryTarget k ps = case k of
      "remote" -> case Map.lookup "host" ps of
        Just (VString h) -> h
        _ -> "<unknown host>"
      "docker" -> case Map.lookup "image" ps of
        Just (VString i) -> i
        _ -> "<unknown image>"
      _ -> k

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
