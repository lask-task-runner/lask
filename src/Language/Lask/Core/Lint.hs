{-# LANGUAGE OverloadedStrings #-}

-- | Core.Lint: checks of a program's Core that both front ends must
-- uphold, run by 'Language.Lask.Embed.assemble' and over every program
-- the elaborator produces in the test suite (#91).
--
-- This is the untyped first step. It checks:
--
-- * every local is bound, by a parameter or an earlier binding of its
--   block;
-- * every top-level reference names a declaration of the program;
-- * every built-in reference names a built-in;
-- * a call to a declaration gives as many positional arguments as it
--   declares, and only keywords it declares, each once;
-- * a call to a built-in gives as many arguments as its scheme takes.
--
-- Types are checked by the front ends; Core does not yet carry the
-- types a bottom-up type check would need.
module Language.Lask.Core.Lint
  ( lintProgram,
    lintDecl,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Names (coreFunctionNames)
import Language.Lask.Builtins.Sig (Scheme (..), builtinSchemes)
import Language.Lask.Core.AST
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), StaticParams (..))

-- | Every problem in the program, each prefixed with its declaration.
lintProgram :: CoreProgram -> [Text]
lintProgram cp = concatMap (lintDecl cp) (Map.elems (cpDecls cp))

-- | The problems of one declaration of the program, each prefixed
-- with its name.
lintDecl :: CoreProgram -> CoreDecl -> [Text]
lintDecl cp cd = map ((cdName cd <> ": ") <>) (nub (go Set.empty (cdCore cd)))
  where
    go :: Set Text -> Core -> [Text]
    go scope c = case coreF c of
      CVar (LocalRef n)
        | n `Set.member` scope -> []
        | otherwise -> ["the local " <> n <> " is not bound"]
      CVar (TopRef p n)
        | Map.member (p, n) (cpDecls cp) -> []
        | otherwise -> ["the declaration " <> n <> " of " <> T.pack p <> " does not exist"]
      CVar (BuiltinRef n)
        | known n -> []
        | otherwise -> ["the built-in " <> n <> " does not exist"]
      CLam lam ->
        let inner = Set.fromList (lamPositional lam <> maybe [] pure (lamVariadic lam) <> map fst (lamKeywords lam)) <> scope
         in concatMap (go inner . snd) (lamKeywords lam) <> go inner (lamBody lam)
      CDo stmts -> block scope stmts
      CApp fn pos kws -> callProblems fn (length pos) (map fst kws) <> go scope fn <> concatMap (go scope) (pos <> map snd kws)
      _ -> concatMap (go scope) (coreChildren c)

    block _ [] = []
    block scope (CSExpr e : rest) = go scope e <> block scope rest
    block scope (CSBind n e : rest) = go scope e <> block (Set.insert n scope) rest

    callProblems fn npos kws = case coreF fn of
      CVar (TopRef p n) | Just callee <- Map.lookup (p, n) (cpDecls cp), Just sp <- cdParams callee ->
        let declared = length (spPositional sp)
            variadic = spVariadic sp /= Nothing
         in [ "calls " <> n <> " with " <> tshow npos <> " positional arguments, but it declares " <> tshow declared
              | if variadic then npos < declared else npos /= declared
            ]
              <> [ "calls " <> n <> " with --" <> k <> ", which it does not declare"
                   | k <- kws,
                     k `notElem` map fst (spKeywords sp)
                 ]
              <> [ "calls " <> n <> " with --" <> k <> " more than once"
                   | k <- nub kws,
                     length (filter (== k) kws) > 1
                 ]
      CVar (BuiltinRef n) | Just s <- Map.lookup n builtinSchemes ->
        let declared = length (schemeParams s)
         in [ "calls the built-in " <> n <> " with " <> tshow npos <> " arguments, but it takes " <> tshow declared
              | npos /= declared
            ]
      _ -> []

    known n = Map.member n builtinSchemes || n `Set.member` coreFunctionNames || n `elem` internalNames

    -- Names the elaborator refers to that no program can write.
    internalNames = ["stdin", "%commandFail"]

    tshow = T.pack . show
