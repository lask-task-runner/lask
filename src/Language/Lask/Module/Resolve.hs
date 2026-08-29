{-# LANGUAGE OverloadedStrings #-}

-- | Symbol tables and name resolution (spec 7.1-7.3).
--
-- Resolution ranks (spec 7.2): 1 innermost local binding, 2 current
-- module top level, 3 named imports, 4 namespace member, 5 builtins
-- (including the reserved @stdin@). Core function names and @stdin@
-- cannot be bound at any rank 1-4 position (@E-NAME-DUPLICATE@).
module Language.Lask.Module.Resolve
  ( GlobalScope (..),
    ValueTarget (..),
    TypeTarget (..),
    Publics (..),
    modulePublics,
    buildScopes,
    validateProgram,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Names
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.ErrorCode
import Language.Lask.Lexer.Token (Spanned (..))
import Language.Lask.Module.Loader (LoadedModule (..), Program (..))
import Language.Lask.Span (Span)
import Language.Lask.Syntax.AST

data ValueTarget
  = -- | Defining module and name.
    VTopLevel FilePath Text
  | VBuiltin Text
  deriving (Show, Eq)

data TypeTarget
  = -- | Defining module and alias name.
    TAlias FilePath Text
  | TBuiltinAlias Text
  deriving (Show, Eq, Ord)

-- | Names visible at the top level of one module.
data GlobalScope = GlobalScope
  { gsValues :: Map Text ValueTarget,
    gsTypes :: Map Text TypeTarget,
    gsNamespaces :: Map Text FilePath
  }
  deriving (Show)

-- | Build per-module scopes, check binding collisions, reference
-- validity and type-alias acyclicity. Returns all diagnostics found.
validateProgram :: Program -> Either [Diagnostic] (Map FilePath GlobalScope)
validateProgram prog =
  let publics = Map.map modulePublics (progModules prog)
      scoped = scopedModules prog
      scopes = Map.fromList [(p, gs) | (p, (gs, _)) <- scoped]
      buildDiags = concat [ds | (_, (_, ds)) <- scoped]
      refDiags =
        concat
          [ checkModule publics (scopes Map.! lmPath lm) lm
          | lm <- Map.elems (progModules prog)
          ]
      cycleDiags = aliasCycleDiags prog scopes
      allDiags = buildDiags <> refDiags <> cycleDiags
   in if null allDiags then Right scopes else Left allDiags

-- | Per-module scopes with the collision diagnostics discarded, so
-- that editor tooling can still resolve names in a program that does
-- not validate.
buildScopes :: Program -> Map FilePath GlobalScope
buildScopes prog = Map.fromList [(p, gs) | (p, (gs, _)) <- scopedModules prog]

scopedModules :: Program -> [(FilePath, (GlobalScope, [Diagnostic]))]
scopedModules prog =
  [ (lmPath lm, buildScope prog publics lm)
  | lm <- Map.elems (progModules prog)
  ]
  where
    publics = Map.map modulePublics (progModules prog)

data Publics = Publics
  { pubValues :: Set Text,
    pubTypes :: Set Text
  }

modulePublics :: LoadedModule -> Publics
modulePublics lm =
  Publics
    { pubValues = Set.fromList (mapMaybe valueName decls),
      pubTypes = Set.fromList (mapMaybe typeName decls)
    }
  where
    decls = map declF (moduleDecls (lmModule lm))
    valueName (DValue n _ _ _) = Just n
    valueName (DFunction n _ _ _) = Just n
    valueName _ = Nothing
    typeName (DTypeAlias n _) = Just n
    typeName _ = Nothing

-- Scope construction ---------------------------------------------------------

buildScope :: Program -> Map FilePath Publics -> LoadedModule -> (GlobalScope, [Diagnostic])
buildScope _prog publics lm = go base [] (moduleDecls (lmModule lm))
  where
    base =
      GlobalScope
        { gsValues =
            Map.fromList
              [(n, VBuiltin n) | n <- Set.toList builtinValueNames]
              <> Map.singleton "stdin" (VBuiltin "stdin"),
          gsTypes =
            Map.fromList
              [(n, TBuiltinAlias n) | n <- Set.toList builtinTypeAliasNames],
          gsNamespaces = Map.empty
        }

    go gs ds [] = (gs, reverse ds)
    go gs ds (Decl sp f : rest) = case f of
      DValue n _ _ _ -> goValue gs ds rest sp n
      DFunction n _ _ _ -> goValue gs ds rest sp n
      DTypeAlias n _ ->
        let dups =
              [ dupDiag sp n
                | userType n (gsTypes gs) || n `Set.member` builtinTypeAliasNames
              ]
         in go gs {gsTypes = Map.insert n (TAlias (lmPath lm) n) (gsTypes gs)} (dups <> ds) rest
      DImportNamed specs path ->
        let key = resolveKey path
            (gs', newDs) = foldl (addImport key) (gs, []) specs
         in go gs' (newDs <> ds) rest
      DImportNamespace alias path ->
        let key = resolveKey path
            dups =
              [dupDiag sp alias | conflictsValue alias gs || Map.member alias (gsNamespaces gs)]
                <> [coreDiag sp alias | isUnbindableName alias]
         in go gs {gsNamespaces = Map.insert alias key (gsNamespaces gs)} (dups <> ds) rest
      where
        goValue gs' ds' rest' sp' n =
          let dups =
                [dupDiag sp' n | conflictsValue n gs' || Map.member n (gsNamespaces gs')]
                  <> [coreDiag sp' n | isUnbindableName n]
           in go gs' {gsValues = Map.insert n (VTopLevel (lmPath lm) n) (gsValues gs')} (dups <> ds') rest'

    resolveKey path = Map.findWithDefault (T.unpack path) path (lmImportKeys lm)

    addImport key (gs, ds) (ImportSpec sp name alias) =
      let visible = maybe name id alias
          isType = startsUpper name
          existsDiag = case Map.lookup key publics of
            Nothing -> []
            Just pub
              | isType && name `Set.member` pubTypes pub -> []
              | not isType && name `Set.member` pubValues pub -> []
              | otherwise ->
                  [ mkDiagnostic ENameUndefined StageStatic sp $
                      "module has no public symbol named '" <> name <> "'"
                  ]
          dups
            | isType =
                [ dupDiag sp visible
                  | userType visible (gsTypes gs) || visible `Set.member` builtinTypeAliasNames
                ]
            | otherwise =
                [dupDiag sp visible | conflictsValue visible gs || Map.member visible (gsNamespaces gs)]
                  <> [coreDiag sp visible | isUnbindableName visible]
          gs'
            | isType = gs {gsTypes = Map.insert visible (TAlias key name) (gsTypes gs)}
            | otherwise = gs {gsValues = Map.insert visible (VTopLevel key name) (gsValues gs)}
       in (gs', existsDiag <> dups <> ds)

    -- A collision exists only against user-introduced names; builtins
    -- (rank 5) are shadowable.
    conflictsValue n gs = case Map.lookup n (gsValues gs) of
      Just (VTopLevel _ _) -> True
      _ -> False
    userType n types = case Map.lookup n types of
      Just (TAlias _ _) -> True
      _ -> False
    startsUpper t = maybe False (\(c, _) -> c >= 'A' && c <= 'Z') (T.uncons t)

dupDiag :: Span -> Text -> Diagnostic
dupDiag sp n =
  mkDiagnostic ENameDuplicate StageStatic sp ("duplicate name: '" <> n <> "'")

coreDiag :: Span -> Text -> Diagnostic
coreDiag sp n =
  mkDiagnostic ENameDuplicate StageStatic sp $
    "'" <> n <> "' is a core function or reserved identifier and cannot be bound"

-- Reference checking ---------------------------------------------------------

type Scope = [Set Text]

inScope :: Text -> Scope -> Bool
inScope n = any (Set.member n)

checkModule :: Map FilePath Publics -> GlobalScope -> LoadedModule -> [Diagnostic]
checkModule publics gs lm = concatMap checkDecl (moduleDecls (lmModule lm))
  where
    checkDecl (Decl _ f) = case f of
      DValue _ _ t e -> maybe [] checkType t <> checkExpr [] e
      DFunction _ ps t body ->
        checkParams [] ps
          <> maybe [] checkType t
          <> checkExpr [paramNames ps] body
      DTypeAlias _ t -> checkType t
      DImportNamed {} -> []
      DImportNamespace {} -> []

    paramNames ps = Set.fromList [paramName p | p <- ps]
    paramName (Param _ (PPositional n _ _)) = n
    paramName (Param _ (PVariadic n _)) = n
    paramName (Param _ (PKeyword n _ _ _)) = n

    -- Parameters: duplicate/core-name checks, annotation checks, and
    -- default expressions checked in the scope of preceding params
    -- (spec 8.3).
    checkParams :: Scope -> [Param] -> [Diagnostic]
    checkParams outer = go Set.empty
      where
        go _ [] = []
        go seen (Param sp f : rest) =
          let (n, t, d) = case f of
                PPositional n' _ t' -> (n', t', Nothing)
                PVariadic n' t' -> (n', t', Nothing)
                PKeyword n' _ t' d' -> (n', t', Just d')
           in [dupDiag sp n | n `Set.member` seen]
                <> [coreDiag sp n | isUnbindableName n]
                <> maybe [] checkType t
                <> maybe [] (checkExpr (seen : outer)) d
                <> go (Set.insert n seen) rest

    checkType :: SType -> [Diagnostic]
    checkType (SType sp f) = case f of
      SNamed n
        | Map.member n (gsTypes gs) -> []
        | otherwise ->
            [mkDiagnostic ENameUndefined StageStatic sp ("undefined type: '" <> n <> "'")]
      SArray t -> checkType t
      SMap t -> checkType t
      SAsyncHandle t -> checkType t
      SRecord fs -> concatMap (checkType . snd) fs
      SFunction ps r -> concatMap checkType ps <> checkType r
      _ -> []

    checkExpr :: Scope -> Expr -> [Diagnostic]
    checkExpr sc (Expr sp f) = case f of
      EVar n
        | inScope n sc || Map.member n (gsValues gs) -> []
        | otherwise -> [undefDiag sp n]
      EDot inner fld -> case exprF inner of
        EVar m
          | not (inScope m sc),
            not (Map.member m (gsValues gs)),
            Just key <- Map.lookup m (gsNamespaces gs) ->
              -- Namespace member reference (rank 4).
              case Map.lookup key publics of
                Just pub
                  | spannedValue fld `Set.member` pubValues pub -> []
                _ ->
                  [ mkDiagnostic ENameUndefined StageStatic (spannedSpan fld) $
                      "namespace '" <> m <> "' has no public symbol '" <> spannedValue fld <> "'"
                  ]
        _ -> checkExpr sc inner
      EIndex e i -> checkExpr sc e <> checkExpr sc i
      ECall fn as -> checkExpr sc fn <> concatMap (checkArg sc) as
      EArray es -> concatMap (checkExpr sc) es
      EObject kvs -> concatMap (checkExpr sc . snd) kvs
      EString ps -> concatMap (checkPart sc) ps
      ELambda ps t body ->
        checkParams sc ps
          <> maybe [] checkType t
          <> checkExpr (paramNames ps : sc) body
      EBin _ a b -> checkExpr sc a <> checkExpr sc b
      ENot e -> checkExpr sc e
      EDo b -> checkBlock sc b
      EIf c t e -> checkExpr sc c <> checkBlock sc t <> maybe [] (checkBlock sc) e
      EFor (Spanned xsp x) xs body ->
        checkExpr sc xs
          <> [coreDiag xsp x | isUnbindableName x]
          <> checkBlock (Set.singleton x : sc) body
      ETry body mCatch mFin ->
        checkBlock sc body
          <> maybe
            []
            ( \(Spanned esp e, h) ->
                [coreDiag esp e | isUnbindableName e]
                  <> checkBlock (Set.singleton e : sc) h
            )
            mCatch
          <> maybe [] (checkBlock sc) mFin
      EAsync e -> checkExpr sc e
      EAwait e -> checkExpr sc e
      ECommand _ env ps -> maybe [] (checkExpr sc) env <> concatMap (checkPart sc) ps
      EEnv _ as -> maybe [] (concatMap (checkArg sc)) as
      _ -> []
      where
        undefDiag s n =
          mkDiagnostic ENameUndefined StageStatic s ("undefined name: '" <> n <> "'")

    checkArg sc (Arg _ (APos e)) = checkExpr sc e
    checkArg sc (Arg _ (AKw _ e)) = checkExpr sc e

    checkPart sc (TPInterp e) = checkExpr sc e
    checkPart _ (TPChunk _) = []

    -- Do blocks: bindings become visible only after their statement;
    -- rebinding in the same block is a duplicate (spec 7.3, 6.5).
    checkBlock :: Scope -> Block -> [Diagnostic]
    checkBlock outer (Block _ stmts) = go (Set.empty : outer) stmts
      where
        go _ [] = []
        go sc@(layer : rest0) (Stmt sp f : rest) = case f of
          SBind n _ e ->
            checkExpr sc e
              <> [dupDiag sp n | n `Set.member` layer]
              <> [coreDiag sp n | isUnbindableName n]
              <> go (Set.insert n layer : rest0) rest
          SExpr e -> checkExpr sc e <> go sc rest
          SReturn e -> checkExpr sc e <> go sc rest
          SGuard c b -> checkExpr sc c <> checkBlock sc b <> go sc rest
        go [] _ = [] -- unreachable: block scope always pushed

-- Type alias cycle detection (spec 4.2) --------------------------------------

aliasCycleDiags :: Program -> Map FilePath GlobalScope -> [Diagnostic]
aliasCycleDiags prog scopes =
  [ mkDiagnostic ETypeIllformed StageStatic sp $
      "type alias '" <> n <> "' is recursive (directly or mutually)"
  | (path, n, sp) <- aliasNodes,
    reachesSelf (path, n)
  ]
  where
    aliasNodes =
      [ (lmPath lm, n, declSpan d)
      | lm <- Map.elems (progModules prog),
        d <- moduleDecls (lmModule lm),
        DTypeAlias n _ <- [declF d]
      ]

    edges :: Map (FilePath, Text) [(FilePath, Text)]
    edges =
      Map.fromList
        [ ((lmPath lm, n), targets (lmPath lm) t)
        | lm <- Map.elems (progModules prog),
          d <- moduleDecls (lmModule lm),
          DTypeAlias n t <- [declF d]
        ]

    targets path t = [tgt | n <- namedRefs t, Just tgt <- [resolveType path n]]

    resolveType path n = case Map.lookup path scopes of
      Just gs -> case Map.lookup n (gsTypes gs) of
        Just (TAlias p a) -> Just (p, a)
        _ -> Nothing
      Nothing -> Nothing

    namedRefs (SType _ f) = case f of
      SNamed n -> [n]
      SArray t -> namedRefs t
      SMap t -> namedRefs t
      SAsyncHandle t -> namedRefs t
      SRecord fs -> concatMap (namedRefs . snd) fs
      SFunction ps r -> concatMap namedRefs ps <> namedRefs r
      _ -> []

    reachesSelf start = go Set.empty (Map.findWithDefault [] start edges)
      where
        go _ [] = False
        go seen (x : xs)
          | x == start = True
          | x `Set.member` seen = go seen xs
          | otherwise = go (Set.insert x seen) (Map.findWithDefault [] x edges <> xs)
