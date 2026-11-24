{-# LANGUAGE OverloadedStrings #-}

-- | Random well-formed surface modules for the syntax properties: a
-- generator, a shrinker, and a printer whose output the parser reads
-- back as the module it was printed from.
--
-- The printer is not "Language.Lask.Syntax.Printer", which formats a
-- source it is given. This one writes every token apart, every block
-- one statement per line, and the parentheses the grammar needs, so
-- that the parser is checked against something simple. A few
-- spellings follow from the module rather than from chance, so that
-- they are covered all the same:
--
--   * a declaration whose name starts with @pub@ is exported, and so is
--     a command declaration whose first word is @npm@;
--   * a string whose only chunk holds a quote or a backslash, and no
--     single quote, is raw;
--   * a command that ends a line is written without braces.
--
-- No wide character (CJK, emoji) is written as itself: one in a string
-- is escaped, and a raw string holds none. Megaparsec counts such a
-- character as two columns, and the formatter does not (see the known
-- bugs in PropertiesSpec).
module Support.SyntaxGen
  ( genModule,
    shrinkModule,
    renderModule,
    genNoise,
    genMutation,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Maybe (isJust, isNothing)
import qualified Data.Scientific as S
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Lexer (lexTokens)
import Language.Lask.Lexer.Token (CmdStream (..), Op (..), Spanned (..), keywordFromText)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST
import Language.Lask.Syntax.Scope (childExprs)
import Numeric (showHex)
import Test.QuickCheck

ex :: ExprF -> Expr
ex = Expr NoSpan

sp :: a -> Spanned a
sp = Spanned NoSpan

ty :: STypeF -> SType
ty = SType NoSpan

-- Names -----------------------------------------------------------------------

-- | Value names, among them the contextual keywords, which stay usable
-- as identifiers (spec 3.3).
lowerNames :: [Text]
lowerNames = ["a", "b", "x", "xs", "f", "go_2", "_tmp", "pub_v", "on", "schema", "within", "access"]

upperNames :: [Text]
upperNames = ["Foo", "Bar", "Pair", "T2"]

typeVars :: [Text]
typeVars = ["T", "U", "Elem"]

namespaces :: [Text]
namespaces = ["ns", "lib"]

-- | Keys of objects and record types: some are identifiers, the others
-- have to be quoted.
keys :: [Text]
keys = ["a", "on", "if", "true", "Foo", "b-c", "with space", "q\"uote", "back\\slash", ""]

envHeads :: [Text]
envHeads = ["local", "alpine:3.12", "golang:1.25", "ghcr.io/org/img@sha256:ab12", "node-20"]

commandWords :: [Text]
commandWords = ["go", "gofmt", "npm", "x-y"]

paths :: [Text]
paths = ["./lib", "../shared/x.lask", "github.com/org/repo@v1"]

-- | What a string chunk is made of. No @{@, so no @#{@ ever forms.
stringChars :: String
stringChars = "ab z09_-./:'\"\\\n\t#}\233\26085\128512"

-- | What a command chunk is made of: nothing a command string treats
-- specially (@#@, @\\@, braces, a line end).
commandChars :: String
commandChars = "abc -_./=:|'\"$()*0\t"

-- Modules -------------------------------------------------------------------

genModule :: Gen Module
genModule = sized $ \n -> do
  let depth = min 4 (n `div` 20)
  k <- choose (0, 2 + n `div` 10)
  ds <- vectorOf k (genTopDecl depth)
  let decls = map fst ds
  pure
    ( Module
        decls
        (Set.fromList [v | (d, True) <- ds, Just v <- [declName d]])
        (Set.fromList [w | (Decl _ (DCommand ws _ _), True) <- ds, Spanned _ w <- ws])
    )

declName :: Decl -> Maybe Text
declName (Decl _ f) = case f of
  DValue n _ _ _ -> Just n
  DFunction n _ _ _ _ _ -> Just n
  DTypeAlias n _ _ -> Just n
  DAccess n _ -> Just n
  _ -> Nothing

-- | A declaration, and whether it is marked @internal@.
genTopDecl :: Int -> Gen (Decl, Bool)
genTopDecl d = do
  f <-
    frequency
      [ (4, DValue <$> elements lowerNames <*> genSecrecy <*> maybeOf (genType d) <*> genExpr d),
        (4, genFunction),
        (2, DTypeAlias <$> elements upperNames <*> genTypeParams d <*> genType d),
        (1, DImportNamed <$> listOf1' genImportSpec <*> elements paths),
        (1, DImportNamespace <$> elements namespaces <*> elements paths),
        (1, DExportFrom <$> listOf1' genImportSpec <*> elements paths),
        (1, DExportNamespace <$> elements namespaces <*> elements paths),
        (1, DWithin <$> genAccess d),
        (1, DAccess <$> elements upperNames <*> genAccessBraces d),
        (1, DCommand <$> genWords <*> genEnv d <*> maybeOf (genExpr d)),
        (1, DImportCommands <$> genWords <*> elements paths),
        (1, DExportCommandsFrom <$> genWords <*> elements paths)
      ]
  marked <- frequency [(4, pure False), (1, pure True)]
  let internal = marked && canBeInternal f
  pure (Decl NoSpan f, internal)
  where
    genFunction =
      DFunction
        <$> elements lowerNames
        <*> genTypeParams d
        <*> genParams d
        <*> maybeOf (genType d)
        <*> maybeOf (genAccess d)
        <*> genExpr d
    genWords = map sp <$> listOf1' (elements commandWords)
    canBeInternal f = case f of
      DValue {} -> True
      DFunction {} -> True
      DTypeAlias {} -> True
      DAccess {} -> True
      DCommand {} -> True
      _ -> False

genImportSpec :: Gen ImportSpec
genImportSpec = do
  upper <- arbitrary
  let pool = if upper then upperNames else lowerNames
  ImportSpec NoSpan <$> elements pool <*> maybeOf (elements pool)

genSecrecy :: Gen Secrecy
genSecrecy = frequency [(4, pure Public), (1, pure Secret)]

genAccess :: Int -> Gen AccessSpec
genAccess d =
  oneof
    [ genAccessBraces d,
      AccessRef NoSpan <$> maybeOf (elements namespaces) <*> elements upperNames
    ]

genAccessBraces :: Int -> Gen AccessSpec
genAccessBraces d = AccessSpec NoSpan <$> listOf' ((,) <$> (sp <$> elements lowerNames) <*> genExpr (d - 1))

-- Types ------------------------------------------------------------------------

genType :: Int -> Gen SType
genType d = frequency [(5, genSingleType d), (1, ty . SUnion <$> vectorOf' 2 3 (genSingleType (d - 1)))]

genSingleType :: Int -> Gen SType
genSingleType d = frequency [(3, genHeadType d), (1, genQualified)]
  where
    genQualified = ty <$> (SNamed . Just <$> elements namespaces <*> elements (upperNames <> ["Array"]) <*> genArgs)
    genArgs = if d <= 0 then pure [] else frequency [(3, pure []), (1, listOf1' (genType (d - 1)))]

-- | A type that may head a case arm (spec 6.4): one that begins with a
-- type name, and is no union.
genHeadType :: Int -> Gen SType
genHeadType d
  | d <= 0 = leaf
  | otherwise =
      frequency
        [ (3, leaf),
          (1, ty . SArray <$> sub),
          (1, ty . SMap <$> sub),
          (1, ty . SAsyncHandle <$> sub),
          (1, ty . SRecord <$> listOf' ((,,) <$> (sp <$> elements keys) <*> arbitrary <*> sub)),
          (1, ty <$> (SFunction <$> listOf' sub <*> sub)),
          (1, ty <$> (SNamed Nothing <$> elements upperNames <*> listOf1' sub))
        ]
  where
    sub = genType (d - 1)
    leaf =
      elements
        ( map ty [SAny, SNumber, SString, SBool, SNull, SVoid, SEnvironment, SRunnable]
            <> [ty (SNamed Nothing n []) | n <- upperNames <> typeVars]
        )

genTypeParams :: Int -> Gen [TypeParam]
genTypeParams d = frequency [(3, pure []), (1, listOf1' param)]
  where
    param = TypeParam <$> (sp <$> elements typeVars) <*> maybeOf bound
    bound = oneof [SBoundNamed . sp <$> elements ["orderable", "comparable"], SBoundType <$> genType (d - 1)]

-- | Positional parameters, then at most one variadic, then keyword
-- parameters (spec 6.1).
genParams :: Int -> Gen [Param]
genParams d = do
  pos <- listOf' (PPositional <$> elements lowerNames <*> genSecrecy <*> maybeOf (genType (d - 1)))
  var <- maybeOf (PVariadic <$> elements lowerNames <*> maybeOf (ty . SArray <$> genType (d - 1)))
  kws <- listOf' (PKeyword <$> elements lowerNames <*> genSecrecy <*> maybeOf (genType (d - 1)) <*> maybeOf (genExpr (d - 1)))
  pure (map (Param NoSpan) (pos <> maybe [] pure var <> kws))

-- Expressions ------------------------------------------------------------------

genExpr :: Int -> Gen Expr
genExpr d
  | d <= 0 = genLeaf
  | otherwise =
      frequency
        [ (4, genLeaf),
          (2, ex . EString <$> genStringParts d'),
          (2, ex . EArray <$> listOf' sub),
          (2, ex . EObject <$> listOf' ((,) <$> (sp <$> elements keys) <*> sub)),
          (2, ex <$> (ELambda <$> genTypeParams d' <*> genParams d' <*> maybeOf (genType d') <*> sub)),
          (3, ex <$> (ECall <$> sub <*> listOf' (genArg d'))),
          (2, ex <$> (EDot <$> sub <*> (sp <$> elements lowerNames))),
          (1, ex <$> (EIndex <$> sub <*> sub)),
          (4, ex <$> (EBin <$> elements binaryOps <*> sub <*> sub)),
          (1, ex . ENot <$> sub),
          (1, ex . ENeg <$> sub),
          (1, ex . EDo <$> genBlock d'),
          (1, ex <$> (EIf <$> sub <*> genBlock d' <*> maybeOf (genBlock d'))),
          (1, ex <$> (ECond <$> sub <*> sub <*> sub)),
          (1, ex <$> (EFor <$> (sp <$> elements lowerNames) <*> sub <*> genBlock d')),
          (1, genCase d'),
          (1, genTry d'),
          (1, ex . EAsync <$> sub),
          (1, ex . EAwait <$> sub),
          (2, genCommand d'),
          (1, genEnv d')
        ]
  where
    d' = d - 1
    sub = genExpr d'

genLeaf :: Gen Expr
genLeaf =
  frequency
    [ (1, pure (ex ENull)),
      (1, ex . EBool <$> arbitrary),
      (2, ex . ENumber <$> genNumber),
      (2, ex . EString <$> genStringParts (-1)),
      (4, ex . EVar <$> elements lowerNames),
      (1, ex <$> (EEnv <$> elements envHeads <*> pure Nothing <*> pure Nothing))
    ]

genNumber :: Gen S.Scientific
genNumber =
  oneof
    [ S.scientific <$> choose (0, 1000) <*> pure 0,
      S.scientific <$> choose (0, 100000) <*> choose (-4, -1),
      S.scientific <$> choose (0, 10 ^ (30 :: Int)) <*> pure 0
    ]

genArg :: Int -> Gen Arg
genArg d = Arg NoSpan <$> oneof [APos <$> genExpr d, AKw <$> elements lowerNames <*> genExpr d]

-- | The parts of an interpreted string as the lexer leaves them: no two
-- chunks in a row, and none empty.
genStringParts :: Int -> Gen [TextPart]
genStringParts d = do
  ps <- listOf' (frequency [(3, TPChunk NoSpan <$> chunkOf stringChars), (if d < 0 then 0 else 1, TPInterp <$> genExpr d)])
  -- The empty raw string is the one chunk that is empty.
  empty <- frequency [(9, pure False), (1, pure True)]
  pure (if empty && null ps then [TPChunk NoSpan ""] else mergeChunks ps)

chunkOf :: String -> Gen Text
chunkOf cs = T.pack <$> (choose (1, 6) >>= \k -> vectorOf k (elements cs))

mergeChunks :: [TextPart] -> [TextPart]
mergeChunks = filter (/= TPChunk NoSpan "") . foldr step []
  where
    step (TPChunk _ a) (TPChunk _ b : rest) = TPChunk NoSpan (a <> b) : rest
    step p rest = p : rest

genCommand :: Int -> Gen Expr
genCommand d = do
  stream <- elements [StreamOut, StreamErr, StreamAll]
  env <- frequency [(2, pure Nothing), (1, Just <$> genExpr d)]
  ps <- listOf1' (frequency [(3, TPChunk NoSpan <$> chunkOf commandChars), (1, TPInterp <$> genExpr d), (1, TPRaw <$> genExpr d)])
  pure (ex (ECommand stream env (commandParts ps)))

-- | The parts of a command string as the lexer leaves them: chunks
-- merged, and the whitespace at either end trimmed. Never empty.
commandParts :: [TextPart] -> [TextPart]
commandParts ps = case trimEnd (trimStart (mergeChunks ps)) of
  [] -> [TPChunk NoSpan "ls"]
  ps' -> ps'
  where
    blank c = c == ' ' || c == '\t'
    trimStart (TPChunk _ c : rest) = mergeChunks (TPChunk NoSpan (T.dropWhile blank c) : rest)
    trimStart xs = xs
    trimEnd xs = case reverse xs of
      (TPChunk _ c : rest) -> reverse (filter (/= TPChunk NoSpan "") [TPChunk NoSpan (T.dropWhileEnd blank c)] <> rest)
      _ -> xs

genEnv :: Int -> Gen Expr
genEnv d = do
  h <- elements envHeads
  as <- maybeOf (listOf' (genArg (d - 1)))
  os <- maybeOf (listOf' (Arg NoSpan <$> (AKw <$> elements lowerNames <*> genExpr (d - 1))))
  pure (ex (EEnv h as os))

genBlock :: Int -> Gen Block
genBlock d = Block NoSpan <$> listOf' (Stmt NoSpan <$> one)
  where
    one =
      oneof
        [ SBind <$> elements lowerNames <*> genSecrecy <*> maybeOf (genType d) <*> genExpr d,
          SExpr <$> genExpr d
        ]

genCase :: Int -> Gen Expr
genCase d = do
  scrut <- maybeOf (genExpr d)
  arms <- listOf' (CaseArm NoSpan <$> (Just <$> heads) <*> genExpr d)
  final <- maybeOf (CaseArm NoSpan Nothing <$> genExpr d)
  pure (ex (ECase scrut (arms <> maybe [] pure final)))
  where
    heads = oneof [ValueHeads <$> listOf1' (genExpr d), TypeHeads <$> listOf1' (genHeadType d)]

-- | A @try@ with a @catch@, a @finally@, or both.
genTry :: Int -> Gen Expr
genTry d = do
  body <- genBlock d
  handler <- maybeOf ((,) <$> (sp <$> elements lowerNames) <*> genBlock d)
  fin <- if isNothing handler then Just <$> genBlock d else maybeOf (genBlock d)
  pure (ex (ETry body handler fin))

binaryOps :: [Op]
binaryOps = [o | o <- [minBound .. maxBound], o /= OpNot]

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = frequency [(1, pure Nothing), (1, Just <$> g)]

-- | Short lists, so that the size of a tree is bounded by its depth.
listOf', listOf1' :: Gen a -> Gen [a]
listOf' = vectorOf' 0 3
listOf1' = vectorOf' 1 3

vectorOf' :: Int -> Int -> Gen a -> Gen [a]
vectorOf' lo hi g = choose (lo, hi) >>= \k -> vectorOf k g

-- Shrinking ---------------------------------------------------------------------

-- | Smaller modules that are still well formed: fewer declarations,
-- and declarations whose parts are smaller.
shrinkModule :: Module -> [Module]
shrinkModule (Module ds ints cmds) =
  [ marked (Module ds' ints cmds)
  | ds' <- shrinkList shrinkDecl ds
  ]
  where
    -- The internal markers of the declarations that are left.
    marked (Module xs is cs) =
      Module
        xs
        (Set.filter (\n -> Just n `elem` map declName xs) is)
        (Set.fromList [w | Decl _ (DCommand ws _ _) <- xs, all ((`Set.member` cs) . spannedValue) ws, Spanned _ w <- ws])

shrinkDecl :: Decl -> [Decl]
shrinkDecl (Decl _ f) = Decl NoSpan <$> case f of
  DValue n sec t e ->
    [DValue n Public t e | sec == Secret]
      <> [DValue n sec t' e | t' <- shrinkMaybe shrinkType t]
      <> [DValue n sec t e' | e' <- shrinkExpr e]
  DFunction n tps ps rt w e ->
    [DFunction n tps' ps rt w e | tps' <- shrinkList (const []) tps]
      <> [DFunction n tps ps' rt w e | ps' <- shrinkList shrinkParam ps]
      <> [DFunction n tps ps rt' w e | rt' <- shrinkMaybe shrinkType rt]
      <> [DFunction n tps ps rt Nothing e | isJust w]
      <> [DFunction n tps ps rt w e' | e' <- shrinkExpr e]
  DTypeAlias n tps t ->
    [DTypeAlias n tps' t | tps' <- shrinkList (const []) tps]
      <> [DTypeAlias n tps t' | t' <- shrinkType t]
  DImportNamed specs p -> [DImportNamed s p | s <- shrinkList (const []) specs, not (null s)]
  DExportFrom specs p -> [DExportFrom s p | s <- shrinkList (const []) specs, not (null s)]
  DWithin w -> DWithin <$> shrinkAccess w
  DAccess n w -> DAccess n <$> shrinkAccess w
  DCommand ws e sch ->
    [DCommand ws' e sch | ws' <- shrinkList (const []) ws, not (null ws')]
      <> [DCommand ws e Nothing | isJust sch]
      <> [DCommand ws e' sch | e' <- shrinkExpr e]
      <> [DCommand ws e (Just s') | Just s <- [sch], s' <- shrinkExpr s]
  DImportCommands ws p -> [DImportCommands ws' p | ws' <- shrinkList (const []) ws, not (null ws')]
  DExportCommandsFrom ws p -> [DExportCommandsFrom ws' p | ws' <- shrinkList (const []) ws, not (null ws')]
  DClass {} -> []
  DInstance {} -> []
  DImportNamespace {} -> []
  DExportNamespace {} -> []

shrinkAccess :: AccessSpec -> [AccessSpec]
shrinkAccess w = case w of
  AccessSpec _ fs -> AccessSpec NoSpan <$> shrinkList (\(k, v) -> (,) k <$> shrinkExpr v) fs
  AccessRef {} -> []

shrinkMaybe :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybe _ Nothing = []
shrinkMaybe f (Just x) = Nothing : map Just (f x)

shrinkParam :: Param -> [Param]
shrinkParam (Param _ f) = Param NoSpan <$> case f of
  PPositional n sec t -> [PPositional n Public t | sec == Secret] <> [PPositional n sec t' | t' <- shrinkMaybe shrinkType t]
  -- The type of a variadic parameter stays an array.
  PVariadic n t -> [PVariadic n Nothing | isJust t] <> [PVariadic n (Just (ty (SArray i'))) | Just (SType _ (SArray i)) <- [t], i' <- shrinkType i]
  PKeyword n sec t e ->
    [PKeyword n Public t e | sec == Secret]
      <> [PKeyword n sec t' e | t' <- shrinkMaybe shrinkType t]
      <> [PKeyword n sec t e' | e' <- shrinkMaybe shrinkExpr e]

shrinkType :: SType -> [SType]
shrinkType (SType _ f) = ty <$> ([SAny | f /= SAny] <> structural)
  where
    structural = case f of
      SArray t -> stypeF t : (SArray <$> shrinkType t)
      SMap t -> stypeF t : (SMap <$> shrinkType t)
      SAsyncHandle t -> stypeF t : (SAsyncHandle <$> shrinkType t)
      SRecord fs -> SRecord <$> shrinkList (\(k, o, t) -> (,,) k o <$> shrinkType t) fs
      SFunction ps r -> stypeF r : ([SFunction ps' r | ps' <- shrinkList shrinkType ps] <> [SFunction ps r' | r' <- shrinkType r])
      SNamed q n as -> [SNamed Nothing n as | isJust q] <> [SNamed q n as' | as' <- shrinkList shrinkType as]
      -- A union of one member is that member, and no member is a union.
      SUnion ts -> map stypeF ts <> [SUnion ts' | ts' <- shrinkList shrinkSingle ts, length ts' >= 2]
      _ -> []

shrinkSingle :: SType -> [SType]
shrinkSingle = filter (not . isUnion) . shrinkType

isUnion :: SType -> Bool
isUnion (SType _ (SUnion _)) = True
isUnion _ = False

-- | A type that may still head a case arm.
shrinkHeadType :: SType -> [SType]
shrinkHeadType = filter headable . shrinkType
  where
    headable (SType _ (SUnion _)) = False
    headable (SType _ (SNamed (Just _) _ _)) = False
    headable _ = True

shrinkExpr :: Expr -> [Expr]
shrinkExpr e@(Expr _ f) = ex <$> ([ENull | f /= ENull] <> map exprF (childExprs e) <> structural)
  where
    structural = case f of
      ENumber n -> [ENumber 0 | n /= 0]
      EString ps -> [EString (mergeChunks ps') | ps' <- shrinkList shrinkPart ps]
      EArray es -> EArray <$> shrinkList shrinkExpr es
      EObject kvs -> EObject <$> shrinkList (\(k, v) -> (,) k <$> shrinkExpr v) kvs
      ELambda tps ps rt b ->
        [ELambda tps' ps rt b | tps' <- shrinkList (const []) tps]
          <> [ELambda tps ps' rt b | ps' <- shrinkList shrinkParam ps]
          <> [ELambda tps ps rt' b | rt' <- shrinkMaybe shrinkType rt]
          <> [ELambda tps ps rt b' | b' <- shrinkExpr b]
      ECall g as -> [ECall g as' | as' <- shrinkList shrinkArg as] <> [ECall g' as | g' <- shrinkExpr g]
      EDot x n -> [EDot x' n | x' <- shrinkExpr x]
      EIndex x i -> [EIndex x' i | x' <- shrinkExpr x] <> [EIndex x i' | i' <- shrinkExpr i]
      EBin o a b -> [EBin o a' b | a' <- shrinkExpr a] <> [EBin o a b' | b' <- shrinkExpr b]
      ENot x -> ENot <$> shrinkExpr x
      ENeg x -> ENeg <$> shrinkExpr x
      EDo b -> EDo <$> shrinkBlock b
      EIf c t me ->
        [EIf c t Nothing | isJust me]
          <> [EIf c' t me | c' <- shrinkExpr c]
          <> [EIf c t' me | t' <- shrinkBlock t]
          <> [EIf c t (Just e') | Just x <- [me], e' <- shrinkBlock x]
      ECond c a b -> [ECond c' a b | c' <- shrinkExpr c] <> [ECond c a' b | a' <- shrinkExpr a] <> [ECond c a b' | b' <- shrinkExpr b]
      EFor x xs b -> [EFor x xs' b | xs' <- shrinkExpr xs] <> [EFor x xs b' | b' <- shrinkBlock b]
      ECase s arms ->
        [ECase Nothing arms | isJust s]
          <> [ECase (Just s') arms | Just x <- [s], s' <- shrinkExpr x]
          <> [ECase s arms' | arms' <- shrinkList shrinkArm arms]
      ETry b c fin ->
        [ETry b Nothing fin | isJust c, isJust fin]
          <> [ETry b c Nothing | isJust c, isJust fin]
          <> [ETry b' c fin | b' <- shrinkBlock b]
          <> [ETry b (Just (n, h')) fin | Just (n, h) <- [c], h' <- shrinkBlock h]
          <> [ETry b c (Just x') | Just x <- [fin], x' <- shrinkBlock x]
      EAsync x -> EAsync <$> shrinkExpr x
      EAwait x -> EAwait <$> shrinkExpr x
      ECommand s env ps ->
        [ECommand StreamOut env ps | s /= StreamOut]
          <> [ECommand s Nothing ps | isJust env]
          <> [ECommand s (Just v') ps | Just v <- [env], v' <- shrinkExpr v]
          <> [ECommand s env (commandParts ps') | ps' <- shrinkList shrinkPart ps, not (null ps')]
      EEnv h as os ->
        [EEnv h Nothing os | isJust as]
          <> [EEnv h as Nothing | isJust os]
          <> [EEnv h (Just as') os | Just xs <- [as], as' <- shrinkList shrinkArg xs]
          <> [EEnv h as (Just os') | Just xs <- [os], os' <- shrinkList shrinkArg xs]
      _ -> []

shrinkPart :: TextPart -> [TextPart]
shrinkPart p = case p of
  TPChunk _ c -> [TPChunk NoSpan c' | c' <- map T.pack (shrinkList (const []) (T.unpack c)), not (T.null c')]
  TPInterp e -> TPInterp <$> shrinkExpr e
  TPRaw e -> TPRaw <$> shrinkExpr e

-- | A keyword argument stays one: a run option has no other form.
shrinkArg :: Arg -> [Arg]
shrinkArg (Arg _ a) = Arg NoSpan <$> case a of
  APos e -> APos <$> shrinkExpr e
  AKw k e -> AKw k <$> shrinkExpr e

shrinkBlock :: Block -> [Block]
shrinkBlock (Block _ ss) = Block NoSpan <$> shrinkList shrinkStmt ss

shrinkStmt :: Stmt -> [Stmt]
shrinkStmt (Stmt _ f) = Stmt NoSpan <$> case f of
  SBind n sec t e ->
    SExpr e
      : [SBind n Public t e | sec == Secret]
      <> [SBind n sec t' e | t' <- shrinkMaybe shrinkType t]
      <> [SBind n sec t e' | e' <- shrinkExpr e]
  SExpr e -> SExpr <$> shrinkExpr e

shrinkArm :: CaseArm -> [CaseArm]
shrinkArm (CaseArm _ hs b) =
  [CaseArm NoSpan hs b' | b' <- shrinkExpr b] <> case hs of
    Just (ValueHeads es) -> [CaseArm NoSpan (Just (ValueHeads es')) b | es' <- shrinkList shrinkExpr es, not (null es')]
    Just (TypeHeads ts) -> [CaseArm NoSpan (Just (TypeHeads ts')) b | ts' <- shrinkList shrinkHeadType ts, not (null ts')]
    Nothing -> []

-- Printing -------------------------------------------------------------------------

-- | Where an expression is printed: on lines at an indentation, or
-- inside an interpolation or a command's environment, whose newlines
-- the lexer drops, so that blocks there separate statements with @;@.
data Mode = Lines Int | Inline

renderModule :: Module -> Text
renderModule m = T.concat [marker d <> decl d <> "\n" | d <- moduleDecls m]
  where
    marker d@(Decl _ f) = case f of
      DCommand ws _ _
        | all ((`Set.member` moduleInternalCommands m) . spannedValue) ws -> "internal "
        | [w | Spanned _ w <- take 1 ws] == ["npm"] -> "export "
      _ -> case declName d of
        Just n
          | n `Set.member` moduleInternal m -> "internal "
          | "pub" `T.isPrefixOf` n -> "export "
        _ -> ""

decl :: Decl -> Text
decl (Decl _ f) = case f of
  DImportNamed specs p -> "import " <> importSpecs specs <> " from " <> quoted p
  DImportNamespace a p -> "import * as " <> a <> " from " <> quoted p
  DExportFrom specs p -> "export " <> importSpecs specs <> " from " <> quoted p
  DExportNamespace a p -> "export * as " <> a <> " from " <> quoted p
  DTypeAlias n tps t -> "type " <> n <> typeParams tps <> " = " <> typ t
  DValue n sec t e -> n <> secrecy sec <> annot t <> " = " <> rhs top e
  DFunction n tps ps rt w e ->
    n <> typeParams tps <> params top ps <> annot rt <> maybe "" ((" within " <>) . access top) w <> " = " <> rhs top e
  DWithin w -> "within " <> access top w
  DAccess n w -> "access " <> n <> " = " <> access top w
  DCommand ws e sch -> "command " <> commandNames ws <> " on " <> expr top 0 False e <> maybe "" ((" schema " <>) . expr top 0 False) sch
  DImportCommands ws p -> "import command " <> commandNames ws <> " from " <> quoted p
  DExportCommandsFrom ws p -> "export command " <> commandNames ws <> " from " <> quoted p
  -- Not generated: written only so that every declaration has a form.
  DClass n tp _ -> "class " <> n <> typeParams [tp] <> " {}"
  DInstance q (Spanned _ c) t _ -> "instance " <> maybe "" ((<> ".") . spannedValue) q <> c <> "<" <> typ t <> "> {}"
  where
    top = Lines 0
    importSpecs specs = "{ " <> commas [n <> maybe "" (" as " <>) a | ImportSpec _ n a <- specs] <> " }"
    commandNames ws = "{ " <> commas [quoted w | Spanned _ w <- ws] <> " }"

access :: Mode -> AccessSpec -> Text
access m w = case w of
  AccessSpec _ [] -> "{}"
  AccessSpec _ fs -> "{ " <> commas [k <> ": " <> expr m 0 False v | (Spanned _ k, v) <- fs] <> " }"
  AccessRef _ q n -> maybe "" (<> ".") q <> n

secrecy :: Secrecy -> Text
secrecy Public = ""
secrecy Secret = "!!"

annot :: Maybe SType -> Text
annot = maybe "" ((": " <>) . typ)

typeParams :: [TypeParam] -> Text
typeParams [] = ""
typeParams tps = "<" <> commas (map one tps) <> ">"
  where
    one (TypeParam (Spanned _ v) b) = v <> maybe "" ((": " <>) . bound) b
    bound (SBoundNamed (Spanned _ n)) = n
    bound (SBoundQualified (Spanned _ q) (Spanned _ n)) = q <> "." <> n
    bound (SBoundType t) = typ t
    bound (SBoundClass _ n) = n
    bound (SBoundAll bs) = T.intercalate " + " (map bound bs)

params :: Mode -> [Param] -> Text
params m ps = "(" <> commas (map one ps) <> ")"
  where
    one (Param _ f) = case f of
      PPositional n sec t -> n <> secrecy sec <> annot t
      PVariadic n t -> "..." <> n <> annot t
      PKeyword n sec t d -> "--" <> n <> secrecy sec <> annot t <> maybe "" ((" = " <>) . expr m 0 False) d

typ :: SType -> Text
typ (SType _ f) = case f of
  SAny -> "Any"
  SNumber -> "Number"
  SString -> "String"
  SBool -> "Bool"
  SNull -> "Null"
  SVoid -> "Void"
  SEnvironment -> "Environment"
  SRunnable -> "Runnable"
  SArray t -> "Array<" <> typ t <> ">"
  SMap t -> "Map<" <> typ t <> ">"
  SAsyncHandle t -> "AsyncHandle<" <> typ t <> ">"
  SRecord fs -> "Record<" <> commas [key k <> (if o then "?" else "") <> ": " <> typ t | (Spanned _ k, o, t) <- fs] <> ">"
  SFunction ps r -> "Function<" <> commas (map typ (ps <> [r])) <> ">"
  SNamed q n as -> maybe "" (<> ".") q <> n <> (if null as then "" else "<" <> commas (map typ as) <> ">")
  SUnion ts -> T.intercalate " | " (map typ ts)

-- | A key as an identifier where it is one, quoted otherwise.
key :: Text -> Text
key k = case T.uncons k of
  Just (c, rest)
    | isAsciiLower c || c == '_',
      T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_') rest,
      isNothing (keywordFromText k),
      k `notElem` ["true", "false", "null"] ->
        k
  _ -> string [TPChunk NoSpan k]

quoted :: Text -> Text
quoted t = "\"" <> T.concatMap escape t <> "\""

escape :: Char -> Text
escape c = case c of
  '\\' -> "\\\\"
  '"' -> "\\\""
  '\n' -> "\\n"
  '\t' -> "\\t"
  '\r' -> "\\r"
  _
    | wide c -> "\\u{" <> T.pack (showHex (ord c) "") <> "}"
    | otherwise -> T.singleton c

-- | A character that may be wider than a column: every one from the
-- first CJK block on.
wide :: Char -> Bool
wide c = ord c >= 0x1100

string :: [TextPart] -> Text
string ps = case ps of
  [TPChunk _ c]
    | not (T.any (== '\'') c),
      not (T.any wide c),
      T.null c || T.any (`elem` ("\\\"" :: String)) c ->
        "'" <> c <> "'"
  _ -> "\"" <> T.concat (map part ps) <> "\""
  where
    part (TPChunk _ c) = T.concatMap escape c
    part (TPInterp e) = "#{" <> expr Inline 0 False e <> "}"
    part (TPRaw e) = "#!{" <> expr Inline 0 False e <> "}"

-- | How tightly an expression binds: the conditional loosest, then the
-- rows of the operator table (spec 6.2), the prefix operators, and the
-- postfix forms and primaries.
level :: ExprF -> Int
level f = case f of
  ECond {} -> 0
  EBin o _ _ -> opLevel o
  ENot _ -> 8
  ENeg _ -> 8
  EAsync _ -> 8
  EAwait _ -> 8
  _ -> 9

opLevel :: Op -> Int
opLevel o
  | o `elem` [OpMul, OpDiv] = 7
  | o `elem` [OpAdd, OpSub] = 6
  | o `elem` [OpEq, OpNe, OpLt, OpLe, OpGt, OpGe] = 5
  | o == OpAnd = 4
  | o == OpOr = 3
  | o `elem` [OpCompR, OpCompL] = 2
  | otherwise = 1

-- | The backward application and composition operators associate to
-- the right; every other binary operator to the left (spec 6.2).
rightAssoc :: Op -> Bool
rightAssoc o = o `elem` [OpPipeL, OpCompL]

opText :: Op -> Text
opText o = case o of
  OpNot -> "!"
  OpMul -> "*"
  OpDiv -> "/"
  OpAdd -> "+"
  OpSub -> "-"
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpAnd -> "&&"
  OpOr -> "||"
  OpPipeR -> "|>"
  OpPipeL -> "<|"
  OpCompR -> ">>"
  OpCompL -> "<<"

-- | An expression that ends with another expression, which takes as
-- much as it can: whatever follows it on the line would become part of
-- its body or branch.
openEnded :: ExprF -> Bool
openEnded f = case f of
  ELambda {} -> True
  ECond {} -> True
  _ -> False

-- | An expression where it must bind at least as tightly as @prec@,
-- told whether more of the enclosing expression follows it.
expr :: Mode -> Int -> Bool -> Expr -> Text
expr m prec more (Expr _ f)
  | level f < prec || (openEnded f && more) = "(" <> exprF' m False f <> ")"
  | otherwise = exprF' m more f

exprF' :: Mode -> Bool -> ExprF -> Text
exprF' m more f = case f of
  ENull -> "null"
  EBool True -> "true"
  EBool False -> "false"
  ENumber n -> T.pack (S.formatScientific S.Fixed Nothing n)
  EString ps -> string ps
  EVar n -> n
  EArray es -> "[" <> commas (map (expr m 0 False) es) <> "]"
  EObject [] -> "{}"
  EObject kvs -> "{ " <> commas [key k <> ": " <> expr m 0 False v | (Spanned _ k, v) <- kvs] <> " }"
  ELambda tps ps rt b -> "\\" <> typeParams tps <> params m ps <> annot rt <> " -> " <> expr m 0 more b
  ECall g as -> base g <> "(" <> commas (map (arg m) as) <> ")"
  EDot e (Spanned _ n) -> base e <> "." <> n
  EIndex e i -> base e <> "[" <> expr m 0 False i <> "]"
  -- An operand on the side the operator associates to may be another
  -- application of it at its level, but not of its twin, which
  -- associates the other way and cannot be mixed in unparenthesized.
  EBin o a b ->
    let lvl = opLevel o
        assocSide c = case c of
          Expr _ (EBin o' _ _) | opLevel o' == lvl, rightAssoc o' /= rightAssoc o -> lvl + 1
          _ -> lvl
        (pa, pb) = if rightAssoc o then (lvl + 1, assocSide b) else (assocSide a, lvl + 1)
     in expr m pa True a <> " " <> opText o <> " " <> expr m pb more b
  -- @!!@ is the secret marker, and @--@ the keyword-parameter mark.
  ENot x -> prefix "!" (expr m 8 more x)
  ENeg x -> prefix "-" (expr m 8 more x)
  EDo b -> "do " <> block m b
  EIf c t me -> "if (" <> expr m 0 False c <> ") " <> block m t <> maybe "" ((" else " <>) . block m) me
  ECond c a b -> expr m 1 True c <> " ? " <> expr m 0 False a <> " : " <> expr m 0 more b
  EFor (Spanned _ x) xs b -> "for (" <> x <> ": " <> expr m 0 False xs <> ") " <> block m b
  ECase s arms -> caseExpr m s arms
  ETry b c fin ->
    "try "
      <> block m b
      <> maybe "" (\(Spanned _ n, h) -> " catch (" <> n <> ") " <> block m h) c
      <> maybe "" ((" finally " <>) . block m) fin
  EAsync x -> "async " <> expr m 8 more x
  EAwait x -> "await " <> expr m 8 more x
  ECommand s env ps -> command False s env ps
  EEnv h as os ->
    "#"
      <> h
      <> maybe "" (\xs -> "(" <> commas (map (arg m) xs) <> ")") as
      <> maybe "" (\xs -> "{" <> commas [k <> ": " <> expr m 0 False v | Arg _ (AKw k v) <- xs] <> "}") os
  where
    prefix o x = o <> (if o `T.isPrefixOf` x then " " else "") <> x
    -- An environment head takes in the characters after it, and a
    -- list right after it is its own (spec 6.7).
    base e@(Expr _ EEnv {}) = "(" <> expr m 0 False e <> ")"
    base e = expr m 9 True e

arg :: Mode -> Arg -> Text
arg m (Arg _ a) = case a of
  APos e -> expr m 0 False e
  AKw k e -> k <> " = " <> expr m 0 False e

-- | A command expression: braced, or running to the end of its line.
command :: Bool -> CmdStream -> Maybe Expr -> [TextPart] -> Text
command eol s env ps =
  "$" <> stream <> maybe "" (\e -> "[" <> expr Inline 0 False e <> "]") env <> body
  where
    stream = case s of
      StreamOut -> ""
      StreamErr -> "2"
      StreamAll -> "*"
    text = T.concat (map part ps)
    body = if eol then " " <> text else "{" <> text <> "}"
    part (TPChunk _ c) = c
    part (TPInterp e) = "#{" <> expr Inline 0 False e <> "}"
    part (TPRaw e) = "#!{" <> expr Inline 0 False e <> "}"

-- | What ends a line: a command there runs to the end of it.
rhs :: Mode -> Expr -> Text
rhs (Lines _) (Expr _ (ECommand s env ps)) = command True s env ps
rhs m e = expr m 0 False e

-- | What begins a line. A line that begins with a binary operator
-- continues the line before it (spec 6.5), and @-@ is one.
leading :: Text -> Text
leading t = if "-" `T.isPrefixOf` t then "(" <> t <> ")" else t

block :: Mode -> Block -> Text
block _ (Block _ []) = "{}"
block m (Block _ ss) = case m of
  Inline -> "{ " <> T.intercalate "; " (map (stmt Inline) ss) <> " }"
  Lines i -> "{\n" <> T.concat [indent (i + 2) <> stmt (Lines (i + 2)) s <> "\n" | s <- ss] <> indent i <> "}"

stmt :: Mode -> Stmt -> Text
stmt m (Stmt _ f) = case f of
  SBind n sec t e -> n <> secrecy sec <> annot t <> " = " <> rhs m e
  SExpr e -> leading (rhs m e)

caseExpr :: Mode -> Maybe Expr -> [CaseArm] -> Text
caseExpr m s arms =
  "case "
    <> maybe "" (\e -> "(" <> expr m 0 False e <> ") ") s
    <> case (m, arms) of
      (_, []) -> "{}"
      (Inline, _) -> "{ " <> T.intercalate "; " (map (arm Inline) arms) <> " }"
      (Lines i, _) -> "{\n" <> T.concat [indent (i + 2) <> arm (Lines (i + 2)) a <> "\n" | a <- arms] <> indent i <> "}"
  where
    arm m' (CaseArm _ hs b) = heads m' hs <> " -> " <> rhs m' b
    heads m' hs = case hs of
      Nothing -> "else"
      Just (ValueHeads (h : more)) -> commas (leading (expr m' 0 False h) : map (expr m' 0 False) more)
      Just (ValueHeads []) -> ""
      Just (TypeHeads ts) -> commas (map typ ts)

indent :: Int -> Text
indent i = T.replicate i " "

commas :: [Text] -> Text
commas = T.intercalate ", "

-- Noise and mutations ---------------------------------------------------------------

-- | Text that is mostly pieces of Lask, with stray characters between
-- them.
genNoise :: Gen Text
genNoise = T.concat <$> listOf (frequency [(4, elements fragments), (1, T.singleton <$> arbitrary)])

fragments :: [Text]
fragments =
  [ "import", "export", "internal", "from", "as", "type", "command", "do", "async", "await",
    "if", "else", "case", "for", "return", "try", "catch", "finally", "true", "null",
    "within", "access", "on", "schema", "x", "Foo", "Array", "Record", "1", "1.5",
    " ", " ", "\n", "\n", "\t", "\r\n", "(", ")", "[", "]", "{", "}", "<", ">", ">>", ">=",
    ",", ":", ";", ".", "...", "->", "=", "==", "\\", "--", "!!", "!", "|", "|>", "?",
    "+", "-", "*", "/", "&&", "||", "\"", "'", "#", "#{", "#!{", "#local", "$", "${", "$[",
    "$2", "\\u{", "\\#{", "ffff", "//", "/*", "*/"
  ]

-- | A program with some of its tokens deleted, duplicated or swapped,
-- or with noise put before one of them.
genMutation :: Text -> Gen Text
genMutation src0 = choose (1, 3) >>= go src0
  where
    go :: Text -> Int -> Gen Text
    go src 0 = pure src
    go src k = case tokenRanges src of
      [] -> T.append src <$> genNoise
      rs -> do
        let n = length rs
        i <- choose (0, n - 1)
        j <- choose (0, n - 1)
        let (a, b) = rs !! i
            (c, e) = rs !! j
            piece = slice a b
            slice x y = T.take (y - x) (T.drop x src)
        src' <-
          oneof
            [ pure (T.take a src <> T.drop b src),
              pure (T.take b src <> " " <> piece <> T.drop b src),
              pure $
                if a == c
                  then src
                  else
                    let ((p, q), (r, s)) = if a < c then ((a, b), (c, e)) else ((c, e), (a, b))
                     in T.take p src <> slice r s <> slice q r <> slice p q <> T.drop s src,
              (\noise -> T.take a src <> noise <> T.drop a src) <$> genNoise
            ]
        go src' (k - 1)

-- | Where each top-level token of a source starts and ends, as offsets
-- into it, if it lexes. Spans that overlap one another, as a swap can
-- leave them, are dropped.
tokenRanges :: Text -> [(Int, Int)]
tokenRanges src = case lexTokens "mutate.lask" src of
  Left _ -> []
  Right toks -> disjoint [(offset s, offset e) | Spanned (Span s e) _ <- toks]
  where
    ls = T.splitOn "\n" src
    starts = scanl (\acc l -> acc + T.length l + 1) 0 ls
    offset (Position _ l c) = (starts !! (l - 1)) + colIndex (ls !! (l - 1)) c
    disjoint = reverse . snd . foldl (\(end, acc) (a, b) -> if a >= end && b >= a then (b, (a, b) : acc) else (end, acc)) (0, [])

-- | The index in a line of the character at a column. Megaparsec
-- advances the column of a tab to the next multiple of eight, plus one.
colIndex :: Text -> Int -> Int
colIndex ln target = go 0 1 (T.unpack ln)
  where
    go i col _ | col >= target = i
    go i col (c : cs) = go (i + 1) (if c == '\t' then col + 8 - ((col - 1) `rem` 8) else col + 1) cs
    go i _ [] = i
