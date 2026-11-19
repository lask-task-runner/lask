{-# LANGUAGE OverloadedStrings #-}

-- | Front-end facade: lexing, parsing, module loading, name
-- resolution and elaboration in one call.
module Language.Lask
  ( Compiled (..),
    Partial (..),
    compileFile,
    compileFileUnconstrained,
    compileFilePartial,
    compileWith,
    compileText,
    compileTextPartial,
    checkText,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.Lask.Deps.File (defaultDepsFileName)
import Language.Lask.Deps.Lock (defaultLockFileName)
import Language.Lask.Confirm (validateConfirm)
import Language.Lask.Core.Access (checkWithin, secretAdvisories, withinConstraints)
import Data.List (isPrefixOf, sortOn)
import Language.Lask.Diagnostic (Advisory (..), Diagnostic, diagSpan, settleDiagnostics)
import Language.Lask.Elaborate (CoreProgram (..), Elaborated, elaborateModules, elaborateRecovering, elaborateRecoveringFrom)
import Language.Lask.Module.Loader (LoadedModule (..), LoaderEnv (..), ModuleReader, Program (..), collapseDots, defaultLoaderEnv, fileReader, loadProgramEnv, loadProgramWith)
import Language.Lask.Module.Resolve (GlobalScope, ResolvedModule, resolveModules, resolveProgram, resolveProgramFrom)
import Language.Lask.Syntax.AST (Module)
import qualified Language.Lask.Syntax.AST as AST
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Lask.Doc (DocComment (..), docBlockAbove, emptyDoc, parseDoc)
import Language.Lask.ErrorCode (AdvisoryCode (..))
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Utils (kebabToSnake)
import Language.Lask.Syntax.Parser (parseModule)
import System.FilePath (normalise, splitDirectories, takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getFileSize, getModificationTime, pathIsSymbolicLink)
import Language.Lask.Deps.Hash (treeStamp)
import Language.Lask.Deps.Cache (cacheDirFor)
import Data.Time.Clock (UTCTime)

data Compiled = Compiled
  { compiledProgram :: Program,
    compiledScopes :: Map FilePath GlobalScope,
    compiledCore :: CoreProgram
  }

-- | Whatever the front end managed to produce for a document that
-- does not compile: each stage is kept only if the ones before it
-- succeeded.
data Partial = Partial
  { partialModule :: Maybe Module,
    partialProgram :: Maybe Program,
    partialScopes :: Map FilePath GlobalScope,
    partialCore :: Maybe CoreProgram
  }

compileFile :: FilePath -> IO (Either [Diagnostic] Compiled)
compileFile = compileWith fileReader

-- | 'compileFile' without checking the access constraints (spec
-- 10.11): for @envs files | network | access@, which report what the
-- constraints find rather than fail on it (11.4).
compileFileUnconstrained :: FilePath -> IO (Either [Diagnostic] Compiled)
compileFileUnconstrained entry = do
  r <- loadProgramWith fileReader entry
  pure $ case r of
    Left ds -> Left (settleDiagnostics ds)
    Right prog -> checkedWith prog (checkProgramWith False prog)

compileWith :: ModuleReader -> FilePath -> IO (Either [Diagnostic] Compiled)
compileWith reader entry = do
  r <- loadProgramWith reader entry
  pure $ case r of
    Left ds -> Left (settleDiagnostics ds)
    Right prog -> checkedWith prog (checkProgram prog)

-- | A compiled program from a loaded one and its 'checkProgram'.
checkedWith :: Program -> ([Diagnostic], Map FilePath GlobalScope, CoreProgram) -> Either [Diagnostic] Compiled
checkedWith prog result = case result of
  ([], scopes, core) ->
    -- The project file's confirmations refer to the program; one
    -- that no longer does is an error, never silently dropped (spec 5).
    case validateConfirm prog scopes core of
      [] -> Right (Compiled prog scopes core)
      ds -> Left (settleDiagnostics ds)
  (ds, _, _) -> Left ds

-- | Name resolution and elaboration of a loaded program. Every
-- independent error is reported (spec 14.3): a declaration that holds
-- an error of name resolution is not elaborated, and the others still
-- are. The core program holds the declarations that did elaborate.
checkProgram :: Program -> ([Diagnostic], Map FilePath GlobalScope, CoreProgram)
checkProgram = checkProgramWith True

checkProgramWith :: Bool -> Program -> ([Diagnostic], Map FilePath GlobalScope, CoreProgram)
checkProgramWith constrained prog =
  let (nameDs, scopes) = resolveProgram prog
   in checkResolved constrained elaborateRecovering prog nameDs scopes

-- | 'checkProgramWith' once names are resolved, elaborating with the
-- given elaborator.
checkResolved ::
  Bool ->
  (Program -> Map FilePath GlobalScope -> [Diagnostic] -> ([Diagnostic], CoreProgram)) ->
  Program ->
  [Diagnostic] ->
  Map FilePath GlobalScope ->
  ([Diagnostic], Map FilePath GlobalScope, CoreProgram)
checkResolved constrained elaborate prog nameDs scopes =
  let (typeDs, core) = elaborate prog scopes nameDs
      -- The access constraints are checked over the whole program
      -- (spec 10.11), which only a program that elaborated is.
      (specDs, cons) = withinConstraints prog
      withinDs
        | constrained && null (nameDs <> typeDs <> specDs) = checkWithin cons core
        | otherwise = []
      -- Where secrets are given is advised on whether or not a
      -- constraint covers them (spec 10.11).
      advised
        | null (nameDs <> typeDs <> specDs) = core {cpAdvisories = sortOn advSpan (cpAdvisories core <> secretAdvisories core <> declarationAdvisories prog)}
        | otherwise = core
   in (settleDiagnostics (nameDs <> typeDs <> specDs <> withinDs), scopes, advised)

-- | What the declarations of the program say that no check of their
-- types sees (spec 14.2): an @\@param@ of a documentation comment that
-- names no parameter of its declaration (3.1), and a keyword parameter
-- named @help@, which the command line cannot give as @--help@ (11.6),
-- and a module that writes @export@ on some declarations and leaves it
-- out on others (5).
declarationAdvisories :: Program -> [Advisory]
declarationAdvisories prog = concatMap ofModule (Map.elems (progModules prog))
  where
    ofModule lm =
      let comments = either (const []) snd (lexTokensWithComments (lmPath lm) (lmSource lm))
          internal = AST.moduleInternal (lmModule lm)
          decls = AST.moduleDecls (lmModule lm)
       in concatMap (ofDecl (lmSource lm) comments internal) decls
            <> exportMix (lmSource lm) internal decls
    -- The marker written before a declaration that can carry one:
    -- what precedes it on its line, which the grammar leaves to be
    -- @export@, @internal@ or nothing.
    exportMix src internal decls =
      let srcLines = T.lines src
          marker d = case AST.declSpan d of
            Span (Position _ l c) _
              | l >= 1 && l <= length srcLines -> T.strip (T.take (c - 1) (srcLines !! (l - 1)))
            _ -> ""
          markable d = case AST.declF d of
            AST.DFunction n _ _ _ _ _ -> Just n
            AST.DValue n _ _ _ -> Just n
            AST.DTypeAlias n _ _ -> Just n
            _ -> Nothing
          public = [(d, n) | d <- decls, Just n <- [markable d], n `Set.notMember` internal]
          explicit = [() | (d, _) <- public, marker d == "export"]
          omitted = [(d, n) | (d, n) <- public, marker d == ""]
       in case (explicit, omitted) of
            (_ : _, (d, n) : _) ->
              [ Advisory WExportInconsistent (AST.declSpan d) $
                  "'" <> n <> "' leaves out export while other declarations of this module write it, so it may be read as not public; it is public either way"
              ]
            _ -> []
    ofDecl src comments internal d = case params d of
      Just (name, ps) ->
        let names = [paramName p | p <- ps]
            doc = case AST.declSpan d of
              Span (Position _ l _) _ -> maybe emptyDoc parseDoc (docBlockAbove src comments l)
              NoSpan -> emptyDoc
         in [ Advisory WDocParamUnknown (AST.declSpan d) $
                "the documentation of '" <> name <> "' describes @param " <> p <> ", which '" <> name <> "' does not have"
            | (p, _) <- docParams doc,
              kebabToSnake p `notElem` names
            ]
              <> [ Advisory WCliParamShadowed sp $
                     "'" <> name <> "' has a keyword parameter named help, which the command line can give only as --help=<value>: --help shows the help"
                 | name `Set.notMember` internal,
                   AST.Param sp (AST.PKeyword "help" _ _ _) <- ps
                 ]
      Nothing -> []
    params d = case AST.declF d of
      AST.DFunction n _ ps _ _ _ -> Just (n, ps)
      AST.DValue n _ _ (AST.Expr _ (AST.ELambda _ ps _ _)) -> Just (n, ps)
      _ -> Nothing
    paramName (AST.Param _ f) = case f of
      AST.PPositional n _ _ -> n
      AST.PVariadic n _ -> n
      AST.PKeyword n _ _ _ -> n

-- | Compile an in-editor document: the entry module's text is
-- provided directly; imported modules are read from disk.
compileText :: FilePath -> Text -> IO (Either [Diagnostic] Compiled)
compileText path txt = do
  r <- editorCompile path txt
  pure $ case r of
    Left ds -> Left (settleDiagnostics ds)
    Right (prog, result) -> checkedWith prog result

-- | Like 'compileText' but keeps the results of the stages that did
-- succeed, for editor features that must work on a buffer being typed.
-- The core program holds every declaration that elaborated, so the
-- rest of a module keeps working while one declaration is broken.
compileTextPartial :: FilePath -> Text -> IO Partial
compileTextPartial path txt = do
  loaded <- editorCompile path txt
  pure $ case loaded of
    Left _ ->
      Partial
        { partialModule = either (const Nothing) Just (parseModule path txt),
          partialProgram = Nothing,
          partialScopes = Map.empty,
          partialCore = Nothing
        }
    Right (prog, (_, scopes, core)) ->
      Partial
        { partialModule = lmModule <$> Map.lookup (progEntry prog) (progModules prog),
          partialProgram = Just prog,
          partialScopes = scopes,
          partialCore = Just core
        }

-- | Like 'compileFile' but keeps whatever the front end managed to
-- produce, together with the diagnostics. CLI help is rendered from
-- the surface syntax even when the module does not type check
-- (spec 11.6), which is exactly when help is most needed.
compileFilePartial :: FilePath -> IO ([Diagnostic], Partial)
compileFilePartial path = do
  loaded <- loadProgramWith fileReader path
  case loaded of
    -- The module graph did not load (syntax error, unresolved import):
    -- parse the entry module on its own so its declarations are still
    -- available.
    Left ds -> do
      src <- fileReader path
      let m = case src of
            Right txt -> either (const Nothing) Just (parseModule path txt)
            Left _ -> Nothing
      pure
        ( settleDiagnostics ds,
          Partial
            { partialModule = m,
              partialProgram = Nothing,
              partialScopes = Map.empty,
              partialCore = Nothing
            }
        )
    Right prog -> do
      let (ds, scopes, c) = checkProgram prog
          core = if null ds then Just c else Nothing
      pure
        ( ds,
          Partial
            { partialModule = lmModule <$> Map.lookup (progEntry prog) (progModules prog),
              partialProgram = Just prog,
              partialScopes = scopes,
              partialCore = core
            }
        )

-- | The loader keys modules by their collapsed, normalised path, so
-- the buffer has to be matched the same way; otherwise a path such as
-- @./main.lask@ misses and the stale file on disk is read instead.
textReader :: FilePath -> FilePath -> Text -> ModuleReader
textReader cacheDir path txt p
  | collapseDots (normalise p) == collapseDots (normalise path) = pure (Right txt)
  | otherwise = readerFor cacheDir p

-- | 'fileReader', except that a module in the dependency cache is
-- read through 'stampedReader': every compile reads every module the
-- buffer imports, most of them a dependency's, which never change. The
-- project's own modules are read afresh, since a modification time may
-- not tell two writes apart.
readerFor :: FilePath -> ModuleReader
readerFor cacheDir p
  | cacheParts `isPrefixOf` splitDirectories (collapseDots (normalise p)) = stampedReader p
  | otherwise = fileReader p
  where
    cacheParts = splitDirectories (collapseDots (normalise cacheDir))

-- | 'fileReader', keeping what a file held for as long as its size and
-- modification time stay the same: for the dependency cache, whose
-- entries are named by the hash of what they hold.
stampedReader :: ModuleReader
stampedReader p = do
  stamp <- try ((,) <$> getFileSize p <*> getModificationTime p)
  case stamp of
    Left e -> const (fileReader p) (e :: IOException)
    Right now -> do
      known <- Map.lookup p <$> readIORef readCache
      case known of
        Just (before, src) | before == now -> pure src
        _ -> do
          src <- fileReader p
          atomicModifyIORef' readCache (\m -> (Map.insert p (now, src) m, ()))
          pure src

-- | 'leHolds', keeping a cache entry's verdict for as long as what its
-- hash is computed from looks the same ('treeStamp'): hashing a
-- dependency is most of what loading a program takes once its modules
-- are parsed.
stampedHolds :: (FilePath -> Text -> Bool -> IO Bool) -> FilePath -> Text -> Bool -> IO Bool
stampedHolds holds p hash single = do
  stamp <- try $ do
    isLink <- pathIsSymbolicLink p
    files <- if single then (\s -> [("", Right s)]) <$> ((,) <$> getFileSize p <*> getModificationTime p) else treeStamp p
    pure (isLink, files)
  case stamp of
    Left e -> const (holds p hash single) (e :: IOException)
    Right now -> do
      known <- Map.lookup (p, hash, single) <$> readIORef holdsCache
      case known of
        Just (before, ok) | before == now -> pure ok
        _ -> do
          ok <- holds p hash single
          atomicModifyIORef' holdsCache (\m -> (Map.insert (p, hash, single) (now, ok) m, ()))
          pure ok

{-# NOINLINE holdsCache #-}
holdsCache :: IORef (Map (FilePath, Text, Bool) ((Bool, [(FilePath, Either FilePath (Integer, UTCTime))]), Bool))
holdsCache = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE readCache #-}
readCache :: IORef (Map FilePath ((Integer, UTCTime), Either Text Text))
readCache = unsafePerformIO (newIORef Map.empty)

-- | Diagnostics for an in-editor document.
checkText :: FilePath -> Text -> IO [Diagnostic]
checkText path txt = either id (const []) <$> compileText path txt

-- ---------------------------------------------------------------------
-- What the language server compiled before
--
-- The editor features compile the buffer for every request, with all
-- the modules it imports: a keystroke brings diagnostics, semantic
-- tokens, inlay hints and completion, each a compile. A dependency
-- such as lask-module-tools is megabytes of source, which takes far
-- longer to lex and parse than the buffer. So a module's parse is kept
-- for as long as its source is the same, and a compile of a buffer is
-- kept for as long as the buffer and every file it read are.
-- ---------------------------------------------------------------------

-- | A file a compile read, and what it held: a module's source as the
-- loader read it, or a project or lock file's bytes ('Nothing' when it
-- was absent).
data Input
  = ModuleInput FilePath (Either Text Text)
  | FileInput FilePath (Maybe BS.ByteString)

-- | A compile of a buffer, with every file it read.
data Memo a = Memo Text [Input] a

-- | Each module's source, by path, with its parse.
{-# NOINLINE parseCache #-}
parseCache :: IORef (Map FilePath (Text, Either [Diagnostic] Module))
parseCache = unsafePerformIO (newIORef Map.empty)

-- | Each buffer's load and check, by path: diagnostics, semantic
-- tokens, hover and the rest share one per version of the buffer.
{-# NOINLINE compileMemo #-}
compileMemo :: IORef (Map FilePath (Memo (Either [Diagnostic] (Program, ([Diagnostic], Map FilePath GlobalScope, CoreProgram)))))
compileMemo = unsafePerformIO (newIORef Map.empty)

-- | A buffer loaded with what it imports, and checked; the check is
-- done when a caller first looks at it.
editorCompile :: FilePath -> Text -> IO (Either [Diagnostic] (Program, ([Diagnostic], Map FilePath GlobalScope, CoreProgram)))
editorCompile path txt = remembered compileMemo path txt $ \load -> do
  loaded <- load
  case loaded of
    Left ds -> pure (Left ds)
    Right prog -> do
      checked <- editorCheck prog
      pure (Right (prog, checked))

-- | What a dependency resolution and elaboration were made from: the
-- dependency modules' sources, and the directories a recipe path is
-- written against.
data DepKey = DepKey FilePath FilePath [(FilePath, Text)]
  deriving (Eq)

-- | The resolution and elaboration of the last program's dependencies.
-- They do not change while the project is edited, yet are most of what
-- a project checks, so each edit checks the project's own modules only.
{-# NOINLINE dependencyMemo #-}
dependencyMemo :: IORef (Maybe (DepKey, Map FilePath ResolvedModule, Maybe Elaborated))
dependencyMemo = unsafePerformIO (newIORef Nothing)

-- | 'checkProgram', resuming from the resolution and elaboration of the
-- program's dependencies when those of the same dependencies are kept.
editorCheck :: Program -> IO ([Diagnostic], Map FilePath GlobalScope, CoreProgram)
editorCheck prog = do
  let deps = dependencyModules prog
      depSet = Set.fromList deps
      key =
        DepKey
          (progBaseDir prog)
          (progCacheDir prog)
          [(p, lmSource lm) | p <- deps, Just lm <- [Map.lookup p (progModules prog)]]
  known <- readIORef dependencyMemo
  (depResolved, resumed) <- case known of
    Just (k, r, e) | k == key -> pure (r, e)
    _ -> do
      let r = resolveModules prog deps
          (nameDs, scopes) = resolveProgramFrom r prog
          -- What resolution reports in the dependencies is all their
          -- elaboration sees of it.
          e
            | null deps = Nothing
            | otherwise = elaborateModules prog (Map.restrictKeys scopes depSet) (filter (inFiles depSet) nameDs) deps
      writeIORef dependencyMemo (Just (key, r, e))
      pure (r, e)
  let (nameDs, scopes) = resolveProgramFrom depResolved prog
  pure (checkResolved True (elaborateRecoveringFrom resumed) prog nameDs scopes)

-- | Whether a diagnostic is reported in one of the files.
inFiles :: Set.Set FilePath -> Diagnostic -> Bool
inFiles files d = case diagSpan d of
  Span (Position f _ _) _ -> f `Set.member` files
  NoSpan -> False

-- | The modules of the program in its dependency cache that import
-- nothing outside it: a dependency never imports the project.
dependencyModules :: Program -> [FilePath]
dependencyModules prog = Set.toList (settle cached)
  where
    cacheParts = splitDirectories (collapseDots (normalise (progCacheDir prog)))
    underCache p = cacheParts `isPrefixOf` splitDirectories (collapseDots (normalise p))
    cached = Set.fromList [p | p <- Map.keys (progModules prog), underCache p]
    settle s =
      let s' = Set.filter (closedIn s) s
       in if Set.size s' == Set.size s then s else settle s'
    closedIn s p = case Map.lookup p (progModules prog) of
      Just lm -> all (`Set.member` s) (Map.elems (lmImportKeys lm))
      Nothing -> False

cachedParse :: FilePath -> Text -> IO (Either [Diagnostic] Module)
cachedParse path src = do
  known <- Map.lookup path <$> readIORef parseCache
  case known of
    Just (before, parsed) | before == src -> pure parsed
    _ -> do
      let parsed = parseModule path src
      atomicModifyIORef' parseCache (\m -> (Map.insert path (src, parsed) m, ()))
      pure parsed

-- | The result of the last compile of the buffer at the path, when the
-- buffer and every file that compile read are unchanged; otherwise a
-- new compile, whose load parses through the cache and records what it
-- reads.
remembered :: IORef (Map FilePath (Memo a)) -> FilePath -> Text -> (IO (Either [Diagnostic] Program) -> IO a) -> IO a
remembered memo path txt compile = do
  cacheDir <- cacheDirFor (takeDirectory path)
  let unchanged (ModuleInput p before) = (== before) <$> readerFor cacheDir p
      unchanged (FileInput p before) = (== before) <$> readMaybe p
  known <- Map.lookup path <$> readIORef memo
  fresh <- case known of
    Just (Memo before inputs result) | before == txt -> do
      same <- and <$> mapM unchanged inputs
      pure (if same then Just result else Nothing)
    _ -> pure Nothing
  case fresh of
    Just result -> pure result
    Nothing -> do
      readsRef <- newIORef []
      let note i = atomicModifyIORef' readsRef (\is -> (i : is, ()))
          entry = collapseDots (normalise path)
          reader p = do
            r <- textReader cacheDir path txt p
            -- The buffer is the memo's key, not one of its inputs.
            if collapseDots (normalise p) == entry then pure () else note (ModuleInput p r)
            pure r
          project dir = mapM_ (\f -> readMaybe (dir </> f) >>= note . FileInput (dir </> f)) [defaultDepsFileName, defaultLockFileName]
      env0 <- defaultLoaderEnv reader path
      let env =
            env0
              { leParse = cachedParse,
                leHolds = stampedHolds (leHolds env0),
                leLoadDeps = \dir -> project dir >> leLoadDeps env0 dir,
                leLoadLock = \dir -> project dir >> leLoadLock env0 dir
              }
      -- The project's lock, which the loader environment reads before
      -- any module, pins every dependency.
      project (takeDirectory entry)
      result <- compile (loadProgramEnv env path)
      inputs <- readIORef readsRef
      atomicModifyIORef' memo (\m -> (Map.insert path (Memo txt inputs result) m, ()))
      pure result
  where
    readMaybe p = either absent Just <$> try (BS.readFile p)
    absent :: IOException -> Maybe BS.ByteString
    absent _ = Nothing
