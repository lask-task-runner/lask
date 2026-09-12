{-# LANGUAGE OverloadedStrings #-}

-- | Shell completion (spec 11.7): the candidate model behind the
-- hidden @lask __complete@ protocol, and the scripts that
-- @lask completion@ prints.
--
-- Two rules shape everything here.
--
-- The first is the boundary rule of spec 11.2: every token after the
-- function name of @run@ \/ @eval@ is an argument of that function, so
-- past that point @lask@'s own options leave the candidate set and the
-- function's keyword parameters take their place.
--
-- The second is that completion runs on every keystroke, against
-- whatever module happens to be in the working directory. It
-- therefore only ever /parses/ the entry module: no elaboration, no
-- import resolution, no dependency cache, no network, and above all
-- no evaluation — a default value is read as the source text that
-- produced it, never run (spec 11.6).
module Command.Lask.Complete
  ( -- * Protocol
    Candidate (..),
    Response (..),
    renderResponse,
    dirError,
    dirNoSpace,
    dirNoFileComp,
    dirKeepOrder,
    activeHelpMarker,

    -- * Answering a request
    Resolver (..),
    fileResolver,
    complete,
    runComplete,

    -- * Where a request is, in the CLI grammar
    Plan (..),
    Opt (..),
    ValueSpec (..),
    PathKind (..),
    classify,

    -- * The completion index
    CompleteIndex (..),
    IndexDecl (..),
    IndexParam (..),
    ParamValues (..),
    buildIndex,

    -- * Scripts
    Shell (..),
    parseShell,
    shellName,
    completionScript,
  )
where

import Command.Lask.Help (ParamKind (..), declParams, isFunctionDecl)
import Control.Exception (IOException, SomeException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Language.Lask.Deps.File (DepsFile (..), defaultDepsFileName, parseDepsFile)
import Language.Lask.Doc (DocComment (..), docBlockAbove, emptyDoc, parseDoc)
import Language.Lask.Lexer (lexTokensWithComments)
import Language.Lask.Lexer.Token (Spanned (..))
import Language.Lask.Span (Position (..), Span (..))
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Syntax.Parser (parseModule)
import Language.Lask.Utils (kebabToSnake)
import System.Directory (doesDirectoryExist, getFileSize, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (hSetEncoding, stdout, utf8)
import System.Timeout (timeout)

-- Protocol ---------------------------------------------------------------------

-- | One candidate. The description is rendered by the shells that
-- have somewhere to put it (zsh, fish) and dropped by bash.
data Candidate = Candidate
  { candValue :: Text,
    candDesc :: Maybe Text
  }
  deriving (Show, Eq)

data Response = Response
  { resCandidates :: [Candidate],
    resDirective :: Int
  }
  deriving (Show, Eq)

-- | Ignore the candidates; let the shell fall back to its default.
dirError :: Int
dirError = 1

-- | Do not append a space (a directory prefix, a @--opt=@ fragment).
dirNoSpace :: Int
dirNoSpace = 2

-- | Do not fall back to file completion when the list is empty.
dirNoFileComp :: Int
dirNoFileComp = 4

-- | Emit in the given order instead of sorting.
dirKeepOrder :: Int
dirKeepOrder = 8

-- | Lines carrying a message rather than a candidate (spec 11.7).
activeHelpMarker :: Text
activeHelpMarker = "_lask_help"

-- | @value[\\tdescription]@ lines, then the directive line.
--
-- Values and descriptions are repository content that ends up inside
-- a shell's completion machinery, so everything that could break the
-- line protocol is removed here, once, for all three shells.
renderResponse :: Response -> Text
renderResponse (Response cands directive) =
  T.unlines (map row cands <> [":" <> T.pack (show directive)])
  where
    row (Candidate v Nothing) = clean v
    row (Candidate v (Just d)) = clean v <> "\t" <> ellipsis (clean d)

    clean = T.filter (\c -> c >= ' ' && c /= '\DEL')

    ellipsis d
      | T.length d <= 60 = d
      | otherwise = T.stripEnd (T.dropWhileEnd (/= ' ') (T.take 61 d)) <> "..."

-- Resolving --------------------------------------------------------------------

-- | The filesystem, as completion needs it. A record rather than
-- direct 'IO' so that the grammar and the index can be tested against
-- an in-memory project.
data Resolver = Resolver
  { -- | 'Nothing' for anything unreadable, or larger than the cap.
    resolveRead :: FilePath -> IO (Maybe Text),
    -- | Directory entries as @(name, isDirectory)@; @[]@ when the
    -- directory cannot be listed.
    resolveList :: FilePath -> IO [(FilePath, Bool)]
  }

-- | Modules above this are not read; completion falls back to the
-- static grammar (spec 11.7).
maxModuleBytes :: Integer
maxModuleBytes = 4 * 1024 * 1024

fileResolver :: Resolver
fileResolver =
  Resolver
    { resolveRead = \path -> do
        sz <- try' (getFileSize path)
        case sz of
          Right n
            | n <= maxModuleBytes ->
                either (const Nothing) Just <$> try' (TIO.readFile path)
          _ -> pure Nothing,
      resolveList = \dir -> do
        entries <- try' (listDirectory dir)
        case entries of
          Left _ -> pure []
          Right names -> mapM (withKind dir) names
    }
  where
    withKind dir n = do
      isDir <- try' (doesDirectoryExist (dir </> n))
      pure (n, either (const False) id isDir)

    try' :: IO a -> IO (Either IOException a)
    try' = try

-- | Answer one request. Never throws: an unreadable module or a
-- missing directory costs candidates, not the response.
complete :: Resolver -> [Text] -> IO Response
complete r ws = case classify ws of
  PCommands cmds pfx ->
    pure (noFiles [Candidate n (Just d) | (n, d) <- cmds, matches pfx n])
  POptions opts used pfx ->
    pure (noFiles (optionCandidates opts used pfx))
  POptValue spec pfx emit ->
    prefixed emit <$> valueCandidates r spec pfx
  PShellName pfx ->
    pure
      ( noFiles
          [ Candidate (shellName s) (Just (shellDesc s))
          | s <- [Bash, Zsh, Fish],
            matches pfx (shellName s)
          ]
      )
  PDepName modPath pfx -> do
    names <- depNames r modPath
    pure (noFiles [Candidate n Nothing | n <- names, matches pfx n])
  PFunctionName modPath pfx -> do
    idx <- loadIndex r modPath
    pure (noFiles (functionCandidates idx pfx))
  PFnFlags modPath fn used pfx -> do
    idx <- loadIndex r modPath
    pure (noFiles (flagCandidates idx fn used pfx))
  PFnValue modPath fn param pfx emit -> do
    idx <- loadIndex r modPath
    case paramOf idx fn param of
      Nothing -> pure (Response [] dirNoFileComp)
      Just p -> prefixed emit <$> valueCandidates r (paramSpec idx (ipValues p)) pfx
  PFnArg modPath fn n -> do
    idx <- loadIndex r modPath
    pure (Response (map hint (positionalHint idx fn n)) 0)
  PFree -> pure (Response [] dirNoFileComp)
  PNothing -> pure (Response [] 0)
  where
    noFiles cs = Response cs dirNoFileComp
    hint t = Candidate activeHelpMarker (Just t)

    -- The @--opt=value@ form completes the whole word, so that zsh and
    -- fish replace it correctly; bash, which splits the word at the
    -- @=@ itself, trims the prefix back off in its script.
    prefixed emit resp
      | T.null emit = resp
      | otherwise =
          resp
            { resCandidates =
                [c {candValue = emit <> candValue c} | c <- resCandidates resp]
            }

-- | The @__complete@ entry point: render the response for @words@ on
-- stdout and exit successfully, whatever happened (spec 11.7).
runComplete :: [String] -> IO ()
runComplete args = do
  hSetEncoding stdout utf8
  r <- try (timeout budgetMicros (render (map T.pack args)))
  TIO.putStr $ case r of
    Right (Just out) -> out
    Right Nothing -> giveUp
    Left e -> const giveUp (e :: SomeException)
  where
    budgetMicros = 200000
    giveUp = renderResponse (Response [] dirError)
    render ws = do
      resp <- complete fileResolver ws
      let out = renderResponse resp
      -- Forced inside the budget: a slow parse must not escape it by
      -- being deferred to the write.
      T.length out `seq` pure out

-- Candidates -------------------------------------------------------------------

optionCandidates :: [Opt] -> [Text] -> Text -> [Candidate]
optionCandidates opts used pfx =
  [ Candidate ("--" <> optLong o) (Just (optHelp o))
  | o <- opts,
    offerable o,
    matches pfx ("--" <> optLong o)
  ]
    <> [ Candidate (T.pack ['-', c]) (Just (optHelp o))
       | o <- opts,
         offerable o,
         Just c <- [optShort o],
         matches pfx (T.pack ['-', c])
       ]
  where
    offerable o =
      optLong o `notElem` used
        && all (`elem` used) (optNeeds o)
        && not (any (`elem` used) (optBlocks o))

-- | Function names (spec 11.2), less everything that could not be
-- called: plain values, @\@hidden@ and @internal@ declarations, and
-- functions taking a positional @Environment@ — no decoding mode can
-- construct one, so offering them would complete into a pre-execution
-- error.
functionCandidates :: CompleteIndex -> Text -> [Candidate]
functionCandidates idx pfx =
  [ Candidate (renderName pfx (idName d)) (idSummary d)
  | d <- ciDecls idx,
    idCallable d,
    not (idHidden d),
    not (idInternal d),
    idCliCallable d,
    matches pfx (idName d)
  ]

-- | The keyword parameters of one function, plus the @--help@ that
-- spec 11.2 intercepts after the function name. @-h@ is deliberately
-- absent there: it is not intercepted, because it would collide with
-- the short form of a single-character keyword parameter.
flagCandidates :: CompleteIndex -> Text -> [Text] -> Text -> [Candidate]
flagCandidates idx fn used pfx =
  [ Candidate (renderName pfx ("--" <> ipName p)) (ipDoc p)
  | p <- keywords,
    matches pfx ("--" <> ipName p)
  ]
    <> [ Candidate (T.cons '-' (ipName p)) (ipDoc p)
       | p <- keywords,
         T.length (ipName p) == 1,
         matches pfx (T.cons '-' (ipName p))
       ]
    <> [ Candidate "--help" (Just "Show this function's help")
       | "help" `notElem` used,
         matches pfx "--help"
       ]
  where
    keywords =
      [ p
      | p <- maybe [] idParams (declOf idx fn),
        ipKind p == ParamKeyword,
        -- Spec 11.2: an Environment keyword parameter keeps its
        -- default and cannot be supplied from the CLI.
        not (ipEnvironment p),
        -- Duplicate binding is a pre-execution error, so a parameter
        -- already given is not offered again.
        ipName p `notElem` used
      ]

valueCandidates :: Resolver -> ValueSpec -> Text -> IO Response
valueCandidates r spec pfx = case spec of
  VChoices cs ->
    pure (Response [Candidate v d | (v, d) <- cs, matches pfx v] dirNoFileComp)
  VOpaque -> pure (Response [] dirNoFileComp)
  VShellFile -> pure (Response [] 0)
  VPath kind -> pathCandidates r kind pfx

-- | Paths are enumerated here rather than delegated to a
-- shell-specific directive, so that the @*.lask@ filter behaves
-- identically in all three shells.
pathCandidates :: Resolver -> PathKind -> Text -> IO Response
pathCandidates r kind pfx = do
  entries <- resolveList r listDir
  let cands =
        [ Candidate (T.pack (dirPart <> name <> (if isDir then "/" else ""))) Nothing
        | (name, isDir) <- sortOn fst entries,
          base `isPrefixOf` name,
          -- A dotfile is offered only once the dot is typed.
          "." `isPrefixOf` base || not ("." `isPrefixOf` name),
          wanted isDir name
        ]
      allDirs = not (null cands) && all (T.isSuffixOf "/" . candValue) cands
  pure (Response cands (dirNoFileComp + (if allDirs then dirNoSpace else 0)))
  where
    typed = T.unpack pfx
    -- @dirPart@ keeps its trailing slash, because it is put back in
    -- front of every candidate; the directory to list does not.
    (dirPart, base) =
      let (revBase, revDir) = break (== '/') (reverse typed)
       in (reverse revDir, reverse revBase)
    listDir = case dirPart of
      "" -> "."
      "/" -> "/"
      d -> init d
    wanted isDir name = case kind of
      PathAny -> True
      PathDir -> isDir
      PathSuffix ext -> isDir || T.pack (takeExtension name) == ext

-- | The message shown while a positional argument is being typed: the
-- one place where completion says what to type rather than typing it.
positionalHint :: CompleteIndex -> Text -> Int -> [Text]
positionalHint idx fn n = case declOf idx fn of
  Nothing -> []
  Just d ->
    case drop n [p | p <- idParams d, ipKind p == ParamPositional] of
      (p : _) -> [describe p]
      [] -> map describe [p | p <- idParams d, ipKind p == ParamVariadic]
  where
    describe p =
      ipName p
        <> maybe "" (" : " <>) (ipTypeText p)
        <> maybe "" (" - " <>) (ipDoc p)

-- | Prefix matching under the @kebab -> snake@ mapping of spec 11.2,
-- so that @show-@ matches @show_version@.
matches :: Text -> Text -> Bool
matches pfx name = kebabToSnake pfx `T.isPrefixOf` kebabToSnake name

-- | Render a candidate in the style being typed: a @-@ inside the
-- typed name means kebab-case; anything else, an empty prefix
-- included, means the name as declared (spec 11.6).
renderName :: Text -> Text -> Text
renderName pfx name
  | "-" `T.isInfixOf` T.dropWhile (== '-') pfx =
      T.map (\c -> if c == '_' then '-' else c) name
  | otherwise = name

declOf :: CompleteIndex -> Text -> Maybe IndexDecl
declOf idx fn = lookup (kebabToSnake fn) [(idName d, d) | d <- ciDecls idx]

paramOf :: CompleteIndex -> Text -> Text -> Maybe IndexParam
paramOf idx fn param = do
  d <- declOf idx fn
  lookup (kebabToSnake param) [(ipName p, p) | p <- idParams d]

-- | @\@complete \<param\> \@keys \<binding\>@ is resolved here: by
-- reading the keys written in the map literal, never by evaluating
-- the binding (spec 3.1).
paramSpec :: CompleteIndex -> ParamValues -> ValueSpec
paramSpec idx pv = case pv of
  PvSpec spec -> spec
  PvKeys binding -> case Map.lookup binding (ciMapKeys idx) of
    Just keys | not (null keys) -> VChoices [(k, Nothing) | k <- keys]
    _ -> VOpaque

depNames :: Resolver -> FilePath -> IO [Text]
depNames r modPath = do
  txt <- resolveRead r (inModuleDir modPath defaultDepsFileName)
  pure $ case txt of
    Nothing -> []
    Just t -> case parseDepsFile (BL.fromStrict (TE.encodeUtf8 t)) of
      Left _ -> []
      Right df -> Map.keys (depsEntries df)

-- | A sibling of the entry module. A bare module name yields a bare
-- name, rather than @takeDirectory@'s @\".\/\"@ prefix.
inModuleDir :: FilePath -> FilePath -> FilePath
inModuleDir modPath name = case break (== '/') (reverse modPath) of
  (_, []) -> name
  (_, _ : revDir) -> reverse revDir </> name

-- The grammar ------------------------------------------------------------------

-- | What a request position asks for. Keeping this separate from the
-- filesystem is what makes the boundary rule testable.
data Plan
  = -- | Subcommand names with their descriptions.
    PCommands [(Text, Text)] Text
  | -- | Options, the long names already used, and the typed prefix.
    POptions [Opt] [Text] Text
  | -- | The value of an option: what to offer, the typed prefix, and
    -- the text to put back in front of each candidate (@--opt=@ for
    -- the inline form, empty otherwise).
    POptValue ValueSpec Text Text
  | PFunctionName FilePath Text
  | PFnFlags FilePath Text [Text] Text
  | PFnValue FilePath Text Text Text Text
  | -- | The nth positional argument of a function is being typed.
    PFnArg FilePath Text Int
  | PDepName FilePath Text
  | PShellName Text
  | -- | A name the user invents (@deps add NAME@).
    PFree
  | PNothing
  deriving (Show, Eq)

data ValueSpec
  = VChoices [(Text, Maybe Text)]
  | VPath PathKind
  | -- | A value the CLI cannot enumerate (an id, a URL): offer
    -- nothing, and do not let the shell offer files either.
    VOpaque
  | -- | Leave it to the shell's own file completion.
    VShellFile
  deriving (Show, Eq)

data PathKind
  = -- | Files and directories alike.
    PathAny
  | -- | Directories only.
    PathDir
  | -- | Directories, and files with this extension.
    PathSuffix Text
  deriving (Show, Eq)

data Opt = Opt
  { optLong :: Text,
    optShort :: Maybe Char,
    optValue :: Maybe ValueSpec,
    optHelp :: Text,
    -- | Offered only once these long options are present.
    optNeeds :: [Text],
    -- | Not offered once any of these is present.
    optBlocks :: [Text]
  }
  deriving (Show, Eq)

data PosSpec = PosFunction | PosDepName | PosShell | PosFree
  deriving (Show, Eq)

data Cmd = Cmd
  { cmdName :: Text,
    cmdHelp :: Text,
    cmdSubs :: [Cmd],
    cmdOpts :: [Opt],
    cmdPos :: [PosSpec],
    -- | @run@ and @eval@: past the function name, spec 11.2 hands
    -- every token to the function.
    cmdBoundary :: Bool
  }

switchOpt :: Text -> Text -> Opt
switchOpt l h = Opt l Nothing Nothing h [] []

valueOpt :: Text -> ValueSpec -> Text -> Opt
valueOpt l v h = Opt l Nothing (Just v) h [] []

helpOpt :: Opt
helpOpt = Opt "help" (Just 'h') Nothing "Show this help text" [] []

commonOpts :: [Opt]
commonOpts =
  [ valueOpt "module" (VPath (PathSuffix ".lask")) "Path to the entry module",
    valueOpt
      "format"
      (VChoices [("text", Just "Human-readable"), ("json", Just "Machine-readable")])
      "Diagnostics output format",
    valueOpt "trace-id" VOpaque "Trace identifier for this execution",
    switchOpt "no-color" "Disable colored output"
  ]

plain :: Text -> Text -> [Opt] -> Cmd
plain n h opts = Cmd n h [] (opts <> [helpOpt]) [] False

-- | The CLI surface of spec 11.1, as data. It mirrors
-- "Command.Lask.Options"; the end-to-end tests compare the two so
-- that they cannot drift apart unnoticed.
rootCommands :: [Cmd]
rootCommands =
  [ plain "serve" "Start the language server" [],
    plain "check" "Statically validate the module" commonOpts,
    runLike "run" "Run a function (result is not printed)",
    runLike "eval" "Run a function and print its result",
    plain "repl" "Interactive session" commonOpts,
    (plain "envs" "List and check environments" (commonOpts <> [switchOpt "check" "Check accessibility of each environment"]))
      { cmdPos = [PosFunction]
      },
    Cmd "deps" "Manage external dependencies" depsSubs [helpOpt] [] False,
    Cmd "env" "Materialize and inspect container images" envSubs [helpOpt] [] False,
    plain "version" "Print the lask version" [],
    (plain "completion" "Print the shell completion script" []) {cmdPos = [PosShell]}
  ]
  where
    runLike n h =
      Cmd
        { cmdName = n,
          cmdHelp = h,
          cmdSubs = [],
          cmdOpts = commonOpts <> runOpts,
          cmdPos = [PosFunction],
          cmdBoundary = True
        }

    runOpts =
      [ valueOpt
          "arg-decode"
          ( VChoices
              [ ("text", Just "Every argument is a String"),
                ("json", Just "Every argument is JSON"),
                ("auto", Just "JSON when it parses, else String")
              ]
          )
          "How to decode function arguments",
        valueOpt
          "stdout-encode"
          ( VChoices
              [ ("text", Just "Human-readable"),
                ("json", Just "Machine-readable"),
                ("pretty-json", Just "Formatted JSON")
              ]
          )
          "How to encode the eval result",
        Opt "help" (Just 'h') Nothing "Show the help of FUNCTION, or list the module's functions" [] []
      ]

    depsSubs =
      [ plain
          "sync"
          "Fetch and verify all declared dependencies"
          (commonOpts <> [switchOpt "frozen" "Fail instead of updating the lock file"]),
        (plain "add" "Fetch a source, record it with its content hash, and cache it" (commonOpts <> addSource))
          { cmdPos = [PosFree]
          },
        (plain "why" "Report the graph paths through which a dependency is reached" commonOpts)
          { cmdPos = [PosDepName]
          },
        (plain "diff" "Report what a dependency bump would change" commonOpts)
          { cmdPos = [PosDepName]
          }
      ]

    addSource =
      [ (valueOpt "git" VOpaque "Git repository URL") {optBlocks = ["url"]},
        (valueOpt "rev" VOpaque "Tag or commit") {optNeeds = ["git"]},
        (valueOpt "url" VOpaque "Archive or single .lask file URL") {optBlocks = ["git", "rev"]}
      ]

    envSubs =
      [ plain "build" "Materialize every image the program requires" commonOpts,
        plain "list" "Report every image reference and whether it is present" commonOpts
      ]

-- | The walk state: what the words consumed so far have established.
data St = St
  { stCmds :: [Cmd],
    stCmd :: Maybe Cmd,
    stOpts :: [Opt],
    stUsed :: [Text],
    stModule :: FilePath,
    stPos :: Int,
    -- | An option whose value is the word under the cursor.
    stPending :: Maybe ValueSpec,
    -- | The function name of @run@ \/ @eval@, once consumed.
    stFn :: Maybe Text,
    stFnUsed :: [Text],
    stFnPos :: Int,
    -- | A keyword parameter whose value is the word under the cursor.
    stPendingFn :: Maybe Text,
    stAfterSep :: Bool
  }

-- | Where the request is, in the grammar above.
--
-- @ws@ are the words after the program name; the last one is the word
-- under the cursor, and may be empty.
classify :: [Text] -> Plan
classify [] = atCursor initialSt ""
classify ws = walk initialSt (init ws) (last ws)

initialSt :: St
initialSt =
  St
    { stCmds = rootCommands,
      stCmd = Nothing,
      stOpts = [helpOpt],
      stUsed = [],
      stModule = "main.lask",
      stPos = 0,
      stPending = Nothing,
      stFn = Nothing,
      stFnUsed = [],
      stFnPos = 0,
      stPendingFn = Nothing,
      stAfterSep = False
    }

cmdSummary :: Cmd -> (Text, Text)
cmdSummary c = (cmdName c, cmdHelp c)

walk :: St -> [Text] -> Text -> Plan
walk st [] cur = atCursor st cur
walk st (w : rest) cur = case stFn st of
  Just _ -> walk (inFunctionArgs st w) rest cur
  Nothing -> case optWord w of
    Just (name, inline) -> optionWord st name inline rest cur
    Nothing -> case matchCommand st w of
      Just st' -> walk st' rest cur
      Nothing -> walk (consumePositional st w) rest cur

-- | Past the function name, spec 11.2 applies: a bare @--@ ends the
-- @--help@ scan, and everything else is the function's own argument.
--
-- 'stPendingFn' does double duty: it remembers the keyword parameter
-- whose value comes next, so that the value is not miscounted as a
-- positional argument, and — when the line ends there — it says that
-- the word being typed /is/ that value.
inFunctionArgs :: St -> Text -> St
inFunctionArgs st w
  | w == "--" = st {stAfterSep = True, stPendingFn = Nothing}
  | stAfterSep st = positional
  | otherwise = case optWord w of
      Nothing
        | Just _ <- stPendingFn st -> st {stPendingFn = Nothing}
        | otherwise -> positional
      Just (name, Just _) -> keyword name Nothing
      Just (name, Nothing) -> keyword name (Just (kebabToSnake name))
  where
    positional = st {stFnPos = stFnPos st + 1, stPendingFn = Nothing}
    keyword name pending =
      st {stFnUsed = kebabToSnake name : stFnUsed st, stPendingFn = pending}

optionWord :: St -> Text -> Maybe Text -> [Text] -> Text -> Plan
optionWord st name inline rest cur = case (inline, optValue =<< lookupOpt (stOpts st) name) of
  (Just v, _) -> walk (record name v used) rest cur
  (Nothing, Just spec) -> case rest of
    -- The value is the next word, or — when there is none — the word
    -- being typed.
    (v : more) -> walk (record name v used) more cur
    [] -> walk used {stPending = Just spec} rest cur
  _ -> walk used rest cur
  where
    used = st {stUsed = name : stUsed st, stPending = Nothing}

-- | @--module@ decides which module the function names come from, so
-- its value is tracked as the line is walked.
record :: Text -> Text -> St -> St
record name v st
  | name == "module" = st {stModule = T.unpack v}
  | otherwise = st

matchCommand :: St -> Text -> Maybe St
matchCommand st w = case [c | c <- stCmds st, cmdName c == w] of
  (c : _) ->
    Just
      st
        { stCmds = cmdSubs c,
          stCmd = Just c,
          stOpts = cmdOpts c,
          stPos = 0,
          stPending = Nothing
        }
  [] -> Nothing

consumePositional :: St -> Text -> St
consumePositional st w = case currentPos st of
  Just PosFunction
    | maybe False cmdBoundary (stCmd st) -> st {stFn = Just (kebabToSnake w), stPending = Nothing}
  _ -> st {stPos = stPos st + 1, stPending = Nothing}

currentPos :: St -> Maybe PosSpec
currentPos st = case stCmd st of
  Just c -> case drop (stPos st) (cmdPos c) of
    (p : _) -> Just p
    [] -> Nothing
  Nothing -> Nothing

-- | A word that is an option: its long name (or a short name without
-- the dash) and, for the @--opt=value@ form, the value.
optWord :: Text -> Maybe (Text, Maybe Text)
optWord w
  | w == "--" || w == "-" = Nothing
  | Just body <- T.stripPrefix "--" w =
      let (name, rest) = T.break (== '=') body
       in Just (name, if T.null rest then Nothing else Just (T.drop 1 rest))
  | Just body <- T.stripPrefix "-" w, not (T.null body) = Just (body, Nothing)
  | otherwise = Nothing

-- | Like 'optWord', but for the word being typed, where a bare @-@ or
-- @--@ is the beginning of an option rather than the separator it
-- would be in a completed word.
cursorOptWord :: Text -> Maybe (Text, Maybe Text)
cursorOptWord w
  | w == "-" || w == "--" = Just ("", Nothing)
  | otherwise = optWord w

lookupOpt :: [Opt] -> Text -> Maybe Opt
lookupOpt opts name =
  case [o | o <- opts, optLong o == name || sameShort o] of
    (o : _) -> Just o
    [] -> Nothing
  where
    -- Two options with no short form are not the same option.
    sameShort o = case (optShort o, T.unpack name) of
      (Just c, [n]) -> c == n
      _ -> False

-- | The word under the cursor decides which position the request is
-- in.
atCursor :: St -> Text -> Plan
atCursor st cur = case stFn st of
  Just fn
    | stAfterSep st -> PNothing
    | otherwise -> case cursorOptWord cur of
        Just (name, Just v) -> PFnValue (stModule st) fn name v ("--" <> name <> "=")
        Just _ -> PFnFlags (stModule st) fn (stFnUsed st) cur
        Nothing -> case stPendingFn st of
          Just param -> PFnValue (stModule st) fn param cur ""
          Nothing -> PFnArg (stModule st) fn (stFnPos st)
  Nothing -> case cursorOptWord cur of
    Just (name, Just v) -> case optValue =<< lookupOpt (stOpts st) name of
      Just spec -> POptValue spec v ("--" <> name <> "=")
      Nothing -> PNothing
    Just _ -> POptions (stOpts st) (stUsed st) cur
    Nothing -> case stPending st of
      Just spec -> POptValue spec cur ""
      Nothing
        | not (null (stCmds st)) -> PCommands (map cmdSummary (stCmds st)) cur
        | otherwise -> case currentPos st of
            Just PosFunction -> PFunctionName (stModule st) cur
            Just PosDepName -> PDepName (stModule st) cur
            Just PosShell -> PShellName cur
            Just PosFree -> PFree
            Nothing -> PNothing

-- The index --------------------------------------------------------------------

data CompleteIndex = CompleteIndex
  { ciDecls :: [IndexDecl],
    -- | Top-level map literals by binding name, for @\@complete
    -- \@keys@.
    ciMapKeys :: Map.Map Text [Text]
  }
  deriving (Show, Eq)

data IndexDecl = IndexDecl
  { idName :: Text,
    -- | A function declaration, or a binding whose value is a lambda.
    idCallable :: Bool,
    idHidden :: Bool,
    idInternal :: Bool,
    -- | False when a positional parameter is an @Environment@
    -- (spec 11.2).
    idCliCallable :: Bool,
    idSummary :: Maybe Text,
    idParams :: [IndexParam]
  }
  deriving (Show, Eq)

data IndexParam = IndexParam
  { ipName :: Text,
    ipKind :: ParamKind,
    ipEnvironment :: Bool,
    ipTypeText :: Maybe Text,
    ipDoc :: Maybe Text,
    ipValues :: ParamValues
  }
  deriving (Show, Eq)

-- | A parameter's value candidates, before the module is consulted
-- for @\@keys@.
data ParamValues = PvSpec ValueSpec | PvKeys Text
  deriving (Show, Eq)

loadIndex :: Resolver -> FilePath -> IO CompleteIndex
loadIndex r path = do
  txt <- resolveRead r path
  pure (maybe (CompleteIndex [] Map.empty) (buildIndex path) txt)

-- | The index of a module, from its source text alone.
--
-- When the module parses, everything comes from the surface syntax
-- and its documentation comments. When it does not — the usual state
-- of a module being edited in another window — declaration heads are
-- recovered by a line scan, so that names and keyword parameters
-- survive a syntax error.
buildIndex :: FilePath -> Text -> CompleteIndex
buildIndex path src = case parseModule path src of
  Right m -> fromModule path src m
  Left _ -> scanIndex src

fromModule :: FilePath -> Text -> AST.Module -> CompleteIndex
fromModule path src m =
  CompleteIndex
    { ciDecls = mapMaybe decl (AST.moduleDecls m),
      ciMapKeys = Map.fromList (mapMaybe mapBinding (AST.moduleDecls m))
    }
  where
    comments = either (const []) snd (lexTokensWithComments path src)

    decl d = do
      n <- declaredName d
      let doc = docFor d
          ps = map (param doc) (declParams d)
      pure
        IndexDecl
          { idName = n,
            idCallable = isFunctionDecl d,
            idHidden = docHidden doc,
            idInternal = n `Set.member` AST.moduleInternal m,
            idCliCallable =
              not (any (\p -> ipKind p == ParamPositional && ipEnvironment p) ps),
            idSummary = docSummary doc,
            idParams = ps
          }

    param doc p = case AST.paramF p of
      AST.PPositional n _ ty -> build n ParamPositional ty
      AST.PVariadic n ty -> build n ParamVariadic ty
      AST.PKeyword n _ ty _ -> build n ParamKeyword ty
      where
        build n kind ty =
          IndexParam
            { ipName = n,
              ipKind = kind,
              ipEnvironment = isEnvType ty,
              ipTypeText = typeText <$> ty,
              ipDoc = lookup n (docParams doc),
              ipValues = valuesFor doc n ty
            }

    valuesFor doc n ty = case lookup n (docComplete doc) of
      Just spec -> completeTag spec
      Nothing
        | isBoolType ty -> PvSpec (VChoices [("true", Nothing), ("false", Nothing)])
        | otherwise -> PvSpec VShellFile

    docFor d = case AST.declSpan d of
      Span (Position _ l _) _ -> maybe emptyDoc parseDoc (docBlockAbove src comments l)
      NoSpan -> emptyDoc

    mapBinding d = case AST.declF d of
      AST.DValue n _ _ (AST.Expr _ (AST.EObject fields)) ->
        Just (n, [k | (Spanned _ k, _) <- fields])
      _ -> Nothing

-- | The words of an @\@complete@ tag (spec 3.1).
completeTag :: [Text] -> ParamValues
completeTag ws = case ws of
  ["@keys", binding] -> PvKeys binding
  ("@file" : pattern') -> PvSpec (VPath (globKind pattern'))
  ["@dir"] -> PvSpec (VPath PathDir)
  [] -> PvSpec VShellFile
  choices
    | not (any ("@" `T.isPrefixOf`) choices) -> PvSpec (VChoices [(c, Nothing) | c <- choices])
    -- Forward compatibility (spec 3.1): an unrecognised form is
    -- ignored rather than reported.
    | otherwise -> PvSpec VShellFile

-- | @\@file@ takes an optional @*.ext@ filter; anything else it is
-- given means "any file".
globKind :: [Text] -> PathKind
globKind ws = case ws of
  [p] | Just ext <- T.stripPrefix "*" p, "." `T.isPrefixOf` ext -> PathSuffix ext
  _ -> PathAny

declaredName :: AST.Decl -> Maybe Text
declaredName d = case AST.declF d of
  AST.DValue n _ _ _ -> Just n
  AST.DFunction n _ _ _ -> Just n
  _ -> Nothing

isEnvType :: Maybe AST.SType -> Bool
isEnvType (Just (AST.SType _ AST.SEnvironment)) = True
isEnvType _ = False

isBoolType :: Maybe AST.SType -> Bool
isBoolType (Just (AST.SType _ AST.SBool)) = True
isBoolType _ = False

typeText :: AST.SType -> Text
typeText (AST.SType _ f) = case f of
  AST.SAny -> "Any"
  AST.SNumber -> "Number"
  AST.SString -> "String"
  AST.SBool -> "Bool"
  AST.SNull -> "Null"
  AST.SVoid -> "Void"
  AST.SEnvironment -> "Environment"
  AST.SArray t -> "Array<" <> typeText t <> ">"
  AST.SMap t -> "Map<" <> typeText t <> ">"
  AST.SRecord _ -> "Record"
  AST.SAsyncHandle t -> "AsyncHandle<" <> typeText t <> ">"
  AST.SFunction ps rt -> "(" <> T.intercalate ", " (map typeText ps) <> ") => " <> typeText rt
  AST.SNamed ns n -> maybe n (\q -> q <> "." <> n) ns

-- | What survives a module that does not parse: the head of every
-- top-level declaration, and the keyword parameters written in its
-- parameter list.
scanIndex :: Text -> CompleteIndex
scanIndex src = CompleteIndex (mapMaybe declOfLine (T.lines src)) Map.empty
  where
    declOfLine l = do
      let (name, rest) = T.span isNameChar l
          body = T.dropWhile (== ' ') rest
      if T.null name || not (startsDecl l) || not (isHead body)
        then Nothing
        else
          Just
            IndexDecl
              { idName = name,
                idCallable = "(" `T.isPrefixOf` body,
                idHidden = False,
                idInternal = False,
                idCliCallable = True,
                idSummary = Nothing,
                idParams = map keyword (keywordsOf body)
              }

    isHead body = any (`T.isPrefixOf` body) ["(", "=", ":"]

    keyword n =
      IndexParam
        { ipName = n,
          ipKind = ParamKeyword,
          ipEnvironment = False,
          ipTypeText = Nothing,
          ipDoc = Nothing,
          ipValues = PvSpec VShellFile
        }

    -- A declaration head starts in column 1 with a lower identifier.
    startsDecl l = case T.uncons l of
      Just (c, _) -> c == '_' || (c >= 'a' && c <= 'z')
      Nothing -> False

    keywordsOf body =
      nub
        [ n
        | chunk <- T.split (`elem` (" ,(" :: String)) body,
          Just afterDashes <- [T.stripPrefix "--" chunk],
          let n = T.takeWhile isNameChar afterDashes,
          not (T.null n)
        ]

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_'

-- Scripts ----------------------------------------------------------------------

data Shell = Bash | Zsh | Fish
  deriving (Show, Eq)

parseShell :: String -> Maybe Shell
parseShell s = case s of
  "bash" -> Just Bash
  "zsh" -> Just Zsh
  "fish" -> Just Fish
  _ -> Nothing

shellName :: Shell -> Text
shellName s = case s of
  Bash -> "bash"
  Zsh -> "zsh"
  Fish -> "fish"

shellDesc :: Shell -> Text
shellDesc s = case s of
  Bash -> "Script for bash"
  Zsh -> "Script for zsh (a #compdef file)"
  Fish -> "Script for fish"

-- | The script is static: every decision lives in the binary, so an
-- installed script keeps working across upgrades.
completionScript :: Shell -> Text
completionScript sh = case sh of
  Bash -> bashScript
  Zsh -> zshScript
  Fish -> fishScript

-- | bash has nowhere to render a description, so only the value
-- before the tab is kept. @compopt@ is guarded because macOS still
-- ships bash 3.2, which does not have it.
bashScript :: Text
bashScript =
  T.unlines
    [ "# lask completion for bash. Generated by `lask completion bash`.",
      "_lask() {",
      "    # IFS is deliberately left alone: bash 3.2, which macOS still",
      "    # ships, mis-expands \"${array[@]:off:len}\" when IFS is changed.",
      "    local line directive=0 cur w i",
      "    local -a raw merged",
      "    raw=(\"${COMP_WORDS[@]:1:COMP_CWORD-1}\" \"${COMP_WORDS[COMP_CWORD]}\")",
      "    merged=()",
      "    # bash splits words at '=' (COMP_WORDBREAKS); rejoin them so",
      "    # that --opt=value reaches lask as the single word it is.",
      "    for w in \"${raw[@]}\"; do",
      "        i=$(( ${#merged[@]} - 1 ))",
      "        if [[ ${#merged[@]} -gt 0 && ( $w == \"=\" || ${merged[$i]} == *= ) ]]; then",
      "            merged[$i]=\"${merged[$i]}$w\"",
      "        else",
      "            merged+=(\"$w\")",
      "        fi",
      "    done",
      "    cur=\"${merged[$(( ${#merged[@]} - 1 ))]}\"",
      "    COMPREPLY=()",
      "    while read -r line; do",
      "        [[ -z $line ]] && continue",
      "        case $line in",
      "            :*) directive=\"${line#:}\" ;;",
      "            _lask_help*) ;;",
      "            *) COMPREPLY+=(\"${line%%$'\\t'*}\") ;;",
      "        esac",
      "    done < <(\"${COMP_WORDS[0]}\" __complete -- \"${merged[@]}\" 2>/dev/null)",
      "    if (( directive & 1 )); then",
      "        COMPREPLY=()",
      "        return 0",
      "    fi",
      "    # bash replaces only the fragment after the '=' it split on.",
      "    if [[ $cur == *=* ]]; then",
      "        COMPREPLY=(\"${COMPREPLY[@]#*=}\")",
      "    fi",
      "    if type compopt &>/dev/null; then",
      "        (( directive & 4 )) && compopt +o default",
      "        (( directive & 2 )) && compopt -o nospace",
      "    fi",
      "    return 0",
      "}",
      "complete -o default -F _lask lask"
    ]

-- | zsh renders the descriptions through @_describe@ and the active
-- help lines through @_message@. The file works both in @fpath@ and
-- when sourced directly.
zshScript :: Text
zshScript =
  T.unlines
    [ "#compdef lask",
      "# lask completion for zsh. Generated by `lask completion zsh`.",
      "",
      "_lask() {",
      "    local -a args cands helps copts response",
      "    local line directive=0 value desc",
      "",
      "    args=(\"${(@)words[2,CURRENT]}\")",
      "    (( ${#args} < CURRENT - 1 )) && args+=('')",
      "",
      "    response=(\"${(@f)$(${words[1]} __complete -- \"${args[@]}\" 2>/dev/null)}\")",
      "",
      "    for line in \"${response[@]}\"; do",
      "        if [[ $line == :* ]]; then",
      "            directive=\"${line#:}\"",
      "        elif [[ $line == _lask_help$'\\t'* ]]; then",
      "            helps+=(\"${line#*$'\\t'}\")",
      "        elif [[ -n $line ]]; then",
      "            value=\"${line%%$'\\t'*}\"",
      "            desc=\"${line#*$'\\t'}\"",
      "            if [[ $desc == $line ]]; then",
      "                cands+=(\"${value//:/\\\\:}\")",
      "            else",
      "                cands+=(\"${value//:/\\\\:}:$desc\")",
      "            fi",
      "        fi",
      "    done",
      "",
      "    (( directive & 1 )) && return 1",
      "    (( directive & 2 )) && copts=(-S '')",
      "",
      "    (( ${#cands} )) && _describe -t lask-candidates lask cands $copts",
      "    for line in \"${helps[@]}\"; do",
      "        _message -r \"$line\"",
      "    done",
      "    (( directive & 4 )) || _files",
      "    return 0",
      "}",
      "",
      "if [ \"$funcstack[1]\" = \"_lask\" ]; then",
      "    _lask \"$@\"",
      "else",
      "    compdef _lask lask",
      "fi"
    ]

-- | fish takes @value\\tdescription@ natively. File completion is
-- produced by the function itself, since a @complete@ rule cannot
-- decide it per request.
fishScript :: Text
fishScript =
  T.unlines
    [ "# lask completion for fish. Generated by `lask completion fish`.",
      "",
      "function __lask_complete",
      "    set -l tokens (commandline --current-process --tokenize --cut-at-cursor)",
      "    set -l tok (commandline --current-token --cut-at-cursor)",
      "    set -l cur \"\"",
      "    if set -q tok[1]",
      "        set cur $tok[1]",
      "    end",
      "    set -e tokens[1]",
      "",
      "    set -l directive 0",
      "    for line in (lask __complete -- $tokens $cur 2>/dev/null)",
      "        if string match -q -- ':*' $line",
      "            set directive (string sub -s 2 -- $line)",
      "        else if string match -q -- '_lask_help*' $line",
      "            continue",
      "        else if test -n \"$line\"",
      "            echo $line",
      "        end",
      "    end",
      "",
      "    # Bit 4 is NoFileComp; the bits below it do not affect files.",
      "    if test (math \"$directive % 8\") -lt 4",
      "        __fish_complete_path $cur",
      "    end",
      "end",
      "",
      "complete -c lask -f -a '(__lask_complete)'"
    ]
