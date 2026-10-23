{-# LANGUAGE OverloadedStrings #-}

-- | The shell reading of a command string (spec 6.6, "Shell syntax"):
-- the POSIX structure the static checks of a command execution
-- expression stand on, namely whether the string is well formed, and
-- the simple commands it runs with their words (5, command schemas).
--
-- The reading follows the determinacy principle of 10.9. It stops at
-- the first construct whose reading depends on the shell dialect or on
-- a value — a reserved word in command position, a here-document, a
-- hole written @#!{e}@, and the others 6.6 lists — and nothing past
-- that point is reported. What it reports before it is an error in
-- every POSIX shell.
--
-- It is a separate reading from the command-word scan of 10.9
-- ('Language.Lask.Syntax.CommandWords'), which dispatch keeps: that
-- scan is specified as a lexical procedure of its own, and treats
-- reserved words and redirections as ordinary words.
module Language.Lask.Syntax.Shell
  ( ShellReading (..),
    SimpleCommand (..),
    ShellWord (..),
    readShell,
  )
where

import Data.List (sortOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Syntax.AST (Expr (..), TextPart (..))

-- | What a command string reads as, up to where reading stops.
data ShellReading = ShellReading
  { -- | The first syntax error, with where it is and what is wrong.
    srProblem :: Maybe (Span, Text),
    -- | The simple commands with a command word, nested ones included,
    -- in source order.
    srCommands :: [SimpleCommand]
  }
  deriving (Show, Eq)

data SimpleCommand = SimpleCommand
  { -- | The command word, after any assignment words.
    scName :: ShellWord,
    -- | The words after it, redirections and their targets left out.
    scArgs :: [ShellWord],
    -- | Reading stopped inside this command, so words may follow the
    -- last of 'scArgs' that were not read.
    scCut :: Bool
  }
  deriving (Show, Eq)

data ShellWord = ShellWord
  { swSpan :: Span,
    -- | The literal text the word begins with, quotes removed: all of
    -- it when 'swLiteral'.
    swPrefix :: Text,
    -- | The word is literal text, its value 'swPrefix'.
    swLiteral :: Bool,
    -- | Nothing in it is quoted, escaped or expanded.
    swPlain :: Bool,
    -- | It expands to exactly one field. A hole does (6.6); an
    -- unquoted parameter expansion, substitution or pattern may not.
    swSingle :: Bool
  }
  deriving (Show, Eq)

-- | Read a command string, given as the parts of its expression.
readShell :: [TextPart] -> ShellReading
readShell parts = case end of
  Left (Bad sp why) -> ShellReading (Just (sp, why)) []
  Left (Unclosed why) -> ShellReading (Just (NoSpan, why)) []
  Left (Stop _) -> combine (parse True toks)
  Right _ -> combine (parse False toks)
  where
    (end, toks, nested) = lexScript False (atoms parts)
    combine (problem, cmds) =
      ShellReading
        { srProblem = listToMaybe (sortOn (spanKey . fst) [p | Just p <- problem : map srProblem nested]),
          srCommands = sortOn (spanKey . swSpan . scName) (cmds <> concatMap srCommands nested)
        }

spanKey :: Span -> Maybe (Int, Int)
spanKey (Span (Position _ l c) _) = Just (l, c)
spanKey NoSpan = Nothing

-- Atoms ---------------------------------------------------------------------

-- | One character, or one hole. A hole written @#!{e}@ is parsed by the
-- shell, so it can be anything (6.6).
data Atom
  = AChar Span Char
  | AHole Span Bool

atoms :: [TextPart] -> [Atom]
atoms = concatMap part
  where
    part (TPInterp e) = [AHole (exprSpan e) False]
    part (TPRaw e) = [AHole (exprSpan e) True]
    part (TPChunk sp t) = walk (startOf sp) (T.unpack t)

    startOf (Span s _) = Just s
    startOf NoSpan = Nothing

    walk _ [] = []
    walk Nothing (c : cs) = AChar NoSpan c : walk Nothing cs
    walk (Just p) (c : cs) =
      let p' = advance p c
       in AChar (Span p p') c : walk (Just p') cs

    advance p '\n' = p {line = line p + 1, column = 1}
    advance p _ = p {column = column p + 1}

-- Tokens --------------------------------------------------------------------

data Tok
  = TWord ShellWord
  | -- | @&&@, @||@, @|@, @;@, @;;@, @&@, @(@, @)@, or a newline as @\\n@.
    TOp Span Text
  | -- | A redirection operator, its target still to come.
    TRedir Span Text

-- | Why lexing ended early.
data End
  = -- | A construct reading cannot see past.
    Stop Span
  | -- | A syntax error.
    Bad Span Text
  | -- | The input ended inside a region; its opener knows where.
    Unclosed Text

-- | A word being built.
data WB = WB
  { wbSpan :: Span,
    wbPrefix :: Text,
    -- | Still literal: the prefix is the whole word so far.
    wbOpen :: Bool,
    wbPlain :: Bool,
    wbSingle :: Bool
  }

data LS = LS
  { lsToks :: [Tok],
    lsNested :: [ShellReading],
    lsWord :: Maybe WB,
    -- | Open @(@ of the input being lexed, so that the @)@ closing a
    -- command substitution is told from one closing a group.
    lsDepth :: Int
  }

-- | Lex a script: the whole string, or with @inner@ the inside of a
-- @$(@, up to its closing @)@. Gives what follows that @)@, or why
-- lexing ended, with the tokens read so far and the readings of the
-- substitutions among them.
lexScript :: Bool -> [Atom] -> (Either End [Atom], [Tok], [ShellReading])
lexScript inner = run (LS [] [] Nothing 0)
  where
    run st as = case step st as of
      Next st' as' -> run st' as'
      Done st' r -> (r, reverse (lsToks st'), reverse (lsNested st'))

    step st [] =
      if inner
        then Done st (Left (Unclosed "unterminated command substitution"))
        else Done (flush st) (Right [])
    step st (AHole sp raw : rest)
      | raw = Done st (Left (Stop sp))
      | otherwise = Next (extend sp (\w -> w {wbOpen = False, wbPlain = False}) st) rest
    step st (AChar sp c : rest) = case c of
      _ | c == ' ' || c == '\t' -> Next (flush st) rest
      _ | c == '\n' || c == '\r' -> Next (emit (TOp sp "\n") (flush st)) rest
      '\\' -> case rest of
        AChar sp2 c2 : rest' -> Next (lit sp2 c2 (escaped sp st)) rest'
        AHole sp2 False : rest' -> Next (extend sp2 dyn (escaped sp st)) rest'
        AHole sp2 True : _ -> Done st (Left (Stop sp2))
        [] -> Next (lit sp '\\' st) rest
      '\'' -> single sp st rest
      '"' -> double sp (extend sp unplain st) rest
      '`' -> backquoted sp False st rest
      '$' -> dollar sp False st rest
      '#' | not (isJust (lsWord st)) -> comment st rest
      '|' -> case rest of
        AChar sp2 '|' : rest' -> Next (op (sp <> sp2) "||" st) rest'
        AChar sp2 '&' : _ -> Done st (Left (Stop sp2))
        _ -> Next (op sp "|" st) rest
      '&' -> case rest of
        AChar sp2 '&' : rest' -> Next (op (sp <> sp2) "&&" st) rest'
        _ -> Next (op sp "&" st) rest
      ';' -> case rest of
        AChar sp2 ';' : rest' -> Next (op (sp <> sp2) ";;" st) rest'
        _ -> Next (op sp ";" st) rest
      '('
        | isJust (lsWord st) -> Done st (Left (Stop sp))
        | otherwise -> Next ((op sp "(" st) {lsDepth = lsDepth st + 1}) rest
      ')'
        | inner && lsDepth st == 0 -> Done (flush st) (Right rest)
        | otherwise -> Next ((op sp ")" st) {lsDepth = max 0 (lsDepth st - 1)}) rest
      '<' -> redirect sp '<' (ioNumber st) rest
      '>' -> redirect sp '>' (ioNumber st) rest
      _
        | c `elem` ("*?[" :: String) -> Next (lit sp c (extend sp (\w -> w {wbOpen = False, wbSingle = False}) st)) rest
        | c == '~' && not (isJust (lsWord st)) -> Next (lit sp c (extend sp dyn st)) rest
        | otherwise -> Next (lit sp c st) rest

    op sp t st = emit (TOp sp t) (flush st)

    -- The digits written right before a redirection operator are the
    -- descriptor it redirects, not a word.
    ioNumber st = case lsWord st of
      Just w
        | wbOpen w,
          wbPlain w,
          not (T.null (wbPrefix w)),
          T.all (`elem` ['0' .. '9']) (wbPrefix w) ->
            st {lsWord = Nothing}
      _ -> flush st

    redirect sp c st rest =
      let (sp', name, rest') = case (c, rest) of
            ('<', AChar s2 '&' : r) -> (sp <> s2, "<&", r)
            ('<', AChar s2 '>' : r) -> (sp <> s2, "<>", r)
            ('>', AChar s2 '>' : r) -> (sp <> s2, ">>", r)
            ('>', AChar s2 '&' : r) -> (sp <> s2, ">&", r)
            ('>', AChar s2 '|' : r) -> (sp <> s2, ">|", r)
            _ -> (sp, T.singleton c, rest)
       in case rest of
            -- A here-document, and process substitution, which only
            -- some shells have.
            AChar s2 '<' : _ | c == '<' -> Done st (Left (Stop s2))
            _ -> case rest' of
              AChar s2 '(' : _ -> Done st (Left (Stop s2))
              _ -> Next (emit (TRedir sp' name) st) rest'

    -- A comment runs to the end of the line. A hole in it may hold a
    -- newline, which would end it.
    comment st rest = case break newlineOrHole rest of
      (_, AHole sp _ : _) -> Done st (Left (Stop sp))
      (_, more) -> Next st more
      where
        newlineOrHole (AChar _ ch) = ch == '\n' || ch == '\r'
        newlineOrHole AHole {} = True

    single sp st0 rest0 = go (extend sp unplain st0) rest0
      where
        go _ [] = Done st0 (Left (Bad sp "unterminated quoted region"))
        go st (AChar _ '\'' : rest) = Next st rest
        go st (AChar sp2 ch : rest) = go (lit sp2 ch st) rest
        go st (AHole sp2 raw : rest)
          | raw = Done st (Left (Stop sp2))
          | otherwise = go (extend sp2 dyn st) rest

    double sp st0 rest0 = go st0 rest0
      where
        go _ [] = Done st0 (Left (Bad sp "unterminated quoted region"))
        go st (a : rest) = case a of
          AChar _ '"' -> Next st rest
          AChar _ '\\' -> case rest of
            AChar sp2 ch : rest'
              | ch `elem` ("$`\"\\\n" :: String) -> go (lit sp2 ch st) rest'
            _ -> go (lit sp '\\' st) rest
          AChar sp2 '$' -> continueWith (dollar sp2 True st rest)
          AChar sp2 '`' -> continueWith (backquoted sp2 True st rest)
          AChar sp2 ch -> go (lit sp2 ch st) rest
          AHole sp2 raw
            | raw -> Done st (Left (Stop sp2))
            | otherwise -> go (extend sp2 dyn st) rest
        continueWith r = case r of
          Next st as -> go st as
          done -> done

    -- What follows a '$'.
    dollar sp inDq st rest = case rest of
      AChar _ '(' : AChar _ '(' : more -> arithmetic sp (expanded sp inDq True st) more
      AChar _ '(' : more -> substitution sp inDq st more
      AChar _ '{' : more -> braced sp (expanded sp inDq False st) more
      AChar sp2 '\'' : _ | not inDq -> Done st (Left (Stop sp2))
      AHole sp2 _ : _ | not inDq -> Done st (Left (Stop sp2))
      AChar _ ch : more
        | nameStart ch -> let (_, more') = span nameChar' more in Next (expanded sp inDq False st) more'
        | ch `elem` ("@*#?$!-" :: String) || ch `elem` ['0' .. '9'] -> Next (expanded sp inDq False st) more
      _ -> Next (lit sp '$' st) rest
      where
        nameChar' (AChar _ ch) = nameStart ch || ch `elem` ['0' .. '9']
        nameChar' AHole {} = False

    nameStart ch = ch `elem` ['a' .. 'z'] || ch `elem` ['A' .. 'Z'] || ch == '_'

    -- An expansion in the word: no longer literal, and unless it is
    -- quoted, not known to stay one field.
    expanded sp inDq one st =
      extend sp (\w -> w {wbOpen = False, wbPlain = False, wbSingle = wbSingle w && (inDq || one)}) st

    -- @$(( ... ))@, read to its closing parentheses.
    arithmetic sp st = go (2 :: Int)
      where
        go _ [] = Done st (Left (Bad sp "unterminated arithmetic expansion"))
        go d (a : more) = case a of
          AChar _ '(' -> go (d + 1) more
          AChar _ ')'
            | d == 1 -> Next st more
            | otherwise -> go (d - 1) more
          AHole sp2 True -> Done st (Left (Stop sp2))
          _ -> go d more

    -- @${ ... }@, read to its closing brace.
    braced sp st = go (1 :: Int) Nothing
      where
        go _ _ [] = Done st (Left (Bad sp "unterminated parameter expansion"))
        go d q (a : more) = case (q, a) of
          (_, AHole sp2 True) -> Done st (Left (Stop sp2))
          (Just '\'', AChar _ '\'') -> go d Nothing more
          (Just '"', AChar _ '"') -> go d Nothing more
          (Just '"', AChar _ '\\') -> go d q (drop 1 more)
          (Just _, _) -> go d q more
          (Nothing, AChar _ '\\') -> go d q (drop 1 more)
          (Nothing, AChar _ ch)
            | ch == '\'' || ch == '"' -> go d (Just ch) more
            | ch == '{' -> go (d + 1) q more
            | ch == '}' -> if d == 1 then Next st more else go (d - 1) q more
          _ -> go d q more

    -- @$( ... )@: its inside is a script of its own.
    substitution sp inDq st rest =
      let (r, toks, nested) = lexScript True rest
          st' = expanded sp inDq False st
       in case r of
            Right more -> Next (nest (finish False toks nested) st') more
            Left (Stop s) -> Done (nest (finish True toks nested) st') (Left (Stop s))
            Left (Unclosed _) -> Done st (Left (Bad sp "unterminated command substitution"))
            Left bad -> Done st (Left bad)

    -- A backquoted substitution, read to the next unescaped backquote.
    -- Inside it a backslash escapes only @\\@, @`@ and @$@, and in
    -- double quotes @"@ as well.
    backquoted sp inDq st rest = collect [] rest
      where
        collect acc as = case as of
          [] -> Done st (Left (Bad sp "unterminated command substitution"))
          AChar _ '`' : more -> inside (reverse acc) more
          AChar _ '\\' : a@(AChar _ ch) : more
            | ch `elem` ("\\`$" :: String) || (inDq && ch == '"') -> collect (a : acc) more
          AHole sp2 True : _ -> Done st (Left (Stop sp2))
          a : more -> collect (a : acc) more
        inside body more =
          let (r, toks, nested) = lexScript False body
              st' = expanded sp inDq False st
           in case r of
                Right _ -> Next (nest (finish False toks nested) st') more
                Left (Stop s) -> Done (nest (finish True toks nested) st') (Left (Stop s))
                Left (Unclosed why) -> Done st (Left (Bad sp why))
                Left bad -> Done st (Left bad)

    finish cut toks nested =
      let (problem, cmds) = parse cut toks
       in ShellReading
            (listToMaybe (sortOn (spanKey . fst) [p | Just p <- problem : map srProblem nested]))
            (cmds <> concatMap srCommands nested)

    nest r st = st {lsNested = r : lsNested st}

    dyn w = w {wbOpen = False, wbPlain = False}
    unplain w = w {wbPlain = False}
    escaped sp = extend sp unplain

data Step = Next LS [Atom] | Done LS (Either End [Atom])

emit :: Tok -> LS -> LS
emit t st = st {lsToks = t : lsToks st}

flush :: LS -> LS
flush st = case lsWord st of
  Nothing -> st
  Just w ->
    emit
      (TWord (ShellWord (wbSpan w) (wbPrefix w) (wbOpen w) (wbPlain w) (wbSingle w)))
      st {lsWord = Nothing}

-- | Apply a change to the word being built, starting one if none is.
extend :: Span -> (WB -> WB) -> LS -> LS
extend sp f st = st {lsWord = Just (f w {wbSpan = wbSpan w <> sp})}
  where
    w = maybe (WB sp "" True True True) id (lsWord st)

-- | A literal character of the word.
lit :: Span -> Char -> LS -> LS
lit sp c = extend sp (\w -> if wbOpen w then w {wbPrefix = wbPrefix w <> T.singleton c} else w)

-- Parsing -------------------------------------------------------------------

-- | Where the parser is between commands.
data Mode
  = -- | At the start of a list, or after @;@, @&@ or a newline.
    AtStart
  | -- | After @&&@, @||@ or @|@: a command must follow.
    Needs Span Text
  | -- | After a command.
    AfterCommand
  | -- | After a group's @)@: only redirections may follow.
    AfterGroup

-- | A command being read.
data Cmd = Cmd
  { cmdName :: Maybe ShellWord,
    cmdArgs :: [ShellWord],
    -- | A redirection whose target is still to come.
    cmdRedirect :: Maybe (Span, Text)
  }

-- | Read the tokens of a script into its simple commands, or the
-- first syntax error among them. With @cut@ the tokens end where
-- reading stopped, so their end is not the end of the script.
parse :: Bool -> [Tok] -> (Maybe (Span, Text), [SimpleCommand])
parse cut = go AtStart [] Nothing []
  where
    -- groups: for each open group, its @(@ and whether it has a command
    go :: Mode -> [(Span, Bool)] -> Maybe Cmd -> [SimpleCommand] -> [Tok] -> (Maybe (Span, Text), [SimpleCommand])
    go mode groups cur acc toks = case (cur, toks) of
      (Just c, []) -> endCommand c True acc $ \acc' -> go AfterCommand groups Nothing acc' []
      (Nothing, []) -> atEnd mode groups acc
      (Just c, TWord w : rest) -> case cmdRedirect c of
        Just _ -> go mode groups (Just c {cmdRedirect = Nothing}) acc rest
        Nothing -> case cmdName c of
          Just _ -> go mode groups (Just c {cmdArgs = cmdArgs c <> [w]}) acc rest
          Nothing
            | assignment w -> go mode groups cur acc rest
            | reserved w -> stopped acc
            | otherwise -> go mode groups (Just c {cmdName = Just w}) acc rest
      (Just c, TRedir sp t : rest) -> case cmdRedirect c of
        Just r -> missingTarget r acc
        Nothing -> go mode groups (Just c {cmdRedirect = Just (sp, t)}) acc rest
      (Just c, TOp sp t : rest)
        | t == "(" -> stopped (finishCut c acc)
        | otherwise -> case cmdRedirect c of
            Just r -> missingTarget r acc
            Nothing ->
              endCommand c False acc $ \acc' ->
                between AfterCommand (markCommand groups) acc' (TOp sp t : rest)
      (Nothing, ts) -> between mode groups acc ts

    -- Between commands.
    between mode groups acc toks = case toks of
      [] -> atEnd mode groups acc
      TWord _ : _ -> case mode of
        AfterGroup -> stopped acc
        _ -> go mode (markCommand groups) (Just (Cmd Nothing [] Nothing)) acc toks
      TRedir _ _ : _ -> go mode (markCommand groups) (Just (Cmd Nothing [] Nothing)) acc toks
      TOp sp t : rest -> case t of
        "\n" -> case mode of
          Needs {} -> go mode groups Nothing acc rest
          _ -> go AtStart groups Nothing acc rest
        "(" -> case mode of
          AtStart -> go AtStart ((sp, False) : markCommand groups) Nothing acc rest
          Needs {} -> go AtStart ((sp, False) : markCommand groups) Nothing acc rest
          _ -> stopped acc
        ")" -> case groups of
          [] -> problem sp "')' has no '(' to close" acc
          (open, hasCommand) : outer -> case mode of
            Needs s o -> notFollowed s o acc
            _
              | not hasCommand -> problem (open <> sp) "a group must contain a command" acc
              | otherwise -> go AfterGroup outer Nothing acc rest
        ";;" -> problem sp "';;' ends a case clause, and there is no case clause here" acc
        _ -> case mode of
          AtStart -> problem sp ("'" <> t <> "' has no command before it") acc
          Needs s o -> notFollowed s o acc
          _
            | t `elem` ["&&", "||", "|"] -> go (Needs sp t) groups Nothing acc rest
            | otherwise -> go AtStart groups Nothing acc rest

    atEnd mode groups acc
      | cut = (Nothing, reverse acc)
      | Needs s o <- mode = notFollowed s o acc
      | (open, _) : _ <- groups = problem open "unterminated group" acc
      | otherwise = (Nothing, reverse acc)

    endCommand c atInputEnd acc k = case cmdRedirect c of
      Just r | not (atInputEnd && cut) -> missingTarget r acc
      _ -> case cmdName c of
        Nothing -> k acc
        Just n -> k (SimpleCommand n (cmdArgs c) (atInputEnd && cut) : acc)

    finishCut c acc = case cmdName c of
      Nothing -> acc
      Just n -> SimpleCommand n (cmdArgs c) True : acc

    stopped acc = (Nothing, reverse acc)
    problem sp why acc = (Just (sp, why), reverse acc)
    notFollowed sp o = problem sp ("'" <> o <> "' is not followed by a command")
    missingTarget (sp, t) = problem sp ("the redirection '" <> t <> "' has no target")

    markCommand ((open, _) : outer) = (open, True) : outer
    markCommand [] = []

    -- 10.9's assignment word, read from the literal start of the word.
    assignment w =
      let (name, rest) = T.breakOn "=" (swPrefix w)
       in not (T.null rest)
            && not (T.null name)
            && T.all (\ch -> ch `elem` ['a' .. 'z'] || ch `elem` ['A' .. 'Z'] || ch == '_' || ch `elem` ['0' .. '9']) name
            && not (T.head name `elem` ['0' .. '9'])

    reserved w = swLiteral w && swPrefix w `elem` reservedWords

-- | Words that open a compound command or change how the rest is read
-- in some shell. Reading stops at any of them in command position.
reservedWords :: [Text]
reservedWords =
  [ "if", "then", "else", "elif", "fi", "do", "done", "case", "esac", "while", "until",
    "for", "{", "}", "!", "[[", "]]", "function", "select", "time", "coproc"
  ]
