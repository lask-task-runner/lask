{-# LANGUAGE OverloadedStrings #-}

-- | Command schemas (spec ch. 5, "Command schemas"): what a command
-- declaration says about the options and subcommands of a program, and
-- the check of a command string's simple commands against it.
--
-- A schema is an ordinary value of the built-in type @CommandSchema@,
-- written with literals alone so that it is known without evaluation.
-- The elaborator hands it here as a 'Static' tree, which 'fromStatic'
-- validates; 'checkCommand' is then pure.
module Language.Lask.CommandSchema
  ( Static (..),
    StaticF (..),
    Schema (..),
    Spec (..),
    Style (..),
    Opt (..),
    ValueKind (..),
    fromStatic,
    Finding (..),
    checkCommand,
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Span (..))
import Language.Lask.Suggest (suggest)
import Language.Lask.Syntax.Shell (ShellWord (..), SimpleCommand (..))

-- | A value known without evaluation, with where each part is written.
data Static = Static Span StaticF
  deriving (Show, Eq)

data StaticF
  = VNull
  | VBool Bool
  | VNumber Double
  | VString Text
  | VArray [Static]
  | VObject [(Text, Static)]
  deriving (Show, Eq)

data Schema = Schema
  { schRoot :: Spec,
    -- | By path: one or more words separated by single spaces.
    schSubcommands :: Map Text Spec
  }
  deriving (Show, Eq)

data Spec = Spec
  { spClosed :: Bool,
    -- | 'Nothing': the style of the parent, @gnu@ at the root.
    spStyle :: Maybe Style,
    spOptions :: [Opt],
    -- | 'Nothing': true in the @gnu@ style, false in @single-dash@.
    spIntermixed :: Maybe Bool,
    spStopAtFirstPositional :: Bool
  }
  deriving (Show, Eq)

data Style = Gnu | SingleDash
  deriving (Show, Eq)

data Opt = Opt
  { optNames :: [Text],
    optValue :: ValueKind,
    optValues :: Maybe [Text],
    optGlobal :: Bool
  }
  deriving (Show, Eq)

data ValueKind = ValueNone | ValueRequired | ValueOptional
  deriving (Show, Eq)

-- Validation -------------------------------------------------------------------

-- | Read and validate one @CommandSchema@ value. Its type has been
-- checked; what is left is what the type cannot say (spec ch. 5).
fromStatic :: Static -> Either (Span, Text) Schema
fromStatic (Static sp f) = do
  fields <- object sp f
  root <- maybe (Left (sp, "a command schema must give 'root'")) spec (lookup "root" fields)
  subs <- case lookup "subcommands" fields of
    Nothing -> pure []
    Just (Static _ VNull) -> pure []
    Just (Static ssp sf) -> do
      entries <- object ssp sf
      mapM (\(k, v) -> (,) k <$> subcommand v k) entries
  let table = Map.fromList [(k, s) | (k, (_, s)) <- subs]
  mapM_ (parentExists table) subs
  let schema = Schema root table
  mapM_ (checkDuplicates schema) ("" : Map.keys table)
  pure schema
  where
    subcommand v@(Static vsp _) k = do
      unless' (validPath k) (vsp, "the subcommand path '" <> k <> "' must be words separated by single spaces, none of them beginning with '-'")
      s <- spec v
      pure (vsp, s)

    parentExists table (k, (vsp, _)) = case T.breakOnEnd " " k of
      ("", _) -> pure ()
      (parentSp, _) ->
        let parent = T.dropEnd 1 parentSp
         in unless' (Map.member parent table) (vsp, "the subcommand '" <> k <> "' needs its parent '" <> parent <> "' in 'subcommands'")

    validPath k =
      let ws = T.splitOn " " k
       in not (null ws) && all validWord ws
    validWord w =
      not (T.null w)
        && not ("-" `T.isPrefixOf` w)
        && T.all (\c -> c `notElem` (" \t\n'\"\\" :: String)) w

    spec (Static ssp sf) = do
      fs <- object ssp sf
      closed <- optBool fs "closed"
      style <- traverse styleOf (present fs "style")
      opts <- case present fs "options" of
        Nothing -> pure []
        Just v -> mapM option =<< array v
      intermixed <- traverse bool (present fs "intermixed")
      stopAt <- optBool fs "stop_at_first_positional"
      pure (Spec closed style opts intermixed stopAt)

    styleOf v@(Static vsp _) = do
      t <- string v
      case t of
        "gnu" -> pure Gnu
        "single-dash" -> pure SingleDash
        _ -> Left (vsp, "'style' must be \"gnu\" or \"single-dash\", not \"" <> t <> "\"")

    option (Static osp of') = do
      fs <- object osp of'
      names <- maybe (Left (osp, "an option must give 'names'")) array (lookup "names" fs) >>= mapM nameOf
      unless' (not (null names)) (osp, "an option must have at least one name")
      kind <- case present fs "value" of
        Nothing -> pure ValueNone
        Just v@(Static vsp _) -> do
          t <- string v
          case t of
            "none" -> pure ValueNone
            "required" -> pure ValueRequired
            "optional" -> pure ValueOptional
            _ -> Left (vsp, "'value' must be \"none\", \"required\" or \"optional\", not \"" <> t <> "\"")
      values <- case present fs "values" of
        Nothing -> pure Nothing
        Just v@(Static vsp _) -> do
          unless' (kind /= ValueNone) (vsp, "'values' is given for an option that takes no value")
          vs <- array v >>= mapM string
          unless' (not (null vs)) (vsp, "'values' must list at least one value")
          pure (Just vs)
      global <- optBool fs "global"
      pure (Opt names kind values global)

    nameOf v@(Static vsp _) = do
      n <- string v
      unless' (validName n) (vsp, "'" <> n <> "' is not an option name: it must begin with '-', not be '-' or '--', and contain no whitespace, '=' or quotation character")
      pure n
    validName n =
      "-" `T.isPrefixOf` n
        && n /= "-"
        && n /= "--"
        && T.all (\c -> c `notElem` (" \t\n='\"\\" :: String)) n

    -- Each spec, with the global options of its ancestors, names each
    -- option once.
    checkDuplicates schema path =
      let opts = optionsAt schema path
          names = concatMap optNames opts
          dup = [n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names]
       in case dup of
            n : _ -> Left (sp, "the option '" <> n <> "' is given twice" <> at path)
            [] -> pure ()
    at "" = ""
    at path = " for the subcommand '" <> path <> "'"

    present fs k = case lookup k fs of
      Just (Static _ VNull) -> Nothing
      other -> other
    optBool fs k = maybe (pure False) bool (present fs k)

    object _ (VObject kvs) = pure kvs
    object osp _ = Left (osp, "expected a record")
    array (Static _ (VArray xs)) = pure xs
    array (Static asp _) = Left (asp, "expected an array")
    string (Static _ (VString t)) = pure t
    string (Static ssp _) = Left (ssp, "expected a string")
    bool (Static _ (VBool b)) = pure b
    bool (Static bsp _) = Left (bsp, "expected a Bool")

    unless' ok e = if ok then pure () else Left e

-- | The options that apply in a spec: its own and the global options
-- of its ancestors.
optionsAt :: Schema -> Text -> [Opt]
optionsAt schema path = spOptions (specAt schema path) <> concatMap globals (ancestors path)
  where
    globals p = filter optGlobal (spOptions (specAt schema p))

ancestors :: Text -> [Text]
ancestors "" = []
ancestors path = case T.breakOnEnd " " path of
  ("", _) -> [""]
  (parentSp, _) -> let parent = T.dropEnd 1 parentSp in parent : ancestors parent

specAt :: Schema -> Text -> Spec
specAt schema "" = schRoot schema
specAt schema path = fromMaybe (schRoot schema) (Map.lookup path (schSubcommands schema))

styleAt :: Schema -> Text -> Style
styleAt schema path = case spStyle (specAt schema path) of
  Just s -> s
  Nothing -> case ancestors path of
    parent : _ -> styleAt schema parent
    [] -> Gnu

-- Checking -----------------------------------------------------------------------

-- | Something wrong with one invocation. A finding that is not sure is
-- an unknown option or subcommand of a spec that is not closed, which
-- the program may still accept.
data Finding = Finding
  { fdSpan :: Span,
    fdSure :: Bool,
    fdMessage :: Text
  }
  deriving (Show, Eq)

data St = St
  { stPath :: Text,
    -- | A positional argument has been read at this level.
    stPositional :: Bool,
    -- | The option before awaits its value.
    stAwaiting :: Maybe (Opt, Text, Span)
  }

-- | Check one simple command, whose command word is @word@, against
-- its schema. Checking stops at the first word it cannot read (spec
-- ch. 5), and at the first finding.
checkCommand :: Schema -> Text -> SimpleCommand -> [Finding]
checkCommand schema word cmd = go (St "" False Nothing) (scArgs cmd)
  where
    go st [] = case stAwaiting st of
      Just (o, n, sp)
        | not (scCut cmd) && optValue o == ValueRequired ->
            [Finding sp True ("the option '" <> n <> "' of '" <> shown (stPath st) <> "' requires a value")]
      _ -> []
    go st (w : rest)
      | Just (o, n, _) <- stAwaiting st =
          if not (swSingle w)
            then []
            else case valueProblem (stPath st) o n w of
              Just f -> [f]
              Nothing -> go st {stAwaiting = Nothing} rest
      | not (swSingle w) = []
      | swLiteral w = literal st (swPrefix w) w rest
      | "-" `T.isPrefixOf` swPrefix w = partial st w
      | otherwise = []

    literal st t w rest
      | t == "--" = []
      | t == "-" || not ("-" `T.isPrefixOf` t) = positional st t w rest
      | otherwise = option st t w rest

    -- A word with a dynamic part: its option name is known only when
    -- the literal start runs past it to an '='.
    partial st w = case T.breakOn "=" (normalised st (swPrefix w)) of
      (name, eq)
        | not (T.null eq) && ("--" `T.isPrefixOf` name || styleAt schema (stPath st) == SingleDash) ->
            case lookupOpt st name of
              Nothing -> [unknownOption st name w]
              Just o
                | optValue o == ValueNone -> [Finding (swSpan w) True ("the option '" <> name <> "' of '" <> shown (stPath st) <> "' takes no value")]
                | otherwise -> []
      _ -> []

    positional st t w rest =
      let children = [k | k <- Map.keys (schSubcommands schema), parentOf k == stPath st]
          path' = if T.null (stPath st) then t else stPath st <> " " <> t
          spec = specAt schema (stPath st)
       in if not (stPositional st) && not (null children)
            then case Map.lookup path' (schSubcommands schema) of
              Just _ -> go (St path' False Nothing) rest
              Nothing ->
                [ Finding
                    (swSpan w)
                    (spClosed spec)
                    ( "'" <> shown (stPath st) <> "' has no subcommand '" <> t <> "'"
                        <> didYouMean t [T.takeWhileEnd (/= ' ') k | k <- children]
                    )
                ]
            else
              if spStopAtFirstPositional spec || not (intermixed st)
                then []
                else go st {stPositional = True} rest

    option st t w rest = case styleAt schema (stPath st) of
      Gnu
        | "--" `T.isPrefixOf` t -> long st t w rest
        | otherwise -> cluster st (T.drop 1 t) w rest
      SingleDash -> long st (normalised st t) w rest

    -- @--name@, @--name=value@, or in the single-dash style @-name@.
    long st t w rest =
      let (name, eq) = T.breakOn "=" t
       in case lookupOpt st name of
            Nothing -> [unknownOption st name w]
            Just o
              | not (T.null eq) ->
                  if optValue o == ValueNone
                    then [Finding (swSpan w) True ("the option '" <> name <> "' of '" <> shown (stPath st) <> "' takes no value")]
                    else case valueProblem (stPath st) o name w {swPrefix = T.drop 1 eq} of
                      Just f -> [f]
                      Nothing -> go st rest
              | optValue o == ValueRequired -> go st {stAwaiting = Just (o, name, swSpan w)} rest
              | otherwise -> go st rest

    -- @-abc@: single-letter options, the first that takes a value
    -- taking the rest of the word.
    cluster st cs w rest = case T.uncons cs of
      Nothing -> go st rest
      Just (c, more) ->
        let name = T.pack ['-', c]
         in case lookupOpt st name of
              Nothing -> [unknownOption st name w]
              Just o -> case optValue o of
                ValueNone -> cluster st more w rest
                kind
                  | not (T.null more) -> case valueProblem (stPath st) o name w {swPrefix = more} of
                      Just f -> [f]
                      Nothing -> go st rest
                  | kind == ValueRequired -> go st {stAwaiting = Just (o, name, swSpan w)} rest
                  | otherwise -> go st rest

    -- In the single-dash style a name may also be written with two
    -- dashes, as Go's flag package reads it.
    normalised st t
      | styleAt schema (stPath st) == SingleDash, "--" `T.isPrefixOf` t = T.drop 1 t
      | otherwise = t

    lookupOpt st name = find (elem name . optNames) (optionsAt schema (stPath st))

    intermixed st = fromMaybe (styleAt schema (stPath st) == Gnu) (spIntermixed (specAt schema (stPath st)))

    unknownOption st name w =
      Finding
        (swSpan w)
        (spClosed (specAt schema (stPath st)))
        ( "'" <> shown (stPath st) <> "' has no option '" <> name <> "'"
            <> didYouMean name (concatMap optNames (optionsAt schema (stPath st)))
        )

    valueProblem path o name w = case optValues o of
      Just vs
        | swLiteral w,
          swPrefix w `notElem` vs ->
            Just
              ( Finding
                  (swSpan w)
                  True
                  ( "the option '" <> name <> "' of '" <> shown path <> "' takes one of "
                      <> T.intercalate ", " ["'" <> v <> "'" | v <- vs]
                      <> ", not '" <> swPrefix w <> "'"
                  )
              )
      _ -> Nothing

    shown "" = word
    shown path = word <> " " <> path

    parentOf k = case T.breakOnEnd " " k of
      ("", _) -> ""
      (p, _) -> T.dropEnd 1 p

didYouMean :: Text -> [Text] -> Text
didYouMean n pool = case suggest n pool of
  [] -> ""
  ss -> "; did you mean " <> T.intercalate " or " ["'" <> s <> "'" | s <- ss] <> "?"
