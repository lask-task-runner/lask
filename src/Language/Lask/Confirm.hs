{-# LANGUAGE OverloadedStrings #-}

-- | Confirmation before a dangerous task runs (spec chapter 5, 11.2).
--
-- The project file names the functions that ask for a typed
-- confirmation, optionally only for some argument values. This module
-- checks those names against the compiled program, so that a
-- protection never silently stops applying, and decides, for the
-- function the CLI is about to call, whether and what to ask.
--
-- It is operational-mistake prevention, not a security boundary: what
-- must not happen at all is enforced by the target's own permissions.
module Language.Lask.Confirm
  ( Prompt (..),
    validateConfirm,
    confirmationFor,
    describeRule,
    ruleFor,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Core.AST
import Language.Lask.Deps.File (ConfirmRule (..), DepsFile (..), defaultDepsFileName)
import Language.Lask.Diagnostic (Diagnostic, mkDiagnostic)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..), Key, StaticParams (..))
import Language.Lask.ErrorCode (ErrorCode (EModuleConfirmTarget), Stage (StageStatic))
import Language.Lask.Module.Loader (Program (..))
import Language.Lask.Module.Resolve (GlobalScope, entryPublicValues)
import Language.Lask.Runtime.Eval (castValueEither, renderCastMismatch)
import Language.Lask.Runtime.Value (Value (..))
import Language.Lask.Serialize (valueFromJson, valueToJson)
import Language.Lask.Span (Position (..), Span (..))
import Language.Lask.Types (Type (..), applySubst)
import System.FilePath ((</>))

-- | What to ask before a run.
data Prompt = Prompt
  { promptFunction :: Text,
    -- | The arguments that matched @when@, as @name=value@ text.
    promptMatched :: [Text],
    -- | What has to be typed.
    promptPhrase :: Text
  }
  deriving (Show, Eq)

-- | Every @confirm@ entry of the root project file, checked against
-- the program: the function exists and is public, the parameters
-- @when@ names exist, its values fit their types, and the parameters
-- @phrase@ interpolates exist. Every entry is checked, whether or not
-- anything calls the function.
validateConfirm :: Program -> Map FilePath GlobalScope -> CoreProgram -> [Diagnostic]
validateConfirm prog scopes core = case progProject prog of
  Nothing -> []
  Just df -> concatMap check (Map.toList (depsConfirm df))
  where
    public = entryPublicValues prog scopes
    check (name, rule) = case lookup name public >>= \k -> Map.lookup k (cpDecls core) of
      Nothing ->
        [ diag rule $
            "confirm '"
              <> name
              <> "': no public function of that name in "
              <> T.pack (progEntry prog)
              <> nearest name (map fst public)
        ]
      Just cd -> case cdParams cd of
        Nothing
          | null (crWhen rule) && maybe True (null . interpolated) (crPhrase rule) -> []
          | otherwise -> [diag rule ("confirm '" <> name <> "': '" <> name <> "' takes no named parameters to refer to")]
        Just params ->
          let named = paramTypes cd params
           in concatMap (checkCondition name named) (crWhen rule)
                <> [ diag rule ("confirm '" <> name <> "': 'phrase' interpolates '" <> p <> "', which is not a parameter of '" <> name <> "'")
                   | Just t <- [crPhrase rule],
                     p <- interpolated t,
                     p `notElem` map fst named
                   ]
      where
        checkCondition fn named (param, values) = case lookup param named of
          Nothing ->
            [ diag rule $
                "confirm '"
                  <> fn
                  <> "': 'when' names '"
                  <> param
                  <> "', which is not a parameter of '"
                  <> fn
                  <> "'"
                  <> nearest param (map fst named)
            ]
          Just ty ->
            [ diag rule $
                "confirm '"
                  <> fn
                  <> "': 'when' value "
                  <> jsonText v
                  <> " does not fit the type of '"
                  <> param
                  <> "'"
                  <> renderCastMismatch cm
            | v <- values,
              Left cm <- [castValueEither ty (valueFromJson v)]
            ]

    diag rule = mkDiagnostic EModuleConfirmTarget StageStatic (spanOf rule)
    spanOf rule = case crAt rule of
      Just (l, c) ->
        let file = progBaseDir prog </> defaultDepsFileName
         in Span (Position file l c) (Position file l c)
      Nothing -> NoSpan

-- | The rule that applies to a declaration: the project file names
-- functions as the entry module publishes them, and a rule belongs to
-- the declaration the name resolves to, so a re-export or an alias is
-- the same function.
ruleFor :: Program -> Map FilePath GlobalScope -> Key -> Maybe (Text, ConfirmRule)
ruleFor prog scopes key = do
  df <- progProject prog
  let public = entryPublicValues prog scopes
  find (\(name, _) -> lookup name public == Just key) (Map.toList (depsConfirm df))

-- | Whether a call of @fn@ with these CLI arguments asks for
-- confirmation, and what. A keyword argument the command line leaves
-- out takes its default when that is a literal. Any other default is
-- only known once the run has started, so a condition on it is taken
-- to hold: a check that cannot tell asks.
confirmationFor :: Text -> ConfirmRule -> CoreDecl -> [Value] -> [(Text, Value)] -> Maybe Prompt
confirmationFor fn rule cd posVals kwVals
  | null (crWhen rule) = Just (Prompt fn [] (phraseWith Nothing))
  | null matched = Nothing
  | otherwise = Just (Prompt fn (map shown matched) (phraseWith (firstKnown matched)))
  where
    args = boundArguments cd posVals kwVals
    matched =
      [ (param, known)
      | (param, values) <- crWhen rule,
        let known = Map.findWithDefault Nothing param args,
        maybe True (\v -> valueToJson v `elem` values) known
      ]
    shown (param, known) = param <> "=" <> maybe "<default>" valueText known
    firstKnown ms = case [v | (_, Just v) <- ms] of
      (v : _) -> Just (valueText v)
      [] -> Nothing
    phraseWith matchedValue = case crPhrase rule of
      Just t -> maybe fn id (interpolate args t)
      Nothing -> maybe fn id matchedValue

-- | The requirement, for help and hovers: @requires confirmation@, or
-- @requires confirmation when env is prod or production@.
describeRule :: ConfirmRule -> Text
describeRule rule = case crWhen rule of
  [] -> "requires confirmation"
  conds ->
    "requires confirmation when "
      <> T.intercalate ", or " [p <> " is " <> T.intercalate " or " (map jsonText vs) | (p, vs) <- conds]

-- helpers ------------------------------------------------------------------

-- | Every parameter a condition may name, with the type the CLI binds
-- it at: a type parameter stands for any type, as the CLI decodes it
-- (spec 11.2).
paramTypes :: CoreDecl -> StaticParams -> [(Text, Type)]
paramTypes cd params =
  [(n, atAny t) | (n, t) <- spPositional params <> spKeywords params]
  where
    atAny = applySubst (Map.fromList [(v, TyAny) | v <- cdTypeVars cd])

-- | The arguments of a call as the CLI binds them: each parameter's
-- value, or 'Nothing' when it is a default that is not a literal.
boundArguments :: CoreDecl -> [Value] -> [(Text, Value)] -> Map Text (Maybe Value)
boundArguments cd posVals kwVals =
  Map.fromList $
    zip positionalNames (map Just posVals)
      <> [(n, Just v) | (n, v) <- kwVals]
      <> [ (n, literal d)
         | (n, d) <- defaults,
           n `notElem` map fst kwVals
         ]
  where
    positionalNames = maybe [] (map fst . spPositional) (cdParams cd)
    defaults = case coreF (cdCore cd) of
      CLam lam -> lamKeywords lam
      _ -> []
    literal d = case coreF d of
      CStrLit t -> Just (VString t)
      CNumber n -> Just (VNumber n)
      CBool b -> Just (VBool b)
      CNull -> Just VNull
      _ -> Nothing

-- | The parameters @#{name}@ refers to in a phrase.
--
-- >>> interpolated (T.pack "reset #{db} on #{host}")
-- ["db","host"]
interpolated :: Text -> [Text]
interpolated t = case T.breakOn "#{" t of
  (_, rest)
    | T.null rest -> []
    | otherwise ->
        let (name, after) = T.breakOn "}" (T.drop 2 rest)
         in if T.null after then [] else T.strip name : interpolated (T.drop 1 after)

-- | A phrase with its parameters filled in; 'Nothing' when one of them
-- is not known before the run.
interpolate :: Map Text (Maybe Value) -> Text -> Maybe Text
interpolate args t = case T.breakOn "#{" t of
  (before, rest)
    | T.null rest -> Just before
    | otherwise -> do
        let (name, after) = T.breakOn "}" (T.drop 2 rest)
        v <- Map.findWithDefault Nothing (T.strip name) args
        tailText <- interpolate args (T.drop 1 after)
        Just (before <> valueText v <> tailText)

valueText :: Value -> Text
valueText v = case v of
  VString s -> s
  other -> jsonText (valueToJson other)

jsonText :: A.Value -> Text
jsonText v = case v of
  A.String s -> s
  other -> TE.decodeUtf8 (BL.toStrict (A.encode other))

nearest :: Text -> [Text] -> Text
nearest wanted candidates = case filter close candidates of
  (c : _) -> "; did you mean '" <> c <> "'?"
  [] -> ""
  where
    close c = c /= wanted && (T.take 3 c == T.take 3 wanted || wanted `T.isInfixOf` c || c `T.isInfixOf` wanted)
