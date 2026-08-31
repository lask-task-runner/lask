{-# LANGUAGE OverloadedStrings #-}

-- | Function help (spec 11.6): the model behind @lask run \<fn\>
-- --help@ and its text and JSON renderings.
--
-- The information comes from three places, as spec 11.6 prescribes:
-- names, parameter kinds, secrecy and default values from the surface
-- syntax; types from static verification; prose from the
-- documentation comment (spec 3.1). Nothing here evaluates the
-- module — default values are shown as the source text that produced
-- them.
module Command.Lask.Help
  ( FunctionHelp (..),
    ParamHelp (..),
    ParamKind (..),
    buildFunctionHelp,
    renderHelpText,
    renderHelpJson,
    renderListText,
    renderListJson,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import Command.Lask.Envs (EnvRef (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Doc (DocComment (..))
import Language.Lask.Elaborate (CoreDecl (..), StaticParams (..))
import Language.Lask.Span (Position (..), Span (..), spanText)
import qualified Language.Lask.Syntax.AST as AST
import Language.Lask.Types (Type (..), renderType)

data ParamKind = ParamPositional | ParamVariadic | ParamKeyword
  deriving (Show, Eq)

data ParamHelp = ParamHelp
  { phName :: Text,
    phKind :: ParamKind,
    -- | 'Nothing' when the module did not type check far enough to
    -- determine it; rendered as @?@.
    phType :: Maybe Text,
    phDefault :: Maybe Text,
    phSecret :: Bool,
    phDoc :: Maybe Text
  }
  deriving (Show, Eq)

data FunctionHelp = FunctionHelp
  { fhName :: Text,
    fhModule :: FilePath,
    fhLine :: Maybe Int,
    fhSummary :: Maybe Text,
    fhDescription :: Maybe Text,
    fhParams :: [ParamHelp],
    fhReturn :: Maybe Text,
    fhReturnDoc :: Maybe Text,
    fhEnvs :: [EnvRef],
    fhExamples :: [Text],
    fhHidden :: Bool,
    -- | Whether the declaration is a function. Plain value bindings
    -- are callable with no arguments (spec 11.2) and have help of
    -- their own, but they are not tasks and are left out of listings.
    fhFunction :: Bool
  }
  deriving (Show, Eq)

-- Building --------------------------------------------------------------------

-- | Assemble the help of one declaration. @src@ is the text of the
-- module the declaration was written in, used to show default values
-- verbatim.
buildFunctionHelp ::
  FilePath ->
  Text ->
  AST.Decl ->
  Maybe CoreDecl ->
  DocComment ->
  [EnvRef] ->
  FunctionHelp
buildFunctionHelp path src decl mCore doc envs =
  FunctionHelp
    { fhName = declName decl,
      fhModule = path,
      fhLine = declLine decl,
      fhSummary = docSummary doc,
      fhDescription = docDescription doc,
      fhParams = map paramHelp (declParams decl),
      fhReturn = returnType,
      fhReturnDoc = docReturn doc,
      fhEnvs = envs,
      fhExamples = docExamples doc,
      fhHidden = docHidden doc,
      fhFunction = isFunctionDecl decl || isFunctionType
    }
  where
    params = cdParams =<< mCore

    isFunctionType = case cdType <$> mCore of
      Just (TyFun _ _) -> True
      _ -> False

    returnType = case cdType <$> mCore of
      Just (TyFun _ r) -> Just (renderType r)
      -- A plain value binding is callable with no arguments
      -- (spec 11.2), so its own type is what the call returns.
      Just t | null (declParams decl) -> Just (renderType t)
      _ -> Nothing

    paramHelp p = case AST.paramF p of
      AST.PPositional n sec _ ->
        ParamHelp n ParamPositional (typeOf spPositional n) Nothing (isSecret sec) (docOf n)
      AST.PVariadic n _ ->
        ParamHelp n ParamVariadic variadicType Nothing False (docOf n)
      AST.PKeyword n sec _ dflt ->
        ParamHelp n ParamKeyword (typeOf spKeywords n) (defaultOf sec dflt) (isSecret sec) (docOf n)

    typeOf field n = do
      sp <- params
      renderType <$> lookup n (field sp)

    variadicType = do
      sp <- params
      (_, elemTy) <- spVariadic sp
      pure (renderType (TyArray elemTy))

    -- Shown as written, never evaluated (spec 11.6); a secret binding
    -- never reveals its value (spec 12.8).
    defaultOf sec dflt
      | isSecret sec = Just "<secret>"
      | otherwise = case T.strip (spanText src (AST.exprSpan dflt)) of
          "" -> Nothing
          t -> Just t

    isSecret AST.Secret = True
    isSecret AST.Public = False

    docOf n = lookup n (docParams doc)

declName :: AST.Decl -> Text
declName d = case AST.declF d of
  AST.DValue n _ _ _ -> n
  AST.DFunction n _ _ _ -> n
  _ -> ""

declLine :: AST.Decl -> Maybe Int
declLine d = case AST.declSpan d of
  Span (Position _ l _) _ -> Just l
  NoSpan -> Nothing

-- | A declaration written as a function, or bound directly to a
-- lambda.
isFunctionDecl :: AST.Decl -> Bool
isFunctionDecl d = case AST.declF d of
  AST.DFunction {} -> True
  AST.DValue _ _ _ (AST.Expr _ AST.ELambda {}) -> True
  _ -> False

-- | The parameters a declaration exposes to the CLI: those of a
-- function declaration, or of a directly lambda-valued binding.
declParams :: AST.Decl -> [AST.Param]
declParams d = case AST.declF d of
  AST.DFunction _ ps _ _ -> ps
  AST.DValue _ _ _ (AST.Expr _ (AST.ELambda ps _ _)) -> ps
  _ -> []

-- Text rendering ---------------------------------------------------------------

-- | The text form of spec 11.6. Sections with no content are omitted.
renderHelpText :: Text -> FunctionHelp -> Text
renderHelpText subcommand fh =
  T.intercalate "\n\n" (filter (not . T.null) sections) <> "\n"
  where
    sections =
      [ title,
        section "Usage:" [usageLine],
        fromMaybe "" (fhDescription fh),
        paramsSection,
        returnsSection,
        envsSection,
        examplesSection,
        definedAt
      ]

    title = case fhSummary fh of
      Just s -> fhName fh <> " - " <> s
      Nothing -> fhName fh

    usageLine =
      T.unwords $
        ["lask", subcommand, fhName fh] <> map usageParam (fhParams fh)

    usageParam p = case phKind p of
      ParamPositional -> "<" <> phName p <> ">"
      ParamVariadic -> "[<" <> phName p <> "> ...]"
      ParamKeyword -> "[--" <> phName p <> " <" <> typeText p <> ">]"

    typeText p = fromMaybe "?" (phType p)

    paramsSection
      | null (fhParams fh) = ""
      | otherwise = section "Parameters:" (concatMap paramLines (fhParams fh))

    paramLines p = paramSignature p : maybe [] (map indentDoc . T.lines) (phDoc p)

    paramSignature p =
      prefix <> phName p <> " : " <> typeText p <> maybe "" (" = " <>) (phDefault p)
      where
        prefix = case phKind p of
          ParamPositional -> ""
          ParamVariadic -> "..."
          ParamKeyword -> "--"

    returnsSection = case fhReturn fh of
      Nothing -> ""
      Just t -> section "Returns:" (t : maybe [] (map indentDoc . T.lines) (fhReturnDoc fh))

    -- A task that runs only in the default environment says nothing
    -- worth a section (spec 11.6).
    envsSection
      | all ((== "local") . refKind) (fhEnvs fh) = ""
      | otherwise = section "Environments:" (columns (map envRow (fhEnvs fh)))

    examplesSection
      | null (fhExamples fh) = ""
      | otherwise = section "Examples:" (concatMap T.lines (fhExamples fh))

    definedAt = case fhLine fh of
      Just l -> "Defined at " <> T.pack (fhModule fh) <> ":" <> T.pack (show l)
      Nothing -> ""

    indentDoc = ("    " <>)

section :: Text -> [Text] -> Text
section header body = T.intercalate "\n" (header : map ("  " <>) body)

-- | Drop the fields that would only repeat one another: a bare
-- @#local@ is just @local@, and @#docker(\"img\")@ is labelled by its
-- own image.
envRow :: EnvRef -> [Text]
envRow ref
  | refLabel ref == refTarget ref && refKind ref == refTarget ref = [refKind ref]
  | refLabel ref == refTarget ref = [refKind ref, refTarget ref]
  | otherwise = [refLabel ref, refKind ref, refTarget ref]

-- | Left-align rows into columns padded to the widest cell.
columns :: [[Text]] -> [Text]
columns rows = map (T.stripEnd . T.concat . zipWith pad [0 ..]) rows
  where
    width :: Int -> Int
    width i = maximum (0 : [T.length cell | r <- rows, (j, cell) <- zip [0 ..] r, j == i])
    pad :: Int -> Text -> Text
    pad i cell = cell <> T.replicate (width i - T.length cell + 2) " "

-- | The function list of @lask run --help@.
renderListText :: FilePath -> [FunctionHelp] -> Text
renderListText path fhs
  | null visible = ""
  | otherwise =
      T.intercalate "\n" $
        ("Functions in " <> T.pack path <> ":")
          : map ("  " <>) (columns [[fhName f, fromMaybe "" (fhSummary f)] | f <- visible])
  where
    visible = filter listed fhs

-- JSON rendering ----------------------------------------------------------------

renderHelpJson :: FunctionHelp -> A.Value
renderHelpJson fh =
  A.object
    [ ("kind", A.String "function-help"),
      ("module", A.String (T.pack (fhModule fh))),
      ("function", A.String (fhName fh)),
      ("location", maybe A.Null locationJson (fhLine fh)),
      ("summary", maybeText (fhSummary fh)),
      ("description", maybeText (fhDescription fh)),
      ("params", A.toJSON (map paramJson (fhParams fh))),
      ("returns", returnsJson),
      ("environments", A.toJSON (map envJson (fhEnvs fh))),
      ("examples", A.toJSON (fhExamples fh))
    ]
  where
    locationJson l = A.object [("line", A.Number (fromIntegral l)), ("column", A.Number 1)]

    returnsJson = case fhReturn fh of
      Nothing -> A.Null
      Just t -> A.object [("type", A.String t), ("doc", maybeText (fhReturnDoc fh))]

    paramJson p =
      A.object
        [ ("name", A.String (phName p)),
          ("kind", A.String (kindText (phKind p))),
          ("type", maybeText (phType p)),
          ("default", maybeText (phDefault p)),
          ("secret", A.Bool (phSecret p)),
          ("doc", maybeText (phDoc p))
        ]

    kindText k = case k of
      ParamPositional -> "positional"
      ParamVariadic -> "variadic"
      ParamKeyword -> "keyword"

renderListJson :: FilePath -> [FunctionHelp] -> A.Value
renderListJson path fhs =
  A.object
    [ ("kind", A.String "function-list"),
      ("module", A.String (T.pack path)),
      ("functions", A.toJSON (map entry (filter listed fhs)))
    ]
  where
    entry fh =
      A.object
        [ ("name", A.String (fhName fh)),
          ("summary", maybeText (fhSummary fh)),
          ("signature", A.String (signature fh))
        ]

-- | A declaration-shaped one-liner: @build(target: String, --out: String = "dist"): String@.
signature :: FunctionHelp -> Text
signature fh =
  fhName fh
    <> "("
    <> T.intercalate ", " (map param (fhParams fh))
    <> ")"
    <> maybe "" (": " <>) (fhReturn fh)
  where
    param p =
      prefix p <> phName p <> ": " <> fromMaybe "?" (phType p) <> maybe "" (" = " <>) (phDefault p)
    prefix p = case phKind p of
      ParamPositional -> ""
      ParamVariadic -> "..."
      ParamKeyword -> "--"

-- | Listings show the module's tasks: its functions, minus the ones
-- marked @\@hidden@ (spec 11.6).
listed :: FunctionHelp -> Bool
listed fh = fhFunction fh && not (fhHidden fh)

envJson :: EnvRef -> A.Value
envJson ref =
  A.object
    [ (AK.fromText "name", A.String (refLabel ref)),
      (AK.fromText "kind", A.String (refKind ref)),
      (AK.fromText "target", A.String (refTarget ref))
    ]

maybeText :: Maybe Text -> A.Value
maybeText = maybe A.Null A.String
