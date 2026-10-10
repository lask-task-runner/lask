{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostics carrying the minimum requirements of spec 14.3:
-- code, message, stage and (when available) source location,
-- plus expected/actual for type mismatches and note lines for
-- resolution candidates etc.
module Language.Lask.Diagnostic
  ( Diagnostic (..),
    Advisory (..),
    mkDiagnostic,
    settleDiagnostics,
    withExpectedActual,
    withNote,
    withSuggestions,
    suggesting,
    suggestingAny,
    didYouMean,
  )
where

import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.ErrorCode (AdvisoryCode, ErrorCode, Stage, advisoryText, codeText, stageText)
import Language.Lask.Span (Span)
import Language.Lask.Suggest (suggestAny)
import Language.Lask.Utils (Pretty (pretty))

data Diagnostic = Diagnostic
  { diagCode :: ErrorCode,
    diagStage :: Stage,
    diagSpan :: Span,
    diagMessage :: Text,
    diagExpected :: Maybe Text,
    diagActual :: Maybe Text,
    diagNotes :: [Text],
    -- | Correction candidates for a name that did not resolve, closest
    -- first (spec 14.3).
    diagSuggestions :: [Text]
  }
  deriving (Show, Eq)

mkDiagnostic :: ErrorCode -> Stage -> Span -> Text -> Diagnostic
mkDiagnostic code stage sp msg =
  Diagnostic
    { diagCode = code,
      diagStage = stage,
      diagSpan = sp,
      diagMessage = msg,
      diagExpected = Nothing,
      diagActual = Nothing,
      diagNotes = [],
      diagSuggestions = []
    }

-- | Diagnostics in the order they are reported (spec 14.3): by file
-- and position, those without a location last, each once.
settleDiagnostics :: [Diagnostic] -> [Diagnostic]
settleDiagnostics = sortOn diagSpan . nub

-- | An advisory diagnostic found by static analysis (spec 14.2): it
-- carries @severity: "warning"@, and neither stops analysis nor changes
-- an exit code.
data Advisory = Advisory
  { advCode :: AdvisoryCode,
    advSpan :: Span,
    advMessage :: Text
  }
  deriving (Show, Eq)

instance Pretty Advisory where
  pretty a =
    pretty (advSpan a)
      <> ": "
      <> T.unpack (advisoryText (advCode a))
      <> " [static]: "
      <> T.unpack (advMessage a)

withExpectedActual :: Text -> Text -> Diagnostic -> Diagnostic
withExpectedActual e a d = d {diagExpected = Just e, diagActual = Just a}

withNote :: Text -> Diagnostic -> Diagnostic
withNote n d = d {diagNotes = diagNotes d <> [n]}

withSuggestions :: [Text] -> Diagnostic -> Diagnostic
withSuggestions ss d = d {diagSuggestions = ss}

-- | Suggest the names of @pool@ that lie close to @n@, the name that
-- did not resolve.
suggesting :: Text -> [Text] -> Diagnostic -> Diagnostic
suggesting n = suggestingAny [n]

-- | 'suggesting' for several names written at once.
suggestingAny :: [Text] -> [Text] -> Diagnostic -> Diagnostic
suggestingAny ns pool = withSuggestions (suggestAny ns pool)

instance Pretty Diagnostic where
  pretty d =
    pretty (diagSpan d)
      <> ": "
      <> T.unpack (codeText (diagCode d))
      <> " ["
      <> T.unpack (stageText (diagStage d))
      <> "]: "
      <> T.unpack (diagMessage d)
      <> maybe "" (\e -> "\n  expected: " <> T.unpack e) (diagExpected d)
      <> maybe "" (\a -> "\n  actual:   " <> T.unpack a) (diagActual d)
      <> concatMap (\n -> "\n  note: " <> T.unpack n) (diagNotes d <> didYouMean (diagSuggestions d))

-- | The note that offers correction candidates (spec 14.3):
-- @did you mean 'a', 'b' or 'c'?@.
--
-- >>> didYouMean (map T.pack ["hello", "help"])
-- ["did you mean 'hello' or 'help'?"]
didYouMean :: [Text] -> [Text]
didYouMean [] = []
didYouMean ss = ["did you mean " <> alternatives (map quote ss) <> "?"]
  where
    quote s = "'" <> s <> "'"
    alternatives [x] = x
    alternatives xs = T.intercalate ", " (init xs) <> " or " <> last xs
