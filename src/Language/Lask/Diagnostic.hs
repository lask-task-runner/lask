-- | Diagnostics carrying the minimum requirements of spec 14.3:
-- code, message, stage and (when available) source location,
-- plus expected/actual for type mismatches and note lines for
-- resolution candidates etc.
module Language.Lask.Diagnostic
  ( Diagnostic (..),
    mkDiagnostic,
    withExpectedActual,
    withNote,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.ErrorCode (ErrorCode, Stage, codeText, stageText)
import Language.Lask.Span (Span)
import Language.Lask.Utils (Pretty (pretty))

data Diagnostic = Diagnostic
  { diagCode :: ErrorCode,
    diagStage :: Stage,
    diagSpan :: Span,
    diagMessage :: Text,
    diagExpected :: Maybe Text,
    diagActual :: Maybe Text,
    diagNotes :: [Text]
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
      diagNotes = []
    }

withExpectedActual :: Text -> Text -> Diagnostic -> Diagnostic
withExpectedActual e a d = d {diagExpected = Just e, diagActual = Just a}

withNote :: Text -> Diagnostic -> Diagnostic
withNote n d = d {diagNotes = diagNotes d <> [n]}

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
      <> concatMap (\n -> "\n  note: " <> T.unpack n) (diagNotes d)
