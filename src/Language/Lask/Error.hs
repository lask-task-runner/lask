{-# LANGUAGE FlexibleInstances #-}

module Language.Lask.Error
  ( LanguageError,
    LanguageError' (SyntaxError, RuntimeError),
    fromParseErrorBundle,
  )
where

import Control.Comonad.Cofree
import Data.Functor.Classes (Eq1 (liftEq), Show1, liftShowsPrec)
import qualified Data.List.NonEmpty as NEL
import Data.Void (Void)
import Language.Lask.Span (Position (Position), Span (Span))
import Language.Lask.Utils (Pretty (pretty))
import System.Exit (ExitCode)
import Text.Megaparsec
  ( ParseErrorBundle (bundleErrors, bundlePosState),
    PosState (pstateSourcePos),
    SourcePos (SourcePos),
    TraversableStream (reachOffset),
    VisualStream,
    errorOffset,
    parseErrorTextPretty,
    unPos,
  )

type LanguageError ann = Cofree (LanguageError' ann) ann

data LanguageError' ann self
  = SyntaxError String
  | RuntimeError String ExitCode
  deriving (Show, Eq)

instance Eq1 (LanguageError' ann) where
  liftEq _ (SyntaxError s1) (SyntaxError s2) = s1 == s2
  liftEq _ (RuntimeError s1 e1) (RuntimeError s2 e2) = s1 == s2 && e1 == e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (LanguageError' ann) where
  liftShowsPrec _ _ _ (SyntaxError s) = showString $ "StaticError " <> show s
  liftShowsPrec _ _ _ (RuntimeError s e) = showString $ "RuntimeError " <> show s <> " " <> show e

instance (Pretty ann) => Pretty (LanguageError ann) where
  pretty (s :< SyntaxError m) = pretty s <> ": syntax error: " <> m
  pretty (s :< RuntimeError m _) = pretty s <> ": runtime error: " <> m

fromParseErrorBundle :: (TraversableStream a, VisualStream a) => ParseErrorBundle a Void -> LanguageError Span
fromParseErrorBundle e =
  mkSpan' (errorBundleSourcePos e)
    :< SyntaxError
      (parseErrorTextPretty $ NEL.head $ bundleErrors e)
  where
    mkSpan' (SourcePos file l c) = Span (Position file (unPos l) (unPos c)) (Position file (unPos l) (unPos c))

errorBundleSourcePos :: (TraversableStream a) => ParseErrorBundle a Void -> SourcePos
errorBundleSourcePos peb = do
  let pst = bundlePosState peb
  let e = NEL.head $ bundleErrors peb
  let (_, pst') = reachOffset (errorOffset e) pst
  pstateSourcePos pst'
