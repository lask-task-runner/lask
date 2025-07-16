{-# LANGUAGE FlexibleInstances #-}

module Language.Lask.Error
  ( LanguageError,
    LanguageError' (..),
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
  | BundleError String
  | UndefinedError String
  | DuplicateNameError String
  | RuntimeError String ExitCode
  deriving (Show, Eq)

instance Eq1 (LanguageError' ann) where
  liftEq _ (SyntaxError s1) (SyntaxError s2) = s1 == s2
  liftEq _ (BundleError s1) (BundleError s2) = s1 == s2
  liftEq _ (UndefinedError s1) (UndefinedError s2) = s1 == s2
  liftEq _ (DuplicateNameError s1) (DuplicateNameError s2) = s1 == s2
  liftEq _ (RuntimeError s1 e1) (RuntimeError s2 e2) = s1 == s2 && e1 == e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (LanguageError' ann) where
  liftShowsPrec _ _ _ (SyntaxError s) = showString $ "SyntaxError " <> show s
  liftShowsPrec _ _ _ (BundleError s) = showString $ "BundleError " <> show s
  liftShowsPrec _ _ _ (UndefinedError s) = showString $ "UndefinedError " <> show s
  liftShowsPrec _ _ _ (DuplicateNameError s) = showString $ "DuplicateNameError " <> show s
  liftShowsPrec _ _ _ (RuntimeError s e) = showString $ "RuntimeError " <> show s <> " " <> show e

instance (Pretty ann) => Pretty (LanguageError ann) where
  pretty (s :< SyntaxError detail) = pretty s <> ": syntax error: " <> detail
  pretty (s :< BundleError detail) = pretty s <> ": bundle error: " <> detail
  pretty (s :< UndefinedError m) = pretty s <> ": not defined: " <> m
  pretty (s :< DuplicateNameError m) = pretty s <> ": already defined: " <> m
  pretty (s :< RuntimeError m _) = pretty s <> ": runtime error: " <> m

instance (Pretty ann) => Pretty [LanguageError ann] where
  pretty = unlines . map pretty

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
