{-# LANGUAGE OverloadedStrings #-}

-- | EDSL twins of the programs under example/02-language: each twin's
-- declarations must reify to the Core the elaborator produces for the
-- same declarations of the example, up to spans and bound names (#91).
module Language.Lask.EmbedTwinSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Core.Compare (sameDecl)
import Language.Lask.Core.Pretty (renderDecl)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..))
import Language.Lask.Embed (Program (..))
import qualified Language.Lask.Embed.Twin.ValuesAndTypes as ValuesAndTypes
import Test.Hspec

-- | Every declaration of the twin agrees with the example's declaration
-- of the same name.
twinOf :: FilePath -> Either [T.Text] Program -> Expectation
twinOf examplePath twin = do
  prog <- either (fail . T.unpack . T.unlines) pure twin
  compiled <- compileFile examplePath >>= either (fail . show) pure
  let source = Map.fromList [(n, cd) | ((_, n), cd) <- Map.toList (cpDecls (compiledCore compiled))]
      embedded = Map.elems (cpDecls (progCore prog))
  length embedded `shouldSatisfy` (> 0)
  forM_ embedded $ \e -> case Map.lookup (cdName e) source of
    Nothing -> expectationFailure ("the example has no " <> T.unpack (cdName e))
    Just s -> case sameDecl e s of
      Nothing -> pure ()
      Just why ->
        expectationFailure $
          T.unpack (cdName e <> ": " <> why <> "\nembedded:\n" <> renderDecl e <> "\nelaborated:\n" <> renderDecl s)

spec :: Spec
spec = describe "EDSL twins of example/02-language" $
  it "01-values-and-types, the declarations the first phase covers" $
    twinOf "example/02-language/01-values-and-types/main.lask" ValuesAndTypes.program
