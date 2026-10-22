{-# LANGUAGE OverloadedStrings #-}

module Language.Lask.Core.LintSpec (spec) where

import Control.Monad (forM, unless)
import qualified Data.Map.Strict as Map
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Language.Lask (Compiled (..), compileFile)
import Language.Lask.Core.AST
import Language.Lask.Core.Lint (lintProgram)
import Language.Lask.Elaborate (CoreDecl (..), CoreProgram (..))
import Language.Lask.Span (Span (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | Every main.lask under example/.
examplePrograms :: IO [FilePath]
examplePrograms = walk "example"
  where
    walk dir = do
      entries <- listDirectory dir
      fmap concat . forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then walk p
          else pure [p | "main.lask" `isSuffixOf` p]

spec :: Spec
spec = describe "Language.Lask.Core.Lint" $ do
  it "finds nothing in any example program the elaborator accepts" $ do
    files <- examplePrograms
    length files `shouldSatisfy` (> 10)
    compiled <- fmap concat . forM files $ \file -> do
      r <- compileFile file
      case r of
        -- A program that needs a fetched dependency does not compile
        -- here; it is not counted.
        Left _ -> pure []
        Right c -> do
          let problems = lintProgram (compiledCore c)
          unless (null problems) $
            expectationFailure (file <> ":\n" <> T.unpack (T.unlines problems))
          pure [file]
    length compiled `shouldSatisfy` (>= 15)

  it "reports an unbound local, a missing declaration, a missing built-in and arity errors" $ do
    let core = Core NoSpan
        decl n params body = CoreDecl "m" n [] (error "type") body params Map.empty
        lam ps body = core (CLam (Lam "f" "m" ps Nothing [] body (error "type")))
        f = decl "f" Nothing (lam ["x"] (core (CApp (core (CVar (TopRef "m" "f"))) [core (CVar (LocalRef "y"))] [("k", core CNull)])))
        g = decl "g" Nothing (core (CApp (core (CVar (BuiltinRef "trim"))) [] []))
        h = decl "h" Nothing (core (CVar (TopRef "m" "nope")))
        i = decl "i" Nothing (core (CVar (BuiltinRef "nope")))
        prog = CoreProgram "m" "." (Map.fromList [(("m", n), d) | d@(CoreDecl _ n _ _ _ _ _) <- [f, g, h, i]]) mempty mempty [] [] [] mempty mempty
    lintProgram prog
      `shouldMatchList` [ "f: the local y is not bound",
                          "g: calls the built-in trim with 0 arguments, but it takes 1",
                          "h: the declaration nope of m does not exist",
                          "i: the built-in nope does not exist"
                        ]
