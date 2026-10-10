{-# LANGUAGE OverloadedStrings #-}

-- | Runs the program in "Example" through the parts of Lask that work on
-- Core, unchanged: the analyses behind @lask envs list@ and @lask
-- secrets@, and the evaluator, with a scripted command runner in place
-- of Docker.
--
-- > stack test lask:edsl-example
module Main (main) where

import Command.Lask.Envs (EnvRef (..), collectEnvReadsFrom, collectEnvRefsFrom)
import Control.Exception (try)
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Example (program)
import Language.Lask.Builtins.Impl (RtHooks (..))
import Language.Lask.Core.Pretty (renderCore, renderDecl)
import Language.Lask.Elaborate (CoreDecl (..), StaticParams (..))
import Language.Lask.Embed
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Obs.ExecLog (noLogSink)
import Language.Lask.Runtime.AsyncTrack (noAsyncTracker)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value (EnvValue (..), LaskFailure (..), Value (..), runtimeFailure)
import Language.Lask.Serialize (renderValueText)
import Language.Lask.Types (renderType)
import System.Exit (exitFailure)

main :: IO ()
main = case program of
  Left errs -> mapM_ T.putStrLn errs >> exitFailure
  Right prog -> do
    let core = progCore prog
        key name = (programModule, name)

    section "The Core each declaration reifies to"
    forM_ (progDecls prog) $ \d -> T.putStrLn (renderDecl (declCore d) <> "\n")

    section "Help, from the types and the keyword applicative"
    forM_ (progDecls prog) $ \d ->
      when (snd (declKey d) `elem` progExports prog) (T.putStrLn (helpText d))

    section "Environments and variables each task can reach (Command.Lask.Envs)"
    forM_ (progExports prog) $ \name -> do
      let envs = Set.toList (Set.fromList [refLabel r | r <- collectEnvRefsFrom core (key name)])
          vars = maybe "any" (T.intercalate ", " . Set.toList) (collectEnvReadsFrom core (key name))
      T.putStrLn (T.justifyLeft 14 ' ' name <> T.intercalate ", " envs <> (if T.null vars then "" else "  reads " <> vars))

    section "Running tasks, with a scripted runner in place of Docker"
    let runTask name args = do
          T.putStrLn ("$ lask run " <> T.unwords (name : map renderValueText args))
          ctx <- mkRtCtx core "" scripted
          r <- try (topValue ctx (key name) >>= \f -> applyValue ctx f args [])
          T.putStrLn $ case r of
            Right v -> "=> " <> T.replace "\n" "\n   " (renderValueText v) <> "\n"
            Left lf -> "failed: " <> renderValueText (lfError lf) <> "\n"
    runTask "fact" [VNumber 5]
    runTask "release" [VString "prod"]
    runTask "ship" []
    runTask "lint_changed" []
    runTask "test_each" []
    runTask "test" []
  where
    section t = T.putStrLn ("\n== " <> t <> " ==\n")

helpText :: Decl -> Text
helpText d =
  T.unwords (("lask run " <> snd (declKey d)) : map positional params <> map keyword (declKeywords d))
    <> maybe "" ("\n    " <>) (declDoc d)
    <> T.concat ["\n    --" <> kiName k <> ": " <> h | k <- declKeywords d, Just h <- [kiHelp k]]
  where
    params = maybe [] spPositional (cdParams (declCore d))
    positional (n, t) = "<" <> n <> ": " <> renderType t <> ">"
    keyword k = "[--" <> kiName k <> " " <> renderType (kiType k) <> " = " <> renderCore 0 (kiDefault k) <> "]"

-- | Prints what would run where, and answers the commands whose output
-- the program reads.
scripted :: RtHooks
scripted =
  RtHooks
    { hookRunCommand = \env cmd -> do
        T.putStrLn ("   [" <> imageOf env <> "] " <> cmd)
        pure (Right (0, reply cmd, "")),
      hookRunFile = \_ _ -> pure (Left (runtimeFailure ERuntimeValue "the example reads no files")),
      hookLog = noLogSink,
      hookAsync = noAsyncTracker,
      hookReadEnv = \_ -> pure Nothing
    }
  where
    imageOf e = case Map.lookup "image" (envParams e) of
      Just (VString i) -> i
      _ -> envKind e
    reply cmd
      | "git describe" `T.isPrefixOf` cmd = "v1.4.0"
      | "git diff" `T.isPrefixOf` cmd = "web/src/app.ts\nweb/src/api.ts"
      | "go list" `T.isPrefixOf` cmd = "example.com/api\nexample.com/api/db"
      | otherwise = "ok: " <> cmd
