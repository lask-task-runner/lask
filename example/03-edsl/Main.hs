{-# LANGUAGE OverloadedStrings #-}

-- | Runs the program in "Example" with "Language.Lask.Embed.Run": what
-- each task lowers to, its help, what it can reach before it runs, and
-- the tasks themselves, with a scripted command runner in place of
-- Docker.
--
-- > stack test lask:edsl-example
module Main (main) where

import Control.Exception (try)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Example (program)
import Language.Lask.Embed
import Language.Lask.Embed.Run
import System.Exit (exitFailure)

main :: IO ()
main = case program of
  Left errs -> mapM_ T.putStrLn errs >> exitFailure
  Right prog -> do
    let tasks = map (snd . declKey) (progDecls prog)

    section "What each task lowers to"
    forM_ tasks $ \name -> mapM_ (T.putStrLn . (<> "\n")) (lowered prog name)

    section "Help"
    forM_ (progExports prog) $ \name -> mapM_ T.putStrLn (usage prog name)

    section "What each task can reach, before it runs"
    forM_ (progExports prog) $ \name -> do
      let vars = maybe "any" (T.intercalate ", " . Set.toList) (variables prog name)
      T.putStrLn $
        T.justifyLeft 14 ' ' name
          <> T.intercalate ", " (environments prog name)
          <> (if T.null vars then "" else "  reads " <> vars)

    section "Running tasks, with a scripted runner in place of Docker"
    let demo name args = do
          T.putStrLn ("$ " <> T.unwords (name : map renderValueText args))
          r <- try (runTask prog (commandHooks scripted) name args [])
          T.putStrLn $ case r of
            Right v -> "=> " <> T.replace "\n" "\n   " (renderValueText v) <> "\n"
            Left lf -> "failed: " <> renderValueText (lfError lf) <> "\n"
    demo "fact" [VNumber 5]
    demo "release" [VString "prod"]
    demo "ship" []
    demo "lint_changed" []
    demo "test_each" []
    demo "test" []
  where
    section t = T.putStrLn ("\n== " <> t <> " ==\n")

-- | Prints what would run where, and answers the commands whose output
-- the program reads.
scripted :: CommandRunner
scripted env cmd = do
  T.putStrLn ("   [" <> imageOf env <> "] " <> cmd)
  pure (Right (0, reply cmd, ""))
  where
    imageOf e = case Map.lookup "image" (envParams e) of
      Just (VString i) -> i
      _ -> envKind e

reply :: Text -> Text
reply cmd
  | "git describe" `T.isPrefixOf` cmd = "v1.4.0"
  | "git diff" `T.isPrefixOf` cmd = "web/src/app.ts\nweb/src/api.ts"
  | "go list" `T.isPrefixOf` cmd = "example.com/api\nexample.com/api/db"
  | otherwise = "ok: " <> cmd
