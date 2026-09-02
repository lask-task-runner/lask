{-# LANGUAGE OverloadedStrings #-}

-- | Container images built from a recipe (spec 10.2, 10.3).
--
-- A recipe environment names a Dockerfile and a build context inside
-- the module tree. The image it denotes is content-addressed by a
-- recipe hash, so a changed recipe is a different image and cannot
-- reuse a cached one. Building is never implicit: @check@ \/ @run@ \/
-- @eval@ \/ @envs@ resolve the tag and report @E-IO-IMAGE-MISSING@
-- when it is absent, and only @deps sync@ \/ @env build@ materialize
-- it (spec 10.3).
module Language.Lask.Runtime.Image
  ( recipeTag,
    imageExists,
    buildRecipe,
  )
where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Lask.Deps.Hash (hashBytes)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- | The content-addressed tag of a recipe: @lask\/\<recipe hash\>@.
-- The hash covers the Dockerfile's contents and the context path, so
-- editing the Dockerfile yields a different tag.
recipeTag :: FilePath -> Text -> Text -> IO (Either Text Text)
recipeTag baseDir dockerfile context = do
  r <- try (BS.readFile (baseDir </> T.unpack dockerfile))
  pure $ case r of
    Left e ->
      Left ("cannot read Dockerfile '" <> dockerfile <> "': " <> T.pack (show (e :: IOException)))
    Right bytes ->
      let key = hashBytes (bytes <> TE.encodeUtf8 ("\0" <> context))
       in Right ("lask/" <> T.replace "sha256-" "" key)

-- | Whether the tag is present on the target Docker daemon.
imageExists :: Text -> IO Bool
imageExists tag = do
  r <- try (readCreateProcessWithExitCode (proc "docker" ["image", "inspect", T.unpack tag]) "")
  pure $ case r of
    Left e -> const False (e :: IOException)
    Right (ExitSuccess, _, _) -> True
    Right _ -> False

-- | Build a recipe into its content-addressed tag. No host mount other
-- than the declared context, no privileged mode, no host networking
-- (spec 10.3).
buildRecipe :: FilePath -> Text -> Text -> Text -> IO (Either Text ())
buildRecipe baseDir dockerfile context tag = do
  let args =
        [ "build",
          "-f",
          baseDir </> T.unpack dockerfile,
          "-t",
          T.unpack tag,
          baseDir </> T.unpack context
        ]
  r <- try (readCreateProcessWithExitCode (proc "docker" args) "")
  pure $ case r of
    Left e -> Left ("cannot run docker build: " <> T.pack (show (e :: IOException)))
    Right (ExitSuccess, _, _) -> Right ()
    Right (_, _, err) -> Left (T.strip (T.pack err))
