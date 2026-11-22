-- | Where the entry module is when @--module@ is not given (spec 11.1):
-- @main.lask@ in the current directory, or in the nearest directory
-- above it that has one, so that a task can be run from anywhere in
-- its project.
module Command.Lask.Locate
  ( defaultEntry,
    locateEntry,
  )
where

import Control.Exception (IOException, try)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (joinPath, takeDirectory, (</>))

-- | The file name of a project's entry module.
defaultEntry :: FilePath
defaultEntry = "main.lask"

-- | The entry module, as a path relative to the current directory:
-- @main.lask@, @../main.lask@, @../../main.lask@ and so on. The
-- module's directory is the project's (paths in it, the mount at
-- @/work@, @lask.json@ and the lock file all resolve against it), so a
-- relative path is all the rest of lask needs, and it keeps the file
-- names diagnostics print short and relative to where lask was run.
-- When no directory up to the root has one, it is @main.lask@, which
-- then does not exist.
locateEntry :: IO FilePath
locateEntry = do
  cwd <- try getCurrentDirectory :: IO (Either IOException FilePath)
  case cwd of
    Left _ -> pure defaultEntry
    Right dir -> go dir (0 :: Int)
  where
    go dir up = do
      found <- either (const False) id <$> (try (doesFileExist (dir </> defaultEntry)) :: IO (Either IOException Bool))
      let parent = takeDirectory dir
      if found
        then pure (joinPath (replicate up "..") </> defaultEntry)
        else
          if parent == dir
            then pure defaultEntry
            else go parent (up + 1)
