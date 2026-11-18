-- | Stand-ins for a container engine, for tests of what lask asks of
-- one (spec 10.10).
--
-- * 'withClient' puts a shell script on @PATH@ under the name of an
--   engine's client, which records each invocation and answers as it
--   is told. The process environment is changed for the extent of the
--   action and restored after, so the specs that use it must not run
--   in parallel with others.
-- * 'withEnvVars' sets and unsets process environment variables for
--   the extent of an action.
module Support.FakeClient
  ( withEnvVars,
    withClient,
    clientCalls,
  )
where

import Control.Exception (bracket)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

-- | Set (@Just@) or unset (@Nothing@) variables for an action, and
-- restore what they were.
withEnvVars :: [(String, Maybe String)] -> IO a -> IO a
withEnvVars vars action = bracket save restore (const (mapM_ set vars >> action))
  where
    save = mapM (\(k, _) -> (,) k <$> lookupEnv k) vars
    restore = mapM_ set
    set (k, v) = maybe (unsetEnv k) (setEnv k) v

-- | Run an action with a client named @name@ first on @PATH@. The
-- script, @bin/<name>@ under a fresh directory, records its arguments,
-- one invocation to a line, in the file 'clientCalls' reads, and then
-- runs @body@, a piece of shell in which @$1@... are the arguments.
-- The action is given that directory.
withClient :: String -> String -> (FilePath -> IO a) -> IO a
withClient name body action =
  withSystemTempDirectory "fake-client" $ \dir -> do
    let bin = dir </> "bin"
    createDirectoryIfMissing True bin
    writeFile (bin </> name) $
      unlines
        [ "#!/bin/sh",
          "printf '%s\\n' \"$*\" >> '" <> dir </> "calls" <> "'",
          body
        ]
    callProcess "chmod" ["+x", bin </> name]
    writeFile (dir </> "calls") ""
    path <- maybe "" id <$> lookupEnv "PATH"
    withEnvVars [("PATH", Just (bin <> ":" <> path))] (action dir)

-- | The invocations the client of 'withClient' recorded.
clientCalls :: FilePath -> IO [String]
clientCalls dir = do
  s <- readFile (dir </> "calls")
  length s `seq` pure (lines s)
