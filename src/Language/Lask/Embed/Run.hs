{-# LANGUAGE OverloadedStrings #-}

-- | Running a program built with "Language.Lask.Embed", and asking what
-- a task can reach before it runs. Experimental, like the rest of the
-- EDSL.
--
-- A task runs through lask's evaluator. How a command runs is up to the
-- hooks you give it: 'commandHooks' takes a function that runs one
-- command in the environment it names and returns its exit code,
-- standard output and standard error, and fills in the rest. A runner
-- that uses Docker can be as small as:
--
-- > dockerRunner :: CommandRunner
-- > dockerRunner env cmd = do
-- >   let (exe, args) = case Map.lookup "image" (envParams env) of
-- >         Just (VString img) -> ("docker", ["run", "--rm", T.unpack img, "sh", "-c", T.unpack cmd])
-- >         _ -> ("sh", ["-c", T.unpack cmd])
-- >   (code, out, err) <- readCreateProcessWithExitCode (proc exe args) ""
-- >   pure (Right (exitCode code, T.pack out, T.pack err))
--
-- Unlike the lask CLI, such a runner neither pins images by digest nor
-- mounts the project; it is yours to decide what it does.
--
-- The types re-exported here are lask's own runtime types, which the
-- hooks are written in. They share the EDSL's experimental status.
module Language.Lask.Embed.Run
  ( -- * Running a task
    runTask,
    commandHooks,

    -- * Before running
    environments,
    variables,
    usage,
    lowered,

    -- * Hooks
    RtHooks (..),
    CommandRunner,
    FileRunner,
    FileOp (..),
    LogSink,
    noLogSink,
    AsyncTracker,
    noAsyncTracker,
    newAsyncTracker,

    -- * Values and failures
    Value (..),
    EnvValue (..),
    LaskFailure (..),
    runtimeFailure,
    ErrorCode (..),
    renderValueText,
  )
where

import Command.Lask.Envs (EnvRef (..), collectEnvReadsFrom, collectEnvRefsFrom)
import Data.List (find, nub)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Builtins.Impl (CommandRunner, FileOp (..), FileRunner, RtHooks (..))
import Language.Lask.Core.Pretty (renderCore, renderDecl)
import Language.Lask.Elaborate (CoreDecl (..), StaticParams (..))
import Language.Lask.Embed.Internal (Decl (..), KwInfo (..), Program (..), programModule)
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Obs.ExecLog (LogSink, noLogSink)
import Language.Lask.Runtime.AsyncTrack (AsyncTracker, newAsyncTracker, noAsyncTracker)
import Language.Lask.Runtime.Eval (applyValue, mkRtCtx, topValue)
import Language.Lask.Runtime.Value (EnvValue (..), LaskFailure (..), Value (..), runtimeFailure)
import Language.Lask.Serialize (renderValueText)
import Language.Lask.Types (renderType)
import System.Environment (lookupEnv)

-- | Run one task of a program with the given positional and keyword
-- arguments. A failure the program does not recover from is thrown as
-- a t'LaskFailure'.
runTask :: Program -> RtHooks -> Text -> [Value] -> [(Text, Value)] -> IO Value
runTask prog hooks name args kws = do
  ctx <- mkRtCtx (progCore prog) "" hooks
  f <- topValue ctx (programModule, name)
  applyValue ctx f args kws

-- | Hooks that run commands with the given runner. The file built-ins
-- (spec 15.11) fail, @log@ output is dropped, computations nobody
-- awaited are not reported, and @get_env@ reads the process
-- environment as it is, without resolving secret references.
commandHooks :: CommandRunner -> RtHooks
commandHooks runner =
  RtHooks
    { hookRunCommand = runner,
      hookRunFile = \_ _ -> pure (Left (runtimeFailure ERuntimeValue "the file built-ins are not available to an embedded program")),
      hookLog = noLogSink,
      hookAsync = noAsyncTracker,
      hookReadEnv = fmap (fmap T.pack) . lookupEnv . T.unpack
    }

-- | The environments a task can reach, over every call and branch,
-- whether or not they run: what @lask envs list@ reports for a @.lask@
-- task (spec 11.4). Images are named as written, @golang:1.22@;
-- @local@ is the host.
environments :: Program -> Text -> [Text]
environments prog name = nub (map refLabel (collectEnvRefsFrom (progCore prog) (programModule, name)))

-- | The environment variables a task can read by name, as @lask
-- secrets@ reports them (spec 11.10). 'Nothing' when a name is
-- computed, so that any variable may be read.
variables :: Program -> Text -> Maybe (Set Text)
variables prog name = collectEnvReadsFrom (progCore prog) (programModule, name)

-- | A task's parameters and docs, in the shape of @--help@:
--
-- > release <target: String> [--dry_run: Bool = true]
-- >     Publish the latest tag to a target
-- >     --dry_run: Only print what would be published
usage :: Program -> Text -> Maybe Text
usage prog name = render <$> find ((== name) . snd . declKey) (progDecls prog)
  where
    render d =
      T.unwords (name : map positional (params d) <> map keyword (declKeywords d))
        <> maybe "" ("\n    " <>) (declDoc d)
        <> T.concat ["\n    --" <> kiName k <> ": " <> h | k <- declKeywords d, Just h <- [kiHelp k]]
    params d = maybe [] spPositional (cdParams (declCore d))
    positional (n, t) = "<" <> n <> ": " <> renderType t <> ">"
    keyword k = "[--" <> kiName k <> ": " <> renderType (kiType k) <> " = " <> renderCore 0 (kiDefault k) <> "]"

-- | What a task lowers to, in a readable notation close to Lask's (not
-- valid Lask: see "Language.Lask.Core.Pretty").
lowered :: Program -> Text -> Maybe Text
lowered prog name = renderDecl . declCore <$> find ((== name) . snd . declKey) (progDecls prog)
