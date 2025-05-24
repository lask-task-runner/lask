{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Lask.Fixture
  ( posParam,
    eImage,
    eString,
    eNumber,
    eRunScript,
    eRunScript1,
    eRunScript2,
    eRunCommand,
    eRightOp,
    eLeftOp,
    eRightPipe,
    eLeftPipe,
    eNumberBinaryOp,
    eConcat,
    eStringAdd,
    mPrelude,
    preludeSpan,
    CommandResult (..),
    runSubprocess,
    runContainer,
    exitCodeToInt,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Concurrent (putMVar, readMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import GHC.Conc (forkIO)
import GHC.IO.Exception (ExitCode (ExitFailure))
import GHC.IO.Handle (Handle)
import qualified Language.Lask.AST as AST
import Language.Lask.Span (Span (), mkSpan)
import System.Directory (getCurrentDirectory)
import System.Directory.Internal.Prelude (hClose)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hIsClosed, hIsEOF)
import qualified System.IO as IO
import System.Process (CreateProcess (..), createProcess, shell, waitForProcess)
import System.Process.Common (showCreateProcessForUser)
import System.Process.Internals (StdStream (CreatePipe))

tAny :: AST.Type Span
tAny = preludeSpan :< AST.AnyType

tNumber :: AST.Type Span
tNumber = preludeSpan :< AST.NumberType

tString :: AST.Type Span
tString = preludeSpan :< AST.StringType

posParam :: String -> Maybe (AST.Type Span) -> Maybe (AST.Expr Span) -> AST.Parameter Span
posParam name paramType defaultValue =
  preludeSpan
    :< AST.PositionedParameter
      name
      False
      False
      paramType
      defaultValue

eImage :: String -> AST.Expr Span
eImage image = preludeSpan :< AST.Image image

eString :: String -> AST.Expr Span
eString s = preludeSpan :< AST.String s

eNumber :: Scientific -> AST.Expr Span
eNumber n = preludeSpan :< AST.Number n

eRunScript :: AST.Expr Span
eRunScript =
  preludeSpan
    :< AST.FixtureFun
      [ posParam "script" (Just tString) Nothing,
        posParam "image" (Just tString) (Just $ eImage "local")
      ]
      ( \args -> do
          let (Just (_ :< AST.String script), _) = fromJust $ lookup "script" args
          let (Just (_ :< AST.Image image), _) = fromJust $ lookup "image" args
          (CommandResult i out err) <- liftIO $ case image of
            "local" -> runSubprocess "local" script
            _ -> runContainer image script
          pure $
            preludeSpan
              :< AST.Object
                [ (eString "exitCode", preludeSpan :< AST.Number (fromIntegral $ exitCodeToInt i)),
                  (eString "stdout", preludeSpan :< AST.String out),
                  (eString "stderr", preludeSpan :< AST.String err)
                ]
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tString,
                preludeSpan :< AST.KeywordParameterType "image" False False tString
              ]
              tAny
      )

eRunScript1 :: AST.Expr Span
eRunScript1 =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "script"
            False
            False
            (Just tString)
            Nothing,
        preludeSpan
          :< AST.KeywordParameter
            "image"
            False
            False
            (Just tString)
            (Just $ preludeSpan :< AST.Image "local")
      ]
      ( \args -> do
          let (Just (_ :< AST.String script), _) = fromJust $ lookup "script" args
          let (Just (_ :< AST.Image image), _) = fromJust $ lookup "image" args
          (CommandResult i out _) <- liftIO $ case image of
            "local" -> runSubprocess "local" script
            _ -> runContainer image script
          case i of
            ExitSuccess -> pure $ preludeSpan :< AST.String out
            ExitFailure _ -> pure $ preludeSpan :< AST.Error ("Error command exec: " <> "$ " <> script) i
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tNumber,
                preludeSpan :< AST.KeywordParameterType "image" False False tNumber
              ]
              tString
      )

eRunScript2 :: AST.Expr Span
eRunScript2 =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "script"
            False
            False
            (Just tString)
            Nothing,
        preludeSpan
          :< AST.KeywordParameter
            "image"
            False
            False
            (Just tString)
            (Just $ preludeSpan :< AST.Image "local")
      ]
      ( \args -> do
          let (Just (_ :< AST.String script), _) = fromJust $ lookup "script" args
          let (Just (_ :< AST.Image image), _) = fromJust $ lookup "image" args
          (CommandResult i _ err) <- liftIO $ case image of
            "local" -> runSubprocess "local" script
            _ -> runContainer image script
          case i of
            ExitSuccess -> pure $ preludeSpan :< AST.String err
            ExitFailure _ -> pure $ preludeSpan :< AST.Error ("Error command exec: " <> "$ " <> script) i
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tString,
                preludeSpan :< AST.KeywordParameterType "image" False False tString
              ]
              tString
      )

eRunCommand :: AST.Expr Span
eRunCommand =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "script"
            False
            False
            (Just tString)
            Nothing,
        preludeSpan
          :< AST.KeywordParameter
            "image"
            False
            False
            (Just tString)
            (Just $ preludeSpan :< AST.Image "local")
      ]
      ( \args -> do
          let (Just (_ :< AST.String script), _) = fromJust $ lookup "script" args
          let (Just (_ :< AST.Image image), _) = fromJust $ lookup "image" args
          (CommandResult i out _) <- liftIO $ case image of
            "local" -> runSubprocess "local" script
            _ -> runContainer image script
          case i of
            ExitSuccess -> pure $ preludeSpan :< AST.String out
            ExitFailure _ -> pure $ preludeSpan :< AST.Error ("Error command exec: " <> "$ " <> script) i
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tString,
                preludeSpan :< AST.KeywordParameterType "image" False False tString
              ]
              tString
      )

eRightOp :: AST.Expr Span
eRightOp =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan :< AST.PositionedParameter "a" False False Nothing Nothing,
        preludeSpan :< AST.PositionedParameter "b" False False Nothing Nothing
      ]
      ( \args -> do
          let (Just e1, _) = fromJust $ lookup "a" args
          let (Just e2, _) = fromJust $ lookup "b" args
          _ <- pure e1 -- execute forcibly
          pure e2
      )
      Nothing

eLeftOp :: AST.Expr Span
eLeftOp =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan :< AST.PositionedParameter "a" False False Nothing Nothing,
        preludeSpan :< AST.PositionedParameter "b" False False Nothing Nothing
      ]
      ( \args -> do
          let (Just e1, _) = fromJust $ lookup "a" args
          let (Just e2, _) = fromJust $ lookup "b" args
          _ <- pure e2 -- execute forcibly
          pure e1
      )
      Nothing

eRightPipe :: AST.Expr Span
eRightPipe =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan :< AST.PositionedParameter "a" False False Nothing Nothing,
        preludeSpan :< AST.PositionedParameter "b" False False Nothing Nothing
      ]
      ( \args -> do
          let (Just e1, _) = fromJust $ lookup "a" args
          let (Just e2, _) = fromJust $ lookup "b" args
          pure $
            preludeSpan
              :< AST.Call
                e2
                [preludeSpan :< AST.PositionedArgument False e1]
      )
      Nothing

eLeftPipe :: AST.Expr Span
eLeftPipe =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan :< AST.PositionedParameter "a" False False Nothing Nothing,
        preludeSpan :< AST.PositionedParameter "b" False False Nothing Nothing
      ]
      ( \args -> do
          let (Just e1, _) = fromJust $ lookup "a" args
          let (Just e2, _) = fromJust $ lookup "b" args
          pure $
            preludeSpan
              :< AST.Call
                e1
                [preludeSpan :< AST.PositionedArgument False e2]
      )
      Nothing

eNumberBinaryOp :: (Scientific -> Scientific -> Scientific) -> AST.Expr Span
eNumberBinaryOp op =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "a"
            False
            False
            (Just tNumber)
            Nothing,
        preludeSpan
          :< AST.PositionedParameter
            "b"
            False
            False
            (Just tNumber)
            Nothing
      ]
      ( \args -> do
          let (Just (_ :< AST.Number a), _) = fromJust $ lookup "a" args
          let (Just (_ :< AST.Number b), _) = fromJust $ lookup "b" args
          pure $ preludeSpan :< AST.Number (a `op` b)
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tNumber,
                preludeSpan :< AST.PositionedParameterType False False tNumber
              ]
              tNumber
      )

eConcat :: AST.Expr Span
eConcat =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "strings"
            False
            False
            (Just tAny)
            Nothing
      ]
      ( \args -> do
          let (Just (_ :< AST.Array strings), _) = fromJust $ lookup "strings" args
          let strings' = map (\(_ :< AST.String s) -> s) strings
          pure $ preludeSpan :< AST.String (intercalate "" strings')
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tAny
              ]
              tString
      )

eStringAdd :: AST.Expr Span
eStringAdd =
  preludeSpan
    :< AST.FixtureFun
      [ preludeSpan
          :< AST.PositionedParameter
            "a"
            False
            False
            (Just tString)
            Nothing,
        preludeSpan
          :< AST.PositionedParameter
            "b"
            False
            False
            (Just tString)
            Nothing
      ]
      ( \args -> do
          let (Just (_ :< AST.String a), _) = fromJust $ lookup "a" args
          let (Just (_ :< AST.String b), _) = fromJust $ lookup "b" args
          pure $ preludeSpan :< AST.String (a <> b)
      )
      ( Just $
          preludeSpan
            :< AST.LambdaType
              [ preludeSpan :< AST.PositionedParameterType False False tString,
                preludeSpan :< AST.PositionedParameterType False False tString
              ]
              tString
      )

mPrelude :: AST.Module Span
mPrelude =
  preludeSpan
    :< AST.Module
      [ preludeSpan :< AST.ExprStatement "$*" eRunScript,
        preludeSpan :< AST.ExprStatement "$" eRunScript1,
        preludeSpan :< AST.ExprStatement "$1" eRunScript1,
        preludeSpan :< AST.ExprStatement "$2" eRunScript2,
        preludeSpan :< AST.ExprStatement "shell" eRunCommand,
        preludeSpan :< AST.ExprStatement ">>" eRightOp,
        preludeSpan :< AST.ExprStatement "<<" eLeftOp,
        preludeSpan :< AST.ExprStatement "|>" eRightPipe,
        preludeSpan :< AST.ExprStatement "<|" eLeftPipe,
        preludeSpan :< AST.ExprStatement "+" (eNumberBinaryOp (+)),
        preludeSpan :< AST.ExprStatement "-" (eNumberBinaryOp (-)),
        preludeSpan :< AST.ExprStatement "*" (eNumberBinaryOp (*)),
        preludeSpan :< AST.ExprStatement "/" (eNumberBinaryOp (/)),
        preludeSpan :< AST.ExprStatement "concat" eConcat,
        preludeSpan :< AST.ExprStatement "++" eStringAdd
      ]

exitCodeToInt :: ExitCode -> Int
exitCodeToInt c = case c of
  ExitSuccess -> 0
  ExitFailure i -> i

preludeSpan :: Span
preludeSpan = mkSpan "prelude" (-1) (-1) (-1) (-1)

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: String -> String -> IO CommandResult
runSubprocess image cmd = do
  hPutStrLn IO.stderr $ T.pack $ "> " <> showCreateProcessForUser (shell cmd)
  (_, Just out, Just err, ps) <-
    createProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe, delegate_ctlc = True}
  stdout' <- newEmptyMVar
  _ <- forkIO $ do
    stdout'' <- hPutAndGetContents ("[" <> image <> "(stdout)]") out
    putMVar stdout' stdout''
  stderr' <- newEmptyMVar
  _ <- forkIO $ do
    stderr'' <- hPutAndGetContents ("[" <> image <> "(stderr)]") err
    putMVar stderr' stderr''
  code <- waitForProcess ps
  hClose out
  hClose err
  CommandResult code <$> readMVar stdout' <*> readMVar stderr'

hPutAndGetContents :: String -> Handle -> IO String
hPutAndGetContents = hPutAndGetContents' ""
  where
    hPutAndGetContents' :: String -> String -> Handle -> IO String
    hPutAndGetContents' str console handle =
      do
        isClosed <- hIsClosed handle
        if isClosed
          then pure str
          else do
            isEof <- hIsEOF handle
            if isEof
              then pure str
              else do
                l <- hPutAndGetLine console handle
                hPutAndGetContents' (str <> l) console handle
    hPutAndGetLine :: String -> Handle -> IO String
    hPutAndGetLine console handle = do
      l <- hGetLine handle
      hPutStrLn IO.stderr $ T.pack console <> T.pack " " <> l
      pure $ T.unpack l

-- TODO: Implement by calling the Docker Engine API via Unix Socket.
-- This module should normally be implemented by making a request to the Docker Engine API via a Unix Socket.
-- However, it is not easy to implement HTTP communication over Unix Socket in Haskell.
-- The existing [docker-hs](https://hackage.haskell.org/package/docker) package is described
-- to configure Docker to communicate over TCP to avoid this problem.
-- The [http-client](https://hackage.haskell.org/package/http-client-0.7.17) package provides
-- a low-level API for HTTP but does not appear to support Unix Sockets.
runContainer :: String -> String -> IO CommandResult
runContainer image args = do
  -- TODO: Implement better default volume mounts and user-customizable methods.
  currentDir <- getCurrentDirectory
  let volume = currentDir <> ":/work"
  runSubprocess image $ "docker run --rm -v " <> volume <> " -w /work " <> image <> " " <> args
