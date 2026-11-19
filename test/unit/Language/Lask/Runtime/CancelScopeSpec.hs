-- | Cancellation scopes (spec 15.7): what a body starts with @async@,
-- however deeply, is cancelled with it.
module Language.Lask.Runtime.CancelScopeSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait)
import Control.Exception (onException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Language.Lask.Runtime.CancelScope
import Language.Lask.Runtime.Value (Value (..))
import Test.Hspec

-- | A computation that waits a second, recording whether it was
-- stopped first.
sleeper :: IORef [String] -> String -> IO Value
sleeper seen tag = do
  threadDelay 1000000 `onException` atomicModifyIORef' seen (\xs -> (xs <> ["stop " <> tag], ()))
  atomicModifyIORef' seen (\xs -> (xs <> ["end " <> tag], ()))
  pure VVoid

spec :: Spec
spec = describe "cancellation scopes" $ do
  it "cancels what was started inside it" $ do
    seen <- newIORef []
    withScope $ \sid -> do
      _ <- spawnInScopes (sleeper seen "a")
      threadDelay 20000
      members <- cancelScope sid
      length members `shouldBe` 1
    readIORef seen `shouldReturn` ["stop a"]

  it "cancels what a computation started inside it started in turn" $ do
    seen <- newIORef []
    withScope $ \sid -> do
      outer <- spawnInScopes $ do
        inner <- spawnInScopes (sleeper seen "inner")
        wait inner
      threadDelay 20000
      _ <- cancelScope sid
      _ <- poll outer
      pure ()
    threadDelay 50000
    readIORef seen `shouldReturn` ["stop inner"]

  it "leaves alone what was started outside it" $ do
    seen <- newIORef []
    outside <- spawnInScopes (sleeper seen "outside")
    withScope $ \sid -> do
      _ <- spawnInScopes (sleeper seen "inside")
      threadDelay 20000
      _ <- cancelScope sid
      pure ()
    _ <- wait outside
    readIORef seen `shouldReturn` ["stop inside", "end outside"]

  it "cancels an inner scope's members with the outer one, and not the reverse" $ do
    seen <- newIORef []
    withScope $ \outerId -> do
      _ <- spawnInScopes (sleeper seen "outer")
      withScope $ \innerId -> do
        _ <- spawnInScopes (sleeper seen "inner")
        threadDelay 20000
        _ <- cancelScope innerId
        threadDelay 20000
        readIORef seen `shouldReturn` ["stop inner"]
      _ <- cancelScope outerId
      pure ()
    readIORef seen `shouldReturn` ["stop inner", "stop outer"]

  it "has no members once it is closed" $ do
    sid <- withScope $ \sid -> do
      a <- spawnInScopes (pure VVoid)
      _ <- wait a
      pure sid
    length <$> cancelScope sid `shouldReturn` 0

  it "lets a member that has finished be cancelled without effect" $
    withScope $ \sid -> do
      a <- spawnInScopes (pure (VBool True))
      _ <- wait a
      _ <- cancelScope sid
      r <- poll a
      isJust r `shouldBe` True
