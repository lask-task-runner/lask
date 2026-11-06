{-# LANGUAGE OverloadedStrings #-}

-- | Where a container given @egress@ is placed (spec 10.12): on a
-- network with no route out, beside a proxy that is on that network and
-- on one with a route, and forwards only what @egress@ lists.
--
-- The networks and the proxy are made at the first use that needs
-- them, shared by every use of the invocation with the same entries,
-- and removed when it ends ('removeEgress'). They are labelled with the
-- process that made them, so that what a process that could not remove
-- them left is removed by a later one ('sweep').
module Language.Lask.Runtime.Egress
  ( egressOf,
    proxyImage,
    proxyHost,
    proxyPort,
    Placement (..),
    placeEgress,
    placedOptions,
    denialMark,
    denialsSince,
    removeEgress,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Language.Lask.Access (EgressEntry, parseEgressEntry, renderEgressEntry)
import Language.Lask.ErrorCode (ErrorCode (..))
import Language.Lask.Runtime.Container (ContainerBackend (..), Engine (..), engineName)
import Language.Lask.Runtime.EgressProxy (embeddedProxyImage)
import Language.Lask.Runtime.ProcessStop (liveOf)
import Language.Lask.Runtime.Value (LaskFailure, Value (..), ioFailure, runtimeFailure)
import System.Entropy (getEntropy)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as P

-- | The entries of a run's @egress@, 'Nothing' when it is not given
-- (spec 10.12). An entry that is not one, or @egress@ given with
-- @network@ or @publish@, is @E-RUNTIME-VALUE@.
egressOf :: Map Text Value -> Either LaskFailure (Maybe [EgressEntry])
egressOf opts = case Map.lookup "egress" opts of
  Nothing -> Right Nothing
  Just VNull -> Right Nothing
  Just (VArray xs) -> do
    es <- mapM entry [x | x <- V.toList xs, x /= VNull]
    case [k | k <- ["network", "publish"], Just v <- [Map.lookup k opts], given v] of
      k : _ -> Left (invalid ("'" <> k <> "' is given with egress, which provides the network"))
      [] -> Right (Just es)
  Just _ -> Left (invalid "egress takes an array of <host>:<port>")
  where
    entry x = case x of
      VString t -> either (Left . invalid) Right (parseEgressEntry t)
      _ -> Left (invalid "an entry of egress is a string, <host>:<port>")
    given v = case v of
      VNull -> False
      VArray xs -> any (/= VNull) (V.toList xs)
      _ -> True
    invalid = runtimeFailure ERuntimeValue

-- | The name, and the port, a container given egress reaches its proxy
-- by, the same in every one (spec 10.12).
proxyHost :: Text
proxyHost = "lask-egress-proxy"

proxyPort :: Int
proxyPort = 3128

-- | The proxy image: @LASK_EGRESS_PROXY_IMAGE@, for a build of Lask
-- under development, or the one this release names.
proxyImage :: IO (Either Text Text)
proxyImage = do
  override <- lookupEnv "LASK_EGRESS_PROXY_IMAGE"
  pure $ case T.strip . T.pack <$> override of
    Just t | not (T.null t) -> Right t
    _ -> maybe (Left "this build of lask names no egress proxy image; set LASK_EGRESS_PROXY_IMAGE to one") Right embeddedProxyImage

-- | The networks and the proxy of one @egress@.
data Placement = Placement
  { plInside :: Text,
    plOutside :: Text,
    plProxy :: Text,
    plBackend :: ContainerBackend
  }

-- | The placements of this process, by their entries, as written back.
placements :: MVar (Map [Text] Placement)
placements = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE placements #-}

swept :: IORef Bool
swept = unsafePerformIO (newIORef False)
{-# NOINLINE swept #-}

ownerLabel :: Text
ownerLabel = "io.lask.egress.owner"

-- | This process, as its labels name it: @<host>:<pid>@.
owner :: IO Text
owner = do
  host <- getHostName
  pid <- P.getCurrentPid
  pure (T.pack host <> ":" <> T.pack (show pid))

-- | The placement for these entries, made if this invocation has none.
placeEgress :: ContainerBackend -> [EgressEntry] -> IO (Either LaskFailure Placement)
placeEgress backend entries = modifyMVar placements $ \m -> do
  let key = sort (nub (map renderEgressEntry entries))
  case Map.lookup key m of
    Just p -> pure (m, Right p)
    Nothing -> do
      first <- atomicModifyIORef' swept (\s -> (True, not s))
      when first (sweep backend)
      r <- make backend key
      pure $ case r of
        Right p -> (Map.insert key p m, Right p)
        Left e -> (m, Left e)

make :: ContainerBackend -> [Text] -> IO (Either LaskFailure Placement)
make backend key = do
  imageE <- proxyImage
  case imageE of
    Left e -> pure (Left (ioFailure EIoEnvResolve e))
    Right image -> do
      present <- cbImageExists backend image
      if not present
        then pure (Left (ioFailure EIoImageMissing ("the egress proxy image " <> image <> " is not materialized; run 'lask sync'")))
        else do
          suffix <- TE.decodeUtf8 . B16.encode <$> getEntropy 6
          who <- owner
          let base = "lask-egress-" <> suffix
              inside = base <> "-in"
              outside = base <> "-out"
              labels = ["--label", T.unpack (ownerLabel <> "=" <> who)]
              client = cbClient backend
              undo = removePieces backend [base] [inside, outside]
              engineText = engineName (cbEngine backend)
          r1 <- client (["network", "create", "--internal"] <> labels <> [T.unpack inside])
          case r1 of
            Left e -> pure (Left (ioFailure EIoEnvResolve ("egress needs a network with no route out, which " <> engineText <> " could not create: " <> e)))
            Right _ -> do
              r2 <- client (["network", "create"] <> labels <> [T.unpack outside])
              case r2 of
                Left e -> undo >> pure (Left (ioFailure EIoEnvResolve ("cannot create the outside network of egress: " <> e)))
                Right _ -> do
                  let args =
                        ["run", "-d", "--pull=never", "--name", T.unpack base]
                          <> labels
                          <> networkArgs (cbEngine backend) inside outside
                          <> [T.unpack image, "-listen", ":" <> show proxyPort]
                          <> map T.unpack key
                  r3 <- client args
                  case r3 of
                    Left e -> undo >> pure (Left (ioFailure EIoEnvResolve ("the egress proxy did not start: " <> e)))
                    Right _ -> do
                      ready <- awaitReady backend base (100 :: Int)
                      case ready of
                        Left e -> undo >> pure (Left (ioFailure EIoEnvResolve ("the egress proxy did not start: " <> e)))
                        Right () -> pure (Right (Placement inside outside base backend))

-- | The proxy is started on both networks at once, nerdctl having no
-- way to add one later, and is found on the inside one by
-- 'proxyHost': an alias of that network for Docker and Podman, and its
-- host name for nerdctl, whose containers on one network know each
-- other's host names.
networkArgs :: Engine -> Text -> Text -> [String]
networkArgs engine inside outside = case engine of
  Docker -> ["--network", T.unpack ("name=" <> inside <> ",alias=" <> proxyHost), "--network", T.unpack outside]
  Podman -> ["--network", T.unpack (inside <> ":alias=" <> proxyHost), "--network", T.unpack outside]
  Nerdctl -> ["--network", T.unpack inside, "--network", T.unpack outside, "--hostname", T.unpack proxyHost]

-- | Wait, 100ms at a time, for the proxy to say it listens.
awaitReady :: ContainerBackend -> Text -> Int -> IO (Either Text ())
awaitReady backend name ticks = do
  r <- cbClient backend ["logs", T.unpack name]
  case r of
    Right out | any ((== Just "ready") . eventOf) (T.lines out) -> pure (Right ())
    _
      | ticks <= 0 -> pure (Left (either id (\out -> "it said nothing within 10 seconds" <> if T.null (T.strip out) then "" else ": " <> T.strip out) r))
      | otherwise -> do
          running <- cbClient backend ["inspect", "--format", "{{.State.Running}}", T.unpack name]
          case running of
            Right t | T.strip t == "false" -> do
              logs <- cbClient backend ["logs", T.unpack name]
              pure (Left ("it exited: " <> either id T.strip logs))
            _ -> threadDelay 100000 >> awaitReady backend name (ticks - 1)

-- | The run options of a container placed with the proxy: on the inside
-- network, with the variables that name the proxy, where the program's
-- own @env@ does not set them (spec 10.12).
placedOptions :: Placement -> Map Text Value -> Map Text Value
placedOptions p opts =
  Map.insert "network" (VString (plInside p)) (Map.insert "env" (VMap (Map.union own given)) (Map.delete "egress" opts))
  where
    own = case Map.lookup "env" opts of
      Just (VMap m) -> m
      _ -> Map.empty
    url = VString ("http://" <> proxyHost <> ":" <> T.pack (show proxyPort))
    noProxy = VString "localhost,127.0.0.1,::1"
    given =
      Map.fromList
        [ ("HTTP_PROXY", url),
          ("HTTPS_PROXY", url),
          ("http_proxy", url),
          ("https_proxy", url),
          ("NO_PROXY", noProxy),
          ("no_proxy", noProxy),
          ("NODE_USE_ENV_PROXY", VString "1")
        ]

-- | How far the proxy's log has gone, to read what is refused after.
denialMark :: Placement -> IO Int
denialMark p = either (const 0) (length . T.lines) <$> cbClient (plBackend p) ["logs", T.unpack (plProxy p)]

-- | The destinations refused since a mark, as @<host>:<port>@.
denialsSince :: Placement -> Int -> IO [Text]
denialsSince p mark = do
  r <- cbClient (plBackend p) ["logs", T.unpack (plProxy p)]
  pure $ case r of
    Left _ -> []
    Right out -> nub (mapMaybe denied (drop mark (T.lines out)))
  where
    denied l = case A.decode (BL.fromStrict (TE.encodeUtf8 l)) :: Maybe (Map Text A.Value) of
      Just o
        | Just (A.String "denied") <- Map.lookup "event" o,
          Just (A.String d) <- Map.lookup "destination" o ->
            Just d
      _ -> Nothing

eventOf :: Text -> Maybe Text
eventOf l = case A.decode (BL.fromStrict (TE.encodeUtf8 l)) :: Maybe (Map Text A.Value) of
  Just m | Just (A.String e) <- Map.lookup "event" m -> Just e
  _ -> Nothing

-- | Remove every placement of this process: when the invocation ends.
removeEgress :: IO ()
removeEgress = do
  m <- modifyMVar placements (\m -> pure (Map.empty, m))
  forM_ (Map.elems m) $ \p -> removePieces (plBackend p) [plProxy p] [plInside p, plOutside p]

removePieces :: ContainerBackend -> [Text] -> [Text] -> IO ()
removePieces backend containers networks = do
  unless (null containers) $ void (cbClient backend (["rm", "--force"] <> map T.unpack containers))
  forM_ networks $ \n -> void (cbClient backend ["network", "rm", T.unpack n])

-- | Remove what an invocation on this host whose process is gone left:
-- the proxies first, then the networks they were on.
sweep :: ContainerBackend -> IO ()
sweep backend = void (try go :: IO (Either SomeException ()))
  where
    go = do
      host <- T.pack <$> getHostName
      cs <- labelled ["ps", "-a", "--filter", "label=" <> T.unpack ownerLabel, "--format", "{{.Names}} {{.Label \"" <> T.unpack ownerLabel <> "\"}}"]
      ns <- labelled ["network", "ls", "--filter", "label=" <> T.unpack ownerLabel, "--format", "{{.Name}} {{.Label \"" <> T.unpack ownerLabel <> "\"}}"]
      let pids = [p | (_, o) <- cs <> ns, Just p <- [pidOn host o]]
      live <- liveOf (nub pids)
      let gone o = maybe False (`notElem` live) (pidOn host o)
      removePieces backend [n | (n, o) <- cs, gone o] [n | (n, o) <- ns, gone o]
    labelled args = do
      r <- cbClient backend args
      pure $ case r of
        Right out -> [(n, T.strip o) | l <- T.lines out, let (n, o) = T.breakOn " " (T.strip l), not (T.null n)]
        Left _ -> []
    pidOn host o = case T.breakOnEnd ":" o of
      (h, p) | T.dropEnd 1 h == host, [(n, "")] <- reads (T.unpack p) -> Just (n :: Int)
      _ -> Nothing

-- | The name of this host, as @hostname@ prints it, which every
-- platform Lask runs on has.
getHostName :: IO String
getHostName = do
  r <- try (P.readProcess "hostname" [] "") :: IO (Either SomeException String)
  pure (either (const "unknown") (takeWhile (`notElem` ("\r\n" :: String))) r)
