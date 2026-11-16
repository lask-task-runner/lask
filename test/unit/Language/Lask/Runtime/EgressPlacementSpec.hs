{-# LANGUAGE OverloadedStrings #-}

-- | Placing a container given @egress@ (spec 10.12) against an engine
-- whose client answers from a script: what the networks and the proxy
-- are made with, for each engine, how each failure on the way is
-- reported and undone, how long the proxy is waited for, and what is
-- swept of an invocation that is gone.
--
-- The placements of a process, and whether it has swept, are kept for
-- its whole life, so every example uses entries of its own, and the
-- sweep is the first example: it is what the first placement of the
-- process does.
module Language.Lask.Runtime.EgressPlacementSpec (spec) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Access (parseEgressEntry)
import Language.Lask.ErrorCode
import Language.Lask.Runtime.Container
import Language.Lask.Runtime.Egress
import Language.Lask.Runtime.Value
import Support.FakeClient (withEnvVars)
import System.Posix.Process (getProcessID)
import System.Process (getPid, readProcess, spawnProcess, waitForProcess)
import Test.Hspec

-- | How the scripted client answers: by the words an invocation starts
-- with, the first rule that matches.
type Script = [([String], Either Text Text)]

-- | A backend whose client answers by the script, recording each
-- invocation, and whose store holds the proxy image or not.
scripted :: Engine -> Bool -> Script -> IO (ContainerBackend, IORef [[String]])
scripted engine present script = do
  calls <- newIORef []
  let client args = do
        atomicModifyIORef' calls (\cs -> (cs <> [args], ()))
        pure (maybe (Right "") snd (lookupRule args))
      lookupRule args = case [r | r@(ws, _) <- script, ws `isPrefixOf` args] of
        r : _ -> Just r
        [] -> Nothing
  pure ((cliBackend engine) {cbClient = client, cbImageExists = const (pure present)}, calls)

ready :: Script
ready = [(["logs"], Right "{\"event\":\"ready\"}\n")]

place :: ContainerBackend -> [Text] -> IO (Either LaskFailure Placement)
place b es = placeEgress b [e | Right e <- map parseEgressEntry es]

failure :: Either LaskFailure a -> Maybe (Maybe ErrorCode, Text)
failure = either (\lf -> Just (lfCode lf, messageOf lf)) (const Nothing)
  where
    messageOf lf = case lfError lf of
      VRecord m | Just (VString t) <- Map.lookup "message" m -> t
      _ -> ""

withProxy :: IO a -> IO a
withProxy = withEnvVars [("LASK_EGRESS_PROXY_IMAGE", Just "proxy:test")]

spec :: Spec
spec = do
  describe "sweeping what a gone invocation left (spec 10.12)" $
    it "removes the proxies and networks of a process on this host that is gone, and keeps the rest" $ withProxy $ do
      host <- takeWhile (`notElem` ("\r\n" :: String)) <$> readProcess "hostname" [] ""
      me <- fromIntegral <$> getProcessID
      ph <- spawnProcess "true" []
      Just gonePid <- getPid ph
      _ <- waitForProcess ph
      let owner p = T.pack host <> ":" <> T.pack (show (p :: Int))
          gone = owner (fromIntegral gonePid)
          live = owner me
      (b, calls) <-
        scripted
          Docker
          False
          [ (["ps"], Right (T.unlines ["lask-egress-gone " <> gone, "lask-egress-live " <> live, "lask-egress-far elsewhere:1", "lask-egress-odd " <> T.pack host <> ":x"])),
            (["network", "ls"], Right "lask-egress-gone-in\nlask-egress-live-in\nunlabelled\n"),
            (["network", "inspect", "lask-egress-gone-in"], Right ("{\"io.lask.egress.owner\":\"" <> gone <> "\"}")),
            (["network", "inspect", "lask-egress-live-in"], Right ("{\"io.lask.egress.owner\":\"" <> live <> "\"}")),
            (["network", "inspect"], Right "null")
          ]
      r <- place b ["sweep.example:443"]
      fmap fst (failure r) `shouldBe` Just (Just EIoImageMissing)
      cs <- readIORef calls
      if not (any (["ps"] `isPrefixOf`) cs)
        then pendingWith "another example of this process placed a container first, which swept"
        else do
          [c | c <- cs, ["rm"] `isPrefixOf` c] `shouldBe` [["rm", "--force", "lask-egress-gone"]]
          [c | c <- cs, ["network", "rm"] `isPrefixOf` c] `shouldBe` [["network", "rm", "lask-egress-gone-in"]]

  describe "the proxy image (spec 10.12)" $ do
    it "is the one LASK_EGRESS_PROXY_IMAGE names, and the release's when it is empty" $ do
      withEnvVars [("LASK_EGRESS_PROXY_IMAGE", Just " proxy:dev ")] $ proxyImage `shouldReturn` Right "proxy:dev"
      withEnvVars [("LASK_EGRESS_PROXY_IMAGE", Just " ")] $
        proxyImage >>= (`shouldSatisfy` either (const False) ("lask-egress-proxy@sha256:" `T.isInfixOf`))
    it "is reported missing, and nothing is made, when the store does not hold it" $ withProxy $ do
      (b, calls) <- scripted Docker False ready
      r <- place b ["missing.example:443"]
      failure r `shouldBe` Just (Just EIoImageMissing, "the egress proxy image proxy:test is not materialized; run 'lask sync'")
      readIORef calls `shouldReturn` []

  describe "making the networks and the proxy (spec 10.12)" $ do
    it "makes an internal network, an outside one and the proxy on both, for docker" $ withProxy $ do
      (b, calls) <- scripted Docker True ready
      r <- place b ["b.example:443", "a.example:80", "a.example:80"]
      p <- either (\lf -> fail (show lf)) pure r
      cs <- readIORef calls
      let creates = [c | c <- cs, ["network", "create"] `isPrefixOf` c]
          runs = [c | c <- cs, ["run"] `isPrefixOf` c]
      map (take 3) creates `shouldBe` [["network", "create", "--internal"], ["network", "create", "--label"]]
      map last creates `shouldBe` [T.unpack (plInside p), T.unpack (plOutside p)]
      case runs of
        [run] -> do
          take 5 run `shouldBe` ["run", "-d", "--pull=never", "--name", T.unpack (plProxy p)]
          run `shouldSatisfy` isInfix ["--network", "name=" <> T.unpack (plInside p) <> ",alias=lask-egress-proxy", "--network", T.unpack (plOutside p)]
          -- The entries once each, in order, after the image.
          dropWhile (/= "proxy:test") run `shouldBe` ["proxy:test", "-listen", ":3128", "a.example:80", "b.example:443"]
        other -> expectationFailure ("runs: " <> show other)
    it "uses the placement it made for the same entries, asking the client nothing" $ withProxy $ do
      (b, _) <- scripted Docker True ready
      first <- place b ["same.example:443"]
      (b', calls') <- scripted Docker True ready
      again <- place b' ["same.example:443"]
      fmap plProxy again `shouldBe` fmap plProxy first
      readIORef calls' `shouldReturn` []
    it "finds the proxy by an alias with podman, and by its host name with nerdctl" $ withProxy $ do
      (pb, pcalls) <- scripted Podman True ready
      Right pp <- place pb ["podman.example:443"]
      prun <- concat . filter (["run"] `isPrefixOf`) <$> readIORef pcalls
      prun `shouldSatisfy` isInfix ["--network", T.unpack (plInside pp) <> ":alias=lask-egress-proxy", "--network", T.unpack (plOutside pp)]
      (nb, ncalls) <- scripted Nerdctl True ready
      Right np <- place nb ["nerdctl.example:443"]
      nrun <- concat . filter (["run"] `isPrefixOf`) <$> readIORef ncalls
      nrun `shouldSatisfy` isInfix ["--network", T.unpack (plInside np), "--network", T.unpack (plOutside np), "--hostname", "lask-egress-proxy"]

  describe "a placement that fails (spec 10.12)" $ do
    it "says the engine could not make the internal network, and has nothing to undo" $ withProxy $ do
      (b, calls) <- scripted Docker True ([(["network", "create", "--internal"], Left "rootless networks need slirp")] <> ready)
      r <- place b ["inside.example:443"]
      failure r `shouldBe` Just (Just EIoEnvResolve, "egress needs a network with no route out, which docker could not create: rootless networks need slirp")
      cs <- readIORef calls
      [c | c <- cs, ["rm"] `isPrefixOf` c || ["network", "rm"] `isPrefixOf` c] `shouldBe` []
    it "removes the inside network when the outside one cannot be made" $ withProxy $ do
      (b, calls) <- scripted Docker True ([(["network", "create", "--label"], Left "address pools exhausted")] <> ready)
      r <- place b ["outside.example:443"]
      failure r `shouldBe` Just (Just EIoEnvResolve, "cannot create the outside network of egress: address pools exhausted")
      cs <- readIORef calls
      length [c | c <- cs, ["network", "rm"] `isPrefixOf` c] `shouldBe` 2
    it "removes both networks when the proxy does not start" $ withProxy $ do
      (b, calls) <- scripted Docker True ([(["run"], Left "port in use")] <> ready)
      r <- place b ["start.example:443"]
      failure r `shouldBe` Just (Just EIoEnvResolve, "the egress proxy did not start: port in use")
      cs <- readIORef calls
      [take 2 c | c <- cs, ["rm"] `isPrefixOf` c || ["network", "rm"] `isPrefixOf` c]
        `shouldBe` [["rm", "--force"], ["network", "rm"], ["network", "rm"]]
    it "says the proxy exited, with what it logged, and removes what was made" $ withProxy $ do
      (b, calls) <- scripted Docker True [(["logs"], Right "bad flag -listen\n"), (["inspect"], Right "false\n")]
      r <- place b ["exited.example:443"]
      failure r `shouldBe` Just (Just EIoEnvResolve, "the egress proxy did not start: it exited: bad flag -listen")
      cs <- readIORef calls
      length [c | c <- cs, ["network", "rm"] `isPrefixOf` c] `shouldBe` 2
    it "gives up on a proxy that says nothing within ten seconds" $ withProxy $ do
      (b, _) <- scripted Docker True [(["logs"], Right "starting\n"), (["inspect"], Right "true\n")]
      r <- place b ["silent.example:443"]
      failure r `shouldBe` Just (Just EIoEnvResolve, "the egress proxy did not start: it said nothing within 10 seconds: starting")

  describe "what the proxy refused (spec 10.12, 12.3)" $
    it "reads the destinations refused after a mark, each once" $ withProxy $ do
      logs <- newIORef ("{\"event\":\"ready\"}\n" :: Text)
      calls <- newIORef []
      let b = (cliBackend Docker) {cbImageExists = const (pure True), cbClient = \args -> do
                atomicModifyIORef' calls (\cs -> (cs <> [args :: [String]], ()))
                if take 1 args == ["logs"] then Right <$> readIORef logs else pure (Right "")}
      Right p <- place b ["denials.example:443"]
      mark <- denialMark p
      atomicModifyIORef' logs (\l -> (l <> "{\"event\":\"denied\",\"destination\":\"evil.example:443\"}\nnot json\n{\"event\":\"denied\",\"destination\":\"evil.example:443\"}\n{\"event\":\"allowed\",\"destination\":\"denials.example:443\"}\n", ()))
      denialsSince p mark `shouldReturn` ["evil.example:443"]
      denialsSince p 99 `shouldReturn` []

  describe "the end of an invocation (spec 10.12)" $
    it "removes the proxy and both networks of every placement" $ withProxy $ do
      (b, calls) <- scripted Docker True ready
      Right p <- place b ["removed.example:443"]
      removeEgress
      cs <- readIORef calls
      [c | c <- cs, ["rm"] `isPrefixOf` c || ["network", "rm"] `isPrefixOf` c]
        `shouldBe` [["rm", "--force", T.unpack (plProxy p)], ["network", "rm", T.unpack (plInside p)], ["network", "rm", T.unpack (plOutside p)]]
  where
    isInfix needle hay = any (needle `isPrefixOf`) (tailsOf hay)
    tailsOf xs = case xs of
      [] -> [[]]
      _ : rest -> xs : tailsOf rest
