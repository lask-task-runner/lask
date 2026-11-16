{-# LANGUAGE OverloadedStrings #-}

-- | Laws of the access checker (spec 10.11, 10.12) over generated
-- constraints, access sets and entries: narrowing a constraint never
-- admits more, widening what a use reaches never admits more, and what
-- is written two ways that mean one thing is judged one way.
--
-- The corpus (test/unit/corpus/within) pins programs; these pin the
-- relation the programs are judged by, over more of it than programs
-- would cover.
module Language.Lask.AccessSpec (spec) where

import Data.List (inits, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Access
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (const 500) $ do
  describe "egress entries (spec 10.12, Entries)" $ do
    prop "an entry covers itself" $ \(E e) -> egressCovers e e
    it "covers transitively, over every name and wildcard of up to three labels" $
      let names = [T.intercalate "." ls | n <- [1 .. 3], ls <- mapM (const ["a", "b"]) [1 .. n :: Int]]
          es = [EgressEntry h 443 | n <- names, h <- [EgressName n, EgressWildcard n]]
       in [(a, c) | a <- es, b <- es, egressCovers a b, c <- es, egressCovers b c, not (egressCovers a c)] `shouldBe` []
    prop "no entry covers one on another port" $ \(E a) (E b) ->
      egPort a /= egPort b ==> not (egressCovers a b)
    prop "a wildcard never covers its own domain" $ \(Name d) p ->
      not (egressCovers (EgressEntry (EgressWildcard d) (port p)) (EgressEntry (EgressName d) (port p)))
    prop "what is rendered reads back as the same entry" $ \(E e) ->
      parseEgressEntry (renderEgressEntry e) === Right e
    it "reads a name in any case and with a final dot as the same name" $
      parseEgressEntry "Registry.NPMjs.org.:443" `shouldBe` parseEgressEntry "registry.npmjs.org:443"

  describe "inbound entries (spec 10.11)" $
    prop "what is rendered reads back as the same entry" $ \(I e) ->
      parseInboundEntry (renderInboundEntry e) === Right e

  describe "host-equivalent paths (spec 10.11, Access sets)" $ do
    prop "a path written with ., .. and // means the path it resolves to" $ \(P p) (Noise n) ->
      hostEquivalent (VolumeHost (noisy n p)) === hostEquivalent (VolumeHost p)
    prop "what is under /proc, /sys or /dev, or under an engine's state, is the host" $ \(P rest) ->
      conjoin [hostEquivalent (VolumeHost (base <> rest)) | base <- ["/proc", "/sys", "/dev", "/var/lib/docker", "/run/containerd", "/etc"]]
    it "is the host for every directory that holds a fixed path of an engine or of the host" $
      [p | s <- ["/var/run/docker.sock", "/run/podman", "/var/lib/containerd", "/root"], p <- ancestors s, not (hostEquivalent (VolumeHost p))] `shouldBe` []
    prop "a path ending in an engine's socket is the host, wherever it is" $ \(P p) ->
      conjoin [hostEquivalent (VolumeHost (p <> s)) | s <- ["/docker.sock", "/podman.sock", "/containerd.sock", "/buildkitd.sock"]]
    it "is the host for the directory a rootless engine keeps its socket in" $ do
      pendingWith
        "known gap: /run/user/<uid> holds a rootless engine's docker.sock, which only a suffix names, so 'any directory that holds one of those' cannot see it; volumes: [\"/run/user/1000:/r\"] passes runs: [\"container\"]"
      hostEquivalent (VolumeHost "/run/user/1000") `shouldBe` True

  describe "checking a use against a constraint (spec 10.11, Constraints)" $ do
    prop "no constraint finds nothing" $ \(A a) -> checkAccess noConstraint a === []
    prop "the host exceeds every component a constraint gives but secrets" $ \(C c) ->
      let found = nub (map findingField (checkAccess c (HostAccess "#local")))
          given = [k | (k, True) <- [("files", has (conFiles c)), ("volumes", has (conVolumes c)), ("outbound", has (conOutbound c)), ("inbound", has (conInbound c))]]
       in all (`elem` found) given
    prop "a narrower constraint never admits what a wider one does not" $ \(C c) (Narrow n) (A a) ->
      let narrow = narrowBy n c
       in not (null (checkAccess c a)) ==> not (null (checkAccess narrow a))
    prop "a use that reaches more is never admitted where a use that reaches less is not" $ \(C c) (A a) (Widen w) ->
      not (null (checkAccess c a)) ==> not (null (checkAccess c (widenBy w a)))
    prop "an outbound class admits every narrower one" $
      forAll arbitraryBoundedEnum $ \b -> forAll arbitraryBoundedEnum $ \r ->
      let found = checkAccess noConstraint {conOutbound = Just (BoundClass b)} (onlyNet (NetReach r Nothing))
       in (r <= b) === null found
    prop "an egress bound admits a use whose entries it covers, and no other" $ \(Es bs) (Es us) ->
      let found = checkAccess noConstraint {conOutbound = Just (BoundEgress bs)} (onlyNet (NetEgress us))
       in null found === all (\u -> any (`egressCovers` u) bs) us
    it "judges a volume by the path it resolves to, not by how it is written" $ do
      pendingWith
        "known bug: volumes are compared as written, so { \"/tmp/cache\": \"rw\" } admits \"/tmp/cache/../../home/me/.ssh\", which the engine mounts as /home/me/.ssh"
      let bound = noConstraint {conVolumes = Just (VolumesListed [(VolumeHost "/tmp/cache", ModeRw)])}
          use = AccessSet Nothing (Just [Volume (VolumeHost "/tmp/cache/../../home/me/.ssh") "/k" ModeRw]) Nothing Nothing
      map findingField (checkAccess bound use) `shouldBe` ["volumes"]

-- Generated values --------------------------------------------------------

newtype Name = Name Text deriving (Show)

instance Arbitrary Name where
  arbitrary = Name . T.intercalate "." <$> resize 3 (listOf1 domainLabel)
  shrink (Name n) = [Name (T.intercalate "." ls) | ls <- shrinkList (const []) (T.splitOn "." n), not (null ls)]

domainLabel :: Gen Text
domainLabel = elements ["api", "github", "com", "registry", "npmjs", "org", "a", "b"]

port :: Bool -> Int
port p = if p then 443 else 80

newtype E = E EgressEntry deriving (Show)

instance Arbitrary E where
  arbitrary = do
    host <-
      frequency
        [ (4, EgressName <$> name),
          (3, EgressWildcard <$> name),
          (1, EgressAddr4 <$> vectorOf 4 (chooseInt (0, 255)))
        ]
    E . EgressEntry host <$> elements [80, 443]
    where
      name = (\(Name n) -> n) <$> arbitrary

newtype Es = Es [EgressEntry] deriving (Show)

instance Arbitrary Es where
  arbitrary = Es . map (\(E e) -> e) <$> resize 4 (listOf arbitrary)
  shrink (Es es) = Es <$> shrinkList (const []) es

newtype I = I InboundEntry deriving (Show)

instance Arbitrary I where
  arbitrary = do
    addr <- elements [BindAddr4 [127, 0, 0, 1], BindAddr4 [0, 0, 0, 0], BindAddr4 [10, 1, 2, 3], BindAddr6 [0, 0, 0, 0, 0, 0, 0, 1], BindAddr6 [0, 0, 0, 0, 0, 0, 0, 0]]
    ports <- oneof [pure Nothing, Just <$> range]
    pure (I (InboundEntry addr ports))
    where
      range = do
        lo <- chooseInt (1, 65000)
        hi <- chooseInt (lo, min 65535 (lo + 20))
        pure (lo, hi)

-- | An absolute path of a few components, without @.@ or @..@.
newtype P = P Text deriving (Show)

instance Arbitrary P where
  arbitrary = P . ("/" <>) . T.intercalate "/" <$> resize 4 (listOf component)
    where
      component = elements ["var", "run", "lib", "docker", "docker.sock", "tmp", "cache", "home", "me", "etc", "proc", "user", "1000"]

-- | How a path is rewritten without changing what it names.
newtype Noise = Noise [Int] deriving (Show)

instance Arbitrary Noise where
  arbitrary = Noise <$> resize 4 (listOf (chooseInt (0, 2)))

noisy :: [Int] -> Text -> Text
noisy ns p = "/" <> T.intercalate "/" (go ns (filter (not . T.null) (T.splitOn "/" p)))
  where
    go (n : rest) (c : cs) = case n of
      0 -> c : "." : go rest cs
      1 -> c : "" : go rest cs
      _ -> "x" : ".." : c : go rest cs
    go _ cs = cs

-- | Every directory above a path, the root included.
ancestors :: Text -> [Text]
ancestors p = ["/" <> T.intercalate "/" ds | ds <- init (inits (filter (not . T.null) (T.splitOn "/" p)))]

has :: Maybe a -> Bool
has = maybe False (const True)

-- Constraints and access sets ---------------------------------------------

newtype C = C Constraint deriving (Show)

instance Arbitrary C where
  arbitrary = do
    files <- maybeOf (table <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum)
    vols <- maybeOf (oneof [pure VolumesAny, VolumesListed <$> resize 2 (listOf ((,) <$> source <*> arbitraryBoundedEnum))])
    out <- maybeOf (oneof [BoundClass <$> elements [NetNone, NetDefault], (\(Es es) -> BoundEgress es) <$> arbitrary])
    inb <- maybeOf (oneof [pure InboundAny, InboundListed . map (\(I e) -> e) <$> resize 2 (listOf arbitrary)])
    runs <- maybeOf (sublistOf [ProfileLocal, ProfileContainer])
    pure (C (Constraint files vols out inb Nothing runs))
    where
      maybeOf g = oneof [pure Nothing, Just <$> g]
      table root dist = [(".", root), ("dist", dist)]

source :: Gen VolumeSource
source = oneof [VolumeHost <$> elements ["/tmp/cache", "/tmp/cache/npm", "/srv/data", "/srv"], VolumeNamed <$> elements ["cache", "data"]]

newtype A = A AccessSet deriving (Show)

instance Arbitrary A where
  arbitrary =
    frequency
      [ (1, pure (A (HostAccess "#local"))),
        ( 6,
          do
            files <- (\r d -> [(".", r), ("dist", d)]) <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum
            vols <- resize 2 (listOf (Volume <$> source <*> pure "/m" <*> elements [ModeRo, ModeRw]))
            net <- oneof [NetReach <$> arbitraryBoundedEnum <*> pure Nothing, (\(Es es) -> NetEgress es) <$> arbitrary]
            pub <- resize 2 (listOf published)
            pure (A (AccessSet (Just files) (Just vols) (Just net) (Just pub)))
        )
      ]
    where
      published = do
        addrs <- elements [[BindAddr4 [127, 0, 0, 1]], [BindAddr4 [0, 0, 0, 0], BindAddr6 (replicate 8 0)], [BindAddr6 [0, 0, 0, 0, 0, 0, 0, 1]]]
        p <- chooseInt (8000, 8010)
        Published addrs <$> elements [Nothing, Just (p, p)]

onlyNet :: NetReach -> AccessSet
onlyNet r = AccessSet Nothing Nothing (Just r) Nothing

-- | Ways to narrow a constraint: lower a mode, drop an admitted entry,
-- or give a component it left unbounded the narrowest bound.
newtype Narrow = Narrow [Int] deriving (Show)

instance Arbitrary Narrow where
  arbitrary = Narrow <$> resize 3 (listOf (chooseInt (0, 5)))

narrowBy :: [Int] -> Constraint -> Constraint
narrowBy ns c0 = foldl step c0 ns
  where
    step c n = case n of
      0 -> c {conFiles = Just (maybe [(".", ModeNone)] (map (fmap lower)) (conFiles c))}
      1 -> c {conVolumes = Just (case conVolumes c of Just (VolumesListed (_ : vs)) -> VolumesListed vs; Just VolumesAny -> VolumesListed []; _ -> VolumesListed [])}
      2 -> c {conOutbound = Just (case conOutbound c of Just (BoundEgress (_ : es)) -> BoundEgress es; Just (BoundClass NetDefault) -> BoundClass NetNone; Just b -> b; Nothing -> BoundClass NetNone)}
      3 -> c {conInbound = Just (case conInbound c of Just (InboundListed (_ : es)) -> InboundListed es; _ -> InboundListed [])}
      4 -> c {conRuns = Just (case conRuns c of Just (_ : ps) -> ps; _ -> [])}
      _ -> c
    lower m = if m == minBound then m else pred m

-- | Ways to widen what a use reaches: raise a mode, add a volume, a
-- wider network or an egress entry, or publish another port.
newtype Widen = Widen [Int] deriving (Show)

instance Arbitrary Widen where
  arbitrary = Widen <$> resize 3 (listOf (chooseInt (0, 4)))

widenBy :: [Int] -> AccessSet -> AccessSet
widenBy ns a0 = foldl step a0 ns
  where
    step a n = case a of
      HostAccess _ -> a
      AccessSet files vols net pub -> case n of
        0 -> AccessSet (map (fmap raise) <$> files) vols net pub
        1 -> AccessSet files ((Volume (VolumeHost "/srv/other") "/o" ModeRw :) <$> vols) net pub
        2 -> AccessSet files vols (wider <$> net) pub
        3 -> AccessSet files vols (moreEgress <$> net) pub
        _ -> AccessSet files vols net ((Published [BindAddr4 [0, 0, 0, 0]] (Just (9999, 9999)) :) <$> pub)
    raise m = if m == maxBound then m else succ m
    wider r = case r of
      NetReach c w -> NetReach (if c == maxBound then c else succ c) w
      NetEgress _ -> NetReach NetDefault Nothing
    moreEgress r = case r of
      NetEgress es -> NetEgress (EgressEntry (EgressName "elsewhere.example") 443 : es)
      _ -> r
