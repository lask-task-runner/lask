{-# LANGUAGE OverloadedStrings #-}

-- | How the options that enter an access set are read (spec 10.2,
-- 10.11, 10.12): every form an entry may take and every one it may
-- not, with the message each refusal gives. "Language.Lask.AccessSpec"
-- holds the laws; this holds the cases.
module Language.Lask.AccessParseSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Access
import Test.Hspec

entry :: Text -> Either Text Text
entry = fmap renderEgressEntry . parseEgressEntry

inbound :: Text -> Either Text Text
inbound = fmap renderInboundEntry . parseInboundEntry

v4 :: [Int] -> BindAddr
v4 = BindAddr4

v6 :: [Int] -> BindAddr
v6 = BindAddr6

everywhere :: [BindAddr]
everywhere = [v4 [0, 0, 0, 0], v6 (replicate 8 0)]

spec :: Spec
spec = do
  describe "modes (spec 10.2)" $ do
    it "reads the three modes, ordered by what they permit" $ do
      mapM parseMode ["none", "ro", "rw"] `shouldBe` Right [ModeNone, ModeRo, ModeRw]
      (ModeNone < ModeRo, ModeRo < ModeRw) `shouldBe` (True, True)
    it "refuses anything else, naming the three" $ do
      parseMode "readonly" `shouldBe` Left "'readonly' is not a mode: give \"rw\", \"ro\" or \"none\""
      parseMode "RW" `shouldSatisfy` isLeft
      parseMode "" `shouldSatisfy` isLeft
    it "writes each back as it is read" $
      map modeText [minBound .. maxBound] `shouldBe` ["none", "ro", "rw"]

  describe "base_mount (spec 10.2)" $ do
    it "accepts an absolute, normalized path" $
      mapM validateBaseMount ["/src", "/a/b/c", "/procx", "/devices", "/work"] `shouldBe` Right ["/src", "/a/b/c", "/procx", "/devices", "/work"]
    it "refuses a relative path" $ do
      validateBaseMount "src" `shouldBe` Left "'base_mount' must be an absolute path, such as \"/src\""
      validateBaseMount "" `shouldSatisfy` isLeft
    it "refuses the root" $
      validateBaseMount "/" `shouldBe` Left "'base_mount' must not be '/'"
    it "refuses an empty, '.' or '..' component and a trailing '/'" $
      mapM_ (\p -> validateBaseMount p `shouldBe` Left "'base_mount' must be normalized: no empty, '.' or '..' component and no trailing '/'") ["/a//b", "/a/./b", "/a/../b", "/a/", "/..", "//a"]
    it "refuses /proc, /sys and /dev, and what is under them" $
      mapM_ (\p -> validateBaseMount p `shouldBe` Left "'base_mount' must not be /proc, /sys or /dev, or under them") ["/proc", "/sys/fs", "/dev/shm"]
    it "refuses a comma, which would split the field of --mount" $
      validateBaseMount "/a,b" `shouldBe` Left "'base_mount' must not contain a comma"

  describe "base_access tables (spec 10.2)" $ do
    it "orders the keys shortest first, so that a longer one is mounted over" $
      accessTable [("a/b/c", "none"), ("a", "ro"), (".", "rw"), ("a/b", "rw")]
        `shouldBe` Right [(".", ModeRw), ("a", ModeRo), ("a/b", ModeRw), ("a/b/c", ModeNone)]
    it "requires \".\"" $
      accessTable [] `shouldBe` Left "a 'base_access' table must give \".\", the mode of every path no other key names"
    it "refuses a key outside the base directory or not normalized" $
      mapM_
        (\k -> accessTable [(".", "ro"), (k, "rw")] `shouldBe` Left ("'" <> k <> "' must be a path inside the base directory, written relative to it without '.' or '..'"))
        ["/etc", "../up", "a/../b", "./a", "a/", "a//b", ".."]
    it "refuses a key with a comma" $
      accessTable [(".", "ro"), ("a,b", "rw")] `shouldBe` Left "'a,b' must not contain a comma"
    it "refuses a mode that is not one" $
      accessTable [(".", "rx")] `shouldBe` Left "'rx' is not a mode: give \"rw\", \"ro\" or \"none\""
    it "gives a path the mode of the longest key that is it or holds it" $ do
      let t = [(".", ModeRo), ("dist", ModeRw), ("dist/secret", ModeNone)]
      map (modeAt t) ["dist", "dist/a.js", "dist/secret", "dist/secret/k", "distx", "src", "dist/secretx"]
        `shouldBe` [ModeRw, ModeRw, ModeNone, ModeNone, ModeRo, ModeRo, ModeRw]
    it "reads the mode alone as the table of \".\"" $
      accessScalar ModeRo `shouldBe` [(".", ModeRo)]
    it "splits a key into its components, none for \".\"" $ do
      keyParts "." `shouldBe` []
      keyParts "a/b" `shouldBe` ["a", "b"]

  describe "volumes, as --volume reads them (spec 10.11)" $ do
    it "reads a host path, a named volume and the mode" $ do
      parseVolume "/data:/d" `shouldBe` Just (Volume (VolumeHost "/data") "/d" ModeRw)
      parseVolume "cache:/c:ro" `shouldBe` Just (Volume (VolumeNamed "cache") "/c" ModeRo)
      parseVolume "/x:/y:z,ro" `shouldBe` Just (Volume (VolumeHost "/x") "/y" ModeRo)
      parseVolume "/x:/y:rw" `shouldBe` Just (Volume (VolumeHost "/x") "/y" ModeRw)
    it "reads an anonymous volume as holding nothing of the host" $
      parseVolume "/only-a-target" `shouldBe` Nothing
    it "reads a source beginning with '.' as a host path relative to the base directory, normalized" $ do
      fmap volSource (parseVolume "./data:/d") `shouldBe` Just (VolumeBase "data")
      fmap volSource (parseVolume "./../../var/run/docker.sock:/s") `shouldBe` Just (VolumeBase "../../var/run/docker.sock")
    it "reads a host path normalized" $
      fmap volSource (parseVolume "/tmp/cache/../../home/me/.ssh:/k") `shouldBe` Just (VolumeHost "/home/me/.ssh")

  describe "the network's class (spec 10.11)" $ do
    it "reads none, the default, a named network and the host" $
      map classifyNetwork [Nothing, Just "none", Just "bridge", Just "my_net-1.x", Just "host"]
        `shouldBe` [NetDefault, NetNone, NetDefault, NetDefault, NetHost]
    it "reads another container's or namespace's network as the host" $
      map classifyNetwork [Just "container:db", Just "ns:/proc/1/ns/net", Just "", Just "-x", Just "a b"]
        `shouldBe` replicate 5 NetHost
    it "writes each class back" $
      map netClassText [minBound .. maxBound] `shouldBe` ["none", "default", "host"]

  describe "egress entries (spec 10.12)" $ do
    it "reads a name without regard to case or a trailing dot" $
      mapM entry ["Example.COM:443", "example.com.:443", "xn--bcher-kva.example:443", "_srv.example.com:8443"]
        `shouldBe` Right ["example.com:443", "example.com:443", "xn--bcher-kva.example:443", "_srv.example.com:8443"]
    it "reads a wildcard over a domain" $
      mapM entry ["*.example.com:443", "*.Example.com.:443", "*.com:443"] `shouldBe` Right ["*.example.com:443", "*.example.com:443", "*.com:443"]
    it "refuses a wildcard anywhere but the leftmost label, or alone" $
      mapM_ (\h -> entry h `shouldSatisfy` isLeft) ["*:443", "a.*.com:443", "*.*.com:443", "**.com:443", "*example.com:443", "*.:443"]
    it "reads IPv4 and bracketed IPv6 addresses" $
      mapM entry ["10.0.0.1:80", "0.0.0.0:1", "255.255.255.255:65535", "[::1]:443", "[::]:80", "[2001:DB8::1]:443", "[1:2:3:4:5:6:7:8]:1"]
        `shouldBe` Right ["10.0.0.1:80", "0.0.0.0:1", "255.255.255.255:65535", "[::1]:443", "[::]:80", "[2001:db8::1]:443", "[1:2:3:4:5:6:7:8]:1"]
    it "writes IPv6 in its shortest form: the longest run of zeroes, the first of equal ones, never a single group" $
      mapM entry ["[1:0:0:2:0:0:0:3]:1", "[1:0:0:2:0:0:3:4]:1", "[1:0:2:3:4:5:6:7]:1", "[0:0:0:0:0:0:0:1]:1", "[1:0:0:0:0:0:0:0]:1", "[0001:0002::0003]:1"]
        `shouldBe` Right ["[1:0:0:2::3]:1", "[1::2:0:0:3:4]:1", "[1:0:2:3:4:5:6:7]:1", "[::1]:1", "[1::]:1", "[1:2::3]:1"]
    it "reads an IPv6 address that ends in an IPv4 one" $
      entry "[::ffff:10.0.0.1]:443" `shouldBe` Right "[::ffff:a00:1]:443"
    it "refuses an IPv6 address that is not in brackets" $
      entry "::1:443" `shouldSatisfy` isLeft
    it "refuses a malformed IPv6 address" $
      mapM_ (\h -> entry h `shouldSatisfy` isLeft) ["[1::2::3]:1", "[1:2:3:4:5:6:7:8:9]:1", "[12345::]:1", "[g::]:1", "[1:2:3:4:5:6:7]:1", "[::1:2:3:4:5:6:7:8]:1", "[]:1"]
    it "refuses a port that is missing, out of range, signed, padded or a range" $
      mapM_ (\h -> entry h `shouldSatisfy` isLeft) ["a.com", "a.com:", "a.com:0", "a.com:65536", "a.com:+443", "a.com:0443", "a.com:8000-8010", "a.com:443 ", "a.com:4a3", "a.com:999999"]
    it "refuses an address range, a scheme, a path or a user" $
      mapM_ (\h -> entry h `shouldSatisfy` isLeft) ["10.0.0.0/8:443", "https://a.com:443", "a.com/x:443", "u@a.com:443", "a com:443"]
    it "refuses a label that starts or ends with a hyphen, or is empty or too long" $ do
      mapM_ (\h -> entry h `shouldSatisfy` isLeft) ["-a.com:1", "a-.com:1", "a..com:1", ".a.com:1", T.replicate 64 "a" <> ".com:1"]
      entry (T.replicate 63 "a" <> ".com:1") `shouldSatisfy` (not . isLeft)
    it "refuses a name longer than 253 characters" $ do
      let label = T.replicate 62 "a"
          name n = T.intercalate "." (replicate n label)
      entry (name 4 <> ":1") `shouldSatisfy` (not . isLeft) -- 251
      entry (name 4 <> ".abc:1") `shouldSatisfy` isLeft -- 255
    it "names in its message what it could not read" $ do
      entry "a.com" `shouldBe` Left "'a.com' is not <host>:<port>"
      entry "a.com:0" `shouldBe` Left "'a.com:0' has no port from 1 to 65535"
      entry "[zz]:1" `shouldBe` Left "'[zz]' is not an IPv6 address"
      entry "::1:443" `shouldBe` Left "'::1:443': an IPv6 address is written in brackets"

  describe "egress coverage (spec 10.12)" $ do
    let e t = either (error . T.unpack) id (parseEgressEntry t)
    it "covers the same host on the same port, compared as read" $
      egressCovers (e "Example.com.:443") (e "example.com:443") `shouldBe` True
    it "covers addresses only by the same address" $ do
      egressCovers (e "[::1]:443") (e "[0:0:0:0:0:0:0:1]:443") `shouldBe` True
      egressCovers (e "10.0.0.1:443") (e "10.0.0.2:443") `shouldBe` False
    it "does not cover an address by a name, or the reverse" $
      egressCovers (e "localhost:443") (e "127.0.0.1:443") `shouldBe` False
    it "does not cover a name that only ends in the same letters" $
      egressCovers (e "*.example.com:443") (e "badexample.com:443") `shouldBe` False

  describe "published ports, as --publish reads them (spec 10.11)" $ do
    it "reads an address, a host port and a container port" $ do
      parsePublish "127.0.0.1:8080:80" `shouldBe` Published [v4 [127, 0, 0, 1]] (Just (8080, 8080))
      parsePublish "[::1]:8080:80" `shouldBe` Published [v6 [0, 0, 0, 0, 0, 0, 0, 1]] (Just (8080, 8080))
    it "reads an entry that names no address as every interface of both families" $ do
      parsePublish "8080:80" `shouldBe` Published everywhere (Just (8080, 8080))
      parsePublish "80" `shouldBe` Published everywhere Nothing
      parsePublish "80/udp" `shouldBe` Published everywhere Nothing
    it "reads ports the engine chooses where no host port is named" $ do
      parsePublish "127.0.0.1::80" `shouldBe` Published [v4 [127, 0, 0, 1]] Nothing
      parsePublish "[::1]::80/tcp" `shouldBe` Published [v6 [0, 0, 0, 0, 0, 0, 0, 1]] Nothing
    it "reads a port range" $
      parsePublish "127.0.0.1:8000-8010:80-90" `shouldBe` Published [v4 [127, 0, 0, 1]] (Just (8000, 8010))
    it "reads what it cannot read as every port on every interface" $
      mapM_
        (\t -> parsePublish t `shouldBe` Published everywhere Nothing)
        ["localhost:8080:80", "8080:0", "9000-8000:80", "a:b:c:d", "", "[::1:8080:80", "127.0.0.1:80", "70000:80"]

  describe "inbound entries (spec 10.11)" $ do
    it "reads an address alone, either family, an IPv6 one with or without brackets" $
      mapM inbound ["127.0.0.1", "0.0.0.0", "::1", "[::1]", "::"] `shouldBe` Right ["127.0.0.1", "0.0.0.0", "::1", "::1", "::"]
    it "reads an address with a port or a range" $
      mapM inbound ["127.0.0.1:8080", "[::1]:8000-8010", "10.0.0.1:1-65535"] `shouldBe` Right ["127.0.0.1:8080", "[::1]:8000-8010", "10.0.0.1:1-65535"]
    it "refuses an IPv6 address with a port and no brackets, a name, or a bad port" $
      mapM_
        (\t -> inbound t `shouldBe` Left ("'" <> t <> "' is not <addr> or <addr>:<port>, with an IP address"))
        ["localhost", "localhost:80", "127.0.0.1:0", "127.0.0.1:65536", "127.0.0.1:90-80", "127.0.0.1:", "1.2.3:80", "[::1]:", "*:80"]

  describe "what a constraint admits of published ports (spec 10.11)" $ do
    let only es = noConstraint {conInbound = Just (InboundListed (either (error . T.unpack) id (mapM parseInboundEntry es)))}
        publishing ps = AccessSet Nothing Nothing Nothing (Just (map parsePublish ps))
        admits c ps = null (checkAccess c (publishing ps))
    it "admits a port on a listed address, with no port or a port range holding it" $ do
      admits (only ["127.0.0.1"]) ["127.0.0.1:8080:80"] `shouldBe` True
      admits (only ["127.0.0.1:8000-8100"]) ["127.0.0.1:8080:80"] `shouldBe` True
      admits (only ["[::1]:8080"]) ["[::1]:8080:80"] `shouldBe` True
    it "does not admit a range wider than the listed one" $
      admits (only ["127.0.0.1:8000-8010"]) ["127.0.0.1:8005-8020:80"] `shouldBe` False
    it "admits a port the engine chooses only by an address listed with no port" $ do
      admits (only ["127.0.0.1"]) ["127.0.0.1::80"] `shouldBe` True
      admits (only ["127.0.0.1:1-65535"]) ["127.0.0.1::80"] `shouldBe` False
    it "admits an entry with no address only when both 0.0.0.0 and :: are listed" $ do
      admits (only ["0.0.0.0"]) ["8080:80"] `shouldBe` False
      admits (only ["0.0.0.0", "::"]) ["8080:80"] `shouldBe` True
    it "compares addresses as addresses, not as they are written" $
      admits (only ["0:0:0:0:0:0:0:1"]) ["[::1]:8080:80"] `shouldBe` True
    it "does not take 0.0.0.0 to admit 127.0.0.1, nor the reverse" $ do
      admits (only ["127.0.0.1"]) ["0.0.0.0:8080:80"] `shouldBe` False
      admits (only ["0.0.0.0"]) ["127.0.0.1:8080:80"] `shouldBe` False
    it "admits nothing with false, and anything with true" $ do
      admits noConstraint {conInbound = Just (InboundListed [])} ["127.0.0.1:8080:80"] `shouldBe` False
      admits noConstraint {conInbound = Just (InboundListed [])} [] `shouldBe` True
      admits noConstraint {conInbound = Just InboundAny} ["8080:80"] `shouldBe` True
    it "names where the port is opened" $
      checkAccess (only ["127.0.0.1"]) (publishing ["[::1]:9000:80"]) `shouldBe` [Exceeds "inbound" "a port is published on [::1]:9000"]

  describe "what a constraint admits of the network (spec 10.11, 10.12)" $ do
    let e t = either (error . T.unpack) id (parseEgressEntry t)
        net n = AccessSet Nothing Nothing (Just n) Nothing
        out b = noConstraint {conOutbound = Just b}
    it "admits egress by a list that covers every entry" $ do
      checkAccess (out (BoundEgress [e "*.npmjs.org:443"])) (net (NetEgress [e "registry.npmjs.org:443"])) `shouldBe` []
      map findingField (checkAccess (out (BoundEgress [e "*.npmjs.org:443"])) (net (NetEgress [e "npmjs.org:443"]))) `shouldBe` ["outbound"]
    it "admits egress with true, and an empty egress with false" $ do
      checkAccess (out (BoundClass NetDefault)) (net (NetEgress [e "a.com:443"])) `shouldBe` []
      checkAccess (out (BoundClass NetNone)) (net (NetEgress [])) `shouldBe` []
      map findingField (checkAccess (out (BoundClass NetNone)) (net (NetEgress [e "a.com:443"]))) `shouldBe` ["outbound"]
    it "does not admit the default network by a list, but admits none" $ do
      map findingField (checkAccess (out (BoundEgress [e "a.com:443"])) (net (NetReach NetDefault Nothing))) `shouldBe` ["outbound"]
      checkAccess (out (BoundEgress [e "a.com:443"])) (net (NetReach NetNone (Just "none"))) `shouldBe` []
    it "does not admit the host's network by true" $
      checkAccess (out (BoundClass NetDefault)) (net (NetReach NetHost (Just "host"))) `shouldBe` [Exceeds "outbound" "the network is \"host\""]

  describe "constraints written back (spec 10.11)" $ do
    it "writes nothing for within {}" $
      renderConstraint noConstraint `shouldBe` "within {}"
    it "writes every field in its order" $
      renderConstraint
        Constraint
          { conFiles = Just [(".", ModeRw)],
            conVolumes = Just (VolumesListed [(VolumeHost "/tmp/c", ModeRo), (VolumeNamed "cache", ModeRw)]),
            conOutbound = Just (BoundEgress [either (error . T.unpack) id (parseEgressEntry "[::1]:443")]),
            conInbound = Just (InboundListed [InboundEntry (v4 [127, 0, 0, 1]) (Just (8000, 8010)), InboundEntry (v6 (replicate 8 0)) Nothing]),
            conSecrets = Just SecretsNone,
            conRuns = Just []
          }
        `shouldBe` "within { files: \"rw\", volumes: { \"/tmp/c\": \"ro\", \"cache\": \"rw\" }, outbound: [\"[::1]:443\"], inbound: [\"127.0.0.1:8000-8010\", \"::\"], secrets: false, runs: false }"
    it "writes true and false where they are meant" $
      renderConstraint noConstraint {conVolumes = Just VolumesAny, conOutbound = Just (BoundClass NetDefault), conInbound = Just InboundAny, conRuns = Just [ProfileLocal, ProfileContainer]}
        `shouldBe` "within { volumes: true, outbound: true, inbound: true, runs: [\"local\", \"container\"] }"
    it "writes an empty egress list as false" $
      renderConstraint noConstraint {conOutbound = Just (BoundEgress []), conInbound = Just (InboundListed [])}
        `shouldBe` "within { outbound: false, inbound: false }"

  describe "profiles (spec 10.11)" $ do
    it "reads local and container, and nothing else" $ do
      map parseProfile ["local", "container", "Local", "host"] `shouldBe` [Just ProfileLocal, Just ProfileContainer, Nothing, Nothing]
      map profileText [minBound .. maxBound] `shouldBe` ["local", "container"]
    it "counts a constraint that gives any field as bounding" $ do
      bounds noConstraint `shouldBe` False
      bounds noConstraint {conRuns = Just [ProfileContainer]} `shouldBe` True
