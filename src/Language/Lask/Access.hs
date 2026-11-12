{-# LANGUAGE OverloadedStrings #-}

-- | What a container reaches past itself (spec 10.2, 10.11): the mount
-- of the base directory, the volumes, and the network. Shared by the
-- elaborator, which checks what is written as a literal, the runtime,
-- which mounts what the options say, and the listings of @lask envs@.
module Language.Lask.Access
  ( -- * Modes
    Mode (..),
    modeText,
    parseMode,

    -- * The base directory
    defaultBaseMount,
    validateBaseMount,
    AccessTable,
    accessTable,
    accessScalar,
    modeAt,
    keyParts,

    -- * Volumes
    Volume (..),
    VolumeSource (..),
    parseVolume,
    hostEquivalent,

    -- * The network
    NetClass (..),
    netClassText,
    classifyNetwork,

    -- * Egress
    EgressHost (..),
    EgressEntry (..),
    parseEgressEntry,
    renderEgressEntry,
    egressCovers,
    NetReach (..),
    NetBound (..),

    -- * Published ports
    BindAddr (..),
    Published (..),
    parsePublish,
    InboundEntry (..),
    parseInboundEntry,
    renderInboundEntry,

    -- * Constraints
    Constraint (..),
    VolumeBound (..),
    InboundBound (..),
    Profile (..),
    profileText,
    parseProfile,
    noConstraint,
    bounds,
    renderConstraint,
    AccessSet (..),
    Finding (..),
    findingField,
    checkAccess,

    -- * Secrets
    Channel (..),
    channelText,
    exposedChannel,
    SecretBound (..),
    checkSecrets,
  )
where

import Data.Char (isAlphaNum)
import Language.Lask.Span (Span (..))
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T

-- Modes ---------------------------------------------------------------

-- | What a mount permits, narrowest first.
data Mode = ModeNone | ModeRo | ModeRw
  deriving (Show, Eq, Ord, Enum, Bounded)

modeText :: Mode -> Text
modeText m = case m of
  ModeNone -> "none"
  ModeRo -> "ro"
  ModeRw -> "rw"

-- |
-- >>> parseMode "ro"
-- Right ModeRo
-- >>> parseMode "read-only"
-- Left "'read-only' is not a mode: give \"rw\", \"ro\" or \"none\""
parseMode :: Text -> Either Text Mode
parseMode t = case t of
  "none" -> Right ModeNone
  "ro" -> Right ModeRo
  "rw" -> Right ModeRw
  _ -> Left ("'" <> t <> "' is not a mode: give \"rw\", \"ro\" or \"none\"")

-- The base directory ----------------------------------------------------

-- | Where the base directory is mounted when @base_mount@ is not given
-- (spec 10.5).
defaultBaseMount :: Text
defaultBaseMount = "/work"

-- | A @base_mount@ (spec 10.2): an absolute, normalized POSIX path,
-- other than the root and the trees the engine provides itself. A
-- comma would split the field of @--mount@ it is given in.
--
-- >>> validateBaseMount "/src"
-- Right "/src"
-- >>> validateBaseMount "/src/"
-- Left "'base_mount' must be normalized: no empty, '.' or '..' component and no trailing '/'"
-- >>> validateBaseMount "/proc/x"
-- Left "'base_mount' must not be /proc, /sys or /dev, or under them"
validateBaseMount :: Text -> Either Text Text
validateBaseMount t
  | not ("/" `T.isPrefixOf` t) = Left "'base_mount' must be an absolute path, such as \"/src\""
  | t == "/" = Left "'base_mount' must not be '/'"
  | not (normalized (T.drop 1 t)) =
      Left "'base_mount' must be normalized: no empty, '.' or '..' component and no trailing '/'"
  | take 1 (T.splitOn "/" (T.drop 1 t)) `elem` [["proc"], ["sys"], ["dev"]] =
      Left "'base_mount' must not be /proc, /sys or /dev, or under them"
  | T.any (== ',') t = Left "'base_mount' must not contain a comma"
  | otherwise = Right t

-- | Whether a relative path is written with no empty, @.@ or @..@
-- component.
normalized :: Text -> Bool
normalized p = all (`notElem` ["", ".", ".."]) (T.splitOn "/" p)

-- | A @base_access@ table (spec 10.2): each path relative to the base
-- directory with its mode, @"."@ among them, shortest first, so that
-- a longer path is mounted over the shorter it is under.
type AccessTable = [(Text, Mode)]

-- | The table a mode alone gives.
accessScalar :: Mode -> AccessTable
accessScalar m = [(".", m)]

-- | Read a @base_access@ table.
--
-- >>> accessTable [("dist", "rw"), (".", "ro")]
-- Right [(".",ModeRo),("dist",ModeRw)]
-- >>> accessTable [("dist", "rw")]
-- Left "a 'base_access' table must give \".\", the mode of every path no other key names"
-- >>> accessTable [(".", "ro"), ("../x", "rw")]
-- Left "'../x' must be a path inside the base directory, written relative to it without '.' or '..'"
accessTable :: [(Text, Text)] -> Either Text AccessTable
accessTable kvs = do
  entries <- mapM entry kvs
  if "." `elem` map fst entries
    then Right (sortOn (\(k, _) -> (length (keyParts k), k)) entries)
    else Left "a 'base_access' table must give \".\", the mode of every path no other key names"
  where
    entry (k, v) = do
      key <- validKey k
      m <- parseMode v
      pure (key, m)
    validKey k
      | k == "." = Right k
      | "/" `T.isPrefixOf` k || not (normalized k) =
          Left ("'" <> k <> "' must be a path inside the base directory, written relative to it without '.' or '..'")
      | T.any (== ',') k = Left ("'" <> k <> "' must not contain a comma")
      | otherwise = Right k

-- | The components of a key; none for @"."@.
keyParts :: Text -> [Text]
keyParts k = if k == "." then [] else T.splitOn "/" k

-- | The mode a table gives a path: that of the longest key that is
-- the path or one of its ancestors.
--
-- >>> modeAt [(".", ModeRo), ("dist", ModeRw)] "dist/a.js"
-- ModeRw
-- >>> modeAt [(".", ModeRo), ("dist", ModeRw)] "distx"
-- ModeRo
modeAt :: AccessTable -> Text -> Mode
modeAt table path =
  case [m | (k, m) <- sortOn (negate . length . keyParts . fst) table, keyParts k `isPrefixOfList` keyParts path] of
    m : _ -> m
    [] -> ModeRw
  where
    isPrefixOfList a b = take (length a) b == a

-- Volumes ---------------------------------------------------------------

data VolumeSource = VolumeHost Text | VolumeNamed Text
  deriving (Show, Eq, Ord)

-- | A volume that holds something of the host: its source, where it is
-- mounted, and its mode.
data Volume = Volume
  { volSource :: VolumeSource,
    volTarget :: Text,
    volMode :: Mode
  }
  deriving (Show, Eq, Ord)

-- | Read an entry of @volumes@ as the client's @--volume@ does,
-- @[<source>:]<target>[:<options>]@. An anonymous volume holds nothing
-- of the host, and is 'Nothing'.
--
-- >>> parseVolume "/var/run/docker.sock:/var/run/docker.sock"
-- Just (Volume {volSource = VolumeHost "/var/run/docker.sock", volTarget = "/var/run/docker.sock", volMode = ModeRw})
-- >>> parseVolume "cache:/root/.cache:ro,z"
-- Just (Volume {volSource = VolumeNamed "cache", volTarget = "/root/.cache", volMode = ModeRo})
-- >>> parseVolume "/data"
-- Nothing
parseVolume :: Text -> Maybe Volume
parseVolume t = case T.splitOn ":" t of
  src : target : rest ->
    let opts = concatMap (T.splitOn ",") rest
        mode = if any (`elem` ["ro", "readonly"]) opts then ModeRo else ModeRw
        source = if "/" `T.isPrefixOf` src then VolumeHost src else VolumeNamed src
     in Just (Volume source target mode)
  _ -> Nothing

-- | Whether a host path, mounted, gives the container what the host has
-- (spec 10.11): the root; the kernel's interfaces, @/proc@, @/sys@ and
-- @/dev@; the socket or the state of a container engine, which starts
-- any container with any option; the host's configuration and root's
-- home; and any directory that holds one of those. The path is read
-- with @.@ and @..@ resolved as written, without following a link.
--
-- >>> map (hostEquivalent . VolumeHost) ["/", "/var/run/docker.sock", "/var/run", "/run/user/1000/podman/podman.sock", "/proc/1", "/etc", "/var/run/../lib/docker", "/home/me/.cfg", "/tmp/cache", "/var/lib/app"]
-- [True,True,True,True,True,True,True,False,False,False]
hostEquivalent :: VolumeSource -> Bool
hostEquivalent src = case src of
  VolumeNamed _ -> False
  VolumeHost p ->
    let parts = resolveDots (T.splitOn "/" p)
        path = "/" <> T.intercalate "/" parts
        under a = path == a || (a <> "/") `T.isPrefixOf` path
        holds a = (path <> "/") `T.isPrefixOf` a
     in null parts
          || any under ["/proc", "/sys", "/dev"]
          || any (\a -> under a || holds a) sensitive
          || any (`T.isSuffixOf` path) ["/docker.sock", "/podman.sock", "/containerd.sock", "/buildkitd.sock"]
  where
    resolveDots = reverse . foldl step []
    step acc c = case c of
      "" -> acc
      "." -> acc
      ".." -> drop 1 acc
      _ -> c : acc
    sensitive =
      [ "/var/run/docker.sock",
        "/run/docker.sock",
        "/var/run/containerd",
        "/run/containerd",
        "/var/run/podman",
        "/run/podman",
        "/var/lib/docker",
        "/var/lib/containerd",
        "/var/lib/containers",
        "/etc",
        "/root",
        "/boot"
      ]

-- The network -----------------------------------------------------------

-- | How far a container's network reaches, narrowest first.
data NetClass = NetNone | NetDefault | NetHost
  deriving (Show, Eq, Ord, Enum, Bounded)

netClassText :: NetClass -> Text
netClassText c = case c of
  NetNone -> "none"
  NetDefault -> "default"
  NetHost -> "host"

-- | The class of a @network@ value (spec 10.11): a value naming the
-- host's network, or another container's or namespace's, whose reach is
-- not known, is @host@.
--
-- >>> map classifyNetwork [Nothing, Just "none", Just "bridge", Just "ci_net", Just "host", Just "container:db"]
-- [NetDefault,NetNone,NetDefault,NetDefault,NetHost,NetHost]
classifyNetwork :: Maybe Text -> NetClass
classifyNetwork v = case v of
  Nothing -> NetDefault
  Just "none" -> NetNone
  Just "host" -> NetHost
  Just n
    | isName n -> NetDefault
    | otherwise -> NetHost
  where
    isName n = case T.uncons n of
      Just (c, _) -> isAlphaNum c && T.all (\x -> isAlphaNum x || x `elem` ("_.-" :: String)) n
      Nothing -> False

-- Egress ----------------------------------------------------------------

-- | The host of an entry of @egress@ (spec 10.12): a domain name,
-- lower-case and without a trailing dot; the domain under a wildcard
-- (@*.example.com@ is 'EgressWildcard' @example.com@); or an address,
-- an IPv6 one as its eight groups.
data EgressHost
  = EgressName Text
  | EgressWildcard Text
  | EgressAddr4 [Int]
  | EgressAddr6 [Int]
  deriving (Show, Eq, Ord)

-- | One @<host>:<port>@ of @egress@.
data EgressEntry = EgressEntry
  { egHost :: EgressHost,
    egPort :: Int
  }
  deriving (Show, Eq, Ord)

-- | Read an entry as it is written (spec 10.12, Entries).
--
-- >>> parseEgressEntry "Registry.NPMjs.org.:443"
-- Right (EgressEntry {egHost = EgressName "registry.npmjs.org", egPort = 443})
-- >>> renderEgressEntry <$> parseEgressEntry "[2001:0db8:0:0::1]:443"
-- Right "[2001:db8::1]:443"
-- >>> either id renderEgressEntry . parseEgressEntry <$> ["example.com", "*:443", "example.com:0", "10.0.0.0/8:443", "http://x.com:80"]
-- ["'example.com' is not <host>:<port>","'*' is not a domain name or an address","'example.com:0' has no port from 1 to 65535","'10.0.0.0/8' is not a domain name or an address","'http://x.com' is not a domain name or an address"]
parseEgressEntry :: Text -> Either Text EgressEntry
parseEgressEntry s = case T.breakOnEnd ":" s of
  (before, portText)
    | T.length before > 1 -> do
        let host = T.dropEnd 1 before
        port <- case T.unpack portText of
          ds@(d : _)
            | all (`elem` ['0' .. '9']) ds,
              d /= '0' || ds == "0",
              length ds <= 5,
              let n = read ds :: Int,
              n >= 1 && n <= 65535 ->
                Right n
          _ -> Left ("'" <> s <> "' has no port from 1 to 65535")
        h <- parseHost host
        pure (EgressEntry h port)
  _ -> Left ("'" <> s <> "' is not <host>:<port>")
  where
    parseHost host
      | Just inner <- T.stripPrefix "[" host >>= T.stripSuffix "]" =
          maybe (Left ("'" <> host <> "' is not an IPv6 address")) (Right . EgressAddr6) (parseIPv6 inner)
      | Just a <- parseIPv4 host = Right (EgressAddr4 a)
      | Just _ <- parseIPv6 host = Left ("'" <> s <> "': an IPv6 address is written in brackets")
      | otherwise =
          let n = T.toLower (fromMaybe' host (T.stripSuffix "." host))
           in case T.stripPrefix "*." n of
                Just rest | validName rest -> Right (EgressWildcard rest)
                Nothing | validName n -> Right (EgressName n)
                _ -> Left ("'" <> host <> "' is not a domain name or an address")
    fromMaybe' d = maybe d id

-- | Labels of letters, digits, hyphens and the underscore some service
-- names carry; an internationalized name is written in this form.
validName :: Text -> Bool
validName n =
  not (T.null n) && T.length n <= 253 && all label (T.splitOn "." n)
  where
    label l =
      not (T.null l)
        && T.length l <= 63
        && T.head l /= '-'
        && T.last l /= '-'
        && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_') l

parseIPv4 :: Text -> Maybe [Int]
parseIPv4 t = case T.splitOn "." t of
  parts@[_, _, _, _] -> mapM octet parts
  _ -> Nothing
  where
    octet p = case T.unpack p of
      ds@(d : _)
        | all (`elem` ['0' .. '9']) ds,
          d /= '0' || ds == "0",
          length ds <= 3,
          let n = read ds :: Int,
          n <= 255 ->
            Just n
      _ -> Nothing

-- | The eight groups of an IPv6 address, written with at most one
-- @::@ and, last, an IPv4 address in place of two groups.
parseIPv6 :: Text -> Maybe [Int]
parseIPv6 t = case T.splitOn "::" t of
  [whole] -> do
    gs <- groups whole
    if length gs == 8 then Just gs else Nothing
  [a, b] -> do
    ga <- if T.null a then Just [] else groups a
    gb <- if T.null b then Just [] else groups b
    let missing = 8 - length ga - length gb
    if missing >= 1 then Just (ga <> replicate missing 0 <> gb) else Nothing
  _ -> Nothing
  where
    groups x =
      let parts = T.splitOn ":" x
       in case reverse parts of
            lastPart : _
              | Just [a, b, c, d] <- parseIPv4 lastPart -> (<> [a * 256 + b, c * 256 + d]) <$> mapM hex (init parts)
            _ -> mapM hex parts
    hex g
      | not (T.null g),
        T.length g <= 4,
        T.all (`elem` ("0123456789abcdefABCDEF" :: String)) g =
          Just (foldl (\acc c -> acc * 16 + hexDigit c) 0 (T.unpack g))
      | otherwise = Nothing
    hexDigit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | otherwise = fromEnum c - fromEnum 'A' + 10

-- | An entry as Lask writes it back: a name lower-case, an IPv6
-- address in its shortest form (RFC 5952).
renderEgressEntry :: EgressEntry -> Text
renderEgressEntry (EgressEntry h port) = host <> ":" <> T.pack (show port)
  where
    host = case h of
      EgressName n -> n
      EgressWildcard n -> "*." <> n
      EgressAddr4 os -> T.intercalate "." (map (T.pack . show) os)
      EgressAddr6 gs -> "[" <> renderIPv6 gs <> "]"

-- | An IPv6 address in its shortest form (RFC 5952).
renderIPv6 :: [Int] -> Text
renderIPv6 gs =
  let zeroes = [(i, len) | (i, len) <- zeroRuns, len >= 2]
      hexT n = T.pack (showHex' n)
   in case sortOn (\(i, len) -> (negate len, i)) zeroes of
        (i, len) : _ ->
          T.intercalate ":" (map hexT (take i gs)) <> "::" <> T.intercalate ":" (map hexT (drop (i + len) gs))
        [] -> T.intercalate ":" (map hexT gs)
  where
    zeroRuns = go 0 gs
      where
        go _ [] = []
        go i xs@(x : rest)
          | x == 0 = let len = length (takeWhile (== 0) xs) in (i, len) : go (i + len) (drop len xs)
          | otherwise = go (i + 1) rest
    showHex' n
      | n < 16 = [digit n]
      | otherwise = showHex' (n `div` 16) <> [digit (n `mod` 16)]
    digit d = "0123456789abcdef" !! d

-- | Whether one entry covers another (spec 10.12): the same port, and
-- the same host, or a wildcard over it, which may itself be a wildcard
-- of a longer name.
--
-- >>> let e = either (error . T.unpack) id . parseEgressEntry
-- >>> [egressCovers (e "*.github.com:443") (e x) | x <- ["api.github.com:443", "a.b.github.com:443", "*.api.github.com:443", "github.com:443", "api.github.com:80"]]
-- [True,True,True,False,False]
egressCovers :: EgressEntry -> EgressEntry -> Bool
egressCovers (EgressEntry a pa) (EgressEntry b pb) =
  pa == pb && case (a, b) of
    (EgressWildcard d, EgressName n) -> ("." <> d) `T.isSuffixOf` n
    (EgressWildcard d, EgressWildcard n) -> n == d || ("." <> d) `T.isSuffixOf` n
    _ -> a == b

-- | What the network of a use reaches (spec 10.11): a class, with the
-- value of @network@ as written, or the entries of @egress@.
data NetReach
  = NetReach NetClass (Maybe Text)
  | NetEgress [EgressEntry]
  deriving (Show, Eq)

-- | What a constraint's @outbound@ admits: up to a class (@false@ is
-- 'NetNone', @true@ 'NetDefault'), or what the entries it gives cover.
data NetBound
  = BoundClass NetClass
  | BoundEgress [EgressEntry]
  deriving (Show, Eq, Ord)

-- Published ports ---------------------------------------------------------

-- | A host address a port is published on: an IPv4 address as its four
-- octets, an IPv6 one as its eight groups. The unspecified addresses,
-- @0.0.0.0@ and @::@, are every interface of their family.
data BindAddr = BindAddr4 [Int] | BindAddr6 [Int]
  deriving (Show, Eq, Ord)

renderBindAddr :: BindAddr -> Text
renderBindAddr a = case a of
  BindAddr4 os -> T.intercalate "." (map (T.pack . show) os)
  BindAddr6 gs -> renderIPv6 gs

-- | Read an address as an entry of @inbound@ writes it, an IPv6 one
-- with or without brackets.
parseBindAddr :: Text -> Maybe BindAddr
parseBindAddr t
  | Just inner <- T.stripPrefix "[" t >>= T.stripSuffix "]" = BindAddr6 <$> parseIPv6 inner
  | Just a <- parseIPv4 t = Just (BindAddr4 a)
  | otherwise = BindAddr6 <$> parseIPv6 t

-- | What an entry of @publish@ opens on the host (spec 10.11): the
-- addresses it listens on, and the host ports, 'Nothing' where the
-- engine chooses them.
data Published = Published
  { pbAddrs :: [BindAddr],
    pbPorts :: Maybe (Int, Int)
  }
  deriving (Show, Eq, Ord)

-- | Read an entry of @publish@ as the client's @--publish@ does,
-- @[[<ip>:][<hostPort>]:]<containerPort>[/<proto>]@, with an IPv6
-- address in brackets and a port as a range where it is one. An entry
-- that names no address listens on every interface of both families;
-- one this does not read is taken to open every port there.
--
-- >>> parsePublish "127.0.0.1:8080:80"
-- Published {pbAddrs = [BindAddr4 [127,0,0,1]], pbPorts = Just (8080,8080)}
-- >>> parsePublish "8000-8010:80-90/udp"
-- Published {pbAddrs = [BindAddr4 [0,0,0,0],BindAddr6 [0,0,0,0,0,0,0,0]], pbPorts = Just (8000,8010)}
-- >>> parsePublish "[::1]::80"
-- Published {pbAddrs = [BindAddr6 [0,0,0,0,0,0,0,1]], pbPorts = Nothing}
parsePublish :: Text -> Published
parsePublish t = maybe (Published everywhere Nothing) id $ do
  let body = fst (T.breakOn "/" t)
  (addrs, rest) <- case T.stripPrefix "[" body of
    Just v6 -> do
      let (inner, after) = T.breakOn "]" v6
      gs <- parseIPv6 inner
      rest <- T.stripPrefix "]:" after
      pure ([BindAddr6 gs], T.splitOn ":" rest)
    Nothing -> case T.splitOn ":" body of
      [ip, h, c] -> do
        a <- parseIPv4 ip
        pure ([BindAddr4 a], [h, c])
      parts -> pure (everywhere, parts)
  case rest of
    [c] -> Published addrs Nothing <$ portRange c
    [h, c] -> do
      _ <- portRange c
      ports <- if T.null h then Just Nothing else Just <$> portRange h
      pure (Published addrs ports)
    _ -> Nothing
  where
    everywhere = [BindAddr4 [0, 0, 0, 0], BindAddr6 (replicate 8 0)]

-- | A port, or a range of them, from 1 to 65535.
portRange :: Text -> Maybe (Int, Int)
portRange t = case T.splitOn "-" t of
  [a] -> (\p -> (p, p)) <$> port a
  [a, b] -> do
    lo <- port a
    hi <- port b
    if lo <= hi then Just (lo, hi) else Nothing
  _ -> Nothing
  where
    port p = case T.unpack p of
      ds@(d : _)
        | all (`elem` ['0' .. '9']) ds,
          d /= '0',
          length ds <= 5,
          let n = read ds :: Int,
          n <= 65535 ->
            Just n
      _ -> Nothing

-- | One entry of a constraint's @inbound@ (spec 10.11): an address a
-- port may be published on, and the ports, 'Nothing' for any.
data InboundEntry = InboundEntry
  { ibAddr :: BindAddr,
    ibPorts :: Maybe (Int, Int)
  }
  deriving (Show, Eq, Ord)

-- | Read an entry of @inbound@: @<addr>@ or @<addr>:<port>@, the port
-- a range where it is one, and an IPv6 address in brackets when a port
-- follows it.
--
-- >>> parseInboundEntry "127.0.0.1:8080"
-- Right (InboundEntry {ibAddr = BindAddr4 [127,0,0,1], ibPorts = Just (8080,8080)})
-- >>> renderInboundEntry <$> parseInboundEntry "[::1]:8000-8010"
-- Right "[::1]:8000-8010"
-- >>> renderInboundEntry <$> parseInboundEntry "::1"
-- Right "::1"
-- >>> parseInboundEntry "localhost"
-- Left "'localhost' is not <addr> or <addr>:<port>, with an IP address"
parseInboundEntry :: Text -> Either Text InboundEntry
parseInboundEntry t = maybe (Left ("'" <> t <> "' is not <addr> or <addr>:<port>, with an IP address")) Right $
  case parseBindAddr t of
    Just a -> Just (InboundEntry a Nothing)
    Nothing -> do
      let (before, p) = T.breakOnEnd ":" t
      host <- T.stripSuffix ":" before
      -- An IPv6 address is bracketed when a port follows it, which
      -- would otherwise read as its last group.
      a <- case parseBindAddr host of
        Just (BindAddr6 _) | not ("[" `T.isPrefixOf` host) -> Nothing
        other -> other
      ports <- portRange p
      pure (InboundEntry a (Just ports))

-- | An entry as Lask writes it back.
renderInboundEntry :: InboundEntry -> Text
renderInboundEntry (InboundEntry a ports) = case ports of
  Nothing -> renderBindAddr a
  Just r -> host <> ":" <> renderRange r
  where
    host = case a of
      BindAddr6 _ -> "[" <> renderBindAddr a <> "]"
      _ -> renderBindAddr a

renderRange :: (Int, Int) -> Text
renderRange (lo, hi)
  | lo == hi = T.pack (show lo)
  | otherwise = T.pack (show lo) <> "-" <> T.pack (show hi)

-- Constraints -----------------------------------------------------------

-- | The kinds of environment a use runs in (spec 10.2): the host, or a
-- container.
data Profile = ProfileLocal | ProfileContainer
  deriving (Show, Eq, Ord, Enum, Bounded)

profileText :: Profile -> Text
profileText p = case p of
  ProfileLocal -> "local"
  ProfileContainer -> "container"

parseProfile :: Text -> Maybe Profile
parseProfile t = case t of
  "local" -> Just ProfileLocal
  "container" -> Just ProfileContainer
  _ -> Nothing

-- | What a constraint's @volumes@ admits: any volume, or the host paths
-- and named volumes it lists, with the widest mode each may be mounted
-- with (@false@ lists none).
data VolumeBound = VolumesAny | VolumesListed [(VolumeSource, Mode)]
  deriving (Show, Eq, Ord)

-- | What a constraint's @inbound@ admits: any published port, or those
-- its entries cover (@false@ lists none).
data InboundBound = InboundAny | InboundListed [InboundEntry]
  deriving (Show, Eq, Ord)

-- | An access constraint (spec 10.11): the bound on each component,
-- 'Nothing' where it gives none.
data Constraint = Constraint
  { conFiles :: Maybe AccessTable,
    conVolumes :: Maybe VolumeBound,
    conOutbound :: Maybe NetBound,
    conInbound :: Maybe InboundBound,
    conSecrets :: Maybe SecretBound,
    -- | The kinds of environment it admits; @Just []@ for @false@.
    conRuns :: Maybe [Profile]
  }
  deriving (Show, Eq, Ord)

-- | @within {}@.
noConstraint :: Constraint
noConstraint = Constraint Nothing Nothing Nothing Nothing Nothing Nothing

-- | Whether a constraint gives any field.
bounds :: Constraint -> Bool
bounds c = c /= noConstraint

-- | A constraint as it is written.
--
-- >>> renderConstraint noConstraint {conFiles = Just [(".", ModeRo), ("dist", ModeRw)], conVolumes = Just (VolumesListed []), conOutbound = Just (BoundClass NetNone), conRuns = Just [ProfileContainer]}
-- "within { files: { \".\": \"ro\", \"dist\": \"rw\" }, volumes: false, outbound: false, runs: [\"container\"] }"
renderConstraint :: Constraint -> Text
renderConstraint c =
  "within {"
    <> (if null fields then "" else " " <> T.intercalate ", " fields <> " ")
    <> "}"
  where
    fields =
      [ "files: " <> files t | Just t <- [conFiles c]
      ]
        <> ["volumes: " <> volumes v | Just v <- [conVolumes c]]
        <> ["outbound: " <> outbound n | Just n <- [conOutbound c]]
        <> ["inbound: " <> inbound b | Just b <- [conInbound c]]
        <> ["secrets: " <> secrets b | Just b <- [conSecrets c]]
        <> ["runs: " <> list (map (quote . profileText) ps) | Just ps <- [conRuns c]]
    secrets b = case b of
      SecretsNone -> "false"
      SecretsNamed ns -> "[" <> T.intercalate ", " (map fst ns) <> "]"
    files t = case t of
      [(".", m)] -> quote (modeText m)
      _ -> "{ " <> T.intercalate ", " [quote k <> ": " <> quote (modeText m) | (k, m) <- t] <> " }"
    volumes v = case v of
      VolumesAny -> "true"
      VolumesListed [] -> "false"
      VolumesListed vs -> "{ " <> T.intercalate ", " [quote (sourceText src) <> ": " <> quote (modeText m) | (src, m) <- vs] <> " }"
    outbound b = case b of
      BoundClass NetNone -> "false"
      BoundClass _ -> "true"
      BoundEgress es -> list (map (quote . renderEgressEntry) es)
    inbound b = case b of
      InboundAny -> "true"
      InboundListed [] -> "false"
      InboundListed es -> list (map (quote . renderInboundEntry) es)
    -- @false@ admits nothing, as an empty list would.
    list xs = if null xs then "false" else "[" <> T.intercalate ", " xs <> "]"
    quote t = "\"" <> t <> "\""

sourceText :: VolumeSource -> Text
sourceText s = case s of
  VolumeHost p -> p
  VolumeNamed n -> n

-- | What a use reaches past its container (spec 10.11): each component
-- as its run options give it, 'Nothing' where an option is computed;
-- or the host, which is unbounded in every one.
--
-- 'AccessSet' holds files, volumes, the network's class with the value
-- of @network@ as written, and what each entry of @publish@ opens.
data AccessSet
  = AccessSet (Maybe AccessTable) (Maybe [Volume]) (Maybe NetReach) (Maybe [Published])
  | -- | The host, and why: @#local@, or a volume that gives what the
    -- host has ('hostEquivalent').
    HostAccess Text
  deriving (Show, Eq)

-- | Where an access set is not shown to be within a constraint: a
-- component that exceeds it, with what it is, or one that cannot be
-- determined.
data Finding = Exceeds Text Text | Unproven Text
  deriving (Show, Eq, Ord)

findingField :: Finding -> Text
findingField f = case f of
  Exceeds k _ -> k
  Unproven k -> k

-- | What keeps an access set from being within a constraint (spec
-- 10.11); none when it is within it.
--
-- >>> let c = noConstraint {conFiles = Just [(".", ModeRo), ("dist", ModeRw)], conOutbound = Just (BoundClass NetNone)}
-- >>> checkAccess c (AccessSet (Just [(".", ModeRo), ("dist", ModeRw)]) (Just []) (Just (NetReach NetNone (Just "none"))) (Just []))
-- []
-- >>> checkAccess c (AccessSet (Just [(".", ModeRw)]) (Just []) (Just (NetReach NetDefault Nothing)) (Just []))
-- [Exceeds "files" "\".\" is \"rw\", wider than \"ro\"",Exceeds "outbound" "the network is \"default\""]
-- >>> checkAccess c (AccessSet (Just [(".", ModeRo)]) Nothing Nothing (Just []))
-- [Unproven "outbound"]
-- >>> checkAccess c (HostAccess "#local reaches the whole host")
-- [Exceeds "files" "#local reaches the whole host",Exceeds "outbound" "#local reaches the whole host"]
-- >>> checkAccess noConstraint {conRuns = Just [ProfileContainer]} (HostAccess "#local reaches the whole host")
-- [Exceeds "runs" "#local reaches the whole host"]
-- >>> checkAccess noConstraint {conInbound = Just (InboundListed [InboundEntry (BindAddr4 [127,0,0,1]) Nothing])} (AccessSet Nothing Nothing Nothing (Just [parsePublish "8080:80"]))
-- [Exceeds "inbound" "a port is published on 0.0.0.0:8080"]
checkAccess :: Constraint -> AccessSet -> [Finding]
checkAccess c a = case a of
  HostAccess why ->
    [ Exceeds k why
    | (k, True) <- [("files", isJust' (conFiles c)), ("volumes", isJust' (conVolumes c)), ("outbound", isJust' (conOutbound c)), ("inbound", isJust' (conInbound c))]
    ]
      <> runsWithin ProfileLocal why
  AccessSet files vols net pub ->
    concat
      [ maybe [] (\bound -> maybe [Unproven "files"] (filesWithin bound) files) (conFiles c),
        maybe [] (\bound -> maybe [Unproven "volumes"] (volumesWithin bound) vols) (conVolumes c),
        maybe [] (\bound -> maybe [Unproven "outbound"] (networkWithin bound) net) (conOutbound c),
        maybe [] (\bound -> maybe [Unproven "inbound"] (inboundWithin bound) pub) (conInbound c),
        runsWithin ProfileContainer "it runs in a container"
      ]
  where
    isJust' = maybe False (const True)

    runsWithin p why = case conRuns c of
      Just ps | p `notElem` ps -> [Exceeds "runs" why]
      _ -> []

    filesWithin bound t =
      take 1
        [ Exceeds "files" (q p <> " is " <> q (modeText m) <> ", wider than " <> q (modeText b))
        | p <- nub ("." : map fst t <> map fst bound),
          let m = modeAt t p
              b = modeAt bound p,
          m > b
        ]

    volumesWithin bound vs = case bound of
      VolumesAny -> []
      VolumesListed listed ->
        take 1
          [ Exceeds "volumes" (q (sourceText (volSource v)) <> " is mounted " <> q (modeText (volMode v)))
          | v <- vs,
            not (any (>= volMode v) [m | (k, m) <- listed, covers k (volSource v)])
          ]

    covers k src = case (k, src) of
      (VolumeHost kp, VolumeHost p) -> kp == p || (kp <> "/") `T.isPrefixOf` p || kp == "/"
      (VolumeNamed kn, VolumeNamed n) -> kn == n
      _ -> False

    networkWithin bound reach = case (bound, reach) of
      (BoundClass b, NetReach cls written)
        | cls > b -> [Exceeds "outbound" ("the network is " <> q (maybe (netClassText cls) id written))]
        | otherwise -> []
      (BoundEgress _, NetReach NetNone _) -> []
      (BoundEgress _, NetReach cls written) -> [Exceeds "outbound" ("the network is " <> q (maybe (netClassText cls) id written))]
      -- An egress that lists nothing reaches nothing, as none does.
      (BoundClass NetNone, NetEgress es) -> take 1 [Exceeds "outbound" ("egress lists " <> q (renderEgressEntry e)) | e <- es]
      (BoundClass _, NetEgress _) -> []
      (BoundEgress bs, NetEgress es) ->
        take 1 [Exceeds "outbound" ("egress lists " <> q (renderEgressEntry e)) | e <- es, not (any (`egressCovers` e) bs)]

    inboundWithin bound ps = case bound of
      InboundAny -> []
      InboundListed es ->
        take 1
          [ Exceeds "inbound" ("a port is published on " <> opened addr (pbPorts p))
          | p <- ps,
            addr <- pbAddrs p,
            not (any (\e -> ibAddr e == addr && portsCover (ibPorts e) (pbPorts p)) es)
          ]

    portsCover bound ports = case (bound, ports) of
      (Nothing, _) -> True
      (Just (lo, hi), Just (a', b')) -> lo <= a' && b' <= hi
      (Just _, Nothing) -> False

    opened addr ports = case ports of
      Nothing -> renderInboundEntry (InboundEntry addr Nothing) <> ", on a port the engine chooses"
      Just r -> renderInboundEntry (InboundEntry addr (Just r))

    q t = "\"" <> t <> "\""

-- Secrets ---------------------------------------------------------------

-- | How a secret reaches a use (spec 10.11): in the value of a variable
-- of @env@, of one whose name is not known statically, in another run
-- option (or a key of @env@), in the command string, or in an argument
-- of a filesystem function.
data Channel
  = ChEnv Text
  | ChEnvComputed
  | ChOption Text
  | ChCommand
  | ChFile
  deriving (Show, Eq, Ord)

channelText :: Channel -> Text
channelText ch = case ch of
  ChEnv n -> "env " <> n
  ChEnvComputed -> "env (computed)"
  ChOption n -> "option " <> n
  ChCommand -> "command"
  ChFile -> "file"

-- | A channel as a diagnostic says it.
channelPhrase :: Channel -> Text
channelPhrase ch = case ch of
  ChEnv n -> "in the variable " <> n <> " of 'env'"
  ChEnvComputed -> "in 'env'"
  ChOption n -> "in the run option '" <> n <> "'"
  ChCommand -> "in the command string"
  ChFile -> "to a filesystem function"

-- | A channel that puts the secret on a command line of the host, or
-- writes it (spec 11.4: marked @!@).
exposedChannel :: Channel -> Bool
exposedChannel ch = case ch of
  ChEnv _ -> False
  ChEnvComputed -> False
  _ -> True

-- | What a @secrets@ field admits: none, or the secrets bound to one
-- of the names it gives (spec 10.11), each where it is written, and
-- those only in @env@.
data SecretBound = SecretsNone | SecretsNamed [(Text, Span)]
  deriving (Show, Eq, Ord)

-- | What keeps a secret reaching a use from being within a constraint
-- (spec 10.11): the channel it is given by, the secret as a diagnostic
-- names it, and whether it is bound to a name the constraint gives.
--
-- >>> checkSecrets noConstraint {conSecrets = Just (SecretsNamed [("token", NoSpan)])} (ChEnv "TOKEN") "'main.lask: token'" True
-- []
-- >>> checkSecrets noConstraint {conSecrets = Just (SecretsNamed [("token", NoSpan)])} ChCommand "'main.lask: token'" True
-- [Exceeds "secrets" "the secret 'main.lask: token' is given in the command string"]
-- >>> checkSecrets noConstraint {conSecrets = Just (SecretsNamed [("token", NoSpan)])} (ChEnv "OTHER") "'main.lask: other'" False
-- [Exceeds "secrets" "the secret 'main.lask: other' is given in the variable OTHER of 'env'"]
checkSecrets :: Constraint -> Channel -> Text -> Bool -> [Finding]
checkSecrets c ch label named = case conSecrets c of
  Nothing -> []
  Just (SecretsNamed _)
    | inEnv ch && named -> []
  Just _ -> [Exceeds "secrets" ("the secret " <> label <> " is given " <> channelPhrase ch)]
  where
    inEnv x = case x of
      ChEnv _ -> True
      ChEnvComputed -> True
      _ -> False
