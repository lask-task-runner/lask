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

    -- * Constraints
    Constraint (..),
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
      EgressAddr6 gs -> "[" <> ipv6 gs <> "]"
    ipv6 gs =
      let runs = [(i, len) | (i, len) <- zeroRuns gs, len >= 2]
          hexT n = T.pack (showHex' n)
       in case sortOn (\(i, len) -> (negate len, i)) runs of
            (i, len) : _ ->
              T.intercalate ":" (map hexT (take i gs)) <> "::" <> T.intercalate ":" (map hexT (drop (i + len) gs))
            [] -> T.intercalate ":" (map hexT gs)
    zeroRuns gs = go 0 gs
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

-- | What a constraint's @network@ admits: up to a class, or what the
-- entries it gives cover.
data NetBound
  = BoundClass NetClass
  | BoundEgress [EgressEntry]
  deriving (Show, Eq, Ord)

-- Constraints -----------------------------------------------------------

-- | An access constraint (spec 10.11): the bound on each component,
-- 'Nothing' where it gives none.
data Constraint = Constraint
  { conFiles :: Maybe AccessTable,
    -- | The host paths and named volumes it admits, with the widest
    -- mode each may be mounted with; @Just []@ for @false@.
    conVolumes :: Maybe [(VolumeSource, Mode)],
    conNetwork :: Maybe NetBound,
    conPublish :: Maybe Bool,
    conSecrets :: Maybe SecretBound
  }
  deriving (Show, Eq, Ord)

-- | @within {}@.
noConstraint :: Constraint
noConstraint = Constraint Nothing Nothing Nothing Nothing Nothing

-- | Whether a constraint gives any field.
bounds :: Constraint -> Bool
bounds c = c /= noConstraint

-- | A constraint as it is written.
--
-- >>> renderConstraint (Constraint (Just [(".", ModeRo), ("dist", ModeRw)]) (Just []) (Just (BoundClass NetNone)) Nothing Nothing)
-- "within { files: { \".\": \"ro\", \"dist\": \"rw\" }, volumes: false, network: \"none\" }"
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
        <> ["network: " <> network n | Just n <- [conNetwork c]]
        <> ["publish: " <> (if p then "true" else "false") | Just p <- [conPublish c]]
        <> ["secrets: " <> secrets b | Just b <- [conSecrets c]]
    secrets b = case b of
      SecretsNone -> "false"
      SecretsNamed ns -> "[" <> T.intercalate ", " (map fst ns) <> "]"
    files t = case t of
      [(".", m)] -> quote (modeText m)
      _ -> "{ " <> T.intercalate ", " [quote k <> ": " <> quote (modeText m) | (k, m) <- t] <> " }"
    volumes v = case v of
      [] -> "false"
      _ -> "{ " <> T.intercalate ", " [quote (sourceText src) <> ": " <> quote (modeText m) | (src, m) <- v] <> " }"
    quote t = "\"" <> t <> "\""
    network b = case b of
      BoundClass n -> quote (netClassText n)
      BoundEgress es -> "[" <> T.intercalate ", " (map (quote . renderEgressEntry) es) <> "]"

sourceText :: VolumeSource -> Text
sourceText s = case s of
  VolumeHost p -> p
  VolumeNamed n -> n

-- | What a use reaches past its container (spec 10.11): each component
-- as its run options give it, 'Nothing' where an option is computed;
-- or the host, which is unbounded in every one.
--
-- 'AccessSet' holds files, volumes, the network's class with the value
-- of @network@ as written, and whether a port is published.
data AccessSet
  = AccessSet (Maybe AccessTable) (Maybe [Volume]) (Maybe NetReach) (Maybe Bool)
  | HostAccess
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
-- >>> let c = noConstraint {conFiles = Just [(".", ModeRo), ("dist", ModeRw)], conNetwork = Just (BoundClass NetNone)}
-- >>> checkAccess c (AccessSet (Just [(".", ModeRo), ("dist", ModeRw)]) (Just []) (Just (NetReach NetNone (Just "none"))) (Just False))
-- []
-- >>> checkAccess c (AccessSet (Just [(".", ModeRw)]) (Just []) (Just (NetReach NetDefault Nothing)) (Just False))
-- [Exceeds "files" "\".\" is \"rw\", wider than \"ro\"",Exceeds "network" "the network is \"default\""]
-- >>> checkAccess c (AccessSet (Just [(".", ModeRo)]) Nothing Nothing (Just False))
-- [Unproven "network"]
-- >>> checkAccess c HostAccess
-- [Exceeds "files" "#local reaches the whole host",Exceeds "network" "#local reaches the whole host"]
checkAccess :: Constraint -> AccessSet -> [Finding]
checkAccess c a = case a of
  HostAccess ->
    [ Exceeds k "#local reaches the whole host"
    | (k, True) <- [("files", isJust' (conFiles c)), ("volumes", isJust' (conVolumes c)), ("network", isJust' (conNetwork c)), ("publish", isJust' (conPublish c))]
    ]
  AccessSet files vols net pub ->
    concat
      [ maybe [] (\bound -> maybe [Unproven "files"] (filesWithin bound) files) (conFiles c),
        maybe [] (\bound -> maybe [Unproven "volumes"] (volumesWithin bound) vols) (conVolumes c),
        maybe [] (\bound -> maybe [Unproven "network"] (networkWithin bound) net) (conNetwork c),
        maybe [] (\bound -> maybe [Unproven "publish"] (publishWithin bound) pub) (conPublish c)
      ]
  where
    isJust' = maybe False (const True)

    filesWithin bound t =
      take 1
        [ Exceeds "files" (q p <> " is " <> q (modeText m) <> ", wider than " <> q (modeText b))
        | p <- nub ("." : map fst t <> map fst bound),
          let m = modeAt t p
              b = modeAt bound p,
          m > b
        ]

    volumesWithin bound vs =
      take 1
        [ Exceeds "volumes" (q (sourceText (volSource v)) <> " is mounted " <> q (modeText (volMode v)))
        | v <- vs,
          not (any (>= volMode v) [m | (k, m) <- bound, covers k (volSource v)])
        ]

    covers k src = case (k, src) of
      (VolumeHost kp, VolumeHost p) -> kp == p || (kp <> "/") `T.isPrefixOf` p || kp == "/"
      (VolumeNamed kn, VolumeNamed n) -> kn == n
      _ -> False

    networkWithin bound reach = case (bound, reach) of
      (BoundClass b, NetReach cls written)
        | cls > b -> [Exceeds "network" ("the network is " <> q (maybe (netClassText cls) id written))]
        | otherwise -> []
      (BoundEgress _, NetReach NetNone _) -> []
      (BoundEgress _, NetReach cls written) -> [Exceeds "network" ("the network is " <> q (maybe (netClassText cls) id written))]
      -- An egress that lists nothing reaches nothing, as none does.
      (BoundClass NetNone, NetEgress es) -> take 1 [Exceeds "network" ("egress lists " <> q (renderEgressEntry e)) | e <- es]
      (BoundClass _, NetEgress _) -> []
      (BoundEgress bs, NetEgress es) ->
        take 1 [Exceeds "network" ("egress lists " <> q (renderEgressEntry e)) | e <- es, not (any (`egressCovers` e) bs)]

    publishWithin bound p
      | p && not bound = [Exceeds "publish" "a port is published"]
      | otherwise = []

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
