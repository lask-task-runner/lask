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

    -- * Constraints
    Constraint (..),
    noConstraint,
    bounds,
    renderConstraint,
    AccessSet (..),
    Finding (..),
    findingField,
    checkAccess,
  )
where

import Data.Char (isAlphaNum)
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

-- Constraints -----------------------------------------------------------

-- | An access constraint (spec 10.11): the bound on each component,
-- 'Nothing' where it gives none.
data Constraint = Constraint
  { conFiles :: Maybe AccessTable,
    -- | The host paths and named volumes it admits, with the widest
    -- mode each may be mounted with; @Just []@ for @false@.
    conVolumes :: Maybe [(VolumeSource, Mode)],
    conNetwork :: Maybe NetClass,
    conPublish :: Maybe Bool
  }
  deriving (Show, Eq, Ord)

-- | @within {}@.
noConstraint :: Constraint
noConstraint = Constraint Nothing Nothing Nothing Nothing

-- | Whether a constraint gives any field.
bounds :: Constraint -> Bool
bounds c = c /= noConstraint

-- | A constraint as it is written.
--
-- >>> renderConstraint (Constraint (Just [(".", ModeRo), ("dist", ModeRw)]) (Just []) (Just NetNone) Nothing)
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
        <> ["network: " <> quote (netClassText n) | Just n <- [conNetwork c]]
        <> ["publish: " <> (if p then "true" else "false") | Just p <- [conPublish c]]
    files t = case t of
      [(".", m)] -> quote (modeText m)
      _ -> "{ " <> T.intercalate ", " [quote k <> ": " <> quote (modeText m) | (k, m) <- t] <> " }"
    volumes v = case v of
      [] -> "false"
      _ -> "{ " <> T.intercalate ", " [quote (sourceText src) <> ": " <> quote (modeText m) | (src, m) <- v] <> " }"
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
-- of @network@ as written, and whether a port is published.
data AccessSet
  = AccessSet (Maybe AccessTable) (Maybe [Volume]) (Maybe (NetClass, Maybe Text)) (Maybe Bool)
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
-- >>> let c = noConstraint {conFiles = Just [(".", ModeRo), ("dist", ModeRw)], conNetwork = Just NetNone}
-- >>> checkAccess c (AccessSet (Just [(".", ModeRo), ("dist", ModeRw)]) (Just []) (Just (NetNone, Just "none")) (Just False))
-- []
-- >>> checkAccess c (AccessSet (Just [(".", ModeRw)]) (Just []) (Just (NetDefault, Nothing)) (Just False))
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

    networkWithin bound (cls, written)
      | cls > bound = [Exceeds "network" ("the network is " <> q (maybe (netClassText cls) id written))]
      | otherwise = []

    publishWithin bound p
      | p && not bound = [Exceeds "publish" "a port is published"]
      | otherwise = []

    q t = "\"" <> t <> "\""
