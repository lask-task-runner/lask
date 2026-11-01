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
  )
where

import Data.Char (isAlphaNum)
import Data.List (sortOn)
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
