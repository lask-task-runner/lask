{-# LANGUAGE OverloadedStrings #-}

-- | @lask secrets list@ and @lask secrets check@ (spec 11.10): the
-- secret references in the environment, and whether the stores they
-- name can be reached, logged in to and read from.
--
-- Neither prints a secret value or a credential. @check@ reads a
-- value only with @--read@, since reading a dynamic secret issues a
-- credential, and gives any lease that read was issued under back.
module Command.Lask.Secrets
  ( Scope (..),
    secretsList,
    secretsCheck,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Language.Lask.ErrorCode (ErrorCode (..), codeText)
import Language.Lask.SecretStore.Resolve (enabledSchemes, openProvider)
import Language.Lask.SecretStore.Types
import System.Exit (ExitCode (..), exitWith)

-- | Which variables to consider: all of them, or those a function
-- reads by name.
data Scope = AllVariables | OnlyVariables Text (Set Text)

-- | One variable whose value is a reference, or starts like one.
data Found = Found
  { fVar :: Text,
    fRaw :: Text,
    fRef :: Either SecretError RawRef
  }

gather :: Map String String -> Scope -> [Found]
gather env scope =
  [ Found name raw ref
  | (k, v) <- Map.toList env,
    let name = T.pack k
        raw = T.pack v,
    inScope name,
    Just ref <- [detectRef raw]
  ]
  where
    inScope name = case scope of
      AllVariables -> True
      OnlyVariables _ names -> name `Set.member` names

-- | The outcome of one line of a report.
data Status = Ok Text | Failed SecretError | Skipped Text

statusWord :: Status -> Text
statusWord s = case s of
  Ok _ -> "ok"
  Failed _ -> "NG"
  Skipped _ -> "skipped"

statusDetail :: Status -> Text
statusDetail s = case s of
  Ok d -> d
  Failed e -> codeText (seCode e) <> ": " <> seMessage e
  Skipped d -> d

statusJson :: Status -> [(AK.Key, A.Value)]
statusJson s =
  ("status", A.String (statusWord s)) : case s of
    Failed e -> [("code", A.String (codeText (seCode e))), ("message", A.String (seMessage e))]
    _ -> [("message", A.String (statusDetail s)) | not (T.null (statusDetail s))]

isFailed :: Status -> Bool
isFailed (Failed _) = True
isFailed _ = False

-- list ---------------------------------------------------------------------

-- | Every reference in scope, and whether it is well formed and its
-- store enabled and configured. Makes no request.
secretsList :: Bool -> Map String String -> Scope -> IO ()
secretsList json env scope = do
  let found = gather env scope
      schemes = nub [rawScheme r | Found _ _ (Right r) <- found]
  providers <- Map.fromList <$> mapM (\s -> (,) s <$> openProvider env s) schemes
  let rows = [(f, statusOf providers f) | f <- found]
  if json
    then
      emitJson
        [ A.object $
            [ ("variable", A.String (fVar f)),
              ("reference", A.String (reference f))
            ]
              <> [("scheme", A.String (rawScheme r)) | Right r <- [fRef f]]
              <> statusJson st
        | (f, st) <- rows
        ]
    else
      if null rows
        then TIO.putStrLn (noReferences scope)
        else
          table
            ["VARIABLE", "SCHEME", "REFERENCE", "STATUS"]
            [ [fVar f, either (const "?") rawScheme (fRef f), refBody f, statusWord st <> suffix st]
            | (f, st) <- rows
            ]
  where
    statusOf providers f = case fRef f of
      Left e -> Failed e
      Right r -> case Map.lookup (rawScheme r) providers of
        Just (Right p) -> either Failed (const (Ok "")) (provParse p (rawBody r))
        Just (Left e) -> Failed e
        Nothing -> Skipped ""
    suffix st = case st of
      Failed _ -> " " <> statusDetail st
      _ -> ""
    refBody f = either (const (fRaw f)) rawBody (fRef f)

-- check --------------------------------------------------------------------

-- | Each store in turn: configuration, reachability, login, then
-- every reference to it. A store stops at its first failed stage; its
-- references are then skipped. Exits 3 when anything failed.
secretsCheck :: Bool -> Bool -> Map String String -> Scope -> IO ()
secretsCheck json readValues env scope = do
  let found = gather env scope
      malformed = [(f, Failed e) | f@(Found _ _ (Left e)) <- found]
      referenced = [(f, r) | f@(Found _ _ (Right r)) <- found]
      schemes = nub (enabledSchemes env <> map (rawScheme . snd) referenced)
  stores <-
    mapM
      (\s -> checkStore readValues env s [f | (f, r) <- referenced, rawScheme r == s])
      schemes
  let failed = any isFailed (map snd malformed) || any storeFailed stores
  if json
    then
      emitJson $
        A.object
          [ ("stores", A.toJSON (map storeJson stores)),
            ("malformed", A.toJSON [refJson f st | (f, st) <- malformed])
          ]
    else
      if null stores && null malformed
        then TIO.putStrLn (noReferences scope <> ", and no store is enabled in LASK_SECRETS")
        else do
          mapM_ printStore stores
          if null malformed
            then pure ()
            else do
              TIO.putStrLn "malformed references"
              table' [[fVar f, fRaw f, statusWord st, statusDetail st] | (f, st) <- malformed]
  exitWith (if failed then ExitFailure 3 else ExitSuccess)
  where
    storeFailed st = any (isFailed . snd) (storeStages st) || any (isFailed . snd) (storeRefs st)
    storeJson st =
      A.object
        [ ("scheme", A.String (storeScheme st)),
          ("target", maybe A.Null A.String (storeTarget st)),
          ("stages", A.toJSON [A.object (("stage", A.String n) : statusJson s) | (n, s) <- storeStages st]),
          ("references", A.toJSON [refJson f s | (f, s) <- storeRefs st])
        ]
    refJson f s = A.object ([("variable", A.String (fVar f)), ("reference", A.String (reference f))] <> statusJson s)
    printStore st = do
      TIO.putStrLn (storeScheme st <> maybe "" ("  " <>) (storeTarget st))
      table' [[n, statusWord s, statusDetail s] | (n, s) <- storeStages st]
      table' [[fVar f, reference f, statusWord s, statusDetail s] | (f, s) <- storeRefs st]
    table' rows = if null rows then pure () else mapM_ (TIO.putStrLn . ("  " <>)) (aligned rows)

data StoreReport = StoreReport
  { storeScheme :: Text,
    storeTarget :: Maybe Text,
    storeStages :: [(Text, Status)],
    storeRefs :: [(Found, Status)]
  }

checkStore :: Bool -> Map String String -> Text -> [Found] -> IO StoreReport
checkStore readValues env scheme refs = do
  provE <- openProvider env scheme
  case provE of
    Left e -> pure (stoppedAt Nothing [("config", Failed e)])
    Right prov -> do
      let configured = ("config", Ok ("enabled in LASK_SECRETS, " <> provTarget prov))
      healthS <- case provHealth prov of
        Nothing -> pure (Skipped "the store has no health endpoint")
        Just h -> either Failed Ok <$> h
      if isFailed healthS
        then pure (stoppedAt (Just (provTarget prov)) [configured, ("reachable", healthS)])
        else do
          sessionE <- provLogin prov
          case sessionE of
            Left e -> pure (stoppedAt (Just (provTarget prov)) [configured, ("reachable", healthS), ("auth", Failed e)])
            Right session -> do
              results <- mapM (\f -> (,) f <$> checkRef prov session f) refs
              pure
                StoreReport
                  { storeScheme = scheme,
                    storeTarget = Just (provTarget prov),
                    storeStages = [configured, ("reachable", healthS), ("auth", Ok (sessSummary session))],
                    storeRefs = results
                  }
  where
    stoppedAt target stages =
      StoreReport scheme target stages [(f, Skipped "not checked: the store is not available") | f <- refs]
    checkRef prov session f = case fRef f of
      Left e -> pure (Failed e)
      Right r -> case provParse prov (rawBody r) of
        Left e -> pure (Failed e)
        Right loc -> do
          probed <- provProbe prov session loc
          case probed of
            Left e -> pure (Failed e)
            Right detail
              | not readValues -> pure (Ok (detail <> " (value not read)"))
              | otherwise -> do
                  d <- provRead prov session loc
                  case d of
                    Left e -> pure (Failed e)
                    Right sd -> do
                      mapM_ (provRelease prov session) (sdLease sd)
                      pure $
                        if Map.member (locField loc) (sdFields sd)
                          then Ok (detail <> ", value read" <> maybe "" (const ", lease revoked") (sdLease sd))
                          else
                            Failed . SecretError EIoSecretNotFound $
                              "no field '" <> locField loc <> "' (fields: " <> T.intercalate ", " (Map.keys (sdFields sd)) <> ")"

-- helpers ------------------------------------------------------------------

reference :: Found -> Text
reference f = either (const (fRaw f)) renderRawRef (fRef f)

noReferences :: Scope -> Text
noReferences scope = case scope of
  AllVariables -> "no secret references in the environment"
  OnlyVariables fn _ -> "no secret references among the variables '" <> fn <> "' reads"

table :: [Text] -> [[Text]] -> IO ()
table header rows = mapM_ TIO.putStrLn (aligned (header : rows))

-- | Columns padded to their widest cell; the last is left as it is.
aligned :: [[Text]] -> [Text]
aligned rows =
  [ T.stripEnd (T.intercalate "  " (zipWith pad widths cells))
  | cells <- rows
  ]
  where
    columns = maximum (0 : map length rows)
    widths =
      [ if i == columns - 1 then 0 else maximum (0 : mapMaybe (fmap T.length . cellAt i) rows)
      | i <- [0 .. columns - 1]
      ]
    cellAt i cells = if i < length cells then Just (cells !! i) else Nothing
    pad w t = T.justifyLeft w ' ' t

emitJson :: (A.ToJSON a) => a -> IO ()
emitJson = TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . A.encode
