{-# LANGUAGE OverloadedStrings #-}

-- | Implementations of the builtin library (spec chapter 15).
--
-- The dispatcher is parameterized by an 'Apply' callback so it can
-- call function values (map\/reduce\/recover\/spawn\/...) without a
-- module cycle with the evaluator, and by a 'CommandRunner' supplied
-- by the runtime context (spec 10, injected for testability).
module Language.Lask.Builtins.Impl
  ( Apply,
    CommandRunner,
    FileOp (..),
    FileRunner,
    RtHooks (..),
    callBuiltin,
  )
where

import Control.Concurrent.Async (async, cancel, waitAnyCatch, waitCatch)
import Control.Exception (throwIO, try)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits ((.&.), (.|.))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, isInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)
import Language.Lask.Builtins.Format (decodeFormat, encodeFormat)
import qualified Language.Lask.Builtins.Path as P
import qualified Language.Lask.Builtins.Regex as Re
import Language.Lask.ErrorCode
import Language.Lask.Obs.ExecLog (LogSink)
import Language.Lask.Runtime.AsyncTrack (AsyncTracker (..), siteOf)
import Language.Lask.Runtime.Secrets (maskSecrets, registerSecret)
import Language.Lask.Runtime.Value
import System.Entropy (getEntropy)
import System.Environment (getEnvironment, lookupEnv)

-- | Apply a function value to positional and keyword arguments.
type Apply = Value -> [Value] -> [(Text, Value)] -> IO Value

-- | Run a command in a resolved environment:
-- returns (exit code, stdout, stderr) or an infrastructure failure.
type CommandRunner = EnvValue -> Text -> IO (Either LaskFailure (Int, Text, Text))

-- | One filesystem operation of spec 15.11. The path is always taken
-- as the program wrote it: resolution against the environment's
-- working directory (10.5) belongs to the runner, which is the only
-- part that knows what that directory is in each environment.
data FileOp
  = FileRead Text
  | FileWrite Text Text
  | FileExists Text
  | FileRemove Text
  | FileMakeDir Text
  | FileListDir Text
  | FileGlob Text
  deriving (Show, Eq)

-- | Perform a filesystem operation in an environment (spec 15.11),
-- yielding the value the built-in returns. Injected for the same
-- reason as 'CommandRunner': the filesystem a program reaches is the
-- one its environment argument names, never an ambient host one.
type FileRunner = EnvValue -> FileOp -> IO (Either LaskFailure Value)

-- | The effectful services the built-in library needs from the host.
-- They are injected rather than imported so that the evaluator stays
-- free of a module cycle, and so that tests can run the whole library
-- without touching a process, a filesystem, or a log.
data RtHooks = RtHooks
  { hookRunCommand :: CommandRunner,
    hookRunFile :: FileRunner,
    hookLog :: LogSink,
    -- | Which computations were started and which were awaited
    -- (spec 6.3).
    hookAsync :: AsyncTracker
  }

callBuiltin :: Apply -> RtHooks -> Text -> [Value] -> [(Text, Value)] -> IO Value
callBuiltin apply hooks name args _kwArgs = case (name, args) of
  -- 15.2 numeric ---------------------------------------------------------
  ("add", [VNumber a, VNumber b]) -> num (a + b)
  ("sub", [VNumber a, VNumber b]) -> num (a - b)
  ("mul", [VNumber a, VNumber b]) -> num (a * b)
  ("div", [VNumber a, VNumber b])
    | b == 0 -> throwIO (runtimeFailure ERuntimeDivByZero "division by zero")
    | otherwise -> num (divS a b)
  ("mod", [VNumber a, VNumber b])
    | b == 0 -> throwIO (runtimeFailure ERuntimeDivByZero "modulo by zero")
    | otherwise -> num (modS a b)
  ("abs", [VNumber a]) -> num (abs a)
  ("floor", [VNumber a]) -> num (fromInteger (floor (toRealTo a)))
  ("ceil", [VNumber a]) -> num (fromInteger (ceiling (toRealTo a)))
  ("round", [VNumber a]) -> num (fromInteger (round (toRealTo a)))
  ("min", [VNumber a, VNumber b]) -> num (min a b)
  ("max", [VNumber a, VNumber b]) -> num (max a b)
  ("sum", [VArray xs]) -> num (sum [n | VNumber n <- V.toList xs])
  ("pow", [VNumber a, VNumber b]) ->
    let r = toRealTo a ** toRealTo b
     in if isNaN r || isInfinite r
          then throwIO (domainError "pow is not a real number for these arguments")
          else num (fromFloatDigits r)
  ("sqrt", [VNumber a])
    | a < 0 -> throwIO (domainError "sqrt of a negative number")
    | otherwise -> num (fromFloatDigits (sqrt (toRealTo a)))
  ("clamp", [VNumber v, VNumber lo, VNumber hi])
    | lo > hi -> throwIO (domainError "clamp was given a low bound above its high bound")
    | otherwise -> num (max lo (min hi v))
  -- 15.3 string ----------------------------------------------------------
  ("length", [VString s]) -> num (fromIntegral (T.length s))
  ("concat", [VString a, VString b]) -> pure (VString (a <> b))
  ("trim", [VString s]) -> pure (VString (T.strip s))
  ("to_lower", [VString s]) -> pure (VString (T.toLower s))
  ("to_upper", [VString s]) -> pure (VString (T.toUpper s))
  ("split", [VString s, VString sep])
    | T.null sep -> pure (VArray (V.fromList (map (VString . T.singleton) (T.unpack s))))
    | otherwise -> pure (VArray (V.fromList (map VString (T.splitOn sep s))))
  ("join", [VArray xs, VString sep]) ->
    pure (VString (T.intercalate sep [s | VString s <- V.toList xs]))
  ("replace", [VString s, VString from, VString to])
    | T.null from -> pure (VString s)
    | otherwise -> pure (VString (T.replace from to s))
  ("contains", [VString s, VString needle]) -> pure (VBool (T.isInfixOf needle s))
  ("starts_with", [VString s, VString p]) -> pure (VBool (T.isPrefixOf p s))
  ("ends_with", [VString s, VString p]) -> pure (VBool (T.isSuffixOf p s))
  -- Absence is a value, not a failure: the language has no option type.
  ("index_of", [VString s, VString needle])
    | T.null needle -> num 0
    | otherwise -> case T.breakOn needle s of
        (_, rest) | T.null rest -> num (-1)
        (before, _) -> num (fromIntegral (T.length before))
  -- Endpoints clamp, so substring never fails (spec 15.3).
  ("substring", [VString s, VNumber a, VNumber b]) ->
    let n = T.length s
        lo = clampIx n a
        hi = clampIx n b
     in pure (VString (if hi <= lo then "" else T.take (hi - lo) (T.drop lo s)))
  ("pad_start", [VString s, VNumber w, VString p]) -> pure (VString (padWith True s w p))
  ("pad_end", [VString s, VNumber w, VString p]) -> pure (VString (padWith False s w p))
  ("repeat", [VString s, VNumber n]) ->
    pure (VString (T.replicate (max 0 (truncate (toRealTo n))) s))
  ("lines", [VString s]) -> pure (textArray (map (T.dropWhileEnd (== '\r')) (T.lines s)))
  ("to_string", [v]) -> case v of
    VString t -> pure (VString t)
    VNumber n -> pure (VString (formatNumber n))
    VBool b -> pure (VString (if b then "true" else "false"))
    other ->
      throwIO . domainError $
        "to_string cannot render " <> typeNameOf other <> "; use to_json for structured values"
  ("to_number", [VString s]) -> case A.eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.strip s))) of
    Right (A.Number n) -> num n
    _ -> throwIO (ioFailure EIoDataDecode ("not a number: '" <> s <> "'"))
  ("regex_test", [VString s, VString pat]) -> withRegex pat (\re -> pure (VBool (Re.test re s)))
  ("regex_match", [VString s, VString pat]) ->
    withRegex pat (\re -> pure (textArray (Re.matchGroups re s)))
  ("regex_replace", [VString s, VString pat, VString rep']) ->
    withRegex pat (\re -> pure (VString (Re.replaceAll re rep' s)))
  -- 15.4 array/map/record -------------------------------------------------
  ("map", [VArray xs, f]) -> VArray <$> V.mapM (\x -> apply f [x] []) xs
  ("filter", [VArray xs, f]) -> VArray <$> V.filterM (\x -> truthy <$> apply f [x] []) xs
  ("reduce", [VArray xs, initV, f]) ->
    V.foldM (\acc x -> apply f [acc, x] []) initV xs
  ("for_each", [VArray xs, f]) -> V.mapM_ (\x -> apply f [x] []) xs >> pure VVoid
  ("append", [VArray xs, x]) -> pure (VArray (V.snoc xs x))
  ("concat_array", [VArray a, VArray b]) -> pure (VArray (a <> b))
  ("get", [VMap m, VString k]) -> case Map.lookup k m of
    Just v -> pure v
    Nothing -> throwIO (runtimeFailure ERuntimeAccess ("key not found: '" <> k <> "'"))
  ("has_key", [VMap m, VString k]) -> pure (VBool (Map.member k m))
  ("keys", [VMap m]) -> pure (VArray (V.fromList (map VString (Map.keys m))))
  ("values", [VMap m]) -> pure (VArray (V.fromList (Map.elems m)))
  ("size", [VArray xs]) -> num (fromIntegral (V.length xs))
  ("is_empty", [VArray xs]) -> pure (VBool (V.null xs))
  -- first/last share the failure contract of xs[0] (spec 8.9).
  ("first", [VArray xs]) -> maybe (throwIO (emptyArray "first")) pure (xs V.!? 0)
  ("last", [VArray xs]) -> maybe (throwIO (emptyArray "last")) pure (xs V.!? (V.length xs - 1))
  ("slice", [VArray xs, VNumber a, VNumber b]) -> pure (VArray (sliceV xs a b))
  ("take", [VArray xs, VNumber n]) -> pure (VArray (sliceV xs 0 n))
  ("drop", [VArray xs, VNumber n]) ->
    pure (VArray (sliceV xs n (fromIntegral (V.length xs))))
  ("reverse", [VArray xs]) -> pure (VArray (V.reverse xs))
  ("sort", [VArray xs]) -> pure (VArray (V.fromList (sortBy compareOrdered (V.toList xs))))
  ("sort_by", [VArray xs, f]) -> do
    keyed <- mapM (\x -> (\k -> (k, x)) <$> apply f [x] []) (V.toList xs)
    -- sortOn is stable, so equal keys keep their input order (15.4).
    pure (VArray (V.fromList (map snd (sortBy (\a b -> compareOrdered (fst a) (fst b)) keyed))))
  ("contains_array", [VArray xs, v]) -> pure (VBool (any (valueEq v) (V.toList xs)))
  ("index_of_array", [VArray xs, v]) ->
    num (fromIntegral (indexOfBy (valueEq v) (V.toList xs)))
  ("find", [VArray xs, f]) -> findM (V.toList xs)
    where
      findM [] = pure VNull
      findM (x : rest) = do
        ok <- truthy <$> apply f [x] []
        if ok then pure x else findM rest
  ("find_index", [VArray xs, f]) -> findIndexM (V.toList xs) 0
    where
      findIndexM [] _ = num (-1)
      findIndexM (x : rest) i = do
        ok <- truthy <$> apply f [x] []
        if ok then num (fromIntegral (i :: Int)) else findIndexM rest (i + 1)
  ("every", [VArray xs, f]) -> allM (V.toList xs)
    where
      allM [] = pure (VBool True)
      allM (x : rest) = do
        ok <- truthy <$> apply f [x] []
        if ok then allM rest else pure (VBool False)
  ("any", [VArray xs, f]) -> anyM (V.toList xs)
    where
      anyM [] = pure (VBool False)
      anyM (x : rest) = do
        ok <- truthy <$> apply f [x] []
        if ok then pure (VBool True) else anyM rest
  ("flatten", [VArray xss]) ->
    pure (VArray (V.concat [inner | VArray inner <- V.toList xss]))
  ("flat_map", [VArray xs, f]) -> do
    parts <- mapM (\x -> apply f [x] []) (V.toList xs)
    pure (VArray (V.concat [inner | VArray inner <- parts]))
  ("zip", [VArray xs, VArray ys]) ->
    pure . VArray . V.fromList $
      [ VRecord (Map.fromList [("first", x), ("second", y)])
      | (x, y) <- zip (V.toList xs) (V.toList ys)
      ]
  ("unique", [VArray xs]) -> pure (VArray (V.fromList (nubValues (V.toList xs))))
  ("range", [VNumber a, VNumber b])
    | not (isInteger a) || not (isInteger b) ->
        throwIO (domainError "range takes integer bounds")
    | otherwise ->
        let lo = truncate (toRealTo a) :: Integer
            hi = truncate (toRealTo b)
         in pure (VArray (V.fromList (map (VNumber . fromInteger) [lo .. hi - 1])))
  ("enumerate", [VArray xs]) ->
    pure . VArray . V.fromList $
      [ VRecord (Map.fromList [("index", VNumber (fromIntegral i)), ("value", x)])
      | (i, x) <- zip [(0 :: Int) ..] (V.toList xs)
      ]
  ("set", [VMap m, VString k, v]) -> pure (VMap (Map.insert k v m))
  ("remove", [VMap m, VString k]) -> pure (VMap (Map.delete k m))
  -- The right operand wins on a duplicate key (spec 15.4).
  ("merge", [VMap a, VMap b]) -> pure (VMap (Map.union b a))
  ("get_or", [VMap m, VString k, fallback]) -> pure (Map.findWithDefault fallback k m)
  ("entries", [VMap m]) ->
    pure . VArray . V.fromList $
      [VRecord (Map.fromList [("key", VString k), ("value", v)]) | (k, v) <- Map.toAscList m]
  ("from_entries", [VArray es]) ->
    pure . VMap . Map.fromList $
      [(k, v) | VRecord r <- V.toList es, Just (VString k) <- [Map.lookup "key" r], Just v <- [Map.lookup "value" r]]
  ("map_values", [VMap m, f]) -> VMap <$> traverse (\v -> apply f [v] []) m
  -- 15.5 command execution --------------------------------------------------
  ("run", [VEnv env, VString cmd]) -> do
    r <- hookRunCommand hooks env cmd
    case r of
      Left failure -> throwIO failure
      Right (code, out, err) ->
        pure . VRecord . Map.fromList $
          [ ("code", VNumber (fromIntegral code)),
            ("stdout", VString out),
            ("stderr", VString err)
          ]
  -- 6.6 does not escape an interpolated value, so a value that may
  -- carry a metacharacter has to be quoted before it is substituted.
  ("shell_quote", [VString v]) ->
    pure (VString ("'" <> T.replace "'" "'\\''" v <> "'"))
  -- 15.6 parallel/async ------------------------------------------------------
  ("spawn", [f]) -> do
    a <- async (apply f [] [])
    trackSpawned (hookAsync hooks) (siteOf f) a
    pure (VAsync (AsyncHandle a))
  ("await", [VAsync (AsyncHandle a)]) -> awaitHandle a
  ("all", [VArray xs]) -> do
    let handles = [a | VAsync (AsyncHandle a) <- V.toList xs]
    -- All of them are consumed, even if one fails before the rest are
    -- reached.
    mapM_ (trackAwaited (hookAsync hooks)) handles
    VArray . V.fromList <$> mapM awaitHandle handles
  ("race", [VArray xs]) -> do
    let handles = [a | VAsync (AsyncHandle a) <- V.toList xs]
    case handles of
      [] -> throwIO (domainError "race on an empty array")
      _ -> do
        -- Every handle given to race is consumed: the winner is
        -- received, and the rest are cancelled.
        mapM_ (trackAwaited (hookAsync hooks)) handles
        (_, r) <- waitAnyCatch handles
        mapM_ cancel handles
        either throwIO pure r
  -- 15.7 error handling ---------------------------------------------------------
  ("recover", [body, handler]) -> do
    r <- try' (apply body [] [])
    case r of
      Right v -> pure v
      Left failure -> apply handler [lfError failure] []
  ("fail", [err]) -> throwIO (userFailure err)
  -- Internal: command-sugar failure (spec 6.6), tagged with
  -- E-RUNTIME-COMMAND-NONZERO for diagnostics (14.5).
  ("%commandFail", [err]) -> throwIO (LaskFailure (Just ERuntimeCommandNonzero) err [])
  ("error", [VNumber code, VString msg]) -> pure (errorValue code msg)
  -- 15.8 serialization ------------------------------------------------------------
  ("to_json", [v]) -> either throwIO (pure . VString) (encodeFormat "json" v)
  ("from_json", [VString s]) -> either throwIO pure (decodeFormat "json" s)
  ("encode", [v, VString fmt]) -> either throwIO (pure . VString) (encodeFormat fmt v)
  ("decode", [VString s, VString fmt]) -> either throwIO pure (decodeFormat fmt s)
  ("base64_encode", [VString v]) ->
    pure (VString (TE.decodeUtf8 (B64.encode (TE.encodeUtf8 v))))
  ("base64_decode", [VString v]) -> case B64.decode (TE.encodeUtf8 (standardB64 v)) of
    Left _ -> throwIO (ioFailure EIoDataDecode ("invalid base64: '" <> v <> "'"))
    Right bs -> case TE.decodeUtf8' bs of
      Left _ -> throwIO (ioFailure EIoDataDecode "base64 does not decode to valid UTF-8")
      Right t -> pure (VString t)
  ("sha256", [VString v]) -> pure (VString (hex (SHA256.hash (TE.encodeUtf8 v))))
  ("md5", [VString v]) -> pure (VString (hex (MD5.hash (TE.encodeUtf8 v))))
  -- 15.11 filesystem ----------------------------------------------------------
  -- The environment is positional and required, exactly as for
  -- run: there is no default execution environment (spec
  -- 10.1), so no read or write can reach a filesystem the program did
  -- not name.
  ("read_file", [VString p, VEnv env]) -> file env (FileRead p)
  ("write_file", [VString p, VString c, VEnv env]) -> file env (FileWrite p c)
  ("file_exists", [VString p, VEnv env]) -> file env (FileExists p)
  ("remove_file", [VString p, VEnv env]) -> file env (FileRemove p)
  ("make_dir", [VString p, VEnv env]) -> file env (FileMakeDir p)
  ("list_dir", [VString p, VEnv env]) -> file env (FileListDir p)
  ("glob", [VString p, VEnv env]) -> file env (FileGlob p)
  -- 15.9 environment access / secret marking ---------------------------------
  -- Reading the environment does not by itself make a value secret:
  -- masking is opt-in through `!!` / `mark_secret` (spec 6.10, 12.8).
  -- get_ presupposes presence, so an unset variable is a failure and
  -- not a value (spec 15.1, 15.9). find_env is the form that looks.
  ("get_env", [VString key]) -> do
    envs <- getEnvironment
    case lookup (T.unpack key) envs of
      Just value -> pure (VString (T.pack value))
      Nothing ->
        throwIO . runtimeFailure ERuntimeAccess $
          "environment variable is not set: '" <> key <> "'"
  ("find_env", [VString key]) ->
    maybe VNull (VString . T.pack) <$> lookupEnv (T.unpack key)
  -- The desugaring target of `!!` secret bindings (spec 6.10): register
  -- the value for log masking (12.8) and hand it back untouched.
  ("has_env", [VString key]) -> VBool . maybe False (const True) <$> lookupEnv (T.unpack key)
  ("get_env_or", [VString key, VString fallback]) ->
    VString . maybe fallback T.pack <$> lookupEnv (T.unpack key)
  ("mark_secret", [VString value]) -> do
    registerSecret value
    pure (VString value)
  -- An absent secret has no text to mask, so nothing is registered: in
  -- particular not the text an absent value would print as.
  ("mark_secret", [VNull]) -> pure VNull
  -- 15.10 path operations. Lexical and POSIX whatever the host is, so
  -- a path computed here still means the same inside a container.
  ("path_join", [VArray parts]) ->
    pure (VString (P.pathJoin [t | VString t <- V.toList parts]))
  ("dirname", [VString p]) -> pure (VString (P.dirname p))
  ("basename", [VString p]) -> pure (VString (P.basename p))
  ("extname", [VString p]) -> pure (VString (P.extname p))
  ("normalize_path", [VString p]) -> pure (VString (P.normalizePath p))
  ("is_absolute_path", [VString p]) -> pure (VBool (P.isAbsolutePath p))
  -- 15.12 diagnostic output. Masked at the source, so no sink can
  -- retain an unmasked copy (spec 12.8).
  ("log", [VString message]) -> do
    masked <- maskSecrets message
    hookLog hooks masked
    pure VVoid
  -- 15.13 nondeterministic -----------------------------------------------
  ("uuid", []) -> VString <$> uuidV4
  ("random_string", [VNumber n])
    | not (isInteger n) || n < 0 ->
        throwIO (domainError "random_string takes a non-negative integer length")
    | otherwise -> VString <$> randomString (truncate (toRealTo n))
  _ ->
    throwIO . runtimeFailure ERuntimeCast $
      "invalid builtin call: '" <> name <> "' with " <> T.pack (show (length args)) <> " arguments"
  where
    file env op = hookRunFile hooks env op >>= either throwIO pure

    num = pure . VNumber

    -- An argument outside a built-in's domain (spec 14.5).
    domainError = runtimeFailure ERuntimeValue

    emptyArray fname =
      runtimeFailure ERuntimeAccess (fname <> " on an empty array")

    textArray = VArray . V.fromList . map VString

    -- Index arguments are truncated toward zero and clamped into the
    -- value, so slicing never fails (spec 15.3, 15.4).
    clampIx n x = max 0 (min n (truncate (toRealTo x)))

    sliceV xs a b =
      let n = V.length xs
          lo = clampIx n a
          hi = clampIx n b
       in if hi <= lo then V.empty else V.slice lo (hi - lo) xs

    padWith atStart s w p
      | T.null p = s
      | T.length s >= width = s
      | otherwise =
          let fill = T.take (width - T.length s) (T.replicate width p)
           in if atStart then fill <> s else s <> fill
      where
        width = max 0 (truncate (toRealTo w))

    -- Only Number and String reach here: the element type of sort is
    -- restricted statically (spec 15.4).
    compareOrdered a b = case (a, b) of
      (VNumber x, VNumber y) -> compare x y
      (VString x, VString y) -> compare x y
      _ -> EQ

    indexOfBy p xs = go' 0 xs
      where
        go' _ [] = -1 :: Int
        go' i (x : rest) = if p x then i else go' (i + 1) rest

    nubValues = go' []
      where
        go' seen [] = reverse seen
        go' seen (x : rest)
          | any (valueEq x) seen = go' seen rest
          | otherwise = go' (x : seen) rest

    withRegex pat k = case Re.compile pat of
      Left e -> throwIO (runtimeFailure ERuntimeRegex e)
      Right re -> k re

    hex = TE.decodeUtf8 . B16.encode

    -- Accept the URL-safe alphabet and missing padding (spec 15.8).
    standardB64 v =
      let core = T.map (\c -> case c of '-' -> '+'; '_' -> '/'; other -> other) (T.strip v)
          pad = case T.length core `mod` 4 of
            0 -> ""
            r -> T.replicate (4 - r) "="
       in core <> pad

    uuidV4 = do
      bs <- getEntropy 16
      let tagged =
            [ case i of
                6 -> (w .&. 0x0f) .|. 0x40 -- version 4
                8 -> (w .&. 0x3f) .|. 0x80 -- variant 10x
                _ -> w
            | (i, w) <- zip [(0 :: Int) ..] (BS.unpack bs)
            ]
          h = hex (BS.pack tagged)
          part from len = T.take len (T.drop from h)
      pure (T.intercalate "-" [part 0 8, part 8 4, part 12 4, part 16 4, part 20 12])

    randomString n = go' n []
      where
        alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"
        -- 252 = 36 * 7: rejecting the remainder keeps every symbol
        -- equally likely instead of favouring the first four.
        go' 0 acc = pure (T.pack (reverse acc))
        go' k acc = do
          bs <- getEntropy (max 16 k)
          let usable =
                [ alphabet !! (fromIntegral w `mod` (36 :: Int))
                | w <- BS.unpack bs,
                  w < (252 :: Word8)
                ]
              taken = take k usable
          go' (k - length taken) (reverse taken <> acc)

    toRealTo :: Scientific -> Double
    toRealTo = toRealFloat

    -- Scientific division can diverge on repeating decimals; go
    -- through Double (integer results stay exact via fromFloatDigits).
    divS a b = fromFloatDigits (toRealTo a / toRealTo b)
    modS a b
      | isInteger a && isInteger b =
          fromInteger (floor (toRealTo a) `mod` floor (toRealTo b))
      | otherwise =
          let d = toRealTo a - toRealTo b * fromInteger (floor (toRealTo a / toRealTo b))
           in fromFloatDigits d

    truthy (VBool b) = b
    truthy _ = False

    try' :: IO a -> IO (Either LaskFailure a)
    try' = try

    -- A failed computation's failure is re-raised as it was (spec 6.3).
    awaitHandle a = do
      trackAwaited (hookAsync hooks) a
      r <- waitCatch a
      either throwIO pure r
