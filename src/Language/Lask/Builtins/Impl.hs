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
    callBuiltin,
  )
where

import Control.Concurrent.Async (async, cancel, waitAnyCatch, waitCatch)
import Control.Exception (SomeException, fromException, throwIO, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, isInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Language.Lask.ErrorCode
import Language.Lask.Runtime.Value
import Language.Lask.Serialize (encodeValue, encodeValuePretty, valueFromJson)
import System.Environment (getEnvironment)

-- | Apply a function value to positional and keyword arguments.
type Apply = Value -> [Value] -> [(Text, Value)] -> IO Value

-- | Run a command in a resolved environment:
-- returns (exit code, stdout, stderr) or an infrastructure failure.
type CommandRunner = EnvValue -> Text -> IO (Either LaskFailure (Int, Text, Text))

callBuiltin :: Apply -> CommandRunner -> Text -> [Value] -> [(Text, Value)] -> IO Value
callBuiltin apply runCmd name args kwArgs = case (name, args) of
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
  -- 15.5 command execution --------------------------------------------------
  ("run_command", [VString cmd]) -> do
    let env = case lookup "env" kwArgs of
          Just (VEnv e) -> e
          _ -> EnvValue "local" Map.empty
    r <- runCmd env cmd
    case r of
      Left failure -> throwIO failure
      Right (code, out, err) ->
        pure . VRecord . Map.fromList $
          [ ("code", VNumber (fromIntegral code)),
            ("stdout", VString out),
            ("stderr", VString err)
          ]
  -- 15.6 parallel/async ------------------------------------------------------
  ("spawn", [f]) -> do
    a <- async (apply f [] [])
    pure (VAsync (AsyncHandle a))
  ("await", [VAsync (AsyncHandle a)]) -> awaitHandle a
  ("all", [VArray xs]) -> do
    let handles = [a | VAsync (AsyncHandle a) <- V.toList xs]
    VArray . V.fromList <$> mapM awaitHandle handles
  ("race", [VArray xs]) -> do
    let handles = [a | VAsync (AsyncHandle a) <- V.toList xs]
    case handles of
      [] -> throwIO (runtimeFailure ERuntimeAwaitFailed "race on an empty array")
      _ -> do
        (_, r) <- waitAnyCatch handles
        mapM_ cancel handles
        either rethrowAsync pure r
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
  ("to_json", [v]) -> pure (VString (encodeValue v))
  ("from_json", [VString s]) -> decodeJson s
  ("encode", [v, VString fmt]) -> case fmt of
    "json" -> pure (VString (encodeValue v))
    "pretty-json" -> pure (VString (encodeValuePretty v))
    _ -> throwIO (ioFailure EIoDataDecode ("unsupported format: '" <> fmt <> "'"))
  ("decode", [VString s, VString fmt]) -> case fmt of
    "json" -> decodeJson s
    "pretty-json" -> decodeJson s
    _ -> throwIO (ioFailure EIoDataDecode ("unsupported format: '" <> fmt <> "'"))
  -- other --------------------------------------------------------------------
  ("get_env", [VString key]) -> do
    envs <- getEnvironment
    case lookup (T.unpack key) envs of
      Just value -> pure (VString (T.pack value))
      Nothing -> pure VNull
  _ ->
    throwIO . runtimeFailure ERuntimeCast $
      "invalid builtin call: '" <> name <> "' with " <> T.pack (show (length args)) <> " arguments"
  where
    num = pure . VNumber

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

    decodeJson s = case A.eitherDecode (BL.fromStrict (TE.encodeUtf8 s)) of
      Right j -> pure (valueFromJson j)
      Left e -> throwIO (ioFailure EIoDataDecode ("invalid JSON: " <> T.pack e))

    try' :: IO a -> IO (Either LaskFailure a)
    try' = try

    awaitHandle a = do
      r <- waitCatch a
      either rethrowAsync pure r

    rethrowAsync :: SomeException -> IO a
    rethrowAsync ex = case fromException ex of
      Just failure -> throwIO (failure :: LaskFailure)
      Nothing ->
        throwIO (runtimeFailure ERuntimeAwaitFailed ("async computation failed: " <> T.pack (show ex)))
