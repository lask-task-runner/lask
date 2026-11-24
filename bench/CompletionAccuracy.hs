{-# LANGUAGE OverloadedStrings #-}


-- | How well the language server's completion finds the name a person
-- is writing.
--
-- For every identifier in the examples that the file has already
-- written once, the identifier is cut back to its first one or two
-- characters, completion is asked for at the cut, and the answer is
-- narrowed and sorted as an editor does: candidates whose label starts
-- with the typed characters, ordered by 'sortText'. The report gives,
-- per kind of position, how often the name is offered at all, first,
-- and among the first five, and how many candidates the editor shows.
--
--   stack bench lask:completion-accuracy
--   stack bench lask:completion-accuracy --ba 'example/02-language'
module Main (main) where

import Control.Lens ((^.))
import Control.Monad (forM)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii)
import Data.List (findIndex, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Clock (getMonotonicTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.LSP.Lask (completionAt)
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Types (Position (..))
import Language.Lask.Lexer (lexTokens)
import qualified Language.Lask.Lexer.Token as Tok
import qualified Language.Lask.Span as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.FilePath (takeExtension, takeFileName, (</>))
import Text.Printf (printf)

-- | One identifier to be completed.
data Case = Case
  { caseFile :: FilePath,
    caseLine :: Int, -- 0-based
    caseCol :: Int, -- 0-based, where the identifier starts
    caseName :: Text,
    caseKind :: String
  }

data Outcome = Outcome
  { outKind :: String,
    outRank :: Maybe Int, -- 0-based place in the editor's list
    outShown :: Int,
    outWhere :: String,
    outSeconds :: Double
  }

-- | At most this many cases per file and prefix length, spread evenly
-- over the file, so a long file does not decide the result alone.
perFile :: Int
perFile = 80

main :: IO ()
main = do
  args <- getArgs
  let roots = if null args then ["example"] else args
  files <- concat <$> mapM laskFiles roots
  outcomes <- fmap concat . forM files $ \f -> do
    src <- TIO.readFile f
    let cs = spread perFile (cases f src)
    fmap concat . forM [1, 2] $ \k ->
      forM [c | c <- cs, T.length (caseName c) > k] $ \c -> (,) k <$> run k src c
  verbose <- lookupEnv "MISSES"
  case verbose of
    Just kind -> mapM_ putStrLn [outWhere o | (1, o) <- outcomes, outKind o == kind, outRank o == Nothing]
    Nothing -> pure ()
  -- TIMES=<file> writes how long each answer took.
  times <- lookupEnv "TIMES"
  case times of
    Just f -> writeFile f (unlines [printf "%d\t%.2f\t%s\t%s" k (1000 * outSeconds o) (outKind o) (outWhere o) | (k, o) <- outcomes])
    Nothing -> pure ()
  -- SLOW=<ms> lists the answers that took longer.
  slow <- lookupEnv "SLOW"
  case slow of
    Just ms -> mapM_ (\o -> printf "%8.0f ms %s\n" (1000 * outSeconds o) (outWhere o)) [o | (_, o) <- outcomes, 1000 * outSeconds o > read ms]
    Nothing -> pure ()
  printf "%d files\n" (length files)
  mapM_ (report outcomes) [1, 2]
  -- How long one answer takes, the analysis of the buffer included.
  let ms = sort [1000 * outSeconds o | (_, o) <- outcomes]
      at q = ms !! min (length ms - 1) (floor (q * fromIntegral (length ms) :: Double))
  printf "\nlatency of %d answers: median %.1f ms, p95 %.1f ms, max %.1f ms\n" (length ms) (at 0.5) (at 0.95) (last ms)

laskFiles :: FilePath -> IO [FilePath]
laskFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure [root | takeExtension root == ".lask"]
    else do
      es <- sort <$> listDirectory root
      -- `.lask` holds the dependency cache: other people's code.
      concat <$> mapM laskFiles [root </> e | e <- es, takeFileName e /= ".lask"]

-- | The identifiers of a file that it wrote before, with the kind of
-- position each sits in. A name being bound (a parameter, a
-- declaration, a binding in a block) is left out: nothing can offer a
-- name that does not exist yet. Lines with characters outside ASCII
-- are left out too, so a column is a column in every unit.
cases :: FilePath -> Text -> [Case]
cases path src = case lexTokens path src of
  Left _ -> []
  Right ts ->
    let srcLines = T.splitOn "\n" src
        ascii l = maybe False (T.all isAscii) (lookup l (zip [1 ..] srcLines))
        vals = map Tok.spannedValue ts
        go _ _ [] = []
        go seen stack ((prev, Tok.Spanned sp t, next) : rest) =
          let stack' = case t of
                _ | t `elem` [Tok.TLParen, Tok.TLBracket, Tok.TLBrace] -> t : stack
                Tok.TOp Tok.OpLt | maybe False isUpper' prev -> t : stack
                _ | t `elem` [Tok.TRParen, Tok.TRBracket, Tok.TRBrace] -> drop 1 stack
                Tok.TOp Tok.OpGt | take 1 stack == [Tok.TOp Tok.OpLt] -> drop 1 stack
                _ -> stack
           in case (ident t, sp) of
                (Just n, S.Span (S.Position _ l c) _)
                  | n `elem` seen,
                    T.length n >= 2,
                    ascii l,
                    Just kind <- kindOf (listToMaybe stack) prev t next ->
                      Case path (l - 1) (c - 1) n kind : go seen stack' rest
                  | otherwise -> go (n : seen) stack' rest
                _ -> go seen stack' rest
     in go [] [] (zip3 (Nothing : map Just vals) ts (map Just (drop 1 vals) <> [Nothing]))
  where
    ident (Tok.TLowerId n) = Just n
    ident (Tok.TUpperId n) = Just n
    ident _ = Nothing
    isUpper' (Tok.TUpperId _) = True
    isUpper' _ = False

-- | The kind of position, from the innermost open bracket and the
-- tokens around it; 'Nothing' where a name is being bound.
kindOf :: Maybe Tok.Token -> Maybe Tok.Token -> Tok.Token -> Maybe Tok.Token -> Maybe String
kindOf inner prev t next
  | prev == Just Tok.TDot = Just "member"
  | prev == Just (Tok.TLowerId "within") = Just "within"
  | prev `elem` map Just [Tok.TDashDash, Tok.TKw Tok.KType, Tok.TLowerId "access", Tok.TKw Tok.KAs] = Nothing
  | next `elem` [Just Tok.TColon, Just Tok.TBangBang] =
      if inner == Just Tok.TLBrace then Just "record key" else Nothing
  | next `elem` [Just Tok.TAssign, Just Tok.TLParen, Just (Tok.TOp Tok.OpLt)],
    prev `elem` [Nothing, Just Tok.TNewline, Just Tok.TLBrace, Just (Tok.TKw Tok.KInternal), Just (Tok.TKw Tok.KExport)] =
      Nothing
  | Tok.TUpperId _ <- t = Just "type"
  | otherwise = Just "value"

spread :: Int -> [a] -> [a]
spread n xs
  | len <= n = xs
  | otherwise = [x | (i, x) <- zip [0 :: Int ..] xs, (i * n) `div` len /= ((i + 1) * n) `div` len]
  where
    len = length xs

-- | Cut the identifier back to its first @k@ characters and ask.
run :: Int -> Text -> Case -> IO Outcome
run k src c = do
  let srcLines = T.splitOn "\n" src
      cut i l
        | i == caseLine c = T.take (caseCol c + k) l <> T.drop (caseCol c + T.length (caseName c)) l
        | otherwise = l
      edited = T.intercalate "\n" (zipWith cut [0 ..] srcLines)
      typed = T.toLower (T.take k (caseName c))
  t0 <- getMonotonicTime
  items <- completionAt (caseFile c) edited (Position (fromIntegral (caseLine c)) (fromIntegral (caseCol c + k)))
  -- What the server sends is the answer encoded whole, so the time
  -- counts encoding it, and nothing of the analysis is kept after.
  t1 <- BL.length (J.encode items) `seq` getMonotonicTime
  let shown =
        sortOn
          (\i -> (fromMaybe (i ^. L.label) (i ^. L.sortText), i ^. L.label))
          [i | i <- items, typed `T.isPrefixOf` T.toLower (i ^. L.label)]
      rank = findIndex (\i -> i ^. L.label == caseName c) shown
      count = length shown
      at = printf "%s:%d:%d %s | %s" (caseFile c) (caseLine c + 1) (caseCol c + 1) (caseName c) (T.strip (srcLines !! caseLine c))
  pure $! rank `seq` count `seq` length at `seq` Outcome (caseKind c) rank count at (t1 - t0)

report :: [(Int, Outcome)] -> Int -> IO ()
report all' k = do
  let os = [o | (k', o) <- all', k' == k]
      byKind = Map.fromListWith (flip (<>)) [(outKind o, [o]) | o <- os]
  printf "\n%d typed character%s\n" k (if k == 1 then "" else "s" :: String)
  printf "%-11s %6s %8s %8s %8s %8s\n" ("position" :: String) ("cases" :: String) ("offered" :: String) ("top 1" :: String) ("top 5" :: String) ("shown" :: String)
  mapM_ (uncurry row) (Map.toList byKind <> [("all", os)])
  where
    row :: String -> [Outcome] -> IO ()
    row name os =
      printf
        "%-11s %6d %7.1f%% %7.1f%% %7.1f%% %8d\n"
        name
        (length os)
        (pct (const True) os)
        (pct (== 0) os)
        (pct (< 5) os)
        (median (map outShown os))
    pct :: (Int -> Bool) -> [Outcome] -> Double
    pct p os = 100 * fromIntegral (length [() | o <- os, maybe False p (outRank o)]) / fromIntegral (max 1 (length os))
    median [] = 0
    median xs = sort xs !! (length xs `div` 2)
