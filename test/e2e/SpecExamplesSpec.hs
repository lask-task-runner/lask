-- | Every @lask@ code block of @doc/spec.md@ passes @lask check@ on its
-- own (spec chapter 2). A block that relies on context it does not show
-- is marked @lask fragment@ and skipped, unless its comments name the
-- errors it shows: then exactly those are what @lask check@ reports.
--
-- The examples are where the specification and the implementation are
-- read side by side, so an example the implementation rejects is either
-- a stale example or a divergence, and both are worth a failing test.
module SpecExamplesSpec (spec) where

import Control.Monad (forM_)
import Data.Char (isDigit, isSpace, isUpper)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Support.Harness
import Test.Hspec

data Block = Block
  { exFragment :: Bool,
    exLine :: Int,
    exSection :: String,
    exSource :: String
  }

-- | The blocks fenced as exactly @```lask@ or @```lask fragment@, with
-- the line of the fence and the nearest heading above it.
examples :: String -> [Block]
examples = go "" . zip [1 ..] . lines
  where
    go _ [] = []
    go sec ((n, l) : rest)
      | "#" `isPrefixOf` l = go l rest
      | trim l == "```lask" = Block False n sec (unlines (dedent body)) : go sec beyond
      | trim l == "```lask fragment" = Block True n sec (unlines (dedent body)) : go sec beyond
      -- Any other block is skipped whole, so that a `#` comment in a
      -- shell example is not taken for a heading.
      | "```" `isPrefixOf` trim l = go sec beyond
      | otherwise = go sec rest
      where
        (bodyLines, closing) = break ((== "```") . trim . snd) rest
        body = map snd bodyLines
        beyond = drop 1 closing
    trim = dropWhileEnd isSpace . dropWhile isSpace
    -- A block inside a list item is indented with it.
    dedent ls =
      let indent = minimum (maxBound : [length (takeWhile (== ' ') l) | l <- ls, any (not . isSpace) l])
       in map (drop indent) ls

spec :: Spec
spec = do
  found <- runIO (examples <$> readFile "doc/spec.md")
  beforeAll findLask $ describe "doc/spec.md examples" $ do
    it "are found" $ \_ -> length found `shouldSatisfy` (> 0)
    forM_ (filter (not . exFragment) found) $ \ex ->
      it ("spec.md:" <> show (exLine ex) <> " " <> exSection ex) $ \lask ->
        withProject [("main.lask", exSource ex)] $ \dir ->
          runLask lask dir ["check"] "" `shouldReturn` Result 0 "the module is valid\n" ""
    describe "fragments that name their errors" $
      forM_ [(ex, cs) | ex <- found, exFragment ex, let cs = namedErrors (exSource ex), not (null cs)] $ \(ex, cs) ->
        it ("spec.md:" <> show (exLine ex) <> " " <> exSection ex) $ \lask ->
          withProject [("main.lask", exSource ex)] $ \dir -> do
            r <- runLask lask dir ["check"] ""
            resExit r `shouldBe` 1
            reportedErrors (resOut r) `shouldBe` cs

-- | The error codes a fragment's comments name, as @// E-...@, each
-- once. Advisories are left out: an advisory is reported only for a
-- program with no error, which such a fragment is not.
namedErrors :: String -> [String]
namedErrors = nubSort . concatMap codesIn . mapMaybe comment . lines
  where
    comment l = case breakOn "//" l of
      (_, "") -> Nothing
      (_, c) -> Just c
    codesIn = filter ("E-" `isPrefixOf`) . map (takeWhile isCodeChar) . words

-- | The error codes in @lask check@'s output, each once.
reportedErrors :: String -> [String]
reportedErrors out = nubSort [takeWhile isCodeChar w | l <- lines out, w <- take 2 (words l), "E-" `isPrefixOf` w]

isCodeChar :: Char -> Bool
isCodeChar c = isUpper c || isDigit c || c == '-'

nubSort :: [String] -> [String]
nubSort = Set.toList . Set.fromList

breakOn :: String -> String -> (String, String)
breakOn pat = go ""
  where
    go acc s
      | pat `isPrefixOf` s = (reverse acc, s)
      | otherwise = case s of
          [] -> (reverse acc, "")
          c : rest -> go (c : acc) rest
