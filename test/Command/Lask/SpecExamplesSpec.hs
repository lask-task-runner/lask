-- | Every @lask@ code block of @doc/spec.md@ passes @lask check@ on its
-- own (spec chapter 2). A block that relies on context it does not show
-- is marked @lask fragment@ and skipped.
--
-- The examples are where the specification and the implementation are
-- read side by side, so an example the implementation rejects is either
-- a stale example or a divergence, and both are worth a failing test.
module Command.Lask.SpecExamplesSpec (spec) where

import Command.Lask.Harness
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Test.Hspec

data Block = Block
  { exLine :: Int,
    exSection :: String,
    exSource :: String
  }

-- | The blocks fenced as exactly @```lask@, with the line of the fence
-- and the nearest heading above it.
examples :: String -> [Block]
examples = go "" . zip [1 ..] . lines
  where
    go _ [] = []
    go sec ((n, l) : rest)
      | "#" `isPrefixOf` l = go l rest
      | trim l == "```lask" = Block n sec (unlines (dedent body)) : go sec beyond
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
    forM_ found $ \ex ->
      it ("spec.md:" <> show (exLine ex) <> " " <> exSection ex) $ \lask ->
        withProject [("main.lask", exSource ex)] $ \dir ->
          runLask lask dir ["check"] "" `shouldReturn` Result 0 "the module is valid\n" ""
