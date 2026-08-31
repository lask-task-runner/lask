{-# LANGUAGE OverloadedStrings #-}

-- | Documentation comments (spec 3.1): the contiguous block of
-- comments directly above a declaration, and the tag conventions
-- (@\@param@, @\@return@, @\@example@, @\@hidden@) layered on top of
-- it. Consumed by editor integration (hover) and by CLI help
-- (spec 11.6); documentation has no effect on the semantics.
module Language.Lask.Doc
  ( DocComment (..),
    emptyDoc,
    docBlockAbove,
    parseDoc,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.Span (Position (..), Span (..), spanText)
import Language.Lask.Utils (kebabToSnake)

-- | A parsed documentation comment. Every field is optional: a
-- declaration with no comment above it yields 'emptyDoc'.
data DocComment = DocComment
  { docSummary :: Maybe Text,
    docDescription :: Maybe Text,
    -- | Parameter name (already mapped to snake_case, spec 11.2) and
    -- its documentation, in order of appearance.
    docParams :: [(Text, Text)],
    docReturn :: Maybe Text,
    docExamples :: [Text],
    docHidden :: Bool
  }
  deriving (Show, Eq)

emptyDoc :: DocComment
emptyDoc = DocComment Nothing Nothing [] Nothing [] False

-- | The raw documentation text of the declaration starting on
-- @declLine@: the run of comments that ends on the line immediately
-- above it and continues upward without a gap, with the comment
-- markers stripped and the pieces joined by newlines.
--
-- @comments@ are the comment spans of @src@, as returned by
-- 'Language.Lask.Lexer.lexTokensWithComments'.
docBlockAbove :: Text -> [Span] -> Int -> Maybe Text
docBlockAbove src comments declLine =
  case blockEndingAt (declLine - 1) of
    [] -> Nothing
    block -> Just (T.intercalate "\n" (map commentText block))
  where
    -- Chains upwards line by line: a blank line (or any token) leaves
    -- no comment ending there and terminates the block.
    blockEndingAt endLine =
      case [c | c@(Span _ (Position _ el _)) <- comments, el == endLine] of
        (c@(Span (Position _ sl _) _) : _) -> blockEndingAt (sl - 1) <> [c]
        _ -> []

    commentText = stripMarkers . spanText src

    stripMarkers t =
      let noLine = maybe t T.stripStart (T.stripPrefix "//" t)
          noBlock = case T.stripPrefix "/*" noLine of
            Just rest -> T.strip (maybe rest id (T.stripSuffix "*/" rest))
            Nothing -> noLine
       in T.stripEnd noBlock

-- | Split the raw text of a documentation comment into its summary,
-- description and tags (spec 3.1).
--
-- The first paragraph is the summary, collapsed to a single line
-- because that is where it is used. The paragraphs up to the first
-- tag line are the description. Unknown tags are not errors: they are
-- carried through into the description as written.
--
-- Example:
--
-- >>> docSummary (parseDoc (T.pack "Build it.\n\n@param out-dir where"))
-- Just "Build it."
--
-- >>> docParams (parseDoc (T.pack "Build it.\n\n@param out-dir where"))
-- [("out_dir","where")]
parseDoc :: Text -> DocComment
parseDoc raw = foldl applyTag base (tagChunks tagLines)
  where
    (bodyLines, tagLines) = break isTagLine (T.lines raw)

    base =
      emptyDoc
        { docSummary = summary,
          docDescription = description
        }

    paragraphs = splitParagraphs bodyLines
    summary = case paragraphs of
      (p : _) -> Just (T.unwords (concatMap T.words p))
      [] -> Nothing
    description = joinParagraphs (drop 1 paragraphs)

    applyTag doc (name, payload) = case name of
      "param" -> case T.break isSpaceChar (T.stripStart payload) of
        (pName, rest)
          | not (T.null pName) ->
              doc {docParams = docParams doc <> [(kebabToSnake pName, T.strip rest)]}
        _ -> doc
      "return" -> doc {docReturn = nonEmpty payload}
      "example" -> case nonEmpty payload of
        Just e -> doc {docExamples = docExamples doc <> [e]}
        Nothing -> doc
      "hidden" -> doc {docHidden = True}
      -- Forward compatibility (spec 3.1): an unrecognised tag is kept
      -- verbatim rather than dropped or reported.
      _ ->
        doc
          { docDescription =
              appendParagraph (docDescription doc) ("@" <> name <> payload)
          }

-- | Group tag lines into @(tag name, payload)@ pairs. A line that is
-- not itself a tag line continues the tag above it.
tagChunks :: [Text] -> [(Text, Text)]
tagChunks = go . dropWhile (not . isTagLine)
  where
    go [] = []
    go (l : rest) =
      let (cont, rest') = break isTagLine rest
          body = T.drop 1 (T.stripStart l)
          (name, payload) = T.break isSpaceChar body
          full = T.intercalate "\n" (payload : cont)
       in (name, T.stripEnd full) : go rest'

isTagLine :: Text -> Bool
isTagLine = T.isPrefixOf "@" . T.stripStart

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t'

splitParagraphs :: [Text] -> [[Text]]
splitParagraphs ls = case dropWhile isBlank ls of
  [] -> []
  rest -> let (p, more) = break isBlank rest in p : splitParagraphs more
  where
    isBlank = T.null . T.strip

joinParagraphs :: [[Text]] -> Maybe Text
joinParagraphs ps = nonEmpty (T.intercalate "\n\n" (map (T.intercalate "\n") ps))

appendParagraph :: Maybe Text -> Text -> Maybe Text
appendParagraph existing extra = case existing of
  Just d -> Just (d <> "\n\n" <> T.stripEnd extra)
  Nothing -> nonEmpty extra

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null (T.strip t) then Nothing else Just (T.strip t)
