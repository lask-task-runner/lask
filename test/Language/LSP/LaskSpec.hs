{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.LaskSpec (spec) where

import Control.Lens ((^.))
import Control.Monad (forM_, unless)
import Data.Char (isDigit)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Language.LSP.Lask (completionAt, hoverAt, lexSemanticTokens, uriPath)
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Types
import Language.Lask (checkText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | (line, startChar, length, type), all 0-based as in the protocol.
type Atom = (Int, Int, Int, SemanticTokenTypes)

toks :: Text -> [Atom]
toks src = case lexSemanticTokens "test.lask" src of
  Right ts ->
    [ (fromIntegral l, fromIntegral c, fromIntegral len, typ)
    | SemanticTokenAbsolute l c len typ _ <- ts
    ]
  Left e -> error (show e)

hasAtom :: Text -> Atom -> Expectation
hasAtom src atom = toks src `shouldSatisfy` elem atom

spec :: Spec
spec = do
  describe "comments" $ do
    it "emits comment tokens for line comments" $
      -- "// hi" occupies columns 0-4 on line 0.
      hasAtom "// hi\na = 1" (0, 0, 5, SemanticTokenTypes_Comment)
    it "emits comment tokens for each line of a block comment" $ do
      let src = "/* a\nb */\nx = 1"
      hasAtom src (0, 0, 4, SemanticTokenTypes_Comment)
      hasAtom src (1, 0, 4, SemanticTokenTypes_Comment)
    it "keeps surrounding code tokens intact" $ do
      let src = "a = 1 // trailing"
      hasAtom src (0, 0, 1, SemanticTokenTypes_Variable)
      hasAtom src (0, 6, 11, SemanticTokenTypes_Comment)

  describe "string interpolation" $ do
    -- s = "Hi, #{name}!"
    let src = "s = \"Hi, #{name}!\""
    it "highlights the interpolated expression" $
      hasAtom src (0, 11, 4, SemanticTokenTypes_Variable)
    it "keeps the string segments around the interpolation" $ do
      -- Opening segment: from the quote up to the nested tokens.
      toks src
        `shouldSatisfy` any
          (\(l, c, _, typ) -> l == 0 && c == 4 && typ == SemanticTokenTypes_String)
      -- Closing segment ends at the closing quote.
      toks src
        `shouldSatisfy` any
          (\(_, c, _, typ) -> c > 11 && typ == SemanticTokenTypes_String)
    it "highlights nested expressions recursively" $
      -- x + 1 inside the interpolation
      hasAtom "s = \"v=#{x + 1}\"" (0, 11, 1, SemanticTokenTypes_Operator)

  describe "command expressions" $ do
    it "marks $ as a function token" $
      -- f() = $ echo hi
      hasAtom "f() = $ echo hi" (0, 6, 1, SemanticTokenTypes_Function)
    it "marks stream selectors as part of the function head" $ do
      hasAtom "f() = $* ls" (0, 6, 2, SemanticTokenTypes_Function)
      hasAtom "f() = $2 ls" (0, 6, 2, SemanticTokenTypes_Function)
    it "renders the command text as string segments" $
      toks "f() = $ echo hi"
        `shouldSatisfy` any
          (\(_, c, _, typ) -> c > 6 && typ == SemanticTokenTypes_String)
    it "highlights interpolation inside commands" $
      -- f() = $ echo #{msg}
      hasAtom "f() = $ echo #{msg}" (0, 15, 3, SemanticTokenTypes_Variable)
    it "highlights the environment expression in commands" $
      -- f() = $[#alpine:3.20] ls   (env head starts at col 8, length 12)
      hasAtom "f() = $[#alpine:3.20] ls" (0, 8, 12, SemanticTokenTypes_Macro)

  describe "plain tokens" $ do
    it "still maps keywords, types, numbers" $ do
      let src = "add(x: Number): Number = x + 1"
      hasAtom src (0, 7, 6, SemanticTokenTypes_Type)
      hasAtom src (0, 29, 1, SemanticTokenTypes_Number)

  describe "hover" $ do
    let src =
          "// Adds one.\n\
          \// Second line.\n\
          \inc(x: Number): Number = x + 1\n\
          \y = inc(2)\n"
        hoverText l c = do
          h <- hoverAt "test.lask" src (Position l c)
          pure $ case h of
            Just (Hover (InL (MarkupContent _ t)) _) -> Just t
            _ -> Nothing
    it "shows the type of a referenced function" $ do
      t <- hoverText 3 4 -- `inc` in `y = inc(2)`
      t `shouldSatisfy` maybe False (T.isInfixOf "inc: Function<Number, Number>")
    it "shows the comment block above the declaration" $ do
      t <- hoverText 3 4
      t `shouldSatisfy` maybe False (T.isInfixOf "Adds one.\nSecond line.")
    it "shows type and docs on the declaration name itself" $ do
      t <- hoverText 2 0 -- `inc` at its definition
      t `shouldSatisfy` maybe False (T.isInfixOf "inc: Function<Number, Number>")
      t `shouldSatisfy` maybe False (T.isInfixOf "Adds one.")
    it "shows the type of local parameter references" $ do
      t <- hoverText 2 25 -- trailing `x` in the body
      t `shouldSatisfy` maybe False (T.isInfixOf "x: Number")
    it "returns nothing on blank positions" $ do
      t <- hoverText 3 1 -- whitespace after `y`
      t `shouldBe` Nothing
    it "shows types of imported symbols with their docs" $
      withSystemTempDirectory "lask-hover" $ \dir -> do
        writeFile (dir </> "lib.lask") "// Doubles a number.\ndouble(n: Number): Number = n * 2\n"
        let docSrc = "import { double } from \"./lib.lask\"\nf() = double(3)\n"
        h <- hoverAt (dir </> "main.lask") docSrc (Position 1 6) -- `double` reference
        let t = case h of
              Just (Hover (InL (MarkupContent _ x)) _) -> Just x
              _ -> Nothing
        t `shouldSatisfy` maybe False (T.isInfixOf "double: Function<Number, Number>")
        t `shouldSatisfy` maybe False (T.isInfixOf "Doubles a number.")

  describe "document paths" $ do
    it "strips the file:// scheme from document URIs" $
      uriPath (Uri "file:///tmp/proj/main.lask") `shouldBe` "/tmp/proj/main.lask"
    it "resolves imports relative to the document's directory" $
      withSystemTempDirectory "lask-lsp" $ \dir -> do
        writeFile (dir </> "a.lask") "a = 1\n"
        let docUri = Uri ("file://" <> T.pack (dir </> "main.lask"))
        ds <- checkText (uriPath docUri) "import { a } from \"./a.lask\"\nx = a"
        ds `shouldBe` []

  describe "completion" $ do
    it "offers the parameters of the enclosing function" $ do
      -- Cursor on `a` in the body of `add`.
      ls <- labels "test.lask" "add(a: Number, b: Number) = a + b" 0 28
      ls `shouldSatisfy` elem "a"
      ls `shouldSatisfy` elem "b"
    it "does not offer parameters of other declarations" $ do
      ls <- labels "test.lask" "f(only: Number) = 1\ng() = 2" 1 6
      ls `shouldSatisfy` notElem "only"
    it "offers do-block bindings made before the cursor" $ do
      let src = "f() = do {\n  x = 1\n  y = 2\n}\n"
      ls <- labels "test.lask" src 2 6
      ls `shouldSatisfy` elem "x"
      ls `shouldSatisfy` notElem "y"
    it "offers builtins with their type as detail" $ do
      items <- completionAt "test.lask" "x = to" (Position 0 6)
      map (^. L.label) items `shouldSatisfy` elem "toLower"
      [i ^. L.detail | i <- items, i ^. L.label == "toLower"]
        `shouldBe` [Just "Function<String, String>"]
    it "leaves narrowing to the client" $ do
      -- The client matches case-insensitively and fuzzily, so a
      -- candidate must not be dropped just because the typed word is
      -- not a literal prefix of it.
      ls <- labels "test.lask" "x = cA" 0 6
      ls `shouldSatisfy` elem "concatArray"
    it "offers reserved words" $ do
      ls <- labels "test.lask" "im" 0 2
      ls `shouldSatisfy` elem "import"
    it "ranks module top-level names above builtins and reserved words" $ do
      items <- completionAt "test.lask" "inc(x: Number) = x + 1\ny = i" (Position 1 5)
      let sortTextOf n = [i ^. L.sortText | i <- items, i ^. L.label == n]
      sortTextOf "inc" `shouldBe` [Just "2-inc"]
      sortTextOf "import" `shouldBe` [Just "6-import"]
    it "offers keyword parameters of the call being written" $ do
      let src = "greet(--name: String = \"World\") = name\ny = greet()"
      items <- completionAt "test.lask" src (Position 1 10)
      [i ^. L.insertText | i <- items, i ^. L.label == "name"]
        `shouldBe` [Just "name = "]
    it "offers the public members of a namespace after a dot" $
      withSystemTempDirectory "lask-completion" $ \dir -> do
        writeFile (dir </> "lib.lask") "double(n: Number): Number = n * 2\nhidden = 1\n"
        let src = "import * as m from \"./lib.lask\"\nf() = m."
        ls <- labels (dir </> "main.lask") src 1 8
        ls `shouldMatchList` ["double", "hidden"]
    it "still offers builtins and reserved words when the buffer does not parse" $ do
      ls <- labels "test.lask" "y = }\nz = to" 1 6
      ls `shouldSatisfy` elem "toJson"

  describe "completion resilience" $ do
    it "offers top-level names while a new declaration is being named" $ do
      -- The name being typed is the whole line, so nothing parses.
      ls <- labels "test.lask" "inc(x: Number) = x + 1\ni" 1 1
      ls `shouldSatisfy` elem "inc"
    it "offers top-level names when another declaration is broken" $ do
      ls <- labels "test.lask" "inc(x: Number) = x + 1\nbroken(\ny = i" 2 5
      ls `shouldSatisfy` elem "inc"
    it "offers top-level names inside an unclosed block" $ do
      ls <- labels "test.lask" "f() = do {\n  x = 1\ngg = g" 2 6
      ls `shouldSatisfy` elem "gg"
    it "offers names bound by an import when the buffer does not parse" $ do
      let src = "import * as m from \"./x.lask\"\nbroken(\ny = m"
      ls <- labels "test.lask" src 2 5
      ls `shouldSatisfy` elem "m"
    it "never offers the healing placeholder" $ do
      ls <- labels "test.lask" "" 0 0
      ls `shouldSatisfy` notElem "_"

  describe "completion after a dot" $ do
    it "offers the fields of a record" $ do
      items <- completionAt "test.lask" "a = {x: 2, y: \"s\"}\nb = a." (Position 1 6)
      map (^. L.label) items `shouldMatchList` ["x", "y"]
      [i ^. L.detail | i <- items, i ^. L.label == "y"] `shouldBe` [Just "String"]
    it "offers the fields of a command result" $ do
      let src = "f() = do {\n  r = $* echo hi\n  return r.\n}"
      ls <- labels "test.lask" src 2 11
      ls `shouldMatchList` ["code", "stderr", "stdout"]
    it "offers nothing for a receiver it cannot resolve" $ do
      ls <- labels "test.lask" "b = zzz." 0 8
      ls `shouldBe` []
    it "offers nothing for a chained receiver" $ do
      ls <- labels "test.lask" "a = {x: {y: 1}}\nb = a.x." 1 8
      ls `shouldBe` []

  describe "completion edge cases" $ do
    it "offers builtins in an empty document" $ do
      ls <- labels "test.lask" "" 0 0
      ls `shouldSatisfy` elem "toLower"
    it "handles a column past the end of the line" $ do
      ls <- labels "test.lask" "y = 1\nz = " 1 99
      ls `shouldSatisfy` elem "toLower"
    it "handles CRLF line endings" $ do
      ls <- labels "test.lask" "inc(x: Number) = x + 1\r\ny = i" 1 5
      ls `shouldSatisfy` elem "inc"
    it "offers locals inside a command interpolation" $ do
      ls <- labels "test.lask" "greet(name: String) = $ echo #{na}" 0 33
      ls `shouldSatisfy` elem "name"

  describe "completion invariants" $
    forM_ corpora $ \(name, src, _) ->
      it ("hold at every position of " <> name) $
        forM_ (positions src) $ \(l, c) -> do
          items <- completionAt "test.lask" src (Position l c)
          let ls = map (^. L.label) items
              context = name <> " at " <> show (l, c) <> ": "
          -- Comparing the whole list forces the result, so a crash in
          -- the analysis surfaces at the position that caused it.
          assert (context <> "duplicate labels in " <> show ls) (nub ls == ls)
          forM_ items $ \i ->
            assert
              (context <> "bad sortText " <> show (i ^. L.sortText) <> " on " <> show (i ^. L.label))
              (sortTextOk i)

  describe "completion while typing" $
    forM_ corpora $ \(name, src, names) -> do
      it ("offers every top-level name of " <> name <> " from a fresh line") $
        forM_ names $ \n ->
          forM_ [1 .. T.length n] $ \k -> do
            let typed = T.take k n
                src' = src <> "\nscratch = " <> typed
                l = fromIntegral (length (T.splitOn "\n" src') - 1)
                c = fromIntegral (10 + k)
            ls <- labels "test.lask" src' l c
            assert
              (name <> ": " <> show n <> " missing after typing " <> show typed)
              (n `elem` ls)
      it ("answers independently of the typed prefix in " <> name) $ do
        -- The client reuses one answer for the rest of the word, so
        -- the answer must not change as the word grows.
        let scratch typed = src <> "\nscratch = " <> typed
            posOf typed =
              ( fromIntegral (length (T.splitOn "\n" (scratch typed)) - 1),
                fromIntegral (10 + T.length typed)
              )
        base <- uncurry (labels "test.lask" (scratch "")) (posOf "")
        forM_ names $ \n ->
          forM_ [1 .. T.length n] $ \k -> do
            let typed = T.take k n
            ls <- uncurry (labels "test.lask" (scratch typed)) (posOf typed)
            assert
              (name <> ": candidates changed after typing " <> show typed)
              (sort ls == sort base)
      it ("survives every truncation of " <> name) $
        forM_ [0, 3 .. T.length src] $ \k -> do
          let typed = T.take k src
              typedLines = T.splitOn "\n" typed
              l = fromIntegral (length typedLines - 1)
              c = fromIntegral (T.length (lastLine typedLines))
          ls <- labels "test.lask" typed l c
          assert
            (name <> " truncated to " <> show k <> ": duplicate labels in " <> show ls)
            (nub ls == ls)

  describe "completion on the repository's own sources" $
    forM_ realSources $ \(file, name) ->
      it ("offers " <> show name <> " while typing in " <> file) $ do
        src <- TIO.readFile file
        forM_ [1 .. T.length name] $ \k -> do
          let typed = T.take k name
              src' = src <> "\nscratch = " <> typed
              l = fromIntegral (length (T.splitOn "\n" src') - 1)
              c = fromIntegral (10 + k)
          ls <- labels file src' l c
          assert
            (file <> ": " <> show name <> " missing after typing " <> show typed)
            (name `elem` ls)

-- | Real sources from the repository, each with a name it declares.
-- The terraform example does not resolve its dependency unless it has
-- been fetched, which is exactly the degraded state to cover.
realSources :: [(FilePath, Text)]
realSources =
  [ ("main.lask", "doctest"),
    ("example/01-basic/main.lask", "cowsay"),
    ("example/03-terraform/main.lask", "as_string")
  ]

-- | Sources exercising the states a buffer passes through while it is
-- being edited. Each carries the top-level names it defines.
corpora :: [(String, Text, [Text])]
corpora =
  [ ( "a valid module",
      "inc(x: Number): Number = x + 1\ntotal = inc(1)\n",
      ["inc", "total"]
    ),
    ( "a do block",
      "run() = do {\n  a = 1\n  b = a + 1\n  return b\n}\n",
      ["run"]
    ),
    ( "a module with a syntax error",
      "inc(x: Number) = x + 1\nbroken(\ntotal = 2\n",
      ["inc", "total"]
    ),
    ( "an unclosed block",
      "inc(x: Number) = x + 1\nrun() = do {\n  a = 1\n",
      ["inc", "run"]
    ),
    ( "CRLF line endings",
      "inc(x: Number) = x + 1\r\ntotal = inc(1)\r\n",
      ["inc", "total"]
    ),
    ( "Japanese comments and strings",
      "// 日本語のコメント\ngreet() = \"こんにちは\"\n",
      ["greet"]
    ),
    ( "an unresolved import",
      "import * as m from \"./missing.lask\"\ninc(x: Number) = x + 1\n",
      ["inc"]
    )
  ]

-- | Every cursor position in a source, including one past each line.
positions :: Text -> [(UInt, UInt)]
positions src =
  [ (fromIntegral l, fromIntegral c)
  | (l, line) <- zip [0 :: Int ..] (T.splitOn "\n" src),
    c <- [0 .. T.length line]
  ]

-- | Sort text is always @\<rank\>-\<label\>@, which is what makes the
-- client show the candidates in resolution order.
sortTextOk :: CompletionItem -> Bool
sortTextOk i = case i ^. L.sortText of
  Nothing -> False
  Just s ->
    let (rank, rest) = T.breakOn "-" s
     in not (T.null rank) && T.all isDigit rank && rest == "-" <> (i ^. L.label)

lastLine :: [Text] -> Text
lastLine ts = case reverse ts of
  (x : _) -> x
  [] -> ""

-- | An assertion that names the failing case, so a sweep reports the
-- position that broke rather than just which check it was.
assert :: String -> Bool -> Expectation
assert context ok = unless ok (expectationFailure context)

labels :: FilePath -> Text -> UInt -> UInt -> IO [Text]
labels path src l c = map (^. L.label) <$> completionAt path src (Position l c)
