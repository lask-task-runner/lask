{-# LANGUAGE OverloadedStrings #-}

-- | The front end reports every independent error, and no error that
-- only follows from another (spec 14.3).
module Language.LaskSpec (spec) where

import Data.Text (Text)
import Language.Lask (compileWith)
import Language.Lask.Diagnostic (Diagnostic (..))
import Language.Lask.ErrorCode
import Language.Lask.Span (Position (..), Span (..))
import Test.Hspec

-- | The errors of a program held in memory, as the file, line and code
-- of each, in the order they are reported.
errorsIn :: [(FilePath, Text)] -> IO [(FilePath, Int, ErrorCode)]
errorsIn files = do
  r <- compileWith reader "main.lask"
  pure $ case r of
    Right _ -> []
    Left ds -> [(f, l, diagCode d) | d <- ds, Span (Position f l _) _ <- [diagSpan d]]
  where
    reader p = pure (maybe (Left "not found") Right (lookup p files))

errors :: Text -> IO [(Int, ErrorCode)]
errors src = map (\(_, l, c) -> (l, c)) <$> errorsIn [("main.lask", src)]

spec :: Spec
spec = do
  describe "type errors (spec 14.3)" $ do
    it "reports an error in each declaration" $
      errors "a(): Number = \"s\"\nb(): String = 1\n"
        `shouldReturn` [(1, ETypeMismatch), (2, ETypeMismatch)]

    it "reports only the first error within one declaration" $
      errors "a(): Number = do {\n  x: Number = \"s\"\n  y: String = 1\n  x\n}\n"
        `shouldReturn` [(2, ETypeMismatch)]

    it "checks a use of a failed declaration against its annotations" $
      errors "a(): Number = \"s\"\nb() = a() + 1\nc(): String = a()\n"
        `shouldReturn` [(1, ETypeMismatch), (3, ETypeMismatch)]

    it "keeps keyword parameters of a failed declaration whose annotations say all of it" $
      errors "f(x: Number, --k: String = \"a\"): Number = \"s\"\ng() = f(1, k = \"b\")\nh(): String = f(2)\n"
        `shouldReturn` [(1, ETypeMismatch), (3, ETypeMismatch)]

    it "says nothing more about a use of a failed declaration without a return type" $
      errors "a() = 1 + \"s\"\nb(): String = a()\nc(): String = 1\n"
        `shouldReturn` [(1, ETypeMismatch), (3, ETypeMismatch)]

    it "says nothing more about a use of a failed declaration with an unannotated keyword parameter" $
      errors "f(--k = \"a\"): Number = \"s\"\ng(): String = f(k = \"b\")\n"
        `shouldReturn` [(1, ETypeMismatch)]

    it "says nothing more about a use of a failed type alias" $
      errors "type P = Record<a: Number, a: String>\nx: P = { a: 1 }\ny: String = 1\n"
        `shouldReturn` [(1, ETypeFieldDuplicate), (3, ETypeMismatch)]

    it "says nothing more about a command whose declaration failed" $
      errors "command { \"go\" } on 1\ng() = $ go build\nh(): String = 1\n"
        `shouldReturn` [(1, ETypeCommandEnv), (3, ETypeMismatch)]

    it "keeps an error that a recursive reference causes" $
      errors "a() = b()\nb() = a()\n" `shouldReturn` [(1, ETypeMismatch)]

    it "reports the errors of every module, in file order" $
      errorsIn
        [ ("main.lask", "import { f, g } from \"./lib.lask\"\nx(): String = f()\ny() = g()\n"),
          ("lib.lask", "export f(): Number = \"s\"\nexport g() = 1 + \"s\"\n")
        ]
        `shouldReturn` [ ("lib.lask", 1, ETypeMismatch),
                         ("lib.lask", 2, ETypeMismatch),
                         ("main.lask", 2, ETypeMismatch)
                       ]

  describe "name errors (spec 14.3)" $ do
    it "type checks the declarations that hold none" $
      errors "a() = nope\nb(): String = 1\nc() = undefined_too\n"
        `shouldReturn` [(1, ENameUndefined), (2, ETypeMismatch), (3, ENameUndefined)]

    it "says nothing more about a use of a declaration that holds one" $
      errors "a() = nope\nb(): String = a()\n" `shouldReturn` [(1, ENameUndefined)]

    it "says nothing more about a use of a name an import could not bind" $
      errorsIn
        [ ("main.lask", "import { nope, ok } from \"./lib.lask\"\nx() = nope()\ny(): String = ok()\n"),
          ("lib.lask", "export ok(): Number = 1\n")
        ]
        `shouldReturn` [("main.lask", 1, ENameUndefined), ("main.lask", 3, ETypeMismatch)]

    it "says nothing more about a use of a type alias that holds one" $
      errors "type T = Nope\nx: T = 1\ny: String = 1\n"
        `shouldReturn` [(1, ENameUndefined), (3, ETypeMismatch)]

  describe "syntax errors (spec 14.3)" $ do
    it "reports one in each top-level declaration, and nothing of the later stages" $
      errors "a = 1 2\nb: String = 1\nc = = 4\nd = 5\n"
        `shouldReturn` [(1, ESyntaxUnexpectedToken), (3, ESyntaxUnexpectedToken)]

    it "reports only the first lexical error" $
      errors "a = 1 @ 2\nb = 3 @ 4\n" `shouldReturn` [(1, ESyntaxUnexpectedToken)]

    it "goes on at the next declaration after a bracket that is never closed" $
      errors "a() = (1 +\nb(): String = 1\nc() = ]\n"
        `shouldReturn` [(2, ESyntaxUnexpectedToken), (3, ESyntaxUnexpectedToken)]
