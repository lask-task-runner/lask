{-# LANGUAGE OverloadedStrings #-}

-- | Command schemas (spec appendix A), called directly: what makes a
-- schema valid ('fromStatic'), and how the words of a command are read
-- against one ('checkCommand', 'walkCommand'). How the elaborator finds
-- a schema and reports what these find is covered by
-- "Language.Lask.ElaborateSpec" and the corpus.
module Language.Lask.CommandSchemaSpec (spec) where

import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Lask.CommandSchema hiding (Spec)
import Language.Lask.Span (Span (..))
import Language.Lask.Syntax.Shell (ShellWord (..), SimpleCommand (..))
import Support.Pending (knownBug)
import Test.Hspec

-- Static values ----------------------------------------------------------------

st :: StaticF -> Static
st = Static NoSpan

str :: Text -> Static
str = st . VString

bool :: Bool -> Static
bool = st . VBool

arr :: [Static] -> Static
arr = st . VArray

obj :: [(Text, Static)] -> Static
obj = st . VObject

-- | An option of these names, with further fields.
opt :: [Text] -> [(Text, Static)] -> Static
opt names more = obj (("names", arr (map str names)) : more)

-- | A spec with these options, and further fields.
spec' :: [Static] -> [(Text, Static)] -> Static
spec' opts more = obj (("options", arr opts) : more)

-- | A schema with a root and subcommands by path.
schema :: Static -> [(Text, Static)] -> Static
schema root [] = obj [("root", root)]
schema root subs = obj [("root", root), ("subcommands", obj subs)]

valid :: Static -> Schema
valid s = either (error . T.unpack . snd) id (fromStatic s)

accepted :: Static -> Expectation
accepted s = fromStatic s `shouldSatisfy` isRight

-- | Rejected, with a message that mentions this.
rejectedFor :: Text -> Static -> Expectation
rejectedFor needle s = case fromStatic s of
  Left (_, msg) -> msg `shouldSatisfy` (needle `T.isInfixOf`)
  Right _ -> expectationFailure ("expected a schema rejected for " <> show needle)

-- Words -------------------------------------------------------------------------

-- | A plain literal word.
lit :: Text -> ShellWord
lit t = ShellWord NoSpan t True True True

-- | A word that begins with literal text and has a hole after it,
-- which expands to one field (spec 6.6).
holed :: Text -> ShellWord
holed t = ShellWord NoSpan t False False True

-- | A word that may expand to other than one field: an unquoted
-- parameter expansion.
spread :: ShellWord
spread = ShellWord NoSpan "" False False False

-- | The words of a command string written with literals alone.
ws :: Text -> [ShellWord]
ws = map lit . T.words

-- | What checking the arguments finds, each as sure (an error) or not
-- (an advisory).
findings :: Schema -> [ShellWord] -> [Bool]
findings s args = map fdSure (checkCommand s "tool" (SimpleCommand (lit "tool") args False))

-- | As 'findings', for a command the shell reading stopped inside.
findingsCut :: Schema -> [ShellWord] -> [Bool]
findingsCut s args = map fdSure (checkCommand s "tool" (SimpleCommand (lit "tool") args True))

-- | The message of the one finding.
message :: Schema -> [ShellWord] -> Text
message s args = case checkCommand s "tool" (SimpleCommand (lit "tool") args False) of
  [f] -> fdMessage f
  other -> error ("expected one finding, got " <> show other)

-- | Each word read, with what it was read as.
roles :: Schema -> [ShellWord] -> [(Text, Text)]
roles s args = [(swPrefix w, roleOf r) | (w, r) <- wkRoles (walkCommand s "tool" args)]
  where
    roleOf r = case r of
      RoleOption path name _ -> "option " <> name <> at path
      RoleValue path name _ -> "value of " <> name <> at path
      RoleSubcommand path -> "subcommand " <> path
      RoleArgument path -> "argument" <> at path
    at "" = ""
    at p = " in " <> p

ok :: [Bool]
ok = []

-- Schemas -----------------------------------------------------------------------

-- | ls in the gnu style.
ls :: Schema
ls =
  valid . schema
    ( spec'
        [ opt ["-l"] [],
          opt ["-a", "--all"] [],
          opt ["-w", "--width"] [("value", str "required")],
          opt ["--color"] [("value", str "optional"), ("values", arr [str "auto", str "never"])]
        ]
        [("closed", bool True)]
    )
    $ []

-- | The go of appendix A, single-dash, with subcommands.
go :: Schema
go =
  valid $
    schema
      (obj [("closed", bool True), ("style", str "single-dash")])
      [ ( "test",
          spec'
            [ opt ["-v"] [],
              opt ["-run"] [("value", str "required")],
              opt ["-covermode"] [("value", str "required"), ("values", arr [str "set", str "count", str "atomic"])]
            ]
            [("closed", bool True), ("intermixed", bool True)]
        ),
        ("run", obj [("closed", bool True), ("stop_at_first_positional", bool True)]),
        ("mod", obj [("closed", bool True)]),
        ("mod tidy", spec' [opt ["-v"] []] [("closed", bool True)])
      ]

spec :: Spec
spec = do
  describe "a valid schema (appendix A)" $ do
    it "gives a root" $
      rejectedFor "root" (obj [])
    it "has the style gnu or single-dash" $ do
      accepted (schema (obj [("style", str "gnu")]) [])
      accepted (schema (obj [("style", str "single-dash")]) [])
      rejectedFor "style" (schema (obj [("style", str "posix")]) [])
    it "has the value none, required or optional" $ do
      accepted (schema (spec' [opt ["-o"] [("value", str "none")]] []) [])
      rejectedFor "value" (schema (spec' [opt ["-o"] [("value", str "maybe")]] []) [])
    it "gives an option at least one name" $
      rejectedFor "at least one name" (schema (spec' [opt [] []] []) [])
    it "begins each name with '-', or with '+' when it has prefix" $ do
      rejectedFor "is not an option name" (schema (spec' [opt ["v"] []] []) [])
      rejectedFor "'+'" (schema (spec' [opt ["+v"] []] []) [])
      accepted (schema (spec' [opt ["+"] [("value", str "required"), ("prefix", bool True)]] []) [])
    it "is neither - nor -- as a name" $ do
      rejectedFor "is not an option name" (schema (spec' [opt ["-"] []] []) [])
      rejectedFor "is not an option name" (schema (spec' [opt ["--"] []] []) [])
    it "has no whitespace, = or quotation character in a name" $
      mapM_
        (\n -> rejectedFor "is not an option name" (schema (spec' [opt [n] []] []) []))
        ["-a b", "--a=b", "-'a'", "-\"a\"", "-a\tb"]
    it "names no option twice in one spec" $
      rejectedFor "given twice" (schema (spec' [opt ["-v"] [], opt ["-x", "-v"] []] []) [])
    it "names no option twice together with the global options of its ancestors" $ do
      let root = spec' [opt ["-v"] [("global", bool True)], opt ["-q"] []] []
      rejectedFor "given twice" (schema root [("sub", spec' [opt ["-v"] []] [])])
      rejectedFor "given twice" (schema root [("sub", obj []), ("sub deep", spec' [opt ["-v"] []] [])])
      -- an option that is not global is the root's own
      accepted (schema root [("sub", spec' [opt ["-q"] []] [])])
    it "lists at least one value, and none for an option that takes no value" $ do
      rejectedFor "at least one value" (schema (spec' [opt ["-o"] [("value", str "required"), ("values", arr [])]] []) [])
      rejectedFor "takes no value" (schema (spec' [opt ["-o"] [("values", arr [str "a"])]] []) [])
      accepted (schema (spec' [opt ["-o"] [("value", str "optional"), ("values", arr [str "a"])]] []) [])
    it "gives an option with prefix a value" $
      rejectedFor "prefix" (schema (spec' [opt ["-W"] [("prefix", bool True)]] []) [])
    it "writes a subcommand path as words separated by single spaces, none beginning with -" $ do
      let sub k = schema (obj []) [("a", obj []), (k, obj [])]
      accepted (sub "a b")
      mapM_ (rejectedFor "subcommand path" . sub) ["a  b", "a b ", " a", "-x", "a -b", "a 'b'", "a\tb"]
    it "has the parent path of a subcommand among the keys" $ do
      rejectedFor "needs its parent" (schema (obj []) [("mod tidy", obj [])])
      rejectedFor "needs its parent 'a b'" (schema (obj []) [("a", obj []), ("a b c", obj [])])

  describe "reading the words (appendix A)" $ do
    it "reads a value after an option that requires one, whatever it is" $ do
      findings go (ws "test -run -v") `shouldBe` ok
      findings go (ws "test -run --") `shouldBe` ok
      roles go (ws "test -run -v") `shouldBe` [("test", "subcommand test"), ("-run", "option -run in test"), ("-v", "value of -run in test")]
    it "checks a literal value against the values listed, and not a value with a hole" $ do
      findings go (ws "test -covermode count") `shouldBe` ok
      findings go (ws "test -covermode fast") `shouldBe` [True]
      findings go (ws "test -covermode=fast") `shouldBe` [True]
      findings go [lit "test", lit "-covermode", holed "c"] `shouldBe` ok
    it "says which values the option takes" $
      message go (ws "test -covermode fast") `shouldBe` "the option '-covermode' of 'tool test' takes one of 'set', 'count', 'atomic', not 'fast'"
    it "ends at an option with stops, once it and its value are read" $ do
      let py = valid (schema (spec' [opt ["-m"] [("value", str "required"), ("stops", bool True)], opt ["-u"] []] [("closed", bool True)]) [])
      findings py (ws "-u -m pytest -k smoke --bogus") `shouldBe` ok
      findings py (ws "--bogus -m pytest") `shouldBe` [True]
    it "ends at a word that may expand to other than one field" $
      findings ls [lit "-l", spread, lit "--bogus"] `shouldBe` ok
    it "ends at a word with a hole, unless its option name and = are literal" $ do
      findings ls [holed "-", lit "--bogus"] `shouldBe` ok
      findings ls [holed "--width="] `shouldBe` ok
      findings ls [holed "--bogus="] `shouldBe` [True]
      findings ls [holed "-l="] `shouldBe` ok
    it "ends at --" $
      findings ls (ws "-l -- --bogus") `shouldBe` ok
    it "reads - alone as an argument" $
      roles ls (ws "-") `shouldBe` [("-", "argument")]

  describe "the gnu style (appendix A)" $ do
    it "reads --name and --name=value as a long option" $ do
      findings ls (ws "--all --width 80 --width=80") `shouldBe` ok
      findings ls (ws "--bogus") `shouldBe` [True]
    it "reads -abc as single-letter options, the first that takes a value taking the rest" $ do
      roles ls (ws "-law80")
        `shouldBe` [("-law80", "option -l"), ("-law80", "option -a"), ("-law80", "option -w")]
      findings ls (ws "-lw") `shouldBe` [True]
      findings ls (ws "-lx") `shouldBe` [True]
    it "rejects =value for an option that takes no value, whatever the spec says" $ do
      findings ls (ws "--all=yes") `shouldBe` [True]
      let open = valid (schema (spec' [opt ["--all"] []] []) [])
      findings open (ws "--all=yes") `shouldBe` [True]
    it "takes an optional value only after =" $ do
      findings ls (ws "--color dir") `shouldBe` ok
      roles ls (ws "--color dir") `shouldBe` [("--color", "option --color"), ("dir", "argument")]
      findings ls (ws "--color=always") `shouldBe` [True]
    it "intermixes options and arguments by default" $
      findings ls (ws "dir --bogus") `shouldBe` [True]
    it "requires a value at the end, unless the shell reading stopped inside the command" $ do
      findings ls (ws "--width") `shouldBe` [True]
      findingsCut ls (ws "--width") `shouldBe` ok

  describe "the single-dash style (appendix A)" $ do
    it "reads -name and -name=value as an option" $ do
      findings go (ws "test -run=X -v") `shouldBe` ok
      findings go (ws "test -vv") `shouldBe` [True]
    it "reads --name as -name, unless an option has the two-dash name" $ do
      findings go (ws "test --run X --v") `shouldBe` ok
      let java = valid (schema (spec' [opt ["-cp"] [("value", str "required")], opt ["--class-path"] [("value", str "required")]] [("closed", bool True), ("style", str "single-dash")]) [])
      roles java (ws "--class-path a --cp b")
        `shouldBe` [("--class-path", "option --class-path"), ("a", "value of --class-path"), ("--cp", "option -cp"), ("b", "value of -cp")]
    it "does not intermix by default, and does where the spec says so" $ do
      let sd = valid (schema (spec' [opt ["-v"] []] [("closed", bool True), ("style", str "single-dash")]) [])
      findings sd (ws "file -bogus") `shouldBe` ok
      findings go (ws "test ./... -bogus") `shouldBe` [True]
    it "inherits the style of its parent" $
      roles go (ws "mod tidy -v") `shouldBe` [("mod", "subcommand mod"), ("tidy", "subcommand mod tidy"), ("-v", "option -v in mod tidy")]

  describe "options with prefix (appendix A)" $ do
    let java =
          valid
            ( schema
                ( spec'
                    [ opt ["-D"] [("value", str "required"), ("prefix", bool True)],
                      opt ["-X", "-XX:"] [("value", str "required"), ("prefix", bool True)],
                      opt ["-version"] []
                    ]
                    [("closed", bool True), ("style", str "single-dash")]
                )
                []
            )
    it "reads a word that names no option but begins with the name of one" $ do
      findings java (ws "-Dapp.env=prod -Xmx2g -version") `shouldBe` ok
      findings java (ws "-Ymx2g") `shouldBe` [True]
    it "takes the longest of several names a word begins with" $
      roles java (ws "-XX:+UseG1GC") `shouldBe` [("-XX:+UseG1GC", "option -XX:")]
    it "prefers an option the word names exactly" $
      roles java (ws "-version") `shouldBe` [("-version", "option -version")]
    it "reads a +word as the option named with +, and otherwise as an argument" $ do
      let rustup = valid (schema (spec' [opt ["+"] [("value", str "required"), ("prefix", bool True)]] [("closed", bool True)]) [("show", obj [("closed", bool True)])])
      roles rustup (ws "+nightly show") `shouldBe` [("+nightly", "option +"), ("show", "subcommand show")]
      let plain = valid (schema (obj [("closed", bool True)]) [])
      roles plain (ws "+x") `shouldBe` [("+x", "argument")]
    it "reads a gnu-style word that begins with a prefix option of several letters" $ do
      let cc = valid (schema (spec' [opt ["-isystem"] [("value", str "required"), ("prefix", bool True)], opt ["-c"] []] [("closed", bool True)]) [])
      -- -isystem/usr/include names no option: read as letters, -i is
      -- none, so by the rule of prefix options it is -isystem with the
      -- value /usr/include ("in either style").
      knownBug "in the gnu style a word beginning with one dash is read as a cluster first, and an unknown letter is reported before the prefix options are consulted" $
        findings cc (ws "-isystem/usr/include -c") `shouldBe` ok

  describe "subcommands (appendix A)" $ do
    it "selects a subcommand by the first positional argument" $ do
      findings go (ws "test -v") `shouldBe` ok
      findings go (ws "tset") `shouldBe` [True]
      message go (ws "tset") `shouldBe` "'tool' has no subcommand 'tset'; did you mean 'test'?"
    it "selects a nested subcommand by its path" $ do
      findings go (ws "mod tidy -v") `shouldBe` ok
      findings go (ws "mod tidy -x") `shouldBe` [True]
      findings go (ws "mod vendor") `shouldBe` [True]
    it "reads the words after a positional argument as arguments, not subcommands" $ do
      let tool = valid (schema (obj [("closed", bool True), ("subcommand_optional", bool True)]) [("a", obj [("closed", bool True)])])
      roles tool (ws "x a") `shouldBe` [("x", "argument"), ("a", "argument")]
      roles tool (ws "a x") `shouldBe` [("a", "subcommand a"), ("x", "argument in a")]
    it "gives a subcommand the global options of its ancestors, and not their own" $ do
      let tool = valid (schema (spec' [opt ["-g"] [("global", bool True)], opt ["-r"] []] [("closed", bool True)]) [("sub", obj [("closed", bool True)]), ("sub deep", obj [("closed", bool True)])])
      findings tool (ws "sub deep -g") `shouldBe` ok
      findings tool (ws "sub -r") `shouldBe` [True]
      findings tool (ws "-r -g sub") `shouldBe` ok
    it "takes an argument that names no subcommand where the subcommand is optional" $ do
      let grype = valid (schema (obj [("closed", bool True), ("subcommand_optional", bool True)]) [("db", obj [("closed", bool True)])])
      roles grype (ws "alpine:3.20") `shouldBe` [("alpine:3.20", "argument")]
      roles grype (ws "db") `shouldBe` [("db", "subcommand db")]
    it "ends at the first positional argument where the spec says so" $
      findings go (ws "run ./cmd -anything") `shouldBe` ok

  describe "how sure a finding is (appendix A)" $ do
    let open = valid (schema (spec' [opt ["--dry-run"] [], opt ["--debug"] [], opt ["-o"] [("value", str "required")]] [("abbreviations", bool True)]) [("sub", obj [])])
    it "is an advisory for an unknown option or subcommand of a spec that is not closed" $ do
      findings open (ws "--bogus") `shouldBe` [False]
      findings open (ws "other") `shouldBe` [False]
    it "is an advisory for an ambiguous abbreviation of a spec that is not closed, and an error of a closed one" $ do
      findings open (ws "--d") `shouldBe` [False]
      let closed = valid (schema (spec' [opt ["--dry-run"] [], opt ["--debug"] []] [("abbreviations", bool True), ("closed", bool True)]) [])
      findings closed (ws "--d") `shouldBe` [True]
    it "is an error for a missing value, whatever the spec says" $
      findings open (ws "-o") `shouldBe` [True]

  describe "abbreviations (appendix A)" $ do
    let mk more = valid (schema (spec' [opt ["-n", "--dry-run"] [("global", bool True)], opt ["--debug"] [("value", str "optional")]] (("closed", bool True) : more)) [("sub", obj [("closed", bool True)])])
    it "reads a unique prefix of a long name where the spec allows it" $ do
      roles (mk [("abbreviations", bool True)]) (ws "--dry") `shouldBe` [("--dry", "option --dry-run")]
      findings (mk [("abbreviations", bool True)]) (ws "--deb=x --dr") `shouldBe` ok
    it "rejects a prefix of several" $
      message (mk [("abbreviations", bool True)]) (ws "--d") `shouldBe` "'--d' of 'tool' is ambiguous: it begins '--dry-run', '--debug'"
    it "reads none by default" $
      findings (mk []) (ws "--dry") `shouldBe` [True]
    it "inherits the setting of the parent" $
      findings (mk [("abbreviations", bool True)]) (ws "sub --dry") `shouldBe` ok
