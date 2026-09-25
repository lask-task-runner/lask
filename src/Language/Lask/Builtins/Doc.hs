{-# LANGUAGE OverloadedStrings #-}

-- | Documentation of builtin symbols (spec 9.3, 15), shown by editor
-- integration (hover). A condensed reading of the specification, not
-- a replacement for it: each entry names the section it summarizes.
-- Type signatures live in "Language.Lask.Builtins.Sig".
module Language.Lask.Builtins.Doc
  ( BuiltinDoc (..),
    builtinDocs,
    renderBuiltinDoc,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data BuiltinDoc = BuiltinDoc
  { -- | The specification section the entry summarizes, e.g. @15.4@.
    bdSection :: Text,
    -- | Markdown paragraphs. The first one opens with a call written
    -- out with parameter names, since a signature carries none.
    bdParagraphs :: [Text]
  }
  deriving (Show, Eq)

-- | The markdown body of a builtin's documentation.
renderBuiltinDoc :: BuiltinDoc -> Text
renderBuiltinDoc (BuiltinDoc sec ps) =
  T.intercalate "\n\n" (ps <> ["*Built-in (spec " <> sec <> ")*"])

doc :: Text -> [Text] -> BuiltinDoc
doc = BuiltinDoc

builtinDocs :: Map Text BuiltinDoc
builtinDocs =
  Map.fromList
    [ -- 9.3 standard input
      ( "stdin",
        doc
          "9.3"
          [ "The decoded standard input of this execution, as a UTF-8 `String`. Read-only and bound once per run.",
            "Structure it in the language: `lines(stdin)`, or `from_json(stdin)` followed by `cast`. Not available in `repl`."
          ]
      ),
      -- 15.2 numeric
      ("add", doc "15.2" ["`add(a, b)` is `a + b`."]),
      ("sub", doc "15.2" ["`sub(a, b)` is `a - b`."]),
      ("mul", doc "15.2" ["`mul(a, b)` is `a * b`."]),
      ("div", doc "15.2" ["`div(a, b)` is `a / b`.", "A zero divisor is `E-RUNTIME-DIV-BY-ZERO`."]),
      ("mod", doc "15.2" ["`mod(a, b)` is `a % b`.", "A zero right-hand side is `E-RUNTIME-DIV-BY-ZERO`."]),
      ("abs", doc "15.2" ["`abs(x)` is the absolute value of `x`."]),
      ("floor", doc "15.2" ["`floor(x)` rounds `x` down to an integer."]),
      ("ceil", doc "15.2" ["`ceil(x)` rounds `x` up to an integer."]),
      ("round", doc "15.2" ["`round(x)` rounds `x` to the nearest integer."]),
      ("min", doc "15.2" ["`min(a, b)` returns the smaller argument, or `a` when they are equal."]),
      ("max", doc "15.2" ["`max(a, b)` returns the larger argument, or `a` when they are equal."]),
      ("sum", doc "15.2" ["`sum(xs)` adds the elements from left to right. The sum of an empty array is `0`."]),
      ( "pow",
        doc
          "15.2"
          [ "`pow(base, exponent)` raises `base` to `exponent`, which need not be an integer. `pow(0, 0)` is `1`.",
            "A result that is not a real number is `E-RUNTIME-VALUE`."
          ]
      ),
      ("sqrt", doc "15.2" ["`sqrt(x)` is the square root of `x`.", "A negative `x` is `E-RUNTIME-VALUE`."]),
      ( "clamp",
        doc
          "15.2"
          [ "`clamp(value, low, high)` returns `low` when `value < low`, `high` when `value > high`, and `value` otherwise.",
            "`low > high` is `E-RUNTIME-VALUE`."
          ]
      ),
      -- 15.3 string
      ("length", doc "15.3" ["`length(s)` is the number of characters in `s`.", "For arrays, use `size`."]),
      ("concat", doc "15.3" ["`concat(a, b)` concatenates two strings. For more, nest `concat` or use `join`.", "`+` is for `Number` only."]),
      ("trim", doc "15.3" ["`trim(s)` removes leading and trailing whitespace."]),
      ("to_lower", doc "15.3" ["`to_lower(s)` converts `s` to lower case."]),
      ("to_upper", doc "15.3" ["`to_upper(s)` converts `s` to upper case."]),
      ("split", doc "15.3" ["`split(s, sep)` splits `s` at every occurrence of `sep`.", "For command output, prefer `lines`."]),
      ("join", doc "15.3" ["`join(parts, sep)` concatenates `parts` with `sep` between them."]),
      ("replace", doc "15.3" ["`replace(s, from, to)` replaces every occurrence of `from` in `s` with `to`.", "For patterns, use `regex_replace`."]),
      ("contains", doc "15.3" ["`contains(s, needle)` is true when `needle` occurs in `s`. An empty `needle` is always true.", "For arrays, use `contains_array`."]),
      ("starts_with", doc "15.3" ["`starts_with(s, prefix)` is true when `s` begins with `prefix`. An empty `prefix` is always true."]),
      ("ends_with", doc "15.3" ["`ends_with(s, suffix)` is true when `s` ends with `suffix`. An empty `suffix` is always true."]),
      ( "index_of",
        doc
          "15.3"
          [ "`index_of(s, needle)` returns the zero-based position of the first occurrence of `needle`, or `-1` when it does not occur. An empty `needle` gives `0`.",
            "For arrays, use `index_of_array`."
          ]
      ),
      ( "substring",
        doc
          "15.3"
          [ "`substring(s, start, end)` returns the characters in `[start, end)`.",
            "Both endpoints are clamped to `[0, length(s)]`, so it never fails."
          ]
      ),
      ( "pad_start",
        doc
          "15.3"
          ["`pad_start(s, width, pad)` prepends repetitions of `pad` until the result is `width` characters long. `s` is returned unchanged when it is already that long or `pad` is empty."]
      ),
      ( "pad_end",
        doc
          "15.3"
          ["`pad_end(s, width, pad)` appends repetitions of `pad` until the result is `width` characters long. `s` is returned unchanged when it is already that long or `pad` is empty."]
      ),
      ("repeat", doc "15.3" ["`repeat(s, n)` concatenates `n` copies of `s`. `n <= 0` gives the empty string."]),
      ( "lines",
        doc
          "15.3"
          [ "`lines(s)` splits `s` into lines, accepting both LF and CRLF. A trailing newline adds no final empty line, and `\"\"` gives `[]`.",
            "This is the form to use on the `stdout` of a `CommandResult`."
          ]
      ),
      ( "to_string",
        doc
          "15.3"
          [ "`to_string(v)` renders a `String`, `Number`, or `Bool` as interpolation would.",
            "The argument's type must be stringifiable, which is checked at the call site. Narrow a union like `String | Null` with `case` first, and use `to_json` for structured values."
          ]
      ),
      ( "to_number",
        doc
          "15.3"
          [ "`to_number(s)` parses `s` as a JSON number, ignoring surrounding whitespace.",
            "Any other text is `E-IO-DATA-DECODE`."
          ]
      ),
      ( "regex_test",
        doc
          "15.3"
          [ "`regex_test(s, pattern)` is true when the POSIX ERE `pattern` matches anywhere in `s`.",
            "A malformed pattern is `E-RUNTIME-REGEX`. Write patterns as raw strings: `'\\d+'`."
          ]
      ),
      ( "regex_match",
        doc
          "15.3"
          [ "`regex_match(s, pattern)` returns the leftmost-longest match: element `0` is the whole match and element `i` the `i`-th capture group. No match gives `[]`, which you can check with `is_empty`.",
            "A malformed pattern is `E-RUNTIME-REGEX`. Write patterns as raw strings: `'(\\d+)'`."
          ]
      ),
      ( "regex_replace",
        doc
          "15.3"
          [ "`regex_replace(s, pattern, replacement)` replaces every non-overlapping match. In `replacement`, `$0` to `$9` stand for the match and its groups, and `$$` for a literal `$`.",
            "A malformed pattern is `E-RUNTIME-REGEX`."
          ]
      ),
      -- 15.4 array/map/record
      ("map", doc "15.4" ["`map(xs, f)` applies `f` to each element from left to right and returns the results in order."]),
      ("filter", doc "15.4" ["`filter(xs, p)` keeps the elements for which `p` is true, in order."]),
      ("reduce", doc "15.4" ["`reduce(xs, init, f)` folds `xs` from left to right, starting from `init`: `f(f(init, x0), x1)`…"]),
      ("for_each", doc "15.4" ["`for_each(xs, f)` applies `f` to each element from left to right for its side effects, discarding the results."]),
      ("append", doc "15.4" ["`append(xs, v)` returns a new array with `v` added at the end."]),
      ("concat_array", doc "15.4" ["`concat_array(xs, ys)` returns the elements of `xs` followed by those of `ys`."]),
      ( "get",
        doc
          "15.4"
          [ "`get(m, k)` returns the value bound to `k`, like `m[k]`.",
            "A missing key is `E-RUNTIME-ACCESS`. Use `get_or` to tolerate one."
          ]
      ),
      ("has_key", doc "15.4" ["`has_key(m, k)` is true when `m` has key `k`.", "To read a value with a fallback, prefer `get_or`."]),
      ("keys", doc "15.4" ["`keys(m)` returns the keys in ascending code point order."]),
      ("values", doc "15.4" ["`values(m)` returns the values in ascending code point order of their keys."]),
      ("size", doc "15.4" ["`size(xs)` is the number of elements.", "For a map, use `size(keys(m))`. For a string, use `length`."]),
      ("is_empty", doc "15.4" ["`is_empty(xs)` is `size(xs) == 0`.", "For a map, use `is_empty(keys(m))`."]),
      ("first", doc "15.4" ["`first(xs)` returns the first element.", "An empty array is `E-RUNTIME-ACCESS`."]),
      ("last", doc "15.4" ["`last(xs)` returns the last element.", "An empty array is `E-RUNTIME-ACCESS`."]),
      ("slice", doc "15.4" ["`slice(xs, start, end)` returns the elements in `[start, end)`. Both endpoints are clamped, so it never fails."]),
      ("take", doc "15.4" ["`take(xs, n)` returns the first `n` elements (`slice(xs, 0, n)`). It never fails."]),
      ("drop", doc "15.4" ["`drop(xs, n)` returns all but the first `n` elements. It never fails."]),
      ("reverse", doc "15.4" ["`reverse(xs)` returns the elements in the opposite order."]),
      ( "sort",
        doc
          "15.4"
          [ "`sort(xs)` returns the elements in ascending order. Strings are ordered by Unicode code point.",
            "The element type must be `Number` or `String`, which is checked at the call site."
          ]
      ),
      ( "sort_by",
        doc
          "15.4"
          [ "`sort_by(xs, key)` sorts by `key` applied once to each element. The sort is stable.",
            "The key type must be `Number` or `String`."
          ]
      ),
      ("contains_array", doc "15.4" ["`contains_array(xs, v)` is true when some element `== v`.", "The element type must be comparable."]),
      ("index_of_array", doc "15.4" ["`index_of_array(xs, v)` returns the index of the first element `== v`, or `-1`.", "The element type must be comparable."]),
      ( "find",
        doc
          "15.4"
          [ "`find(xs, p)` returns the first element for which `p` is true, or `Null` when there is none.",
            "Handle the `Null` case with `case` before using the result."
          ]
      ),
      ("find_index", doc "15.4" ["`find_index(xs, p)` returns the index of the first element for which `p` is true, or `-1`."]),
      ("every", doc "15.4" ["`every(xs, p)` is true when `p` holds for every element, including when `xs` is empty. It stops at the first `false`."]),
      ("any", doc "15.4" ["`any(xs, p)` is true when `p` holds for at least one element, and false when `xs` is empty. It stops at the first `true`."]),
      ("flatten", doc "15.4" ["`flatten(xss)` concatenates the inner arrays in order, removing one level of nesting."]),
      ("flat_map", doc "15.4" ["`flat_map(xs, f)` is `flatten(map(xs, f))`."]),
      ("zip", doc "15.4" ["`zip(xs, ys)` pairs elements at the same position as `{first, second}`, stopping at the shorter input."]),
      ("unique", doc "15.4" ["`unique(xs)` removes an element equal to an earlier one, keeping the first occurrence and the order.", "The element type must be comparable."]),
      ( "range",
        doc
          "15.4"
          [ "`range(start, end)` returns the integers from `start` up to but not including `end`, or `[]` when `end <= start`. Use it with `for` to loop a fixed number of times.",
            "A non-integer argument is `E-RUNTIME-VALUE`."
          ]
      ),
      ("enumerate", doc "15.4" ["`enumerate(xs)` pairs each element with its zero-based index as `{index, value}`."]),
      ("set", doc "15.4" ["`set(m, k, v)` returns a new map with `k` bound to `v`, replacing any existing value."]),
      ("remove", doc "15.4" ["`remove(m, k)` returns a new map without `k`. A missing key leaves the map unchanged."]),
      ("merge", doc "15.4" ["`merge(m1, m2)` returns a new map with the entries of both. `m2` wins on a shared key."]),
      ("get_or", doc "15.4" ["`get_or(m, k, fallback)` returns the value bound to `k`, or `fallback` when the key is missing. It never fails."]),
      ("entries", doc "15.4" ["`entries(m)` returns one `{key, value}` per entry, in ascending code point order of the key."]),
      ("from_entries", doc "15.4" ["`from_entries(es)` builds a map from `{key, value}` records. It is the inverse of `entries`, and a later duplicate key wins."]),
      ("map_values", doc "15.4" ["`map_values(m, f)` applies `f` to every value and keeps the keys."]),
      -- 15.5 command execution
      ( "run",
        doc
          "15.5"
          [ "`run(env, command)` runs `command` in `env` and returns its `CommandResult`. Unlike `$`, a non-zero exit code is not a failure.",
            "Environment resolution failure is `E-IO-ENV-RESOLVE`."
          ]
      ),
      ( "shell_quote",
        doc
          "15.5"
          [ "`shell_quote(s)` returns one POSIX shell word that expands to exactly `s`. Pass any value that can contain spaces or shell metacharacters through it before interpolating: `$ cp #{shell_quote(src)} #{shell_quote(dst)}`.",
            "The result is already quoted, so don't quote it again."
          ]
      ),
      -- 15.6 parallel/async
      ("spawn", doc "15.6" ["`spawn(f)` starts `f` concurrently and returns a handle. Get the result with `await h`."]),
      ( "all",
        doc
          "15.6"
          [ "`all(hs)` waits for every handle and returns the results in input order.",
            "It may fail as soon as any handle fails."
          ]
      ),
      ("race", doc "15.6" ["`race(hs)` returns the result of the first handle to finish, whether it succeeded or failed."]),
      -- 15.7 error handling
      ( "recover",
        doc
          "15.7"
          ["`recover(body, handler)` evaluates `body()`. If it fails, it returns `handler(e)` instead, where `e` is the `Error`."]
      ),
      ( "fail",
        doc
          "15.7"
          [ "`fail(e)` raises a failure carrying the `Error` `e`. Its result type comes from the context.",
            "If nothing catches it, the process exits with `e.code`."
          ]
      ),
      ("error", doc "15.7" ["`error(code, message)` builds the `Error` value `{code: code, message: message}`."]),
      -- 15.8 serialization / cast
      ("to_json", doc "15.8" ["`to_json(v)` encodes `v` as JSON."]),
      ( "from_json",
        doc
          "15.8"
          [ "`from_json(s)` decodes JSON into an `Any`. Convert it to a concrete type with `cast`.",
            "Invalid JSON is `E-IO-DATA-DECODE`."
          ]
      ),
      ( "encode",
        doc
          "15.8"
          [ "`encode(v, format)` serializes `v` as `json`, `pretty-json`, `yaml`, `toml`, `csv`, or `dotenv`.",
            "A value the format cannot represent is `E-RUNTIME-VALUE`."
          ]
      ),
      ( "decode",
        doc
          "15.8"
          [ "`decode(s, format)` parses `s` as `json`, `pretty-json`, `yaml`, `toml`, `csv`, or `dotenv` into an `Any`. Convert it to a concrete type with `cast`.",
            "Malformed input is `E-IO-DATA-DECODE`."
          ]
      ),
      ( "cast",
        doc
          "15.8"
          [ "`cast(v)` checks at runtime that `v` conforms to the target type `T`, which is inferred from the expected type, and returns it as a `T`: `user: Record<name: String> = cast(from_json(stdin))`.",
            "A value that doesn't conform is `E-RUNTIME-CAST`. To handle other shapes instead of failing, use `case`."
          ]
      ),
      ("base64_encode", doc "15.8" ["`base64_encode(s)` encodes the UTF-8 bytes of `s` as standard base64 with padding."]),
      ( "base64_decode",
        doc
          "15.8"
          [ "`base64_decode(s)` decodes standard or URL-safe base64, with or without padding.",
            "Invalid base64, or bytes that aren't valid UTF-8, are `E-IO-DATA-DECODE`."
          ]
      ),
      ("sha256", doc "15.8" ["`sha256(s)` returns the SHA-256 digest of the UTF-8 bytes of `s` as 64 lowercase hex characters."]),
      ( "md5",
        doc
          "15.8"
          [ "`md5(s)` returns the MD5 digest of the UTF-8 bytes of `s` as 32 lowercase hex characters.",
            "Use it only to interoperate with tools that require MD5. Where collision resistance matters, use `sha256`."
          ]
      ),
      -- 15.9 environment access / secret marking
      ( "get_env",
        doc
          "15.9"
          [ "`get_env(name)` returns the process environment variable `name`, for a variable the task requires.",
            "An unset variable is `E-RUNTIME-ACCESS`. The value isn't masked unless it's bound with `!!`."
          ]
      ),
      ( "find_env",
        doc
          "15.9"
          ["`find_env(name)` returns the process environment variable `name`, or `Null` when it isn't set. Use it for a variable the task merely accepts."]
      ),
      ("has_env", doc "15.9" ["`has_env(name)` is true when `name` is set in the process environment, even to the empty string."]),
      ("get_env_or", doc "15.9" ["`get_env_or(name, fallback)` returns the process environment variable `name`, or `fallback` when it isn't set."]),
      ( "mark_secret",
        doc
          "15.9"
          ["`mark_secret(v)` registers `v` for masking in logs and diagnostics, and returns `v` unchanged. A `!!` binding calls it for you."]
      ),
      -- 15.10 path operations
      ( "path_join",
        doc
          "15.10"
          ["`path_join(parts)` joins `parts` with `/`, drops empty parts, and normalizes the result. An absolute part discards everything before it."]
      ),
      ("dirname", doc "15.10" ["`dirname(p)` returns everything before the final component: `\"a/b/c\"` gives `\"a/b\"`, and `\"c\"` gives `\".\"`."]),
      ("basename", doc "15.10" ["`basename(p)` returns the final component, ignoring trailing slashes: `\"a/b/c.txt\"` gives `\"c.txt\"`."]),
      ("extname", doc "15.10" ["`extname(p)` returns the final extension, including the dot: `\"c.tar.gz\"` gives `\".gz\"`, and `\".env\"` gives `\"\"`."]),
      ("normalize_path", doc "15.10" ["`normalize_path(p)` collapses repeated `/` and resolves `.` and `..` lexically, without touching the filesystem."]),
      ("is_absolute_path", doc "15.10" ["`is_absolute_path(p)` is true when `p` begins with `/`."]),
      -- 15.11 filesystem
      ( "read_file",
        doc
          "15.11"
          [ "`read_file(path, env)` returns the file's contents, decoded as UTF-8, from the filesystem `env` sees.",
            "Contents that aren't valid UTF-8 are `E-IO-DATA-DECODE`. Access failures are `E-IO-FS`."
          ]
      ),
      ( "write_file",
        doc
          "15.11"
          [ "`write_file(path, contents, env)` creates or truncates the file and writes `contents` as UTF-8, with no trailing newline added.",
            "A missing parent directory is `E-IO-FS`, so create it with `make_dir` first. Registered secrets are written in the clear."
          ]
      ),
      ("file_exists", doc "15.11" ["`file_exists(path, env)` is true when a file or directory exists at `path`. It follows symbolic links."]),
      ( "remove_file",
        doc
          "15.11"
          [ "`remove_file(path, env)` removes a file. A missing path succeeds and does nothing.",
            "A directory is `E-IO-FS`. There is no recursive removal."
          ]
      ),
      ( "make_dir",
        doc
          "15.11"
          [ "`make_dir(path, env)` creates the directory and any missing parents. An existing directory is fine.",
            "An existing non-directory is `E-IO-FS`."
          ]
      ),
      ( "list_dir",
        doc
          "15.11"
          [ "`list_dir(path, env)` returns the names of the entries directly inside the directory, sorted by code point.",
            "A path that isn't a directory is `E-IO-FS`."
          ]
      ),
      ( "glob",
        doc
          "15.11"
          ["`glob(pattern, env)` returns the matching paths, sorted by code point. `*` and `?` match within a component, `[...]` is a class, and `**` spans components. No match gives `[]`."]
      ),
      -- 15.12 diagnostic output
      ( "log",
        doc
          "15.12"
          ["`log(message)` writes one informational line to the execution log on stderr, with registered secrets masked. No builtin writes to stdout."]
      ),
      -- 15.13 nondeterministic
      ("uuid", doc "15.13" ["`uuid()` returns a new random version 4 UUID in lowercase hyphenated form."]),
      ( "random_string",
        doc
          "15.13"
          [ "`random_string(n)` returns `n` characters drawn from `0-9` and `a-z` by a cryptographically secure source.",
            "`n` must be a non-negative integer, or it is `E-RUNTIME-VALUE`. The result isn't masked, so use `mark_secret` for a secret."
          ]
      )
    ]
