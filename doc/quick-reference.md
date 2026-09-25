# Lask Quick Reference

The whole language and CLI on one page, for someone who wants to write a task
now and read the rules later. Every section links to the chapter of the
[language specification](spec.md) that defines it; when this page is not enough,
follow the link. Installation and the case for Lask are in the
[README](../README.md).

## A module, end to end

```lask
// main.lask — the default target of every subcommand.

// Environments are values: pin an image once, reuse it everywhere.
go   = #golang:1.25
node = #node:20.20.2-alpine3.23

// Declare which image provides each program. A command naming no
// environment is a static error, never a silent fall back to the host.
command { "go" } on go
command { "npm", "npx" } on node
command { "git" } on #local

// Run both suites concurrently, then build.
//
// @param dry_run  Report what would happen instead of building.
// @example lask run release --dry-run true
release(--dry_run = false): String = do {
  api = async $ go test ./...
  web = async $ npm test
  await api
  await web
  if (dry_run) { return "would build" }
  $ go build -o dist/app
  "released"
}
```

```bash
$ lask check            # resolve every name, argument and type — nothing runs
$ lask run release      # execute
$ lask run release --help
```

## CLI

→ [spec ch. 11](spec.md#11-cli-specification)

| Command | What it does |
| --- | --- |
| `lask check` | Static validation only. Nothing is evaluated, no image is pulled. |
| `lask run <fn> [args...]` | Execute a task. Writes **nothing** to stdout. |
| `lask eval <fn> [args...]` | Same, and writes the return value to stdout (JSON by default). |
| `lask cmd <prog> [args...]` | Run a declared command in its declared image, stdio passed through. |
| `lask repl` | Evaluate expressions interactively. |
| `lask envs [fn] [--check]` | List the environments a module uses; `--check` tests access. |
| `lask env build \| list` | Materialize / inspect container images. |
| `lask deps sync \| add \| why \| diff` | Fetch, verify and report on external dependencies. |
| `lask serve` | Language server (LSP). |
| `lask completion <shell>` | Emit a completion script (bash, zsh, fish). |
| `lask version` | Print the version. |

Options come **before** the function name — everything after it belongs to the
function. `--module <path>` (default `main.lask`), `--format text\|json`,
`--stdout-encode text\|json\|pretty-json` (default `json`),
`--arg-decode text\|json\|auto` (default `auto`), `--trace-id`, `--no-color`.
→ [11.2](spec.md#112-function-invocation)

A task's signature is its command line. `-` maps to `_`, so
`release(--dry_run = false)` is reachable as `lask run release --dry-run true`,
and `show_version` as `lask run show-version`.

Exit codes: `0` success · `1` syntax or static error · `4` CLI usage error ·
otherwise the `code` of the uncaught `Error` — a failed command passes its own
exit code through. → [11.3](spec.md#113-inputoutput-contract),
[14.8](spec.md#148-correspondence-to-cli-exit-codes)

## Declarations

→ [spec ch. 5](spec.md#5-declarations-and-modules)

```lask
name = "app"                        // value; evaluated lazily, once, on first use
tag: String = "latest"              // with a type annotation
greet(name: String, --prefix = "hi"): String = concat(prefix, name)
type Config = Record<name: String, port: Number>

internal helper() = 1               // not visible outside this module
export public_task() = 2            // the default; the marker just states it
```

One file is one module. Every top-level declaration is public unless marked
`internal`. A declaration ends at a newline (or `;`); it continues across lines
after `=`, `,`, `:`, `->`, `!`, an unclosed bracket, or a binary operator at
either end of the break.

```lask
import { add, Config } from "./lib/math.lask"   // named, may rename with `as`
import * as m from "./lib/util.lask"            // namespace: m.symbol, m.TypeName
import { send } from "notify"                   // external dependency, by name
export { rollout } from "./lib/deploy.lask"     // re-export, parameter list intact
```

External dependencies are declared in `lask.json` (`git` + `rev`, or `url`) and
pinned by content hash in the committed `lask.lock.json`. `check`, `run`, `eval`
and `envs` never touch the network; `lask deps sync` is what fetches and
verifies. The import graph must be acyclic, and an import reaches only a
dependency's root `main.lask`. → [11.5](spec.md#115-dependency-management-deps)

## Types

→ [spec ch. 4](spec.md#4-type-system)

| Kind | Types |
| --- | --- |
| Base | `Any` `Number` `String` `Bool` `Null` `Void` `Environment` |
| Composite | `Array<T>` · `Map<T>` (string keys) · `Record<a: T, b?: U>` · `AsyncHandle<T>` · `Function<T1, T2, R>` (last is the return type) |
| Union | `String \| Null` — the way a value that may be absent is typed |
| Built-in aliases | `CommandResult = Record<code: Number, stdout: String, stderr: String>` · `Error = Record<code: Number, message: String>` |

```lask
type Strings = Array<String>          // alias; may not reference itself
type Opt<A> = A | Null                // alias with type parameters
type Config = Record<
  name: String,                       // key required, value a String
  port: Number | Null,                // key required, value may be null
  tags?: Array<String>                // key may be absent; reads as T | Null
>
xs: Array<String | Null> = ["a", null]

first_or<T>(xs: Array<T>, fallback: T): T =      // a type parameter, never
  if (is_empty(xs)) { fallback } else { xs[0] }  // written at a call site
n = first_or([1, 2], 0)                          // instantiated at Number here
```

Annotations are optional and inference fills the rest: a parameter with neither
an annotation nor a default is `Any`, and a mixed array literal is `Array<Any>`.
Neither a union nor an optional field is **ever** inferred — each is only ever
something someone wrote.

`?` qualifies the key and the field's type qualifies the value: `a: String | Null`
must be present and may be null, `a?: String` may be absent. A type parameter is
opaque inside its own declaration — it conforms only to itself and `Any` — so a
body may pass such a value around but not compare, order or interpolate one.
→ [4.2](spec.md#42-type-syntax)

Conformance is small on purpose. Everything conforms to `Any`; a member conforms
to its union; nothing else does. There is no variance: `Array<Number>` does not
conform to `Array<Any>`, and `Record` conforms only when the required set, the
optional set and every field type are identical. Getting *out* of `Any` or a
union takes a runtime check — `cast(v)` (fails on anything else) or `case` type
dispatch (tests instead of failing).
→ [4.4](spec.md#44-type-semantics)

## Expressions and operators

→ [spec 6.2](spec.md#62-operators)

`!` › `*` `/` › `+` `-` › `==` `!=` `<` `<=` `>` `>=` › `&&` `||` ›
`|>` `<|` `>>` `<<` — all left-associative.

- `x |> f` is `f(x)`; `f <| x` is `f(x)`.
- `f >> g` is `\(x) -> g(f(x))`; `f << g` is `\(x) -> f(g(x))`.
- `&&` and `||` short-circuit.
- Arithmetic is `Number` only. **`+` does not concatenate strings** — use
  `concat`, or interpolation.
- Number literals carry no sign and there is no unary minus: write `0 - 1`.
- `==` needs comparable types; `Any`, `Function` and `AsyncHandle` are not.

```lask
"#{name}:#{tag}"        // interpolation, interpreted strings only
'no #{interpolation}'   // raw string, verbatim, may span lines
user.name               // record field; the field set is fixed, so it cannot fail
xs[0]                   // array index, zero-based
m["APP_ENV"]            // map key, or a field whose name is not an identifier
\(x: Number) -> x + 1   // lambda
```

Records have a statically fixed field set and an absent optional key reads as
null, so `.f` never fails at runtime; maps are for keys that vary, and `m[k]`
fails on a missing key (`get_or` does not).
→ [6.8](spec.md#68-accessor-expressions)

## Functions

→ [spec 6.1](spec.md#61-function-parameters-lambda-expressions-and-higher-order-functions)

```lask
f(a, b, ...rest: Array<Number>, --opt: String = "x") = [a, b, rest, opt]
```

Positional parameters are required and take no default. Keyword parameters
(`--name`) **must** have a default and are bound only by name — `f(1, opt = "y")`
in the language, `--opt y` on the CLI. At most one variadic parameter, right
after the positional ones; it collects what is left over, as `Array<T>`.

Declaration order is enforced: positional, variadic, keyword. `f(x) = body` is
sugar for `f = \(x) -> body`. Calling through a *function value* loses the
parameter list: positional arguments only, every keyword parameter defaulted.

A declaration may bind type parameters before its parameter list
([Types](#types)); a default that mentions one must hold for every
instantiation, so `--xs: Array<T> = []` is legal and `--y: T = 1` is not.

## Control flow

→ [spec 6.4](spec.md#64-control-structures), [6.5](spec.md#65-procedural-notation)

```lask
do {                          // sequence; the value is that of the last statement
  a = compute()               // a!!: String = ... marks a secret
  if (a == "") { return "skip: empty" }   // early return; guard must end in return
  log(a)
  "done"
}

if (c) { x } else { y }       // an expression — `else` is mandatory

case (shell) {                // value dispatch, scrutinee evaluated once
  "bash", "zsh" -> "posix"
  "fish"        -> "fish"
  else          -> fail(error(4, "unknown shell"))
}

case (v) {                    // type dispatch over a union or Any, with narrowing
  Null   -> "(none)"
  Number -> to_string(v)      // v: Number here
  else   -> v                 // v: String here, the only member left
}

case {                        // condition form, in place of a chained `else if`
  n >= 500 -> "error"
  n >= 400 -> "warn"
  else     -> "info"
}

for (x : xs) { concat("item:", x) }   // an Array<R>; Void body becomes for_each
```

Exactly one `else` arm, last, always — exhaustiveness is never inferred. An arm
body is an *expression*: use `do { ... }` for several statements, and note that
`{` right after `->` is an object literal. `for` iterates arrays only: count with
`range(0, n)`, index with `enumerate(xs)`, traverse a map with `entries(m)`.

Narrowing is a property of `case` alone, and only when the scrutinee is written
as a plain local name. `if (x == null) { ... }` narrows nothing.

## Commands and environments

→ [spec 6.6](spec.md#66-command-execution-expressions),
[6.7](spec.md#67-environment-expressions), [ch. 10](spec.md#10-execution-environments)

```lask
$  go build          // String: stdout; fails (with the command's exit code) if code != 0
$1 go build          // same as $
$2 go build          // String: stderr, same failure rule
$* go build          // CommandResult {code, stdout, stderr} — never fails on exit code
$[#alpine:3.20] uname -a        // explicit environment
$[env] sh -lc 'echo hi'         // any expression of type Environment
```

A command string runs **to the end of the line** — nothing of the enclosing
expression may follow it on that line. End the line with `\` to continue it, and
put `|> trim` on the next line to keep piping. `#{e}` interpolates a
stringifiable value (`String | Null` is not one: resolve it first).

```lask
#local                          // the host
#alpine:3.20                    // sugar for #docker("alpine:3.20")
#ubuntu@sha256:9cee...          // a digest works too
#docker("alpine:3.20", memory = "4g")
#docker(dockerfile = "infra/Dockerfile", context = ".")   // built from a recipe
#docker("node:20-alpine", env = {"CI": "1"}, network = "none", read_only = true)
```

A registry reference must carry a tag or a digest — a bare name is a static
error. `dockerfile` and `context` must be literals inside the module's own tree,
which is what makes every image enumerable and pinnable.

The container is configured by further keyword arguments: `memory`,
`memory_swap`, `memory_reservation`, `cpus`, `cpu_shares`, `cpuset_cpus`,
`cpuset_mems`, `pids_limit`, `shm_size`, `blkio_weight`, `ulimits`; `workdir`,
`user`, `env`, `platform`, `hostname`, `init`; `read_only`, `tmpfs`,
`cap_drop`; `network`, `dns`, `dns_search`, `add_hosts`, `publish`; `volumes`;
and `build_args` on a recipe, which is a literal like `dockerfile`. Environment
variable names are not `lower_id`, so quote them: `env = {"CI": "1"}`.
→ [10.2](spec.md#102-target-environment-profiles-and-environment-constructor-signatures)

**Dispatch.** A `$` with no `[env]` gets its environment from the command words
in the string, matched against the command words the module declares or imports.
→ [10.9](spec.md#109-command-dispatch)

```lask
command { "go" } on #golang:1.25
command { "node", "npm", "npx" } on node
command { "aws" } on tools.aws(profile = "dev")     // any Environment expression
internal command { "ls" } on #local                 // not importable elsewhere

import command { "python", "pip" } from "tools"     // words another module exports
export command { "helm" } from "./lib/k8s.lask"     // import, and export again
```

Matching is lexical and exact: `cd web && npm ci` selects the Node image,
`FOO=1 npm ci` still does, and `/usr/bin/npm` does not (never a basename).
Selection is unanimity — `ls dist && npm publish` is `E-TYPE-COMMAND-CONFLICT`.
Two words agree when their declarations name the same literal environment or the
same binding; two separate calls do not, so declare such words together.
Nothing matched is `E-TYPE-COMMAND-NOENV`, not a fall back to the host. A command
word arrives only by `import command`, never by a named or namespace import.

A declaration's environment is evaluated when a command runs, and may not have
effects — no command, file, `stdin`, `log` or random value, directly or through
what it calls (`E-TYPE-COMMAND-EFFECT`); `get_env` is fine.
→ [5](spec.md#5-declarations-and-modules)

Images are materialized only by `lask deps sync` and `lask env build`; `run` and
`eval` never pull or build, and a missing image is `E-IO-IMAGE-MISSING` naming
the command that would fix it. → [10.3](spec.md#103-container-image-resolution-and-materialization)

## Concurrency

→ [spec 6.3](spec.md#63-asynchronous-invocation-and-awaiting)

```lask
a = async test_api()      // starts now, returns AsyncHandle<T>
b = async test_web()
r = { api: await a, web: await b }    // await joins; a failure inside is raised here
```

`async e` is `spawn(\() -> e)`. Awaiting the same handle twice gives the same
result. `all(handles)` waits for every one, `race(handles)` for the first.

## Errors

→ [spec 6.9](spec.md#69-error-handling-expressions), [ch. 14](spec.md#14-error-system)

```lask
command { "make", "rm" } on #local

build(): String = try {
  $ make build
} catch (e) {                  // e: Error = {code, message}
  if (e.code == 2) {
    $ make clean
    $ make build
  } else {
    fail(e)
  }
} finally {
  $ rm -rf ./tmp               // runs exactly once, on both paths
}
```

`fail(error(4, "unknown shell"))` raises. Body and `catch` must have the same
type; `finally`'s value is discarded. Static errors are found before evaluation
and are never catchable. A failure inside `catch` or `finally` propagates
outward.

## Input, output, secrets

→ [spec ch. 9](spec.md#9-standard-io-and-data-flow), [6.10](spec.md#610-secret-bindings)

`stdin` is a reserved identifier bound once per run, always `String`. Structure
it in the language: `lines(stdin)`, `from_json(stdin)`, then `cast` or `case`.

`run` writes nothing to stdout, `eval` writes the return value there, and logs
and diagnostics always go to stderr. That is what makes `lask eval a | lask run b`
work. → [11.3](spec.md#113-inputoutput-contract)

```lask
deploy(--key!!: String = get_env("AWS_SECRET_ACCESS_KEY")) = do {
  $[#amazon/aws-cli:2.31.9] aws configure set aws_secret_access_key #{key}
}
```

`!!` marks a binding secret: the value is masked in the command execution log
wherever it later appears. It is allowed on `String` only, carries no meaning in
the type system, and never masks a `CommandResult` or `eval`'s own output.
→ [12.8](spec.md#128-protection-of-sensitive-information-and-retention-policy)

## Documentation comments

→ [spec 3.1](spec.md#31-comments), [11.6](spec.md#116-help-display---help)

The unbroken run of comments directly above a declaration is its documentation,
and it is the single source for both `--help` and the editor's hover.

```lask
// Build and publish the image.        <- summary (first paragraph)
//
// Longer description here.
//
// @param tag   The tag to publish.
// @return      The digest that was pushed.
// @example     lask run publish --tag v1.2.0
// @complete tag @keys known_tags
publish(--tag: String = "latest"): String =
  $[#docker:27.3.1-cli] docker push app:#{tag}
```

`@hidden` keeps a declaration out of listings while leaving help by name
working. `@complete <param>` steers shell completion, as `@keys <map binding>`,
`@file [*.ext]`, `@dir`, or a literal list of choices. Unknown tags are carried
through rather than rejected, and `@param` matches after the `-`/`_` mapping, so
`out_dir` and `out-dir` name the same parameter.

## Built-in library

→ [spec ch. 15](spec.md#15-built-in-library). No overloading: one name, one
signature.

| Group | Functions |
| --- | --- |
| Numbers | `add` `sub` `mul` `div` `mod` `abs` `floor` `ceil` `round` `min` `max` `sum` `pow` `sqrt` `clamp` |
| Strings | `length` `concat` `trim` `to_lower` `to_upper` `split` `join` `replace` `contains` `starts_with` `ends_with` `index_of` `substring` `pad_start` `pad_end` `repeat` `lines` `to_string` `to_number` `regex_test` `regex_match` `regex_replace` |
| Arrays | `map` `filter` `reduce` `for_each` `append` `concat_array` `size` `is_empty` `first` `last` `slice` `take` `drop` `reverse` `sort` `sort_by` `contains_array` `index_of_array` `find` `find_index` `every` `any` `flatten` `flat_map` `zip` `unique` `range` `enumerate` |
| Maps | `get` `get_or` `has_key` `keys` `values` `set` `remove` `merge` `entries` `from_entries` `map_values` |
| Commands | `run` `shell_quote` |
| Async | `spawn` `await` `all` `race` |
| Errors | `recover` `fail` `error` |
| Data | `to_json` `from_json` `encode` `decode` `cast` `base64_encode` `base64_decode` `sha256` `md5` |
| Environment | `get_env` `find_env` `has_env` `get_env_or` `mark_secret` |
| Paths | `path_join` `dirname` `basename` `extname` `normalize_path` `is_absolute_path` |
| Filesystem | `read_file` `write_file` `file_exists` `remove_file` `make_dir` `list_dir` `glob` — each takes the `Environment` as its last argument |
| Other | `log` `uuid` `random_string` |

Absence is reported two ways, deliberately: a function that returns a *position*
reports it as `-1` (`index_of`, `find_index`), and one that returns a *value*
reports it as `Null` (`find`, `find_env`).

## Things that catch people out

- `+` is arithmetic only. Strings join with `concat` or `#{...}`.
- There is no unary minus. `0 - 1`.
- `if` without `else` is not an expression; it exists only as a `return` guard.
- Every `case` needs an `else`, and it must be last.
- A command with no environment is an error, never the host. `#local` is
  something you write.
- A command string swallows the rest of its line.
- `lask run` prints nothing. You wanted `lask eval`.
- Conformance is invariant: `Array<Number>` is not an `Array<Any>`.
- Keyword parameters must have defaults; positional parameters must not.
- `for` takes an array, not a number and not a map.
- Interpolating a `String | Null` is a type error — resolve it with `case` first.
- `a?: T` and `a: T | Null` are different questions: the first is about the key,
  the second about the value.
- A type parameter is opaque inside its own body: take the operation you need
  on a `T` as an argument.

## Where to look next

| Question | Spec |
| --- | --- |
| What exactly is the grammar? | [ch. 3](spec.md#3-lexical-specification), [ch. 6](spec.md#6-expressions) |
| When does this type check? | [ch. 4](spec.md#4-type-system), [ch. 7](spec.md#7-static-semantics) |
| What order does this evaluate in? | [ch. 8](spec.md#8-dynamic-semantics) |
| How are images resolved and pinned? | [ch. 10](spec.md#10-execution-environments) |
| What does the CLI guarantee? | [ch. 11](spec.md#11-cli-specification) |
| What appears in the log, and what is masked? | [ch. 12](spec.md#12-observability) |
| What is this error code? | [ch. 14](spec.md#14-error-system) |
| Show me more complete programs. | [ch. 16](spec.md#16-examples), [example/](../example) |
