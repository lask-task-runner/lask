# The language, one topic at a time

Each directory here is a whole Lask project — a `main.lask` you can
check, run and edit in place. The files are written to be read: the
commentary is in the code, next to the thing it is about, and the
commands below are the ones worth running while you read.

The numbers are a reading order, not a dependency order. Starting from
nothing, they group like this:

1. [01](#01-values-and-types) · [02](#02-functions) · [03](#03-control-flow) — the language itself, all pure.
2. [04](#04-commands) · [05](#05-environments) — what makes it a task runner.
3. [06](#06-concurrency) · [07](#07-errors) · [08](#08-modules) · [09](#09-dependencies) — what makes it hold up.
4. [10](#10-standard-library) · [11](#11-data) · [12](#12-io-and-secrets) · [13](#13-files-and-paths) — the library, as it is actually used.
5. [14](#14-docs-and-cli) — how the same file becomes a command line and a help page.

Five of the topics run nothing but the language and need no Docker
(**pure** below). The rest run real commands in real containers:

```bash
cd example/02-language/<topic>
lask check          # always first: nothing runs, everything is resolved
lask envs           # which images this module can reach for
lask env build      # materialize them — run and eval never pull
```

`lask run` prints nothing by design; `lask eval` prints the return
value. Below, `eval` is used wherever there is something to see.

---

## 01-values-and-types

**Pure.** Bindings and annotations, type aliases, records against maps,
unions and optional fields, `case` type dispatch, `cast`, and a generic
function.

```bash
lask eval config-summary
lask eval describe 42 ; lask eval describe true ; lask eval describe hi
```

The thing to take away: `a?: T` and `a: T | Null` answer different
questions, and neither is ever inferred — each one is something you
wrote.

## 02-functions

**Pure.** Positional, variadic and keyword parameters, lambdas,
closures, higher-order functions, `|>` `<|` `>>` `<<`, and type
parameters.

```bash
lask eval greet alice --prefix hi
lask eval tag-all v1 web api worker
lask eval compose-demo 3
```

The thing to take away: a keyword parameter must have a default and is
bound only by name — and calling through a *function value* loses the
parameter list entirely.

## 03-control-flow

**Pure.** `do` blocks, `if` as an expression, the guard `return`, all
three forms of `case`, and `for`.

```bash
lask eval plan-release --dry-run true
lask eval classify 503
lask eval checklist ; lask eval steps
```

The thing to take away: every `case` needs an `else`, last, always —
exhaustiveness is never inferred — and `case` is the only thing that
narrows a union.

## 04-commands

**Docker.** `$`, `$1`, `$2` and `$*`, explicit `$[env]`, interpolation,
`shell_quote`, `run_command`, and the `command ... on ...` declarations
that dispatch a bare `$`.

```bash
lask env build
lask eval probe --path /etc/os-release
lask eval inspect            # a command that exits 3, without raising
lask eval report
```

The thing to take away: a command string swallows the rest of its line.
`$ uname -s |> trim` hands `|> trim` to the shell and returns the empty
string; the pipe belongs on the next line.

## 05-environments

**Docker.** `#local`, a tag-pinned image, a digest-pinned one, resource
limits, an image built from the local `Dockerfile`, and environments
held in a `Map<Environment>` and chosen at run time.

```bash
lask envs --check            # is each one actually reachable?
lask env build
lask eval tool-versions
lask eval uname-in --env built
```

The thing to take away: an environment is a value. A command that names
none is a static error, never a quiet fall back to the host.

## 06-concurrency

**Docker.** `async` / `await`, `spawn`, `all`, `race`, and where a
background failure surfaces.

```bash
lask env build
time lask run sequential     # two one-second steps, one after the other
time lask run concurrent     # the same two, overlapped
lask eval gather ; lask eval fastest ; lask eval handled
```

The thing to take away: `async` starts the work, `await` joins it, and a
failure inside a handle is raised where it is awaited.

## 07-errors

**Docker.** `try` / `catch` / `finally`, `fail` and `error`, `recover`,
`$*` for an expected non-zero exit, and how a code becomes the exit code
of the process.

```bash
lask env build
lask eval build-with-retry
lask eval classify --path /nope ; echo "exit $?"
lask run always-fails ; echo "exit $?"
```

The thing to take away: static errors are not catchable. `lask check`
finds them before anything is evaluated at all.

## 08-modules

**Docker** for the last task. Named imports with `as`, namespace
imports, `internal`, re-export, and command declarations that stay
inside the module that wrote them.

```bash
lask eval page-title "  Release Notes  "
lask eval known-targets
lask env build && lask eval ship --version 1.4.0
```

The thing to take away: importing a module brings none of its command
words with it — dispatch is resolved where the command is written.

## 09-dependencies

**Docker, and one network step.** The same imports as 08, reaching a
module in another repository: `lask.json` says what a dependency is, the
committed `lask.lock.json` says which bytes it is, and `lask deps` is
the only thing that fetches.

```bash
lask deps sync               # the only step that touches the network
lask deps why terraform
lask deps diff terraform
lask env build && lask eval status
```

The thing to take away: `check`, `run` and `eval` never reach the
network, and an import reaches a dependency's root `main.lask` and
nothing deeper.

## 10-standard-library

**Pure.** Numbers, strings, POSIX regular expressions, arrays, maps, and
the two ways the library reports absence.

```bash
lask eval slug "  Release Notes, 2026  "
lask eval version-parts v1.4.0
lask eval pipeline ; lask eval map-work
lask eval lookup --needle worker
```

The thing to take away: there is no overloading. One name, one
signature — which is why `size` is for arrays and a map's size is
`size(keys(m))`.

## 11-data

**Pure.** `from_json` and `to_json`, `encode` / `decode` for YAML, TOML,
CSV and dotenv, `cast`, `sha256`, `md5` and base64.

```bash
lask eval parse-config
lask eval render --format yaml
lask eval csv-ports ; lask eval dotenv-keys
lask eval checksum lask
```

The thing to take away: every format decodes to the same handful of
value kinds, so `cast` is the same step whichever syntax the data
arrived in.

## 12-io-and-secrets

**Docker** for the two secret tasks. `stdin`, the stdout contract,
`get_env` / `find_env` / `has_env` / `get_env_or`, `!!` secret bindings,
`mark_secret`, and `log`.

```bash
echo '{"name":"api","port":8080}' | lask eval service-line
printf 'api\nweb-frontend\n' | lask eval longest-name
APP_ENV=prod lask eval where-am-i
lask env build && lask run show-token     # watch the log mask it
```

The thing to take away: `run` writes nothing to stdout and `eval` writes
the return value there, which is what makes `lask eval a | lask run b`
a pipeline.

## 13-files-and-paths

**Docker** for two tasks; the rest is `#local`. `read_file`,
`write_file`, `make_dir`, `list_dir`, `glob`, `file_exists`,
`remove_file`, and the path helpers.

```bash
lask eval describe-path tmp/report.json
lask run write-report && lask eval read-report
lask eval inventory
lask env build && lask eval same-path-two-worlds
lask run clean
```

The thing to take away: every filesystem call names the environment it
acts in, as its last argument. There is no default and no host by
accident.

## 14-docs-and-cli

**Pure.** Doc comments end to end — summary, description, `@param`,
`@return`, `@example`, `@complete` in all four forms, `@hidden` — and
what the CLI does with a signature.

```bash
lask run --help              # the listing, from the first line of each comment
lask run publish --help      # one task, from the same comment
lask eval publish --tag stable
lask eval --stdout-encode pretty-json matrix
```

The thing to take away: `-` maps to `_`, so `publish(--dry_run = false)`
is `--dry-run` on the command line, and the comment above a task is the
only place its help is written.
