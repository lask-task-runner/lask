# Lask

Lask (lambda + task) is a locally verifiable, type-safe task runner. It brings the composability
of functional programming to your daily automation and CI/CD pipelines.

Instead of sprawling shell scripts or YAML pipelines, tasks in Lask are plain functions with real
types and arguments. This allows `lask check` to catch typos, missing arguments, and type mismatches
before execution begins. Whether running on your laptop, inside a Docker container, or over SSH,
the workflow remains cleanly reproducible.

The language, CLI, execution environments, and observability are defined by the specification in
[doc/spec.md](doc/spec.md).

```lask
// main.lask
greet(name: String, --prefix: String = "hello"): String =
  concat(prefix, concat(", ", name))

publish(tag: String): String = do {
  if (tag == "") { return "skip: no tag" }
  r = $* ./release.sh #{tag}
  if (r.code != 0) { return r.stderr }
  "released"
}

build_in_docker() = $[#golang:1.22] go build ./...
```

```bash
$ lask eval greet alice --prefix hi
"hi, alice"
```

### Why Lask

- **Locally Verifiable**: The exact same task definitions run locally and in CI. Static analysis (`lask check`) guarantees your arguments and types are correct before execution starts, ending the "push and pray" cycle.
- **First-Class Environments**: Execution environments (`#local`, `#docker(...)`, `#env(...)`) are treated as normal language values. Pinning the exact execution environment directly in code—alongside your functions and types—ensures strict reproducibility anywhere, without forcing you to migrate to a heavy framework.
- **Composable & Modular**: Unlike Makefiles or shell scripts where passing arguments safely and reusing code is difficult, Lask uses structured function arguments (keyword, variadic, defaults) and standard module imports.

### Features

- Statically checked before execution: syntax, name resolution, and a structural type system
  (`Number`, `String`, `Bool`, `Array<T>`, `Map<T>`, `Record<...>`, `Function<...>`,
  `AsyncHandle<T>`, `Environment`).
- Procedural sugar (`do`, `if`/`else`, `for`, `return`, `try`/`catch`/`finally`,
  `async`/`await`) normalized onto a small functional core.
- Command execution with environment selection: `$ cmd` (stdout), `$2 cmd` (stderr),
  `$* cmd` (whole result), `$[#alpine:3.20] cmd` (Docker),
  `$[#env("name")] cmd` (named environments from `environments.lask.json`, including SSH remotes).
- Modules with named/namespace imports (`./`-relative paths); stdin bound as the `stdin` string; JSON I/O.
- External dependencies fetched over the internet, pinned by content hash in
  `dependencies.lask.json` (`lask deps add` / `lask deps sync`); execution never touches
  the network — modules resolve from a verified local cache.
- Observability: trace IDs, `call`/`return`/`fail` execution events (`--format json`),
  stack traces, spec-defined exit codes.

### Status

Lask is pre-1.0: features are `experimental` until the first tagged release, and breaking
changes are still possible. See [doc/compatibility.md](doc/compatibility.md) for what
`stable` will mean once released.

### Installation

#### Homebrew (macOS)

```bash
$ brew tap lask-task-runner/tap
$ brew trust lask-task-runner/tap
$ brew install lask
```

#### APT (Debian/Ubuntu)

```bash
$ curl -fsSL https://lask-task-runner.github.io/lask/lask-archive-keyring.gpg | \
  sudo tee /usr/share/keyrings/lask-archive-keyring.gpg > /dev/null
$ echo "deb [signed-by=/usr/share/keyrings/lask-archive-keyring.gpg] https://lask-task-runner.github.io/lask stable main" | \
  sudo tee /etc/apt/sources.list.d/lask.list > /dev/null
$ sudo apt update
$ sudo apt install -y lask
```

#### Build from source

To build Lask, you need the Haskell toolchain ([GHCup](https://www.haskell.org/ghcup/)
or `brew install haskell-stack`).

```bash
$ stack --local-bin-path /usr/local/bin/ install
```

### Usage

```bash
$ lask check                       # static validation
$ lask run <function> [args...]    # execute (result not printed)
$ lask eval <function> [args...]   # execute and print the result as JSON
$ lask infer [--symbol <name>]     # show inferred types
$ lask envs [--check]              # list/check referenced environments
$ lask deps sync                   # fetch + verify external dependencies
$ lask deps add <name> --git <url> --rev <rev>   # or --url <url>
$ lask repl                        # interactive session
$ lask serve                       # language server (LSP)
```

Function and keyword-argument names map from kebab-case on the CLI
(`lask run show-version --out-dir /tmp` calls `show_version(--out_dir ...)`).

### Example

```bash
$ cd ./example/01-basic
$ lask run hello
$ lask eval add 1 2
{"result":3}
```

### Development

```bash
$ stack test
```
