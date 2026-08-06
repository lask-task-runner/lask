# Lask

Lask (lambda + task) is a locally verifiable, type-safe task runner. It brings the composability of functional programming to your daily automation and CI/CD pipelines.

Instead of sprawling shell scripts or YAML pipelines, tasks in Lask are plain functions with real types and arguments. This allows `lask check` to catch typos, missing arguments, and type mismatches before execution begins. Whether running on your laptop, inside a Docker container, or over SSH, the workflow remains cleanly reproducible.

The language, CLI, execution environments, and observability are defined by the specification in [doc/spec.md](doc/spec.md).

```lask
// Sample function to greet using `echo`.
// example:
// >>> lask run hello --name Lask
hello(--name: String = "World"): String = $ echo "Hello, #{name}!"

// Sample function to run Go tests in the Docker environment.
// example:
// >>> lask run test_backend
test_backend(): String = $[#golang:1.22] go test ./...

// Sample function to run Node.js tests in the Docker environment.
// example:
// >>> lask run test_frontend
test_frontend(): String = $[#node:20] npm test

// Sample function to run both backend and frontend tests concurrently.
// example:
// >>> lask run test_all
test_all(): String = do {
  test1 = async test_backend()
  test2 = async test_frontend()
  await test1
  await test2
  return "successfully ran all tests"
}

// Sample function to build a Go binary with version info.
// example:
// >>> lask run build --version 1.0.0
build(--version: String = "latest"): String = do {
  build1 = async $[#golang:1.22] go build -o my-api --ldflags "-X main.version=#{version}" ./cmd/my-api
  build2 = async $[#node:20] npm run build
  await build1
  $ docker build -t my-api:#{version} ./api
  await build2
  $ docker build -t my-gui:#{version} ./gui
  return "successfully built my-api:#{version} and my-gui:#{version}"
}
```

### Why Lask

- **Locally Verifiable**: The exact same task definitions run locally and in CI. Static analysis (`lask check`) guarantees your arguments and types are correct before execution starts, ending the "push and pray" cycle.
- **First-Class Environments**: Execution environments (`#local`, `#docker(...)`, `#env(...)`) are treated as normal language values. Pinning the exact execution environment directly in code—alongside your functions and types—ensures strict reproducibility anywhere, without forcing you to migrate to a heavy framework.
- **Composable & Modular**: Unlike Makefiles or shell scripts where passing arguments safely and reusing code is difficult, Lask uses structured function arguments (keyword, variadic, defaults) and standard module imports.

### Features

- Statically checked before execution: syntax, name resolution, and a structural type system (`Number`, `String`, `Bool`, `Array<T>`, `Map<T>`, `Record<...>`, `Function<...>`, `AsyncHandle<T>`, `Environment`).
- Procedural sugar (`do`, `if`/`else`, `for`, `return`, `try`/`catch`/`finally`, `async`/`await`) normalized onto a small functional core.
- Command execution with environment selection: `$ cmd` (stdout), `$2 cmd` (stderr), `$* cmd` (whole result), `$[#alpine:3.20] cmd` (Docker), `$[#env("name")] cmd` (named environments from `environments.lask.json`, including SSH remotes).
- Modules with named/namespace imports (`./`-relative paths); stdin bound as the `stdin` string; JSON I/O.
- External dependencies fetched over the internet, pinned by content hash in `dependencies.lask.json` (`lask deps add` / `lask deps sync`); execution never touches the network — modules resolve from a verified local cache.
- Observability: trace IDs, `call`/`return`/`fail` execution events (`--format json`), stack traces, spec-defined exit codes.

### Comparison

Lask is a task runner, not a build system. Here is how it compares to the tools it most often replaces:

|                                        | Lask | make | just | Task (go-task) |
| -------------------------------------- | :--: | :--: | :--: | :------------: |
| Static checks before execution (`lask check`) | ✅ types, names, arity | — | syntax only | schema only |
| Typed task arguments with defaults      | ✅ `--name: String = "World"` | — | untyped strings | untyped vars |
| Execution environments as values (Docker / SSH) | ✅ `$[#golang:1.22]` | — | — | — |
| Concurrency in the language             | ✅ `async` / `await` | `-j` (per-target) | — | `deps` run in parallel |
| Code reuse across projects              | ✅ hash-pinned module imports | `include` | `import` (local) | `includes` |
| File-based incremental rebuilds         | — | ✅ | — | ✅ checksum / timestamp |
| Config format                           | typed language | Makefile | justfile | YAML |
| Install                                 | single binary (Homebrew / Releases) | preinstalled | single binary | single binary |

**When to use something else:**

- If your tasks are primarily *"rebuild only what changed"* over file targets, `make` (or a real build system like Bazel) is the right tool. Lask does not track file freshness.
- If all you need is a flat list of one-line command aliases, `just` is simpler and that simplicity is a feature.

**When Lask pays off:** tasks that take arguments, call each other, run in pinned Docker/SSH environments, or run concurrently — the point where Makefiles and YAML pipelines usually turn into untestable shell scripts. `lask check` verifies all of it before anything executes.

### Status

Lask is pre-1.0: features are `experimental` until the first tagged release, and breaking changes are still possible. See [doc/compatibility.md](doc/compatibility.md) for what `stable` will mean once released.

### Supported Editors

Lask provides a VS Code extension for a rich editing experience, including syntax highlighting, type checking, go-to-definition, and autocomplete powered by the built-in language server.

You can install the [Lask extension from the VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=ToruIkeda.vscode-lask) or search for "Lask" within VS Code.

### Installation

#### Homebrew (macOS)

```bash
$ brew tap lask-task-runner/tap
$ brew trust lask-task-runner/tap
$ brew install lask
```

#### Manual install (download binary)

Download the archive for your platform from the [latest release](https://github.com/lask-task-runner/lask/releases/latest) and put the `lask` binary on your `PATH`.

macOS / Linux:

```bash
$ VERSION=$(curl -fsSL https://api.github.com/repos/lask-task-runner/lask/releases/latest | grep -m1 '"tag_name"' | cut -d '"' -f4)
$ TARGET=linux-amd64 # or macos-amd64, macos-arm64
$ curl -fsSL -o lask.tar.gz "https://github.com/lask-task-runner/lask/releases/download/${VERSION}/lask-${VERSION}-${TARGET}.tar.gz"
$ tar -xzf lask.tar.gz
$ sudo mv ./lask /usr/local/bin
```

Make sure `~/.local/bin` is on your `PATH` (add `export PATH="$HOME/.local/bin:$PATH"` to your shell profile if it isn't), then verify with `lask --help`.

Windows (PowerShell):

```powershell
> $version = (Invoke-RestMethod https://api.github.com/repos/lask-task-runner/lask/releases/latest).tag_name
> Invoke-WebRequest -Uri "https://github.com/lask-task-runner/lask/releases/download/$version/lask-$version-windows-amd64.zip" -OutFile lask.zip
> Expand-Archive -Path lask.zip -DestinationPath "$env:LOCALAPPDATA\Programs\lask" -Force
> setx PATH "$env:PATH;$env:LOCALAPPDATA\Programs\lask"
```

Restart your terminal for the updated `PATH` to take effect, then verify with `lask --help`.

To uninstall, remove the installed binary:

```bash
$ rm ~/.local/bin/lask
```

```powershell
> Remove-Item -Recurse -Force "$env:LOCALAPPDATA\Programs\lask"
```

> **Note**: Package manager support beyond Homebrew (APT, Chocolatey) is planned for a future release.

#### Build from source

To build Lask, you need the Haskell toolchain ([GHCup](https://www.haskell.org/ghcup/) or `brew install haskell-stack`).

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

`lask run show-version --out-dir /tmp` calls `show_version(--out_dir ...)`).

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
