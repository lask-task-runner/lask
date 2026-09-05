# Lask

Lask (lambda + task) is a locally verifiable, type-safe task runner. It brings the composability of functional programming to your daily automation and CI/CD pipelines.

Instead of sprawling shell scripts or YAML pipelines, tasks in Lask are plain functions with real types and arguments. This allows `lask check` to catch typos, missing arguments, and type mismatches before execution begins. Whether it runs on your laptop or inside a pinned container, the workflow stays reproducible.

The language, CLI, execution environments, and observability are defined by the specification in [doc/spec.md](doc/spec.md).

<div align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="doc/assets/main-dark.svg">
    <img alt="Lask task definitions: three pinned environments as values, two test suites running concurrently in separate containers, then a build and either a terraform plan or apply" src="doc/assets/main-light.svg" width="611">
  </picture>
</div>

<details>
<summary>Copy the source of this example</summary>

```lask
// Environments are values: pin an image once, reuse it everywhere.
go    = #golang:1.22
node  = #node:20
// A custom image builds from a Dockerfile and is used the same way.
infra = #docker(dockerfile = "Dockerfile", context = ".")

test_api(): String = $[go] go test ./...
test_web(): String = $[node] npm test

// Both suites run concurrently; the build and deploy follow in order.
// >>> lask run release --dry-run true
release(--dry_run = false) = do {
  api = async test_api()
  web = async test_web()
  await api
  await web
  $[go] go build
  if (dry_run) {
    $[infra] terraform plan
  } else {
    $[infra] terraform apply -auto-approve
  }
}
```

</details>

### Install

<details open>
<summary><b>macOS</b> &middot; Homebrew</summary>

```bash
$ brew tap lask-task-runner/tap
$ brew trust lask-task-runner/tap
$ brew install lask
```

</details>

<details>
<summary><b>macOS / Linux</b> &middot; download the binary</summary>

```bash
$ VERSION=$(curl -fsSL https://api.github.com/repos/lask-task-runner/lask/releases/latest | grep -m1 '"tag_name"' | cut -d '"' -f4)
$ TARGET=linux-amd64 # or macos-amd64, macos-arm64
$ curl -fsSL -o lask.tar.gz "https://github.com/lask-task-runner/lask/releases/download/${VERSION}/lask-${VERSION}-${TARGET}.tar.gz"
$ tar -xzf lask.tar.gz
$ sudo mv ./lask /usr/local/bin
```

Uninstall with `sudo rm /usr/local/bin/lask`.

</details>

<details>
<summary><b>Windows</b> &middot; PowerShell</summary>

```powershell
> $version = (Invoke-RestMethod https://api.github.com/repos/lask-task-runner/lask/releases/latest).tag_name
> Invoke-WebRequest -Uri "https://github.com/lask-task-runner/lask/releases/download/$version/lask-$version-windows-amd64.zip" -OutFile lask.zip
> Expand-Archive -Path lask.zip -DestinationPath "$env:LOCALAPPDATA\Programs\lask" -Force
> setx PATH "$env:PATH;$env:LOCALAPPDATA\Programs\lask"
```

Restart your terminal for the updated `PATH` to take effect. Uninstall with
`Remove-Item -Recurse -Force "$env:LOCALAPPDATA\Programs\lask"`.

</details>

<details>
<summary><b>From source</b> &middot; Haskell toolchain</summary>

Needs [GHCup](https://www.haskell.org/ghcup/) or `brew install haskell-stack`.

```bash
$ stack --local-bin-path /usr/local/bin/ install
```

</details>

Verify with `lask --help`. Archives for every platform are on the
[latest release](https://github.com/lask-task-runner/lask/releases/latest); APT and
Chocolatey support is planned.

### Why Lask

- **Locally Verifiable**: The exact same task definitions run locally and in CI. Static analysis (`lask check`) guarantees your arguments and types are correct before execution starts, ending the "push and pray" cycle.
- **First-Class Environments**: Execution environments (`#local`, `#docker(...)`) are treated as normal language values. Pinning the exact execution environment directly in code—alongside your functions and types—ensures strict reproducibility anywhere, without forcing you to migrate to a heavy framework.
- **Composable & Modular**: Unlike Makefiles or shell scripts where passing arguments safely and reusing code is difficult, Lask uses structured function arguments (keyword, variadic, defaults) and standard module imports.

### Features

- Statically checked before execution: syntax, name resolution, and a structural type system (`Number`, `String`, `Bool`, `Array<T>`, `Map<T>`, `Record<...>`, `Function<...>`, `AsyncHandle<T>`, `Environment`).
- Procedural sugar (`do`, `if`/`else`, `for`, `return`, `try`/`catch`/`finally`, `async`/`await`) normalized onto a small functional core.
- Command execution with environment selection: `$ cmd` (stdout), `$2 cmd` (stderr), `$* cmd` (whole result), `$[#alpine:3.20] cmd` (a Docker image), `$[#docker(dockerfile = "...", context = ".")] cmd` (an image built from a Dockerfile, with optional `memory` / `cpus` limits).
- Modules with named/namespace imports (`./`-relative paths); stdin bound as the `stdin` string; JSON I/O.
- External dependencies declared in `lask.json` and pinned by content hash in the committed `lask.lock.json` (`lask deps add` / `lask deps sync` / `lask deps why`); `check`, `run`, and `eval` refuse to proceed against a stale lock, and never resolve anything the lock does not already pin — execution touches no network, only a verified local cache.
- Observability: trace IDs, `call`/`return`/`fail` execution events (`--format json`), stack traces, spec-defined exit codes.

### Comparison

Lask is a task runner, not a build system. Here is how it compares to the tools it most often replaces:

|                                        | Lask | make | just | Task (go-task) |
| -------------------------------------- | :--: | :--: | :--: | :------------: |
| Static checks before execution (`lask check`) | ✅ types, names, arity | — | syntax only | schema only |
| Typed task arguments with defaults      | ✅ `--name: String = "World"` | — | untyped strings | untyped vars |
| Execution environments as values (Docker) | ✅ `$[#golang:1.22]` | — | — | — |
| Concurrency in the language             | ✅ `async` / `await` | `-j` (per-target) | — | `deps` run in parallel |
| Code reuse across projects              | ✅ hash-pinned module imports | `include` | `import` (local) | `includes` |
| File-based incremental rebuilds         | — | ✅ | — | ✅ checksum / timestamp |
| Config format                           | typed language | Makefile | justfile | YAML |
| Install                                 | single binary (Homebrew / Releases) | preinstalled | single binary | single binary |

**When to use something else:**

- If your tasks are primarily *"rebuild only what changed"* over file targets, `make` (or a real build system like Bazel) is the right tool. Lask does not track file freshness.
- If all you need is a flat list of one-line command aliases, `just` is simpler and that simplicity is a feature.

**When Lask pays off:** tasks that take arguments, call each other, run in pinned Docker environments, or run concurrently — the point where Makefiles and YAML pipelines usually turn into untestable shell scripts. `lask check` verifies all of it before anything executes.

### Status

Lask is pre-1.0: features are `experimental` until the first tagged release, and breaking changes are still possible. See [doc/compatibility.md](doc/compatibility.md) for what `stable` will mean once released.

### Supported Editors

Lask provides a VS Code extension for a rich editing experience, including syntax highlighting, type checking, go-to-definition, and autocomplete powered by the built-in language server.

You can install the [Lask extension from the VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=ToruIkeda.vscode-lask) or search for "Lask" within VS Code.

### Usage

```bash
$ lask check                       # static validation
$ lask run <function> [args...]    # execute (result not printed)
$ lask eval <function> [args...]   # execute and print the result as JSON
$ lask envs [--check]              # list/check referenced environments
$ lask env build | list            # materialize / inspect container images
$ lask deps sync                   # fetch + verify external dependencies
$ lask deps add <name> --git <url> --rev <rev>   # or --url <url>
$ lask deps why <name>             # show why a dependency is in the graph
$ lask repl                        # interactive session
$ lask serve                       # language server (LSP)
$ lask version                     # print the lask version
```

Function and keyword-argument names map from kebab-case on the CLI

`lask run show-version --out-dir /tmp` calls `show_version(--out_dir ...)`).

### Example

```bash
$ cd ./example/01-basic
$ lask eval hello --name Lask
"Hello, Lask!"
$ lask run cowsay-hello Lask
2026-09-04T15:43:40.248Z [#rancher/cowsay:1] $ cowsay "Hello, Lask!"
2026-09-04T15:43:40.541Z [#rancher/cowsay:1] 1|  ______________
2026-09-04T15:43:40.541Z [#rancher/cowsay:1] 1| < Hello, Lask! >
2026-09-04T15:43:40.542Z [#rancher/cowsay:1] 1|  --------------
2026-09-04T15:43:40.542Z [#rancher/cowsay:1] 1|         \   ^__^
2026-09-04T15:43:40.542Z [#rancher/cowsay:1] 1|          \  (oo)\_______
2026-09-04T15:43:40.543Z [#rancher/cowsay:1] 1|             (__)\       )\/\
2026-09-04T15:43:40.543Z [#rancher/cowsay:1] 1|                 ||----w |
2026-09-04T15:43:40.544Z [#rancher/cowsay:1] 1|                 ||     ||
2026-09-04T15:43:40.858Z [#rancher/cowsay:1] exit 0
```

`hello` is a pure function and needs nothing installed; `cowsay_hello` calls it and
runs the result in a container, so the second command needs Docker. Every command Lask
runs is logged with its environment, its stream (`1|` stdout, `2|` stderr) and its exit
status.

### Development

```bash
$ lask run test
```
