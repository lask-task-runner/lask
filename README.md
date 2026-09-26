# Lask

[![test](https://github.com/lask-task-runner/lask/actions/workflows/test.yml/badge.svg)](https://github.com/lask-task-runner/lask/actions/workflows/test.yml)
[![release](https://img.shields.io/github/v/release/lask-task-runner/lask?sort=semver)](https://github.com/lask-task-runner/lask/releases/latest)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

<div align="center">
  <img alt="Lask in 15 seconds: lambda + task becomes Lask, where it fits between make or Taskfile and Dagger, then quick cuts of the example/01-projects/02-webapp-on-aws project showing its seven strengths" src="doc/assets/lask-pv-short.gif" width="960">
  <br>
  <a href="doc/assets/lask-pv.mp4">▶ Watch the full 90-second tour (MP4)</a>
</div>

Lask (lambda + task) is a task runner with a small language behind it, giving automation what shell scripts and CI YAML never had: portability, reproducibility, and verification before anything runs.

Lask is for anyone who finds Makefiles and Taskfiles not quite enough, and Dagger too much. The first are screwdrivers from the kitchen drawer; the second, a factory floor of your own to operate — its own engine, its own SDK, a general-purpose language and its entire ecosystem. Lask is the garage in between: install Docker and Lask, and you have everything you need.

Lask aims to stay simple and light to use, while bringing along the parts of the heavyweight platforms that most automation needs: pinned environments, checks before anything runs, concurrency and reuse.

The recording above and the excerpt below come from [example/01-projects/02-webapp-on-aws](example/01-projects/02-webapp-on-aws), which builds and deploys a full AWS stack — a Python Lambda API, a React front end, RDS Postgres, Cognito, CloudFront and S3, with Playwright end-to-end tests — from a machine with none of those tools installed. For the language itself, [example/02-language](example/02-language) is a tour of the whole language, one runnable module per topic.

<div align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="doc/assets/example-dark.svg">
    <img alt="An excerpt of the example's main.lask: Python and Node commands declared on pinned images from an imported tools module, a one-line unit-test task, and a typed end-to-end test task that runs Playwright in its own image" src="doc/assets/example-light.svg" width="620">
  </picture>
</div>

<p align="center"><a href="example/01-projects/02-webapp-on-aws/main.lask">See the full main.lask →</a></p>

## Why Lask

Lask makes automation *approachable*, *verifiable*, *portable*, *programmable*, *reusable*, *runnable* and *discoverable*.

**Approachable**. A directory with one `.lask` file in it is already a project: no scaffolding, no config file, nothing to install but Lask and Docker. The surface is small, and most of it is borrowed from languages you already write — C-family braces and calls, `try` / `catch`, `async` / `await`, TypeScript's type notation — so `Array<String>`, `String | Null` and `--name: String = "World"` need no explanation. Ten minutes with the [Quick Reference](doc/quick-reference.md) covers the whole language and CLI — one page that a coding model can hold in context too, instead of an SDK's worth of API surface.

**Verifiable**. `lask check` resolves every name, argument and type before a single command runs — over the very definitions CI will execute, with no second copy in YAML to drift out of sync. The same errors appear in your editor as you type, so a typo costs seconds instead of a red CI log.

**Portable**. An execution environment is a value: pin an image once with `command { "go" } on #golang:1.22`, and every command that names it runs there — reproducibly, on your laptop and in CI alike. A command that names no environment is a static error, never a silent fall back to the host, so the only things to install are Lask and Docker.

**Programmable**. A task is an ordinary function — typed keyword arguments with defaults, a return value, callable on its own. Control flow, error handling and concurrency belong to the language rather than to shell convention. That language is a DSL and not a general-purpose one, so the same task takes fewer lines than an SDK in Go or TypeScript would, with no project to build around it.

**Reusable**. Inside a project, tasks call each other like the functions they are; across projects, shared tasks live in their own repository and are imported rather than copied — and so do the environments programs run in, with `import command { "go" } from "tools"`. Imports are pinned by content hash in a committed lock file, so every machine resolves the same code and a run reaches no network.

**Runnable**. The CLI is small, and every piece of a project runs on its own: a task with `lask run`, an expression in the REPL, or a one-off command inside its own container with `lask cmd go test ./...`. Nothing has to be pushed, and nothing has to be run through a shell to try it. A task's signature is its command line, too: keyword arguments become flags, so `release(--dry_run = false)` is `lask run release --dry-run true` with nothing to wire up.

**Discoverable**. A documentation comment above a task — `@param`, `@return`, `@example` — is the single source for both `--help` and the editor's hover, so prose never drifts from the code it describes. Types are already in the source, so `--help` needs no hand-written usage string: it reports the signature, the inferred return type, and every image the task will need before you run it.

## Comparison

Lask is a task runner, not a build system — and not a CI platform. It does not replace GitHub Actions, GitLab CI, or Jenkins; it replaces what your jobs run, so one definition executes on your laptop and inside whatever runner you already have. Your provider's YAML keeps the part it is genuinely good at — triggers, permissions, secrets — wrapped around a step that calls `lask run`. Switching providers then means rewriting that step, not your pipeline.

Here is how Lask compares to the lighter tools it replaces and to the heavier one it stops short of:

|                                         | Lask | make | Taskfile | Dagger |
| --------------------------------------- | :--: | :--: | :------: | :----: |
| Static checks before execution           | ✅ types, names, arity (`lask check`) | — | schema only | via the SDK's language |
| Typed task arguments with defaults       | ✅ `--name: String = "World"` | — | untyped vars | ✅ in the SDK's language |
| Execution environments as values         | ✅ `command { "go" } on #golang:1.22` | — | — | ✅ containers in the API |
| Concurrency                              | ✅ `async` / `await` | `-j` (per-target) | `deps` run in parallel | ✅ implicit in the DAG |
| Code reuse across projects               | ✅ hash-pinned module imports | `include` | `includes` | ✅ Git modules |
| Incremental rebuilds                     | — | ✅ file targets | ✅ checksum / timestamp | ✅ content-addressed cache |
| Config format                            | typed DSL | Makefile | YAML | Go / Python / TypeScript |
| What you install                         | ✅ Lask + Docker, and nothing a task uses | make, plus every tool a task uses | single binary, plus every tool a task uses | binary + Docker + an SDK toolchain |

**When to use something else:**

- If your tasks are primarily *"rebuild only what changed"* over file targets, `make` (or a real build system like Bazel) is the right tool. Lask does not track file freshness.
- If you need artifact caching at build-system scale, or you want your pipeline written in Go or TypeScript with a full SDK behind it, Dagger goes further than Lask does — at the cost of an engine to run and an ecosystem to keep.

**When Lask pays off:** tasks that take arguments, call each other, run in pinned Docker environments, or run concurrently — the point where Makefiles and YAML pipelines usually turn into untestable shell scripts. `lask check` verifies all of it before anything executes.

## Install

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
<summary><b>Debian / Ubuntu</b> &middot; .deb package</summary>

```bash
$ VERSION=$(curl -fsSL https://api.github.com/repos/lask-task-runner/lask/releases/latest | grep -m1 '"tag_name"' | cut -d '"' -f4)
$ curl -fsSL -o lask.deb "https://github.com/lask-task-runner/lask/releases/download/${VERSION}/lask-${VERSION}-linux-amd64.deb"
$ sudo dpkg -i lask.deb
```

amd64 only; on another architecture, take the tarball above. Uninstall with `sudo apt remove lask`.

</details>

<details>
<summary><b>Windows</b> &middot; PowerShell</summary>

```powershell
> $version = (Invoke-RestMethod https://api.github.com/repos/lask-task-runner/lask/releases/latest).tag_name
> Invoke-WebRequest -Uri "https://github.com/lask-task-runner/lask/releases/download/$version/lask-$version-windows-amd64.zip" -OutFile lask.zip
> Expand-Archive -Path lask.zip -DestinationPath "$env:LOCALAPPDATA\Programs\lask" -Force
> setx PATH "$env:PATH;$env:LOCALAPPDATA\Programs\lask"
```

Restart your terminal for the updated `PATH` to take effect. Uninstall with `Remove-Item -Recurse -Force "$env:LOCALAPPDATA\Programs\lask"`.

</details>

<details>
<summary><b>From source</b> &middot; Haskell toolchain</summary>

Needs [GHCup](https://www.haskell.org/ghcup/) or `brew install haskell-stack`.

```bash
$ stack --local-bin-path /usr/local/bin/ install
```

</details>

Verify with `lask --help`. Archives and packages for every platform are on the [latest release](https://github.com/lask-task-runner/lask/releases/latest); an APT repository and Chocolatey support are planned.

<details>
<summary><b>Shell completion</b> &middot; bash, zsh, fish</summary>

Completion knows your module, not just the CLI: it completes the functions the repository you are standing in defines, each function's keyword parameters, and the commands it declares.

```bash
$ lask run <TAB>
build_on_docker     install             test                uninstall_completion
doctest             install_completion  uninstall           unittest
$ lask run install --<TAB>
--output  --env  --help
$ lask run install --env <TAB>
docker  local
$ lask cmd <TAB>
mv  rm  stack  uname
```

**fish** — nothing else to do:

```fish
$ lask completion fish > ~/.config/fish/completions/lask.fish
```

**bash** — write the script somewhere and source it:

```bash
$ mkdir -p ~/.bash_completion.d
$ lask completion bash > ~/.bash_completion.d/lask
```

then, in `~/.bash_profile` (macOS Terminal starts a login shell, which does not read `~/.bashrc`) or in `~/.bashrc` (Linux):

```bash
source ~/.bash_completion.d/lask
```

With the `bash-completion` package installed — most Linux distributions have it — writing the script to `~/.local/share/bash-completion/completions/lask` instead loads it on demand, with nothing added to your rc file. That directory does nothing on a system without the package, which includes a stock macOS. Note that `source <(lask completion bash)` cannot be used there either: the `source` builtin in bash 3.2, still macOS's `/bin/bash`, silently reads nothing from a process substitution.

**zsh** — the completion system has to be switched on, which macOS does not do for you:

```zsh
$ mkdir -p ~/.zsh/completions
$ lask completion zsh > ~/.zsh/completions/_lask
```

then, in `~/.zshrc`:

```zsh
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit
```

If `compinit` already runs in your `~/.zshrc` — every framework does it for you — only the `fpath` line is new, and any directory already on `$fpath` works just as well. `compinit` caches what it found, so after adding a file, delete `~/.zcompdump*` and open a new shell. If completion does nothing and `command not found: compdef` appears when zsh starts, `compinit` has not run.

The script only ever asks the binary, so it keeps working across upgrades. Completion reads your module without running it: no task, no default value, and no environment is ever evaluated to answer a `<TAB>`.

</details>

## Example

Before a task first runs in a container, `lask env build` pulls its image and pins the digest in `lask.lock.json`; `lask run` itself never reaches the network, so every machine runs the image the lock names.

Run a command in any image straight from the REPL, with nothing installed locally:

```
$ lask repl
lask> $[#rancher/cowsay] cowsay "Lask"
```

## Editor Support

`lask serve` is a language server built into the same binary. Install the [Lask extension from the VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=ToruIkeda.vscode-lask), or search for "Lask" in VS Code.

## Usage

```bash
$ lask check                       # static validation
$ lask run <function> [args...]    # execute (result not printed)
$ lask eval <function> [args...]   # execute and print the result as JSON
$ lask cmd <command> [args...]     # run a declared command in its declared image
$ lask envs [--check]              # list/check referenced environments
$ lask env build | list            # materialize / inspect container images
$ lask deps sync                   # fetch + verify external dependencies
$ lask deps add <name> <source>    # add a dependency: --git <url> --rev <rev>, or --url <url>
$ lask deps why <name>             # show why a dependency is in the graph
$ lask repl                        # interactive session
$ lask serve                       # language server (LSP)
$ lask version                     # print the lask version
```

The [Quick Reference](doc/quick-reference.md) covers the whole language and CLI in ten minutes; [doc/spec.md](doc/spec.md) is the full specification behind it.

## Status

Lask is pre-1.0: features are `experimental` until 1.0, and breaking changes are still possible. See [doc/compatibility.md](doc/compatibility.md) for what `stable` will mean once released. Until then, [feedback](#feedback) shapes what becomes stable.

## Feedback

Questions, ideas and feedback of any kind are welcome in [GitHub Discussions](https://github.com/lask-task-runner/lask/discussions): ask in [Q&A](https://github.com/lask-task-runner/lask/discussions/categories/q-a), suggest a feature in [Ideas](https://github.com/lask-task-runner/lask/discussions/categories/ideas), or share what you built in [Show and tell](https://github.com/lask-task-runner/lask/discussions/categories/show-and-tell). If something in this README or the [Quick Reference](doc/quick-reference.md) was unclear, or a task you wanted to write did not fit the language, that is worth a discussion too.

Found a bug? Please open an [issue](https://github.com/lask-task-runner/lask/issues).

## Development

```bash
$ lask run test
```
