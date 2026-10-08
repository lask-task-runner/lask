# Migrating from Make

This guide moves a Makefile's tasks into a `main.lask`, one task at a time, without a day where nothing works. It assumes you know Make and have skimmed the [Quick Reference](../quick-reference.md); everything here is linked back to it or to the [specification](../spec.md).

## Should you migrate?

Make answers one question very well: *which files are out of date, and what rebuilds them?* Lask does not answer it at all — it has no file targets and no notion of freshness. So look at what your Makefile actually does:

- **Mostly `.PHONY` targets** — `test`, `lint`, `build`, `deploy`, `clean` — used as a list of named commands. This is what Lask is for, and what this guide covers.
- **Mostly file rules** — `%.o: %.c`, generated sources, a dependency graph Make walks to skip work. Keep Make for those. You can still call it from Lask (see [Step 1](#step-1-wrap-the-makefile)) and move only the phony part.

Most real Makefiles are a mix, and the migration below lets you stop at any point with both files working.

## What you get

- **No tools on the host.** Each command runs in an image pinned by digest in `lask.lock.json`. A new contributor installs Lask and Docker, and `go`, `golangci-lint` and the rest come from the lock — not from a "Prerequisites" section in the README.
- **Checks before anything runs.** `lask check` resolves every task name, argument and type. A typo in a target name is an error in your editor, not a `make: *** No rule to make target` halfway through a release.
- **Real arguments.** `make deploy ENV=prod` becomes `lask run deploy --env prod`, with a default, a type, `--help` text and shell completion, and a misspelt `--evn` is rejected instead of silently ignored.
- **Concurrency you write down.** `async` / `await` in the task itself, rather than `make -j` for the whole invocation.

## A complete example

A typical Makefile for a Go service:

```make
APP      ?= hello
VERSION  ?= $(shell git describe --tags --always)
REGISTRY := registry.example.com
ENV      ?= dev

.PHONY: all ci test lint build image deploy clean

all: build

ci: lint test build

test:
	go test ./...

lint:
	golangci-lint run

build:
	CGO_ENABLED=0 go build -trimpath -ldflags "-X main.version=$(VERSION)" -o bin/$(APP) ./cmd/$(APP)

image: build
	docker build -t $(REGISTRY)/$(APP):$(VERSION) .

deploy: image
	docker push $(REGISTRY)/$(APP):$(VERSION)
	./scripts/deploy.sh $(ENV) $(VERSION)

clean:
	-rm -r bin
```

The same thing as `main.lask`:

```lask
// Every Go command runs in this image, with the build cache kept in a
// named volume between runs.
go = #golang:1.25-alpine{
  env: {"CGO_ENABLED": "0"},
  volumes: ["lask-go-cache:/root/.cache"]
}
lint = #golangci/golangci-lint:v2.5.0-alpine

command { "go" } on go
command { "golangci-lint" } on lint
command { "git", "docker", "rm" } on #local

registry = "registry.example.com"

// What `git describe` says this commit is, worked out once per run.
version = $ git describe --tags --always
  |> trim

// Run the unit tests.
test(): String = $ go test ./...

// Run the linters.
lint_go(): String = $ golangci-lint run

// Build the binary.
//
// @param app      The command under ./cmd to build.
// @param version  Version stamped into the binary.
// @param os       Target operating system; the container is Linux.
// @return The path of the binary.
// @complete os linux darwin windows
build(--app = "hello", --version: String = version, --os = "linux"): String = do {
  $ GOOS=#{os} go build -trimpath -ldflags "-X main.version=#{version}" -o bin/#{app} ./cmd/#{app}
  "bin/#{app}"
}

// Build and tag the container image.
//
// @return The image reference.
image(--app = "hello", --version: String = version): String = do {
  build(app = app, version = version)
  ref = "#{registry}/#{app}:#{version}"
  $ docker build -t #{ref} .
  ref
}

// Deploy a version.
//
// @param env  Where to deploy.
// @complete env dev staging prod
deploy(--env = "dev", --app = "hello", --version: String = version) = do {
  ref = image(app = app, version = version)
  $ docker push #{ref}
  $[#local] ./scripts/deploy.sh #{env} #{version}
}

// Lint and test in parallel, then build.
ci(): String = do {
  all([async lint_go(), async test()])
  build()
}

// Remove build output; a missing directory is not an error.
clean(): CommandResult = $* rm -r bin
```

And a `lask.json` next to it, so that a production deploy has to be confirmed:

```json
{"confirm": {"deploy": {"when": {"env": ["prod"]}}}}
```

Running it:

```bash
$ lask sync                       # once, and whenever an image changes: pulls and pins
$ lask check                      # nothing runs
$ lask run ci                     # was: make ci (or make -j ci)
$ lask run build --os darwin      # a binary for your Mac, built in the Linux image
$ lask run deploy --env prod      # was: make deploy ENV=prod — now asks first
$ lask run deploy --help
```

The rest of this guide explains each change, and how to get from the first file to the second in small steps.

## Migrating step by step

### Step 1: Wrap the Makefile

Start with a `main.lask` that only calls Make. Nothing changes about how the work is done, but every target now has a checked name, `--help`, and shell completion, and you can start calling `lask run` from CI.

```lask
command { "make" } on #local

// Run the unit tests.
test(): String = $ make test

// Build the binary.
build(--version: String = "dev"): String = $ make build VERSION=#{version}
```

`#local` is the host: these commands use whatever `make` and `go` are installed there, exactly as before. It has to be written out — a command whose environment is not declared is a static error, never a silent fall back to the host.

### Step 2: Move one task at a time

Take a target, copy its recipe into the task body, and translate the Make syntax ([reference below](#reference-make-to-lask)). Keep it on `#local` for now, so that a difference in behaviour can only come from the translation:

```lask
command { "go" } on #local

test(): String = $ go test ./...
```

Run both and compare. Delete the target from the Makefile once nothing calls it — `grep` for `make <target>` in scripts, CI files and docs.

### Step 3: Pin the environments

Now change where the tools come from. Move each command word from `#local` to an image, run `lask sync` to pull it and record its digest, and the host no longer needs that tool:

```diff
-command { "go" } on #local
+command { "go" } on #golang:1.25-alpine
```

Only the `command` line changes; the tasks that run `go` stay as they are. `lask envs list` shows every image the module uses and whether it is pinned. Commit `lask.lock.json`: it is what makes your laptop and CI run the same image.

Some tools are better left on the host, and that is fine. In the example, `docker` and `git` stay on `#local`: `docker build` needs the host's daemon, and `git describe` reads the host's repository and credentials.

### Step 4: Remove the Makefile — or keep what Make is good at

When only file rules remain, keep them and call Make from the task that needs them (`$ make bin/parser.c`). When nothing remains, delete the Makefile, and leave a short note in the README for people whose fingers still type `make test`.

## Reference: Make to Lask

| Make | Lask |
| --- | --- |
| `test:` (a `.PHONY` target) | `test() = ...` |
| A recipe of several lines | `do { ... }`, one `$` per line |
| `$(VAR)` | `#{var}` |
| `VAR ?= default`, `make t VAR=x` | `t(--var = "default")`, `lask run t --var x` |
| `VAR ?= $(ENV_VAR)` | `--var = get_env_or("ENV_VAR", "default")` |
| `VAR := $(shell cmd)` | `var = $ cmd` (top level, evaluated once on first use) |
| `$$HOME` | `$HOME` — only `#{...}` is Lask's |
| `target: dep1 dep2` | call `dep1()` and `dep2()` in the body |
| `make -j` | `async` / `await`, `all([...])` |
| `-cmd` (ignore failure) | `$* cmd`, or `try` / `catch` |
| `make -k` | `try` / `catch` around each step |
| `@cmd` (don't echo) | — every command is logged to stderr; `run` prints nothing to stdout |
| `export VAR` | `env: {"VAR": ...}` on the environment |
| `include other.mk` | `import { task } from "./other.lask"` |
| A hand-written `help` target | doc comments, shown by `lask run <task> --help` |
| `.DEFAULT_GOAL` | — name the task: `lask run build` |
| "requires Go 1.25" in the README | `command { "go" } on #golang:1.25-alpine` |

## Differences worth knowing

### Arguments instead of variables

A Make variable can be set from the command line, from the environment, or in the file, and a misspelt one is silently ignored. A Lask keyword parameter has one default, written next to the task, and the CLI rejects a parameter the task does not declare:

```bash
$ make deploy EVN=prod          # deploys to dev
$ lask run deploy --evn prod    # error: unknown parameter
```

When a value really should come from the environment, say so in the default: `--region = get_env_or("AWS_REGION", "us-east-1")`. Use `get_env` instead when the variable is required, so that a missing one fails with its name.

Parameters keep their `_` in the code and take `-` on the command line: `build(--dry_run = false)` is `lask run build --dry-run true`.

### Prerequisites are calls, and each call runs

`deploy: image` becomes `image()` in the body of `deploy`. The ordering is the same, but one rule is different: **Make runs each target at most once per invocation; Lask runs a task every time it is called.** With a diamond such as

```make
release: test image
image: build
test: build
```

`make release` builds once, while the literal translation builds twice. Two ways out:

- Call shared steps from one place. Let `release` call `build()` once and pass the result on, and let `test` and `image` take what they need as parameters instead of calling `build` themselves.
- Compute it in a top-level value. A top-level value is evaluated at most once, on first use, which is why `version` in the example runs `git describe` once however many tasks read it.

### Concurrency is in the task, not on the command line

`make -j4 ci` parallelizes whatever Make finds independent, and a Makefile that was never written for `-j` breaks under it. In Lask you say what runs in parallel:

```lask
ci(): String = do {
  all([async lint_go(), async test()])
  build()
}
```

`async` starts a call and returns a handle; `await` or `all` waits, and a failure inside is raised there. Avoid naming a task `all` — it would shadow the built-in `all`.

### Each line is still its own process

As in Make, each `$` line is a separate process, so `cd` does not carry over to the next line: write `$ cd web && npm ci`. Unlike Make, a command string runs to the end of its line, so a pipe into a Lask function goes on the next line:

```lask
version = $ git describe --tags --always
  |> trim
```

### Containers do not see the host's environment

A command in an image starts with the image's own variables, not yours. `export GOFLAGS` in a Makefile, or a variable set in your shell, does not reach it. Pass what it needs with the `env` run option:

```lask
command { "go" } on #golang:1.25-alpine{env: {"GOFLAGS": get_env_or("GOFLAGS", "")}}
```

For credentials, bind the value with `!!` so it is masked in the log — see [Input, output, secrets](../quick-reference.md#input-output-secrets).

### Container builds produce Linux binaries

`make build` on a Mac produces a Mac binary; the same `go build` in a Linux image produces a Linux one. This is usually what you want for an image or a server, and a cross-compiler such as Go's makes the other case one parameter, as `build --os darwin` does in the example.

### Caches live in volumes

The container is fresh on every run, so a tool's download and build caches are thrown away unless you keep them. Mount a named volume on the cache directory, as the example does for Go:

```lask
go = #golang:1.25-alpine{volumes: ["lask-go-cache:/root/.cache"]}
```

### Scripts are run with an explicit environment

A `command` declaration names program words, not paths, so `./scripts/deploy.sh` cannot be declared there. Name its environment at the call instead: `$[#local] ./scripts/deploy.sh`, or run it through an interpreter you have declared, such as `$ sh scripts/deploy.sh`.

### `run` prints nothing

`lask run` writes nothing to stdout — commands' output goes to the log on stderr. When a task's result is the point, as in `make print-version`, use `lask eval`:

```bash
$ lask eval build      # "bin/hello"
```

### Dangerous targets can ask first

There is no Make equivalent of the `confirm` entry in `lask.json`: `lask run deploy --env prod` stops and asks you to type `prod`. Where there is no terminal, such as in CI, it fails with exit code 4 unless `--confirm` is given — so a deploy job states that it means it.

## Checklist

- [ ] `lask check` passes, and runs in CI before anything else.
- [ ] `lask.lock.json` is committed, and CI runs `lask sync --frozen`.
- [ ] Every tool that should not be on the host is declared on an image; `lask envs list` shows no `#local` you did not intend.
- [ ] Every task that had a `help` line has a doc comment.
- [ ] No target is reached twice through prerequisites without your knowing.
- [ ] Variables that came from the environment are read with `get_env_or` or `get_env`, and passed to containers with `env`.
- [ ] Scripts, CI configuration and docs no longer call `make <target>` for a target you removed.
