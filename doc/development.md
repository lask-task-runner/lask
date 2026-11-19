# Development

How to build, test and release Lask. For the language itself, see the [specification](spec.md); for what a release may change, the [compatibility policy](compatibility.md).

## Setup

You need [Stack](https://docs.haskellstack.org/) (through [GHCup](https://www.haskell.org/ghcup/) or `brew install haskell-stack`), Docker, and a `lask` binary to run the tasks in [main.lask](../main.lask). The first `lask run install` below gives you one from this checkout; until then, `stack --local-bin-path ~/.local/bin install` does the same.

Work happens on `dev`: branch from it, and open pull requests against it. `main` moves only when a release is made.

## Commands

The tasks in [main.lask](../main.lask) wrap what you run day to day. `lask run --help` lists them, and `lask run <task> --help` describes one.

```bash
$ lask sync                              # once: pulls the Haskell image the docker environment uses

$ lask run test                          # every suite, in the Haskell image
$ lask run test --env local              # the same on the host, with your own Stack
$ lask run unittest [--match Parser]     # unit tests only (test/unit): seconds, no binary, no engine
$ lask run e2etest [--match Cli.Help]    # end-to-end tests only (test/e2e): the built binary
$ lask run doctest                       # doctests of app/ and src/ only

# one kind of test at a time (each also takes --env local)

$ lask run corpustest                    # the diagnostics corpus, test/unit/corpus
$ lask run proptest                      # the QuickCheck properties of the syntax
$ lask run goldentest [--accept]         # the golden outputs, test/e2e/golden; --accept rewrites them
$ lask run spectest                      # the lask blocks of doc/spec.md
$ lask run exampletest [--offline]       # every project under example/
$ lask run errorcodetest                 # every error and advisory code, through the binary

$ lask run install                       # build and install lask into ~/.local/bin
$ lask run install --output /usr/local/bin
$ lask run install-completion --shell zsh   # or bash, fish; prints the line your rc file still needs
$ lask run uninstall
$ lask run uninstall-completion --shell zsh
```

Stack directly, for what the tasks do not cover:

```bash
$ stack build                            # compile only
$ stack test lask:lask-unit              # one suite: lask-unit or lask-e2e
$ stack test --ta '--match "Parser"'     # the hspec specs whose description matches
$ stack exec lask -- check               # the binary just built, without installing it
```

### Tests

Two hspec suites, and the doctests:

| Where | Suite | What it tests |
|---|---|---|
| `test/unit` | `lask-unit` | The library, called in process: no binary, no container engine. A spec mirrors the module it tests, `test/unit/Language/Lask/Lexer/LayoutSpec.hs` for `src/Language/Lask/Lexer/Layout.hs`. |
| `test/e2e` | `lask-e2e` | The built `lask` binary, run as a person or CI runs it. `Cli/` holds one spec per area of the command line; the rest check whole sets: every error code, every example, every spec example, the golden outputs. Some start containers; those that need an engine they cannot have use a fake one. |
| `test/support` | both | Helpers, never specs: `Support.Harness` (finding and running the binary, throwaway projects, the fake engine), `Support.FakeVault`, `Support.SyntaxGen` (QuickCheck generators of the syntax), `Support.EmbedTwin`, `Support.Pending` (`knownBug`, a test of a known bug). |
| `src`, `app` | doctests | The `>>>` examples in the documentation of functions. |

A new test goes in `lask-unit` unless it needs the binary. Two kinds of test are files rather than code:

- **`test/unit/corpus`**: programs that state their own diagnostics. A line ending in `// expect: E-A, W-B` expects those codes there, and every code reported must be expected; a file with none must check cleanly. A case is a `.lask` file, or a directory whose `main.lask` imports the files beside it. Add a case for each way a rule can be broken.
- **`test/e2e/golden`**: a directory per command line, with its `.lask` files, `args` (one per line) and `expected` (exit code, stdout, stderr, with the project's directory written `<dir>`). For output read whole: help, the `envs` tables, the text of diagnostics. `lask run goldentest --accept` rewrites `expected` from the binary; read the diff before committing it.

**Pending** means one of two things, and its message says which:

- A known bug, with its reproduction. A corpus case marks it with a first line `// pending: <reason>`, a spec with `knownBug`, which still runs it. It fails once it passes, so the mark goes with the fix.
- A test that needs the network, left pending with `LASK_TEST_OFFLINE=1`, as the macOS job runs.

**Coverage** counts both suites, and with `LASK_TEST_TIX_DIR=<dir>` the runs of the binary the end-to-end tests start, each writing its own tix there; `ci-test` sets it, and `coverage-report` sums them.

### What CI runs

[.github/workflows/test.yml](../.github/workflows/test.yml) runs on every push to every branch. It builds lask with `stack build --copy-bins --local-bin-path dist/bin`, then runs the `ci` task with that binary, so a commit is checked by itself:

```bash
$ stack build --copy-bins --local-bin-path dist/bin
$ dist/bin/lask run ci                   # what CI runs: everything below, in order
$ dist/bin/lask run ci-cabal             # lask.cabal is what stack generates from package.yaml
$ dist/bin/lask run ci-test              # unit tests with coverage, then doctests, on the host
$ dist/bin/lask run smoke-colon-dir      # a containerized task from a directory with a colon in its path
$ dist/bin/lask run smoke-completion     # the completion scripts, through bash, zsh and fish if present
$ dist/bin/lask eval --output text coverage-report   # per-module coverage, after ci-test
```

`ci-cabal` fails when `package.yaml` changed and the regenerated `lask.cabal` was not committed: commit it along with the change.

## Releasing

A release is version `X.Y.Z`, tagged `vX.Y.Z` on `main`. Each of the three repositories involved has a `release` task in its `main.lask`, run from a checkout on the host with git and an authenticated [gh](https://cli.github.com/). The documentation site's `release` asks you to type `release X.Y.Z` before it starts.

### 1. Open the release pull request (this repository)

```bash
$ lask run release-bump X.Y.Z
```

Switches to `dev` and pulls it, sets the version in [package.yaml](../package.yaml) and `lask.cabal`, commits `chore: bump version to X.Y.Z`, pushes `dev`, and opens the pull request from `dev` to `main`. Complete its description with the breaking changes since the previous release, and merge it once CI passes.

### 2. Build, publish and test the release (this repository)

Write the release notes to a file. They open with a paragraph on what the release is about, followed by sections of bullets, breaking changes first and fixes last; see [v0.8.0](https://github.com/lask-task-runner/lask/releases/tag/v0.8.0). Then:

```bash
$ lask run release X.Y.Z --notes notes.md
```

It checks that `main` declares `X.Y.Z` and that `vX.Y.Z` does not exist yet, then:

1. runs the **Release** workflow ([release.yml](../.github/workflows/release.yml)) on `main` and waits for it, about an hour. Each platform's job builds lask and runs the `release-package` task, which writes the archives, a `.deb` on Linux and a `.sha256` beside each;
2. downloads the files into `dist/release`;
3. creates the GitHub release `vX.Y.Z` on `main` with the notes and the archives and `.deb`, which creates the tag;
4. runs the **Install test** workflow ([install-test.yml](../.github/workflows/install-test.yml)) against it and waits for it. It installs the release the way the [installation page](https://lask-task-runner.github.io/guide/installation) says to on each platform and runs [example/01-projects/01-hello-world](../example/01-projects/01-hello-world).

If a workflow fails, the task stops there; fix the cause and run the remaining steps by hand, as the list above gives them.

### 3. Update the Homebrew formula ([homebrew-tap](https://github.com/lask-task-runner/homebrew-tap))

```bash
$ lask run release X.Y.Z
```

Rewrites `Formula/lask.rb` for `vX.Y.Z` and adds `Formula/lask@X.Y.Z.rb`, keg-only, for installing that version on its own, both with the `sha256` GitHub records for each archive, then commits and pushes `main`. Check it with `brew update && brew upgrade lask && lask version`.

### 4. Release the documentation ([lask-task-runner.github.io](https://github.com/lask-task-runner/lask-task-runner.github.io))

```bash
$ lask run release X.Y.Z X.Y+1.0
```

Names `X.Y.Z` in Getting started and Running in CI, tags that state `docs-X.Y.Z`, points `X.Y.Z` at the tag in `versions.json` and adds the next version on top, then pushes `main` and the tag. The tag `vX.Y.Z` from step 2 is what marks the version released on the site: the next deploy, at the latest the daily one, drops "(unreleased)" and sends `/` to it.
