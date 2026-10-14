# Development

How to build, test and release Lask. For the language itself, see the [specification](spec.md); for what a release may change, the [compatibility policy](compatibility.md).

## Setup

You need [Stack](https://docs.haskellstack.org/) (through [GHCup](https://www.haskell.org/ghcup/) or `brew install haskell-stack`), Docker, and a `lask` binary to run the tasks in [main.lask](../main.lask). The first `lask run install` below gives you one from this checkout; until then, `stack --local-bin-path ~/.local/bin install` does the same.

Work happens on `dev`: branch from it, and open pull requests against it. `main` moves only when a release is made.

## Commands

The tasks in [main.lask](../main.lask) wrap what you run day to day. `lask run --help` lists them, and `lask run <task> --help` describes one.

```bash
$ lask sync                              # once: pulls the Haskell image the docker environment uses

$ lask run test                          # unit tests and doctests in parallel, in the Haskell image
$ lask run test --env local              # the same on the host, with your own Stack
$ lask run unittest [--env local]        # unit tests only (test/)
$ lask run doctest [--env local]         # doctests of app/ and src/ only

$ lask run install                       # build and install lask into ~/.local/bin
$ lask run install --output /usr/local/bin
$ lask run install-completion --shell zsh   # or bash, fish; prints the line your rc file still needs
$ lask run uninstall
$ lask run uninstall-completion --shell zsh
```

Stack directly, for what the tasks do not cover:

```bash
$ stack build                            # compile only
$ stack test --ta '--match "Parser"'     # the hspec specs whose description matches
$ stack exec lask -- check               # the binary just built, without installing it
```

### What CI runs

[.github/workflows/test.yml](../.github/workflows/test.yml) runs on every push to every branch. It builds lask with `stack build --copy-bins --local-bin-path dist/bin`, then runs the `ci` task with that binary, so a commit is checked by itself:

```bash
$ stack build --copy-bins --local-bin-path dist/bin
$ dist/bin/lask run ci                   # what CI runs: everything below, in order
$ dist/bin/lask run ci-cabal             # lask.cabal is what stack generates from package.yaml
$ dist/bin/lask run ci-test              # unit tests with coverage, then doctests, on the host
$ dist/bin/lask run smoke-colon-dir      # a containerized task from a directory with a colon in its path
$ dist/bin/lask run smoke-completion     # the completion scripts, through bash, zsh and fish if present
$ dist/bin/lask eval --stdout-encode text coverage-report   # per-module coverage, after ci-test
```

`ci-cabal` fails when `package.yaml` changed and the regenerated `lask.cabal` was not committed: commit it along with the change.

## Releasing

A release is version `X.Y.Z`, tagged `vX.Y.Z` on `main`. The steps below span this repository, [homebrew-tap](https://github.com/lask-task-runner/homebrew-tap) and the [documentation site](https://github.com/lask-task-runner/lask-task-runner.github.io).

### 1. Bump the version on `dev`

Set `version: X.Y.Z` in [package.yaml](../package.yaml), run `stack build` so that `lask.cabal` follows, and commit both (`chore: bump version to X.Y.Z`). The Release workflow refuses to package a version `package.yaml` does not declare.

### 2. Merge `dev` into `main`

Open a pull request from `dev` to `main`, titled after the bump. Its description summarizes the release and lists the breaking changes since the previous version. Merge it once CI passes.

### 3. Build the release files

Run the **Release** workflow ([.github/workflows/release.yml](../.github/workflows/release.yml)) from the Actions tab, with `version` `X.Y.Z`, `check_version` on and `ref` `main`:

```bash
$ gh workflow run release.yml -f version=X.Y.Z -f check_version=true -f ref=main
```

It builds on Linux, macOS (Intel and Apple silicon) and Windows, and runs the `release-package` task on each, which writes the archives, a `.deb` on Linux and a `.sha256` beside each file. The `publish` job gathers them into one workflow artifact, `lask-X.Y.Z`, kept for 90 days. A full run takes about an hour.

```bash
$ gh run download <run-id> -n lask-X.Y.Z -D dist/release
```

### 4. Publish the GitHub release

Create the release `vX.Y.Z` on `main` and attach the archives and the `.deb`:

```bash
$ gh release create vX.Y.Z --target main --title vX.Y.Z --notes-file notes.md \
    dist/release/lask-vX.Y.Z-*.tar.gz dist/release/lask-vX.Y.Z-*.zip dist/release/lask-vX.Y.Z-*.deb
```

This creates the tag. The notes open with a paragraph on what the release is about, followed by sections of bullets, breaking changes first and fixes last; see [v0.8.0](https://github.com/lask-task-runner/lask/releases/tag/v0.8.0).

### 5. Update the Homebrew formula

In homebrew-tap, set the three URLs in `Formula/lask.rb` to `vX.Y.Z` and each `sha256` to the one in the matching `.sha256` file (macos-arm64, macos-amd64, linux-amd64). Commit (`Update Lask formula to version X.Y.Z …`) and push to `main`, then check it:

```bash
$ brew update && brew upgrade lask && lask version
```

### 6. Run the install test

Run the **Install test** workflow ([.github/workflows/install-test.yml](../.github/workflows/install-test.yml)) with `version` `X.Y.Z`. It installs the published release the way the [installation page](https://lask-task-runner.github.io/guide/installation) says to on each platform, and runs [example/01-projects/01-hello-world](../example/01-projects/01-hello-world) with it.

```bash
$ gh workflow run install-test.yml -f version=X.Y.Z
```

### 7. Release the documentation

The site keeps one copy per version, and the tag `vX.Y.Z` is what marks it released: the next deploy, at the latest the daily one, drops "(unreleased)" and sends `/` to it. Then, in the site repository, freeze the released version and start the next one, as its README describes under "When a version is released and the next one starts": update the version shown in Getting started and Running in CI, tag `docs-X.Y.Z`, and in `versions.json` point `X.Y.Z` at that tag and add the next version on top.
