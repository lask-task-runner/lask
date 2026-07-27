# Lask

Lask (coined by combining "lambda" and "task") is a task runner based on functional programming.
Tasks are expressed as functions in `.lask` modules and executed from the CLI. The language,
CLI, execution environments and observability are defined by the specification in
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

### Features

- Statically checked before execution: syntax, name resolution, and a structural type system
  (`Number`, `String`, `Bool`, `Array<T>`, `Map<T>`, `Record<...>`, `Function<...>`,
  `AsyncHandle<T>`, `Environment`).
- Procedural sugar (`do`, `if`/`else`, `for`, `return`, `try`/`catch`/`finally`,
  `async`/`await`) normalized onto a small functional core.
- Command execution with environment selection: `$ cmd` (stdout), `$2 cmd` (stderr),
  `$* cmd` (whole result), `$[#alpine:3.20] cmd` (Docker),
  `$[#env("name")] cmd` (named environments from `environments.lask.json`, including SSH remotes).
- Modules with named/namespace imports; stdin bound as the `stdin` string; JSON I/O.
- Observability: trace IDs, `call`/`return`/`fail` execution events (`--format json`),
  stack traces, spec-defined exit codes.

### Installation

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
