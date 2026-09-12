# Specification: Command Declarations, Explicit Environments, and `lask cmd`

Status: normative draft
Affects: spec.md 3.3, 5, 6.1, 6.6, 7.5, 7.7, 8.7, 9, 10.1, 10.3, 11.1, 11.3, 11.6, 12.3, 14.2
Companion document: `command-dispatch.md`

This document specifies:

1. the `command` declaration, which binds a program name to the environment that
   provides it (§1);
2. the explicit environment rule, which removes the implicit `#local` fallback
   (§2) and the consequent change to `run_command` (§3);
3. `lask cmd`, which invokes a declared command from the CLI (§4).

How a command execution expression reaches a declared environment is specified
in `command-dispatch.md`, which builds on this document.

## 1. The `command` Declaration

```ebnf
TopLevelDecl  = ( ImportDecl | ExportDecl | CommandDecl
                | [ Visibility ] ( TypeAliasDecl | Declaration ) ) decl_end .
CommandDecl   = "command" command_names "on" Expression .
command_names = string_lit { "," string_lit } .
```

A command declaration registers each name as a **command word**: the name of a
program that the given environment provides. It binds nothing.

```lask
node = #node:20.20.2-alpine3.23

command "go" on #golang:1.25
command "node", "npm", "npx" on node
command "docker-compose" on #docker("docker/compose:2.29.7")
command "7z", "g++" on #alpine:3.20
command "mv", "rm", "ls" on #local
```

Syntax rules:

- `command` is a **reserved word** (3.3). `on` is a contextual keyword
  recognized only within a `CommandDecl`, where the position after the name list
  admits nothing else; it remains usable as an identifier everywhere. (No
  occurrence of either word as an identifier exists in this repository,
  `lask-terraform`, or `lask-aws`.)
- A name is a string literal containing no interpolation, for the same reason
  10.2 requires it of `dockerfile` and `context`: the set of command words a
  module declares must be determinable without evaluating it.
- The declaration takes no visibility marker. It binds no name and does not
  cross module boundaries (below), so `export` and `internal` would qualify
  nothing; writing one is a syntax error.

Validity of a name:

- A name is valid exactly when feeding it alone, as a whole command string,
  through the dispatch procedure (`command-dispatch.md` §2.1-§2.4) yields
  exactly one candidate whose text is the name itself. Otherwise it is
  `E-TYPE-COMMAND-NAME`.
- The rule is stated against the procedure rather than as a character list so
  that the two cannot drift apart: a name that could never be recognized in a
  command string is rejected where it is written. `docker-compose`, `7z`,
  `g++`, `pip3` and `mvn.cmd` are valid; `my prog` (whitespace), `a=b` (read as
  an assignment word), `/usr/bin/x` (contains `/`) and `foo*` (a glob) are not.

Typing rules:

- The environment must be **statically resolvable**: an environment expression
  (6.7), or an identifier resolving to a top-level value declaration whose
  right-hand side is, transitively, an environment expression. Anything else is
  `E-TYPE-COMMAND-DECL`.
- The restriction exists so that every declared environment is enumerable by
  `envs` (11.4), pinnable in the lock file (10.3), and resolvable by `lask cmd`
  (§4) without evaluating the module.

Scope rules:

- The command words of a module are exactly those its own `CommandDecl`s
  register. Declarations do not cross module boundaries: importing a module
  never changes what a command execution expression in the importing module
  means.
- Declaring the same name twice in one module is `E-TYPE-COMMAND-DUPLICATE`.
- The names occupy no namespace shared with values or types, so no collision
  with a declaration, an import, or a type alias is possible.
- A module that needs the environment as a value binds it in the ordinary way
  and registers commands on the binding, as `node` does above. This is also how
  the environment reaches a bracketed environment specification: `$[node] ...`.

A shared table across the files of one project is a later, backward-compatible
addition — an explicit `import command { "npm", "node" } from "./tools.lask"`,
which keeps a file's command words determined by that file alone. It is not
part of this specification.

## 2. The Explicit Environment Rule

The implicit `#local` fallback is removed. **Every command execution expression
must determine an environment**, and a command that determines none is a static
error.

Amendment to 10.1 — the sentence

> The default execution environment (the environment used by `$ cmd` /
> `run_command`; 6.6, 8.7) is always `#local`. The default execution environment
> must not be changed by CLI options or other external configuration.

is replaced by

> There is no default execution environment. Every command execution expression
> determines its environment from the expression itself (6.6), and an expression
> that determines none is a static error (`E-TYPE-COMMAND-NOENV`). Local
> execution is written `#local` like any other environment. No CLI option or
> other external configuration may supply, alter, or override the environment of
> a command.

Rationale:

- The failure this removes is silent. Under the previous rule, deleting a
  `command` declaration — or mistyping a reference to one — left the command
  running against whatever the host happened to provide, producing a result that
  is plausible, unreproducible, and unremarked. Requirement 2.4 ("implicit
  dependence on the execution environment") is the problem Lask exists to solve,
  and an implicit fallback to the host is that problem in the language itself.
- Under the new rule the same mistake is a static error naming the expression.
- Host execution remains available, unprivileged, and one token away. It is now
  a statement rather than a silence: `$[#local] mv ./lask #{output}/lask`.

Rules:

- `$[e] cmd` is unaffected: `e` may be `#local`.
- A command execution expression with no environment specification is
  `E-TYPE-COMMAND-NOENV` whenever dispatch (`command-dispatch.md` §2) selects
  nothing.
- `#local` is an environment like any other in a `command` declaration.
  `command "mv", "rm", "ls" on #local` is well-formed, and is how a project
  states which programs it runs on the host. Dispatch treats `#local` on equal
  terms with every other environment, so a line mixing a host-declared and a
  container-declared program is a static error rather than a silent choice
  (`command-dispatch.md` §2.5).
- No shorthand for `#local` is introduced. Writing it repeatedly is a sign that
  the host programs involved should be declared; where the explicit form is
  still wanted, an ordinary binding serves (`l = #local`, then `$[l] ...`).

Migration:

Across this repository, `lask-terraform` and `lask-aws`, 18 command execution
expressions are bare and 38 already carry an environment. Each of the 18 is
repaired either by inserting `[#local]` or by declaring the program it runs —
they are mostly `mv`, `rm`, `uname` and `docker`, so a few declarations cover
most of them. A codemod shipped with the change should offer the mechanical
form, leaving the declaration form to the author.

## 3. `run_command`

The core function changes signature (6.6, 7.5, 8.7):

```lask
run_command(cmd: String, env: Environment): CommandResult
```

- `env` becomes a required **positional** parameter. A keyword parameter must
  have a default value (6.1), so a required environment cannot be expressed as
  one; making it positional keeps the change inside the existing parameter
  model rather than extending it.
- The function type becomes `Function<String, Environment, CommandResult>`.
- The desugaring of 6.6 passes it positionally: `$[e] cmd` normalizes to
  `run_command("cmd", e)`.
- `run_command("...")` with one argument is `E-TYPE-ARITY`, which closes the
  escape hatch that would otherwise have survived §2: without this change, the
  core function would still default to the host while the sugar over it could
  not.

This is a signature change to a built-in, which `compatibility.md` §3 prohibits
"in principle". It is taken deliberately, before the first release, under the
effective-date clause of `compatibility.md` §1.

## 4. `lask cmd`

Declared commands are useful outside tasks. Operationally one wants to run the
project's pinned `terraform` or `psql` by hand — against the same image the
tasks use, with the same working directory — without writing a task for it and
without reconstructing the `docker run` invocation.

### 4.1 Syntax

```text
lask cmd [--module <path>] [lask options ...] <command> [args ...]
lask cmd --list [--module <path>]
```

`<command>` is a command word in the scope of the target module (§1). All tokens
after it are arguments to the program.

### 4.2 Argument boundary

- Options of `lask` itself must be placed **before** `<command>`, following the
  boundary rule of 11.2.
- **All tokens after `<command>` are passed to the program verbatim**, with no
  interception whatsoever. This diverges deliberately from 11.2, which
  intercepts a standalone `--help` after a function name: here `--help`, `-h`
  and `--` are the program's own flags, and `lask cmd go --help` must reach
  `go`. The help of the subcommand is `lask cmd --help` with no command name.
- Arguments are not decoded. `--arg-decode` (11.2) does not apply: an argument
  is the byte string the invoking shell produced.

### 4.3 Execution model

- The program is executed with `<command>` as `argv[0]` and the following tokens
  as the remaining argv entries, **each preserved as one argument**. No shell is
  created, and no shell metacharacter is interpreted by Lask. Operators the user
  types (`&&`, `|`, `>`) are processed by the invoking shell before Lask is
  reached, which is the desired behaviour.
- This differs from a command execution expression (6.6), which passes one
  string to a shell inside the environment. `lask cmd` cannot lose or re-split
  an argument containing whitespace, and is therefore the safe path for ad-hoc
  arguments: `lask cmd go test -run 'Test A'` passes `Test A` as one argument.
- Working directory and environment variable rules are those of Chapter 10
  (10.5, 10.6) unchanged.
- Image materialization follows 10.3 unchanged: `cmd` **must not** pull or
  build. An absent image is `E-IO-IMAGE-MISSING`, and the diagnostic must name
  `lask env build`.
- `cmd` requires a lock file covering every declared dependency, on the same
  terms as `run` / `eval` / `envs` (Chapter 5).

### 4.4 Interactive attachment

`cmd` is the one subcommand that attaches a process to the user's terminal.

- When Lask's stdin, stdout and stderr are all terminals, they are attached to
  the process, with a TTY allocated in the `docker` profile. `terraform apply`
  prompts, pagers, and progress rendering therefore behave as they do when the
  program is run directly.
- When they are not, they are forwarded as streams: stdin to EOF, stdout and
  stderr relayed as the command execution log (§4.5).
- `INT` and `TERM` are forwarded to the process, and the container is removed on
  exit including on signal.
- This is an exception to Chapter 9, whose model — stdin consumed to EOF and
  bound as the `stdin` value — applies to function invocation. `cmd` evaluates
  no function, so nothing is bound. Command execution expressions, including
  those entered in the REPL, remain non-interactive.

### 4.5 Output, logging, and exit code

A `cmd` invocation is an ordinary command execution as far as Chapter 12 is
concerned, except where relaying would destroy interactivity.

- The **start line** (`$`) and the **exit line** are always written to stderr in
  the text format of 12.3, carrying the timestamp, the environment summary and
  the execution number:

  ```text
  2026-09-12T12:56:40.217Z [#local:1] $ terraform apply
  2026-09-12T12:57:03.914Z [#local:1] exit 0
  ```

- The **relay lines** (`1|`, `2|`) are written when the streams are not
  terminals. When they are, the program's stdout and stderr are connected
  directly and no relay line is produced: interactivity wins over the record,
  because a prefixed, line-buffered relay cannot carry a prompt, a pager or a
  progress display.
- `--format json` forces relay mode even on a terminal, and emits the JSON Lines
  of 12.3. Asking for the machine-readable form is asking for the record.
- Execution numbering is per invocation, so a `cmd` invocation is always `:1`.
- 11.3's rule that `run` must not write to stdout does not apply: `cmd` produces
  no evaluation result, and stdout belongs to the program.
- The program's exit code becomes Lask's exit code verbatim, consistent with
  11.3 ("command failure passes through that command's exit code") and subject
  to the overlap caveat already stated there.
- Lask's own failures before the program starts use the existing
  classification: `1` for a static error (unknown command, stale lock), `3` for
  an external I/O error (`E-IO-IMAGE-MISSING`, daemon unreachable), `4` for a
  usage error.

### 4.6 Name resolution

- `<command>` is resolved against the command words of the target module
  (§1), by exact text. The `-` to `_` mapping of 11.2 is **not** applied: this
  is a program name, not a function name, so `lask cmd docker-compose up`
  resolves `docker-compose`.
- A name that is not a command word is `E-CLI-USAGE`, with a diagnostic naming
  `lask cmd --list`.
- No `--env` option is provided. 10.4 states that the in-language environment
  expression is the only means of specifying an execution environment, and
  `cmd` does not become an exception: it runs a declared command in *that
  command's* declared environment, or it fails.

### 4.7 Listing

`lask cmd --list` reports every command word the target module declares: the
name, the resolved environment, and whether the image is present on the target
daemon. `--format json` produces the structured form. It
performs no network access and no build, on the same terms as `envs` (11.4).

```text
$ lask cmd --list
go          docker  golang:1.25                      ok
npm         docker  node:20.20.2-alpine3.23          ok
npx         docker  node:20.20.2-alpine3.23          ok
terraform   docker  hashicorp/terraform:1.16.2       missing (lask env build)
```

Shell completion of command names after `lask cmd` belongs to the completion
work in #10.

### 4.8 What `lask cmd` is not

- Not a shell: it interprets no operators and performs no expansion.
- Not a task invocation: no argument decoding, no keyword binding, no return
  value, no `--stdout-encode`.
- Not a way to run an undeclared program, and not a way to choose an image from
  the command line. Both would reintroduce exactly the host dependence §2
  removes.

### 4.9 The REPL

No REPL-specific form is provided. A command execution expression entered at the
prompt is elaborated against the target module's command words like any other,
so dispatch resolves it:

```text
lask> $ go test ./...
```

This requires only that the REPL's evaluation context carry the loaded module's
command table. Such an expression is an ordinary command execution: it relays
through 12.3 and is not interactive (§4.4). The interactive path is `lask cmd`.

### 4.10 Why this argues for the declaration form

`lask cmd` needs only the name-to-environment registration of §1, not the
notation that reaches it from a command execution expression. The same pinned
environment is therefore reachable from a task, from a command string, from the
REPL, and from the operator's shell, with one place to change the version.

## 5. Specification Deltas

| section | change |
|---|---|
| 3.3 | add `command` to the reserved words |
| 5 | add `CommandDecl` to `TopLevelDecl`; §1 |
| 6.6 | a command execution expression determines its environment from the expression or by dispatch; none is `E-TYPE-COMMAND-NOENV`; the desugaring passes the environment positionally |
| 7.5 | `run_command` typed `Function<String, Environment, CommandResult>` |
| 7.7 / 14.2 | add `E-TYPE-COMMAND-DECL`, `E-TYPE-COMMAND-NAME`, `E-TYPE-COMMAND-DUPLICATE`, `E-TYPE-COMMAND-NOENV` |
| 8.7 | `run_command` receives the environment positionally |
| 9 | note the `cmd` exception to the stdin model (§4.4) |
| 10.1 | replaced as in §2 |
| 10.3 | `cmd` added to the subcommands that must not pull or build |
| 10.9 | new: the dispatch procedure (`command-dispatch.md` §2) |
| 11.1 | add the `cmd` subcommand |
| 11.3 | `cmd` exit code and stream contract (§4.5) |
| 11.6 | `lask cmd --help`; no interception after the command name |
| 12.3 | `cmd` start and exit lines always; relay lines except on a terminal |

## 6. Compatibility

Per `compatibility.md` §2, taken together the changes are `source-breaking`,
`behavior-breaking` and `tooling-breaking`, and are made under the
effective-date clause of §1.

- Source: `$ cmd` requires an environment (18 occurrences; §2), `run_command`
  gains a positional parameter, `command` becomes reserved (no occurrence as an
  identifier).
- Behaviour: no existing program changes result silently. Every affected
  construct becomes a static error, which is the property being bought.
- Tooling: `envs` output, a new subcommand, and new error codes.

## 7. Decisions Taken

Recorded because each was open during the design and each shapes a rule above.

1. **Command names are string literals, not identifiers.** An identifier form
   (`command go on ...`, with an `as` clause for `docker-compose`) was drafted
   first. Strings make every program name expressible without a second form, and
   they remove the question of what an identifier would bind. Nothing is bound.
2. **`on`, not `=`.** Every other declaration reads "this name denotes this
   value". A command declaration denotes nothing; it registers a fact. `=` would
   misstate it.
3. **`run_command` takes the environment positionally.** 6.1 requires a keyword
   parameter to have a default, so a required environment cannot be a keyword
   parameter without extending the language. A positional parameter stays inside
   the existing model; required keyword parameters are a separate question with
   uses beyond this one.
4. **Declarations are module-local.** A shared table is a backward-compatible
   addition (§1) and is deferred until a project needs it.
5. **Interactivity wins over the relay.** §4.5. The start and exit lines are
   never dropped, so every `cmd` invocation is still recorded with its
   environment and exit status.
6. **No REPL-specific form.** §4.9.
