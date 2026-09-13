# Specification: Command Dispatch

Status: normative draft
Affects: spec.md 6.6, 7.6, 7.7, 8.8, 11.4, 14.2
Depends on: `command-declarations.md` (the `command` declaration, the explicit
environment rule, `run_command`, `lask cmd`)

This document specifies how a command execution expression reaches its
environment: with no environment specification, it takes it from the programs
the command string invokes. The declaration it refers to, and the rule that
every command execution expression must determine an environment, are specified
in `command-declarations.md`.

## 1. Environment Resolution

The grammar of `CommandExpr` (6.6) is unchanged. The rule that supplies the
environment is:

1. If an environment specification `[ Expression ]` is present, that expression
   is the environment. **No dispatch is performed**, whatever the command string
   contains.
2. Otherwise the command string is dispatched (§2). If dispatch selects an
   environment, that is the environment.
3. Otherwise it is a static error (`E-TYPE-COMMAND-NOENV`,
   `command-declarations.md` §2).

Rules:

- Dispatch applies only to the command execution expression (6.6). The core
  function `run_command` receives its command as a runtime `String` and takes
  its environment as a required argument (`command-declarations.md` §3), so it
  is never dispatched.
- Dispatch is performed during static expansion (7.6), before normalization of
  `CommandExpr` to `run_command`. The stream specifiers `1`, `2` and `*` do not
  affect it.
- The result is syntactic sugar: a dispatched `$ cmd` expands to `$[e] cmd` for
  the selected `e`. Typing, evaluation order and failure semantics are
  unaffected.
- Whitespace after `$` is not significant. The command string begins immediately
  after `$` — after the stream selector, or after the closing `]` — and its
  leading whitespace is trimmed (6.6), so `$go test` and `$ go test` are the
  same expression and dispatch treats them identically. **No form attaches an
  identifier to `$`**: the only things that may follow `$` without whitespace
  are a stream selector and an environment specification, as today. The
  environment is never named by a token whose meaning depends on whether a space
  precedes it.

## 2. Dispatch

Dispatch determines the environment of a command string from the programs the
string invokes. It is a lexical procedure over the command string as written,
not an interpretation of shell syntax: no dialect-dependent construct is given
meaning beyond the structure enumerated below.

**Determinacy principle.** Dispatch decides only where the command words are
determined by the text alone, and selects nothing everywhere else. It may fail
to determine an environment for a command string whose programs are in fact
obvious to a reader; it must never determine one from a construct it cannot
read. Selecting nothing is `E-TYPE-COMMAND-NOENV` (§1.3) — a static error
naming the expression — so the cost of the analysis being incomplete is an
explicit environment specification, never a command running somewhere the author
did not intend. Every rule below is written to that asymmetry, and §6.5 explains
why the analysis is deliberately not made more capable.

### 2.1 Input

The procedure operates on the command string **before interpolation**, after the
lexical normalization of 6.6 (leading and trailing whitespace removed,
continuation lines joined). Each interpolation `#{...}` is treated as a single
opaque character belonging to no other class.

### 2.2 Regions and Segmentation

Three kinds of structure are recognised, and nothing else is.

**Quoted regions.** A `'` opens a region ending at the next `'`; a `"` opens a
region ending at the next unescaped `"`. A character preceded by `\` is
literal. An interpolation hole is one opaque character. Nothing inside a quoted
region is a separator or opens a region.

**Nested regions.** A command substitution `$(` … matching `)`, a backquoted
substitution `` ` `` … next `` ` ``, and a grouping `(` … matching `)` each open
a nested region. The content of a nested region is segmented by these same rules
and contributes its own command words. In the enclosing text the region occupies
the position of part of one word: `npm ci --prefix $(pwd)/web` has the words
`npm`, `ci`, `--prefix`, `$(pwd)/web`, and contributes the command words `npm`
and `pwd`.

**Separators.** Outside quoted and nested regions, `&&`, `||`, `|`, `;`, `&` and
a newline are separators, matched longest-first. The text between two separators,
before the first, or after the last, is a **segment**.

If a quoted region or a nested region is not closed before the end of the
string, the command string is not analysable: dispatch selects nothing, and the
diagnostic for `E-TYPE-COMMAND-NOENV` must say that the string could not be
segmented rather than that no command was declared.

### 2.3 The Command Word of a Segment

Within a segment:

1. Leading whitespace is skipped.
2. Zero or more **assignment words** are skipped. An assignment word is a word
   whose text before the first `=` is non-empty and matches
   `[A-Za-z_] [A-Za-z0-9_]*`.
3. The next word, if any, is the segment's **command word**. A segment with no
   such word contributes nothing.

A **word** is a maximal run of characters that contains no whitespace lying
outside a quoted or nested region. Whitespace inside `'...'`, `"..."`, `$(...)`
or `` `...` `` does not divide a word, and an interpolation hole never divides
one. `MSG="hello world" npm ci` therefore has the words `MSG="hello world"`, `npm`, `ci`, and its
command word is `npm`; splitting on raw whitespace would instead take `world"`
as the command word and select nothing.

### 2.4 Candidates

A command word is a **candidate** if and only if all of the following hold:

- it contains no quotation character, no `\`, no interpolation hole, and no
  nested region;
- it contains no `/` (a path such as `/usr/bin/npm` is never a candidate; the
  match is on the word as written, never on a basename).

No further restriction is placed on its text. A command word is declared as a
string literal (the shared specification §1), so candidacy does not need to
anticipate what a program may be called; matching is exact against the declared
names, and a word matching none of them is neutral.

The third condition is what keeps a command substitution from inventing a
command position after itself: in `` `which go` test ``, the segment's first
word is `` `which go` ``, which contains a nested region and is therefore not a
candidate, so `test` is read as its argument and not as a program.

A candidate matches when its text is identical to a command word in scope
(`command-declarations.md` §1). Matching is exact; no normalization is applied.

### 2.5 Selection

Let `E` be the sequence of `Environment` values of the matched candidates, in
order of occurrence.

- If `E` is empty, dispatch selects nothing, and the expression is
  `E-TYPE-COMMAND-NOENV` (§1.3).
- If every element of `E` is structurally equal (8.8) to the first, dispatch
  selects that value.
- Otherwise it is a static error (`E-TYPE-COMMAND-CONFLICT`). The diagnostic
  must name at least two of the conflicting command words and their
  environments.

Selection is unanimity, not majority: the number of times an environment is
named plays no part. `ls dist && go test ./... && go build ./...` selects
`[#local, golang, golang]` and is a conflict, exactly as
`ls dist && go test ./...` is. Order plays no part either — the first element is
the point of comparison only because equality is transitive.

Two distinct declarations denoting the same environment do not conflict:
selection compares environment values, not declarations.

`#local` is an environment like any other here. `command "mv", "ls" on #local` is
well-formed and is the way a project states which programs it runs on the host:
`$ ls dist` then selects `#local` and runs, where an undeclared `ls` would be
`E-TYPE-COMMAND-NOENV`. It participates in selection on equal terms, so a line
that invokes both a host-declared and a container-declared program —
`ls dist && go test ./...` — is `E-TYPE-COMMAND-CONFLICT`, and the author writes
the environment explicitly. This is the intended outcome: a command string is
one process in one environment, so a line claiming both has to say which it
means.

Undeclared command words remain neutral: they are not candidates, and they
neither select nor prevent selection. Whether to declare a program is therefore
a real choice. Declaring it makes a line that invokes it alone work bare, and
makes a line that mixes it with another environment a static error; leaving it
undeclared keeps it neutral everywhere, which is what suits a prefix such as
`cd` in `cd web && npm ci`.

Adding a `#local` declaration can only ever turn a working line into a static
error, never relocate one silently: `#local` is what a line without the
declaration would have had to state anyway.

### 2.6 Undeterminable Command Words

A command word containing an interpolation hole cannot be resolved before
execution. It is not a candidate (§2.4) and contributes nothing to selection; if
no other candidate matches, the expression is `E-TYPE-COMMAND-NOENV`, and the
diagnostic must state that the command word is not statically determinable and
that an explicit environment specification is required.

Dispatch is never attempted at run time on the interpolated string. An
environment not determined statically is not enumerable (11.4) and not pinnable
(10.3), which is the property this restriction preserves.

### 2.7 Worked Examples

Given `command "go" on #golang:1.25`, `command "node", "npm", "npx" on #node:20.20.2-alpine3.23`,
`command "python", "pip" on #python:3.12.14-alpine3.24`, and
`command "terraform" on #docker("hashicorp/terraform:1.16.2")`:

| command string | command words | selection |
|---|---|---|
| `go test -v ./...` | `go` | `#golang:1.25` |
| `cd web && npm ci && npm test` | `cd`, `npm`, `npm` | node |
| `pip install -r req.txt && python -m unittest` | `pip`, `python` | python |
| `FOO=1 PATH=/x npm ci` | `npm` | node |
| `MSG="hello world" npm ci` | `npm` | node |
| `VITE_API_URL="#{u}" npm run build` | `npm` | node |
| `ls dist && npm publish` | `ls`, `npm` | node (`ls` runs there too) |
| `ls dist && npm publish`, `ls` declared `#local` | `ls`, `npm` | `E-TYPE-COMMAND-CONFLICT` |
| `ls && go test ./... && go build ./...`, `ls` declared `#local` | `ls`, `go`, `go` | `E-TYPE-COMMAND-CONFLICT` |
| `echo "npm ci"` | `echo` | `E-TYPE-COMMAND-NOENV` |
| `which npm` | `which` | `E-TYPE-COMMAND-NOENV` |
| `/usr/bin/npm ci` | `/usr/bin/npm` (not a candidate) | `E-TYPE-COMMAND-NOENV` |
| `docker build -t x .` | `docker` | `E-TYPE-COMMAND-NOENV` |
| `mv ./lask #{output}/lask` | `mv` | `E-TYPE-COMMAND-NOENV`; `#local` if `mv` is declared |
| `#{bin} -chdir=infra init` | (undeterminable) | `E-TYPE-COMMAND-NOENV` |
| `npm ci --prefix $(pwd)/web` | `npm`, `pwd` | node |
| `` `which go` test `` | (first word contains a nested region) | `E-TYPE-COMMAND-NOENV` |
| `sudo go build` | `sudo` | `E-TYPE-COMMAND-NOENV` |
| `env FOO=1 go test` | `env` | `E-TYPE-COMMAND-NOENV` |
| `for f in *.go; do go build $f; done` | `for`, `do`, `done` | `E-TYPE-COMMAND-NOENV` |
| `echo "unterminated` | (not analysable) | `E-TYPE-COMMAND-NOENV` |
| `terraform init && aws s3 sync . s3://b` | `terraform`, `aws` | `E-TYPE-COMMAND-CONFLICT` |

The `E-TYPE-COMMAND-NOENV` rows are repaired either by writing `$[#local] ...`
or `$[e] ...`, or by declaring the program (§2.5): with
`command "ls", "mv", "echo", "which" on #local` the `echo`, `which` and `mv` rows select
`#local` and run. Under the previous draft of this document they ran on the host
silently; that is the change the explicit environment rule makes.

## 3. Errors

Added to 14.2:

- `E-TYPE-COMMAND-CONFLICT`: one command string selects two structurally
  different environments (§2.5).

`E-TYPE-COMMAND-NOENV` and `E-TYPE-COMMAND-DECL` are specified in
`command-declarations.md`. `E-NAME-DUPLICATE` and `E-TYPE-COMMAND-ENV` are
existing codes, used unchanged.

## 4. Enumeration and Editor Support

### 4.1 Enumeration

11.4 is amended as follows.

- Environments introduced by dispatch are enumerated with the environments of
  explicit specifications. They are statically determined (§2.4), so the
  enumeration remains exact and no new over-approximation is introduced.
- For each enumerated environment the report must state how it was selected: the
  command word for a dispatched environment, nothing for an explicit one.

```text
$ lask envs test_web --check
docker  node:20.20.2-alpine3.23  (command npm)  ok
```

### 4.2 Semantic Highlighting of Command Words

Under this design a command string is no longer uniformly opaque: some of the
words in it are references to declarations, and which ones they are decides
where the line runs. An editor must show that, and this is the mitigation §6.1
depends on.

- A matched candidate (§2.4) is reported through `textDocument/semanticTokens`
  with the token type `function`, inside the span of the command string. It is a
  reference to a `command` declaration, so `function` is both truer than
  `keyword` and more useful: it makes the word carry hover, go-to-definition and
  find-references, and every theme renders it distinctly from the surrounding
  string.
- Unmatched command words and the rest of the command string keep the `string`
  token type. Marking them would be noise: they are neutral by design.
- The absence of any highlighted word in a bracket-less command string is
  therefore the visible form of `E-TYPE-COMMAND-NOENV`. A line where nothing
  lights up has no environment, and the diagnostic will say so.
- A command string carrying an explicit environment specification has no
  highlighted words at all: dispatch was not performed, so no word in it is a
  reference to anything. What lights up is exactly what decided.
- Highlighting is computed from the same command table and the same procedure as
  dispatch. An implementation must not maintain a second, looser matcher for the
  editor: a word that lights up is a word that voted.

### 4.3 Inlay Hints

The resolved environment is displayed at the `$` of every command execution
expression as an inlay hint (`textDocument/inlayHint`), reading the image
reference for a `docker` environment and `local` for `#local`, with the command
word that selected it. The hint is what makes a diff reviewable: the environment
that dispatch derived is not otherwise present in the text.

## 5. Migration of the Examples

```lask
// example/02-docker/main.lask
command "go" on #golang:1.25
command "docker" on #local

test()  = $ go test -v ./...
build() = $ docker build -t #{image}:#{version} .

// example/04-webapp/main.lask
python     = #python:3.12.14-alpine3.24
node       = #node:20.20.2-alpine3.23
curl       = #curlimages/curl:8.21.0
playwright = #mcr.microsoft.com/playwright:v1.62.1-jammy

command "python", "pip" on python
command "node", "npm", "npx" on node
command "curl" on curl

test_api() = $ pip install -q --no-cache-dir -r api/requirements.txt && python -m unittest discover -s api -p "test_*.py"
test_web() = $ cd web && npm ci --no-audit --no-fund && npm test
test_e2e() = $[playwright] cd e2e && npm ci && npx playwright test   // required: §6.2
```

Six of `example/04-webapp`'s seven command execution expressions need no
environment specification: `cd` and the assignment words of `build_web` are
skipped or neutral, and the remaining command words agree. The seventh is
`test_e2e`, which is §6.2.

## 6. Residual Hazards

These are properties of the design, not of the rules above; no refinement of §2
removes them.

### 6.1 A declaration changes the meaning of lines that do not mention it

Adding `command "npm" on ...` changes where every existing `$ ... npm ...` line in
the module runs, including lines whose author did not intend a container.
Reading a command execution expression requires knowing the command table; the
expression alone does not determine where it runs.

The explicit environment rule bounds the damage in one direction: **removing** a
declaration no longer falls back to the host silently — it becomes
`E-TYPE-COMMAND-NOENV` at every line that depended on it, and lines that are
explicitly `$[#local]` cannot be affected by a declaration at all. What remains
is the other direction: adding a declaration, or adding a command word to an
existing declaration, silently moves matching lines into a container.

This is the design's defining property, not a defect in its rules: dispatch
exists precisely so that a line need not name its environment.

### 6.2 An image chosen for a reason other than the program

`example/04-webapp`'s `test_e2e` runs `npm ci && npx playwright test` in the
Playwright image, chosen for the browsers it carries. Dispatch selects the plain
Node image, which is confidently wrong, and the failure appears at run time as a
missing browser. An explicit environment specification is the only repair, and
nothing in the rules signals that it is needed.

### 6.3 Quoted text that names a program

`$ sh -c "cd web && npm ci"` places `&&` and `npm` inside a quoted region, so
dispatch sees only `sh` and the expression is `E-TYPE-COMMAND-NOENV` — loud, but
it means the accuracy of dispatch stops exactly at the depth of §2.2 and the
author must know where that is.

### 6.4 Asymmetry with `run_command`

`$ npm ci` and `run_command("npm ci", e)` do not correspond: the former derives
its environment, the latter states it. 6.6 presents the command execution
expression as sugar for the core function, and under this design the equivalence
holds only after dispatch has been applied.

### 6.5 The analysis is deliberately incomplete

§2 requires the implementation to segment and tokenize the command string, which
6.6 otherwise declines to interpret ("shell dialect differences are execution-
environment dependent; the specification prescribes only the evaluation procedure
of the command string"). The boundary that kept shell syntax out of the
specification is crossed, and the obvious next question is whether to cross it
properly: parse the shell, and extract the commands exactly.

The answer is no, for reasons that are worth stating because they will be asked
again.

**Parsing is not the hard part.** The POSIX shell grammar is specified
(POSIX.1 XCU §2.10) and production-quality parsers exist — `mvdan/sh`,
ShellCheck's (Haskell, but GPL-3 and so unavailable to an MIT project),
`morbig`, `tree-sitter-bash`. A parser is buildable.

**A parse does not yield the programs that run.** It yields the syntactic
command words. `sudo go build`, `env FOO=1 go test`, `timeout 10 go test`,
`xargs go test` and `nohup go run .` all have a wrapper in command position and
the real program in an argument, and recovering it requires the semantics of
each wrapper, each with its own flag syntax — a table that is always incomplete.
`$CMD test`, `eval "$x"`, `` `which go` test ``, and a shell function named `go`
are not recoverable at all. Parsing improves the reading of the text; it does
not improve the reading of the program.

**The object is a template, not a script.** `#{...}` holes mean the string is
not known at check time, and a hole may contain quotes or separators, so it can
change the token stream of any parse around it. Exact analysis of a template is
unsound in principle, which is why §2.1 treats a hole as one opaque character —
not an approximation chosen for simplicity, but the most that can be claimed.

**There is no single dialect to parse.** The string is executed by whichever
shell the image provides: busybox `ash` on Alpine, `dash` as `/bin/sh` on
Debian, `bash` elsewhere. A parser must choose, the specification would have to
name that grammar normatively, and every future report becomes "Lask's parser
disagrees with the image's shell". 6.6's position would be reversed: Lask would
be prescribing shell semantics.

**The remaining gain is small and the loss is large.** Over §2, a full parser
would correctly read `sh -c "cd web && npm ci"` (§6.3) and some exotic quoting,
and nothing else of consequence. Against that, an analysis that looks exact
invites the belief that it is, and the cases it still cannot see —
`sudo go build` above all — would be mis-analysed silently instead of stopping.
The present rules fail in the direction of `E-TYPE-COMMAND-NOENV`, which is a
static error the author repairs with an explicit environment. An approximation
known to be an approximation, whose failures are loud, is the safer artifact.

It is also worth noting that no comparable tool — Earthly, Dagger, Bazel, Nix,
Make — derives an execution environment from a command string at all. The reason
is not that the parsing is hard; it is that which program runs is not a property
of the text.

## 7. Alternatives Considered

Recorded because each was worked through before this design was settled on, and
each failed for a reason worth keeping.

### 7.1 An explicit reference attached to `$` (`$go test`)

```lask
command go = #golang:1.25
test() = $go test -v ./...      // sugar for $[go] go test -v ./...
```

The environment is named at the use site, so no declaration elsewhere can change
what a line does, and no implementation reads the command string. It was the
leading candidate for most of the design discussion (its full specification is
preserved in the discussion of #16).

Rejected for two reasons.

- `$go test` and `$ go test` would differ by one space and by
  host-versus-container. Even with the explicit environment rule turning the
  spaced form into a static error rather than a silent host execution, the two
  spellings reading as different things is the objection: a notation should not
  put that much weight on whitespace.
- The form prepends the command word, so nothing can precede the program on the
  line: `ENV=1 go test`, and wrappers such as `env`, `sudo`, `time`, `nice`,
  `xargs`, `timeout`, all fall back to the bracketed form. In
  `example/04-webapp`, two of the seven command execution expressions carry
  environment assignments — `build_web` carries five — so the short form covered
  less than the premise suggested. §2.3 of this design skips assignment words
  and covers them.

### 7.2 Scoped default environment (`use E in do { ... }`)

Rejected on the evidence: almost every task function in the examples contains
exactly one command execution expression, so a scope form saves nothing in the
common case and is longer than `$[node] ...` for a one-liner. It also requires a
reader to scan outward to learn where a line runs, without the compensating
brevity dispatch provides.

### 7.3 Commands as values, applied CLI-style

```lask
go = create_command(#golang:1.25, "go")
main() = do { go test }
```

The value half is viable and is discussed in #16; the application half is not.
Barewords and expressions have opposite defaults (`go test` needs `test` to be
the string, `f 1 2` needs `1` to be an expression, and `main.lask` already
declares a function named `test`); shell tokens are not Lask tokens (`-v` is
binary minus, `./...` does not lex, `&&` is the Bool operator); arity is
ambiguous under variadics; and 6.1 forbids keyword arguments through function
values, so the CLI symmetry that motivated it is not reachable.

### 7.4 Strings by default in argument position, `#{}` for expressions

Fixes 7.3's bareword conflict by choosing a side and providing an escape, and it
is coherent with two rules Lask already has (the command string of 6.6, and the
argument decoding of 11.2). Rejected because it makes Lask a two-mode language
permanently: either command mode replaces parenthesised calls, which is a
different language, or the two coexist, which is PowerShell's documented
parsing-mode hazard.

### 7.5 Mode switch triggered by a declared name

```lask
command go = #golang:1.25
main() = do { go test ./... }
```

Sound — the mode is decided by one leading token that is explicitly written and
explicitly declared, so nothing is reinterpreted silently. Rejected on
implementation cost: shell-word lexing must begin before the parser knows which
names are commands, so lexical analysis becomes dependent on name resolution,
and across `import` it becomes dependent on the module graph. This is C's
typedef lexer hack and Ruby's method-versus-regex ambiguity, whose cost lands
permanently on incremental reparse, error recovery and the LSP — against
requirement 3.4. Keeping the command string behind `$` keeps lexical analysis
independent of every name table.

### 7.6 Prohibiting `#local` in a command declaration

An earlier draft forbade `command "mv", "ls" on #local`, reasoning from
`cd web && npm ci` — `#local` from `cd`, the Node image from `npm`, a conflict
on an entirely ordinary line — that host programs should not be declared at all.

That conflated the declaration with the selection rule. The prohibition required
a special case in the declaration rules for no reason, and it left the
host-execution ergonomics of the explicit environment rule unaddressed. The
declaration is now well-formed (§2.5), while selection still treats `#local` on
equal terms, so the conflict the earlier draft worried about is preserved — as a
static error on the line that mixes environments, which is where it belongs. The
`cd` case is answered by leaving `cd` undeclared, which is what a prefix wants.

### 7.7 `#local` yielding to other environments

A variant in which `#local` selects the host only when no other environment is
selected, so that `command "cd" on #local` and `cd web && npm ci` would run in the
Node image rather than conflicting. Rejected: it would silently pull a program
that genuinely requires the host — the `docker` CLI needing the host daemon,
`systemctl`, `pbcopy` — into a container whenever the line also invoked a
containerized program. Uniform equality keeps every mixed line an error the
author resolves, at the cost of requiring `cd` to stay undeclared.
