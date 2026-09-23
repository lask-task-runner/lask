# Examples

Two ways in, depending on what you came for.

**[01-projects/](01-projects)** — whole projects, where the language is
in service of a job. Start here to see what a real `main.lask` looks
like.

**[02-language/](02-language)** — a tour of the language itself: one
small, runnable module per topic, commented as you read it. Start here
to find out how something is written.

Everything here is checked the same way:

```bash
cd example/<directory>
lask check          # nothing runs: names, arguments and types only
lask run --help     # every task in the module, from its own comments
```

## 01-projects

| Directory | What it does | Needs |
| --- | --- | --- |
| [01-hello-world/](01-projects/01-hello-world) | The smallest thing that is still a project: a pure task, a containerized one, and one calling the other. | Docker |
| [02-webapp-on-aws/](01-projects/02-webapp-on-aws) | A full AWS stack — Python Lambda API, React front end, RDS, Cognito, CloudFront, Playwright tests — from a machine with none of those tools installed. | Docker, AWS account |

## 02-language

One directory per topic, in reading order. The whole of the
[Quick Reference](../doc/quick-reference.md) is covered.

| Topic | Covers |
| --- | --- |
| [01-values-and-types](02-language/01-values-and-types) | Bindings, annotations, type aliases, records, maps, unions, optional fields, generics, `cast` |
| [02-functions](02-language/02-functions) | Positional, variadic and keyword parameters, lambdas, higher-order functions, `\|>` and `>>` |
| [03-control-flow](02-language/03-control-flow) | `do`, `if`, guard `return`, all three forms of `case`, `for` |
| [04-commands](02-language/04-commands) | `$`, `$1`, `$2`, `$*`, interpolation, dispatch, `run_command`, `shell_quote` |
| [05-environments](02-language/05-environments) | `#local`, tags, digests, `#docker(...)`, a Dockerfile recipe, environments as values |
| [06-concurrency](02-language/06-concurrency) | `async` / `await`, `spawn`, `all`, `race` |
| [07-errors](02-language/07-errors) | `try` / `catch` / `finally`, `fail`, `error`, `recover`, exit codes |
| [08-modules](02-language/08-modules) | Named and namespace imports, `internal`, re-export, per-module command declarations |
| [09-dependencies](02-language/09-dependencies) | `lask.json`, the committed lock file, importing a shared module by name, `lask deps` |
| [10-standard-library](02-language/10-standard-library) | Numbers, strings, regular expressions, arrays, maps |
| [11-data](02-language/11-data) | JSON, YAML, TOML, CSV, dotenv, `cast`, hashes, base64 |
| [12-io-and-secrets](02-language/12-io-and-secrets) | `stdin`, the stdout contract, `get_env` and friends, `!!`, `log` |
| [13-files-and-paths](02-language/13-files-and-paths) | `read_file` / `write_file` / `glob` and the environment each one names |
| [14-docs-and-cli](02-language/14-docs-and-cli) | Doc comments, `@param`, `@complete`, `@hidden`, and how a signature becomes a command line |

The rules behind all of it are in the
[specification](../doc/spec.md).
