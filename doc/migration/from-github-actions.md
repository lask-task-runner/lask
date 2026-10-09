# Migrating from GitHub Actions

This guide moves the work in your GitHub Actions workflows into a `main.lask`, so that a pipeline runs the same way on your laptop as on a runner, and a workflow file shrinks to the part only GitHub can do. It assumes you know GitHub Actions and have skimmed the [Quick Reference](../quick-reference.md). If you are coming from a Makefile as well, start with [Migrating from Make](from-make.md): this guide builds on its example.

## Lask does not replace GitHub Actions

Lask is a task runner, not a CI platform. GitHub Actions keeps everything that is about GitHub — when a workflow runs, with which permissions, on which runner, with which secrets, and what happens to its results. Lask takes over what the steps *do*:

| Stays in the workflow | Moves to Lask |
| --- | --- |
| `on:` triggers, `concurrency:`, `permissions:` | the contents of `run:` steps |
| `runs-on:` and `timeout-minutes:` | `actions/setup-go`, `setup-node`, `setup-python` and other tool installs |
| `environment:` and its required reviewers | `needs:` between jobs that only exist to order work |
| `secrets.*`, OIDC, `docker/login-action` | `services:` for test databases |
| `actions/checkout` | `strategy.matrix` over tool versions (optionally) |
| `actions/cache`, `upload-artifact` | retries, timeouts and `continue-on-error` |
| comments, statuses, `$GITHUB_STEP_SUMMARY` | `if:` conditions on inputs |

What you get for it:

- **Run CI locally.** `lask run ci` on your laptop runs the same steps in the same images as the runner. No commit-push-wait loop to find out whether a change to the pipeline works.
- **Checks before anything runs.** `lask check` resolves every task, argument and type in the pipeline. A YAML workflow is only checked by running it.
- **Pinned tools.** `setup-go` with `go-version: "1.25"` installs whatever 1.25.x is current; `lask.lock.json` pins the image digest, and `lask sync --frozen` fails the build if it would change.
- **A portable pipeline.** Moving to GitLab CI, CircleCI or Jenkins means rewriting the few lines that install Lask and call `lask run`, not the pipeline.

## A complete example

The service from the Make guide, as two workflows: CI on every push, and a manual deploy.

### Before

`.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-go@v5
        with:
          go-version: "1.25"
      - uses: golangci/golangci-lint-action@v8
        with:
          version: v2.5.0

  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        go: ["1.24", "1.25"]
    services:
      postgres:
        image: postgres:17-alpine
        env:
          POSTGRES_PASSWORD: test
        ports: ["5432:5432"]
        options: >-
          --health-cmd pg_isready --health-interval 2s --health-retries 15
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-go@v5
        with:
          go-version: ${{ matrix.go }}
      - run: go test ./...
      - run: go test -tags integration ./...
        env:
          DATABASE_URL: postgres://postgres:test@localhost:5432/postgres?sslmode=disable

  build:
    needs: [lint, test]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: actions/setup-go@v5
        with:
          go-version: "1.25"
      - run: |
          CGO_ENABLED=0 go build -trimpath \
            -ldflags "-X main.version=$(git describe --tags --always)" \
            -o bin/hello ./cmd/hello
      - uses: actions/upload-artifact@v4
        with:
          name: hello
          path: bin/hello
```

`.github/workflows/deploy.yml`:

```yaml
name: Deploy

on:
  workflow_dispatch:
    inputs:
      env:
        type: choice
        options: [dev, staging, prod]
        default: dev

jobs:
  deploy:
    runs-on: ubuntu-latest
    environment: ${{ inputs.env }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: actions/setup-go@v5
        with:
          go-version: "1.25"
      - uses: docker/login-action@v3
        with:
          registry: registry.example.com
          username: ${{ secrets.REGISTRY_USER }}
          password: ${{ secrets.REGISTRY_TOKEN }}
      - run: |
          VERSION=$(git describe --tags --always)
          CGO_ENABLED=0 go build -trimpath -ldflags "-X main.version=${VERSION}" -o bin/hello ./cmd/hello
          docker build -t registry.example.com/hello:${VERSION} .
          docker push registry.example.com/hello:${VERSION}
          ./scripts/deploy.sh ${{ inputs.env }} ${VERSION}
```

### After: the tasks

Start from the `main.lask` of the [Make guide](from-make.md#a-complete-example). It already has `lint_go`, `build`, `image` and `deploy`; CI needs three changes.

**Tool versions as a value.** The matrix becomes a map from version to image, and `test` takes the version as an argument. The cache directory comes from `GO_CACHE` when it is set, so that CI can point it at a directory `actions/cache` saves:

```lask
// Go's caches: a named volume by default, a directory when GO_CACHE
// names one (CI caches it between runs).
go_cache = "#{get_env_or("GO_CACHE", "lask-go-cache")}:/root/.cache"

go_images: Map<Environment> = {
  "1.24": #golang:1.24-alpine,
  "1.25": #golang:1.25-alpine
}

go_on(version: String): Runnable =
  runnable(go_images[version], env = {"CGO_ENABLED": "0"}, volumes = [go_cache])

go = go_on("1.25")

command { "go" } on go

// Run the unit tests.
//
// @param go  Go version to test with.
// @complete go @keys go_images
test(--go = "1.25"): String = $[go_on(go)] go test ./...

// Run the unit tests on every Go version.
test_all(): Array<String> = all(map(keys(go_images), \(v) -> async test(go = v)))
```

**The service as part of the task.** The database starts with the tests and stops when they finish, so `lask run integration` works on a laptop exactly as on the runner:

```lask
// Postgres for the integration tests, published on the host, and the
// way back to it from another container.
db = #postgres:17-alpine{env: {"POSTGRES_PASSWORD": "test"}, publish: ["55432:5432"]}
db_host: Map<String | Null> = {"host.docker.internal": "host-gateway"}
db_url = "postgres://postgres:test@host.docker.internal:55432/postgres?sslmode=disable"

internal serve_db(): String = $[db] docker-entrypoint.sh postgres

internal db_ready(): CommandResult =
  $*[#postgres:17-alpine{add_hosts: db_host}] pg_isready -h host.docker.internal -p 55432

internal go_integration(): String =
  $[#golang:1.25-alpine{env: {"DATABASE_URL": db_url}, add_hosts: db_host, volumes: [go_cache]}] go test -tags integration ./...

// Run the integration tests against a fresh Postgres.
//
// The database runs only as long as the tests do: `race` stops it as
// soon as the tests finish, pass or fail.
integration(): String = race([async serve_db(), async do {
  until(backoff_fixed(1, 30), \(r: CommandResult) -> r.code == 0, \() -> db_ready())
  go_integration()
}])
```

**One entry point for CI.** What were three jobs and a `needs:` is one task:

```lask
// Lint and test everything in parallel, then build.
ci(): String = do {
  lint = async lint_go()
  tests = async test_all()
  it = async integration()
  await lint
  await tests
  await it
  build()
}
```

Before touching the workflow, run it where you are:

```bash
$ lask sync
$ lask run ci
```

### After: the workflows

`.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:

permissions:
  contents: read

env:
  LASK_VERSION: v0.7.0

jobs:
  ci:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0 # git describe needs the tags

      - name: Install Lask
        run: |
          curl -fsSL "https://github.com/lask-task-runner/lask/releases/download/${LASK_VERSION}/lask-${LASK_VERSION}-linux-amd64.tar.gz" \
            | sudo tar -xz -C /usr/local/bin lask

      - name: Pull the pinned images
        run: lask sync --frozen

      - name: Check
        run: lask check

      - uses: actions/cache@v4
        with:
          path: .cache/go
          key: go-${{ hashFiles('go.sum') }}
          restore-keys: go-

      - name: Lint, test and build
        run: lask run ci
        env:
          GO_CACHE: ${{ github.workspace }}/.cache/go

      - uses: actions/upload-artifact@v4
        with:
          name: hello
          path: bin/hello
```

`.github/workflows/deploy.yml`:

```yaml
name: Deploy

on:
  workflow_dispatch:
    inputs:
      env:
        type: choice
        options: [dev, staging, prod]
        default: dev

permissions:
  contents: read

env:
  LASK_VERSION: v0.7.0

jobs:
  deploy:
    runs-on: ubuntu-latest
    environment: ${{ inputs.env }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Install Lask
        run: |
          curl -fsSL "https://github.com/lask-task-runner/lask/releases/download/${LASK_VERSION}/lask-${LASK_VERSION}-linux-amd64.tar.gz" \
            | sudo tar -xz -C /usr/local/bin lask

      - run: lask sync --frozen

      - uses: docker/login-action@v3
        with:
          registry: registry.example.com
          username: ${{ secrets.REGISTRY_USER }}
          password: ${{ secrets.REGISTRY_TOKEN }}

      - name: Deploy
        run: lask run --confirm deploy --env "$DEPLOY_ENV"
        env:
          DEPLOY_ENV: ${{ inputs.env }}
```

What is left in YAML is what GitHub is for: when to run, the environment's reviewers, the registry credentials, the cache and the artifact. Everything between checkout and upload is `lask run`.

## Migrating step by step

### Step 1: Call Lask from one step

Pick the job you change most often and replace its `run:` steps with a task, keeping the job's other steps as they are. With the tools still installed by `setup-go`, you can start on the host:

```lask
command { "go" } on #local

test(): String = $ go test ./...
```

```yaml
      - uses: actions/setup-go@v5
        with:
          go-version: "1.25"
      - run: lask run test
```

The pipeline is now runnable locally, and `lask check` covers it.

### Step 2: Move the tools into images

Change the `command` line to an image, run `lask sync`, and commit `lask.lock.json`. Then delete the `setup-*` step and add `lask sync --frozen` in its place:

```diff
-command { "go" } on #local
+command { "go" } on #golang:1.25-alpine
```

```diff
-      - uses: actions/setup-go@v5
-        with:
-          go-version: "1.25"
+      - run: lask sync --frozen
       - run: lask run test
```

`--frozen` makes the runner fail rather than pin something new: the images CI uses are the ones a reviewer saw in the lock file.

### Step 3: Fold jobs into tasks

Jobs that exist only to put work in an order — `needs: [lint, test]` followed by a build — become one task that calls the others. Jobs that exist for GitHub's sake stay separate: a different runner, a different `environment:`, different permissions, or a manual trigger.

### Step 4: Move services and matrices if you want them local

`services:` and `strategy.matrix` work as they are, around a `lask run` step. Move them into Lask when you want to run them locally too, as the example does. See [the sections below](#services) for the trade-offs.

## Reference: GitHub Actions to Lask

| GitHub Actions | Lask |
| --- | --- |
| `run:` with several commands | a task, one `$` per line |
| `actions/setup-go` etc. | `command { "go" } on #golang:1.25-alpine` |
| `needs:` (ordering only) | calls in the task body |
| jobs that run in parallel | `async` / `await`, `all([...])` |
| `strategy.matrix` | a `Map<Environment>` and a parameter, or the matrix kept in YAML |
| `services:` | the service started by the task, stopped by `race` |
| `env:` on a step | `get_env` / `get_env_or`, then `env:` on the environment |
| `${{ secrets.X }}` | `env:` on the step, read with `get_env` into a `!!` binding |
| `workflow_dispatch.inputs` | task parameters, passed through `env:` |
| `if: inputs.dry_run` | an `if` in the task |
| `continue-on-error: true` | `$*`, or `try` / `catch` |
| retry actions | `retry`, `retry_if`, `until` |
| `timeout-minutes:` on a step | `timeout(seconds, ...)`, with `timeout-minutes:` kept as a backstop |
| `$GITHUB_OUTPUT` | `lask eval --stdout-encode text <task>` |
| `environment:` approval | stays; add a `confirm` entry in `lask.json` for local runs |

## Differences worth knowing

### Install Lask by version

The install step downloads a fixed release rather than asking the API for the latest. A pinned version cannot change under you, and it makes no API call, so it does not hit the anonymous rate limit shared runners often run into. Put `LASK_VERSION` in the workflow's `env:` and bump it on purpose, as you would an action's version.

### `sync` before `check`

`lask sync --frozen` comes first: `check` reads imported modules from the local cache, and on a fresh runner only `sync` fills it. Both fail with exit code 1 on a problem, so either one stops the job before any task runs.

### Inputs go through the environment

Write `--env "$DEPLOY_ENV"` with `DEPLOY_ENV: ${{ inputs.env }}` in the step's `env:`, not `--env ${{ inputs.env }}` in the script. An expression in `run:` is pasted into the shell script before it runs, so an input or a branch name can inject commands; a variable cannot. The task then checks the value like any other argument.

### Secrets: GitHub fetches, Lask passes on

Let the workflow obtain credentials — `secrets.*`, OIDC with `aws-actions/configure-aws-credentials`, `docker/login-action` — and let the task read them from the environment. Tools on `#local`, such as `docker push`, use what the login action left on the runner. A tool in an image does not see the runner's environment, so hand it the credential explicitly, bound with `!!` so its value is masked in Lask's log:

```lask
aws = #amazon/aws-cli:2.31.9

// Upload the binary.
upload(--bucket = "example-artifacts", --key!!: String = get_env("AWS_SECRET_ACCESS_KEY")) =
  $[runnable(aws, env = {"AWS_ACCESS_KEY_ID": get_env("AWS_ACCESS_KEY_ID"), "AWS_SECRET_ACCESS_KEY": key, "AWS_SESSION_TOKEN": find_env("AWS_SESSION_TOKEN")})] aws s3 cp bin/hello s3://#{bucket}/hello
```

A value passed in `env:` never appears on a command line. `find_env` returns `null` when the variable is not set, and a `null` variable is left out, so the same task works with long-lived keys and with OIDC's session tokens. To keep secrets out of CI configuration entirely, see the secret references in the [Quick Reference](../quick-reference.md#input-output-secrets).

### Services

A `services:` container lives for the whole job and is only there in CI. Started from the task, the service lives exactly as long as the tests: `race` returns as soon as the tests finish, pass or fail, and stops the database. The tests reach it through a published port and `host.docker.internal`, which works on Linux runners and Docker Desktop alike. Two runs on one machine would need different ports — pass the port as a parameter if that matters to you.

If you would rather keep `services:`, do: the tests then reach `localhost:5432` on the runner, and a `db_url` parameter lets the task be pointed at it.

### Matrices

A matrix in YAML runs each combination as its own job, on its own runner, with its own line in the checks list. A matrix in Lask runs them concurrently in one job, and locally. Choose by what you need: separate checks and more machines, or one command a developer can run. Both can call the same task:

```yaml
    strategy:
      matrix:
        go: ["1.24", "1.25"]
    steps:
      # ...
      - run: lask run test --go "$GO_VERSION"
        env:
          GO_VERSION: ${{ matrix.go }}
```

A version missing from `go_images` fails instead of running, and each listed one is pinned in the lock — a matrix entry cannot pull an image nobody reviewed.

### Caching

`lask sync` pulls the images on every fresh runner; there is no cache for them in this guide. Tool caches are another matter: a directory mounted from the workspace (`GO_CACHE` in the example) is an ordinary path for `actions/cache`, while locally the same task uses a named Docker volume. The program stays the same; only the environment differs.

### Results and outputs

`lask run` writes nothing to stdout, and every command's output goes to the log on stderr, which is what the job log shows. To hand a value to a later step, use `lask eval`:

```yaml
      - id: image
        run: echo "ref=$(lask eval --stdout-encode text image)" >> "$GITHUB_OUTPUT"
```

`--stdout-encode text` writes a `String` result without JSON quotes.

### Exit codes

A step fails when `lask` exits non-zero, and a failed command passes its own exit code through. Static errors and a stale lock are `1`, a usage error — including a deploy that was not confirmed — is `4`. → [Quick Reference: CLI](../quick-reference.md#cli)

### Confirmation in CI

A `confirm` entry in `lask.json` makes `lask run deploy --env prod` ask on a terminal. A runner has none, so the deploy fails with exit code 4 unless the workflow passes `--confirm`, before the task name. Writing `--confirm` in the workflow is the deliberate act; the `environment:` reviewers remain the actual gate.

### Runners without Docker

Containers need a Linux runner. Hosted macOS runners on Apple silicon have no nested virtualization for a Docker VM, and hosted Windows runners run only Windows containers. On those, only tasks on `#local` can run — which is often enough for a build or test of platform-specific code.

### Shallow checkouts

`actions/checkout` fetches one commit and no tags by default, so `git describe --tags` finds nothing and `--always` falls back to a commit hash. Set `fetch-depth: 0` in any job whose tasks read the history.

### Standard input

`lask run` and `lask eval` read standard input to the end before a task starts, so that a task can use `stdin`. A GitHub Actions step has none to read, so this never waits there. A wrapper script or another CI system that leaves stdin open will make Lask wait for it: add `< /dev/null`.

## Other CI systems

The task is the same everywhere; only the wrapper changes. GitLab CI, for example, on a runner with the shell executor and Docker installed:

```yaml
ci:
  tags: [docker-host]
  variables:
    LASK_VERSION: v0.7.0
  script:
    - curl -fsSL "https://github.com/lask-task-runner/lask/releases/download/${LASK_VERSION}/lask-${LASK_VERSION}-linux-amd64.tar.gz" | tar -xz -C /tmp lask
    - /tmp/lask sync --frozen
    - /tmp/lask check
    - /tmp/lask run ci
```

Wherever it runs, Lask needs two things from the machine. The Linux binary is built against glibc, so it does not start in an Alpine-based image such as `docker:27`. And Lask mounts the project directory into each container, so the Docker daemon has to see the same filesystem as Lask: a Docker-in-Docker service does not, unless the build directory is shared with it.

## Checklist

- [ ] `lask run ci` passes on your machine before the workflow calls it.
- [ ] `lask.lock.json` is committed, and every workflow runs `lask sync --frozen` before anything else.
- [ ] No `setup-*` action is left for a tool a task runs in an image.
- [ ] Inputs and other `${{ }}` values reach `lask` through `env:`, not pasted into `run:`.
- [ ] Credentials are read with `get_env` into `!!` bindings, and passed to images with `env:`.
- [ ] Jobs that call `git describe` or read history check out with `fetch-depth: 0`.
- [ ] A task that needs `confirm` is called with `--confirm` only from the workflow that is meant to run it.
