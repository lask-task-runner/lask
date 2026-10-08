A small Next.js app (App Router, plain JavaScript) with Playwright end-to-end tests, where installing, linting, building, testing and the dev server are all Lask tasks. The Playwright run on your laptop is the same one CI runs: same images, pinned by digest, same command.

## Try it right now

You need **Lask** and **Docker**. No Node.js, no npm, no browsers:

```bash
cd example/01-projects/04-nextjs-e2e
lask sync        # one-time, needs network: fetches the tools module, pulls the images
lask run ci      # install, lint and build, then the browser tests
```

`ci` installs the dependencies from [package-lock.json](package-lock.json), runs ESLint and `next build` side by side, then runs the [e2e/](e2e) tests in Chromium and Firefox against the production build:

```
[#mcr.microsoft.com/playwright:v1.63.0-noble:4] 1|   ✓  2 [chromium] › e2e/todos.spec.js:14:1 › adds, completes and deletes a todo (336ms)
...
[#mcr.microsoft.com/playwright:v1.63.0-noble:4] 1|   8 passed (6.2s)
```

`node_modules/`, `.next/` and `playwright-report/` land in this directory, as they would with a local Node; everything else stays in the containers, which are thrown away after each step.

To work on the app:

```bash
lask run dev     # http://localhost:3000, reloads as you edit
```

## What the tasks look like

Everything lives in [main.lask](main.lask). The two environments come from [lask-module-tools](https://github.com/lask-task-runner/lask-module-tools) (declared in [lask.json](lask.json), pinned in [lask.lock.json](lask.lock.json)): Node for the npm steps, and Playwright's own image, which carries the browsers and the libraries they need:

```lask
command { "node", "npm", "npx" } on node

node_image = #node:24.21.0-bookworm-slim
node = tools.node(image = node_image, extra_env = quiet)

playwright = tools.playwright(
  image = #mcr.microsoft.com/playwright:v1.63.0-noble,
  extra_env = merge(quiet, {"CI": "1"})
)
```

The Playwright image's tag must match the `@playwright/test` version in [package.json](package.json) (both 1.63.0): the image has that release's browsers and no other, and nothing downloads browsers at run time. Upgrade the two together. The Node image is Debian-based like Playwright's, so the native packages npm installs (Next's SWC compiler) load in both.

Most tasks are one line, and the `npm` in them picks the Node container:

```lask
install() = $ npm ci --no-audit --no-fund
lint() = $ npm run lint
build() = $ npm run build
```

The browser tests name their environment explicitly, since `npx` is declared on Node. Playwright starts `npm run start` itself ([playwright.config.js](playwright.config.js)), inside the same container as the browsers, so the test is a single command:

```lask
test_e2e(--grep: String = "") = case {
  grep == "" -> $[playwright] npx playwright test
  else -> $[playwright] npx playwright test --grep #{shell_quote(grep)}
}
```

`ci` composes them. Lint and build don't depend on each other, so they run at the same time, in two containers:

```lask
ci() = do {
  install()
  checked = async lint()
  built = async build()
  await checked
  await built
  test_e2e()
}
```

`dev` needs one thing the others don't: its port published to the host. Run options go on the environment, so the same image gets them for this task only:

```lask
internal dev_server(port: Number): Runnable =
  runnable(node_image, env = quiet, publish = ["#{port}:3000"])

dev(--port: Number = 3000) = do {
  log("open http://localhost:#{port}")
  $[dev_server(port)] npm run dev
}
```

`lask run dev --port 8080` serves it on another port. The project directory is mounted into the container, and not every Docker setup passes file-change events across that mount, so [next.config.mjs](next.config.mjs) has the dev server poll for edits instead.

## In CI

The pipeline is `lask sync --frozen && lask run ci`, so a CI job only installs Lask. On GitHub Actions, whose Ubuntu runners already have Docker:

```yaml
jobs:
  e2e:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install Lask
        run: |
          VERSION=$(curl -fsSL https://api.github.com/repos/lask-task-runner/lask/releases/latest | grep -m1 '"tag_name"' | cut -d '"' -f4)
          curl -fsSL -o lask.tar.gz "https://github.com/lask-task-runner/lask/releases/download/${VERSION}/lask-${VERSION}-linux-amd64.tar.gz"
          tar -xzf lask.tar.gz && sudo mv ./lask /usr/local/bin
      - run: lask sync --frozen && lask run ci
        working-directory: example/01-projects/04-nextjs-e2e
      - uses: actions/upload-artifact@v4
        if: failure()
        with:
          name: playwright-report
          path: example/01-projects/04-nextjs-e2e/playwright-report
```

`--frozen` makes `sync` fail rather than move the lock, so CI pulls exactly the image digests your laptop ran. Pin the Lask version too, instead of `latest`, once you depend on it.

## Check it without running anything

```bash
lask check            # names, arguments and types; no image is pulled
lask envs list        # the images, and the digest each is pinned to
lask run --help       # every task, from its own comments
lask run test-e2e --help
```
