// Configuration for the deployed-stack E2E run.
//
// Everything here is shaped by two facts: the target is the real
// CloudFront + Lambda + RDS stack (slow, shared, mutable), and the runner
// is lask's docker environment, which offers only `memory` / `cpus` --
// no --shm-size, no --ipc=host.
import { defineConfig } from '@playwright/test'

const baseURL = process.env.E2E_BASE_URL

if (!baseURL) {
  throw new Error(
    'E2E_BASE_URL is not set. Run this through `lask run test_e2e`, which ' +
      'fills it in from `terraform -chdir=infra output -raw website_url`.',
  )
}

export default defineConfig({
  testDir: './tests',
  outputDir: './test-results',

  // The tests mutate the deployed RDS instance, which every user shares
  // (the orders table has no owner column). A single worker keeps the run
  // deterministic and keeps one Cognito user from signing in from two
  // browsers at once. This is not a tuning knob: raising it makes the
  // suite wrong, not just flaky.
  workers: 1,
  fullyParallel: false,

  // A first request after an idle period pays a CloudFront miss, a VPC
  // Lambda cold start and a fresh Postgres connection. Budgets are
  // generous on purpose: a timeout here should mean "the stack is
  // broken", not "the stack was slow".
  timeout: 180_000,
  expect: { timeout: 30_000 },

  // One retry absorbs a genuinely transient cold start without hiding a
  // real regression (a real break fails twice).
  retries: 1,

  reporter: [
    ['list'],
    ['html', { open: 'never', outputFolder: './playwright-report' }],
  ],

  use: {
    baseURL,
    actionTimeout: 30_000,
    navigationTimeout: 60_000,

    // Artifacts land under /work/e2e/... which is the host's
    // example/04-webapp/e2e/... via lask's bind mount.
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },

  projects: [
    {
      name: 'chromium',
      use: {
        // Set explicitly rather than via devices['Desktop Chrome'], which
        // pins a branded `channel` that the image does not ship.
        browserName: 'chromium',

        // Must stay >= 992px (the Bootstrap `md` breakpoint): the Cognito
        // Hosted UI renders two copies of its sign-in form and swaps
        // between them there. helpers.js targets the visible one, so a
        // narrower viewport would silently change which form is driven.
        viewport: { width: 1280, height: 900 },

        launchOptions: {
          // lask's `docker run` has no --shm-size, so /dev/shm is the
          // default 64MB and Chromium's renderer would die with a bus
          // error as soon as a page needs real shared memory.
          args: ['--disable-dev-shm-usage'],
        },
      },
    },
  ],
})
