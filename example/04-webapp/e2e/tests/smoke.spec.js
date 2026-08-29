// Unauthenticated landing state. This runs without a login, so it is the
// test that tells you whether the deploy itself is healthy: CloudFront
// serving the current bundle, and the open `GET /` route reaching the
// Lambda and the database.
import { expect, test } from '@playwright/test'
import { healthRow } from './helpers.js'

test('landing page renders signed out, with the health panel filled in', async ({
  page,
}) => {
  await page.goto('/')

  await expect(
    page.getByRole('heading', { name: 'Lask Webapp Example', level: 1 }),
  ).toBeVisible()

  // The health route is deliberately unauthenticated (infra/apigw.tf), so
  // this resolves before any sign-in. The extra budget is the VPC Lambda
  // cold start plus the first Postgres connection.
  await expect(page.locator('dl.health')).toBeVisible({ timeout: 90_000 })

  await expect(healthRow(page, 'message').locator('dd')).toHaveText(
    'Hello from Lask API',
  )

  // "connected" is the only healthy value; _check_database() in
  // api/handler.py otherwise yields "not configured" or "error: ...".
  await expect(
    healthRow(page, 'database').locator('span.badge'),
  ).toHaveAttribute('data-state', 'connected')

  await expect(page.getByText('Sign in to view and manage orders.')).toBeVisible()
  await expect(page.getByRole('button', { name: 'Sign in' })).toBeVisible()

  // Orders belong to the authenticated branch only.
  await expect(page.getByRole('heading', { name: 'Orders' })).toHaveCount(0)
  await expect(page.getByRole('button', { name: 'Sign out' })).toHaveCount(0)

  // No <p role="alert">Error: ...</p> anywhere.
  await expect(page.getByRole('alert')).toHaveCount(0)
})
