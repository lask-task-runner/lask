// Shared helpers. Deliberately not named *.spec.js, so Playwright's
// default testMatch does not treat this file as a suite.
import { expect } from '@playwright/test'

/** Reads a required env var, with a message that points at the lask task. */
export function requireEnv(name) {
  const value = process.env[name]
  if (!value) {
    throw new Error(`${name} is not set. Run this through \`lask run test_e2e\`.`)
  }
  return value
}

/** The row of the orders table whose Item cell holds `item`. */
export function orderRow(page, item) {
  return page.locator('table tbody tr').filter({ hasText: item })
}

/** The <div> of the health <dl> whose <dt> is `label`. */
export function healthRow(page, label) {
  return page
    .locator('dl.health > div')
    .filter({ has: page.getByText(label, { exact: true }) })
}

/**
 * Signs in through the real Cognito Hosted UI.
 *
 * A browser login is mandatory here: the /orders routes require the
 * custom scopes orders/read|write|delete (infra/apigw.tf), and Cognito
 * issues those only through the OAuth2 /token endpoint at the end of the
 * authorization-code + PKCE flow. A token from
 * `aws cognito-idp admin-initiate-auth` carries only
 * aws.cognito.signin.user.admin and API Gateway rejects it with 403.
 *
 * SELECTOR TRAP (verified against the live page): the classic Hosted UI
 * renders its sign-in form TWICE, as Bootstrap responsive variants --
 * `.modal-content...visible-xs visible-sm` (mobile) comes FIRST in the
 * DOM and `.modal-content...visible-md visible-lg` (desktop) second. So
 * a bare input[name="username"] matches two nodes and trips strict mode,
 * and `.first()` would drive the *hidden mobile* copy at the 1280px
 * viewport this suite runs at. Scoping to the visible container is what
 * makes this correct, not incidental ordering.
 */
export async function signInThroughHostedUi(page, { email, password }) {
  const appOrigin = new URL(requireEnv('E2E_BASE_URL')).origin

  await page.getByRole('button', { name: 'Sign in' }).click()

  // /oauth2/authorize bounces to /login; wait for the domain, not a path.
  await page.waitForURL(/\.auth\.[a-z0-9-]+\.amazoncognito\.com\//, {
    timeout: 60_000,
  })

  // The one form actually laid out at this viewport.
  const form = page.locator('.modal-content:visible form').first()
  const username = form.locator('input[name="username"]')
  const secret = form.locator('input[name="password"]')

  await expect(
    username,
    'Cognito Hosted UI sign-in form did not render',
  ).toBeVisible()

  await username.fill(email)
  await secret.fill(password)

  // The classic Hosted UI submits with
  // <input type="Submit" name="signInSubmitButton">; managed login v2
  // would use a plain <button type="submit">. Probe for the classic
  // control and fall back, so a future AWS default flip degrades into a
  // clear failure rather than a mystery timeout.
  const classicSubmit = form.locator('input[name="signInSubmitButton"]')
  const submit = (await classicSubmit.count())
    ? classicSubmit
    : form.locator('button[type="submit"]')

  await submit.click()

  // Back on the app, oidc-client-ts exchanges ?code= for tokens and
  // onSigninCallback (web/src/auth.js) strips the query string.
  try {
    await page.waitForURL((url) => url.origin === appOrigin, { timeout: 60_000 })
  } catch (cause) {
    // prevent_user_existence_errors = ENABLED (infra/cognito.tf) turns
    // both "no such user" and "wrong password" into the same generic
    // message, so surface whatever Cognito rendered instead of letting
    // this fail as an anonymous 60s timeout.
    const shown = await page
      .locator('#loginErrorMessage, .errorMessage, [role="alert"]')
      .first()
      .textContent()
      .catch(() => null)

    throw new Error(
      shown?.trim()
        ? `Cognito rejected the sign-in: "${shown.trim()}". Check E2E_EMAIL / ` +
          'E2E_PASSWORD in .env, and that the user was created with ' +
          '`lask run create_user`.'
        : `Never returned to ${appOrigin} after submitting the Hosted UI form ` +
          `(still at ${page.url()}).`,
      { cause },
    )
  }

  await expect(page.getByRole('button', { name: 'Sign out' })).toBeVisible()
  await expect(page.locator('span.muted', { hasText: email })).toBeVisible()
}

/**
 * Best-effort removal of every row named `item`.
 *
 * The orders table is real and shared, so a run that dies mid-flight must
 * not leave litter behind. Failures here are swallowed on purpose:
 * cleanup must never replace the assertion failure that is the actual
 * news.
 */
export async function deleteOrdersNamed(page, item) {
  try {
    if (!(await page.getByRole('button', { name: 'Sign out' }).count())) return

    await page.goto('/') // same tab, so the sessionStorage token survives

    for (let attempt = 0; attempt < 5; attempt += 1) {
      const row = orderRow(page, item)
      if ((await row.count()) === 0) return
      await row.first().getByRole('button', { name: /^Delete order / }).click()
      await expect(row).toHaveCount(0, { timeout: 30_000 })
    }
  } catch {
    // ignored on purpose
  }
}
