// The signed-in lifecycle, as one test made of steps rather than four
// tests. The steps are strictly ordered (you cannot delete before you
// create) and the Hosted UI login is far too slow to repeat, so a single
// test keeps them on one page -- and, unlike a serial describe sharing a
// hand-made context, it keeps Playwright's automatic trace, screenshot
// and video capture, which is what you actually debug a failure against.
import { randomUUID } from 'node:crypto'
import { expect, test } from '@playwright/test'
import {
  deleteOrdersNamed,
  orderRow,
  requireEnv,
  signInThroughHostedUi,
} from './helpers.js'

const EMAIL = requireEnv('E2E_EMAIL')
const PASSWORD = requireEnv('E2E_PASSWORD')
const QUANTITY = '3'

// The row this run creates, named so no other run (or retry) can collide
// with it: the table lives in the deployed RDS instance and every
// signed-in user sees all of it.
let item

test.beforeEach(() => {
  item = `e2e-${randomUUID().slice(0, 8)}`
})

test.afterEach(async ({ page }) => {
  await deleteOrdersNamed(page, item)
})

test('signed-in order lifecycle: create, list, delete', async ({ page }) => {
  await test.step('starts signed out', async () => {
    await page.goto('/')
    await expect(page.getByRole('button', { name: 'Sign in' })).toBeVisible()
    await expect(page.getByRole('heading', { name: 'Orders' })).toHaveCount(0)
  })

  await test.step('signs in through the Cognito Hosted UI', async () => {
    await signInThroughHostedUi(page, { email: EMAIL, password: PASSWORD })
  })

  await test.step('shows the orders panel', async () => {
    await expect(page.getByRole('heading', { name: 'Orders' })).toBeVisible()
    // GET /orders needs the orders/read scope; an alert here means the
    // access token reached API Gateway without it.
    await expect(page.getByRole('alert')).toHaveCount(0)
    await expect(page.getByText('Loading orders...')).toHaveCount(0)
  })

  await test.step('creates an order', async () => {
    // <label>Item<input/></label> -- an implicit label association, which
    // getByLabel resolves.
    await page.getByLabel('Item', { exact: true }).fill(item)
    await page.getByLabel('Quantity', { exact: true }).fill(QUANTITY)
    await page.getByRole('button', { name: 'Add order' }).click()

    // The app reloads the list after POSTing, so the row appearing proves
    // the round trip, not just local state.
    const row = orderRow(page, item)
    await expect(row).toHaveCount(1)
    await expect(row.locator('td').nth(1)).toHaveText(item)
    await expect(row.locator('td').nth(2)).toHaveText(QUANTITY)
    // Created At goes through Intl.DateTimeFormat, so it is locale
    // dependent; assert only that it is populated.
    await expect(row.locator('td').nth(3)).not.toBeEmpty()
  })

  await test.step('survives a reload (it is really in the database)', async () => {
    await page.reload()
    await expect(orderRow(page, item)).toHaveCount(1)
  })

  await test.step('deletes the order', async () => {
    const row = orderRow(page, item)
    const id = (await row.locator('td').first().innerText()).trim()

    await row.getByRole('button', { name: `Delete order ${id}` }).click()
    await expect(row).toHaveCount(0)
  })

  await test.step('the deletion is persisted, not just local', async () => {
    await page.reload()
    await expect(orderRow(page, item)).toHaveCount(0)
    await expect(page.getByRole('alert')).toHaveCount(0)
  })

  await test.step('signs out', async () => {
    // RP-initiated logout (web/src/auth.js) ends the Cognito session too,
    // so this leaves nothing behind for the next run.
    await page.getByRole('button', { name: 'Sign out' }).click()
    await expect(page.getByRole('button', { name: 'Sign in' })).toBeVisible()
    await expect(page.getByRole('heading', { name: 'Orders' })).toHaveCount(0)
  })
})
