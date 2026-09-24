import { fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import App from './App.jsx'

const HEALTH_URL = 'https://example.test/'
const ORDERS_URL = 'https://example.test/orders'
const ACCESS_TOKEN = 'test-access-token'

// Stands in for the real OIDC session so the tests exercise the app, not
// Cognito. `authState` is what useAuth() returns; each test sets it.
let authState

vi.mock('react-oidc-context', () => ({
  useAuth: () => authState,
}))

function signedIn(overrides = {}) {
  return {
    isLoading: false,
    isAuthenticated: true,
    error: null,
    user: { access_token: ACCESS_TOKEN, profile: { email: 'user@example.test' } },
    signinRedirect: vi.fn(),
    // Sign-out drops the local session through removeUser and then
    // navigates to Cognito's /logout by hand; signoutRedirect is not used
    // (see cognitoLogoutUrl in ./auth).
    removeUser: vi.fn().mockResolvedValue(undefined),
    ...overrides,
  }
}

function jsonResponse(body, { ok = true, status = 200 } = {}) {
  return Promise.resolve({ ok, status, json: async () => body })
}

const HEALTH_OK = {
  message: 'Hello from Lask API',
  method: 'GET',
  path: '/',
  database: 'connected',
}

describe('App', () => {
  beforeEach(() => {
    authState = signedIn()
    vi.stubGlobal('fetch', vi.fn())
  })

  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('renders health status and the orders list', async () => {
    fetch.mockImplementation((url) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === ORDERS_URL) {
        return jsonResponse({
          orders: [{ id: 1, item: 'Widget', quantity: 3, created_at: '2024-01-01T00:00:00+00:00' }],
        })
      }
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)

    expect(await screen.findByText('Hello from Lask API')).toBeInTheDocument()
    expect(await screen.findByText('Widget')).toBeInTheDocument()
  })

  it('shows an empty state when there are no orders', async () => {
    fetch.mockImplementation((url) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === ORDERS_URL) return jsonResponse({ orders: [] })
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)

    expect(await screen.findByText(/no orders yet/i)).toBeInTheDocument()
    expect(screen.queryByRole('table')).not.toBeInTheDocument()
  })

  it('submits a new order and shows it in the list', async () => {
    let orders = []

    fetch.mockImplementation((url, options) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === ORDERS_URL && options?.method === 'POST') {
        const created = { id: 1, ...JSON.parse(options.body), created_at: '2024-01-01T00:00:00+00:00' }
        orders = [created]
        return jsonResponse({ order: created }, { status: 201 })
      }
      if (url === ORDERS_URL) return jsonResponse({ orders })
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)
    await screen.findByText('Hello from Lask API')
    await screen.findByText('Orders')

    fireEvent.change(screen.getByLabelText('Item'), { target: { value: 'Widget' } })
    fireEvent.change(screen.getByLabelText('Quantity'), { target: { value: '3' } })
    fireEvent.click(screen.getByRole('button', { name: /add order/i }))

    expect(await screen.findByText('Widget')).toBeInTheDocument()
  })

  it('deletes an order and refreshes the list', async () => {
    let orders = [{ id: 1, item: 'Widget', quantity: 3, created_at: '2024-01-01T00:00:00+00:00' }]
    const deleteUrl = `${ORDERS_URL}/1`

    fetch.mockImplementation((url, options) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === deleteUrl && options?.method === 'DELETE') {
        orders = []
        // 204 No Content: the component must not parse a body here.
        return Promise.resolve({ ok: true, status: 204 })
      }
      if (url === ORDERS_URL) return jsonResponse({ orders })
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)
    expect(await screen.findByText('Widget')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /delete order 1/i }))

    expect(await screen.findByText(/no orders yet/i)).toBeInTheDocument()
    expect(screen.queryByText('Widget')).not.toBeInTheDocument()
  })

  it('surfaces the API error when a delete fails', async () => {
    const orders = [{ id: 1, item: 'Widget', quantity: 3, created_at: '2024-01-01T00:00:00+00:00' }]

    fetch.mockImplementation((url, options) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === `${ORDERS_URL}/1` && options?.method === 'DELETE') {
        return jsonResponse({ error: 'order 1 not found' }, { ok: false, status: 404 })
      }
      if (url === ORDERS_URL) return jsonResponse({ orders })
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)
    fireEvent.click(await screen.findByRole('button', { name: /delete order 1/i }))

    expect(await screen.findByRole('alert')).toHaveTextContent('order 1 not found')
  })

  it('shows an error message when loading orders fails', async () => {
    fetch.mockImplementation((url) => {
      if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
      if (url === ORDERS_URL) return jsonResponse({}, { ok: false, status: 500 })
      throw new Error(`unexpected fetch: ${url}`)
    })

    render(<App />)

    expect(await screen.findByRole('alert')).toHaveTextContent('500')
  })

  describe('authentication', () => {
    it('sends the access token as a Bearer header on order requests', async () => {
      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        if (url === ORDERS_URL) return jsonResponse({ orders: [] })
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)
      await screen.findByText(/no orders yet/i)

      const ordersCall = fetch.mock.calls.find(([url]) => url === ORDERS_URL)
      expect(ordersCall[1].headers.Authorization).toBe(`Bearer ${ACCESS_TOKEN}`)
    })

    it('does not send a token on the public health request', async () => {
      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        if (url === ORDERS_URL) return jsonResponse({ orders: [] })
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)
      await screen.findByText('Hello from Lask API')

      const healthCall = fetch.mock.calls.find(([url]) => url === HEALTH_URL)
      expect(healthCall[1]).toBeUndefined()
    })

    it('prompts for sign-in and does not call the orders API when signed out', async () => {
      authState = signedIn({ isAuthenticated: false, user: null })
      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)

      const button = await screen.findByRole('button', { name: /sign in/i })
      fireEvent.click(button)
      expect(authState.signinRedirect).toHaveBeenCalled()
      expect(fetch.mock.calls.some(([url]) => url === ORDERS_URL)).toBe(false)
    })

    it('reports an expired session rather than the raw 401', async () => {
      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        if (url === ORDERS_URL) return jsonResponse({}, { ok: false, status: 401 })
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)

      expect(await screen.findByRole('alert')).toHaveTextContent(/session expired/i)
    })

    it('reports a missing scope as a permission problem on 403', async () => {
      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        if (url === ORDERS_URL) return jsonResponse({}, { ok: false, status: 403 })
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)

      expect(await screen.findByRole('alert')).toHaveTextContent(/permission/i)
    })

    // Regression test for a bug the browser E2E suite caught (e2e/):
    // sign-out used to call auth.signoutRedirect(), which sends the
    // standard OIDC `id_token_hint` / `post_logout_redirect_uri`. Cognito
    // advertises an end_session_endpoint but does not accept those -- it
    // requires client_id + logout_uri and answers anything else with
    // "Client does not exist", so signing out failed in production while
    // every mocked test here still passed.
    it('signs out by clearing the session and going to Cognito /logout', async () => {
      const assign = vi.fn()
      vi.spyOn(window, 'location', 'get').mockReturnValue({ assign })

      fetch.mockImplementation((url) => {
        if (url === HEALTH_URL) return jsonResponse(HEALTH_OK)
        if (url === ORDERS_URL) return jsonResponse({ orders: [] })
        throw new Error(`unexpected fetch: ${url}`)
      })

      render(<App />)

      fireEvent.click(await screen.findByRole('button', { name: /sign out/i }))

      await vi.waitFor(() => expect(assign).toHaveBeenCalled())
      expect(authState.removeUser).toHaveBeenCalled()

      const target = new URL(assign.mock.calls[0][0])
      expect(target.origin + target.pathname).toBe('https://auth.example.test/logout')
      expect(target.searchParams.get('client_id')).toBe('test-client-id')
      expect(target.searchParams.get('logout_uri')).toBe('https://app.example.test/')
      // The parameters Cognito rejects must not be what we rely on.
      expect(target.searchParams.get('id_token_hint')).toBeNull()
    })
  })
})
