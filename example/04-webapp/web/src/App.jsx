import { useCallback, useEffect, useState } from 'react'
import { useAuth } from 'react-oidc-context'
import { cognitoLogoutUrl } from './auth'
import './App.css'

// Injected at build time by `lask run deploy` (see build_web() in
// ../../main.lask), from the API Gateway stage URL that Terraform creates.
const API_URL = import.meta.env.VITE_API_URL ?? ''
const ORDERS_URL = API_URL ? new URL('orders', API_URL).toString() : ''

const dateFormat = new Intl.DateTimeFormat(undefined, {
  dateStyle: 'medium',
  timeStyle: 'short',
})

function formatCreatedAt(value) {
  const parsed = new Date(value)
  return Number.isNaN(parsed.getTime()) ? value : dateFormat.format(parsed)
}

/** Turns a failed API response into the most useful message available. */
async function errorFrom(res) {
  if (res.status === 401) return 'Session expired. Please sign in again.'
  if (res.status === 403) return 'You do not have permission for this action.'
  const body = await res.json().catch(() => ({}))
  return body.error ?? `request failed with status ${res.status}`
}

function useHealth() {
  const [state, setState] = useState({ status: 'loading' })

  useEffect(() => {
    // The health route is deliberately left unauthenticated
    // (infra/apigw.tf), so this runs before sign-in.
    fetch(API_URL)
      .then((res) => {
        if (!res.ok) {
          throw new Error(`request failed with status ${res.status}`)
        }
        return res.json()
      })
      .then((data) => setState({ status: 'ready', data }))
      .catch((error) => setState({ status: 'error', error: error.message }))
  }, [])

  return state
}

function OrdersPanel({ accessToken }) {
  const [orders, setOrders] = useState([])
  const [status, setStatus] = useState('loading')
  const [error, setError] = useState(null)
  const [item, setItem] = useState('')
  const [quantity, setQuantity] = useState('1')
  const [submitting, setSubmitting] = useState(false)
  const [deletingId, setDeletingId] = useState(null)

  // Every /orders call carries the Cognito access token; API Gateway
  // checks its signature and required scope before the Lambda runs.
  const authFetch = useCallback(
    (url, options = {}) =>
      fetch(url, {
        ...options,
        headers: { ...options.headers, Authorization: `Bearer ${accessToken}` },
      }),
    [accessToken],
  )

  const loadOrders = useCallback(async () => {
    setStatus('loading')
    try {
      const res = await authFetch(ORDERS_URL)
      if (!res.ok) {
        throw new Error(await errorFrom(res))
      }
      const data = await res.json()
      setOrders(data.orders ?? [])
      setStatus('ready')
    } catch (err) {
      setError(err.message)
      setStatus('error')
    }
  }, [authFetch])

  useEffect(() => {
    loadOrders()
  }, [loadOrders])

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitting(true)
    setError(null)
    try {
      const res = await authFetch(ORDERS_URL, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ item, quantity: Number(quantity) }),
      })
      if (!res.ok) {
        throw new Error(await errorFrom(res))
      }
      setItem('')
      setQuantity('1')
      await loadOrders()
    } catch (err) {
      setError(err.message)
    } finally {
      setSubmitting(false)
    }
  }

  const handleDelete = async (id) => {
    setDeletingId(id)
    setError(null)
    try {
      const res = await authFetch(`${ORDERS_URL}/${id}`, { method: 'DELETE' })
      // 204 carries no body, so nothing is parsed on the success path.
      if (!res.ok) {
        throw new Error(await errorFrom(res))
      }
      await loadOrders()
    } catch (err) {
      setError(err.message)
    } finally {
      setDeletingId(null)
    }
  }

  return (
    <section className="card">
      <h2>Orders</h2>

      <form className="order-form" onSubmit={handleSubmit}>
        <label>
          Item
          <input value={item} onChange={(e) => setItem(e.target.value)} required />
        </label>
        <label className="qty">
          Quantity
          <input
            type="number"
            min="1"
            value={quantity}
            onChange={(e) => setQuantity(e.target.value)}
            required
          />
        </label>
        <button type="submit" disabled={submitting}>
          {submitting ? 'Adding...' : 'Add order'}
        </button>
      </form>

      {error && <p role="alert">Error: {error}</p>}
      {status === 'loading' && <p className="muted">Loading orders...</p>}
      {status === 'ready' && orders.length === 0 && (
        <p className="muted">No orders yet. Add the first one above.</p>
      )}

      {status === 'ready' && orders.length > 0 && (
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>ID</th>
                <th>Item</th>
                <th>Quantity</th>
                <th>Created At</th>
                <th aria-label="Actions" />
              </tr>
            </thead>
            <tbody>
              {orders.map((order) => (
                <tr key={order.id}>
                  <td className="num">{order.id}</td>
                  <td>{order.item}</td>
                  <td className="num">{order.quantity}</td>
                  <td>{formatCreatedAt(order.created_at)}</td>
                  <td>
                    <button
                      type="button"
                      className="link-danger"
                      aria-label={`Delete order ${order.id}`}
                      disabled={deletingId === order.id}
                      onClick={() => handleDelete(order.id)}
                    >
                      {deletingId === order.id ? 'Deleting...' : 'Delete'}
                    </button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </section>
  )
}

function SignInPrompt({ auth }) {
  return (
    <section className="card signin">
      <p className="muted">Sign in to view and manage orders.</p>
      <button type="button" onClick={() => auth.signinRedirect()}>
        Sign in
      </button>
    </section>
  )
}

export default function App() {
  const auth = useAuth()
  const health = useHealth()

  // Drop the local session first, then hand the browser to Cognito's
  // /logout so the Hosted UI does not simply sign the user straight back
  // in. `auth.signoutRedirect()` cannot do this: Cognito does not accept
  // the standard OIDC logout parameters it would send (see
  // cognitoLogoutUrl in ./auth).
  const signOut = async () => {
    await auth.removeUser()
    window.location.assign(cognitoLogoutUrl())
  }

  return (
    <main>
      <header className="topbar">
        <div>
          <h1>Lask Webapp Example</h1>
          <p className="lede">React on CloudFront, calling a Cognito-protected API.</p>
        </div>
        {auth.isAuthenticated && (
          <div className="user">
            <span className="muted">{auth.user?.profile?.email}</span>
            <button type="button" className="secondary" onClick={signOut}>
              Sign out
            </button>
          </div>
        )}
      </header>

      <section className="card">
        {health.status === 'loading' && <p className="muted">Loading...</p>}
        {health.status === 'error' && <p role="alert">Error: {health.error}</p>}
        {health.status === 'ready' && (
          <dl className="health">
            <div>
              <dt>message</dt>
              <dd>{health.data.message}</dd>
            </div>
            <div>
              <dt>database</dt>
              <dd>
                <span className="badge" data-state={health.data.database}>
                  {health.data.database}
                </span>
              </dd>
            </div>
          </dl>
        )}
      </section>

      {auth.isLoading && <p className="muted">Signing in...</p>}
      {auth.error && <p role="alert">Authentication error: {auth.error.message}</p>}
      {!auth.isLoading && !auth.isAuthenticated && <SignInPrompt auth={auth} />}
      {auth.isAuthenticated && <OrdersPanel accessToken={auth.user.access_token} />}
    </main>
  )
}
