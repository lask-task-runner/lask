// OIDC settings for the Cognito user pool, injected at build time by
// `lask run deploy` (see build_web() in ../../main.lask). None of these
// are secrets: a public OIDC client has no client secret, and the issuer
// and client id are visible to the browser by design.
const AUTHORITY = import.meta.env.VITE_COGNITO_AUTHORITY ?? ''
const CLIENT_ID = import.meta.env.VITE_COGNITO_CLIENT_ID ?? ''
const DOMAIN = import.meta.env.VITE_COGNITO_DOMAIN ?? ''
const REDIRECT_URI = import.meta.env.VITE_REDIRECT_URI ?? window.location.origin + '/'

export const authConfig = {
  authority: AUTHORITY,
  client_id: CLIENT_ID,
  redirect_uri: REDIRECT_URI,
  post_logout_redirect_uri: REDIRECT_URI,

  // Authorization code flow with PKCE: the flow for browser apps that
  // cannot keep a secret (the implicit flow is deprecated and is not
  // enabled on the app client).
  response_type: 'code',

  // The custom scopes are what the API Gateway authorizer enforces per
  // route (infra/apigw.tf); `openid` is what makes this an OIDC request
  // rather than plain OAuth.
  scope: 'openid email orders/read orders/write orders/delete',

  // Drop `?code=...&state=...` from the address bar after the exchange.
  onSigninCallback: () => {
    window.history.replaceState({}, document.title, window.location.pathname)
  },
}

/**
 * The URL that ends the Cognito session.
 *
 * This is built by hand rather than going through
 * `auth.signoutRedirect()`, because Cognito advertises an
 * `end_session_endpoint` in its discovery document without implementing
 * the OIDC RP-initiated logout parameters that implies. Its /logout
 * ignores `id_token_hint` / `post_logout_redirect_uri` (what
 * oidc-client-ts sends) and instead requires `client_id` + `logout_uri`,
 * answering anything else with "Client does not exist".
 *
 * `logout_uri` must exactly match an entry in `logout_urls` on the app
 * client (infra/cognito.tf), trailing slash included.
 */
export function cognitoLogoutUrl() {
  const url = new URL(`${DOMAIN}/logout`)
  url.searchParams.set('client_id', CLIENT_ID)
  url.searchParams.set('logout_uri', REDIRECT_URI)
  return url.toString()
}
