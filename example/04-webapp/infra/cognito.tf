# OIDC identity provider for the app. Sign-up is disabled: users are
# created by an administrator (`lask run create_user`), so the publicly
# reachable Hosted UI cannot be used to open accounts.
resource "aws_cognito_user_pool" "webapp" {
  name = "webapp-user-pool"

  admin_create_user_config {
    allow_admin_create_user_only = true
  }

  username_attributes      = ["email"]
  auto_verified_attributes = ["email"]

  password_policy {
    minimum_length    = 8
    require_lowercase = true
    require_numbers   = true
    require_uppercase = true
    require_symbols   = false
  }

  # An example environment is torn down often; don't block `destroy`.
  deletion_protection = "INACTIVE"
}

# Hosted UI / OIDC endpoints live under a Cognito-provided domain, so no
# custom domain or ACM certificate is needed. The prefix is suffixed with
# the account id because it must be globally unique.
resource "aws_cognito_user_pool_domain" "webapp" {
  domain       = "lask-webapp-${data.aws_caller_identity.current.account_id}"
  user_pool_id = aws_cognito_user_pool.webapp.id
}

# Custom scopes carried by the access token. API Gateway enforces these
# per route (apigw.tf), so the Lambda needs no authorization code.
resource "aws_cognito_resource_server" "orders" {
  identifier   = "orders"
  name         = "Orders API"
  user_pool_id = aws_cognito_user_pool.webapp.id

  scope {
    scope_name        = "read"
    scope_description = "List orders"
  }

  scope {
    scope_name        = "write"
    scope_description = "Create orders"
  }

  scope {
    scope_name        = "delete"
    scope_description = "Delete orders"
  }
}

# Public SPA client: authorization code flow with PKCE and no client
# secret (the OAuth 2.1 / OIDC recommendation for browser apps; the
# implicit flow is deliberately not enabled).
resource "aws_cognito_user_pool_client" "webapp" {
  name         = "webapp-spa"
  user_pool_id = aws_cognito_user_pool.webapp.id

  generate_secret = false

  allowed_oauth_flows_user_pool_client = true
  allowed_oauth_flows                  = ["code"]
  allowed_oauth_scopes = [
    "openid",
    "email",
    "orders/read",
    "orders/write",
    "orders/delete",
  ]
  supported_identity_providers = ["COGNITO"]

  # Cognito requires HTTPS callbacks, with http://localhost as the only
  # exception -- hence CloudFront in front of S3 (cloudfront.tf); the
  # localhost entry is for `npm run dev`.
  callback_urls = [
    "https://${aws_cloudfront_distribution.frontend.domain_name}/",
    "http://localhost:5173/",
  ]
  logout_urls = [
    "https://${aws_cloudfront_distribution.frontend.domain_name}/",
    "http://localhost:5173/",
  ]

  # Refresh silently rather than bouncing the user back to the login page.
  access_token_validity  = 60
  id_token_validity      = 60
  refresh_token_validity = 1

  token_validity_units {
    access_token  = "minutes"
    id_token      = "minutes"
    refresh_token = "days"
  }

  # Do not reveal whether an address is registered.
  prevent_user_existence_errors = "ENABLED"

  depends_on = [aws_cognito_resource_server.orders]
}

output "user_pool_id" {
  value = aws_cognito_user_pool.webapp.id
}

output "user_pool_client_id" {
  value = aws_cognito_user_pool_client.webapp.id
}

output "cognito_domain" {
  value = "https://${aws_cognito_user_pool_domain.webapp.domain}.auth.${var.aws_region}.amazoncognito.com"
}

# The OIDC issuer, which is also the discovery base for the SPA
# (<issuer>/.well-known/openid-configuration).
output "cognito_issuer" {
  value = "https://cognito-idp.${var.aws_region}.amazonaws.com/${aws_cognito_user_pool.webapp.id}"
}
