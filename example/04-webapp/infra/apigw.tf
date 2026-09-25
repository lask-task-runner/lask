# HTTP API in front of the Lambda. Chosen over a Lambda Function URL
# because only API Gateway can validate Cognito JWTs natively: a Function
# URL offers just NONE / AWS_IAM, which would force signature checking
# into the handler (and a non-pure-Python crypto dependency that the
# musl-based vendoring step could not produce a working wheel for).
resource "aws_apigatewayv2_api" "webapp" {
  name          = "webapp-http-api"
  protocol_type = "HTTP"

  # Preflight is answered by API Gateway before the authorizer runs, so
  # the browser can send the Authorization header on the real request.
  cors_configuration {
    allow_origins = ["*"]
    allow_methods = ["GET", "POST", "DELETE", "OPTIONS"]
    allow_headers = ["authorization", "content-type"]
    max_age       = 300
  }
}

# Validates issuer, signature, expiry and audience. Cognito access tokens
# carry `client_id` rather than `aud`; the JWT authorizer accepts either,
# so the app client id is the right audience value here.
resource "aws_apigatewayv2_authorizer" "cognito" {
  api_id           = aws_apigatewayv2_api.webapp.id
  authorizer_type  = "JWT"
  identity_sources = ["$request.header.Authorization"]
  name             = "cognito-jwt"

  jwt_configuration {
    audience = [aws_cognito_user_pool_client.webapp.id]
    issuer   = "https://cognito-idp.${var.aws_region}.amazonaws.com/${aws_cognito_user_pool.webapp.id}"
  }
}

resource "aws_apigatewayv2_integration" "lambda" {
  api_id                 = aws_apigatewayv2_api.webapp.id
  integration_type       = "AWS_PROXY"
  integration_uri        = aws_lambda_function.test_terraform.invoke_arn
  payload_format_version = "2.0"
}

# Health check stays open so database connectivity can be diagnosed
# without a login.
resource "aws_apigatewayv2_route" "health" {
  api_id    = aws_apigatewayv2_api.webapp.id
  route_key = "GET /"
  target    = "integrations/${aws_apigatewayv2_integration.lambda.id}"
}

# The /orders routes require a matching custom scope (cognito.tf). A
# request without one is rejected with 403 before reaching the Lambda.
resource "aws_apigatewayv2_route" "list_orders" {
  api_id               = aws_apigatewayv2_api.webapp.id
  route_key            = "GET /orders"
  target               = "integrations/${aws_apigatewayv2_integration.lambda.id}"
  authorization_type   = "JWT"
  authorizer_id        = aws_apigatewayv2_authorizer.cognito.id
  authorization_scopes = ["orders/read"]
}

resource "aws_apigatewayv2_route" "create_order" {
  api_id               = aws_apigatewayv2_api.webapp.id
  route_key            = "POST /orders"
  target               = "integrations/${aws_apigatewayv2_integration.lambda.id}"
  authorization_type   = "JWT"
  authorizer_id        = aws_apigatewayv2_authorizer.cognito.id
  authorization_scopes = ["orders/write"]
}

resource "aws_apigatewayv2_route" "delete_order" {
  api_id               = aws_apigatewayv2_api.webapp.id
  route_key            = "DELETE /orders/{id}"
  target               = "integrations/${aws_apigatewayv2_integration.lambda.id}"
  authorization_type   = "JWT"
  authorizer_id        = aws_apigatewayv2_authorizer.cognito.id
  authorization_scopes = ["orders/delete"]
}

# $default keeps the stage out of the request path, so `rawPath` reaching
# the handler is `/orders` rather than `/<stage>/orders`.
resource "aws_apigatewayv2_stage" "default" {
  api_id      = aws_apigatewayv2_api.webapp.id
  name        = "$default"
  auto_deploy = true
}

resource "aws_lambda_permission" "api_gateway" {
  statement_id  = "AllowExecutionFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.test_terraform.function_name
  principal     = "apigateway.amazonaws.com"
  source_arn    = "${aws_apigatewayv2_api.webapp.execution_arn}/*/*"
}

# The $default stage's invoke_url already ends in "/", which is what the
# frontend's `new URL('orders', API_URL)` needs as a base.
output "api_url" {
  value = aws_apigatewayv2_stage.default.invoke_url
}
