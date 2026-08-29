data "archive_file" "test_terraform" {
  type        = "zip"
  source_dir  = "../api"
  output_path = "../api/archive.zip"
}

resource "aws_lambda_function" "test_terraform" {
  function_name    = "test_terraform"
  filename         = data.archive_file.test_terraform.output_path
  source_code_hash = data.archive_file.test_terraform.output_base64sha256
  runtime          = "python3.12"
  role             = aws_iam_role.lambda_iam_role.arn
  handler          = "handler.lambda_handler"

  # Placed in the default VPC (rds.tf) so it can reach the RDS instance.
  vpc_config {
    subnet_ids         = data.aws_subnets.default.ids
    security_group_ids = [aws_security_group.lambda.id]
  }

  environment {
    variables = {
      DB_HOST     = aws_db_instance.webapp.address
      DB_PORT     = tostring(aws_db_instance.webapp.port)
      DB_NAME     = aws_db_instance.webapp.db_name
      DB_USER     = aws_db_instance.webapp.username
      DB_PASSWORD = random_password.db.result
    }
  }
}

# The function is reached through the HTTP API (apigw.tf), which
# validates the Cognito JWT before invoking it. The former public
# Function URL is gone: it could not check tokens, so leaving it in place
# would be an unauthenticated way around the authorizer.
