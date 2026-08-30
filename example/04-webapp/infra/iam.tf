resource "aws_iam_role" "lambda_iam_role" {
  name = "terraform_lambda_iam_role"

  assume_role_policy = <<POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
POLICY
}

// Covers CloudWatch Logs permissions plus the EC2 ENI permissions the
// Lambda execution role needs to run inside the default VPC (rds.tf) so it
// can reach RDS. Replaces the previous logs-only inline policy.
resource "aws_iam_role_policy_attachment" "lambda_vpc_access" {
  role       = aws_iam_role.lambda_iam_role.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole"
}
