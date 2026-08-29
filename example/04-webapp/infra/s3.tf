# Private origin bucket for the React frontend (web/). It is not public:
# CloudFront (cloudfront.tf) is the only reader, via Origin Access
# Control, and is also what supplies HTTPS -- which Cognito requires of
# its redirect URIs.
data "aws_caller_identity" "current" {}

resource "aws_s3_bucket" "frontend" {
  bucket = "lask-webapp-frontend-${data.aws_caller_identity.current.account_id}"

  # The bucket is populated by `aws s3 sync` (main.lask), outside
  # Terraform's own state, so `terraform destroy` needs this to remove it
  # non-empty.
  force_destroy = true
}

resource "aws_s3_bucket_public_access_block" "frontend" {
  bucket = aws_s3_bucket.frontend.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# Readable only by this CloudFront distribution: the service principal
# narrowed to the distribution ARN, so another account's distribution
# cannot be pointed at this bucket.
resource "aws_s3_bucket_policy" "frontend_cloudfront_read" {
  bucket = aws_s3_bucket.frontend.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid       = "AllowCloudFrontServicePrincipalReadOnly"
        Effect    = "Allow"
        Principal = { Service = "cloudfront.amazonaws.com" }
        Action    = "s3:GetObject"
        Resource  = "${aws_s3_bucket.frontend.arn}/*"
        Condition = {
          StringEquals = {
            "AWS:SourceArn" = aws_cloudfront_distribution.frontend.arn
          }
        }
      }
    ]
  })

  depends_on = [aws_s3_bucket_public_access_block.frontend]
}

output "frontend_bucket" {
  value = aws_s3_bucket.frontend.id
}
