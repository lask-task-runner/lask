# CloudFront exists here for a specific reason: Cognito only accepts
# HTTPS redirect URIs (http://localhost being the sole exception), and an
# S3 website endpoint is HTTP-only. It also supplies the certificate, so
# no custom domain or ACM certificate is needed.
resource "aws_cloudfront_origin_access_control" "frontend" {
  name                              = "webapp-frontend-oac"
  origin_access_control_origin_type = "s3"
  signing_behavior                  = "always"
  signing_protocol                  = "sigv4"
}

resource "aws_cloudfront_distribution" "frontend" {
  enabled             = true
  default_root_object = "index.html"
  comment             = "Lask webapp example frontend"

  origin {
    # The REST endpoint, not the website endpoint: OAC signs requests
    # with SigV4, which the website endpoint does not support.
    domain_name              = aws_s3_bucket.frontend.bucket_regional_domain_name
    origin_id                = "s3-frontend"
    origin_access_control_id = aws_cloudfront_origin_access_control.frontend.id
  }

  default_cache_behavior {
    target_origin_id       = "s3-frontend"
    viewer_protocol_policy = "redirect-to-https"
    allowed_methods        = ["GET", "HEAD", "OPTIONS"]
    cached_methods         = ["GET", "HEAD"]

    # Managed-CachingOptimized.
    cache_policy_id = "658327ea-f89d-4fab-a63d-7e88639e58f6"
  }

  # Single-page app fallback. With a REST origin S3 returns 403 for an
  # unknown key (it does not reveal existence), so both codes map to the
  # app shell and client-side routing takes over.
  custom_error_response {
    error_code         = 403
    response_code      = 200
    response_page_path = "/index.html"
  }

  custom_error_response {
    error_code         = 404
    response_code      = 200
    response_page_path = "/index.html"
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    cloudfront_default_certificate = true
  }

  # PriceClass_100 (North America / Europe) is the cheapest tier and is
  # enough for an example.
  price_class = "PriceClass_100"
}

output "website_url" {
  value = "https://${aws_cloudfront_distribution.frontend.domain_name}"
}

output "cloudfront_distribution_id" {
  value = aws_cloudfront_distribution.frontend.id
}
