variable "app_version" {
  type    = string
  default = "0.0.0"
}

resource "terraform_data" "release" {
  input = var.app_version
}

output "deployed_version" {
  value = terraform_data.release.output
}
