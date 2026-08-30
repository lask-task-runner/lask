# Uses the account's default VPC/subnets to keep this example's networking
# minimal and cheap. For a production setup, use dedicated private subnets
# and a NAT gateway (or VPC endpoints) instead.
data "aws_vpc" "default" {
  default = true
}

data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

# Resolves to the latest available minor version for Postgres 16, so this
# example doesn't go stale as AWS retires old minor versions.
data "aws_rds_engine_version" "postgres" {
  engine       = "postgres"
  version      = "16"
  default_only = true
}

resource "random_password" "db" {
  length  = 20
  special = false
}

# Attached to the Lambda function (lambda.tf) so it can reach the database.
resource "aws_security_group" "lambda" {
  name        = "webapp-lambda-sg"
  description = "Outbound access for the webapp Lambda function"
  vpc_id      = data.aws_vpc.default.id

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "rds" {
  name        = "webapp-rds-sg"
  description = "Allow Postgres access from the webapp Lambda function only"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.lambda.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_db_subnet_group" "webapp" {
  name       = "webapp-db-subnet-group"
  subnet_ids = data.aws_subnets.default.ids
}

resource "aws_db_instance" "webapp" {
  identifier        = "webapp-db"
  engine            = "postgres"
  engine_version    = data.aws_rds_engine_version.postgres.version
  instance_class    = "db.t4g.micro"
  allocated_storage = 20
  storage_type      = "gp3"

  db_name  = "webapp"
  username = "webapp_admin"
  password = random_password.db.result

  db_subnet_group_name   = aws_db_subnet_group.webapp.name
  vpc_security_group_ids = [aws_security_group.rds.id]
  publicly_accessible    = false
  multi_az               = false

  # Kept minimal/cheap for an example environment. Raise
  # backup_retention_period and enable deletion_protection for anything real.
  backup_retention_period = 0
  skip_final_snapshot     = true
  deletion_protection     = false
  apply_immediately       = true
}
