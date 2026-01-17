# Infrastructure as Code


### Terraform

**Project Structure:**
```
terraform/
├── environments/
│   ├── dev/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── terraform.tfvars
│   ├── staging/
│   └── production/
├── modules/
│   ├── vpc/
│   ├── eks/
│   ├── rds/
│   └── s3/
└── shared/
    └── backend.tf
```

**Example Module (EKS Cluster):**
```hcl
# modules/eks/main.tf
resource "aws_eks_cluster" "main" {
  name     = var.cluster_name
  role_arn = aws_iam_role.cluster.arn
  version  = var.kubernetes_version

  vpc_config {
    subnet_ids              = var.subnet_ids
    endpoint_private_access = true
    endpoint_public_access  = var.public_access
    security_group_ids      = [aws_security_group.cluster.id]
  }

  enabled_cluster_log_types = ["api", "audit", "authenticator", "controllerManager", "scheduler"]

  tags = merge(
    var.tags,
    {
      Name = var.cluster_name
    }
  )
}

resource "aws_eks_node_group" "main" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "${var.cluster_name}-node-group"
  node_role_arn   = aws_iam_role.node.arn
  subnet_ids      = var.subnet_ids

  scaling_config {
    desired_size = var.desired_size
    max_size     = var.max_size
    min_size     = var.min_size
  }

  instance_types = var.instance_types
  capacity_type  = var.capacity_type

  update_config {
    max_unavailable = 1
  }

  tags = var.tags
}

# modules/eks/variables.tf
variable "cluster_name" {
  description = "Name of the EKS cluster"
  type        = string
}

variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
  default     = "1.28"
}

variable "subnet_ids" {
  description = "List of subnet IDs"
  type        = list(string)
}

variable "desired_size" {
  description = "Desired number of nodes"
  type        = number
  default     = 3
}

variable "max_size" {
  description = "Maximum number of nodes"
  type        = number
  default     = 5
}

variable "min_size" {
  description = "Minimum number of nodes"
  type        = number
  default     = 1
}

# modules/eks/outputs.tf
output "cluster_id" {
  value = aws_eks_cluster.main.id
}

output "cluster_endpoint" {
  value = aws_eks_cluster.main.endpoint
}

output "cluster_security_group_id" {
  value = aws_security_group.cluster.id
}
```

**Environment Configuration:**
```hcl
# environments/production/main.tf
terraform {
  required_version = ">= 1.5"
  
  backend "s3" {
    bucket         = "company-terraform-state"
    key            = "production/eks/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "terraform-locks"
  }
}

provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Environment = "production"
      ManagedBy   = "Terraform"
      Project     = "main-app"
    }
  }
}

module "vpc" {
  source = "../../modules/vpc"
  
  vpc_cidr           = "10.0.0.0/16"
  availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]
  environment        = "production"
}

module "eks" {
  source = "../../modules/eks"
  
  cluster_name       = "production-cluster"
  kubernetes_version = "1.28"
  subnet_ids         = module.vpc.private_subnet_ids
  desired_size       = 5
  max_size           = 10
  min_size           = 3
  instance_types     = ["t3.large"]
}
```

### Terraform Best Practices

- **State management**: Use remote state with locking (S3 + DynamoDB)
- **Module reusability**: Create reusable modules for common resources
- **Variable validation**: Add validation rules to variables
- **Output values**: Expose useful outputs for other modules
- **Workspace separation**: Use workspaces or directories for environments
- **Plan before apply**: Always review `terraform plan` before applying
- **Version pinning**: Pin provider and module versions

---
