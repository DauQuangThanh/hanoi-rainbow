# Infrastructure as Code

## Terraform for Alibaba Cloud

### Provider Configuration

**Basic Setup**
```hcl
terraform {
  required_providers {
    alicloud = {
      source  = "aliyun/alicloud"
      version = "~> 1.210"
    }
  }
  required_version = ">= 1.0"
}

provider "alicloud" {
  access_key = var.access_key
  secret_key = var.secret_key
  region     = var.region
}

# Better: Use assume role for security
provider "alicloud" {
  region = var.region
  assume_role {
    role_arn     = "acs:ram::123456789:role/TerraformRole"
    session_name = "terraform-session"
  }
}
```

**Environment Variables**
```bash
export ALICLOUD_ACCESS_KEY="your-access-key"
export ALICLOUD_SECRET_KEY="your-secret-key"
export ALICLOUD_REGION="cn-hangzhou"
```

### State Management

**Remote Backend (OSS)**
```hcl
terraform {
  backend "oss" {
    bucket              = "terraform-state-bucket"
    prefix              = "prod"
    key                 = "terraform.tfstate"
    region              = "cn-hangzhou"
    tablestore_endpoint = "https://tf-state-lock.cn-hangzhou.ots.aliyuncs.com"
    tablestore_table    = "terraform_state_lock"
  }
}
```

**State Locking with TableStore**
```hcl
# Create TableStore for locking
resource "alicloud_ots_instance" "state_lock" {
  name        = "tf-state-lock"
  description = "Terraform state lock"
  accessed_by = "Any"
  tags = {
    Environment = "production"
    Purpose     = "terraform-lock"
  }
}

resource "alicloud_ots_table" "state_lock" {
  instance_name = alicloud_ots_instance.state_lock.name
  table_name    = "terraform_state_lock"
  primary_key {
    name = "LockID"
    type = "String"
  }
  time_to_live = -1
  max_version  = 1
}
```

### VPC and Networking

**VPC Module**
```hcl
module "vpc" {
  source = "./modules/vpc"

  vpc_name       = "production-vpc"
  vpc_cidr       = "10.0.0.0/16"
  
  availability_zones = ["cn-hangzhou-h", "cn-hangzhou-i", "cn-hangzhou-j"]
  
  public_subnets  = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  private_subnets = ["10.0.11.0/24", "10.0.12.0/24", "10.0.13.0/24"]
  
  enable_nat_gateway = true
  enable_vpn_gateway = false
  
  tags = {
    Environment = "production"
    ManagedBy   = "terraform"
  }
}
```

**VPC Resource**
```hcl
resource "alicloud_vpc" "main" {
  vpc_name   = var.vpc_name
  cidr_block = var.vpc_cidr
  
  tags = merge(
    var.tags,
    {
      Name = var.vpc_name
    }
  )
}

resource "alicloud_vswitch" "public" {
  count             = length(var.public_subnets)
  vpc_id            = alicloud_vpc.main.id
  cidr_block        = var.public_subnets[count.index]
  zone_id           = var.availability_zones[count.index]
  vswitch_name      = "${var.vpc_name}-public-${count.index + 1}"
  
  tags = merge(
    var.tags,
    {
      Name = "${var.vpc_name}-public-${count.index + 1}"
      Type = "public"
    }
  )
}

resource "alicloud_vswitch" "private" {
  count             = length(var.private_subnets)
  vpc_id            = alicloud_vpc.main.id
  cidr_block        = var.private_subnets[count.index]
  zone_id           = var.availability_zones[count.index]
  vswitch_name      = "${var.vpc_name}-private-${count.index + 1}"
  
  tags = merge(
    var.tags,
    {
      Name = "${var.vpc_name}-private-${count.index + 1}"
      Type = "private"
    }
  )
}

resource "alicloud_nat_gateway" "main" {
  count              = var.enable_nat_gateway ? 1 : 0
  vpc_id             = alicloud_vpc.main.id
  nat_gateway_name   = "${var.vpc_name}-nat"
  nat_type           = "Enhanced"
  vswitch_id         = alicloud_vswitch.public[0].id
  payment_type       = "PayAsYouGo"
  
  tags = merge(
    var.tags,
    {
      Name = "${var.vpc_name}-nat"
    }
  )
}

resource "alicloud_eip_address" "nat" {
  count                = var.enable_nat_gateway ? 1 : 0
  address_name         = "${var.vpc_name}-nat-eip"
  internet_charge_type = "PayByTraffic"
  bandwidth            = "100"
}

resource "alicloud_eip_association" "nat" {
  count         = var.enable_nat_gateway ? 1 : 0
  allocation_id = alicloud_eip_address.nat[0].id
  instance_id   = alicloud_nat_gateway.main[0].id
}

resource "alicloud_snat_entry" "private" {
  count             = var.enable_nat_gateway ? length(var.private_subnets) : 0
  snat_table_id     = alicloud_nat_gateway.main[0].snat_table_ids
  source_vswitch_id = alicloud_vswitch.private[count.index].id
  snat_ip           = alicloud_eip_address.nat[0].ip_address
}
```

### ECS Instance Module

**Module Structure**
```hcl
# modules/ecs/main.tf
resource "alicloud_security_group" "main" {
  name        = "${var.name_prefix}-sg"
  vpc_id      = var.vpc_id
  description = "Security group for ${var.name_prefix}"
  
  tags = var.tags
}

resource "alicloud_security_group_rule" "allow_ssh" {
  type              = "ingress"
  ip_protocol       = "tcp"
  port_range        = "22/22"
  security_group_id = alicloud_security_group.main.id
  cidr_ip           = var.allowed_ssh_cidr
}

resource "alicloud_instance" "main" {
  count                = var.instance_count
  instance_name        = "${var.name_prefix}-${count.index + 1}"
  instance_type        = var.instance_type
  image_id             = var.image_id
  vswitch_id           = var.vswitch_ids[count.index % length(var.vswitch_ids)]
  security_groups      = [alicloud_security_group.main.id]
  internet_charge_type = var.allocate_public_ip ? "PayByTraffic" : null
  internet_max_bandwidth_out = var.allocate_public_ip ? var.internet_bandwidth : 0
  
  system_disk_category = var.system_disk_type
  system_disk_size     = var.system_disk_size
  
  dynamic "data_disks" {
    for_each = var.data_disks
    content {
      category = data_disks.value.type
      size     = data_disks.value.size
      name     = "${var.name_prefix}-data-${data_disks.key}"
    }
  }
  
  user_data = var.user_data
  
  tags = merge(
    var.tags,
    {
      Name  = "${var.name_prefix}-${count.index + 1}"
      Index = count.index + 1
    }
  )
}

# modules/ecs/variables.tf
variable "name_prefix" {
  type        = string
  description = "Prefix for resource names"
}

variable "vpc_id" {
  type        = string
  description = "VPC ID"
}

variable "vswitch_ids" {
  type        = list(string)
  description = "List of VSwitch IDs"
}

variable "instance_count" {
  type        = number
  default     = 1
  description = "Number of instances to create"
}

variable "instance_type" {
  type        = string
  description = "ECS instance type"
}

variable "image_id" {
  type        = string
  description = "Image ID"
}

variable "system_disk_type" {
  type        = string
  default     = "cloud_essd"
  description = "System disk type"
}

variable "system_disk_size" {
  type        = number
  default     = 40
  description = "System disk size in GB"
}

variable "data_disks" {
  type = list(object({
    type = string
    size = number
  }))
  default     = []
  description = "List of data disks"
}

variable "allocate_public_ip" {
  type        = bool
  default     = false
  description = "Allocate public IP"
}

variable "internet_bandwidth" {
  type        = number
  default     = 10
  description = "Internet bandwidth in Mbps"
}

variable "allowed_ssh_cidr" {
  type        = string
  default     = "0.0.0.0/0"
  description = "CIDR for SSH access"
}

variable "user_data" {
  type        = string
  default     = ""
  description = "User data script"
}

variable "tags" {
  type        = map(string)
  default     = {}
  description = "Tags to apply to resources"
}

# modules/ecs/outputs.tf
output "instance_ids" {
  value       = alicloud_instance.main[*].id
  description = "List of instance IDs"
}

output "private_ips" {
  value       = alicloud_instance.main[*].private_ip
  description = "List of private IPs"
}

output "public_ips" {
  value       = alicloud_instance.main[*].public_ip
  description = "List of public IPs"
}

output "security_group_id" {
  value       = alicloud_security_group.main.id
  description = "Security group ID"
}
```

**Usage Example**
```hcl
module "web_servers" {
  source = "./modules/ecs"
  
  name_prefix         = "web"
  vpc_id              = module.vpc.vpc_id
  vswitch_ids         = module.vpc.public_vswitch_ids
  instance_count      = 3
  instance_type       = "ecs.g7.large"
  image_id            = "ubuntu_20_04_x64_20G_alibase_20230208.vhd"
  system_disk_size    = 40
  allocate_public_ip  = true
  internet_bandwidth  = 100
  
  data_disks = [
    {
      type = "cloud_essd"
      size = 100
    }
  ]
  
  user_data = templatefile("${path.module}/templates/web_init.sh", {
    app_version = var.app_version
    environment = "production"
  })
  
  tags = {
    Environment = "production"
    Application = "web"
    ManagedBy   = "terraform"
  }
}
```

### RDS Module

**RDS Instance**
```hcl
# modules/rds/main.tf
resource "alicloud_db_instance" "main" {
  engine               = var.engine
  engine_version       = var.engine_version
  instance_type        = var.instance_type
  instance_storage     = var.storage_size
  instance_storage_type = var.storage_type
  instance_name        = var.instance_name
  vswitch_id           = var.vswitch_id
  security_ips         = var.security_ips
  
  # High Availability
  zone_id              = var.primary_zone_id
  zone_id_slave_a      = var.secondary_zone_id
  instance_charge_type = var.charge_type
  
  # Backup configuration
  backup_period        = var.backup_period
  backup_time          = var.backup_time
  backup_retention_period = var.backup_retention_period
  
  # Maintenance window
  maintenance_window   = var.maintenance_window
  
  # Monitoring
  monitoring_period    = 60
  
  # Security
  ssl_action           = var.enable_ssl ? "Open" : "Close"
  
  tags = var.tags
}

resource "alicloud_db_database" "main" {
  for_each      = toset(var.databases)
  instance_id   = alicloud_db_instance.main.id
  name          = each.value
  character_set = var.character_set
}

resource "alicloud_db_account" "main" {
  for_each     = var.accounts
  db_instance_id = alicloud_db_instance.main.id
  account_name = each.key
  account_password = each.value.password
  account_type     = each.value.type
}

resource "alicloud_db_account_privilege" "main" {
  for_each = {
    for item in flatten([
      for account, config in var.accounts : [
        for db in config.databases : {
          account = account
          database = db
          privilege = config.privilege
        }
      ]
    ]) : "${item.account}-${item.database}" => item
  }
  
  instance_id  = alicloud_db_instance.main.id
  account_name = each.value.account
  db_names     = [each.value.database]
  privilege    = each.value.privilege
  
  depends_on = [
    alicloud_db_database.main,
    alicloud_db_account.main
  ]
}

resource "alicloud_db_readonly_instance" "replica" {
  count               = var.readonly_instance_count
  master_db_instance_id = alicloud_db_instance.main.id
  engine_version      = alicloud_db_instance.main.engine_version
  instance_type       = var.readonly_instance_type
  instance_storage    = var.storage_size
  instance_name       = "${var.instance_name}-ro-${count.index + 1}"
  vswitch_id          = var.vswitch_id
  zone_id             = var.readonly_zone_ids[count.index % length(var.readonly_zone_ids)]
  
  tags = merge(
    var.tags,
    {
      Type = "readonly"
    }
  )
}

# modules/rds/variables.tf
variable "instance_name" {
  type        = string
  description = "RDS instance name"
}

variable "engine" {
  type        = string
  description = "Database engine (MySQL, PostgreSQL, etc.)"
}

variable "engine_version" {
  type        = string
  description = "Engine version"
}

variable "instance_type" {
  type        = string
  description = "Instance type"
}

variable "storage_size" {
  type        = number
  description = "Storage size in GB"
}

variable "storage_type" {
  type        = string
  default     = "cloud_essd"
  description = "Storage type"
}

variable "vswitch_id" {
  type        = string
  description = "VSwitch ID"
}

variable "primary_zone_id" {
  type        = string
  description = "Primary zone ID"
}

variable "secondary_zone_id" {
  type        = string
  default     = ""
  description = "Secondary zone ID for HA"
}

variable "security_ips" {
  type        = list(string)
  description = "IP whitelist"
}

variable "charge_type" {
  type        = string
  default     = "Postpaid"
  description = "Charge type (Postpaid or Prepaid)"
}

variable "backup_period" {
  type        = list(string)
  default     = ["Monday", "Wednesday", "Friday"]
  description = "Backup days"
}

variable "backup_time" {
  type        = string
  default     = "02:00Z-03:00Z"
  description = "Backup time window"
}

variable "backup_retention_period" {
  type        = number
  default     = 7
  description = "Backup retention in days"
}

variable "maintenance_window" {
  type        = string
  default     = "Mon:03:00Z-Mon:04:00Z"
  description = "Maintenance window"
}

variable "enable_ssl" {
  type        = bool
  default     = true
  description = "Enable SSL connection"
}

variable "character_set" {
  type        = string
  default     = "utf8mb4"
  description = "Character set for databases"
}

variable "databases" {
  type        = list(string)
  default     = []
  description = "List of databases to create"
}

variable "accounts" {
  type = map(object({
    password  = string
    type      = string
    privilege = string
    databases = list(string)
  }))
  default     = {}
  description = "Database accounts"
}

variable "readonly_instance_count" {
  type        = number
  default     = 0
  description = "Number of read-only instances"
}

variable "readonly_instance_type" {
  type        = string
  default     = ""
  description = "Read-only instance type"
}

variable "readonly_zone_ids" {
  type        = list(string)
  default     = []
  description = "Zone IDs for read-only instances"
}

variable "tags" {
  type        = map(string)
  default     = {}
  description = "Tags"
}
```

**Usage Example**
```hcl
module "database" {
  source = "./modules/rds"
  
  instance_name      = "production-mysql"
  engine             = "MySQL"
  engine_version     = "8.0"
  instance_type      = "mysql.n4.medium.1"
  storage_size       = 100
  storage_type       = "cloud_essd"
  vswitch_id         = module.vpc.private_vswitch_ids[0]
  primary_zone_id    = "cn-hangzhou-h"
  secondary_zone_id  = "cn-hangzhou-i"
  
  security_ips = [
    module.vpc.vpc_cidr,
    "172.16.0.0/16"
  ]
  
  databases = ["app_db", "analytics_db"]
  
  accounts = {
    app_user = {
      password  = var.db_app_password
      type      = "Normal"
      privilege = "ReadWrite"
      databases = ["app_db"]
    }
    analytics_user = {
      password  = var.db_analytics_password
      type      = "Normal"
      privilege = "ReadOnly"
      databases = ["analytics_db"]
    }
    admin = {
      password  = var.db_admin_password
      type      = "Super"
      privilege = "DBOwner"
      databases = ["app_db", "analytics_db"]
    }
  }
  
  readonly_instance_count = 2
  readonly_instance_type  = "mysql.n4.medium.1"
  readonly_zone_ids       = ["cn-hangzhou-i", "cn-hangzhou-j"]
  
  backup_period            = ["Monday", "Wednesday", "Friday", "Sunday"]
  backup_time              = "02:00Z-03:00Z"
  backup_retention_period  = 30
  
  tags = {
    Environment = "production"
    ManagedBy   = "terraform"
  }
}
```

### Best Practices

**Project Structure**
```
terraform/
├── environments/
│   ├── dev/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── terraform.tfvars
│   │   └── backend.tf
│   ├── staging/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── terraform.tfvars
│   │   └── backend.tf
│   └── prod/
│       ├── main.tf
│       ├── variables.tf
│       ├── terraform.tfvars
│       └── backend.tf
├── modules/
│   ├── vpc/
│   ├── ecs/
│   ├── rds/
│   ├── slb/
│   └── oss/
└── shared/
    ├── variables.tf
    └── outputs.tf
```

**Variable Management**
```hcl
# Use sensitive variables
variable "db_password" {
  type      = string
  sensitive = true
}

# Use validation
variable "environment" {
  type = string
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

# Use descriptions
variable "instance_type" {
  type        = string
  description = "ECS instance type (e.g., ecs.g7.large)"
}
```

**Resource Naming**
```hcl
locals {
  name_prefix = "${var.project}-${var.environment}"
  
  common_tags = {
    Project     = var.project
    Environment = var.environment
    ManagedBy   = "terraform"
    CreatedAt   = timestamp()
  }
}

resource "alicloud_vpc" "main" {
  vpc_name = "${local.name_prefix}-vpc"
  tags     = local.common_tags
}
```

**State Management**
- Use remote state with OSS
- Enable state locking
- Use workspaces for environments
- Never commit .tfstate files
- Regular state backups

**Module Design**
- Keep modules focused and reusable
- Use semantic versioning for modules
- Document inputs and outputs
- Include examples in module README
- Test modules independently

## ROS (Resource Orchestration Service)

### Template Structure

**Basic Template**
```yaml
ROSTemplateFormatVersion: '2015-09-01'
Transform: 'Aliyun::Serverless-2018-04-03'
Description: 'Production infrastructure stack'

Parameters:
  VpcCidr:
    Type: String
    Default: '10.0.0.0/16'
    Description: 'VPC CIDR block'
  
  Environment:
    Type: String
    AllowedValues:
      - dev
      - staging
      - prod
    Default: prod
    Description: 'Environment name'

Resources:
  Vpc:
    Type: 'ALIYUN::ECS::VPC'
    Properties:
      CidrBlock: !Ref VpcCidr
      VpcName: !Sub '${Environment}-vpc'
      Tags:
        - Key: Environment
          Value: !Ref Environment
        - Key: ManagedBy
          Value: ROS

  VSwitch1:
    Type: 'ALIYUN::ECS::VSwitch'
    Properties:
      VpcId: !Ref Vpc
      ZoneId: !Select ['0', !GetAZs '']
      CidrBlock: '10.0.1.0/24'
      VSwitchName: !Sub '${Environment}-vswitch-1'

  VSwitch2:
    Type: 'ALIYUN::ECS::VSwitch'
    Properties:
      VpcId: !Ref Vpc
      ZoneId: !Select ['1', !GetAZs '']
      CidrBlock: '10.0.2.0/24'
      VSwitchName: !Sub '${Environment}-vswitch-2'

  SecurityGroup:
    Type: 'ALIYUN::ECS::SecurityGroup'
    Properties:
      VpcId: !Ref Vpc
      SecurityGroupName: !Sub '${Environment}-sg'
      SecurityGroupIngress:
        - PortRange: 22/22
          Priority: 1
          SourceCidrIp: 0.0.0.0/0
          IpProtocol: tcp
        - PortRange: 80/80
          Priority: 1
          SourceCidrIp: 0.0.0.0/0
          IpProtocol: tcp
        - PortRange: 443/443
          Priority: 1
          SourceCidrIp: 0.0.0.0/0
          IpProtocol: tcp

Outputs:
  VpcId:
    Description: 'VPC ID'
    Value: !Ref Vpc
  
  VSwitchIds:
    Description: 'VSwitch IDs'
    Value: !Join
      - ','
      - - !Ref VSwitch1
        - !Ref VSwitch2
  
  SecurityGroupId:
    Description: 'Security Group ID'
    Value: !Ref SecurityGroup
```

**Nested Stacks**
```yaml
# Parent stack
Resources:
  NetworkStack:
    Type: 'ALIYUN::ROS::Stack'
    Properties:
      TemplateURL: 'oss://my-bucket/templates/network.yaml'
      Parameters:
        VpcCidr: !Ref VpcCidr
        Environment: !Ref Environment
  
  ComputeStack:
    Type: 'ALIYUN::ROS::Stack'
    Properties:
      TemplateURL: 'oss://my-bucket/templates/compute.yaml'
      Parameters:
        VpcId: !GetAtt NetworkStack.Outputs.VpcId
        VSwitchIds: !GetAtt NetworkStack.Outputs.VSwitchIds
```

### Best Practices

**Template Organization**
- Break large templates into nested stacks
- Use parameters for flexibility
- Document all parameters and resources
- Version control templates in Git
- Store templates in OSS for reuse

**Change Sets**
- Preview changes before applying
- Review all modifications
- Test in non-production first
- Have rollback plan ready

**Stack Policies**
- Protect critical resources
- Prevent accidental deletion
- Use policies for production stacks
