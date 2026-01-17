# Terraform for Oracle Cloud Infrastructure

## Provider Configuration

### Basic Provider Setup

```hcl
terraform {
  required_providers {
    oci = {
      source  = "oracle/oci"
      version = "~> 5.0"
    }
  }
  required_version = ">= 1.0"
}

provider "oci" {
  region           = var.region
  tenancy_ocid     = var.tenancy_ocid
  user_ocid        = var.user_ocid
  fingerprint      = var.fingerprint
  private_key_path = var.private_key_path
}
```

### Using Instance Principal Authentication

**For compute instances with dynamic groups**:
```hcl
provider "oci" {
  region = var.region
  auth   = "InstancePrincipal"
}
```

### Multi-Region Setup

```hcl
provider "oci" {
  alias            = "home"
  region           = "us-ashburn-1"
  tenancy_ocid     = var.tenancy_ocid
  user_ocid        = var.user_ocid
  fingerprint      = var.fingerprint
  private_key_path = var.private_key_path
}

provider "oci" {
  alias            = "dr"
  region           = "us-phoenix-1"
  tenancy_ocid     = var.tenancy_ocid
  user_ocid        = var.user_ocid
  fingerprint      = var.fingerprint
  private_key_path = var.private_key_path
}
```

## Remote State Configuration

### OCI Object Storage Backend

```hcl
terraform {
  backend "http" {
    address = "https://objectstorage.us-ashburn-1.oraclecloud.com/n/your-namespace/b/terraform-state/o/prod/terraform.tfstate"
    update_method = "PUT"
  }
}
```

### S3-Compatible Backend

```hcl
terraform {
  backend "s3" {
    bucket   = "terraform-state"
    key      = "prod/terraform.tfstate"
    region   = "us-ashburn-1"
    endpoint = "https://your-namespace.compat.objectstorage.us-ashburn-1.oraclecloud.com"
    
    skip_region_validation      = true
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    force_path_style           = true
  }
}
```

## Variables Configuration

### variables.tf

```hcl
variable "tenancy_ocid" {
  description = "OCID of the tenancy"
  type        = string
}

variable "user_ocid" {
  description = "OCID of the user"
  type        = string
}

variable "fingerprint" {
  description = "API key fingerprint"
  type        = string
}

variable "private_key_path" {
  description = "Path to private key"
  type        = string
  default     = "~/.oci/oci_api_key.pem"
}

variable "region" {
  description = "OCI region"
  type        = string
  default     = "us-ashburn-1"
}

variable "compartment_id" {
  description = "Compartment OCID"
  type        = string
}

variable "availability_domain" {
  description = "Availability domain"
  type        = string
}

variable "ssh_public_key" {
  description = "SSH public key"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}
```

### terraform.tfvars

```hcl
tenancy_ocid     = "ocid1.tenancy.oc1..aaaa..."
user_ocid        = "ocid1.user.oc1..aaaa..."
fingerprint      = "12:34:56:78:90:ab:cd:ef"
private_key_path = "~/.oci/oci_api_key.pem"
region           = "us-ashburn-1"
compartment_id   = "ocid1.compartment.oc1..aaaa..."
ssh_public_key   = "ssh-rsa AAAAB3NzaC1..."
environment      = "prod"
```

## Data Sources

### Common Data Sources

```hcl
# Get availability domains
data "oci_identity_availability_domains" "ads" {
  compartment_id = var.tenancy_ocid
}

# Get compute shapes
data "oci_core_shapes" "available_shapes" {
  compartment_id = var.compartment_id
  
  filter {
    name   = "name"
    values = ["VM.Standard.E4.Flex"]
  }
}

# Get latest Oracle Linux image
data "oci_core_images" "oracle_linux" {
  compartment_id           = var.compartment_id
  operating_system         = "Oracle Linux"
  operating_system_version = "8"
  shape                    = "VM.Standard.E4.Flex"
  sort_by                  = "TIMECREATED"
  sort_order               = "DESC"
}

# Get fault domains
data "oci_identity_fault_domains" "fault_domains" {
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  compartment_id      = var.compartment_id
}

# Get OCI services for service gateway
data "oci_core_services" "all_services" {
  filter {
    name   = "name"
    values = ["All .* Services In Oracle Services Network"]
    regex  = true
  }
}
```

## Module Structure

### Project Layout

```
terraform/
├── environments/
│   ├── dev/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── terraform.tfvars
│   ├── staging/
│   └── prod/
├── modules/
│   ├── compute/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── outputs.tf
│   ├── networking/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   └── outputs.tf
│   └── database/
│       ├── main.tf
│       ├── variables.tf
│       └── outputs.tf
└── README.md
```

### Compute Module Example

**modules/compute/main.tf**:
```hcl
resource "oci_core_instance" "this" {
  availability_domain = var.availability_domain
  compartment_id      = var.compartment_id
  shape               = var.shape
  display_name        = var.instance_name

  shape_config {
    ocpus         = var.ocpus
    memory_in_gbs = var.memory_in_gbs
  }

  create_vnic_details {
    subnet_id                 = var.subnet_id
    assign_public_ip          = var.assign_public_ip
    display_name              = "${var.instance_name}-vnic"
    hostname_label            = var.hostname_label
    nsg_ids                   = var.nsg_ids
    skip_source_dest_check    = var.skip_source_dest_check
  }

  source_details {
    source_type             = "image"
    source_id               = var.source_image_id
    boot_volume_size_in_gbs = var.boot_volume_size
  }

  metadata = {
    ssh_authorized_keys = var.ssh_public_key
    user_data           = var.user_data_base64
  }

  freeform_tags = var.tags
  defined_tags  = var.defined_tags

  lifecycle {
    ignore_changes = [
      source_details[0].source_id,
      metadata["ssh_authorized_keys"]
    ]
  }
}

resource "oci_core_volume" "data_volume" {
  count               = var.create_data_volume ? 1 : 0
  availability_domain = var.availability_domain
  compartment_id      = var.compartment_id
  display_name        = "${var.instance_name}-data"
  size_in_gbs         = var.data_volume_size
  vpus_per_gb         = var.data_volume_vpus_per_gb
}

resource "oci_core_volume_attachment" "data_volume_attachment" {
  count           = var.create_data_volume ? 1 : 0
  attachment_type = "paravirtualized"
  instance_id     = oci_core_instance.this.id
  volume_id       = oci_core_volume.data_volume[0].id
  device          = "/dev/oracleoci/oraclevdb"
}
```

**modules/compute/variables.tf**:
```hcl
variable "compartment_id" {
  description = "Compartment OCID"
  type        = string
}

variable "availability_domain" {
  description = "Availability domain"
  type        = string
}

variable "instance_name" {
  description = "Instance display name"
  type        = string
}

variable "shape" {
  description = "Instance shape"
  type        = string
  default     = "VM.Standard.E4.Flex"
}

variable "ocpus" {
  description = "Number of OCPUs"
  type        = number
  default     = 2
}

variable "memory_in_gbs" {
  description = "Memory in GB"
  type        = number
  default     = 16
}

variable "subnet_id" {
  description = "Subnet OCID"
  type        = string
}

variable "assign_public_ip" {
  description = "Assign public IP"
  type        = bool
  default     = false
}

variable "source_image_id" {
  description = "Source image OCID"
  type        = string
}

variable "ssh_public_key" {
  description = "SSH public key"
  type        = string
}

variable "boot_volume_size" {
  description = "Boot volume size in GB"
  type        = number
  default     = 50
}

variable "create_data_volume" {
  description = "Create additional data volume"
  type        = bool
  default     = false
}

variable "data_volume_size" {
  description = "Data volume size in GB"
  type        = number
  default     = 100
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
```

**modules/compute/outputs.tf**:
```hcl
output "instance_id" {
  description = "Instance OCID"
  value       = oci_core_instance.this.id
}

output "private_ip" {
  description = "Private IP address"
  value       = oci_core_instance.this.private_ip
}

output "public_ip" {
  description = "Public IP address"
  value       = oci_core_instance.this.public_ip
}

output "instance_state" {
  description = "Instance state"
  value       = oci_core_instance.this.state
}
```

### Using the Module

```hcl
module "web_server" {
  source = "../../modules/compute"

  compartment_id      = var.compartment_id
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  instance_name       = "web-server-01"
  shape               = "VM.Standard.E4.Flex"
  ocpus               = 2
  memory_in_gbs       = 16
  subnet_id           = module.networking.public_subnet_id
  assign_public_ip    = true
  source_image_id     = data.oci_core_images.oracle_linux.images[0].id
  ssh_public_key      = var.ssh_public_key
  boot_volume_size    = 100
  create_data_volume  = true
  data_volume_size    = 200

  tags = {
    Environment = "Production"
    Application = "WebServer"
    ManagedBy   = "Terraform"
  }
}
```

## Common Patterns

### Three-Tier Architecture

```hcl
# VCN and Networking
module "networking" {
  source = "./modules/networking"

  compartment_id = var.compartment_id
  vcn_cidr       = "10.0.0.0/16"
  vcn_name       = "three-tier-vcn"

  public_subnet_cidr  = "10.0.1.0/24"
  app_subnet_cidr     = "10.0.2.0/24"
  db_subnet_cidr      = "10.0.3.0/24"
}

# Web Tier (Public)
module "web_servers" {
  source = "./modules/compute"
  count  = 2

  compartment_id      = var.compartment_id
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[count.index % 2].name
  instance_name       = "web-${count.index + 1}"
  subnet_id           = module.networking.public_subnet_id
  assign_public_ip    = true
  source_image_id     = data.oci_core_images.oracle_linux.images[0].id
  ssh_public_key      = var.ssh_public_key
}

# App Tier (Private)
module "app_servers" {
  source = "./modules/compute"
  count  = 3

  compartment_id      = var.compartment_id
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[count.index % 2].name
  instance_name       = "app-${count.index + 1}"
  subnet_id           = module.networking.app_subnet_id
  assign_public_ip    = false
  source_image_id     = data.oci_core_images.oracle_linux.images[0].id
  ssh_public_key      = var.ssh_public_key
}

# Database
module "database" {
  source = "./modules/database"

  compartment_id = var.compartment_id
  db_name        = "proddb"
  admin_password = var.db_admin_password
  subnet_id      = module.networking.db_subnet_id
  cpu_core_count = 2
  storage_in_tbs = 1
}
```

### Conditional Resource Creation

```hcl
resource "oci_core_instance" "app_server" {
  count = var.create_instance ? 1 : 0

  # configuration...
}

resource "oci_load_balancer_load_balancer" "app_lb" {
  count = var.environment == "prod" ? 1 : 0

  # configuration...
}
```

### Dynamic Blocks

```hcl
resource "oci_core_security_list" "app_sl" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main.id

  dynamic "ingress_security_rules" {
    for_each = var.allowed_ingress_ports
    content {
      protocol    = "6"
      source      = "10.0.0.0/16"
      source_type = "CIDR_BLOCK"
      stateless   = false

      tcp_options {
        min = ingress_security_rules.value
        max = ingress_security_rules.value
      }
    }
  }
}
```

## State Management

### Workspaces

```bash
# Create workspace
terraform workspace new prod

# List workspaces
terraform workspace list

# Switch workspace
terraform workspace select prod

# Show current workspace
terraform workspace show
```

### Import Existing Resources

```bash
# Import VCN
terraform import oci_core_vcn.main ocid1.vcn.oc1..aaaa...

# Import instance
terraform import oci_core_instance.web ocid1.instance.oc1..aaaa...

# Import subnet
terraform import oci_core_subnet.public ocid1.subnet.oc1..aaaa...
```

## Terraform Commands

### Common Workflow

```bash
# Initialize
terraform init

# Validate configuration
terraform validate

# Format code
terraform fmt -recursive

# Plan changes
terraform plan -out=tfplan

# Apply changes
terraform apply tfplan

# Show current state
terraform show

# List resources
terraform state list

# Destroy resources
terraform destroy
```

### Targeted Operations

```bash
# Plan specific resource
terraform plan -target=module.web_server

# Apply specific resource
terraform apply -target=module.web_server

# Destroy specific resource
terraform destroy -target=module.web_server
```

### State Operations

```bash
# Show resource
terraform state show oci_core_instance.web

# Move resource
terraform state mv oci_core_instance.old oci_core_instance.new

# Remove resource from state
terraform state rm oci_core_instance.old

# Pull remote state
terraform state pull > terraform.tfstate.backup

# Push state
terraform state push terraform.tfstate
```

## Best Practices

### 1. Version Control

```hcl
terraform {
  required_version = "~> 1.5"
  
  required_providers {
    oci = {
      source  = "oracle/oci"
      version = "~> 5.20"
    }
  }
}
```

### 2. Use Variables

```hcl
# Good
cidr_block = var.vcn_cidr

# Avoid
cidr_block = "10.0.0.0/16"
```

### 3. Output Important Values

```hcl
output "vcn_id" {
  description = "VCN OCID"
  value       = oci_core_vcn.main.id
}

output "load_balancer_ip" {
  description = "Load balancer public IP"
  value       = oci_load_balancer_load_balancer.app_lb.ip_addresses[0].ip_address
  sensitive   = false
}
```

### 4. Use Locals for Computed Values

```hcl
locals {
  common_tags = {
    Environment = var.environment
    ManagedBy   = "Terraform"
    CostCenter  = var.cost_center
  }
  
  instance_count = var.environment == "prod" ? 3 : 1
  
  db_name = "${var.project_name}-${var.environment}-db"
}
```

### 5. Implement Lifecycle Rules

```hcl
resource "oci_core_instance" "web" {
  # configuration...

  lifecycle {
    create_before_destroy = true
    prevent_destroy       = true
    ignore_changes        = [
      metadata["ssh_authorized_keys"],
      source_details[0].source_id
    ]
  }
}
```

### 6. Use Data Sources

```hcl
# Instead of hardcoding image OCID
data "oci_core_images" "latest_ol8" {
  compartment_id           = var.compartment_id
  operating_system         = "Oracle Linux"
  operating_system_version = "8"
  sort_by                  = "TIMECREATED"
  sort_order               = "DESC"
}

resource "oci_core_instance" "web" {
  source_details {
    source_id = data.oci_core_images.latest_ol8.images[0].id
  }
}
```

### 7. Tag Everything

```hcl
freeform_tags = merge(
  local.common_tags,
  {
    Name = "web-server-01"
    Role = "WebServer"
  }
)
```

### 8. Use Remote State

```hcl
data "terraform_remote_state" "networking" {
  backend = "http"
  config = {
    address = "https://objectstorage.../networking/terraform.tfstate"
  }
}

resource "oci_core_instance" "app" {
  subnet_id = data.terraform_remote_state.networking.outputs.app_subnet_id
}
```

## Troubleshooting

### Common Issues

**Issue**: 409 Conflict - Resource already exists
**Solution**: Import existing resource or use different name

**Issue**: Service limit exceeded
**Solution**: Request service limit increase or clean up unused resources

**Issue**: Authentication error
**Solution**: Verify API key configuration and permissions

**Issue**: Terraform state lock
**Solution**: Force unlock (use with caution)
```bash
terraform force-unlock LOCK_ID
```

### Debug Mode

```bash
# Enable debug logging
export TF_LOG=DEBUG
export TF_LOG_PATH=./terraform-debug.log

terraform apply
```
