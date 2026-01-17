# CLI & Automation

## Table of Contents
- [IBM Cloud CLI](#ibm-cloud-cli)
- [Terraform Provider](#terraform-provider)
- [Schematics](#schematics)
- [Resource Manager APIs](#resource-manager-apis)
- [CI/CD Integration](#cicd-integration)

---

## IBM Cloud CLI

### Installation

```bash
# macOS
curl -fsSL https://clis.cloud.ibm.com/install/osx | sh

# Linux
curl -fsSL https://clis.cloud.ibm.com/install/linux | sh

# Windows (PowerShell as Admin)
iex (New-Object Net.WebClient).DownloadString('https://clis.cloud.ibm.com/install/powershell')

# Verify
ibmcloud version
```

### Essential Plugins

```bash
# Container Service (IKS)
ibmcloud plugin install container-service

# Container Registry
ibmcloud plugin install container-registry

# VPC Infrastructure
ibmcloud plugin install vpc-infrastructure

# Cloud Object Storage
ibmcloud plugin install cloud-object-storage

# Databases
ibmcloud plugin install cloud-databases

# Schematics
ibmcloud plugin install schematics

# List installed plugins
ibmcloud plugin list

# Update all plugins
ibmcloud plugin update --all
```

### Common Commands

```bash
# Login
ibmcloud login
ibmcloud login --sso
ibmcloud login --apikey @apikey.txt

# Target region and resource group
ibmcloud target -r us-south -g production-rg

# List resources
ibmcloud resource service-instances
ibmcloud is vpcs
ibmcloud ks clusters

# Create resource
ibmcloud resource service-instance-create my-cos \
  cloud-object-storage standard global

# Update resource
ibmcloud resource service-instance-update my-cos \
  --service-plan-id <NEW-PLAN-ID>

# Delete resource
ibmcloud resource service-instance-delete my-cos
```

### CLI Configuration

```bash
# Set CLI preferences
ibmcloud config --usage-stats-collect false
ibmcloud config --check-version false

# View configuration
ibmcloud config --list

# Set output format (JSON)
ibmcloud config --output json
```

### Scripting with CLI

**Bash Script:**
```bash
#!/bin/bash
set -e

# Variables
RESOURCE_GROUP="production-rg"
REGION="us-south"
COS_NAME="my-app-storage"

# Login
ibmcloud login --apikey "${IBMCLOUD_API_KEY}" --no-region

# Target region and resource group
ibmcloud target -r "${REGION}" -g "${RESOURCE_GROUP}"

# Create COS instance
ibmcloud resource service-instance-create "${COS_NAME}" \
  cloud-object-storage standard global \
  -g "${RESOURCE_GROUP}"

# Create service credentials
ibmcloud resource service-key-create "${COS_NAME}-creds" \
  Writer --instance-name "${COS_NAME}" \
  --parameters '{"HMAC": true}'

# Get credentials
CREDENTIALS=$(ibmcloud resource service-key "${COS_NAME}-creds" --output json)
echo "${CREDENTIALS}" | jq -r '.credentials'
```

---

## Terraform Provider

### Setup

```hcl
# versions.tf
terraform {
  required_version = ">= 1.0"
  required_providers {
    ibm = {
      source  = "IBM-Cloud/ibm"
      version = "~> 1.59"
    }
  }
}

# provider.tf
provider "ibm" {
  ibmcloud_api_key = var.ibmcloud_api_key
  region           = var.region
}

# variables.tf
variable "ibmcloud_api_key" {
  description = "IBM Cloud API Key"
  type        = string
  sensitive   = true
}

variable "region" {
  description = "IBM Cloud region"
  type        = string
  default     = "us-south"
}
```

### Complete Infrastructure Example

```hcl
# main.tf

# Resource Group
resource "ibm_resource_group" "rg" {
  name = "my-app-rg"
}

# VPC
resource "ibm_is_vpc" "vpc" {
  name                      = "my-vpc"
  resource_group            = ibm_resource_group.rg.id
  address_prefix_management = "auto"
}

# Subnets (Multi-Zone)
resource "ibm_is_subnet" "subnet_1" {
  name            = "subnet-zone-1"
  vpc             = ibm_is_vpc.vpc.id
  zone            = "us-south-1"
  ipv4_cidr_block = "10.240.1.0/24"
  resource_group  = ibm_resource_group.rg.id
}

resource "ibm_is_subnet" "subnet_2" {
  name            = "subnet-zone-2"
  vpc             = ibm_is_vpc.vpc.id
  zone            = "us-south-2"
  ipv4_cidr_block = "10.240.2.0/24"
  resource_group  = ibm_resource_group.rg.id
}

resource "ibm_is_subnet" "subnet_3" {
  name            = "subnet-zone-3"
  vpc             = ibm_is_vpc.vpc.id
  zone            = "us-south-3"
  ipv4_cidr_block = "10.240.3.0/24"
  resource_group  = ibm_resource_group.rg.id
}

# IKS Cluster
resource "ibm_container_vpc_cluster" "cluster" {
  name              = "my-cluster"
  vpc_id            = ibm_is_vpc.vpc.id
  flavor            = "bx2.4x16"
  worker_count      = 3
  kube_version      = "1.28"
  resource_group_id = ibm_resource_group.rg.id

  zones {
    name      = "us-south-1"
    subnet_id = ibm_is_subnet.subnet_1.id
  }

  zones {
    name      = "us-south-2"
    subnet_id = ibm_is_subnet.subnet_2.id
  }

  zones {
    name      = "us-south-3"
    subnet_id = ibm_is_subnet.subnet_3.id
  }
}

# PostgreSQL Database
resource "ibm_database" "postgresql" {
  name              = "my-postgres"
  plan              = "standard"
  location          = "us-south"
  service           = "databases-for-postgresql"
  resource_group_id = ibm_resource_group.rg.id
  version           = "15"

  adminpassword = var.postgres_password

  group {
    group_id = "member"
    memory {
      allocation_mb = 4096
    }
    disk {
      allocation_mb = 20480
    }
    cpu {
      allocation_count = 3
    }
  }

  service_endpoints = "private"
}

# Cloud Object Storage
resource "ibm_resource_instance" "cos" {
  name              = "my-cos"
  service           = "cloud-object-storage"
  plan              = "standard"
  location          = "global"
  resource_group_id = ibm_resource_group.rg.id
}

resource "ibm_cos_bucket" "bucket" {
  bucket_name          = "my-app-bucket"
  resource_instance_id = ibm_resource_instance.cos.id
  region_location      = "us-south"
  storage_class        = "smart"
}

# Outputs
output "cluster_id" {
  value = ibm_container_vpc_cluster.cluster.id
}

output "postgres_connection" {
  value     = ibm_database.postgresql.connectionstrings
  sensitive = true
}

output "cos_endpoint" {
  value = ibm_cos_bucket.bucket.s3_endpoint_public
}
```

### State Management

```hcl
# backend.tf - Remote state in COS
terraform {
  backend "s3" {
    bucket                      = "terraform-state"
    key                         = "prod/terraform.tfstate"
    region                      = "us-south"
    endpoint                    = "s3.us-south.cloud-object-storage.appdomain.cloud"
    skip_credentials_validation = true
    skip_region_validation      = true
  }
}
```

### Terraform Commands

```bash
# Initialize
terraform init

# Plan
terraform plan -out=tfplan

# Apply
terraform apply tfplan

# Destroy
terraform destroy

# Import existing resource
terraform import ibm_is_vpc.vpc <VPC-ID>

# Show state
terraform show

# List resources
terraform state list
```

---

## Schematics

### Overview

Managed Terraform service for infrastructure as code.

### Create Workspace

```bash
# Create workspace
ibmcloud schematics workspace new \
  --name my-workspace \
  --type terraform_v1.5 \
  --location us-south \
  --resource-group production-rg \
  --template-repo https://github.com/username/terraform-config \
  --template-repo-branch main

# Plan
ibmcloud schematics plan --id <WORKSPACE-ID>

# Apply
ibmcloud schematics apply --id <WORKSPACE-ID>

# Destroy
ibmcloud schematics destroy --id <WORKSPACE-ID>
```

**Terraform Configuration:**
```hcl
# schematics.tf
terraform {
  backend "http" {
    address        = "https://schematics.cloud.ibm.com/v1/workspaces/<WORKSPACE-ID>/state"
    lock_address   = "https://schematics.cloud.ibm.com/v1/workspaces/<WORKSPACE-ID>/state"
    unlock_address = "https://schematics.cloud.ibm.com/v1/workspaces/<WORKSPACE-ID>/state"
  }
}
```

---

## Resource Manager APIs

### REST API Examples

**Python:**
```python
import requests
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator

# Authenticate
authenticator = IAMAuthenticator('<API-KEY>')
token = authenticator.token_manager.get_token()

# List resource instances
url = 'https://resource-controller.cloud.ibm.com/v2/resource_instances'
headers = {
    'Authorization': f'Bearer {token}',
    'Content-Type': 'application/json'
}

response = requests.get(url, headers=headers)
instances = response.json()['resources']

for instance in instances:
    print(f"{instance['name']}: {instance['state']}")

# Create resource instance
create_url = 'https://resource-controller.cloud.ibm.com/v2/resource_instances'
payload = {
    'name': 'my-cos-instance',
    'target': 'global',
    'resource_group': '<RESOURCE-GROUP-ID>',
    'resource_plan_id': '<PLAN-ID>'
}

response = requests.post(create_url, headers=headers, json=payload)
print(response.json())
```

---

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/deploy.yml
name: Deploy to IBM Cloud

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install IBM Cloud CLI
        run: |
          curl -fsSL https://clis.cloud.ibm.com/install/linux | sh
          ibmcloud plugin install container-service
          ibmcloud plugin install container-registry
      
      - name: Login to IBM Cloud
        run: |
          ibmcloud login --apikey ${{ secrets.IBMCLOUD_API_KEY }} -r us-south
          ibmcloud cr login
      
      - name: Build and Push Image
        run: |
          docker build -t us.icr.io/namespace/myapp:${{ github.sha }} .
          docker push us.icr.io/namespace/myapp:${{ github.sha }}
      
      - name: Deploy to Kubernetes
        run: |
          ibmcloud ks cluster config --cluster my-cluster
          kubectl set image deployment/myapp myapp=us.icr.io/namespace/myapp:${{ github.sha }}
```

### GitLab CI

```yaml
# .gitlab-ci.yml
stages:
  - build
  - deploy

variables:
  IMAGE_TAG: $CI_REGISTRY_IMAGE:$CI_COMMIT_SHORT_SHA

build:
  stage: build
  script:
    - docker build -t $IMAGE_TAG .
    - docker push $IMAGE_TAG

deploy:
  stage: deploy
  script:
    - curl -fsSL https://clis.cloud.ibm.com/install/linux | sh
    - ibmcloud plugin install code-engine
    - ibmcloud login --apikey $IBMCLOUD_API_KEY -r us-south
    - ibmcloud ce project select -n my-project
    - ibmcloud ce app update -n myapp --image $IMAGE_TAG
  only:
    - main
```

### Terraform in CI/CD

```yaml
# .github/workflows/terraform.yml
name: Terraform

on:
  push:
    branches: [main]
  pull_request:

jobs:
  terraform:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: 1.5.0
      
      - name: Terraform Init
        run: terraform init
        env:
          TF_VAR_ibmcloud_api_key: ${{ secrets.IBMCLOUD_API_KEY }}
      
      - name: Terraform Plan
        run: terraform plan -out=tfplan
        env:
          TF_VAR_ibmcloud_api_key: ${{ secrets.IBMCLOUD_API_KEY }}
      
      - name: Terraform Apply
        if: github.ref == 'refs/heads/main'
        run: terraform apply -auto-approve tfplan
        env:
          TF_VAR_ibmcloud_api_key: ${{ secrets.IBMCLOUD_API_KEY }}
```

---

## Best Practices

### CLI Usage

1. **API Keys**: Use API keys, not passwords
2. **Scripting**: Set `set -e` for error handling
3. **JSON Output**: Parse with `jq` for automation
4. **Version Pinning**: Pin CLI and plugin versions
5. **Region Awareness**: Always specify region

### Terraform

1. **State Management**: Use remote state (COS)
2. **Modules**: Create reusable modules
3. **Variables**: Externalize all configuration
4. **Outputs**: Export important resource IDs
5. **Version Control**: Track `.tf` files in Git
6. **Plan Review**: Always review plans before apply
7. **State Locking**: Enable for team collaboration

### CI/CD

1. **Secrets Management**: Use CI/CD secrets, not hardcoded
2. **Approval Gates**: Require manual approval for production
3. **Rollback**: Implement automated rollback
4. **Testing**: Test infrastructure changes in dev first
5. **Monitoring**: Track deployment success rates
