# Identity and Access Management (IAM)

## Compartment Design

### Compartment Hierarchy Best Practices

```
Root Compartment (Tenancy)
├── Network (shared networking resources)
│   ├── VCN-Prod
│   └── VCN-Dev
├── Security (shared security resources)
│   ├── Vaults
│   └── Keys
├── Applications
│   ├── App1-Prod
│   ├── App1-Dev
│   ├── App2-Prod
│   └── App2-Dev
└── Shared-Services
    ├── Monitoring
    └── Logging
```

### Creating Compartments

**Terraform**:
```hcl
resource "oci_identity_compartment" "network" {
  compartment_id = var.tenancy_ocid
  description    = "Network resources compartment"
  name           = "Network"
  
  freeform_tags = {
    Department = "IT"
  }
}

resource "oci_identity_compartment" "app_prod" {
  compartment_id = oci_identity_compartment.applications.id
  description    = "Production environment for App1"
  name           = "App1-Prod"
  
  enable_delete = false  # Prevent accidental deletion
}
```

**OCI CLI**:
```bash
oci iam compartment create \
  --compartment-id ocid1.tenancy... \
  --name "Network" \
  --description "Network resources compartment"
```

## Users and Groups

### User Management

**Create User**:
```hcl
resource "oci_identity_user" "developer" {
  compartment_id = var.tenancy_ocid
  description    = "Developer user"
  name           = "john.doe@example.com"
  email          = "john.doe@example.com"
  
  freeform_tags = {
    Role = "Developer"
  }
}
```

**Create API Key**:
```bash
# Generate key pair
openssl genrsa -out ~/.oci/oci_api_key.pem 2048
openssl rsa -pubout -in ~/.oci/oci_api_key.pem -out ~/.oci/oci_api_key_public.pem

# Get fingerprint
openssl rsa -pubout -outform DER -in ~/.oci/oci_api_key.pem | openssl md5 -c

# Upload public key via CLI
oci iam user api-key upload \
  --user-id ocid1.user... \
  --key-file ~/.oci/oci_api_key_public.pem
```

### Group Management

```hcl
resource "oci_identity_group" "developers" {
  compartment_id = var.tenancy_ocid
  description    = "Developer group"
  name           = "Developers"
}

resource "oci_identity_group" "admins" {
  compartment_id = var.tenancy_ocid
  description    = "Administrator group"
  name           = "Administrators"
}

resource "oci_identity_user_group_membership" "dev_membership" {
  group_id = oci_identity_group.developers.id
  user_id  = oci_identity_user.developer.id
}
```

## IAM Policies

### Policy Syntax

**Format**: `Allow <subject> to <verb> <resource-type> in <location> where <conditions>`

**Subjects**:
- `group <group-name>` - User group
- `dynamic-group <dynamic-group-name>` - Dynamic group
- `any-user` - All authenticated users
- `service <service-name>` - OCI service

**Verbs**:
- `inspect` - List resources
- `read` - View resource details
- `use` - Use existing resources
- `manage` - Full control (create, update, delete)

### Common Policy Examples

**Read-Only Access**:
```hcl
resource "oci_identity_policy" "read_only" {
  compartment_id = var.compartment_id
  description    = "Read-only access to compute and networking"
  name           = "ReadOnlyPolicy"
  
  statements = [
    "Allow group Developers to inspect instances in compartment App1-Prod",
    "Allow group Developers to inspect vcns in compartment Network",
    "Allow group Developers to read metrics in compartment App1-Prod"
  ]
}
```

**Developer Policy**:
```hcl
resource "oci_identity_policy" "developers" {
  compartment_id = var.compartment_id
  description    = "Developer policy for non-prod environments"
  name           = "DeveloperPolicy"
  
  statements = [
    "Allow group Developers to manage instances in compartment App1-Dev",
    "Allow group Developers to manage volumes in compartment App1-Dev",
    "Allow group Developers to use vnics in compartment Network",
    "Allow group Developers to use subnets in compartment Network",
    "Allow group Developers to use network-security-groups in compartment Network",
    "Allow group Developers to read autonomous-databases in compartment App1-Dev"
  ]
}
```

**Administrator Policy**:
```hcl
resource "oci_identity_policy" "admins" {
  compartment_id = var.tenancy_ocid
  description    = "Full administrator access"
  name           = "AdministratorPolicy"
  
  statements = [
    "Allow group Administrators to manage all-resources in tenancy"
  ]
}
```

**Network Administrator**:
```hcl
resource "oci_identity_policy" "network_admins" {
  compartment_id = var.tenancy_ocid
  description    = "Network administrator policy"
  name           = "NetworkAdminPolicy"
  
  statements = [
    "Allow group NetworkAdmins to manage vcns in compartment Network",
    "Allow group NetworkAdmins to manage subnets in compartment Network",
    "Allow group NetworkAdmins to manage internet-gateways in compartment Network",
    "Allow group NetworkAdmins to manage nat-gateways in compartment Network",
    "Allow group NetworkAdmins to manage service-gateways in compartment Network",
    "Allow group NetworkAdmins to manage security-lists in compartment Network",
    "Allow group NetworkAdmins to manage network-security-groups in compartment Network",
    "Allow group NetworkAdmins to manage route-tables in compartment Network",
    "Allow group NetworkAdmins to manage drgs in compartment Network",
    "Allow group NetworkAdmins to manage load-balancers in compartment Network"
  ]
}
```

**Database Administrator**:
```hcl
resource "oci_identity_policy" "db_admins" {
  compartment_id = var.compartment_id
  description    = "Database administrator policy"
  name           = "DBAdminPolicy"
  
  statements = [
    "Allow group DBAdmins to manage autonomous-databases in compartment App1-Prod",
    "Allow group DBAdmins to manage autonomous-backups in compartment App1-Prod",
    "Allow group DBAdmins to manage database-family in compartment App1-Prod",
    "Allow group DBAdmins to read metrics in compartment App1-Prod"
  ]
}
```

### Conditional Policies

**IP-Based Access**:
```hcl
resource "oci_identity_policy" "ip_restricted" {
  compartment_id = var.tenancy_ocid
  description    = "Allow access only from specific IP"
  name           = "IPRestrictedPolicy"
  
  statements = [
    "Allow group Developers to manage all-resources in compartment App1-Dev where request.networkSource.name = 'CorporateNetwork'"
  ]
}

resource "oci_identity_network_source" "corporate" {
  compartment_id = var.tenancy_ocid
  description    = "Corporate network"
  name           = "CorporateNetwork"
  
  public_source_list = [
    "203.0.113.0/24",
    "198.51.100.0/24"
  ]
}
```

**Tag-Based Access**:
```hcl
resource "oci_identity_policy" "tag_based" {
  compartment_id = var.compartment_id
  description    = "Tag-based policy"
  name           = "TagBasedPolicy"
  
  statements = [
    "Allow group Developers to manage instances in compartment App1-Dev where target.resource.tag.Environment = 'Development'"
  ]
}
```

## Dynamic Groups

### Create Dynamic Group

**For Compute Instances in Compartment**:
```hcl
resource "oci_identity_dynamic_group" "app_instances" {
  compartment_id = var.tenancy_ocid
  description    = "Dynamic group for app instances"
  name           = "AppInstancesDynamicGroup"
  
  matching_rule = "ALL {instance.compartment.id = '${var.compartment_id}'}"
}
```

**For OKE Clusters**:
```hcl
resource "oci_identity_dynamic_group" "oke_clusters" {
  compartment_id = var.tenancy_ocid
  description    = "Dynamic group for OKE clusters"
  name           = "OKEClustersDynamicGroup"
  
  matching_rule = "ALL {resource.type = 'cluster', resource.compartment.id = '${var.compartment_id}'}"
}
```

**Complex Matching Rules**:
```hcl
resource "oci_identity_dynamic_group" "complex" {
  compartment_id = var.tenancy_ocid
  description    = "Complex dynamic group"
  name           = "ComplexDynamicGroup"
  
  matching_rule = <<-EOT
    ANY {
      instance.compartment.id = '${var.app_compartment_id}',
      resource.type = 'fnfunc',
      resource.type = 'autonomousdatabase'
    }
  EOT
}
```

### Dynamic Group Policies

```hcl
resource "oci_identity_policy" "instance_principal" {
  compartment_id = var.tenancy_ocid
  description    = "Policy for instance principals"
  name           = "InstancePrincipalPolicy"
  
  statements = [
    "Allow dynamic-group AppInstancesDynamicGroup to read secret-bundles in compartment Security",
    "Allow dynamic-group AppInstancesDynamicGroup to use keys in compartment Security",
    "Allow dynamic-group AppInstancesDynamicGroup to manage objects in compartment App1-Prod where target.bucket.name='app-data'",
    "Allow dynamic-group AppInstancesDynamicGroup to read autonomous-databases in compartment App1-Prod"
  ]
}
```

## Federation

### Identity Provider (SAML 2.0)

```hcl
resource "oci_identity_identity_provider" "saml_idp" {
  compartment_id      = var.tenancy_ocid
  name                = "CorporateSSO"
  description         = "Corporate SAML SSO"
  product_type        = "IDCS"
  protocol            = "SAML2"
  
  metadata_url = "https://idp.example.com/metadata"
  
  freeform_tags = {
    Type = "SSO"
  }
}

resource "oci_identity_idp_group_mapping" "idp_mapping" {
  identity_provider_id = oci_identity_identity_provider.saml_idp.id
  idp_group_name       = "OCI-Developers"
  group_id             = oci_identity_group.developers.id
}
```

## Multi-Factor Authentication (MFA)

### Enable MFA for User

```bash
# Enable MFA
oci iam user update-user-capabilities \
  --user-id ocid1.user... \
  --can-use-console-password true

# User must enable MFA in console settings
```

**Policy to Require MFA**:
```hcl
resource "oci_identity_policy" "require_mfa" {
  compartment_id = var.tenancy_ocid
  description    = "Require MFA for sensitive operations"
  name           = "RequireMFAPolicy"
  
  statements = [
    "Allow group Administrators to manage all-resources in tenancy where request.user.mfaTotpVerified='true'"
  ]
}
```

## Service Accounts

### Create Service Account User

```hcl
resource "oci_identity_user" "service_account" {
  compartment_id = var.tenancy_ocid
  description    = "Service account for CI/CD"
  name           = "svc-cicd"
  
  freeform_tags = {
    Type = "ServiceAccount"
  }
}

resource "oci_identity_api_key" "service_account_key" {
  user_id = oci_identity_user.service_account.id
  key_value = file("${path.module}/keys/service_account_public_key.pem")
}

resource "oci_identity_user_group_membership" "service_account_membership" {
  group_id = oci_identity_group.cicd_group.id
  user_id  = oci_identity_user.service_account.id
}
```

## Tagging Strategy

### Tag Namespaces and Keys

```hcl
resource "oci_identity_tag_namespace" "corporate" {
  compartment_id = var.tenancy_ocid
  description    = "Corporate tag namespace"
  name           = "Corporate"
}

resource "oci_identity_tag" "cost_center" {
  tag_namespace_id = oci_identity_tag_namespace.corporate.id
  description      = "Cost center for billing"
  name             = "CostCenter"
  
  validator {
    validator_type = "ENUM"
    values         = ["IT", "Finance", "HR", "Engineering"]
  }
}

resource "oci_identity_tag" "environment" {
  tag_namespace_id = oci_identity_tag_namespace.corporate.id
  description      = "Environment type"
  name             = "Environment"
  
  validator {
    validator_type = "ENUM"
    values         = ["Development", "Staging", "Production"]
  }
  
  is_cost_tracking = true
}
```

### Tag Defaults

```hcl
resource "oci_identity_tag_default" "default_env" {
  compartment_id    = var.compartment_id
  tag_definition_id = oci_identity_tag.environment.id
  value             = "Development"
  is_required       = true
}
```

### Using Tags

```hcl
resource "oci_core_instance" "tagged_instance" {
  # ... other configuration ...
  
  freeform_tags = {
    Application = "WebApp"
    Team        = "Platform"
  }
  
  defined_tags = {
    "${oci_identity_tag_namespace.corporate.name}.${oci_identity_tag.cost_center.name}"   = "Engineering"
    "${oci_identity_tag_namespace.corporate.name}.${oci_identity_tag.environment.name}" = "Production"
  }
}
```

## Resource Limits and Quotas

### Check Service Limits

```bash
# List all service limits
oci limits value list \
  --compartment-id ocid1.tenancy... \
  --service-name compute

# Get specific limit
oci limits value get \
  --compartment-id ocid1.tenancy... \
  --service-name compute \
  --scope-type REGION \
  --limit-name vm-standard-e4-flex-core-count \
  --availability-domain "AD-1"
```

### Request Limit Increase

```bash
oci limits quota create \
  --compartment-id ocid1.tenancy... \
  --description "Increase compute quota" \
  --name "compute-quota-increase" \
  --statements '["Set compute quotas vm-standard-e4-flex-core-count to 100 in compartment App1-Prod"]'
```

## Audit Logging

### Enable Audit Logs

**OCI automatically logs all API calls**. View audit logs:

```bash
# List audit events
oci audit event list \
  --compartment-id ocid1.compartment... \
  --start-time 2024-01-15T00:00:00.000Z \
  --end-time 2024-01-15T23:59:59.000Z
```

### Audit Log Analysis

```bash
# Get audit events for specific user
oci audit event list \
  --compartment-id ocid1.tenancy... \
  --start-time 2024-01-15T00:00:00.000Z \
  --end-time 2024-01-15T23:59:59.000Z \
  --query "data[?\"principal-id\"=='ocid1.user...'].{time:\"event-time\",action:\"event-name\",resource:\"resource-name\"}"

# Filter by event type
oci audit event list \
  --compartment-id ocid1.tenancy... \
  --start-time 2024-01-15T00:00:00.000Z \
  --end-time 2024-01-15T23:59:59.000Z \
  --query "data[?\"event-name\"=='DeleteInstance']"
```

## Best Practices

### IAM Best Practices

1. **Least Privilege**: Grant minimum required permissions
2. **Use Groups**: Never assign policies directly to users
3. **Compartment Isolation**: Separate resources by compartment
4. **Regular Reviews**: Audit users, groups, and policies quarterly
5. **Service Accounts**: Use dynamic groups for compute instances
6. **MFA**: Enable MFA for all administrator accounts
7. **API Key Rotation**: Rotate API keys every 90 days
8. **Tag Everything**: Use tags for cost tracking and access control

### Policy Design Patterns

**Principle of Least Privilege**:
```hcl
# Good - Specific permissions
"Allow group Developers to use instances in compartment App1-Dev"
"Allow group Developers to use vnics in compartment Network"

# Avoid - Overly broad permissions
"Allow group Developers to manage all-resources in tenancy"
```

**Separation of Duties**:
```hcl
# Network team manages networking
"Allow group NetworkAdmins to manage vcns in compartment Network"

# App team uses networking
"Allow group Developers to use vnics in compartment Network"
"Allow group Developers to use subnets in compartment Network"
```

### Security Recommendations

1. **Enable Cloud Guard**: Automated threat detection
2. **Use Security Zones**: Enforce security policies
3. **Regular Audits**: Review audit logs for suspicious activity
4. **Implement Network Sources**: Restrict access by IP
5. **Use Vault**: Store secrets and encryption keys
6. **Enable Logging**: Comprehensive logging for compliance
