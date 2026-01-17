# IAM & Security

## Table of Contents
- [Identity and Access Management (IAM)](#identity-and-access-management-iam)
- [Resource Groups and Access Groups](#resource-groups-and-access-groups)
- [Service IDs and API Keys](#service-ids-and-api-keys)
- [Key Protect](#key-protect)
- [Secrets Manager](#secrets-manager)
- [Security and Compliance Center](#security-and-compliance-center)
- [Activity Tracker](#activity-tracker)

---

## Identity and Access Management (IAM)

### Overview

IAM provides centralized access control for IBM Cloud resources through policies, roles, and access groups.

**Core Concepts:**
- **Users**: Human identities (IBMid, AppID)
- **Service IDs**: Application identities
- **Access Groups**: Group users/service IDs for policy assignment
- **Policies**: Grant specific roles on specific resources
- **Roles**: Predefined permission sets

### IAM Roles

**Platform Roles:**
- **Viewer**: Read-only access, view resources
- **Operator**: Perform actions (start/stop), no modify
- **Editor**: Modify resources, create/delete
- **Administrator**: Full control, assign access to others

**Service Roles:**
- **Reader**: Read service data
- **Writer**: Write service data
- **Manager**: Full service management

**Custom Roles** (Enterprise plans):
- Define custom permissions

### Assign Access

**Via CLI:**
```bash
# Assign platform role
ibmcloud iam user-policy-create user@example.com \
  --roles Viewer,Operator \
  --resource-group-name my-rg

# Assign service role
ibmcloud iam user-policy-create user@example.com \
  --roles Reader,Writer \
  --service-name cloud-object-storage \
  --service-instance <COS-INSTANCE-ID>

# Assign specific resource access
ibmcloud iam user-policy-create user@example.com \
  --roles Editor \
  --service-name is \
  --resource-type vpc \
  --resource <VPC-ID>

# List user policies
ibmcloud iam user-policies user@example.com
```

**Terraform:**
```hcl
# IAM policy for user
resource "ibm_iam_user_policy" "policy" {
  ibm_id = "user@example.com"
  roles  = ["Viewer", "Operator"]

  resources {
    resource_group_id = ibm_resource_group.rg.id
  }
}

# IAM policy for specific service
resource "ibm_iam_user_policy" "cos_policy" {
  ibm_id = "user@example.com"
  roles  = ["Reader", "Writer"]

  resources {
    service              = "cloud-object-storage"
    resource_instance_id = ibm_resource_instance.cos.guid
  }
}
```

### Policy Examples

**Grant Full Access to Resource Group:**
```bash
ibmcloud iam user-policy-create user@example.com \
  --roles Administrator,Manager \
  --resource-group-name production-rg
```

**Grant Kubernetes Cluster Access:**
```bash
ibmcloud iam user-policy-create user@example.com \
  --service-name containers-kubernetes \
  --service-instance <CLUSTER-ID> \
  --roles Administrator,Manager
```

**Grant COS Bucket Access:**
```bash
ibmcloud iam user-policy-create user@example.com \
  --service-name cloud-object-storage \
  --service-instance <COS-INSTANCE-ID> \
  --resource-type bucket \
  --resource my-bucket \
  --roles Reader,ContentReader
```

---

## Resource Groups and Access Groups

### Resource Groups

Organizational units for resources. Cannot be nested.

```bash
# Create resource group
ibmcloud resource group-create production-rg

# List resource groups
ibmcloud resource groups

# Target resource group
ibmcloud target -g production-rg
```

**Best Practices:**
- Organize by environment (dev, staging, prod)
- Or by application/project
- Or by department/team
- Plan before creating (cannot easily move resources)

### Access Groups

Group users and service IDs for policy assignment.

**CLI:**
```bash
# Create access group
ibmcloud iam access-group-create Developers \
  -d "Developer team members"

# Add user to access group
ibmcloud iam access-group-user-add Developers user@example.com

# Add service ID to access group
ibmcloud iam access-group-service-id-add Developers ServiceId-123

# Assign policy to access group
ibmcloud iam access-group-policy-create Developers \
  --roles Editor,Writer \
  --resource-group-name production-rg

# List access groups
ibmcloud iam access-groups
```

**Terraform:**
```hcl
# Create access group
resource "ibm_iam_access_group" "developers" {
  name        = "Developers"
  description = "Developer team members"
}

# Add users
resource "ibm_iam_access_group_members" "dev_members" {
  access_group_id = ibm_iam_access_group.developers.id
  ibm_ids         = ["user1@example.com", "user2@example.com"]
}

# Assign policy
resource "ibm_iam_access_group_policy" "dev_policy" {
  access_group_id = ibm_iam_access_group.developers.id
  roles           = ["Editor", "Writer"]

  resources {
    resource_group_id = ibm_resource_group.prod_rg.id
  }
}
```

**Example Access Groups:**
- **Administrators**: Full platform and service access
- **Developers**: Editor + Writer on dev/staging
- **Operators**: Operator on production resources
- **Viewers**: Viewer on all resources
- **Billing Admins**: Billing management only

---

## Service IDs and API Keys

### Service IDs

Non-human identities for applications and services.

**CLI:**
```bash
# Create service ID
ibmcloud iam service-id-create my-app-id \
  -d "Service ID for my application"

# Create API key for service ID
ibmcloud iam service-api-key-create my-app-key ServiceId-123 \
  -d "API key for production deployment"

# Assign policy to service ID
ibmcloud iam service-policy-create ServiceId-123 \
  --roles Writer \
  --service-name cloud-object-storage \
  --service-instance <COS-INSTANCE-ID>

# List service IDs
ibmcloud iam service-ids
```

**Terraform:**
```hcl
# Create service ID
resource "ibm_iam_service_id" "app_id" {
  name        = "my-app-id"
  description = "Service ID for my application"
}

# Create API key
resource "ibm_iam_service_api_key" "app_key" {
  name           = "my-app-key"
  iam_service_id = ibm_iam_service_id.app_id.iam_id
  description    = "API key for production"
}

# Assign policy
resource "ibm_iam_service_policy" "app_policy" {
  iam_service_id = ibm_iam_service_id.app_id.iam_id
  roles          = ["Writer", "Manager"]

  resources {
    service              = "cloud-object-storage"
    resource_instance_id = ibm_resource_instance.cos.guid
  }
}
```

### API Keys

**User API Keys:**
```bash
# Create user API key
ibmcloud iam api-key-create my-api-key \
  -d "Personal API key for CLI access"

# List API keys
ibmcloud iam api-keys

# Delete API key
ibmcloud iam api-key-delete my-api-key
```

**Authenticate with API Key:**
```bash
# CLI login with API key
ibmcloud login --apikey @api-key.txt

# Or with environment variable
export IBMCLOUD_API_KEY="<API-KEY>"
ibmcloud login --no-region
```

**Best Practices:**
- Rotate API keys every 90 days
- Use service IDs for applications
- Store keys in Secrets Manager
- Never commit keys to version control
- Use least privilege principle
- Monitor API key usage with Activity Tracker

---

## Key Protect

### Overview

FIPS 140-2 Level 3 certified key management service for encryption keys.

**Use Cases:**
- Encrypt data at rest (COS, databases, volumes)
- Bring Your Own Key (BYOK)
- Keep Your Own Key (KYOK) with Hyper Protect Crypto Services
- Envelope encryption

### Create Key Protect Instance

**CLI:**
```bash
# Create Key Protect instance
ibmcloud resource service-instance-create my-key-protect \
  kms tiered-pricing us-south

# Create root key
ibmcloud kp key create my-root-key \
  --instance-id <KP-INSTANCE-ID> \
  --key-material <BASE64-KEY-MATERIAL>

# Create standard key
ibmcloud kp key create my-standard-key \
  --instance-id <KP-INSTANCE-ID> \
  --standard-key

# List keys
ibmcloud kp keys --instance-id <KP-INSTANCE-ID>
```

**Terraform:**
```hcl
resource "ibm_resource_instance" "key_protect" {
  name              = "my-key-protect"
  service           = "kms"
  plan              = "tiered-pricing"
  location          = "us-south"
  resource_group_id = ibm_resource_group.rg.id
}

resource "ibm_kms_key" "root_key" {
  instance_id  = ibm_resource_instance.key_protect.guid
  key_name     = "my-root-key"
  standard_key = false
  force_delete = true
}

resource "ibm_kms_key" "standard_key" {
  instance_id  = ibm_resource_instance.key_protect.guid
  key_name     = "my-standard-key"
  standard_key = true
}
```

### Encrypt Resources

**Cloud Object Storage:**
```hcl
resource "ibm_cos_bucket" "encrypted_bucket" {
  bucket_name          = "my-encrypted-bucket"
  resource_instance_id = ibm_resource_instance.cos.id
  region_location      = "us-south"
  storage_class        = "standard"
  
  kms_key_crn = ibm_kms_key.root_key.crn
}
```

**Block Storage:**
```hcl
resource "ibm_is_volume" "encrypted_volume" {
  name           = "my-encrypted-volume"
  profile        = "10iops-tier"
  zone           = "us-south-1"
  capacity       = 100
  encryption_key = ibm_kms_key.root_key.crn
}
```

**Database:**
```hcl
resource "ibm_database" "postgresql" {
  name              = "my-postgres"
  plan              = "standard"
  location          = "us-south"
  service           = "databases-for-postgresql"
  
  backup_encryption_key_crn = ibm_kms_key.root_key.crn
}
```

### Key Rotation

```bash
# Rotate root key (creates new version)
ibmcloud kp key rotate <KEY-ID> \
  --instance-id <KP-INSTANCE-ID>

# Set rotation policy (automatic rotation)
ibmcloud kp key-policy-rotation-set <KEY-ID> \
  --instance-id <KP-INSTANCE-ID> \
  --interval-month 3
```

---

## Secrets Manager

### Overview

Centralized secrets management for API keys, passwords, certificates, and credentials.

**Secret Types:**
- Arbitrary (generic secrets)
- User credentials
- IAM credentials (automatic rotation)
- Certificates (Let's Encrypt, uploaded, imported)
- Key-value secrets

### Create Secrets Manager Instance

**CLI:**
```bash
# Create Secrets Manager instance
ibmcloud resource service-instance-create my-secrets-manager \
  secrets-manager trial us-south

# Create secret group
ibmcloud secrets-manager secret-group-create \
  --name production-secrets \
  --description "Production environment secrets"

# Create arbitrary secret
ibmcloud secrets-manager secret-create \
  --secret-type arbitrary \
  --name db-password \
  --secret-group-id <GROUP-ID> \
  --payload "supersecretpassword"

# Get secret value
ibmcloud secrets-manager secret-get \
  --secret-type arbitrary \
  --id <SECRET-ID>
```

**Terraform:**
```hcl
resource "ibm_resource_instance" "secrets_manager" {
  name              = "my-secrets-manager"
  service           = "secrets-manager"
  plan              = "standard"
  location          = "us-south"
  resource_group_id = ibm_resource_group.rg.id
}

resource "ibm_sm_secret_group" "group" {
  instance_id = ibm_resource_instance.secrets_manager.guid
  region      = "us-south"
  name        = "production-secrets"
  description = "Production secrets"
}

resource "ibm_sm_arbitrary_secret" "db_password" {
  instance_id  = ibm_resource_instance.secrets_manager.guid
  region       = "us-south"
  name         = "db-password"
  description  = "Database password"
  secret_group_id = ibm_sm_secret_group.group.secret_group_id
  payload      = "supersecretpassword"
  
  labels = ["production", "database"]
}

resource "ibm_sm_iam_credentials_secret" "iam_creds" {
  instance_id = ibm_resource_instance.secrets_manager.guid
  region      = "us-south"
  name        = "app-service-id"
  description = "Application service ID credentials"
  secret_group_id = ibm_sm_secret_group.group.secret_group_id
  service_id  = ibm_iam_service_id.app.iam_id
  ttl         = "7776000"  # 90 days
  reuse_api_key = false
}
```

### Access Secrets in Applications

**Python:**
```python
from ibm_secrets_manager_sdk.secrets_manager_v2 import *
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator

authenticator = IAMAuthenticator('<API-KEY>')
service = SecretsManagerV2(authenticator=authenticator)
service.set_service_url('<SECRETS-MANAGER-URL>')

# Get secret
secret = service.get_secret(
    id='<SECRET-ID>',
    secret_type='arbitrary'
).get_result()

db_password = secret['payload']
print(f"Password: {db_password}")
```

**Node.js:**
```javascript
const SecretsManagerV2 = require('@ibm-cloud/secrets-manager/secrets-manager/v2');
const { IamAuthenticator } = require('@ibm-cloud/secrets-manager/auth');

const authenticator = new IamAuthenticator({
  apikey: '<API-KEY>',
});

const secretsManager = new SecretsManagerV2({
  authenticator: authenticator,
  serviceUrl: '<SECRETS-MANAGER-URL>',
});

const params = {
  id: '<SECRET-ID>',
  secretType: 'arbitrary',
};

secretsManager.getSecret(params)
  .then(res => {
    const password = res.result.payload;
    console.log(`Password: ${password}`);
  });
```

---

## Security and Compliance Center

### Overview

Unified security and compliance management across IBM Cloud.

**Features:**
- Security posture monitoring
- Compliance validation
- Configuration scanning
- Vulnerability assessment
- Compliance reporting

### Create Security and Compliance Center

```bash
# Create SCC instance
ibmcloud resource service-instance-create my-scc \
  compliance standard us-south

# Attach scope (resource group)
ibmcloud scc scope-create \
  --name production-scope \
  --description "Production resources" \
  --environment ibm-cloud \
  --properties '{"scope_id": "<RESOURCE-GROUP-ID>"}'

# Create profile attachment
ibmcloud scc attachment-create \
  --profile-id <PROFILE-ID> \
  --scope-id <SCOPE-ID> \
  --name "CIS Benchmark"
```

### Compliance Profiles

**Available Profiles:**
- **CIS IBM Cloud Foundations Benchmark**: 139 controls
- **IBM Cloud Framework for Financial Services**: 300+ controls
- **NIST 800-53**: Security controls
- **PCI-DSS**: Payment card industry
- **HIPAA**: Healthcare compliance
- **ISO 27001**: Information security

### Scan and Monitor

```bash
# Run compliance scan
ibmcloud scc scan-create \
  --attachment-id <ATTACHMENT-ID>

# View scan results
ibmcloud scc scan-get <SCAN-ID>

# Generate compliance report
ibmcloud scc report-create \
  --attachment-id <ATTACHMENT-ID> \
  --type compliance
```

---

## Activity Tracker

### Overview

Audit trail for IBM Cloud account and resource activities.

**Event Types:**
- Account management
- IAM activities
- Resource lifecycle
- Data access
- Network events

### Create Activity Tracker

```bash
# Create Activity Tracker instance
ibmcloud resource service-instance-create my-activity-tracker \
  logdnaat 7-day us-south

# Configure platform logs
ibmcloud at route create \
  --target-type logdna \
  --target-id <AT-INSTANCE-ID> \
  --target-region us-south
```

**Terraform:**
```hcl
resource "ibm_resource_instance" "activity_tracker" {
  name              = "my-activity-tracker"
  service           = "logdnaat"
  plan              = "7-day"
  location          = "us-south"
  resource_group_id = ibm_resource_group.rg.id
}
```

### View Events

```bash
# View activity logs (web console)
ibmcloud at view

# Export events
ibmcloud at export \
  --start "2024-01-01T00:00:00Z" \
  --end "2024-01-31T23:59:59Z" \
  --type json \
  --output events.json
```

### Common Audit Queries

**IAM Policy Changes:**
```
action:iam-access-management.policy.*
```

**Resource Creation:**
```
action:*.create
```

**Failed Login Attempts:**
```
action:iam-identity.user-apikey.login outcome:failure
```

**Data Access (COS):**
```
target.typeURI:cloud-object-storage/bucket action:cloud-object-storage.object.read
```

---

## Security Best Practices

### Authentication

1. **Enable MFA**: Require MFA for all users
2. **API Key Rotation**: Rotate keys every 90 days
3. **Service IDs**: Use service IDs for applications
4. **Strong Passwords**: Enforce password complexity
5. **Session Timeouts**: Configure appropriate timeouts

### Authorization

1. **Least Privilege**: Grant minimum required permissions
2. **Access Groups**: Use access groups for policy management
3. **Resource Groups**: Organize resources logically
4. **Regular Audits**: Review access policies quarterly
5. **Just-in-Time Access**: Temporary elevated access

### Encryption

1. **Encryption at Rest**: Enable for all storage
2. **Encryption in Transit**: TLS 1.2+ only
3. **Key Management**: Use Key Protect/HPCS
4. **Key Rotation**: Rotate encryption keys annually
5. **BYOK**: Bring Your Own Key for sensitive data

### Network Security

1. **Private Endpoints**: Use private connectivity
2. **Security Groups**: Restrict inbound/outbound traffic
3. **Network ACLs**: Subnet-level access control
4. **VPN/Direct Link**: Secure hybrid connectivity
5. **WAF**: Web Application Firewall for public apps

### Monitoring and Compliance

1. **Activity Tracker**: Enable for all accounts
2. **Security Center**: Continuous compliance monitoring
3. **Vulnerability Scanning**: Regular security assessments
4. **Incident Response**: Document and test procedures
5. **Compliance Reporting**: Generate regular reports

### Data Protection

1. **Backup Strategy**: Regular automated backups
2. **Data Classification**: Classify and label data
3. **Data Retention**: Define retention policies
4. **Data Residency**: Choose appropriate regions
5. **DLP**: Data loss prevention controls
