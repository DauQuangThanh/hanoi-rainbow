# Azure Security Configuration

## Azure Active Directory (Entra ID)

### User and Group Management

```bash
# Create user
az ad user create \
  --display-name "John Doe" \
  --user-principal-name john.doe@contoso.com \
  --password "SecurePassword123!" \
  --force-change-password-next-sign-in true

# Create security group
az ad group create \
  --display-name "DevOps Engineers" \
  --mail-nickname devops \
  --description "DevOps team members"

# Add user to group
az ad group member add \
  --group "DevOps Engineers" \
  --member-id {USER_OBJECT_ID}

# List group members
az ad group member list \
  --group "DevOps Engineers" \
  --output table
```

### Service Principals

```bash
# Create service principal for app
az ad sp create-for-rbac \
  --name myApplication \
  --role Contributor \
  --scopes /subscriptions/{subscription-id}/resourceGroups/myResourceGroup

# Create service principal with certificate
az ad sp create-for-rbac \
  --name myApplication \
  --role Contributor \
  --scopes /subscriptions/{subscription-id} \
  --create-cert

# Reset credentials
az ad sp credential reset \
  --id {APP_ID}

# Delete service principal
az ad sp delete --id {APP_ID}
```

## Managed Identities

### System-Assigned Managed Identity

```bash
# Enable system-assigned identity on VM
az vm identity assign \
  --resource-group myResourceGroup \
  --name myVM

# Enable on App Service
az webapp identity assign \
  --resource-group myResourceGroup \
  --name myWebApp

# Enable on AKS
az aks update \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --enable-managed-identity
```

### User-Assigned Managed Identity

```bash
# Create user-assigned identity
az identity create \
  --resource-group myResourceGroup \
  --name myUserAssignedIdentity

# Get identity details
IDENTITY_ID=$(az identity show \
  --resource-group myResourceGroup \
  --name myUserAssignedIdentity \
  --query id -o tsv)

IDENTITY_PRINCIPAL_ID=$(az identity show \
  --resource-group myResourceGroup \
  --name myUserAssignedIdentity \
  --query principalId -o tsv)

# Assign identity to VM
az vm identity assign \
  --resource-group myResourceGroup \
  --name myVM \
  --identities $IDENTITY_ID

# Assign identity to App Service
az webapp identity assign \
  --resource-group myResourceGroup \
  --name myWebApp \
  --identities $IDENTITY_ID
```

## Role-Based Access Control (RBAC)

### Built-in Roles

```bash
# List built-in roles
az role definition list --output table

# Show role definition
az role definition list --name "Contributor" --output json

# Common built-in roles:
# - Owner: Full access including access management
# - Contributor: Full access except access management
# - Reader: View only access
# - User Access Administrator: Manage user access only
# - Kubernetes roles: Azure Kubernetes Service RBAC Admin, Cluster Admin, etc.
```

### Role Assignments

```bash
# Assign role to user at subscription level
az role assignment create \
  --assignee user@contoso.com \
  --role "Contributor" \
  --scope /subscriptions/{subscription-id}

# Assign role to group at resource group level
az role assignment create \
  --assignee-object-id {GROUP_OBJECT_ID} \
  --assignee-principal-type Group \
  --role "Reader" \
  --resource-group myResourceGroup

# Assign role to managed identity
az role assignment create \
  --assignee $IDENTITY_PRINCIPAL_ID \
  --role "Storage Blob Data Contributor" \
  --scope /subscriptions/{subscription-id}/resourceGroups/myResourceGroup/providers/Microsoft.Storage/storageAccounts/mystorageaccount

# List role assignments
az role assignment list \
  --assignee user@contoso.com \
  --output table

# Remove role assignment
az role assignment delete \
  --assignee user@contoso.com \
  --role "Contributor" \
  --resource-group myResourceGroup
```

### Custom Roles

```json
// custom-role.json
{
  "Name": "Virtual Machine Operator",
  "IsCustom": true,
  "Description": "Can monitor and restart virtual machines",
  "Actions": [
    "Microsoft.Compute/*/read",
    "Microsoft.Compute/virtualMachines/start/action",
    "Microsoft.Compute/virtualMachines/restart/action",
    "Microsoft.Resources/subscriptions/resourceGroups/read",
    "Microsoft.Insights/alertRules/*",
    "Microsoft.Support/*"
  ],
  "NotActions": [],
  "AssignableScopes": [
    "/subscriptions/{subscription-id}"
  ]
}
```

```bash
# Create custom role
az role definition create --role-definition custom-role.json

# Update custom role
az role definition update --role-definition custom-role.json

# Delete custom role
az role definition delete --name "Virtual Machine Operator"
```

## Azure Key Vault

### Creating Key Vault

```bash
# Create Key Vault
az keyvault create \
  --name myKeyVault \
  --resource-group myResourceGroup \
  --location eastus \
  --enable-rbac-authorization false \
  --enable-soft-delete true \
  --soft-delete-retention-days 90 \
  --enable-purge-protection true

# Set access policy for user
az keyvault set-policy \
  --name myKeyVault \
  --upn user@contoso.com \
  --secret-permissions get list set delete \
  --key-permissions get list create delete \
  --certificate-permissions get list create delete

# Set access policy for managed identity
az keyvault set-policy \
  --name myKeyVault \
  --object-id $IDENTITY_PRINCIPAL_ID \
  --secret-permissions get list
```

### Managing Secrets

```bash
# Create secret
az keyvault secret set \
  --vault-name myKeyVault \
  --name database-connection-string \
  --value "Server=myserver;Database=mydb;User=admin;Password=SecurePass123!"

# Get secret
az keyvault secret show \
  --vault-name myKeyVault \
  --name database-connection-string \
  --query value -o tsv

# List secrets
az keyvault secret list \
  --vault-name myKeyVault \
  --output table

# Delete secret (soft delete)
az keyvault secret delete \
  --vault-name myKeyVault \
  --name database-connection-string

# Recover deleted secret
az keyvault secret recover \
  --vault-name myKeyVault \
  --name database-connection-string

# Purge deleted secret (permanent)
az keyvault secret purge \
  --vault-name myKeyVault \
  --name database-connection-string

# Set expiration date
az keyvault secret set \
  --vault-name myKeyVault \
  --name temp-secret \
  --value "temporary" \
  --expires "2024-12-31T23:59:59Z"
```

### Managing Keys

```bash
# Create key
az keyvault key create \
  --vault-name myKeyVault \
  --name myKey \
  --kty RSA \
  --size 4096 \
  --ops encrypt decrypt sign verify wrapKey unwrapKey

# Import key
az keyvault key import \
  --vault-name myKeyVault \
  --name imported-key \
  --pem-file key.pem

# Backup key
az keyvault key backup \
  --vault-name myKeyVault \
  --name myKey \
  --file key-backup.blob

# Restore key
az keyvault key restore \
  --vault-name myKeyVault \
  --file key-backup.blob
```

### Managing Certificates

```bash
# Create self-signed certificate
az keyvault certificate create \
  --vault-name myKeyVault \
  --name myCertificate \
  --policy @policy.json

# policy.json
cat > policy.json << EOF
{
  "issuerParameters": {
    "name": "Self"
  },
  "keyProperties": {
    "exportable": true,
    "keySize": 2048,
    "keyType": "RSA",
    "reuseKey": false
  },
  "x509CertificateProperties": {
    "subject": "CN=myapp.contoso.com",
    "validityInMonths": 12,
    "subjectAlternativeNames": {
      "dnsNames": [
        "myapp.contoso.com",
        "www.myapp.contoso.com"
      ]
    }
  }
}
EOF

# Download certificate
az keyvault certificate download \
  --vault-name myKeyVault \
  --name myCertificate \
  --file certificate.crt

# Import certificate from Let's Encrypt or CA
az keyvault certificate import \
  --vault-name myKeyVault \
  --name imported-cert \
  --file certificate.pfx \
  --password "password"
```

## Network Security Groups (NSGs)

### Creating NSG Rules

```bash
# Create NSG
az network nsg create \
  --resource-group myResourceGroup \
  --name myNSG \
  --location eastus

# Add inbound rule to allow HTTPS
az network nsg rule create \
  --resource-group myResourceGroup \
  --nsg-name myNSG \
  --name AllowHTTPS \
  --priority 100 \
  --direction Inbound \
  --access Allow \
  --protocol Tcp \
  --source-address-prefixes '*' \
  --source-port-ranges '*' \
  --destination-address-prefixes '*' \
  --destination-port-ranges 443

# Add rule to allow SSH from specific IP
az network nsg rule create \
  --resource-group myResourceGroup \
  --nsg-name myNSG \
  --name AllowSSHFromOffice \
  --priority 110 \
  --direction Inbound \
  --access Allow \
  --protocol Tcp \
  --source-address-prefixes '203.0.113.0/24' \
  --source-port-ranges '*' \
  --destination-address-prefixes '*' \
  --destination-port-ranges 22

# Deny all other inbound traffic
az network nsg rule create \
  --resource-group myResourceGroup \
  --nsg-name myNSG \
  --name DenyAllInbound \
  --priority 4096 \
  --direction Inbound \
  --access Deny \
  --protocol '*' \
  --source-address-prefixes '*' \
  --source-port-ranges '*' \
  --destination-address-prefixes '*' \
  --destination-port-ranges '*'

# Associate NSG with subnet
az network vnet subnet update \
  --resource-group myResourceGroup \
  --vnet-name myVNet \
  --name mySubnet \
  --network-security-group myNSG

# List NSG rules
az network nsg rule list \
  --resource-group myResourceGroup \
  --nsg-name myNSG \
  --output table
```

## Azure Firewall

### Deploying Azure Firewall

```bash
# Create Azure Firewall subnet
az network vnet subnet create \
  --resource-group myResourceGroup \
  --vnet-name myVNet \
  --name AzureFirewallSubnet \
  --address-prefix 10.0.100.0/26

# Create public IP for firewall
az network public-ip create \
  --resource-group myResourceGroup \
  --name fw-public-ip \
  --sku Standard \
  --allocation-method Static

# Create Azure Firewall
az network firewall create \
  --resource-group myResourceGroup \
  --name myFirewall \
  --location eastus \
  --enable-dns-proxy true

# Create firewall IP configuration
az network firewall ip-config create \
  --firewall-name myFirewall \
  --name fw-config \
  --public-ip-address fw-public-ip \
  --resource-group myResourceGroup \
  --vnet-name myVNet

# Create application rule collection
az network firewall application-rule create \
  --resource-group myResourceGroup \
  --firewall-name myFirewall \
  --collection-name app-rules \
  --priority 200 \
  --action Allow \
  --name AllowWindowsUpdate \
  --protocols Http=80 Https=443 \
  --source-addresses '*' \
  --target-fqdns '*.microsoft.com' '*.windows.net'

# Create network rule collection
az network firewall network-rule create \
  --resource-group myResourceGroup \
  --firewall-name myFirewall \
  --collection-name net-rules \
  --priority 300 \
  --action Allow \
  --name AllowDNS \
  --protocols UDP \
  --source-addresses '*' \
  --destination-addresses '*' \
  --destination-ports 53
```

## Azure Policy

### Built-in Policies

```bash
# List policy definitions
az policy definition list --output table

# Show policy definition
az policy definition show \
  --name '06a78e20-9358-41c9-923c-fb736d382a4d' # Audit VMs without managed disks

# Assign policy at subscription level
az policy assignment create \
  --name 'audit-vm-managed-disks' \
  --display-name 'Audit VMs without managed disks' \
  --scope /subscriptions/{subscription-id} \
  --policy '06a78e20-9358-41c9-923c-fb736d382a4d'

# Assign policy at resource group level
az policy assignment create \
  --name 'require-tags' \
  --display-name 'Require tags on resources' \
  --scope /subscriptions/{subscription-id}/resourceGroups/myResourceGroup \
  --policy '871b6d14-10aa-478d-b590-94f262ecfa99' \
  --params '{ "tagName": { "value": "Environment" } }'

# List policy assignments
az policy assignment list --output table

# Check compliance
az policy state list \
  --resource-group myResourceGroup \
  --output table
```

### Custom Policy

```json
// custom-policy.json
{
  "properties": {
    "displayName": "Require specific tags",
    "policyType": "Custom",
    "mode": "Indexed",
    "description": "Enforces required tags on resources",
    "metadata": {
      "category": "Tags"
    },
    "parameters": {
      "tagName": {
        "type": "String",
        "metadata": {
          "displayName": "Tag Name",
          "description": "Name of the tag, such as 'Environment'"
        }
      }
    },
    "policyRule": {
      "if": {
        "field": "[concat('tags[', parameters('tagName'), ']')]",
        "exists": "false"
      },
      "then": {
        "effect": "deny"
      }
    }
  }
}
```

```bash
# Create custom policy definition
az policy definition create \
  --name 'require-specific-tags' \
  --display-name 'Require specific tags' \
  --rules 'custom-policy.json' \
  --mode Indexed

# Assign custom policy
az policy assignment create \
  --name 'enforce-tags' \
  --display-name 'Enforce required tags' \
  --scope /subscriptions/{subscription-id}/resourceGroups/myResourceGroup \
  --policy 'require-specific-tags' \
  --params '{ "tagName": { "value": "CostCenter" } }'
```

## Azure Security Center / Defender for Cloud

```bash
# Enable Defender for Cloud
az security pricing create \
  --name VirtualMachines \
  --tier Standard

az security pricing create \
  --name SqlServers \
  --tier Standard

az security pricing create \
  --name AppServices \
  --tier Standard

az security pricing create \
  --name StorageAccounts \
  --tier Standard

az security pricing create \
  --name KubernetesService \
  --tier Standard

# List security assessments
az security assessment list --output table

# Get security score
az security secure-score-controls list --output table

# Enable auto-provisioning
az security auto-provisioning-setting update \
  --auto-provision On \
  --name default
```

## Encryption Best Practices

### Disk Encryption

```bash
# Enable Azure Disk Encryption
az vm encryption enable \
  --resource-group myResourceGroup \
  --name myVM \
  --disk-encryption-keyvault myKeyVault \
  --volume-type all

# Check encryption status
az vm encryption show \
  --resource-group myResourceGroup \
  --name myVM
```

### Storage Encryption

```bash
# Enable infrastructure encryption (double encryption)
az storage account create \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --location eastus \
  --sku Standard_ZRS \
  --encryption-services blob file \
  --require-infrastructure-encryption true

# Use customer-managed key
az storage account update \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --encryption-key-source Microsoft.Keyvault \
  --encryption-key-vault https://mykeyvault.vault.azure.net \
  --encryption-key-name mykey \
  --encryption-key-version {version}
```

## Security Checklist

1. **Identity and Access**
   - ✓ Enable Azure AD Multi-Factor Authentication
   - ✓ Use managed identities instead of credentials
   - ✓ Implement RBAC with least privilege
   - ✓ Regular review of role assignments
   - ✓ Disable local accounts where possible

2. **Network Security**
   - ✓ Use private endpoints for Azure services
   - ✓ Implement NSGs with explicit allow rules
   - ✓ Deploy Azure Firewall for centralized security
   - ✓ Enable DDoS Protection Standard
   - ✓ Use service endpoints for Azure services

3. **Data Protection**
   - ✓ Enable encryption at rest and in transit
   - ✓ Use Azure Key Vault for secrets management
   - ✓ Enable soft delete and purge protection
   - ✓ Implement backup and disaster recovery
   - ✓ Use customer-managed keys for sensitive data

4. **Monitoring and Compliance**
   - ✓ Enable Azure Security Center/Defender
   - ✓ Configure Azure Policy for governance
   - ✓ Enable diagnostic logging on all resources
   - ✓ Set up alerts for security events
   - ✓ Regular security assessments and audits

5. **Application Security**
   - ✓ Use Web Application Firewall (WAF)
   - ✓ Enable Application Insights
   - ✓ Implement certificate management
   - ✓ Use Key Vault references in App Service
   - ✓ Scan container images for vulnerabilities
