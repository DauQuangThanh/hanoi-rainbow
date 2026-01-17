# Azure Storage Solutions

## Azure Blob Storage

### Storage Account Types

**Performance Tiers:**
- **Standard**: General-purpose, cost-effective, backed by HDDs
- **Premium**: High-performance, backed by SSDs, low latency

**Account Kinds:**
- **StorageV2 (General-purpose v2)**: Recommended for most scenarios
- **BlobStorage**: Legacy, blob-only accounts
- **BlockBlobStorage**: Premium block blobs and append blobs
- **FileStorage**: Premium file shares

### Access Tiers

**Hot Tier**
- Optimized for frequent access
- Higher storage costs, lower access costs
- Use for active data

**Cool Tier**
- Optimized for infrequent access (stored 30+ days)
- Lower storage costs, higher access costs
- Use for short-term backup and archives

**Archive Tier**
- Lowest storage costs, highest access costs
- Hours of latency for retrieval
- Use for long-term archival (stored 180+ days)

### Creating Storage Account

```bash
# Create storage account with best practices
az storage account create \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --location eastus \
  --sku Standard_ZRS \
  --kind StorageV2 \
  --access-tier Hot \
  --https-only true \
  --min-tls-version TLS1_2 \
  --allow-blob-public-access false \
  --default-action Deny \
  --bypass AzureServices

# Enable soft delete for blobs
az storage blob service-properties delete-policy update \
  --account-name mystorageaccount \
  --enable true \
  --days-retained 30

# Enable versioning
az storage account blob-service-properties update \
  --account-name mystorageaccount \
  --resource-group myResourceGroup \
  --enable-versioning true
```

### Blob Operations

```bash
# Upload blob
az storage blob upload \
  --account-name mystorageaccount \
  --container-name mycontainer \
  --name myblob.txt \
  --file ./local-file.txt \
  --auth-mode login

# Set blob tier
az storage blob set-tier \
  --account-name mystorageaccount \
  --container-name mycontainer \
  --name myblob.txt \
  --tier Cool

# Copy blob between containers
az storage blob copy start \
  --account-name mystorageaccount \
  --destination-blob dest-blob.txt \
  --destination-container dest-container \
  --source-container source-container \
  --source-blob source-blob.txt

# Generate SAS token
az storage blob generate-sas \
  --account-name mystorageaccount \
  --container-name mycontainer \
  --name myblob.txt \
  --permissions r \
  --expiry 2024-12-31T23:59Z \
  --https-only
```

### Lifecycle Management

```json
{
  "rules": [
    {
      "name": "moveToCool",
      "enabled": true,
      "type": "Lifecycle",
      "definition": {
        "filters": {
          "blobTypes": ["blockBlob"],
          "prefixMatch": ["logs/"]
        },
        "actions": {
          "baseBlob": {
            "tierToCool": {
              "daysAfterModificationGreaterThan": 30
            },
            "tierToArchive": {
              "daysAfterModificationGreaterThan": 90
            },
            "delete": {
              "daysAfterModificationGreaterThan": 365
            }
          }
        }
      }
    }
  ]
}
```

### Immutable Storage (WORM)

```bash
# Enable immutability policy
az storage container immutability-policy create \
  --account-name mystorageaccount \
  --container-name compliance-container \
  --period 2555 \
  --resource-group myResourceGroup

# Enable legal hold
az storage container legal-hold set \
  --account-name mystorageaccount \
  --container-name compliance-container \
  --tags audit investigation
```

## Azure Files

### File Share Tiers

**Premium (FileStorage account)**
- SSD-backed, low latency
- Provisioned model (pay for provisioned size)
- Up to 100,000 IOPS per share
- Use for high-performance workloads

**Transaction Optimized (Standard)**
- HDD-backed
- Pay for used storage + transactions
- Use for general-purpose workloads

**Hot/Cool (Standard)**
- Cost-optimized tiers
- Lower storage costs than transaction optimized
- Use based on access patterns

### Creating File Share

```bash
# Create file share
az storage share create \
  --account-name mystorageaccount \
  --name myfileshare \
  --quota 1024 \
  --enabled-protocols SMB

# Create NFS file share (Premium only)
az storage share create \
  --account-name mypremiumstorage \
  --name mynfsshare \
  --enabled-protocols NFS \
  --quota 100
```

### Mounting File Shares

**Windows:**
```powershell
$connectTestResult = Test-NetConnection -ComputerName mystorageaccount.file.core.windows.net -Port 445
if ($connectTestResult.TcpTestSucceeded) {
    cmd.exe /C "cmdkey /add:`"mystorageaccount.file.core.windows.net`" /user:`"Azure\mystorageaccount`" /pass:`"storagekey`""
    New-PSDrive -Name Z -PSProvider FileSystem -Root "\\mystorageaccount.file.core.windows.net\myfileshare" -Persist
}
```

**Linux:**
```bash
# Install cifs-utils
sudo apt-get update
sudo apt-get install cifs-utils

# Create credentials file
sudo bash -c 'echo "username=mystorageaccount" >> /etc/smbcredentials.txt'
sudo bash -c 'echo "password=storagekey" >> /etc/smbcredentials.txt'
sudo chmod 600 /etc/smbcredentials.txt

# Mount file share
sudo mkdir -p /mnt/myfileshare
sudo mount -t cifs //mystorageaccount.file.core.windows.net/myfileshare /mnt/myfileshare -o vers=3.0,credentials=/etc/smbcredentials.txt,dir_mode=0777,file_mode=0777,serverino

# Add to /etc/fstab for persistent mount
echo "//mystorageaccount.file.core.windows.net/myfileshare /mnt/myfileshare cifs vers=3.0,credentials=/etc/smbcredentials.txt,dir_mode=0777,file_mode=0777,serverino 0 0" | sudo tee -a /etc/fstab
```

### Azure File Sync

```bash
# Install Azure File Sync agent on Windows Server
# Register server with Storage Sync Service
Import-Module "C:\Program Files\Azure\StorageSyncAgent\StorageSync.Management.PowerShell.Cmdlets.dll"
Register-AzStorageSyncServer -ResourceGroupName myResourceGroup -StorageSyncServiceName myStorageSyncService

# Create sync group
New-AzStorageSyncGroup -ResourceGroupName myResourceGroup -StorageSyncServiceName myStorageSyncService -SyncGroupName mySyncGroup

# Add cloud endpoint (Azure File Share)
New-AzStorageSyncCloudEndpoint -ResourceGroupName myResourceGroup -StorageSyncServiceName myStorageSyncService -SyncGroupName mySyncGroup -StorageAccountResourceId $storageAccountId -AzureFileShareName myfileshare

# Add server endpoint
New-AzStorageSyncServerEndpoint -ResourceGroupName myResourceGroup -StorageSyncServiceName myStorageSyncService -SyncGroupName mySyncGroup -ServerId $serverId -ServerLocalPath "D:\SyncFolder" -CloudTiering -VolumeFreeSpacePercent 20
```

## Azure Disk Storage

### Disk Types Comparison

| Feature | Ultra Disk | Premium SSD v2 | Premium SSD | Standard SSD | Standard HDD |
|---------|-----------|----------------|-------------|--------------|--------------|
| Max IOPS | 160,000 | 80,000 | 20,000 | 6,000 | 2,000 |
| Max Throughput | 4,000 MB/s | 1,200 MB/s | 900 MB/s | 750 MB/s | 500 MB/s |
| Latency | Sub-ms | Sub-ms | Single-digit ms | Single-digit ms | Tens of ms |
| Use Case | Mission-critical | High-performance | Production | Dev/test | Backup |

### Creating Managed Disks

```bash
# Create Premium SSD
az disk create \
  --resource-group myResourceGroup \
  --name myPremiumDisk \
  --size-gb 128 \
  --sku Premium_LRS \
  --zone 1

# Create Ultra Disk
az disk create \
  --resource-group myResourceGroup \
  --name myUltraDisk \
  --size-gb 1024 \
  --sku UltraSSD_LRS \
  --disk-iops-read-write 80000 \
  --disk-mbps-read-write 1200 \
  --zone 1

# Attach disk to VM
az vm disk attach \
  --resource-group myResourceGroup \
  --vm-name myVM \
  --name myPremiumDisk

# Detach disk
az vm disk detach \
  --resource-group myResourceGroup \
  --vm-name myVM \
  --name myPremiumDisk
```

### Disk Encryption

```bash
# Create Key Vault for disk encryption
az keyvault create \
  --name myKeyVault \
  --resource-group myResourceGroup \
  --location eastus \
  --enabled-for-disk-encryption true

# Enable disk encryption on VM
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

### Disk Snapshots

```bash
# Create snapshot
az snapshot create \
  --resource-group myResourceGroup \
  --name mySnapshot \
  --source myDisk

# Create disk from snapshot
az disk create \
  --resource-group myResourceGroup \
  --name myNewDisk \
  --source mySnapshot \
  --sku Premium_LRS
```

## Azure Data Lake Storage Gen2

### Features

- Hierarchical namespace for efficient directory operations
- POSIX-compliant ACLs
- Big data analytics optimized
- Built on Azure Blob Storage (all blob features available)

### Creating ADLS Gen2

```bash
# Create storage account with hierarchical namespace
az storage account create \
  --name myadlsaccount \
  --resource-group myResourceGroup \
  --location eastus \
  --sku Standard_LRS \
  --kind StorageV2 \
  --hierarchical-namespace true

# Create file system (container)
az storage fs create \
  --name myfilesystem \
  --account-name myadlsaccount \
  --auth-mode login

# Create directory
az storage fs directory create \
  --name data/2024/01 \
  --file-system myfilesystem \
  --account-name myadlsaccount \
  --auth-mode login
```

### Access Control (POSIX ACLs)

```bash
# Set ACL on directory
az storage fs access set \
  --acl "user::rwx,group::r-x,other::---,user:oid:rwx" \
  --path data/sensitive \
  --file-system myfilesystem \
  --account-name myadlsaccount

# Set default ACL (inherited by new files)
az storage fs access set \
  --acl "default:user::rwx,default:group::r-x,default:other::---" \
  --path data \
  --file-system myfilesystem \
  --account-name myadlsaccount
```

## Security Best Practices

### Private Endpoints

```bash
# Disable public access
az storage account update \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --default-action Deny

# Create private endpoint
az network private-endpoint create \
  --name myPrivateEndpoint \
  --resource-group myResourceGroup \
  --vnet-name myVNet \
  --subnet privateEndpointSubnet \
  --private-connection-resource-id $storageAccountId \
  --group-id blob \
  --connection-name myConnection

# Create private DNS zone
az network private-dns zone create \
  --resource-group myResourceGroup \
  --name "privatelink.blob.core.windows.net"

# Link DNS zone to VNet
az network private-dns link vnet create \
  --resource-group myResourceGroup \
  --zone-name "privatelink.blob.core.windows.net" \
  --name MyDNSLink \
  --virtual-network myVNet \
  --registration-enabled false
```

### Encryption Options

1. **Microsoft-managed keys (default)**: Automatic key rotation
2. **Customer-managed keys**: Use Azure Key Vault for key control
3. **Customer-provided keys**: Provide keys per-request (Blob only)

```bash
# Enable customer-managed key encryption
az storage account update \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --encryption-key-source Microsoft.Keyvault \
  --encryption-key-vault https://mykeyvault.vault.azure.net \
  --encryption-key-name mykey
```

### Network Security

```bash
# Allow specific VNets
az storage account network-rule add \
  --account-name mystorageaccount \
  --resource-group myResourceGroup \
  --vnet-name myVNet \
  --subnet mySubnet

# Allow specific IP addresses
az storage account network-rule add \
  --account-name mystorageaccount \
  --resource-group myResourceGroup \
  --ip-address 203.0.113.5

# Enable Azure Services bypass
az storage account update \
  --name mystorageaccount \
  --resource-group myResourceGroup \
  --bypass AzureServices Logging Metrics
```
