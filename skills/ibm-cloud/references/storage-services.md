# Storage Services

## Table of Contents
- [Cloud Object Storage (COS)](#cloud-object-storage-cos)
- [Block Storage for VPC](#block-storage-for-vpc)
- [File Storage for VPC](#file-storage-for-vpc)
- [Backup and Disaster Recovery](#backup-and-disaster-recovery)

---

## Cloud Object Storage (COS)

### Overview

S3-compatible object storage for unstructured data with 99.999999999% (11 nines) durability.

**Use Cases:**
- Static website hosting
- Backup and archival
- Data lake for analytics
- Media files and assets
- Container image registry

### Storage Classes

| Class | Use Case | Retrieval | Cost |
|-------|----------|-----------|------|
| **Standard** | Hot data, frequent access | Instant | $$$ |
| **Vault** | Cool data, monthly access | Instant | $$ |
| **Cold Vault** | Cold data, yearly access | Instant | $ |
| **Flex** | Mixed workload, unpredictable | Instant | Dynamic |
| **Smart Tier** | Automatic tier optimization | Instant | Auto |

### Create Bucket

**CLI:**
```bash
# Install COS plugin
ibmcloud plugin install cloud-object-storage

# Create COS instance
ibmcloud resource service-instance-create my-cos \
  cloud-object-storage standard global \
  -g my-resource-group

# Create service credentials
ibmcloud resource service-key-create my-cos-creds \
  Writer --instance-name my-cos \
  --parameters '{"HMAC": true}'

# Set COS configuration
ibmcloud cos config crn --crn <COS-CRN>
ibmcloud cos config auth --method IAM
ibmcloud cos config region --region us-south

# Create bucket (Standard)
ibmcloud cos bucket-create --bucket my-bucket \
  --ibm-service-instance-id <COS-INSTANCE-ID> \
  --region us-south

# Create bucket (Smart Tier)
ibmcloud cos bucket-create --bucket my-smart-bucket \
  --ibm-service-instance-id <COS-INSTANCE-ID> \
  --region us-south \
  --class Smart
```

**Terraform:**
```hcl
resource "ibm_resource_instance" "cos_instance" {
  name              = "my-cos"
  service           = "cloud-object-storage"
  plan              = "standard"
  location          = "global"
  resource_group_id = ibm_resource_group.rg.id
}

resource "ibm_cos_bucket" "bucket" {
  bucket_name          = "my-bucket"
  resource_instance_id = ibm_resource_instance.cos_instance.id
  region_location      = "us-south"
  storage_class        = "smart"

  activity_tracking {
    read_data_events     = true
    write_data_events    = true
    activity_tracker_crn = ibm_resource_instance.at.id
  }

  metrics_monitoring {
    usage_metrics_enabled  = true
    request_metrics_enabled = true
    metrics_monitoring_crn = ibm_resource_instance.monitor.id
  }
}
```

### Upload and Download Objects

**CLI:**
```bash
# Upload file
ibmcloud cos object-put --bucket my-bucket \
  --key path/to/file.txt \
  --body ./local-file.txt

# Upload with metadata
ibmcloud cos object-put --bucket my-bucket \
  --key data.json \
  --body ./data.json \
  --metadata '{"key1":"value1","key2":"value2"}' \
  --content-type application/json

# Download file
ibmcloud cos object-get --bucket my-bucket \
  --key path/to/file.txt \
  ./downloaded-file.txt

# List objects
ibmcloud cos objects --bucket my-bucket

# Delete object
ibmcloud cos object-delete --bucket my-bucket \
  --key path/to/file.txt
```

**Python SDK:**
```python
import ibm_boto3
from ibm_botocore.client import Config

# Initialize client
cos = ibm_boto3.client(
    's3',
    ibm_api_key_id='<API-KEY>',
    ibm_service_instance_id='<COS-INSTANCE-ID>',
    config=Config(signature_version='oauth'),
    endpoint_url='https://s3.us-south.cloud-object-storage.appdomain.cloud'
)

# Upload object
cos.upload_file(
    Filename='./local-file.txt',
    Bucket='my-bucket',
    Key='path/to/file.txt'
)

# Download object
cos.download_file(
    Bucket='my-bucket',
    Key='path/to/file.txt',
    Filename='./downloaded-file.txt'
)

# List objects
response = cos.list_objects_v2(Bucket='my-bucket')
for obj in response.get('Contents', []):
    print(obj['Key'])

# Delete object
cos.delete_object(Bucket='my-bucket', Key='path/to/file.txt')
```

### Multipart Upload (Large Files)

```python
# Multipart upload for files > 100MB
import ibm_boto3
from ibm_boto3.s3.transfer import TransferConfig

# Configure multipart threshold and chunk size
transfer_config = TransferConfig(
    multipart_threshold=100 * 1024 * 1024,  # 100MB
    max_concurrency=10,
    multipart_chunksize=50 * 1024 * 1024    # 50MB chunks
)

# Upload large file
cos.upload_file(
    Filename='./large-file.zip',
    Bucket='my-bucket',
    Key='archives/large-file.zip',
    Config=transfer_config
)
```

### Bucket Lifecycle Policy

**CLI:**
```bash
# Create lifecycle config file
cat > lifecycle.json <<EOF
{
  "Rules": [
    {
      "ID": "Archive old data",
      "Status": "Enabled",
      "Filter": {
        "Prefix": "logs/"
      },
      "Transitions": [
        {
          "Days": 30,
          "StorageClass": "GLACIER"
        }
      ]
    },
    {
      "ID": "Delete temporary files",
      "Status": "Enabled",
      "Filter": {
        "Prefix": "temp/"
      },
      "Expiration": {
        "Days": 7
      }
    }
  ]
}
EOF

# Apply lifecycle policy
ibmcloud cos bucket-lifecycle-put --bucket my-bucket \
  --lifecycle-configuration file://lifecycle.json
```

**Terraform:**
```hcl
resource "ibm_cos_bucket" "bucket" {
  bucket_name          = "my-bucket"
  resource_instance_id = ibm_resource_instance.cos_instance.id
  region_location      = "us-south"
  storage_class        = "standard"

  lifecycle_rule {
    id      = "archive-old-data"
    enabled = true
    prefix  = "logs/"

    transition {
      days          = 30
      storage_class = "GLACIER"
    }
  }

  lifecycle_rule {
    id      = "delete-temp"
    enabled = true
    prefix  = "temp/"

    expiration {
      days = 7
    }
  }
}
```

### Static Website Hosting

```bash
# Enable website configuration
ibmcloud cos bucket-website-put --bucket my-bucket \
  --website-configuration '{
    "IndexDocument": {"Suffix": "index.html"},
    "ErrorDocument": {"Key": "error.html"}
  }'

# Set bucket public read access
ibmcloud cos bucket-cors-put --bucket my-bucket \
  --cors-configuration '{
    "CORSRules": [{
      "AllowedOrigins": ["*"],
      "AllowedMethods": ["GET", "HEAD"],
      "AllowedHeaders": ["*"]
    }]
  }'

# Website URL
# https://my-bucket.s3-web.us-south.cloud-object-storage.appdomain.cloud
```

### Bucket Replication

```bash
# Enable versioning (required for replication)
ibmcloud cos bucket-versioning-put --bucket my-bucket \
  --versioning-configuration Status=Enabled

# Create replication rule
ibmcloud cos bucket-replication-put --bucket my-bucket \
  --replication-configuration '{
    "Role": "<IAM-ROLE-ARN>",
    "Rules": [{
      "ID": "Replicate to DR region",
      "Status": "Enabled",
      "Priority": 1,
      "Destination": {
        "Bucket": "arn:aws:s3:::my-dr-bucket",
        "StorageClass": "STANDARD"
      }
    }]
  }'
```

### Encryption

**Server-Side Encryption (SSE):**
```bash
# Upload with SSE-S3 (IBM managed keys)
ibmcloud cos object-put --bucket my-bucket \
  --key encrypted-file.txt \
  --body ./file.txt \
  --server-side-encryption AES256

# Upload with SSE-KP (Key Protect)
ibmcloud cos object-put --bucket my-bucket \
  --key encrypted-file.txt \
  --body ./file.txt \
  --server-side-encryption kp \
  --sse-kms-key-id <KEY-PROTECT-KEY-CRN>
```

**Bucket-Level Encryption:**
```hcl
resource "ibm_cos_bucket" "bucket" {
  bucket_name          = "my-encrypted-bucket"
  resource_instance_id = ibm_resource_instance.cos_instance.id
  region_location      = "us-south"
  storage_class        = "standard"
  
  kms_key_crn = ibm_kms_key.key.id
}
```

### Access Control

**Bucket Policy:**
```bash
# Create bucket policy
cat > policy.json <<EOF
{
  "Version": "2012-10-17",
  "Statement": [{
    "Sid": "AllowPublicRead",
    "Effect": "Allow",
    "Principal": "*",
    "Action": "s3:GetObject",
    "Resource": "arn:aws:s3:::my-bucket/*"
  }]
}
EOF

# Apply policy
ibmcloud cos bucket-policy-put --bucket my-bucket \
  --policy file://policy.json
```

**IAM Access:**
```bash
# Grant user access to bucket
ibmcloud iam user-policy-create user@example.com \
  --roles Reader,Writer \
  --service-name cloud-object-storage \
  --service-instance <COS-INSTANCE-ID> \
  --resource-type bucket \
  --resource my-bucket
```

---

## Block Storage for VPC

### Overview

High-performance block storage volumes for virtual server instances.

**Performance Tiers:**
- **3 IOPS/GB**: General purpose (max 48,000 IOPS)
- **5 IOPS/GB**: High-performance apps
- **10 IOPS/GB**: Database workloads (max 96,000 IOPS)
- **Custom**: Define specific IOPS (100-96,000)

### Create and Attach Volume

**CLI:**
```bash
# Create volume
ibmcloud is volume-create my-data-volume \
  10iops-tier \
  us-south-1 \
  --capacity 100 \
  --resource-group-name my-rg

# Attach volume to instance
ibmcloud is instance-volume-attachment-add my-attachment \
  my-vsi my-data-volume

# List volumes
ibmcloud is volumes

# Get volume details
ibmcloud is volume my-data-volume
```

**Terraform:**
```hcl
resource "ibm_is_volume" "data_volume" {
  name           = "my-data-volume"
  profile        = "10iops-tier"
  zone           = "us-south-1"
  capacity       = 100
  resource_group = ibm_resource_group.rg.id

  encryption_key = ibm_kms_key.key.crn
}

resource "ibm_is_instance_volume_attachment" "attachment" {
  instance = ibm_is_instance.vsi.id
  volume   = ibm_is_volume.data_volume.id
  name     = "my-attachment"
}
```

### Format and Mount Volume

```bash
# SSH to instance
ssh root@<VSI-IP>

# List block devices
lsblk

# Create filesystem
mkfs.ext4 /dev/vdd

# Create mount point
mkdir /data

# Mount volume
mount /dev/vdd /data

# Persist mount (add to /etc/fstab)
echo '/dev/vdd /data ext4 defaults,nofail 0 2' >> /etc/fstab

# Verify
df -h /data
```

### Snapshots

```bash
# Create snapshot
ibmcloud is snapshot-create my-snapshot \
  --source-volume my-data-volume \
  --name "backup-2024-01-15"

# List snapshots
ibmcloud is snapshots

# Restore from snapshot
ibmcloud is volume-create restored-volume \
  10iops-tier \
  us-south-1 \
  --snapshot my-snapshot
```

**Terraform:**
```hcl
resource "ibm_is_snapshot" "snapshot" {
  name          = "my-snapshot"
  source_volume = ibm_is_volume.data_volume.id
  resource_group = ibm_resource_group.rg.id
}

resource "ibm_is_volume" "restored_volume" {
  name           = "restored-volume"
  profile        = "10iops-tier"
  zone           = "us-south-1"
  source_snapshot = ibm_is_snapshot.snapshot.id
  resource_group = ibm_resource_group.rg.id
}
```

### Expand Volume

```bash
# Expand volume capacity
ibmcloud is volume-update my-data-volume \
  --capacity 200

# After expansion, resize filesystem
ssh root@<VSI-IP>

# For ext4
resize2fs /dev/vdd

# For xfs
xfs_growfs /data
```

---

## File Storage for VPC

### Overview

NFS-based shared file storage for multiple VPC instances.

**Use Cases:**
- Shared application data
- Content management systems
- Container persistent volumes
- Development environments

### Create File Share

**CLI:**
```bash
# Create file share
ibmcloud is share-create my-share \
  --zone us-south-1 \
  --profile dp2 \
  --size 100 \
  --resource-group-name my-rg

# Create mount target
ibmcloud is share-mount-target-create my-share \
  --vpc my-vpc \
  --name my-mount-target \
  --subnet my-subnet

# Get mount path
ibmcloud is share-mount-target my-share my-mount-target
```

**Terraform:**
```hcl
resource "ibm_is_share" "share" {
  name           = "my-share"
  size           = 100
  profile        = "dp2"
  zone           = "us-south-1"
  resource_group = ibm_resource_group.rg.id
}

resource "ibm_is_share_mount_target" "mount_target" {
  share = ibm_is_share.share.id
  name  = "my-mount-target"
  vpc   = ibm_is_vpc.vpc.id

  virtual_network_interface {
    name   = "my-vni"
    subnet = ibm_is_subnet.subnet.id
  }
}
```

### Mount File Share

```bash
# SSH to instance
ssh root@<VSI-IP>

# Install NFS client
apt-get update && apt-get install -y nfs-common

# Create mount point
mkdir /mnt/share

# Mount file share
mount -t nfs4 -o sec=sys,vers=4.1 \
  <MOUNT-TARGET-IP>:/<SHARE-ID> /mnt/share

# Persist mount (add to /etc/fstab)
echo '<MOUNT-TARGET-IP>:/<SHARE-ID> /mnt/share nfs4 sec=sys,vers=4.1,defaults 0 0' >> /etc/fstab

# Verify
df -h /mnt/share
```

### Kubernetes Persistent Volume

```yaml
# storage-class.yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: ibm-vpc-file
provisioner: vpc.file.csi.ibm.io
parameters:
  profile: "dp2"
  zone: "us-south-1"
  tags: "env:prod"
reclaimPolicy: Delete
volumeBindingMode: Immediate
```

```yaml
# pvc.yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: my-pvc
spec:
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: 100Gi
  storageClassName: ibm-vpc-file
```

```yaml
# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: my-app
  template:
    metadata:
      labels:
        app: my-app
    spec:
      containers:
      - name: app
        image: nginx
        volumeMounts:
        - name: data
          mountPath: /data
      volumes:
      - name: data
        persistentVolumeClaim:
          claimName: my-pvc
```

---

## Backup and Disaster Recovery

### Backup Strategies

**1. COS Bucket Replication:**
```bash
# Cross-region replication for disaster recovery
ibmcloud cos bucket-replication-put --bucket prod-bucket \
  --replication-configuration '{
    "Role": "<IAM-ROLE>",
    "Rules": [{
      "Status": "Enabled",
      "Priority": 1,
      "Destination": {
        "Bucket": "arn:aws:s3:::dr-bucket",
        "Region": "eu-gb"
      }
    }]
  }'
```

**2. Block Storage Snapshots:**
```bash
# Automated snapshot schedule
# Create snapshots daily at 2 AM
ibmcloud is snapshot-consistency-group-create my-cg \
  --name daily-backup \
  --volume-ids <VOLUME-1>,<VOLUME-2>

# Retention: Keep 7 daily, 4 weekly, 12 monthly
```

**3. Database Backups to COS:**
```bash
# PostgreSQL backup to COS
pg_dump -h <DB-HOST> -U admin mydatabase | \
  ibmcloud cos object-put --bucket backups \
  --key postgres/mydatabase-$(date +%Y%m%d).sql.gz

# Automated with cron
0 2 * * * /usr/local/bin/backup-db.sh
```

### Disaster Recovery Architecture

```
Primary Region (us-south)
├── Production Resources
│   ├── IKS Cluster
│   ├── Databases
│   └── Object Storage
└── Continuous Replication

DR Region (eu-gb)
├── Standby Resources
│   ├── IKS Cluster (scaled down)
│   ├── Database Read Replicas
│   └── Object Storage (replicated)
└── Automated Failover
```

**Recovery Time Objective (RTO):**
- Active-Active: 0 (no downtime)
- Active-Passive: < 1 hour
- Backup-Restore: < 4 hours

**Recovery Point Objective (RPO):**
- Synchronous replication: 0 (no data loss)
- Asynchronous replication: < 5 minutes
- Snapshot-based: < 24 hours

---

## Best Practices

### Cloud Object Storage

1. **Use Smart Tier**: Let COS automatically optimize storage costs
2. **Enable Versioning**: Protect against accidental deletion
3. **Lifecycle Policies**: Automatically archive/delete old data
4. **Multipart Upload**: For files > 100MB
5. **CDN Integration**: Use Cloud Internet Services for global delivery
6. **Encryption**: Enable Key Protect for sensitive data
7. **Access Logging**: Track bucket access with Activity Tracker

### Block Storage

1. **Choose Right IOPS**: Match performance to workload
2. **Regular Snapshots**: Daily/weekly backup schedule
3. **Encryption**: Enable encryption with Key Protect
4. **Monitor Performance**: Track IOPS, throughput, latency
5. **Expand Proactively**: Monitor capacity usage

### File Storage

1. **Use dp2 Profile**: Better performance for most workloads
2. **Multiple Mount Targets**: Deploy across zones for HA
3. **NFS v4.1**: Use latest protocol version
4. **Access Control**: Restrict by security group
5. **Backup Strategy**: Sync to COS for disaster recovery

### Cost Optimization

1. **Storage Tiers**: Use appropriate COS storage class
2. **Lifecycle Policies**: Automatically move/delete old data
3. **Right-Size Volumes**: Avoid over-provisioning
4. **Delete Unused Snapshots**: Prune old backups
5. **Monitor Usage**: Track storage metrics and costs
