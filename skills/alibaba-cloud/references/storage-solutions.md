# Storage Solutions

## OSS (Object Storage Service)

### Bucket Configuration

**Storage Classes**
- **Standard**: Frequent access, low latency, high throughput
- **Infrequent Access (IA)**: Less frequent access, lower storage cost
- **Archive**: Long-term archival, lowest cost, retrieval time required
- **Cold Archive**: Ultra-low cost, longer retrieval time (hours)

**Use Case Mapping**
```
Hot data (daily access)      → Standard
Warm data (monthly access)   → IA
Cold data (yearly access)    → Archive
Compliance archives          → Cold Archive
```

### Access Control

**Bucket Policies**
```json
{
  "Version": "1",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "oss:GetObject"
      ],
      "Resource": [
        "acs:oss:*:*:my-bucket/public/*"
      ],
      "Principal": ["*"]
    },
    {
      "Effect": "Deny",
      "Action": [
        "oss:PutObject"
      ],
      "Resource": [
        "acs:oss:*:*:my-bucket/protected/*"
      ],
      "Principal": ["*"]
    }
  ]
}
```

**RAM Policy Example**
```json
{
  "Version": "1",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "oss:ListBuckets",
        "oss:GetBucket*"
      ],
      "Resource": [
        "acs:oss:*:*:*"
      ]
    },
    {
      "Effect": "Allow",
      "Action": [
        "oss:GetObject",
        "oss:PutObject",
        "oss:DeleteObject"
      ],
      "Resource": [
        "acs:oss:*:*:my-bucket/user-${ram:user-name}/*"
      ]
    }
  ]
}
```

**Signed URLs**
```python
import oss2

auth = oss2.Auth('AccessKeyId', 'AccessKeySecret')
bucket = oss2.Bucket(auth, 'https://oss-cn-hangzhou.aliyuncs.com', 'my-bucket')

# Generate signed URL (expires in 1 hour)
url = bucket.sign_url('GET', 'file.pdf', 3600)

# Upload with signed URL
upload_url = bucket.sign_url('PUT', 'upload.jpg', 3600)
```

### Encryption

**Server-Side Encryption (SSE)**
- **SSE-OSS**: Managed by OSS using AES-256
- **SSE-KMS**: Managed by KMS with envelope encryption
- **SSE-C**: Customer-provided keys

**Configuration**
```python
# Enable SSE-OSS at bucket level
bucket.put_bucket_encryption(
    oss2.models.ServerSideEncryptionRule(
        sse_algorithm='AES256'
    )
)

# Enable SSE-KMS
bucket.put_bucket_encryption(
    oss2.models.ServerSideEncryptionRule(
        sse_algorithm='KMS',
        kms_master_key_id='your-kms-key-id'
    )
)
```

### Lifecycle Management

**Example Rules**
```xml
<LifecycleConfiguration>
  <Rule>
    <ID>Delete old logs</ID>
    <Prefix>logs/</Prefix>
    <Status>Enabled</Status>
    <Expiration>
      <Days>30</Days>
    </Expiration>
  </Rule>
  
  <Rule>
    <ID>Archive old backups</ID>
    <Prefix>backups/</Prefix>
    <Status>Enabled</Status>
    <Transition>
      <Days>90</Days>
      <StorageClass>Archive</StorageClass>
    </Transition>
  </Rule>
  
  <Rule>
    <ID>Transition to IA</ID>
    <Prefix>documents/</Prefix>
    <Status>Enabled</Status>
    <Transition>
      <Days>30</Days>
      <StorageClass>IA</StorageClass>
    </Transition>
  </Rule>
</LifecycleConfiguration>
```

### Versioning

**Enable Versioning**
```python
bucket.put_bucket_versioning(
    oss2.models.BucketVersioningConfig(oss2.BUCKET_VERSIONING_ENABLE)
)

# List object versions
for obj in oss2.ObjectVersionIterator(bucket, prefix='documents/'):
    print(f'{obj.key}, {obj.versionid}, {obj.is_latest}')

# Restore specific version
bucket.copy_object(
    source_bucket_name='my-bucket',
    source_key='file.txt',
    target_key='file.txt',
    params={'versionId': 'version-id'}
)
```

### Cross-Region Replication

**Configuration**
```xml
<ReplicationConfiguration>
  <Rule>
    <ID>Replicate to backup region</ID>
    <Prefix>critical/</Prefix>
    <Status>Enabled</Status>
    <Destination>
      <Bucket>acs:oss:oss-cn-beijing::backup-bucket</Bucket>
      <Location>oss-cn-beijing</Location>
    </Destination>
    <HistoricalObjectReplication>enabled</HistoricalObjectReplication>
  </Rule>
</ReplicationConfiguration>
```

**Use Cases**
- Disaster recovery and backup
- Data sovereignty and compliance
- Latency optimization for global users
- Aggregate logs from multiple regions

### CDN Integration

**Enable CDN for OSS**
```
1. Create CDN domain
2. Set origin as OSS bucket endpoint
3. Configure cache rules
4. Enable HTTPS with SSL certificate
5. Configure access control
```

**Cache Rules**
```
File Type       TTL     Priority
--------------------------------
.jpg, .png      1 day   1
.css, .js       7 days  2
.html           1 hour  3
```

### Performance Optimization

**Multipart Upload**
```python
# For files > 100MB
import oss2

bucket = oss2.Bucket(auth, endpoint, bucket_name)

# Initialize multipart upload
upload_id = bucket.init_multipart_upload('large-file.zip').upload_id

# Upload parts (can be parallel)
parts = []
part_size = 10 * 1024 * 1024  # 10MB per part
with open('large-file.zip', 'rb') as f:
    part_number = 1
    while True:
        data = f.read(part_size)
        if not data:
            break
        result = bucket.upload_part('large-file.zip', upload_id, part_number, data)
        parts.append(oss2.models.PartInfo(part_number, result.etag))
        part_number += 1

# Complete upload
bucket.complete_multipart_upload('large-file.zip', upload_id, parts)
```

**Resumable Upload**
```python
# Automatically handles interruption and resume
oss2.resumable_upload(
    bucket,
    'large-file.zip',
    'local-file.zip',
    multipart_threshold=10 * 1024 * 1024,
    part_size=10 * 1024 * 1024,
    num_threads=4
)
```

**Batch Operations**
```python
# Delete multiple objects
bucket.batch_delete_objects(['file1.txt', 'file2.txt', 'file3.txt'])

# List objects with pagination
for obj in oss2.ObjectIterator(bucket, prefix='documents/', max_keys=100):
    print(obj.key)
```

### Image Processing

**URL-based Processing**
```
# Resize to 200x200
https://bucket.oss-cn-hangzhou.aliyuncs.com/image.jpg?x-oss-process=image/resize,w_200,h_200

# Add watermark
?x-oss-process=image/watermark,text_SGVsbG8,color_FF0000,size_30

# Multiple operations (pipeline)
?x-oss-process=image/resize,w_300|image/watermark,text_Logo
```

**Supported Operations**
- Resize, crop, rotate
- Watermark (text/image)
- Format conversion
- Quality adjustment
- Blur, sharpen, brightness

### Monitoring and Logging

**Access Logging**
```python
# Enable access logging
bucket.put_bucket_logging(
    oss2.models.BucketLogging(
        target_bucket='log-bucket',
        target_prefix='oss-access-logs/'
    )
)
```

**Real-time Log Service**
```python
# Enable real-time logging to SLS
bucket.put_bucket_logging_config(
    oss2.models.PutBucketLoggingRequest(
        logging_enabled=oss2.models.LoggingEnabled(
            target_bucket='log-bucket',
            target_prefix='realtime-logs/'
        )
    )
)
```

**Metrics to Monitor**
- Request count
- Traffic (inbound/outbound)
- Error rates (4xx, 5xx)
- Average latency
- Storage capacity

## NAS (Network Attached Storage)

### NAS Types

**Capacity NAS**
- Cost-effective for large-scale storage
- 1PB+ capacity
- Use cases: Big data, backup, archiving

**Performance NAS**
- High throughput and IOPS
- < 1ms latency
- Use cases: Databases, high-performance computing

**Extreme NAS**
- Ultra-high performance
- Up to 100GB/s throughput
- Use cases: AI training, HPC, media processing

### Mount Configuration

**Linux (NFSv3/NFSv4)**
```bash
# Install NFS client
yum install -y nfs-utils

# Create mount point
mkdir -p /mnt/nas

# Mount NAS
mount -t nfs -o vers=3,nolock,proto=tcp,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2 \
  file-system-id.region.nas.aliyuncs.com:/ /mnt/nas

# Persistent mount (add to /etc/fstab)
echo "file-system-id.region.nas.aliyuncs.com:/ /mnt/nas nfs vers=3,nolock,proto=tcp,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2 0 0" >> /etc/fstab
```

**Windows (SMB)**
```powershell
# Map network drive
net use Z: \\file-system-id.region.nas.aliyuncs.com\myshare /persistent:yes
```

### Performance Optimization

**Best Practices**
- Use appropriate mount options (rsize/wsize)
- Enable async writes for better performance
- Use multiple mount points for parallelism
- Monitor bandwidth and IOPS usage
- Configure appropriate file system quotas

### Access Control

**Permission Groups**
```
Rule Type: IP Address
IP Address: 192.168.1.0/24
Permission: Read/Write
User Mapping: root -> nobody (squash)
```

**VPC Configuration**
- Create NAS file system in VPC
- Add mount targets in VSwitches
- Configure security groups to allow NFS/SMB ports

## Table Store (NoSQL)

### Data Model

**Table Structure**
```
Table
├── Primary Keys (1-4 columns)
│   ├── Partition Key (required)
│   └── Sort Keys (optional, 1-3)
└── Attribute Columns (unlimited)
```

### Use Cases

**Time Series Data**
```
Table: metrics
PK: device_id, timestamp
Attributes: temperature, humidity, pressure
TTL: 30 days
```

**User Profile**
```
Table: users
PK: user_id
Attributes: name, email, preferences (JSON), last_login
Global Secondary Index: email
```

**Shopping Cart**
```
Table: carts
PK: user_id, item_id
Attributes: quantity, price, added_at
```

### Index Types

**Global Secondary Index (GSI)**
- Different partition key from main table
- Supports different attributes
- Async replication

**Local Secondary Index (LSI)**
- Same partition key as main table
- Different sort key
- Strongly consistent reads

### Best Practices

**Schema Design**
- Choose partition key with high cardinality
- Use sort key for range queries
- Limit attribute column size (< 2MB per row)
- Use sparse columns for optional data
- Implement TTL for time-bound data

**Performance**
- Pre-shard tables for high write throughput
- Use batch operations for bulk reads/writes
- Enable auto-scaling for capacity units
- Monitor throttling and adjust capacity
- Use indexes strategically (cost vs query needs)

**Cost Optimization**
- Use reserved capacity for predictable workloads
- Enable TTL to automatically delete old data
- Compress large attribute values
- Use on-demand capacity for unpredictable traffic
