# Cloud Migration to Alibaba Cloud

## Migration Strategies

### Assessment Phase

**Inventory Discovery**
```
1. Application Inventory
   - List all applications and services
   - Identify dependencies
   - Document data flows
   - Map integration points

2. Infrastructure Inventory
   - Compute resources (VMs, containers)
   - Storage (object, block, file)
   - Databases (relational, NoSQL)
   - Networking (VPCs, load balancers, DNS)
   
3. Data Inventory
   - Data volumes
   - Data types and classifications
   - Compliance requirements
   - Backup and retention policies
```

**Migration Approach Selection**
```
Rehost (Lift and Shift)
├── Pros: Fast, minimal changes, low risk
├── Cons: No cloud optimization, higher long-term cost
└── Use when: Time-constrained, minimal budget, legacy apps

Replatform (Lift, Tinker, and Shift)
├── Pros: Some optimization, moderate effort
├── Cons: Testing required, moderate risk
└── Use when: Minor optimizations desired, compatible services exist

Refactor (Re-architect)
├── Pros: Full cloud-native benefits, optimized performance
├── Cons: High effort, time-consuming, expensive
└── Use when: Legacy limitations, scalability needs, modernization goals

Replace (SaaS)
├── Pros: Minimal maintenance, quick deployment
├── Cons: Vendor lock-in, limited customization
└── Use when: Standard functionality, non-differentiating systems

Retire
├── Pros: Cost savings, reduced complexity
└── Use when: Unused or redundant systems

Retain
├── Pros: No migration risk
└── Use when: Recently upgraded, not ready for cloud, compliance
```

## Service Mapping

### AWS to Alibaba Cloud

**Compute**
```
AWS EC2                 → Alibaba Cloud ECS
AWS Lambda              → Function Compute
AWS Elastic Beanstalk   → Web App Service
AWS Batch               → BatchCompute
AWS Lightsail           → Simple Application Server
```

**Storage**
```
AWS S3                  → OSS (Object Storage Service)
AWS EBS                 → Cloud Disk
AWS EFS                 → NAS (Network Attached Storage)
AWS Glacier             → OSS Archive/Cold Archive
AWS Storage Gateway     → Cloud Storage Gateway
```

**Database**
```
AWS RDS MySQL           → ApsaraDB RDS for MySQL
AWS RDS PostgreSQL      → ApsaraDB RDS for PostgreSQL
AWS Aurora              → PolarDB
AWS DynamoDB            → Table Store
AWS ElastiCache Redis   → ApsaraDB for Redis
AWS ElastiCache Memcached → ApsaraDB for Memcache
AWS DocumentDB          → ApsaraDB for MongoDB
```

**Networking**
```
AWS VPC                 → VPC (Virtual Private Cloud)
AWS ELB                 → SLB (Server Load Balancer)
AWS Route 53            → Alibaba Cloud DNS
AWS CloudFront          → Alibaba Cloud CDN
AWS Direct Connect      → Express Connect
AWS VPN                 → VPN Gateway
AWS API Gateway         → API Gateway
```

**Container & Kubernetes**
```
AWS ECS                 → Container Service
AWS EKS                 → ACK (Container Service for Kubernetes)
AWS ECR                 → Container Registry
AWS Fargate             → Serverless Kubernetes (ASK)
```

**Security & Identity**
```
AWS IAM                 → RAM (Resource Access Management)
AWS KMS                 → KMS (Key Management Service)
AWS WAF                 → Web Application Firewall
AWS Shield              → Anti-DDoS
AWS Security Hub        → Security Center
AWS Secrets Manager     → Secrets Manager
```

**Monitoring & Management**
```
AWS CloudWatch          → CloudMonitor
AWS CloudTrail          → ActionTrail
AWS Config              → Config
AWS Systems Manager     → OOS (Operation Orchestration Service)
```

**DevOps**
```
AWS CodePipeline        → DevOps Pipeline
AWS CodeBuild           → Container Registry Build
AWS CodeDeploy          → CodePipeline
AWS CloudFormation      → ROS (Resource Orchestration Service)
```

### GCP to Alibaba Cloud

**Compute**
```
Compute Engine          → ECS
Cloud Functions         → Function Compute
App Engine              → Web App Service
Cloud Run               → Serverless App Engine (SAE)
```

**Storage**
```
Cloud Storage           → OSS
Persistent Disk         → Cloud Disk
Filestore               → NAS
```

**Database**
```
Cloud SQL MySQL         → ApsaraDB RDS for MySQL
Cloud SQL PostgreSQL    → ApsaraDB RDS for PostgreSQL
Cloud Spanner           → PolarDB-X
Bigtable                → Table Store (Wide Column)
Firestore               → ApsaraDB for MongoDB
Memorystore Redis       → ApsaraDB for Redis
```

**Networking**
```
VPC                     → VPC
Cloud Load Balancing    → SLB
Cloud CDN               → Alibaba Cloud CDN
Cloud DNS               → Alibaba Cloud DNS
Cloud Interconnect      → Express Connect
Cloud VPN               → VPN Gateway
```

**Container & Kubernetes**
```
GKE                     → ACK
Artifact Registry       → Container Registry
Cloud Run               → Serverless Kubernetes (ASK)
```

### Azure to Alibaba Cloud

**Compute**
```
Azure Virtual Machines  → ECS
Azure Functions         → Function Compute
Azure App Service       → Web App Service
Azure Batch             → BatchCompute
```

**Storage**
```
Azure Blob Storage      → OSS
Azure Disk Storage      → Cloud Disk
Azure Files             → NAS
```

**Database**
```
Azure Database for MySQL     → ApsaraDB RDS for MySQL
Azure Database for PostgreSQL → ApsaraDB RDS for PostgreSQL
Azure Cosmos DB              → PolarDB / Table Store
Azure Cache for Redis        → ApsaraDB for Redis
```

**Networking**
```
Azure Virtual Network   → VPC
Azure Load Balancer     → SLB
Azure CDN               → Alibaba Cloud CDN
Azure DNS               → Alibaba Cloud DNS
Azure ExpressRoute      → Express Connect
Azure VPN Gateway       → VPN Gateway
```

**Container & Kubernetes**
```
Azure Kubernetes Service → ACK
Azure Container Registry → Container Registry
Azure Container Instances → Serverless Kubernetes (ASK)
```

## Migration Methods

### Compute Migration

**VM Migration**
```
1. Using Alibaba Cloud Migration Tool (SMC)
   - Install SMC client on source VM
   - Configure Alibaba Cloud credentials
   - Run discovery and create migration task
   - Monitor migration progress
   - Validate migrated instance

2. Manual Image Migration
   - Export VM disk image
   - Upload to OSS
   - Import as custom image
   - Launch ECS from image
   - Configure and test

3. Application-Level Migration
   - Setup target ECS instances
   - Install application dependencies
   - Deploy application code
   - Migrate configuration
   - Cutover DNS
```

**Container Migration**
```
1. Container Registry Migration
   docker pull source-registry.com/image:tag
   docker tag source-registry.com/image:tag target-registry.aliyuncs.com/namespace/image:tag
   docker push target-registry.aliyuncs.com/namespace/image:tag

2. Kubernetes Migration
   - Export manifests from source cluster
   - Modify for ACK compatibility
   - Deploy to ACK cluster
   - Migrate persistent volumes
   - Update DNS/ingress
```

### Database Migration

**RDS Migration Methods**

**1. DTS (Data Transmission Service) - Recommended**
```yaml
Migration Type: Full + Incremental
Source: AWS RDS MySQL / Self-hosted MySQL
Target: Alibaba Cloud RDS MySQL

Steps:
1. Create DTS migration task
2. Configure source database connection
3. Configure target database connection
4. Select migration objects (databases/tables)
5. Pre-check (connectivity, permissions, conflicts)
6. Start migration (full data + incremental sync)
7. Monitor replication lag
8. Cutover when lag < 1 second
9. Verify data integrity
```

**2. mysqldump (For smaller databases < 100GB)**
```bash
# Export from source
mysqldump -h source-host -u user -p \
  --single-transaction \
  --quick \
  --lock-tables=false \
  --databases mydb > dump.sql

# Import to target
mysql -h rm-xxxxx.mysql.rds.aliyuncs.com -u user -p mydb < dump.sql
```

**3. Physical Backup Restore (For large databases)**
```bash
# AWS RDS Snapshot → S3 → OSS → RDS Restore
1. Create RDS snapshot in AWS
2. Export snapshot to S3
3. Copy from S3 to OSS using OssImport
4. Restore from OSS to Alibaba Cloud RDS
```

**PostgreSQL Migration**
```bash
# Using pg_dump/pg_restore
pg_dump -h source-host -U user -F c -d mydb > dump.dump
pg_restore -h target-host -U user -d mydb dump.dump

# Using DTS
- Create DTS migration task
- Select PostgreSQL source/target
- Configure incremental sync
- Monitor and cutover
```

**MongoDB Migration**
```bash
# Using mongodump/mongorestore
mongodump --host source-host --port 27017 \
  --username user --password pass \
  --db mydb --out /backup

mongorestore --host dds-xxxxx.mongodb.rds.aliyuncs.com \
  --port 3717 --username user --password pass \
  --db mydb /backup/mydb

# Using DTS
- Create DTS migration task for MongoDB
- Configure source/target connections
- Select collections to migrate
- Enable incremental sync
- Cutover when lag is minimal
```

**Redis Migration**
```bash
# Using redis-shake (Alibaba tool)
./redis-shake -type sync \
  -source_address source-redis:6379 \
  -source_password pass \
  -target_address r-xxxxx.redis.rds.aliyuncs.com:6379 \
  -target_password pass

# Using RDB file
redis-cli --rdb dump.rdb
# Upload to OSS
# Import from OSS to ApsaraDB Redis
```

### Storage Migration

**Object Storage Migration**

**Using OssImport**
```bash
# Install OssImport
wget http://gosspublic.alicdn.com/ossimport/standalone/ossimport-x.x.x.zip
unzip ossimport-x.x.x.zip
cd ossimport-x.x.x

# Configure local_job.cfg
srcType=s3
srcAccessKey=AWS_ACCESS_KEY
srcSecretKey=AWS_SECRET_KEY
srcDomain=s3.amazonaws.com
srcBucket=source-bucket
srcPrefix=folder/

destAccessKey=ALIYUN_ACCESS_KEY
destSecretKey=ALIYUN_SECRET_KEY
destDomain=oss-cn-hangzhou.aliyuncs.com
destBucket=target-bucket
destPrefix=folder/

# Run migration
bash import.sh

# Monitor progress
bash console.sh stat
```

**Using rclone**
```bash
# Configure rclone
rclone config

# Sync from S3 to OSS
rclone sync s3:source-bucket oss:target-bucket \
  --progress \
  --checkers 20 \
  --transfers 10 \
  --stats 1m

# Copy with verification
rclone copy s3:source-bucket oss:target-bucket \
  --checksum \
  --verbose
```

**Using ossutil**
```bash
# Batch upload from local
ossutil cp -r /local/path oss://bucket/path \
  --jobs 5 \
  --parallel 10

# Sync directories
ossutil sync /local/path oss://bucket/path \
  --delete \
  --update \
  --snapshot-path /path/to/snapshot
```

**File Storage Migration**
```bash
# NAS Migration using rsync
rsync -avz --progress \
  /source/mount/ \
  /alibaba-nas/mount/

# With bandwidth limit
rsync -avz --progress --bwlimit=10240 \
  /source/mount/ \
  /alibaba-nas/mount/

# Incremental sync
rsync -avz --progress --delete \
  /source/mount/ \
  /alibaba-nas/mount/
```

### Network Migration

**VPN Setup for Hybrid Connectivity**
```
1. Setup VPN Gateway in Alibaba Cloud
   - Create VPN Gateway in VPC
   - Create Customer Gateway (source site public IP)
   - Create IPsec connection
   - Configure routing

2. Configure Source Site
   - Setup IPsec VPN client/device
   - Configure phase 1/2 parameters
   - Establish tunnel
   - Test connectivity

3. Verify Connectivity
   ping <alibaba-vpc-ip>
   traceroute <alibaba-vpc-ip>
```

**Express Connect for Dedicated Connection**
```
1. Apply for Express Connect
2. Physical connection setup
3. Configure VBR (Virtual Border Router)
4. Setup routing
5. Test bandwidth and latency
```

## Migration Best Practices

### Pre-Migration Checklist

**Technical Assessment**
```
□ Document current architecture
□ Identify all dependencies
□ Map data flows
□ Assess bandwidth requirements
□ Plan downtime windows
□ Identify security requirements
□ Review compliance needs
```

**Resource Planning**
```
□ Size target infrastructure
□ Calculate costs
□ Plan network connectivity
□ Prepare migration tools
□ Setup monitoring
□ Create rollback plan
```

**Team Preparation**
```
□ Train team on Alibaba Cloud
□ Define roles and responsibilities
□ Establish communication channels
□ Schedule migration windows
□ Prepare runbooks
```

### During Migration

**Best Practices**
```
1. Use incremental migration
   - Minimize downtime
   - Reduce risk
   - Enable validation

2. Monitor continuously
   - Track migration progress
   - Watch for errors
   - Monitor performance

3. Validate data integrity
   - Compare row counts
   - Verify checksums
   - Test application functionality

4. Maintain documentation
   - Log all changes
   - Document issues and resolutions
   - Update diagrams
```

**Cutover Checklist**
```
□ Verify data sync lag < threshold
□ Stop writes to source
□ Final data sync
□ Verify data integrity
□ Update DNS records
□ Test application functionality
□ Monitor for errors
□ Enable production traffic
□ Keep source as backup (hot standby)
```

### Post-Migration

**Validation**
```
□ Verify all applications running
□ Check data consistency
□ Test all integrations
□ Monitor performance
□ Review logs for errors
□ Conduct user acceptance testing
```

**Optimization**
```
□ Right-size resources
□ Implement cost optimizations
□ Setup auto-scaling
□ Configure backups
□ Enable monitoring alerts
□ Document final architecture
```

**Decommission**
```
□ Keep source running for rollback period (1-4 weeks)
□ Cancel old subscriptions
□ Delete temporary resources
□ Archive migration documentation
□ Conduct post-mortem
```

## Migration Timeline Example

**Phase 1: Assessment (2-4 weeks)**
- Infrastructure discovery
- Application mapping
- Dependency analysis
- Migration strategy selection
- Cost estimation
- Team training

**Phase 2: Planning (2-3 weeks)**
- Detailed migration plan
- Runbook creation
- Tool setup
- Pilot migration test
- Risk assessment
- Approval process

**Phase 3: Pilot Migration (1-2 weeks)**
- Migrate non-critical workload
- Test procedures
- Refine runbooks
- Validate tools and processes
- Document lessons learned

**Phase 4: Production Migration (4-8 weeks)**
- Wave-based migration
- Continuous monitoring
- Issue resolution
- User validation
- Performance tuning

**Phase 5: Optimization (2-4 weeks)**
- Right-sizing
- Cost optimization
- Security hardening
- Backup configuration
- Documentation finalization

**Phase 6: Decommission (2-4 weeks)**
- Source environment cleanup
- Final verification
- Knowledge transfer
- Post-mortem review
