# Database Services

## RDS (Relational Database Service)

### Supported Engines

**MySQL**
- Versions: 5.6, 5.7, 8.0
- Max connections: Up to 10,000
- Storage: 20GB - 64TB
- Use cases: Web applications, e-commerce, CMS

**PostgreSQL**
- Versions: 10, 11, 12, 13, 14, 15
- Advanced features: JSON, full-text search, PostGIS
- Use cases: Complex queries, geospatial data, data warehousing

**SQL Server**
- Versions: 2008 R2, 2012, 2016, 2017, 2019, 2022
- Editions: Web, Standard, Enterprise
- Use cases: .NET applications, Windows environments

**MariaDB**
- Compatible with MySQL
- Enhanced performance and features
- Use cases: MySQL migration, high-performance scenarios

### Instance Specifications

**Basic Edition**
- Single node (no HA)
- Cost-effective for dev/test
- Not recommended for production

**High-Availability Edition**
- Primary-standby architecture
- Auto-failover within 30 seconds
- Recommended for production

**Cluster Edition (MySQL/PostgreSQL)**
- 1 primary + 1+ read replicas
- Higher read capacity
- Auto-failover and load balancing

### Storage Types

**ESSD (Enhanced SSD)**
```
PL1: 50,000 IOPS, 150 MB/s (standard)
PL2: 100,000 IOPS, 350 MB/s (high performance)
PL3: 1,000,000 IOPS, 4,000 MB/s (ultra performance)
```

**SSD**
- Legacy option
- Lower performance than ESSD
- Consider ESSD PL1 for new deployments

**Selection Guide**
```
Workload              Storage Type    Size
---------------------------------------------
Dev/Test              ESSD PL1        20-100GB
Small Production      ESSD PL1        100-500GB
Medium Production     ESSD PL2        500GB-2TB
Large Production      ESSD PL3        2TB+
Analytics/DW          ESSD PL3        1TB+
```

### Network Configuration

**VPC Setup**
```
1. Create RDS in VPC
2. Select VSwitch in availability zone
3. Configure security group/IP whitelist
4. Enable internal endpoint for ECS access
5. Optional: Enable public endpoint (with EIP)
```

**Security Group Rules**
```
Type: Custom TCP
Port: 3306 (MySQL), 5432 (PostgreSQL), 1433 (SQL Server)
Source: 
  - ECS security group (recommended)
  - VPC CIDR block
  - Specific IP addresses
```

**IP Whitelist**
```
# Internal access
192.168.0.0/16

# Specific ECS instances
172.16.1.10
172.16.1.11

# On-premises via VPN
10.0.0.0/8
```

### Read-Only Instances

**Configuration**
```
Primary Instance
├── Read-Only Instance 1 (same zone)
├── Read-Only Instance 2 (different zone)
└── Read-Only Instance 3 (different zone)
```

**Read-Only Routing**
```
Read Endpoint: rr-xxxxxx.mysql.rds.aliyuncs.com
Delay Threshold: 30 seconds
Load Balancing: Least connections
Weight Distribution: Auto (based on specs)
```

**Use Cases**
- Offload read traffic from primary
- Analytics and reporting queries
- Read-heavy application scaling
- Cross-AZ disaster recovery

**Best Practices**
- Match specs with primary for consistent performance
- Monitor replication lag
- Route heavy analytics to read replicas
- Use multiple read replicas across AZs

### Backup and Recovery

**Automatic Backup**
```
Backup Time: 02:00-03:00 (off-peak)
Retention: 7-730 days (7 days default)
Backup Method: 
  - Physical (faster restore)
  - Logical (smaller backup size)
Backup Frequency: Daily
```

**Snapshot Backup**
```
# Manual snapshot
Create before:
- Schema changes
- Application upgrades
- Data migrations

Retention: Permanent (until manually deleted)
```

**Point-in-Time Recovery (PITR)**
```
Recovery Window: Within backup retention period
Granularity: Any point in time (5-minute intervals)
Method: Restore to new instance
Use cases: 
  - Recover from accidental deletion
  - Rollback bad deployment
  - Create test environment from production
```

**Recovery Procedures**
```
1. Clone to New Instance
   - Creates new RDS from backup/snapshot
   - Original instance remains unchanged
   - Useful for testing recovery

2. Overwrite Current Instance
   - Restores data to existing instance
   - Causes downtime
   - Use for critical recovery only
```

### Performance Optimization

**Parameter Tuning (MySQL)**
```sql
-- Connection pool
max_connections = 1000
wait_timeout = 300
interactive_timeout = 300

-- Buffer pool (70-80% of RAM)
innodb_buffer_pool_size = 8G
innodb_buffer_pool_instances = 8

-- Query cache (use with caution in 5.7, removed in 8.0)
query_cache_type = 0  # Disable for write-heavy workloads

-- Logging
slow_query_log = 1
long_query_time = 2
log_queries_not_using_indexes = 1

-- InnoDB settings
innodb_flush_log_at_trx_commit = 2  # Relaxed durability
innodb_log_file_size = 512M
innodb_io_capacity = 2000
```

**Query Optimization**
```sql
-- Use EXPLAIN to analyze queries
EXPLAIN SELECT * FROM users WHERE email = 'user@example.com';

-- Create appropriate indexes
CREATE INDEX idx_email ON users(email);
CREATE INDEX idx_status_created ON orders(status, created_at);

-- Avoid SELECT *
SELECT id, name, email FROM users WHERE status = 'active';

-- Use prepared statements
PREPARE stmt FROM 'SELECT * FROM users WHERE id = ?';
```

**Monitoring Metrics**
- CPU utilization (< 70% average)
- Memory usage (< 80%)
- IOPS (< 80% of provisioned)
- Connections (< 80% of max)
- Replication lag (< 10 seconds)
- Slow query count
- Lock waits

### Security

**SSL/TLS Connection**
```python
import pymysql

connection = pymysql.connect(
    host='rm-xxxxxx.mysql.rds.aliyuncs.com',
    user='username',
    password='password',
    database='mydb',
    ssl={'ca': '/path/to/ca-cert.pem'}
)
```

**Transparent Data Encryption (TDE)**
```sql
-- Enable TDE (MySQL 5.7+)
ALTER TABLE sensitive_data ENCRYPTION='Y';

-- PostgreSQL (enabled at instance level)
-- Automatic for all tables
```

**SQL Audit**
```
Enable SQL Audit for compliance:
- All SQL statements logged
- Retention: 30 days to 5 years
- Filter by: User, Database, SQL type
- Export to OSS or Log Service
```

**RAM Access Control**
```json
{
  "Version": "1",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "rds:DescribeDBInstances",
        "rds:DescribeBackups"
      ],
      "Resource": "*"
    },
    {
      "Effect": "Allow",
      "Action": [
        "rds:ModifySecurityIps",
        "rds:CreateBackup"
      ],
      "Resource": "acs:rds:*:*:dbinstance/rm-xxxxxx"
    }
  ]
}
```

## PolarDB

### Architecture

**Compute-Storage Separation**
```
Compute Nodes (1 Primary + N Read-Only)
         ↓
Shared Storage Pool (up to 100TB)
         ↓
3-way Replication (across AZs)
```

**Key Features**
- Storage scales independently
- Add read nodes without storage copy
- Faster failover (< 30 seconds)
- Parallel queries (PolarDB-X)

### Use Cases

**vs RDS MySQL**
```
Choose PolarDB when:
- Need > 10TB storage
- Heavy read workloads (> 5 read replicas)
- Rapid scaling requirements
- Cost-sensitive large databases

Choose RDS MySQL when:
- < 10TB storage
- Simple deployment
- MySQL compatibility critical
- Budget constrained (smaller instances)
```

### Cluster Configuration

**Node Specifications**
```
polar.mysql.x2.medium: 2 cores, 4GB RAM
polar.mysql.x4.large: 4 cores, 16GB RAM
polar.mysql.x4.xlarge: 8 cores, 32GB RAM
polar.mysql.x8.xlarge: 8 cores, 64GB RAM
polar.mysql.x8.4xlarge: 32 cores, 256GB RAM
```

**Cluster Endpoint Types**
- Primary Endpoint: All write operations
- Cluster Endpoint: Auto read/write splitting
- Custom Endpoint: Specific read node group

### Multi-Zone Deployment

**Configuration**
```
Primary Node: Zone A
Read Node 1: Zone B
Read Node 2: Zone C

Storage Replication:
- Zone A: Primary copy
- Zone B: Replica 1
- Zone C: Replica 2
```

**Benefits**
- High availability across AZ failures
- Lower latency for distributed users
- Disaster recovery

## Redis (ApsaraDB for Redis)

### Editions

**Community Edition**
- Open source Redis compatibility
- Standard/Cluster architecture
- Cost-effective

**Enhanced Edition (Tair)**
- Alibaba-optimized
- Additional data structures
- Better performance

### Architecture Types

**Standard (Master-Replica)**
```
Master Node
    ↓
Replica Node
```
- 256MB - 64GB
- Basic high availability
- Use for: Small apps, dev/test

**Cluster (Sharded)**
```
Shard 1: Master + Replica
Shard 2: Master + Replica
Shard 3: Master + Replica
...
Shard 256: Master + Replica (max)
```
- 4GB - 8TB
- Horizontal scaling
- Use for: Large datasets, high throughput

**Read/Write Splitting**
```
Master (writes)
    ↓
Replica 1 (reads) ─┐
Replica 2 (reads) ─┼→ Read Endpoint
Replica 3 (reads) ─┘
```
- Offload read traffic
- Up to 5 read replicas
- Use for: Read-heavy workloads

### Configuration Best Practices

**Memory Management**
```
maxmemory-policy: allkeys-lru
maxmemory: 80% of instance memory

# For cache use case
maxmemory-policy: volatile-lru  # Expire keys with TTL first

# For persistent data
maxmemory-policy: noeviction  # Return error when full
```

**Connection Pooling**
```python
import redis

pool = redis.ConnectionPool(
    host='r-xxxxxx.redis.rds.aliyuncs.com',
    port=6379,
    password='password',
    max_connections=50,
    socket_timeout=5,
    socket_connect_timeout=5
)

r = redis.Redis(connection_pool=pool)
```

**Persistence**
```
# RDB snapshot
save 900 1      # Save if 1 key changed in 900s
save 300 10     # Save if 10 keys changed in 300s
save 60 10000   # Save if 10000 keys changed in 60s

# AOF (append-only file)
appendonly yes
appendfsync everysec  # Balance between durability and performance
```

### Performance Optimization

**Key Design**
```
# Good: Use namespaces
user:1000:profile
user:1000:sessions
order:2000:details

# Bad: No structure
user_1000_profile
u1000s
order2000
```

**Data Structure Selection**
```
Use Case                  Structure       Command
--------------------------------------------------
Counter                   String          INCR, DECR
Cache                     String          SET, GET with TTL
Queue                     List            LPUSH, RPOP
Leaderboard              Sorted Set       ZADD, ZRANGE
Session                   Hash            HSET, HGET
Unique visitors          Set/HyperLogLog  SADD, PFADD
```

**Batch Operations**
```python
# Use pipeline for multiple operations
pipe = r.pipeline()
pipe.set('key1', 'value1')
pipe.set('key2', 'value2')
pipe.set('key3', 'value3')
pipe.execute()

# Use mget for multiple keys
values = r.mget(['key1', 'key2', 'key3'])
```

**Avoid Large Keys**
```
# Problem
HSET large_hash field1 value1  # Repeat millions of times

# Solution: Split into smaller keys
HSET user:1000:data:0 field1 value1
HSET user:1000:data:1 field1001 value1001
```

### Monitoring and Alerts

**Key Metrics**
```
CPU Usage: < 70%
Memory Usage: < 80%
Connections: < max_connections
Hit Rate: > 90%
Network Traffic: Monitor bandwidth
Slow Queries: < 10ms threshold
```

**CloudMonitor Alerts**
```
Alert when:
- Memory usage > 85% for 5 minutes
- CPU usage > 80% for 5 minutes
- Connection count > 80% of max
- Hit rate < 80%
```

## MongoDB (ApsaraDB for MongoDB)

### Deployment Types

**Standalone**
- Single node
- Dev/test only
- Not for production

**Replica Set**
- 1 Primary + 1-5 Secondaries
- Auto-failover
- Production ready

**Sharded Cluster**
- Horizontal scaling
- Multiple shards with replica sets
- Handle large datasets (> 1TB)

### Sharding Strategy

**Choose Shard Key**
```javascript
// Good shard keys (high cardinality, even distribution)
{userId: 1}  // If users are evenly distributed
{tenantId: 1, timestamp: 1}  // Compound key
{customerId: "hashed"}  // Hashed shard key

// Bad shard keys
{status: 1}  // Low cardinality (few unique values)
{timestamp: 1}  // Monotonically increasing (hot shard)
```

**Shard Configuration**
```
Shard 1: 3-node replica set (Zones A, B, C)
Shard 2: 3-node replica set (Zones A, B, C)
Shard 3: 3-node replica set (Zones A, B, C)

Config Servers: 3-node replica set
Mongos Routers: 2+ nodes
```

### Best Practices

**Schema Design**
```javascript
// Embed related data (1:1, 1:few)
{
  _id: ObjectId("..."),
  name: "John Doe",
  address: {
    street: "123 Main St",
    city: "Beijing"
  },
  phones: ["123-456-7890", "098-765-4321"]
}

// Reference for 1:many, many:many
{
  _id: ObjectId("..."),
  userId: ObjectId("user_id"),
  products: [ObjectId("prod1"), ObjectId("prod2")]
}
```

**Indexing**
```javascript
// Create indexes for queries
db.users.createIndex({email: 1}, {unique: true})
db.orders.createIndex({userId: 1, createdAt: -1})

// Compound index for multiple fields
db.products.createIndex({category: 1, price: -1})

// Text index for search
db.articles.createIndex({title: "text", content: "text"})

// Monitor index usage
db.users.aggregate([{$indexStats: {}}])
```

**Read Preference**
```javascript
// Primary (default): All reads from primary
db.collection.find().readPref("primary")

// Secondary: Read from secondaries (eventual consistency)
db.collection.find().readPref("secondary")

// Nearest: Read from nearest node (lowest latency)
db.collection.find().readPref("nearest")
```

**Write Concern**
```javascript
// Acknowledged (default)
db.collection.insertOne({...}, {writeConcern: {w: 1}})

// Majority: Wait for majority of nodes
db.collection.insertOne({...}, {writeConcern: {w: "majority"}})

// Custom: Wait for specific number
db.collection.insertOne({...}, {writeConcern: {w: 3}})
```

### Monitoring

**Key Metrics**
- CPU and memory usage
- Disk IOPS and throughput
- Replication lag
- Connection count
- Query execution time
- Operation counters (insert/update/delete/query)

**Slow Query Analysis**
```javascript
// Enable profiling
db.setProfilingLevel(1, {slowms: 100})  // Log queries > 100ms

// View slow queries
db.system.profile.find().sort({ts: -1}).limit(10)

// Analyze query performance
db.collection.find({...}).explain("executionStats")
```
