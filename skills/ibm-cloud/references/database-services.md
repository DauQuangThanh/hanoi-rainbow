# Database Services

## Table of Contents
- [Db2 on Cloud](#db2-on-cloud)
- [Cloudant (NoSQL)](#cloudant-nosql)
- [Databases for PostgreSQL](#databases-for-postgresql)
- [Databases for MySQL](#databases-for-mysql)
- [Databases for MongoDB](#databases-for-mongodb)
- [Databases for Redis](#databases-for-redis)
- [Databases for Elasticsearch](#databases-for-elasticsearch)
- [Database Migration](#database-migration)

---

## Db2 on Cloud

### Overview

Enterprise-grade relational database with high availability, automatic scaling, and AI-powered optimization.

**Use Cases:**
- Enterprise applications
- Data warehousing
- OLTP workloads
- Analytics and reporting

### Plans

- **Lite**: Free tier for testing
- **Standard**: Production workloads
- **Enterprise**: High availability, disaster recovery

### Provision Db2

**CLI:**
```bash
# Create Db2 instance
ibmcloud resource service-instance-create my-db2 \
  dashdb-for-transactions standard us-south \
  -p '{"members_memory_allocation_mb": "8192",
       "members_disk_allocation_mb": "20480",
       "members_cpu_allocation_count": "3"}'

# Create credentials
ibmcloud resource service-key-create my-db2-creds \
  Manager --instance-name my-db2

# Get connection details
ibmcloud resource service-key my-db2-creds
```

**Terraform:**
```hcl
resource "ibm_database" "db2" {
  name              = "my-db2"
  plan              = "standard"
  location          = "us-south"
  service           = "dashdb-for-transactions"
  resource_group_id = ibm_resource_group.rg.id

  adminpassword = var.db2_admin_password

  group {
    group_id = "member"
    memory {
      allocation_mb = 8192
    }
    disk {
      allocation_mb = 20480
    }
    cpu {
      allocation_count = 3
    }
  }

  backup_id = var.backup_id
}
```

### Connect to Db2

**JDBC Connection:**
```java
import java.sql.*;

public class Db2Connection {
    public static void main(String[] args) {
        String jdbcUrl = "jdbc:db2://<HOST>:<PORT>/<DATABASE>:sslConnection=true;";
        String username = "<USERNAME>";
        String password = "<PASSWORD>";

        try (Connection conn = DriverManager.getConnection(jdbcUrl, username, password)) {
            System.out.println("Connected to Db2");
            
            Statement stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery("SELECT * FROM USERS");
            
            while (rs.next()) {
                System.out.println(rs.getString("name"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}
```

**Python Connection:**
```python
import ibm_db

# Connection string
conn_str = (
    f"DATABASE=<DATABASE>;"
    f"HOSTNAME=<HOST>;"
    f"PORT=<PORT>;"
    f"PROTOCOL=TCPIP;"
    f"UID=<USERNAME>;"
    f"PWD=<PASSWORD>;"
    f"Security=SSL;"
)

# Connect
conn = ibm_db.connect(conn_str, "", "")

# Execute query
stmt = ibm_db.exec_immediate(conn, "SELECT * FROM USERS")
result = ibm_db.fetch_assoc(stmt)

while result:
    print(result['NAME'])
    result = ibm_db.fetch_assoc(stmt)

# Close connection
ibm_db.close(conn)
```

---

## Cloudant (NoSQL)

### Overview

Fully managed NoSQL JSON database based on Apache CouchDB with HTTP API.

**Use Cases:**
- Mobile and web apps
- IoT data storage
- Session management
- Document storage

### Provision Cloudant

**CLI:**
```bash
# Create Cloudant instance
ibmcloud resource service-instance-create my-cloudant \
  cloudantnosqldb standard us-south \
  -p '{"legacyCredentials": false}'

# Create credentials
ibmcloud resource service-key-create my-cloudant-creds \
  Manager --instance-name my-cloudant

# Get connection URL
ibmcloud resource service-key my-cloudant-creds
```

**Terraform:**
```hcl
resource "ibm_cloudant" "cloudant" {
  name              = "my-cloudant"
  plan              = "standard"
  location          = "us-south"
  resource_group_id = ibm_resource_group.rg.id
  
  capacity = 1
  enable_cors = true
  
  cors_config {
    allow_credentials = true
    origins           = ["https://example.com"]
  }
}
```

### CRUD Operations

**Node.js:**
```javascript
const { CloudantV1 } = require('@ibm-cloud/cloudant');
const { IamAuthenticator } = require('ibm-cloud-sdk-core');

const authenticator = new IamAuthenticator({
  apikey: '<API-KEY>',
});

const cloudant = CloudantV1.newInstance({
  authenticator: authenticator,
});
cloudant.setServiceUrl('<SERVICE-URL>');

// Create database
await cloudant.putDatabase({ db: 'mydb' });

// Create document
const document = {
  _id: 'user1',
  name: 'John Doe',
  email: 'john@example.com',
  age: 30
};
await cloudant.postDocument({
  db: 'mydb',
  document: document
});

// Read document
const { result } = await cloudant.getDocument({
  db: 'mydb',
  docId: 'user1'
});
console.log(result);

// Update document
result.age = 31;
await cloudant.postDocument({
  db: 'mydb',
  document: result
});

// Delete document
await cloudant.deleteDocument({
  db: 'mydb',
  docId: 'user1',
  rev: result._rev
});

// Query with selector
const { result: queryResult } = await cloudant.postFind({
  db: 'mydb',
  selector: { age: { '$gt': 25 } },
  fields: ['name', 'email', 'age'],
  sort: [{ age: 'asc' }]
});
console.log(queryResult.docs);
```

**Python:**
```python
from ibmcloudant.cloudant_v1 import CloudantV1
from ibm_cloud_sdk_core.authenticators import IAMAuthenticator

authenticator = IAMAuthenticator('<API-KEY>')
service = CloudantV1(authenticator=authenticator)
service.set_service_url('<SERVICE-URL>')

# Create database
service.put_database(db='mydb').get_result()

# Create document
document = {
    '_id': 'user1',
    'name': 'John Doe',
    'email': 'john@example.com',
    'age': 30
}
service.post_document(db='mydb', document=document).get_result()

# Read document
result = service.get_document(db='mydb', doc_id='user1').get_result()
print(result)

# Query
query_result = service.post_find(
    db='mydb',
    selector={'age': {'$gt': 25}},
    fields=['name', 'email', 'age'],
    sort=[{'age': 'asc'}]
).get_result()
print(query_result['docs'])
```

### Indexes and Views

**Create Index:**
```javascript
// Create index for fast queries
await cloudant.postIndex({
  db: 'mydb',
  index: {
    fields: ['age', 'name']
  },
  name: 'age-name-index',
  type: 'json'
});
```

**MapReduce View:**
```javascript
// Create design document with view
const designDoc = {
  _id: '_design/users',
  views: {
    by_age: {
      map: function(doc) {
        if (doc.age) {
          emit(doc.age, doc.name);
        }
      }.toString(),
      reduce: '_count'
    }
  }
};

await cloudant.postDocument({
  db: 'mydb',
  document: designDoc
});

// Query view
const { result } = await cloudant.postView({
  db: 'mydb',
  ddoc: 'users',
  view: 'by_age',
  group: true
});
console.log(result.rows);
```

---

## Databases for PostgreSQL

### Provision PostgreSQL

**CLI:**
```bash
# Create PostgreSQL instance
ibmcloud resource service-instance-create my-postgres \
  databases-for-postgresql standard us-south \
  -p '{"members_memory_allocation_mb": "4096",
       "members_disk_allocation_mb": "20480",
       "members_cpu_allocation_count": "3",
       "version": "15"}'

# Create credentials
ibmcloud resource service-key-create my-postgres-creds \
  Manager --instance-name my-postgres

# Get connection string
ibmcloud resource service-key my-postgres-creds
```

**Terraform:**
```hcl
resource "ibm_database" "postgresql" {
  name              = "my-postgres"
  plan              = "standard"
  location          = "us-south"
  service           = "databases-for-postgresql"
  resource_group_id = ibm_resource_group.rg.id
  
  adminpassword = var.postgres_password
  version       = "15"

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

  backup_id                     = var.backup_id
  backup_encryption_key_crn     = ibm_kms_key.key.crn
  point_in_time_recovery_deployment_id = var.pitr_id
  point_in_time_recovery_time          = var.pitr_time
}
```

### Connect to PostgreSQL

**Python:**
```python
import psycopg2

# Connection parameters
conn_params = {
    'host': '<HOST>',
    'port': 32541,
    'database': 'ibmclouddb',
    'user': 'admin',
    'password': '<PASSWORD>',
    'sslmode': 'require'
}

# Connect
conn = psycopg2.connect(**conn_params)
cur = conn.cursor()

# Create table
cur.execute('''
    CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        name VARCHAR(100),
        email VARCHAR(100) UNIQUE,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
''')

# Insert data
cur.execute(
    "INSERT INTO users (name, email) VALUES (%s, %s)",
    ('John Doe', 'john@example.com')
)

# Query data
cur.execute("SELECT * FROM users")
for row in cur.fetchall():
    print(row)

# Commit and close
conn.commit()
cur.close()
conn.close()
```

### Scaling and Backups

```bash
# Scale resources
ibmcloud resource service-instance-update my-postgres \
  -p '{"members_memory_allocation_mb": "8192",
       "members_disk_allocation_mb": "40960"}'

# Create on-demand backup
ibmcloud cdb deployment-backups-create <DEPLOYMENT-ID>

# List backups
ibmcloud cdb deployment-backups-list <DEPLOYMENT-ID>

# Restore from backup
ibmcloud resource service-instance-create restored-postgres \
  databases-for-postgresql standard us-south \
  -p '{"backup_id": "<BACKUP-ID>"}'

# Point-in-time recovery
ibmcloud resource service-instance-create pitr-postgres \
  databases-for-postgresql standard us-south \
  -p '{"point_in_time_recovery_deployment_id": "<DEPLOYMENT-ID>",
       "point_in_time_recovery_time": "2024-01-15T12:00:00Z"}'
```

---

## Databases for MySQL

### Provision MySQL

```bash
# Create MySQL instance
ibmcloud resource service-instance-create my-mysql \
  databases-for-mysql standard us-south \
  -p '{"members_memory_allocation_mb": "4096",
       "members_disk_allocation_mb": "20480",
       "members_cpu_allocation_count": "3",
       "version": "8.0"}'
```

### Connect to MySQL

**Python:**
```python
import mysql.connector

# Connect
conn = mysql.connector.connect(
    host='<HOST>',
    port=32542,
    user='admin',
    password='<PASSWORD>',
    database='ibmclouddb',
    ssl_ca='<CA-CERT-PATH>',
    ssl_verify_cert=True
)

cursor = conn.cursor()

# Create table
cursor.execute('''
    CREATE TABLE IF NOT EXISTS products (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(100),
        price DECIMAL(10, 2),
        stock INT
    )
''')

# Insert data
cursor.execute(
    "INSERT INTO products (name, price, stock) VALUES (%s, %s, %s)",
    ('Widget', 19.99, 100)
)

# Query
cursor.execute("SELECT * FROM products")
for row in cursor.fetchall():
    print(row)

conn.commit()
cursor.close()
conn.close()
```

---

## Databases for MongoDB

### Provision MongoDB

```bash
# Create MongoDB instance
ibmcloud resource service-instance-create my-mongodb \
  databases-for-mongodb standard us-south \
  -p '{"members_memory_allocation_mb": "4096",
       "members_disk_allocation_mb": "20480",
       "members_cpu_allocation_count": "3",
       "version": "6.0"}'
```

### Connect to MongoDB

**Python:**
```python
from pymongo import MongoClient

# Connection string
uri = "mongodb://admin:<PASSWORD>@<HOST>:<PORT>/ibmclouddb?authSource=admin&replicaSet=replset&tls=true&tlsCAFile=<CA-CERT-PATH>"

# Connect
client = MongoClient(uri)
db = client['ibmclouddb']
collection = db['users']

# Insert document
user = {
    'name': 'John Doe',
    'email': 'john@example.com',
    'age': 30,
    'tags': ['developer', 'python']
}
result = collection.insert_one(user)
print(f'Inserted ID: {result.inserted_id}')

# Find documents
for doc in collection.find({'age': {'$gt': 25}}):
    print(doc)

# Update document
collection.update_one(
    {'email': 'john@example.com'},
    {'$set': {'age': 31}}
)

# Delete document
collection.delete_one({'email': 'john@example.com'})

client.close()
```

---

## Databases for Redis

### Provision Redis

```bash
# Create Redis instance
ibmcloud resource service-instance-create my-redis \
  databases-for-redis standard us-south \
  -p '{"members_memory_allocation_mb": "2048",
       "members_disk_allocation_mb": "10240",
       "members_cpu_allocation_count": "3",
       "version": "7.0"}'
```

### Connect to Redis

**Python:**
```python
import redis

# Connect
r = redis.Redis(
    host='<HOST>',
    port=32543,
    password='<PASSWORD>',
    ssl=True,
    ssl_ca_certs='<CA-CERT-PATH>',
    decode_responses=True
)

# String operations
r.set('user:1:name', 'John Doe')
print(r.get('user:1:name'))

# Hash operations
r.hset('user:1', mapping={
    'name': 'John Doe',
    'email': 'john@example.com',
    'age': 30
})
print(r.hgetall('user:1'))

# List operations
r.lpush('tasks', 'task1', 'task2', 'task3')
print(r.lrange('tasks', 0, -1))

# Set operations
r.sadd('tags', 'python', 'redis', 'database')
print(r.smembers('tags'))

# Sorted set operations
r.zadd('leaderboard', {'player1': 100, 'player2': 200, 'player3': 150})
print(r.zrange('leaderboard', 0, -1, withscores=True))

# Expiration
r.setex('session:abc123', 3600, 'user_data')

# Pub/Sub
pubsub = r.pubsub()
pubsub.subscribe('channel1')

# Publish
r.publish('channel1', 'Hello, Redis!')
```

---

## Databases for Elasticsearch

### Provision Elasticsearch

```bash
# Create Elasticsearch instance
ibmcloud resource service-instance-create my-elasticsearch \
  databases-for-elasticsearch standard us-south \
  -p '{"members_memory_allocation_mb": "4096",
       "members_disk_allocation_mb": "20480",
       "members_cpu_allocation_count": "3",
       "version": "8.7"}'
```

### Connect to Elasticsearch

**Python:**
```python
from elasticsearch import Elasticsearch

# Connect
es = Elasticsearch(
    ['https://<HOST>:<PORT>'],
    basic_auth=('admin', '<PASSWORD>'),
    ca_certs='<CA-CERT-PATH>',
    verify_certs=True
)

# Index document
doc = {
    'title': 'Introduction to Elasticsearch',
    'author': 'John Doe',
    'content': 'Elasticsearch is a powerful search engine...',
    'tags': ['search', 'elasticsearch', 'database'],
    'published_at': '2024-01-15'
}
es.index(index='articles', id=1, document=doc)

# Search documents
query = {
    'query': {
        'match': {
            'content': 'search engine'
        }
    }
}
results = es.search(index='articles', body=query)
for hit in results['hits']['hits']:
    print(hit['_source'])

# Aggregation
agg_query = {
    'aggs': {
        'tags_count': {
            'terms': {
                'field': 'tags.keyword'
            }
        }
    }
}
agg_results = es.search(index='articles', body=agg_query, size=0)
print(agg_results['aggregations'])
```

---

## Database Migration

### From On-Premises to IBM Cloud

**PostgreSQL Migration:**
```bash
# Dump on-premises database
pg_dump -h localhost -U postgres -d mydb > mydb_dump.sql

# Upload to COS
ibmcloud cos object-put --bucket backups \
  --key postgres/mydb_dump.sql --body mydb_dump.sql

# Download on IBM Cloud instance
ibmcloud cos object-get --bucket backups \
  --key postgres/mydb_dump.sql mydb_dump.sql

# Restore to IBM Cloud PostgreSQL
psql -h <IBM-CLOUD-HOST> -U admin -d ibmclouddb < mydb_dump.sql
```

**MongoDB Migration:**
```bash
# Dump on-premises MongoDB
mongodump --host localhost --db mydb --out /tmp/mongodump

# Upload to COS
ibmcloud cos object-put --bucket backups \
  --key mongodb/mydb.tar.gz \
  --body /tmp/mongodump.tar.gz

# Restore to IBM Cloud MongoDB
mongorestore --host <IBM-CLOUD-HOST> \
  --username admin --password <PASSWORD> \
  --db ibmclouddb /tmp/mongodump/mydb
```

### Database Replication

**PostgreSQL Logical Replication:**
```sql
-- On source database
CREATE PUBLICATION my_pub FOR ALL TABLES;

-- On IBM Cloud PostgreSQL
CREATE SUBSCRIPTION my_sub 
  CONNECTION 'host=<SOURCE-HOST> dbname=mydb user=repl_user password=<PASSWORD>'
  PUBLICATION my_pub;
```

---

## Best Practices

### High Availability

1. **Multi-Zone Deployment**: Deploy across 3 availability zones
2. **Read Replicas**: Create read replicas for scalability
3. **Connection Pooling**: Use PgBouncer for PostgreSQL
4. **Health Monitoring**: Track database metrics
5. **Failover Testing**: Regular DR drills

### Security

1. **Private Endpoints**: Use private service endpoints
2. **Encryption**: Enable encryption at rest and in transit
3. **IAM Authentication**: Use IAM for database access
4. **Network Isolation**: Deploy in VPC with security groups
5. **Audit Logging**: Enable activity tracking

### Performance

1. **Indexing**: Create appropriate indexes
2. **Query Optimization**: Analyze slow queries
3. **Resource Sizing**: Right-size CPU, memory, disk
4. **Caching**: Use Redis for session/data caching
5. **Connection Management**: Limit max connections

### Backup & Recovery

1. **Automated Backups**: Daily automatic backups (30-day retention)
2. **Point-in-Time Recovery**: Enable PITR for critical databases
3. **Test Restores**: Regular backup restoration tests
4. **Cross-Region Backup**: Replicate backups to DR region
5. **Backup Monitoring**: Alert on backup failures
