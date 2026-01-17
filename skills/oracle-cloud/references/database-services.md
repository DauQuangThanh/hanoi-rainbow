# Database Services

## Autonomous Database

### Autonomous Transaction Processing (ATP)

**Use Cases**: OLTP workloads, web applications, SaaS applications

**Provisioning**:
```hcl
resource "oci_database_autonomous_database" "atp" {
  compartment_id           = var.compartment_id
  db_name                  = "atpdb"
  display_name             = "ATP Database"
  admin_password           = var.admin_password
  cpu_core_count           = 1
  data_storage_size_in_tbs = 1
  db_version               = "19c"
  db_workload              = "OLTP"
  is_auto_scaling_enabled  = true
  is_free_tier             = false
  license_model            = "LICENSE_INCLUDED"
  
  subnet_id                = var.private_subnet_id
  nsg_ids                  = [oci_core_network_security_group.db_nsg.id]
  
  whitelisted_ips = ["10.0.0.0/16"]
  
  freeform_tags = {
    Environment = "Production"
  }
}
```

### Autonomous Data Warehouse (ADW)

**Use Cases**: Analytics, data warehousing, reporting

```hcl
resource "oci_database_autonomous_database" "adw" {
  compartment_id           = var.compartment_id
  db_name                  = "adwdb"
  display_name             = "ADW Database"
  admin_password           = var.admin_password
  cpu_core_count           = 2
  data_storage_size_in_tbs = 2
  db_version               = "19c"
  db_workload              = "DW"
  is_auto_scaling_enabled  = true
  license_model            = "LICENSE_INCLUDED"
}
```

### Autonomous Database Connection

**Wallet Download**:
```bash
# Download wallet
oci db autonomous-database generate-wallet \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --file wallet.zip \
  --password MyWalletPassword123
```

**Connection String**:
```python
import cx_Oracle

# Using wallet
connection = cx_Oracle.connect(
    user="admin",
    password="MyPassword123",
    dsn="atpdb_high",
    config_dir="/path/to/wallet",
    wallet_location="/path/to/wallet",
    wallet_password="MyWalletPassword123"
)
```

### Auto Scaling Configuration

**CPU Auto Scaling**:
- Automatically scales up to 3x the base OCPU count
- Scales based on CPU utilization
- No additional configuration needed when `is_auto_scaling_enabled = true`

**Storage Auto Scaling**:
```hcl
resource "oci_database_autonomous_database" "atp_auto_scale" {
  # ... other configuration ...
  
  is_auto_scaling_enabled           = true
  is_auto_scaling_for_storage_enabled = true
}
```

## MySQL Database Service

### MySQL DB System

```hcl
resource "oci_mysql_mysql_db_system" "main_mysql" {
  compartment_id      = var.compartment_id
  shape_name          = "MySQL.VM.Standard.E4.1.8GB"
  subnet_id           = var.private_subnet_id
  admin_password      = var.admin_password
  admin_username      = "admin"
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  display_name        = "main-mysql"
  
  data_storage_size_in_gb = 50
  
  configuration_id = data.oci_mysql_mysql_configurations.mysql_configs.configurations[0].id
  
  backup_policy {
    is_enabled        = true
    retention_in_days = 7
    window_start_time = "02:00"
  }
  
  maintenance {
    window_start_time = "SUNDAY 02:00"
  }
  
  is_highly_available = true
  
  freeform_tags = {
    Environment = "Production"
  }
}
```

### MySQL High Availability

```hcl
resource "oci_mysql_mysql_db_system" "ha_mysql" {
  # ... base configuration ...
  
  is_highly_available = true
  
  # HA configuration creates:
  # - Primary instance
  # - Secondary instance in different AD
  # - Automatic failover
}
```

### MySQL Backup and Recovery

**Manual Backup**:
```bash
# Create manual backup
oci mysql backup create \
  --db-system-id ocid1.mysqldbsystem... \
  --display-name "manual-backup-$(date +%Y%m%d)"

# Restore from backup
oci mysql db-system create-from-backup \
  --backup-id ocid1.mysqlbackup... \
  --compartment-id ocid1.compartment... \
  --shape-name "MySQL.VM.Standard.E4.1.8GB" \
  --subnet-id ocid1.subnet...
```

### MySQL Connection

```python
import mysql.connector

config = {
    'host': '10.0.2.10',
    'port': 3306,
    'user': 'admin',
    'password': 'MyPassword123',
    'database': 'myapp',
    'ssl_ca': '/path/to/ca-cert.pem',
    'ssl_verify_cert': True
}

connection = mysql.connector.connect(**config)
cursor = connection.cursor()
```

## PostgreSQL Database Service

### PostgreSQL DB System

```hcl
resource "oci_psql_db_system" "main_postgres" {
  compartment_id = var.compartment_id
  display_name   = "main-postgres"
  
  db_version = "14"
  
  shape = "PostgreSQL.VM.Standard.E4.Flex.2.32GB"
  
  instance_count       = 1
  instance_ocpu_count  = 2
  instance_memory_size_in_gbs = 32
  
  storage_details {
    is_regionally_durable = true
    system_type          = "OCI_OPTIMIZED_STORAGE"
  }
  
  network_details {
    subnet_id = var.private_subnet_id
    nsg_ids   = [oci_core_network_security_group.db_nsg.id]
  }
  
  credentials {
    username = "postgres"
    password_details {
      password_type = "PLAIN_TEXT"
      password      = var.postgres_password
    }
  }
}
```

### PostgreSQL Connection

```python
import psycopg2

connection = psycopg2.connect(
    host="10.0.2.20",
    port=5432,
    database="myapp",
    user="postgres",
    password="MyPassword123",
    sslmode="require"
)
```

## NoSQL Database Service

### NoSQL Table

```hcl
resource "oci_nosql_table" "user_table" {
  compartment_id = var.compartment_id
  name           = "users"
  
  ddl_statement = <<-EOT
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER,
      email STRING,
      name STRING,
      created_at TIMESTAMP(3),
      PRIMARY KEY (id)
    )
  EOT
  
  table_limits {
    max_read_units     = 50
    max_write_units    = 50
    max_storage_in_gbs = 25
  }
}

resource "oci_nosql_index" "email_index" {
  table_name_or_id = oci_nosql_table.user_table.id
  name             = "emailIndex"
  
  keys {
    column_name = "email"
  }
}
```

### NoSQL Operations

**Python SDK Example**:
```python
from oci import nosql
from oci.config import from_file

config = from_file()
nosql_client = nosql.NosqlClient(config)

# Put row
put_request = nosql.models.UpdateRowDetails(
    value={
        "id": 1,
        "email": "user@example.com",
        "name": "John Doe",
        "created_at": "2024-01-15T10:00:00.000Z"
    },
    compartment_id=compartment_id
)

nosql_client.update_row(
    table_name_or_id="users",
    update_row_details=put_request
)

# Get row
get_request = nosql.models.GetRowDetails(
    key={"id": 1}
)

response = nosql_client.get_row(
    table_name_or_id="users",
    key=["id:1"]
)

# Query
query_request = nosql.models.QueryDetails(
    statement="SELECT * FROM users WHERE email = 'user@example.com'",
    compartment_id=compartment_id
)

query_response = nosql_client.query(query_request)
```

## Database Backup Strategies

### Autonomous Database Backups

**Automatic Backups**:
- Retained for 60 days by default
- Daily incremental backups
- Weekly full backups
- Point-in-time recovery available

**Manual Backups**:
```bash
# Create manual backup
oci db autonomous-database create-backup \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --display-name "pre-upgrade-backup"

# Restore from backup
oci db autonomous-database restore \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --timestamp "2024-01-15T10:00:00.000Z"
```

### MySQL Backup Policy

```hcl
resource "oci_mysql_mysql_db_system" "mysql_with_backup" {
  # ... base configuration ...
  
  backup_policy {
    is_enabled        = true
    retention_in_days = 30
    window_start_time = "02:00"
    
    # Point-in-time recovery
    pitr_policy {
      is_enabled = true
    }
  }
}
```

## Database Migration

### MySQL Migration Using Data Pump

**Export from Source**:
```bash
mysqldump -h source-host \
  -u admin -p \
  --single-transaction \
  --routines \
  --triggers \
  --databases myapp > myapp_dump.sql
```

**Import to OCI MySQL**:
```bash
mysql -h mysql-oci-host \
  -u admin -p \
  --ssl-ca=/path/to/ca-cert.pem < myapp_dump.sql
```

### PostgreSQL Migration

**Using pg_dump/pg_restore**:
```bash
# Export
pg_dump -h source-host \
  -U postgres \
  -d myapp \
  -F c \
  -f myapp_dump.dump

# Restore
pg_restore -h postgres-oci-host \
  -U postgres \
  -d myapp \
  myapp_dump.dump
```

## Connection Pooling

### PgBouncer for PostgreSQL

```ini
[databases]
myapp = host=postgres-host port=5432 dbname=myapp

[pgbouncer]
listen_addr = *
listen_port = 6432
auth_type = md5
auth_file = /etc/pgbouncer/userlist.txt
pool_mode = transaction
max_client_conn = 1000
default_pool_size = 25
```

### MySQL Connection Pool (Python)

```python
from mysql.connector import pooling

pool = pooling.MySQLConnectionPool(
    pool_name="myapp_pool",
    pool_size=10,
    pool_reset_session=True,
    host='mysql-host',
    database='myapp',
    user='admin',
    password='password'
)

connection = pool.get_connection()
```

## Performance Optimization

### Autonomous Database Performance

**Enable Auto Scaling**:
```hcl
resource "oci_database_autonomous_database" "optimized_atp" {
  # ... base configuration ...
  
  is_auto_scaling_enabled = true
  cpu_core_count          = 1  # Can scale up to 3 OCPUs
}
```

**Query Performance**:
- Use bind variables to enable plan caching
- Create appropriate indexes
- Monitor Performance Hub for slow queries
- Use result cache for frequently accessed data

### MySQL Performance Tuning

**Key Parameters**:
```sql
-- Connection settings
SET GLOBAL max_connections = 500;

-- Buffer pool size (set to 70-80% of available memory)
SET GLOBAL innodb_buffer_pool_size = 25769803776;  -- 24GB

-- Query cache (for read-heavy workloads)
SET GLOBAL query_cache_size = 268435456;  -- 256MB
SET GLOBAL query_cache_type = 1;

-- Slow query log
SET GLOBAL slow_query_log = 1;
SET GLOBAL long_query_time = 2;
```

### NoSQL Performance

**Optimize Table Limits**:
```hcl
resource "oci_nosql_table" "high_performance" {
  # ... base configuration ...
  
  table_limits {
    max_read_units     = 500   # Increase for read-heavy workloads
    max_write_units    = 200   # Increase for write-heavy workloads
    max_storage_in_gbs = 100
  }
}
```

## Monitoring and Alerts

### Database Metrics

**Key Metrics to Monitor**:
- CPU Utilization (target < 80%)
- Storage Used (alert at 80%)
- Connection Count (monitor for connection pool exhaustion)
- Query Response Time (track slow queries)
- Replication Lag (for HA configurations)

**OCI Monitoring Query**:
```bash
# Get CPU utilization for Autonomous Database
oci monitoring metric-data summarize-metrics-data \
  --compartment-id ocid1.compartment... \
  --namespace oci_autonomous_database \
  --query-text "CpuUtilization[1m].mean()" \
  --start-time 2024-01-15T00:00:00.000Z \
  --end-time 2024-01-15T23:59:59.000Z
```

### Alarm Configuration

```hcl
resource "oci_monitoring_alarm" "db_cpu_alarm" {
  compartment_id        = var.compartment_id
  display_name          = "db-high-cpu"
  is_enabled            = true
  metric_compartment_id = var.compartment_id
  namespace             = "oci_autonomous_database"
  query                 = "CpuUtilization[1m].mean() > 80"
  severity              = "CRITICAL"
  
  destinations = [oci_ons_notification_topic.alerts.id]
  
  repeat_notification_duration = "PT2H"
}
```

## Security Best Practices

1. **Encryption**: Enable TDE (Transparent Data Encryption) for all databases
2. **Network Access**: Use private subnets and NSGs
3. **Authentication**: Use strong passwords and rotate regularly
4. **Audit Logging**: Enable database audit logs
5. **Backup Encryption**: Ensure backups are encrypted
6. **Least Privilege**: Grant minimum required permissions
7. **Connection Security**: Always use SSL/TLS for connections
