# Cloud-Specific Migrations


### AWS RDS Migration

```bash
# On-Premise PostgreSQL → AWS RDS PostgreSQL

# Option 1: pg_dump/pg_restore
pg_dump -h source-host -U user -d dbname -F c -f dump.backup
pg_restore -h rds-endpoint -U user -d dbname dump.backup

# Option 2: AWS DMS
# Use AWS Console or CLI to create migration task

# Option 3: RDS Snapshot (for RDS to RDS)
aws rds create-db-snapshot \
  --db-instance-identifier source-db \
  --db-snapshot-identifier migration-snapshot

aws rds restore-db-instance-from-db-snapshot \
  --db-instance-identifier target-db \
  --db-snapshot-identifier migration-snapshot
```

### GCP Cloud SQL Migration

```bash
# MySQL → Cloud SQL MySQL

# Using Database Migration Service (DMS)
gcloud sql connect-db-instances create CONNECTION_PROFILE \
  --source=mysql \
  --host=SOURCE_IP \
  --port=3306 \
  --username=USER

# Or using mysqldump
mysqldump -h source-host -u user -p --databases dbname \
  --single-transaction --set-gtid-purged=OFF > dump.sql

# Import to Cloud SQL
gcloud sql import sql INSTANCE_NAME gs://BUCKET/dump.sql \
  --database=DATABASE_NAME
```

### Azure Database Migration

```bash
# SQL Server → Azure SQL Database

# Using Azure Database Migration Service
# Or using BACPAC export/import

# Export
SqlPackage.exe /Action:Export \
  /SourceServerName:source-server \
  /SourceDatabaseName:mydb \
  /TargetFile:mydb.bacpac

# Upload to Azure Blob Storage
az storage blob upload \
  --account-name mystorageaccount \
  --container-name backups \
  --file mydb.bacpac

# Import to Azure SQL
az sql db import \
  --resource-group mygroup \
  --server target-server \
  --name mydb \
  --storage-key-type StorageAccessKey \
  --storage-key STORAGE_KEY \
  --storage-uri https://mystorageaccount.blob.core.windows.net/backups/mydb.bacpac
```
