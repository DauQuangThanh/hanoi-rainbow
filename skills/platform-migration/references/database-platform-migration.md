# Database Platform Migration


```markdown
RDS PostgreSQL → Cloud SQL PostgreSQL

Phase 1: Preparation
- Assess database size and load
- Plan maintenance window
- Set up replication if possible
- Test restore procedures

Phase 2: Setup Target
- Create Cloud SQL instance
- Configure network access
- Set parameters matching source
- Create users and permissions

Phase 3: Data Migration
Option A: Dump and Restore
  # Export from RDS
  pg_dump -h rds-host -U postgres -d mydb > dump.sql
  
  # Import to Cloud SQL
  psql -h cloudsql-ip -U postgres -d mydb < dump.sql

Option B: Streaming Replication
  # Set up logical replication
  # Continuous sync during migration
  # Switch over when ready

Phase 4: Validation
- Compare row counts
- Validate data integrity
- Test application queries
- Performance benchmarking

Phase 5: Cutover
- Stop writes to source
- Final sync
- Update connection strings
- Monitor application
```
