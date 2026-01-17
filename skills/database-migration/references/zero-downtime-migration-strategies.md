# Zero-Downtime Migration Strategies


### Logical Replication

```sql
-- PostgreSQL Logical Replication Setup

-- On source (publisher)
ALTER SYSTEM SET wal_level = logical;
-- Restart PostgreSQL

CREATE PUBLICATION my_publication FOR ALL TABLES;

-- On target (subscriber)
CREATE SUBSCRIPTION my_subscription
    CONNECTION 'host=source-host port=5432 dbname=mydb user=repuser password=pass'
    PUBLICATION my_publication;

-- Monitor replication
SELECT * FROM pg_stat_subscription;

-- Cutover
-- 1. Stop application writes
-- 2. Wait for replication to catch up
SELECT pg_current_wal_lsn();  -- On source
SELECT latest_end_lsn FROM pg_stat_subscription;  -- On target
-- When they match, replication is current

-- 3. Drop subscription
DROP SUBSCRIPTION my_subscription;

-- 4. Update application connection to target
-- 5. Resume operations
```

### Dual-Write Strategy

```markdown
Approach:
1. Write to both old and new databases
2. Read from old database initially
3. Validate data consistency
4. Switch reads to new database
5. Stop writing to old database

Pros:
- Very low downtime
- Easy rollback

Cons:
- Application complexity
- Requires code changes
- Potential data inconsistency

Implementation:
- Use application middleware
- Queue-based async writes
- Monitoring and reconciliation
```
