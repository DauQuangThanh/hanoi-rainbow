# Migration Types


### 1. Homogeneous Database Migration

**Definition:** Same database engine, different version or platform

**Examples:**
- PostgreSQL 11 → PostgreSQL 15
- MySQL 5.7 → MySQL 8.0
- On-premise PostgreSQL → AWS RDS PostgreSQL
- Self-managed MySQL → Cloud SQL MySQL

**Migration Strategy:**

```markdown
Approach 1: Dump and Restore
Pros: Simple, reliable, clean database
Cons: Downtime required

Steps:
1. Take backup using native tools
2. Stop application writes
3. Final incremental backup
4. Restore to target database
5. Verify data integrity
6. Update application connection
7. Resume operations

Approach 2: Replication
Pros: Minimal downtime, gradual cutover
Cons: More complex setup

Steps:
1. Set up replication (primary → replica)
2. Monitor replication lag
3. When synchronized, plan cutover
4. Stop writes briefly
5. Promote replica to primary
6. Update application connections
7. Resume operations
```

### 2. Heterogeneous Database Migration

**Definition:** Different database engines

**Common Migrations:**

```markdown
Popular Paths:
- Oracle → PostgreSQL
- MySQL → PostgreSQL
- SQL Server → PostgreSQL
- MongoDB → PostgreSQL
- Oracle → MySQL
- SQL Server → MySQL

Reasons:
- Cost reduction (licensing)
- Open source preference
- Cloud-native features
- Better performance
- Vendor lock-in avoidance
```

**Heterogeneous Migration Challenges:**

```markdown
Schema Differences:
- Data types (Oracle NUMBER → PostgreSQL NUMERIC)
- Stored procedures (PL/SQL → PL/pgSQL)
- Triggers and functions
- Sequences and auto-increment
- Index types
- Constraints and defaults

SQL Dialect Differences:
- Syntax variations
- Function names (NVL vs COALESCE)
- Date/time handling
- String concatenation (|| vs CONCAT)
- LIMIT vs ROWNUM vs TOP

Feature Gaps:
- Packages (Oracle) → Schemas (PostgreSQL)
- Synonyms → Views
- Materialized views
- Partitioning approaches
- Full-text search
```
