# Migration Phases


### Phase 1: Assessment and Planning

```markdown
1. Database Inventory
   - Database size (tables, indexes, total GB)
   - Number of objects (tables, views, procedures, functions)
   - Dependencies (foreign keys, triggers)
   - Integration points
   - Peak usage patterns

2. Compatibility Analysis
   - Identify incompatible features
   - Map data types
   - Review stored procedures
   - Check SQL queries
   - Validate application framework

3. Performance Baseline
   - Query response times
   - Transaction throughput
   - Concurrent connections
   - Resource utilization
   - Batch job timings

4. Migration Strategy Selection
   - Downtime tolerance
   - Data volume
   - Budget constraints
   - Technical complexity
   - Risk tolerance

5. Timeline and Resources
   - Duration estimate
   - Team assignment
   - Tool selection
   - Budget allocation
```

### Phase 2: Schema Migration

**Automated Schema Conversion:**

```bash
# Using pgLoader (MySQL to PostgreSQL)
pgloader mysql://user:pass@source-host/dbname \
          postgresql://user:pass@target-host/dbname

# Using AWS Schema Conversion Tool (SCT)
# GUI-based tool for Oracle/SQL Server to PostgreSQL/MySQL

# Using ora2pg (Oracle to PostgreSQL)
ora2pg -c ora2pg.conf -t TABLE -o schema.sql
ora2pg -c ora2pg.conf -t VIEW -o views.sql
ora2pg -c ora2pg.conf -t PROCEDURE -o procedures.sql
```

**Manual Schema Conversion:**

```sql
-- Oracle to PostgreSQL Example

-- Oracle
CREATE TABLE employees (
    emp_id NUMBER(10) PRIMARY KEY,
    emp_name VARCHAR2(100),
    hire_date DATE DEFAULT SYSDATE,
    salary NUMBER(10,2)
);

-- PostgreSQL
CREATE TABLE employees (
    emp_id INTEGER PRIMARY KEY,
    emp_name VARCHAR(100),
    hire_date DATE DEFAULT CURRENT_DATE,
    salary NUMERIC(10,2)
);

-- Oracle Sequence
CREATE SEQUENCE emp_seq START WITH 1;

-- PostgreSQL (using SERIAL or IDENTITY)
CREATE TABLE employees (
    emp_id SERIAL PRIMARY KEY,
    -- or
    emp_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ...
);

-- Oracle PL/SQL Function
CREATE OR REPLACE FUNCTION get_employee_count
RETURN NUMBER IS
    v_count NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_count FROM employees;
    RETURN v_count;
END;

-- PostgreSQL PL/pgSQL Function
CREATE OR REPLACE FUNCTION get_employee_count()
RETURNS INTEGER AS $$
DECLARE
    v_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO v_count FROM employees;
    RETURN v_count;
END;
$$ LANGUAGE plpgsql;
```

**Data Type Mapping:**

```markdown
Oracle → PostgreSQL:
- NUMBER → NUMERIC or INTEGER
- VARCHAR2 → VARCHAR
- DATE → DATE or TIMESTAMP
- CLOB → TEXT
- BLOB → BYTEA
- RAW → BYTEA

MySQL → PostgreSQL:
- INT → INTEGER
- TINYINT → SMALLINT
- BIGINT → BIGINT
- VARCHAR → VARCHAR
- TEXT → TEXT
- DATETIME → TIMESTAMP
- ENUM → Custom TYPE or VARCHAR with CHECK

SQL Server → PostgreSQL:
- INT → INTEGER
- NVARCHAR → VARCHAR
- DATETIME → TIMESTAMP
- BIT → BOOLEAN
- UNIQUEIDENTIFIER → UUID
- IMAGE → BYTEA
```

### Phase 3: Data Migration

**Small Database (< 100 GB):**

```bash
# PostgreSQL dump and restore
pg_dump -h source-host -U user -d dbname -F c -f dump.backup
pg_restore -h target-host -U user -d dbname dump.backup

# MySQL dump and restore
mysqldump -h source-host -u user -p dbname > dump.sql
mysql -h target-host -u user -p dbname < dump.sql

# With compression
mysqldump -h source-host -u user -p dbname | gzip > dump.sql.gz
gunzip < dump.sql.gz | mysql -h target-host -u user -p dbname
```

**Large Database (> 100 GB):**

```bash
# Parallel export/import (PostgreSQL)
pg_dump -h source-host -U user -d dbname -F d -j 8 -f dumpdir/
pg_restore -h target-host -U user -d dbname -j 8 dumpdir/

# Table-by-table migration
for table in $(psql -h source -U user -d db -t -c "SELECT tablename FROM pg_tables WHERE schemaname='public'"); do
    pg_dump -h source -U user -d db -t $table -F c -f ${table}.backup
    pg_restore -h target -U user -d db ${table}.backup
done

# Using COPY for fast data transfer
psql -h source -U user -d db -c "COPY table TO STDOUT" | \
psql -h target -U user -d db -c "COPY table FROM STDIN"
```

**Cross-Engine Migration:**

```bash
# MySQL to PostgreSQL using pgLoader
pgloader mysql://user:pass@mysql-host/dbname \
          postgresql://user:pass@pg-host/dbname

# With custom configuration
cat > migration.load <<EOF
LOAD DATABASE
    FROM mysql://user:pass@mysql-host/dbname
    INTO postgresql://user:pass@pg-host/dbname

WITH include drop, create tables, create indexes,
     reset sequences, workers = 8, concurrency = 1

CAST type datetime to timestamp
     drop default drop not null using zero-dates-to-null,
     
     type date drop not null drop default using zero-dates-to-null

EXCLUDING TABLE NAMES MATCHING 'temp_', 'backup_'

BEFORE LOAD DO
    \$\$ DROP SCHEMA IF EXISTS public CASCADE; \$\$,
    \$\$ CREATE SCHEMA public; \$\$;
EOF

pgloader migration.load
```

**AWS Database Migration Service (DMS):**

```markdown
DMS Migration Types:

1. Full Load
   - Migrate all existing data
   - Database stays online
   - No ongoing replication

2. Full Load + CDC (Change Data Capture)
   - Initial full load
   - Continuous replication
   - Minimal downtime cutover

3. CDC Only
   - Replicate changes only
   - Assumes initial data already migrated
   - For validation or sync

DMS Setup:
1. Create replication instance
2. Create source endpoint
3. Create target endpoint
4. Create migration task
5. Start migration
6. Monitor progress
7. Perform cutover
```

### Phase 4: Application Code Migration

```markdown
Code Changes Required:

1. Connection Strings
   # Oracle
   jdbc:oracle:thin:@host:1521:SID
   
   # PostgreSQL
   jdbc:postgresql://host:5432/database

2. SQL Query Syntax
   -- Oracle
   SELECT * FROM employees WHERE ROWNUM <= 10;
   SELECT NVL(column, 'default') FROM table;
   SELECT TO_DATE('2024-01-01', 'YYYY-MM-DD');
   
   -- PostgreSQL
   SELECT * FROM employees LIMIT 10;
   SELECT COALESCE(column, 'default') FROM table;
   SELECT TO_DATE('2024-01-01', 'YYYY-MM-DD');

3. ORM Framework Updates
   # Hibernate
   # Update hibernate.dialect
   # Oracle
   hibernate.dialect=org.hibernate.dialect.Oracle12cDialect
   
   # PostgreSQL
   hibernate.dialect=org.hibernate.dialect.PostgreSQL10Dialect

4. Stored Procedure Calls
   # Update procedure call syntax
   # May need to rewrite in target database language
```

### Phase 5: Testing and Validation

```markdown
Testing Checklist:

Schema Validation:
- [ ] All tables created
- [ ] Columns match (names, types, constraints)
- [ ] Indexes created
- [ ] Foreign keys established
- [ ] Views functional
- [ ] Stored procedures working
- [ ] Triggers active

Data Validation:
- [ ] Row counts match
  SELECT COUNT(*) FROM table;
  
- [ ] Data integrity checks
  SELECT MD5(string_agg(column, '')) FROM 
    (SELECT column FROM table ORDER BY id) t;
  
- [ ] Sample data comparison
- [ ] Referential integrity maintained
- [ ] No data truncation

Functional Testing:
- [ ] All CRUD operations work
- [ ] Queries return correct results
- [ ] Transactions commit/rollback properly
- [ ] Concurrency handling correct
- [ ] Batch jobs complete successfully

Performance Testing:
- [ ] Query response times acceptable
- [ ] Index usage optimal
- [ ] Connection pooling works
- [ ] Resource utilization normal
- [ ] Load testing passed

Application Testing:
- [ ] Application starts successfully
- [ ] All features functional
- [ ] Reports generate correctly
- [ ] APIs respond properly
- [ ] User acceptance testing passed
```
