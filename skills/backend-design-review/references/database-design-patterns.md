# Database Design Patterns Reference

This document provides comprehensive guidance on database design patterns, normalization strategies, indexing techniques, and scalability approaches for relational and NoSQL databases.

## Table of Contents

1. [Data Modeling Fundamentals](#data-modeling-fundamentals)
2. [Normalization & Denormalization](#normalization--denormalization)
3. [Indexing Strategies](#indexing-strategies)
4. [Query Optimization](#query-optimization)
5. [Database Scaling Patterns](#database-scaling-patterns)
6. [NoSQL Database Patterns](#nosql-database-patterns)
7. [Caching Strategies](#caching-strategies)
8. [Database Security](#database-security)

---

## Data Modeling Fundamentals

### Entity-Relationship Modeling

**Entities**: Objects or concepts (User, Order, Product)
**Attributes**: Properties of entities (name, email, price)
**Relationships**: Connections between entities (one-to-many, many-to-many)

**Relationship Types:**

**One-to-One (1:1):**
```
User ← one-to-one → Profile
(One user has one profile)

users table:
id | email | name

profiles table:
id | user_id (FK, UNIQUE) | bio | avatar_url
```

**One-to-Many (1:N):**
```
User ← one-to-many → Orders
(One user has many orders)

users table:
id | email | name

orders table:
id | user_id (FK) | total | created_at
```

**Many-to-Many (M:N):**
```
Students ← many-to-many → Courses
(Students enroll in many courses, courses have many students)

students table:
id | name | email

courses table:
id | title | credits

enrollments table (junction/bridge table):
id | student_id (FK) | course_id (FK) | enrolled_at
UNIQUE(student_id, course_id)
```

### Primary Keys

**Natural Keys** (business data):
- Example: email, SSN, ISBN
- Pros: Meaningful, no additional column
- Cons: May change, may be composite, privacy concerns

**Surrogate Keys** (artificial):
- Example: auto-increment INT, UUID
- Pros: Never changes, simple, no business logic
- Cons: Additional column, less meaningful

**Recommendation:** Use surrogate keys (INT BIGINT or UUID) as primary keys

**Auto-Increment INT vs. UUID:**

| Aspect | Auto-Increment INT | UUID |
|--------|-------------------|------|
| Size | 4-8 bytes | 16 bytes |
| Readability | Easy (1, 2, 3) | Hard (123e4567-e89b...) |
| Distribution | Sequential | Random |
| Guessability | Easy (predictable) | Hard (random) |
| Multi-master | Issues (conflicts) | Works (globally unique) |
| Index Performance | Excellent (sequential) | Moderate (random inserts) |

**Best Practices:**
- Use BIGINT (8 bytes) for auto-increment to avoid overflow
- Use UUID v4 for distributed systems
- Use UUID v7 (time-ordered) for better index performance

### Foreign Keys

**Purpose:** Maintain referential integrity between tables

**Syntax:**
```sql
CREATE TABLE orders (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    user_id BIGINT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);
```

**Cascade Options:**

| Option | Behavior |
|--------|----------|
| CASCADE | Delete/update related rows |
| SET NULL | Set foreign key to NULL |
| SET DEFAULT | Set foreign key to default value |
| RESTRICT | Prevent delete/update (default) |
| NO ACTION | Same as RESTRICT |

**Example:**
```sql
-- When user deleted, delete all their orders
FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE

-- When user deleted, keep orders but set user_id to NULL
FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET NULL

-- Prevent deleting user if they have orders
FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE RESTRICT
```

**Considerations:**
- Foreign keys enforce integrity but add overhead
- Indexes automatically created on foreign keys (some databases)
- May need to manually index foreign keys for join performance

---

## Normalization & Denormalization

### Normalization Forms

**1NF (First Normal Form):**
- Atomic values (no repeating groups or arrays in columns)
- Each column contains only one value

❌ **Violates 1NF:**
```sql
CREATE TABLE orders (
    id INT,
    customer_name VARCHAR(100),
    items VARCHAR(500)  -- "Item1, Item2, Item3"
);
```

✅ **Conforms to 1NF:**
```sql
CREATE TABLE orders (
    id INT,
    customer_name VARCHAR(100)
);

CREATE TABLE order_items (
    id INT,
    order_id INT,
    item_name VARCHAR(100)
);
```

**2NF (Second Normal Form):**
- Meets 1NF
- No partial dependencies (non-key columns depend on entire primary key)

❌ **Violates 2NF:**
```sql
CREATE TABLE order_items (
    order_id INT,
    item_id INT,
    item_name VARCHAR(100),  -- Depends only on item_id, not (order_id, item_id)
    quantity INT,
    PRIMARY KEY (order_id, item_id)
);
```

✅ **Conforms to 2NF:**
```sql
CREATE TABLE items (
    id INT PRIMARY KEY,
    name VARCHAR(100)
);

CREATE TABLE order_items (
    order_id INT,
    item_id INT,
    quantity INT,
    PRIMARY KEY (order_id, item_id),
    FOREIGN KEY (item_id) REFERENCES items(id)
);
```

**3NF (Third Normal Form):**
- Meets 2NF
- No transitive dependencies (non-key columns don't depend on other non-key columns)

❌ **Violates 3NF:**
```sql
CREATE TABLE orders (
    id INT PRIMARY KEY,
    customer_id INT,
    customer_name VARCHAR(100),    -- Depends on customer_id, not order id
    customer_email VARCHAR(100),   -- Depends on customer_id, not order id
    total DECIMAL(10, 2)
);
```

✅ **Conforms to 3NF:**
```sql
CREATE TABLE customers (
    id INT PRIMARY KEY,
    name VARCHAR(100),
    email VARCHAR(100)
);

CREATE TABLE orders (
    id INT PRIMARY KEY,
    customer_id INT,
    total DECIMAL(10, 2),
    FOREIGN KEY (customer_id) REFERENCES customers(id)
);
```

**BCNF (Boyce-Codd Normal Form):**
- Stricter version of 3NF
- Every determinant is a candidate key

**4NF, 5NF:** Rarely needed in practice

### Normalization Benefits

✅ **Pros:**
- Eliminates data redundancy
- Maintains data integrity
- Easier updates (update once, not in multiple places)
- Smaller storage requirements

❌ **Cons:**
- More tables and joins
- Complex queries
- Potentially slower reads

### Denormalization

**Purpose:** Improve read performance by adding redundancy

**When to Denormalize:**
- Read-heavy workloads (10:1 reads to writes)
- Expensive join queries affecting performance
- Frequently accessed computed values
- Reporting and analytics use cases

**Denormalization Patterns:**

**1. Duplicate Data:**
```sql
-- Normalized
customers: id, name, email
orders: id, customer_id, total

-- Denormalized (duplicate customer_name)
orders: id, customer_id, customer_name, total

-- Benefit: No join needed to display order with customer name
-- Trade-off: Must update customer_name in orders when customer name changes
```

**2. Precompute Aggregates:**
```sql
-- Normalized
users: id, name
posts: id, user_id, title

-- Denormalized (add post_count)
users: id, name, post_count

-- Benefit: Get post count without COUNT query
-- Trade-off: Must update post_count when posts added/removed
```

**3. Collapse One-to-One:**
```sql
-- Normalized
users: id, email, name
profiles: id, user_id, bio, avatar

-- Denormalized (merge into one table)
users: id, email, name, bio, avatar

-- Benefit: No join needed
-- Trade-off: Larger table, NULL values if profile incomplete
```

**Maintaining Denormalized Data:**

1. **Application Logic:** Update denormalized data in application code
2. **Database Triggers:** Automatically update on INSERT/UPDATE/DELETE
3. **Scheduled Jobs:** Periodically recalculate denormalized data
4. **Event Sourcing:** Rebuild denormalized views from event log

**Example Trigger:**
```sql
CREATE TRIGGER update_post_count AFTER INSERT ON posts
FOR EACH ROW
BEGIN
    UPDATE users
    SET post_count = post_count + 1
    WHERE id = NEW.user_id;
END;
```

---

## Indexing Strategies

### Index Types

**1. Primary Key Index:**
- Automatically created on PRIMARY KEY
- Clustered index (data stored in index order) in most databases
- Unique and non-null

**2. Unique Index:**
- Enforces uniqueness
- Automatically created on UNIQUE constraint

```sql
CREATE UNIQUE INDEX idx_users_email ON users(email);
```

**3. Non-Unique Index (Secondary Index):**
- Improves query performance
- Allows duplicates

```sql
CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_orders_status ON orders(status);
```

**4. Composite Index (Multi-Column):**
- Index on multiple columns
- Column order matters (left-to-right prefix rule)

```sql
CREATE INDEX idx_orders_user_status ON orders(user_id, status, created_at);

-- Works with this index:
SELECT * FROM orders WHERE user_id = 123;
SELECT * FROM orders WHERE user_id = 123 AND status = 'pending';
SELECT * FROM orders WHERE user_id = 123 AND status = 'pending' AND created_at > '2026-01-01';

-- Does NOT work with this index:
SELECT * FROM orders WHERE status = 'pending';  -- Missing user_id (leftmost column)
SELECT * FROM orders WHERE created_at > '2026-01-01';  -- Missing user_id and status
```

**Column Order in Composite Indexes:**
1. Equality conditions first (WHERE column = value)
2. Range conditions last (WHERE column > value)
3. Most selective columns first (filters most rows)

**5. Covering Index (Include Columns):**
- Index contains all columns needed by query (no table lookup)

```sql
-- Query:
SELECT id, user_id, status, total FROM orders WHERE user_id = 123 AND status = 'pending';

-- Covering index:
CREATE INDEX idx_orders_covering ON orders(user_id, status) INCLUDE (total);
-- or
CREATE INDEX idx_orders_covering ON orders(user_id, status, total);
```

**6. Partial Index (Filtered Index):**
- Index only subset of rows

```sql
-- Index only active users
CREATE INDEX idx_users_active ON users(email) WHERE status = 'active';

-- Smaller index, faster for queries filtering on active users
SELECT * FROM users WHERE email = 'john@example.com' AND status = 'active';
```

**7. Full-Text Index:**
- Optimize text search

```sql
CREATE FULLTEXT INDEX idx_posts_content ON posts(title, content);

SELECT * FROM posts WHERE MATCH(title, content) AGAINST('database design');
```

**8. Spatial Index:**
- Optimize geographic queries

```sql
CREATE SPATIAL INDEX idx_locations_coords ON locations(coordinates);

SELECT * FROM locations
WHERE ST_Distance_Sphere(coordinates, POINT(lat, lng)) < 10000;
```

### When to Add Indexes

✅ **Add Index When:**
- Column used in WHERE clause frequently
- Column used in JOIN conditions
- Column used in ORDER BY frequently
- Column used in GROUP BY
- Foreign key columns (for join performance)
- Queries slow without index (check EXPLAIN plan)

❌ **Don't Index When:**
- Table very small (< 1000 rows)
- Column has low cardinality (few distinct values, like boolean)
- Column updated frequently (index maintenance overhead)
- Table has heavy write workload (inserts/updates/deletes slow down)

### Index Maintenance Overhead

**Write Operations:**
- INSERT: Update all indexes on table
- UPDATE: Update indexes if indexed columns change
- DELETE: Update all indexes on table

**Trade-off:** Indexes speed up reads but slow down writes

**Guideline:** Limit indexes to 5-7 per table (adjust based on workload)

### Analyzing Index Usage

**MySQL:**
```sql
-- Check index usage
SHOW INDEX FROM orders;

-- Explain query execution plan
EXPLAIN SELECT * FROM orders WHERE user_id = 123;

-- Check index statistics
SELECT * FROM information_schema.INNODB_SYS_TABLESTATS WHERE name = 'mydb/orders';
```

**PostgreSQL:**
```sql
-- Explain query
EXPLAIN ANALYZE SELECT * FROM orders WHERE user_id = 123;

-- Check index usage
SELECT * FROM pg_stat_user_indexes WHERE relname = 'orders';

-- Find unused indexes
SELECT schemaname, tablename, indexname
FROM pg_stat_user_indexes
WHERE idx_scan = 0 AND schemaname NOT IN ('pg_catalog', 'information_schema');
```

---

## Query Optimization

### N+1 Query Problem

**Problem:** 1 query to get parent records, then N queries to get related records

❌ **Bad (N+1 queries):**
```python
# 1 query
users = db.query("SELECT * FROM users LIMIT 10")

# 10 queries (N queries)
for user in users:
    orders = db.query(f"SELECT * FROM orders WHERE user_id = {user.id}")
```

✅ **Solution 1: JOIN (1 query):**
```sql
SELECT u.*, o.*
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
WHERE u.id IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
```

✅ **Solution 2: IN clause (2 queries):**
```python
# 1 query
users = db.query("SELECT * FROM users LIMIT 10")
user_ids = [user.id for user in users]

# 1 query (not N queries)
orders = db.query(f"SELECT * FROM orders WHERE user_id IN ({','.join(map(str, user_ids))})")

# Group orders by user_id in application
```

✅ **Solution 3: ORM Eager Loading:**
```python
# Django ORM
users = User.objects.prefetch_related('orders').all()[:10]

# SQLAlchemy
users = session.query(User).options(joinedload(User.orders)).limit(10).all()
```

### SELECT * Considered Harmful

❌ **Bad:**
```sql
SELECT * FROM users WHERE id = 123;
-- Returns all 20 columns, even if only need 3
```

✅ **Good:**
```sql
SELECT id, first_name, last_name, email FROM users WHERE id = 123;
-- Returns only needed columns
```

**Benefits:**
- Reduces data transfer (network bandwidth)
- Improves query performance
- Covering indexes more effective
- Avoids issues when schema changes

### Query Execution Plans (EXPLAIN)

**MySQL:**
```sql
EXPLAIN SELECT * FROM orders WHERE user_id = 123;

+----+-------------+--------+------+---------------+------+---------+------+------+-------------+
| id | select_type | table  | type | possible_keys | key  | key_len | ref  | rows | Extra       |
+----+-------------+--------+------+---------------+------+---------+------+------+-------------+
|  1 | SIMPLE      | orders | ref  | idx_user_id   | idx  | 8       | const|  50  | Using where |
+----+-------------+--------+------+---------------+------+---------+------+------+-------------+
```

**Key Columns:**
- **type**: Access method (const, eq_ref, ref, range, index, ALL)
  - `const`: Best (constant lookup)
  - `eq_ref`: Unique index lookup
  - `ref`: Non-unique index lookup
  - `range`: Index range scan (WHERE id > 100)
  - `index`: Full index scan
  - `ALL`: Full table scan (worst, avoid)
- **key**: Index used (NULL if no index)
- **rows**: Estimated rows scanned
- **Extra**: Additional info (Using filesort, Using temporary - both bad)

**PostgreSQL:**
```sql
EXPLAIN ANALYZE SELECT * FROM orders WHERE user_id = 123;

Index Scan using idx_orders_user_id on orders  (cost=0.42..8.44 rows=1 width=50) (actual time=0.015..0.016 rows=1 loops=1)
  Index Cond: (user_id = 123)
Planning Time: 0.080 ms
Execution Time: 0.030 ms
```

**Red Flags:**
- Seq Scan (sequential scan) on large table
- Full table scan (type = ALL)
- Using filesort (sorting without index)
- Using temporary (temp table created)
- High cost values

### Query Optimization Techniques

**1. Use Indexes:**
- Add indexes on frequently queried columns
- Check EXPLAIN plan to verify index usage

**2. Optimize JOIN:**
- Join on indexed columns (foreign keys)
- Filter before joining (WHERE clauses early)
- Use INNER JOIN when possible (faster than LEFT JOIN)

**3. Avoid Functions on Indexed Columns:**

❌ **Bad (index not used):**
```sql
SELECT * FROM users WHERE LOWER(email) = 'john@example.com';
SELECT * FROM orders WHERE YEAR(created_at) = 2026;
```

✅ **Good (index used):**
```sql
SELECT * FROM users WHERE email = 'john@example.com';  -- Case-insensitive collation
SELECT * FROM orders WHERE created_at >= '2026-01-01' AND created_at < '2027-01-01';
```

**4. Limit Results:**
- Always use LIMIT for pagination
- Don't fetch more rows than needed

**5. Avoid Subqueries (sometimes):**

❌ **Slow (correlated subquery):**
```sql
SELECT u.id, u.name,
    (SELECT COUNT(*) FROM orders o WHERE o.user_id = u.id) AS order_count
FROM users u;
-- Executes subquery for each user (N+1 problem)
```

✅ **Fast (JOIN with GROUP BY):**
```sql
SELECT u.id, u.name, COUNT(o.id) AS order_count
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
GROUP BY u.id, u.name;
```

**6. Use UNION ALL instead of UNION:**
- UNION removes duplicates (expensive)
- UNION ALL keeps duplicates (faster)

**7. Batch Operations:**
- Bulk INSERT instead of individual INSERTs
- Update multiple rows in one query

```sql
-- Instead of 1000 INSERTs
INSERT INTO users (name, email) VALUES
('User 1', 'user1@example.com'),
('User 2', 'user2@example.com'),
...
('User 1000', 'user1000@example.com');
```

---

## Database Scaling Patterns

### Vertical Scaling (Scale Up)

**Approach:** Increase resources (CPU, RAM, disk) of single database server

**Pros:**
- Simple (no code changes)
- No data distribution complexity
- Strong consistency

**Cons:**
- Hardware limits (max instance size)
- Expensive (exponential cost increase)
- Single point of failure
- Downtime for upgrades

**Use When:** Small to medium workloads, budget allows, simplicity valued

### Horizontal Scaling (Scale Out)

**Approach:** Add more database servers

**Methods:**
1. Read Replicas
2. Sharding (Partitioning)
3. Database Clustering

### 1. Read Replicas

**Pattern:** Primary (write) + multiple replicas (read)

```
        ┌─────────┐
        │ Primary │  (writes)
        └────┬────┘
             │ replication
        ┌────┴─────────────┬────────────┐
        │                  │            │
    ┌───▼────┐      ┌─────▼──┐   ┌────▼────┐
    │Replica1│      │Replica2│   │Replica3 │  (reads)
    └────────┘      └────────┘   └─────────┘
```

**Implementation:**
```python
# Write to primary
primary_db.execute("INSERT INTO users (name) VALUES ('John')")

# Read from replica
user = replica_db.query("SELECT * FROM users WHERE id = 123")
```

**Pros:**
- Scale reads horizontally
- Offload read traffic from primary
- Geographic distribution (read from nearest replica)

**Cons:**
- Replication lag (eventual consistency)
- Writes still go to single primary
- Application must route reads to replicas

**Replication Lag:**
- **Synchronous replication**: Slow (wait for replicas), strong consistency
- **Asynchronous replication**: Fast (don't wait), eventual consistency

**Strategies for Replication Lag:**
1. **Read from primary for critical reads** (after writes)
2. **Session stickiness** (same user reads from same replica)
3. **Read-your-writes consistency** (read from primary briefly after write)

### 2. Sharding (Horizontal Partitioning)

**Pattern:** Split data across multiple database servers

**Sharding Strategies:**

**1. Range-Based Sharding:**
```
Shard 1: user_id 1-1,000,000
Shard 2: user_id 1,000,001-2,000,000
Shard 3: user_id 2,000,001-3,000,000
```

**Pros:** Simple, range queries efficient  
**Cons:** Unbalanced shards (hotspots), resharding complex

**2. Hash-Based Sharding:**
```
Shard = hash(user_id) % num_shards

user_id 123 → hash(123) % 3 = Shard 2
user_id 456 → hash(456) % 3 = Shard 0
```

**Pros:** Balanced distribution  
**Cons:** Range queries difficult, resharding requires rehashing

**3. Geographic Sharding:**
```
Shard 1: US users
Shard 2: EU users
Shard 3: APAC users
```

**Pros:** Low latency (data close to users), compliance (data residency)  
**Cons:** Unbalanced if regions have different usage

**4. Entity-Based Sharding (Lookup Table):**
```
Shard lookup table:
user_id | shard_id
123     | 1
456     | 2
789     | 1
```

**Pros:** Flexible, custom logic  
**Cons:** Lookup table required, extra query

**Sharding Challenges:**

**1. Cross-Shard Queries:**
```sql
-- Query all orders (requires querying all shards)
SELECT * FROM orders WHERE status = 'pending';
```

**Solution:**
- Avoid cross-shard queries (design around shard key)
- Query all shards and merge results (slower)
- Denormalize data to avoid cross-shard joins

**2. Distributed Transactions:**
- 2PC (Two-Phase Commit) slow and complex
- Prefer eventual consistency, saga pattern

**3. Resharding:**
- Adding/removing shards requires data migration
- Consistent hashing reduces reshuffling

**4. Shard Key Selection:**
- Choose key that balances load
- Choose key that isolates queries to single shard
- Immutable (don't change shard key value)

**Good Shard Keys:**
- `user_id` (if queries scoped to user)
- `tenant_id` (for multi-tenant apps)
- `geographic_region`

**Bad Shard Keys:**
- `created_at` (time-based, creates hotspots)
- Low cardinality columns (status, type)

### 3. Connection Pooling

**Problem:** Creating database connections expensive (100-200ms)

**Solution:** Reuse connections from a pool

```python
from sqlalchemy import create_engine
from sqlalchemy.pool import QueuePool

engine = create_engine(
    'postgresql://user:pass@localhost/db',
    poolclass=QueuePool,
    pool_size=20,        # Max connections in pool
    max_overflow=10,     # Max overflow connections
    pool_timeout=30,     # Timeout waiting for connection
    pool_recycle=3600    # Recycle connections after 1 hour
)
```

**Best Practices:**
- Set `pool_size` based on workload (20-50 typical)
- Set `max_overflow` for burst traffic
- Set `pool_recycle` to avoid idle connection timeouts
- Monitor connection pool usage

### 4. Database Partitioning

**Partitioning:** Split large table into smaller physical pieces (same database)

**vs. Sharding:** Sharding splits across different databases

**Partition Types:**

**1. Range Partitioning:**
```sql
CREATE TABLE orders (
    id BIGINT,
    user_id BIGINT,
    created_at TIMESTAMP,
    ...
) PARTITION BY RANGE (YEAR(created_at)) (
    PARTITION p2024 VALUES LESS THAN (2025),
    PARTITION p2025 VALUES LESS THAN (2026),
    PARTITION p2026 VALUES LESS THAN (2027)
);
```

**2. List Partitioning:**
```sql
PARTITION BY LIST (region) (
    PARTITION p_us VALUES IN ('US', 'CA', 'MX'),
    PARTITION p_eu VALUES IN ('UK', 'DE', 'FR'),
    PARTITION p_apac VALUES IN ('JP', 'CN', 'IN')
);
```

**3. Hash Partitioning:**
```sql
PARTITION BY HASH (user_id) PARTITIONS 4;
```

**Benefits:**
- Query performance (partition pruning)
- Maintenance (archive old partitions)
- Parallel query execution

---

## NoSQL Database Patterns

### Document Databases (MongoDB, Couchbase)

**Data Model:** JSON-like documents

**When to Use:**
- Flexible schema (schema evolves frequently)
- Nested/hierarchical data
- Document-centric queries

**Example Schema:**
```json
{
  "_id": "123",
  "firstName": "John",
  "lastName": "Doe",
  "email": "john@example.com",
  "addresses": [
    {
      "type": "home",
      "street": "123 Main St",
      "city": "New York",
      "zip": "10001"
    },
    {
      "type": "work",
      "street": "456 Office Blvd",
      "city": "New York",
      "zip": "10002"
    }
  ],
  "orders": [
    {"orderId": "A001", "total": 99.99, "date": "2026-01-01"},
    {"orderId": "A002", "total": 149.99, "date": "2026-01-10"}
  ]
}
```

**Embed vs. Reference:**

**Embed (denormalize):**
- Use when: Related data always accessed together, 1:few relationship
- Benefit: Single query, no joins
- Drawback: Data duplication, large documents

**Reference (normalize):**
- Use when: Many-to-many, 1:many (large), independent access
- Benefit: No duplication, smaller documents
- Drawback: Multiple queries (manual joins)

### Key-Value Stores (Redis, DynamoDB)

**Data Model:** Key → Value

**When to Use:**
- Caching
- Session storage
- Simple lookups by key

**Example:**
```
user:123:session → {"userId": "123", "token": "abc...", "expiresAt": "..."}
user:123:cart → [{"productId": "P001", "quantity": 2}]
```

### Column-Family Stores (Cassandra, HBase)

**Data Model:** Rows with dynamic columns

**When to Use:**
- Time-series data
- Write-heavy workloads
- Horizontal scalability critical

**Example:**
```
Row Key: user:123
Columns:
  firstName: "John"
  lastName: "Doe"
  email: "john@example.com"
```

### Graph Databases (Neo4j, Amazon Neptune)

**Data Model:** Nodes and relationships

**When to Use:**
- Social networks
- Recommendation engines
- Fraud detection
- Knowledge graphs

**Example:**
```
(User:John)-[:FRIENDS_WITH]->(User:Jane)
(User:John)-[:PURCHASED]->(Product:Laptop)
(Product:Laptop)-[:CATEGORY]->(Category:Electronics)
```

---

## Caching Strategies

### Cache-Aside (Lazy Loading)

**Pattern:**
1. Check cache
2. If miss, load from database
3. Store in cache
4. Return data

```python
def get_user(user_id):
    # 1. Check cache
    user = cache.get(f"user:{user_id}")
    
    if user:
        return user  # Cache hit
    
    # 2. Cache miss, load from database
    user = db.query("SELECT * FROM users WHERE id = ?", [user_id])
    
    # 3. Store in cache
    cache.set(f"user:{user_id}", user, ttl=3600)
    
    return user
```

**Pros:** Only caches requested data, handles cache failures gracefully  
**Cons:** Cache miss penalty (3x slower: check cache, query DB, write cache)

### Write-Through

**Pattern:**
1. Write to cache
2. Write to database synchronously

```python
def update_user(user_id, data):
    # 1. Write to database
    db.execute("UPDATE users SET ... WHERE id = ?", [user_id])
    
    # 2. Update cache
    cache.set(f"user:{user_id}", data, ttl=3600)
```

**Pros:** Cache always consistent with database  
**Cons:** Write latency (wait for both cache and database)

### Write-Behind (Write-Back)

**Pattern:**
1. Write to cache
2. Write to database asynchronously (batch)

**Pros:** Fast writes (only cache), batch writes reduce database load  
**Cons:** Data loss risk (cache failure before DB write), complex

### Cache Invalidation

**Strategies:**

**1. TTL (Time-To-Live):**
```python
cache.set(f"user:{user_id}", user, ttl=3600)  # Expires in 1 hour
```

**2. Explicit Invalidation:**
```python
def update_user(user_id, data):
    db.execute("UPDATE users SET ... WHERE id = ?", [user_id])
    cache.delete(f"user:{user_id}")  # Invalidate cache
```

**3. Event-Based Invalidation:**
```python
@event_handler('user.updated')
def on_user_updated(event):
    cache.delete(f"user:{event.user_id}")
```

**Cache Stampede Prevention:**

**Problem:** Cache expires, many requests hit database simultaneously

**Solution 1: Lock (first request regenerates cache):**
```python
def get_user(user_id):
    user = cache.get(f"user:{user_id}")
    if user:
        return user
    
    lock = cache.lock(f"lock:user:{user_id}", timeout=10)
    if lock.acquire(blocking=False):
        try:
            user = db.query("SELECT * FROM users WHERE id = ?", [user_id])
            cache.set(f"user:{user_id}", user, ttl=3600)
            return user
        finally:
            lock.release()
    else:
        # Wait and retry (another request is regenerating)
        time.sleep(0.1)
        return get_user(user_id)
```

**Solution 2: Probabilistic Early Expiration:**
```python
# Regenerate cache before TTL expires (based on probability)
remaining_ttl = cache.ttl(f"user:{user_id}")
if random.random() < (3600 - remaining_ttl) / 3600 * 0.1:
    # Regenerate cache early (10% probability as TTL decreases)
    user = db.query("SELECT * FROM users WHERE id = ?", [user_id])
    cache.set(f"user:{user_id}", user, ttl=3600)
```

---

## Database Security

### 1. Authentication

- Use strong passwords (enforce complexity requirements)
- Rotate database passwords regularly
- Use IAM database authentication (AWS RDS, Azure SQL)
- Avoid hardcoding credentials (use secrets manager)

### 2. Authorization (Least Privilege)

- Create separate users for applications (don't use root/admin)
- Grant minimal permissions needed
- Read-only users for reporting/analytics
- Use roles for permission management

```sql
-- Create read-only user
CREATE USER 'readonly'@'%' IDENTIFIED BY 'password';
GRANT SELECT ON mydb.* TO 'readonly'@'%';

-- Create application user with specific permissions
CREATE USER 'app_user'@'%' IDENTIFIED BY 'password';
GRANT SELECT, INSERT, UPDATE, DELETE ON mydb.users TO 'app_user'@'%';
GRANT SELECT, INSERT, UPDATE, DELETE ON mydb.orders TO 'app_user'@'%';
```

### 3. Network Security

- Firewall rules (whitelist application servers only)
- VPC/Private subnet (database not publicly accessible)
- SSL/TLS for connections
- Rotate SSL certificates

### 4. Encryption

**Encryption at Rest:**
- Transparent Data Encryption (TDE)
- AWS RDS encryption, Azure SQL TDE
- Encrypt backups

**Encryption in Transit:**
- Require SSL/TLS connections
- Certificate validation

```sql
-- MySQL: Require SSL
GRANT ALL ON mydb.* TO 'user'@'%' REQUIRE SSL;

-- PostgreSQL: SSL mode
postgresql://user:pass@host/db?sslmode=require
```

### 5. SQL Injection Prevention

❌ **Vulnerable (string concatenation):**
```python
user_id = request.get('user_id')
query = f"SELECT * FROM users WHERE id = {user_id}"
db.execute(query)
# Attack: user_id = "1 OR 1=1" → Returns all users
```

✅ **Safe (parameterized queries):**
```python
user_id = request.get('user_id')
query = "SELECT * FROM users WHERE id = ?"
db.execute(query, [user_id])
# user_id = "1 OR 1=1" → Treated as string, not SQL
```

✅ **Safe (ORM):**
```python
user_id = request.get('user_id')
user = User.objects.get(id=user_id)  # ORM uses parameterized queries
```

### 6. Audit Logging

- Log all authentication attempts
- Log privileged operations (GRANT, DROP, ALTER)
- Log data access (SELECT on sensitive tables)
- Monitor logs for anomalies

```sql
-- MySQL: Enable general log (all queries)
SET GLOBAL general_log = 'ON';

-- PostgreSQL: Enable log connections and statements
log_connections = on
log_statement = 'all'
```

### 7. Backup & Recovery

- Automated backups daily
- Test restore process regularly
- Backup retention policy (30 days typical)
- Offsite/cross-region backups
- Point-in-time recovery for critical databases

---

## Summary

- **Data Modeling:** Use surrogate keys, enforce referential integrity with foreign keys
- **Normalization:** Normalize to 3NF, denormalize selectively for performance
- **Indexing:** Index frequently queried columns, limit indexes per table (5-7)
- **Query Optimization:** Avoid N+1 queries, use EXPLAIN, limit results
- **Scaling:** Read replicas for reads, sharding for writes, connection pooling
- **NoSQL:** Choose database type based on access patterns
- **Caching:** Cache-aside pattern, set TTL, invalidate on updates
- **Security:** Least privilege, parameterized queries, encryption, audit logging

Well-designed databases are normalized appropriately, indexed effectively, scaled strategically, and secured comprehensively.
