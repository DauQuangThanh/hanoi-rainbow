# Database Design Reference

Comprehensive guide for designing relational and NoSQL databases.

## Relational Database Design

### 1. Normal Forms

**First Normal Form (1NF)**
- Eliminate repeating groups
- Each column contains atomic values
- Each row is unique (has primary key)

```sql
-- ❌ Violates 1NF (repeating groups)
CREATE TABLE orders (
  id INT PRIMARY KEY,
  product1 VARCHAR(100),
  product2 VARCHAR(100),
  product3 VARCHAR(100)
);

-- ✅ Complies with 1NF
CREATE TABLE orders (
  id INT PRIMARY KEY,
  customer_id INT
);

CREATE TABLE order_items (
  id INT PRIMARY KEY,
  order_id INT,
  product_id INT,
  FOREIGN KEY (order_id) REFERENCES orders(id)
);
```

**Second Normal Form (2NF)**
- Must be in 1NF
- Remove partial dependencies (non-key attributes depend on entire key)

```sql
-- ❌ Violates 2NF (product_name depends only on product_id)
CREATE TABLE order_items (
  order_id INT,
  product_id INT,
  product_name VARCHAR(100),
  quantity INT,
  PRIMARY KEY (order_id, product_id)
);

-- ✅ Complies with 2NF
CREATE TABLE order_items (
  order_id INT,
  product_id INT,
  quantity INT,
  PRIMARY KEY (order_id, product_id),
  FOREIGN KEY (product_id) REFERENCES products(id)
);

CREATE TABLE products (
  id INT PRIMARY KEY,
  name VARCHAR(100)
);
```

**Third Normal Form (3NF)**
- Must be in 2NF
- Remove transitive dependencies (non-key attributes depend only on primary key)

```sql
-- ❌ Violates 3NF (city depends on zip_code, not user_id)
CREATE TABLE users (
  id INT PRIMARY KEY,
  name VARCHAR(100),
  zip_code VARCHAR(10),
  city VARCHAR(100)
);

-- ✅ Complies with 3NF
CREATE TABLE users (
  id INT PRIMARY KEY,
  name VARCHAR(100),
  zip_code VARCHAR(10),
  FOREIGN KEY (zip_code) REFERENCES zip_codes(code)
);

CREATE TABLE zip_codes (
  code VARCHAR(10) PRIMARY KEY,
  city VARCHAR(100)
);
```

**Boyce-Codd Normal Form (BCNF)**
- Must be in 3NF
- Every determinant is a candidate key

---

### 2. Indexing Strategies

**Primary Key Index**
```sql
-- Automatically indexed
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid()
);
```

**Foreign Key Indexes**
```sql
CREATE TABLE posts (
  id UUID PRIMARY KEY,
  user_id UUID NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Add index for foreign key
CREATE INDEX idx_posts_user_id ON posts(user_id);
```

**Single Column Index**
```sql
-- Index for WHERE clause
CREATE INDEX idx_users_email ON users(email);

-- Query benefits: WHERE email = 'user@example.com'
```

**Composite Index**
```sql
-- For queries filtering/sorting multiple columns
CREATE INDEX idx_posts_user_status_created 
ON posts(user_id, status, created_at DESC);

-- Benefits queries like:
-- WHERE user_id = ? AND status = ?
-- WHERE user_id = ? AND status = ? ORDER BY created_at DESC
```

**Partial Index**
```sql
-- Index only subset of rows
CREATE INDEX idx_posts_published 
ON posts(published_at) 
WHERE status = 'published';
```

**Full-Text Index**
```sql
-- PostgreSQL
CREATE INDEX idx_posts_fulltext 
ON posts USING GIN(to_tsvector('english', title || ' ' || content));

-- Query
SELECT * FROM posts 
WHERE to_tsvector('english', title || ' ' || content) 
@@ to_tsquery('english', 'search & terms');
```

**Index Best Practices**
- Index columns used in WHERE, JOIN, ORDER BY
- Don't over-index (slows writes)
- Index foreign keys
- Use composite indexes for multi-column queries
- Monitor index usage and remove unused ones
- Consider index size vs. benefit

---

### 3. Common Table Patterns

**Timestamps**
```sql
CREATE TABLE base_table (
  id UUID PRIMARY KEY,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- Trigger to auto-update updated_at
CREATE TRIGGER set_updated_at
BEFORE UPDATE ON base_table
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();
```

**Soft Deletes**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  deleted_at TIMESTAMP NULL,
  
  -- Ensure active emails are unique
  CONSTRAINT unique_active_email 
  UNIQUE (email) WHERE deleted_at IS NULL
);

-- Query active users
SELECT * FROM users WHERE deleted_at IS NULL;
```

**Versioning/Audit Trail**
```sql
CREATE TABLE posts (
  id UUID PRIMARY KEY,
  title VARCHAR(500),
  content TEXT,
  version INT NOT NULL DEFAULT 1,
  created_by UUID REFERENCES users(id),
  updated_by UUID REFERENCES users(id),
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE TABLE posts_history (
  id UUID PRIMARY KEY,
  post_id UUID REFERENCES posts(id),
  title VARCHAR(500),
  content TEXT,
  version INT NOT NULL,
  changed_by UUID REFERENCES users(id),
  changed_at TIMESTAMP NOT NULL DEFAULT NOW()
);
```

**Polymorphic Associations**
```sql
CREATE TABLE comments (
  id UUID PRIMARY KEY,
  commentable_type VARCHAR(50) NOT NULL, -- 'post', 'photo', etc.
  commentable_id UUID NOT NULL,
  content TEXT NOT NULL,
  user_id UUID REFERENCES users(id)
);

CREATE INDEX idx_comments_polymorphic 
ON comments(commentable_type, commentable_id);
```

**Self-Referencing (Tree Structure)**
```sql
CREATE TABLE categories (
  id UUID PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  parent_id UUID REFERENCES categories(id),
  path VARCHAR(500) -- Materialized path: '/electronics/computers/laptops'
);

-- Get all descendants
WITH RECURSIVE category_tree AS (
  SELECT id, name, parent_id, 1 as depth
  FROM categories
  WHERE id = 'root_category_id'
  
  UNION ALL
  
  SELECT c.id, c.name, c.parent_id, ct.depth + 1
  FROM categories c
  INNER JOIN category_tree ct ON c.parent_id = ct.id
)
SELECT * FROM category_tree;
```

**Many-to-Many (Junction Table)**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY,
  name VARCHAR(100)
);

CREATE TABLE roles (
  id UUID PRIMARY KEY,
  name VARCHAR(50)
);

CREATE TABLE user_roles (
  user_id UUID REFERENCES users(id) ON DELETE CASCADE,
  role_id UUID REFERENCES roles(id) ON DELETE CASCADE,
  assigned_at TIMESTAMP NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, role_id)
);

CREATE INDEX idx_user_roles_user ON user_roles(user_id);
CREATE INDEX idx_user_roles_role ON user_roles(role_id);
```

---

### 4. Data Types

**PostgreSQL Recommended Types**
```sql
CREATE TABLE data_types_example (
  -- Primary keys
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  -- id SERIAL PRIMARY KEY,  -- Auto-increment integer
  
  -- Text
  short_text VARCHAR(255),
  long_text TEXT,
  fixed_length CHAR(10),
  
  -- Numbers
  small_int SMALLINT,        -- -32768 to 32767
  regular_int INTEGER,       -- -2147483648 to 2147483647
  big_int BIGINT,           -- Very large numbers
  decimal_number DECIMAL(10, 2),  -- Precise (10 digits, 2 decimal)
  float_number REAL,        -- Approximate
  double_number DOUBLE PRECISION,
  
  -- Boolean
  is_active BOOLEAN DEFAULT true,
  
  -- Date/Time
  date_only DATE,
  time_only TIME,
  timestamp_val TIMESTAMP,
  timestamp_tz TIMESTAMPTZ,  -- With timezone (recommended)
  
  -- JSON
  json_data JSON,
  jsonb_data JSONB,  -- Binary JSON (faster, recommended)
  
  -- Arrays
  tags TEXT[],
  numbers INTEGER[],
  
  -- Binary
  file_data BYTEA,
  
  -- Network
  ip_address INET,
  mac_address MACADDR,
  
  -- Other
  uuid_val UUID,
  enum_val status_enum
);

-- Enum type
CREATE TYPE status_enum AS ENUM ('draft', 'published', 'archived');
```

---

### 5. Constraints

**Primary Key**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid()
);
```

**Foreign Key**
```sql
CREATE TABLE posts (
  id UUID PRIMARY KEY,
  user_id UUID NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(id) 
    ON DELETE CASCADE 
    ON UPDATE CASCADE
);
```

**Unique**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  username VARCHAR(100) NOT NULL UNIQUE
);

-- Composite unique
CREATE TABLE user_preferences (
  user_id UUID,
  key VARCHAR(100),
  value TEXT,
  UNIQUE (user_id, key)
);
```

**Check**
```sql
CREATE TABLE products (
  id UUID PRIMARY KEY,
  price DECIMAL(10, 2) CHECK (price >= 0),
  quantity INTEGER CHECK (quantity >= 0),
  status VARCHAR(20) CHECK (status IN ('active', 'inactive', 'discontinued'))
);
```

**Not Null**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  name VARCHAR(100) NOT NULL
);
```

---

### 6. Transactions

**ACID Properties**
- **Atomicity**: All or nothing
- **Consistency**: Valid state transitions
- **Isolation**: Concurrent transactions don't interfere
- **Durability**: Committed changes persist

**Usage Example**
```sql
BEGIN;

-- Transfer money between accounts
UPDATE accounts SET balance = balance - 100 WHERE id = 'acc_123';
UPDATE accounts SET balance = balance + 100 WHERE id = 'acc_456';

-- Insert transaction record
INSERT INTO transactions (from_account, to_account, amount) 
VALUES ('acc_123', 'acc_456', 100);

COMMIT;
-- or ROLLBACK on error
```

**Isolation Levels**
```sql
-- Read Uncommitted (lowest isolation, dirty reads possible)
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

-- Read Committed (default in PostgreSQL)
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;

-- Repeatable Read (prevents non-repeatable reads)
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Serializable (highest isolation, prevents anomalies)
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
```

---

### 7. Query Optimization

**Explain Query Plan**
```sql
EXPLAIN ANALYZE
SELECT u.name, COUNT(p.id) as post_count
FROM users u
LEFT JOIN posts p ON u.id = p.user_id
WHERE u.created_at > '2026-01-01'
GROUP BY u.id, u.name
ORDER BY post_count DESC
LIMIT 10;
```

**Avoid N+1 Queries**
```sql
-- ❌ Bad (N+1)
SELECT * FROM posts;
-- Then for each post: SELECT * FROM comments WHERE post_id = ?

-- ✅ Good (Single query or join)
SELECT p.*, 
       COALESCE(json_agg(c.*) FILTER (WHERE c.id IS NOT NULL), '[]') as comments
FROM posts p
LEFT JOIN comments c ON p.id = c.post_id
GROUP BY p.id;
```

**Use Covering Indexes**
```sql
-- Include all columns needed in query
CREATE INDEX idx_posts_covering 
ON posts(user_id, status) 
INCLUDE (title, created_at);

-- This query only needs index (no table access)
SELECT title, created_at 
FROM posts 
WHERE user_id = ? AND status = 'published';
```

**Pagination Optimization**
```sql
-- ❌ Bad (OFFSET on large datasets)
SELECT * FROM posts 
ORDER BY created_at DESC 
LIMIT 20 OFFSET 10000;

-- ✅ Good (Cursor-based)
SELECT * FROM posts 
WHERE created_at < ?  -- Last seen timestamp
ORDER BY created_at DESC 
LIMIT 20;
```

---

## NoSQL Database Design

### 1. Document Database (MongoDB)

**Schema Design Patterns**

**Embedding (Denormalization)**
```javascript
// One-to-Few: Embed addresses
{
  _id: ObjectId("..."),
  name: "John Doe",
  email: "john@example.com",
  addresses: [
    {
      type: "home",
      street: "123 Main St",
      city: "New York",
      zip: "10001"
    },
    {
      type: "work",
      street: "456 Work Ave",
      city: "New York",
      zip: "10002"
    }
  ]
}
```

**Referencing (Normalization)**
```javascript
// One-to-Many: Reference posts
// User document
{
  _id: ObjectId("user123"),
  name: "John Doe",
  email: "john@example.com"
}

// Post documents
{
  _id: ObjectId("post456"),
  user_id: ObjectId("user123"),
  title: "My Post",
  content: "..."
}
```

**Two-Way Referencing**
```javascript
// Many-to-Many: Users and Groups
// User document
{
  _id: ObjectId("user123"),
  name: "John Doe",
  group_ids: [ObjectId("group1"), ObjectId("group2")]
}

// Group document
{
  _id: ObjectId("group1"),
  name: "Developers",
  member_ids: [ObjectId("user123"), ObjectId("user456")]
}
```

**Extended Reference (Denormalization)**
```javascript
// Store frequently accessed fields
{
  _id: ObjectId("post456"),
  title: "My Post",
  author: {
    id: ObjectId("user123"),
    name: "John Doe",  // Denormalized
    avatar: "https://..."  // Denormalized
  },
  content: "..."
}
```

**Indexing**
```javascript
// Single field
db.users.createIndex({ email: 1 });

// Compound index
db.posts.createIndex({ user_id: 1, created_at: -1 });

// Text index
db.posts.createIndex({ title: "text", content: "text" });

// Geospatial index
db.stores.createIndex({ location: "2dsphere" });

// Unique index
db.users.createIndex({ email: 1 }, { unique: true });
```

---

### 2. Key-Value Store (Redis)

**Data Structures**

**Strings**
```redis
SET user:123:name "John Doe"
GET user:123:name
SETEX session:abc 3600 "session_data"  -- With expiry
INCR page_views:home
```

**Hashes (Objects)**
```redis
HSET user:123 name "John Doe" email "john@example.com" age 30
HGET user:123 name
HGETALL user:123
HINCRBY user:123 age 1
```

**Lists (Ordered)**
```redis
LPUSH notifications:user123 "New message"
LRANGE notifications:user123 0 9  -- Get first 10
LTRIM notifications:user123 0 99  -- Keep only latest 100
```

**Sets (Unique Values)**
```redis
SADD tags:post123 "javascript" "nodejs" "api"
SMEMBERS tags:post123
SINTER tags:post123 tags:post456  -- Common tags
```

**Sorted Sets (Ordered by Score)**
```redis
ZADD leaderboard 100 "user123" 95 "user456" 88 "user789"
ZRANGE leaderboard 0 9 WITHSCORES  -- Top 10
ZREVRANK leaderboard "user123"  -- User's rank
```

**Common Patterns**

**Caching**
```redis
-- Cache user data
SET cache:user:123 '{"name":"John","email":"..."}' EX 300

-- Check cache first in application
user = redis.get('cache:user:123')
if (!user) {
  user = db.users.find(123)
  redis.setex('cache:user:123', 300, JSON.stringify(user))
}
```

**Session Storage**
```redis
SETEX session:abc123 3600 '{"user_id":"123","role":"admin"}'
```

**Rate Limiting**
```redis
-- Fixed window
key = "ratelimit:api:" + userId + ":" + currentHour
INCR key
EXPIRE key 3600
count = GET key
if count > limit: reject()
```

**Pub/Sub**
```redis
-- Publisher
PUBLISH notifications "New message"

-- Subscriber
SUBSCRIBE notifications
```

---

### 3. Column-Family (Cassandra)

**Data Modeling**
```cql
-- Query-first design
CREATE TABLE users_by_email (
  email TEXT PRIMARY KEY,
  user_id UUID,
  name TEXT,
  created_at TIMESTAMP
);

CREATE TABLE posts_by_user (
  user_id UUID,
  created_at TIMESTAMP,
  post_id UUID,
  title TEXT,
  content TEXT,
  PRIMARY KEY ((user_id), created_at, post_id)
) WITH CLUSTERING ORDER BY (created_at DESC);

-- Query efficiently
SELECT * FROM posts_by_user 
WHERE user_id = ? 
ORDER BY created_at DESC 
LIMIT 10;
```

---

### 4. Graph Database (Neo4j)

**Data Model**
```cypher
// Create nodes
CREATE (u:User {id: 'user123', name: 'John Doe'})
CREATE (p:Post {id: 'post456', title: 'My Post'})

// Create relationships
CREATE (u)-[:AUTHORED]->(p)
CREATE (u1:User)-[:FOLLOWS]->(u2:User)
CREATE (u)-[:LIKES]->(p)

// Query relationships
MATCH (u:User {name: 'John Doe'})-[:AUTHORED]->(p:Post)
RETURN p

// Find friends of friends
MATCH (me:User {id: 'user123'})-[:FOLLOWS]->()-[:FOLLOWS]->(fof:User)
WHERE NOT (me)-[:FOLLOWS]->(fof) AND me <> fof
RETURN fof

// Shortest path
MATCH path = shortestPath((u1:User)-[*]-(u2:User))
WHERE u1.id = 'user123' AND u2.id = 'user456'
RETURN path
```

---

## Database Scaling Strategies

### 1. Read Replicas
```
Master (Write) ─┬─► Replica 1 (Read)
                ├─► Replica 2 (Read)
                └─► Replica 3 (Read)
```

### 2. Sharding (Horizontal Partitioning)
```
Shard 1: Users A-F
Shard 2: Users G-M
Shard 3: Users N-S
Shard 4: Users T-Z
```

### 3. Vertical Partitioning
```
Users table ─┬─► Basic info (id, email, name)
             └─► Extended profile (bio, preferences)
```

### 4. Caching Layer
```
Application ─► Cache (Redis) ─► Database
```

---

## Database Design Checklist

- [ ] Normalized to 3NF (or denormalized with reason)
- [ ] Primary keys defined
- [ ] Foreign keys with appropriate constraints
- [ ] Indexes on frequently queried columns
- [ ] Timestamps (created_at, updated_at)
- [ ] Soft delete strategy (if needed)
- [ ] Appropriate data types chosen
- [ ] Constraints (NOT NULL, CHECK, UNIQUE)
- [ ] Migration strategy planned
- [ ] Backup and recovery plan
- [ ] Scaling strategy (read replicas, sharding)
- [ ] Query performance tested
- [ ] Connection pooling configured

This reference provides comprehensive patterns for both relational and NoSQL database design.
