# Database Checklist

Comprehensive guidelines for reviewing database design, queries, and patterns.

## Table of Contents
- [SQL Query Optimization](#sql-query-optimization)
- [Schema Design](#schema-design)
- [Indexing Strategy](#indexing-strategy)
- [Transaction Management](#transaction-management)
- [ORM Best Practices](#orm-best-practices)
- [NoSQL Patterns](#nosql-patterns)
- [Connection Pooling](#connection-pooling)
- [Migration Management](#migration-management)

## SQL Query Optimization

### N+1 Query Problem

**Bad:**
```typescript
// ❌ N+1 queries - 1 query for users + N queries for posts
const users = await User.findAll();

for (const user of users) {
  user.posts = await Post.findAll({ where: { userId: user.id } });
}
```

**Good:**
```typescript
// ✅ Single query with JOIN
const users = await User.findAll({
  include: [{ model: Post, as: 'posts' }]
});

// ✅ Or batch loading
const users = await User.findAll();
const userIds = users.map(u => u.id);
const posts = await Post.findAll({
  where: { userId: userIds }
});

const postsByUser = _.groupBy(posts, 'userId');
users.forEach(user => {
  user.posts = postsByUser[user.id] || [];
});
```

### Index Usage

**Good:**
```sql
-- ✅ Query uses index
SELECT * FROM users WHERE email = 'user@example.com';
-- Index: CREATE INDEX idx_users_email ON users(email);

-- ✅ Composite index for multiple columns
SELECT * FROM orders WHERE user_id = 123 AND status = 'pending';
-- Index: CREATE INDEX idx_orders_user_status ON orders(user_id, status);

-- ✅ Covering index
SELECT id, email, name FROM users WHERE status = 'active';
-- Index: CREATE INDEX idx_users_status_cover ON users(status, id, email, name);
```

**Bad:**
```sql
-- ❌ Function on indexed column prevents index usage
SELECT * FROM users WHERE LOWER(email) = 'user@example.com';

-- ❌ Leading wildcard prevents index usage
SELECT * FROM users WHERE email LIKE '%@example.com';

-- ❌ OR on different columns
SELECT * FROM users WHERE email = 'a@b.com' OR name = 'John';
```

### Query Execution Plans

```sql
-- Analyze query performance
EXPLAIN ANALYZE
SELECT u.name, COUNT(o.id) as order_count
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
WHERE u.created_at > '2024-01-01'
GROUP BY u.id, u.name
HAVING COUNT(o.id) > 5;

-- Look for:
-- ✅ Index Scan instead of Seq Scan
-- ✅ Low cost and execution time
-- ❌ Nested Loop with large datasets
-- ❌ Sort operations on large datasets
```

### Pagination

**Good (Offset):**
```sql
-- ✅ For small to medium datasets
SELECT * FROM users
ORDER BY created_at DESC
LIMIT 20 OFFSET 40; -- Page 3

-- Index: CREATE INDEX idx_users_created ON users(created_at DESC);
```

**Good (Cursor-based):**
```sql
-- ✅ For large datasets, better performance
SELECT * FROM users
WHERE created_at < '2024-01-14 10:30:00'
ORDER BY created_at DESC
LIMIT 20;

-- Next page uses last item's created_at
```

## Schema Design

### Normalization

**Good:**
```sql
-- ✅ Normalized schema

-- Users table
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) UNIQUE NOT NULL,
  name VARCHAR(255) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Addresses table (one-to-many)
CREATE TABLE addresses (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  street VARCHAR(255) NOT NULL,
  city VARCHAR(100) NOT NULL,
  country VARCHAR(100) NOT NULL,
  CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Orders table
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id),
  total DECIMAL(10, 2) NOT NULL,
  status VARCHAR(50) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_user_id (user_id),
  INDEX idx_status (status)
);
```

**Bad:**
```sql
-- ❌ Denormalized with redundant data
CREATE TABLE orders (
  id SERIAL PRIMARY KEY,
  user_email VARCHAR(255),  -- Duplicated from users
  user_name VARCHAR(255),   -- Duplicated from users
  user_city VARCHAR(100),   -- Duplicated from addresses
  total DECIMAL(10, 2),
  -- Data inconsistency risk
);
```

### Data Types

**Good:**
```sql
-- ✅ Appropriate data types
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  description TEXT,
  price DECIMAL(10, 2) NOT NULL,  -- For money
  quantity INTEGER NOT NULL,
  is_active BOOLEAN DEFAULT true,
  metadata JSONB,  -- For flexible data
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);
```

**Bad:**
```sql
-- ❌ Wrong data types
CREATE TABLE products (
  id VARCHAR(255),  -- Should be INTEGER or UUID
  price VARCHAR(50),  -- Should be DECIMAL for money
  quantity VARCHAR(20),  -- Should be INTEGER
  is_active VARCHAR(10),  -- Should be BOOLEAN
  created_at VARCHAR(50)  -- Should be TIMESTAMP
);
```

## Indexing Strategy

### Index Types

```sql
-- ✅ B-tree index (default) for equality and range queries
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_orders_created ON orders(created_at);

-- ✅ Hash index for equality only (PostgreSQL)
CREATE INDEX idx_users_email_hash ON users USING HASH (email);

-- ✅ Partial index for specific conditions
CREATE INDEX idx_active_users ON users(email) WHERE status = 'active';

-- ✅ Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));

-- ✅ Composite index (order matters!)
CREATE INDEX idx_orders_user_status_date 
ON orders(user_id, status, created_at);

-- ✅ Full-text search index
CREATE INDEX idx_products_search ON products USING GIN(to_tsvector('english', name || ' ' || description));
```

### Index Maintenance

```sql
-- Monitor index usage
SELECT
  schemaname,
  tablename,
  indexname,
  idx_scan,
  idx_tup_read,
  idx_tup_fetch
FROM pg_stat_user_indexes
WHERE idx_scan = 0  -- Unused indexes
ORDER BY relname;

-- Rebuild fragmented indexes
REINDEX INDEX idx_users_email;
REINDEX TABLE users;
```

## Transaction Management

### ACID Principles

**Good:**
```typescript
// ✅ Proper transaction handling

// Node.js/Sequelize
await sequelize.transaction(async (t) => {
  // All queries use same transaction
  const user = await User.create({ email: 'user@example.com' }, { transaction: t });
  const account = await Account.create({ userId: user.id, balance: 1000 }, { transaction: t });
  
  // Both succeed or both rollback
});

// Python/Django
from django.db import transaction

@transaction.atomic
def transfer_money(from_account_id, to_account_id, amount):
    from_account = Account.objects.select_for_update().get(id=from_account_id)
    to_account = Account.objects.select_for_update().get(id=to_account_id)
    
    if from_account.balance < amount:
        raise ValueError("Insufficient funds")
    
    from_account.balance -= amount
    from_account.save()
    
    to_account.balance += amount
    to_account.save()
    
    # Creates transaction record
    Transaction.objects.create(
        from_account=from_account,
        to_account=to_account,
        amount=amount
    )
```

**Bad:**
```typescript
// ❌ No transaction - partial updates possible
async function transferMoney(fromId: string, toId: string, amount: number) {
  const fromAccount = await Account.findById(fromId);
  fromAccount.balance -= amount;
  await fromAccount.save();  // What if this succeeds...
  
  const toAccount = await Account.findById(toId);
  toAccount.balance += amount;
  await toAccount.save();  // ...but this fails?
}
```

### Isolation Levels

```sql
-- Set isolation level
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;  -- Default
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- Prevent race conditions
BEGIN;
SELECT * FROM accounts WHERE id = 123 FOR UPDATE;  -- Lock row
UPDATE accounts SET balance = balance - 100 WHERE id = 123;
COMMIT;
```

## ORM Best Practices

### Efficient Loading

**Good:**
```typescript
// ✅ Eager loading to prevent N+1
const users = await User.findAll({
  include: [
    { model: Post, as: 'posts' },
    { model: Profile, as: 'profile' }
  ]
});

// ✅ Lazy loading when appropriate
const user = await User.findByPk(userId);
if (needsPosts) {
  user.posts = await user.getPosts();
}

// ✅ Select specific fields
const users = await User.findAll({
  attributes: ['id', 'email', 'name']
});
```

### Raw Queries

**Good:**
```typescript
// ✅ Parameterized queries (safe from SQL injection)
const users = await sequelize.query(
  'SELECT * FROM users WHERE email = :email AND status = :status',
  {
    replacements: { email: userEmail, status: 'active' },
    type: QueryTypes.SELECT
  }
);

// ✅ Named parameters in Python
cursor.execute(
    "SELECT * FROM users WHERE email = %(email)s",
    {"email": user_email}
)
```

**Bad:**
```typescript
// ❌ SQL injection vulnerability!
const users = await sequelize.query(
  `SELECT * FROM users WHERE email = '${userEmail}'`
);
```

## NoSQL Patterns

### MongoDB

**Good:**
```typescript
// ✅ Embedded documents for one-to-few relationships
const userSchema = new Schema({
  email: { type: String, required: true, unique: true },
  name: String,
  addresses: [{
    street: String,
    city: String,
    country: String,
    isPrimary: Boolean
  }]
});

// ✅ References for one-to-many relationships
const orderSchema = new Schema({
  userId: { type: Schema.Types.ObjectId, ref: 'User', required: true },
  items: [{
    productId: { type: Schema.Types.ObjectId, ref: 'Product' },
    quantity: Number,
    price: Number
  }],
  total: Number,
  status: String
});

// ✅ Efficient queries with indexes
userSchema.index({ email: 1 });
orderSchema.index({ userId: 1, createdAt: -1 });
orderSchema.index({ status: 1 });

// ✅ Aggregation pipeline
const results = await Order.aggregate([
  { $match: { status: 'completed' } },
  { $group: {
    _id: '$userId',
    totalSpent: { $sum: '$total' },
    orderCount: { $sum: 1 }
  }},
  { $sort: { totalSpent: -1 } },
  { $limit: 10 }
]);
```

## Connection Pooling

**Good:**
```typescript
// ✅ PostgreSQL connection pool
import { Pool } from 'pg';

const pool = new Pool({
  host: process.env.DB_HOST,
  port: 5432,
  database: process.env.DB_NAME,
  user: process.env.DB_USER,
  password: process.env.DB_PASSWORD,
  max: 20,  // Maximum pool size
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

// Use pool for queries
async function getUser(id: string) {
  const client = await pool.connect();
  try {
    const result = await client.query('SELECT * FROM users WHERE id = $1', [id]);
    return result.rows[0];
  } finally {
    client.release();  // Always release!
  }
}

// Graceful shutdown
process.on('SIGTERM', async () => {
  await pool.end();
});
```

## Migration Management

**Good:**
```typescript
// ✅ Versioned migrations

// migrations/20240114100000-create-users.ts
export async function up(queryInterface: QueryInterface) {
  await queryInterface.createTable('users', {
    id: {
      type: DataTypes.INTEGER,
      primaryKey: true,
      autoIncrement: true
    },
    email: {
      type: DataTypes.STRING(255),
      allowNull: false,
      unique: true
    },
    created_at: {
      type: DataTypes.DATE,
      allowNull: false,
      defaultValue: DataTypes.NOW
    }
  });
  
  await queryInterface.addIndex('users', ['email']);
}

export async function down(queryInterface: QueryInterface) {
  await queryInterface.dropTable('users');
}

// migrations/20240114110000-add-user-status.ts
export async function up(queryInterface: QueryInterface) {
  await queryInterface.addColumn('users', 'status', {
    type: DataTypes.ENUM('active', 'inactive', 'banned'),
    defaultValue: 'active',
    allowNull: false
  });
  
  await queryInterface.addIndex('users', ['status']);
}

export async function down(queryInterface: QueryInterface) {
  await queryInterface.removeColumn('users', 'status');
}
```

**Key Principles:**
1. Always provide both `up` and `down` migrations
2. Never modify existing migrations
3. Test migrations on staging before production
4. Keep migrations small and focused
5. Run migrations in CI/CD pipeline
6. Backup database before migrations
