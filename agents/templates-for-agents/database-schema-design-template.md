# Database Schema Design Document

## Database: [Database Name]

**Prepared By**: [Architect Name]  
**Date Created**: [YYYY-MM-DD]  
**Last Updated**: [YYYY-MM-DD]  
**Version**: [1.0]  
**Database Type**: [PostgreSQL / MySQL / MongoDB / etc]

---

## 1. Overview

### 1.1 Purpose

[Describe the purpose of this database and what application/system it supports]

### 1.2 Scope

This document covers:

* Database schema design and relationships
* Data models and entity definitions
* Indexes and constraints
* Data access patterns
* Performance considerations
* Security and compliance requirements

---

## 2. Database Architecture

### 2.1 Database Technology

**Database System**: [PostgreSQL 15 / MySQL 8.0 / MongoDB 6.0 / etc]

**Rationale**:

* [Reason 1]: [e.g., "ACID compliance required for financial transactions"]
* [Reason 2]: [e.g., "Strong JSON support needed"]
* [Reason 3]: [e.g., "Excellent performance for read-heavy workloads"]

### 2.2 High-Level Architecture

**Database Topology**: [Single instance / Primary-Replica / Multi-region / Sharded]

```
┌──────────────────────────────────────────┐
│        Application Servers               │
└──────────────────────────────────────────┘
                 ▼
┌──────────────────────────────────────────┐
│     Connection Pool / Load Balancer      │
└──────────────────────────────────────────┘
         ▼                           ▼
┌──────────────────┐        ┌──────────────────┐
│  Primary (Write) │───────▶│ Replica 1 (Read) │
│                  │        │                  │
└──────────────────┘        └──────────────────┘
         │
         └─────────────────▶┌──────────────────┐
                            │ Replica 2 (Read) │
                            └──────────────────┘
```

**Replication Strategy**: [Asynchronous / Synchronous]  
**Backup Strategy**: [Daily full backup + hourly incremental]  
**Retention Policy**: [30 days]

---

## 3. Data Models

### 3.1 Entity-Relationship Diagram (ERD)

[Include or link to ERD showing all tables and relationships]

```
┌─────────────┐         ┌─────────────┐
│   Users     │────1:N──│   Orders    │
│             │         │             │
│ id (PK)     │         │ id (PK)     │
│ email       │         │ user_id (FK)│
│ ...         │         │ ...         │
└─────────────┘         └─────────────┘
                             │
                            1:N
                             ▼
                        ┌─────────────┐
                        │ OrderItems  │
                        │             │
                        │ id (PK)     │
                        │ order_id(FK)│
                        │ product_id  │
                        └─────────────┘
```

---

## 4. Table Definitions

### 4.1 Users Table

**Purpose**: Store user account information

**Table Name**: `users`

| Column Name | Data Type | Nullable | Default | Description |
| :--- | :--- | :---: | :--- | :--- |
| `id` | UUID | No | gen_random_uuid() | Primary key |
| `email` | VARCHAR(255) | No | - | Unique user email |
| `password_hash` | VARCHAR(255) | No | - | Bcrypt hashed password |
| `first_name` | VARCHAR(100) | No | - | User's first name |
| `last_name` | VARCHAR(100) | No | - | User's last name |
| `status` | VARCHAR(20) | No | 'active' | active, inactive, suspended |
| `email_verified` | BOOLEAN | No | false | Email verification status |
| `created_at` | TIMESTAMP | No | CURRENT_TIMESTAMP | Record creation timestamp |
| `updated_at` | TIMESTAMP | No | CURRENT_TIMESTAMP | Last update timestamp |
| `deleted_at` | TIMESTAMP | Yes | NULL | Soft delete timestamp |

**Indexes**:

* `PRIMARY KEY (id)`
* `UNIQUE INDEX idx_users_email ON users(email)` - Unique email constraint
* `INDEX idx_users_status ON users(status)` - Filter by status
* `INDEX idx_users_created_at ON users(created_at)` - Sort by creation date

**Constraints**:

* `CHECK (status IN ('active', 'inactive', 'suspended'))`
* `CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')` - Email format validation

**Triggers**:

* `update_updated_at_column` - Automatically update `updated_at` on row modification

**SQL Definition**:

```sql
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    email_verified BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP,
    CONSTRAINT chk_users_status CHECK (status IN ('active', 'inactive', 'suspended')),
    CONSTRAINT chk_users_email CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

CREATE INDEX idx_users_status ON users(status);
CREATE INDEX idx_users_created_at ON users(created_at);

-- Trigger to update updated_at
CREATE TRIGGER update_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
```

---

### 4.2 Orders Table

**Purpose**: Store customer orders

**Table Name**: `orders`

| Column Name | Data Type | Nullable | Default | Description |
| :--- | :--- | :---: | :--- | :--- |
| `id` | UUID | No | gen_random_uuid() | Primary key |
| `user_id` | UUID | No | - | Foreign key to users table |
| `order_number` | VARCHAR(50) | No | - | Human-readable order number |
| `status` | VARCHAR(20) | No | 'pending' | Order status |
| `total_amount` | DECIMAL(10,2) | No | 0.00 | Total order amount |
| `currency` | CHAR(3) | No | 'USD' | Currency code (ISO 4217) |
| `shipping_address` | JSONB | Yes | NULL | Shipping address details |
| `billing_address` | JSONB | Yes | NULL | Billing address details |
| `payment_method` | VARCHAR(50) | Yes | NULL | Payment method used |
| `paid_at` | TIMESTAMP | Yes | NULL | Payment completion timestamp |
| `shipped_at` | TIMESTAMP | Yes | NULL | Shipment timestamp |
| `delivered_at` | TIMESTAMP | Yes | NULL | Delivery timestamp |
| `created_at` | TIMESTAMP | No | CURRENT_TIMESTAMP | Order creation timestamp |
| `updated_at` | TIMESTAMP | No | CURRENT_TIMESTAMP | Last update timestamp |

**Indexes**:

* `PRIMARY KEY (id)`
* `UNIQUE INDEX idx_orders_order_number ON orders(order_number)` - Unique order number
* `INDEX idx_orders_user_id ON orders(user_id)` - Find orders by user
* `INDEX idx_orders_status ON orders(status)` - Filter by status
* `INDEX idx_orders_created_at ON orders(created_at DESC)` - Recent orders first

**Foreign Keys**:

* `FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE RESTRICT`

**Constraints**:

* `CHECK (status IN ('pending', 'processing', 'shipped', 'delivered', 'cancelled'))`
* `CHECK (total_amount >= 0)`

**SQL Definition**:

```sql
CREATE TABLE orders (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL,
    order_number VARCHAR(50) NOT NULL UNIQUE,
    status VARCHAR(20) NOT NULL DEFAULT 'pending',
    total_amount DECIMAL(10,2) NOT NULL DEFAULT 0.00,
    currency CHAR(3) NOT NULL DEFAULT 'USD',
    shipping_address JSONB,
    billing_address JSONB,
    payment_method VARCHAR(50),
    paid_at TIMESTAMP,
    shipped_at TIMESTAMP,
    delivered_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE RESTRICT,
    CONSTRAINT chk_orders_status CHECK (status IN ('pending', 'processing', 'shipped', 'delivered', 'cancelled')),
    CONSTRAINT chk_orders_total CHECK (total_amount >= 0)
);

CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_orders_created_at ON orders(created_at DESC);

CREATE TRIGGER update_orders_updated_at
    BEFORE UPDATE ON orders
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
```

---

### 4.3 [Additional Tables]

[Repeat the structure above for each table in the database]

---

## 5. Relationships

### 5.1 One-to-Many Relationships

| Parent Table | Child Table | Relationship | Foreign Key | Notes |
| :--- | :--- | :--- | :--- | :--- |
| `users` | `orders` | 1:N | `orders.user_id` | One user can have many orders |
| `orders` | `order_items` | 1:N | `order_items.order_id` | One order can have many items |
| `products` | `order_items` | 1:N | `order_items.product_id` | One product can be in many orders |

### 5.2 Many-to-Many Relationships

| Table 1 | Table 2 | Junction Table | Notes |
| :--- | :--- | :--- | :--- |
| `users` | `roles` | `user_roles` | Users can have multiple roles |
| `products` | `categories` | `product_categories` | Products can belong to multiple categories |

### 5.3 Referential Integrity

**Cascading Rules**:

* `ON DELETE RESTRICT`: Prevent deletion if referenced (e.g., users with orders)
* `ON DELETE CASCADE`: Delete child records (e.g., order items when order deleted)
* `ON DELETE SET NULL`: Set foreign key to NULL (e.g., optional relationships)

---

## 6. Indexes and Performance

### 6.1 Index Strategy

**Primary Keys**:

* All tables use UUID primary keys for distributed system compatibility
* Alternative: Use BIGSERIAL for single-database systems

**Secondary Indexes**:

| Index Name | Table | Columns | Type | Purpose |
| :--- | :--- | :--- | :--- | :--- |
| `idx_users_email` | users | email | UNIQUE | Login, user lookup |
| `idx_orders_user_id` | orders | user_id | BTREE | Find user's orders |
| `idx_orders_status` | orders | status | BTREE | Filter by status |
| `idx_orders_created_at` | orders | created_at DESC | BTREE | Recent orders first |

**Composite Indexes**:

```sql
-- Find user's orders by status
CREATE INDEX idx_orders_user_status ON orders(user_id, status);

-- Search products by category and price
CREATE INDEX idx_products_category_price ON products(category_id, price);
```

### 6.2 Query Optimization

**Common Query Patterns**:

1. **Get user's recent orders**:

```sql
SELECT * FROM orders
WHERE user_id = $1
ORDER BY created_at DESC
LIMIT 10;
-- Uses: idx_orders_user_id + idx_orders_created_at
```

1. **Find pending orders**:

```sql
SELECT * FROM orders
WHERE status = 'pending'
  AND created_at > NOW() - INTERVAL '7 days';
-- Uses: idx_orders_status + idx_orders_created_at
```

**Avoid**:

* SELECT * (specify needed columns)
* N+1 queries (use JOINs or batch queries)
* Unindexed WHERE clauses on large tables

---

## 7. Data Access Patterns

### 7.1 Read Patterns

| Use Case | Frequency | Pattern | Optimization |
| :--- | :--- | :--- | :--- |
| User login | High | Single record by email | Indexed on email |
| User's orders | Medium | Multi-record by user_id | Indexed on user_id |
| Recent orders | Low | Time-range query | Indexed on created_at |
| Order details | High | Single record by id | Primary key |

### 7.2 Write Patterns

| Operation | Frequency | Pattern | Considerations |
| :--- | :--- | :--- | :--- |
| User registration | Low | Single INSERT | Email uniqueness check |
| Create order | Medium | Multiple INSERTs (order + items) | Transaction required |
| Update order status | Medium | Single UPDATE | Trigger updated_at |
| Soft delete user | Low | UPDATE set deleted_at | Preserve referential integrity |

### 7.3 Caching Strategy

**Cache Layers**:

* **Application Cache (Redis)**: User sessions, frequently accessed data
* **Database Query Cache**: Read replicas for read-heavy queries
* **CDN**: Static content (product images, etc.)

**Cache Invalidation**:

* TTL-based: [e.g., User profile: 15 minutes]
* Event-based: Invalidate on write operations

---

## 8. Data Integrity and Validation

### 8.1 Constraints

**NOT NULL Constraints**:

* All foreign keys must be NOT NULL
* Required business fields (email, names, etc.)

**UNIQUE Constraints**:

* `users.email` - One account per email
* `orders.order_number` - Unique order identifier

**CHECK Constraints**:

* Enum-like fields (status values)
* Business rules (e.g., total_amount >= 0)
* Data format validation (email regex)

### 8.2 Referential Integrity

**Foreign Key Rules**:

```sql
-- Prevent user deletion if they have orders
FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE RESTRICT

-- Delete order items when order is deleted
FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE

-- Set to NULL if optional relationship
FOREIGN KEY (assigned_user_id) REFERENCES users(id) ON DELETE SET NULL
```

### 8.3 Data Validation

**Application-Level Validation**:

* Email format (regex in CHECK constraint + application)
* Password strength (application only, not stored plaintext)
* Business logic (order total matches item sum)

**Database-Level Validation**:

* Data types (VARCHAR length, DECIMAL precision)
* CHECK constraints for enum values
* Triggers for complex validation

---

## 9. Security and Compliance

### 9.1 Data Security

**Encryption**:

* **At Rest**: Database-level encryption (e.g., AWS RDS encryption)
* **In Transit**: TLS 1.3 for all connections
* **Application-Level**: Sensitive fields (e.g., payment info) encrypted before storage

**Access Control**:

```sql
-- Application user (read/write)
CREATE USER app_user WITH PASSWORD 'secure_password';
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO app_user;

-- Read-only user (analytics)
CREATE USER analytics_user WITH PASSWORD 'secure_password';
GRANT SELECT ON ALL TABLES IN SCHEMA public TO analytics_user;

-- Admin user (schema changes)
CREATE USER admin_user WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE mydb TO admin_user;
```

### 9.2 Sensitive Data

**PII (Personally Identifiable Information)**:

* `users.email` - Encrypted or pseudonymized in backups
* `users.first_name`, `users.last_name` - Access logged
* `orders.shipping_address` - Encrypted in backups

**Compliance Requirements**:

* **GDPR**: Right to be forgotten (soft delete with `deleted_at`)
* **PCI DSS**: No storage of CVV, full card numbers tokenized
* **HIPAA**: Audit logging for all PHI access (if applicable)

### 9.3 Audit Logging

**Audit Trail Table**:

```sql
CREATE TABLE audit_log (
    id BIGSERIAL PRIMARY KEY,
    table_name VARCHAR(100) NOT NULL,
    record_id UUID NOT NULL,
    operation VARCHAR(10) NOT NULL, -- INSERT, UPDATE, DELETE
    old_values JSONB,
    new_values JSONB,
    changed_by UUID REFERENCES users(id),
    changed_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Trigger for audit logging
CREATE TRIGGER audit_users_changes
    AFTER INSERT OR UPDATE OR DELETE ON users
    FOR EACH ROW
    EXECUTE FUNCTION log_audit_trail();
```

---

## 10. Maintenance and Operations

### 10.1 Backup Strategy

**Backup Schedule**:

* **Full Backup**: Daily at 2:00 AM UTC
* **Incremental Backup**: Every 4 hours
* **Transaction Log Backup**: Every 15 minutes

**Retention Policy**:

* Daily backups: 30 days
* Weekly backups: 3 months
* Monthly backups: 1 year

**Recovery Testing**: Monthly restore test to staging environment

### 10.2 Database Migrations

**Migration Tool**: [Flyway / Liquibase / Alembic / dbmate]

**Migration Process**:

1. Create migration script (versioned: V001__initial_schema.sql)
2. Test in development environment
3. Review in staging environment
4. Apply to production during maintenance window

**Rollback Strategy**:

* All migrations must have corresponding DOWN migration
* Test rollback in staging before production deployment

### 10.3 Monitoring and Alerts

**Metrics to Monitor**:

* Query performance (slow query log > 1 second)
* Connection pool utilization (alert at 80%)
* Disk space (alert at 75% full)
* Replication lag (alert if > 5 seconds)
* Failed queries and deadlocks

**Alerting Thresholds**:

| Metric | Warning | Critical |
| :--- | :--- | :--- |
| Disk Usage | 75% | 90% |
| CPU Usage | 70% | 85% |
| Connection Pool | 80% | 95% |
| Replication Lag | 5s | 30s |

---

## 11. Scaling Strategy

### 11.1 Vertical Scaling

* **Current**: [e.g., db.t3.medium (2 vCPU, 4 GB RAM)]
* **Next Tier**: [e.g., db.r5.large (2 vCPU, 16 GB RAM)]
* **Maximum**: [e.g., db.r5.4xlarge (16 vCPU, 128 GB RAM)]

### 11.2 Horizontal Scaling

**Read Replicas**:

* Deploy 2-3 read replicas for read-heavy workloads
* Route analytics queries to replicas
* Use connection pooler (e.g., PgBouncer) to manage connections

**Sharding Strategy** (if needed):

* **Shard Key**: `user_id` (partition by user)
* **Shard Count**: Start with 4 shards, expand as needed
* **Routing**: Application-level routing based on user_id hash

### 11.3 Partitioning

**Table Partitioning**:

```sql
-- Partition orders table by creation date (monthly)
CREATE TABLE orders (
    -- columns...
) PARTITION BY RANGE (created_at);

CREATE TABLE orders_2025_01 PARTITION OF orders
    FOR VALUES FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE orders_2025_02 PARTITION OF orders
    FOR VALUES FROM ('2025-02-01') TO ('2025-03-01');
```

**Benefits**:

* Faster queries on recent data
* Easier archival (detach old partitions)
* Better index performance

---

## 12. Appendices

### Appendix A: Complete Schema SQL

[Link to full schema creation script]

### Appendix B: Sample Data

[Link to seed data script for development/testing]

### Appendix C: Migration Scripts

[Link to database migration history]

### Appendix D: Performance Benchmarks

[Link to query performance test results]

---

## 13. Revision History

| Version | Date | Author | Changes |
| :--- | :--- | :--- | :--- |
| 1.0 | YYYY-MM-DD | [Name] | Initial database design |
| 1.1 | YYYY-MM-DD | [Name] | Added audit logging tables |
