# Advanced Design Patterns


### 1. Many-to-Many Relationships

```sql
-- Junction/Bridge table pattern
CREATE TABLE students (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100)
);

CREATE TABLE courses (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100)
);

-- Junction table with additional attributes
CREATE TABLE enrollments (
    student_id BIGINT REFERENCES students(id),
    course_id BIGINT REFERENCES courses(id),
    enrolled_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    grade DECIMAL(3, 2),
    
    PRIMARY KEY (student_id, course_id)
);
```

### 2. Inheritance/Polymorphism

**Single Table Inheritance:**

```sql
-- All types in one table with discriminator column
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    user_type VARCHAR(20) NOT NULL,  -- 'customer', 'admin', 'vendor'
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100),
    
    -- Customer-specific
    shipping_address TEXT,
    
    -- Vendor-specific
    company_name VARCHAR(100),
    tax_id VARCHAR(50),
    
    -- Admin-specific
    access_level INTEGER,
    
    CHECK (user_type IN ('customer', 'admin', 'vendor'))
);

-- Pros: Simple queries, single table
-- Cons: Sparse columns, complex constraints
```

**Class Table Inheritance:**

```sql
-- Base table
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL
);

-- Subtype tables
CREATE TABLE customers (
    user_id BIGINT PRIMARY KEY REFERENCES users(id),
    shipping_address TEXT,
    loyalty_points INTEGER DEFAULT 0
);

CREATE TABLE vendors (
    user_id BIGINT PRIMARY KEY REFERENCES users(id),
    company_name VARCHAR(100) NOT NULL,
    tax_id VARCHAR(50) UNIQUE
);

-- Pros: Normalized, no NULL columns
-- Cons: Requires joins, complex queries
```

### 3. Temporal Data (Historization)

```sql
-- Version tracking approach
CREATE TABLE products (
    id BIGSERIAL,
    version INTEGER,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10, 2) NOT NULL,
    valid_from TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    valid_to TIMESTAMP,
    is_current BOOLEAN DEFAULT true,
    
    PRIMARY KEY (id, version)
);

-- Query current version
SELECT * FROM products WHERE is_current = true;

-- Query at specific date
SELECT * FROM products 
WHERE '2024-01-01' BETWEEN valid_from AND COALESCE(valid_to, 'infinity');
```

### 4. Soft Deletes

```sql
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(100) NOT NULL,
    deleted_at TIMESTAMP,
    
    CONSTRAINT unique_active_email 
        UNIQUE (email) WHERE deleted_at IS NULL
);

-- Query active records
SELECT * FROM customers WHERE deleted_at IS NULL;

-- Soft delete
UPDATE customers SET deleted_at = CURRENT_TIMESTAMP WHERE id = 123;

-- Restore
UPDATE customers SET deleted_at = NULL WHERE id = 123;
```

### 5. Hierarchical Data

**Adjacency List:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    parent_id BIGINT REFERENCES categories(id)
);

-- Pros: Simple, easy updates
-- Cons: Recursive queries needed for full tree
```

**Closure Table:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL
);

CREATE TABLE category_paths (
    ancestor_id BIGINT REFERENCES categories(id),
    descendant_id BIGINT REFERENCES categories(id),
    depth INTEGER NOT NULL,
    
    PRIMARY KEY (ancestor_id, descendant_id)
);

-- Pros: Fast queries at all levels
-- Cons: More storage, complex updates
```

**Materialized Path:**

```sql
CREATE TABLE categories (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    path VARCHAR(500) NOT NULL  -- e.g., '1.3.7'
);

CREATE INDEX idx_categories_path ON categories USING GIST (path);

-- Pros: Fast queries, easy to find ancestors/descendants
-- Cons: Path updates on move
```
