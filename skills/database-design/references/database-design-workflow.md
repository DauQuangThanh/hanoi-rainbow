# Database Design Workflow


### 1. Requirements Analysis

**Gather Requirements:**

```markdown
Business Requirements:
- What data needs to be stored?
- What operations will be performed?
- What queries will be most common?
- What are the performance requirements?
- What is the expected data volume and growth?
- What are compliance/security requirements?

Example: E-commerce Platform
- Store: products, customers, orders, payments, reviews
- Operations: browse products, place orders, process payments, track shipments
- Common queries: product search, order history, inventory checks
- Performance: <100ms for product searches, <50ms for checkout
- Volume: 1M products, 100K customers, 10K orders/day
- Compliance: PCI-DSS for payments, GDPR for customer data
```

**Identify Entities and Attributes:**

```markdown
Entities:
- Customer: id, email, name, address, phone, created_at
- Product: id, name, description, price, category, stock, sku
- Order: id, customer_id, order_date, status, total_amount
- OrderItem: id, order_id, product_id, quantity, price
- Payment: id, order_id, amount, method, status, transaction_id
- Review: id, product_id, customer_id, rating, comment, created_at
```

### 2. Conceptual Data Modeling

**Create Entity-Relationship Diagram (ERD):**

```
Customer (1) ----< (M) Order (1) ----< (M) OrderItem (M) >---- (1) Product
    |                                                                  |
    | (1)                                                              | (1)
    |                                                                  |
    v (M)                                                              v (M)
  Review                                                             Review
```

**Define Relationships:**

```markdown
Customer-Order: One-to-Many
- One customer can have many orders
- One order belongs to one customer

Order-OrderItem: One-to-Many
- One order contains many items
- One item belongs to one order

Product-OrderItem: One-to-Many
- One product can appear in many order items
- One order item references one product

Customer-Review: One-to-Many
Product-Review: One-to-Many
- One customer can write many reviews
- One product can have many reviews
```

**Determine Cardinality and Optionality:**

```markdown
Customer -> Order: 1:M (optional)
- A customer can exist without orders
- An order must have a customer

Order -> OrderItem: 1:M (mandatory)
- An order must have at least one item

Product -> OrderItem: 1:M (optional)
- A product can exist without being ordered

Order -> Payment: 1:1 (mandatory)
- Each order must have a payment
- Each payment belongs to one order
```

### 3. Logical Data Modeling

**Apply Normalization:**

**First Normal Form (1NF):**
- Eliminate repeating groups
- Each cell contains atomic values

```sql
-- Violates 1NF: Multiple values in one column
BAD:
customers (id, name, phone_numbers)
1, 'John', '123-456, 789-012'

-- Compliant with 1NF
GOOD:
customers (id, name)
1, 'John'

customer_phones (id, customer_id, phone_number, phone_type)
1, 1, '123-456', 'mobile'
2, 1, '789-012', 'home'
```

**Second Normal Form (2NF):**
- Must be in 1NF
- Remove partial dependencies (non-key attributes depend on entire primary key)

```sql
-- Violates 2NF: product_name depends only on product_id, not the composite key
BAD:
order_items (order_id, product_id, product_name, quantity, price)
PK: (order_id, product_id)

-- Compliant with 2NF
GOOD:
order_items (order_id, product_id, quantity, price)
PK: (order_id, product_id)

products (product_id, product_name)
PK: product_id
```

**Third Normal Form (3NF):**
- Must be in 2NF
- Remove transitive dependencies (non-key attributes depend on other non-key attributes)

```sql
-- Violates 3NF: city_name depends on zip_code, not on customer_id
BAD:
customers (id, name, zip_code, city_name, state)

-- Compliant with 3NF
GOOD:
customers (id, name, zip_code)
zip_codes (zip_code, city_name, state)
```

**When to Denormalize:**

```markdown
Acceptable Denormalization Scenarios:

1. Read-Heavy Systems
   - Duplicate data to avoid expensive joins
   - Example: Store product name in order_items for order history queries

2. Reporting and Analytics
   - Aggregate tables for dashboard queries
   - Example: daily_sales_summary table

3. Caching Computed Values
   - Store calculated values to avoid repeated computation
   - Example: order.total_amount instead of SUM(order_items.price)

4. Historical Snapshots
   - Preserve data as it was at transaction time
   - Example: Store product price in order_items, not reference products.price
```

### 4. Physical Data Modeling

**Define Tables with Data Types:**

```sql
-- PostgreSQL Example
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    phone VARCHAR(20),
    date_of_birth DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT true,
    
    CONSTRAINT email_format CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$')
);

CREATE TABLE products (
    id BIGSERIAL PRIMARY KEY,
    sku VARCHAR(50) UNIQUE NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price DECIMAL(10, 2) NOT NULL CHECK (price >= 0),
    category_id INTEGER REFERENCES categories(id),
    stock_quantity INTEGER DEFAULT 0 CHECK (stock_quantity >= 0),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT price_positive CHECK (price > 0)
);

CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    customer_id BIGINT NOT NULL REFERENCES customers(id),
    order_number VARCHAR(50) UNIQUE NOT NULL,
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(20) DEFAULT 'pending',
    total_amount DECIMAL(10, 2) NOT NULL,
    shipping_address_id BIGINT REFERENCES addresses(id),
    billing_address_id BIGINT REFERENCES addresses(id),
    
    CONSTRAINT status_values CHECK (status IN ('pending', 'processing', 'shipped', 'delivered', 'cancelled'))
);

CREATE TABLE order_items (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
    product_id BIGINT NOT NULL REFERENCES products(id),
    quantity INTEGER NOT NULL CHECK (quantity > 0),
    unit_price DECIMAL(10, 2) NOT NULL,
    subtotal DECIMAL(10, 2) NOT NULL,
    
    UNIQUE (order_id, product_id)
);
```

**Choose Appropriate Data Types:**

```markdown
PostgreSQL/MySQL Data Type Guidelines:

Integers:
- SMALLINT: -32,768 to 32,767 (2 bytes)
- INTEGER: -2B to 2B (4 bytes)
- BIGINT: -9 quintillion to 9 quintillion (8 bytes)
- Use BIGINT for IDs that might grow large

Strings:
- CHAR(n): Fixed length, padded with spaces
- VARCHAR(n): Variable length, up to n characters
- TEXT: Unlimited length (use for descriptions, comments)
- Use VARCHAR for most text fields with reasonable limit

Numbers:
- DECIMAL(p, s): Fixed precision (p=total digits, s=decimal places)
- FLOAT/REAL: 4 bytes, approximate
- DOUBLE: 8 bytes, approximate
- Use DECIMAL for money, FLOAT for scientific data

Dates/Times:
- DATE: Date only (2024-01-14)
- TIME: Time only (14:30:00)
- TIMESTAMP: Date and time with timezone
- INTERVAL: Duration (3 days, 2 hours)

Boolean:
- BOOLEAN: true/false/null

Binary:
- BYTEA (PostgreSQL): Binary data
- BLOB (MySQL): Binary large object
- Use for files, images (consider object storage instead)

JSON:
- JSON: Validates JSON, stores as text
- JSONB (PostgreSQL): Binary JSON, indexable
- Use for flexible schemas, API responses
```

### 5. Indexing Strategy

**Primary Key Indexes:**

```sql
-- Automatically created
CREATE TABLE users (
    id BIGSERIAL PRIMARY KEY,  -- Implicit index on id
    email VARCHAR(255) UNIQUE   -- Implicit index on email
);
```

**Single-Column Indexes:**

```sql
-- For frequently queried columns
CREATE INDEX idx_orders_customer_id ON orders(customer_id);
CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_products_category ON products(category_id);
CREATE INDEX idx_orders_date ON orders(order_date);
```

**Composite Indexes:**

```sql
-- For queries filtering on multiple columns
-- Order matters: most selective column first
CREATE INDEX idx_orders_customer_date ON orders(customer_id, order_date);
CREATE INDEX idx_products_category_price ON products(category_id, price);

-- Query using leftmost columns can use the index
-- ✅ Uses idx_orders_customer_date:
SELECT * FROM orders WHERE customer_id = 123;
SELECT * FROM orders WHERE customer_id = 123 AND order_date > '2024-01-01';

-- ❌ Cannot use idx_orders_customer_date:
SELECT * FROM orders WHERE order_date > '2024-01-01';
```

**Partial Indexes:**

```sql
-- Index only subset of rows (PostgreSQL)
CREATE INDEX idx_active_customers ON customers(id) WHERE is_active = true;
CREATE INDEX idx_pending_orders ON orders(id) WHERE status = 'pending';
```

**Full-Text Search Indexes:**

```sql
-- PostgreSQL
CREATE INDEX idx_products_search ON products 
USING GIN (to_tsvector('english', name || ' ' || description));

-- Query
SELECT * FROM products 
WHERE to_tsvector('english', name || ' ' || description) 
@@ to_tsquery('english', 'laptop & gaming');

-- MySQL
ALTER TABLE products ADD FULLTEXT INDEX idx_products_fulltext (name, description);

-- Query
SELECT * FROM products 
WHERE MATCH(name, description) AGAINST('gaming laptop' IN NATURAL LANGUAGE MODE);
```

**Covering Indexes:**

```sql
-- Include all columns needed by query
CREATE INDEX idx_orders_customer_covering 
ON orders(customer_id) 
INCLUDE (order_date, total_amount, status);

-- Query can be satisfied entirely from index
SELECT order_date, total_amount, status 
FROM orders 
WHERE customer_id = 123;
```

**Index Guidelines:**

```markdown
When to Add Indexes:
✅ Foreign key columns
✅ Columns in WHERE clauses
✅ Columns in JOIN conditions
✅ Columns in ORDER BY clauses
✅ Columns in GROUP BY clauses

When NOT to Add Indexes:
❌ Small tables (< 1000 rows)
❌ Columns with low cardinality (e.g., gender, boolean)
❌ Columns rarely queried
❌ Tables with high write volume and few reads

Index Maintenance:
- Monitor index usage: pg_stat_user_indexes (PostgreSQL)
- Drop unused indexes
- Rebuild fragmented indexes periodically
- Update statistics: ANALYZE table_name
```

### 6. Constraints and Data Integrity

**Primary Keys:**

```sql
-- Single column
CREATE TABLE customers (
    id BIGSERIAL PRIMARY KEY
);

-- Composite key
CREATE TABLE order_items (
    order_id BIGINT,
    product_id BIGINT,
    PRIMARY KEY (order_id, product_id)
);

-- Natural vs. Surrogate keys
-- Natural: email, sku (business meaningful)
-- Surrogate: id (system generated)
-- Recommendation: Use surrogate keys for primary keys
```

**Foreign Keys:**

```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    customer_id BIGINT NOT NULL,
    
    FOREIGN KEY (customer_id) 
        REFERENCES customers(id) 
        ON DELETE RESTRICT      -- Prevent deletion if orders exist
        ON UPDATE CASCADE       -- Update orders if customer ID changes
);

-- Cascade options:
-- CASCADE: Delete/update child rows automatically
-- RESTRICT: Prevent if child rows exist (default)
-- SET NULL: Set foreign key to NULL
-- SET DEFAULT: Set foreign key to default value
-- NO ACTION: Similar to RESTRICT
```

**Unique Constraints:**

```sql
-- Single column
ALTER TABLE customers ADD CONSTRAINT unique_email UNIQUE (email);

-- Multiple columns
ALTER TABLE products ADD CONSTRAINT unique_sku_per_vendor 
    UNIQUE (vendor_id, sku);
```

**Check Constraints:**

```sql
-- Simple validation
ALTER TABLE products ADD CONSTRAINT price_positive 
    CHECK (price > 0);

-- Complex validation
ALTER TABLE orders ADD CONSTRAINT valid_order 
    CHECK (
        (status = 'cancelled' AND cancelled_at IS NOT NULL)
        OR (status != 'cancelled' AND cancelled_at IS NULL)
    );

-- Date range validation
ALTER TABLE promotions ADD CONSTRAINT valid_date_range 
    CHECK (end_date > start_date);
```

**Default Values:**

```sql
CREATE TABLE orders (
    id BIGSERIAL PRIMARY KEY,
    status VARCHAR(20) DEFAULT 'pending',
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    is_paid BOOLEAN DEFAULT false
);
```
