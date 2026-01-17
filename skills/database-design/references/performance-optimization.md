# Performance Optimization


### Query Optimization

```sql
-- Use EXPLAIN to analyze queries
EXPLAIN ANALYZE
SELECT c.name, o.order_date, o.total_amount
FROM customers c
JOIN orders o ON c.id = o.customer_id
WHERE o.order_date > '2024-01-01'
  AND c.is_active = true;

-- Optimization techniques:
-- 1. Add indexes on join columns and WHERE conditions
-- 2. Select only needed columns (avoid SELECT *)
-- 3. Use WHERE instead of HAVING when possible
-- 4. Use EXISTS instead of IN for subqueries
-- 5. Avoid functions on indexed columns in WHERE
```

### Partitioning

```sql
-- Range partitioning by date (PostgreSQL)
CREATE TABLE orders (
    id BIGSERIAL,
    order_date DATE NOT NULL,
    customer_id BIGINT,
    total_amount DECIMAL(10, 2)
) PARTITION BY RANGE (order_date);

CREATE TABLE orders_2024_q1 PARTITION OF orders
    FOR VALUES FROM ('2024-01-01') TO ('2024-04-01');

CREATE TABLE orders_2024_q2 PARTITION OF orders
    FOR VALUES FROM ('2024-04-01') TO ('2024-07-01');

-- List partitioning by region
CREATE TABLE customers (
    id BIGSERIAL,
    region VARCHAR(20),
    name VARCHAR(100)
) PARTITION BY LIST (region);

CREATE TABLE customers_us PARTITION OF customers
    FOR VALUES IN ('US', 'CA');

CREATE TABLE customers_eu PARTITION OF customers
    FOR VALUES IN ('UK', 'DE', 'FR');
```
