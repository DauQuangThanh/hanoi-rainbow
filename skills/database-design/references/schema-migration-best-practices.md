# Schema Migration Best Practices


```sql
-- Always use transactions
BEGIN;

-- 1. Additive changes (safe)
ALTER TABLE customers ADD COLUMN middle_name VARCHAR(100);

-- 2. Create indexes concurrently (PostgreSQL)
CREATE INDEX CONCURRENTLY idx_orders_date ON orders(order_date);

-- 3. Make nullable first, then add NOT NULL
ALTER TABLE products ADD COLUMN brand_id BIGINT;
-- Populate data
UPDATE products SET brand_id = 1 WHERE brand_id IS NULL;
-- Add constraint
ALTER TABLE products ALTER COLUMN brand_id SET NOT NULL;

-- 4. Rename columns/tables
ALTER TABLE customers RENAME COLUMN phone_number TO phone;

-- 5. Drop columns (last resort)
-- First make sure no code references it
ALTER TABLE customers DROP COLUMN old_field;

COMMIT;
```
