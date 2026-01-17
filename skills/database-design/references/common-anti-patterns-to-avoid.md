# Common Anti-Patterns to Avoid


❌ **Entity-Attribute-Value (EAV)**: Flexible but unqueryable
❌ **Storing Arrays as Strings**: '1,2,3' - use arrays or junction tables
❌ **Premature Optimization**: Don't add indexes until you measure
❌ **UUID as Primary Key**: Can cause index fragmentation (use if needed for distribution)
❌ **Storing Files in Database**: Use object storage (S3, etc.) instead
❌ **Using NULL for Multiple Meanings**: NULL should mean "unknown", not "N/A" or 0
❌ **Over-Normalization**: Don't split tables excessively
❌ **Missing Foreign Keys**: They prevent orphaned records
❌ **No Indexes on Foreign Keys**: Critical for join performance
❌ **Generic Column Names**: Use specific names (user_id, not id)
