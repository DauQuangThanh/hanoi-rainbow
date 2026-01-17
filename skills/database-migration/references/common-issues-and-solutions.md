# Common Issues and Solutions


```markdown
Issue: Replication Lag Too High
Solution:
- Increase network bandwidth
- Optimize source DB performance
- Reduce concurrent writes during migration
- Use parallel replication streams

Issue: Data Type Conversion Errors
Solution:
- Review and fix schema mapping
- Handle NULL values appropriately
- Cast incompatible types explicitly
- Test with sample data first

Issue: Foreign Key Constraint Violations
Solution:
- Disable constraints during load
- Migrate in dependency order
- Load parent tables before child tables
- Re-enable and validate constraints after

Issue: Performance Worse on Target
Solution:
- Analyze query plans
- Create missing indexes
- Update database statistics
- Tune database parameters
- Consider partitioning
```
