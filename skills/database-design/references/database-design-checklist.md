# Database Design Checklist


```markdown
Design Phase:
- [ ] Requirements gathered and documented
- [ ] Entities and attributes identified
- [ ] Relationships defined with cardinality
- [ ] ERD created and reviewed
- [ ] Normalization applied (at least 3NF)
- [ ] Denormalization justified where needed

Implementation Phase:
- [ ] Tables created with appropriate data types
- [ ] Primary keys defined
- [ ] Foreign keys with referential actions
- [ ] Unique constraints added
- [ ] Check constraints for data validation
- [ ] Default values set where appropriate
- [ ] Indexes created for common queries
- [ ] Partitioning strategy implemented if needed

Testing Phase:
- [ ] Sample data loaded
- [ ] Common queries tested and optimized
- [ ] EXPLAIN ANALYZE run on critical queries
- [ ] Constraint violations tested
- [ ] Concurrent access tested
- [ ] Backup and restore tested

Documentation:
- [ ] ERD documented
- [ ] Table relationships documented
- [ ] Index strategy documented
- [ ] Migration scripts version controlled
- [ ] Sample queries documented
```
