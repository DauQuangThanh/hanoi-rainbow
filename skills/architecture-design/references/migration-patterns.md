# Migration Patterns


When migrating from legacy systems:

### Strangler Fig Pattern
- Gradually replace legacy functionality
- Run old and new systems in parallel
- Route traffic incrementally to new system
- Decommission old components progressively

### Anti-Corruption Layer
- Create abstraction layer between old and new
- Translate between different models
- Protect new system from legacy complexity

### Database Migration Strategies
- **Big Bang**: Complete cutover (high risk, minimal complexity)
- **Trickle Migration**: Gradual data migration (lower risk, higher complexity)
- **Change Data Capture**: Real-time synchronization during migration
