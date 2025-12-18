---
description: Create comprehensive data migration strategy from legacy databases to modern data stores
handoffs:
  - label: Convert Batch Jobs
    agent: rainbow.convert-batch
    prompt: Design modern batch processing based on data migration plan
    send: true
  - label: Consult Data Migration Engineer
    agent: hanoi.data-migration-engineer
    prompt: Get expert guidance on data migration approach
scripts:
  sh: scripts/bash/setup-design-data-migration.sh --json
  ps: scripts/powershell/setup-design-data-migration.ps1 -Json
agent_scripts:
  sh: scripts/bash/update-agent-context.sh __AGENT__
  ps: scripts/powershell/update-agent-context.ps1 -AgentType __AGENT__
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add data migration strategy for feature-name') and commit data-migration.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON output.

2. **Load context**: Read FEATURE_SPEC, LEGACY_ASSESSMENT, BUSINESS_LOGIC_MAP (if exist), `memory/ground-rules.md`.

3. **Execute data migration design**: Follow template structure to:
   - Analyze source data structures (hierarchical, relational, sequential)
   - Design target schema (normalized relational or NoSQL)
   - Create source-to-target mapping
   - Define transformation rules
   - Plan data quality improvements
   - Define cutover strategy
   - Design reconciliation approach

4. **Update agent context**: Run `{AGENT_SCRIPT}`.

5. **Report completion**.

## Phases

### Phase 0: Source Data Analysis

1. **Catalog source data structures**:
   - DB2/IMS/VSAM/IDMS file structures
   - Hierarchical relationships
   - Key structures and indexes
   - Data volumes and growth rates

2. **Assess data quality**:
   - Missing/null values
   - Data inconsistencies
   - Duplicate records
   - Referential integrity issues

**Output**: Source System Analysis section

### Phase 1: Target Schema Design

1. **Design target data model**:
   - Normalize hierarchical structures
   - Define table relationships
   - Choose appropriate data types
   - Plan for scalability

2. **Select target technologies**:
   - Relational (PostgreSQL, SQL Server) vs. NoSQL (MongoDB, DynamoDB)
   - Caching layer (Redis, Memcached)
   - Data warehouse (Snowflake, Redshift) if needed

**Output**: Target System Design section

### Phase 2: Data Mapping & Transformation

1. **Create field-level mappings**:
   - Source field → Target field
   - Data type conversions
   - Format transformations
   - Default values for new fields

2. **Define transformation logic**:
   - Data cleansing rules
   - Business rule transformations
   - Data enrichment logic
   - Null handling strategies

**Output**: Data Mapping and Transformation Rules sections

### Phase 3: Migration Approach & ETL Design

1. **Choose migration strategy**:
   - One-time (big bang) vs. incremental
   - Cutover strategy (weekend, phased, rolling)
   - Data synchronization approach (dual-write, CDC, batch sync)

2. **Design ETL/ELT pipeline**:
   - Extract mechanisms
   - Transform logic
   - Load procedures
   - Error handling
   - Performance optimization

**Output**: Migration Approach and ETL Pipeline sections

### Phase 4: Testing & Validation

1. **Design validation strategy**:
   - Row count reconciliation
   - Checksum validation
   - Sample data verification
   - Referential integrity checks
   - Performance benchmarks

2. **Plan rollback procedures**:
   - Backup strategy
   - Rollback triggers
   - Recovery procedures

**Output**: Testing & Validation and Rollback Plan sections

## Success Criteria

- [ ] Source data structures documented
- [ ] Target schema designed
- [ ] Complete field-level mappings
- [ ] Transformation rules defined
- [ ] ETL/ELT pipeline designed
- [ ] Validation procedures defined
- [ ] Rollback plan documented
