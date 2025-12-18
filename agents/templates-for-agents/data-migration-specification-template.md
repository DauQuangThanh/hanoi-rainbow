# Data Migration Specification: [Feature Name]

**Migration**: [Source] → [Target]
**Date**: [YYYY-MM-DD]

## 1. Executive Summary

**Source**: [DB2/IMS/VSAM on z/OS]
**Target**: [PostgreSQL on AWS RDS]
**Data Volume**: [X TB]
**Timeline**: [Weeks/Months]

## 2. Source System Analysis

### 2.1 Data Structures

| Structure | Type | Size | Records |
|-----------|------|------|---------|
| [Table/File] | [Relational/Hierarchical/Sequential] | [GB] | [Millions] |

### 2.2 Data Quality Assessment

| Issue | Severity | Prevalence | Impact |
|-------|----------|------------|--------|
| [Missing values] | [High] | [5%] | [High] |

## 3. Target System Design

### 3.1 Target Schema

[Normalized relational schema or NoSQL design]

### 3.2 Technology Selection

- **Database**: [PostgreSQL/MongoDB/DynamoDB]
- **Rationale**: [Why chosen]

## 4. Data Mapping

### 4.1 Source-to-Target Mapping

| Source Field | Source Type | Target Field | Target Type | Transformation |
|--------------|-------------|--------------|-------------|----------------|
| [CUST-ID] | [PIC 9(8)] | [customer_id] | [BIGINT] | [Direct map] |

### 4.2 Transformation Rules

| Rule ID | Transformation Logic | Example |
|---------|---------------------|---------|
| T-001 | [EBCDIC → UTF-8] | [Convert encoding] |

## 5. Migration Approach

### 5.1 Strategy

- **Type**: [One-time/Incremental/Hybrid]
- **Cutover**: [Big Bang/Phased/Rolling]
- **Synchronization**: [Dual-write/CDC/Batch]

### 5.2 ETL/ELT Pipeline

```mermaid
flowchart LR
    A[Extract] --> B[Transform]
    B --> C[Load]
    C --> D[Validate]
```

[Document pipeline architecture]

## 6. Testing & Validation

### 6.1 Validation Procedures

- **Row Count**: Compare source vs target
- **Checksum**: Validate data integrity
- **Sample Verification**: Manual spot checks

### 6.2 Performance Validation

- **Load Time**: [Target < X hours]
- **Query Performance**: [Target < Y ms]

## 7. Rollback Plan

### 7.1 Rollback Triggers

- [Data loss detected]
- [Performance degradation > 50%]

### 7.2 Rollback Procedures

1. [Stop ETL pipeline]
2. [Restore from backup]
3. [Switch back to legacy]
