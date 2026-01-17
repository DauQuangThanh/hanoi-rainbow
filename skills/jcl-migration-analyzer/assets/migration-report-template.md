# JCL Job Migration Report

**Program**: {{PROGRAM_NAME}}  
**Migration Date**: {{MIGRATION_DATE}}  
**Analyst**: {{ANALYST_NAME}}  
**Status**: {{STATUS}}

---

## 1. Executive Summary

### Program Overview
- **Purpose**: {{PROGRAM_PURPOSE}}
- **Type**: {{PROGRAM_TYPE}} (Batch/Online/Utility)
- **Lines of Code**: {{LOC}}
- **Complexity**: {{COMPLEXITY_LEVEL}}
- **Estimated Effort**: {{EFFORT_DAYS}} person-days

### Migration Recommendation
{{RECOMMENDATION}}

---

## 2. Source Analysis

### Program Structure

#### Divisions
- IDENTIFICATION DIVISION: Line {{ID_DIV_LINE}}
- ENVIRONMENT DIVISION: Line {{ENV_DIV_LINE}}
- DATA DIVISION: Line {{DATA_DIV_LINE}}
- PROCEDURE DIVISION: Line {{PROC_DIV_LINE}}

#### Key Data Structures
| Structure | Type | Lines | Description |
|-----------|------|-------|-------------|
{{DATA_STRUCTURES_TABLE}}

#### Copybooks Used
| Copybook | Purpose | Lines |
|----------|---------|-------|
{{COPYBOOKS_TABLE}}

### Business Logic Summary
{{BUSINESS_LOGIC_SUMMARY}}

### Control Flow
{{CONTROL_FLOW_DESCRIPTION}}

---

## 3. Dependencies

### Program Calls
| Called Program | Purpose | Frequency |
|----------------|---------|-----------|
{{PROGRAM_CALLS_TABLE}}

### File Operations
| File Name | Access Mode | Operations |
|-----------|-------------|------------|
{{FILE_OPERATIONS_TABLE}}

### Database Operations
| Table | Operations | Estimated Rows |
|-------|------------|----------------|
{{DATABASE_OPERATIONS_TABLE}}

### Dependency Graph
```
{{DEPENDENCY_GRAPH}}
```

---

## 4. Java Design

### Proposed Architecture

#### Package Structure
```
{{PACKAGE_STRUCTURE}}
```

#### Class Design
| Java Class | Purpose | COBOL Equivalent |
|------------|---------|------------------|
{{CLASS_DESIGN_TABLE}}

#### Method Signatures
```java
{{METHOD_SIGNATURES}}
```

### Data Model
{{DATA_MODEL_DESCRIPTION}}

### Service Layer
{{SERVICE_LAYER_DESCRIPTION}}

---

## 5. Complexity Assessment

### Metrics
- **Cyclomatic Complexity**: {{CYCLOMATIC_COMPLEXITY}}
- **Number of Paragraphs**: {{PARAGRAPH_COUNT}}
- **External Dependencies**: {{DEPENDENCY_COUNT}}
- **File Operations**: {{FILE_OP_COUNT}}
- **Database Operations**: {{DB_OP_COUNT}}
- **Complexity Score**: {{COMPLEXITY_SCORE}}

### Risk Factors
{{RISK_FACTORS_LIST}}

### Technical Challenges
1. {{CHALLENGE_1}}
2. {{CHALLENGE_2}}
3. {{CHALLENGE_3}}

---

## 6. Migration Strategy

### Approach
{{MIGRATION_APPROACH}}

### Phase 1: Preparation ({{PHASE1_DAYS}} days)
- [ ] {{PREP_TASK_1}}
- [ ] {{PREP_TASK_2}}
- [ ] {{PREP_TASK_3}}

### Phase 2: Implementation ({{PHASE2_DAYS}} days)
- [ ] {{IMPL_TASK_1}}
- [ ] {{IMPL_TASK_2}}
- [ ] {{IMPL_TASK_3}}

### Phase 3: Testing ({{PHASE3_DAYS}} days)
- [ ] {{TEST_TASK_1}}
- [ ] {{TEST_TASK_2}}
- [ ] {{TEST_TASK_3}}

### Phase 4: Deployment ({{PHASE4_DAYS}} days)
- [ ] {{DEPLOY_TASK_1}}
- [ ] {{DEPLOY_TASK_2}}
- [ ] {{DEPLOY_TASK_3}}

---

## 7. Testing Plan

### Unit Testing
{{UNIT_TEST_PLAN}}

### Integration Testing
{{INTEGRATION_TEST_PLAN}}

### Parallel Run Testing
{{PARALLEL_RUN_PLAN}}

### Performance Testing
{{PERFORMANCE_TEST_PLAN}}

---

## 8. Data Migration

### Data Structures to Migrate
{{DATA_MIGRATION_STRUCTURES}}

### Migration Scripts Required
- [ ] {{MIGRATION_SCRIPT_1}}
- [ ] {{MIGRATION_SCRIPT_2}}
- [ ] {{MIGRATION_SCRIPT_3}}

### Validation Approach
{{VALIDATION_APPROACH}}

---

## 9. Implementation Notes

### Special Considerations
{{SPECIAL_CONSIDERATIONS}}

### Code Patterns
{{CODE_PATTERNS}}

### Performance Optimization
{{PERFORMANCE_NOTES}}

---

## 10. Timeline and Resources

### Schedule
| Phase | Start Date | End Date | Duration |
|-------|------------|----------|----------|
| Preparation | {{P1_START}} | {{P1_END}} | {{P1_DURATION}} |
| Implementation | {{P2_START}} | {{P2_END}} | {{P2_DURATION}} |
| Testing | {{P3_START}} | {{P3_END}} | {{P3_DURATION}} |
| Deployment | {{P4_START}} | {{P4_END}} | {{P4_DURATION}} |

### Resource Requirements
- **Java Developers**: {{JAVA_DEV_COUNT}}
- **COBOL Analysts**: {{COBOL_ANALYST_COUNT}}
- **QA Engineers**: {{QA_COUNT}}
- **DevOps**: {{DEVOPS_COUNT}}

### Dependencies
- [ ] {{EXTERNAL_DEPENDENCY_1}}
- [ ] {{EXTERNAL_DEPENDENCY_2}}
- [ ] {{EXTERNAL_DEPENDENCY_3}}

---

## 11. Success Criteria

### Functional
- [ ] All business logic correctly migrated
- [ ] 100% test coverage for critical paths
- [ ] Parallel run matches COBOL output (99.9%+)

### Non-Functional
- [ ] Performance meets or exceeds COBOL
- [ ] Response time < {{RESPONSE_TIME_TARGET}}ms
- [ ] Throughput >= {{THROUGHPUT_TARGET}} TPS

### Quality
- [ ] Code review completed
- [ ] Documentation complete
- [ ] Knowledge transfer completed

---

## 12. Appendices

### A. COBOL Source Reference
{{COBOL_SOURCE_EXCERPTS}}

### B. Generated Java Code Samples
{{JAVA_CODE_SAMPLES}}

### C. Test Cases
{{TEST_CASES}}

### D. References
- COBOL Program: `{{COBOL_FILE_PATH}}`
- Design Document: `{{DESIGN_DOC_PATH}}`
- Test Data: `{{TEST_DATA_PATH}}`

---

**Report Generated**: {{REPORT_DATE}}  
**Tool**: COBOL Migration Analyzer Agent Skill v1.0.0
