# Legacy System Assessment: [System Name]

**Prepared By**: [Name]
**Date Created**: [YYYY-MM-DD]
**Last Updated**: [YYYY-MM-DD]
**Version**: [1.0]

---

## Executive Summary

[Provide brief overview of assessment findings, complexity score, and modernization recommendation]

**System**: [Name]
**Platform**: [IBM z/OS, IBM i, Unix, etc.]
**Complexity Score**: [0-100] - [Low/Medium/High/Very High]
**Recommended Approach**: [Rehost/Replatform/Refactor/Replace/Hybrid]

---

## 1. System Inventory

### 1.1 Hardware/Software Configuration

- **Platform**: [Mainframe model, OS version]
- **MIPS/CPU**: [Processing capacity]
- **Memory**: [RAM]
- **Storage**: [Total storage]

### 1.2 Programming Languages & Versions

| Language | Version | LOC | Program Count | Notes |
|----------|---------|-----|---------------|-------|
| COBOL | [Version] | [Lines] | [Count] | [Business logic] |
| JCL | N/A | [Lines] | [Count] | [Batch jobs] |
| Assembler | [Version] | [Lines] | [Count] | [Low-level code] |

### 1.3 Databases & File Systems

| Type | Technology | Version | Size | Growth Rate |
|------|------------|---------|------|-------------|
| Relational | DB2 | [Version] | [TB] | [% per year] |
| Hierarchical | IMS | [Version] | [GB] | [% per year] |
| Sequential | VSAM | N/A | [GB] | [% per year] |

### 1.4 Integration Points

| Integration | Type | Protocol | Frequency | Critical |
|-------------|------|----------|-----------|----------|
| [System Name] | [API/File/MQ] | [Protocol] | [Daily/etc] | [Yes/No] |

---

## 2. Code Analysis

### 2.1 Complexity Metrics

- **Total LOC**: [Count]
- **Average Cyclomatic Complexity**: [Score]
- **Maximum Nesting Depth**: [Levels]
- **Dead Code %**: [Percentage]
- **Code Duplication %**: [Percentage]

### 2.2 Call Graph Analysis

[Describe program dependencies, tight coupling, circular dependencies]

### 2.3 Technical Debt Inventory

| Debt Item | Impact | Effort to Fix | Priority |
|-----------|--------|---------------|----------|
| [Hard-coded values] | [High/Med/Low] | [Person-days] | [High/Med/Low] |

---

## 3. Business Logic Catalog

### 3.1 Business Capabilities

| Capability | Programs | Criticality | Usage Frequency |
|------------|----------|-------------|-----------------|
| [Loan Processing] | [LN001-LN050] | [Critical] | [Daily] |

### 3.2 Business Rules Summary

- **Total Rules Identified**: [Count]
- **Validation Rules**: [Count]
- **Calculation Rules**: [Count]
- **Workflow Rules**: [Count]

---

## 4. Data Architecture

### 4.1 Data Models

[Describe database schemas, file layouts, hierarchical structures]

### 4.2 Data Volumes

| Dataset | Current Size | Growth Rate | Retention |
|---------|-------------|-------------|-----------|
| [Name] | [Size] | [% per year] | [Years] |

### 4.3 Data Quality Issues

| Issue | Severity | Prevalence | Impact |
|-------|----------|------------|--------|
| [Missing values] | [High] | [10%] | [High] |

---

## 5. Integration Landscape

### 5.1 External Systems

[Document all external system integrations]

### 5.2 Integration Patterns

- **Synchronous APIs**: [Count]
- **Asynchronous Messaging**: [Count]
- **File-based**: [Count]
- **Batch**: [Count]

---

## 6. Non-Functional Characteristics

### 6.1 Performance Baselines

- **Transaction Volume**: [TPS]
- **Response Time**: [P50/P95/P99]
- **Batch Runtime**: [Hours]

### 6.2 Availability Requirements

- **Uptime SLA**: [99.9%]
- **Downtime Window**: [Hours/week]

### 6.3 Security & Compliance

- **Regulatory**: [SOX, GDPR, HIPAA, etc.]
- **Audit Requirements**: [Describe]
- **Data Classification**: [Sensitive/Confidential/Public]

---

## 7. Modernization Readiness

### 7.1 Complexity Score Calculation

```
Complexity Score = (Code × 0.40) + (Data × 0.25) + (Integration × 0.20) + (Criticality × 0.15)

Where:
- Code Complexity: [0-100 based on LOC, cyclomatic complexity, dependencies]
- Data Complexity: [0-100 based on model complexity, data quality]
- Integration Complexity: [0-100 based on integration count, protocol diversity]
- Business Criticality: [0-100 based on uptime, transaction volume]

= ([Score] × 0.40) + ([Score] × 0.25) + ([Score] × 0.20) + ([Score] × 0.15)
= [Final Score]
```

**Interpretation**: [Low/Medium/High/Very High Complexity]

### 7.2 Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| [Knowledge loss] | [High] | [High] | [Document, train] |

### 7.3 Modernization Approach Recommendation

**Recommended Approach**: [Rehost/Replatform/Refactor/Replace/Hybrid]

**Rationale**: [Explain why this approach is best given complexity, timeline, budget, business value]

**Alternative Approaches Considered**:
- [Approach 2]: [Why not chosen]
- [Approach 3]: [Why not chosen]

### 7.4 Effort Estimate

- **Timeline**: [Months]
- **Team Size**: [FTEs]
- **Person-Months**: [Estimate]
- **Budget Range**: [$X - $Y]

---

## 8. Recommendations

### 8.1 Immediate Actions

1. [Action 1]
2. [Action 2]

### 8.2 Next Steps

1. [Step 1]
2. [Step 2]

### 8.3 Success Factors

- [Factor 1]
- [Factor 2]

---

## Appendices

### Appendix A: Program Inventory
[Link or attach detailed program list]

### Appendix B: Detailed Metrics
[Link or attach detailed metrics]

### Appendix C: Code Samples
[Include representative code samples]

---

**Assessment Completed By**: [Name]
**Review Date**: [YYYY-MM-DD]
**Approved By**: [Name]
