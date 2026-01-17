# Review Report Format

## Output Guidelines

**Be Specific:**
- Reference exact line numbers and file paths
- Quote problematic code snippets
- Provide concrete examples and metrics

**Be Constructive:**
- Explain WHY something is an issue (impact on maintainability, performance, security)
- Suggest HOW to fix it with specific recommendations
- Provide improved code examples showing the solution

**Be Balanced:**
- Acknowledge good practices and strengths
- Prioritize issues appropriately (P0-P3)
- Consider effort vs. benefit in recommendations

**Be Professional:**
- Focus on code quality, not developers
- Use objective criteria and metrics
- Provide educational context where helpful

## Report Structure

### Executive Summary
```
Code Quality Score: [X/100]
Maintainability Index: [X]
Technical Debt Ratio: [X%]
Critical Issues: [N]
Major Issues: [N]
Minor Issues: [N]

Overall Assessment: [Brief summary]
Top Priorities: [Top 3-5 issues to address]
```

### Detailed Findings

For each issue, provide:

```markdown
#### Issue [N]: [Brief Title]

**Severity:** Critical | Major | Minor | Info
**Category:** Code Smell | Complexity | Naming | Duplication | Design | Performance | Security | Documentation
**Location:** [file.ext#LX-LY]

**Description:**
[Clear explanation of the issue]

**Current Code:**
```[language]
[problematic code snippet]
```

**Impact:**
- Maintainability: [impact level]
- Readability: [impact level]
- Performance: [impact level if relevant]
- Risk: [potential problems]

**Recommendation:**
[Specific improvement suggestion]

**Improved Code:**
```[language]
[suggested improvement]
```

**Effort:** Low | Medium | High
**Priority:** P0 | P1 | P2 | P3
```

### Metrics Summary

```markdown
| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Cyclomatic Complexity (avg) | X | <10 | ⚠️ |
| Lines per Method (avg) | X | <50 | ✅ |
| Code Duplication | X% | <5% | ⚠️ |
| Test Coverage | X% | >80% | ❌ |
| Documentation Coverage | X% | >70% | ✅ |
| Technical Debt Ratio | X% | <5% | ⚠️ |
```

### Recommendations by Priority

**P0 - Critical (Fix Immediately):**
1. [Issue with major impact]
2. [Issue with major impact]

**P1 - High (Fix Soon):**
1. [Important issue]
2. [Important issue]

**P2 - Medium (Plan for Next Sprint):**
1. [Moderate issue]
2. [Moderate issue]

**P3 - Low (Technical Debt Backlog):**
1. [Minor improvement]
2. [Minor improvement]

### Positive Observations

Always acknowledge good practices found in the code:

**Examples:**
- ✅ **Excellent Test Coverage:** 87% overall coverage with quality assertions
- ✅ **Clear Naming:** Consistent and descriptive naming throughout
- ✅ **Good Package Structure:** Clean separation of concerns
- ✅ **Effective Logging:** Comprehensive logging at appropriate levels
- ✅ **Modern Language Features:** Good use of modern patterns and idioms
- ✅ **Proper Error Handling:** Comprehensive exception handling and validation
- ✅ **Good Documentation:** Clear comments and API documentation

### Recommendations by Priority

**P0 - Critical (Fix Immediately):**
1. [Issue with major impact on security, correctness, or stability]
2. [Issue causing immediate problems]

**P1 - High (Fix Soon):**
1. [Important maintainability or performance issue]
2. [Significant code quality problem]

**P2 - Medium (Plan for Next Sprint):**
1. [Moderate improvement opportunity]
2. [Technical debt item]

**P3 - Low (Technical Debt Backlog):**
1. [Minor improvement]
2. [Nice-to-have enhancement]

### Technical Debt Summary

```
Total Estimated Effort: [X] person-days
- P0 Issues: [X] days
- P1 Issues: [X] days
- P2 Issues: [X] days
- P3 Issues: [X] days

ROI Analysis:
- Reduced maintenance time: [X] hours/month
- Improved developer productivity: [X]%
- Reduced bug rate: [estimated reduction]
```
