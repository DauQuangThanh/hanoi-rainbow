# Backend Design Review Report Template

Use this template to structure comprehensive backend design review reports.

---

## 1. Executive Summary

**Project**: [Project name]  
**Review Date**: [Date]  
**Reviewer**: [Name/Team]  
**Review Duration**: [Hours/Days]  
**Design Documents Reviewed**: 
- Mermaid architecture diagrams: [Links]
- API specifications: [Links]
- Database schemas: [Links]
- ADRs: [Links]

**Overall Assessment**: [2-3 sentences summarizing design quality and major concerns]

**Key Metrics**:
- Total findings: [Count]
- Critical severity: [Count]
- High severity: [Count]
- Medium severity: [Count]
- Low severity: [Count]

---

## 2. Review Scope

**Areas Covered**:
- ✅ API Design (REST/GraphQL/gRPC)
- ✅ Database Architecture
- ✅ Microservices Patterns
- ✅ Integration Architecture
- ✅ Security Design
- ✅ Performance & Scalability

**Review Depth**:
- [High-level architectural review / Detailed technical review]
- Focus areas: [List prioritized components]

**Out of Scope**:
- [List what was not reviewed]

**Assumptions & Constraints**:
- [List assumptions made during review]
- [Note constraints: technology, budget, timeline]

---

## 3. Key Findings Summary

### Critical Issues (🔴)
1. [Issue title] - [Brief description]
2. [Issue title] - [Brief description]

### High Priority Issues (🟠)
1. [Issue title] - [Brief description]
2. [Issue title] - [Brief description]

### Summary Statistics
- API Design Issues: [Count by severity]
- Database Design Issues: [Count by severity]
- Security Issues: [Count by severity]
- Performance Issues: [Count by severity]

---

## 4. Detailed Findings

### Finding #1: [Issue Title]

**Severity**: 🔴 Critical / 🟠 High / 🟡 Medium / 🟢 Low

**Category**: API Design / Database / Security / Performance / Architecture

**Description**:
[Detailed description of the issue]

**Current Design**:
```
[Code snippet, diagram, or description of current design]
```

**Impact**:
- [Impact on security / performance / scalability / maintainability]
- [Specific consequences if not addressed]

**Recommendation**:
[Specific, actionable recommendation to fix the issue]

**Proposed Design**:
```
[Code snippet, diagram, or description of recommended design]
```

**Effort Estimate**: [Small / Medium / Large]

**Dependencies**: [Any dependencies for fixing this issue]

---

### Finding #2: [Issue Title]

[Repeat structure for each finding]

---

## 5. Positive Observations

Highlight strengths and good design decisions:

1. **[Strength Title]**
   - [Description of what was done well]
   - [Why this is a good practice]

2. **[Strength Title]**
   - [Description]

---

## 6. Recommendations Summary

### Immediate Actions (Must Fix Before Implementation)
1. **[Recommendation]** - Fix [Critical Issue #X]
   - Effort: [Size]
   - Impact: [High/Medium/Low]
   - Owner: [Team/Person]

### High Priority (Should Fix Before Go-Live)
1. **[Recommendation]** - Address [High Issue #X]
   - Effort: [Size]
   - Impact: [High/Medium/Low]
   - Owner: [Team/Person]

### Medium Priority (Next Iteration)
1. **[Recommendation]** - Improve [Medium Issue #X]
   - Effort: [Size]
   - Impact: [High/Medium/Low]
   - Owner: [Team/Person]

### Future Improvements (Track for Later)
1. **[Recommendation]** - Optimize [Low Issue #X]
   - Effort: [Size]
   - Impact: [High/Medium/Low]

---

## 7. Architecture Diagrams (Mermaid Format)

### Current Architecture
```mermaid
[Mermaid diagram of current design]
```

### Proposed Improvements
```mermaid
[Mermaid diagram with recommended changes highlighted]
```

### Data Flow
```mermaid
sequenceDiagram
    [Sequence diagrams showing improved data flows]
```

---

## 8. Action Items

| ID | Issue | Severity | Recommendation | Owner | Deadline | Status |
|----|-------|----------|----------------|-------|----------|--------|
| 1 | [Issue title] | 🔴 Critical | [Brief action] | [Name] | [Date] | Not Started |
| 2 | [Issue title] | 🟠 High | [Brief action] | [Name] | [Date] | In Progress |
| 3 | [Issue title] | 🟡 Medium | [Brief action] | [Name] | [Date] | Not Started |

---

## 9. Next Steps

### Immediate Actions (This Week)
1. [Action 1] - Priority: Critical
2. [Action 2] - Priority: Critical

### Short-term Actions (Next 2-4 Weeks)
1. [Action 1] - Priority: High
2. [Action 2] - Priority: High
3. [Action 3] - Priority: Medium

### Long-term Improvements (Next Quarter)
1. [Action 1] - Priority: Medium
2. [Action 2] - Priority: Low

### Follow-up Review
- **Schedule**: [Date - recommend 2-4 weeks after initial fixes]
- **Focus Areas**: [List areas to re-review based on changes]
- **Success Criteria**: [Define what "fixed" looks like]

---

## 10. Appendix

### A. References
- [Link to API specification]
- [Link to Mermaid architecture diagrams]
- [Link to database schema]
- [Relevant documentation]

### B. Review Methodology
- [Describe review approach]
- [Tools used]
- [Standards/guidelines referenced]

### C. Glossary
- **Term**: Definition
- **Acronym**: Expansion

---

**Report Version**: 1.0  
**Last Updated**: [Date]  
**Prepared By**: [Name/Team]
