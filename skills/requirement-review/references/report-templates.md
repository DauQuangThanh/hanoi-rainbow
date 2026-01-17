# Review Report Templates

Templates for generating comprehensive requirements review reports.

## Table of Contents
- [Full Review Report Template](#full-review-report-template)
- [Quick Review Summary Template](#quick-review-summary-template)
- [Issue Tracking Template](#issue-tracking-template)

## Full Review Report Template

```markdown
# Requirements Review Report

**Document**: [Document name and version number]
**Review Date**: [Date performed]
**Reviewers**: [Names and roles of review team]
**Review Scope**: [What was included in this review]
**Review Type**: [Initial / Update / Quality Gate / Pre-Design]

---

## Executive Summary

### Overall Assessment
- **Overall Quality Rating**: [Excellent / Good / Fair / Poor]
- **Recommendation**: [Approve / Approve with Conditions / Major Revision Needed / Reject]

**Key Strengths** (2-3 bullet points):
- ✅ [Strength 1 - be specific]
- ✅ [Strength 2 - be specific]
- ✅ [Strength 3 - be specific]

**Critical Issues** (2-3 bullet points):
- ⛔ [Critical issue 1 - be specific]
- ⛔ [Critical issue 2 - be specific]
- ⛔ [Critical issue 3 - be specific]

**Top Recommended Actions** (3-5 priority items):
1. [Action 1 - specific and actionable]
2. [Action 2 - specific and actionable]
3. [Action 3 - specific and actionable]
4. [Action 4 - specific and actionable]
5. [Action 5 - specific and actionable]

### Review Metrics
- **Total Requirements Reviewed**: [number]
- **Issues Found**:
  - Critical: [number]
  - Major: [number]
  - Minor: [number]
  - Total: [number]
- **Quality Scores**:
  - Completeness: [percentage]%
  - Clarity: [percentage]%
  - Consistency: [percentage]%
  - Testability: [percentage]%
  - Traceability: [percentage]%
  - **Overall Quality**: [percentage]% ([Excellent/Good/Fair/Poor])

---

## Detailed Findings

### 1. Completeness Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Strengths**:
- ✅ [Specific strength example with requirement IDs if applicable]
- ✅ [Specific strength example]

**Issues Found**:

#### Critical Issues
- ⛔ **[Issue ID e.g., COMP-001]**: [Issue description]
  - **Location**: [Section/Requirement ID]
  - **Impact**: [Explanation of business/technical impact]
  - **Recommendation**: [Specific fix needed]

#### Major Issues
- ⚠️ **[Issue ID]**: [Issue description]
  - **Location**: [Section/Requirement ID]
  - **Impact**: [Impact explanation]
  - **Recommendation**: [Specific fix]

#### Missing Requirements
- 📋 **[Gap ID]**: [Description of what's missing]
  - **Area**: [Module/Feature area]
  - **Impact**: [Business/technical impact if not addressed]
  - **Recommendation**: [What needs to be added]

### 2. Clarity & Understandability Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Strengths**:
- ✅ [Example of well-written requirements]

**Issues Found**:

#### Ambiguous Requirements
- ⚠️ **[REQ-ID]**: "[Original ambiguous requirement text]"
  - **Issue**: [Why it's ambiguous - multiple interpretations possible]
  - **Interpretation A**: [Possible meaning 1]
  - **Interpretation B**: [Possible meaning 2]
  - **Recommendation**: "[Suggested clearer requirement text]"

- ⚠️ **[REQ-ID]**: "[Another ambiguous requirement]"
  - **Issue**: [Explanation]
  - **Recommendation**: "[Clearer version]"

#### Vague Terms Found
Requirements using unclear language:
- "User-friendly" appears in: [REQ-023, REQ-034, REQ-056]
- "Fast performance" in: [REQ-045, REQ-078]
- "Easy to use" in: [REQ-067, REQ-089, REQ-091]
- "Intuitive" in: [REQ-102, REQ-115]

**Recommendation**: Replace with specific, measurable criteria. Examples:
- "User-friendly" → "New users complete first task in < 5 minutes without help"
- "Fast" → "Response time < 2 seconds for 95% of requests"

### 3. Consistency & Conflict Analysis

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Issues Found**:

#### Conflicting Requirements
- 🔴 **Conflict #1**: Requirements [REQ-012] vs [REQ-089]
  - **REQ-012**: "[Full requirement text]"
  - **REQ-089**: "[Conflicting requirement text]"
  - **Conflict Nature**: [Explanation of the conflict]
  - **Business Impact**: [Impact if not resolved]
  - **Recommended Resolution**: [Specific resolution approach]

- 🔴 **Conflict #2**: [Similar structure for other conflicts]

#### Terminology Inconsistencies
| Inconsistent Terms Used | Occurrences | Recommended Standard Term |
|-------------------------|-------------|---------------------------|
| User / Customer / Client / End-user | 45 / 23 / 12 / 8 | "User" (most common) |
| Login / Sign-in / Log-in / Sign in | 15 / 8 / 7 / 4 | "Login" (matches system) |
| Order / Purchase / Transaction | 34 / 12 / 8 | "Order" (business term) |

**Recommendation**: Create glossary defining standard terms and update all requirements.

#### Priority Conflicts
- [REQ-045] marked Critical but depends on [REQ-078] marked Medium
- Too many requirements (35 of 120) marked as "Critical" priority
- [REQ-089] and [REQ-091] both marked Priority 1 but mutually exclusive

### 4. Testability Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Issues Found**:

#### Untestable Requirements
- ❌ **[REQ-034]**: "The system shall be secure"
  - **Issue**: No measurable criteria for determining if "secure"
  - **Recommendation**: Break into testable requirements:
    - "System shall enforce password complexity (8+ chars, 1 uppercase, 1 number, 1 special char)"
    - "System shall lock accounts after 5 failed login attempts within 15 minutes"
    - "System shall encrypt all data in transit using TLS 1.3"
    - "System shall encrypt sensitive PII at rest using AES-256"
    - "System shall log all authentication attempts with timestamp and IP"

- ❌ **[REQ-067]**: "Application shall be maintainable"
  - **Issue**: Subjective, no objective pass/fail
  - **Recommendation**:
    - "Code shall maintain 80% unit test coverage"
    - "Cyclomatic complexity shall not exceed 10 per function"
    - "All public APIs shall have JSDoc/XML documentation"
    - "No code duplication blocks exceeding 10 lines"

#### Missing Acceptance Criteria
User stories lacking acceptance criteria:
- [US-045]: "As a user, I want to search products..."
- [US-056]: "As a customer, I want to track my order..."
- [US-078]: "As an admin, I want to manage users..."
- [US-089]: "As a user, I want to save favorites..."

**Total**: 23 user stories (of 87) lack acceptance criteria

**Recommendation**: Add Given-When-Then acceptance criteria for all stories before sprint planning.

#### Requirements Without Verification Method
- [REQ-023, REQ-034, REQ-056, REQ-078, REQ-091]: No test approach defined
- No verification method specified for non-functional requirements in Section 4.3

### 5. Traceability Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Issues Found**:

#### Missing Backward Traceability
**Orphan Requirements** (no business justification):
- [REQ-145]: Real-time dashboard feature (Who requested? Why needed?)
- [REQ-167]: Advanced reporting module (Business value unclear)
- [REQ-189]: Mobile app support (Not in original scope?)

**Recommendation**: Link each requirement to:
- Business objective or goal
- Stakeholder who requested it
- Business case element (ROI, cost savings, revenue)

#### Missing Forward Traceability
**Requirements Without Design Coverage**: [45 requirements]
- Section 3.2 (User Management): No architecture/design documented
- Section 3.5 (Reporting): No UI mockups or data model

**Requirements Without Test Cases**: [78 requirements]
- No test cases mapped for functional requirements in Sections 3.1-3.4
- Non-functional requirements (Section 4) have no test strategy

**Untraced Business Needs**:
Business objectives mentioned in charter but not in requirements:
- "Support mobile access for field workers" - No mobile requirements
- "Integrate with payment gateway" - No payment processing requirements
- "Provide analytics dashboard" - Basic reporting only, no analytics

**Recommendation**: Create or update Requirements Traceability Matrix linking:
- Business Need → Requirement → Design → Implementation → Test Case

### 6. Standards Compliance Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

**Issues Found**:

#### IEEE 830 Compliance (if applicable)
- ❌ Missing: Glossary of terms (Section 1.4)
- ❌ Missing: Assumptions and dependencies documented
- ⚠️ Incomplete: Version control information (no change history)
- ⚠️ Incomplete: Only 60% of requirements have verification methods

#### Industry Standards (if applicable)
- [List specific standard] requirement [X] not met: [explanation]
- [Standard] compliance requirement missing: [what's needed]

#### Agile Best Practices (if applicable)
- 23 user stories do not follow INVEST criteria (missing testability)
- Definition of Done not documented
- Stories not properly sized (12 stories estimated > 13 points)
- Epic decomposition incomplete for [Epic-005] and [Epic-007]

### 7. Feasibility & Risk Assessment

**Rating**: [Excellent / Good / Fair / Poor] ([score]%)

#### High-Risk Requirements

| Req ID | Requirement Summary | Risk Category | Risk Level | Impact | Mitigation Needed |
|--------|-------------------|---------------|------------|--------|-------------------|
| REQ-045 | Real-time sync across 100K concurrent users | Technical complexity, Performance | High | Project timeline, Budget | POC/spike required, architecture review |
| REQ-067 | AI-based product recommendations | Unproven technology, Resource | Medium | Feature delivery | Evaluate ML platforms, skill assessment |
| REQ-089 | Sub-second global search | Performance, Infrastructure | High | User satisfaction | Load testing, CDN strategy |

**Recommendations**:
- **REQ-045**: Conduct proof-of-concept for real-time architecture before committing
- **REQ-067**: Evaluate 3rd-party ML services vs build, assess team ML skills
- **REQ-089**: Performance testing with realistic data volumes, consider search service

#### Feasibility Concerns
- **Technical**: [List technical feasibility concerns]
- **Resource**: [List resource/skill availability concerns]
- **Timeline**: [List schedule risk concerns]
- **Budget**: [List cost concerns]

---

## Quality Metrics Summary

### Quality Scores by Attribute

| Quality Attribute | Score | Target | Status | Gap |
|-------------------|-------|--------|--------|-----|
| Completeness | 75% | 95% | ⚠️ Below | -20% |
| Clarity | 82% | 90% | ⚠️ Below | -8% |
| Consistency | 70% | 95% | ❌ Poor | -25% |
| Testability | 65% | 90% | ❌ Poor | -25% |
| Traceability | 68% | 95% | ❌ Poor | -27% |
| Feasibility | 85% | 90% | ⚠️ Below | -5% |
| **Overall Quality** | **74%** | **90%** | **⚠️ Below Target** | **-16%** |

### Issue Distribution

| Severity | Count | Percentage | Examples |
|----------|-------|------------|----------|
| Critical | 5 | 4% | Missing security requirements, fundamental scope conflicts |
| Major | 18 | 15% | Ambiguous performance criteria, missing acceptance criteria, terminology inconsistencies |
| Minor | 32 | 27% | Formatting inconsistencies, minor typos, diagram improvements |
| **Total Issues** | **55** | **46% of requirements** | |

### Requirements Coverage

| Category | Count | Percentage | Status |
|----------|-------|------------|--------|
| Requirements Reviewed | 120 | 100% | |
| Requirements with Issues | 55 | 46% | ⚠️ High |
| Requirements without Issues | 65 | 54% | ✅ Good |
| High-Risk Requirements | 8 | 7% | ⚠️ Needs attention |
| Untestable Requirements | 23 | 19% | ❌ Critical gap |
| Orphan Requirements (no traceability) | 15 | 13% | ⚠️ Needs justification |

---

## Recommendations

### Critical (Must Fix Before Approval)

1. **Add Missing Security Requirements (REQ-SEC-*)**
   - **Priority**: Critical
   - **Effort**: 2-3 days
   - **Actions**:
     - Define authentication mechanisms (OAuth2, JWT)
     - Specify authorization rules (RBAC model)
     - Document data protection requirements (encryption at rest/transit)
     - Define audit logging requirements (what, when, retention)
     - Specify session management (timeout, concurrent sessions)
   - **Owner**: [Security Architect + Business Analyst]
   - **Due**: [Date]

2. **Resolve Requirement Conflicts (Conflicts #1, #3, #5)**
   - **Priority**: Critical
   - **Effort**: 1 week
   - **Actions**:
     - Conflict #1 (REQ-012 vs REQ-089): Clarify whether phone number is mandatory
     - Conflict #3 (REQ-034 vs REQ-078): Align on real-time vs batch processing
     - Conflict #5 (REQ-091 vs REQ-102): Resolve mobile-first vs desktop-first priority
   - **Owner**: [Product Owner + Business Analyst]
   - **Due**: [Date]

3. **Add Acceptance Criteria to All User Stories (23 stories)**
   - **Priority**: Critical
   - **Effort**: 1-2 weeks
   - **Actions**:
     - Use Given-When-Then format for each story
     - Include positive scenarios (happy path)
     - Include negative scenarios (error cases)
     - Include edge cases
     - Define clear pass/fail criteria
   - **Owner**: [Business Analyst + Development Team]
   - **Due**: [Date]

### Major (Should Fix for Quality)

4. **Improve Clarity of Performance Requirements**
   - **Priority**: Major
   - **Effort**: 3-5 days
   - **Actions**:
     - Replace vague terms ("fast", "quick", "responsive") with specific targets
     - Specify response time targets with percentiles (e.g., "< 2s for 95% of requests")
     - Define load/concurrency requirements (concurrent users, transactions/second)
     - Document performance measurement approach
   - **Owner**: [Business Analyst + Performance Engineer]
   - **Due**: [Date]

5. **Enhance Requirements Traceability**
   - **Priority**: Major
   - **Effort**: 1 week
   - **Actions**:
     - Link all requirements to business objectives/goals
     - Create comprehensive Requirements Traceability Matrix
     - Map requirements to design elements
     - Map requirements to test cases
     - Document requirement sources (stakeholder, regulation, etc.)
   - **Owner**: [Business Analyst]
   - **Due**: [Date]

6. **Standardize Terminology**
   - **Priority**: Major
   - **Effort**: 2-3 days
   - **Actions**:
     - Create glossary of terms with definitions
     - Define standard terms for identified inconsistencies
     - Update all requirements to use standard terminology
     - Distribute glossary to all stakeholders
   - **Owner**: [Business Analyst]
   - **Due**: [Date]

### Minor (Nice to Have)

7. **Add More Diagrams for Complex Workflows**
   - **Priority**: Minor
   - **Effort**: 1-2 days
   - **Recommendation**: Add workflow diagrams for user registration, checkout, and order fulfillment processes

8. **Include More Usage Examples**
   - **Priority**: Minor
   - **Effort**: 1 day
   - **Recommendation**: Add concrete examples for complex requirements to improve understanding

9. **Improve Document Formatting Consistency**
   - **Priority**: Minor
   - **Effort**: 0.5 day
   - **Recommendation**: Consistent numbering, heading styles, and table formats throughout

---

## Action Items

| ID | Action | Owner | Priority | Effort | Due Date | Status | Dependencies |
|----|--------|-------|----------|--------|----------|--------|--------------|
| A-001 | Add security requirements section | [Security Architect, BA] | Critical | 2-3 days | [Date] | Open | None |
| A-002 | Resolve conflict: REQ-012 vs REQ-089 | [PO, BA] | Critical | 0.5 day | [Date] | Open | A-001 |
| A-003 | Add acceptance criteria to US-045 through US-078 | [BA, Dev Team] | Critical | 1-2 weeks | [Date] | Open | None |
| A-004 | Create glossary of terms | [BA] | Major | 0.5 day | [Date] | Open | None |
| A-005 | Update Requirements Traceability Matrix | [BA] | Major | 3 days | [Date] | Open | A-004 |
| A-006 | Replace vague performance terms with targets | [BA, Perf Engineer] | Major | 3 days | [Date] | Open | None |
| A-007 | Add workflow diagrams | [BA, UX] | Minor | 1-2 days | [Date] | Backlog | A-005 |

---

## Approval Recommendation

### Decision
**[Approve with Conditions / Major Revision Needed / Reject]**

### Rationale
[Provide 2-3 sentences explaining the recommendation based on findings. Example:]

The requirements show a solid foundation with clear business objectives and well-structured functional requirements (65 of 120 requirements have no issues). However, critical gaps exist in security requirements, requirement conflicts need resolution, and 23 user stories lack acceptance criteria needed for development. The testability and traceability scores (65% and 68%) fall significantly below the 90% target, indicating substantial rework is needed before the requirements can serve as a reliable foundation for design and development.

### Conditions for Approval (if "Approve with Conditions")

1. **All critical issues (A-001 through A-003) must be resolved**
   - Security requirements section added and reviewed
   - All requirement conflicts resolved with stakeholder agreement
   - Acceptance criteria added to all user stories

2. **Major issues should be addressed or have mitigation plans**
   - Performance requirements clarified with specific targets
   - Traceability matrix completed and validated
   - Terminology standardized across all requirements

3. **Updated version to be re-reviewed**
   - Submit updated requirements document by [date]
   - Targeted review of previously identified issues
   - Final approval within 3 business days of submission

### Next Steps

1. **Immediate**: BA team addresses critical issues (A-001 through A-003)
2. **Week 1**: Resolve requirement conflicts with stakeholder meetings
3. **Week 2**: Complete major quality improvements (A-004 through A-006)
4. **Week 3**: Submit updated requirements for targeted re-review
5. **Week 4**: Final approval and handoff to design team

---

## Sign-Off

**Review Team**:
- Reviewer: _________________ Role: _________ Date: _______
- Reviewer: _________________ Role: _________ Date: _______
- Reviewer: _________________ Role: _________ Date: _______

**Approvals** (after conditions met):
- Business Owner: _________________ Date: _______
- Technical Lead: _________________ Date: _______
- Project Manager: _________________ Date: _______

**Document Version Control**:
- Review Report Version: [1.0]
- Requirements Document Version Reviewed: [Version number]
- Next Review Scheduled: [Date] (if conditional approval)

---

**End of Review Report**
```

## Quick Review Summary Template

For rapid reviews or status checks:

```markdown
# Quick Requirements Review Summary

**Document**: [Name] v[Version]
**Review Date**: [Date]
**Reviewer**: [Name]

## Quick Assessment

**Overall**: [🟢 Good / 🟡 Needs Work / 🔴 Major Issues]

**Key Findings**:
- Total Requirements: [number]
- Critical Issues: [number]
- Major Issues: [number]
- Overall Quality: [percentage]%

## Top 3 Issues

1. **[Issue]**: [Brief description] → [Quick fix]
2. **[Issue]**: [Brief description] → [Quick fix]
3. **[Issue]**: [Brief description] → [Quick fix]

## Recommendation

[Approve / Conditional Approve / Revise / Reject]

**Next Action**: [What needs to happen next]
```

## Issue Tracking Template

For tracking individual requirement issues:

```markdown
| Issue ID | Severity | Req ID | Issue Type | Description | Recommendation | Owner | Status | Due Date |
|----------|----------|--------|------------|-------------|----------------|-------|--------|----------|
| ISS-001 | Critical | REQ-045 | Ambiguity | "Fast" undefined | Specify: "< 2s for 95%" | [BA] | Open | [Date] |
| ISS-002 | Major | REQ-067 | Untestable | "Intuitive UI" subjective | Define usability metrics | [UX] | Open | [Date] |
| ISS-003 | Critical | REQ-012, REQ-089 | Conflict | Phone number mandatory/optional | Align with stakeholders | [PO] | Open | [Date] |
| ISS-004 | Major | US-045 | Missing AC | No acceptance criteria | Add Given-When-Then | [BA] | In Progress | [Date] |
| ISS-005 | Minor | Section 3.2 | Formatting | Inconsistent numbering | Fix numbering | [BA] | Closed | [Date] |
```
