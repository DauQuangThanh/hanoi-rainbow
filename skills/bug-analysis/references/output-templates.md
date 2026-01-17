# Bug Report Templates

## Standard Bug Report Template

```markdown
## Bug Report

### Summary
**Bug ID**: [BUG-XXXX]
**Title**: [Concise, descriptive title]
**Reporter**: [Name/Email]
**Date**: [YYYY-MM-DD]
**Severity**: [Critical/High/Medium/Low]
**Priority**: [P0/P1/P2/P3]
**Type**: [Functional/Performance/Security/UI/Data/Integration/Configuration/Regression]
**Status**: [New/Confirmed/In Progress/Fixed/Closed]

### Description
[Clear, detailed description of the bug including what's wrong and why it matters]

### Environment
- **Operating System**: [Windows 10/macOS 13/Ubuntu 22.04/etc.]
- **Browser/Client**: [Chrome 120/Firefox 121/Safari 17/Mobile App 2.1]
- **Application Version**: [v2.3.1]
- **Environment**: [Production/Staging/Development]
- **Configuration**: [Any relevant settings or features enabled]
- **Other**: [Database version, API version, etc.]

### Reproduction Steps
1. [First action]
2. [Second action]
3. [Continue with numbered steps]
4. [Observe the result]

**Expected Behavior**: [What should happen]

**Actual Behavior**: [What actually happens]

**Reproducibility**: [Always (100%) / Intermittent (50%) / Rare (<10%)]

### Supporting Evidence
- **Screenshots**: [Attach or link]
- **Videos**: [Screen recording if helpful]
- **Error Messages**: 
```
[Paste exact error messages or stack traces]
```
- **Logs**:
```
[Relevant log entries with timestamps]
```
- **Network Requests**: [API calls, responses]

### Impact Assessment
**Users Affected**: [Number, percentage, or user segment description]
**Frequency**: [How often does this occur?]
**Business Impact**: [Revenue loss, SLA violation, customer satisfaction, etc.]
**Workaround Available**: [Yes/No - describe if yes]

### Related Information
- **Related Issues**: [Links to similar bugs, duplicates, or related features]
- **Recent Changes**: [Recent deployments, config changes, or updates that might be related]
- **External References**: [Documentation, API specs, design docs]
```

---

## Root Cause Analysis Report Template

```markdown
## Root Cause Analysis

### Bug Summary
**Bug ID**: [BUG-XXXX]
**Title**: [Bug title]
**Analyzed By**: [Your name]
**Analysis Date**: [YYYY-MM-DD]

### Symptoms
[Description of observable symptoms and behavior]

### Root Cause
**Primary Cause**: [Main underlying cause of the bug]

**Technical Details**:
- **Location**: [File path, method name, line numbers]
- **Issue**: [Specific code/logic problem]
- **Contributing Factors**: [Additional factors that contributed]
- **Why It Occurred**: [Explanation of mechanism]

### Evidence
**Error Messages**:
```
[Stack traces, error logs]
```

**Code Reference**:
```[language]
// Problematic code
[paste relevant code snippet]
```

**Data Evidence**:
- [Database queries showing problematic data]
- [API responses showing issues]
- [Log entries showing sequence of events]

**Timeline**:
- [When bug was introduced]
- [When it was first reported]
- [Pattern of occurrences]

### Analysis Process
1. **Initial Hypothesis**: [What you initially suspected]
2. **Investigation Steps**: [What you checked and tested]
3. **Findings**: [What you discovered]
4. **Conclusion**: [How you confirmed the root cause]

### Impact Analysis
**User Impact**:
- Affected users: [number/percentage]
- User workflows disrupted: [description]
- User experience impact: [severity description]

**Business Impact**:
- Revenue impact: [$amount or percentage]
- SLA violations: [Yes/No - details]
- Customer satisfaction: [description]
- Reputation risk: [assessment]

**System Impact**:
- Performance degradation: [metrics]
- Resource consumption: [details]
- Cascading failures: [related systems affected]
- Data integrity: [any data issues]

**Security Impact**:
- Confidentiality: [any data exposure]
- Integrity: [any unauthorized modifications]
- Availability: [any service disruptions]

### Recommended Fix

**Immediate Mitigation** (if not already done):
1. [Emergency fix or workaround]
2. [Configuration changes]
3. [Feature flag to disable problematic code]
4. [Rollback considerations]

**Permanent Solution**:

**Code Changes Required**:
```[language]
// Before (problematic code)
[current code]

// After (proposed fix)
[fixed code with explanation]
```

**Files to Modify**:
- `src/path/to/file1.js` - [description of changes]
- `src/path/to/file2.js` - [description of changes]

**Database Changes** (if any):
```sql
-- Migration script or data fixes
[SQL or migration commands]
```

**Configuration Changes** (if any):
- [Environment variables to update]
- [Config files to modify]

**Testing Requirements**:
- **Unit Tests**:
  - [Test case 1]
  - [Test case 2]
- **Integration Tests**:
  - [Test scenario 1]
  - [Test scenario 2]
- **Regression Tests**:
  - [Test to prevent recurrence]
  - [Tests for related functionality]
- **Manual Testing**:
  - [Steps to verify fix in each environment]

**Deployment Plan**:
1. [Deploy to dev/test environment]
2. [Verify fix works]
3. [Deploy to staging]
4. [Run regression tests]
5. [Deploy to production during maintenance window]
6. [Monitor for issues]

**Rollback Plan**:
[Steps to rollback if fix causes issues]

### Prevention Measures

**Immediate Actions**:
- [ ] Add validation for [specific input]
- [ ] Add error handling for [specific scenario]
- [ ] Add logging for [specific events]
- [ ] Update documentation for [specific feature]

**Long-term Improvements**:
- **Code Quality**:
  - [Refactoring needed]
  - [Design improvements]
  - [Code review focus areas]
  
- **Testing**:
  - [Test coverage gaps to address]
  - [New test scenarios to add]
  - [Testing process improvements]
  
- **Monitoring**:
  - [Metrics to track]
  - [Alerts to add]
  - [Dashboards to create]
  
- **Process**:
  - [Development process improvements]
  - [Code review checklist updates]
  - [Deployment checklist updates]

- **Documentation**:
  - [Documentation gaps to fill]
  - [Knowledge base articles to create]
  - [Training needs identified]

### Lessons Learned
- [Key takeaway 1]
- [Key takeaway 2]
- [Patterns to watch for in future]

### Estimated Effort
**Fix Implementation**: [X hours/days]
**Testing**: [X hours/days]
**Deployment**: [X hours]
**Total**: [X hours/days]

**Resources Required**: [Team members, approvals, infrastructure access, etc.]
```

---

## Security Vulnerability Report Template

```markdown
## Security Vulnerability Analysis

### Classification
**CVE ID**: [If assigned]
**Severity**: [Critical/High/Medium/Low based on CVSS]
**CVSS Score**: [X.X]
**Type**: [SQL Injection/XSS/CSRF/Authentication Bypass/Authorization Issue/etc.]
**Affected Versions**: [Version range]

### Vulnerability Description
[Detailed description of the security issue]

### Attack Vector
**Attack Complexity**: [Low/High]
**Privileges Required**: [None/Low/High]
**User Interaction**: [None/Required]
**Scope**: [Unchanged/Changed]

**Attack Scenario**:
1. [Step-by-step description of how attack would work]
2. [What attacker needs]
3. [What attacker gains]

### Proof of Concept
```[language]
[Code or commands demonstrating the vulnerability]
```

**Example Exploit**:
[Real or hypothetical example showing exploitation]

### Impact Assessment

**Confidentiality Impact**: [None/Low/High]
- [What data can be exposed]

**Integrity Impact**: [None/Low/High]
- [What data/systems can be modified]

**Availability Impact**: [None/Low/High]
- [What services can be disrupted]

**Scope**: [Limited to component / Affects other components]

### Current Security Controls
[What security measures are currently in place (if any)]

### Recommended Fix

**Immediate Actions** (within 24-48 hours):
1. [Emergency mitigation - WAF rules, rate limiting, etc.]
2. [Access restrictions]
3. [Monitoring for exploitation attempts]
4. [Incident response readiness]

**Permanent Fix**:
- [Secure code changes needed]
- [Security controls to implement]
- [Input validation requirements]
- [Output encoding requirements]
- [Authentication/authorization fixes]

**Secure Code Example**:
```[language]
// Vulnerable code
[problematic code]

// Secure code
[fixed code with security best practices]
```

### Verification Testing
- [ ] Verify attack no longer works
- [ ] Test edge cases
- [ ] Verify no bypass methods
- [ ] Security scan results clean
- [ ] Penetration test passed

### Disclosure Plan
**Timeline**:
- Day 0: Vulnerability discovered
- Day 1-2: Initial analysis and triage
- Day 2-7: Develop and test fix
- Day 7-14: Deploy fix to production
- Day 30: Public disclosure (if applicable)

**Notifications**:
- [ ] Internal security team notified
- [ ] Development team notified
- [ ] Management notified
- [ ] Affected customers notified (if required)
- [ ] Public disclosure prepared (if required)

**Compliance Requirements**:
- [ ] GDPR notification (if data breach)
- [ ] PCI-DSS reporting (if payment data)
- [ ] HIPAA reporting (if health data)
- [ ] CVE request (if applicable)

### References
- [OWASP guidelines]
- [CWE reference]
- [Related CVEs]
- [Security advisories]
```

---

## Performance Bug Report Template

```markdown
## Performance Issue Analysis

### Performance Problem Summary
**Issue**: [Concise description of performance problem]
**Severity**: [Critical/High/Medium/Low]
**Affected Component**: [System/service/feature]

### Performance Metrics

**Current Performance**:
- Response Time: [p50: Xms, p95: Yms, p99: Zms]
- Throughput: [N requests/second]
- Error Rate: [X%]
- Resource Usage: [CPU: X%, Memory: Y GB, Disk: Z%]

**Expected Performance**:
- Response Time: [Target SLA]
- Throughput: [Target capacity]
- Error Rate: [Target < X%]

**Degradation**:
- Response time increased by: [X%]
- Throughput decreased by: [Y%]
- Started occurring: [Date/time]

### Reproduction
**Load Conditions**:
- Concurrent users: [N]
- Request rate: [X req/s]
- Data volume: [Y records]
- Duration: [Z minutes]

**Steps to Reproduce**:
1. [Setup conditions]
2. [Apply load]
3. [Measure metrics]
4. [Observe degradation]

### Performance Analysis

**Profiling Results**:
- **CPU Bottleneck**: [Function/method taking most CPU time]
- **Memory Issues**: [Memory usage patterns, leaks]
- **I/O Bottleneck**: [Disk/network operations]
- **Database**: [Slow queries, N+1 problems]

**Evidence**:
```
[Profiling output, slow query logs, flame graphs]
```

### Root Cause
**Primary Bottleneck**: [Specific performance issue]

**Technical Details**:
- [Algorithm complexity issue]
- [Inefficient query]
- [Missing index]
- [Resource leak]
- [Synchronous blocking]

### Recommended Optimization

**Quick Wins** (immediate improvements):
1. [Add database index]
2. [Increase cache TTL]
3. [Add query pagination]
4. [Enable compression]

**Long-term Solutions**:
- [Algorithm optimization]
- [Database schema changes]
- [Caching strategy]
- [Architecture changes]
- [Horizontal scaling]

**Expected Improvement**:
- Response time: [Reduce by X%]
- Throughput: [Increase by Y%]
- Resource usage: [Reduce by Z%]

### Testing Plan
- Load testing scenarios
- Stress testing requirements
- Performance benchmarks
- Monitoring during rollout
```

---

## Crash Analysis Report Template

```markdown
## Crash Analysis Report

### Crash Summary
**Crash ID**: [Unique identifier]
**Occurrence**: [First seen / Total occurrences / Frequency]
**Affected Versions**: [Version range]
**Platforms**: [OS/Browser/Device affected]

### Crash Details

**Exception/Signal**:
```
[Exception type, error code, signal number]
```

**Stack Trace**:
```
[Full stack trace from crash dump]
```

**Thread State**:
```
[Thread information, deadlocks, race conditions]
```

**Memory State**:
- Heap usage: [X MB]
- Stack usage: [Y KB]
- Memory pressure: [High/Normal/Low]

### Crash Trigger

**User Action**:
[What was the user doing when crash occurred]

**System Condition**:
- [Low memory]
- [High CPU load]
- [Network issue]
- [Specific data input]

**Timing**:
- [Time-dependent]
- [Load-dependent]
- [Sequence-dependent]

### Impact
- **Crash Rate**: [X crashes per Y sessions]
- **Users Affected**: [N users / X%]
- **Data Loss**: [Yes/No - description]
- **Recovery**: [Automatic/Manual/None]

### Root Cause
[Detailed explanation of what causes the crash]

### Fix Recommendation
**Immediate**:
- [Crash handling to prevent data loss]
- [Graceful degradation]
- [User notification]

**Permanent**:
- [Bug fix]
- [Resource management]
- [Error handling]
- [Crash reporting improvements]

### Prevention
- [Input validation]
- [Resource limits]
- [Error boundaries]
- [Monitoring and alerts]
```

---

## Duplicate Bug Template

```markdown
## Duplicate Bug Report

**This bug is a duplicate of**: [Link to original bug #XXXX]

### Verification
- [ ] Same symptoms observed
- [ ] Same component affected
- [ ] Same root cause (if known)
- [ ] Same reproduction steps

### Additional Information
[Any new information from this report that might be useful:]
- Different environment or configuration
- Additional affected versions
- Alternative reproduction steps
- More detailed error messages
- Different workaround suggestions

### Action Taken
- Merged comments and attachments to original issue
- Notified reporter and redirected to original
- Updated affected version list in original
- Added any new reproduction steps to original
```
