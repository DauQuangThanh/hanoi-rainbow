# Severity Guidelines and Triage

## Severity Classification

### Critical (P0)

**Criteria:**
- Complete system outage or crash
- Data loss or corruption
- Security breach or critical vulnerability
- Payment/transaction processing failure
- No workaround available
- Affects all or majority of users

**Response Time:** Immediate (< 1 hour)

**Examples:**
- Database connection failure causing site downtime
- Payment gateway not processing transactions
- User data exposed to unauthorized access
- System crashes on startup for all users
- Critical security vulnerability actively exploited

**Required Actions:**
- Immediate team notification
- Escalate to on-call engineer
- Create incident war room
- Implement emergency fix or rollback
- Post-mortem required

---

### High (P1)

**Criteria:**
- Major feature completely broken
- Significant user impact (>25% of users)
- Moderate security vulnerability
- Difficult or complex workaround
- Revenue-impacting issue
- SLA violation risk

**Response Time:** Same day (< 4 hours)

**Examples:**
- Login failure for subset of users
- Core feature (search, checkout) not working
- API returning errors for multiple clients
- Performance degradation causing timeouts
- Data sync failures between systems

**Required Actions:**
- Assign to senior engineer
- Provide status updates every 2 hours
- Implement fix in hotfix branch
- Deploy outside normal release cycle
- Notify affected customers

---

### Medium (P2)

**Criteria:**
- Feature partially broken
- Moderate user impact (<25% of users)
- Reasonable workaround available
- Non-critical functionality affected
- Minor performance degradation

**Response Time:** Within 1 week

**Examples:**
- Secondary feature not working correctly
- UI element not displaying properly
- Export feature failing for specific file types
- Search results occasionally incorrect
- Minor memory leak not causing immediate issues

**Required Actions:**
- Include in next sprint planning
- Fix in regular release cycle
- Add to regression test suite
- Document workaround in knowledge base

---

### Low (P3)

**Criteria:**
- Minor issue or edge case
- Cosmetic problem
- Minimal user impact
- Easy workaround
- Nice-to-have enhancement

**Response Time:** Backlog/next release

**Examples:**
- Spelling/grammar errors
- Minor alignment issues
- Console warnings (non-breaking)
- Rare edge case handling
- Legacy browser compatibility

**Required Actions:**
- Add to backlog
- Fix when convenient
- May be combined with other small fixes
- Consider for future releases

---

## Priority vs Severity Matrix

| Severity | High Business Impact | Medium Business Impact | Low Business Impact |
|----------|---------------------|----------------------|-------------------|
| Critical | P0 - Immediate | P0 - Immediate | P1 - Same day |
| High | P1 - Same day | P1 - Same day | P2 - This week |
| Medium | P2 - This week | P2 - This week | P3 - Backlog |
| Low | P3 - Backlog | P3 - Backlog | P4 - Won't fix |

## Bug Categories

### By Type

**Functional Bugs:**
- Feature not working as specified
- Incorrect business logic
- Missing validation
- Wrong calculations or data processing

**Performance Bugs:**
- Slow response times
- High resource consumption
- Memory leaks
- Inefficient algorithms

**Security Bugs:**
- Authentication/authorization failures
- Data exposure vulnerabilities
- Injection vulnerabilities (SQL, XSS, etc.)
- Insecure configurations

**UI/UX Bugs:**
- Visual glitches or misalignments
- Broken layouts
- Incorrect styling
- Accessibility issues

**Data Bugs:**
- Data corruption
- Data loss
- Incorrect data transformation
- Data inconsistency between systems

**Integration Bugs:**
- API failures
- Third-party service issues
- Message queue problems
- Webhook failures

**Configuration Bugs:**
- Environment-specific issues
- Incorrect settings
- Deployment problems
- Infrastructure issues

**Regression Bugs:**
- Previously working feature broken
- New code breaks existing functionality
- Deployment breaks production

### By Root Cause Pattern

**Logic Errors:**
- Incorrect conditional logic
- Missing edge cases
- Wrong algorithm implementation
- Calculation errors

**Race Conditions:**
- Concurrent access issues
- Thread safety problems
- Timing-dependent bugs

**Resource Issues:**
- Memory leaks
- Connection pool exhaustion
- File handle leaks
- Deadlocks

**Null/Undefined Errors:**
- Null pointer exceptions
- Undefined variables
- Missing null checks

**Boundary Conditions:**
- Off-by-one errors
- Array index out of bounds
- Integer overflow
- Empty collection handling

**Integration Failures:**
- API contract changes
- Version incompatibilities
- Network failures
- Timeout issues

---

## Triage Process

### Step 1: Initial Assessment (5 minutes)

1. Read bug description
2. Check if reproducible
3. Identify affected component
4. Assess initial severity
5. Check for duplicates

### Step 2: Categorization (5 minutes)

1. Assign bug type
2. Identify root cause pattern (if obvious)
3. Tag with relevant labels
4. Assign to appropriate team/component

### Step 3: Priority Assignment (5 minutes)

Consider:
- Number of users affected
- Business impact (revenue, reputation)
- Security implications
- Workaround availability
- Effort to fix

### Step 4: Assignment (2 minutes)

- P0/P1: Assign immediately to on-call or senior engineer
- P2: Add to current sprint
- P3: Add to backlog

### Step 5: Stakeholder Communication

- P0/P1: Notify stakeholders immediately
- P2: Include in sprint planning
- P3: Update in weekly summary

---

## Special Considerations

### Security Vulnerabilities

**Always escalate to security team for:**
- Authentication bypasses
- Authorization failures
- Data exposure
- Injection vulnerabilities
- Known CVEs

**Use CVSS scoring:**
- 9.0-10.0: Critical
- 7.0-8.9: High
- 4.0-6.9: Medium
- 0.1-3.9: Low

### Production vs Non-Production

**Production bugs:**
- Higher priority by default
- Require faster response
- May need hotfix deployment

**Non-production bugs:**
- Can wait for regular release
- Use for testing improvements
- May indicate test coverage gaps

### Customer-Reported vs Internal

**Customer-reported:**
- Higher priority (affects users)
- Requires customer communication
- May need workaround documentation

**Internal:**
- Can be fixed proactively
- Opportunity to prevent customer impact
- Use for quality improvements
