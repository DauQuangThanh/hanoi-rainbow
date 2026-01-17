# Bug Analysis Techniques

## Root Cause Analysis Methods

### 1. Five Whys Technique

Ask "Why?" repeatedly to drill down to root cause:

**Example:**
- **Bug**: User login fails
- **Why?** Authentication token is invalid
- **Why?** Token expired before validation
- **Why?** Token lifetime is too short
- **Why?** Configuration set to 5 minutes instead of 30 minutes
- **Why?** Default configuration was not updated during deployment
- **Root Cause**: Missing deployment checklist item for configuration validation

### 2. Stack Trace Analysis

**Steps:**
1. **Identify Exception Type**
   - What kind of error occurred?
   - Is it a system exception or application exception?

2. **Find Failure Point**
   - Which line/method threw the exception?
   - What was the code doing at that point?

3. **Trace Call Chain**
   - How did execution reach this point?
   - What were the calling methods?
   - What were the inputs?

4. **Identify Root Caller**
   - What user action triggered this?
   - What was the entry point?

**Example Analysis:**
```
Exception: NullPointerException at UserService.getUserProfile(UserService.java:45)
  at ProfileController.getProfile(ProfileController.java:23)
  at HttpRequest.handle(HttpRequest.java:102)

Analysis:
- Line 45 in UserService.getUserProfile() tried to access null object
- Called from ProfileController.getProfile() at line 23
- Triggered by HTTP request handler
- Likely: user ID passed to getUserProfile was valid but user record was null
- Root cause: Missing null check after database query
```

### 3. Differential Analysis

Compare working vs non-working scenarios:

**What Changed?**
- Code changes (git diff)
- Configuration changes
- Data changes
- Environment changes
- Dependency updates

**Matrix Approach:**

| Scenario | Works? | Version | Environment | Data |
|----------|--------|---------|-------------|------|
| Production | No | v2.1.0 | Prod | Real data |
| Staging | Yes | v2.1.0 | Staging | Test data |
| Local | Yes | v2.1.0 | Dev | Mock data |

**Analysis**: Issue only in production with real data → likely data-specific problem

### 4. Reproduction Analysis

**Systematic Reproduction:**

1. **Establish Baseline**
   - Can you reproduce the bug?
   - How consistently? (Always/Sometimes/Rare)

2. **Vary One Factor at a Time**
   - Different user accounts
   - Different data inputs
   - Different browsers/devices
   - Different times/loads
   - Different environment configs

3. **Identify Minimal Reproduction**
   - What's the simplest way to reproduce?
   - What's the minimal test case?
   - What are the exact prerequisites?

4. **Document Reproduction Steps**
   - Clear, numbered steps
   - Expected vs actual result
   - Screenshots or videos
   - Relevant system state

### 5. Timing Analysis

For race conditions and performance issues:

**Questions to Ask:**
- Does the bug occur at specific times?
- Is it load-dependent?
- Does it happen during concurrent operations?
- Is there a delay or timing window?

**Investigation Techniques:**
- Add logging with timestamps
- Use debugger with breakpoints
- Profile execution time
- Check for locks and synchronization
- Review async operations

### 6. Data Flow Analysis

Track data through the system:

**Steps:**
1. **Identify Input Data**
   - What data enters the system?
   - What format is it in?

2. **Trace Transformations**
   - How is data processed at each step?
   - What validations are applied?
   - What conversions occur?

3. **Check State Changes**
   - How does data change?
   - What's stored in database?
   - What's cached?

4. **Verify Output**
   - What data is returned?
   - Is it correct?
   - Where did it diverge from expected?

**Example:**
```
Input: {"amount": "1,234.56"}
→ Parser: parseFloat("1,234.56") = NaN (Bug: doesn't handle comma)
→ Expected: Remove comma first: "1234.56" → 1234.56
```

---

## Evidence Collection

### Log Analysis

**What to Look For:**
- Error messages before the bug
- Warnings that might indicate issues
- Unusual patterns or frequencies
- Missing expected log entries
- Correlation with other events

**Techniques:**
- Search for error patterns: `grep -i "error" app.log`
- Find related errors: `grep -A 5 -B 5 "specific_error" app.log`
- Count occurrences: `grep "error" app.log | wc -l`
- Timeline analysis: Sort by timestamp and look for sequences

### Database Analysis

**Queries to Run:**
- Check data integrity
- Look for missing or duplicate records
- Verify relationships (foreign keys)
- Check for unexpected null values
- Review recent data changes

**Example Queries:**
```sql
-- Find orphaned records
SELECT * FROM orders WHERE user_id NOT IN (SELECT id FROM users);

-- Check for duplicates
SELECT email, COUNT(*) FROM users GROUP BY email HAVING COUNT(*) > 1;

-- Find recent problematic data
SELECT * FROM transactions WHERE status = 'failed' AND created_at > NOW() - INTERVAL 1 DAY;
```

### Performance Analysis

**Profiling:**
- CPU profiling (identify hot spots)
- Memory profiling (identify leaks)
- I/O profiling (identify bottlenecks)
- Database query profiling (slow queries)

**Metrics to Collect:**
- Response times (p50, p95, p99)
- Throughput (requests per second)
- Error rates
- Resource utilization (CPU, memory, disk)

### Network Analysis

**Tools:**
- Packet capture (Wireshark, tcpdump)
- Network monitoring (latency, packet loss)
- API request/response logging
- Browser developer tools (Network tab)

**What to Check:**
- Request/response headers
- Payload sizes
- HTTP status codes
- Timing (DNS, connection, SSL, transfer)
- Failed requests

---

## Hypothesis Testing

### Forming Hypotheses

**Good Hypothesis Characteristics:**
- Specific and testable
- Based on evidence
- Falsifiable
- Explains observed symptoms

**Example:**
- ❌ Bad: "There's a bug in the code"
- ✅ Good: "The null pointer exception occurs when getUserProfile() is called with a valid user ID but the user record has been soft-deleted, because the query doesn't filter deleted records"

### Testing Hypotheses

**1. Code Review**
- Read the suspected code section
- Look for the hypothesized issue
- Check for similar patterns elsewhere

**2. Experiment**
- Modify code to test hypothesis
- Add logging to verify assumptions
- Create unit test that should fail

**3. Verify**
- Does the fix resolve the issue?
- Does it explain all symptoms?
- Does it work in all scenarios?

### Multiple Hypotheses

When multiple possible causes exist:

**Prioritize by:**
- Likelihood (how probable)
- Impact (how severe if true)
- Ease of testing (quick to verify)

**Document All Hypotheses:**
- Hypothesis description
- Evidence supporting it
- Test performed
- Result (confirmed/rejected)

---

## Common Root Cause Patterns

### Null/Undefined Errors

**Symptoms:**
- NullPointerException, TypeError
- "Cannot read property of undefined"
- Unexpected null values

**Common Causes:**
- Missing null checks
- Uninitialized variables
- Incorrect default values
- Optional fields not handled
- Database query returning null

**Investigation:**
- Where was the variable assigned?
- What conditions lead to null?
- Are all code paths checked?

### Race Conditions

**Symptoms:**
- Intermittent failures
- Works most of the time
- Fails under load
- Timing-dependent

**Common Causes:**
- Concurrent access to shared resources
- Missing synchronization
- Incorrect locking
- Atomic operation violations

**Investigation:**
- What resources are shared?
- Are there multiple threads/processes?
- Is synchronization proper?
- Review async operations

### Memory Leaks

**Symptoms:**
- Memory usage grows over time
- Out of memory errors
- Performance degradation
- System slowdown

**Common Causes:**
- Objects not being garbage collected
- Event listeners not removed
- Circular references
- Cache without eviction
- Unclosed connections/resources

**Investigation:**
- Memory profiling over time
- Check for retained objects
- Review lifecycle management
- Look for resource cleanup

### Configuration Issues

**Symptoms:**
- Works in some environments, not others
- Fails after deployment
- Environment-specific behavior

**Common Causes:**
- Missing configuration values
- Incorrect environment variables
- Wrong API endpoints
- Credential issues
- Feature flags misconfigured

**Investigation:**
- Compare configurations across environments
- Verify all required configs present
- Check configuration precedence
- Review deployment logs

### Dependency Problems

**Symptoms:**
- Works locally but fails in production
- "Cannot find module" errors
- Incompatible versions
- Unexpected behavior after updates

**Common Causes:**
- Version mismatches
- Transitive dependency conflicts
- Breaking changes in dependencies
- Missing dependencies

**Investigation:**
- Check package lock files
- Review dependency versions
- Check for breaking changes in changelogs
- Test with specific dependency versions

---

## Debugging Strategies

### Divide and Conquer

**Break down the problem:**
1. Identify the system boundaries (input → output)
2. Find the midpoint in the flow
3. Check if data is correct at midpoint
4. If yes: bug is in second half; if no: bug is in first half
5. Repeat until bug is isolated

### Binary Search Debugging

**For finding which commit introduced a bug:**

Use `git bisect`:
```bash
git bisect start
git bisect bad  # Current version has bug
git bisect good <commit>  # Known good commit
# Git will checkout commits to test
# Mark each as good or bad
git bisect good/bad
# Git will find the problematic commit
```

### Rubber Duck Debugging

**Explain the problem out loud:**
1. Describe what the code should do
2. Explain what it actually does
3. Walk through the logic step by step
4. Often you'll spot the issue while explaining

### Add Logging

**Strategic logging:**
- Log inputs at function entry
- Log outputs before function exit
- Log intermediate values in complex calculations
- Log error paths
- Log state changes

**Good log message:**
```
// Bad
console.log(user);

// Good
console.log('[UserService.getProfile] Fetching profile for userId:', userId, 'timestamp:', Date.now());
```

### Breakpoint Debugging

**Use debugger effectively:**
- Set breakpoint before suspected issue
- Step through code line by line
- Inspect variable values
- Check call stack
- Watch expressions
- Conditional breakpoints for specific cases

---

## Anti-Patterns to Avoid

### Random Changes

❌ Making changes without understanding root cause
✅ Form hypothesis, test it, verify fix

### Incomplete Investigation

❌ Stopping at first potential cause
✅ Verify the cause explains all symptoms

### Fixing Symptoms

❌ Patching the visible issue without fixing root cause
✅ Address the underlying problem

### No Reproduction

❌ Attempting to fix without reproducing
✅ Always reproduce before fixing

### One-Size-Fits-All

❌ Applying the same fix pattern to all similar bugs
✅ Analyze each bug individually

### Ignoring Evidence

❌ Dismissing data that doesn't fit your hypothesis
✅ Follow the evidence wherever it leads
