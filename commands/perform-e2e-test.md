---
description: Execute end-to-end tests based on the E2E test plan and generate detailed test result reports.
handoffs: 
  - label: Review Test Plan
    agent: rainbow.design-e2e-test
    prompt: Review or update the E2E test plan
  - label: Review Architecture
    agent: rainbow.architect
    prompt: Review product architecture design
scripts:
  sh: scripts/bash/setup-perform-e2e-test.sh --json
  ps: scripts/powershell/setup-perform-e2e-test.ps1 -Json
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

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for E2E_TEST_PLAN, TEST_RESULTS_DIR, TIMESTAMP. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read `docs/e2e-test-plan.md`, `docs/architecture.md`, `docs/standards.md` (if exists), and `memory/ground-rules.md`. Parse test scenarios, priorities, and execution requirements.

3. **Execute E2E test workflow**: Follow the execution plan from E2E_TEST_PLAN to:
   - Generate test scripts from test scenarios
   - Set up test environment and test data
   - Execute test suites (smoke, regression, or full)
   - Capture test results, screenshots, and logs
   - Generate detailed test result report
   - Analyze failures and identify root causes

4. **Stop and report**: Command ends after test execution and report generation. Report test results file path, pass/fail summary, and critical failures.

## Phases

### Phase 0: Test Preparation & Environment Setup

1. **Parse E2E test plan**:
   - Read `docs/e2e-test-plan.md`
   - Extract test scenarios by priority (P0, P1, P2, P3)
   - Identify test data requirements
   - Review test environment configuration
   - Parse test framework and tool specifications

2. **Determine test suite to execute**:
   - **Smoke**: P0 critical scenarios only (~10-15 min)
   - **Regression**: P0 + P1 scenarios (~1 hour)
   - **Full**: All scenarios P0-P3 (~3-4 hours)
   - **Custom**: User-specified scenario selection
   - Default to **Smoke** if not specified

3. **Validate prerequisites**:
   - Check test environment availability
   - Verify test framework installation
   - Validate test data is accessible
   - Ensure database/services are running
   - Confirm external service mocks are configured

4. **Set up test environment**:
   - Initialize test database with seed data
   - Start application under test
   - Configure test framework
   - Set environment variables
   - Create test result directory with timestamp

**Output**: Test environment ready, test suite selected, prerequisites validated

### Phase 1: Test Script Generation

**Prerequisites:** Phase 0 complete

1. **Generate test scripts from scenarios**:
   - For each test scenario in the selected suite:
     - Convert scenario steps to executable test code
     - Use framework-specific syntax (Cypress/Playwright/Selenium)
     - Include assertions for expected outcomes
     - Add error handling and retries
     - Follow test naming conventions from e2e-test-plan.md
     - **If `docs/standards.md` exists**: Follow testing standards and file naming conventions
     - Follow test naming conventions from e2e-test-plan.md
     - **If `docs/standards.md` exists**: Follow testing standards and file naming conventions
     - Convert scenario steps to executable test code
     - Implement preconditions and setup
     - Add assertions for expected results
     - Include postconditions and cleanup
     - Add error handling and logging

2. **Organize test files**:
   - Group by feature/journey
   - Apply naming conventions
   - Add test tags (smoke, regression, critical, etc.)
   - Include test metadata (priority, duration estimate)

3. **Create test utilities**:
   - Authentication helpers
   - Database query helpers
   - API request helpers
   - Screenshot/recording utilities
   - Custom wait conditions
   - Data factory functions

4. **Generate test data fixtures**:
   - Create user accounts for testing
   - Generate product/inventory data
   - Prepare transaction test data
   - Set up test configuration files

**Output**: Complete test scripts in `tests/e2e/scenarios/`, test utilities in `tests/e2e/helpers/`

### Phase 2: Test Execution

**Prerequisites:** Phase 1 complete

1. **Execute test suite**:
   - Run tests according to execution plan
   - Execute in priority order (P0 first)
   - Use parallel execution if configured
   - Capture test output and logs
   - Take screenshots on failures
   - Record videos for failed tests

2. **Monitor test execution**:
   - Track test progress
   - Log test start/end times
   - Capture error messages
   - Monitor test environment health
   - Detect flaky tests (retry logic)

3. **Handle test failures**:
   - Continue execution on non-critical failures
   - Stop on critical (P0) failures if configured
   - Capture failure context (DOM state, network logs)
   - Generate failure artifacts

4. **Collect test artifacts**:
   - Screenshots of failures
   - Video recordings (if enabled)
   - Browser console logs
   - Network traffic logs
   - Database state snapshots
   - Application logs

**Output**: Test execution complete, artifacts collected in `test-results/`

### Phase 3: Results Analysis & Reporting

**Prerequisites:** Phase 2 complete

1. **Analyze test results**:
   - Count passed, failed, skipped tests
   - Calculate pass rate percentage
   - Identify test execution duration
   - Detect flaky tests (inconsistent results)
   - Categorize failures by type (assertion, timeout, environment)

2. **Generate failure analysis**:
   - Group failures by scenario/feature
   - Identify common failure patterns
   - Determine root causes where possible
   - Prioritize critical failures
   - Suggest remediation actions

3. **Calculate metrics**:
   - Overall pass rate
   - Pass rate by priority (P0, P1, P2, P3)
   - Pass rate by feature/journey
   - Test execution time
   - Test coverage percentage
   - Flaky test percentage

4. **Create detailed test report**:
   - Generate `test-results/e2e-test-result_YYYYMMDD_hhmmss.md`
   - Include executive summary
   - List all test results with details
   - Embed screenshots for failures
   - Link to video recordings
   - Include logs and stack traces
   - Add failure analysis section
   - Provide recommendations

**Output**: Detailed test result report, failure analysis, metrics dashboard

### Phase 4: Report Generation & Notification

**Prerequisites:** Phase 3 complete

1. **Generate test result document**:
   - Use timestamp format: `e2e-test-result_YYYYMMDD_hhmmss.md`
   - Include:
     - **Executive Summary**: Pass rate, critical failures, duration
     - **Test Environment**: Configuration, versions, infrastructure
     - **Test Results Summary**: Passed/Failed/Skipped counts
     - **Detailed Results**: Test-by-test breakdown with status
     - **Failure Analysis**: Root causes and recommendations
     - **Test Artifacts**: Links to screenshots, videos, logs
     - **Metrics**: Charts and graphs (Mermaid)
     - **Recommendations**: Next steps, bug reports to file

2. **Generate supplementary reports**:
   - HTML report (if framework supports)
   - JSON report (for CI/CD consumption)
   - JUnit XML (for test result integration)
   - Allure report (if configured)

3. **Create failure tickets** (optional):
   - Generate bug report templates for failures
   - Include steps to reproduce
   - Attach failure artifacts
   - Assign priority based on test priority

4. **Send notifications**:
   - Log summary to console
   - Create GitHub PR comment (if CI)
   - Send Slack notification (if configured)
   - Email report (if configured)

**Output**: Complete test result report, supplementary reports, notifications sent

### Phase 5: Cleanup & Agent Context Update

**Prerequisites:** Phase 4 complete

1. **Clean up test environment**:
   - Delete test data from database
   - Stop test services
   - Archive test artifacts
   - Clear temporary files
   - Reset test environment state

2. **Archive test results**:
   - Move results to `test-results/archive/`
   - Compress old test results (> 30 days)
   - Update test result index
   - Maintain result history

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent context with latest test results
   - Add common failure patterns
   - Include test execution best practices
   - Preserve manual additions between markers

4. **Validation**:
   - Ensure test result report is complete
   - Verify all artifacts are captured
   - Confirm cleanup was successful
   - Validate report is readable and accurate

**Output**: Test environment cleaned, results archived, agent context updated

## Key Rules

- Use absolute paths for all file operations
- Test result reports go to `test-results/e2e-test-result_YYYYMMDD_hhmmss.md`
- ALWAYS include timestamp in result filename for traceability
- Test scripts generated in `tests/e2e/scenarios/` (if not already exists)
- Capture screenshots for ALL failures
- Include full stack traces and logs in failure reports
- DO NOT commit test result files to git (add to .gitignore)
- DO NOT expose sensitive data (passwords, API keys) in reports
- STOP execution if environment setup fails
- RETRY flaky tests up to 2 times before marking as failed
- Generate human-readable AND machine-readable reports

## Test Execution Modes

User can rainbow execution mode via arguments:

| Mode | Command | Scenarios | Duration | Use Case |
|------|---------|-----------|----------|----------|
| **Smoke** | `perform-e2e-test smoke` | P0 only | ~10-15 min | Quick validation, every commit |
| **Regression** | `perform-e2e-test regression` | P0 + P1 | ~1 hour | Daily/nightly builds |
| **Full** | `perform-e2e-test full` | All (P0-P3) | ~3-4 hours | Weekly, pre-release |
| **Custom** | `perform-e2e-test --scenarios=login,checkout` | Specified | Varies | Targeted testing |
| **Priority** | `perform-e2e-test --priority=P1` | Specific priority | Varies | Priority-based testing |

**Default**: Smoke tests if no mode specified

## Test Script Generation

**Script Template Structure**:

```typescript
// Example: Generated test script
import { test, expect } from '@playwright/test';
import { LoginPage } from '../pages/LoginPage';
import { UserFactory } from '../helpers/factories';

test.describe('Authentication - User Login', { tag: '@smoke' }, () => {
  let loginPage: LoginPage;
  let testUser;
  
  test.beforeEach(async ({ page }) => {
    // Setup from test plan preconditions
    testUser = await UserFactory.create();
    loginPage = new LoginPage(page);
    await loginPage.navigate();
  });
  
  test('Login with valid credentials - P0', async ({ page }) => {
    // Test steps from test plan
    await loginPage.login(testUser.email, testUser.password);
    
    // Expected results from test plan
    await loginPage.expectLoginSuccess();
    await expect(page.locator('.welcome-message')).toBeVisible();
    
    // Database validation from test plan
    const dbUser = await database.users.findByEmail(testUser.email);
    expect(dbUser.lastLoginAt).toBeTruthy();
  });
  
  test.afterEach(async () => {
    // Cleanup from test plan postconditions
    await database.users.delete(testUser.id);
  });
});
```

## Result Report Structure

**Test Result Document Template**:

```markdown
# E2E Test Execution Report

**Test Run**: YYYY-MM-DD HH:MM:SS  
**Test Suite**: [Smoke/Regression/Full]  
**Environment**: [Staging/QA]  
**Status**: [PASSED/FAILED/PARTIAL]

---

## Executive Summary

- **Total Tests**: 45
- **Passed**: 42 (93.3%)
- **Failed**: 3 (6.7%)
- **Skipped**: 0
- **Duration**: 12 minutes
- **Critical Failures**: 1 (P0 failure detected)

### Pass Rate by Priority
- P0 (Critical): 8/9 (88.9%) ⚠️
- P1 (High): 20/20 (100%) ✓
- P2 (Medium): 14/16 (87.5%)

---

## Test Environment

- **Application URL**: https://staging.example.com
- **Test Framework**: Playwright 1.40.0
- **Browsers**: Chromium 120.0, Firefox 121.0
- **Node Version**: 20.10.0
- **OS**: Ubuntu 22.04

---

## Test Results Summary

[Detailed table of all tests with status, duration, screenshots]

---

## Failed Tests Analysis

### 1. Login with valid credentials (P0) - FAILED
**Failure Reason**: Assertion failed - Welcome message not displayed  
**Stack Trace**: [...]  
**Screenshot**: ![failure-screenshot](./screenshots/login-failure.png)  
**Root Cause**: API timeout (backend service slow to respond)  
**Recommendation**: Investigate backend performance, file BUG-123

---

## Metrics

[Mermaid charts showing pass rates, duration trends, etc.]

---

## Recommendations

1. Fix critical P0 failure in login flow immediately
2. Investigate intermittent failures in checkout flow
3. Optimize test data generation (saves 30% execution time)

---

## Artifacts

- Screenshots: `./screenshots/`
- Videos: `./videos/`
- Logs: `./logs/`
- Raw Results: `./results.json`
```

## Success Criteria

Test execution is successful when:

- [ ] Test environment was set up successfully
- [ ] Test scripts were generated from test plan
- [ ] Test suite executed to completion
- [ ] Test results were captured and analyzed
- [ ] Test result report was generated with timestamp
- [ ] All failure artifacts (screenshots, logs) were collected
- [ ] Pass rate meets minimum threshold (smoke: 100%, regression: 95%)
- [ ] Critical (P0) failures are identified and documented
- [ ] Recommendations are provided for failures
- [ ] Test environment was cleaned up
- [ ] Agent context was updated

## Output Files

Expected output structure:

```
test-results/
├── e2e-test-result_20250130_143022.md   # Main test result report
├── results.json                          # Machine-readable results
├── junit.xml                             # JUnit format results
├── screenshots/                          # Failure screenshots
│   ├── login-failure-20250130-143045.png
│   └── checkout-error-20250130-143102.png
├── videos/                               # Test execution videos
│   └── login-test-20250130-143045.webm
├── logs/                                 # Application and test logs
│   ├── app.log
│   └── test-execution.log
└── archive/                              # Archived results (>30 days)

tests/
└── e2e/
    ├── scenarios/                        # Generated test scripts
    │   ├── authentication/
    │   │   ├── login.spec.ts
    │   │   └── registration.spec.ts
    │   └── checkout/
    │       └── purchase.spec.ts
    └── helpers/                          # Generated utilities
        ├── auth.helper.ts
        ├── database.helper.ts
        └── factories.ts
```

## Example Usage

```bash
# Run smoke tests (default, P0 only)
/rainbow.perform-e2e-test

# Run full regression suite
/rainbow.perform-e2e-test regression

# Run complete test suite
/rainbow.perform-e2e-test full

# Run specific scenarios
/rainbow.perform-e2e-test --scenarios=login,checkout,payment

# Run specific priority
/rainbow.perform-e2e-test --priority=P1

# Run with custom configuration
/rainbow.perform-e2e-test smoke --browser=firefox --retries=3 --parallel=4

# Run in headless mode
/rainbow.perform-e2e-test regression --headless

# Generate test scripts only (no execution)
/rainbow.perform-e2e-test --generate-only
```

## Research Guidelines

When generating test scripts or analyzing failures:

1. **For test script generation**:
   - Research: "{framework} best practices for {test type}"
   - Research: "Page Object Model implementation in {framework}"
   - Examples: "Playwright Page Object Model", "Cypress custom commands"

2. **For failure analysis**:
   - Research: "Common causes of {error type} in E2E tests"
   - Research: "Debugging {framework} test failures"
   - Examples: "Playwright timeout errors", "Cypress flaky tests"

3. **For test optimization**:
   - Research: "Speeding up {framework} test execution"
   - Research: "Parallel test execution in {framework}"
   - Research: "Test data management best practices"

## Notes

- This command **executes tests** based on the E2E test plan from `design-e2e-test`
- Test result reports are timestamped for traceability
- Failed tests include screenshots, logs, and reproduction steps
- Test scripts are generated from test plan scenarios if not already exists
- Test results are NOT committed to git (should be in .gitignore)
- Use `--generate-only` flag to create test scripts without executing
- Supports multiple execution modes (smoke, regression, full)
- Automatically retries flaky tests before marking as failed
- Generates both human-readable and machine-readable reports
- Cleans up test data and environment after execution
- Updates agent context with common patterns and failures
