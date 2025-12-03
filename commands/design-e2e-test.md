---
description: Execute the E2E test design workflow to create comprehensive end-to-end test specifications for the entire product.
handoffs: 
  - label: Create Standards
    agent: rainbow.standardize
    prompt: Create coding standards and conventions for the product
    send: true
  - label: Review Architecture
    agent: rainbow.architect
    prompt: Review product architecture design
scripts:
  sh: scripts/bash/setup-design-e2e-test.sh --json
  ps: scripts/powershell/setup-design-e2e-test.ps1 -Json
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

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for E2E_TEST_DOC, DOCS_DIR, ARCH_DOC, SPECS_DIR. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read `memory/ground-rules.md`, `docs/architecture.md`, `docs/standards.md` (if exists), and all feature specifications from `specs/*/spec.md`. Load E2E_TEST_DOC template (already copied to docs/).

3. **Execute E2E test design workflow**: Follow the structure in E2E_TEST_DOC template to:
   - Define E2E test strategy and scope
   - Identify critical user journeys from feature specs
   - Design test scenarios covering system integration points
   - Define test data management strategy
   - Rainbow test environment requirements
   - Document test automation framework and tools
   - Create test execution plan

4. **Stop and report**: Command ends after E2E test document completion. Report E2E_TEST_DOC path and generated artifacts.

## Phases

### Phase 0: Test Strategy & Scope Definition

1. **Analyze architecture and features**:
   - Read architecture.md to understand system components
   - Read all `specs/*/spec.md` files
   - Identify integration points between components
   - Map external system dependencies
   - Review ground-rules for testing constraints

2. **Define E2E test scope**:
   - Identify what is IN scope (critical paths, integrations)
   - Identify what is OUT of scope (unit-level, component-level)
   - Define testing boundaries (UI-to-database, API-to-API, etc.)
   - Determine test coverage goals

3. **Select test approach**:
   - UI-driven E2E tests (Selenium, Cypress, Playwright)
   - API-driven E2E tests (REST Assured, Postman, Supertest)
   - Mobile E2E tests (Appium, Detox)
   - Mixed approach (UI + API + Database validation)

**Output**: Sections 1 (Introduction), 2 (Test Strategy), 3 (Scope) completed

### Phase 1: User Journey & Scenario Identification

**Prerequisites:** Phase 0 complete

1. **Extract user journeys from feature specs**:
   - Identify all user personas from specs
   - Map happy path workflows for each persona
   - Identify alternative paths and edge cases
   - Document cross-feature user flows

2. **Prioritize test scenarios**:
   - **Critical (P0)**: Core business flows, payment, authentication
   - **High (P1)**: Important features, data integrity, security
   - **Medium (P2)**: Secondary features, non-critical paths
   - **Low (P3)**: Edge cases, cosmetic issues

3. **Design test scenarios**:
   - For each user journey, create detailed test scenarios
   - Include: Preconditions, Steps, Expected Results, Postconditions
   - Cover positive, negative, and boundary conditions
   - Ensure scenarios test end-to-end integration

**Output**: Section 4 (User Journeys), Section 5 (Test Scenarios) completed

### Phase 2: Test Data & Environment Strategy

**Prerequisites:** Phase 1 complete

1. **Design test data management**:
   - Identify required test data types (users, products, orders, etc.)
   - Define test data generation approach (fixtures, factories, seeding)
   - Plan data cleanup and isolation strategies
   - Document sensitive data handling (PII, passwords, API keys)

2. **Define test environments**:
   - Rainbow environment configurations (staging, QA, pre-prod)
   - Document infrastructure requirements
   - Define service mocking/stubbing strategy for external dependencies
   - Plan database seeding and reset procedures

3. **Design test isolation strategy**:
   - Parallel execution approach
   - Test data isolation techniques
   - State management between tests
   - Cleanup and rollback strategies

**Output**: Section 6 (Test Data Management), Section 7 (Test Environments) completed

### Phase 3: Test Automation Framework & Tools

**Prerequisites:** Phase 2 complete

1. **Select testing framework and tools**:
   - **UI Testing**: Cypress, Playwright, Selenium, Puppeteer
   - **API Testing**: REST Assured, Supertest, Postman
   - **Mobile Testing**: Appium, Detox, Maestro
   - **Visual Testing**: Percy, Applitools, BackstopJS
   - **Performance Testing**: k6, JMeter, Gatling
   - Justify selection based on tech stack and requirements

2. **Design test architecture**:
   - Test organization structure (Page Object Model, Screenplay, etc.)
   - Test utility and helper functions
   - Custom assertions and matchers
   - Reporting and logging strategy
   - CI/CD integration approach

3. **Define coding standards for tests**:
   - Test naming conventions
   - Test structure (AAA pattern: Arrange-Act-Assert)
   - Code reusability patterns
   - Documentation requirements

**Output**: Section 8 (Test Framework), Section 9 (Test Architecture) completed

### Phase 4: Execution Plan & Reporting

**Prerequisites:** Phase 3 complete

1. **Create test execution plan**:
   - Define test execution schedule (nightly, pre-release, on-demand)
   - Rainbow execution triggers (CI/CD pipeline, manual, scheduled)
   - Plan test suite organization (smoke, regression, full)
   - Define execution sequence and dependencies

2. **Design reporting strategy**:
   - Test result reporting format (HTML, JSON, XML)
   - Dashboard and visualization (Allure, ReportPortal, custom)
   - Failure notification and alerting
   - Metrics tracking (pass rate, execution time, flakiness)

3. **Plan maintenance and updates**:
   - Test suite maintenance schedule
   - Flaky test identification and resolution
   - Test refactoring guidelines
   - Update process for new features

**Output**: Section 10 (Execution Plan), Section 11 (Reporting) completed

### Phase 5: Finalization & Agent Context Update

**Prerequisites:** Phase 4 complete

1. **Document test scenarios in detail**:
   - Create detailed test case specifications
   - Generate test scenario flowcharts (Mermaid)
   - Document test data samples
   - List prerequisite setup steps

2. **Generate supplementary documents** (if needed):
   - `docs/e2e-test-scenarios.md` - Detailed test scenario catalog
   - `docs/test-data-guide.md` - Test data management guide
   - `docs/e2e-test-setup.md` - Environment setup instructions
   - `tests/e2e/README.md` - Quick start guide for developers

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent-specific context with E2E test strategy
   - Add test patterns and best practices
   - Preserve manual additions between markers

4. **Validation**:
   - Ensure all critical user journeys have test scenarios
   - Verify test scenarios cover all integration points
   - Check that test data strategy is comprehensive
   - Validate test framework selection justification
   - Ensure consistency with architecture and ground-rules

**Output**: Complete e2e-test-plan.md in docs/, supplementary files, updated agent context

## Key Rules

- Use absolute paths for all file operations
- E2E test document goes to `docs/e2e-test-plan.md` (product-level), NOT `specs/` (feature-level)
- All test scenario flows SHOULD use Mermaid diagrams where helpful
- Test scenarios MUST cover critical integration points from architecture
- Test data strategy MUST address data privacy and security
- Ground-rules constraints MUST be respected (e.g., no production data in tests)
- ERROR if critical user journeys are not covered
- Reference all feature specs that influenced test scenarios
- Keep test scenarios understandable for both QA and developers

## Research Guidelines

When researching E2E testing approaches:

1. **For testing frameworks**:
   - Research: "{framework} best practices for {technology}"
   - Research: "Compare {framework1} vs {framework2} for {use case}"
   - Examples: "Cypress vs Playwright for React", "Appium vs Detox for React Native"

2. **For test patterns**:
   - Research: "Page Object Model vs Screenplay pattern"
   - Research: "E2E test data management strategies"
   - Research: "Handling flaky E2E tests"

3. **For test environments**:
   - Research: "{platform} staging environment best practices"
   - Research: "Docker for E2E test environments"
   - Research: "Mock vs real external services in E2E tests"

4. **For CI/CD integration**:
   - Research: "E2E tests in {CI platform} pipeline"
   - Research: "Parallel E2E test execution strategies"
   - Examples: "Cypress in GitHub Actions", "Playwright sharding"

## Template Sections Mapping

| Phase | Template Section | Content Source |
|-------|------------------|----------------|
| 0 | Introduction | Architecture + ground-rules |
| 0 | Test Strategy | Architecture analysis |
| 0 | Scope | Feature specs + architecture |
| 1 | User Journeys | Feature specs |
| 1 | Test Scenarios | User journeys + edge cases |
| 2 | Test Data Management | Feature specs + privacy requirements |
| 2 | Test Environments | Architecture + infrastructure |
| 3 | Test Framework | Tech stack + research |
| 3 | Test Architecture | Design patterns + best practices |
| 4 | Execution Plan | CI/CD + scheduling |
| 4 | Reporting | Metrics + alerting |
| 5 | Appendices | References + examples |

## Success Criteria

The E2E test document is complete when:

- [ ] Test strategy clearly defines scope and boundaries
- [ ] All critical user journeys are identified and prioritized
- [ ] Test scenarios cover all major integration points from architecture
- [ ] Test data management strategy addresses privacy and security
- [ ] Test environment requirements are specified
- [ ] Testing framework and tools are selected with justification
- [ ] Test automation architecture is designed
- [ ] Execution plan includes CI/CD integration
- [ ] Reporting strategy is comprehensive
- [ ] Test scenarios are detailed enough to implement
- [ ] Ground-rules constraints are respected
- [ ] Agent context is updated with test strategy

## Output Files

Expected output structure:

```
docs/
├── e2e-test-plan.md         # Main E2E test document (from e2e-test-template.md)
├── e2e-test-scenarios.md    # Optional: Detailed test scenario catalog
├── test-data-guide.md       # Optional: Test data management guide
└── e2e-test-setup.md        # Optional: Environment setup instructions

tests/
└── e2e/
    ├── README.md            # Quick start guide
    ├── fixtures/            # Test data fixtures
    ├── helpers/             # Test utilities
    └── scenarios/           # Test scenario implementations
```

## Example Usage

```bash
# Generate E2E test plan for the entire product
/rainbow.design-e2e-test

# Generate E2E test plan with specific focus
/rainbow.design-e2e-test Focus on payment and checkout flows with PCI compliance

# Generate E2E test plan with constraints
/rainbow.design-e2e-test Use Cypress for web, Maestro for mobile, prioritize smoke tests
```

## Notes

- This command creates **product-level E2E test plan**, not feature-level tests
- Run this AFTER creating architecture (`/rainbow.architect`)
- Run this BEFORE or ALONGSIDE standards (`/rainbow.standardize`)
- The E2E test plan guides test automation implementation
- E2E tests should focus on critical paths and integration points
- Avoid testing implementation details - test user-visible behavior
- Balance coverage with maintenance cost - prioritize critical flows
- Update E2E test plan as new features are added
- Treat e2e-test-plan.md as a living document that evolves with the product
