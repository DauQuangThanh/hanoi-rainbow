---
description: Verify that modernized system matches legacy behavior and meets quality standards
handoffs:
  - label: Create Modern Feature Spec
    agent: rainbow.specify
    prompt: Document modern system specification
    send: true
  - label: Get Testing Expert Support
    agent: hanoi.mainframe-tester
    prompt: Get expert testing guidance for validation
scripts:
  sh: scripts/bash/setup-validate-modernization.sh --json
  ps: scripts/powershell/setup-validate-modernization.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'test:' prefixed git commit message (e.g., 'test: add modernization validation plan for feature-name') and commit modernization-validation.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON output with MODERNIZATION_VALIDATION, CHECKLISTS_DIR paths.

2. **Load context**: Read all migration artifacts, `memory/ground-rules.md`.

3. **Execute validation planning**: Follow template to:
   - Design parallel run strategy
   - Create data reconciliation procedures
   - Define acceptance criteria
   - Plan regression testing
   - Design compliance validation

4. **Generate validation checklist**: Create `CHECKLISTS_DIR/modernization-validation.md`.

5. **Update agent context**: Run `{AGENT_SCRIPT}`.

6. **Report completion**.

## Phases

### Phase 0: Parallel Run Strategy

1. **Design parallel execution**:
   - Run legacy and modern systems simultaneously
   - Capture inputs and outputs
   - Compare results
   - Duration (typically 2-4 weeks)

2. **Define comparison criteria**:
   - Functional equivalence
   - Performance metrics
   - Data accuracy

**Output**: Parallel Run Plan section

### Phase 1: Data Reconciliation

1. **Design reconciliation procedures**:
   - Daily data comparison
   - Automated reconciliation reports
   - Exception handling
   - Audit trails

2. **Define tolerance thresholds**:
   - Acceptable differences (e.g., rounding)
   - Critical vs. non-critical discrepancies

**Output**: Reconciliation Procedures section

### Phase 2: Test Scenarios

1. **Create test scenarios**:
   - Functional test cases from business logic
   - Integration test scenarios
   - Performance benchmarks
   - Security tests
   - Edge cases

2. **Plan regression testing**:
   - Automated test suite
   - Manual test scripts
   - User acceptance testing

**Output**: Testing Strategy section with test scenarios

### Phase 3: Compliance Validation

1. **Validate regulatory compliance**:
   - SOX controls
   - GDPR requirements
   - HIPAA compliance
   - Industry-specific regulations

2. **Audit readiness**:
   - Audit trails
   - Change logs
   - Documentation completeness

**Output**: Compliance Checklist section

### Phase 4: Go-Live Criteria

1. **Define go-live criteria**:
   - All parallel run cycles passed
   - Data reconciliation < threshold
   - Performance meets targets
   - User acceptance achieved
   - Compliance validated

2. **Create validation checklist**:
   - Generate `checklists/modernization-validation.md`
   - Include all validation items
   - Define pass/fail criteria

**Output**: Go-Live Readiness Criteria and validation checklist

## Success Criteria

- [ ] Parallel run strategy defined
- [ ] Reconciliation procedures documented
- [ ] Test scenarios created
- [ ] Compliance validation planned
- [ ] Go-live criteria defined
- [ ] Validation checklist generated

## Example Usage

```bash
# Plan validation for modernized system
/rainbow.validate-modernization Design comprehensive validation strategy for our modernized loan processing system, focusing on financial accuracy and regulatory compliance
```

## Notes

- This command creates validation plans, NOT execution results
- Run this AFTER strangler plan but BEFORE implementation
- Use `/hanoi.mainframe-tester` for detailed test case development
- Parallel run duration depends on transaction volume and complexity
- Financial systems require higher validation rigor
