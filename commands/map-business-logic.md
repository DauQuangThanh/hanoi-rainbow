---
description: Extract and document business rules from legacy code for reimplementation
handoffs:
  - label: Design Data Migration
    agent: rainbow.design-data-migration
    prompt: Create data migration strategy based on business logic
    send: true
  - label: Create Modern Specification
    agent: rainbow.specify
    prompt: Create specification for modern system based on extracted business logic
scripts:
  sh: scripts/bash/setup-map-business-logic.sh --json
  ps: scripts/powershell/setup-map-business-logic.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add business logic extraction for feature-name') and commit business-logic-map.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for FEATURE_SPEC, BUSINESS_LOGIC_MAP, SPECS_DIR, BRANCH.

2. **Load context**: Read FEATURE_SPEC, LEGACY_ASSESSMENT (if exists), `memory/ground-rules.md`. Load BUSINESS_LOGIC_MAP template (already copied).

3. **Execute business logic extraction**: Follow the structure in template to:
   - Extract business rules from legacy code
   - Document data validations
   - Catalog calculations and formulas
   - Map business workflows
   - Document exception handling
   - Identify regulatory/compliance requirements

4. **Update agent context**: Run `{AGENT_SCRIPT}`.

5. **Report completion**: Branch, file path, and business rules count.

## Phases

### Phase 0: Code Analysis & Rule Identification

1. **Analyze legacy code structure**:
   - Identify program sections containing business logic
   - Find validation routines
   - Locate calculation paragraphs/procedures
   - Identify decision logic (IF/CASE/EVALUATE statements)
   - Find copybooks with business rules

2. **Extract business rules**:
   - Document each rule with unique ID
   - Capture source location (program, line number)
   - Describe rule in business terms
   - Assess criticality (must-have, should-have, nice-to-have)
   - Note dependencies between rules

**Output**: Business Rules Catalog with initial entries

### Phase 1: Data Validation Rules

**Prerequisites:** Phase 0 complete

1. **Extract field-level validations**:
   - Required/optional fields
   - Data type validations
   - Format validations (patterns, masks)
   - Range validations (min/max)
   - Length constraints

2. **Extract cross-field validations**:
   - Conditional requirements
   - Mutual exclusivity rules
   - Consistency checks
   - Referential integrity rules

**Output**: Data Validations section

### Phase 2: Calculations & Formulas

**Prerequisites:** Phase 1 complete

1. **Document financial calculations**:
   - Interest calculations
   - Tax calculations
   - Discount/pricing logic
   - Commission calculations
   - Balance calculations

2. **Document derived fields**:
   - Field derivation logic
   - Aggregation rules
   - Transformation formulas

3. **Preserve precision requirements**:
   - Decimal precision (critical for COBOL)
   - Rounding rules
   - Overflow handling

**Output**: Calculations & Formulas section

### Phase 3: Business Workflows

**Prerequisites:** Phase 2 complete

1. **Map process flows**:
   - Step-by-step workflows
   - Decision points
   - State transitions
   - Business events and triggers

2. **Document business workflows using flow diagrams**:
   - Use Mermaid flowcharts
   - Show decision logic
   - Document alternative paths

**Output**: Business Workflows section with diagrams

### Phase 4: Exception Handling & Regulatory Requirements

**Prerequisites:** Phase 3 complete

1. **Document exception handling**:
   - Error conditions
   - Compensating transactions
   - Rollback procedures
   - Error notifications

2. **Identify regulatory requirements**:
   - Compliance rules (SOX, GDPR, HIPAA)
   - Audit requirements
   - Retention policies
   - Security requirements

**Output**: Complete business-logic-map.md

## Key Rules

- Every business rule MUST have a unique ID (BR-XXX format)
- Every rule MUST include source code location
- All calculations MUST preserve original precision
- Use business terminology, not technical jargon
- Document "why" not just "what"
- Flag rules that are unclear or contradictory
- ERROR if critical rules cannot be extracted

## Success Criteria

- [ ] All business rules cataloged with unique IDs
- [ ] Source code locations documented
- [ ] Validations completely documented
- [ ] Calculations with precision requirements
- [ ] Workflows mapped with diagrams
- [ ] Regulatory requirements identified
- [ ] Agent context updated

## Example Usage

```bash
# Extract business logic from COBOL programs
/rainbow.map-business-logic Extract all business rules from our loan processing COBOL programs, focusing on interest calculation and approval logic

# Extract from RPG programs
/rainbow.map-business-logic Map business logic from order entry RPG programs on IBM i
```
