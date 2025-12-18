---
description: Analyze existing legacy systems to understand current state before modernization
handoffs:
  - label: Map Business Logic
    agent: rainbow.map-business-logic
    prompt: Extract business rules from analyzed legacy code
    send: true
  - label: Get Legacy Analyst Support
    agent: hanoi.legacy-analyst
    prompt: Help analyze legacy codebase and extract insights
scripts:
  sh: scripts/bash/setup-assess-legacy.sh --json
  ps: scripts/powershell/setup-assess-legacy.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add legacy system assessment for feature-name') and commit legacy-assessment.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for FEATURE_SPEC, LEGACY_ASSESSMENT, SPECS_DIR, BRANCH. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read FEATURE_SPEC (if exists), `memory/ground-rules.md`. Load LEGACY_ASSESSMENT template (already copied). Adhere to the principles for maximizing system clarity, structural simplicity, and long-term maintainability.

3. **Execute legacy assessment workflow**: Follow the structure in LEGACY_ASSESSMENT template to:
   - Gather system inventory (hardware, software, languages, versions)
   - Analyze code complexity and quality metrics
   - Catalog business capabilities and processes
   - Document data architecture and models
   - Map integration points and dependencies
   - Assess non-functional characteristics (performance, security, availability)
   - Calculate modernization readiness score
   - Recommend modernization approach (rehost, replatform, refactor, replace)

4. **Update agent context**: Run `{AGENT_SCRIPT}` to update agent-specific context with assessment findings.

5. **Stop and report**: Command ends after assessment completion. Report branch, LEGACY_ASSESSMENT path, and readiness score.

## Phases

### Phase 0: System Inventory & Discovery

1. **Collect system information**:
   - Hardware configuration (mainframe model, MIPS, memory, storage)
   - Software inventory (OS, subsystems, middleware, databases)
   - Programming languages and versions (COBOL, RPG, PL/I, JCL, Assembler)
   - Database systems (DB2, IMS, VSAM, IDMS, flat files)
   - Integration technologies (MQ, CICS, IMS TM, batch interfaces)

2. **Code inventory**:
   - Count lines of code by language
   - Identify program types (online, batch, utilities)
   - List copybooks and includes
   - Catalog JCL jobs and procedures
   - Document screen/panel definitions (CICS maps, DDS, RPG display files)

3. **Document current metrics**:
   - Transaction volumes
   - Batch job runtimes
   - Database sizes and growth rates
   - System availability and uptime
   - User counts and concurrency

**Output**: Executive Summary and System Inventory sections completed

### Phase 1: Code Analysis

**Prerequisites:** Phase 0 complete

1. **Perform static code analysis**:
   - Calculate complexity metrics (cyclomatic complexity, nesting depth)
   - Identify dead code and unused programs
   - Find code duplication
   - Detect common anti-patterns
   - Measure code quality indicators

2. **Analyze dependencies**:
   - Build call graphs (program-to-program calls)
   - Map data dependencies (file/table usage)
   - Identify tight coupling
   - Find circular dependencies
   - Document external system dependencies

3. **Assess technical debt**:
   - Identify hard-coded values
   - Find obsolete language features
   - Locate workarounds and patches
   - Document missing error handling
   - Assess maintainability issues

**Output**: Code Analysis section with complexity scores and technical debt inventory

### Phase 2: Business Logic Cataloging

**Prerequisites:** Phase 1 complete

1. **Identify business capabilities**:
   - Extract business functions from program documentation
   - Map programs to business capabilities
   - Identify core vs. supporting functions
   - Document business-critical processes
   - Assess regulatory/compliance requirements

2. **Catalog business rules**:
   - Extract validation rules from code
   - Document calculation formulas
   - Identify decision logic
   - Map business workflows
   - Document exception handling

3. **Assess business value**:
   - Identify high-value capabilities
   - Find rarely-used features
   - Document business pain points
   - Assess impact of system limitations

**Output**: Business Logic Catalog section

### Phase 3: Data Architecture Assessment

**Prerequisites:** Phase 2 complete

1. **Analyze data models**:
   - Document database schemas
   - Map file layouts (VSAM, sequential, indexed)
   - Identify data relationships
   - Document data volumes
   - Assess data quality issues

2. **Analyze data flows**:
   - Map data lineage
   - Identify data sources and sinks
   - Document transformation logic
   - Find data redundancy
   - Assess data governance

3. **Assess data migration complexity**:
   - Identify hierarchical data structures
   - Document non-relational data
   - Assess data quality issues
   - Estimate data cleansing effort

**Output**: Data Architecture section

### Phase 4: Integration Landscape Mapping

**Prerequisites:** Phase 3 complete

1. **Catalog integrations**:
   - Document external system interfaces
   - Map API endpoints and protocols
   - Identify file-based integrations
   - Document message queue usage
   - Map batch interfaces

2. **Assess integration patterns**:
   - Identify synchronous vs. asynchronous
   - Document data formats
   - Assess error handling
   - Evaluate integration reliability

**Output**: Integration Landscape section

### Phase 5: Modernization Readiness Assessment

**Prerequisites:** Phase 4 complete

1. **Calculate complexity score**:
   - Code complexity (40%): Lines of code, cyclomatic complexity, dependencies
   - Data complexity (25%): Data model complexity, data quality issues
   - Integration complexity (20%): Number of integrations, protocols used
   - Business criticality (15%): Uptime requirements, transaction volumes

2. **Assess risks**:
   - Technical risks (obsolete technology, knowledge gaps)
   - Business risks (regulatory compliance, business continuity)
   - Organizational risks (team capacity, stakeholder alignment)

3. **Recommend modernization approach**:
   - **Rehost (Lift-and-Shift)**: Move to cloud with minimal changes
     - When: Low complexity, time-sensitive, cost-constrained
   - **Replatform**: Migrate to modern platform with minor changes
     - When: Medium complexity, want some optimization
   - **Refactor**: Restructure code while preserving behavior
     - When: High technical debt, but sound architecture
   - **Replace**: Build new system from scratch
     - When: High complexity, poor architecture, major business changes
   - **Hybrid**: Combination of approaches for different components
     - When: Large system with varying component characteristics

4. **Estimate effort**:
   - Person-months by approach
   - Timeline estimates
   - Resource requirements
   - Budget considerations

**Output**: Modernization Readiness section with recommendation

### Phase 6: Finalization & Agent Context Update

**Prerequisites:** Phase 5 complete

1. **Complete executive summary**:
   - Key findings and insights
   - Complexity assessment summary
   - Risk summary
   - Recommended approach with justification
   - High-level timeline and cost

2. **Validate completeness**:
   - Ensure all sections are complete
   - Verify metrics and calculations
   - Check that all "ACTION REQUIRED" comments are addressed
   - Validate consistency with ground-rules

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent-specific context with assessment findings
   - Add key technologies and complexity factors
   - Preserve manual additions between markers

**Output**: Complete legacy-assessment.md, updated agent context

## Key Rules

- Use absolute paths for all file operations
- Assessment document goes to `specs/<feature>/legacy-assessment.md` (feature-level)
- All complexity calculations MUST show methodology and formulas
- Every major finding MUST have supporting evidence (metrics, code samples)
- Modernization recommendation MUST be justified with data
- Risk assessment MUST include mitigation strategies
- ERROR if assessment is incomplete or lacks data
- Reference ground-rules constraints in recommendations
- Consider organizational capacity and constraints

## Complexity Scoring Formula

```
Complexity Score (0-100) = (
  Code Complexity × 0.40 +
  Data Complexity × 0.25 +
  Integration Complexity × 0.20 +
  Business Criticality × 0.15
)

Where:
- Code Complexity: Based on LOC, cyclomatic complexity, dependencies
- Data Complexity: Based on data model complexity, data quality
- Integration Complexity: Based on integration count, protocol diversity
- Business Criticality: Based on uptime requirements, transaction volume
```

**Interpretation**:
- 0-25: Low complexity (Rehost recommended)
- 26-50: Medium complexity (Replatform recommended)
- 51-75: High complexity (Refactor recommended)
- 76-100: Very high complexity (Replace or hybrid recommended)

## Success Criteria

The legacy assessment is complete when:

- [ ] Complete system inventory documented
- [ ] Code analysis completed with metrics
- [ ] Business capabilities cataloged
- [ ] Data architecture documented
- [ ] Integration landscape mapped
- [ ] Complexity score calculated with formula shown
- [ ] Risk assessment completed
- [ ] Modernization approach recommended with justification
- [ ] Effort estimates provided
- [ ] Executive summary completed
- [ ] Agent context updated

## Example Usage

```bash
# Assess mainframe COBOL system
/rainbow.assess-legacy Analyze our banking core system running on z/OS with COBOL/CICS applications and DB2 database

# Assess IBM i (AS/400) system
/rainbow.assess-legacy Assess our inventory management system on IBM i with RPG programs and DB2 for i

# Assess with specific focus
/rainbow.assess-legacy Focus on data migration complexity for our claims processing system
```

## Notes

- This command creates **feature-level assessment**, documenting a specific system or component
- Run this AFTER creating feature specification or as first step in modernization
- Run this BEFORE mapping business logic or planning migration
- The assessment guides all subsequent modernization decisions
- Use `/hanoi.legacy-analyst` agent for deep code analysis support
- Follow up with `/rainbow.map-business-logic` to extract business rules
