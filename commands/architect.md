---
description: Execute the architecture design workflow to create comprehensive system architecture documentation for the entire product.
handoffs: 
  - label: Create Feature Plan
    agent: rainbow.design
    prompt: Create implementation plan for a specific feature
    send: true
  - label: Review Ground-rules
    agent: rainbow.regulate
    prompt: Review project ground rules and constraints
scripts:
  sh: scripts/bash/setup-architect.sh --json
  ps: scripts/powershell/setup-architect.ps1 -Json
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

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for ARCH_DOC, DOCS_DIR, SPECS_DIR. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read `memory/ground-rules.md` and all feature specifications from `specs/*/spec.md`. Load ARCH_DOC template (already copied to docs/).

3. **Execute architecture workflow**: Follow the structure in ARCH_DOC template to:
   - Fill Executive Summary with product overview
   - Identify stakeholders and architectural drivers
   - Document quality attribute requirements (performance, security, scalability, etc.)
   - Create C4 Model diagrams (Context, Container, Component, Code views)
   - Design deployment architecture
   - Document architecture decisions (ADRs)
   - Map quality strategies to requirements
   - Identify risks and technical debt

4. **Stop and report**: Command ends after architecture document completion. Report ARCH_DOC path and generated artifacts.

## Phases

### Phase 0: Architecture Analysis & Stakeholder Identification

1. **Analyze feature specifications**:
   - Read all `specs/*/spec.md` files
   - Extract common patterns and requirements
   - Identify system boundaries
   - List all external integrations

2. **Identify stakeholders** from ground-rules and specs:
   - End users and their concerns
   - Development team requirements
   - Operations team needs
   - Security and compliance requirements
   - Business stakeholders

3. **Extract architectural drivers**:
   - Business goals from feature specs
   - Quality attributes (performance, scalability, security, etc.)
   - Technical constraints from ground-rules
   - Organizational constraints
   - Assumptions and dependencies

**Output**: Sections 1 (Introduction) and 2 (Architectural Drivers) completed

### Phase 1: System Design & C4 Model

**Prerequisites:** Phase 0 complete

1. **Create System Context View (C4 Level 1)**:
   - Identify all user types from feature specs
   - List all external systems and integrations
   - Generate Mermaid context diagram
   - Document system responsibilities

2. **Design Container View (C4 Level 2)**:
   - Identify technical containers (web app, API, database, cache, etc.)
   - Determine technology stack for each container
   - Define inter-container communication protocols
   - Generate Mermaid container diagram

3. **Design Component View (C4 Level 3)**:
   - Break down critical containers into components
   - Define component responsibilities and interfaces
   - Document component interaction patterns
   - Generate Mermaid component diagrams

4. **Define Code View (C4 Level 4)** (optional):
   - Document code organization and directory structure
   - Define naming conventions
   - List key design patterns

**Output**: Sections 3 (System Context), 4 (Container), 5 (Component), 6 (Code) completed with Mermaid diagrams

### Phase 2: Deployment & Infrastructure Design

**Prerequisites:** Phase 1 complete

1. **Design deployment architecture**:
   - Define production environment topology
   - Rainbow infrastructure components (compute, storage, networking)
   - Design multi-region/multi-AZ setup if needed
   - Generate Mermaid deployment diagram

2. **Design CI/CD pipeline**:
   - Define build, test, and deployment stages
   - Rainbow deployment strategy (blue/green, canary, rolling)
   - Document Infrastructure as Code approach
   - Generate Mermaid pipeline diagram

3. **Plan disaster recovery**:
   - Define backup strategy
   - Set RTO (Recovery Time Objective) and RPO (Recovery Point Objective)
   - Document recovery procedures

**Output**: Section 7 (Deployment View) completed with infrastructure details

### Phase 3: Architecture Decisions & Quality Strategies

**Prerequisites:** Phase 2 complete

1. **Document Architecture Decision Records (ADRs)**:
   - For each major architectural choice (microservices vs monolith, database choice, etc.)
   - Include: Context, Decision, Rationale, Consequences, Alternatives
   - Use ADR template format

2. **Map quality strategies to requirements**:
   - Performance strategies (caching, optimization, async processing)
   - Scalability strategies (horizontal scaling, sharding, CDN)
   - Availability strategies (redundancy, health checks, circuit breakers)
   - Security strategies (authentication, encryption, input validation)
   - Maintainability strategies (testing, observability, documentation)

3. **Identify risks and technical debt**:
   - Architecture risks with mitigation strategies
   - Known technical debt with remediation plans
   - Open questions and future considerations

**Output**: Sections 8 (Architecture Decisions), 9 (Quality Attributes), 10 (Risks & Technical Debt) completed

### Phase 4: Finalization & Agent Context Update

**Prerequisites:** Phase 3 complete

1. **Complete appendices**:
   - Glossary of terms
   - References to standards and methodologies
   - Links to related documents
   - Diagram and model index

2. **Generate supplementary documents** (if needed):
   - `docs/architecture-overview.md` - High-level summary for stakeholders
   - `docs/deployment-guide.md` - Detailed deployment instructions
   - `docs/adr/` - Individual ADR files for version control

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent-specific context with architecture decisions
   - Add technology stack and design patterns
   - Preserve manual additions between markers

4. **Validation**:
   - Ensure all sections are complete
   - Verify all Mermaid diagrams render correctly
   - Check that all "ACTION REQUIRED" comments are addressed
   - Validate consistency with ground-rules

**Output**: Complete architecture.md in docs/, supplementary files, updated agent context

## Key Rules

- Use absolute paths for all file operations
- Architecture document goes to `docs/architecture.md` (product-level), NOT `specs/` (feature-level)
- All diagrams MUST use Mermaid format embedded in markdown
- Every major architectural decision MUST have an ADR
- Quality strategies MUST map to specific quality attribute requirements
- Ground-rules constraints MUST be respected and documented
- ERROR if ground-rules violations are not justified
- Reference all feature specs that influenced the architecture
- Keep Executive Summary understandable for non-technical stakeholders

## Research Guidelines

When researching architectural patterns or technologies:

1. **For architectural styles**:
   - Research: "Compare {style1} vs {style2} for {use case}"
   - Consider: Microservices, Monolith, Event-Driven, Layered, Hexagonal, etc.

2. **For technology choices**:
   - Research: "Best practices for {technology} in {domain}"
   - Research: "{technology} scalability and performance benchmarks"
   - Consider: Maturity, community support, team expertise

3. **For quality attributes**:
   - Research: "Achieve {attribute} in {architecture style}"
   - Examples: "Achieve 99.9% availability in microservices"

4. **For deployment strategies**:
   - Research: "{platform} deployment best practices"
   - Research: "Blue/green vs canary vs rolling deployment"

## Template Sections Mapping

| Phase | Template Section | Content Source |
|-------|------------------|----------------|
| 0 | Introduction | User input + ground-rules |
| 0 | Architectural Drivers | Feature specs + ground-rules |
| 1 | System Context View | All feature specs |
| 1 | Container View | Technical analysis + research |
| 1 | Component View | Feature specs + design patterns |
| 1 | Code View | Project structure + conventions |
| 2 | Deployment View | Infrastructure requirements |
| 3 | Architecture Decisions | All architectural choices |
| 3 | Quality Attributes | Requirements + strategies |
| 3 | Risks & Technical Debt | Analysis + constraints |
| 4 | Appendices | References + glossary |

## Success Criteria

The architecture document is complete when:

- [ ] All stakeholders and their concerns are identified
- [ ] Quality attribute requirements are measurable and specific
- [ ] C4 Model diagrams (Context, Container, Component) are created with Mermaid
- [ ] Deployment architecture is detailed with infrastructure specs
- [ ] At least 3-5 ADRs document major architectural decisions
- [ ] Quality strategies map to each quality attribute requirement
- [ ] Risks are identified with mitigation strategies
- [ ] All Mermaid diagrams render correctly
- [ ] Executive summary is understandable for business stakeholders
- [ ] Ground-rules constraints are respected and documented
- [ ] Agent context is updated with architectural decisions

## Output Files

Expected output structure:

```
docs/
├── architecture.md          # Main architecture document (from arch-template.md)
├── architecture-overview.md # Optional: High-level summary
├── deployment-guide.md      # Optional: Deployment instructions
└── adr/                     # Optional: Individual ADR files
    ├── 001-microservices-architecture.md
    ├── 002-database-choice.md
    └── 003-api-gateway-pattern.md
```

## Example Usage

```bash
# Generate architecture for the entire product
/rainbow.architect

# Generate architecture with specific focus
/rainbow.architect Focus on scalability and multi-region deployment

# Generate architecture with constraints
/rainbow.architect Budget-conscious architecture with AWS services only
```

## Notes

- This command creates **product-level architecture**, not feature-level plans
- Run this AFTER creating feature specs (`/rainbow.specify`) and ground-rules
- Run this BEFORE creating feature implementation plans (`/rainbow.design`)
- The architecture guides ALL subsequent feature implementation plans
- Update the architecture document as the product evolves
- Treat architecture.md as a living document that grows with the product
