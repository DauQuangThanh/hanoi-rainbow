---
description: Create detailed strangler pattern implementation plan for phased migration
handoffs:
  - label: Validate Modernization
    agent: rainbow.validate-modernization
    prompt: Plan validation strategy for strangler migration
    send: true
  - label: Create Implementation Tasks
    agent: rainbow.taskify
    prompt: Break strangler plan into implementation tasks
scripts:
  sh: scripts/bash/setup-plan-strangler.sh --json
  ps: scripts/powershell/setup-plan-strangler.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add strangler pattern plan for feature-name') and commit strangler-plan.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON output.

2. **Load context**: Read all prior migration artifacts (LEGACY_ASSESSMENT, BUSINESS_LOGIC_MAP, DATA_MIGRATION, BATCH_MODERNIZATION, SCREEN_MIGRATION), `memory/ground-rules.md`.

3. **Execute strangler plan**: Follow template to:
   - Identify bounded contexts and service boundaries
   - Prioritize components for extraction
   - Design routing/proxy layer
   - Plan data synchronization
   - Define phase gates and rollback strategies

4. **Update agent context**: Run `{AGENT_SCRIPT}`.

5. **Report completion**.

## Phases

### Phase 0: Bounded Context Analysis

1. **Identify service boundaries**:
   - Apply Domain-Driven Design principles
   - Group related business capabilities
   - Find natural seams in the system
   - Assess coupling between components

2. **Define candidate services**:
   - Service name and responsibility
   - Business capabilities
   - Data ownership
   - Dependencies

**Output**: Bounded Context Analysis section

### Phase 1: Prioritization & Sequencing

1. **Prioritize extraction order**:
   - Risk vs. Value matrix
   - Dependency order (extract leaf services first)
   - Business value priority
   - Technical complexity

2. **Define phases**:
   - Phase 1: Low-risk, high-value services
   - Phase 2-N: Incrementally extract remaining services

**Output**: Prioritization section with extraction sequence

### Phase 2: Routing Architecture

1. **Design proxy/gateway layer**:
   - Request routing rules
   - Legacy vs. modern routing decisions
   - Session management
   - API versioning

2. **Plan gradual cutover**:
   - Feature flags for gradual rollout
   - Canary deployments
   - A/B testing strategy

**Output**: Routing Architecture section with diagrams

### Phase 3: Phase Plans

For each phase:

1. **Component extraction**:
   - Services to extract
   - API design
   - Data synchronization strategy

2. **Testing strategy**:
   - Functional equivalence tests
   - Performance tests
   - Integration tests

3. **Rollback plan**:
   - Rollback triggers
   - Rollback procedures
   - Success criteria

**Output**: Detailed phase plans

### Phase 4: Integration Patterns

1. **Legacy-to-modern communication**:
   - API adapters
   - Message translation
   - Data synchronization

2. **Modern-to-legacy communication**:
   - Legacy API wrappers
   - Event propagation

**Output**: Integration Patterns section

## Success Criteria

- [ ] Bounded contexts identified
- [ ] Service extraction sequence defined
- [ ] Routing architecture designed
- [ ] Phase plans detailed
- [ ] Integration patterns defined
- [ ] Rollback procedures documented
