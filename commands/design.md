---
description: Execute the implementation planning workflow using the plan template to generate design artifacts.
handoffs: 
  - label: Create Tasks
    agent: rainbow.taskify
    prompt: Break the plan into tasks
    send: true
  - label: Create Checklist
    agent: rainbow.checklist
    prompt: Create a checklist for the following domain...
scripts:
  sh: scripts/bash/setup-design.sh --json
  ps: scripts/powershell/setup-design.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add implementation plan for feature-name') and commit design.md, research.md, data-model.md, and contracts/ upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for FEATURE_SPEC, IMPL_PLAN, SPECS_DIR, BRANCH. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read FEATURE_SPEC, `memory/ground-rules.md`, and `docs/architecture.md` (if it exists). Load IMPL_PLAN template (already copied). Adhere to the principles for maximizing system clarity, structural simplicity, and long-term maintainability.

3. **Execute plan workflow**: Follow the structure in IMPL_PLAN template to:
   - Fill Technical Context (mark unknowns as "NEEDS CLARIFICATION")
   - Fill Ground-rules Check section from ground-rules
   - Align with architecture decisions from architecture.md (if available)
   - Evaluate gates (ERROR if violations unjustified)
   - Phase 0: Generate research.md (resolve all NEEDS CLARIFICATION)
   - Phase 1: Generate data-model.md, contracts/, quickstart.md
   - Phase 1: Update agent context by running the agent script
   - Re-evaluate Ground-rules Check post-design

4. **Stop and report**: Command ends after Phase 2 planning. Report branch, IMPL_PLAN path, and generated artifacts.

## Phases

### Phase 0: Outline & Research

1. **Extract unknowns from Technical Context** above:
   - For each NEEDS CLARIFICATION → research task
   - For each dependency → best practices task
   - For each integration → patterns task
   - Review architecture.md (if exists) for relevant architectural decisions and patterns

2. **Generate and dispatch research agents**:

   ```text
   For each unknown in Technical Context:
     Task: "Research {unknown} for {feature context}"
   For each technology choice:
     Task: "Find best practices for {tech} in {domain}"
   If architecture.md exists:
     Review: Architectural patterns, ADRs, and quality strategies relevant to this feature
   ```

3. **Consolidate findings** in `research.md` using format:
   - Decision: [what was chosen]
   - Rationale: [why chosen]
   - Alternatives considered: [what else evaluated]
   - Architecture alignment: [how decision aligns with architecture.md, if applicable]

**Output**: research.md with all NEEDS CLARIFICATION resolved

### Phase 1: Design & Contracts

**Prerequisites:** `research.md` complete

1. **Extract entities from feature spec** → `data-model.md`:
   - Entity name, fields, relationships
   - Validation rules from requirements
   - State transitions if applicable
   - Align with data models and component designs from architecture.md (if available)

2. **Generate API contracts** from functional requirements:
   - For each user action → endpoint
   - Use standard REST/GraphQL patterns
   - Follow API design patterns from architecture.md (if available)
   - Output OpenAPI/GraphQL schema to `/contracts/`

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - These scripts detect which AI agent is in use
   - Update the appropriate agent-specific context file
   - Add only new technology from current plan
   - Preserve manual additions between markers

**Output**: data-model.md, /contracts/*, quickstart.md, agent-specific file

## Key rules

- Use absolute paths
- ERROR on gate failures or unresolved clarifications
- If `docs/architecture.md` exists, ensure feature design aligns with architectural decisions, patterns, and quality strategies
- Reference relevant ADRs (Architecture Decision Records) from architecture.md when making design choices
- Maintain consistency with the technology stack and deployment architecture defined in architecture.md
