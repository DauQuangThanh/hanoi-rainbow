---
description: Convert green-screen/5250 interfaces to modern web/mobile UI
handoffs:
  - label: Plan Strangler Pattern
    agent: rainbow.plan-strangler
    prompt: Create phased migration plan using strangler pattern
    send: true
  - label: Get UX Designer Support
    agent: hanoi.ux-ui-designer
    prompt: Get expert UX guidance for screen migration
scripts:
  sh: scripts/bash/setup-design-screen-migration.sh --json
  ps: scripts/powershell/setup-design-screen-migration.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add screen migration design for feature-name') and commit screen-migration.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON output.

2. **Load context**: Read FEATURE_SPEC, LEGACY_ASSESSMENT, BUSINESS_LOGIC_MAP (if exist), `memory/ground-rules.md`.

3. **Execute screen migration design**: Follow template to:
   - Inventory legacy screens
   - Extract screen layouts and navigation
   - Design modern UI wireframes
   - Map function keys to modern patterns
   - Plan for accessibility and responsive design

4. **Update agent context**: Run `{AGENT_SCRIPT}`.

5. **Report completion**.

## Phases

### Phase 0: Screen Inventory

1. **Catalog all screens**:
   - Screen IDs and names
   - Purpose and business function
   - Usage frequency
   - User roles

2. **Extract screen details**:
   - Field layouts
   - Input validations
   - Function key mappings
   - Navigation paths

**Output**: Legacy Screen Inventory section

### Phase 1: Screen Analysis

1. **Analyze screen layouts**:
   - Input fields and types
   - Display-only fields
   - Action buttons/function keys
   - Validation messages

2. **Map navigation flows**:
   - Screen-to-screen transitions
   - Menu hierarchies
   - Shortcut keys

**Output**: Screen Analysis section with navigation diagrams

### Phase 2: Modern UI Design

1. **Create wireframes**:
   - Responsive layouts
   - Modern UI patterns
   - Improved user workflows
   - Accessibility features

2. **Map legacy to modern**:
   - F1-F12 → Buttons/Icons
   - Green screen → Web forms
   - Navigate screens → SPA routing

**Output**: Modern UI Design with wireframes

### Phase 3: API Requirements

1. **Define backend services**:
   - REST API endpoints
   - Data requirements
   - Authentication/authorization

**Output**: API Requirements section

## Success Criteria

- [ ] All screens inventoried
- [ ] Navigation flows mapped
- [ ] Modern wireframes created
- [ ] Accessibility requirements defined
- [ ] API requirements documented
