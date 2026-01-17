---
name: pseudocode-to-specification
description: Reverse engineers and generates comprehensive technical specifications from pseudocode, algorithms, or code snippets. Produces detailed requirements documents, functional specifications, API documentation, data models, and workflow diagrams. Use when analyzing pseudocode, extracting requirements from algorithms, documenting undocumented code, reverse engineering specifications, creating specifications from implementation details, or when users mention "pseudocode to spec", "reverse engineer specification", "generate spec from code", "document algorithm", "extract requirements from pseudocode", "code to specification", or "specification from implementation".
---

# Pseudocode to Specification

Reverse engineer technical specifications from pseudocode, algorithms, or code snippets.

## Core Workflow

### 1. Analyze Pseudocode

Parse structure, control flow, and identify:
- Inputs, outputs, and data transformations
- Variables, constants, and data structures
- Algorithms and logic patterns
- Assumptions and implicit requirements

Ask for clarification on:
- Ambiguous variable names or operations
- Missing context about data sources/destinations
- Unclear business rules or constraints
- Undefined error handling or edge cases

### 2. Extract Functional Requirements

Document core functionality as requirements:
```
FR-001: [Function Name]
Description: System shall [action] when [condition]
Inputs: [data, parameters, context]
Processing: [steps and business rules]
Outputs: [results, side effects]
Preconditions: [required state]
Postconditions: [resulting state]
```

### 3. Extract Non-Functional Requirements

**Performance:** Time/space complexity, throughput, latency, scalability
**Quality:** Error handling, validation, security, reliability, maintainability

For detailed requirement patterns: [requirements-patterns.md](references/requirements-patterns.md)

### 4. Generate Data Model

Extract from pseudocode:
- Entities and attributes
- Data structures (arrays, objects, maps)
- Data types and constraints
- Relationships between entities
- Validation rules

Document as:
```
Entity: [EntityName]
Attributes:
  - name: [type] - [description, constraints]
  - field: [type] - [validation rules]
Relationships:
  - [RelationType] with [OtherEntity]
Constraints:
  - [Business rules, uniqueness, referential integrity]
```

### 5. Document Workflow and Logic

Analyze control flow:
- Sequential operations
- Conditional branches and decision points
- Loops and iterations
- Exception handling paths
- Async operations

Generate workflow specification:
```
Workflow: [ProcessName]

Step 1: [Action]
  - Condition: [when/if]
  - Action: [what happens]
  - Next: [step or branch]

Step 2: [Action]
  [branches]
    - If [condition]: Go to Step 3
    - Else: Go to Step 5

Error Handling:
  - [ErrorType]: [Recovery action]
```

For complex logic, use decision tables or state diagrams. See [mermaid-diagrams.md](references/mermaid-diagrams.md) for notation.

### 6. Generate API/Interface Specification

Document functions and endpoints:
```
Function: functionName
Purpose: [What it does and why]
Parameters:
  - param1: [type] - [description, constraints]
  - param2: [type] - [description, optional/required]
Returns: [type] - [description]
Throws: [exceptions and conditions]
Complexity: O([time]) time, O([space]) space
Example:
  Input: [sample input]
  Output: [expected output]
```

### 7. Identify Integration Points

Document external dependencies:
```
Integration: [ServiceName]
Purpose: [Why needed]
Protocol: [REST, GraphQL, gRPC, etc.]
Authentication: [Method]
Data Format: [JSON, XML, etc.]
Error Handling: [How failures are managed]
```

### 8. Document Assumptions and Constraints

**Technical:** Platform, resources, concurrency, data volume
**Business:** Regulatory, SLA, budget, timeline

### 9. Create Test Scenarios

Generate from pseudocode logic:
- Happy path scenarios
- Edge cases and boundary conditions
- Error conditions and exception paths
- Performance and load scenarios
- Security scenarios

Format:
```
Test Case: TC-001
Objective: Verify [behavior]
Preconditions: [setup]
Input: [test data]
Expected Output: [results]
Steps:
  1. [action]
  2. [verification]
```

## Output Formats

Generate specification based on context and needs. Standard formats include:

**Complete Specification Document:**

```markdown
# [System/Component] Specification

## 1. Overview
[Purpose, scope, and objectives]

## 2. Functional Requirements
[FR-001, FR-002, etc.]

## 3. Non-Functional Requirements
[Performance, security, reliability, etc.]

## 4. Data Model
[Entities, relationships, constraints]

## 5. API Specification
[Functions, endpoints, interfaces]

## 6. Workflow and Logic
[Process flows, decision tables]

## 7. Integration Points
[External dependencies and interfaces]

## 8. Assumptions and Constraints
[Technical and business constraints]

## 9. Test Scenarios
[Test cases and acceptance criteria]

## 10. Appendices
[Diagrams, references, glossary]
```

**User Stories (Agile):**

```
Epic: [High-level feature]

Story 1: As a [role], I want [capability] so that [benefit]
Acceptance Criteria:
  - Given [context], when [action], then [outcome]
  - Given [context], when [action], then [outcome]

Story 2: [Next story]
```

**Architecture Decision Records (ADR):**

```markdown
# ADR-001: [Decision Title]

## Status
[Proposed/Accepted/Deprecated]

## Context
[From pseudocode analysis: problem and constraints]

## Decision
[What approach the pseudocode implements]

## Consequences
[Benefits and trade-offs of this approach]

## Alternatives Considered
[Other approaches and why not chosen]
```

For additional specification formats: [specification-templates.md](references/specification-templates.md)

## Key Principles

**Maintain Traceability:**
- Link requirements to specific pseudocode sections
- Use unique identifiers for each requirement
- Reference line numbers or code blocks

**Be Explicit About Inferences:**
- Mark what is stated vs inferred
- Document assumptions
- Highlight areas needing clarification

**Focus on Intent:**
- Extract WHAT and WHY, not just HOW
- Describe business value
- Separate essential requirements from implementation choices

**Validate Completeness:**
- Ensure all pseudocode paths documented
- Verify all inputs/outputs specified
- Check all error conditions addressed

## Common Patterns

Recognize and apply standard patterns:

**CRUD Operations:** Create/read/update/delete → Standard CRUD spec with data model
**State Machines:** State transitions → State diagram with transition table
**Pipeline Processing:** Sequential transformations → Pipeline spec with stages
**Request-Response:** Input → process → output → API spec with contracts
**Event-Driven:** Listen → react → Event spec with handlers and triggers

## Quality Checklist

Before finalizing:
- ✓ All pseudocode elements covered
- ✓ Requirements clear, testable, unambiguous
- ✓ Data models complete with constraints
- ✓ Workflows cover all branches and errors
- ✓ Assumptions and constraints documented
- ✓ Integration points identified
- ✓ Test scenarios cover major use cases
- ✓ Specification follows standard format

## Examples

**Simple Algorithm:**

Input pseudocode:
```
function calculateShippingCost(weight, distance, priority):
    if weight <= 0 or distance <= 0:
        throw error "Invalid input"
    baseCost = weight * 0.5 + distance * 0.1
    if priority == "express":
        baseCost = baseCost * 1.5
    return round(baseCost, 2)
```

Generated specification excerpt:
```
FR-001: Shipping Cost Calculation
Inputs:
  - weight: decimal (kg, must be > 0)
  - distance: decimal (km, must be > 0)
  - priority: enum ["express", "standard"]
Processing:
  1. Validate inputs (weight > 0, distance > 0)
  2. Calculate: (weight × $0.50) + (distance × $0.10)
  3. Apply multiplier: express=1.5×, standard=1.0×
  4. Round to 2 decimal places
Output: decimal (USD)
Exceptions:
  - InvalidInputError: weight ≤ 0 or distance ≤ 0
  - UnknownPriorityError: invalid priority value
```

**Complex Workflow:**

For detailed workflow examples with branching logic and integration points, see [specification-templates.md](references/specification-templates.md).

## Additional Resources

Load as needed:
- [specification-templates.md](references/specification-templates.md) - Industry-standard spec formats
- [requirements-patterns.md](references/requirements-patterns.md) - Common requirement structures
- [mermaid-diagrams.md](references/mermaid-diagrams.md) - Mermaid diagram notation and examples
