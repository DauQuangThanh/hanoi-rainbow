# Specification Templates

This document provides industry-standard templates for various specification documents generated from pseudocode analysis.

## Software Requirements Specification (SRS) Template

Based on IEEE 830-1998 Standard

```markdown
# Software Requirements Specification
# [System Name]

Version: [X.Y]
Date: [YYYY-MM-DD]
Status: [Draft/Review/Approved]

## 1. Introduction

### 1.1 Purpose
[What this SRS describes and who should read it]

### 1.2 Scope
[System name, what it does, benefits, objectives]

### 1.3 Definitions, Acronyms, and Abbreviations
[Glossary of terms]

### 1.4 References
[Related documents]

### 1.5 Overview
[Organization of remainder of SRS]

## 2. Overall Description

### 2.1 Product Perspective
[System context, interfaces, operations, site adaptation]

### 2.2 Product Functions
[Summary of major functions]

### 2.3 User Characteristics
[User education, experience, technical expertise]

### 2.4 Constraints
[Regulatory, hardware limitations, interfaces, etc.]

### 2.5 Assumptions and Dependencies
[Factors that affect requirements]

## 3. Specific Requirements

### 3.1 Functional Requirements

#### 3.1.1 [Function/Feature Name]
**Introduction:** [Purpose and scope]
**Inputs:** [Data sources and formats]
**Processing:** [Steps and business rules]
**Outputs:** [Results and side effects]

### 3.2 External Interface Requirements

#### 3.2.1 User Interfaces
[GUI, CLI, API interfaces]

#### 3.2.2 Hardware Interfaces
[Physical connections]

#### 3.2.3 Software Interfaces
[Connections to other systems]

#### 3.2.4 Communications Interfaces
[Network protocols, data formats]

### 3.3 Performance Requirements
[Response time, throughput, capacity]

### 3.4 Design Constraints
[Standards compliance, hardware limitations]

### 3.5 Software System Attributes

#### 3.5.1 Reliability
[MTBF, MTTR, error handling]

#### 3.5.2 Availability
[Uptime requirements]

#### 3.5.3 Security
[Authentication, authorization, encryption]

#### 3.5.4 Maintainability
[Modularity, testability]

#### 3.5.5 Portability
[Platform independence]

### 3.6 Other Requirements
[Database, operations, site adaptation]

## 4. Appendices

### Appendix A: Data Dictionary
[Data element definitions]

### Appendix B: Models
[Mermaid diagrams and models]
```

## Functional Specification Template

```markdown
# Functional Specification
# [Feature/Component Name]

## Document Control
- Version: [X.Y]
- Author: [Name]
- Date: [YYYY-MM-DD]
- Status: [Draft/Review/Final]

## 1. Executive Summary
[High-level overview of feature and business value]

## 2. Background and Context
[Problem statement, business drivers, current state]

## 3. Goals and Objectives
- Goal 1: [Measurable objective]
- Goal 2: [Measurable objective]

## 4. Scope
### In Scope
- [Item 1]
- [Item 2]

### Out of Scope
- [Item 1]
- [Item 2]

## 5. Functional Requirements

### 5.1 User Stories
```
As a [role]
I want [capability]
So that [benefit]

Acceptance Criteria:
- [ ] Criterion 1
- [ ] Criterion 2
```

### 5.2 Detailed Functionality

#### 5.2.1 [Feature Name]
**Description:** [What it does]
**User Interaction:** [How users access/use it]
**Business Rules:**
- Rule 1: [Condition and action]
- Rule 2: [Condition and action]

**Data Requirements:**
- Input: [Data needed]
- Output: [Data produced]
- Validation: [Rules]

**Error Handling:**
- Error 1: [Condition and message]
- Error 2: [Condition and message]

## 6. Non-Functional Requirements
- Performance: [Metrics]
- Security: [Requirements]
- Usability: [Standards]
- Reliability: [Targets]

## 7. User Interface Specifications
[Wireframes, mockups, navigation flows]

## 8. Data Model
[Mermaid entity diagrams, data dictionary]

## 9. API Specifications
[Endpoints, request/response formats]

## 10. Integration Points
[External systems, dependencies]

## 11. Test Strategy
[Test approach, coverage, acceptance criteria]

## 12. Open Issues and Risks
- [Issue 1] - [Impact] - [Mitigation]
- [Issue 2] - [Impact] - [Mitigation]

## 13. Appendices
[Additional Mermaid diagrams, references]
```

## API Specification Template

Based on OpenAPI/Swagger standards

```markdown
# API Specification
# [API Name]

## Overview
**Version:** [X.Y.Z]
**Base URL:** `https://api.example.com/v1`
**Protocol:** HTTPS only
**Authentication:** [Method]

## Authentication
```http
Authorization: Bearer {token}
```

## Endpoints

### [Endpoint Name]
**Purpose:** [What this endpoint does]

#### Request
```http
[METHOD] /resource/{id}
Host: api.example.com
Content-Type: application/json
Authorization: Bearer {token}
```

**Path Parameters:**
- `id` (string, required) - [Description]

**Query Parameters:**
- `filter` (string, optional) - [Description]
- `limit` (integer, optional) - [Description, default, range]

**Request Body:**
```json
{
  "field1": "string",
  "field2": 123,
  "field3": {
    "nested": "value"
  }
}
```

**Field Definitions:**
- `field1` (string, required, max 255) - [Description]
- `field2` (integer, required, 1-1000) - [Description]
- `field3` (object, optional) - [Description]

#### Responses

**200 OK - Success**
```json
{
  "status": "success",
  "data": {
    "id": "12345",
    "field": "value"
  }
}
```

**400 Bad Request - Validation Error**
```json
{
  "status": "error",
  "code": "VALIDATION_ERROR",
  "message": "Invalid input",
  "errors": [
    {
      "field": "field1",
      "message": "Required field missing"
    }
  ]
}
```

**401 Unauthorized**
```json
{
  "status": "error",
  "code": "UNAUTHORIZED",
  "message": "Invalid or expired token"
}
```

**500 Internal Server Error**
```json
{
  "status": "error",
  "code": "INTERNAL_ERROR",
  "message": "An unexpected error occurred"
}
```

#### Example Usage

**cURL:**
```bash
curl -X [METHOD] \
  https://api.example.com/v1/resource/123 \
  -H "Authorization: Bearer {token}" \
  -H "Content-Type: application/json" \
  -d '{"field1": "value"}'
```

**JavaScript:**
```javascript
const response = await fetch('https://api.example.com/v1/resource/123', {
  method: '[METHOD]',
  headers: {
    'Authorization': 'Bearer ' + token,
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({field1: 'value'})
});
```

#### Rate Limiting
- Limit: [X requests per minute]
- Header: `X-RateLimit-Remaining`

#### Notes
[Additional context, edge cases, deprecation notices]
```

## Data Model Specification Template

```markdown
# Data Model Specification

## Entity: [EntityName]

### Description
[Purpose and role of this entity in the system]

### Attributes

| Attribute | Type | Nullable | Default | Constraints | Description |
|-----------|------|----------|---------|-------------|-------------|
| id | UUID | No | auto | Primary Key | Unique identifier |
| name | String(255) | No | - | Unique | Entity name |
| status | Enum | No | 'active' | ['active','inactive'] | Current status |
| created_at | Timestamp | No | NOW() | - | Creation timestamp |
| updated_at | Timestamp | No | NOW() | - | Last update timestamp |

### Relationships

**One-to-Many with [OtherEntity]:**
- Foreign Key: [other_entity_id]
- Cascade: [Delete/Update behavior]
- Description: [Relationship meaning]

**Many-to-Many with [ThirdEntity]:**
- Join Table: [entity_third_entity]
- Description: [Relationship meaning]

### Indexes
- Primary: `id` (clustered)
- Unique: `name`
- Index: `status, created_at` (for filtering and sorting)

### Constraints
- `name` must be unique within tenant
- `status` transition rules: 'active' → 'inactive' allowed, reverse requires approval
- Soft delete: Set `deleted_at` timestamp instead of physical deletion

### Business Rules
- New entities default to 'active' status
- Cannot delete entity if related [OtherEntity] exists
- Audit log entry created on every modification

### Example Record
```json
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "name": "Example Entity",
  "status": "active",
  "created_at": "2024-01-15T10:30:00Z",
  "updated_at": "2024-01-15T10:30:00Z"
}
```
```

## Test Case Specification Template

```markdown
# Test Specification

## Test Case: TC-[ID]

### Metadata
- **Test ID:** TC-[ID]
- **Feature:** [Feature name]
- **Priority:** [High/Medium/Low]
- **Type:** [Functional/Integration/Performance/Security]
- **Author:** [Name]
- **Date:** [YYYY-MM-DD]

### Objective
[What this test verifies]

### Preconditions
- [Required state or setup]
- [Test data needed]
- [Environment configuration]

### Test Data
```json
{
  "input": {
    "field1": "value1",
    "field2": 123
  }
}
```

### Test Steps

| Step | Action | Expected Result |
|------|--------|----------------|
| 1 | [Action to perform] | [What should happen] |
| 2 | [Next action] | [Expected outcome] |
| 3 | [Verification step] | [Expected state] |

### Expected Results
- Output: [Expected output data/response]
- State Changes: [Database or system state changes]
- Side Effects: [Logs, events, notifications]

### Actual Results
[To be filled during test execution]

### Pass/Fail Criteria
- Pass if: [Conditions for success]
- Fail if: [Conditions for failure]

### Postconditions
[System state after test, cleanup needed]

### Notes
[Additional context, edge cases, known issues]
```

## Architecture Decision Record (ADR) Template

```markdown
# ADR-[Number]: [Decision Title]

## Status
[Proposed | Accepted | Deprecated | Superseded by ADR-XXX]

## Date
[YYYY-MM-DD]

## Context
[What is the issue we're facing? What factors are at play?
Include technical, organizational, and business context.
From pseudocode analysis: what problem does this algorithm solve?]

## Decision
[What is the change we're proposing/making?
What approach does the pseudocode implement?]

## Rationale
[Why this approach? What makes it appropriate?
Analysis of the algorithm/pattern chosen]

## Consequences

### Positive
- [Benefit 1]
- [Benefit 2]

### Negative
- [Trade-off 1]
- [Trade-off 2]

### Neutral
- [Impact 1]
- [Impact 2]

## Alternatives Considered

### Alternative 1: [Name]
**Description:** [Brief explanation]
**Pros:** [Benefits]
**Cons:** [Drawbacks]
**Why rejected:** [Reason]

### Alternative 2: [Name]
**Description:** [Brief explanation]
**Pros:** [Benefits]
**Cons:** [Drawbacks]
**Why rejected:** [Reason]

## Implementation Notes
[Technical details, dependencies, migration strategy]

## Related Decisions
- Relates to: [ADR-XXX]
- Supersedes: [ADR-XXX]
- Related to: [Issue/Requirement ID]

## References
- [Document 1]
- [Research paper]
- [Code location]
```

## Usage Guidelines

**Choose Template Based on Context:**
- **SRS** - Complete system specification, formal documentation
- **Functional Spec** - Feature-level detail, agile-friendly
- **API Spec** - Service interfaces, integration documentation
- **Data Model** - Database design, entity relationships
- **Test Spec** - Quality assurance, acceptance testing
- **ADR** - Design decisions, architectural choices

**Adapt Templates:**
- Remove sections not relevant to pseudocode analysis
- Add sections for domain-specific requirements
- Adjust detail level based on audience
- Maintain consistency within project

**Traceability:**
- Link specifications back to pseudocode sections
- Use consistent identifiers (FR-001, TC-001, ADR-001)
- Reference line numbers or code blocks
- Maintain requirements traceability matrix
