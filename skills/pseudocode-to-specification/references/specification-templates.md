# Specification Templates

This document provides templates for functional specification documents generated from pseudocode analysis. These templates focus exclusively on **what** the system does (functional requirements) and exclude design, implementation, and testing details.

## Software Requirements Specification (SRS) Template

```markdown
# Software Requirements Specification
# [System Name]

Version: [X.Y]
Date: [YYYY-MM-DD]
Status: [Draft/Review/Approved]

## 1. Introduction

## 1.1 Purpose
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
[User roles, business knowledge, domain expertise]

## 3. Specific Requirements

### 3.1 Functional Requirements

#### 3.1.1 [Function/Feature Name]
**Introduction:** [Purpose and scope]
**Inputs:** [Data sources and formats]
**Processing:** [Steps and business rules]
**Outputs:** [Results and side effects]

### 3.2 External Interface Requirements

#### 3.2.1 User Interactions
[Types of user interactions, modes of access]

#### 3.2.2 System Interfaces
[Logical connections to other systems, data exchanged]

#### 3.2.3 Data Formats
[Input/output data structures and constraints]

### 3.3 Business Rules and Constraints
[Complete business logic, validation rules, calculations]

## 4. Appendices

### Appendix A: Data Dictionary
[Business data element definitions and constraints]

### Appendix B: Business Rules Matrix
[Complete listing of conditions, actions, and business logic]
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

## 6. Business Rules and Constraints
[Complete business logic, validation rules, calculation formulas]

## 7. Data Requirements
[Business entities, relationships, attributes, constraints]

## 8. Dependencies and Assumptions
[External system dependencies at functional level, assumptions made]

## 9. Open Issues and Risks
- [Issue 1] - [Impact] - [Mitigation]
- [Issue 2] - [Impact] - [Mitigation]

## 10. Appendices
[Additional business rules, glossary, references]
```

## Functional Service Requirements Template

```markdown
# Functional Service Requirements
# [Service Name]

## Overview
**Purpose:** [What business problem this service solves]
**Version:** [X.Y.Z]
**Scope:** [Business boundaries and responsibilities]

## Service Functions

### [Function Name]

**Business Purpose:** [Why this function exists, business value]

**Functional Description:** [What this function does in business terms]

**Inputs:**
- Input 1: [Business data required]
  - Required: [Yes/No]
  - Constraints: [Business rules and validation]
  - Format: [Logical format description]

**Processing Logic:**
1. [Business rule or logic step]
2. [Calculation or transformation]
3. [Decision point and branching logic]

**Outputs:**
- Output 1: [Business data produced]
  - Conditions: [When this output is generated]
  - Format: [Logical format description]

**Business Rules:**
- Rule 1: [Condition] → [Action]
- Rule 2: [Validation or constraint]
- Rule 3: [Calculation formula]

**Error Conditions:**
- Error 1: [Business condition that causes error]
  - Message: [User-facing description]
  - Recovery: [What user should do]

**Preconditions:**
[What must be true before function can execute]

**Postconditions:**
[What will be true after successful execution]

## Service Dependencies

### [External Service/System]
**Purpose:** [Why this dependency exists]
**Data Exchanged:** [Business data sent/received]
**Frequency:** [When/how often interaction occurs]
**Failure Handling:** [Business impact and mitigation]

## Data Definitions

### [Data Structure Name]
**Purpose:** [Business meaning and use]

**Fields:**
- field_name: [Business meaning, constraints, validation rules]

**Business Rules:**
- [Rule governing this data structure]

```

## Business Entity Requirements Template

```markdown
# Business Entity Requirements

## Entity: [EntityName]

### Business Description
[What this entity represents in business terms, its purpose and role]

### Business Attributes

| Attribute | Business Meaning | Required | Constraints | Valid Values |
|-----------|------------------|----------|-------------|-------------|
| identifier | Unique business identifier | Yes | Must be unique | [Format/pattern] |
| name | Business name/label | Yes | Max 255 characters | [Any restrictions] |
| status | Current state in lifecycle | Yes | Single value | active, inactive, pending |
| category | Business classification | No | From approved list | [Category values] |

### Business Relationships

**Relationship to [OtherEntity]:**
- Type: [One-to-one / One-to-many / Many-to-many]
- Business Meaning: [Why this relationship exists]
- Cardinality: [Minimum and maximum occurrences]
- Business Rules: [Rules governing the relationship]

### Business Rules and Constraints

#### Creation Rules
- Rule 1: [What must be true to create this entity]
- Rule 2: [Default values and initialization]

#### Modification Rules
- Rule 1: [What can/cannot be changed]
- Rule 2: [Conditions for updates]
- Rule 3: [State transition rules]

#### Deletion Rules
- Rule 1: [When entity can be removed]
- Rule 2: [Dependencies that prevent deletion]
- Rule 3: [Cascade behavior in business terms]

#### Validation Rules
- Rule 1: [Format or pattern validation]
- Rule 2: [Business logic validation]
- Rule 3: [Cross-field validation]

### Lifecycle States

**State: [StateName]**
- Business Meaning: [What this state means]
- Entry Conditions: [How entity enters this state]
- Exit Conditions: [How entity leaves this state]
- Allowed Transitions: [StateName] → [NextState]

### Business Invariants
[Conditions that must always be true for this entity]

### Example Business Scenarios

**Scenario 1: [Scenario Name]**
- Context: [Business situation]
- Entity State: [Attribute values]
- Business Rules Applied: [Which rules are relevant]

```

## Usage Guidelines

**Strict Functional Focus:**

These templates focus exclusively on **functional requirements** - what the system does, the business logic it implements, and the rules it enforces. They intentionally exclude:

- **Design details** - UI layouts, visual design, architecture patterns
- **Implementation details** - Technologies, frameworks, database schemas, API protocols
- **Testing details** - Test cases, test strategies, quality metrics
- **Non-functional requirements** - Performance, scalability, security implementations

**Choose Template Based on Context:**

- **SRS** - Complete system functional specification, formal documentation of business behavior
- **Functional Spec** - Feature-level functional detail, user stories, business rules
- **Functional Service Requirements** - Service-level business logic and data flows
- **Business Entity Requirements** - Business data definitions, rules, and constraints

**Adapt Templates:**

- Remove sections not relevant to business logic from pseudocode
- Add sections for domain-specific business rules and constraints
- Focus on "what" the system does, not "how" it's built or "how well" it performs
- Describe behavior, not implementation
- Specify business rules, not technical solutions

**What to Include:**

- Business logic and rules
- Data validation constraints
- User interactions and workflows
- Business calculations and formulas
- State transitions and conditions
- Error conditions and business exceptions

**Traceability:**

- Link specifications back to pseudocode sections
- Use consistent identifiers (FR-001, BR-001, etc.)
- Reference line numbers or code blocks from pseudocode
- Maintain requirements traceability matrix
