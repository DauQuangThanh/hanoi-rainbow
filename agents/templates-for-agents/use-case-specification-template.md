# Use Case Specification Template (UCS)

## Use Case: [UC-XXX] [Use Case Name]

### 1. Use Case Overview

**Use Case ID**: UC-XXX  
**Use Case Name**: [Descriptive name of the use case]  
**Created By**: [Author name]  
**Date Created**: [YYYY-MM-DD]  
**Last Updated**: [YYYY-MM-DD]  
**Version**: [1.0]

### 2. Brief Description

[Provide a brief summary of what this use case accomplishes and its purpose in the system.]

### 3. Actors

**Primary Actor(s)**: [The main user or system that initiates this use case]

* [Actor 1]: [Brief description of their role]
* [Actor 2]: [Brief description of their role]

**Secondary Actor(s)**: [Supporting actors or systems involved]

* [Actor 3]: [Brief description of their role]
* [External System]: [Brief description of integration]

### 4. Stakeholders and Interests

* **[Stakeholder 1]**: [Their interest in this use case]
* **[Stakeholder 2]**: [Their interest in this use case]

### 5. Preconditions

[What must be true or what state the system must be in before this use case can begin]

* [Precondition 1]
* [Precondition 2]

### 6. Postconditions

**Success Postconditions** (Minimal Guarantee):

* [What is guaranteed to be true after the use case completes successfully]

**Failure Postconditions**:

* [What state the system is left in if the use case fails]

### 7. Main Success Scenario (Basic Flow)

1. [Actor] [initiates action]
2. System [responds/validates/processes]
3. System [displays/calculates/stores]
4. [Actor] [provides input/confirms]
5. System [performs final action]
6. Use case ends successfully

### 8. Extensions (Alternative Flows)

**8.1. [Alternative Flow Name]**

* **Trigger**: [What causes this alternative path]
* **Steps**:
  1. [Step 1]
  2. [Step 2]
* **Resumption**: [Where the flow returns to the main scenario or ends]

**8.2. [Exception Flow Name]**

* **Trigger**: [What error or exception occurs]
* **Steps**:
  1. System [detects error]
  2. System [displays error message]
  3. [Actor] [takes corrective action]
* **Resumption**: [Where the flow returns or if it terminates]

### 9. Special Requirements

**Performance**:

* [e.g., Response time under 2 seconds]

**Security**:

* [e.g., User must be authenticated]
* [e.g., Sensitive data must be encrypted]

**Usability**:

* [e.g., Must be accessible to users with disabilities]

**Business Rules**:

* [BR-XXX]: [Business rule that applies to this use case]

### 10. Technology and Data Variations

**Data Formats**:

* [Input data format requirements]
* [Output data format specifications]

**Technology Variations**:

* [e.g., Must work on mobile and desktop browsers]
* [e.g., Compatible with specific API versions]

### 11. Frequency of Occurrence

[How often this use case is expected to be executed]

* **Expected Frequency**: [e.g., 100 times per day, once per week]
* **Peak Usage**: [When highest usage occurs]

### 12. Open Issues

* [Issue 1]: [Description and potential impact]
* [Issue 2]: [Description and proposed resolution]

### 13. Related Use Cases

* **Includes**: [UC-YYY] [Use case that this one always includes]
* **Extends**: [UC-ZZZ] [Use case that this one extends]
* **Related**: [UC-AAA] [Related use case for reference]

### 14. User Interface Mockups

[Link to or embed UI mockups, wireframes, or screenshots]

### 15. Revision History

| Version | Date | Author | Description of Changes |
| :--- | :--- | :--- | :--- |
| 1.0 | YYYY-MM-DD | [Name] | Initial version |
| 1.1 | YYYY-MM-DD | [Name] | [Changes made] |
