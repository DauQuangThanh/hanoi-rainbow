# Requirements Patterns

This document describes common **business and functional patterns** to recognize in pseudocode and how to extract corresponding functional requirements. These patterns focus on WHAT the system does from a business perspective, excluding implementation, design, and performance details.

## Pattern Recognition Guide

## 1. CRUD Operations Pattern

**Pseudocode Indicators:**

```
create/insert/add
read/get/fetch/retrieve
update/modify/change
delete/remove
```

**Extract:**

- **Business Entity:** What business concept is being managed
- **Business Operations:** Create, Read, Update, Delete from business perspective
- **Business Validation Rules:** Business constraints on input data
- **Business Access Rules:** Who can perform which business operations
- **Business Audit:** What business events need tracking

**Requirements Template:**

```
FR-[ID]: [Entity] Business Management

Create Operation:
- Business Input: [Entity fields with business meaning and constraints]
- Business Validation: [Required fields, business format rules, business logic]
- Business Output: [Created entity with business identifier]
- Business Side Effects: [Business notifications, audit events]

Read Operation:
- Business Input: [Business identifier or search criteria]
- Business Output: [Entity business data or list]
- Business Filtering: [Available business filter criteria]
- Business Sorting: [Available business sort options]

Update Operation:
- Business Input: [Entity identifier + fields to update]
- Business Validation: [Field constraints, business rules]
- Business Output: [Updated entity]
- Business Rules: [What can/cannot be changed, when]

Delete Operation:
- Business Input: [Entity identifier]
- Business Validation: [Check business dependencies]
- Business Type: [Permanent removal or business archival]
- Business Side Effects: [Impact on related business entities]
```

### 2. Validation Pattern

**Pseudocode Indicators:**

```
if field is empty/null
if length > max or < min
if not matches pattern
throw error "validation failed"
```

**Extract:**

- **Field-Level Rules:** Type, format, length, range
- **Business Rules:** Cross-field validation, contextual rules
- **Error Messages:** User-facing validation messages
- **Error Handling:** How validation failures are reported

**Requirements Template:**

```
BR-[ID]: [Field/Entity] Business Validation

Business Field Rules:
- [field1]: Required for business, Must match business format [pattern]
- [field2]: Optional, Business range: 1-100
- [field3]: Required, Business values: [value1, value2, value3]

Business Logic Rules:
- Rule 1: If [business condition], then [field] must [business constraint]
- Rule 2: [field1] combined with [field2] must satisfy [business rule]

Business Error Messages:
- Missing required field: "[Business-friendly message]"
- Invalid format: "[Business-friendly explanation]"
- Out of range: "[Business constraint explanation]"
```

### 3. State Machine Pattern

**Pseudocode Indicators:**

```
if state == "initial"
  state = "processing"
else if state == "processing"
  if condition:
    state = "completed"
  else:
    state = "failed"
```

**Extract:**

- **Business States:** All possible business states
- **Business Transitions:** Valid business state changes
- **Business Triggers:** Business events causing transitions
- **Business Guards:** Business conditions for transitions
- **Business Actions:** Business side effects during transition

**Requirements Template:**

```
FR-[ID]: [Entity] Business Lifecycle

Business States:
- [state1]: [Business meaning, business entry/exit actions]
- [state2]: [Business meaning, business entry/exit actions]

Business Transitions:
From [state1] to [state2]:
- Business Trigger: [Business event or user action]
- Business Guard: [Business condition that must be true]
- Business Actions: [Business notifications, updates to related entities]

Initial Business State: [state1]
Final Business States: [state2], [error_state]

Business Invariants:
- [Business property that must hold in all states]
```

### 4. Workflow/Pipeline Pattern

**Pseudocode Indicators:**

```
step1()
result1 = process(input)
result2 = transform(result1)
output = finalize(result2)
```

**Extract:**

- **Business Stages:** Sequential business processing steps
- **Business Data Flow:** Business input/output at each stage
- **Business Transformations:** How business data changes
- **Business Dependencies:** Business stage dependencies
- **Business Error Handling:** Business failure recovery

**Requirements Template:**

```
FR-[ID]: [Process Name] Business Workflow

Business Purpose: [Business goal and value]

Business Stages:

Stage 1: [Business Stage Name]
- Business Input: [Business data required]
- Business Processing: [Business logic applied]
- Business Output: [Business result]
- Business Rules: [Applicable business constraints]
- Business Error Handling: [Business recovery strategy]

Stage 2: [Business Stage Name]
- Business Input: [Output from Stage 1]
- Business Processing: [Business transformation logic]
- Business Output: [Business result]
- Business Dependencies: [Other business processes]

Stage N: [Business Stage Name]
- Business Input: [Previous stage output]
- Business Processing: [Final business actions]
- Business Output: [Final business result]

Business Error Handling:
- Stage failure: [Business impact and mitigation]
- Business validation failure: [Business decision: reject or proceed]
```

### 5. Calculation/Algorithm Pattern

**Pseudocode Indicators:**

```
result = 0
for each item:
  result += calculate(item)
return result
```

**Extract:**

- **Business Inputs:** Business parameters and preconditions
- **Business Algorithm:** Business calculation steps and logic
- **Business Outputs:** Business results and format
- **Business Edge Cases:** Business boundary conditions

**Requirements Template:**

```
FR-[ID]: [Calculation Name]

Business Purpose: [What business value this calculates and why]

Business Inputs:
- [param1]: [Business meaning, constraints, valid business range]
- [param2]: [Business meaning, description]

Business Calculation Logic:
1. [Business step description with business formula if applicable]
2. For each [business item] in [business collection]:
   - [Business calculation or operation]
3. [Final business step]

Business Formula: [Business calculation expression]

Business Outputs:
- [Result]: [Business meaning, format, precision]

Business Edge Cases:
- Empty input: [Business behavior]
- Zero/negative values: [Business behavior]
- Maximum values: [Business behavior]
- Division by zero: [Business error handling]

Business Examples:
- Business Scenario 1: [Context] → [Expected result]
- Business Scenario 2: [Edge case context] → [Expected result]
```

### 6. Authentication/Authorization Pattern

**Pseudocode Indicators:**

```
if not authenticated:
  return "Unauthorized"
if not hasPermission(user, resource):
  return "Forbidden"
```

**Extract:**

- **Business Authentication:** How business identity is verified
- **Business Authorization:** Business permission checking
- **Business Roles:** User roles and business capabilities
- **Business Resources:** Protected business resources
- **Business Security Rules:** Business access control policies

**Requirements Template:**

```
FR-[ID]: [Resource] Business Access Control

Business Authentication:
- Required: [Yes/No for this business resource]
- Business Identity: [How user identity is established]

Business Authorization:

Business Roles:
- [role1]: [Business description and capabilities]
- [role2]: [Business description and capabilities]

Business Permissions:
- [resource].[action]: Required business roles: [role1, role2]
- [resource].[action]: Required business roles: [role3]

Business Access Rules:
- Rule 1: [role] can [action] if [business condition]
- Rule 2: Owner can always [action] on own [resource]
- Rule 3: Public [resources] allow [action] without authentication

Business Error Conditions:
- Missing authentication: [Business impact and user message]
- Insufficient permissions: [Business impact and user message]

Business Audit:
- Log: [What business events to track]
- Alert: [Business conditions for alerts]
```

### 7. Event-Driven Pattern

**Pseudocode Indicators:**

```
on event:
  handle(event)
  trigger otherEvent
```

**Extract:**

- **Business Events:** Business event types and data
- **Business Handlers:** Business processing logic for each event
- **Business Publishers:** What business actions generate events
- **Business Subscribers:** Which business functions react to events
- **Business Event Flow:** Cascading business events

**Requirements Template:**

```
FR-[ID]: [Business Event Name]

Business Event Definition:
- Name: [Business event name]
- Business Trigger: [What business action causes this event]
- Business Data:
  - eventId: Unique event identifier
  - timestamp: When business event occurred
  - businessData: [Business-relevant data fields]

Business Publishers:
- [System/Process]: Publishes when [business condition occurs]

Business Subscribers:
- [BusinessHandler1]: [What business action it performs]
- [BusinessHandler2]: [What business action it performs]

Business Processing Rules:
- Must process event: [Yes/No - business requirement]
- Processing order matters: [Yes/No - business requirement]

Cascading Business Events:
- On business success: Trigger [next business event]
- On business failure: Trigger [error business event]

Business Idempotency:
- Can process same event multiple times: [Yes/No]
- Business deduplication strategy: [How to handle duplicates]
```

### 8. Batch Processing Pattern

**Pseudocode Indicators:**
```
batch = []
for each item in items:
  batch.add(item)
  if batch.size >= batchSize:
    processBatch(batch)
    batch.clear()
if batch.size > 0:
  processBatch(batch)
```

**Extract:**
- **Business Batch Size:** Items per business batch
- **Business Batch Processing:** How batches are handled in business terms
- **Business Triggers:** What causes batch processing
- **Business Partial Failures:** Handling of partial batch failures

**Requirements Template:**
```
FR-[ID]: [Operation] Business Batch Processing

Business Batch Configuration:
- Batch Size: [Number of business items]
- Business Trigger: [Size threshold OR time threshold]

Business Batching Strategy:
- Collect: [How business items are accumulated]
- Process: [How batch is processed from business perspective]

Business Error Handling:
- Partial Success: [Business decision: continue, reject all, or mark failed items]
- Business Impact: [Effect on related business processes]

Business Rules:
- Maintain Order: [Yes/No - business requirement]
- Process Order: [Sequential or can be parallel from business view]
```

## Usage Guide

**When analyzing pseudocode:**

1. **Identify Business Pattern:** Match code structure to business patterns above
2. **Extract Business Elements:** Pull out business-specific components
3. **Generate Functional Requirements:** Use appropriate template focused on business logic
4. **Add Business Context:** Include business rules and constraints
5. **Validate Completeness:** Ensure all business logic paths covered

**Focus on Business Functionality:**
- Describe WHAT from business perspective, not HOW from technical perspective
- Extract business rules, not implementation strategies
- Document business behavior, not performance characteristics
- Specify business constraints, not technical optimizations

**For Multiple Patterns:**
- Code often contains multiple business patterns
- Extract functional requirements for each pattern
- Link related business requirements
- Create hierarchy (business process contains business rules contains business validation)

**Pattern Combinations:**
- Workflow + CRUD + Validation (common for business processes)
- Event + State Machine (common for business event systems)
- Authentication + Authorization + CRUD (common for business APIs)
