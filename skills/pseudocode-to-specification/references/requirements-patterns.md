# Requirements Patterns

This document describes common patterns to recognize in pseudocode and how to extract corresponding requirements.

## Pattern Recognition Guide

### 1. CRUD Operations Pattern

**Pseudocode Indicators:**
```
create/insert/add
read/get/fetch/retrieve
update/modify/change
delete/remove
```

**Extract:**
- **Data Model:** Entity being manipulated
- **Operations:** Create, Read, Update, Delete specifications
- **Validation Rules:** Input constraints
- **Access Control:** Who can perform which operations
- **Audit:** Logging and tracking requirements

**Requirements Template:**
```
FR-[ID]: [Entity] Management

Create Operation:
- Input: [Entity fields with types and constraints]
- Validation: [Required fields, format rules, business rules]
- Output: [Created entity with generated ID]
- Side Effects: [Audit log, notifications]

Read Operation:
- Input: [ID or filter criteria]
- Output: [Entity data or list]
- Filtering: [Available filter fields]
- Sorting: [Available sort options]
- Pagination: [Page size, offset/cursor]

Update Operation:
- Input: [Entity ID + fields to update]
- Validation: [Field constraints, business rules]
- Concurrency: [Optimistic locking, versioning]
- Output: [Updated entity]

Delete Operation:
- Input: [Entity ID]
- Validation: [Check dependencies]
- Type: [Hard delete or soft delete]
- Side Effects: [Cascade rules, cleanup]
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
VR-[ID]: [Field/Entity] Validation

Field Validations:
- [field1]: Required, String, Length: 1-255, Pattern: [regex]
- [field2]: Optional, Integer, Range: 1-100
- [field3]: Required, Enum: [value1, value2, value3]

Business Rules:
- Rule 1: If [condition], then [field] must [constraint]
- Rule 2: [field1] + [field2] must be [constraint]

Error Responses:
- Empty required field: "Field '[name]' is required"
- Invalid format: "Field '[name]' must match format [format]"
- Out of range: "Field '[name]' must be between [min] and [max]"
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
- **States:** All possible states
- **Transitions:** Valid state changes
- **Triggers:** Events or conditions causing transitions
- **Guards:** Conditions that must be met for transition
- **Actions:** Side effects during transition

**Requirements Template:**
```
SM-[ID]: [Entity] State Machine

States:
- [state1]: [Description, entry/exit actions]
- [state2]: [Description, entry/exit actions]

Transitions:
From [state1] to [state2]:
- Trigger: [Event or action]
- Guard: [Condition that must be true]
- Actions: [Side effects, notifications, updates]

State Diagram:
[initial] → [state1] → [state2] → [terminal]
           ↓
         [error_state]

Initial State: [state1]
Terminal States: [state2], [error_state]

Invariants:
- [Property that must hold in all states]
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
- **Stages:** Sequential processing steps
- **Data Flow:** Input/output at each stage
- **Transformations:** How data changes
- **Dependencies:** Stage dependencies
- **Error Handling:** Failure recovery at each stage

**Requirements Template:**
```
WF-[ID]: [Process Name] Workflow

Overview: [Purpose and scope]

Stages:

Stage 1: [Name]
- Input: [Data format and source]
- Processing: [What happens]
- Output: [Result format]
- Duration: [Expected time]
- Error Handling: [Recovery strategy]

Stage 2: [Name]
- Input: [Output from Stage 1]
- Processing: [Transformation logic]
- Output: [Result format]
- Dependencies: [External systems]

Stage N: [Name]
- Input: [Previous stage output]
- Processing: [Final actions]
- Output: [Final result]

Error Handling:
- Stage failure: [Retry, rollback, or forward to error queue]
- Timeout: [Action after max duration]
- Data validation failure: [Reject or partial process]

Monitoring:
- Track: [Metrics to monitor at each stage]
- Alert: [Conditions for alerts]
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
- **Inputs:** Parameters and preconditions
- **Algorithm:** Steps and logic
- **Outputs:** Results and format
- **Complexity:** Time and space requirements
- **Edge Cases:** Boundary conditions

**Requirements Template:**
```
ALG-[ID]: [Algorithm Name]

Purpose: [What it calculates and why]

Inputs:
- [param1]: [Type] - [Constraints, valid range]
- [param2]: [Type] - [Description]

Algorithm:
1. [Step description with formula if applicable]
2. For each [item] in [collection]:
   - [Calculation or operation]
3. [Final step]

Formula: [Mathematical expression if applicable]

Outputs:
- [Result]: [Type, format, precision]

Edge Cases:
- Empty input: [Behavior]
- Zero/negative values: [Behavior]
- Maximum values: [Behavior]
- Division by zero: [Error handling]

Performance:
- Time Complexity: O([expression])
- Space Complexity: O([expression])
- Maximum input size: [Limit]

Examples:
- Input: [Sample] → Output: [Expected result]
- Input: [Edge case] → Output: [Expected result]
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
- **Authentication:** How identity is verified
- **Authorization:** Permission checking
- **Roles:** User roles and capabilities
- **Resources:** Protected resources
- **Security Rules:** Access control policies

**Requirements Template:**
```
SEC-[ID]: [Resource] Access Control

Authentication:
- Method: [JWT, OAuth, API Key, Session]
- Required: [Yes/No for this resource]
- Token Location: [Header, Cookie, Query]
- Token Format: [Structure]

Authorization:

Roles:
- [role1]: [Description and capabilities]
- [role2]: [Description and capabilities]

Permissions:
- [resource].[action]: Required roles: [role1, role2]
- [resource].[action]: Required roles: [role3]

Access Rules:
- Rule 1: [role] can [action] if [condition]
- Rule 2: Owner can always [action] on own [resource]
- Rule 3: Public [resources] allow [action] without authentication

Error Responses:
- 401 Unauthorized: Missing or invalid authentication
- 403 Forbidden: Insufficient permissions
- 404 Not Found: Hide existence of protected resources

Audit:
- Log: [What to log for access attempts]
- Alert: [Conditions for security alerts]
```

### 7. Event-Driven Pattern

**Pseudocode Indicators:**
```
on event:
  handle(event)
  trigger otherEvent
```

**Extract:**
- **Events:** Event types and payloads
- **Handlers:** Processing logic for each event
- **Publishers:** What generates events
- **Subscribers:** Who listens to events
- **Event Flow:** Cascading events

**Requirements Template:**
```
EV-[ID]: [Event Name]

Event Definition:
- Name: [event.name.format]
- Trigger: [What causes this event]
- Payload:
  ```json
  {
    "eventId": "uuid",
    "timestamp": "ISO8601",
    "data": {
      "field1": "type",
      "field2": "type"
    }
  }
  ```

Publishers:
- [System/Component]: Publishes when [condition]

Subscribers:
- [Handler1]: [What it does with the event]
- [Handler2]: [What it does with the event]

Processing:
- Delivery: [At-least-once, exactly-once, at-most-once]
- Ordering: [Guaranteed order or not]
- Retry: [Retry strategy for failures]
- Dead Letter: [What happens to failed events]

Cascading Events:
- On success: Trigger [next.event]
- On failure: Trigger [error.event]

Idempotency:
- Handler must be idempotent: [Yes/No]
- Deduplication: [Strategy using eventId or other key]
```

### 8. Caching Pattern

**Pseudocode Indicators:**
```
if cache.has(key):
  return cache.get(key)
result = fetchFromDatabase(key)
cache.set(key, result)
return result
```

**Extract:**
- **Cache Strategy:** What to cache
- **Cache Key:** How items are keyed
- **TTL:** How long cached
- **Invalidation:** When to clear cache
- **Fallback:** What happens on cache miss

**Requirements Template:**
```
CACHE-[ID]: [Data Type] Caching

Strategy: [Read-through, write-through, write-behind, cache-aside]

Cache Configuration:
- Storage: [In-memory, Redis, Memcached]
- Key Format: [Pattern for cache keys]
- TTL: [Time to live in seconds]
- Max Size: [Memory or entry limit]
- Eviction Policy: [LRU, LFU, FIFO]

Caching Rules:
- Cache: [What data to cache]
- Don't Cache: [What to exclude]
- Cache if: [Conditional caching rules]

Invalidation:
- On Update: [Clear affected cache entries]
- On Delete: [Remove from cache]
- On Schedule: [Periodic refresh]

Fallback:
- Cache miss: [Fetch from source]
- Cache error: [Bypass cache, use source]
- Stale data: [Serve stale or refresh]

Monitoring:
- Hit rate: [Target percentage]
- Miss rate: [Alert threshold]
- Memory usage: [Alert threshold]
```

### 9. Retry/Resilience Pattern

**Pseudocode Indicators:**
```
attempts = 0
while attempts < maxRetries:
  try:
    result = operation()
    return result
  catch error:
    attempts++
    wait exponentialBackoff(attempts)
```

**Extract:**
- **Retry Logic:** When and how to retry
- **Backoff Strategy:** Delay between retries
- **Max Attempts:** Retry limits
- **Circuit Breaker:** When to stop retrying
- **Fallback:** Alternative action on failure

**Requirements Template:**
```
RES-[ID]: [Operation] Resilience

Retry Configuration:
- Max Attempts: [Number]
- Backoff Strategy: [Fixed, Exponential, Jittered]
- Initial Delay: [Milliseconds]
- Max Delay: [Milliseconds]
- Multiplier: [For exponential backoff]

Retry Conditions:
- Retry on: [Transient errors, timeouts, 5xx responses]
- Don't retry on: [4xx errors, business logic errors]

Circuit Breaker:
- Failure Threshold: [Number of failures to open circuit]
- Timeout: [How long circuit stays open]
- Half-Open: [Test requests before fully closing]

Timeout:
- Operation Timeout: [Max time for single attempt]
- Total Timeout: [Max time including all retries]

Fallback:
- On exhausted retries: [Return default, cached value, or error]
- On circuit open: [Fast fail or return fallback]

Monitoring:
- Track: [Success rate, retry rate, circuit state]
- Alert: [When circuit opens, high retry rate]
```

### 10. Batch Processing Pattern

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
- **Batch Size:** Items per batch
- **Batch Processing:** How batches are handled
- **Triggers:** What causes batch processing
- **Partial Failures:** Handling of partial batch failures

**Requirements Template:**
```
BATCH-[ID]: [Operation] Batch Processing

Batch Configuration:
- Batch Size: [Number of items]
- Max Wait Time: [Process incomplete batch after duration]
- Max Batch Size: [Upper limit]

Batching Strategy:
- Collect: [How items are accumulated]
- Trigger: [Size threshold OR time threshold]
- Processing: [How batch is processed]

Error Handling:
- Partial Success: [Continue, rollback all, or mark failed items]
- Retry: [Retry entire batch or just failed items]
- Dead Letter: [Failed items moved to error queue]

Ordering:
- Maintain Order: [Yes/No within batch]
- Process Order: [Sequential or parallel]

Monitoring:
- Batch size: [Average and max]
- Processing time: [Per batch]
- Success rate: [Percentage of items processed]
```

## Usage Guide

**When analyzing pseudocode:**

1. **Identify Pattern:** Match code structure to patterns above
2. **Extract Elements:** Pull out pattern-specific components
3. **Generate Requirements:** Use appropriate template
4. **Add Context:** Include business rules and constraints
5. **Validate Completeness:** Ensure all code paths covered

**For Multiple Patterns:**
- Code often contains multiple patterns
- Extract requirements for each pattern
- Link related requirements
- Create hierarchy (workflow contains validation contains CRUD)

**Pattern Combinations:**
- Workflow + CRUD + Validation (common for business processes)
- Event + Retry + State Machine (common for async systems)
- Authentication + Authorization + CRUD (common for APIs)
