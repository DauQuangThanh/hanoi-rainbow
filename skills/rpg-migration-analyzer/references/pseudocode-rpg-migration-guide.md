]633;E;cat /tmp/rpg-migration.txt >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "---" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md;a450241c-8948-4114-8aa1-a76bbe99bad9]633;C# RPG Migration Guide

**Prerequisites**: Read all other RPG translation rule files first.

---

This file provides comprehensive guidance for planning and executing RPG to modern language migrations.

## Advanced Translation Patterns

### Refactoring Global Indicators

```rpg
// Legacy: Global indicators throughout program
C                   EVAL      *IN01 = *ON
C                   IF        *IN01
C                   EXSR      Process
C                   ENDIF
```

→

```pseudocode
// Better: Named boolean at appropriate scope
isRecordFound: BOOLEAN = TRUE

IF isRecordFound THEN
    CALL Process()
END IF

// Best practices:
// - Use descriptive names
// - Limit scope (local vs global)
// - Consider state objects for complex indicator sets
```

### Refactoring %PARMS Checks

```rpg
D MyProc          PR
D   Required                    10A
D   Optional1                   10A   OPTIONS(*NOPASS)
D   Optional2                   10A   OPTIONS(*NOPASS)

P MyProc          B
C                   SELECT
C                   WHEN      %PARMS = 1
C                   // Use only Required
C                   WHEN      %PARMS = 2
C                   // Use Required and Optional1
C                   WHEN      %PARMS = 3
C                   // Use all parameters
C                   ENDSL
P MyProc          E
```

→

```pseudocode
FUNCTION MyProc(
    required: STRING[10],
    optional1: OPTIONAL STRING[10] = NULL,
    optional2: OPTIONAL STRING[10] = NULL
)
BEGIN
    // Use null checks instead of parameter count
    IF optional1 IS NOT NULL THEN
        // Process optional1
    END IF
    
    IF optional2 IS NOT NULL THEN
        // Process optional2
    END IF
END FUNCTION

// Modern approach: Use optional parameters with defaults
// or method overloading in target language
```

### Converting Fixed-Format to Free-Format Logic

```rpg
// Legacy fixed-format
C     CustNo        CHAIN     CustMast
C                   IF        %FOUND
C                   EVAL      Name = CustName
C                   ENDIF
```

→

```pseudocode
record = READ_BY_KEY(custMast, custNo)
IF RECORD_FOUND(custMast) THEN
    name = record.custName
END IF

// Note: Free-format RPG and pseudocode are already similar
// Focus on extracting business logic from I/O operations
```

## Translation Workflow

1. **Analyze Program Structure**
   - Identify H-spec compiler directives (ACTGRP, BNDDIR, NOMAIN)
   - Map F-spec files to data structures/interfaces
   - Catalog all D-spec definitions

2. **Extract Data Definitions**
   - Convert standalone fields (preserve packed decimal precision!)
   - Transform data structures (handle QUALIFIED, LIKEDS, OVERLAY)
   - Document multiple occurrence DS as arrays
   - Identify special data structures (PSDS, INFDS, DTAARA)

3. **Convert Control Flow**
   - Map BEGSR/ENDSR → Named procedures
   - Convert P-spec procedures → Functions with prototypes
   - Translate parameter lists (PLIST/PARM)

4. **Translate Operations**
   - C-spec calculations → Pseudocode expressions
   - File I/O operations → Standard file functions
   - Embedded SQL → Database query patterns
   - Error handling → TRY-CATCH blocks
   - Indicator logic → Boolean variables

5. **Handle Special Cases**
   - API calls → Function interfaces
   - Data area operations → Persistent storage
   - Commitment control → Transaction management
   - Output specs → Report formatting logic

6. **Generate Documentation**
   - Create Mermaid flowchart
   - Document business rules
   - Add example traces
   - Note integration points

7. **Verification**
   - Verify decimal precision preserved
   - Confirm error handling coverage
   - Validate date/time conversions
   - Check array bounds handling
   - Test IFS and web service integrations
   - Validate transaction boundaries

## Migration Best Practices

### 1. Decimal Precision Strategy

```pseudocode
// CRITICAL: RPG packed decimals must map to exact precision types
// RPG: 15P 2 → Target: DECIMAL(15,2) or BigDecimal
// NEVER use: float, double for financial data

CONSTANTS:
    ROUNDING_MODE = HALF_UP  // RPG default rounding
END CONSTANTS

FUNCTION CalculateTax(amount: DECIMAL(15,2), rate: DECIMAL(5,3)) RETURNS DECIMAL(15,2)
BEGIN
    result: DECIMAL(18,5)
    result = amount * rate
    RETURN ROUND(result, 2, ROUNDING_MODE)
END FUNCTION
```

### 2. Indicator Refactoring Strategy

```pseudocode
// Phase 1: Direct translation (indicators → booleans)
indicator01: BOOLEAN  // EOF
indicator02: BOOLEAN  // Record found
indicator03: BOOLEAN  // Error occurred

// Phase 2: Semantic naming
endOfFile: BOOLEAN
recordFound: BOOLEAN
errorOccurred: BOOLEAN

// Phase 3: Encapsulation (for complex programs)
STRUCTURE ProgramState:
    endOfFile: BOOLEAN
    recordFound: BOOLEAN
    errorOccurred: BOOLEAN
    // ... other state
END STRUCTURE

// Phase 4: Eliminate where possible
// Replace indicator checks with direct return values or exceptions
```

### 3. File I/O Abstraction

```pseudocode
// Abstraction layer for file operations
INTERFACE FileOperations:
    FUNCTION ReadRecord(key: STRING) RETURNS Record
    FUNCTION UpdateRecord(record: Record) RETURNS BOOLEAN
    FUNCTION DeleteRecord(key: STRING) RETURNS BOOLEAN
END INTERFACE

// Implementation can be:
// - Database (most common for RPG files)
// - REST API
// - Message queue
// - Legacy file system

CLASS DatabaseFileOperations IMPLEMENTS FileOperations:
    FUNCTION ReadRecord(key: STRING) RETURNS Record
    BEGIN
        EXECUTE SQL
            SELECT * INTO :record FROM TABLE WHERE KEY = :key
        END SQL
        RETURN record
    END FUNCTION
END CLASS
```

### 4. Error Handling Modernization

```pseudocode
// RPG pattern: Indicators and status codes
// Modern pattern: Exceptions with context

STRUCTURE FileOperationException EXTENDS Exception:
    fileName: STRING
    operation: STRING
    statusCode: INTEGER
    recordKey: STRING
END STRUCTURE

FUNCTION ReadCustomer(custNo: DECIMAL(10,0)) RETURNS Customer
BEGIN
    TRY:
        record = READ_BY_KEY(custMast, custNo)
        IF NOT RECORD_FOUND(custMast) THEN
            THROW NEW RecordNotFoundException(
                MESSAGE="Customer not found",
                KEY=custNo
            )
        END IF
        RETURN record
    CATCH DatabaseException AS e:
        THROW NEW FileOperationException(
            MESSAGE="Failed to read customer",
            FILE="CUSTMAST",
            OPERATION="READ",
            KEY=custNo,
            CAUSE=e
        )
    END TRY
END FUNCTION
```

### 5. Transaction Pattern

```pseudocode
// Preserve RPG commitment control in modern transactions

FUNCTION ProcessOrder(order: Order) RETURNS BOOLEAN
BEGIN
    TRY:
        BEGIN_TRANSACTION()
        
        // Update inventory
        inventory = READ_BY_KEY_WITH_LOCK(invFile, order.itemNo)
        inventory.quantity = inventory.quantity - order.quantity
        UPDATE_RECORD(invFile, inventory)
        
        // Create order record
        WRITE_RECORD(orderFile, order)
        
        // Update customer balance
        customer = READ_BY_KEY_WITH_LOCK(custFile, order.custNo)
        customer.balance = customer.balance + order.total
        UPDATE_RECORD(custFile, customer)
        
        COMMIT_TRANSACTION()
        RETURN TRUE
        
    CATCH Exception AS e:
        ROLLBACK_TRANSACTION()
        LOG_ERROR("Order processing failed", e)
        RETURN FALSE
    END TRY
END FUNCTION
```

### 6. Testing Strategy

```pseudocode
// Unit test template for migrated RPG logic

TEST_SUITE CustomerProcessing:
    
    SETUP:
        // Initialize test database
        testDb = CREATE_TEST_DATABASE()
        // Load test data
        LOAD_TEST_DATA("test-customers.sql")
    END SETUP
    
    TEST CalculateDiscount_StandardCustomer:
        // Given
        customer = NEW Customer(type="STANDARD", balance=1000.00)
        orderAmount: DECIMAL(15,2) = 500.00
        
        // When
        discount = CalculateDiscount(customer, orderAmount)
        
        // Then
        ASSERT_EQUALS(discount, 25.00)  // 5% discount
        ASSERT_PRECISION(discount, 2)    // Verify decimal places
    END TEST
    
    TEST ProcessOrder_InsufficientInventory:
        // Given
        order = NEW Order(itemNo="ITEM001", quantity=100)
        inventory = NEW Inventory(itemNo="ITEM001", quantity=50)
        
        // When/Then
        ASSERT_THROWS(InsufficientInventoryException, 
                     ProcessOrder(order))
    END TEST
    
    TEARDOWN:
        testDb.CLOSE()
    END TEARDOWN
    
END TEST_SUITE
```

### 7. Performance Considerations

```pseudocode
// Maintain RPG batch processing efficiency

// Pattern 1: Bulk read and process
FUNCTION ProcessDailyTransactions() 
BEGIN
    BATCH_SIZE = 1000
    transactions: ARRAY OF Transaction
    
    // Use cursor or streaming for large datasets
    CURSOR txCursor FOR
        SELECT * FROM TRANSACTIONS 
        WHERE PROCESS_DATE = CURRENT_DATE
        ORDER BY TRANSACTION_TIME
    END CURSOR
    
    OPEN_CURSOR(txCursor)
    transactions = FETCH_BATCH(txCursor, BATCH_SIZE)
    
    WHILE SIZE(transactions) > 0 DO
        // Process batch
        FOR EACH tx IN transactions DO
            CALL ProcessTransaction(tx)
        END FOR
        
        // Commit batch
        COMMIT_TRANSACTION()
        
        // Fetch next batch
        transactions = FETCH_BATCH(txCursor, BATCH_SIZE)
    END WHILE
    
    CLOSE_CURSOR(txCursor)
END FUNCTION

// Pattern 2: Parallel processing (where appropriate)
FUNCTION ProcessBatchParallel(records: ARRAY OF Record)
BEGIN
    // Split into chunks
    chunks = SPLIT_INTO_CHUNKS(records, THREAD_COUNT)
    
    // Process in parallel
    PARALLEL_FOR_EACH chunk IN chunks DO
        FOR EACH record IN chunk DO
            CALL ProcessRecord(record)
        END FOR
    END PARALLEL_FOR_EACH
END FUNCTION
```

## Common Migration Challenges

### Challenge 1: MOVE/MOVEL Operations

```pseudocode
// RPG MOVE is right-aligned, MOVEL is left-aligned
// Must preserve padding behavior

FUNCTION RPG_MOVE(source: STRING, targetLength: INTEGER) RETURNS STRING
BEGIN
    // Right-align and pad with spaces on left
    IF LENGTH(source) >= targetLength THEN
        RETURN SUBSTRING(source, LENGTH(source) - targetLength + 1, targetLength)
    ELSE
        padding = REPEAT(" ", targetLength - LENGTH(source))
        RETURN padding + source
    END IF
END FUNCTION

FUNCTION RPG_MOVEL(source: STRING, targetLength: INTEGER) RETURNS STRING
BEGIN
    // Left-align and pad with spaces on right
    IF LENGTH(source) >= targetLength THEN
        RETURN SUBSTRING(source, 1, targetLength)
    ELSE
        padding = REPEAT(" ", targetLength - LENGTH(source))
        RETURN source + padding
    END IF
END FUNCTION
```

### Challenge 2: Overlay and Shared Memory

```pseudocode
// RPG OVERLAY shares memory space
// Must preserve data relationships

STRUCTURE DateStructure:
    fullDate: STRING[8]      // YYYYMMDD
    year: COMPUTED_FIELD     // Positions 1-4
    month: COMPUTED_FIELD    // Positions 5-6
    day: COMPUTED_FIELD      // Positions 7-8
END STRUCTURE

// Implementation with computed properties
CLASS DateStructure:
    PRIVATE fullDate: STRING[8]
    
    PROPERTY Year:
        GET: RETURN SUBSTRING(fullDate, 1, 4)
        SET: fullDate = value + SUBSTRING(fullDate, 5, 4)
    END PROPERTY
    
    PROPERTY Month:
        GET: RETURN SUBSTRING(fullDate, 5, 2)
        SET: fullDate = SUBSTRING(fullDate, 1, 4) + value + 
                       SUBSTRING(fullDate, 7, 2)
    END PROPERTY
    
    PROPERTY Day:
        GET: RETURN SUBSTRING(fullDate, 7, 2)
        SET: fullDate = SUBSTRING(fullDate, 1, 6) + value
    END PROPERTY
END CLASS
```

### Challenge 3: Multiple Occurrence Data Structures

```pseudocode
// RPG: Multiple occurrence DS with %OCCUR
// Modern: Array with explicit indexing

// RPG concept:
// LineItem DS OCCURS(99)
// %OCCUR(LineItem) = 5  // Set to 5th occurrence

// Modern equivalent:
lineItems: ARRAY[99] OF LineItem
currentLineIndex: INTEGER = 5
currentLine = lineItems[currentLineIndex]

// Better: Use collections with iteration
lineItems: LIST OF LineItem
FOR EACH item IN lineItems DO
    CALL ProcessLineItem(item)
END FOR
```

### Challenge 4: *ALL and Special Value Comparisons

```pseudocode
// RPG special values need careful translation

// *BLANK/*BLANKS
IF field = EMPTY_STRING OR TRIM(field) = "" THEN ...

// *ZERO/*ZEROS
IF field = 0 THEN ...

// *HIVAL (highest value for type)
IF field >= MAX_VALUE_FOR_TYPE THEN ...

// *LOVAL (lowest value for type)
IF field <= MIN_VALUE_FOR_TYPE THEN ...

// *ALL'X' (all positions contain 'X')
IF field = REPEAT('X', LENGTH(field)) THEN ...
```

## Documentation Template

When documenting migrated RPG programs, include:

```markdown
# [PROGRAM-NAME] - Description

## Original RPG Information
- **Original Source**: [Path/Library/Program]
- **RPG Version**: [RPG III/RPG IV/ILE RPG]
- **Compile Date**: [Date]
- **Dependencies**: [Called programs, files, procedures]

## Migration Information
- **Migration Date**: [Date]
- **Target Platform**: [Java/Python/C#/etc.]
- **Migrated By**: [Name/Team]
- **Verification Status**: [Unit Tested/Integration Tested/Production Ready]

## Program Overview
- **Purpose**: [What the program does]
- **Trigger**: [Batch job/Online/API/Event]
- **Frequency**: [Daily/Real-time/On-demand]
- **Input**: [Files, parameters, databases]
- **Output**: [Files, reports, databases, messages]

## Data Structures
[All structures with field definitions and RPG equivalents]

## Core Business Logic
[Key procedures/functions with pseudocode]

## Translation Notes
[Special cases, assumptions, deviations from original]

## Testing Notes
[Test cases, edge cases, known issues]

## Performance Benchmarks
[RPG vs. migrated performance comparison if available]
```

**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide

---

**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide
