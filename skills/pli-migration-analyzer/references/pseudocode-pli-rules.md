# PL/I Translation Rules

**Prerequisites**: Read [pseudocode-common-rules.md](pseudocode-common-rules.md) for syntax, naming, and structure.

---

## Structure Mapping

| PL/I Element | Pseudocode Section |
| -------------- | ------------------- |
| `PROCEDURE OPTIONS(MAIN)` | Program Overview + Main Algorithm |
| `DECLARE` / `DCL` | Data Structures |
| `DCL file FILE` | Data Structures (file controls) |
| Procedures | Core Processing Logic |
| `ON condition` | Error Handling |

## Data Types

| PL/I | Pseudocode | Notes |
| ------ | ----------- |-------|
| `FIXED DECIMAL(n,m)` | `DECIMAL(n,m)` | **Preserve precision!** |
| `FIXED BINARY(n)` | `INTEGER` | Binary integer |
| `FLOAT DECIMAL(n)` | ⚠️ Use `DECIMAL` | Financial: never float! |
| `CHARACTER(n)` | `STRING[n]` | Fixed string |
| `BIT(n)` | `BOOLEAN` (if n=1) | Bit string |
| `POINTER` | Reference/Object | Memory pointer |
| Arrays `(n)` | `ARRAY[n] OF TYPE` | 1-based indexing |
| `VARYING` | `STRING` | Variable-length |

## Statement Mapping

| PL/I | Pseudocode |
| ------ | ----------- |
| `a = b;` | `a = b` |
| `IF condition THEN ... ELSE` | `IF condition THEN ... ELSE` |
| `DO WHILE(condition);` | `WHILE condition DO` |
| `DO i = 1 TO n;` | `FOR i FROM 1 TO n DO` |
| `CALL proc(args);` | `CALL ProcName(args)` |
| `RETURN(value);` | `RETURN value` |
| `GO TO label;` | Refactor to structured flow |
| `SELECT; WHEN ... END;` | `SWITCH ... CASE ... END SWITCH` |

## Built-in Functions

| PL/I BIF | Pseudocode | Notes |
| ---------- | ----------- |-------|
| `SUBSTR(str,pos,len)` | `SUBSTRING(str, pos, len)` | 1-based position |
| `INDEX(str,search)` | `FIND(str, search)` | Returns position or 0 |
| `LENGTH(str)` | `LENGTH(str)` | String length |
| `TRIM(str)` | `TRIM(str)` | Remove trailing blanks |
| `ROUND(num,dec)` | `ROUND(num, dec)` | Round to decimals |
| `MOD(a,b)` | `a MOD b` | Modulo operation |
| `MAX(a,b,...)` | `MAX(a, b, ...)` | Maximum value |
| `MIN(a,b,...)` | `MIN(a, b, ...)` | Minimum value |
| `ABS(num)` | `ABS(num)` | Absolute value |
| `CEIL(num)` | `CEILING(num)` | Round up |
| `FLOOR(num)` | `FLOOR(num)` | Round down |
| `DATE()` | `CURRENT_DATE()` | Current date |
| `TIME()` | `CURRENT_TIME()` | Current time |
| `DATETIME()` | `CURRENT_DATETIME()` | Current timestamp |
| `VERIFY(str,valid)` | `VALIDATE_CHARS(str, valid)` | Check valid characters |
| `TRANSLATE(str,to,from)` | `REPLACE_CHARS(str, to, from)` | Character translation |
| `REPEAT(str,n)` | `REPEAT(str, n)` | Repeat string |
| `REVERSE(str)` | `REVERSE(str)` | Reverse string |
| `UNSPEC(var)` | Bitwise representation | Low-level bit access |

## Translation Patterns

## Procedure → Function

```pli
CALC_TOTAL: PROCEDURE(qty, price) RETURNS(FIXED DECIMAL(15,2));
    DCL qty FIXED DECIMAL(7,2);
    DCL price FIXED DECIMAL(9,2);
    RETURN(qty * price);
END CALC_TOTAL;
```

→

```
FUNCTION CalcTotal(qty: DECIMAL(7,2), price: DECIMAL(9,2)): DECIMAL(15,2)
BEGIN
    RETURN qty * price
END FUNCTION
```

### ON Condition → TRY-CATCH

```pli
ON ENDFILE(infile) EOF = '1'B;
READ FILE(infile) INTO(rec);
```

→

```
TRY
    record = READ_RECORD(inFile)
CATCH EndOfFile
    eof = TRUE
END TRY
```

### Structure Declaration

```pli
DCL 1 EMPLOYEE,
      2 ID CHAR(10),
      2 NAME CHAR(50),
      2 SALARY FIXED DECIMAL(9,2);
```

→

```
STRUCTURE Employee
    id: STRING[10]
    name: STRING[50]
    salary: DECIMAL(9,2)
END STRUCTURE
```

### File Processing Loop

```pli
ON ENDFILE(INFILE) EOF = '1'B;
OPEN FILE(INFILE) INPUT;
EOF = '0'B;
DO WHILE(EOF = '0'B);
    READ FILE(INFILE) INTO(RECORD);
    IF EOF = '0'B THEN
        CALL PROCESS_RECORD(RECORD);
END;
CLOSE FILE(INFILE);
```

→

```
PROCEDURE ProcessFile(filePath: STRING)
BEGIN
    file = OPEN(filePath) FOR READING
    
    WHILE NOT END_OF_FILE(file) DO
        TRY
            record = READ_RECORD(file)
            CALL ProcessRecord(record)
        CATCH EndOfFile
            BREAK
        END TRY
    END WHILE
    
    CLOSE(file)
END PROCEDURE
```

### DO UNTIL Loop

```pli
DO UNTIL(condition);
    statements;
END;
```

→

```
REPEAT
    statements
UNTIL condition
```

### SELECT/WHEN → SWITCH/CASE

```pli
SELECT;
    WHEN (code = 'A') result = 10;
    WHEN (code = 'B') result = 20;
    WHEN (code = 'C') result = 30;
    OTHERWISE result = 0;
END;
```

→

```
SWITCH code
    CASE 'A': result = 10; BREAK
    CASE 'B': result = 20; BREAK
    CASE 'C': result = 30; BREAK
    DEFAULT: result = 0
END SWITCH
```

### Array Operations

```pli
DCL TOTALS(12) FIXED DECIMAL(15,2);
DO I = 1 TO 12;
    TOTALS(I) = 0;
END;
```

→

```
totals: ARRAY[12] OF DECIMAL(15,2)
FOR i FROM 1 TO 12 DO
    totals[i] = 0
END FOR
```

### String Concatenation

```pli
FULL_NAME = TRIM(FIRST_NAME) || ' ' || TRIM(LAST_NAME);
```

→

```
fullName = TRIM(firstName) + ' ' + TRIM(lastName)
```

### Nested Structures

```pli
DCL 1 ORDER,
      2 ORDER_ID CHAR(10),
      2 CUSTOMER,
        3 CUST_ID CHAR(8),
        3 CUST_NAME CHAR(50),
      2 AMOUNT FIXED DECIMAL(15,2);
```

→

```
STRUCTURE Customer
    custId: STRING[8]
    custName: STRING[50]
END STRUCTURE

STRUCTURE Order
    orderId: STRING[10]
    customer: Customer
    amount: DECIMAL(15,2)
END STRUCTURE
```

### Entry Parameters

```pli
CALC_TAX: PROCEDURE(amount, rate) RETURNS(FIXED DECIMAL(15,2));
    DCL amount FIXED DECIMAL(15,2);
    DCL rate FIXED DECIMAL(5,4);
    RETURN(amount * rate);
END CALC_TAX;
```

→

```
FUNCTION CalcTax(amount: DECIMAL(15,2), rate: DECIMAL(5,4)): DECIMAL(15,2)
BEGIN
    RETURN amount * rate
END FUNCTION
```

## Critical Rules

1. **1-based indexing**: PL/I arrays start at 1 (preserve or document)
2. **FIXED DECIMAL**: MUST map to DECIMAL with exact precision
3. **FLOAT**: Prohibited for financial calculations - always use DECIMAL
4. **ON conditions**: Map to TRY-CATCH blocks
5. **GO TO**: Refactor to structured control flow
6. **Bit strings**: Convert '1'B/'0'B to TRUE/FALSE8. **Substring positions**: PL/I uses 1-based indexing (document conversion to 0-based if needed)
9. **File buffers**: LOCATE allocates buffer - map to appropriate memory management
10. **Concatenation**: `||` operator maps to `+` or CONCAT()

## Common PL/I Idioms

### Initialization Pattern
```pli
DCL counter FIXED BIN(31) INIT(0);
```
→
```
counter: INTEGER = 0
```

### Null String Check
```pli
IF name = '' THEN ...
```
→
```
IF name = '' OR name = NULL THEN ...
```

### Numeric Validation
```pli
ON CONVERSION SNAP;
value = input_string;
REVERT CONVERSION;
```
→
```
TRY
    value = CONVERT_TO_NUMBER(inputString)
CATCH ConversionError
    // Handle invalid input
END TRY
```

### Counter Increment
```pli
count = count + 1;
```
→
```
count = count + 1  // or INCREMENT(count)
```

### Boolean Toggle
```pli
flag = ¬flag;  /* or */ flag = ^flag;
```
→
```
flag = NOT flag
```

### Range Check
```pli
IF value >= MIN_VALUE & value <= MAX_VALUE THEN ...
```
→
```
IF value >= MIN_VALUE AND value <= MAX_VALUE THEN ...
```

### Packed Decimal Precision
```pli
DCL price FIXED DECIMAL(9,2);  /* 999999999.99 */
```
→
```
price: DECIMAL(9,2)  // CRITICAL: Preserve exact precision!
```
## Translation Workflow

### Step-by-Step Process

1. **Identify Program Structure**
   - Locate `PROCEDURE OPTIONS(MAIN)` → Main entry point
   - List all nested procedures → Subroutines
   - Map %INCLUDE directives → Dependencies

2. **Extract Data Declarations**
   - Convert all DCL/DECLARE → Data Structures
   - ⚠️ **CRITICAL**: Preserve FIXED DECIMAL(n,m) precision exactly
   - Map FILE declarations → File definitions
   - Identify constants (INIT values)

3. **Translate Control Flow**
   - Map DO WHILE/DO UNTIL → WHILE/REPEAT loops
   - Convert SELECT/WHEN → SWITCH/CASE
   - Refactor GO TO → Structured flow
   - Preserve loop counters (1-based indexing)

4. **Convert Exception Handling**
   - Map ON ENDFILE → TRY-CATCH EndOfFile
   - Convert ON ERROR → General exception handling
   - Translate ON CONVERSION, OVERFLOW, etc. → Specific catches
   - Document REVERT statements

5. **Translate Procedures**
   - Convert PROCEDURE → FUNCTION (if RETURNS) or PROCEDURE
   - Map parameters → Typed parameters
   - Preserve return types with exact precision
   - Document ENTRY points if multiple

6. **Process File I/O**
   - OPEN FILE → OPEN with mode
   - READ/WRITE → Corresponding operations
   - Handle LOCATE buffer allocations
   - Map CLOSE operations

7. **Translate Built-in Functions**
   - Map all BIFs using table above
   - Document 1-based SUBSTR positions
   - Preserve rounding behavior

8. **Generate Documentation**
   - Create Program Overview section
   - Document all Data Structures
   - Write Main Algorithm (high-level)
   - Detail Core Processing Logic
   - Add Mermaid flowchart
   - Include Decision Logic and edge cases

9. **Validation Checklist**
   - ✅ All FIXED DECIMAL preserved (no float!)
   - ✅ 1-based indexing documented
   - ✅ File operations properly handled
   - ✅ Exception handling comprehensive
   - ✅ No GO TO statements (refactored)
   - ✅ All procedures translated
   - ✅ Bit strings converted to boolean
   - ✅ Flowchart matches logic

## Best Practices

1. **Always preserve numeric precision** - FIXED DECIMAL(n,m) must map to DECIMAL(n,m)
2. **Document indexing** - Note 1-based arrays if converting to 0-based
3. **Simplify nested structures** - Break complex hierarchies into clear types
4. **Eliminate GO TO** - Use structured programming (loops, functions)
5. **Handle all ON conditions** - Don't skip exception scenarios
6. **Test numeric calculations** - Verify precision in edge cases
7. **Document assumptions** - Note any PL/I-specific behaviors

**Reference**: IBM Enterprise PL/I for z/OS Language Reference
