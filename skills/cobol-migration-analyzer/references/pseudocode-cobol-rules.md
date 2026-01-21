# COBOL Translation Rules

**Prerequisites**: Read [PSEUDOCODE-COMMON-RULES.md](PSEUDOCODE-COMMON-RULES.md) for syntax, naming, and structure.

---

## Division Mapping

| COBOL | Pseudocode Section |
| ------- | ------------------- |
| IDENTIFICATION DIVISION | Program Overview |
| ENVIRONMENT DIVISION | Data Structures (file controls) |
| DATA DIVISION | Data Structures |
| PROCEDURE DIVISION | Main Algorithm + Core Processing |

## Data Types

| COBOL | Pseudocode | Notes |
| ------- | ----------- |-------|
| `PIC 9(n)` | `INTEGER` | Unsigned numeric |
| `PIC S9(n)` | `INTEGER` | Signed numeric |
| `PIC 9(n)V9(m)` | `DECIMAL(n+m,m)` | Financial precision |
| `PIC S9(n)V9(m) COMP-3` | `DECIMAL(n+m,m)` | **Packed decimal - preserve exactly!** |
| `PIC S9(n) COMP` / `BINARY` | `INTEGER` | Binary storage |
| `PIC S9(n) COMP-1` | `FLOAT` | Single precision float |
| `PIC S9(n) COMP-2` | `DOUBLE` | Double precision float |
| `PIC X(n)` | `STRING[n]` | Character/alphanumeric |
| `PIC A(n)` | `STRING[n]` | Alphabetic only |
| `PIC N(n)` | `STRING[n]` | National/Unicode |
| `88 level` | `BOOLEAN` or `ENUM` | Condition names |
| `OCCURS n` | `ARRAY[n] OF TYPE` | Fixed arrays |
| `OCCURS n DEPENDING ON x` | `ARRAY[DYNAMIC] OF TYPE` | Variable-length arrays |
| `INDEX` | `INTEGER` | Table index (document 1-based) |

## Statement Mapping

| COBOL | Pseudocode |
| ------- | ----------- |
| `MOVE src TO dest` | `dest = src` |
| `MOVE CORRESPONDING src TO dest` | `dest = CopyMatchingFields(src, dest)` |
| `INITIALIZE record` | `record = InitializeToDefaults()` |
| `ADD x TO y` | `y = y + x` |
| `ADD x TO y GIVING z` | `z = x + y` |
| `SUBTRACT x FROM y` | `y = y - x` |
| `MULTIPLY x BY y` | `y = y * x` |
| `DIVIDE x INTO y` | `y = y / x` |
| `DIVIDE x BY y GIVING z REMAINDER r` | `z = x / y; r = x MOD y` |
| `COMPUTE result = expr` | `result = expr` |
| `IF condition ... END-IF` | `IF condition THEN ... END IF` |
| `EVALUATE var ...` | `SWITCH var: CASE val: ... END SWITCH` |
| `PERFORM UNTIL cond` | `WHILE NOT cond DO` |
| `PERFORM VARYING i FROM x TO y` | `FOR i FROM x TO y DO` |
| `PERFORM n TIMES` | `FOR i FROM 1 TO n DO` |
| `OPEN I-O file` | `file = OPEN(path) FOR READING_AND_WRITING` |
| `OPEN EXTEND file` | `file = OPEN(path) FOR APPENDING` |
| `READ file AT END` | `TRY: record = READ_RECORD(file) CATCH EndOfFile:` |
| `READ file INTO ws-record` | `wsRecord = READ_RECORD(file)` |
| `READ file KEY IS key INVALID KEY` | `TRY: record = READ_BY_KEY(file, key) CATCH RecordNotFound:` |
| `WRITE record` | `WRITE_RECORD(file, record)` |
| `WRITE record FROM ws-record` | `WRITE_RECORD(file, wsRecord)` |
| `REWRITE record` | `UPDATE_RECORD(file, record)` |
| `DELETE file INVALID KEY` | `TRY: DELETE_RECORD(file) CATCH RecordNotFound:` |
| `START file KEY >= key` | `POSITION_FILE(file, key, GREATER_EQUALaragraphRange()` |
| `ACCEPT field` | `field = READ_INPUT()` |
| `ACCEPT field FROM DATE` | `field = CURRENT_DATE()` |
| `ACCEPT field FROM TIME` | `field = CURRENT_TIME()` |
| `DISPLAY text` | `DISPLAY text` |
| `STOP RUN` | `EXIT PROGRAM` |
| `GOBACK` | `RETURN` |
| `EXIT PARAGRAPH` | `RETURN FROM PROCEDURE` |

## File Operations

| CString Operations

| COBOL | Pseudocode |
| ------- | ----------- |
| `STRING s1 s2 DELIMITED BY SPACE INTO dest` | `dest = CONCATENATE(s1, s2, delimiter=' ')` |
| `UNSTRING source DELIMITED BY ',' INTO f1 f2` | `[f1, f2] = SPLIT(source, ',')` |
| `INSPECT text TALLYING count FOR ALL 'x'` | `count = COUNT_OCCURRENCES(text, 'x')` |
| `INSPECT text REPLACING ALL 'x' BY 'y'` | `text = REPLACE_ALL(text, 'x', 'y')` |
| `INSPECT text CONVERTING 'abc' TO 'xyz'` | `text = TRANSLATE(text, 'abc', 'xyz')` |

## Table/Array Operations

| COBOL | Pseudocode |
| ------- | ----------- |
| `SEARCH table AT END ... WHEN cond ...` | `FOR i FROM 1 TO tableSize: IF cond THEN ... END FOR` |
| `SEARCH ALL table AT END ... WHEN key = val` | `index = BINARY_SEARCH(table, key, val)` |
| `SET index TO 1` | `index = 1` |
| `SET index UP BY 1` | `index = index + 1` |
| `SET index DOWN BY 1` | `index = index - 1` |

## Program Control

| COBOL | Pseudocode |
| ------- | ----------- |
| `CALL 'SUBPROG' USING param1 param2` | `CALL ExternalProgram('SUBPROG', param1, param2)` |
| `CALL subprog USING BY REFERENCE param` | `CALL Subprog(REF param)` |
| `CALL subprog USING BY CONTENT param` | `CALL Subprog(COPY param)` |
| `CALL subprog ON EXCEPTION ...` | `TRY: CALL Subprog() CATCH ProgramError:` |
| `CANCEL 'SUBPROG'` | `UNLOAD_PROGRAM('SUBPROG')` |

## Translation Patterns

### REDEFINES → Union/Overlay

```cobol
01 DATA-FIELD PIC X(10).
01 DATA-NUMERIC REDEFINES DATA-FIELD PIC 9(10).
```

→

```
STRUCTURE DataOverlay:
    UNION:
        dataField: STRING[10]
        dataNumeric: INTEGER  // Same memory location
    END UNION
END STRUCTURE
// Document: dataField and dataNumeric share same storage
```

### OCCURS DEPENDING ON → Dynamic Array

```cobol
01 TABLE-RECORD.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 DEPENDING ON ITEM-COUNT.
      10 ITEM-CODE PIC X(5).
```

→

```
STRUCTURE TableRecord:
    itemCount: INTEGER, 30=I/O error)
5. **ROUNDED**: Use `ROUND(expr, decimals)` with HALF_UP
6. **REDEFINES**: Document memory overlay with UNION or comments
7. **Group moves**: `MOVE group1 TO group2` copies entire structure by bytes
8. **Numeric moves**: Automatic alignment and truncation - document conversions
9. **Reference modification**: `field(start:length)` → `SUBSTRING(field, start, length)`
10. **CORRESPONDING**: Only moves fields with matching names
11. **Nested programs**: Map to nested procedures or separate modules
12. **GOBACK vs STOP RUN**: GOBACK returns to caller, STOP RUN terminates
13. **BY REFERENCE vs BY CONTENT**: Document parameter passing modes
14. **USAGE differences**: DISPLAY (character), COMP (binary), COMP-3 (packed)
15.Special Considerations

### Working-Storage vs Linkage Section

```cobol
WORKING-STORAGE SECTION.  → Local variables/data structures
LINKAGE SECTION.          → Parameters passed to program
```

→ Document parameter lists separately from local variables

### Picture Editing Characters

| COBOL Picture | Meaning | Pseudocode Note |
| ------------- | ------- | --------------- |
| `PIC Z(5)9` | Zero suppression | Format for display only |
| `PIC $(5)9.99` | Floating currency | Format for display only |
| `PIC 9(5).99CR` | Credit indicator | Format for display only |
| `PIC +9(5).99` | Sign indicator | Format for display only |

→ Use `FORMAT(value, pattern)` for display logic

### Date Handling

```cobol
ACCEPT WS-DATE FROM DATE  → WS-DATE = 'YYMMDD'
ACCEPT WS-DATE FROM DATE YYYYMMDD → WS-DATE = 'YYYYMMDD'
ACCEPT WS-DATETIME FROM DATETIME → 'YYYYMMDDHHMMSSnnnnnn'
```

→ Use `CURRENT_DATE()` with format specification

### SORT/MERGE Operations

```cobol
SORT SORT-FILE
   ON ASCENDING KEY SORT-KEY
   USING INPUT-FILE
   GIVING OUTPUT-FILE
```

→

```
SORT_FILE(inputFile, outputFile, 
          sortKeys=[{field: 'sortKey', order: ASCENDING}])
```

## Translation Workflow

1. Map IDENTIFICATION → Program Overview
2. Extract DATA DIVISION → Data Structures (preserve COMP-3!)
3. Identify copybooks → Document includes
4. Map 88-levels → ENUMs/BOOLEANs
5. Convert paragraphs → Procedures  
6. Translate PROCEDURE DIVISION → Main Algorithm
7. Handle special cases (REDEFINES, ODO, SEARCH)
8. Generate Mermaid flowchart
9. Document implicit conversions
10. Verify precision preserved
11. Document file status codes and error handling

## Common Gotchas

1. **Implicit type conversions**: Make all numeric↔string conversions explicit
2. **Truncation in moves**: Document when data may be truncated/padded
3. **PERFORM ranges**: Map THRU carefully - includes all paragraphs in range
4. **GO TO elimination**: Restructure as procedures/loops with proper exits
5. **CALL by reference**: All parameters shared unless BY CONTENT specified
6. **Index vs subscript**: INDEX is optimized pointer, subscript is computed offset
7. **Condition names**: Single 88-level may test multiple values (VALUE 'A' 'B' 'C')
8. **Computational fields**: Different USAGE affects storage and performance
9. **Sign handling**: SIGN LEADING/TRAILING SEPARATE affects storage format
10. **Floating-point**: COMP-1/COMP-2 NOT suitable for financial - document risks
EVALUATE TRUE
   WHEN STATUS-CODE = '00'
      PERFORM SUCCESS-ROUTINE
   WHEN STATUS-CODE = '10'
      PERFORM EOF-ROUTINE
   WHEN OTHER
      PERFORM ERROR-ROUTINE
END-EVALUATE

```

→

```

SWITCH statusCode:
    CASE '00':
        CALL SuccessRoutine()
        BREAK
    CASE '10':
        CALL EofRoutine()
        BREAK
    DEFAULT:
        CALL ErrorRoutine()
END SWITCH

```

### SEARCH → Linear Search

```cobol
SET IDX TO 1
SEARCH TABLE-ENTRY
   AT END PERFORM NOT-FOUND
   WHEN KEY-FIELD(IDX) = SEARCH-KEY
      PERFORM FOUND-ROUTINE
END-SEARCH
```

→

```
found = FALSE
FOR idx FROM 1 TO tableSize DO
    IF keyField[idx] == searchKey THEN
        CALL FoundRoutine()
        found = TRUE
        BREAK
    END IF
END FOR
IF NOT found THEN
    CALL NotFound()
END IF
```

### SEARCH ALL → Binary Search

```cobol
SEARCH ALL TABLE-ENTRY
   AT END PERFORM NOT-FOUND
   WHEN KEY-FIELD(IDX) = SEARCH-KEY
      PERFORM FOUND-ROUTINE
END-SEARCH
```

→

```
idx = BINARY_SEARCH(tableEntry, 'keyField', searchKey)
IF idx > 0 THEN
    CALL FoundRoutine()
ELSE
    CALL NotFound()
END IF
// Note: Table must be sorted on keyField
```

### COPY/Copybook → Include

```cobol
COPY CUSTOMER-REC.
```

→

```
INCLUDE "CustomerRecord.pseudo"
// Or inline the structure definition with comment:
// FROM COPYBOOK: CUSTOMER-REC
STRUCTURE CustomerRecord:
    ...
END STRUCTURE
```

### PERFORM THRU → Procedure Sequence

```cobol
PERFORM 1000-INIT THRU 1000-INIT-EXIT.
```

→

```
PROCEDURE InitSequence()  // Represents 1000-INIT through 1000-INIT-EXIT
BEGIN
    // All logic from 1000-INIT paragraph
    // ... through 1000-INIT-EXIT paragraph
END PROCEDURE
CALL InitSequence()
```

| ------- | ----------- |
| `OPEN INPUT file` | `file = OPEN(path) FOR READING` |
| `OPEN OUTPUT file` | `file = OPEN(path) FOR WRITING` |
| `READ file AT END` | `TRY: record = READ_RECORD(file) CATCH EndOfFile:` |
| `WRITE record` | `WRITE_RECORD(file, record)` |
| `CLOSE file` | `CLOSE(file)` |

## Translation Patterns

## 88-Level → ENUM

```cobol
01 RECORD-TYPE PIC X.
   88 TYPE-HEADER VALUE 'H'.
IF TYPE-HEADER THEN ...
```

→

```
ENUM RecordType: HEADER='H', DETAIL='D' END ENUM
IF recordType == RecordType.HEADER THEN ...
```

### Paragraph → Procedure

```cobol
CALC-TOTAL.
    COMPUTE TOTAL = QTY * PRICE.
PERFORM CALC-TOTAL.
```

→

```
PROCEDURE CalcTotal()
BEGIN
    total = qty * price
END PROCEDURE
CALL CalcTotal()
```

### File Loop

```cobol
PERFORM UNTIL eof = 'Y'
    READ file AT END MOVE 'Y' TO eof
    NOT AT END PERFORM PROCESS-REC
    END-READ
END-PERFORM
```

→

```
eof = FALSE
WHILE NOT eof DO
    TRY:
        record = READ_RECORD(file)
        CALL ProcessRecord(record)
    CATCH EndOfFile:
        eof = TRUE
    END TRY
END WHILE
```

## Critical Rules

1. **1-based indexing**: COBOL starts at 1 → document in comments
2. **COMP-3 precision**: MUST preserve exactly using DECIMAL(n,m)
3. **88-levels**: Convert to ENUM or BOOLEAN
4. **File status**: Map to exceptions (00=OK, 10=EOF, 23=not found)
5. **ROUNDED**: Use `ROUND(expr, decimals)` with HALF_UP

## Translation Workflow

1. Map IDENTIFICATION → Program Overview
2. Extract DATA DIVISION → Data Structures (preserve COMP-3!)
3. Convert paragraphs → Procedures  
4. Translate PROCEDURE DIVISION
5. Generate Mermaid flowchart
6. Verify precision preserved

**Reference**: ISO/IEC 1989:2014 (COBOL Standard)
