# COBOL Translation Rules

**Prerequisites**: Read [PSEUDOCODE-COMMON-RULES.md](PSEUDOCODE-COMMON-RULES.md) for syntax, naming, and structure.

---

## Division Mapping

| COBOL | Pseudocode Section |
|-------|-------------------|
| IDENTIFICATION DIVISION | Program Overview |
| ENVIRONMENT DIVISION | Data Structures (file controls) |
| DATA DIVISION | Data Structures |
| PROCEDURE DIVISION | Main Algorithm + Core Processing |

## Data Types

| COBOL | Pseudocode | Notes |
|-------|-----------|-------|
| `PIC 9(n)` | `INTEGER` | |
| `PIC 9(n)V9(m)` | `DECIMAL(n+m,m)` | Financial precision |
| `PIC S9(n)V9(m) COMP-3` | `DECIMAL(n+m,m)` | **Packed decimal - preserve exactly!** |
| `PIC X(n)` | `STRING[n]` | Character |
| `88 level` | `BOOLEAN` or `ENUM` | Condition names |
| `OCCURS n` | `ARRAY[n] OF TYPE` | Tables |

## Statement Mapping

| COBOL | Pseudocode |
|-------|-----------|
| `MOVE src TO dest` | `dest = src` |
| `ADD x TO y` | `y = y + x` |
| `COMPUTE result = expr` | `result = expr` |
| `IF condition ... END-IF` | `IF condition THEN ... END IF` |
| `PERFORM UNTIL cond` | `WHILE NOT cond DO` |
| `PERFORM VARYING i FROM x TO y` | `FOR i FROM x TO y DO` |
| `PERFORM paragraph` | `CALL ProcedureName()` |
| `DISPLAY text` | `DISPLAY text` |

## File Operations

| COBOL | Pseudocode |
|-------|-----------|
| `OPEN INPUT file` | `file = OPEN(path) FOR READING` |
| `OPEN OUTPUT file` | `file = OPEN(path) FOR WRITING` |
| `READ file AT END` | `TRY: record = READ_RECORD(file) CATCH EndOfFile:` |
| `WRITE record` | `WRITE_RECORD(file, record)` |
| `CLOSE file` | `CLOSE(file)` |

## Translation Patterns

### 88-Level → ENUM
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
