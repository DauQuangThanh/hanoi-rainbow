# RPG Translation Rules

**Prerequisites**: Read [PSEUDOCODE-COMMON-RULES.md](PSEUDOCODE-COMMON-RULES.md) for syntax, naming, and structure.

---

## Specification Mapping

| RPG Spec | Pseudocode Section |
|----------|-------------------|
| H-spec (Header) | Program Overview + Constants |
| F-spec (File) | Data Structures (file controls) |
| D-spec (Data) | Data Structures |
| C-spec (Calculation) | Main Algorithm + Core Processing |
| P-spec (Procedure) | Core Processing Logic |

## Data Types

| RPG | Pseudocode | Notes |
|-----|-----------|-------|
| `nP m` (packed) | `DECIMAL(n,m)` | **Packed - preserve precision!** |
| `nS m` (zoned) | `DECIMAL(n,m)` | Zoned decimal |
| `A` (character) | `STRING[n]` | Character |
| `D` (date) | `DATE` | Date |
| `N` (indicator) | `BOOLEAN` | True/False |
| `I` (integer) | `INTEGER` | Binary integer |
| `DIM(n)` | `ARRAY[n] OF TYPE` | Arrays |

## Operation Mapping

| RPG | Pseudocode |
|-----|-----------|
| `EVAL result = expr` | `result = expr` |
| `ADD(E) a b result` | `result = a + b` |
| `IF condition` | `IF condition THEN` |
| `DOW condition` | `WHILE condition DO` |
| `DOU condition` | `DO ... WHILE condition` |
| `FOR index = start TO end` | `FOR index FROM start TO end DO` |
| `EXSR subroutine` | `CALL SubroutineName()` |
| `READ file` | `record = READ_RECORD(file)` |
| `CHAIN key file` | `record = READ_BY_KEY(file, key)` |
| `SETLL key file` | `POSITION(file, key)` |

## Built-in Functions

| RPG BIF | Pseudocode |
|---------|-----------|
| `%SUBST(str:pos:len)` | `SUBSTRING(str, pos, len)` |
| `%TRIM(str)` | `TRIM(str)` |
| `%LEN(str)` | `LENGTH(str)` |
| `%EOF(file)` | `END_OF_FILE(file)` |
| `%FOUND(file)` | `RECORD_FOUND(file)` |
| `%CHAR(num)` | `TO_STRING(num)` |

## Translation Patterns

### Indicators → Boolean
```rpg
D EOF             S               N   INZ(*OFF)
C                   IF        EOF
C                   EVAL      EOF = *ON
```
→
```
eof: BOOLEAN = FALSE
IF eof THEN ...
eof = TRUE
```

### Subroutine → Procedure
```rpg
C     CALC_TOTAL    BEGSR
C                   EVAL      Total = Qty * Price
C                   ENDSR
C                   EXSR      CALC_TOTAL
```
→
```
PROCEDURE CalcTotal()
BEGIN
    total = qty * price
END PROCEDURE
CALL CalcTotal()
```

### File Loop with CHAIN
```rpg
C     Key           CHAIN     File
C                   DOW       %FOUND(File)
C                   EXSR      ProcessRec
C     Key           CHAIN     File
C                   ENDDO
```
→
```
record = READ_BY_KEY(file, key)
WHILE RECORD_FOUND(file) DO
    CALL ProcessRec(record)
    record = READ_BY_KEY(file, key)
END WHILE
```

## Critical Rules

1. **Indicators**: Convert *IN01-*IN99 to named booleans
2. **Packed decimal (P)**: MUST preserve precision using DECIMAL(n,m)
3. **%ERROR**: Convert to TRY-CATCH blocks
4. **%FOUND**: Check after CHAIN/SETLL operations
5. **Half-adjust (H)**: Use `ROUND(expr, decimals)` with HALF_UP

## Translation Workflow

1. Map specs → Pseudocode sections
2. Extract D-spec → Data Structures (preserve packed decimal!)
3. Convert BEGSR/ENDSR → Procedures
4. Translate C-spec/Free → Main Algorithm
5. Generate Mermaid flowchart
6. Verify precision preserved

**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide
