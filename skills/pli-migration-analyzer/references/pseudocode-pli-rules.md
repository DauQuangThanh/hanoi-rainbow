# PL/I Translation Rules

**Prerequisites**: Read [PSEUDOCODE-COMMON-RULES.md](PSEUDOCODE-COMMON-RULES.md) for syntax, naming, and structure.

---

## Structure Mapping

| PL/I Element | Pseudocode Section |
|--------------|-------------------|
| `PROCEDURE OPTIONS(MAIN)` | Program Overview + Main Algorithm |
| `DECLARE` / `DCL` | Data Structures |
| `DCL file FILE` | Data Structures (file controls) |
| Procedures | Core Processing Logic |
| `ON condition` | Error Handling |

## Data Types

| PL/I | Pseudocode | Notes |
|------|-----------|-------|
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
|------|-----------|
| `a = b;` | `a = b` |
| `IF condition THEN ... ELSE` | `IF condition THEN ... ELSE` |
| `DO WHILE(condition);` | `WHILE condition DO` |
| `DO i = 1 TO n;` | `FOR i FROM 1 TO n DO` |
| `CALL proc(args);` | `CALL ProcName(args)` |
| `RETURN(value);` | `RETURN value` |
| `GO TO label;` | Refactor to structured flow |
| `SELECT; WHEN ... END;` | `SWITCH ... CASE ... END SWITCH` |

## Built-in Functions

| PL/I BIF | Pseudocode |
|----------|-----------|
| `SUBSTR(str,pos,len)` | `SUBSTRING(str, pos, len)` |
| `INDEX(str,search)` | `FIND(str, search)` |
| `LENGTH(str)` | `LENGTH(str)` |
| `TRIM(str)` | `TRIM(str)` |
| `ROUND(num,dec)` | `ROUND(num, dec)` |
| `MOD(a,b)` | `a MOD b` |

## Translation Patterns

### Procedure → Function
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

## Critical Rules

1. **1-based indexing**: PL/I arrays start at 1 (preserve or document)
2. **FIXED DECIMAL**: MUST map to DECIMAL with exact precision
3. **FLOAT**: Prohibited for financial calculations - always use DECIMAL
4. **ON conditions**: Map to TRY-CATCH blocks
5. **GO TO**: Refactor to structured control flow
6. **Bit strings**: Convert '1'B/'0'B to TRUE/FALSE

## Translation Workflow

1. Extract PROCEDURE structure → Program Overview
2. Convert DCL → Data Structures (preserve decimal precision!)
3. Translate procedures → Functions/Procedures
4. Convert ON conditions → Error handling
5. Translate main logic → Main Algorithm
6. Generate Mermaid flowchart
7. Verify precision preserved, no float usage

**Reference**: IBM Enterprise PL/I for z/OS Language Reference
