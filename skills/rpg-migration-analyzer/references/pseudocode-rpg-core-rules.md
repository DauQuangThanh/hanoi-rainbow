# RPG Core Translation Rules

**Prerequisites**: Read [pseudocode-common-rules.md](pseudocode-common-rules.md) for syntax, naming, and structure.

---

## Specification Mapping

| RPG Spec | Pseudocode Section |
| ---------- | ------------------- |
| H-spec (Header) | Program Overview + Constants |
| F-spec (File) | Data Structures (file controls) |
| D-spec (Data) | Data Structures |
| I-spec (Input) | Data Structures (legacy input) |
| C-spec (Calculation) | Main Algorithm + Core Processing |
| O-spec (Output) | Core Processing (legacy output) |
| P-spec (Procedure) | Core Processing Logic |

## F-Spec File Declarations

### Database Files

```rpg
F CustMast   IF   E           K DISK    USROPN
F OrderFile  UF A E           K DISK
F TransFile  O    E             DISK
```

→

```pseudocode
// Input File - Keyed access, user open
CONSTANTS:
    CUSTMAST_FILE = "CustMast"
    FILE_TYPE_INPUT = "INPUT"
    FILE_KEYED = TRUE
END CONSTANTS

// Update File - Keyed access, auto open
// OrderFile: INPUT/UPDATE mode, keyed access

// Output File - Sequential
// TransFile: OUTPUT mode, sequential
```

### Display Files (Workstation)

```rpg
F ScreenDsp  CF   E             WORKSTN SFILE(Detail:RRN)
F             INDDS(Indicators)
```

→

```pseudocode
// Display file with subfile
STRUCTURE ScreenDisplay:
    subfileRecords: ARRAY OF DetailRecord
    subfileRRN: INTEGER
    indicators: IndicatorDS
END STRUCTURE
```

### Printer Files

```rpg
F ReportPrt  O    E             PRINTER OFLIND(*INOF)
```

→

```pseudocode
// Printer output file
reportPrinter: OUTPUT_FILE
overflowIndicator: BOOLEAN  // Set when page overflow
```

### File Keywords Translation

| F-Spec Keyword | Meaning | Pseudocode |
| -------------- | ------- | ---------- |
| `USROPN` | User controlled open | Manual OPEN_FILE() required |
| `INFDS(ds)` | File info data structure | Capture file status in structure |
| `SFILE(fmt:rrn)` | Subfile definition | Array with relative record number |
| `INDDS(ds)` | Indicator data structure | Map indicators to boolean structure |
| `OFLIND(*INxx)` | Overflow indicator | Boolean for page overflow |
| `COMMIT` | Under commitment control | Transaction-controlled file |
| `IGNORE(fmt)` | Ignore record format | Skip specified format |
| `INCLUDE(fmt)` | Include record format | Process specified format |
| `PREFIX(str)` | Prefix field names | Add prefix to all field names |
| `RENAME(old:new)` | Rename record format | Use new name for format |

## Data Types

| RPG | Pseudocode | Notes |
| --- | --------- | ----- |
| `nP m` (packed) | `DECIMAL(n,m)` | **Packed - preserve precision!** |
| `nS m` (zoned) | `DECIMAL(n,m)` | Zoned decimal |
| `A` (character) | `STRING[n]` | Character |
| `D` (date) | `DATE` | Date |
| `T` (time) | `TIME` | Time |
| `Z` (timestamp) | `DATETIME` | Timestamp |
| `N` (indicator) | `BOOLEAN` | True/False |
| `I` (integer) | `INTEGER` | Binary integer |
| `U` (unsigned) | `UNSIGNED_INTEGER` | Unsigned integer |
| `F` (float) | `FLOAT` | Floating point (avoid for money!) |
| `*` (pointer) | `POINTER` | Memory address |
| `DIM(n)` | `ARRAY[n] OF TYPE` | Arrays |
| `LIKEDS(ds)` | `STRUCTURE_INSTANCE` | Data structure reference |

### RPG Special Values

| RPG Value | Pseudocode | Notes |
| --------- | --------- | ----- |
| `*BLANK` / `*BLANKS` | `""` or `EMPTY_STRING` | Empty string |
| `*ZERO` / `*ZEROS` | `0` | Numeric zero |
| `*HIVAL` | `MAX_VALUE` | Highest value for type |
| `*LOVAL` | `MIN_VALUE` | Lowest value for type |
| `*ALL'x'` | `REPEAT('x', length)` | Repeated character |
| `*ON` | `TRUE` | Boolean true |
| `*OFF` | `FALSE` | Boolean false |
| `*NULL` | `NULL` | Null pointer |

## Operation Mapping

### Basic Operations

| RPG | Pseudocode |
| --- | --------- |
| `EVAL result = expr` | `result = expr` |
| `ADD(E) a b result` | `result = a + b` |
| `SUB(E) a b result` | `result = a - b` |
| `MULT(E) a b result` | `result = a * b` |
| `DIV(E) a b result` | `result = a / b` |
| `MVR remainder` | `remainder = MODULO(a, b)` |
| `Z-ADD value result` | `result = value` (zero and add) |
| `Z-SUB value result` | `result = -value` (zero and subtract) |

### Data Movement

| RPG | Pseudocode |
| --- | --------- |
| `MOVE src dest` | `dest = RIGHT_ALIGN(src, LENGTH(dest))` |
| `MOVEL src dest` | `dest = LEFT_ALIGN(src, LENGTH(dest))` |
| `CLEAR field` | `field = DEFAULT_VALUE` |
| `MOVEA arr1 arr2` | `COPY_ARRAY(arr1, arr2)` |

### String Operations

| RPG | Pseudocode |
| --- | --------- |
| `CAT str1:str2 result` | `result = CONCATENATE(str1, str2)` |
| `SCAN pattern str` | `position = FIND(str, pattern)` |
| `CHECK charset str` | `position = FIND_INVALID_CHAR(str, charset)` |
| `CHECKR charset str` | `position = FIND_INVALID_CHAR_REVERSE(str, charset)` |
| `XLATE from:to str` | `str = TRANSLATE(str, from, to)` |
| `SUBST str:pos:len dest` | `dest = SUBSTRING(str, pos, len)` |

### Control Flow

| RPG | Pseudocode |
| --- | --------- |
| `IF condition` | `IF condition THEN` |
| `ELSE` | `ELSE` |
| `ELSEIF condition` | `ELSE IF condition THEN` |
| `ENDIF` | `END IF` |
| `DOW condition` | `WHILE condition DO` |
| `DOU condition` | `DO ... WHILE NOT condition` |
| `FOR index = start TO end` | `FOR index FROM start TO end DO` |
| `ITER` | `CONTINUE` |
| `LEAVE` | `BREAK` |
| `SELECT` | `SWITCH` |
| `WHEN condition` | `CASE condition:` |
| `OTHER` | `DEFAULT:` |
| `ENDSL` | `END SWITCH` |

### Comparison

| RPG | Pseudocode |
| --- | --------- |
| `COMP a b` | `COMPARE(a, b)` |
| `IFEQ / IF a = b` | `IF a = b THEN` |
| `IFNE / IF a <> b` | `IF a != b THEN` |
| `IFGT / IF a > b` | `IF a > b THEN` |
| `IFLT / IF a < b` | `IF a < b THEN` |
| `IFGE / IF a >= b` | `IF a >= b THEN` |
| `IFLE / IF a <= b` | `IF a <= b THEN` |

### Procedure Calls

| RPG | Pseudocode |
| --- | --------- |
| `EXSR subroutine` | `CALL SubroutineName()` |
| `CALLP procedure(parms)` | `CALL Procedure(parms)` |
| `CALLB procedure(parms)` | `result = CALL_BOUND(procedure, parms)` |
| `CALL pgm` with PLIST | `CALL_PROGRAM(pgm, paramList)` |
| `RETURN value` | `RETURN value` |

### File Operations - Basic

| RPG | Pseudocode |
| --- | --------- |
| `READ file` | `record = READ_RECORD(file)` |
| `READP file` | `record = READ_PREVIOUS(file)` |
| `READE key file` | `record = READ_EQUAL_KEY(file, key)` |
| `READPE key file` | `record = READ_PREVIOUS_EQUAL(file, key)` |
| `CHAIN key file` | `record = READ_BY_KEY(file, key)` |
| `WRITE file` | `WRITE_RECORD(file, record)` |
| `UPDATE file` | `UPDATE_RECORD(file, record)` |
| `DELETE file` | `DELETE_RECORD(file)` |
| `UNLOCK file` | `UNLOCK_RECORD(file)` |

### File Operations - Positioning

| RPG | Pseudocode |
| --- | --------- |
| `SETLL key file` | `POSITION_LOWER_LIMIT(file, key)` |
| `SETGT key file` | `POSITION_GREATER_THAN(file, key)` |
| `OPEN file` | `OPEN_FILE(file)` |
| `CLOSE file` | `CLOSE_FILE(file)` |
| `FEOD file` | `FORCE_END_OF_DATA(file)` |

### Output Operations

| RPG | Pseudocode |
| --- | --------- |
| `EXCEPT format` | `WRITE_OUTPUT_FORMAT(format)` |
| `WRITE format` | `WRITE_RECORD(format)` |

### Data Validation

| RPG | Pseudocode |
| --- | --------- |
| `TEST(DE) date` | `IS_VALID_DATE(date)` |
| `TEST(T) time` | `IS_VALID_TIME(time)` |
| `TEST(Z) timestamp` | `IS_VALID_TIMESTAMP(timestamp)` |
| `TEST(N) field` | `IS_NUMERIC(field)` |

---

**Next**: See [pseudocode-rpg-functions.md](pseudocode-rpg-functions.md) for built-in functions.
