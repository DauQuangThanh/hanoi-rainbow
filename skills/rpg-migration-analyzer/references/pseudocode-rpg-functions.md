# RPG Built-in Functions

**Prerequisites**: Read [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md) for basic operations.

---

## String Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%SUBST(str:pos:len)` | `SUBSTRING(str, pos, len)` |
| `%TRIM(str)` | `TRIM(str)` |
| `%TRIML(str)` | `TRIM_LEFT(str)` |
| `%TRIMR(str)` | `TRIM_RIGHT(str)` |
| `%LEN(str)` | `LENGTH(str)` |
| `%SIZE(var)` | `SIZE_OF(var)` |
| `%SCAN(pattern:str:start)` | `FIND(str, pattern, start)` |
| `%CHECK(charset:str:start)` | `FIND_INVALID_CHAR(str, charset, start)` |
| `%CHECKR(charset:str:start)` | `FIND_INVALID_CHAR_REVERSE(str, charset, start)` |
| `%REPLACE(repl:str:pos:len)` | `REPLACE(str, repl, pos, len)` |
| `%XLATE(from:to:str)` | `TRANSLATE(str, from, to)` |

## Conversion Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%CHAR(value)` | `TO_STRING(value)` |
| `%INT(value)` | `TO_INTEGER(value)` |
| `%UNS(value)` | `TO_UNSIGNED(value)` |
| `%DEC(value:digits:decimals)` | `TO_DECIMAL(value, digits, decimals)` |
| `%DECH(value:digits:decimals)` | `TO_DECIMAL_HALF_ADJUST(value, digits, decimals)` |
| `%FLOAT(value)` | `TO_FLOAT(value)` |
| `%EDITC(num:code)` | `FORMAT_NUMERIC(num, code)` |
| `%EDITW(num:pattern)` | `FORMAT_WITH_PATTERN(num, pattern)` |
| `%EDITFLT(num)` | `FORMAT_FLOAT(num)` |

## Date/Time Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%DATE()` | `CURRENT_DATE()` |
| `%TIME()` | `CURRENT_TIME()` |
| `%TIMESTAMP()` | `CURRENT_TIMESTAMP()` |
| `%DATE(value:format)` | `PARSE_DATE(value, format)` |
| `%TIME(value:format)` | `PARSE_TIME(value, format)` |
| `%TIMESTAMP(value:format)` | `PARSE_TIMESTAMP(value, format)` |
| `%DIFF(dt1:dt2:unit)` | `DATE_DIFFERENCE(dt1, dt2, unit)` |
| `%YEARS(num)` | `DURATION_YEARS(num)` |
| `%MONTHS(num)` | `DURATION_MONTHS(num)` |
| `%DAYS(num)` | `DURATION_DAYS(num)` |
| `%HOURS(num)` | `DURATION_HOURS(num)` |
| `%MINUTES(num)` | `DURATION_MINUTES(num)` |
| `%SECONDS(num)` | `DURATION_SECONDS(num)` |
| `%MSECONDS(num)` | `DURATION_MILLISECONDS(num)` |

## File Status Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%EOF(file)` | `END_OF_FILE(file)` |
| `%EQUAL(file)` | `EQUAL_CONDITION(file)` |
| `%FOUND(file)` | `RECORD_FOUND(file)` |
| `%OPEN(file)` | `IS_FILE_OPEN(file)` |
| `%ERROR` | `ERROR_OCCURRED()` |
| `%STATUS` | `GET_STATUS_CODE()` |

## Array/Data Structure Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%ELEM(array)` | `ELEMENT_COUNT(array)` |
| `%OCCUR(ds)` | `GET_OCCURRENCE(ds)` |
| `%ADDR(var)` | `ADDRESS_OF(var)` |
| `%PADDR(proc)` | `PROCEDURE_ADDRESS(proc)` |
| `%LOOKUP(val:arr)` | `ARRAY_SEARCH(arr, val)` |
| `%TLOOKUP(val:arr:seq)` | `TABLE_LOOKUP(arr, val, seq)` |

## Arithmetic/Utility Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%ABS(value)` | `ABSOLUTE_VALUE(value)` |
| `%DIV(a:b)` | `INTEGER_DIVIDE(a, b)` |
| `%REM(a:b)` | `REMAINDER(a, b)` |
| `%SQRT(value)` | `SQUARE_ROOT(value)` |
| `%INTH(value)` | `INTEGER_HALF_ADJUST(value)` |
| `%DECH(value:d:p)` | `DECIMAL_HALF_ADJUST(value, d, p)` |

## Conditional Functions

| RPG BIF | Pseudocode |
| ------- | --------- |
| `%NULLIND(field)` | `GET_NULL_INDICATOR(field)` |
| `%PARMS` | `PARAMETER_COUNT()` |
| `%PARMNUM(parm)` | `PARAMETER_NUMBER(parm)` |

---

**Next**: See [pseudocode-rpg-data-structures.md](pseudocode-rpg-data-structures.md) for data structure patterns.
