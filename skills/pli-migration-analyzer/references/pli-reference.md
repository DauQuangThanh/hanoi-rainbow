# PL/I to Java Migration Reference

Complete reference for analyzing PL/I programs and migrating to Java.

## Table of Contents

- [Data Type Mapping](#data-type-mapping)
- [Code Pattern Conversions](#code-pattern-conversions)
- [Key Transformation Rules](#key-transformation-rules)
- [Migration Checklist](#migration-checklist)
- [Common Pitfalls](#common-pitfalls)

---

## Data Type Mapping

### Type Conversion Table

| PL/I Type | Java Type | Notes |
|-----------|-----------|-------|
| `FIXED DECIMAL(n,m)` | `BigDecimal` | **Always use BigDecimal - NEVER float/double for financial data** |
| `FIXED BINARY(15)` | `short` | 16-bit signed integer |
| `FIXED BINARY(31)` | `int` | 32-bit signed integer |
| `FIXED BINARY(63)` | `long` | 64-bit signed integer |
| `FLOAT DECIMAL(n)` | `BigDecimal` | **For financial: use BigDecimal, not float** |
| `CHARACTER(n)` | `String` | Fixed-length string |
| `CHARACTER(n) VARYING` | `String` | Variable-length string |
| `BIT(1)` | `boolean` | Boolean flag |
| `BIT(n)` where n>1 | `BitSet` or `byte[]` | Bit string operations |
| `POINTER` | Object reference | Reference to object |
| Arrays `(n)` | `List<T>` or `T[]` | **PL/I uses 1-based, Java uses 0-based indexing** |
| Structure (`DCL 1 ...`) | Java class | POJO with fields |

### Array Index Conversion

**Critical:** PL/I arrays are 1-based, Java arrays are 0-based.

```pli
DCL arr(10) FIXED DECIMAL(9,2);
arr(1) = 100.00;  /* First element */
arr(10) = 200.00; /* Last element */
```

```java
BigDecimal[] arr = new BigDecimal[10];
arr[0] = new BigDecimal("100.00");  // First element
arr[9] = new BigDecimal("200.00");  // Last element
```

---

## Code Pattern Conversions

### Procedures to Methods

**PL/I Procedure:**
```pli
CALC_TOTAL: PROCEDURE(qty, price) RETURNS(FIXED DECIMAL(15,2));
    DCL qty FIXED DECIMAL(9,2);
    DCL price FIXED DECIMAL(9,2);
    DCL result FIXED DECIMAL(15,2);
    
    result = qty * price;
    IF result > 1000 THEN 
        result = result * 0.90; /* 10% discount */
    RETURN(result);
END CALC_TOTAL;
```

**Java Method:**
```java
public BigDecimal calcTotal(BigDecimal qty, BigDecimal price) {
    BigDecimal result = qty.multiply(price);
    
    if (result.compareTo(new BigDecimal("1000")) > 0) {
        result = result.multiply(new BigDecimal("0.90")); // 10% discount
    }
    
    return result;
}
```

### File I/O Operations

**PL/I Sequential File Read:**
```pli
DCL infile FILE INPUT RECORD SEQUENTIAL;
DCL rec CHARACTER(100);
DCL eof BIT(1) INIT('0'B);

ON ENDFILE(infile) eof = '1'B;
OPEN FILE(infile);

DO WHILE(¬eof);
    READ FILE(infile) INTO(rec);
    IF ¬eof THEN CALL process_record(rec);
END;

CLOSE FILE(infile);
```

**Java File Read:**
```java
try (BufferedReader reader = Files.newBufferedReader(Paths.get("inputfile"))) {
    reader.lines().forEach(this::processRecord);
} catch (IOException e) {
    logger.error("Error reading file", e);
    throw new RuntimeException("Failed to process input file", e);
}
```

### Structure to Class

**PL/I Structure:**
```pli
DCL 1 EMPLOYEE,
      2 ID CHARACTER(10),
      2 NAME CHARACTER(50),
      2 SALARY FIXED DECIMAL(9,2),
      2 HIRE_DATE CHARACTER(10);
```

**Java Class:**
```java
public class Employee {
    private String id;
    private String name;
    private BigDecimal salary;
    private LocalDate hireDate;
    
    // Constructors
    public Employee() {}
    
    public Employee(String id, String name, BigDecimal salary, LocalDate hireDate) {
        this.id = id;
        this.name = name;
        this.salary = salary;
        this.hireDate = hireDate;
    }
    
    // Getters and Setters
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public BigDecimal getSalary() { return salary; }
    public void setSalary(BigDecimal salary) { this.salary = salary; }
    
    public LocalDate getHireDate() { return hireDate; }
    public void setHireDate(LocalDate hireDate) { this.hireDate = hireDate; }
}
```

### Loop Conversions

**DO WHILE:**
```pli
DCL i FIXED BINARY(31);
i = 1;
DO WHILE(i <= 10);
    CALL process(i);
    i = i + 1;
END;
```
```java
int i = 1;
while (i <= 10) {
    process(i);
    i++;
}
```

**DO index = start TO end:**
```pli
DCL i FIXED BINARY(31);
DO i = 1 TO 10;
    CALL process(i);
END;
```
```java
for (int i = 1; i <= 10; i++) {
    process(i);
}
```

**DO index = start TO end BY step:**
```pli
DO i = 1 TO 10 BY 2;
    CALL process(i);
END;
```
```java
for (int i = 1; i <= 10; i += 2) {
    process(i);
}
```

### String Operations

| PL/I Function | Java Equivalent | Notes |
|---------------|-----------------|-------|
| `SUBSTR(str, 10, 20)` | `str.substring(9, 29)` | **0-based indexing!** |
| `INDEX(str, 'search')` | `str.indexOf("search")` | Returns -1 if not found (vs 0 in PL/I) |
| `LENGTH(str)` | `str.length()` | Same semantics |
| `TRIM(str)` | `str.trim()` | Removes leading/trailing whitespace |
| `str1 || str2` | `str1 + str2` or `str1.concat(str2)` | String concatenation |

**Example - SUBSTR conversion:**
```pli
DCL text CHARACTER(100);
DCL part CHARACTER(20);
text = 'The quick brown fox jumps';
part = SUBSTR(text, 5, 5);  /* 'quick' - position 5, length 5 */
```
```java
String text = "The quick brown fox jumps";
String part = text.substring(4, 9);  // 'quick' - start 4, end 9 (0-based, exclusive end)
```

### SELECT/WHEN to Switch/Case

**PL/I SELECT:**
```pli
SELECT (status);
    WHEN ('A') CALL handle_active();
    WHEN ('I') CALL handle_inactive();
    WHEN ('P') CALL handle_pending();
    OTHERWISE CALL handle_unknown();
END;
```

**Java Switch (traditional):**
```java
switch (status) {
    case 'A':
        handleActive();
        break;
    case 'I':
        handleInactive();
        break;
    case 'P':
        handlePending();
        break;
    default:
        handleUnknown();
        break;
}
```

**Java Switch (modern, Java 14+):**
```java
switch (status) {
    case 'A' -> handleActive();
    case 'I' -> handleInactive();
    case 'P' -> handlePending();
    default -> handleUnknown();
}
```

### Exception Handling (ON Conditions)

**PL/I ON Conditions:**
```pli
DCL infile FILE INPUT;

ON ENDFILE(infile) GO TO end_processing;
ON ERROR BEGIN;
    PUT SKIP LIST('Error occurred');
    GO TO cleanup;
END;

OPEN FILE(infile);
/* processing */

end_processing:
    CLOSE FILE(infile);
```

**Java Try-Catch:**
```java
try (BufferedReader reader = Files.newBufferedReader(path)) {
    // Processing
    String line;
    while ((line = reader.readLine()) != null) {
        processLine(line);
    }
} catch (IOException e) {
    logger.error("Error occurred", e);
    throw new RuntimeException("Failed to process file", e);
} finally {
    // Cleanup if needed
}
```

---

## Key Transformation Rules

### 1. GO TO Statement Refactoring

**Never translate GO TO directly.** Refactor to structured control flow.

**Anti-pattern (PL/I):**
```pli
    IF error_flag THEN GO TO error_handler;
    CALL process_data();
    GO TO end_routine;

error_handler:
    CALL handle_error();
    
end_routine:
    RETURN;
```

**Correct Java Pattern:**
```java
if (errorFlag) {
    handleError();
} else {
    processData();
}
```

### 2. FIXED DECIMAL Precision

**Critical:** Always preserve decimal precision. Never use float or double for financial calculations.

```pli
DCL amount FIXED DECIMAL(15,2);  /* 15 digits total, 2 decimal places */
```
```java
// Correct
BigDecimal amount = new BigDecimal("0.00");

// WRONG - loses precision
double amount = 0.00;  // Never do this for financial data!
```

### 3. Array Bounds

**Always adjust loop bounds when converting arrays:**

```pli
DCL arr(10) FIXED DECIMAL(9,2);
DO i = 1 TO 10;
    PUT SKIP LIST(arr(i));
END;
```

```java
BigDecimal[] arr = new BigDecimal[10];
for (int i = 0; i < 10; i++) {  // 0 to 9, not 1 to 10
    System.out.println(arr[i]);
}
```

### 4. Boolean Operations

| PL/I | Java | Notes |
|------|------|-------|
| `¬` or `^` | `!` | Logical NOT |
| `&` | `&&` | Logical AND |
| `|` | `||` | Logical OR |
| `'1'B` | `true` | Boolean true |
| `'0'B` | `false` | Boolean false |

---

## Migration Checklist

Use this checklist for each PL/I program:

### Analysis Phase
- [ ] Identify entry point (PROCEDURE OPTIONS(MAIN))
- [ ] Extract all DCL statements and create data dictionary
- [ ] Map all procedures to methods
- [ ] Identify file operations (READ, WRITE, OPEN, CLOSE)
- [ ] Extract all ON conditions and exception handlers
- [ ] Identify external procedure calls
- [ ] Document all %INCLUDE directives
- [ ] Map all GO TO statements for refactoring

### Design Phase
- [ ] Convert structures to Java classes with appropriate types
- [ ] Use BigDecimal for all FIXED DECIMAL types
- [ ] Design exception handling strategy (map ON conditions)
- [ ] Plan file I/O migration (files vs database)
- [ ] Identify transaction boundaries
- [ ] Design API interfaces for procedure conversions
- [ ] Plan test data and validation approach

### Implementation Phase
- [ ] Create POJOs for all data structures
- [ ] Implement service methods for procedures
- [ ] Convert file operations to Java I/O or database operations
- [ ] Implement exception handling with try-catch
- [ ] Refactor GO TO statements to structured flow
- [ ] Adjust array indexing (1-based to 0-based)
- [ ] Convert string operations (SUBSTR, INDEX)
- [ ] Add Bean Validation annotations (@NotNull, @Size, etc.)

### Validation Phase
- [ ] Create unit tests for each converted procedure
- [ ] Test boundary conditions (arrays, strings)
- [ ] Validate decimal precision (BigDecimal calculations)
- [ ] Test exception handling paths
- [ ] Compare outputs with legacy system
- [ ] Performance testing
- [ ] Code review for Java best practices

---

## Common Pitfalls

### Pitfall 1: Using float/double for FIXED DECIMAL

**Problem:**
```java
// WRONG
double salary = 1234.56;
double tax = salary * 0.15;
```

**Solution:**
```java
// CORRECT
BigDecimal salary = new BigDecimal("1234.56");
BigDecimal tax = salary.multiply(new BigDecimal("0.15"));
```

### Pitfall 2: Forgetting Array Index Offset

**Problem:**
```java
// PL/I: arr(1) to arr(10)
// Java array of size 10, but accessing arr[1] to arr[10] causes ArrayIndexOutOfBoundsException
```

**Solution:**
```java
// Adjust indices: PL/I arr(i) becomes Java arr[i-1]
// Or adjust loop: FOR i = 1 TO 10 becomes for (int i = 0; i < 10; i++)
```

### Pitfall 3: Direct GO TO Translation

**Problem:**
```java
// Attempting to use labels and goto (not supported in Java)
```

**Solution:**
```java
// Refactor to if-else, loops, or early returns
// Use exceptions for error handling instead of jumping to error labels
```

### Pitfall 4: String SUBSTR Off-by-One Errors

**Problem:**
```java
// PL/I: SUBSTR(str, 5, 10) starts at position 5 (1-based)
// Java: str.substring(5, 10) starts at index 5 (0-based)
```

**Solution:**
```java
// PL/I SUBSTR(str, pos, len) → Java str.substring(pos-1, pos-1+len)
// Example: SUBSTR(str, 5, 10) → str.substring(4, 14)
```

### Pitfall 5: Ignoring File vs Database Migration

**Problem:**
```java
// Simply converting FILE operations to File I/O without considering database
```

**Solution:**
```java
// Evaluate if sequential file processing should become database operations
// Consider transaction management, concurrent access, data integrity
```

### Pitfall 6: Not Handling NULL Values

**Problem:**
```java
// PL/I has implicit initialization, Java requires explicit null handling
```

**Solution:**
```java
// Use Optional<T> or null checks
// Add @NotNull, @Nullable annotations
// Implement proper null validation
```

---

## Additional Resources

For specific topics, see:
- **Pseudocode translation**: [pseudocode-pli-rules.md](pseudocode-pli-rules.md)
- **Transaction patterns**: [transaction-handling.md](transaction-handling.md)
- **Performance optimization**: [performance-patterns.md](performance-patterns.md)
- **Messaging integration**: [messaging-integration.md](messaging-integration.md)
- **Testing strategies**: [testing-strategy.md](testing-strategy.md)
