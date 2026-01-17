---
name: cobol-migration-analyzer
description: Analyzes legacy COBOL programs and JCL jobs to assist with migration to modern Java applications. Extracts business logic, identifies dependencies, generates migration reports, and creates Java implementation strategies. Use when working with mainframe migration, COBOL analysis, legacy system modernization, JCL workflows, or when users mention COBOL to Java conversion, analyzing .cbl/.CBL/.cob files, working with copybooks, or planning Java service implementations from COBOL programs.
---

# COBOL Migration Analyzer

Analyze legacy COBOL programs and JCL scripts for migration to Java. Extract business logic, data structures, and dependencies to generate actionable migration strategies.

## Core Capabilities

### 1. COBOL Program Analysis
Extract COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE), Working-Storage variables, file definitions (FD), business logic paragraphs, PERFORM statements, CALL hierarchies, embedded SQL, and error handling patterns.

### 2. JCL Job Analysis
Parse JCL job steps, program invocations, data dependencies (DD statements), conditional logic (COND, IF/THEN/ELSE), return codes, and resource requirements.

### 3. Copybook Processing
Extract record layouts with level numbers, REDEFINES clauses, group items, OCCURS clauses, and picture clauses. Generate Java POJOs from copybook structures.

### 4. Dependency Mapping
Build complete dependency graphs showing CALL hierarchies, copybook usage, file dependencies, database table access, and shared utility references across the codebase.

## Workflow

### Step 1: Discover COBOL Assets
Find COBOL programs, JCL jobs, and copybooks:
```bash
find . -name "*.cbl" -o -name "*.CBL" -o -name "*.cob"
find . -name "*.jcl" -o -name "*.JCL"
find . -name "*.cpy" -o -name "*.CPY"
```

Use `scripts/analyze-dependencies.sh` or `scripts/analyze-dependencies.ps1` to generate dependency graph.

### Step 2: Extract Structure
Use `scripts/extract-structure.py` to parse COBOL programs and extract divisions, variables, paragraphs, and dependencies in JSON format.

### Step 3: Generate Java Code
Use `scripts/generate-java-classes.py` to convert copybooks to Java POJOs with appropriate data types and Bean Validation annotations.

### Step 4: Estimate Complexity
Use `scripts/estimate-complexity.py` to calculate migration complexity based on LOC, external calls, file operations, SQL statements, and control flow.

### Step 5: Create Migration Strategy
Document program overview, dependencies, data structures, business logic patterns, proposed Java design, migration estimate, and action items.

## Quick Reference

### COBOL to Java Type Mapping
| COBOL Picture | Java Type | Notes |
|---------------|-----------|-------|
| `PIC 9(n)` | `int`, `long`, `BigInteger` | Unsigned numeric |
| `PIC S9(n)V9(m)` | `BigDecimal` | Signed decimal |
| `PIC X(n)` | `String` | Alphanumeric |
| `COMP-3` | `BigDecimal` | Packed decimal |
| `OCCURS n` | `List<T>` or `T[]` | Arrays/tables |

### Common Pattern Conversions
- **File I/O**: `READ...AT END` → `BufferedReader` with try-with-resources
- **Table lookup**: `SEARCH ALL` → `stream().filter().findFirst()`
- **Date arithmetic**: `FUNCTION INTEGER-OF-DATE` → `LocalDate` operations
- **Condition names (Level 88)**: → `enum` or constants
- **Computed GO TO**: → Strategy pattern or switch statement
- **REDEFINES**: → Union types or separate accessors

### Example: Copybook to Java POJO
**COBOL Copybook:**
```cobol
01  EMPLOYEE-RECORD.
    05  EMP-ID        PIC 9(6).
    05  EMP-NAME      PIC X(30).
    05  EMP-SALARY    PIC S9(7)V99 COMP-3.
```

**Generated Java:**
```java
public class EmployeeRecord {
    private int empId;
    private String empName;
    private BigDecimal empSalary;
    // getters/setters
}
```

## Migration Considerations

**Critical Patterns:**
1. Always use `BigDecimal` for COMP-3 and numeric with decimals (never float/double)
2. Computed GO TO → refactor to strategy pattern or switch
3. ALTER statement → refactor to structured control flow
4. REDEFINES → model as union type or separate view classes
5. Test with production data samples for validation

**Output Requirements:**
- Program overview and type classification
- Complete dependency graph (CALL tree, copybooks, files, DB tables)
- Data structure mapping (copybooks → Java classes)
- Business logic summary (key paragraphs → methods)
- Proposed Java architecture (services, repositories, entities)
- Migration effort estimate (complexity score, LOC, risk factors)
- Prioritized action items

## Advanced Topics

For detailed conversion rules and patterns, see:
- **[pseudocode-cobol-rules.md](references/pseudocode-cobol-rules.md)** - Detailed COBOL to pseudocode conversion patterns
- **[pseudocode-jcl-rules.md](references/pseudocode-jcl-rules.md)** - JCL workflow to orchestration patterns
- **[transaction-handling.md](references/transaction-handling.md)** - Transaction management and rollback strategies
- **[messaging-integration.md](references/messaging-integration.md)** - Message queue and async patterns
- **[performance-patterns.md](references/performance-patterns.md)** - Batch processing and optimization
- **[testing-strategy.md](references/testing-strategy.md)** - Unit, integration, and parallel validation testing

## Tools and Scripts

All scripts support cross-platform execution (Windows PowerShell, bash):
- `analyze-dependencies.sh/ps1` - Generate dependency graph
- `extract-structure.py` - Parse COBOL structure to JSON
- `generate-java-classes.py` - Convert copybooks to Java POJOs
- `estimate-complexity.py` - Calculate migration complexity score

Scripts use standard libraries only and output JSON for easy integration with CI/CD pipelines.
