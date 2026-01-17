---
name: rpg-migration-analyzer
description: Analyzes legacy RPG (Report Program Generator) programs to assist with migration to modern Java applications. Extracts business logic from RPG III/IV/ILE, identifies data structures, file operations, and dependencies. Generates migration reports and creates Java implementation strategies. Use when working with AS/400 or IBM i system migration, RPG analysis, legacy system modernization, or when users mention RPG to Java conversion, analyzing .rpg/.RPG/.rpgle files, working with data specifications, or planning Java service implementations from RPG programs.
---

# RPG Migration Analyzer

Analyzes legacy RPG programs (RPG III/IV/ILE) for migration to Java, extracting business logic, data structures, file operations, and generating actionable migration strategies.

## Core Capabilities

### 1. Program Analysis
Extract specification types (H/F/D/C/P-specs), data structures, file definitions, business logic, indicators (*IN), built-in functions (%SUBST, %TRIM, %EOF), and error handling (%ERROR, %STATUS).

### 2. Data Structure Mapping
Convert D-specs, data types (packed P, zoned S, character A, indicators N), arrays (DIM), nested DS, external data structures (EXTNAME), and qualifiers (LIKEDS, QUALIFIED, INZ) to Java classes/collections.

### 3. File Operations
Parse physical/logical/display files, access methods (sequential, keyed), I/O operations (READ, WRITE, UPDATE, DELETE, CHAIN, SETLL), and file status (%EOF, %FOUND, %ERROR).

### 4. Java Migration
Generate POJOs, service methods, JDBC/JPA patterns, Bean Validation, exception handling (try-catch from %ERROR), and collections from arrays.

### 5. Dependency Analysis
Map CALLB/CALLP calls, service programs (BNDDIR), file dependencies, database tables, and /COPY members.

## Quick Usage Guide

### Find Programs
```bash
find . -name "*.rpg" -o -name "*.rpgle" -o -name "*.RPGLE"
```

### Type Mapping

Convert RPG data types to Java:

| RPG Type | Java Type | Notes |
|----------|-----------|-------|
| `nP m` (packed) | `BigDecimal` | Preserve precision! |
| `nS m` (zoned) | `BigDecimal` | Decimal with sign |
| `A` (character) | `String` | Character data |
| `D` (date) | `LocalDate` | Date field |
| `N` (indicator) | `boolean` | True/False |
| `I` (integer) | `int` or `long` | Binary integer |
| `DIM(n)` | `List<T>` or `T[]` | Arrays |

### Code Patterns

**Business Logic:**
```rpg
C    EVAL  Total = Qty * Price
C    IF    Total > 1000
C    EVAL  Total = Total * 0.90
C    ENDIF
```
```java
BigDecimal total = qty.multiply(price);
if (total.compareTo(new BigDecimal("1000")) > 0) {
    total = total.multiply(new BigDecimal("0.90"));
}
```

**File Operations:**
```rpg
C     custId  CHAIN  CUSTFILE
C             IF     %FOUND(CUSTFILE)
C             EXSR   ProcessCustomer
C             ENDIF
```
```java
customerRepository.findById(custId).ifPresent(this::processCustomer);
```

**Data Structure:**
```rpg
D Employee   DS
D   EmpId             6  0
D   Salary           63  2P
```
```java
public class Employee {
    private int empId;
    private BigDecimal salary;
}
```

## Key Patterns

**Arrays:** `FOR idx = 1 TO %ELEM(Items)` → `for (int i = 0; i < items.length; i++)`
**Dates:** `Today + %DAYS(30)` → `today.plusDays(30)`
**Strings:** `%SUBST(Name:1:10)` → `name.substring(0, 10)` (0-based!)
**Indicators:** `*IN01 = *ON` → `errorCondition = true`

## Migration Checklist

- [ ] Identify program type, extract specifications (H/F/D/C/P), list CALLB/CALLP dependencies, document file operations
- [ ] Convert D-specs to Java classes with BigDecimal for packed decimal, map arrays to collections
- [ ] Convert subroutines/procedures to methods, replace indicators with named booleans, handle %ERROR
- [ ] Map file operations to database/JPA with transaction boundaries
- [ ] Create unit/integration tests, validate with real data
- [ ] Document business rules and assumptions

## Critical Tips

1. **Indicators** → named boolean variables
2. **Always use BigDecimal** for packed/zoned decimal - never float/double
3. **File operations** → database access (JDBC/JPA)
4. **EXTNAME** → extract physical file definitions, create entities
5. **%EOF, %FOUND, %ERROR** → Java exceptions or Optional
6. **Test with AS/400 data** samples for validation

## Output Structure

Provide: Program overview, dependencies, data structures, logic summary, Java design, migration estimate, action items.

## Automation Scripts

The `scripts/` directory contains automation tools for RPG analysis:

- **analyze-dependencies.sh/.ps1** - Scans RPG source files and generates dependency graph in JSON format
- **extract-structure.py** - Extracts structural information from RPG source files (specifications, variables, subroutines, dependencies)
- **generate-java-classes.py** - Generates Java POJO classes from RPG data structures
- **estimate-complexity.py** - Estimates migration complexity based on program analysis

Run scripts directly when analyzing large codebases or automating migration tasks. Scripts include usage documentation in their headers.

## Reference Materials

Load these references when specific guidance is needed:

- **pseudocode-common-rules.md** - General pseudocode syntax and conventions
- **pseudocode-rpg-rules.md** - RPG-specific translation patterns and rules
- **testing-strategy.md** - Testing approach for RPG to Java migration
- **transaction-handling.md** - AS/400 transaction to Java transaction patterns
- **performance-patterns.md** - Performance optimization patterns
- **messaging-integration.md** - Message queue and integration patterns

## Integration

Works with AS/400 analysis tools, DB2 for i schemas, version control, and modern IDEs (IntelliJ IDEA, Eclipse, VS Code).
