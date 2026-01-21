# RPG Translation Rules - Index

This is the main index for RPG to pseudocode translation rules. The rules have been organized into focused, topic-specific files for easier navigation and context loading by AI agents.

---

## Quick Start

**New to RPG migration?** Start here:

1. Read [pseudocode-common-rules.md](pseudocode-common-rules.md) for basic pseudocode syntax
2. Read [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md) for fundamental RPG translations
3. Consult other files as needed for specific features

---

## File Guide

### 📘 [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md)

**Size**: ~400 lines  
**Use for**: Basic RPG syntax, data types, and operations

**Contains**:

- Specification mapping (H-spec, F-spec, D-spec, C-spec, P-spec)
- F-spec file declarations (Database, Display, Printer)
- Core data types (Packed, Zoned, Character, Date/Time, Indicators)
- RPG special values (*BLANK,*ZERO, *HIVAL,*LOVAL, etc.)
- Basic operations (arithmetic, data movement, string ops)
- Control flow (IF, DOW, DOU, FOR, SELECT/WHEN)
- File operations (READ, WRITE, CHAIN, UPDATE, SETLL)
- Data validation operations

**When to use**: Start here for any RPG translation. This file contains the foundational rules needed for basic RPG programs.

---

### 🔧 [pseudocode-rpg-functions.md](pseudocode-rpg-functions.md)

**Size**: ~130 lines  
**Use for**: RPG built-in functions (BIFs)

**Contains**:

- String functions (%SUBST, %TRIM, %SCAN, %REPLACE, %XLATE)
- Conversion functions (%CHAR, %INT, %DEC, %EDITC, %EDITW)
- Date/Time functions (%DATE, %TIME, %TIMESTAMP, %DIFF, durations)
- File status functions (%EOF, %FOUND, %EQUAL, %ERROR)
- Array/DS functions (%ELEM, %OCCUR, %ADDR, %LOOKUP)
- Arithmetic functions (%ABS, %DIV, %REM, %SQRT)
- Conditional functions (%NULLIND, %PARMS)

**When to use**: Reference this when encountering RPG BIFs (functions starting with `%`).

---

### 📦 [pseudocode-rpg-data-structures.md](pseudocode-rpg-data-structures.md)

**Size**: ~200 lines  
**Use for**: Complex data structure patterns

**Contains**:

- Simple data structures
- Qualified data structures
- LIKEDS (structure references)
- OVERLAY (shared memory patterns)
- Multiple occurrence data structures
- I-spec (Input specification) patterns for fixed-format input
- O-spec (Output specification) patterns for reports

**When to use**: When translating programs with complex data structures, especially those using QUALIFIED, LIKEDS, OVERLAY, or legacy I/O specs.

---

### 🎯 [pseudocode-rpg-patterns.md](pseudocode-rpg-patterns.md)

**Size**: ~700 lines  
**Use for**: Common RPG idioms and translation patterns

**Contains**:

- Indicator → Boolean conversion
- Subroutine → Procedure patterns
- File loop patterns (CHAIN, READ, SETLL/READE)
- Update/Write record patterns
- Error handling (MONITOR, %ERROR)
- Procedure with parameters
- String manipulation patterns
- Date arithmetic
- SELECT/WHEN patterns
- Subfile operations (interactive programs)
- RPG cycle handling
- Array initialization (CTDATA)
- Program initialization/termination
- Message handling
- Data queue operations
- **Common Pitfalls** (23 gotchas to avoid)
- **Critical Rules** (12 must-follow rules for accurate migration)

**When to use**: This is your primary reference for everyday RPG patterns. Consult frequently during translation to avoid common mistakes.

---

### 🚀 [pseudocode-rpg-advanced.md](pseudocode-rpg-advanced.md)

**Size**: ~1,300 lines  
**Use for**: Modern ILE RPG features and integrations

**Contains**:

- File/Program status data structures (INFDS, PSDS)
- Data area operations
- Commitment control
- API call patterns
- Service programs and binding (NOMAIN, EXPORT)
- Parameter passing options (CONST, *NOPASS,*OMIT, *VARSIZE)
- Return values vs output parameters
- Performance optimization patterns
- Activation group management
- **Embedded SQL** (cursors, INSERT/UPDATE/DELETE, dynamic SQL, stored procedures)
- **IFS Operations** (file I/O, directory operations, file stats)
- **XML and JSON** (parsing and generation)
- **HTTP/Web Services** (REST, SOAP)
- Advanced file locking patterns
- User space operations
- Multiple threading (job submission, data queues)
- External program calls
- Object-oriented features (procedure pointers, factory pattern)

**When to use**: Reference when encountering ILE RPG features, SQL, web services, or modern RPG capabilities.

---

### 📋 [pseudocode-rpg-migration-guide.md](pseudocode-rpg-migration-guide.md)

**Size**: ~530 lines  
**Use for**: Migration planning and best practices

**Contains**:

- Advanced refactoring patterns
- Translation workflow (7-step process)
- **Migration Best Practices**:
  - Decimal precision strategy
  - Indicator refactoring (4 phases)
  - File I/O abstraction layers
  - Error handling modernization
  - Transaction patterns
  - Testing strategy with examples
  - Performance considerations
- **Common Migration Challenges**:
  - MOVE/MOVEL operations
  - Overlay and shared memory
  - Multiple occurrence data structures
  - Special value comparisons
- Documentation template

**When to use**: Use this for project planning, establishing migration strategies, and understanding how to modernize legacy RPG patterns.

---

## Translation Workflow

For a complete RPG program migration:

1. **Analyze** with [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md)
   - Map specifications (H/F/D/C/P)
   - Identify data types and file declarations

2. **Convert Data** with [pseudocode-rpg-data-structures.md](pseudocode-rpg-data-structures.md)
   - Transform DS patterns
   - Handle special cases (OVERLAY, LIKEDS, multiple occurrence)

3. **Translate Logic** with [pseudocode-rpg-patterns.md](pseudocode-rpg-patterns.md)
   - Apply common translation patterns
   - Follow critical rules
   - Avoid documented pitfalls

4. **Handle Functions** with [pseudocode-rpg-functions.md](pseudocode-rpg-functions.md)
   - Convert BIFs to pseudocode equivalents

5. **Address Advanced Features** with [pseudocode-rpg-advanced.md](pseudocode-rpg-advanced.md)
   - Translate SQL, web services, IFS operations
   - Handle service programs and binding

6. **Apply Best Practices** with [pseudocode-rpg-migration-guide.md](pseudocode-rpg-migration-guide.md)
   - Refactor indicators and error handling
   - Establish testing strategy
   - Document migration decisions

---

## Quick Reference

### Critical Data Type Rules

- **Packed Decimal (nP m)** → `DECIMAL(n,m)` - **NEVER USE FLOAT FOR MONEY!**
- **Indicators (*INxx)** → Named `BOOLEAN` variables
- **Arrays** → 1-based indexing in RPG, adjust for target language
- **Dates** → Handle format differences (*ISO,*USA, *EUR,*MDY, etc.)

### Most Common Patterns

- File loop: `READ` in `DOW NOT %EOF` loop
- Key processing: `SETLL` + `READE` in loop
- Update: `CHAIN` + check `%FOUND` + `UPDATE`
- Error handling: `MONITOR`/`ON-ERROR` → TRY/CATCH

### Files Referenced

- [pseudocode-common-rules.md](pseudocode-common-rules.md) - Base pseudocode syntax
- [testing-strategy.md](testing-strategy.md) - Unit testing approaches
- [transaction-handling.md](transaction-handling.md) - Transaction patterns
- [performance-patterns.md](performance-patterns.md) - Optimization techniques
- [messaging-integration.md](messaging-integration.md) - Message queue integration

---

## File Statistics

| File | Lines | Focus | Complexity |
|------|-------|-------|------------|
| pseudocode-rpg-core-rules.md | ~400 | Foundation | Basic |
| pseudocode-rpg-functions.md | ~130 | BIFs | Basic |
| pseudocode-rpg-data-structures.md | ~200 | Data Patterns | Intermediate |
| pseudocode-rpg-patterns.md | ~700 | Common Idioms | Intermediate |
| pseudocode-rpg-advanced.md | ~1,300 | ILE RPG, SQL, Web | Advanced |
| pseudocode-rpg-migration-guide.md | ~530 | Best Practices | Strategic |
| **Total** | **~3,260** | **Complete Coverage** | **All Levels** |

---

## Version History

### v2.0 - Split Architecture (2026-01-20)

- **Breaking Change**: Split single 2,993-line file into 6 focused files
- **Benefit**: Improved Agent Skills compliance (<500 line recommendation)
- **Benefit**: Easier context loading for AI agents
- **Benefit**: Progressive disclosure of complexity

### v1.0 - Monolithic (Original)

- Single comprehensive file
- 2,993 lines total
- Complete but difficult to navigate

---

**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide
