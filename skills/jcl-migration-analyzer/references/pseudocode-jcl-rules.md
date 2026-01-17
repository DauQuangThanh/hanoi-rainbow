# JCL Translation Rules

**Prerequisites**: Read [PSEUDOCODE-COMMON-RULES.md](PSEUDOCODE-COMMON-RULES.md) for syntax, naming, and structure.

---

## Element Mapping

| JCL Element | Pseudocode Equivalent |
|-------------|----------------------|
| `//JOB` | Job Definition |
| `//EXEC PGM=` | Execute Program |
| `//EXEC PROC=` | Call Procedure |
| `//DD` | Dataset Definition |
| `//COND` | Conditional Execution |
| `//IF/THEN/ELSE` | IF-THEN-ELSE logic |
| `//PROC` | Procedure Definition |

## Return Code Mapping

| RC | Meaning | Pseudocode |
|----|---------|-----------|
| 0 | Success | Success |
| 4 | Warning | Warning (continue) |
| 8 | Error | Error (may stop) |
| 12 | Severe error | Severe Error (stop) |
| 16 | Fatal error | Fatal Error (abort) |

## DISP Parameter Translation

| DISP | Status | Normal End | Abnormal End | Pseudocode |
|------|--------|------------|--------------|-----------|
| `SHR` | Shared | Keep | Keep | READ_SHARED(dataset) |
| `OLD` | Exclusive | Keep | Keep | READ_EXCLUSIVE(dataset) |
| `NEW` | Create | CATLG | DELETE | CREATE(dataset) |
| `MOD` | Append | Keep | Keep | APPEND(dataset) |

## Translation Patterns

### Job Step Sequence
```jcl
//STEP010  EXEC PGM=PROG1
//STEP020  EXEC PGM=PROG2
//STEP030  EXEC PGM=PROG3
```
→
```
JOB JobName
BEGIN
    STEP Step010
        EXECUTE PROG1
        IF RETURN_CODE > 0 THEN STOP
    END STEP
    
    STEP Step020
        EXECUTE PROG2
        IF RETURN_CODE > 0 THEN STOP
    END STEP
    
    STEP Step030
        EXECUTE PROG3
    END STEP
END JOB
```

### COND Logic (⚠️ INVERTED!)
```jcl
//STEP020  EXEC PGM=PROG2,COND=(0,NE)
```
→
```
STEP Step020
    IF PREVIOUS_RC = 0 THEN SKIP  // INVERTED: run if condition FALSE!
    EXECUTE PROG2
END STEP
```

**CRITICAL**: COND logic is inverted - step runs if condition is FALSE!

### IF/THEN/ELSE
```jcl
//IF1      IF RC = 0 THEN
//STEP020  EXEC PGM=PROG2
//ENDIF
```
→
```
IF PREVIOUS_RC = 0 THEN
    STEP Step020
        EXECUTE PROG2
    END STEP
END IF
```

### DD Statement
```jcl
//INPUT    DD DSN=PROD.DATA.FILE,DISP=SHR
//OUTPUT   DD DSN=PROD.OUTPUT.FILE,DISP=(NEW,CATLG,DELETE)
```
→
```
input = OPEN_DATASET("PROD.DATA.FILE", SHARED, READ)
output = CREATE_DATASET("PROD.OUTPUT.FILE")
    // Normal end: CATALOG
    // Abnormal end: DELETE
```

### Proc Call with Symbolics
```jcl
//PROC1    PROC MEMBER=,INFILE=
//STEP1    EXEC PGM=PROG1
//SYSIN    DD DSN=&MEMBER,DISP=SHR
//INPUT    DD DSN=&INFILE,DISP=SHR
//         PEND
//CALLPROC EXEC PROC1,MEMBER=TEST.DATA,INFILE=PROD.FILE
```
→
```
PROCEDURE Proc1(member: STRING, inFile: STRING)
BEGIN
    STEP Step1
        EXECUTE PROG1
        sysin = OPEN_DATASET(member, SHARED)
        input = OPEN_DATASET(inFile, SHARED)
    END STEP
END PROCEDURE

CALL Proc1("TEST.DATA", "PROD.FILE")
```

## Critical Rules

1. **COND is inverted**: Step runs when condition is FALSE (opposite of normal IF)
2. **Return codes**: 0=success, 4=warning (OK), 8+=error
3. **DISP triple**: (status, normal-end, abnormal-end)
4. **Sequential execution**: Steps run in order unless COND/IF skips
5. **Dataset lifecycle**: Track NEW→CATLG→DELETE transitions
6. **Symbolic parameters**: &VAR replaced with actual values

## Translation Workflow

1. Parse JOB card → Job Definition
2. For each STEP: Extract PGM/PROC → Execution steps
3. Convert COND → Inverted IF logic (!condition)
4. Map IF/THEN/ELSE → Standard conditionals
5. Translate DD → Dataset operations
6. Convert PROC → Procedure definitions
7. Generate Mermaid flowchart showing step dependencies
8. Document return code handling

**Reference**: IBM z/OS MVS JCL Reference, JCL User's Guide
