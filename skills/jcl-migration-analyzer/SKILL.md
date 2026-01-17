---
name: jcl-migration-analyzer
description: Analyzes legacy JCL (Job Control Language) scripts to assist with migration to modern workflow orchestration and batch processing systems. Extracts job flows, step sequences, data dependencies, conditional logic, and program invocations. Generates migration reports and creates implementation strategies for Spring Batch, Apache Airflow, or shell scripts. Use when working with mainframe job migration, JCL analysis, batch workflow modernization, or when users mention JCL conversion, analyzing .jcl/.JCL files, working with job steps, procedures, or planning workflow orchestration from JCL jobs.
---

# JCL Migration Analyzer

Analyzes legacy JCL scripts for migration to modern batch processing and workflow orchestration systems like Spring Batch, Apache Airflow, Kubernetes Jobs, or shell scripts.

## Core Capabilities

### 1. Job Analysis
Extract job structure (JOB card), step sequences, program invocations (EXEC PGM/PROC), conditional logic (COND, IF/THEN/ELSE), return codes, data sets (DD statements), resource requirements, and symbolic parameters.

### 2. Data Dependency Mapping
Extract input/output datasets, temporary datasets, GDG handling, concatenation, DISP parameters, and data flow between steps.

### 3. Procedure Analysis
Parse PROC definitions, symbolic parameters, PROC overrides, nested procedures, INCLUDE statements, and JCLLIB references.

### 4. Workflow Migration
Generate Spring Batch jobs, Apache Airflow DAGs, Kubernetes Jobs, shell scripts, AWS Step Functions, or Azure Logic Apps.

### 5. Conditional Logic Translation
**CRITICAL**: COND logic is INVERTED! Map COND parameters, IF/THEN/ELSE, return codes, step bypassing, and restart logic to modern constructs.

## Quick Usage Guide

### Find Jobs
```bash
find . -name "*.jcl" -o -name "*.JCL"
find . -name "*.proc" -o -name "*.PROC"
```

### Critical: COND Logic is INVERTED!

**JCL COND (inverted):**
```jcl
//STEP020 EXEC PGM=PROG2,COND=(0,NE)
```
Means: "Run if previous RC ≠ 0" → **Run on ERROR!**

**Modern (normal logic):**
```bash
if [ $rc -ne 0 ]; then run_prog2; fi
```

**JCL IF/THEN (normal logic):**
```jcl
//IF1 IF RC = 0 THEN
//STEP020 EXEC PGM=PROG2
//ENDIF
```

**Modern:**
```bash
if [ $rc -eq 0 ]; then run_prog2; fi
```

### Code Patterns

**Simple Sequential:**
```jcl
//STEP010 EXEC PGM=PROG1
//INPUT   DD DSN=INPUT.FILE,DISP=SHR
//OUTPUT  DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG)
//STEP020 EXEC PGM=PROG2
//INPUT   DD DSN=OUTPUT.FILE,DISP=SHR
```
```bash
#!/bin/bash
set -e
prog1 --input="input.file" --output="output.file" || exit 8
prog2 --input="output.file" || exit 8
```

**Conditional (COND - inverted!):**
```jcl
//STEP010 EXEC PGM=VALIDATE
//STEP020 EXEC PGM=PROCESS,COND=(0,NE)
```
```bash
validate_data
rc=$?
if [ $rc -ne 0 ]; then process_data; fi  # INVERTED!
```

**IF/THEN/ELSE (normal logic):**
```jcl
//STEP010 EXEC PGM=VALIDATE
//IF1 IF RC = 0 THEN
//STEP020 EXEC PGM=PROCESSOK
//ELSE
//STEP030 EXEC PGM=PROCESSERR
//ENDIF
```
```bash
validate_data
rc=$?
if [ $rc -eq 0 ]; then processok; else processerr; fi
```

**Procedure:**
```jcl
//MYPROC PROC MEMBER=,INFILE=
//STEP1  EXEC PGM=PROG1
//SYSIN  DD DSN=&MEMBER,DISP=SHR
//       PEND
```
```bash
function myproc() {
    prog1 --sysin="$1" --input="$2"
}
myproc "test.data" "prod.file"
```

### Target Platforms

**Spring Batch:**
```java
@Bean
public Job job() {
    return jobBuilderFactory.get("job")
        .start(step1()).next(step2())
        .on("FAILED").to(errorStep())
        .from(step2()).on("*").to(step3())
        .end().build();
}
```

**Airflow DAG:**
```python
with DAG('job', schedule_interval='@daily') as dag:
    step1 = BashOperator(task_id='step1', bash_command='prog1.sh')
    step2 = BashOperator(task_id='step2', bash_command='prog2.sh')
    step1 >> step2
```

## Key Patterns

**Error Handling:** COND-based → `if [ $rc -ne 0 ]; then error_handler; fi`
**GDG:** `GDG(0)` → `get_latest_generation`, `GDG(+1)` → `create_new_generation`
**Concatenation:** Multiple DD → `cat file1 file2 file3 | process`
**Restart:** COND restart → checkpoint files (`touch .checkpoint_step`)

### Return Code Reference

| RC | Meaning | Action |
|----|---------|--------|
| 0 | Success | Continue |
| 4 | Warning | Continue (informational) |
| 8 | Error | May continue based on COND |
| 12 | Severe Error | Typically stop |
| 16 | Fatal Error | Abort job |

## Migration Checklist

- [ ] Extract job structure, list steps in order, identify programs/procedures, document COND/IF logic
- [ ] Map input/output datasets, identify temp datasets, document GDG usage, track data dependencies
- [ ] Convert COND to normal logic (**INVERT!**), translate IF/THEN/ELSE, handle error paths
- [ ] Choose target (Spring Batch/Airflow/shell), define job structure, implement steps, add monitoring
- [ ] Test normal path, error conditions, conditional branches with production-like data
- [ ] Document job purpose, schedule, dependencies, special requirements

## Critical Tips

1. **COND is INVERTED** - step runs when condition is FALSE! Draw truth tables if needed.
2. **Return codes**: 0=success, 4=warning (OK), 8+=error
3. **Data dependencies**: Carefully map to avoid race conditions
4. **Restart capability**: Implement checkpointing if needed
5. **Monitoring**: Add logging and alerting to modern workflows

## Output Structure

Provide: Job overview, step sequence, data flow, conditional logic, migration target, workflow definition, migration estimate, action items.

## Bundled Resources

### Scripts (scripts/)
Available automation tools for JCL analysis:
- **analyze-dependencies.sh/.ps1**: Generate dependency graphs in JSON format
- **extract-structure.py**: Extract structural information from JCL files  
- **generate-java-classes.py**: Generate Java POJOs from data structures
- **estimate-complexity.py**: Estimate migration complexity and effort

Use these scripts when detailed structural analysis is needed or for batch processing multiple JCL files.

### References (references/)
Load these on-demand for detailed guidance:
- **pseudocode-common-rules.md**: General pseudocode syntax and conventions
- **pseudocode-jcl-rules.md**: JCL-specific translation rules and patterns (load when generating pseudocode)
- **testing-strategy.md**: Comprehensive testing approach for migrated workflows (load when planning testing)
- **transaction-handling.md**: Transaction patterns and ACID compliance (load when dealing with transactional jobs)
- **messaging-integration.md**: Message queue integration patterns (load when jobs involve messaging)
- **performance-patterns.md**: Performance optimization strategies (load when optimizing workflows)

### Templates (assets/)
- **migration-report-template.md**: Standard format for migration analysis reports
- **java-class-template.java**: Template for generating Java classes from data structures

## Integration

Works with job schedulers (Control-M, cron), workflow platforms (Spring Batch, Airflow, K8s), monitoring tools, version control, and CI/CD pipelines.
