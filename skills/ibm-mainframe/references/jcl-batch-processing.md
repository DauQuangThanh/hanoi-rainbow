# JCL & Batch Processing

## JCL Fundamentals

### Job Statement
```jcl
//JOBNAME  JOB (account),'programmer-name',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,
//         REGION=0M,
//         TIME=(0,30)
```

**Parameters:**
- **CLASS**: Job class for execution priority
- **MSGCLASS**: Output class for job logs
- **MSGLEVEL**: (statements,messages) for job log detail
- **NOTIFY**: User to notify on completion
- **REGION**: Memory allocation (0M = unlimited)
- **TIME**: Maximum execution time (minutes,seconds)

### EXEC Statement
```jcl
//STEPNAME EXEC PGM=program-name,
//         PARM='parameters',
//         COND=(code,operator),
//         TIME=(0,10),
//         REGION=4M
```

**Or call a procedure:**
```jcl
//STEPNAME EXEC procedure-name,
//         PARM.stepname='parameters'
```

### DD Statement
```jcl
//ddname   DD DSN=dataset-name,
//         DISP=(status,normal-end,abnormal-end),
//         SPACE=(unit,(primary,secondary),RLSE),
//         UNIT=device-type,
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
```

## Common JCL Patterns

### 1. Copy Dataset
```jcl
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=SOURCE.DATA,DISP=SHR
//SYSUT2   DD DSN=TARGET.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(100,10),RLSE),
//         UNIT=SYSDA,
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
```

### 2. Sort Data
```jcl
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=INPUT.DATA,DISP=SHR
//SORTOUT  DD DSN=SORTED.DATA,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(CYL,(10,5),RLSE),
//         UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,20,5,ZD,D)
  SUM FIELDS=(30,8,ZD)
/*
```

### 3. Conditional Execution
```jcl
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2,COND=(0,NE,STEP1)
//STEP3    EXEC PGM=PROG3,COND=((0,NE,STEP1),(0,NE,STEP2))
```

**Or using IF/THEN/ELSE:**
```jcl
//STEP1    EXEC PGM=PROG1
//IF1      IF (STEP1.RC = 0) THEN
//STEP2      EXEC PGM=PROG2
//ENDIF    ENDIF
```

### 4. GDG (Generation Data Group)
```jcl
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG(NAME(MY.GDG.BASE) -
    LIMIT(7) -
    SCRATCH -
    NOEMPTY)
/*

//CREATE   EXEC PGM=IEBGENER
//SYSUT1   DD *
data
/*
//SYSUT2   DD DSN=MY.GDG.BASE(+1),
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(10,5)),
//         UNIT=SYSDA
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
```

**Reference GDG versions:**
- `(0)` = Current generation
- `(+1)` = New generation
- `(-1)` = Previous generation
- `(-2)` = Two generations back

### 5. Concatenated Datasets
```jcl
//INPUT    DD DSN=FILE1.DATA,DISP=SHR
//         DD DSN=FILE2.DATA,DISP=SHR
//         DD DSN=FILE3.DATA,DISP=SHR
```

### 6. In-stream Data
```jcl
//SYSIN    DD *
input data line 1
input data line 2
/*

//Or with delimiter:
//SYSIN    DD DATA,DLM=$$
input data with /* inside
$$
```

### 7. Temporary Datasets
```jcl
//TEMP1    DD DSN=&&TEMPFILE,
//         DISP=(NEW,PASS),
//         SPACE=(CYL,(5,1)),
//         UNIT=SYSDA
```

## JCL Utilities

### IEFBR14 - Allocate/Delete
```jcl
//ALLOC    EXEC PGM=IEFBR14
//NEWFILE  DD DSN=MY.NEW.FILE,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(CYL,(10,5)),
//         UNIT=SYSDA,
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)

//DELETE   EXEC PGM=IEFBR14
//OLDFILE  DD DSN=MY.OLD.FILE,DISP=(OLD,DELETE,DELETE)
```

### IEBGENER - Copy
```jcl
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=INPUT,DISP=SHR
//SYSUT2   DD DSN=OUTPUT,DISP=(NEW,CATLG)
//SYSIN    DD *
  GENERATE MAXFLDS=1
  RECORD FIELD=(80,1,,1)
/*
```

### IEBCOPY - Copy PDS
```jcl
//COPY     EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INDD     DD DSN=SOURCE.PDS,DISP=SHR
//OUTDD    DD DSN=TARGET.PDS,DISP=(NEW,CATLG)
//SYSIN    DD *
  COPY INDD=INDD,OUTDD=OUTDD
  SELECT MEMBER=(MEM1,MEM2,MEM3)
/*
```

### IEBUPDTE - Update PDS
```jcl
//UPDATE   EXEC PGM=IEBUPDTE
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=MY.PDS,DISP=OLD
//SYSUT2   DD DSN=MY.PDS,DISP=OLD
//SYSIN    DD *
./ ADD NAME=MEMBER1,LIST=ALL
source line 1
source line 2
./ ENDUP
/*
```

### DFSORT - Sort/Merge
```jcl
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=INPUT,DISP=SHR
//SORTOUT  DD DSN=OUTPUT,DISP=(NEW,CATLG)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,15,5,ZD,D)
  INCLUDE COND=(20,2,CH,EQ,C'NY')
  OUTREC FIELDS=(1,80,100X)
/*
```

**Advanced sort:**
```jcl
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
  SUM FIELDS=(20,8,ZD,30,8,PD)
  OUTFIL FNAMES=OUT1,INCLUDE=(15,2,CH,EQ,C'CA'),
    BUILD=(1,10,15,20)
  OUTFIL FNAMES=OUT2,INCLUDE=(15,2,CH,EQ,C'NY'),
    BUILD=(1,10,15,20)
/*
```

### IDCAMS - Catalog Management
```jcl
//IDCAMS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Define VSAM cluster */
  DEFINE CLUSTER( -
    NAME(MY.VSAM.FILE) -
    KEYS(10 0) -
    RECORDSIZE(100 100) -
    CYLINDERS(5 1) -
    INDEXED -
    SHAREOPTIONS(2 3)) -
  DATA( -
    NAME(MY.VSAM.FILE.DATA)) -
  INDEX( -
    NAME(MY.VSAM.FILE.INDEX))
    
  /* Copy */
  REPRO INFILE(INDD) OUTFILE(OUTDD)
  
  /* Delete */
  DELETE MY.OLD.FILE CLUSTER
  
  /* List catalog */
  LISTCAT ENTRIES(MY.VSAM.FILE) ALL
/*
```

## Procedures (PROCs)

### Define Procedure
```jcl
//MYPROC   PROC MBR=,LIB=USER.LOAD
//STEP1    EXEC PGM=PROG1
//STEPLIB  DD DSN=&LIB,DISP=SHR
//INPUT    DD DSN=INPUT.&MBR,DISP=SHR
//OUTPUT   DD DSN=OUTPUT.&MBR,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(CYL,(5,1)),
//         UNIT=SYSDA
```

### Call Procedure
```jcl
//CALL     EXEC MYPROC,MBR=TEST01,LIB=PROD.LOAD
```

### Override DD in Procedure
```jcl
//CALL     EXEC MYPROC
//STEP1.INPUT DD DSN=ALTERNATE.INPUT,DISP=SHR
//STEP1.OUTPUT DD DSN=ALTERNATE.OUTPUT,DISP=(NEW,CATLG)
```

## Symbolic Parameters

### JCL Symbols
```jcl
//SET1     SET ENV='PROD'
//SET2     SET DATE='&YYMMDD'
//STEP1    EXEC PGM=PROG1
//INPUT    DD DSN=&ENV..DATA.&DATE,DISP=SHR
```

**System symbols:**
- `&SYSUID` - User ID
- `&SYSDATE` - Current date (yyddd)
- `&SYSTIME` - Current time (hhmm)
- `&LDAY` - Day of month (dd)
- `&LMONTH` - Month (mm)
- `&LYEAR` - Year (yyyy)

### Example with Symbols
```jcl
//BACKUP   JOB
//SET1     SET DATE='&LYEAR&LMONTH&LDAY'
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD DSN=PROD.DATA,DISP=SHR
//SYSUT2   DD DSN=BACKUP.DATA.D&DATE,
//         DISP=(NEW,CATLG)
//SYSIN    DD DUMMY
//SYSPRINT DD SYSOUT=*
```

## Batch Scheduling

### Job Dependencies

**Using JES2 job dependency:**
```jcl
//JOB1     JOB
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2

//JOB2     JOB
//*AFTER JOB1
//STEP1    EXEC PGM=PROG3
```

**Using automation tools:**
- TWS (Tivoli Workload Scheduler)
- Control-M
- CA-7
- Zeke

### Job Submission Automation

**Submit via TSO SUBMIT:**
```
SUBMIT 'MY.JCL(JOBNAME)'
```

**Submit via automation:**
```rexx
/* REXX script */
QUEUE "//JOBNAME  JOB"
QUEUE "//STEP1    EXEC PGM=IEFBR14"
"SUBMIT * END(/*)"
```

## Performance Optimization

### Efficient JCL Practices

1. **Block size optimization:**
```jcl
//FILE     DD DSN=MY.DATA,
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
/* Optimal: BLKSIZE = track size - control bytes */
/* For 3390: ~27920 for LRECL=80 */
```

2. **Space allocation:**
```jcl
//FILE     DD SPACE=(CYL,(primary,secondary),RLSE)
/* Use RLSE to release unused space */
/* Allocate enough primary to avoid secondary extends */
```

3. **Temporary datasets:**
```jcl
//TEMP     DD DSN=&&TEMP,
//         DISP=(NEW,PASS),
//         SPACE=(CYL,(10,5)),
//         UNIT=VIO  /* Virtual I/O in memory */
```

4. **Concatenation order:**
```jcl
/* Put most frequently accessed first */
//STEPLIB  DD DSN=FREQUENT.LOAD,DISP=SHR
//         DD DSN=LESS.FREQUENT.LOAD,DISP=SHR
```

### Parallel Execution

**Independent steps:**
```jcl
//STEP1A   EXEC PGM=PROG1A
//STEP1B   EXEC PGM=PROG1B
//STEP2    EXEC PGM=PROG2,COND=((0,NE,STEP1A),(0,NE,STEP1B))
```

**Multiple jobs:**
Submit multiple independent jobs to run in parallel.

## Error Handling

### Return Codes

**Check in JCL:**
```jcl
//STEP1    EXEC PGM=PROG1
//IF1      IF (STEP1.RC = 0) THEN
//SUCCESS    EXEC PGM=SUCCESS
//ELSE     ELSE
//FAILURE    EXEC PGM=FAILURE
//ENDIF    ENDIF
```

**Condition codes:**
```jcl
//STEP2    EXEC PGM=PROG2,COND=(4,LT,STEP1)
/* Execute STEP2 if STEP1 RC < 4 */

/* Operators: EQ NE LT LE GT GE */
```

### ABEND Handling

**Catch ABEND:**
```jcl
//STEP1    EXEC PGM=PROG1
//IF1      IF (STEP1.ABEND = TRUE) THEN
//CLEANUP    EXEC PGM=CLEANUP
//ENDIF    ENDIF
```

**Common ABENDs:**
- **S0C7**: Data exception (invalid numeric)
- **S0C4**: Protection exception (memory access)
- **S806**: Program not found
- **S322**: Time out
- **S013**: OPEN error (dataset)

## Advanced Techniques

### Dynamic Allocation

**IEFBR14 with dynamic:**
```jcl
//ALLOC    EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  ALLOC DA('MY.NEW.FILE') NEW -
    SPACE(5,1) CYLINDERS -
    RECFM(F B) LRECL(80) BLKSIZE(27920) -
    UNIT(SYSDA) CATALOG
  FREE DA('MY.OLD.FILE') DELETE
/*
```

### JCL Include

**Member in JCLLIB:**
```jcl
//MYJOB    JOB
//         JCLLIB ORDER=MY.PROCLIB
//         INCLUDE MEMBER=COMMON
//STEP1    EXEC PGM=PROG1
```

**COMMON member:**
```jcl
//STEPLIB  DD DSN=MY.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
```

### REXX Integration

```jcl
//REXX     EXEC PGM=IKJEFT01
//SYSEXEC  DD DSN=MY.REXX.LIB,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  %MYSCRIPT parm1 parm2
/*
```

## Best Practices

### Naming Conventions
- Job names: 8 characters max, alphanumeric
- Step names: meaningful, describe function
- DD names: standard names (SYSOUT, SYSPRINT, etc.)
- Dataset names: hierarchical, logical grouping

### Documentation
```jcl
//*********************************************************
//* Job: MONTHEND                                        *
//* Purpose: Monthly closing batch                       *
//* Schedule: Last day of month, 8 PM                    *
//* Dependencies: DAILYUPD must complete first           *
//* Runtime: Approximately 2 hours                       *
//* Contact: Operations Team (x1234)                     *
//*********************************************************
```

### Testing
1. Test with small datasets first
2. Use TYPRUN=SCAN to check syntax
3. Validate with COND=(0,EQ) for conditional testing
4. Keep test and production jobs separate

### Maintenance
1. Use procedures for repeated logic
2. Symbolic parameters for flexibility
3. Comment complex logic
4. Version control JCL members
5. Regular cleanup of old jobs

## Troubleshooting

### JCL Errors

**JCL ERROR:**
- Syntax errors in JCL statements
- Check JESMSGLG for details

**S806 (Program not found):**
```jcl
//STEPLIB  DD DSN=CORRECT.LOAD.LIB,DISP=SHR
```

**S013 (OPEN error):**
- Dataset not found
- DISP conflict
- Volume not mounted
- Insufficient space

**S0C4 (Protection):**
- Program error accessing memory
- Check SYSUDUMP/SYSABEND

### Performance Issues

**Long run time:**
1. Check sort efficiency
2. Optimize I/O
3. Review dataset allocation
4. Check for contention

**Space issues:**
```jcl
/* Use RLSE to return unused space */
//FILE     DD SPACE=(CYL,(10,5),RLSE)

/* Or use compression */
//TAPE     DD DSN=BACKUP,UNIT=TAPE,
//         DISP=(NEW,CATLG),
//         DCB=TRTCH=COMP
```

## Migration to Modern Batch

### Spring Batch Equivalent

**JCL batch job:**
```jcl
//DAILY    JOB
//STEP1    EXEC PGM=EXTRACT
//STEP2    EXEC PGM=TRANSFORM
//STEP3    EXEC PGM=LOAD
```

**Spring Batch:**
```java
@Bean
public Job dailyJob() {
    return jobBuilderFactory.get("dailyJob")
        .start(extractStep())
        .next(transformStep())
        .next(loadStep())
        .build();
}
```

### Airflow DAG Equivalent

```python
from airflow import DAG
from airflow.operators.bash import BashOperator

dag = DAG('daily_batch', schedule_interval='@daily')

extract = BashOperator(task_id='extract', bash_command='extract.sh', dag=dag)
transform = BashOperator(task_id='transform', bash_command='transform.sh', dag=dag)
load = BashOperator(task_id='load', bash_command='load.sh', dag=dag)

extract >> transform >> load
```
