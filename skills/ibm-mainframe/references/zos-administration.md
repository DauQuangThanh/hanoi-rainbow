# z/OS System Administration

## System Operations

### IPL (Initial Program Load)

**Cold start:**
```
1. Issue QUIESCE command
2. Select IPL volume
3. Specify LOADPARM parameters
4. Monitor console for IPL messages
```

**Warm start (system restart):**
```
C U=CPU,RESTART        /* Restart specific CPU */
```

### Console Commands

**Display commands:**
```
/D A,ALL                /* All active address spaces */
/D A,L                  /* Address spaces by LPAR */
/D M=CPU               /* CPU usage */
/D M=STOR              /* Storage usage */
/D U,ALL               /* All units */
/D PROG,APF            /* APF authorized libraries */
/D PROD,STATE          /* Product status */
/D IOS,CONFIG          /* I/O configuration */
/D SMS,STORGRP(ALL)    /* Storage groups */
```

**System control:**
```
/V OFFLINE             /* Vary device offline */
/V ONLINE              /* Vary device online */
/STOP subsystem        /* Stop subsystem */
/START subsystem       /* Start subsystem */
/CANCEL jobname        /* Cancel job */
/F jobname,command     /* Send command to job */
```

### JES2 Commands

**Job management:**
```
/$DA                   /* Display active jobs */
/$DQ                   /* Display job queue */
/$DJ jobname           /* Display job status */
/$PJOBNAME            /* Purge job */
/$CANCEL jobname       /* Cancel job */
```

**Device management:**
```
/$DSPL                 /* Display spool */
/$DDEV                 /* Display devices */
/$S PRINTER1           /* Start printer */
/$P PRINTER1           /* Stop printer */
```

**Configuration:**
```
/$T A=ALL              /* Display all options */
/$T JOBCLASS(A),QHOLD  /* Hold job class */
/$T JOBCLASS(A),QREL   /* Release job class */
```

## Storage Management

### SMS (Storage Management Subsystem)

**Storage groups:**
```
DEFINE STORAGEGROUP(STGRP1)
  VOLUMES(VOL001 VOL002 VOL003)
  VSAMDATACLASS(VSAM1)
  AVGREC(U)
  
LISTCAT STORAGEGROUP(STGRP1) ALL
```

**Management classes:**
```
DEFINE MANAGEMENTCLASS(MGMT1)
  MIGRATION(DAYS(30))
  BACKUP(DAYS(7))
  RETENTION(DAYS(365))
```

**Data classes:**
```
DEFINE DATACLASS(DCLASS1)
  RECFM(FB)
  LRECL(80)
  SPACEAVG(100 CYL)
```

### Dataset Migration & Backup

**DFHSM (HSM) commands:**
```
HSEND MIGRATE DSN('USER.DATA.**')
HSEND RECALL DSN('USER.DATA.FILE')
HSEND BACKUP DSN('USER.DATA.**')
HSEND RECOVER DSN('USER.DATA.FILE')
HSEND DELETE BACKUP DSN('USER.DATA.FILE')
```

**ADRDSSU (DSS) backup:**
```
//BACKUP   EXEC PGM=ADRDSSU
//SYSPRINT DD SYSOUT=*
//DASD1    DD UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD
//TAPE1    DD DSN=BACKUP.TAPE,UNIT=TAPE,
//            VOL=SER=TAPE01,DISP=(NEW,CATLG)
//SYSIN    DD *
  DUMP DATASET( -
    INCLUDE(**) -
    BY(DSORG(PS,PO)) ) -
    INDDNAME(DASD1) -
    OUTDDNAME(TAPE1) -
    COMPRESS
/*
```

## System Maintenance

### SMP/E (System Modification Program)

**Apply maintenance:**
```
SET BDY(GLOBAL)
RECEIVE S(PTF12345)
SET BDY(TARGET)
APPLY S(PTF12345) CHECK
APPLY S(PTF12345)
SET BDY(DLIB)
ACCEPT S(PTF12345)
```

**List maintenance:**
```
LIST PTFS
LIST SYSMOD(PTF12345)
LIST MOD(IEFBR14)
```

### LLA (Library Lookaside)

**Refresh LLA:**
```
/F LLA,UPDATE=xx
/F LLA,REFRESH
/F LLA,DISPLAY
```

### System Parameters (PARMLIB)

**IPL parameters (LOADxx):**
```
SYSCAT    CATALOG.MASTER
NUCLEUS   01
PARMLIB   SYS1.PARMLIB
SYSPLEX   PLEXNAME
```

**System options (IEASYSxx):**
```
APF=00           /* APF list */
CMD=00           /* Commands */
CON=00           /* Consoles */
GRS=YES          /* Global Resource Serialization */
PROG=00          /* Programs */
SMF=00           /* SMF parameters */
```

## Subsystem Management

### Subsystem Initialization Table (IEFSSNxx)

```
SUBSYS SUBNAME(JES2)
  INITRTN(HASJES20)
  INITPARM('WARM')
  
SUBSYS SUBNAME(CICS)
  INITRTN(DFHSIP)
  
SUBSYS SUBNAME(DB2P)
  INITRTN(DSNMSTR)
```

### Dynamic Subsystem Control

**Add subsystem:**
```
/SETSSI ADD,S=subsys,I=initrtn
```

**Delete subsystem:**
```
/SETSSI DELETE,S=subsys
```

## Performance Monitoring

### RMF (Resource Measurement Facility)

**Start RMF:**
```
/S RMF,PARM=xx
/F RMF,START(type)
```

**RMF reports:**
- **Monitor I**: CPU activity
- **Monitor II**: DASD activity
- **Monitor III**: Workload activity
- **OVERVIEW**: System summary
- **WKLD**: Workload by service class

### SMF (System Management Facilities)

**SMF parameters (SMFPRMxx):**
```
SYS(INTERVAL(30))
SUB(TYPE(0,30,70,80,89,90))
REC(EXITS(SYS(ON),SUBSYS(ON)))
JWT(INTERVAL(15))
```

**Extract SMF data:**
```
//EXTRACT  EXEC PGM=IFASMFDP
//SYSPRINT DD SYSOUT=*
//DUMPIN   DD DISP=SHR,DSN=SYS1.MANX
//DUMPOUT  DD DISP=(NEW,CATLG),DSN=SMF.EXTRACT,
//            SPACE=(CYL,(50,10))
//SYSIN    DD *
  INDD(DUMPIN,OPTIONS(DUMP))
  OUTDD(DUMPOUT,TYPE(30,80))
  DATE(2024001,2024365)
/*
```

## Automation

### Automation Table (ATx00)

**Define automated responses:**
```
IF MSGID = 'IEF404I' THEN
  EXEC(PGM=CLEANUP,PARM='&JOBNAME')
ENDIF

IF TEXT = 'HASP395' THEN
  DO
    SUB('RESTART.JCL')
  DONE
ENDIF
```

### NetView Automation

**AutoOps definitions:**
```
IF (MSGID = 'IEE361I') THEN
  DO
    WRITE('MOUNT VOLUME &1')
    WAIT(MSGID = 'IEA511I')
    CLIST(AUTOVOLM,&1)
  DONE
ENDIF
```

## Disaster Recovery

### Parallel Sysplex

**Coupling Facility structures:**
```
STRNAME  TYPE    SIZE     POLICY
LOCK01   LOCK    256KB    SIZE(256)
SCA01    SCA     512KB    SIZE(512)
LOGR01   LOGR    1024KB   SIZE(1024)
```

**XCF (Cross-System Coupling Facility):**
```
/D XCF                  /* Display XCF status */
/D XCF,STR,STRNAME=ALL  /* Display structures */
/D XCF,GROUP            /* Display groups */
```

### GDPS (Geographically Dispersed Parallel Sysplex)

**Configuration:**
- Primary site with active workload
- Secondary site with standby systems
- Metro Mirror or Global Mirror for data replication
- Automation for workload failover

## System Tuning

### LPAR Configuration

**Adjust LPAR weights:**
```
/T LPAR=LPAR1,WEIGHT=50
/D GRS,RES=(*,*)       /* Display GRS resources */
```

### Workload Manager (WLM)

**Service classes:**
```
SERVICE CLASS: CICS_PROD
  IMPORTANCE: 1
  GOALS: RESPONSE(90%,0.2SEC)
  
SERVICE CLASS: BATCH
  IMPORTANCE: 3
  GOALS: VELOCITY(50)
```

**Application environments:**
- Define Java, CICS, IMS environments
- Resource limits (CPU, memory, threads)
- Classification rules

## System Security

### APF (Authorized Program Facility)

**APF list (PROGxx):**
```
APF ADD DSNAME(SYS1.USERLIB) VOLUME(SYSR01)
APF DELETE DSNAME(OLD.LOAD.LIB)
```

**Dynamic APF:**
```
SETPROG APF,ADD,DSNAME=SYS1.NEWLIB,VOLUME=VOL001
SETPROG APF,DELETE,DSNAME=OLD.LOAD.LIB
```

### Program Properties Table (PPT)

**Define in SCHEDxx:**
```
PPT PGMNAME(MYPROG)
  KEY(7)
  NOSWAP
  PRIV
  PASS
```

## Console Operations

### Master Console (MSTR)

**Console definition (CONSOLxx):**
```
CONSOLE DEVNUM(001C)
  NAME(MSTCONS)
  AUTH(MASTER)
  CMDSYS(ALL)
  AREA(F)
  ROUTCDE(ALL)
  MSCOPE(*)
  
CONSOLE DEVNUM(001D)
  NAME(SYSTEC)
  AUTH(SYS)
```

### Console Security

**RACF console profiles:**
```
RDEFINE OPERCMDS MVS.VARY.TAPE.* UACC(NONE)
PERMIT MVS.VARY.TAPE.* CLASS(OPERCMDS) ID(OPERGRP) ACCESS(UPDATE)
SETROPTS RACLIST(OPERCMDS) REFRESH
```

## System Capacity Planning

### Monitoring Points

**CPU:**
- LPAR utilization
- Service class CPU consumption
- Transaction response times

**Storage:**
- Real storage usage
- Auxiliary storage usage
- Common area usage

**I/O:**
- DASD response times
- Channel utilization
- Cache hit ratios

### Capacity Modeling

**Data collection:**
1. RMF data (CPU, storage, I/O)
2. SMF records (workload characteristics)
3. CICS statistics
4. DB2 statistics

**Analysis:**
- Peak usage times
- Growth trends
- Resource bottlenecks
- Service level compliance

**Planning:**
- Hardware upgrades
- LPAR weight adjustments
- Workload balancing
- Storage expansion

## Troubleshooting

### System Dumps

**Initiate dump:**
```
/DUMP COMM=('System dump reason')
```

**IPCS (Interactive Problem Control System):**
```
IPCS
SETDEF FILE(dataset) DSNAME('DUMP.DATA')
STATUS
SYSTRACE CT
SUMMARY
VERBEXIT CBFORMAT
```

### Wait States

**Common wait codes:**
- **001**: WTOR (Write To Operator with Reply)
- **021**: Master trace buffer full
- **02E**: Trace table full
- **091**: Unit check on DASD

### Performance Issues

**High CPU:**
```
/D A,ALL               /* Check active jobs */
/F RMF,D              /* RMF display */
/D PROG,LNKLST        /* Check link list */
```

**Storage shortage:**
```
/D M=STOR             /* Display storage */
/D ASM                /* Display auxiliary storage */
/D GRS,RES=(*,*)      /* Check for serialization */
```

## Best Practices

### Daily Operations
1. Review system logs (SYSLOG, OPERLOG)
2. Check job completion status
3. Monitor space utilization
4. Review SMF data for anomalies
5. Verify backup completion

### Change Management
1. Test in development environment first
2. Schedule maintenance windows
3. Document all changes
4. Have backout procedures ready
5. Communicate with stakeholders

### Security
1. Regular RACF audits
2. Review unauthorized attempts
3. Monitor privileged access
4. Keep security patches current
5. Implement separation of duties

### Performance
1. Baseline normal operations
2. Trend analysis
3. Proactive tuning
4. Regular capacity reviews
5. Workload balancing
