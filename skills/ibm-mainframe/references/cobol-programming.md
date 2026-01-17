# COBOL Programming on Mainframe

## COBOL Program Structure

### Complete Program Template
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.
       AUTHOR. DEVELOPMENT TEAM.
       DATE-WRITTEN. 2024-01-15.
      *****************************************************************
      * PROGRAM: CUSTINQ - Customer Inquiry                          *
      * PURPOSE: Query customer data from DB2                        *
      * INPUT:   Customer ID from terminal                           *
      * OUTPUT:  Customer details to terminal                        *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO CUSTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CUSTOMER-RECORD.
           05  CUST-ID                 PIC 9(10).
           05  CUST-NAME               PIC X(50).
           05  CUST-ADDRESS            PIC X(100).
           05  CUST-BALANCE            PIC S9(11)V99 COMP-3.
           05  CUST-STATUS             PIC X(01).
               88  ACTIVE-CUSTOMER     VALUE 'A'.
               88  INACTIVE-CUSTOMER   VALUE 'I'.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS              PIC XX.
           88  FILE-SUCCESS            VALUE '00'.
           88  EOF-REACHED             VALUE '10'.
           88  RECORD-NOT-FOUND        VALUE '23'.
       
       01  WS-COUNTERS.
           05  WS-RECORDS-READ         PIC 9(07) COMP VALUE ZERO.
           05  WS-RECORDS-WRITTEN      PIC 9(07) COMP VALUE ZERO.
       
       01  WS-DISPLAYS.
           05  WS-EDIT-BALANCE         PIC ZZZ,ZZZ,ZZ9.99-.
       
       PROCEDURE DIVISION.
       0000-MAIN-ROUTINE.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-RECORDS
           PERFORM 3000-FINALIZE
           STOP RUN.
       
       1000-INITIALIZE.
           OPEN INPUT CUSTOMER-FILE
           IF NOT FILE-SUCCESS
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' WS-FILE-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF
           DISPLAY 'INITIALIZATION COMPLETE'.
       
       2000-PROCESS-RECORDS.
           PERFORM 2100-READ-CUSTOMER
           PERFORM UNTIL EOF-REACHED
               PERFORM 2200-PROCESS-CUSTOMER
               PERFORM 2100-READ-CUSTOMER
           END-PERFORM.
       
       2100-READ-CUSTOMER.
           READ CUSTOMER-FILE NEXT
               AT END
                   SET EOF-REACHED TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
           END-READ.
       
       2200-PROCESS-CUSTOMER.
           IF ACTIVE-CUSTOMER
               MOVE CUST-BALANCE TO WS-EDIT-BALANCE
               DISPLAY 'CUSTOMER: ' CUST-NAME
               DISPLAY 'BALANCE:  ' WS-EDIT-BALANCE
               ADD 1 TO WS-RECORDS-WRITTEN
           END-IF.
       
       3000-FINALIZE.
           CLOSE CUSTOMER-FILE
           DISPLAY 'RECORDS READ:    ' WS-RECORDS-READ
           DISPLAY 'RECORDS WRITTEN: ' WS-RECORDS-WRITTEN
           DISPLAY 'PROCESSING COMPLETE'.
```

## Data Types & Definitions

### Numeric Types
```cobol
      * Display numeric (EBCDIC)
       01  WS-AMOUNT-1         PIC 9(07)V99 VALUE ZERO.
       
      * Signed display numeric
       01  WS-AMOUNT-2         PIC S9(07)V99 VALUE ZERO.
       
      * Binary (COMP)
       01  WS-COUNTER          PIC 9(07) COMP VALUE ZERO.
       01  WS-INDEX            PIC S9(04) COMP VALUE ZERO.
       
      * Packed decimal (COMP-3) - Recommended for calculations
       01  WS-BALANCE          PIC S9(11)V99 COMP-3 VALUE ZERO.
       01  WS-TOTAL            PIC S9(13)V99 COMP-3 VALUE ZERO.
```

### Character Types
```cobol
      * Alphanumeric
       01  WS-NAME             PIC X(50) VALUE SPACES.
       01  WS-ADDRESS          PIC X(100) VALUE SPACES.
       
      * Alphabetic
       01  WS-ALPHA-FIELD      PIC A(30) VALUE SPACES.
       
      * National (Unicode)
       01  WS-UNICODE-NAME     PIC N(50) VALUE SPACES.
```

### Group Items & Structures
```cobol
       01  CUSTOMER-RECORD.
           05  CUST-KEY.
               10  CUST-TYPE           PIC X(02).
               10  CUST-ID             PIC 9(10).
           05  CUST-DATA.
               10  CUST-NAME           PIC X(50).
               10  CUST-ADDRESS.
                   15  ADDR-LINE-1     PIC X(40).
                   15  ADDR-LINE-2     PIC X(40).
                   15  ADDR-CITY       PIC X(30).
                   15  ADDR-STATE      PIC X(02).
                   15  ADDR-ZIP        PIC 9(05).
           05  CUST-FINANCIAL.
               10  CUST-BALANCE        PIC S9(11)V99 COMP-3.
               10  CUST-CREDIT-LIMIT   PIC S9(09)V99 COMP-3.
```

### Arrays (OCCURS)
```cobol
      * Fixed array
       01  MONTHLY-SALES.
           05  MONTH-SALE          PIC S9(09)V99 COMP-3
                                   OCCURS 12 TIMES.
       
      * Variable array
       01  TRANSACTION-TABLE.
           05  TXN-COUNT           PIC 9(04) COMP.
           05  TXN-ENTRY           OCCURS 1 TO 9999 TIMES
                                   DEPENDING ON TXN-COUNT.
               10  TXN-DATE        PIC 9(08).
               10  TXN-AMOUNT      PIC S9(09)V99 COMP-3.
       
      * Indexed array
       01  PRODUCT-TABLE.
           05  PRODUCT-ENTRY       OCCURS 500 TIMES
                                   INDEXED BY PROD-IDX.
               10  PROD-CODE       PIC X(10).
               10  PROD-DESC       PIC X(50).
               10  PROD-PRICE      PIC S9(07)V99 COMP-3.
```

### Condition Names (88 Levels)
```cobol
       01  RECORD-STATUS           PIC X(01).
           88  STATUS-ACTIVE       VALUE 'A'.
           88  STATUS-INACTIVE     VALUE 'I'.
           88  STATUS-PENDING      VALUE 'P'.
           88  STATUS-CLOSED       VALUE 'C'.
           88  VALID-STATUS        VALUES 'A' 'I' 'P' 'C'.
       
       01  TRANSACTION-TYPE        PIC XX.
           88  DEBIT-TXN           VALUE '01' '02' '05'.
           88  CREDIT-TXN          VALUE '10' '11' '15'.
```

### REDEFINES
```cobol
       01  DATE-FIELD.
           05  DATE-NUMERIC        PIC 9(08).
           05  DATE-PARTS REDEFINES DATE-NUMERIC.
               10  DATE-YEAR       PIC 9(04).
               10  DATE-MONTH      PIC 9(02).
               10  DATE-DAY        PIC 9(02).
       
       01  ACCOUNT-KEY.
           05  ACCOUNT-NUMBER      PIC X(10).
           05  ACCOUNT-BREAKDOWN REDEFINES ACCOUNT-NUMBER.
               10  ACCT-BRANCH     PIC X(03).
               10  ACCT-TYPE       PIC X(02).
               10  ACCT-SEQUENCE   PIC X(05).
```

## File Processing

### Sequential File
```cobol
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO INFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-INPUT-STATUS.
       
       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE
           PERFORM READ-RECORD
           PERFORM UNTIL EOF-INPUT
               PERFORM PROCESS-RECORD
               PERFORM READ-RECORD
           END-PERFORM
           CLOSE INPUT-FILE.
       
       READ-RECORD.
           READ INPUT-FILE
               AT END
                   SET EOF-INPUT TO TRUE
           END-READ.
```

### Indexed File (VSAM KSDS)
```cobol
       FILE-CONTROL.
           SELECT MASTER-FILE
               ASSIGN TO MASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MASTER-KEY
               ALTERNATE RECORD KEY IS MASTER-ALT-KEY
                   WITH DUPLICATES
               FILE STATUS IS WS-MASTER-STATUS.
       
       PROCEDURE DIVISION.
      * Random read
           MOVE '1234567890' TO MASTER-KEY
           READ MASTER-FILE
               INVALID KEY
                   DISPLAY 'RECORD NOT FOUND'
           END-READ.
       
      * Sequential read
           OPEN INPUT MASTER-FILE
           PERFORM READ-NEXT
           PERFORM UNTIL EOF-MASTER
               PERFORM PROCESS-RECORD
               PERFORM READ-NEXT
           END-PERFORM.
       
       READ-NEXT.
           READ MASTER-FILE NEXT
               AT END
                   SET EOF-MASTER TO TRUE
           END-READ.
       
      * Update
           MOVE '1234567890' TO MASTER-KEY
           READ MASTER-FILE UPDATE
           IF FILE-SUCCESS
               MOVE 'NEW VALUE' TO MASTER-DATA
               REWRITE MASTER-RECORD
           END-IF.
       
      * Write new record
           MOVE '9999999999' TO MASTER-KEY
           MOVE 'NEW CUSTOMER' TO MASTER-NAME
           WRITE MASTER-RECORD
               INVALID KEY
                   DISPLAY 'DUPLICATE KEY'
           END-WRITE.
       
      * Delete
           MOVE '1234567890' TO MASTER-KEY
           DELETE MASTER-FILE
               INVALID KEY
                   DISPLAY 'RECORD NOT FOUND'
           END-DELETE.
```

## DB2 Integration

### SQL Declaration
```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           
           EXEC SQL
               INCLUDE CUSTOMER
           END-EXEC.
           
           EXEC SQL DECLARE CUSTCUR CURSOR FOR
               SELECT CUST_ID, CUST_NAME, CUST_BALANCE
               FROM CUSTOMER
               WHERE CUST_STATUS = 'A'
               ORDER BY CUST_NAME
           END-EXEC.
       
       01  WS-CUST-ID              PIC 9(10).
       01  WS-CUST-NAME            PIC X(50).
       01  WS-CUST-BALANCE         PIC S9(11)V99 COMP-3.
```

### SQL Operations
```cobol
       PROCEDURE DIVISION.
      * Single row SELECT
           EXEC SQL
               SELECT CUST_NAME, CUST_BALANCE
               INTO :WS-CUST-NAME, :WS-CUST-BALANCE
               FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC
           
           IF SQLCODE = 0
               DISPLAY 'CUSTOMER: ' WS-CUST-NAME
           ELSE
               IF SQLCODE = 100
                   DISPLAY 'CUSTOMER NOT FOUND'
               ELSE
                   DISPLAY 'SQL ERROR: ' SQLCODE
               END-IF
           END-IF.
       
      * Cursor processing
           EXEC SQL
               OPEN CUSTCUR
           END-EXEC
           
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUSTCUR
                   INTO :WS-CUST-ID, :WS-CUST-NAME, :WS-CUST-BALANCE
               END-EXEC
               
               IF SQLCODE = 0
                   PERFORM PROCESS-CUSTOMER
               END-IF
           END-PERFORM
           
           EXEC SQL
               CLOSE CUSTCUR
           END-EXEC.
       
      * INSERT
           EXEC SQL
               INSERT INTO CUSTOMER
                   (CUST_ID, CUST_NAME, CUST_BALANCE)
               VALUES
                   (:WS-CUST-ID, :WS-CUST-NAME, :WS-CUST-BALANCE)
           END-EXEC.
       
      * UPDATE
           EXEC SQL
               UPDATE CUSTOMER
               SET CUST_BALANCE = :WS-CUST-BALANCE
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
       
      * DELETE
           EXEC SQL
               DELETE FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
       
      * COMMIT/ROLLBACK
           IF PROCESSING-OK
               EXEC SQL
                   COMMIT
               END-EXEC
           ELSE
               EXEC SQL
                   ROLLBACK
               END-EXEC
           END-IF.
```

## CICS Programming

### CICS Commands
```cobol
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  CA-FUNCTION         PIC X(08).
           05  CA-CUST-ID          PIC 9(10).
           05  CA-CUST-DATA        PIC X(200).
       
       01  WS-RESP                 PIC S9(08) COMP.
       01  WS-RESP2                PIC S9(08) COMP.
       
       PROCEDURE DIVISION.
      * Receive input
           EXEC CICS RECEIVE
               INTO(WS-INPUT-AREA)
               LENGTH(WS-INPUT-LENGTH)
               RESP(WS-RESP)
           END-EXEC.
       
      * Read from file
           EXEC CICS READ
               FILE('CUSTMAST')
               INTO(CUSTOMER-RECORD)
               RIDFLD(WS-CUST-ID)
               RESP(WS-RESP)
           END-EXEC
           
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM PROCESS-CUSTOMER
               WHEN DFHRESP(NOTFND)
                   DISPLAY 'CUSTOMER NOT FOUND'
               WHEN OTHER
                   PERFORM ERROR-ROUTINE
           END-EVALUATE.
       
      * Write/Update
           EXEC CICS WRITE
               FILE('CUSTMAST')
               FROM(CUSTOMER-RECORD)
               RIDFLD(WS-CUST-ID)
               RESP(WS-RESP)
           END-EXEC.
           
           EXEC CICS REWRITE
               FILE('CUSTMAST')
               FROM(CUSTOMER-RECORD)
               RESP(WS-RESP)
           END-EXEC.
       
      * Link to another program
           EXEC CICS LINK
               PROGRAM('CUSTVAL')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
               RESP(WS-RESP)
           END-EXEC.
       
      * Return to CICS
           EXEC CICS RETURN
           END-EXEC.
       
      * Send output
           EXEC CICS SEND
               FROM(WS-OUTPUT-AREA)
               LENGTH(WS-OUTPUT-LENGTH)
               ERASE
           END-EXEC.
```

## Copybooks

### Data Structure Copybook
```cobol
      *****************************************************************
      * COPYBOOK: CUSTCOPY                                           *
      * PURPOSE:  Customer record structure                          *
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-KEY.
               10  CUST-ID             PIC 9(10).
           05  CUST-PERSONAL.
               10  CUST-FIRST-NAME     PIC X(30).
               10  CUST-LAST-NAME      PIC X(30).
               10  CUST-DOB            PIC 9(08).
           05  CUST-ADDRESS.
               10  CUST-STREET         PIC X(40).
               10  CUST-CITY           PIC X(30).
               10  CUST-STATE          PIC X(02).
               10  CUST-ZIP            PIC 9(05).
           05  CUST-FINANCIAL.
               10  CUST-BALANCE        PIC S9(11)V99 COMP-3.
               10  CUST-CREDIT-LIMIT   PIC S9(09)V99 COMP-3.
           05  CUST-AUDIT.
               10  CUST-CREATE-DATE    PIC 9(08).
               10  CUST-UPDATE-DATE    PIC 9(08).
               10  CUST-UPDATE-USER    PIC X(08).
```

### Using Copybook
```cobol
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTCOPY.
       
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-REC.
       COPY CUSTCOPY.
```

## Subprograms & Calls

### Main Program
```cobol
       WORKING-STORAGE SECTION.
       01  WS-PARM-AREA.
           05  PARM-FUNCTION       PIC X(08).
           05  PARM-INPUT          PIC X(100).
           05  PARM-OUTPUT         PIC X(200).
           05  PARM-RETURN-CODE    PIC 9(04) COMP.
       
       PROCEDURE DIVISION.
           MOVE 'VALIDATE' TO PARM-FUNCTION
           MOVE CUSTOMER-ID TO PARM-INPUT
           
           CALL 'VALIDSUB' USING WS-PARM-AREA
           
           IF PARM-RETURN-CODE = 0
               PERFORM PROCESS-VALID-CUSTOMER
           ELSE
               PERFORM HANDLE-VALIDATION-ERROR
           END-IF.
```

### Subprogram
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDSUB.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-PARM-AREA.
           05  LS-FUNCTION         PIC X(08).
           05  LS-INPUT            PIC X(100).
           05  LS-OUTPUT           PIC X(200).
           05  LS-RETURN-CODE      PIC 9(04) COMP.
       
       PROCEDURE DIVISION USING LS-PARM-AREA.
           MOVE ZERO TO LS-RETURN-CODE
           
           EVALUATE LS-FUNCTION
               WHEN 'VALIDATE'
                   PERFORM VALIDATE-INPUT
               WHEN 'FORMAT  '
                   PERFORM FORMAT-OUTPUT
               WHEN OTHER
                   MOVE 9999 TO LS-RETURN-CODE
           END-EVALUATE
           
           GOBACK.
       
       VALIDATE-INPUT.
           IF LS-INPUT IS NUMERIC
               MOVE 'VALID INPUT' TO LS-OUTPUT
           ELSE
               MOVE 'INVALID INPUT' TO LS-OUTPUT
               MOVE 0001 TO LS-RETURN-CODE
           END-IF.
```

## Date & Time Functions

### Intrinsic Functions
```cobol
       01  WS-CURRENT-DATE-TIME.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR     PIC 9(04).
               10  WS-CURRENT-MONTH    PIC 9(02).
               10  WS-CURRENT-DAY      PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR     PIC 9(02).
               10  WS-CURRENT-MINUTE   PIC 9(02).
               10  WS-CURRENT-SECOND   PIC 9(02).
           05  WS-CURRENT-MS           PIC 9(02).
           05  WS-DIFF-FROM-GMT        PIC S9(04).
       
       PROCEDURE DIVISION.
      * Get current date/time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           DISPLAY 'DATE: ' WS-CURRENT-DATE
           DISPLAY 'TIME: ' WS-CURRENT-TIME.
       
      * Date arithmetic
           COMPUTE WS-INTEGER-DATE =
               FUNCTION INTEGER-OF-DATE(20240115)
           ADD 30 TO WS-INTEGER-DATE
           COMPUTE WS-NEW-DATE =
               FUNCTION DATE-OF-INTEGER(WS-INTEGER-DATE).
       
      * Date validation
           IF FUNCTION TEST-DATE-YYYYMMDD(WS-DATE-FIELD) = 0
               DISPLAY 'VALID DATE'
           ELSE
               DISPLAY 'INVALID DATE'
           END-IF.
```

## Error Handling

### Comprehensive Error Handling
```cobol
       01  WS-ERROR-HANDLING.
           05  WS-ERROR-OCCURRED       PIC X(01) VALUE 'N'.
               88  ERROR-YES           VALUE 'Y'.
               88  ERROR-NO            VALUE 'N'.
           05  WS-ERROR-CODE           PIC 9(04) COMP VALUE ZERO.
           05  WS-ERROR-MESSAGE        PIC X(100) VALUE SPACES.
       
       PROCEDURE DIVISION.
           PERFORM 1000-INITIALIZE
           IF ERROR-NO
               PERFORM 2000-PROCESS
           END-IF
           PERFORM 3000-FINALIZE
           
           IF ERROR-YES
               MOVE WS-ERROR-CODE TO RETURN-CODE
           END-IF
           
           STOP RUN.
       
       1000-INITIALIZE.
           OPEN INPUT MASTER-FILE
           IF NOT FILE-SUCCESS
               MOVE 'Y' TO WS-ERROR-OCCURRED
               MOVE 0100 TO WS-ERROR-CODE
               STRING 'OPEN ERROR: ' DELIMITED BY SIZE
                      WS-FILE-STATUS DELIMITED BY SIZE
                   INTO WS-ERROR-MESSAGE
               PERFORM 9000-LOG-ERROR
           END-IF.
       
       9000-LOG-ERROR.
           DISPLAY 'ERROR CODE: ' WS-ERROR-CODE
           DISPLAY 'ERROR MSG:  ' WS-ERROR-MESSAGE
           
      * Write to error log file
           WRITE ERROR-LOG-RECORD FROM WS-ERROR-HANDLING.
```

## Compilation & Linkage

### Compile JCL
```jcl
//COMPILE  EXEC PGM=IGYCRCTL,
//         PARM='LIST,MAP,XREF,RENT,SSRANGE'
//STEPLIB  DD DSN=IGY.V6R3M0.SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=PROD.COPYLIB,DISP=SHR
//         DD DSN=DB2.DCLGENS,DISP=SHR
//SYSIN    DD DSN=DEV.SOURCE(CUSTINQ),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&LOADSET,
//         DISP=(MOD,PASS),
//         SPACE=(CYL,(1,1)),
//         UNIT=SYSDA
```

### Link-Edit JCL
```jcl
//LKED     EXEC PGM=IEWL,
//         PARM='LIST,MAP,XREF,RENT,REUS'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=DB2.SDSNLOAD,DISP=SHR
//         DD DSN=CICS.SDFHLOAD,DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD DDNAME=SYSIN
//SYSLMOD  DD DSN=PROD.LOAD(CUSTINQ),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ENTRY CUSTINQ
  NAME CUSTINQ(R)
/*
```

## Best Practices

1. **Use COMP-3 for numeric calculations** - Better performance
2. **Avoid ALTER statement** - Use structured programming
3. **Use explicit scope terminators** - END-IF, END-PERFORM, etc.
4. **Initialize variables** - Use VALUE clauses or INITIALIZE
5. **Check FILE STATUS** - After every I/O operation
6. **Use meaningful names** - Self-documenting code
7. **Modularize code** - Small, focused paragraphs
8. **Document complex logic** - Comments for maintainability
9. **Handle errors comprehensively** - Check SQLCODE, FILE-STATUS
10. **Use copybooks** - Reduce redundancy, ensure consistency
