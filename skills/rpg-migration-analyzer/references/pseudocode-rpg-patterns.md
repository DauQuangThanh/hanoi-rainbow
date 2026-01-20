# RPG Translation Patterns

**Prerequisites**: Read [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md) and [pseudocode-rpg-data-structures.md](pseudocode-rpg-data-structures.md).

---

This file contains common RPG translation patterns, gotchas, and critical rules for everyday RPG programming constructs.

## Translation Patterns

### Indicators → Boolean

```rpg
D EOF             S               N   INZ(*OFF)
C                   IF        EOF
C                   EVAL      EOF = *ON
```

→

```pseudocode
eof: BOOLEAN = FALSE
IF eof THEN ...
eof = TRUE
```

### Subroutine → Procedure

```rpg
C     CALC_TOTAL    BEGSR
C                   EVAL      Total = Qty * Price
C                   ENDSR
C                   EXSR      CALC_TOTAL
```

→

```pseudocode
PROCEDURE CalcTotal()
BEGIN
    total = qty * price
END PROCEDURE
CALL CalcTotal()
```

### File Loop with CHAIN

```rpg
C     Key           CHAIN     File
C                   DOW       %FOUND(File)
C                   EXSR      ProcessRec
C     Key           CHAIN     File
C                   ENDDO
```

→

```pseudocode
record = READ_BY_KEY(file, key)
WHILE RECORD_FOUND(file) DO
    CALL ProcessRec(record)
    record = READ_BY_KEY(file, key)
END WHILE
```

### Sequential Read Loop

```rpg
C                   READ      CustMast
C                   DOW       NOT %EOF(CustMast)
C                   EXSR      ProcessCustomer
C                   READ      CustMast
C                   ENDDO
```

→

```pseudocode
record = READ_RECORD(custMast)
WHILE NOT END_OF_FILE(custMast) DO
    CALL ProcessCustomer(record)
    record = READ_RECORD(custMast)
END WHILE
```

### SETLL/READE Pattern (Key Processing)

```rpg
C     Key           SETLL     File
C     Key           READE     File
C                   DOW       NOT %EOF(File)
C                   EXSR      Process
C     Key           READE     File
C                   ENDDO
```

→

```pseudocode
POSITION_LOWER_LIMIT(file, key)
record = READ_EQUAL_KEY(file, key)
WHILE NOT END_OF_FILE(file) DO
    CALL Process(record)
    record = READ_EQUAL_KEY(file, key)
END WHILE
```

### Update Record Pattern

```rpg
C     CustNo        CHAIN     CustMast
C                   IF        %FOUND(CustMast)
C                   EVAL      Balance = Balance + Amount
C                   UPDATE    CustRec
C                   ENDIF
```

→

```pseudocode
record = READ_BY_KEY(custMast, custNo)
IF RECORD_FOUND(custMast) THEN
    record.balance = record.balance + amount
    UPDATE_RECORD(custMast, record)
END IF
```

### Write New Record Pattern

```rpg
C                   CLEAR     CustRec
C                   EVAL      CustNo = NewCustNo
C                   EVAL      Name = NewName
C                   WRITE     CustRec
```

→

```pseudocode
record = NEW CustomerRecord
record.custNo = newCustNo
record.name = newName
WRITE_RECORD(custMast, record)
```

### Modern Error Handling (MONITOR)

```rpg
C                   MONITOR
C     Key           CHAIN     File
C                   EVAL      Result = Amt1 / Amt2
C                   ON-ERROR  1211:1299
C                   EVAL      ErrMsg = 'File error occurred'
C                   ON-ERROR  *ALL
C                   EVAL      ErrMsg = 'Unknown error'
C                   ENDMON
```

→

```pseudocode
TRY:
    record = READ_BY_KEY(file, key)
    result = amt1 / amt2
CATCH FileException:
    errMsg = "File error occurred"
CATCH Exception:
    errMsg = "Unknown error"
END TRY
```

### Legacy Error Handling (%ERROR)

```rpg
C     Key           CHAIN(E)  File
C                   IF        %ERROR
C                   EVAL      ErrMsg = 'CHAIN failed'
C                   ENDIF
```

→

```pseudocode
TRY:
    record = READ_BY_KEY(file, key)
CATCH Exception:
    errMsg = "CHAIN failed"
END TRY
```

### Procedure with Parameters

```rpg
D CalcTax         PR            15P 2
D   Amount                      15P 2 CONST
D   TaxRate                      5P 3 CONST

P CalcTax         B
D CalcTax         PI            15P 2
D   Amount                      15P 2 CONST
D   TaxRate                      5P 3 CONST

C                   RETURN    Amount * TaxRate
P CalcTax         E
```

→

```pseudocode
FUNCTION CalcTax(amount: DECIMAL(15,2), taxRate: DECIMAL(5,3)) RETURNS DECIMAL(15,2)
BEGIN
    RETURN amount * taxRate
END FUNCTION
```

### String Manipulation

```rpg
C                   EVAL      FullName = %TRIM(FirstName) + ' ' +
C                                        %TRIM(LastName)
C                   EVAL      Pos = %SCAN('&':Message)
C                   IF        Pos > 0
C                   EVAL      Message = %REPLACE(Value:Message:Pos:1)
C                   ENDIF
```

→

```pseudocode
fullName = TRIM(firstName) + " " + TRIM(lastName)
pos = FIND(message, "&")
IF pos > 0 THEN
    message = REPLACE(message, value, pos, 1)
END IF
```

### Date Arithmetic

```rpg
C                   EVAL      Today = %DATE()
C                   EVAL      DueDate = Today + %DAYS(30)
C                   EVAL      DaysLate = %DIFF(Today:InvDate:*DAYS)
C                   IF        DaysLate > 30
C                   EVAL      Status = 'OVERDUE'
C                   ENDIF
```

→

```pseudocode
today = CURRENT_DATE()
dueDate = today + DURATION_DAYS(30)
daysLate = DATE_DIFFERENCE(today, invDate, DAYS)
IF daysLate > 30 THEN
    status = "OVERDUE"
END IF
```

### Numeric Formatting

```rpg
C                   EVAL      Display = %EDITC(Amount:'J')
C                   EVAL      Formatted = %EDITW(SSN:'0  -  -    ')
```

→

```pseudocode
display = FORMAT_NUMERIC(amount, COMMA_WITH_DECIMALS)
formatted = FORMAT_WITH_PATTERN(ssn, "0  -  -    ")
```

### SELECT/WHEN Pattern

```rpg
C                   SELECT
C                   WHEN      Status = 'A'
C                   EVAL      Desc = 'Active'
C                   WHEN      Status = 'I'
C                   EVAL      Desc = 'Inactive'
C                   OTHER
C                   EVAL      Desc = 'Unknown'
C                   ENDSL
```

→

```pseudocode
SWITCH status:
    CASE "A":
        desc = "Active"
        BREAK
    CASE "I":
        desc = "Inactive"
        BREAK
    DEFAULT:
        desc = "Unknown"
END SWITCH
```

### Subfile Operations (Interactive Programs)

```rpg
// Clear and load subfile
C                   EVAL      *IN31 = *OFF
C                   WRITE     SflCtl
C                   EVAL      *IN31 = *ON

C                   EVAL      RRN = 0
C                   READ      DataFile
C                   DOW       NOT %EOF(DataFile)
C                   EVAL      RRN = RRN + 1
C                   EVAL      SflCustNo = CustNo
C                   EVAL      SflName = Name
C                   WRITE     SflRec
C                   READ      DataFile
C                   ENDDO

C                   EVAL      *IN32 = *ON
C                   EXFMT     SflCtl
```

→

```pseudocode
// Clear subfile
subfileClear = TRUE
WRITE_SCREEN_FORMAT(screen, "SflCtl")
subfileClear = FALSE
subfileDisplay = TRUE

// Load subfile records
rrn = 0
record = READ_RECORD(dataFile)
WHILE NOT END_OF_FILE(dataFile) DO
    rrn = rrn + 1
    subfileRecord.custNo = record.custNo
    subfileRecord.name = record.name
    subfileRecord.rrn = rrn
    WRITE_SUBFILE_RECORD(screen, "SflRec", subfileRecord)
    record = READ_RECORD(dataFile)
END WHILE

// Display subfile and wait for input
subfileDisplayControl = TRUE
DISPLAY_AND_READ(screen, "SflCtl")
```

### Subfile Processing User Selections

```rpg
C                   EVAL      RRN = 0
C                   READC     SflRec
C                   DOW       NOT %EOF(ScreenDsp)
C                   IF        SflSelect = 'X'
C                   EXSR      ProcessSelection
C                   ENDIF
C                   READC     SflRec
C                   ENDDO
```

→

```pseudocode
rrn = 0
changedRecord = READ_CHANGED_SUBFILE_RECORD(screen, "SflRec")
WHILE RECORD_FOUND(screen) DO
    IF changedRecord.select = "X" THEN
        CALL ProcessSelection(changedRecord)
    END IF
    changedRecord = READ_CHANGED_SUBFILE_RECORD(screen, "SflRec")
END WHILE
```

### RPG Cycle Handling (Legacy)

```rpg
// Primary file drives RPG cycle
F DataFile   IP   E             DISK

C                   READ      DataFile
C                   DOW       NOT %EOF(DataFile)
C     *LOVAL        SETLL     DetailFile
C     Key           READE     DetailFile
C                   DOW       NOT %EOF(DetailFile)
C                   EXSR      ProcessDetail
C     Key           READE     DetailFile
C                   ENDDO
C                   READ      DataFile
C                   ENDDO
```

→

```pseudocode
// Convert RPG cycle to explicit loop
PROCEDURE MainProcess()
BEGIN
    dataRecord = READ_RECORD(dataFile)
    WHILE NOT END_OF_FILE(dataFile) DO
        // Level break logic would go here if present
        CALL ProcessMasterRecord(dataRecord)
        
        // Process matching detail records
        POSITION_LOWER_LIMIT(detailFile, MIN_VALUE)
        detailRecord = READ_EQUAL_KEY(detailFile, dataRecord.key)
        WHILE NOT END_OF_FILE(detailFile) DO
            CALL ProcessDetail(detailRecord)
            detailRecord = READ_EQUAL_KEY(detailFile, dataRecord.key)
        END WHILE
        
        dataRecord = READ_RECORD(dataFile)
    END WHILE
END PROCEDURE
```

### Array Initialization and Processing

```rpg
D Months          S              3    DIM(12) CTDATA PERRCD(4)
D MonthIdx        S              3  0

C                   FOR       MonthIdx = 1 TO 12
C                   EVAL      Display = Months(MonthIdx)
C                   ENDFOR

**CTDATA Months
JanFebMarApr
MayJunJulAug
SepOctNovDec
```

→

```pseudocode
// Compile-time data array
months: ARRAY[12] OF STRING[3] = [
    "Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
]

FOR monthIdx FROM 1 TO 12 DO
    display = months[monthIdx]
END FOR
```

### Dynamic Array Sorting

```rpg
C                   SORTA     %SUBARR(Array:1:Count)
```

→

```pseudocode
SORT_ARRAY(array, 1, count, ASCENDING)
```

### Program Initialization and Termination

```rpg
C     *INZSR        BEGSR
C                   EVAL      StartTime = %TIME()
C                   EXSR      LoadConfig
C                   ENDSR

C     *INLR         IFEQ      *ON
C                   EVAL      EndTime = %TIME()
C                   EXSR      Cleanup
C                   ENDIF
```

→

```pseudocode
PROCEDURE Initialize()
BEGIN
    startTime = CURRENT_TIME()
    CALL LoadConfig()
END PROCEDURE

PROCEDURE Terminate()
BEGIN
    endTime = CURRENT_TIME()
    CALL Cleanup()
    lastRecord = TRUE  // *INLR equivalent
END PROCEDURE

// Main program flow
BEGIN PROGRAM
    CALL Initialize()
    CALL MainProcessing()
    CALL Terminate()
END PROGRAM
```

### Message Handling

```rpg
D Msg             S             52
D MsgKey          S              4

C                   CALL      'QMHSNDPM'
C                   PARM                    MsgId
C                   PARM                    MsgFile
C                   PARM                    MsgData
C                   PARM                    MsgLen
C                   PARM      '*INFO'       MsgType
C                   PARM      '*'          CallStk
C                   PARM      0             StkCntr
C                   PARM                    MsgKey
```

→

```pseudocode
PROCEDURE SendProgramMessage(
    msgId: STRING,
    msgFile: STRING,
    msgData: STRING,
    msgType: STRING
) RETURNS STRING
BEGIN
    msgKey: STRING[4]
    // Call system API to send program message
    CALL_SYSTEM_API("QMHSNDPM", [
        msgId, msgFile, msgData, LENGTH(msgData),
        msgType, "*", 0, msgKey
    ])
    RETURN msgKey
END PROCEDURE
```

### Data Queue Operations

```rpg
D DtaQ            S             10    INZ('MYDTAQ')
D DtaQLib         S             10    INZ('MYLIB')
D DtaQData        S            100
D DtaQLen         S              5  0

C                   CALL      'QSNDDTAQ'
C                   PARM                    DtaQ
C                   PARM                    DtaQLib
C                   PARM      100           DtaQLen
C                   PARM                    DtaQData
```

→

```pseudocode
STRUCTURE DataQueue:
    name: STRING[10]
    library: STRING[10]
END STRUCTURE

PROCEDURE SendToDataQueue(
    queue: DataQueue,
    data: STRING,
    length: INTEGER
)
BEGIN
    CALL_SYSTEM_API("QSNDDTAQ", [
        queue.name, queue.library, length, data
    ])
END PROCEDURE

PROCEDURE ReceiveFromDataQueue(
    queue: DataQueue,
    waitTime: INTEGER
) RETURNS STRING
BEGIN
    data: STRING[100]
    dataLength: INTEGER
    CALL_SYSTEM_API("QRCVDTAQ", [
        queue.name, queue.library, dataLength, data, waitTime
    ])
    RETURN SUBSTRING(data, 1, dataLength)
END PROCEDURE
```

## Common Pitfalls to Avoid

1. **Losing precision**: Always check packed decimal field sizes (nP m)
2. **Ignoring indicators**: Named booleans must have meaningful names
3. **MOVE/MOVEL**: Remember these are position-based, not simple assignments
4. **Array indexing**: RPG uses 1-based indexing, adjust for target language
5. **%FOUND vs %EOF**: Use correct check after different file operations
6. **Date formats**: RPG date formats vary (*ISO, *USA, *EUR, *JIS, *MDY, etc.)
7. **String position**: RPG %SUBST uses 1-based positions
8. **Half-adjust**: Don't forget (H) extender implies ROUND with HALF_UP
9. **File scope**: RPG files are global; modern code may need different scope
10. **Indicator arrays**: *IN(01) array syntax vs individual *IN01
11. **EVAL optional**: Free format may omit EVAL but it's an assignment
12. **Procedure naming**: BEGSR names are local; P-spec procs may be exported
13. **Data structure arrays**: Multiple occurrence DS != modern arrays
14. **LR indicator**: Setting *INLR=*ON closes files and frees resources
15. **Factor 1 and 2**: Some ops use Factor 1 as control (LOKUP, SCAN old style)
16. **Subfile operations**: Preserve subfile load/display/read patterns
17. **READC operation**: Convert to "read changed records" pattern
18. **RPG cycle**: Convert to explicit loops with clear control flow
19. **CTDATA arrays**: Extract to initialization code or configuration
20. **F-spec prefixes**: Apply field name prefixes consistently
21. **USROPN files**: Ensure explicit OPEN/CLOSE operations
22. **Overflow indicators**: Convert to page management logic
23. **System APIs**: Document external dependencies clearly

## Critical Rules

1. **Indicators**: Convert *IN01-*IN99 to named booleans with descriptive names
2. **Packed decimal (P)**: MUST preserve precision using DECIMAL(n,m)
3. **%ERROR**: Convert to TRY-CATCH blocks
4. **%FOUND**: Check after CHAIN/SETLL/READE operations
5. **Half-adjust (H)**: Use `ROUND(expr, decimals)` with HALF_UP rounding
6. **MONITOR/ON-ERROR**: Convert to modern TRY-CATCH-FINALLY blocks
7. **Multiple occurrence DS**: Convert to arrays with explicit indexing
8. **OVERLAY**: Document shared memory with clear comments
9. **Data areas**: Convert to persistent storage or configuration
10. **Commitment control**: Preserve transaction boundaries
11. **Pointers**: Use safe abstractions where possible
12. **Special values**: Convert to language-appropriate equivalents

---

**Next**: See [pseudocode-rpg-advanced.md](pseudocode-rpg-advanced.md) for advanced RPG features.
