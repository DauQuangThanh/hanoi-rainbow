]633;E;cat /tmp/rpg-migration.txt >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "---" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md && echo "**Reference**: IBM RPG IV Reference, ILE RPG Programmer's Guide" >> /Users/thanhdq.coe/99-hanoi-rainbow/hanoi-rainbow/skills/rpg-migration-analyzer/references/pseudocode-rpg-migration-guide.md;a450241c-8948-4114-8aa1-a76bbe99bad9]633;C# RPG Advanced Features

**Prerequisites**: Read [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md) and [pseudocode-rpg-patterns.md](pseudocode-rpg-patterns.md).

---

This file covers advanced RPG features including ILE RPG, embedded SQL, web services, and modern RPG capabilities.

## Advanced Features

### File Information Data Structure (INFDS)

```rpg
F CustMast   IF   E           K DISK    INFDS(FileInfo)
D FileInfo          DS
D   FileName          *FILE
D   FileStatus        *STATUS
D   OpCode            *OPCODE
D   Routine           *ROUTINE
D   NumRecs           *RECORD
```

→

```pseudocode
STRUCTURE FileInformationDS:
    fileName: STRING[10]
    fileStatus: INTEGER
    opCode: STRING[6]
    routine: STRING[8]
    numRecs: INTEGER
END STRUCTURE

fileInfo: FileInformationDS
// Access: fileInfo.fileStatus after file operations
```

### Program Status Data Structure (PSDS)

```rpg
D PSDS           SDS
D   PgmName         *PROC
D   PgmStatus       *STATUS
D   PrevStatus           16     20S 0
D   LineNum              21     28
D   Routine              29     36
D   UserName             254    263
```

→

```pseudocode
STRUCTURE ProgramStatusDS:
    pgmName: STRING[10]
    pgmStatus: INTEGER
    prevStatus: DECIMAL(5,0)
    lineNum: STRING[8]
    routine: STRING[8]
    userName: STRING[10]
END STRUCTURE

psds: ProgramStatusDS
// Access: psds.pgmStatus for error handling
```

### Data Area Operations

```rpg
D Counter         S              5P 0 DTAARA(MYCOUNTER)

C     *DTAARA       DEFINE    Counter
C                   IN        Counter
C                   EVAL      Counter = Counter + 1
C                   OUT       Counter
```

→

```pseudocode
counter: DECIMAL(5,0)

// Read from persistent storage
counter = READ_DATA_AREA("MYCOUNTER")
counter = counter + 1
// Write back to persistent storage
WRITE_DATA_AREA("MYCOUNTER", counter)
```

### Commitment Control

```rpg
C                   COMMIT
C                   IF        %ERROR
C                   EVAL      ErrMsg = 'Commit failed'
C                   ENDIF

C                   ROLBK
```

→

```pseudocode
TRY:
    COMMIT_TRANSACTION()
CATCH Exception:
    errMsg = "Commit failed"
    ROLLBACK_TRANSACTION()
END TRY
```

### API Call Pattern

```rpg
D QCmdExc         PR                  EXTPGM('QCMDEXC')
D   Command                   3000A   CONST OPTIONS(*VARSIZE)
D   Length                      15P 5 CONST

D Cmd             S           3000A
D CmdLen          S             15P 5

C                   EVAL      Cmd = 'DLTF FILE(MYLIB/MYFILE)'
C                   EVAL      CmdLen = %LEN(%TRIM(Cmd))
C                   CALLB     QCmdExc(Cmd:CmdLen)
```

→

```pseudocode
// Define API interface
FUNCTION QCmdExc(command: STRING, length: DECIMAL(15,5))
BEGIN
    // External system API call
END FUNCTION

cmd: STRING[3000]
cmdLen: DECIMAL(15,5)

cmd = "DLTF FILE(MYLIB/MYFILE)"
cmdLen = LENGTH(TRIM(cmd))
CALL QCmdExc(cmd, cmdLen)
```

### Service Programs and Binding

```rpg
H NOMAIN  // Service program indicator

/COPY QRPGLESRC,PROTOTYPES

D GetCustomer     PR                  EXTPROC('GetCustomer')
D                                     LIKEDS(Customer)
D   CustNo                      10P 0 CONST

P GetCustomer     B                   EXPORT
D GetCustomer     PI                  LIKEDS(Customer)
D   CustNo                      10P 0 CONST
  // Implementation
P GetCustomer     E
```

→

```pseudocode
// Service program module (no main entry point)
// IMPORT: PROTOTYPES module

FUNCTION GetCustomer(custNo: DECIMAL(10,0)) RETURNS Customer EXPORTED
BEGIN
    customer: Customer
    // Implementation to retrieve customer
    RETURN customer
END FUNCTION

// Notes:
// - NOMAIN indicates service program (library of procedures)
// - EXPORT makes procedure available to external programs
// - /COPY includes common definitions
// - Binding directory references needed for linking
```

### Binding Directory (H-Spec)

```rpg
H BNDDIR('MYLIB/MYBNDDIR')
H ACTGRP(*NEW)
```

→

```pseudocode
// Program Configuration
CONSTANTS:
    BINDING_DIRECTORY = "MYLIB/MYBNDDIR"  // External references
    ACTIVATION_GROUP = "NEW"               // Isolated execution
END CONSTANTS

// Notes:
// - BNDDIR specifies external service programs to link
// - ACTGRP controls resource isolation and cleanup
// - *NEW = new activation group (recommended for modern programs)
// - *CALLER = use caller's activation group
```

### Parameter Passing Options

```rpg
D ProcessRecord   PR
D   Record                            LIKEDS(Customer) CONST
D   Options                           OPTIONS(*NOPASS:*OMIT)
D   ErrCode                           LIKEDS(ApiError) OPTIONS(*NOPASS)

P ProcessRecord   B
D ProcessRecord   PI
D   Record                            LIKEDS(Customer) CONST
D   Options                           OPTIONS(*NOPASS:*OMIT)
D   ErrCode                           LIKEDS(ApiError) OPTIONS(*NOPASS)

C                   IF        %PARMS >= 2 AND %ADDR(Options) <> *NULL
C                   // Use optional parameter
C                   ENDIF
P ProcessRecord   E
```

→

```pseudocode
FUNCTION ProcessRecord(
    record: Customer,                    // CONST - pass by value
    options: OPTIONAL NULLABLE STRING,    // *NOPASS:*OMIT
    errCode: OPTIONAL ApiError           // *NOPASS
)
BEGIN
    // Check if optional parameters provided
    IF PARAMETER_COUNT() >= 2 AND options IS NOT NULL THEN
        // Use optional parameter
    END IF
END FUNCTION

// Parameter Options:
// - CONST: Pass by value (read-only)
// - VALUE: Pass by value (copy)
// - *NOPASS: Parameter is optional
// - *OMIT: Parameter can be passed as *OMIT (null)
// - *STRING: Null-terminated string
// - *VARSIZE: Variable-length parameter
// - OPTIONS(*TRIM): Trim trailing blanks
```

### Data Structure Parameter Passing

```rpg
D ProcessDS       PR
D   InDS                              LIKEDS(InputDS) CONST
D   OutDS                             LIKEDS(OutputDS)

C                   EVAL      OutDS = InDS  // Structure assignment
C                   CALLP     ProcessDS(MyInput:MyOutput)
```

→

```pseudocode
FUNCTION ProcessDS(inDS: InputDS, outDS: OUTPUT OutputDS)
BEGIN
    outDS = inDS  // Copy all fields from input to output
END FUNCTION

// Call with structures
CALL ProcessDS(myInput, myOutput)

// Note: Structure assignment copies all fields
```

### Return Value vs. Output Parameters

```rpg
// Return value style (modern)
D CalcTotal       PR            15P 2
D   Quantity                     7P 0 CONST
D   Price                       11P 2 CONST

C                   EVAL      Total = CalcTotal(Qty:Price)

// Output parameter style (legacy)
D CalcTotal2      PR
D   Total                       15P 2
D   Quantity                     7P 0 CONST
D   Price                       11P 2 CONST

C                   CALLP     CalcTotal2(Total:Qty:Price)
```

→

```pseudocode
// Modern style - return value
FUNCTION CalcTotal(quantity: DECIMAL(7,0), price: DECIMAL(11,2)) RETURNS DECIMAL(15,2)
BEGIN
    RETURN quantity * price
END FUNCTION

total = CalcTotal(qty, price)

// Legacy style - output parameter
PROCEDURE CalcTotal2(total: OUTPUT DECIMAL(15,2), quantity: DECIMAL(7,0), price: DECIMAL(11,2))
BEGIN
    total = quantity * price
END PROCEDURE

CALL CalcTotal2(total, qty, price)
```

### Performance Optimization Patterns

```rpg
// Pre-allocate structures in loops (avoid in tight loops)
C                   DOW       NOT %EOF(File)
C                   CLEAR     TempDS  // Expensive if repeated
C                   READ      File
C                   ENDDO

// Better approach - reuse
C                   CLEAR     TempDS
C                   DOW       NOT %EOF(File)
C                   // Reuse TempDS, clear only needed fields
C                   READ      File
C                   ENDDO
```

→

```pseudocode
// AVOID: Clearing structure in every iteration
WHILE NOT END_OF_FILE(file) DO
    tempDS = NEW TempStructure()  // Expensive
    record = READ_RECORD(file)
END WHILE

// BETTER: Reuse structure, clear only when needed
tempDS = NEW TempStructure()
WHILE NOT END_OF_FILE(file) DO
    // Reuse tempDS, update only changed fields
    record = READ_RECORD(file)
END WHILE

// Performance notes:
// - Minimize object allocation in loops
// - Reuse data structures when possible
// - Use *NOPASS parameters to avoid unnecessary copying
// - Avoid string concatenation in tight loops
```

### Activation Group Management

```rpg
H ACTGRP(*NEW)     // New activation group
H ACTGRP(*CALLER)  // Caller's activation group
H ACTGRP('MYGRP')  // Named activation group

C                   EVAL      *INLR = *ON  // End program, reclaim resources
```

→

```pseudocode
// Program Configuration
CONSTANTS:
    ACTIVATION_GROUP_NEW = TRUE
    // or ACTIVATION_GROUP_NAME = "MYGRP"
END CONSTANTS

PROCEDURE Terminate()
BEGIN
    lastRecord = TRUE  // *INLR = *ON
    // Automatic cleanup:
    // - Close all files
    // - Deallocate memory
    // - Free resources
    // - Reclaim activation group if *NEW
END PROCEDURE

// Activation Group Notes:
// - *NEW: Isolated, automatic cleanup, recommended for batch
// - *CALLER: Share resources with caller, use for service programs
// - Named: Shared resources across multiple programs
// - *INLR=*ON: Triggers full cleanup and resource reclamation
```

## Embedded SQL Operations

### Basic SQL Select

```rpg
C/EXEC SQL
C+  SELECT CUSTNO, NAME, BALANCE
C+    INTO :CustNo, :Name, :Balance
C+    FROM CUSTOMER
C+   WHERE CUSTNO = :SearchCustNo
C/END-EXEC

C                   IF        SQLCOD = 0
C                   // Record found
C                   ENDIF
```

→

```pseudocode
TRY:
    EXECUTE SQL
        SELECT CUSTNO, NAME, BALANCE
        INTO :custNo, :name, :balance
        FROM CUSTOMER
        WHERE CUSTNO = :searchCustNo
    END SQL
    
    IF SQL_CODE = 0 THEN
        // Record found
    END IF
CATCH SQLException:
    // Handle SQL error
END TRY
```

### SQL Cursor Processing

```rpg
C/EXEC SQL
C+  DECLARE C1 CURSOR FOR
C+    SELECT CUSTNO, NAME, BALANCE
C+      FROM CUSTOMER
C+     WHERE STATE = :StateCode
C+     ORDER BY NAME
C/END-EXEC

C/EXEC SQL OPEN C1 END-EXEC

C                   DOU       SQLCOD <> 0
C/EXEC SQL
C+  FETCH C1 INTO :CustNo, :Name, :Balance
C/END-EXEC
C                   IF        SQLCOD = 0
C                   EXSR      ProcessCustomer
C                   ENDIF
C                   ENDDO

C/EXEC SQL CLOSE C1 END-EXEC
```

→

```pseudocode
// Declare cursor
CURSOR c1 FOR
    SELECT CUSTNO, NAME, BALANCE
    FROM CUSTOMER
    WHERE STATE = :stateCode
    ORDER BY NAME
END CURSOR

OPEN_CURSOR(c1)

DO
    FETCH c1 INTO custNo, name, balance
    IF SQL_CODE = 0 THEN
        CALL ProcessCustomer(custNo, name, balance)
    END IF
WHILE SQL_CODE = 0

CLOSE_CURSOR(c1)
```

### SQL Insert/Update/Delete

```rpg
C/EXEC SQL
C+  INSERT INTO CUSTOMER
C+    (CUSTNO, NAME, BALANCE)
C+  VALUES (:CustNo, :Name, :Balance)
C/END-EXEC

C/EXEC SQL
C+  UPDATE CUSTOMER
C+     SET BALANCE = BALANCE + :Amount
C+   WHERE CUSTNO = :CustNo
C/END-EXEC

C/EXEC SQL
C+  DELETE FROM CUSTOMER
C+   WHERE CUSTNO = :CustNo
C/END-EXEC

C                   EVAL      RowsAffected = SQLERRD(3)
```

→

```pseudocode
// Insert
EXECUTE SQL
    INSERT INTO CUSTOMER (CUSTNO, NAME, BALANCE)
    VALUES (:custNo, :name, :balance)
END SQL

// Update
EXECUTE SQL
    UPDATE CUSTOMER
    SET BALANCE = BALANCE + :amount
    WHERE CUSTNO = :custNo
END SQL

// Delete
EXECUTE SQL
    DELETE FROM CUSTOMER
    WHERE CUSTNO = :custNo
END SQL

rowsAffected = SQL_ROWS_AFFECTED()
```

### Dynamic SQL

```rpg
D SqlStmt         S            512A
D CustNo          S              9P 0

C                   EVAL      SqlStmt = 'SELECT CUSTNO ' +
C                                       'FROM CUSTOMER ' +
C                                       'WHERE STATE = ?'

C/EXEC SQL
C+  PREPARE S1 FROM :SqlStmt
C/END-EXEC

C/EXEC SQL
C+  EXECUTE S1 USING :StateCode INTO :CustNo
C/END-EXEC
```

→

```pseudocode
sqlStmt: STRING[512]
custNo: DECIMAL(9,0)

sqlStmt = "SELECT CUSTNO " +
          "FROM CUSTOMER " +
          "WHERE STATE = ?"

PREPARE_SQL_STATEMENT("S1", sqlStmt)
EXECUTE_PREPARED("S1", [stateCode], custNo)
```

### SQL Error Handling

```rpg
D SQLSTT          S              5A
D SQLCOD          S             10I 0

C/EXEC SQL
C+  WHENEVER SQLERROR CONTINUE
C/END-EXEC

C/EXEC SQL
C+  SELECT NAME INTO :Name
C+    FROM CUSTOMER
C+   WHERE CUSTNO = :CustNo
C/END-EXEC

C                   SELECT
C                   WHEN      SQLCOD = 0
C                   EVAL      Msg = 'Success'
C                   WHEN      SQLCOD = 100
C                   EVAL      Msg = 'Not found'
C                   OTHER
C                   EVAL      Msg = 'SQL Error: ' + SQLSTT
C                   ENDSL
```

→

```pseudocode
sqlState: STRING[5]
sqlCode: INTEGER

TRY:
    EXECUTE SQL
        SELECT NAME INTO :name
        FROM CUSTOMER
        WHERE CUSTNO = :custNo
    END SQL
    
    SWITCH sqlCode:
        CASE 0:
            msg = "Success"
            BREAK
        CASE 100:
            msg = "Not found"
            BREAK
        DEFAULT:
            msg = "SQL Error: " + sqlState
    END SWITCH
CATCH SQLException:
    msg = "SQL Error: " + GET_SQL_STATE()
END TRY

// SQL Status Codes:
// 0 = Success
// 100 = No data found
// negative = Error occurred
// SQLSTT/SQLSTATE = 5-character error code
```

### Stored Procedure Calls

```rpg
C/EXEC SQL
C+  CALL MYPROC(:InParm1, :InParm2, :OutParm)
C/END-EXEC
```

→

```pseudocode
EXECUTE SQL
    CALL MYPROC(:inParm1, :inParm2, :outParm)
END SQL
```

### SQL Transaction Control

```rpg
C/EXEC SQL
C+  SET TRANSACTION ISOLATION LEVEL READ COMMITTED
C/END-EXEC

C/EXEC SQL COMMIT END-EXEC

C/EXEC SQL ROLLBACK END-EXEC
```

→

```pseudocode
// Set isolation level
EXECUTE SQL
    SET TRANSACTION ISOLATION LEVEL READ COMMITTED
END SQL

// Commit transaction
COMMIT_TRANSACTION()

// Rollback transaction
ROLLBACK_TRANSACTION()
```

## IFS (Integrated File System) Operations

### IFS File Operations

```rpg
D FD              S             10I 0
D Buffer          S           1024A
D BytesRead       S             10I 0

C                   EVAL      FD = open('/home/myfile.txt':
C                                        O_RDONLY)
C                   IF        FD >= 0
C                   EVAL      BytesRead = read(FD:Buffer:%SIZE(Buffer))
C                   CALLP     close(FD)
C                   ENDIF
```

→

```pseudocode
fileDescriptor: INTEGER
buffer: STRING[1024]
bytesRead: INTEGER

fileDescriptor = IFS_OPEN("/home/myfile.txt", READ_ONLY)
IF fileDescriptor >= 0 THEN
    bytesRead = IFS_READ(fileDescriptor, buffer, SIZE_OF(buffer))
    IFS_CLOSE(fileDescriptor)
END IF

// IFS File Modes:
// O_RDONLY - Read only
// O_WRONLY - Write only
// O_RDWR - Read and write
// O_CREAT - Create if doesn't exist
// O_TRUNC - Truncate to zero length
// O_APPEND - Append to end
```

### IFS Directory Operations

```rpg
D Dir             S               *
D Entry           DS                  LIKEDS(Dirent)

C                   EVAL      Dir = opendir('/home/mydir')
C                   IF        Dir <> *NULL
C                   DOW       readdir(Dir:Entry) <> *NULL
C                   // Process Entry.d_name
C                   ENDDO
C                   CALLP     closedir(Dir)
C                   ENDIF
```

→

```pseudocode
STRUCTURE DirectoryEntry:
    name: STRING[256]
    type: STRING[10]
END STRUCTURE

directory: POINTER
entry: DirectoryEntry

directory = IFS_OPEN_DIR("/home/mydir")
IF directory IS NOT NULL THEN
    WHILE IFS_READ_DIR(directory, entry) IS NOT NULL DO
        // Process entry.name
    END WHILE
    IFS_CLOSE_DIR(directory)
END IF
```

### IFS File Information

```rpg
D StatDS          DS                  QUALIFIED
D   FileSize                    10I 0
D   ModTime                     10I 0
D   FileType                     5I 0

C                   IF        stat('/home/myfile.txt':StatDS) = 0
C                   // File exists, check StatDS fields
C                   ENDIF
```

→

```pseudocode
STRUCTURE FileStats:
    fileSize: INTEGER
    modTime: INTEGER
    fileType: INTEGER
    permissions: INTEGER
END STRUCTURE

stats: FileStats

IF IFS_STAT("/home/myfile.txt", stats) = 0 THEN
    // File exists, access stats.fileSize, etc.
END IF
```

## XML and JSON Operations

### XML Parsing (xml-into)

```rpg
D Customer        DS                  QUALIFIED
D   Name                        30A
D   City                        20A
D   Balance                     15P 2

D XmlDoc          S           1000A

C                   EVAL      XmlDoc = '<customer>' +
C                                      '<name>John Smith</name>' +
C                                      '<city>Chicago</city>' +
C                                      '<balance>1500.00</balance>' +
C                                      '</customer>'

C                   XML-INTO  Customer %XML(XmlDoc)
```

→

```pseudocode
STRUCTURE Customer:
    name: STRING[30]
    city: STRING[20]
    balance: DECIMAL(15,2)
END STRUCTURE

xmlDoc: STRING[1000]
customer: Customer

xmlDoc = "<customer>" +
         "<name>John Smith</name>" +
         "<city>Chicago</city>" +
         "<balance>1500.00</balance>" +
         "</customer>"

customer = PARSE_XML(xmlDoc, Customer)
```

### JSON Parsing (DATA-INTO)

```rpg
D Order           DS                  QUALIFIED
D   OrderNo                      9P 0
D   CustName                    30A
D   Total                       15P 2

D JsonDoc         S           1000A

C                   EVAL      JsonDoc = '{"orderNo":12345,' +
C                                       '"custName":"John Smith",' +
C                                       '"total":1500.00}'

C                   DATA-INTO Order %DATA(JsonDoc:'doc=string')
```

→

```pseudocode
STRUCTURE Order:
    orderNo: DECIMAL(9,0)
    custName: STRING[30]
    total: DECIMAL(15,2)
END STRUCTURE

jsonDoc: STRING[1000]
order: Order

jsonDoc = '{"orderNo":12345,' +
          '"custName":"John Smith",' +
          '"total":1500.00}'

order = PARSE_JSON(jsonDoc, Order)
```

### XML Generation (xml-sax)

```rpg
C                   CALLP     StartElement('customer')
C                   CALLP     AddElement('name':'John Smith')
C                   CALLP     AddElement('city':'Chicago')
C                   CALLP     AddElement('balance':'1500.00')
C                   CALLP     EndElement('customer')
```

→

```pseudocode
XML_START_ELEMENT("customer")
XML_ADD_ELEMENT("name", "John Smith")
XML_ADD_ELEMENT("city", "Chicago")
XML_ADD_ELEMENT("balance", "1500.00")
XML_END_ELEMENT("customer")
```

### JSON Generation (YAJL)

```rpg
D JsonGen         S               *

C                   EVAL      JsonGen = yajl_genOpen(*OFF)
C                   CALLP     yajl_beginObj(JsonGen)
C                   CALLP     yajl_addNum(JsonGen:'orderNo':'12345')
C                   CALLP     yajl_addChar(JsonGen:'custName':'John Smith')
C                   CALLP     yajl_addNum(JsonGen:'total':'1500.00')
C                   CALLP     yajl_endObj(JsonGen)
C                   EVAL      JsonDoc = yajl_getString(JsonGen)
C                   CALLP     yajl_genClose(JsonGen)
```

→

```pseudocode
jsonGenerator: POINTER

jsonGenerator = JSON_OPEN_GENERATOR()
JSON_BEGIN_OBJECT(jsonGenerator)
JSON_ADD_NUMBER(jsonGenerator, "orderNo", 12345)
JSON_ADD_STRING(jsonGenerator, "custName", "John Smith")
JSON_ADD_NUMBER(jsonGenerator, "total", 1500.00)
JSON_END_OBJECT(jsonGenerator)
jsonDoc = JSON_GET_STRING(jsonGenerator)
JSON_CLOSE_GENERATOR(jsonGenerator)
```

## HTTP/Web Services

### HTTP GET Request

```rpg
D HttpResp        S          65535A
D Rc              S             10I 0

C                   EVAL      Rc = http_get(
C                                  'http://api.example.com/data':
C                                  HttpResp)
C                   IF        Rc = 200
C                   // Process HttpResp
C                   ENDIF
```

→

```pseudocode
httpResponse: STRING[65535]
responseCode: INTEGER

responseCode = HTTP_GET("http://api.example.com/data", httpResponse)
IF responseCode = 200 THEN
    // Process httpResponse
END IF
```

### HTTP POST Request

```rpg
D PostData        S           1000A
D HttpResp        S          65535A
D Rc              S             10I 0

C                   EVAL      PostData = '{"key":"value"}'
C                   EVAL      Rc = http_post_stmf(
C                                  'http://api.example.com/update':
C                                  '/tmp/request.json':
C                                  '/tmp/response.json':
C                                  HTTP_TIMEOUT)
```

→

```pseudocode
postData: STRING[1000]
httpResponse: STRING[65535]
responseCode: INTEGER

postData = '{"key":"value"}'
responseCode = HTTP_POST(
    "http://api.example.com/update",
    postData,
    httpResponse,
    TIMEOUT_SECONDS
)
```

### Web Service Call (SOAP)

```rpg
D SoapRequest     S           5000A
D SoapResponse    S          65535A

C                   EVAL      SoapRequest = 
C                             '<?xml version="1.0"?>' +
C                             '<soap:Envelope>' +
C                             '<soap:Body>' +
C                             '<GetCustomer>' +
C                             '<CustNo>12345</CustNo>' +
C                             '</GetCustomer>' +
C                             '</soap:Body>' +
C                             '</soap:Envelope>'

C                   EVAL      Rc = http_post_xml(
C                                  'http://api.example.com/soap':
C                                  SoapRequest:
C                                  SoapResponse)
```

→

```pseudocode
soapRequest: STRING[5000]
soapResponse: STRING[65535]

soapRequest = 
    '<?xml version="1.0"?>' +
    '<soap:Envelope>' +
    '<soap:Body>' +
    '<GetCustomer>' +
    '<CustNo>12345</CustNo>' +
    '</GetCustomer>' +
    '</soap:Body>' +
    '</soap:Envelope>'

responseCode = HTTP_POST_XML(
    "http://api.example.com/soap",
    soapRequest,
    soapResponse
)
```

## Advanced File Locking Patterns

### Record Locking

```rpg
C     Key           CHAIN(N)  File          // Read without lock
C                   IF        %FOUND(File)
C     Key           CHAIN     File          // Lock record
C                   IF        %FOUND(File)
C                   EVAL      Balance = Balance + Amount
C                   UPDATE    FileRec
C                   ENDIF
C                   ENDIF
```

→

```pseudocode
// Read without lock for validation
record = READ_BY_KEY_NO_LOCK(file, key)
IF RECORD_FOUND(file) THEN
    // Lock and read again for update
    record = READ_BY_KEY_WITH_LOCK(file, key)
    IF RECORD_FOUND(file) THEN
        record.balance = record.balance + amount
        UPDATE_RECORD(file, record)
        // Lock automatically released after update
    END IF
END IF
```

### File Override and Open Options

```rpg
C                   CALL      'QCMDEXC'
C                   PARM                    Cmd
C                   PARM                    CmdLen
C                   // Cmd = 'OVRDBF FILE(MYFILE) SHARE(*YES)'

C                   OPEN      MyFile
```

→

```pseudocode
// Override database file attributes
EXECUTE_COMMAND("OVRDBF FILE(MYFILE) SHARE(*YES)")

// Open file with overrides applied
OPEN_FILE(myFile)

// File sharing options:
// *YES - Allow sharing
// *NO - Exclusive access
// *SHRREAD - Share for read only
// *SHRUPD - Share for read and update
```

## User Space Operations

### Creating and Using User Spaces

```rpg
D UserSpace       DS                  QUALIFIED
D   Name                        10A   INZ('MYUSRSPC')
D   Lib                         10A   INZ('MYLIB')

D UsrSpcPtr       S               *
D DataPtr         S               *   BASED(UsrSpcPtr)
D DataString      S          32767A   BASED(DataPtr)

C                   CALL      'QUSCRTUS'
C                   PARM                    UserSpace
C                   // ... other parameters

C                   CALL      'QUSPTRUS'
C                   PARM                    UserSpace
C                   PARM                    UsrSpcPtr

C                   EVAL      DataString = 'My Data'
```

→

```pseudocode
STRUCTURE UserSpaceID:
    name: STRING[10] = "MYUSRSPC"
    library: STRING[10] = "MYLIB"
END STRUCTURE

userSpace: UserSpaceID
userSpacePointer: POINTER
dataString: STRING[32767]

// Create user space
CREATE_USER_SPACE(userSpace, SIZE=65536, AUTHORITY="*ALL")

// Get pointer to user space
userSpacePointer = GET_USER_SPACE_POINTER(userSpace)

// Access data through pointer
dataString = READ_FROM_POINTER(userSpacePointer, LENGTH=32767)
WRITE_TO_POINTER(userSpacePointer, "My Data")

// Notes:
// - User spaces provide shared memory between programs
// - Useful for large data structures or inter-program communication
// - Must manage memory layout manually
```

## Multiple Threading (Limited Support)

### Job Submission (Parallel Processing)

```rpg
D JobName         S             10A
D JobNumber       S              6A

C                   CALL      'QCMDEXC'
C                   PARM      'SBMJOB CMD(CALL PGM(PROCESS))' Cmd
C                   PARM      %LEN(%TRIM(Cmd))                CmdLen
```

→

```pseudocode
jobName: STRING[10]
jobNumber: STRING[6]

// Submit job to run in parallel
SUBMIT_JOB(
    COMMAND="CALL PGM(PROCESS)",
    JOB_NAME=jobName,
    JOB_NUMBER=jobNumber
)

// Notes:
// - RPG traditionally single-threaded
// - Use SBMJOB for parallel batch processing
// - Java migration: Consider Thread pools or ExecutorService
// - Monitor job completion via QSYSOPR message queue or data areas
```

### Data Queue for Inter-Job Communication

```rpg
D DtaQMsg         S            100A

// Send message to data queue
C                   CALL      'QSNDDTAQ'
C                   PARM      'MSGQUEUE' DQName
C                   PARM      'MYLIB'    DQLib
C                   PARM      100        MsgLen
C                   PARM                 DtaQMsg

// Receive from data queue (wait 5 seconds)
C                   CALL      'QRCVDTAQ'
C                   PARM      'MSGQUEUE' DQName
C                   PARM      'MYLIB'    DQLib
C                   PARM      100        MsgLen
C                   PARM                 DtaQMsg
C                   PARM      5          WaitTime
```

→

```pseudocode
dataQueueMessage: STRING[100]

// Send message (producer)
SEND_TO_DATA_QUEUE(
    QUEUE="MSGQUEUE",
    LIBRARY="MYLIB",
    MESSAGE=dataQueueMessage
)

// Receive message (consumer, wait 5 seconds)
dataQueueMessage = RECEIVE_FROM_DATA_QUEUE(
    QUEUE="MSGQUEUE",
    LIBRARY="MYLIB",
    WAIT_SECONDS=5
)

// Migration note: Consider message queues (RabbitMQ, Kafka) or Redis
```

## External Program Calls

### Dynamic Program Call

```rpg
D PgmName         S             10A
D PgmLib          S             10A
D ParmList        DS
D   Parm1                       10A
D   Parm2                       15P 2

C                   EVAL      PgmName = 'CUSTPGM'
C                   EVAL      PgmLib = 'MYLIB'
C                   CALL(E)   PgmName(PgmLib)
C                   PARM                    Parm1
C                   PARM                    Parm2
C                   IF        %ERROR
C                   // Handle error
C                   ENDIF
```

→

```pseudocode
programName: STRING[10]
programLibrary: STRING[10]
parm1: STRING[10]
parm2: DECIMAL(15,2)

programName = "CUSTPGM"
programLibrary = "MYLIB"

TRY:
    CALL_EXTERNAL_PROGRAM(
        PROGRAM=programName,
        LIBRARY=programLibrary,
        PARAMETERS=[parm1, parm2]
    )
CATCH ProgramException:
    // Handle program not found or other errors
END TRY
```

### Call Java Program from RPG

```rpg
D JavaClass       S            256A
D JavaMethod      S            256A
D JavaParm        S            100A

C                   EVAL      JavaClass = 'com.example.MyClass'
C                   EVAL      JavaMethod = 'processData'
C                   EVAL      JavaParm = 'Input Data'

C                   CALL      'QJVACMDSRV'
C                   // Parameters for Java invocation
```

→

```pseudocode
javaClass: STRING[256]
javaMethod: STRING[256]
javaParm: STRING[100]

javaClass = "com.example.MyClass"
javaMethod = "processData"
javaParm = "Input Data"

// Call Java method
CALL_JAVA_METHOD(
    CLASS=javaClass,
    METHOD=javaMethod,
    PARAMETERS=[javaParm]
)

// Migration note: Direct method call in target language
```

## Object-Oriented Features (Limited)

### Object Reference (Pointers to Procedures)

```rpg
D ProcPtr         S               *   PROCPTR
D ProcessFunc     PR
D                                     EXTPROC(ProcPtr)
D   Data                       100A

C                   EVAL      ProcPtr = %PADDR('PROCESSDATA')
C                   CALLB     ProcessFunc(MyData)
```

→

```pseudocode
processProcedure: POINTER TO FUNCTION

processProcedure = GET_PROCEDURE_ADDRESS("ProcessData")
CALL_FUNCTION_POINTER(processProcedure, [myData])

// Migration note: Use function references, lambdas, or strategy pattern
```

### Factory Pattern with Procedure Pointers

```rpg
D ProcessorPtr    S               *   PROCPTR
D Processor       PR
D                                     EXTPROC(ProcessorPtr)
D   Record                            LIKEDS(DataRec)

C                   SELECT
C                   WHEN      Type = 'A'
C                   EVAL      ProcessorPtr = %PADDR('PROCESS_A')
C                   WHEN      Type = 'B'
C                   EVAL      ProcessorPtr = %PADDR('PROCESS_B')
C                   ENDSL

C                   CALLB     Processor(Record)
```

→

```pseudocode
processorFunction: POINTER TO FUNCTION

SWITCH type:
    CASE "A":
        processorFunction = GET_PROCEDURE_ADDRESS("ProcessA")
        BREAK
    CASE "B":
        processorFunction = GET_PROCEDURE_ADDRESS("ProcessB")
        BREAK
END SWITCH

CALL_FUNCTION_POINTER(processorFunction, [record])

// Migration note: Use Strategy or Factory pattern with interfaces
```

---

**Next**: See [pseudocode-rpg-migration-guide.md](pseudocode-rpg-migration-guide.md) for migration best practices.
