# RPG Data Structure Patterns

**Prerequisites**: Read [pseudocode-rpg-core-rules.md](pseudocode-rpg-core-rules.md) for basic data types.

---

## Data Structure Patterns

### Simple Data Structure

```rpg
D Customer        DS
D   CustNo                       10P 0
D   Name                         30A
D   Balance                      15P 2
```

→

```pseudocode
STRUCTURE Customer:
    custNo: DECIMAL(10,0)
    name: STRING[30]
    balance: DECIMAL(15,2)
END STRUCTURE
```

### Qualified Data Structure

```rpg
D Customer        DS                  QUALIFIED
D   CustNo                       10P 0
D   Name                         30A
```

→

```pseudocode
STRUCTURE Customer:    // Qualified - access as Customer.custNo
    custNo: DECIMAL(10,0)
    name: STRING[30]
END STRUCTURE
```

### Data Structure with LIKEDS

```rpg
D Address         DS                  QUALIFIED
D   Street                       50A
D   City                         30A
D
D Customer        DS                  QUALIFIED
D   Name                         30A
D   HomeAddr                          LIKEDS(Address)
D   BillAddr                          LIKEDS(Address)
```

→

```pseudocode
STRUCTURE Address:
    street: STRING[50]
    city: STRING[30]
END STRUCTURE

STRUCTURE Customer:
    name: STRING[30]
    homeAddr: Address
    billAddr: Address
END STRUCTURE
```

### Overlay

```rpg
D FullDate        DS
D   Year                          4S 0
D   Month                         2S 0  OVERLAY(FullDate:5)
D   Day                           2S 0  OVERLAY(FullDate:7)
```

→

```pseudocode
STRUCTURE FullDate:
    year: DECIMAL(4,0)           // Positions 1-4
    month: DECIMAL(2,0)          // Positions 5-6 (overlay)
    day: DECIMAL(2,0)            // Positions 7-8 (overlay)
    // Note: month and day share memory space with year
END STRUCTURE
```

### Multiple Occurrence Data Structure

```rpg
D LineItem        DS                  OCCURS(99)
D   ItemNo                        7P 0
D   Qty                           5P 0
D   Price                        11P 2

C                   EVAL      *IN01 = %OCCUR(LineItem)
C                   OCCUR     5         LineItem
```

→

```pseudocode
STRUCTURE LineItem:
    itemNo: DECIMAL(7,0)
    qty: DECIMAL(5,0)
    price: DECIMAL(11,2)
END STRUCTURE

lineItems: ARRAY[99] OF LineItem
currentOccurrence: INTEGER

currentOccurrence = 5
currentLine = lineItems[currentOccurrence]
```

## I-Spec (Input Specification) Patterns

### Fixed Format Input

```rpg
I            AA  01
I                                        1  10  CustNo
I                                       11  40  CustName
I                                       41  48 0Balance
```

→

```pseudocode
STRUCTURE InputRecord_AA:
    custNo: STRING[10]        // Positions 1-10
    custName: STRING[30]      // Positions 11-40
    balance: DECIMAL(8,0)     // Positions 41-48
END STRUCTURE

PROCEDURE ParseInputRecord(line: STRING) RETURNS InputRecord_AA
BEGIN
    record: InputRecord_AA
    record.custNo = SUBSTRING(line, 1, 10)
    record.custName = SUBSTRING(line, 11, 30)
    record.balance = TO_DECIMAL(SUBSTRING(line, 41, 8), 8, 0)
    RETURN record
END PROCEDURE
```

### Record Identification

```rpg
I            AA  01
I            BB  02
```

→

```pseudocode
// Record type AA = indicator 01
// Record type BB = indicator 02
IF recordType = "AA" THEN
    indicator01 = TRUE
ELSE IF recordType = "BB" THEN
    indicator02 = TRUE
END IF
```

## O-Spec (Output Specification) Patterns

### Report Output

```rpg
O ReportPrt  E            Heading
O                                        10 'CUSTOMER REPORT'
O                          CustNo        50
O                          CustName      80
```

→

```pseudocode
PROCEDURE WriteHeading()
BEGIN
    WRITE_LINE(reportPrinter, "CUSTOMER REPORT", POSITION=10)
END PROCEDURE

PROCEDURE WriteDetail(record: CustomerRecord)
BEGIN
    line = FORMAT_FIELD(record.custNo, 50) + 
           FORMAT_FIELD(record.custName, 80)
    WRITE_LINE(reportPrinter, line)
END PROCEDURE
```

### Conditional Output

```rpg
O                    N01                  'NO DATA'
O                     01                  'DATA FOUND'
```

→

```pseudocode
IF NOT indicator01 THEN
    WRITE_LINE(reportPrinter, "NO DATA")
ELSE
    WRITE_LINE(reportPrinter, "DATA FOUND")
END IF
```

---

**Next**: See [pseudocode-rpg-patterns.md](pseudocode-rpg-patterns.md) for common translation patterns.
