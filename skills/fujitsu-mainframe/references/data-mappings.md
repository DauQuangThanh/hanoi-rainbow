# Fujitsu Mainframe Data Type Mappings

Comprehensive mapping guide for converting Fujitsu mainframe data types to modern platforms.

## Fujitsu COBOL to Java Type Mapping

| Fujitsu COBOL | Java Type | Notes | Example |
|---------------|-----------|-------|---------|
| `PIC 9(n)` where n ≤ 9 | `int` | Unsigned numeric | `PIC 9(6)` → `int` |
| `PIC 9(n)` where n ≤ 18 | `long` | Unsigned numeric | `PIC 9(12)` → `long` |
| `PIC 9(n)` where n > 18 | `BigInteger` | Large unsigned | `PIC 9(25)` → `BigInteger` |
| `PIC S9(n)` where n ≤ 9 | `int` | Signed numeric | `PIC S9(6)` → `int` |
| `PIC S9(n)` where n ≤ 18 | `long` | Signed numeric | `PIC S9(12)` → `long` |
| `PIC S9(n)V9(m)` | `BigDecimal` | Decimal with precision | `PIC S9(7)V99` → `BigDecimal(9,2)` |
| `PIC 9(n)V9(m)` | `BigDecimal` | Unsigned decimal | `PIC 9(5)V99` → `BigDecimal(7,2)` |
| `PIC X(n)` | `String` | Alphanumeric | `PIC X(30)` → `String` |
| `PIC A(n)` | `String` | Alphabetic only | `PIC A(20)` → `String` |
| `PIC N(n)` | `String` | National (Unicode) | `PIC N(10)` → `String` |
| `COMP` / `BINARY` | `int`, `long` | Binary integer | Size dependent |
| `COMP-1` | `float` | Single precision | Use with caution |
| `COMP-2` | `double` | Double precision | Use with caution |
| `COMP-3` / `PACKED-DECIMAL` | `BigDecimal` | Packed decimal | **Always use BigDecimal** |
| `COMP-5` | `int`, `long` | Binary (Fujitsu) | Native binary |
| `POINTER` | Reference/Object | Object reference | Context dependent |
| `OCCURS n` | `List<T>`, `T[]` | Arrays/tables | Prefer `List<T>` |
| `OCCURS DEPENDING ON` | `List<T>` | Variable arrays | Use dynamic list |

### Critical Notes

1. **Never use `float` or `double` for monetary values** - always use `BigDecimal`
2. **COMP-3 must be BigDecimal** - financial calculations require precision
3. **Preserve precision** - don't lose decimal places during conversion
4. **Unicode handling** - `PIC N` requires proper encoding

## SYMFOWARE to PostgreSQL Type Mapping

| SYMFOWARE Type | PostgreSQL Type | Notes | Size Limits |
|----------------|-----------------|-------|-------------|
| `CHAR(n)` | `CHAR(n)` | Fixed length, space-padded | 1-10485760 |
| `VARCHAR(n)` | `VARCHAR(n)` | Variable length | 1-10485760 |
| `NCHAR(n)` | `CHAR(n)` | National char | 1-5242880 |
| `NVARCHAR(n)` | `VARCHAR(n)` | National varchar | 1-5242880 |
| `INTEGER` | `INTEGER` | 32-bit signed integer | -2147483648 to 2147483647 |
| `SMALLINT` | `SMALLINT` | 16-bit signed integer | -32768 to 32767 |
| `BIGINT` | `BIGINT` | 64-bit signed integer | Large range |
| `DECIMAL(p,s)` | `NUMERIC(p,s)` | Exact numeric | p: 1-1000, s: 0-p |
| `NUMERIC(p,s)` | `NUMERIC(p,s)` | Exact numeric | Same as DECIMAL |
| `REAL` | `REAL` | Single precision float | 6 decimal digits |
| `FLOAT` | `DOUBLE PRECISION` | Double precision float | 15 decimal digits |
| `DOUBLE PRECISION` | `DOUBLE PRECISION` | Double precision float | 15 decimal digits |
| `DATE` | `DATE` | Date only (YYYY-MM-DD) | 4713 BC to 5874897 AD |
| `TIME` | `TIME` | Time without timezone | HH:MI:SS |
| `TIME WITH TIME ZONE` | `TIME WITH TIME ZONE` | Time with timezone | |
| `TIMESTAMP` | `TIMESTAMP` | Date and time | No timezone |
| `TIMESTAMP WITH TIME ZONE` | `TIMESTAMP WITH TIME ZONE` | Date/time with zone | Recommended |
| `INTERVAL` | `INTERVAL` | Time interval | Year/month/day/time |
| `BOOLEAN` | `BOOLEAN` | True/false | PostgreSQL native |
| `BLOB` | `BYTEA` | Binary large object | Up to 1GB |
| `CLOB` | `TEXT` | Character large object | Unlimited |
| `NCLOB` | `TEXT` | National CLOB | Unlimited |
| `BINARY(n)` | `BYTEA` | Fixed binary | Fixed size |
| `VARBINARY(n)` | `BYTEA` | Variable binary | Variable size |

### PostgreSQL Advantages

1. **TEXT vs VARCHAR**: PostgreSQL's `TEXT` has no performance penalty
2. **SERIAL types**: Use `SERIAL`, `BIGSERIAL` for auto-increment
3. **Arrays**: PostgreSQL native array support: `INTEGER[]`, `TEXT[]`
4. **JSON**: `JSON` and `JSONB` for semi-structured data
5. **UUID**: Native UUID type for globally unique identifiers

## File System to Database Mapping

### Fujitsu File Organizations

| Fujitsu File Type | Description | Modern Equivalent | Implementation |
|-------------------|-------------|-------------------|----------------|
| SAM (Sequential Access) | Sequential file processing | Sequential processing | `BufferedReader`/`BufferedWriter` |
| PAM (Partitioned Access) | Directory with members | File system directory | `Files.list()`, `Files.walk()` |
| ISAM (Indexed Sequential) | Indexed file access | Database table with index | JPA Entity with `@Index` |
| VSAM KSDS | Key-sequenced dataset | DB table primary key | `@Id` field in JPA |
| VSAM ESDS | Entry-sequenced dataset | DB table auto-increment | `@GeneratedValue` |
| VSAM RRDS | Relative record dataset | Array/list-like access | Rare; use indexed table |
| GDG (Generation Data Group) | Versioned datasets | Version control files | Timestamp-based naming |

### Migration Strategy by File Type

**Sequential Files (SAM):**
```java
// Option 1: Keep as file for batch processing
Files.lines(Paths.get("input.dat"))
    .map(this::parseRecord)
    .forEach(this::processRecord);

// Option 2: Load into database
@BatchSize(1000)
List<Record> records = readRecordsFromFile();
repository.saveAll(records);
```

**Indexed Files (ISAM):**
```java
// Always migrate to database table
@Entity
@Table(name = "indexed_data", indexes = {
    @Index(name = "idx_key", columnList = "recordKey")
})
public class IndexedData {
    @Id
    private String recordKey;
    private String data;
}
```

**Partitioned Files (PAM):**
```java
// Option 1: Keep as files
Path directory = Paths.get("members");
Files.list(directory).forEach(this::processMember);

// Option 2: Store in database with member name
@Entity
public class Member {
    @Id
    private String memberName;
    @Lob
    private String content;
}
```

## COBOL Data Structures to Java Classes

### Simple Record
**COBOL:**
```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID          PIC 9(8).
    05  CUST-NAME        PIC X(40).
    05  CUST-BALANCE     PIC S9(9)V99 COMP-3.
    05  CUST-STATUS      PIC X.
```

**Java:**
```java
@Entity
@Table(name = "customers")
public class CustomerRecord {
    @Id
    @Column(name = "cust_id")
    private Integer custId;
    
    @Column(name = "cust_name", length = 40)
    private String custName;
    
    @Column(name = "cust_balance", precision = 11, scale = 2)
    private BigDecimal custBalance;
    
    @Column(name = "cust_status", length = 1)
    private String custStatus;
}
```

### Nested Structure
**COBOL:**
```cobol
01  ORDER-RECORD.
    05  ORDER-ID         PIC 9(10).
    05  CUSTOMER-INFO.
        10  CUST-ID      PIC 9(8).
        10  CUST-NAME    PIC X(30).
    05  ORDER-DATE       PIC X(8).
    05  ORDER-TOTAL      PIC 9(9)V99.
```

**Java:**
```java
@Embeddable
public class CustomerInfo {
    @Column(name = "cust_id")
    private Integer custId;
    
    @Column(name = "cust_name", length = 30)
    private String custName;
}

@Entity
@Table(name = "orders")
public class OrderRecord {
    @Id
    @Column(name = "order_id")
    private Long orderId;
    
    @Embedded
    private CustomerInfo customerInfo;
    
    @Column(name = "order_date", length = 8)
    private String orderDate;  // Or use LocalDate
    
    @Column(name = "order_total", precision = 11, scale = 2)
    private BigDecimal orderTotal;
}
```

### Array/Table (OCCURS)
**COBOL:**
```cobol
01  MONTHLY-SALES-RECORD.
    05  YEAR             PIC 9(4).
    05  MONTHLY-DATA OCCURS 12 TIMES.
        10  MONTH        PIC 99.
        10  SALES-AMOUNT PIC 9(9)V99.
```

**Java (Separate Entity):**
```java
@Entity
@Table(name = "monthly_sales")
public class MonthlySalesRecord {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "year")
    private Integer year;
    
    @OneToMany(mappedBy = "salesRecord", cascade = CascadeType.ALL)
    private List<MonthlyData> monthlyData;
}

@Entity
@Table(name = "monthly_data")
public class MonthlyData {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "month")
    private Integer month;
    
    @Column(name = "sales_amount", precision = 11, scale = 2)
    private BigDecimal salesAmount;
    
    @ManyToOne
    @JoinColumn(name = "sales_record_id")
    private MonthlySalesRecord salesRecord;
}
```

### REDEFINES (Union Types)
**COBOL:**
```cobol
01  TRANSACTION-RECORD.
    05  TRANS-TYPE       PIC X.
    05  TRANS-DATA       PIC X(50).
    05  PAYMENT-DATA REDEFINES TRANS-DATA.
        10  PAYMENT-AMT  PIC 9(7)V99.
        10  PAYMENT-DATE PIC X(8).
        10  FILLER       PIC X(33).
    05  REFUND-DATA REDEFINES TRANS-DATA.
        10  REFUND-AMT   PIC 9(7)V99.
        10  REFUND-DATE  PIC X(8).
        10  REASON       PIC X(33).
```

**Java (Separate View Classes):**
```java
@Entity
@Table(name = "transactions")
public class TransactionRecord {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "trans_type", length = 1)
    private String transType;
    
    @Column(name = "trans_data", length = 50)
    private String transData;
    
    // View as payment
    public PaymentView asPayment() {
        return new PaymentView(this.transData);
    }
    
    // View as refund
    public RefundView asRefund() {
        return new RefundView(this.transData);
    }
}

public class PaymentView {
    private BigDecimal paymentAmt;
    private String paymentDate;
    
    public PaymentView(String transData) {
        // Parse transData into payment fields
    }
}

public class RefundView {
    private BigDecimal refundAmt;
    private String refundDate;
    private String reason;
    
    public RefundView(String transData) {
        // Parse transData into refund fields
    }
}
```

## Date and Time Conversions

### COBOL Date Formats to Java

| COBOL Format | Example | Java Type | Conversion |
|--------------|---------|-----------|------------|
| `PIC 9(8)` (YYYYMMDD) | 20260115 | `LocalDate` | `LocalDate.parse("20260115", DateTimeFormatter.ofPattern("yyyyMMdd"))` |
| `PIC 9(6)` (YYMMDD) | 260115 | `LocalDate` | Handle century properly |
| `PIC X(10)` (YYYY-MM-DD) | 2026-01-15 | `LocalDate` | `LocalDate.parse("2026-01-15")` |
| `PIC 9(6)` (HHMMSS) | 143025 | `LocalTime` | `LocalTime.parse("143025", DateTimeFormatter.ofPattern("HHmmss"))` |
| `PIC 9(14)` (YYYYMMDDHHMMSS) | 20260115143025 | `LocalDateTime` | Combined parsing |

### Date Arithmetic

**COBOL:**
```cobol
COMPUTE WS-DAYS = FUNCTION INTEGER-OF-DATE(WS-END-DATE)
                - FUNCTION INTEGER-OF-DATE(WS-START-DATE)
```

**Java:**
```java
long days = ChronoUnit.DAYS.between(startDate, endDate);
```

## Best Practices

1. **Always use BigDecimal for money** - never float/double
2. **Use appropriate precision** - match COBOL precision in database
3. **Handle nulls properly** - COBOL spaces vs Java null
4. **Validate conversions** - test with production data samples
5. **Document mappings** - create mapping tables for team reference
6. **Consider performance** - index columns used in WHERE clauses
7. **Use constraints** - NOT NULL, CHECK, FOREIGN KEY
8. **Plan for Unicode** - UTF-8 encoding throughout
9. **Version control schemas** - use Liquibase/Flyway
10. **Test boundaries** - max values, min values, edge cases
