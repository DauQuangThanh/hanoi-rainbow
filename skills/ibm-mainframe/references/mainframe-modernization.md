# Mainframe Modernization

## Assessment Framework

### Discovery Phase

**1. Application Inventory**
```
Component Analysis:
- Number of programs (COBOL, PL/I, Assembler, REXX)
- Total lines of code by language
- JCL jobs and procedures
- CICS/IMS transactions
- Copybooks and include files
- Database schemas (DB2, IMS, VSAM)
- Screens/maps (BMS, MFS)
- External interfaces
```

**2. Technical Complexity Scoring**
```
For each program, score:
- Lines of code (1-5 points based on size)
- External calls (1 point per CALL)
- File I/O operations (2 points per file)
- Database operations (2 points per table accessed)
- CICS commands (3 points if CICS-enabled)
- Dynamic program calls (5 points each)
- Computed GO TO / ALTER (10 points each - high risk)
- Embedded assembler (10 points)

Complexity Rating:
0-20: Low
21-50: Medium
51-100: High
100+: Very High
```

**3. Business Criticality Matrix**
```
Rate each application:
- Transaction volume (1-5)
- Revenue impact (1-5)
- Customer-facing (1-5)
- Regulatory requirements (1-5)
- Frequency of change (1-5)

Priority = Sum of ratings
20-25: Critical
15-19: High
10-14: Medium
5-9: Low
```

### Dependency Analysis

**Program Call Hierarchy:**
```bash
# Extract CALL statements from COBOL
grep -r "CALL '" *.cbl | \
  sed "s/.*CALL '\([^']*\)'.*/\1/" | \
  sort | uniq > call-hierarchy.txt

# Visualize with GraphViz
echo "digraph calls {" > calls.dot
while read line; do
  echo "  \"$program\" -> \"$line\";" >> calls.dot
done < call-hierarchy.txt
echo "}" >> calls.dot
dot -Tpng calls.dot -o call-hierarchy.png
```

**Data Dependencies:**
```
Map:
- VSAM files → Programs that read/write
- DB2 tables → Programs that access
- Copybooks → Programs that include them
- JCL datasets → Jobs that use them
- GDG relationships
```

**Integration Points:**
```
Identify:
- MQ message flows
- CICS/IMS communication
- File transfers (NDM, FTP)
- Web services (z/OS Connect)
- Batch scheduling dependencies
- External system interfaces
```

## Migration Strategies

### 1. Rehost (Lift & Shift)

**Approach:**
- Migrate to mainframe emulator (Micro Focus, LzLabs, Raincode)
- Run on x86 servers or cloud
- Minimal code changes

**Pros:**
- Fastest migration (6-12 months)
- Lowest risk
- Preserve existing skills
- Quick cloud benefits

**Cons:**
- Technical debt remains
- Limited modernization
- Ongoing emulator licensing
- Performance may vary

**Best For:**
- Time-sensitive migrations
- Limited budget
- Stable applications
- Skills shortage

**Implementation Steps:**
```
1. Select emulator platform
2. Install and configure emulator
3. Migrate source code
4. Recompile programs
5. Migrate data files
6. Test functionality
7. Performance tune
8. Cutover
```

**Example: Micro Focus Enterprise Server**
```bash
# Compile COBOL to native code
cblc -C "anim" CUSTINQ.cbl

# Deploy to server
cobrun CUSTINQ

# Or compile to .NET/JVM
cblc -C "netmf" CUSTINQ.cbl
```

### 2. Replatform

**Approach:**
- Migrate to modern platform
- Keep similar architecture
- Update technology stack

**Pros:**
- Moderate risk
- Some modernization
- Better performance
- Cloud-native deployment

**Cons:**
- Longer timeline (12-24 months)
- More complex
- Requires reskilling
- Testing overhead

**Best For:**
- Moderate complexity apps
- Need for scalability
- Hybrid approach
- Gradual modernization

**Implementation: COBOL to Java**

**COBOL Source:**
```cobol
       PROCEDURE DIVISION.
           PERFORM READ-CUSTOMER
           IF CUSTOMER-FOUND
               PERFORM CALCULATE-BALANCE
               PERFORM UPDATE-RECORD
           END-IF.
```

**Java Equivalent:**
```java
public class CustomerProcessor {
    public void processCustomer(String customerId) {
        Customer customer = customerRepository.findById(customerId);
        if (customer != null) {
            BigDecimal balance = calculateBalance(customer);
            customer.setBalance(balance);
            customerRepository.save(customer);
        }
    }
    
    private BigDecimal calculateBalance(Customer customer) {
        return customer.getCredits()
            .subtract(customer.getDebits());
    }
}
```

### 3. Refactor (Microservices)

**Approach:**
- Decompose monolith
- Build microservices
- API-first design

**Pros:**
- Full modernization
- Cloud-native
- Agile development
- Technology flexibility

**Cons:**
- Longest timeline (24-36 months)
- Highest risk
- Complete reskilling
- Significant investment

**Best For:**
- Complex applications
- Long-term strategy
- Digital transformation
- Scalability needs

**Architecture Transformation:**

**Before (Monolithic):**
```
CICS Region
├── Customer Management Programs
├── Account Management Programs
├── Transaction Processing Programs
└── Reporting Programs
```

**After (Microservices):**
```
API Gateway
├── Customer Service (Spring Boot)
├── Account Service (Node.js)
├── Transaction Service (Java)
└── Reporting Service (Python)
```

**Example Microservice:**
```java
@RestController
@RequestMapping("/api/v1/customers")
public class CustomerController {
    
    @Autowired
    private CustomerService customerService;
    
    @GetMapping("/{id}")
    public ResponseEntity<CustomerDTO> getCustomer(
            @PathVariable String id) {
        return customerService.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
    
    @PostMapping
    public ResponseEntity<CustomerDTO> createCustomer(
            @Valid @RequestBody CustomerDTO customer) {
        CustomerDTO created = customerService.create(customer);
        return ResponseEntity.created(
            URI.create("/api/v1/customers/" + created.getId()))
            .body(created);
    }
    
    @PutMapping("/{id}")
    public ResponseEntity<CustomerDTO> updateCustomer(
            @PathVariable String id,
            @Valid @RequestBody CustomerDTO customer) {
        return customerService.update(id, customer)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
}
```

### 4. Retire & Replace

**Approach:**
- Replace with COTS/SaaS
- Commercial alternatives
- Modern platforms

**Pros:**
- No development
- Modern features
- Vendor support
- Quick deployment

**Cons:**
- Customization limits
- Data migration
- Process changes
- Vendor lock-in

**Best For:**
- Commodity functions
- Standard processes
- Budget constraints
- Quick wins

## Hybrid Integration Patterns

### Pattern 1: API Wrapper (Strangler Fig)

**Expose mainframe as API:**
```
External Systems → API Gateway → z/OS Connect → CICS/IMS
```

**z/OS Connect Configuration:**
```json
{
  "serviceName": "CustomerInquiry",
  "serviceProvider": "CICS",
  "endpoint": {
    "program": "CUSTINQ",
    "commarea": {
      "customerNumber": "string",
      "requestType": "string"
    }
  },
  "apiDefinition": {
    "path": "/customers/{id}",
    "method": "GET",
    "requestMapping": {
      "id": "customerNumber"
    }
  }
}
```

**API Gateway (Kong/Apigee):**
```yaml
services:
  - name: customer-service
    url: https://zosconnect.company.com/api/customers
    routes:
      - name: get-customer
        paths:
          - /api/customers
    plugins:
      - name: rate-limiting
        config:
          minute: 100
      - name: authentication
        config:
          type: oauth2
```

### Pattern 2: Data Replication (CDC)

**Real-time data sync:**
```
Mainframe DB2 → CDC (IBM InfoSphere) → Kafka → Cloud Database
```

**CDC Configuration:**
```sql
-- Source (DB2 z/OS)
CREATE TABLE CUSTOMER (
    CUST_ID INT PRIMARY KEY,
    CUST_NAME VARCHAR(50),
    LAST_UPDATE TIMESTAMP
);

-- Enable CDC
ALTER TABLE CUSTOMER 
    DATA CAPTURE CHANGES;

-- CDC capture
CREATE SUBSCRIPTION CUST_SUB FOR TABLE CUSTOMER;
```

**Kafka Consumer:**
```java
@KafkaListener(topics = "mainframe.customer")
public void processCustomerUpdate(CustomerEvent event) {
    switch(event.getOperation()) {
        case INSERT:
            cloudRepository.save(event.getData());
            break;
        case UPDATE:
            cloudRepository.update(event.getData());
            break;
        case DELETE:
            cloudRepository.delete(event.getKey());
            break;
    }
}
```

### Pattern 3: Event-Driven Integration

**Asynchronous messaging:**
```
CICS/IMS → IBM MQ → Message Broker → Cloud Services
```

**CICS to MQ:**
```cobol
       EXEC CICS WRITEQ TS
           QUEUE('CUSTOMER.UPDATES')
           FROM(CUSTOMER-RECORD)
           LENGTH(LENGTH OF CUSTOMER-RECORD)
       END-EXEC.
```

**Spring Boot Consumer:**
```java
@JmsListener(destination = "CUSTOMER.UPDATES")
public void handleCustomerUpdate(CustomerMessage msg) {
    log.info("Received customer update: {}", msg.getCustomerId());
    customerService.processUpdate(msg);
}
```

## Data Migration Strategy

### Phase 1: Analysis
```
1. Data profiling
   - Volume
   - Quality issues
   - Relationships
   - Dependencies

2. Schema mapping
   - Source to target
   - Type conversions
   - Transformation rules
   - Default values

3. Validation rules
   - Business rules
   - Referential integrity
   - Data quality checks
```

### Phase 2: Design

**VSAM to Relational Database:**

**Source (VSAM):**
```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID             PIC 9(10).
    05  CUST-NAME           PIC X(50).
    05  CUST-BALANCE        PIC S9(11)V99 COMP-3.
    05  CUST-STATUS         PIC X(01).
```

**Target (PostgreSQL):**
```sql
CREATE TABLE customer (
    cust_id BIGINT PRIMARY KEY,
    cust_name VARCHAR(50) NOT NULL,
    cust_balance NUMERIC(13,2) DEFAULT 0.00,
    cust_status CHAR(1) CHECK (cust_status IN ('A','I')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT valid_balance CHECK (cust_balance >= 0)
);

CREATE INDEX idx_customer_status ON customer(cust_status);
CREATE INDEX idx_customer_name ON customer(cust_name);
```

### Phase 3: ETL Process

**Extract from DB2:**
```sql
-- DB2 z/OS
UNLOAD EXTERNAL TABLE CUSTOMER
TO '/data/customer_extract.dat'
USING DELIMITER ','
WITH HEADER;
```

**Transform & Load:**
```python
import pandas as pd
from sqlalchemy import create_engine

# Extract
df = pd.read_csv('customer_extract.dat')

# Transform
df['cust_id'] = pd.to_numeric(df['cust_id'])
df['cust_balance'] = pd.to_numeric(df['cust_balance']) / 100
df['cust_status'] = df['cust_status'].str.strip()
df['created_at'] = pd.to_datetime('now')
df['updated_at'] = pd.to_datetime('now')

# Validate
assert df['cust_id'].is_unique, "Duplicate customer IDs"
assert df['cust_status'].isin(['A', 'I']).all(), "Invalid status"
assert (df['cust_balance'] >= 0).all(), "Negative balance"

# Load
engine = create_engine('postgresql://host/database')
df.to_sql('customer', engine, if_exists='append', index=False)

print(f"Loaded {len(df)} records")
```

### Phase 4: Validation

**Reconciliation Query:**
```sql
-- Source count
SELECT COUNT(*) FROM CUSTOMER WHERE CUST_STATUS = 'A';

-- Target count
SELECT COUNT(*) FROM customer WHERE cust_status = 'A';

-- Balance total
SELECT SUM(CUST_BALANCE) FROM CUSTOMER;
SELECT SUM(cust_balance) FROM customer;
```

## DevOps Integration

### CI/CD Pipeline for Mainframe

**1. Source Control (Git):**
```bash
# Clone mainframe source
git clone https://github.com/company/mainframe-apps.git

# Structure
mainframe-apps/
├── cobol/
│   ├── programs/
│   ├── copybooks/
│   └── tests/
├── jcl/
├── bms/
├── db2/
└── .github/workflows/
```

**2. Build Pipeline (Jenkins):**
```groovy
pipeline {
    agent any
    
    stages {
        stage('Checkout') {
            steps {
                git 'https://github.com/company/mainframe-apps.git'
            }
        }
        
        stage('Compile') {
            steps {
                script {
                    // Use Zowe CLI or DBB
                    sh '''
                        zowe files upload file-to-data-set \
                            "cobol/programs/CUSTINQ.cbl" \
                            "DEV.SOURCE(CUSTINQ)"
                        
                        zowe jobs submit data-set "DEV.JCL(COMPILE)" \
                            --wait-for-output
                    '''
                }
            }
        }
        
        stage('Test') {
            steps {
                sh '''
                    zowe jobs submit data-set "DEV.JCL(UNITTEST)" \
                        --wait-for-output
                '''
            }
        }
        
        stage('Deploy') {
            when {
                branch 'main'
            }
            steps {
                sh '''
                    zowe files copy data-set \
                        "DEV.LOAD(CUSTINQ)" \
                        "PROD.LOAD(CUSTINQ)"
                '''
            }
        }
    }
}
```

**3. Testing Strategy:**
```java
// Unit test for migrated code
@Test
public void testCalculateBalance() {
    Customer customer = new Customer();
    customer.setCredits(new BigDecimal("1000.00"));
    customer.setDebits(new BigDecimal("250.50"));
    
    BigDecimal balance = customerService.calculateBalance(customer);
    
    assertEquals(new BigDecimal("749.50"), balance);
}

// Integration test
@SpringBootTest
@AutoConfigureMockMvc
public class CustomerAPITest {
    @Autowired
    private MockMvc mockMvc;
    
    @Test
    public void testGetCustomer() throws Exception {
        mockMvc.perform(get("/api/customers/1234567890"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.customerId").value("1234567890"))
            .andExpect(jsonPath("$.balance").value(1000.00));
    }
}
```

## Migration Tools

### Automated Code Conversion

**1. COBOL to Java (AWS Blu Age):**
```bash
# Convert COBOL program
blu-age convert \
    --source cobol/CUSTINQ.cbl \
    --target java/CustomerInquiry.java \
    --framework spring-boot

# Generated Spring Boot service
```

**2. Micro Focus Visual COBOL:**
```bash
# Compile COBOL to .NET
cobc -m -x CUSTINQ.cbl -o CustomerInquiry.dll

# Or to JVM
cobc -m -x CUSTINQ.cbl -t java -o CustomerInquiry.jar
```

**3. LzLabs SDM (Software Defined Mainframe):**
```bash
# Deploy workload to container
lzlabs deploy \
    --programs cobol/*.cbl \
    --jcl jcl/*.jcl \
    --data vsam/*.dat \
    --target kubernetes
```

### Static Analysis

**Analyze code quality:**
```bash
# SonarQube for COBOL
sonar-scanner \
    -Dsonar.projectKey=mainframe-app \
    -Dsonar.sources=cobol \
    -Dsonar.language=cobol

# Code metrics
sloccount cobol/
# Output: Lines of code, complexity, maintainability
```

## Migration Timeline Example

### 18-Month Replatform Project

**Months 1-3: Discovery & Planning**
- Application inventory
- Dependency mapping
- Complexity assessment
- Tool selection
- Team training

**Months 4-6: Proof of Concept**
- Select pilot application
- Convert and deploy
- Performance testing
- Lessons learned

**Months 7-12: Bulk Migration**
- Wave 1: Low complexity (months 7-8)
- Wave 2: Medium complexity (months 9-10)
- Wave 3: High complexity (months 11-12)

**Months 13-15: Integration & Testing**
- End-to-end testing
- Performance tuning
- Security hardening
- User acceptance testing

**Months 16-18: Cutover & Stabilization**
- Production deployment
- Parallel running
- Issue resolution
- Knowledge transfer

## Post-Migration

### Decommissioning Checklist
```
□ Verify all data migrated
□ Confirm all integrations working
□ Archive mainframe code
□ Document new architecture
□ Train operations team
□ Establish monitoring
□ Update disaster recovery
□ Release mainframe resources
□ Contract negotiations
□ Celebrate success!
```

### Cost Optimization
```
Before (Mainframe):
- MIPS charges: $1M/year
- Storage: $200K/year
- Software licenses: $500K/year
Total: $1.7M/year

After (Cloud):
- Compute (VMs): $300K/year
- Storage: $50K/year
- Software licenses: $200K/year
Total: $550K/year

Savings: $1.15M/year (68% reduction)
ROI: Migration cost recovered in 18 months
```

## Success Factors

1. **Executive sponsorship** - Critical for funding and support
2. **Incremental approach** - Reduce risk, show progress
3. **Skill development** - Train team on new technologies
4. **Automated testing** - Ensure functional equivalence
5. **Data quality** - Clean data before migration
6. **Documentation** - Knowledge capture throughout
7. **Change management** - Prepare organization
8. **Risk mitigation** - Backout plans at every stage
