# Fujitsu Mainframe Migration Patterns

Detailed migration examples and patterns for converting Fujitsu mainframe applications to modern platforms.

## Pattern 1: Fujitsu COBOL to Java Spring Boot

### Fujitsu COBOL Program Example
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. EMPMAINT.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EMPFILE ASSIGN TO "EMPDATA"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS EMP-ID
        FILE STATUS IS WS-FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD  EMPFILE.
01  EMP-RECORD.
    05  EMP-ID           PIC 9(6).
    05  EMP-NAME         PIC X(30).
    05  EMP-SALARY       PIC 9(7)V99.

WORKING-STORAGE SECTION.
01  WS-FILE-STATUS       PIC XX.

PROCEDURE DIVISION.
    OPEN INPUT EMPFILE
    MOVE 123456 TO EMP-ID
    READ EMPFILE KEY IS EMP-ID
        INVALID KEY
            DISPLAY "EMPLOYEE NOT FOUND"
        NOT INVALID KEY
            DISPLAY EMP-NAME
    END-READ
    CLOSE EMPFILE
    STOP RUN.
```

### Modern Java Spring Boot Implementation

**Entity:**
```java
@Entity
@Table(name = "employees")
public class Employee {
    @Id
    private Integer empId;
    
    @Column(length = 30)
    private String empName;
    
    @Column(precision = 9, scale = 2)
    private BigDecimal empSalary;
    
    // Constructors, getters, setters
}
```

**Repository:**
```java
@Repository
public interface EmployeeRepository extends JpaRepository<Employee, Integer> {
}
```

**Service:**
```java
@Service
public class EmployeeService {
    @Autowired
    private EmployeeRepository repository;
    
    public Employee findEmployee(Integer empId) {
        return repository.findById(empId)
            .orElseThrow(() -> new EmployeeNotFoundException("Employee not found: " + empId));
    }
}
```

**Controller:**
```java
@RestController
@RequestMapping("/api/employees")
public class EmployeeController {
    @Autowired
    private EmployeeService service;
    
    @GetMapping("/{empId}")
    public ResponseEntity<Employee> getEmployee(@PathVariable Integer empId) {
        Employee employee = service.findEmployee(empId);
        return ResponseEntity.ok(employee);
    }
}
```

## Pattern 2: Fujitsu JCL to Shell Script + Kubernetes

### Fujitsu JCL Example
```jcl
//DAILYJOB JOB (ACCT123),'DAILY PROCESS',CLASS=A,MSGCLASS=X
//STEP010  EXEC PGM=VALIDATE
//INFILE   ASSIGN DSN=INPUT.DAILY.DATA,DISP=SHR
//OUTFILE  ASSIGN DSN=VALID.DATA,DISP=(NEW,CATLG)
//STEP020  EXEC PGM=PROCESS,COND=(0,EQ,STEP010)
//INFILE   ASSIGN DSN=VALID.DATA,DISP=SHR
//OUTFILE  ASSIGN DSN=PROCESSED.DATA,DISP=(NEW,CATLG)
//STEP030  EXEC PGM=REPORT
//INFILE   ASSIGN DSN=PROCESSED.DATA,DISP=SHR
```

### Modern Shell Script
```bash
#!/bin/bash
set -e

LOG_FILE="/var/log/daily_job_$(date +%Y%m%d).log"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG_FILE"
}

# Step 1: Validate
log "Starting validation..."
./validate --input="input_daily_data.csv" --output="valid_data.csv"
validate_rc=$?

if [ $validate_rc -ne 0 ]; then
    log "Validation failed with RC=$validate_rc"
    exit $validate_rc
fi

# Step 2: Process (only if validation succeeded)
log "Starting processing..."
./process --input="valid_data.csv" --output="processed_data.csv"
process_rc=$?

if [ $process_rc -ne 0 ]; then
    log "Processing failed with RC=$process_rc"
    exit $process_rc
fi

# Step 3: Generate report
log "Generating report..."
./report --input="processed_data.csv" --output="daily_report.pdf"

log "Job completed successfully"
```

### Kubernetes CronJob
```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: daily-job
  namespace: production
spec:
  schedule: "0 2 * * *"  # Run at 2 AM daily
  successfulJobsHistoryLimit: 3
  failedJobsHistoryLimit: 3
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: daily-job
            image: company/daily-job:latest
            command: ["/scripts/daily_job.sh"]
            env:
            - name: ENV
              value: "production"
            volumeMounts:
            - name: data-volume
              mountPath: /data
            - name: logs-volume
              mountPath: /var/log
          volumes:
          - name: data-volume
            persistentVolumeClaim:
              claimName: data-pvc
          - name: logs-volume
            persistentVolumeClaim:
              claimName: logs-pvc
          restartPolicy: OnFailure
```

## Pattern 3: SYMFOWARE SQL to PostgreSQL with JPA

### SYMFOWARE Embedded SQL
```cobol
EXEC SQL
    DECLARE EMP_CURSOR CURSOR FOR
    SELECT EMP_ID, EMP_NAME, SALARY
    FROM EMPLOYEES
    WHERE DEPT_ID = :WS-DEPT-ID
    ORDER BY EMP_NAME
END-EXEC.

EXEC SQL OPEN EMP_CURSOR END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
    EXEC SQL
        FETCH EMP_CURSOR INTO :EMP-ID, :EMP-NAME, :SALARY
    END-EXEC
    IF SQLCODE = 0
        PERFORM PROCESS-EMPLOYEE
    END-IF
END-PERFORM.

EXEC SQL CLOSE EMP_CURSOR END-EXEC.
```

### Java JPA/Spring Data
```java
@Repository
public interface EmployeeRepository extends JpaRepository<Employee, Integer> {
    
    @Query("SELECT e FROM Employee e WHERE e.deptId = :deptId ORDER BY e.empName")
    List<Employee> findByDeptIdOrderByEmpName(@Param("deptId") Integer deptId);
    
    // Using method naming convention
    List<Employee> findByDeptIdOrderByEmpNameAsc(Integer deptId);
}

@Service
public class EmployeeService {
    
    @Autowired
    private EmployeeRepository repository;
    
    @Transactional(readOnly = true)
    public void processEmployeesByDept(Integer deptId) {
        List<Employee> employees = repository.findByDeptIdOrderByEmpName(deptId);
        employees.forEach(this::processEmployee);
    }
    
    private void processEmployee(Employee employee) {
        // Business logic here
        System.out.println("Processing: " + employee.getEmpName());
    }
}
```

## Pattern 4: Screen Handling - Terminal to Web

### Fujitsu COBOL Screen Definition
```cobol
01  EMPLOYEE-SCREEN.
    05  BLANK SCREEN.
    05  LINE 01 COLUMN 01 VALUE "*** EMPLOYEE MAINTENANCE ***".
    05  LINE 03 COLUMN 01 VALUE "Employee ID: ".
    05  LINE 03 COLUMN 15 PIC 9(6) TO EMP-ID.
    05  LINE 05 COLUMN 01 VALUE "Name: ".
    05  LINE 05 COLUMN 15 PIC X(30) TO EMP-NAME.
    05  LINE 07 COLUMN 01 VALUE "Salary: ".
    05  LINE 07 COLUMN 15 PIC Z(7)9.99 TO EMP-SALARY.
    05  LINE 10 COLUMN 01 VALUE "F1=Help F3=Exit F5=Refresh F8=Update".

PROCEDURE DIVISION.
    DISPLAY EMPLOYEE-SCREEN.
    ACCEPT EMPLOYEE-SCREEN.
```

### Modern Web UI with React
```jsx
import React, { useState } from 'react';
import axios from 'axios';

const EmployeeMaintenance = () => {
  const [employee, setEmployee] = useState({
    empId: '',
    empName: '',
    empSalary: ''
  });

  const handleChange = (e) => {
    setEmployee({
      ...employee,
      [e.target.name]: e.target.value
    });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const response = await axios.put(
        `/api/employees/${employee.empId}`,
        employee
      );
      alert('Employee updated successfully');
    } catch (error) {
      alert('Error updating employee: ' + error.message);
    }
  };

  return (
    <div className="container">
      <h1>*** EMPLOYEE MAINTENANCE ***</h1>
      <form onSubmit={handleSubmit}>
        <div className="form-group">
          <label>Employee ID:</label>
          <input
            type="number"
            name="empId"
            value={employee.empId}
            onChange={handleChange}
            maxLength={6}
            required
          />
        </div>
        <div className="form-group">
          <label>Name:</label>
          <input
            type="text"
            name="empName"
            value={employee.empName}
            onChange={handleChange}
            maxLength={30}
            required
          />
        </div>
        <div className="form-group">
          <label>Salary:</label>
          <input
            type="number"
            name="empSalary"
            value={employee.empSalary}
            onChange={handleChange}
            step="0.01"
            required
          />
        </div>
        <div className="button-group">
          <button type="button" onClick={() => window.showHelp()}>
            F1 - Help
          </button>
          <button type="button" onClick={() => window.close()}>
            F3 - Exit
          </button>
          <button type="button" onClick={() => window.location.reload()}>
            F5 - Refresh
          </button>
          <button type="submit">F8 - Update</button>
        </div>
      </form>
    </div>
  );
};

export default EmployeeMaintenance;
```

## Pattern 5: Batch Processing - File to Database

### Fujitsu COBOL Batch File Processing
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BATCHLOAD.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "DATAFILE"
        ORGANIZATION IS SEQUENTIAL
        FILE STATUS IS WS-INPUT-STATUS.

DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE.
01  INPUT-RECORD.
    05  IN-CUST-ID       PIC 9(8).
    05  IN-CUST-NAME     PIC X(40).
    05  IN-AMOUNT        PIC 9(9)V99.

WORKING-STORAGE SECTION.
01  WS-INPUT-STATUS      PIC XX.
01  WS-RECORD-COUNT      PIC 9(7) VALUE 0.
01  WS-ERROR-COUNT       PIC 9(7) VALUE 0.

PROCEDURE DIVISION.
    OPEN INPUT INPUT-FILE
    PERFORM UNTIL WS-INPUT-STATUS NOT = '00'
        READ INPUT-FILE
            AT END
                CONTINUE
            NOT AT END
                PERFORM PROCESS-RECORD
        END-READ
    END-PERFORM
    CLOSE INPUT-FILE
    DISPLAY 'PROCESSED: ' WS-RECORD-COUNT
    DISPLAY 'ERRORS: ' WS-ERROR-COUNT
    STOP RUN.

PROCESS-RECORD.
    ADD 1 TO WS-RECORD-COUNT
    EXEC SQL
        INSERT INTO CUSTOMERS (CUST_ID, CUST_NAME, AMOUNT)
        VALUES (:IN-CUST-ID, :IN-CUST-NAME, :IN-AMOUNT)
    END-EXEC
    IF SQLCODE NOT = 0
        ADD 1 TO WS-ERROR-COUNT
        DISPLAY 'ERROR ON RECORD: ' IN-CUST-ID
    END-IF.
```

### Modern Spring Batch
```java
@Configuration
@EnableBatchProcessing
public class BatchLoadConfiguration {
    
    @Bean
    public Job batchLoadJob(JobBuilderFactory jobBuilderFactory,
                            Step batchLoadStep) {
        return jobBuilderFactory.get("batchLoadJob")
            .incrementer(new RunIdIncrementer())
            .flow(batchLoadStep)
            .end()
            .build();
    }
    
    @Bean
    public Step batchLoadStep(StepBuilderFactory stepBuilderFactory,
                              ItemReader<Customer> reader,
                              ItemProcessor<Customer, Customer> processor,
                              ItemWriter<Customer> writer) {
        return stepBuilderFactory.get("batchLoadStep")
            .<Customer, Customer>chunk(1000)
            .reader(reader)
            .processor(processor)
            .writer(writer)
            .faultTolerant()
            .skipLimit(100)
            .skip(DataIntegrityViolationException.class)
            .listener(new JobExecutionListener() {
                @Override
                public void beforeJob(JobExecution jobExecution) {
                    log.info("Starting batch job");
                }
                
                @Override
                public void afterJob(JobExecution jobExecution) {
                    log.info("Batch job completed");
                    log.info("Records processed: " + jobExecution.getStepExecutions()
                        .iterator().next().getWriteCount());
                    log.info("Errors: " + jobExecution.getStepExecutions()
                        .iterator().next().getSkipCount());
                }
            })
            .build();
    }
    
    @Bean
    public FlatFileItemReader<Customer> reader() {
        return new FlatFileItemReaderBuilder<Customer>()
            .name("customerReader")
            .resource(new ClassPathResource("datafile.txt"))
            .delimited()
            .names("custId", "custName", "amount")
            .fieldSetMapper(new BeanWrapperFieldSetMapper<>() {{
                setTargetType(Customer.class);
            }})
            .build();
    }
    
    @Bean
    public ItemProcessor<Customer, Customer> processor() {
        return customer -> {
            // Validation logic
            if (customer.getCustId() == null || customer.getCustName() == null) {
                throw new ValidationException("Invalid customer data");
            }
            return customer;
        };
    }
    
    @Bean
    public RepositoryItemWriter<Customer> writer(CustomerRepository repository) {
        RepositoryItemWriter<Customer> writer = new RepositoryItemWriter<>();
        writer.setRepository(repository);
        writer.setMethodName("save");
        return writer;
    }
}
```

## Migration Strategy Best Practices

### 1. Start with Data Layer
- Migrate file structures to database tables first
- Validate data conversion with production samples
- Create ETL pipelines for ongoing sync during transition

### 2. Service Layer Next
- Extract business logic into services
- Maintain same inputs/outputs initially
- Add unit tests for each service

### 3. API Layer
- Expose services via REST/GraphQL APIs
- Document with OpenAPI/Swagger
- Version APIs for backward compatibility

### 4. UI Layer Last
- Modern web/mobile UI
- Progressive enhancement
- Parallel run with legacy screens

### 5. Testing Strategy
- Compare outputs: legacy vs new
- Performance benchmarking
- Load testing with production volumes
- User acceptance testing

### 6. Deployment Strategy
- Blue-green deployment
- Feature flags for gradual rollout
- Monitoring and alerting
- Rollback plan ready
