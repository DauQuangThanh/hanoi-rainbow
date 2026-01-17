# Transaction Handling in Migrated Applications

This document explains how to handle transactions when migrating from CICS/mainframe environments to Java.

## Overview

COBOL programs running under CICS or IMS use implicit transaction management. In Java, transactions must be explicitly managed using appropriate frameworks and patterns.

## CICS Transaction Concepts

### CICS Transaction Model

**Key Characteristics**:
- Implicit transaction boundaries
- SYNCPOINT for explicit commits
- Automatic rollback on ABEND
- Resource coordination (files, databases, queues)
- Conversational vs. pseudo-conversational

### Common CICS Commands

```cobol
EXEC CICS SYNCPOINT END-EXEC.
EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
EXEC CICS LINK PROGRAM(name) END-EXEC.
EXEC CICS RETURN END-EXEC.
```

## Java Transaction Patterns

### 1. Programmatic Transactions

For simple cases, use JDBC transactions:

```java
Connection conn = dataSource.getConnection();
try {
    conn.setAutoCommit(false);
    
    // Perform operations
    updateCustomer(conn, customer);
    insertOrder(conn, order);
    
    conn.commit();
} catch (Exception e) {
    conn.rollback();
    throw e;
} finally {
    conn.close();
}
```

### 2. Declarative Transactions (Spring)

For most migrations, use Spring's declarative approach:

```java
@Service
@Transactional
public class OrderService {
    
    @Autowired
    private CustomerRepository customerRepo;
    
    @Autowired
    private OrderRepository orderRepo;
    
    @Transactional
    public void processOrder(OrderRequest request) {
        // All operations in one transaction
        Customer customer = customerRepo.findById(request.getCustomerId())
            .orElseThrow(() -> new CustomerNotFoundException());
        
        Order order = new Order();
        order.setCustomer(customer);
        order.setAmount(request.getAmount());
        
        orderRepo.save(order);
        
        // Transaction commits automatically on success
        // Rolls back on unchecked exceptions
    }
    
    @Transactional(readOnly = true)
    public Order getOrder(Long orderId) {
        // Read-only optimization
        return orderRepo.findById(orderId)
            .orElseThrow(() -> new OrderNotFoundException());
    }
}
```

### 3. Transaction Propagation

Map CICS patterns to Spring propagation:

```java
// CICS: New transaction (like EXEC CICS LINK SYNCONRETURN)
@Transactional(propagation = Propagation.REQUIRES_NEW)
public void processInNewTransaction() {
    // Runs in separate transaction
}

// CICS: Join existing transaction
@Transactional(propagation = Propagation.REQUIRED)
public void processInCurrentTransaction() {
    // Joins existing or creates new
}

// CICS: No transaction coordination
@Transactional(propagation = Propagation.NOT_SUPPORTED)
public void processWithoutTransaction() {
    // Suspends any existing transaction
}
```

### 4. Isolation Levels

Match CICS resource locking to SQL isolation:

```java
// CICS UPDATE (exclusive lock)
@Transactional(isolation = Isolation.SERIALIZABLE)
public void updateWithExclusiveLock() {
    // Prevents concurrent access
}

// CICS READ (shared lock)
@Transactional(isolation = Isolation.REPEATABLE_READ)
public void readWithConsistency() {
    // Consistent reads within transaction
}
```

## Migration Patterns

### Pattern 1: Batch SYNCPOINT

**COBOL/CICS**:
```cobol
PERFORM VARYING WS-COUNTER FROM 1 BY 1
    UNTIL WS-COUNTER > 1000
    
    PERFORM PROCESS-RECORD
    
    IF WS-COUNTER = 100
        EXEC CICS SYNCPOINT END-EXEC
        MOVE 0 TO WS-COUNTER
    END-IF
END-PERFORM.
```

**Java**:
```java
@Service
public class BatchProcessor {
    
    @Autowired
    private TransactionTemplate transactionTemplate;
    
    public void processBatch(List<Record> records) {
        int batchSize = 100;
        List<Record> batch = new ArrayList<>();
        
        for (Record record : records) {
            batch.add(record);
            
            if (batch.size() >= batchSize) {
                processBatchInTransaction(new ArrayList<>(batch));
                batch.clear();
            }
        }
        
        if (!batch.isEmpty()) {
            processBatchInTransaction(batch);
        }
    }
    
    private void processBatchInTransaction(List<Record> batch) {
        transactionTemplate.execute(status -> {
            batch.forEach(this::processRecord);
            return null;
        });
    }
}
```

### Pattern 2: Compensating Transactions

**Challenge**: Multi-system transactions without XA
**Solution**: Saga pattern

```java
@Service
public class OrderSagaService {
    
    public void processOrder(OrderRequest request) {
        String sagaId = UUID.randomUUID().toString();
        
        try {
            // Step 1: Reserve inventory
            String reservationId = inventoryService.reserve(request.getItems());
            
            // Step 2: Charge payment
            String paymentId = paymentService.charge(request.getPayment());
            
            // Step 3: Create order
            Order order = orderService.create(request, reservationId, paymentId);
            
            // Success - commit all
            inventoryService.confirm(reservationId);
            paymentService.confirm(paymentId);
            
        } catch (Exception e) {
            // Compensate in reverse order
            if (paymentId != null) {
                paymentService.refund(paymentId);
            }
            if (reservationId != null) {
                inventoryService.release(reservationId);
            }
            throw new OrderProcessingException("Order failed", e);
        }
    }
}
```

### Pattern 3: Distributed Transactions (XA)

For true 2-phase commit across resources:

```java
@Configuration
public class XAConfig {
    
    @Bean
    public PlatformTransactionManager transactionManager() {
        JtaTransactionManager tm = new JtaTransactionManager();
        tm.setTransactionManager(atomikosTransactionManager());
        return tm;
    }
    
    @Bean
    public DataSource xaDataSource1() {
        AtomikosDataSourceBean ds = new AtomikosDataSourceBean();
        ds.setUniqueResourceName("db1");
        ds.setXaDataSourceClassName("org.postgresql.xa.PGXADataSource");
        // ... configuration
        return ds;
    }
    
    @Bean
    public DataSource xaDataSource2() {
        AtomikosDataSourceBean ds = new AtomikosDataSourceBean();
        ds.setUniqueResourceName("db2");
        // ... configuration
        return ds;
    }
}

@Service
public class DistributedService {
    
    @Transactional
    public void updateBothDatabases() {
        // Both updates in same XA transaction
        db1Repository.update(data1);
        db2Repository.update(data2);
        // Atomikos coordinates 2PC
    }
}
```

## Error Handling

### CICS ABEND → Java Exception

**COBOL/CICS**:
```cobol
IF ERROR-CONDITION
    EXEC CICS ABEND ABCODE('APPL') END-EXEC
END-IF.
```

**Java**:
```java
@Transactional
public void process() {
    if (errorCondition) {
        // Unchecked exception triggers rollback
        throw new ApplicationException("APPL");
    }
}

// For checked exceptions that should rollback
@Transactional(rollbackFor = BusinessException.class)
public void processWithChecked() throws BusinessException {
    if (errorCondition) {
        throw new BusinessException("Error occurred");
    }
}
```

## Testing Strategies

### Unit Testing with Transactions

```java
@SpringBootTest
@Transactional
class OrderServiceTest {
    
    @Autowired
    private OrderService orderService;
    
    @Test
    @Rollback  // Default behavior
    void testOrderProcessing() {
        // Test runs in transaction
        // Automatically rolled back after test
        Order order = orderService.processOrder(request);
        assertNotNull(order.getId());
    }
    
    @Test
    @Commit  // Explicitly commit for verification
    void testOrderPersistence() {
        Order order = orderService.processOrder(request);
        // Verify in separate transaction
    }
}
```

### Integration Testing

```java
@SpringBootTest
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_EACH_TEST_METHOD)
class TransactionIntegrationTest {
    
    @Test
    void testRollbackOnError() {
        assertThrows(BusinessException.class, () -> {
            orderService.processInvalidOrder(invalidRequest);
        });
        
        // Verify rollback
        assertFalse(orderRepository.findById(orderId).isPresent());
    }
}
```

## Configuration Best Practices

### Spring Boot Configuration

```yaml
spring:
  datasource:
    hikari:
      auto-commit: false
      maximum-pool-size: 20
      connection-timeout: 30000
  jpa:
    properties:
      hibernate:
        connection:
          isolation: 2  # READ_COMMITTED
        jdbc:
          batch_size: 50
        order_inserts: true
        order_updates: true
```

### Transaction Manager Tuning

```java
@Configuration
public class TransactionConfig {
    
    @Bean
    public PlatformTransactionManager transactionManager(EntityManagerFactory emf) {
        JpaTransactionManager tm = new JpaTransactionManager(emf);
        tm.setDefaultTimeout(30);  // 30 seconds
        return tm;
    }
}
```

## Monitoring and Debugging

### Enable Transaction Logging

```yaml
logging:
  level:
    org.springframework.transaction: DEBUG
    org.springframework.orm.jpa: DEBUG
    org.hibernate.engine.transaction: DEBUG
```

### Transaction Events

```java
@Component
public class TransactionEventListener {
    
    @TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
    public void beforeCommit(OrderCreatedEvent event) {
        log.info("About to commit order: {}", event.getOrderId());
    }
    
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMPLETION)
    public void afterCompletion(OrderCreatedEvent event) {
        log.info("Transaction completed for order: {}", event.getOrderId());
    }
}
```

## Common Issues and Solutions

### Issue: Lost Updates

**Problem**: Concurrent modifications
**Solution**: Use optimistic or pessimistic locking

```java
@Entity
public class Account {
    @Id
    private Long id;
    
    @Version  // Optimistic locking
    private Long version;
    
    private BigDecimal balance;
}

// Or pessimistic locking
@Lock(LockModeType.PESSIMISTIC_WRITE)
Account findById(Long id);
```

### Issue: Long-Running Transactions

**Problem**: Locks held too long
**Solution**: Minimize transaction scope

```java
// Bad: Transaction spans user interaction
@Transactional
public void processOrderBad(OrderRequest request) {
    Order order = createOrder(request);
    sendEmail(order);  // Slow!
    updateInventory(order);
}

// Good: Transaction only for data operations
public void processOrderGood(OrderRequest request) {
    Order order;
    
    // Transaction scope
    order = transactionTemplate.execute(status -> {
        Order o = createOrder(request);
        updateInventory(o);
        return o;
    });
    
    // Outside transaction
    sendEmail(order);
}
```

## Migration Checklist

- [ ] Identify all SYNCPOINT locations
- [ ] Map CICS transaction boundaries
- [ ] Determine isolation requirements
- [ ] Plan for distributed transactions
- [ ] Define compensating actions
- [ ] Implement error handling
- [ ] Add transaction monitoring
- [ ] Test rollback scenarios
- [ ] Performance test with load
- [ ] Document transaction flows
