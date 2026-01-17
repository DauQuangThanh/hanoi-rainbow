# Common Architecture Anti-Patterns

A comprehensive guide to identifying and avoiding common architecture anti-patterns.

## Overview

Architecture anti-patterns are common solutions to recurring problems that are ineffective and counterproductive. This guide helps identify these patterns and provides alternatives.

## Table of Contents
1. [Distributed System Anti-Patterns](#distributed-system-anti-patterns)
2. [Data Management Anti-Patterns](#data-management-anti-patterns)
3. [Integration Anti-Patterns](#integration-anti-patterns)
4. [Design Anti-Patterns](#design-anti-patterns)
5. [Deployment Anti-Patterns](#deployment-anti-patterns)

---

## Distributed System Anti-Patterns

### 1. Distributed Monolith

**Description**: Microservices that are tightly coupled and must be deployed together.

**Symptoms**:
- Services share same database
- Services call each other synchronously for every operation
- Cannot deploy services independently
- Cascading failures common
- All services must be updated together

**Example**:
```
OrderService → calls → InventoryService → calls → PaymentService
      ↓                        ↓                       ↓
   Same Database         Same Database          Same Database
```

**Why It's Bad**:
- Complexity of microservices without benefits
- Cannot scale services independently
- No fault isolation
- Difficult to develop independently
- Defeats purpose of microservices

**How to Fix**:
- Database per service pattern
- Asynchronous communication via events
- API Gateway for client requests
- Circuit breakers for resilience
- Each service independently deployable

**Alternative Pattern**: Start with modular monolith, extract services when needed.

---

### 2. Chatty Communication

**Description**: Too many fine-grained service calls causing performance issues.

**Symptoms**:
- Multiple API calls to complete one operation
- High network latency
- N+1 query problem across services
- Poor user experience
- High bandwidth usage

**Example**:
```
Client → getOrder() → Order Service
       → getCustomer() → Customer Service
       → getAddress() → Address Service
       → getPayment() → Payment Service
       → getItems() → Item Service (N times for N items)
```

**Why It's Bad**:
- High latency (network round trips)
- Increased failure points
- Resource intensive
- Poor performance
- Difficult to troubleshoot

**How to Fix**:
- Aggregate APIs (BFF - Backend for Frontend)
- GraphQL for flexible querying
- Data denormalization where appropriate
- Batch API endpoints
- Caching layer

**Pattern**: API Gateway with data aggregation

---

### 3. Single Point of Failure (SPOF)

**Description**: Critical component with no redundancy causing system-wide failure.

**Common SPOFs**:
- Single database instance
- Single load balancer
- Single message broker
- Single cache instance
- Single authentication service

**Example**:
```
All Services → Single Database → Failure = Total Outage
```

**Why It's Bad**:
- Complete system failure risk
- No fault tolerance
- Poor availability
- Business impact high
- Customer trust erosion

**How to Fix**:
- Database replication (primary-replica)
- Load balancer redundancy
- Multi-AZ deployment
- Circuit breakers for graceful degradation
- Health checks and auto-recovery

**Pattern**: Active-Passive or Active-Active redundancy

---

### 4. Cascading Failures

**Description**: Failure in one service propagates and causes failures in dependent services.

**Symptoms**:
- One service failure brings down entire system
- Thread pool exhaustion
- Connection pool exhaustion
- Timeout not configured
- No circuit breakers

**Example**:
```
Service A (healthy) → Service B (slow) → Service C (down)
Result: All services fail as threads wait
```

**Why It's Bad**:
- Magnifies impact of single failure
- Difficult to recover
- Affects all dependent services
- Hard to identify root cause
- Extended downtime

**How to Fix**:
- Circuit breaker pattern (Hystrix, Resilience4j)
- Timeouts on all external calls
- Bulkhead pattern for isolation
- Graceful degradation
- Health checks and monitoring

**Patterns**:
- Circuit Breaker
- Bulkhead
- Timeout
- Fallback

---

## Data Management Anti-Patterns

### 5. Database as Integration Point

**Description**: Multiple services sharing the same database tables.

**Symptoms**:
- Services access same tables
- Schema changes affect multiple services
- Tight coupling through data
- Cannot deploy independently
- No service ownership

**Example**:
```
OrderService    →     Shared Database    ←    CustomerService
InvoiceService  →     (Orders, Customers) ←   ShippingService
```

**Why It's Bad**:
- Tight coupling defeats microservices
- Schema changes require coordination
- Cannot scale database per service needs
- No data ownership boundaries
- Transaction spanning services

**How to Fix**:
- Database per service pattern
- Services own their data
- API calls for cross-service data
- Event-driven data synchronization
- CQRS for read models

**Pattern**: Database per Service

---

### 6. Data Monolith

**Description**: Single massive database with all application data.

**Symptoms**:
- One database for all services
- Schema grows indefinitely
- Cannot scale independently
- Backup/restore takes hours
- Performance degradation over time

**Why It's Bad**:
- Scalability bottleneck
- Single point of failure
- Difficult to maintain
- Long backup/restore times
- Technology lock-in

**How to Fix**:
- Separate databases by bounded context
- Read replicas for read-heavy workloads
- Sharding for horizontal scaling
- Archive old data
- Different databases for different needs (SQL, NoSQL)

---

### 7. Shared Mutable State

**Description**: Multiple components sharing and modifying the same state.

**Symptoms**:
- Race conditions
- Data corruption
- Difficult to debug
- Unpredictable behavior
- Cannot scale horizontally

**Why It's Bad**:
- Concurrency issues
- Not thread-safe
- Cannot scale horizontally
- Difficult to reason about
- Bugs hard to reproduce

**How to Fix**:
- Immutable data structures
- Event sourcing
- Stateless services
- External session store (Redis)
- Message-based communication

---

## Integration Anti-Patterns

### 8. God Service / API

**Description**: One service that does everything.

**Symptoms**:
- Service has 100+ endpoints
- Handles multiple business domains
- Large codebase (10K+ lines)
- Multiple teams working on it
- Frequent conflicts and deployments

**Why It's Bad**:
- Violates Single Responsibility
- Difficult to maintain
- Cannot scale specific functions
- Deployment risk high
- Team coordination overhead

**How to Fix**:
- Decompose by business capability
- Extract smaller services
- Domain-Driven Design
- Clear service boundaries
- Strangler fig pattern for gradual migration

---

### 9. Anemic Domain Model

**Description**: Domain objects with no behavior, just data and getters/setters.

**Symptoms**:
- POJOs with only getters/setters
- Business logic in service layer
- Entities are just data containers
- No domain behavior
- Procedural programming in OO language

**Example**:
```java
// Anemic
class Order {
    private double total;
    public double getTotal() { return total; }
    public void setTotal(double total) { this.total = total; }
}

class OrderService {
    public void calculateTotal(Order order, List<Item> items) {
        double total = 0;
        for (Item item : items) {
            total += item.getPrice() * item.getQuantity();
        }
        order.setTotal(total);
    }
}
```

**Why It's Bad**:
- Not object-oriented
- Business logic scattered
- Difficult to test
- Violates encapsulation
- Procedural mindset

**How to Fix - Rich Domain Model**:
```java
class Order {
    private List<OrderItem> items;
    private Money total;
    
    public void addItem(Product product, int quantity) {
        items.add(new OrderItem(product, quantity));
        recalculateTotal();
    }
    
    private void recalculateTotal() {
        total = items.stream()
            .map(OrderItem::getSubtotal)
            .reduce(Money.ZERO, Money::add);
    }
    
    public Money getTotal() { return total; }
}
```

---

### 10. Integration Spaghetti

**Description**: Point-to-point integrations creating a tangled mess.

**Symptoms**:
- Direct service-to-service calls everywhere
- No integration layer
- Difficult to trace requests
- Each service knows about many others
- Circular dependencies

**Example**:
```
ServiceA ←→ ServiceB ←→ ServiceC
    ↕           ↕           ↕
ServiceD ←→ ServiceE ←→ ServiceF
```

**Why It's Bad**:
- High coupling
- Difficult to change
- Hard to understand dependencies
- Cannot add new service easily
- Testing difficult

**How to Fix**:
- API Gateway pattern
- Event-driven architecture
- Service mesh (Istio, Linkerd)
- Message broker (Kafka, RabbitMQ)
- Clear service boundaries

---

## Design Anti-Patterns

### 11. Big Ball of Mud

**Description**: System with no recognizable structure.

**Symptoms**:
- No clear architecture
- High coupling everywhere
- No separation of concerns
- Spaghetti code
- "Just make it work" mentality

**Why It's Bad**:
- Impossible to maintain
- Difficult to understand
- Cannot scale or evolve
- High bug rate
- New features take forever

**How to Fix**:
- Identify bounded contexts
- Refactor toward layered architecture
- Apply SOLID principles
- Introduce clear boundaries
- Gradual improvement (boy scout rule)

---

### 12. Premature Optimization

**Description**: Optimizing before understanding actual performance issues.

**Symptoms**:
- Complex code for theoretical performance
- Micro-optimizations everywhere
- Caching everything
- No performance measurements
- "It might be slow" reasoning

**Example**:
```java
// Premature optimization
String result = new StringBuilder()
    .append("Hello")
    .append(" ")
    .append("World")
    .toString();

// Simple is better
String result = "Hello World";
```

**Why It's Bad**:
- Increased complexity
- Harder to maintain
- May not solve real problems
- Wastes development time
- "The root of all evil" - Donald Knuth

**How to Fix**:
- Measure first, optimize second
- Use profiling tools
- Focus on algorithmic improvements
- Keep it simple (KISS)
- Optimize bottlenecks only

---

### 13. Golden Hammer

**Description**: Using same solution/technology for all problems.

**Examples**:
- "Everything is a microservice"
- "Always use NoSQL"
- "Use same framework for all projects"
- "Always use Kubernetes"

**Why It's Bad**:
- Wrong tool for the job
- Unnecessary complexity
- Poor performance
- Higher costs
- Team frustration

**How to Fix**:
- Understand problem first
- Evaluate alternatives
- Choose appropriate tool
- Consider trade-offs
- Polyglot architecture when needed

---

## Deployment Anti-Patterns

### 14. Manual Deployment

**Description**: Deploying software manually through UI or scripts.

**Symptoms**:
- Deployments take hours
- Different process for each environment
- "Works on my machine"
- Frequent deployment failures
- Manual configuration steps

**Why It's Bad**:
- Error-prone
- Not reproducible
- Time-consuming
- Risky
- Cannot deploy frequently

**How to Fix**:
- CI/CD pipeline automation
- Infrastructure as Code
- Containerization (Docker)
- Automated testing in pipeline
- Blue-green or canary deployments

---

### 15. Shared Development Database

**Description**: All developers sharing single database instance.

**Symptoms**:
- Cannot test locally
- Conflicts with other developers
- Schema changes affect everyone
- Cannot reproduce bugs
- Test data pollution

**Why It's Bad**:
- Development slowdown
- Cannot work offline
- Difficult to test
- Data inconsistency
- Lack of isolation

**How to Fix**:
- Local database per developer
- Database migration scripts (Flyway, Liquibase)
- Docker for local databases
- Test containers
- Database seeding scripts

---

### 16. Configuration in Code

**Description**: Hardcoding configuration values.

**Symptoms**:
- Environment-specific values in code
- Need to rebuild for config changes
- Secrets in source control
- Different code per environment

**Why It's Bad**:
- Security risk (secrets exposed)
- Cannot change config without rebuild
- Different builds per environment
- Violates 12-factor app

**How to Fix**:
- Externalize configuration
- Environment variables
- Configuration server (Spring Cloud Config)
- Secrets manager (Vault, AWS Secrets Manager)
- 12-factor app principles

---

## Anti-Pattern Detection Checklist

Use this checklist during architecture reviews:

**Distributed Systems**:
- [ ] Services can deploy independently?
- [ ] No shared databases between services?
- [ ] Asynchronous communication where possible?
- [ ] Circuit breakers implemented?
- [ ] No single points of failure?

**Data Management**:
- [ ] Each service owns its data?
- [ ] No shared mutable state?
- [ ] Appropriate database type selected?
- [ ] Data consistency model defined?

**Integration**:
- [ ] No point-to-point integration spaghetti?
- [ ] API Gateway or service mesh used?
- [ ] Services not chatty?
- [ ] Bounded contexts clear?

**Design**:
- [ ] Clean architecture principles followed?
- [ ] Not over-engineered?
- [ ] Appropriate technology choices?
- [ ] Not premature optimization?

**Deployment**:
- [ ] Automated deployment?
- [ ] Configuration externalized?
- [ ] Infrastructure as Code?
- [ ] Environment parity?

---

## Summary

Anti-patterns are common pitfalls that seem like good solutions but cause problems. Key takeaways:

1. **Recognize anti-patterns early** - easier to fix
2. **Understand the "why"** - learn the root cause
3. **Apply patterns, not anti-patterns** - know the difference
4. **Context matters** - what's anti-pattern in one context may be acceptable in another
5. **Refactor gradually** - don't try to fix everything at once

Remember: "The first rule of architecture is: don't over-architect." Keep it simple, measure, and improve based on actual needs.
