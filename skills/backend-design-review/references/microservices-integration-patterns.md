# Microservices and Integration Patterns Reference

This document provides comprehensive guidance on microservices architecture patterns, service decomposition strategies, communication patterns, data management, resilience patterns, and integration approaches.

## Table of Contents

1. [Service Decomposition](#service-decomposition)
2. [Communication Patterns](#communication-patterns)
3. [Data Management in Microservices](#data-management-in-microservices)
4. [Resilience Patterns](#resilience-patterns)
5. [Service Discovery](#service-discovery)
6. [Message Queue Patterns](#message-queue-patterns)
7. [Event Streaming Patterns](#event-streaming-patterns)
8. [Microservices Anti-Patterns](#microservices-anti-patterns)

---

## Service Decomposition

### Bounded Context (Domain-Driven Design)

**Definition:** A boundary within which a particular domain model applies

**Example: E-Commerce System**

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Order Context   │    │ Inventory       │    │ Shipping        │
│                 │    │ Context         │    │ Context         │
│ - Order         │    │ - Product       │    │ - Shipment      │
│ - OrderItem     │    │ - Stock         │    │ - Delivery      │
│ - Customer      │    │ - Warehouse     │    │ - Carrier       │
│   (reference)   │    │                 │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

**Same Entity, Different Context:**
- **Customer** in Order Context: CustomerId, Name, Email (for orders)
- **Customer** in Marketing Context: CustomerId, Preferences, Segments
- **Customer** in Billing Context: CustomerId, PaymentMethods, BillingAddress

**Identifying Bounded Contexts:**
1. Start with business capabilities (e.g., Order Management, Inventory Management)
2. Look for linguistic boundaries (different teams use different terms)
3. Identify aggregates (cluster of entities treated as a unit)
4. Consider team boundaries (Conway's Law)

### Service Size Guidelines

**Microservice:** Not too small, not too large

❌ **Too Small (Nanoservice):**
- Service does trivial task (e.g., "AddTwoNumbers" service)
- Too many inter-service calls (network overhead)
- Deployment complexity outweighs benefits

❌ **Too Large (Monolith):**
- Multiple unrelated business capabilities
- Many teams working on same codebase
- Difficult to deploy independently

✅ **Right Size:**
- Single business capability or bounded context
- Can be developed by single team (2-pizza team, ~6-10 people)
- Deployable independently
- Database can fit in memory of single instance (optional guideline)

**Rule of Thumb:** Start larger, decompose as needed (avoid premature decomposition)

### Decomposition Strategies

**1. Decompose by Business Capability**

```
E-Commerce System:
├── User Management Service
├── Product Catalog Service
├── Order Management Service
├── Payment Service
├── Inventory Service
├── Shipping Service
└── Notification Service
```

**2. Decompose by Subdomain (DDD)**

```
Core Domain (competitive advantage):
├── Order Management
├── Recommendation Engine

Supporting Domain (necessary but not differentiating):
├── Inventory Management
├── Shipping

Generic Domain (off-the-shelf):
├── Authentication (use Auth0, Okta)
├── Payment Processing (use Stripe, PayPal)
```

**3. Decompose by Verb/Use Case**

```
User Registration Service
User Authentication Service
Order Placement Service
Order Tracking Service
```

**4. Decompose by Noun/Entity**

```
User Service
Product Service
Order Service
Payment Service
```

**Recommendation:** Combine approaches (business capability + subdomain)

### Service Granularity Trade-offs

| Aspect | Coarse-Grained (Few Services) | Fine-Grained (Many Services) |
|--------|-------------------------------|------------------------------|
| Complexity | Lower | Higher |
| Team Autonomy | Lower | Higher |
| Deployment | Simpler | More complex |
| Technology Diversity | Limited | High |
| Performance | Fewer network calls | More network calls |
| Data Consistency | Easier | Harder |
| Testing | Easier | More complex |

---

## Communication Patterns

### Synchronous Communication

**1. HTTP/REST**

**Use When:**
- Request-response pattern needed
- Immediate response required
- Simple CRUD operations

**Example:**
```
Order Service → GET /api/inventory/products/123 → Inventory Service
                ← 200 OK {"id": 123, "stock": 50}
```

**Pros:**
- Simple to implement
- Well-understood
- Browser-compatible

**Cons:**
- Blocking (caller waits)
- Temporal coupling (both services must be up)
- Cascading failures

**2. gRPC**

**Use When:**
- High performance required
- Polyglot services (multiple languages)
- Streaming needed

**Example:**
```protobuf
service OrderService {
  rpc GetOrder(GetOrderRequest) returns (Order);
  rpc StreamOrders(StreamRequest) returns (stream Order);
}
```

**Pros:**
- Fast (binary protocol, HTTP/2)
- Strong typing (protobuf)
- Bi-directional streaming

**Cons:**
- Not browser-compatible (needs grpc-web)
- More complex than REST

**3. GraphQL**

**Use When:**
- Flexible client queries needed
- Multiple clients with different data needs
- Aggregation layer over multiple services

**Example:**
```graphql
query {
  user(id: "123") {
    id
    name
    orders {
      id
      total
      products {
        name
        price
      }
    }
  }
}
```

**Pros:**
- Single endpoint
- Client specifies data needed
- Reduces over-fetching/under-fetching

**Cons:**
- Complex to implement
- Query complexity management needed
- Caching more difficult

### Asynchronous Communication

**1. Message Queues (Point-to-Point)**

**Pattern:** Producer → Queue → Consumer

```
Order Service → [Order.Created Event] → Queue → Notification Service
```

**Use When:**
- Decoupling producer and consumer
- Load leveling (absorb traffic spikes)
- Guaranteed delivery needed

**Technologies:** RabbitMQ, AWS SQS, Azure Service Bus

**2. Publish-Subscribe (Topics)**

**Pattern:** Publisher → Topic → Multiple Subscribers

```
Order Service → [Order.Created Event] → Topic → Notification Service
                                              → Inventory Service
                                              → Analytics Service
```

**Use When:**
- Multiple services interested in same event
- Event-driven architecture
- Fan-out pattern

**Technologies:** Kafka, AWS SNS, Google Pub/Sub

**3. Event Streaming**

**Pattern:** Continuous stream of events, consumers read at own pace

```
Order Service → [Event Stream] → Topic (partitioned) → Consumer Group 1
                                                     → Consumer Group 2
```

**Use When:**
- High throughput needed
- Event replay required
- Real-time processing

**Technologies:** Apache Kafka, AWS Kinesis

### Synchronous vs. Asynchronous

| Aspect | Synchronous | Asynchronous |
|--------|-------------|--------------|
| Response | Immediate | Eventual |
| Coupling | Tight (temporal) | Loose |
| Failure Handling | Caller must handle | Retry mechanism |
| Complexity | Simpler | More complex |
| Throughput | Lower | Higher |
| Latency | Lower (single request) | Higher (eventual) |
| Use Case | Read, CRUD | Write, events, long-running |

**Recommendation:** Use synchronous for reads, asynchronous for writes

### API Gateway Pattern

**Purpose:** Single entry point for all clients

```
        ┌──────────────┐
        │ API Gateway  │
        └──────┬───────┘
               │
     ┌─────────┼─────────┬─────────┐
     │         │         │         │
┌────▼───┐ ┌──▼────┐ ┌──▼────┐ ┌──▼────┐
│ Auth   │ │ Order │ │ User  │ │ Product│
│ Service│ │Service│ │Service│ │Service│
└────────┘ └───────┘ └───────┘ └───────┘
```

**Responsibilities:**
- Request routing
- Authentication/authorization
- Rate limiting
- Request/response transformation
- Load balancing
- Caching
- Logging/monitoring

**Technologies:** Kong, Apigee, AWS API Gateway, Azure API Management

**Benefits:**
- Single entry point for clients
- Centralized cross-cutting concerns
- Client-specific APIs (BFF pattern)

**Drawbacks:**
- Single point of failure (mitigate with HA)
- Potential bottleneck
- Additional network hop

### Service Mesh Pattern

**Purpose:** Infrastructure layer for service-to-service communication

```
┌──────────────────┐      ┌──────────────────┐
│  Service A       │      │  Service B       │
│  ┌────────────┐  │      │  ┌────────────┐  │
│  │Application │  │      │  │Application │  │
│  └─────┬──────┘  │      │  └─────┬──────┘  │
│        │         │      │        │         │
│  ┌─────▼──────┐  │      │  ┌─────▼──────┐  │
│  │Sidecar Proxy│◄─┼──────┼─►│Sidecar Proxy│
│  │(Envoy)     │  │      │  │(Envoy)     │  │
│  └────────────┘  │      │  └────────────┘  │
└──────────────────┘      └──────────────────┘
         │                         │
         └─────────┬───────────────┘
                   │
         ┌─────────▼──────────┐
         │  Control Plane     │
         │  (Istio, Linkerd)  │
         └────────────────────┘
```

**Features:**
- Service discovery
- Load balancing
- Circuit breaking
- Retries
- Timeouts
- Mutual TLS
- Observability (metrics, traces, logs)

**Technologies:** Istio, Linkerd, Consul

**Benefits:**
- Security (mTLS, authorization)
- Reliability (retries, circuit breakers)
- Observability (automatic metrics)
- Language-agnostic (proxy handles communication)

**Drawbacks:**
- Complexity
- Resource overhead (sidecar per pod)
- Learning curve

---

## Data Management in Microservices

### Database Per Service Pattern

**Principle:** Each service owns its database (schema or entire database)

```
┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
│ Order Service   │      │ User Service    │      │ Inventory       │
│ ┌─────────────┐ │      │ ┌─────────────┐ │      │ Service         │
│ │ orders      │ │      │ │ users       │ │      │ ┌─────────────┐ │
│ │ order_items │ │      │ │ profiles    │ │      │ │ products    │ │
│ └─────────────┘ │      │ └─────────────┘ │      │ │ stock       │ │
└─────────────────┘      └─────────────────┘      │ └─────────────┘ │
                                                    └─────────────────┘
```

**Benefits:**
- Loose coupling (service can change schema independently)
- Technology diversity (choose database type per service)
- Independent scaling

**Challenges:**
- No foreign keys across services
- Joins require application logic or API calls
- Distributed transactions complex

### Shared Database Anti-Pattern

❌ **Avoid:** Multiple services sharing same database

```
┌─────────────────┐      ┌─────────────────┐
│ Order Service   │      │ Inventory       │
│                 │      │ Service         │
└────────┬────────┘      └────────┬────────┘
         │                        │
         └────────┬───────────────┘
                  │
         ┌────────▼────────┐
         │ Shared Database │
         │ ┌─────────────┐ │
         │ │ orders      │ │
         │ │ products    │ │
         │ │ users       │ │
         │ └─────────────┘ │
         └─────────────────┘
```

**Problems:**
- Tight coupling (schema changes affect all services)
- No independent deployment
- Database becomes bottleneck
- Violates service autonomy

**Exception:** Read-only shared database for reporting/analytics (with eventual consistency)

### Saga Pattern (Distributed Transactions)

**Problem:** No ACID transactions across services

**Solution:** Saga = sequence of local transactions

**Example: Order Creation Saga**

```
1. Order Service: Create Order (status=PENDING)
2. Payment Service: Reserve Payment
3. Inventory Service: Reserve Stock
4. Shipping Service: Create Shipment
5. Order Service: Update Order (status=CONFIRMED)
```

**Two Approaches:**

**1. Choreography (Event-Driven)**

```
Order Service → OrderCreated event → Payment Service
Payment Service → PaymentReserved event → Inventory Service
Inventory Service → StockReserved event → Shipping Service
Shipping Service → ShipmentCreated event → Order Service
```

**Pros:**
- Decentralized (no orchestrator)
- Simple for small sagas

**Cons:**
- Hard to understand flow (events scattered)
- Difficult to add new steps
- Cyclic dependencies risk

**2. Orchestration (Centralized)**

```
           ┌──────────────────┐
           │ Order Saga       │
           │ Orchestrator     │
           └────────┬─────────┘
                    │
        ┌───────────┼───────────┬───────────┐
        │           │           │           │
   ┌────▼────┐ ┌───▼─────┐ ┌───▼─────┐ ┌───▼─────┐
   │Payment  │ │Inventory│ │Shipping │ │Order    │
   │Service  │ │Service  │ │Service  │ │Service  │
   └─────────┘ └─────────┘ └─────────┘ └─────────┘
```

**Orchestrator Code:**
```python
class OrderSagaOrchestrator:
    def execute_saga(self, order):
        try:
            # Step 1: Reserve payment
            payment = payment_service.reserve(order.user_id, order.total)
            
            # Step 2: Reserve stock
            inventory = inventory_service.reserve(order.items)
            
            # Step 3: Create shipment
            shipment = shipping_service.create(order.id)
            
            # Step 4: Confirm order
            order_service.confirm(order.id)
            
        except Exception as e:
            # Compensating transactions (rollback)
            self.compensate(payment, inventory, shipment)
    
    def compensate(self, payment, inventory, shipment):
        if shipment:
            shipping_service.cancel(shipment.id)
        if inventory:
            inventory_service.release(inventory.id)
        if payment:
            payment_service.refund(payment.id)
        order_service.cancel(order.id)
```

**Pros:**
- Centralized logic (easy to understand)
- Easy to add steps
- Better observability

**Cons:**
- Orchestrator is single point of failure
- Orchestrator can become complex

**Recommendation:** Use orchestration for complex sagas

### Compensating Transactions

**Concept:** Undo local transaction if saga fails

**Example:**
```
Create Order → Reserve Payment → Reserve Stock → ❌ Shipping Failed
                                                   ↓
                 Refund Payment ← Release Stock ←─┘
```

**Compensating Actions:**
- Create Order → Cancel Order
- Reserve Payment → Refund Payment
- Reserve Stock → Release Stock
- Create Shipment → Cancel Shipment

**Best Practices:**
- Design compensating transactions upfront
- Make operations idempotent (can be retried safely)
- Log saga state for recovery
- Handle partial failures gracefully

### Event Sourcing

**Concept:** Store events (state changes) instead of current state

**Traditional (State Storage):**
```
accounts table:
id  | balance
123 | 500
```

**Event Sourcing (Event Storage):**
```
account_events table:
id | account_id | type          | amount | timestamp
1  | 123        | AccountOpened | 0      | 2026-01-01
2  | 123        | MoneyDeposited| 500    | 2026-01-02
3  | 123        | MoneyWithdrawn| 100    | 2026-01-03
4  | 123        | MoneyDeposited| 100    | 2026-01-04

Current balance = sum of events = 500
```

**Benefits:**
- Complete audit trail (who, what, when)
- Time travel (reconstruct state at any point)
- Event replay (rebuild projections)
- Event-driven architecture

**Challenges:**
- Query complexity (need to aggregate events)
- Event schema evolution
- Snapshots needed for performance

**Use Cases:**
- Financial systems (audit trail critical)
- Collaborative applications (undo/redo)
- Analytics (historical data)

### CQRS (Command Query Responsibility Segregation)

**Concept:** Separate read and write models

```
         Commands (Write)            Queries (Read)
              │                           │
              │                           │
      ┌───────▼────────┐          ┌──────▼──────┐
      │ Write Model    │          │ Read Model  │
      │ (normalized)   │─ events ─►│ (denorm.)   │
      │ PostgreSQL     │          │ MongoDB     │
      └────────────────┘          └─────────────┘
```

**Write Model:**
- Normalized database
- Business logic validation
- Event sourcing

**Read Model:**
- Denormalized views (projections)
- Optimized for queries
- Eventually consistent

**Example:**
```python
# Command (write)
create_order(user_id, items) → events → OrderCreated event

# Query (read)
get_orders_by_user(user_id) → Read from denormalized view
```

**Benefits:**
- Independent scaling (read vs. write)
- Optimize each model separately
- Flexibility (different databases for read/write)

**Challenges:**
- Eventual consistency
- Complexity
- Data synchronization

**Use Cases:**
- Read-heavy applications (10:1 read/write ratio)
- Complex queries (multiple aggregations)
- Different performance needs (reads need caching, writes need ACID)

---

## Resilience Patterns

### Circuit Breaker

**Problem:** Cascading failures (one slow service blocks all callers)

**Solution:** Stop calling failing service temporarily

**States:**
```
   ┌─────────────┐
   │   Closed    │ (Normal)
   │ (Allow all) │
   └──────┬──────┘
          │ failures > threshold
          │
   ┌──────▼──────┐
   │    Open     │ (Failing)
   │ (Block all) │
   └──────┬──────┘
          │ timeout
          │
   ┌──────▼──────┐
   │ Half-Open   │ (Testing)
   │ (Allow some)│
   └──────┬──────┘
          │ success → Closed
          │ failure → Open
```

**Implementation (Resilience4j):**
```java
CircuitBreakerConfig config = CircuitBreakerConfig.custom()
    .failureRateThreshold(50)              // Open after 50% failure
    .waitDurationInOpenState(Duration.ofSeconds(30))
    .slidingWindowSize(10)                 // Track last 10 calls
    .build();

CircuitBreaker circuitBreaker = CircuitBreaker.of("inventory", config);

Supplier<String> decoratedSupplier = CircuitBreaker
    .decorateSupplier(circuitBreaker, () -> inventoryService.getStock(productId));

try {
    String result = decoratedSupplier.get();
} catch (CallNotPermittedException e) {
    // Circuit breaker is open, return fallback
    return "Inventory unavailable";
}
```

**Benefits:**
- Prevents cascading failures
- Allows failing service to recover
- Fast failure (don't wait for timeout)

**Best Practices:**
- Set appropriate thresholds (50-80% failure rate)
- Set reasonable timeout (5-30 seconds)
- Provide fallback responses
- Monitor circuit breaker state

### Retry Pattern

**Concept:** Automatically retry failed requests

**Example:**
```python
from tenacity import retry, stop_after_attempt, wait_exponential

@retry(
    stop=stop_after_attempt(3),
    wait=wait_exponential(multiplier=1, min=1, max=10)
)
def call_inventory_service(product_id):
    response = requests.get(f"{INVENTORY_URL}/products/{product_id}")
    response.raise_for_status()
    return response.json()

# Retries: 1s, 2s, 4s (exponential backoff)
```

**Exponential Backoff:**
- Retry 1: Wait 1 second
- Retry 2: Wait 2 seconds
- Retry 3: Wait 4 seconds
- Retry 4: Wait 8 seconds

**Jitter:** Add randomness to avoid thundering herd
```python
wait_time = base * (2 ** attempt) + random(0, 1)
```

**When to Retry:**
- Transient failures (network timeout, 503 Service Unavailable)
- Idempotent operations (GET, PUT, DELETE)

**When NOT to Retry:**
- Non-idempotent operations (POST without idempotency key)
- Client errors (400, 401, 404)
- Business logic errors

**Best Practices:**
- Limit retry attempts (3-5 max)
- Use exponential backoff with jitter
- Only retry idempotent operations
- Log retries for monitoring

### Timeout Pattern

**Concept:** Don't wait forever for response

```python
import requests

try:
    response = requests.get(
        f"{INVENTORY_URL}/products/{product_id}",
        timeout=5  # 5 second timeout
    )
except requests.Timeout:
    return fallback_response()
```

**Timeout Guidelines:**

| Service Type | Timeout |
|--------------|---------|
| Fast API (simple query) | 1-3 seconds |
| Normal API (with DB query) | 3-10 seconds |
| Slow API (aggregation) | 10-30 seconds |
| Async operations | Immediate (return 202 Accepted) |

**Best Practices:**
- Set reasonable timeouts (not too short, not too long)
- Propagate timeouts (if A calls B with 5s timeout, B calls C with 3s)
- Return partial results if possible
- Monitor timeout rates

### Bulkhead Pattern

**Concept:** Isolate resources to prevent one failure affecting others

**Thread Pool Isolation:**
```
┌─────────────────────────────┐
│ Application                 │
│                             │
│ ┌───────────┐ ┌───────────┐│
│ │Thread Pool│ │Thread Pool││
│ │for Service│ │for Service││
│ │A (10)     │ │B (10)     ││
│ └───────────┘ └───────────┘│
└─────────────────────────────┘
```

**Implementation (Resilience4j):**
```java
ThreadPoolBulkheadConfig config = ThreadPoolBulkheadConfig.custom()
    .maxThreadPoolSize(10)
    .coreThreadPoolSize(5)
    .queueCapacity(20)
    .build();

ThreadPoolBulkhead bulkhead = ThreadPoolBulkhead.of("inventory", config);

CompletableFuture<String> future = bulkhead.executeSupplier(
    () -> inventoryService.getStock(productId)
);
```

**Benefits:**
- Isolate failures (Service A slow doesn't affect Service B)
- Resource limits (prevent resource exhaustion)

**Semaphore Isolation (Lightweight):**
```java
SemaphoreBulkheadConfig config = SemaphoreBulkheadConfig.custom()
    .maxConcurrentCalls(25)
    .build();
```

### Fallback Pattern

**Concept:** Return default/cached response when primary fails

```python
def get_product_recommendations(user_id):
    try:
        # Primary: ML recommendation service
        return recommendation_service.get(user_id)
    except Exception:
        try:
            # Fallback 1: Return cached recommendations
            return cache.get(f"recommendations:{user_id}")
        except Exception:
            # Fallback 2: Return popular products
            return product_service.get_popular_products()
```

**Fallback Strategies:**
- Return cached data
- Return default/empty response
- Return data from secondary source
- Degrade functionality gracefully

---

## Service Discovery

### Client-Side Discovery

**Pattern:** Client queries registry and load balances

```
┌────────────┐
│  Client    │
└──────┬─────┘
       │ 1. Query instances
       │
┌──────▼─────────────┐
│ Service Registry   │
│ (Consul, Eureka)   │
└──────┬─────────────┘
       │ 2. Return instances
       │    [A:8001, A:8002, A:8003]
┌──────▼─────┘
│  Client    │ 3. Load balance and call
└──────┬─────┘
       │
       ├────► Service A Instance 1 (8001)
       ├────► Service A Instance 2 (8002)
       └────► Service A Instance 3 (8003)
```

**Technologies:** Netflix Eureka, Consul, Zookeeper

**Pros:**
- Client controls load balancing
- No extra hop (direct to service)

**Cons:**
- Client complexity (discovery logic)
- Language-specific clients

### Server-Side Discovery

**Pattern:** Load balancer queries registry

```
┌────────────┐
│  Client    │
└──────┬─────┘
       │ 1. Call load balancer
       │
┌──────▼─────────────┐
│ Load Balancer      │
│ (Kubernetes, ELB)  │
└──────┬─────────────┘
       │ 2. Query registry
       │
┌──────▼─────────────┐
│ Service Registry   │
│ (Kubernetes API)   │
└────────────────────┘
       │ 3. Return instances
       │
┌──────▼─────┐
│ Load Bal.  │ 4. Load balance
└──────┬─────┘
       │
       ├────► Service A Instance 1
       ├────► Service A Instance 2
       └────► Service A Instance 3
```

**Technologies:** Kubernetes (kube-proxy), AWS ELB, NGINX

**Pros:**
- Simple client (just call load balancer)
- Language-agnostic

**Cons:**
- Extra network hop
- Load balancer single point of failure

### Health Checks

**Purpose:** Determine if service instance is healthy

**Types:**

**1. Liveness Probe:** Is service running?
```
GET /health/live → 200 OK
```

**2. Readiness Probe:** Is service ready to accept traffic?
```
GET /health/ready → 200 OK (dependencies healthy)
                  → 503 Service Unavailable (database down)
```

**Example (Spring Boot):**
```java
@RestController
public class HealthController {
    
    @GetMapping("/health/live")
    public ResponseEntity<String> liveness() {
        return ResponseEntity.ok("OK");
    }
    
    @GetMapping("/health/ready")
    public ResponseEntity<String> readiness() {
        if (databaseHealthy() && cacheHealthy()) {
            return ResponseEntity.ok("OK");
        }
        return ResponseEntity.status(503).body("Not Ready");
    }
}
```

**Kubernetes Health Checks:**
```yaml
livenessProbe:
  httpGet:
    path: /health/live
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10

readinessProbe:
  httpGet:
    path: /health/ready
    port: 8080
  initialDelaySeconds: 10
  periodSeconds: 5
```

---

## Message Queue Patterns

### Queue vs. Topic

**Queue (Point-to-Point):**
```
Producer → Queue → Consumer 1 (receives message)
                   Consumer 2 (does not receive same message)
```
- One consumer receives message
- Load balancing (distribute work)

**Topic (Publish-Subscribe):**
```
Producer → Topic → Consumer 1 (receives message)
                → Consumer 2 (receives same message)
                → Consumer 3 (receives same message)
```
- All subscribers receive message
- Fan-out pattern

### Message Schema Design

**Example: OrderCreated Event**
```json
{
  "eventId": "evt_123456",
  "eventType": "order.created",
  "timestamp": "2026-01-15T10:30:00Z",
  "version": "1.0",
  "data": {
    "orderId": "ord_789",
    "userId": "usr_123",
    "items": [
      {
        "productId": "prd_456",
        "quantity": 2,
        "price": 29.99
      }
    ],
    "total": 59.98,
    "currency": "USD"
  },
  "metadata": {
    "source": "order-service",
    "correlationId": "cor_abc"
  }
}
```

**Best Practices:**
- Include event ID (idempotency)
- Include timestamp (ordering, debugging)
- Include version (schema evolution)
- Include correlation ID (tracing)
- Self-contained (all data needed by consumers)

### Message Ordering

**Problem:** Messages may be processed out of order

**Solutions:**

**1. Partition Key (Kafka):**
```
Messages with same key go to same partition (ordered within partition)

Order 123 events → Partition 0 (ordered)
Order 456 events → Partition 1 (ordered)
```

**2. Sequence Number:**
```json
{
  "orderId": "ord_123",
  "sequenceNumber": 5,
  "eventType": "order.updated"
}
```

Consumer tracks sequence number and processes in order

**3. Single Consumer (Simple):**
- Only one consumer processes messages (ordered)
- No parallelism (slow)

### Dead Letter Queue (DLQ)

**Purpose:** Store messages that fail processing

```
Producer → Queue → Consumer
                    │ (fails after 3 retries)
                    ▼
                   DLQ → Manual intervention / Retry later
```

**When to Use DLQ:**
- Message processing fails repeatedly
- Poison message (malformed, invalid)
- Business logic error (e.g., product not found)

**Best Practices:**
- Set max retry attempts (3-5)
- Log error reason before sending to DLQ
- Monitor DLQ depth
- Process DLQ messages periodically (fix and reprocess)

### Idempotent Consumers

**Problem:** Message processed multiple times (at-least-once delivery)

**Solution:** Idempotent processing (same message processed multiple times = same result)

**Techniques:**

**1. Track Processed Message IDs:**
```python
def process_message(message):
    message_id = message['eventId']
    
    # Check if already processed
    if redis.exists(f"processed:{message_id}"):
        return  # Already processed, skip
    
    # Process message
    create_order(message['data'])
    
    # Mark as processed
    redis.setex(f"processed:{message_id}", 86400, "1")  # 24 hour TTL
```

**2. Natural Idempotency:**
```python
# Idempotent: Running multiple times = same result
UPDATE users SET email = 'new@example.com' WHERE id = 123;

# NOT idempotent: Running multiple times = different result
UPDATE users SET login_count = login_count + 1 WHERE id = 123;
```

**3. Idempotency Key (API):**
```python
POST /api/orders
Headers:
  Idempotency-Key: key_123

# Server stores result by idempotency key
# Subsequent requests with same key return cached result
```

---

## Event Streaming Patterns

### Kafka Architecture

```
┌────────────────────────────────────────┐
│            Topic: orders               │
│                                        │
│ Partition 0  Partition 1  Partition 2 │
│ ┌────────┐  ┌────────┐    ┌────────┐  │
│ │ Msg 0  │  │ Msg 1  │    │ Msg 2  │  │
│ │ Msg 3  │  │ Msg 4  │    │ Msg 5  │  │
│ │ Msg 6  │  │ Msg 7  │    │ Msg 8  │  │
│ └────────┘  └────────┘    └────────┘  │
└────────────────────────────────────────┘
         │           │            │
    ┌────┴───────────┴────────────┴────┐
    │      Consumer Group 1             │
    │  Consumer 1  Consumer 2  Consumer 3
    └───────────────────────────────────┘
```

**Key Concepts:**

- **Topic:** Category of messages (e.g., "orders", "users")
- **Partition:** Ordered, immutable sequence of messages within a topic
- **Offset:** Position of message in partition (0, 1, 2, ...)
- **Consumer Group:** Group of consumers working together
  - Each partition consumed by one consumer in group
  - Enables parallel processing

### Partitioning Strategy

**Purpose:** Distribute messages across partitions

**1. Key-Based Partitioning:**
```python
# Same key → same partition (ordered)
producer.send('orders', key=f'order:{order_id}', value=message)

partition = hash(key) % num_partitions
```

**2. Round-Robin (Default):**
- Distribute evenly if no key specified

**3. Custom Partitioner:**
```python
def custom_partitioner(key, all_partitions, available_partitions):
    # Route VIP customers to partition 0
    if key.startswith('vip:'):
        return 0
    return hash(key) % len(all_partitions)
```

**Best Practices:**
- Use key if ordering needed
- Choose high-cardinality key (many distinct values)
- Avoid hot partitions (one partition gets most traffic)

### Event Schema Evolution

**Problem:** Events in Kafka persist for days/weeks, schema may change

**Solution: Schema Registry** (Confluent Schema Registry, AWS Glue)

**Versioned Schemas:**
```
orders-value (Avro schema):
- Version 1: {orderId, userId, total}
- Version 2: {orderId, userId, items[], total, currency}  (added currency)
- Version 3: {orderId, userId, items[], total, currency, shippingAddress}
```

**Compatibility Modes:**

| Mode | Consumers | Producers |
|------|-----------|-----------|
| BACKWARD | Old consumer can read new events | New producer |
| FORWARD | New consumer can read old events | Old producer |
| FULL | Both directions compatible | Both |

**Example (Avro):**
```json
// Version 1
{
  "type": "record",
  "name": "Order",
  "fields": [
    {"name": "orderId", "type": "string"},
    {"name": "userId", "type": "string"},
    {"name": "total", "type": "double"}
  ]
}

// Version 2 (backward compatible: added field with default)
{
  "type": "record",
  "name": "Order",
  "fields": [
    {"name": "orderId", "type": "string"},
    {"name": "userId", "type": "string"},
    {"name": "total", "type": "double"},
    {"name": "currency", "type": "string", "default": "USD"}
  ]
}
```

### Stream Processing Patterns

**1. Stateless Processing:**
```python
# Filter events
stream.filter(lambda event: event['amount'] > 1000)

# Map/transform events
stream.map(lambda event: {
    'userId': event['userId'],
    'totalAmount': event['amount'] * 1.1  # Add 10% tax
})
```

**2. Stateful Processing (Aggregations):**
```python
# Count events by user (windowed)
stream \
    .groupBy('userId') \
    .windowedBy(TimeWindows.of(Duration.ofMinutes(5))) \
    .count()

# Running total per user
stream \
    .groupBy('userId') \
    .aggregate(
        initializer=lambda: 0,
        aggregator=lambda key, value, aggregate: aggregate + value['amount']
    )
```

**3. Stream Joins:**
```python
# Join orders with shipments
orders_stream.join(
    shipments_stream,
    lambda order: order['orderId'],
    lambda shipment: shipment['orderId'],
    lambda order, shipment: {**order, **shipment}
)
```

### Exactly-Once Semantics

**Delivery Guarantees:**

| Guarantee | Meaning |
|-----------|---------|
| At-most-once | Message may be lost, never duplicated |
| At-least-once | Message never lost, may be duplicated |
| Exactly-once | Message delivered exactly once |

**Kafka Exactly-Once:**
```python
producer = KafkaProducer(
    enable_idempotence=True,        # Deduplication
    transactional_id='txn-id-123'   # Transactions
)

# Transactional write (all-or-nothing)
producer.begin_transaction()
try:
    producer.send('topic1', message1)
    producer.send('topic2', message2)
    producer.commit_transaction()
except:
    producer.abort_transaction()
```

**Trade-offs:**
- Performance overhead (~30% throughput reduction)
- Complexity

**Use When:**
- Financial transactions
- Critical data (no duplicates acceptable)

---

## Microservices Anti-Patterns

### 1. Distributed Monolith

**Problem:** Microservices with tight coupling (all must be deployed together)

❌ **Symptoms:**
- Service A calls Service B synchronously
- Service B calls Service C synchronously
- All services must be deployed together
- Single point of failure cascades

✅ **Solution:**
- Use asynchronous communication
- Apply circuit breaker pattern
- Design for independent deployment

### 2. Chatty Services

**Problem:** Too many inter-service calls for single operation

❌ **Bad:**
```
Client → API Gateway → Order Service → User Service
                                    → Product Service (for each item)
                                    → Inventory Service (for each item)
                                    → Pricing Service (for each item)
# 1 + N + N + N calls for N items
```

✅ **Good:**
- Batch APIs (get multiple items in one call)
- Aggregate data in Order Service (denormalize)
- Use API Gateway to aggregate (BFF pattern)

### 3. God Service

**Problem:** One service with too many responsibilities

❌ **Symptoms:**
- Service handles multiple business capabilities
- Large codebase (>100k lines)
- Many teams working on same service

✅ **Solution:**
- Decompose by business capability
- Apply Single Responsibility Principle
- Split into smaller services

### 4. Shared Database

**Problem:** Multiple services access same database

(Already covered in Data Management section)

### 5. Nanose rvices

**Problem:** Too many tiny services (over-decomposition)

❌ **Symptoms:**
- Service does trivial task
- More network calls than business logic
- Deployment overhead > benefit

✅ **Solution:**
- Start with larger services
- Decompose only when needed (team scalability, performance)

### 6. No API Gateway

**Problem:** Clients call services directly

❌ **Consequences:**
- Tight coupling (clients know all service URLs)
- Duplicate logic (authentication in every service)
- Security issues (services exposed publicly)

✅ **Solution:**
- Implement API Gateway
- Centralize cross-cutting concerns

### 7. Ignoring Data Consistency

**Problem:** Assume strong consistency in distributed system

❌ **Expecting:**
```
Order Service creates order → Immediately visible in Analytics Service
```

**Reality:** Eventual consistency (may take seconds/minutes)

✅ **Solution:**
- Design for eventual consistency
- Use saga pattern for distributed transactions
- Communicate consistency model to users

---

## Summary

- **Service Decomposition:** Use bounded contexts, right-size services (not too small)
- **Communication:** Synchronous for reads, asynchronous for writes
- **Data Management:** Database per service, saga pattern for distributed transactions
- **Resilience:** Circuit breaker, retry with exponential backoff, timeouts, bulkhead
- **Service Discovery:** Server-side discovery (Kubernetes) simpler than client-side
- **Message Queues:** Use queues for load leveling, topics for fan-out
- **Event Streaming:** Kafka for high throughput, partitioning for ordering, schema registry for evolution
- **Anti-Patterns:** Avoid distributed monoliths, chatty services, shared databases

Well-architected microservices are loosely coupled, independently deployable, resilient, and scalable.
