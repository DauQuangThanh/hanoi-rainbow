# Backend Design Review Process


### Phase 1: Pre-Review Preparation

**Activities:**
1. **Gather Design Documentation**
   - Collect architecture diagrams (Mermaid C4 model, sequence diagrams)
   - Obtain API specifications (OpenAPI, GraphQL schemas, protobuf definitions)
   - Review database schemas (Mermaid ERD, schema diagrams)
   - Gather ADRs (Architecture Decision Records)
   - Review requirements and technical specifications

2. **Understand Context**
   - Review functional requirements and business objectives
   - Understand scalability requirements (users, requests/second, data volume)
   - Identify compliance requirements (GDPR, HIPAA, PCI-DSS)
   - Note technical constraints (cloud provider, existing systems, budget)
   - Understand team expertise and organizational structure

3. **Define Review Scope**
   - Identify critical components to review in depth
   - Determine review depth (high-level vs. detailed)
   - Set review priorities based on risk and importance
   - Establish timeline and deliverables
   - Identify key stakeholders

**Deliverables:**
- Review scope document
- Documentation inventory
- Context summary with constraints
- Review timeline and milestones

### Phase 2: API Design Review

**Review Areas:**

**RESTful API Design:**

**Resource Modeling:**
- [ ] Resources represent domain entities clearly
- [ ] Resource hierarchies logical and intuitive
- [ ] Nested resources used appropriately (max 2 levels)
- [ ] Collection and singular resources distinguished
- [ ] Resource naming follows conventions (plural nouns: `/users`, `/orders`)

**HTTP Method Usage:**
- [ ] GET for retrieval (no side effects, idempotent)
- [ ] POST for creation (non-idempotent)
- [ ] PUT for full replacement (idempotent)
- [ ] PATCH for partial updates (idempotent recommended)
- [ ] DELETE for removal (idempotent)
- [ ] Methods used semantically correctly

**Status Codes:**
- [ ] 200 OK for successful GET, PUT, PATCH
- [ ] 201 Created for successful POST with `Location` header
- [ ] 204 No Content for successful DELETE
- [ ] 400 Bad Request for validation errors with error details
- [ ] 401 Unauthorized for authentication failures
- [ ] 403 Forbidden for authorization failures
- [ ] 404 Not Found for non-existent resources
- [ ] 409 Conflict for business logic conflicts
- [ ] 429 Too Many Requests for rate limiting
- [ ] 500 Internal Server Error for server errors
- [ ] 503 Service Unavailable for maintenance or overload

**URL Design:**
- [ ] URLs intuitive and readable
- [ ] Kebab-case or snake_case consistent (prefer kebab-case)
- [ ] Query parameters for filtering, sorting, pagination
- [ ] No verbs in URLs (use HTTP methods instead)
- [ ] Versioning strategy clear (URL path, header, or content negotiation)

**Example URL Patterns:**
```
✅ Good:
GET    /api/v1/users
GET    /api/v1/users/{id}
POST   /api/v1/users
PUT    /api/v1/users/{id}
PATCH  /api/v1/users/{id}
DELETE /api/v1/users/{id}
GET    /api/v1/users/{id}/orders
GET    /api/v1/orders?status=pending&sort=-createdAt&page=2&limit=20

❌ Bad:
GET    /api/v1/getUsers
POST   /api/v1/createUser
GET    /api/v1/user (singular for collection)
GET    /api/v1/users/{id}/orders/{id2}/items/{id3} (too nested)
```

**Request/Response Design:**
- [ ] Request bodies use JSON (or appropriate format)
- [ ] Response bodies consistent structure
- [ ] Field naming consistent (camelCase or snake_case)
- [ ] Enveloping avoided (return data directly, use HTTP for metadata)
- [ ] Pagination metadata included (total, page, limit, links)
- [ ] Error responses follow consistent format
- [ ] Date/time in ISO 8601 format (UTC recommended)
- [ ] Null vs. omitting fields strategy defined

**Pagination:**
```json
✅ Good (Cursor-based):
GET /api/v1/users?cursor=eyJpZCI6MTAwfQ==&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "nextCursor": "eyJpZCI6MTIwfQ==",
    "hasMore": true
  }
}

✅ Good (Offset-based):
GET /api/v1/users?page=2&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 150,
    "totalPages": 8
  }
}
```

**GraphQL Schema Design:**

**Type Definitions:**
- [ ] Types represent domain concepts clearly
- [ ] Scalar types used appropriately (ID, String, Int, Float, Boolean)
- [ ] Custom scalars defined where needed (DateTime, URL, Email)
- [ ] Enums used for fixed sets of values
- [ ] Non-null fields marked with `!` appropriately
- [ ] Lists marked with `[]` appropriately

**Query Design:**
- [ ] Queries follow naming conventions (noun-based)
- [ ] Arguments documented with descriptions
- [ ] Pagination implemented (cursor-based recommended)
- [ ] Filtering and sorting arguments provided
- [ ] Query complexity limits defined
- [ ] Query depth limits enforced

**Mutation Design:**
- [ ] Mutations follow naming conventions (verb-based: `createUser`, `updateOrder`)
- [ ] Input types used for complex arguments
- [ ] Mutations return updated objects or payloads
- [ ] Error handling strategy clear
- [ ] Idempotency considered for critical mutations

**N+1 Query Prevention:**
- [ ] DataLoader pattern implemented
- [ ] Batching strategy for related data
- [ ] Resolver complexity analyzed
- [ ] Query cost analysis performed

**gRPC Service Design:**

**Protobuf Definitions:**
- [ ] Message types well-defined and documented
- [ ] Field numbering consistent and never reused
- [ ] Required vs. optional fields appropriate
- [ ] Enums used for fixed value sets
- [ ] Nested messages used appropriately
- [ ] Backward compatibility maintained

**Service Definitions:**
- [ ] Service methods clearly named (verb + noun)
- [ ] Unary, server streaming, client streaming, bidirectional streaming used appropriately
- [ ] Request/response types specific (not generic)
- [ ] Error handling strategy defined (status codes, error details)
- [ ] Service versioning strategy clear

**API Security:**
- [ ] Authentication mechanism defined (JWT, OAuth 2.0, API keys)
- [ ] Authorization checks at API gateway and service level
- [ ] Input validation comprehensive (type, format, range, length)
- [ ] SQL injection prevention (parameterized queries, ORMs)
- [ ] Rate limiting implemented (per user, per IP, per endpoint)
- [ ] CORS configured appropriately
- [ ] Sensitive data not logged or exposed in errors

**API Documentation:**
- [ ] OpenAPI/Swagger spec complete and accurate
- [ ] All endpoints documented with descriptions
- [ ] Request/response examples provided
- [ ] Error responses documented
- [ ] Authentication requirements clear
- [ ] Deprecation warnings for legacy endpoints
- [ ] Changelog maintained for API versions

**Severity Ratings:**
- 🔴 **Critical**: Security vulnerabilities, data loss risks, or broken core functionality
- 🟠 **High**: Significant design flaws affecting scalability, performance, or reliability
- 🟡 **Medium**: Moderate issues or deviations from best practices
- 🟢 **Low**: Minor improvements or optimization opportunities

### Phase 3: Database Design Review

**Review Areas:**

**Data Modeling:**

**Entity Relationships:**
- [ ] Entities represent domain concepts clearly
- [ ] Relationships accurately model business rules
- [ ] One-to-many, many-to-many relationships correct
- [ ] Self-referencing relationships handled properly
- [ ] Recursive relationships (hierarchies) designed efficiently

**Normalization:**
- [ ] Appropriate normalization level (typically 3NF)
- [ ] Functional dependencies identified
- [ ] Redundancy eliminated where appropriate
- [ ] Denormalization justified for performance (documented)
- [ ] Update anomalies prevented

**Schema Design:**

**Table Structure:**
- [ ] Table naming conventions followed (plural nouns: `users`, `orders`)
- [ ] Column naming conventions followed (snake_case typical)
- [ ] Primary keys defined (prefer surrogate keys: UUID, bigint)
- [ ] Foreign keys defined with proper constraints
- [ ] Unique constraints defined where needed
- [ ] Check constraints for business rules
- [ ] Default values appropriate
- [ ] Nullable vs. NOT NULL decisions appropriate

**Data Types:**
- [ ] Column types appropriate for data (INT, BIGINT, VARCHAR, TEXT, JSON, etc.)
- [ ] String lengths appropriate (VARCHAR(255) vs. TEXT)
- [ ] Numeric precision appropriate (DECIMAL for money)
- [ ] Date/time types appropriate (TIMESTAMP vs. DATE vs. TIME)
- [ ] Boolean types used for true/false values
- [ ] JSON/JSONB used appropriately (not overused)
- [ ] Enum types vs. lookup tables decision justified

**Example Schema Issues:**

❌ **Bad:**
```sql
CREATE TABLE user (  -- singular
    id INT,  -- may overflow
    name VARCHAR(50),  -- too short
    email VARCHAR(100),  -- no unique constraint
    balance FLOAT,  -- precision issues for money
    created DATETIME  -- no timezone
);
```

✅ **Good:**
```sql
CREATE TABLE users (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    balance DECIMAL(19, 4) NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_email (email),
    INDEX idx_created_at (created_at)
);
```

**Indexes:**
- [ ] Primary keys automatically indexed
- [ ] Foreign keys indexed for join performance
- [ ] Indexes on frequently queried columns
- [ ] Composite indexes for multi-column queries (column order optimized)
- [ ] Unique indexes for uniqueness constraints
- [ ] Covering indexes considered for query optimization
- [ ] Index overhead balanced (too many indexes hurt writes)
- [ ] Index maintenance strategy defined

**Partitioning:**
- [ ] Partitioning strategy defined for large tables (> 100M rows)
- [ ] Partition key chosen appropriately (date, range, hash)
- [ ] Partition pruning considered in query design
- [ ] Partition maintenance strategy defined
- [ ] Partition limits considered (max partitions per table)

**Query Patterns:**

**Query Efficiency:**
- [ ] Queries use indexes effectively
- [ ] SELECT * avoided (select only needed columns)
- [ ] N+1 query problem prevented (eager loading, joins, batching)
- [ ] Unnecessary joins avoided
- [ ] Subqueries vs. joins trade-offs considered
- [ ] LIMIT used for pagination
- [ ] Query complexity reasonable (avoid deeply nested subqueries)

**Example N+1 Query Problem:**

❌ **Bad (N+1 queries):**
```python
# 1 query to get users
users = db.query("SELECT * FROM users")

# N queries to get orders for each user
for user in users:
    orders = db.query(f"SELECT * FROM orders WHERE user_id = {user.id}")
```

✅ **Good (2 queries with batching or 1 query with join):**
```python
# Approach 1: Batching (2 queries)
users = db.query("SELECT * FROM users")
user_ids = [user.id for user in users]
orders = db.query(f"SELECT * FROM orders WHERE user_id IN ({user_ids})")

# Approach 2: Join (1 query)
results = db.query("""
    SELECT u.*, o.*
    FROM users u
    LEFT JOIN orders o ON u.id = o.user_id
""")
```

**Data Integrity:**
- [ ] Foreign key constraints enforced
- [ ] Cascading deletes defined appropriately (CASCADE, SET NULL, RESTRICT)
- [ ] Check constraints for business rules
- [ ] Unique constraints for uniqueness requirements
- [ ] Triggers used sparingly (complexity, performance impact)
- [ ] Application-level validation complements database constraints

**Scalability Considerations:**

**Database Scaling Strategies:**
- [ ] Vertical scaling limits identified
- [ ] Horizontal scaling strategy defined (sharding, read replicas)
- [ ] Sharding key chosen appropriately (balanced distribution, query locality)
- [ ] Read replicas for read-heavy workloads
- [ ] Caching layer designed (Redis, Memcached)
- [ ] Connection pooling configured
- [ ] Database monitoring and alerting defined

**Caching Strategy:**
- [ ] Cache-aside pattern for reads
- [ ] Write-through or write-behind for writes
- [ ] Cache invalidation strategy clear
- [ ] TTL (Time-To-Live) defined for cached data
- [ ] Cache warming strategy for critical data
- [ ] Cache stampede prevention (locking, probabilistic early expiration)

### Phase 4: Microservices Architecture Review

**Review Areas:**

**Service Boundaries:**

**Service Decomposition:**
- [ ] Services aligned with bounded contexts (DDD)
- [ ] Services have clear responsibilities (high cohesion)
- [ ] Services loosely coupled (low coupling)
- [ ] Services independently deployable
- [ ] Services own their data (database-per-service pattern)
- [ ] Services sized appropriately (not too small, not too large)
- [ ] Service boundaries stable (infrequent changes)

**Bounded Context Alignment:**
- [ ] Each service represents a bounded context
- [ ] Domain model clear within each context
- [ ] Context boundaries well-defined
- [ ] Ubiquitous language used within context
- [ ] Context maps document relationships

**Anti-patterns to Avoid:**
- ❌ Distributed monolith (services tightly coupled)
- ❌ God service (one service doing too much)
- ❌ Anemic services (services with only CRUD operations)
- ❌ Shared database (multiple services accessing same tables)

**Communication Patterns:**

**Synchronous Communication (HTTP, gRPC):**
- [ ] Used for request/response scenarios
- [ ] Timeouts configured appropriately
- [ ] Circuit breakers implemented
- [ ] Retries with exponential backoff
- [ ] Service discovery mechanism defined
- [ ] Load balancing strategy clear
- [ ] API gateway for external access

**Asynchronous Communication (Message Queues, Event Streaming):**
- [ ] Used for fire-and-forget scenarios
- [ ] Message schemas versioned
- [ ] Idempotency handled (duplicate message processing)
- [ ] Message ordering guarantees understood
- [ ] Dead letter queues for failed messages
- [ ] Message TTL defined
- [ ] Poison message handling

**Event-Driven Architecture:**
- [ ] Events represent domain occurrences (past tense: `OrderCreated`, `UserRegistered`)
- [ ] Event schemas well-defined and versioned
- [ ] Event sourcing used appropriately (if applicable)
- [ ] CQRS pattern for read/write separation (if applicable)
- [ ] Eventual consistency handled gracefully
- [ ] Event replay capability for recovery
- [ ] Event ordering and causality maintained

**Data Management:**

**Database-per-Service Pattern:**
- [ ] Each service has its own database
- [ ] Services don't access other services' databases directly
- [ ] Data duplication justified and managed
- [ ] Eventual consistency strategy defined
- [ ] Data synchronization mechanisms clear

**Distributed Transactions:**
- [ ] Two-phase commit avoided (performance, complexity)
- [ ] Saga pattern used for distributed transactions
- [ ] Saga orchestration or choreography chosen
- [ ] Compensation logic defined for rollbacks
- [ ] Saga timeout and failure handling defined

**Saga Patterns:**

**Orchestration (centralized):**
- Saga orchestrator coordinates transaction steps
- Simpler to understand and debug
- Single point of failure (orchestrator)

**Choreography (decentralized):**
- Services react to events and publish new events
- More resilient (no single point of failure)
- Complex to understand and debug

**Review Checklist:**
- [ ] Saga pattern chosen appropriately
- [ ] Compensation actions defined for each step
- [ ] Saga timeout strategy defined
- [ ] Saga state persisted for recovery
- [ ] Idempotency ensured for saga steps

**Resilience Patterns:**

**Circuit Breaker:**
- [ ] Circuit breaker configured for external service calls
- [ ] Failure threshold defined
- [ ] Timeout configured
- [ ] Half-open state for recovery testing
- [ ] Fallback behavior defined

**Retry Logic:**
- [ ] Retries implemented with exponential backoff
- [ ] Maximum retry attempts defined
- [ ] Idempotency ensured (retries safe)
- [ ] Jitter added to prevent thundering herd
- [ ] Retries only for transient failures (not business errors)

**Timeouts:**
- [ ] Timeouts configured for all external calls
- [ ] Timeout values appropriate (not too short, not too long)
- [ ] Cascading timeouts prevented (child timeout < parent timeout)
- [ ] Timeout errors handled gracefully

**Bulkhead:**
- [ ] Thread pools isolated for different service calls
- [ ] Connection pools configured per dependency
- [ ] Resource exhaustion prevented
- [ ] Bulkhead limits configured appropriately

**Service Discovery:**
- [ ] Service registry mechanism defined (Consul, Eureka, etcd, Kubernetes)
- [ ] Health check endpoints implemented
- [ ] Service registration automatic
- [ ] Service deregistration on shutdown
- [ ] Load balancing strategy (round-robin, least connections, sticky sessions)
- [ ] Client-side vs. server-side discovery chosen

### Phase 5: Integration Architecture Review

**Review Areas:**

**Integration Patterns:**

**API Integration:**
- [ ] REST API integration for synchronous communication
- [ ] Error handling comprehensive (network errors, API errors, timeouts)
- [ ] Retry logic with exponential backoff
- [ ] Circuit breaker for failing APIs
- [ ] API versioning handled (version pinning, migration strategy)
- [ ] Authentication/authorization configured
- [ ] Rate limiting respected
- [ ] Webhooks for event notifications (if applicable)

**Message Queue Integration:**
- [ ] Message queue chosen appropriately (RabbitMQ, AWS SQS, Azure Service Bus)
- [ ] Message schemas defined and versioned
- [ ] Queue naming conventions followed
- [ ] Message durability configured (persistent messages)
- [ ] Dead letter queue configured
- [ ] Message TTL defined
- [ ] Poison message handling implemented
- [ ] Idempotent message processing

**Example Message Schema:**
```json
{
  "eventType": "order.created",
  "eventVersion": "1.0",
  "eventId": "uuid",
  "timestamp": "ISO 8601",
  "payload": {
    "orderId": "string",
    "userId": "string",
    "totalAmount": "decimal"
  },
  "metadata": {
    "correlationId": "uuid",
    "causationId": "uuid"
  }
}
```

**Event Streaming:**
- [ ] Event streaming platform chosen (Kafka, AWS Kinesis, Azure Event Hubs)
- [ ] Event schemas defined with schema registry
- [ ] Topic naming conventions followed
- [ ] Partitioning strategy defined (for parallelism and ordering)
- [ ] Consumer groups configured
- [ ] Offset management strategy (commit strategy)
- [ ] Event retention policy defined
- [ ] Stream processing framework chosen (Kafka Streams, Flink, Spark)

**Batch Processing:**
- [ ] Batch job scheduling defined (cron, scheduled tasks)
- [ ] ETL pipelines designed with proper error handling
- [ ] Incremental processing strategy (delta loads)
- [ ] Batch job monitoring and alerting
- [ ] Failed job retry and recovery
- [ ] Batch job idempotency
- [ ] Data validation in pipelines

**Third-Party API Integration:**
- [ ] API documentation reviewed
- [ ] Rate limits understood and respected
- [ ] API key management (secrets manager)
- [ ] Retry logic for transient failures
- [ ] Circuit breaker for availability issues
- [ ] Fallback behavior for API failures
- [ ] API versioning and deprecation strategy
- [ ] Webhook signature validation (if applicable)
- [ ] API cost monitoring (for paid APIs)

### Phase 6: Security Architecture Review

**Review Areas:**

**Authentication Design:**

**Authentication Mechanisms:**
- [ ] Mechanism chosen appropriately (JWT, OAuth 2.0, SAML, API keys)
- [ ] Password hashing with strong algorithm (bcrypt, Argon2, PBKDF2)
- [ ] Password requirements defined (length, complexity, history)
- [ ] Multi-factor authentication (MFA) supported for sensitive operations
- [ ] Session management secure (httpOnly, secure, sameSite cookies)
- [ ] Token expiration appropriate (access tokens short-lived, refresh tokens long-lived)
- [ ] Token revocation mechanism defined
- [ ] Account lockout after failed attempts

**JWT Design:**
- [ ] JWT signature algorithm strong (RS256, ES256, not HS256 with weak secret)
- [ ] JWT claims appropriate (sub, iss, aud, exp, iat)
- [ ] JWT expiration short (15 minutes for access tokens)
- [ ] Refresh token mechanism for token renewal
- [ ] JWT stored securely (not in localStorage for XSS risk)
- [ ] JWT validated on every request
- [ ] JWT blacklisting for revocation (if needed)

**OAuth 2.0 Design:**
- [ ] Grant type appropriate (authorization code, client credentials)
- [ ] PKCE used for public clients (mobile, SPA)
- [ ] Redirect URI validated strictly
- [ ] State parameter for CSRF protection
- [ ] Scope-based authorization
- [ ] Token endpoint authenticated
- [ ] Refresh token rotation for security

**Authorization Design:**

**Authorization Models:**
- [ ] Model chosen appropriately (RBAC, ABAC, ACL)
- [ ] Roles/permissions well-defined
- [ ] Principle of least privilege followed
- [ ] Authorization checks at API gateway and service level
- [ ] Fine-grained permissions for sensitive operations
- [ ] Permission inheritance strategy clear
- [ ] Permission caching for performance (with invalidation)

**RBAC (Role-Based Access Control):**
- [ ] Roles represent job functions (Admin, Editor, Viewer)
- [ ] Permissions assigned to roles, not users directly
- [ ] User-role assignments dynamic
- [ ] Role hierarchy supported (if needed)
- [ ] Default role for new users

**ABAC (Attribute-Based Access Control):**
- [ ] Attributes defined (user attributes, resource attributes, environment attributes)
- [ ] Policies express fine-grained rules
- [ ] Policy evaluation efficient
- [ ] Policy language expressive (XACML, OPA Rego)

**Data Protection:**

**Encryption at Rest:**
- [ ] Sensitive data encrypted in database (field-level or database-level)
- [ ] Encryption keys managed securely (KMS, HSM)
- [ ] Key rotation policy defined
- [ ] Encryption algorithm strong (AES-256)

**Encryption in Transit:**
- [ ] TLS/SSL for all external communication
- [ ] TLS version appropriate (TLS 1.2 minimum, TLS 1.3 preferred)
- [ ] Certificate management automated (Let's Encrypt, cert-manager)
- [ ] mTLS for service-to-service communication (if needed)

**Secrets Management:**
- [ ] Secrets not hardcoded in source code
- [ ] Secrets stored in secrets manager (AWS Secrets Manager, HashiCorp Vault, Azure Key Vault)
- [ ] Secrets rotated regularly
- [ ] Secrets access audited
- [ ] Least privilege access to secrets

**Sensitive Data Handling:**
- [ ] PII (Personally Identifiable Information) identified
- [ ] PII access logged and audited
- [ ] Data masking for non-production environments
- [ ] Data retention policy defined
- [ ] Data deletion mechanism (GDPR right to be forgotten)
- [ ] Sensitive data not logged (passwords, credit cards, SSNs)

**API Security:**

**Input Validation:**
- [ ] All inputs validated (type, format, range, length)
- [ ] Whitelist validation preferred over blacklist
- [ ] Parameterized queries for SQL (prevent SQL injection)
- [ ] Output encoding for HTML (prevent XSS)
- [ ] File upload validation (type, size, content)
- [ ] JSON schema validation for API requests

**Common Vulnerabilities Prevention:**
- [ ] SQL Injection: Use ORMs or parameterized queries
- [ ] XSS (Cross-Site Scripting): Output encoding, CSP headers
- [ ] CSRF (Cross-Site Request Forgery): CSRF tokens, SameSite cookies
- [ ] SSRF (Server-Side Request Forgery): Validate and sanitize URLs
- [ ] XXE (XML External Entity): Disable external entity processing
- [ ] Path Traversal: Validate and sanitize file paths
- [ ] Command Injection: Avoid shell execution, sanitize inputs

**Rate Limiting:**
- [ ] Rate limiting per user, per IP, per endpoint
- [ ] Rate limit thresholds appropriate (100 req/min per user typical)
- [ ] Rate limit headers included (X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset)
- [ ] Rate limit algorithm appropriate (token bucket, leaky bucket, sliding window)
- [ ] DDoS protection at infrastructure level

**Security Headers:**
- [ ] Content-Security-Policy (CSP) for XSS prevention
- [ ] X-Content-Type-Options: nosniff
- [ ] X-Frame-Options: DENY or SAMEORIGIN
- [ ] Strict-Transport-Security (HSTS)
- [ ] X-XSS-Protection: 1; mode=block (legacy browsers)

**Security Monitoring:**

**Audit Logging:**
- [ ] Security events logged (authentication, authorization, data access)
- [ ] Logs include user ID, timestamp, action, resource, outcome
- [ ] Logs immutable (append-only)
- [ ] Logs centralized (ELK, Splunk, CloudWatch)
- [ ] Logs retained per compliance requirements
- [ ] Log analysis automated (alerts for anomalies)

**Security Event Monitoring:**
- [ ] Failed login attempts monitored and alerted
- [ ] Privilege escalation attempts detected
- [ ] Unusual data access patterns detected
- [ ] API abuse detected (rate limit violations)
- [ ] Security alerts routed to security team

### Phase 7: Performance & Scalability Review

**Review Areas:**

**Performance Requirements:**
- [ ] Response time targets defined (p50, p95, p99)
- [ ] Throughput targets defined (requests per second)
- [ ] Concurrent user targets defined
- [ ] Data volume targets defined (records, storage)
- [ ] Performance tested under load
- [ ] Performance budgets defined

**Caching Strategy:**
- [ ] Caching layers defined (CDN, API gateway, application, database)
- [ ] Cache-aside pattern for reads
- [ ] Write-through or write-behind for writes
- [ ] Cache invalidation strategy (TTL, event-based)
- [ ] Cache warming for critical data
- [ ] Cache key design (avoid cache key conflicts)
- [ ] Distributed cache for multi-instance deployments

**Database Performance:**
- [ ] Indexes on frequently queried columns
- [ ] Query optimization (EXPLAIN plans analyzed)
- [ ] Connection pooling configured
- [ ] Database read replicas for read-heavy workloads
- [ ] Database sharding for horizontal scaling
- [ ] Slow query logging enabled and monitored
- [ ] Database vacuum/optimization scheduled

**API Performance:**
- [ ] Response payloads minimal (only necessary data)
- [ ] Pagination for large datasets
- [ ] Compression enabled (gzip, brotli)
- [ ] CDN for static assets
- [ ] GraphQL query complexity limits
- [ ] API response caching (ETag, Cache-Control headers)
- [ ] Rate limiting prevents abuse

**Asynchronous Processing:**
- [ ] Long-running tasks offloaded to background jobs
- [ ] Message queues for async processing
- [ ] Job status tracking for users
- [ ] Job retry logic for failures
- [ ] Job prioritization for critical tasks

**Scalability Strategy:**

**Horizontal Scaling:**
- [ ] Stateless application design (scale by adding instances)
- [ ] Load balancer distributes traffic
- [ ] Auto-scaling policies defined (CPU, memory, request count)
- [ ] Session state externalized (Redis, database)
- [ ] File uploads to object storage (S3, Azure Blob)

**Vertical Scaling:**
- [ ] Vertical scaling limits identified (maximum instance size)
- [ ] Cost-effectiveness compared to horizontal scaling
- [ ] Downtime required for vertical scaling

**Database Scaling:**
- [ ] Read replicas for read scaling
- [ ] Sharding for write scaling and data partitioning
- [ ] Database connection pooling
- [ ] Caching layer to reduce database load

### Phase 8: Reporting & Recommendations

**Activities:**
1. **Consolidate Findings**
   - Categorize issues by severity and area
   - Document each finding with examples and evidence
   - Provide specific locations in design documents
   - Estimate effort required to address

2. **Prioritize Recommendations**
   - Critical issues must be fixed before implementation
   - High-priority issues should be fixed soon
   - Medium issues addressed in next iteration
   - Low issues tracked for future improvements

3. **Create Action Items**
   - Assign ownership for each issue
   - Set realistic timelines for fixes
   - Track progress on recommendations
   - Schedule follow-up review
