# Architecture Review Checklists

Comprehensive checklists for reviewing different aspects of software architecture.

## Table of Contents
1. [General Architecture Review](#general-architecture-review)
2. [Microservices Review](#microservices-review)
3. [Cloud Architecture Review](#cloud-architecture-review)
4. [Security Architecture Review](#security-architecture-review)
5. [Data Architecture Review](#data-architecture-review)
6. [Performance Review](#performance-review)
7. [API Design Review](#api-design-review)

---

## General Architecture Review

### Architecture Documentation
- [ ] System context diagram available (C4 Level 1)
- [ ] Container diagram showing components (C4 Level 2)
- [ ] Component diagrams for complex containers (C4 Level 3)
- [ ] Sequence diagrams for key scenarios
- [ ] Deployment diagram with infrastructure
- [ ] Data flow diagrams
- [ ] Architecture Decision Records (ADRs) maintained
- [ ] Technical specifications documented
- [ ] API contracts defined (OpenAPI/Swagger)
- [ ] README files comprehensive

### Requirements Alignment
- [ ] Functional requirements addressed
- [ ] Non-functional requirements defined
- [ ] Performance requirements specified (response time, throughput)
- [ ] Scalability requirements documented (concurrent users, data volume)
- [ ] Availability targets defined (SLA, uptime)
- [ ] Security requirements identified
- [ ] Compliance requirements listed
- [ ] Budget constraints considered
- [ ] Timeline constraints factored in
- [ ] Team skills assessed

### Architecture Style & Patterns
- [ ] Architecture style clearly identified (monolithic, microservices, serverless, etc.)
- [ ] Architecture pattern appropriate for requirements
- [ ] Pattern advantages leveraged
- [ ] Pattern trade-offs acknowledged
- [ ] Alternative patterns evaluated
- [ ] Hybrid patterns justified if used
- [ ] Anti-patterns avoided
- [ ] Design patterns documented

### System Decomposition
- [ ] Components organized logically
- [ ] Component boundaries clear
- [ ] Component responsibilities defined
- [ ] Component size appropriate
- [ ] Component coupling minimized
- [ ] Component cohesion maximized
- [ ] Reusability considered
- [ ] Dependency direction appropriate

### Design Principles
- [ ] SOLID principles followed
- [ ] Separation of concerns applied
- [ ] DRY principle maintained
- [ ] KISS principle followed
- [ ] YAGNI principle applied
- [ ] Fail-fast approach used
- [ ] Defensive programming practiced
- [ ] Single point of failure avoided

### Technology Stack
- [ ] Technology choices justified
- [ ] Technology maturity assessed
- [ ] Team expertise considered
- [ ] Community support available
- [ ] Long-term viability evaluated
- [ ] Licensing issues reviewed
- [ ] Vendor lock-in risks assessed
- [ ] Migration path considered

### Integration & Communication
- [ ] Integration patterns defined
- [ ] Communication protocols specified
- [ ] Synchronous vs asynchronous justified
- [ ] API contracts documented
- [ ] Error handling standardized
- [ ] Retry logic defined
- [ ] Circuit breakers implemented
- [ ] Timeouts configured

### Data Management
- [ ] Data ownership clear
- [ ] Data models documented
- [ ] Database selection justified
- [ ] Data consistency model defined
- [ ] Caching strategy specified
- [ ] Data migration planned
- [ ] Backup strategy defined
- [ ] Data retention policies set

### Quality Attributes
- [ ] Scalability addressed
- [ ] Performance optimized
- [ ] Security designed in
- [ ] Reliability ensured
- [ ] Availability planned
- [ ] Maintainability considered
- [ ] Testability designed
- [ ] Observability included

### Deployment & Operations
- [ ] Deployment strategy defined
- [ ] CI/CD pipeline configured
- [ ] Infrastructure as Code used
- [ ] Monitoring implemented
- [ ] Logging centralized
- [ ] Alerting configured
- [ ] Disaster recovery planned
- [ ] Runbooks created

---

## Microservices Review

### Service Design
- [ ] Services aligned with business domains (DDD)
- [ ] Services independently deployable
- [ ] Services independently scalable
- [ ] Service size appropriate (not too large/small)
- [ ] Service responsibilities clear
- [ ] Service boundaries well-defined
- [ ] Shared libraries minimized
- [ ] Service versioning strategy defined

### Service Communication
- [ ] API Gateway implemented
- [ ] Service discovery mechanism configured
- [ ] Load balancing setup
- [ ] Communication patterns defined (sync/async)
- [ ] Inter-service authentication secured
- [ ] Circuit breakers implemented
- [ ] Retry policies defined
- [ ] Timeout strategies configured
- [ ] Bulkhead pattern for isolation

### Data Management
- [ ] Database per service enforced
- [ ] No shared database tables
- [ ] Data consistency strategy defined (eventual vs strong)
- [ ] Saga pattern for distributed transactions
- [ ] Event sourcing considered
- [ ] CQRS applied where appropriate
- [ ] Data duplication justified
- [ ] Data synchronization strategy

### Service Dependencies
- [ ] Service dependencies minimized
- [ ] Circular dependencies avoided
- [ ] Dependency graph documented
- [ ] Cascading failures prevented
- [ ] Service mesh considered (Istio, Linkerd)
- [ ] Backward compatibility maintained
- [ ] API versioning strategy

### Resilience Patterns
- [ ] Circuit breaker for external calls
- [ ] Retry with exponential backoff
- [ ] Fallback mechanisms defined
- [ ] Bulkhead for resource isolation
- [ ] Health checks implemented
- [ ] Graceful degradation designed
- [ ] Chaos engineering considered

### Observability
- [ ] Distributed tracing (Jaeger, Zipkin)
- [ ] Centralized logging (ELK, Splunk)
- [ ] Metrics collection (Prometheus, Grafana)
- [ ] Correlation IDs for requests
- [ ] Service mesh telemetry
- [ ] APM tools integrated
- [ ] Alerting rules defined

### Deployment
- [ ] Container images optimized
- [ ] Kubernetes/orchestration configured
- [ ] Service deployment independent
- [ ] Blue-green or canary deployments
- [ ] Rollback procedures defined
- [ ] Configuration externalized
- [ ] Secrets management (Vault, KMS)

### Testing Strategy
- [ ] Unit tests for each service
- [ ] Integration tests defined
- [ ] Contract testing between services
- [ ] End-to-end tests automated
- [ ] Performance testing planned
- [ ] Chaos testing considered

---

## Cloud Architecture Review

### Cloud Provider Selection
- [ ] Cloud provider choice justified (AWS, Azure, GCP)
- [ ] Multi-cloud strategy considered
- [ ] Vendor lock-in risks assessed
- [ ] Exit strategy defined
- [ ] Cost analysis performed
- [ ] Regional presence adequate
- [ ] Compliance requirements met

### Compute Resources
- [ ] Compute type appropriate (VMs, containers, serverless)
- [ ] Auto-scaling configured
- [ ] Right-sizing performed
- [ ] Reserved instances considered
- [ ] Spot instances where appropriate
- [ ] Resource limits defined
- [ ] Cost optimization strategies

### Networking
- [ ] VPC/VNet architecture designed
- [ ] Subnet segmentation appropriate
- [ ] Network security groups configured
- [ ] Private vs public subnets defined
- [ ] NAT gateway for private resources
- [ ] VPN or Direct Connect for on-premise
- [ ] Load balancer configured
- [ ] CDN for static content

### Storage
- [ ] Storage type appropriate (block, file, object)
- [ ] Storage class optimized (hot, cool, archive)
- [ ] Backup strategy automated
- [ ] Replication configured
- [ ] Encryption at rest enabled
- [ ] Lifecycle policies defined
- [ ] Cost optimization applied

### Managed Services
- [ ] Managed databases vs self-hosted justified
- [ ] Managed Kubernetes vs self-managed
- [ ] Managed message queues used
- [ ] Managed caching services
- [ ] Managed monitoring services
- [ ] Build vs buy decisions documented

### High Availability
- [ ] Multi-AZ deployment
- [ ] Multi-region considered for critical workloads
- [ ] Availability zones utilized
- [ ] Health checks configured
- [ ] Automatic failover setup
- [ ] Load balancing across zones

### Disaster Recovery
- [ ] DR strategy defined (backup, pilot light, warm standby, hot site)
- [ ] RTO (Recovery Time Objective) defined
- [ ] RPO (Recovery Point Objective) defined
- [ ] Backup frequency appropriate
- [ ] Cross-region backup replication
- [ ] DR testing scheduled
- [ ] Runbooks for DR scenarios

### Security
- [ ] IAM roles and policies defined
- [ ] Least privilege principle applied
- [ ] MFA enabled for admin access
- [ ] Security groups restrictive
- [ ] Encryption in transit (TLS)
- [ ] Encryption at rest enabled
- [ ] Secrets management (AWS Secrets Manager, Azure Key Vault)
- [ ] Compliance standards met (SOC 2, ISO 27001)

### Cost Optimization
- [ ] Resource tagging for cost allocation
- [ ] Cost monitoring and alerts
- [ ] Unused resources identified
- [ ] Right-sizing recommendations
- [ ] Reserved instances for predictable workloads
- [ ] Spot instances for fault-tolerant workloads
- [ ] Cost anomaly detection
- [ ] Budget tracking

### Infrastructure as Code
- [ ] IaC tool selected (Terraform, CloudFormation, Pulumi)
- [ ] All infrastructure codified
- [ ] Version controlled
- [ ] Modular and reusable
- [ ] Environment parity (dev/staging/prod)
- [ ] State management secure
- [ ] CI/CD for infrastructure changes

---

## Security Architecture Review

### Authentication
- [ ] Authentication mechanism appropriate (OAuth 2.0, SAML, OpenID Connect)
- [ ] Multi-factor authentication (MFA) supported
- [ ] Password policies enforced
- [ ] Password hashing strong (bcrypt, Argon2)
- [ ] Account lockout policies
- [ ] Session management secure
- [ ] Token expiration configured
- [ ] Remember me functionality secure

### Authorization
- [ ] Authorization model defined (RBAC, ABAC, policy-based)
- [ ] Least privilege principle applied
- [ ] Role hierarchy designed
- [ ] Permission granularity appropriate
- [ ] Authorization checked on every request
- [ ] Horizontal privilege escalation prevented
- [ ] Vertical privilege escalation prevented

### Data Protection
- [ ] Sensitive data identified (PII, PHI, payment info)
- [ ] Data classification defined
- [ ] Encryption at rest for sensitive data
- [ ] Encryption in transit (TLS 1.2+)
- [ ] Key management strategy (KMS, HSM, Vault)
- [ ] Key rotation policies
- [ ] Data masking for logs and non-prod
- [ ] Secure data disposal procedures

### API Security
- [ ] Authentication required for APIs
- [ ] API keys managed securely
- [ ] OAuth 2.0 for third-party access
- [ ] Rate limiting implemented
- [ ] Input validation on all inputs
- [ ] Output encoding for XSS prevention
- [ ] SQL injection prevention (parameterized queries)
- [ ] API security testing (OWASP API Top 10)

### Network Security
- [ ] Network segmentation (VPC, DMZ)
- [ ] Firewall rules restrictive
- [ ] Security groups least privilege
- [ ] DDoS protection (CloudFlare, AWS Shield)
- [ ] Web Application Firewall (WAF)
- [ ] Intrusion Detection System (IDS)
- [ ] Intrusion Prevention System (IPS)
- [ ] VPN for remote access

### Application Security
- [ ] Input validation comprehensive
- [ ] Output encoding applied
- [ ] CSRF tokens for state-changing operations
- [ ] XSS prevention (Content Security Policy)
- [ ] SQL injection prevention
- [ ] Command injection prevention
- [ ] Path traversal prevention
- [ ] Insecure deserialization prevented
- [ ] OWASP Top 10 addressed

### Secrets Management
- [ ] No secrets in source code
- [ ] No secrets in environment variables
- [ ] Secrets in dedicated vault (HashiCorp Vault, AWS Secrets Manager)
- [ ] Secrets encrypted at rest
- [ ] Access to secrets audited
- [ ] Secret rotation policies
- [ ] Least privilege access to secrets

### Logging & Monitoring
- [ ] Security events logged
- [ ] Authentication attempts logged
- [ ] Authorization failures logged
- [ ] Admin actions logged
- [ ] Logs tamper-proof
- [ ] Log retention policy defined
- [ ] SIEM integration considered
- [ ] Alerting for security events

### Compliance
- [ ] GDPR compliance (if applicable)
- [ ] HIPAA compliance (if applicable)
- [ ] PCI DSS compliance (if applicable)
- [ ] SOC 2 requirements
- [ ] ISO 27001 alignment
- [ ] Data residency requirements met
- [ ] Right to be forgotten implemented
- [ ] Consent management

### Vulnerability Management
- [ ] Dependency scanning automated
- [ ] Container image scanning
- [ ] SAST (Static Application Security Testing)
- [ ] DAST (Dynamic Application Security Testing)
- [ ] Penetration testing scheduled
- [ ] Vulnerability disclosure policy
- [ ] Patch management process

### Incident Response
- [ ] Incident response plan documented
- [ ] Incident classification defined
- [ ] Escalation procedures clear
- [ ] Forensics capabilities
- [ ] Communication plan (internal/external)
- [ ] Post-mortem process
- [ ] Lessons learned captured

---

## Data Architecture Review

### Data Modeling
- [ ] Conceptual data model defined
- [ ] Logical data model documented
- [ ] Physical data model optimized
- [ ] Entity relationships clear
- [ ] Normalization level appropriate
- [ ] Denormalization justified
- [ ] Constraints defined (primary key, foreign key, unique, check)
- [ ] Indexes planned strategically

### Database Selection
- [ ] Relational vs NoSQL justified
- [ ] Database type appropriate (document, key-value, column, graph)
- [ ] ACID vs BASE requirements defined
- [ ] Consistency requirements met
- [ ] Query patterns supported
- [ ] Scalability characteristics adequate
- [ ] Licensing and cost reviewed

### Data Consistency
- [ ] Consistency model defined (strong, eventual, causal)
- [ ] Transaction boundaries clear
- [ ] Isolation levels appropriate
- [ ] Distributed transaction handling (Saga, 2PC)
- [ ] Conflict resolution strategy
- [ ] Data synchronization approach
- [ ] Idempotency for operations

### Data Access Patterns
- [ ] Read/write ratio analyzed
- [ ] Query patterns identified
- [ ] Access patterns optimized
- [ ] CQRS considered for read-heavy
- [ ] Database per service (microservices)
- [ ] Data duplication justified
- [ ] Cache-aside pattern

### Schema Management
- [ ] Schema versioning strategy
- [ ] Schema evolution approach
- [ ] Backward compatibility ensured
- [ ] Forward compatibility considered
- [ ] Migration scripts automated
- [ ] Rollback procedures defined

### Data Migration
- [ ] Migration strategy defined (big bang, trickle, parallel run)
- [ ] Data validation procedures
- [ ] Data cleansing planned
- [ ] Data transformation documented
- [ ] Downtime requirements
- [ ] Rollback plan
- [ ] Post-migration verification

### Data Retention & Archival
- [ ] Data retention policies defined
- [ ] Archival strategy specified
- [ ] Hot vs cold storage
- [ ] Data purging procedures
- [ ] Legal hold capabilities
- [ ] Compliance requirements met
- [ ] Cost optimization

### Data Quality
- [ ] Data validation at entry points
- [ ] Data quality rules defined
- [ ] Data profiling performed
- [ ] Duplicate detection
- [ ] Data cleansing processes
- [ ] Data quality metrics tracked
- [ ] Data governance policies

### Backup & Recovery
- [ ] Backup frequency appropriate
- [ ] Backup retention policy
- [ ] Full vs incremental backups
- [ ] Backup testing scheduled
- [ ] Point-in-time recovery capability
- [ ] Cross-region backup replication
- [ ] Backup encryption
- [ ] RTO and RPO targets met

### Data Security & Privacy
- [ ] Sensitive data identified
- [ ] Data classification defined
- [ ] Encryption at rest
- [ ] Encryption in transit
- [ ] Field-level encryption for sensitive data
- [ ] Data masking for non-prod
- [ ] Anonymization techniques
- [ ] Access controls granular
- [ ] Audit logging for data access
- [ ] GDPR compliance (right to erasure, portability)

---

## Performance Review

### Performance Requirements
- [ ] Response time targets defined (p50, p95, p99)
- [ ] Throughput requirements specified (requests/sec)
- [ ] Concurrent user load defined
- [ ] Data volume projections
- [ ] Growth projections documented
- [ ] Peak load scenarios identified
- [ ] Performance SLAs defined

### Application Performance
- [ ] Database query optimization
- [ ] N+1 query problem avoided
- [ ] Indexes used effectively
- [ ] Connection pooling configured
- [ ] Asynchronous processing for long operations
- [ ] Batch operations where appropriate
- [ ] Lazy loading vs eager loading optimized
- [ ] Pagination for large datasets

### Caching Strategy
- [ ] Cache layers identified (browser, CDN, application, database)
- [ ] Cache-aside pattern used
- [ ] Write-through vs write-behind defined
- [ ] Cache invalidation strategy
- [ ] Cache eviction policies (LRU, LFU)
- [ ] Cache warming strategy
- [ ] Distributed caching for stateless
- [ ] Cache hit ratio monitored

### Database Performance
- [ ] Indexes created on frequently queried columns
- [ ] Composite indexes for multi-column queries
- [ ] Index maintenance scheduled
- [ ] Query execution plans analyzed
- [ ] Slow query logging enabled
- [ ] Database connection pooling
- [ ] Read replicas for read-heavy workloads
- [ ] Database sharding considered for scale
- [ ] Query timeout configured

### API Performance
- [ ] Response size optimized
- [ ] Pagination implemented
- [ ] Field filtering/sparse fieldsets supported (GraphQL)
- [ ] Compression enabled (gzip, brotli)
- [ ] Keep-alive connections
- [ ] HTTP/2 or HTTP/3 considered
- [ ] API response caching
- [ ] Rate limiting to prevent abuse

### Frontend Performance
- [ ] Code splitting and lazy loading
- [ ] Image optimization
- [ ] Minification and bundling
- [ ] CDN for static assets
- [ ] Browser caching headers
- [ ] Service workers for offline
- [ ] Critical CSS inline
- [ ] Font loading optimized
- [ ] Third-party script impact minimized

### Network Performance
- [ ] CDN for global distribution
- [ ] Load balancing configured
- [ ] Keep-alive connections
- [ ] Connection pooling
- [ ] Compression enabled
- [ ] Latency optimized (regional deployment)
- [ ] Network bandwidth adequate

### Monitoring & Profiling
- [ ] APM tool integrated (New Relic, DataDog, Dynatrace)
- [ ] Performance metrics collected (response time, throughput, error rate)
- [ ] Real User Monitoring (RUM)
- [ ] Synthetic monitoring
- [ ] Profiling for bottlenecks
- [ ] Performance regression testing
- [ ] Alerting for performance degradation

### Load Testing
- [ ] Load testing tools selected (JMeter, Gatling, k6)
- [ ] Realistic load scenarios defined
- [ ] Ramp-up testing
- [ ] Soak/endurance testing
- [ ] Spike testing
- [ ] Stress testing for breaking point
- [ ] Load testing in CI/CD

---

## API Design Review

### RESTful Design
- [ ] Resource-based URLs
- [ ] Proper HTTP verbs (GET, POST, PUT, DELETE, PATCH)
- [ ] HTTP status codes appropriate
- [ ] Idempotency for PUT and DELETE
- [ ] Stateless design
- [ ] HATEOAS considered
- [ ] Versioning strategy (URL, header, content negotiation)
- [ ] Consistent naming conventions

### API Documentation
- [ ] OpenAPI/Swagger specification
- [ ] Auto-generated from code
- [ ] Request/response examples
- [ ] Error responses documented
- [ ] Authentication documented
- [ ] Rate limiting documented
- [ ] Changelog maintained
- [ ] Interactive documentation (Swagger UI)

### Request/Response Design
- [ ] Request payload validation
- [ ] Response format consistent (JSON, XML)
- [ ] Pagination for collections (limit/offset or cursor)
- [ ] Filtering supported
- [ ] Sorting supported
- [ ] Field selection (sparse fieldsets)
- [ ] Nested resources appropriately
- [ ] Date/time in ISO 8601 format

### Error Handling
- [ ] Standard error response format
- [ ] Error codes meaningful
- [ ] Error messages clear
- [ ] Detailed errors for 4xx (client errors)
- [ ] Generic errors for 5xx (server errors)
- [ ] Validation errors detailed
- [ ] Correlation ID in responses
- [ ] Stack traces not exposed in production

### Security
- [ ] Authentication required
- [ ] OAuth 2.0 for third-party access
- [ ] API keys for simple use cases
- [ ] Input validation comprehensive
- [ ] SQL injection prevention
- [ ] Rate limiting per user/API key
- [ ] CORS configured properly
- [ ] HTTPS enforced

### Versioning
- [ ] Versioning strategy defined
- [ ] Version in URL (v1, v2) or header
- [ ] Backward compatibility maintained
- [ ] Deprecation policy defined
- [ ] Sunset header for deprecated APIs
- [ ] Migration guide for breaking changes

### Performance
- [ ] Response time acceptable (<200ms for simple queries)
- [ ] Caching headers appropriate
- [ ] Compression enabled
- [ ] Pagination for large collections
- [ ] Asynchronous for long operations (webhooks, polling)
- [ ] Rate limiting to prevent abuse
- [ ] Bulk operations supported

### GraphQL Specific
- [ ] Schema well-designed
- [ ] Query complexity limits
- [ ] Depth limits to prevent abuse
- [ ] N+1 problem solved (DataLoader)
- [ ] Field-level authorization
- [ ] Introspection disabled in production
- [ ] Persisted queries for security

### gRPC Specific
- [ ] Protobuf schemas well-defined
- [ ] Backward compatibility with schema changes
- [ ] Streaming for appropriate use cases
- [ ] Error handling with status codes
- [ ] Metadata for cross-cutting concerns
- [ ] Load balancing configured

This comprehensive set of checklists provides a structured approach to reviewing all aspects of software architecture, ensuring nothing critical is overlooked.
