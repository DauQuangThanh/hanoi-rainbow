# Severity Levels


### 🔴 Critical
- **Definition**: Issues that pose security risks, data loss, or broken core functionality
- **Examples**:
  - SQL injection vulnerability
  - Missing authentication on sensitive endpoints
  - Data loss risk from missing foreign key constraints
  - No input validation on critical APIs
- **Action Required**: Must be fixed before implementation

### 🟠 High
- **Definition**: Significant design flaws affecting scalability, performance, or reliability
- **Examples**:
  - N+1 query problems causing performance issues
  - Missing indexes on frequently queried columns
  - Tight coupling between microservices
  - No circuit breaker for external service calls
- **Action Required**: Should be fixed before go-live

### 🟡 Medium
- **Definition**: Moderate issues or deviations from best practices
- **Examples**:
  - Inconsistent API naming conventions
  - Suboptimal denormalization strategy
  - Missing API documentation for some endpoints
  - Inefficient caching strategy
- **Action Required**: Address in next iteration

### 🟢 Low
- **Definition**: Minor improvements or optimization opportunities
- **Examples**:
  - Additional API endpoints for convenience
  - Enhanced error messages
  - Additional indexes for edge case queries
  - Documentation improvements
- **Action Required**: Track for future improvements
