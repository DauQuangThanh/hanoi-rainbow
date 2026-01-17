# API Design Best Practices Reference

Comprehensive guide for designing robust, scalable, and maintainable APIs.

## REST API Design Principles

### 1. Resource Naming

**Use Nouns, Not Verbs**
```
✅ Good:
GET    /users
POST   /users
GET    /users/123
PUT    /users/123
DELETE /users/123

❌ Bad:
GET    /getUsers
POST   /createUser
GET    /getUserById/123
POST   /updateUser/123
DELETE /removeUser/123
```

**Use Plural Nouns**
```
✅ Good: /users, /products, /orders
❌ Bad: /user, /product, /order
```

**Use Hyphens for Multi-Word Resources**
```
✅ Good: /order-items, /user-preferences
❌ Bad: /orderItems, /order_items, /OrderItems
```

**Avoid Deep Nesting**
```
✅ Good:
GET /users/123/posts
GET /posts?user_id=123

❌ Bad:
GET /users/123/posts/456/comments/789/likes
```

---

### 2. HTTP Methods

**Standard CRUD Operations**
```
POST   /resources         - Create new resource
GET    /resources         - List resources
GET    /resources/{id}    - Get single resource
PUT    /resources/{id}    - Replace resource (full update)
PATCH  /resources/{id}    - Update resource (partial update)
DELETE /resources/{id}    - Delete resource
```

**Idempotency**
- GET, PUT, DELETE are idempotent (same result on multiple calls)
- POST is not idempotent (creates new resource each time)
- PATCH may or may not be idempotent

**Safe Methods**
- GET, HEAD, OPTIONS are safe (read-only, no side effects)

---

### 3. HTTP Status Codes

**Success Codes (2xx)**
```
200 OK              - Successful GET, PUT, PATCH, or DELETE
201 Created         - Successful POST that creates a resource
202 Accepted        - Request accepted for async processing
204 No Content      - Successful request with no response body (DELETE)
206 Partial Content - Partial GET (range requests)
```

**Redirection (3xx)**
```
301 Moved Permanently   - Resource permanently moved
302 Found              - Temporary redirect
304 Not Modified       - Cached version still valid
```

**Client Errors (4xx)**
```
400 Bad Request              - Invalid request syntax
401 Unauthorized             - Authentication required/failed
403 Forbidden                - Authenticated but not authorized
404 Not Found                - Resource doesn't exist
405 Method Not Allowed       - HTTP method not supported
406 Not Acceptable           - Can't produce requested format
409 Conflict                 - Resource conflict (duplicate, version)
410 Gone                     - Resource permanently deleted
415 Unsupported Media Type   - Invalid Content-Type
422 Unprocessable Entity     - Validation errors
429 Too Many Requests        - Rate limit exceeded
```

**Server Errors (5xx)**
```
500 Internal Server Error - Generic server error
502 Bad Gateway          - Invalid response from upstream
503 Service Unavailable  - Server temporarily unavailable
504 Gateway Timeout      - Upstream server timeout
```

---

### 4. Request/Response Format

**Consistent JSON Structure**
```json
// Single resource
{
  "id": "usr_123",
  "email": "user@example.com",
  "name": "John Doe",
  "created_at": "2026-01-14T10:00:00Z"
}

// Collection
{
  "data": [
    { "id": "usr_123", "name": "John" },
    { "id": "usr_456", "name": "Jane" }
  ],
  "pagination": {
    "page": 1,
    "limit": 20,
    "total": 150,
    "pages": 8
  }
}

// Error response
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Validation failed",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format",
        "code": "INVALID_FORMAT"
      }
    ]
  }
}
```

**Content Negotiation**
```
Request:
Accept: application/json
Content-Type: application/json

Response:
Content-Type: application/json; charset=utf-8
```

---

### 5. Versioning Strategies

**URL Path Versioning (Recommended)**
```
/api/v1/users
/api/v2/users
```
✅ Pros: Clear, easy to route, cacheable
❌ Cons: Multiple base URLs

**Header Versioning**
```
GET /api/users
Accept: application/vnd.myapi.v1+json
```
✅ Pros: Clean URLs, RESTful
❌ Cons: Less visible, harder to test

**Query Parameter Versioning**
```
/api/users?version=1
```
❌ Not recommended: Caching issues, unclear

**When to Version**
- Breaking changes (removed fields, changed data types)
- Changed business logic
- Security updates requiring client changes

**When NOT to Version**
- Adding new optional fields
- Adding new endpoints
- Bug fixes
- Performance improvements

---

### 6. Filtering, Sorting, Pagination

**Filtering**
```
GET /users?status=active
GET /users?role=admin&status=active
GET /users?created_after=2026-01-01
GET /products?min_price=10&max_price=100
```

**Sorting**
```
GET /users?sort=created_at        // Ascending (default)
GET /users?sort=-created_at       // Descending
GET /users?sort=name,-created_at  // Multiple fields
```

**Pagination - Offset/Limit**
```
GET /users?page=2&limit=20
GET /users?offset=40&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 150,
    "pages": 8
  },
  "links": {
    "first": "/users?page=1&limit=20",
    "prev": "/users?page=1&limit=20",
    "self": "/users?page=2&limit=20",
    "next": "/users?page=3&limit=20",
    "last": "/users?page=8&limit=20"
  }
}
```

**Pagination - Cursor-Based**
```
GET /users?cursor=eyJpZCI6MTIzfQ&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "next_cursor": "eyJpZCI6MTQzfQ",
    "has_more": true
  }
}
```
✅ Better for large datasets, consistent results

**Sparse Fieldsets**
```
GET /users?fields=id,email,name
```

**Search**
```
GET /products?q=laptop
GET /users?search=john
```

---

### 7. HATEOAS (Hypermedia)

**Include Related Links**
```json
{
  "id": "usr_123",
  "name": "John Doe",
  "email": "john@example.com",
  "_links": {
    "self": { "href": "/users/usr_123" },
    "posts": { "href": "/users/usr_123/posts" },
    "avatar": { "href": "/users/usr_123/avatar" }
  }
}
```

---

### 8. Rate Limiting

**Response Headers**
```
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 950
X-RateLimit-Reset: 1705228800
Retry-After: 60
```

**429 Response**
```json
{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Too many requests. Please try again in 60 seconds.",
    "retry_after": 60
  }
}
```

**Rate Limiting Strategies**
- Fixed window: 1000 requests per hour
- Sliding window: More accurate, complex
- Token bucket: Burst handling
- Leaky bucket: Smooth rate

---

### 9. Authentication & Security

**Bearer Token (JWT)**
```
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**API Key**
```
X-API-Key: your-api-key-here
```

**Basic Auth (Avoid for Production)**
```
Authorization: Basic base64(username:password)
```

**Security Headers**
```
Strict-Transport-Security: max-age=31536000
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
Content-Security-Policy: default-src 'self'
```

---

### 10. Caching

**Cache-Control Header**
```
Cache-Control: public, max-age=3600          // 1 hour
Cache-Control: private, max-age=300          // 5 minutes, user-specific
Cache-Control: no-cache                      // Validate before use
Cache-Control: no-store                      // Don't cache
```

**ETag (Entity Tag)**
```
Response:
ETag: "33a64df551425fcc55e4d42a148795d9f25f89d4"

Request:
If-None-Match: "33a64df551425fcc55e4d42a148795d9f25f89d4"

Response:
304 Not Modified (if unchanged)
```

**Last-Modified**
```
Response:
Last-Modified: Wed, 14 Jan 2026 10:00:00 GMT

Request:
If-Modified-Since: Wed, 14 Jan 2026 10:00:00 GMT

Response:
304 Not Modified (if unchanged)
```

---

### 11. Bulk Operations

**Batch Create**
```
POST /users/batch
[
  { "email": "user1@example.com", "name": "User 1" },
  { "email": "user2@example.com", "name": "User 2" }
]

Response: 207 Multi-Status
{
  "results": [
    { "status": 201, "id": "usr_123" },
    { "status": 409, "error": "Email already exists" }
  ]
}
```

**Batch Update**
```
PATCH /users/batch
[
  { "id": "usr_123", "name": "Updated Name" },
  { "id": "usr_456", "status": "active" }
]
```

**Batch Delete**
```
DELETE /users/batch
{ "ids": ["usr_123", "usr_456", "usr_789"] }
```

---

### 12. Async Operations

**Accepted for Processing**
```
POST /reports/generate
{
  "type": "sales",
  "date_range": "2026-01"
}

Response: 202 Accepted
{
  "job_id": "job_123",
  "status": "pending",
  "status_url": "/jobs/job_123"
}
```

**Check Status**
```
GET /jobs/job_123

Response: 200 OK
{
  "id": "job_123",
  "status": "completed",
  "result_url": "/reports/rep_123"
}
```

---

### 13. Error Handling

**Consistent Error Format**
```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "details": [...],
    "request_id": "req_123",
    "timestamp": "2026-01-14T10:00:00Z"
  }
}
```

**Validation Errors**
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Validation failed",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format",
        "code": "INVALID_FORMAT"
      },
      {
        "field": "password",
        "message": "Password must be at least 8 characters",
        "code": "TOO_SHORT"
      }
    ]
  }
}
```

---

### 14. Documentation (OpenAPI 3.0)

```yaml
openapi: 3.0.0
info:
  title: User API
  version: 1.0.0
  description: API for managing users

paths:
  /users:
    get:
      summary: List users
      parameters:
        - name: page
          in: query
          schema:
            type: integer
            default: 1
        - name: limit
          in: query
          schema:
            type: integer
            default: 20
      responses:
        '200':
          description: Successful response
          content:
            application/json:
              schema:
                type: object
                properties:
                  data:
                    type: array
                    items:
                      $ref: '#/components/schemas/User'
    
    post:
      summary: Create user
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/UserInput'
      responses:
        '201':
          description: User created
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'

components:
  schemas:
    User:
      type: object
      properties:
        id:
          type: string
        email:
          type: string
          format: email
        name:
          type: string
        created_at:
          type: string
          format: date-time
```

---

## GraphQL API Design

### Schema Definition
```graphql
type User {
  id: ID!
  email: String!
  name: String!
  posts: [Post!]!
  createdAt: DateTime!
}

type Post {
  id: ID!
  title: String!
  content: String!
  author: User!
  comments: [Comment!]!
  publishedAt: DateTime
}

type Query {
  user(id: ID!): User
  users(limit: Int, offset: Int): [User!]!
  post(id: ID!): Post
  posts(filter: PostFilter): [Post!]!
}

type Mutation {
  createUser(input: CreateUserInput!): User!
  updateUser(id: ID!, input: UpdateUserInput!): User!
  deleteUser(id: ID!): Boolean!
}

input CreateUserInput {
  email: String!
  name: String!
  password: String!
}

input PostFilter {
  status: PostStatus
  authorId: ID
}

enum PostStatus {
  DRAFT
  PUBLISHED
  ARCHIVED
}
```

### Queries
```graphql
# Get user with nested data
query {
  user(id: "usr_123") {
    id
    name
    email
    posts(limit: 5) {
      id
      title
      comments {
        id
        content
      }
    }
  }
}

# Pagination
query {
  users(limit: 20, offset: 40) {
    id
    name
  }
}
```

### Mutations
```graphql
mutation {
  createUser(input: {
    email: "user@example.com"
    name: "John Doe"
    password: "password123"
  }) {
    id
    email
    name
  }
}
```

---

## gRPC API Design

### Protocol Buffers Definition
```protobuf
syntax = "proto3";

package user.v1;

service UserService {
  rpc GetUser(GetUserRequest) returns (User);
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);
  rpc CreateUser(CreateUserRequest) returns (User);
  rpc UpdateUser(UpdateUserRequest) returns (User);
  rpc DeleteUser(DeleteUserRequest) returns (google.protobuf.Empty);
}

message User {
  string id = 1;
  string email = 2;
  string name = 3;
  google.protobuf.Timestamp created_at = 4;
}

message GetUserRequest {
  string id = 1;
}

message ListUsersRequest {
  int32 page = 1;
  int32 limit = 2;
}

message ListUsersResponse {
  repeated User users = 1;
  int32 total = 2;
}

message CreateUserRequest {
  string email = 1;
  string name = 2;
  string password = 3;
}
```

---

## API Design Checklist

- [ ] Consistent naming conventions (nouns, plural, hyphens)
- [ ] Proper HTTP methods and status codes
- [ ] Versioning strategy implemented
- [ ] Pagination for collections
- [ ] Filtering and sorting support
- [ ] Error handling with consistent format
- [ ] Authentication and authorization
- [ ] Rate limiting
- [ ] Caching headers
- [ ] CORS configuration
- [ ] OpenAPI/Swagger documentation
- [ ] Request/response validation
- [ ] Idempotency for non-GET requests
- [ ] Bulk operation support (if needed)
- [ ] Async operation handling (if needed)
- [ ] Monitoring and logging
- [ ] Security headers
- [ ] HTTPS only

This reference provides comprehensive guidelines for building robust, scalable APIs.
