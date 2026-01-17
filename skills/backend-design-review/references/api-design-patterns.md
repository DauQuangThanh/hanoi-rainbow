# API Design Patterns Reference

This document provides comprehensive guidance on API design patterns, best practices, and common pitfalls for REST, GraphQL, and gRPC APIs.

## Table of Contents

1. [RESTful API Design](#restful-api-design)
2. [GraphQL API Design](#graphql-api-design)
3. [gRPC API Design](#grpc-api-design)
4. [API Versioning Strategies](#api-versioning-strategies)
5. [API Authentication & Authorization](#api-authentication--authorization)
6. [API Documentation](#api-documentation)
7. [API Testing](#api-testing)
8. [Common API Anti-Patterns](#common-api-anti-patterns)

---

## RESTful API Design

### REST Principles

**RE**presentational **S**tate **T**ransfer (REST) is an architectural style with six constraints:

1. **Client-Server**: Separation of concerns
2. **Stateless**: Each request contains all necessary information
3. **Cacheable**: Responses explicitly indicate cacheability
4. **Uniform Interface**: Consistent resource identification and manipulation
5. **Layered System**: Architecture composed of hierarchical layers
6. **Code on Demand** (optional): Server can extend client functionality

### Resource Naming Conventions

**Best Practices:**

✅ **Use Plural Nouns for Collections:**
```
GET    /api/users          (get all users)
POST   /api/users          (create a user)
GET    /api/users/123      (get specific user)
PUT    /api/users/123      (update user)
DELETE /api/users/123      (delete user)
```

✅ **Use Hierarchical Structure for Relationships:**
```
GET /api/users/123/orders           (get orders for user 123)
GET /api/orders/456/items           (get items for order 456)
GET /api/users/123/orders/456       (get specific order for user 123)
```

❌ **Avoid:**
- Verbs in URLs: `/api/getUsers`, `/api/createUser`
- Singular for collections: `/api/user`
- Deeply nested resources (max 2 levels): `/api/users/123/orders/456/items/789/reviews`
- Underscores: `/api/user_profiles` (use `/api/user-profiles`)

### HTTP Methods

| Method | Idempotent | Safe | Usage | Success Status |
|--------|-----------|------|-------|----------------|
| GET | ✅ | ✅ | Retrieve resource(s) | 200 OK |
| POST | ❌ | ❌ | Create new resource | 201 Created |
| PUT | ✅ | ❌ | Replace entire resource | 200 OK, 204 No Content |
| PATCH | ✅* | ❌ | Partial update | 200 OK, 204 No Content |
| DELETE | ✅ | ❌ | Remove resource | 200 OK, 204 No Content |
| HEAD | ✅ | ✅ | Get headers only | 200 OK |
| OPTIONS | ✅ | ✅ | Get available methods | 200 OK |

*PATCH idempotency depends on implementation

### HTTP Status Codes

**Success Codes (2xx):**
- **200 OK**: Request successful, response body included
- **201 Created**: Resource created, `Location` header with new resource URL
- **202 Accepted**: Request accepted for async processing
- **204 No Content**: Request successful, no response body

**Redirection Codes (3xx):**
- **301 Moved Permanently**: Resource permanently moved
- **302 Found**: Temporary redirect
- **304 Not Modified**: Cached version still valid

**Client Error Codes (4xx):**
- **400 Bad Request**: Invalid request syntax or validation error
- **401 Unauthorized**: Authentication required or failed
- **403 Forbidden**: Authenticated but not authorized
- **404 Not Found**: Resource doesn't exist
- **405 Method Not Allowed**: HTTP method not supported for resource
- **409 Conflict**: Request conflicts with current state (e.g., duplicate)
- **422 Unprocessable Entity**: Semantic errors in request
- **429 Too Many Requests**: Rate limit exceeded

**Server Error Codes (5xx):**
- **500 Internal Server Error**: Unexpected server error
- **502 Bad Gateway**: Invalid response from upstream server
- **503 Service Unavailable**: Server temporarily unavailable
- **504 Gateway Timeout**: Upstream server timeout

### Request/Response Design

**Request Body (JSON):**
```json
POST /api/users
Content-Type: application/json

{
  "firstName": "John",
  "lastName": "Doe",
  "email": "john.doe@example.com",
  "role": "editor"
}
```

**Success Response:**
```json
HTTP/1.1 201 Created
Location: /api/users/123
Content-Type: application/json

{
  "id": "123",
  "firstName": "John",
  "lastName": "Doe",
  "email": "john.doe@example.com",
  "role": "editor",
  "createdAt": "2026-01-14T10:30:00Z",
  "updatedAt": "2026-01-14T10:30:00Z"
}
```

**Error Response:**
```json
HTTP/1.1 400 Bad Request
Content-Type: application/json

{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format",
        "code": "INVALID_FORMAT"
      },
      {
        "field": "role",
        "message": "Role must be one of: admin, editor, viewer",
        "code": "INVALID_VALUE"
      }
    ]
  }
}
```

### Pagination

**Offset-Based Pagination:**
```
GET /api/users?page=2&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 150,
    "totalPages": 8,
    "hasMore": true
  },
  "links": {
    "self": "/api/users?page=2&limit=20",
    "first": "/api/users?page=1&limit=20",
    "prev": "/api/users?page=1&limit=20",
    "next": "/api/users?page=3&limit=20",
    "last": "/api/users?page=8&limit=20"
  }
}
```

**Pros:** Simple, familiar, supports jumping to specific pages  
**Cons:** Performance degrades with large datasets, inconsistent with data changes

**Cursor-Based Pagination:**
```
GET /api/users?cursor=eyJpZCI6MTIzfQ==&limit=20

Response:
{
  "data": [...],
  "pagination": {
    "cursor": "eyJpZCI6MTQzfQ==",
    "limit": 20,
    "hasMore": true
  },
  "links": {
    "self": "/api/users?cursor=eyJpZCI6MTIzfQ==&limit=20",
    "next": "/api/users?cursor=eyJpZCI6MTQzfQ==&limit=20"
  }
}
```

**Pros:** Consistent performance, handles data changes gracefully  
**Cons:** Can't jump to specific page, more complex to implement

**Best Practice:** Use cursor-based for large datasets and real-time data, offset-based for small datasets and when page jumping is needed.

### Filtering, Sorting, Searching

**Filtering:**
```
GET /api/products?category=electronics&inStock=true&minPrice=100&maxPrice=500
```

**Sorting:**
```
GET /api/users?sort=lastName,firstName    (ascending)
GET /api/users?sort=-createdAt            (descending, note the minus sign)
GET /api/users?sort=lastName,-createdAt   (multiple fields)
```

**Searching:**
```
GET /api/products?search=laptop           (full-text search)
GET /api/users?q=john                     (query parameter)
```

**Field Selection (Sparse Fieldsets):**
```
GET /api/users?fields=id,firstName,lastName,email
(only return specified fields)
```

### API Versioning

See [API Versioning Strategies](#api-versioning-strategies) section below.

### HATEOAS (Hypermedia as the Engine of Application State)

**Principle:** Responses include links to related resources and available actions.

**Example:**
```json
{
  "id": "123",
  "firstName": "John",
  "lastName": "Doe",
  "links": {
    "self": "/api/users/123",
    "orders": "/api/users/123/orders",
    "edit": "/api/users/123",
    "delete": "/api/users/123"
  }
}
```

**Benefits:** Self-documenting API, clients less coupled to URL structure  
**Drawbacks:** Increased payload size, added complexity

### Idempotency

**Definition:** Multiple identical requests have the same effect as a single request.

**Idempotent Methods:** GET, PUT, DELETE, HEAD, OPTIONS  
**Non-Idempotent:** POST

**Idempotency Keys (for POST):**
```
POST /api/orders
Idempotency-Key: a1b2c3d4-e5f6-7890-g1h2-i3j4k5l6m7n8
Content-Type: application/json

{
  "productId": "456",
  "quantity": 2
}
```

Server stores the idempotency key and returns the same response for duplicate requests.

### Caching

**Cache-Control Header:**
```
Cache-Control: max-age=3600, public
```

**ETag (Entity Tag) for Conditional Requests:**
```
GET /api/users/123
Response:
ETag: "33a64df551425fcc"

Subsequent request:
GET /api/users/123
If-None-Match: "33a64df551425fcc"

Response (if not modified):
HTTP/1.1 304 Not Modified
```

**Last-Modified:**
```
Last-Modified: Tue, 14 Jan 2026 10:00:00 GMT

Subsequent request:
If-Modified-Since: Tue, 14 Jan 2026 10:00:00 GMT
```

### Rate Limiting

**Headers:**
```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 87
X-RateLimit-Reset: 1642161600
```

**Response when rate limit exceeded:**
```
HTTP/1.1 429 Too Many Requests
Retry-After: 3600

{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Rate limit exceeded. Try again in 1 hour."
  }
}
```

---

## GraphQL API Design

### Schema Design Best Practices

**Type Definitions:**
```graphql
type User {
  id: ID!
  firstName: String!
  lastName: String!
  email: String!
  role: UserRole!
  posts: [Post!]!
  createdAt: DateTime!
  updatedAt: DateTime!
}

enum UserRole {
  ADMIN
  EDITOR
  VIEWER
}

type Post {
  id: ID!
  title: String!
  content: String!
  author: User!
  published: Boolean!
  createdAt: DateTime!
}
```

**Best Practices:**
- Use `!` for non-nullable fields
- Use clear, descriptive names (nouns for types, verbs for mutations)
- Use enums for fixed value sets
- Define custom scalars (DateTime, Email, URL)
- Keep types focused and cohesive

### Query Design

**Simple Query:**
```graphql
type Query {
  user(id: ID!): User
  users(limit: Int, cursor: String): UserConnection!
}
```

**Relay-Style Connection (Pagination):**
```graphql
type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
}

type UserEdge {
  cursor: String!
  node: User!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}
```

**Query with Arguments:**
```graphql
type Query {
  posts(
    authorId: ID
    published: Boolean
    search: String
    orderBy: PostOrderBy
    limit: Int = 10
    cursor: String
  ): PostConnection!
}

enum PostOrderBy {
  CREATED_AT_ASC
  CREATED_AT_DESC
  TITLE_ASC
  TITLE_DESC
}
```

### Mutation Design

**Naming Convention:** `verb + Noun`

**Input Types:**
```graphql
type Mutation {
  createUser(input: CreateUserInput!): CreateUserPayload!
  updateUser(id: ID!, input: UpdateUserInput!): UpdateUserPayload!
  deleteUser(id: ID!): DeleteUserPayload!
}

input CreateUserInput {
  firstName: String!
  lastName: String!
  email: String!
  role: UserRole!
}

type CreateUserPayload {
  user: User
  userEdge: UserEdge
  errors: [UserError!]
}

type UserError {
  field: String!
  message: String!
  code: ErrorCode!
}
```

**Best Practices:**
- Use input types for complex arguments
- Return payload types (not just the object)
- Include errors in payload (not just throw exceptions)
- Return the updated object in payload
- Consider returning an edge for list updates

### N+1 Query Problem

**Problem:**
```graphql
query {
  posts {
    id
    title
    author {  # Triggers N queries (one per post)
      id
      firstName
    }
  }
}
```

**Solution: DataLoader**
```javascript
const DataLoader = require('dataloader');

const userLoader = new DataLoader(async (userIds) => {
  const users = await db.users.findByIds(userIds);
  return userIds.map(id => users.find(user => user.id === id));
});

const resolvers = {
  Post: {
    author: (post) => userLoader.load(post.authorId)
  }
};
```

### Query Complexity and Depth Limiting

**Depth Limiting:**
```javascript
const depthLimit = require('graphql-depth-limit');

const server = new ApolloServer({
  schema,
  validationRules: [depthLimit(5)]  // Max 5 levels deep
});
```

**Query Cost Analysis:**
```javascript
const costAnalysis = require('graphql-cost-analysis');

const server = new ApolloServer({
  schema,
  validationRules: [
    costAnalysis({
      maximumCost: 1000,
      defaultCost: 1,
      multipliers: ['first', 'last']
    })
  ]
});
```

### Error Handling

**Error Response:**
```json
{
  "errors": [
    {
      "message": "User not found",
      "locations": [{"line": 2, "column": 3}],
      "path": ["user"],
      "extensions": {
        "code": "NOT_FOUND",
        "userId": "123"
      }
    }
  ],
  "data": {
    "user": null
  }
}
```

**Custom Error Codes:**
- `UNAUTHENTICATED`: Not authenticated
- `FORBIDDEN`: Not authorized
- `BAD_USER_INPUT`: Validation error
- `NOT_FOUND`: Resource not found
- `INTERNAL_SERVER_ERROR`: Server error

### Subscriptions

**Schema Definition:**
```graphql
type Subscription {
  postCreated: Post!
  postUpdated(id: ID!): Post!
  messageAdded(conversationId: ID!): Message!
}
```

**Client Usage:**
```graphql
subscription {
  postCreated {
    id
    title
    author {
      firstName
    }
  }
}
```

**Best Practices:**
- Use subscriptions for real-time updates
- Filter events server-side (not client-side)
- Consider scalability (WebSocket connections)
- Clean up subscriptions on client disconnect

---

## gRPC API Design

### Protocol Buffers (Protobuf)

**Service Definition:**
```protobuf
syntax = "proto3";

package user.v1;

service UserService {
  rpc GetUser(GetUserRequest) returns (GetUserResponse);
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);
  rpc CreateUser(CreateUserRequest) returns (CreateUserResponse);
  rpc UpdateUser(UpdateUserRequest) returns (UpdateUserResponse);
  rpc DeleteUser(DeleteUserRequest) returns (DeleteUserResponse);
  
  // Server streaming
  rpc StreamUsers(StreamUsersRequest) returns (stream User);
  
  // Client streaming
  rpc UploadUsers(stream User) returns (UploadUsersResponse);
  
  // Bidirectional streaming
  rpc Chat(stream ChatMessage) returns (stream ChatMessage);
}

message User {
  string id = 1;
  string first_name = 2;
  string last_name = 3;
  string email = 4;
  UserRole role = 5;
  int64 created_at = 6;
  int64 updated_at = 7;
}

enum UserRole {
  USER_ROLE_UNSPECIFIED = 0;
  USER_ROLE_ADMIN = 1;
  USER_ROLE_EDITOR = 2;
  USER_ROLE_VIEWER = 3;
}

message GetUserRequest {
  string id = 1;
}

message GetUserResponse {
  User user = 1;
}

message ListUsersRequest {
  int32 page_size = 1;
  string page_token = 2;
  string filter = 3;
  string order_by = 4;
}

message ListUsersResponse {
  repeated User users = 1;
  string next_page_token = 2;
  int32 total_size = 3;
}
```

### Field Numbering

**Best Practices:**
- Never reuse field numbers (breaks backward compatibility)
- Reserve numbers 1-15 for frequently used fields (1 byte encoding)
- Reserve numbers for deleted fields to prevent reuse
- Group related fields with similar numbers

**Reserved Fields:**
```protobuf
message User {
  reserved 4, 5;  // Reserved field numbers
  reserved "old_field_name";  // Reserved field name
  
  string id = 1;
  string first_name = 2;
  string last_name = 3;
  // field 4 and 5 reserved (deleted fields)
  string email = 6;
}
```

### Streaming Patterns

**Server Streaming (one request, multiple responses):**
- Use case: Download large dataset, real-time updates
```protobuf
rpc StreamOrders(StreamOrdersRequest) returns (stream Order);
```

**Client Streaming (multiple requests, one response):**
- Use case: Upload large dataset, batch processing
```protobuf
rpc UploadImages(stream Image) returns (UploadImagesResponse);
```

**Bidirectional Streaming (multiple requests and responses):**
- Use case: Chat, real-time collaboration
```protobuf
rpc Chat(stream ChatMessage) returns (stream ChatMessage);
```

### Error Handling

**gRPC Status Codes:**
- `OK` (0): Success
- `CANCELLED` (1): Operation cancelled
- `INVALID_ARGUMENT` (3): Invalid client argument
- `DEADLINE_EXCEEDED` (4): Timeout
- `NOT_FOUND` (5): Resource not found
- `ALREADY_EXISTS` (6): Resource already exists
- `PERMISSION_DENIED` (7): Not authorized
- `UNAUTHENTICATED` (16): Not authenticated
- `UNAVAILABLE` (14): Service unavailable
- `INTERNAL` (13): Internal server error

**Error with Details:**
```protobuf
message ErrorInfo {
  string reason = 1;
  string domain = 2;
  map<string, string> metadata = 3;
}
```

### Metadata (Headers)

**Setting Metadata (Server):**
```javascript
const metadata = new grpc.Metadata();
metadata.add('x-request-id', '123');
callback(null, response, metadata);
```

**Reading Metadata (Client):**
```javascript
const metadata = new grpc.Metadata();
metadata.add('authorization', 'Bearer token');
client.getUser(request, metadata, callback);
```

### Service Versioning

**Package Versioning:**
```protobuf
package user.v1;  // Version 1
package user.v2;  // Version 2
```

**Running Multiple Versions:**
- Deploy v1 and v2 simultaneously
- Clients specify version in package name
- Gradual migration from v1 to v2

---

## API Versioning Strategies

### 1. URL Path Versioning

**Example:**
```
/api/v1/users
/api/v2/users
```

**Pros:** Simple, visible in URL, easy to route  
**Cons:** URL changes, requires updating documentation

**Best For:** Public APIs, major breaking changes

### 2. Header Versioning

**Example:**
```
GET /api/users
Accept: application/vnd.api+json; version=1
```

**Pros:** Clean URLs, content negotiation  
**Cons:** Less visible, harder to test in browser

**Best For:** Internal APIs, sophisticated clients

### 3. Query Parameter Versioning

**Example:**
```
/api/users?version=1
```

**Pros:** Simple, optional parameter  
**Cons:** Clutters query string, easy to forget

**Best For:** Quick prototypes, simple APIs

### 4. Content Negotiation (Media Type)

**Example:**
```
Accept: application/vnd.company.user.v1+json
```

**Pros:** True REST, flexible  
**Cons:** Complex, less common

**Best For:** Mature REST APIs

### Versioning Best Practices

1. **Version from Day One**: Start with v1
2. **Semantic Versioning**: Major.Minor.Patch (1.0.0)
3. **Document Breaking Changes**: Clear changelog
4. **Support Multiple Versions**: At least 2 versions concurrently
5. **Deprecation Warnings**: Give clients advance notice
6. **Sunset Policy**: Define version support duration
7. **Backward Compatibility**: Prefer non-breaking changes

**Non-Breaking Changes:**
- Adding new endpoints
- Adding optional fields
- Adding new values to enums
- Relaxing validation rules

**Breaking Changes:**
- Removing endpoints or fields
- Renaming fields
- Changing field types
- Making optional fields required
- Changing response structure

---

## API Authentication & Authorization

### Authentication Methods

**1. API Keys**
```
GET /api/users
X-API-Key: abc123def456
```

**Pros:** Simple, good for server-to-server  
**Cons:** No user context, revocation difficult

**2. JWT (JSON Web Tokens)**
```
GET /api/users
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**JWT Structure:**
```json
{
  "header": {
    "alg": "RS256",
    "typ": "JWT"
  },
  "payload": {
    "sub": "user-123",
    "name": "John Doe",
    "role": "admin",
    "iat": 1516239022,
    "exp": 1516242622
  }
}
```

**Pros:** Stateless, contains user info, widely supported  
**Cons:** Token size, revocation complexity

**3. OAuth 2.0**

**Authorization Code Flow (for web apps):**
1. Redirect user to authorization server
2. User authenticates and grants permissions
3. Authorization server redirects back with code
4. Exchange code for access token
5. Use access token to access API

**Client Credentials Flow (for server-to-server):**
```
POST /oauth/token
Content-Type: application/x-www-form-urlencoded

grant_type=client_credentials
&client_id=abc123
&client_secret=secret123
&scope=read:users

Response:
{
  "access_token": "eyJhbGci...",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

### Authorization Models

**RBAC (Role-Based Access Control):**
```json
{
  "userId": "123",
  "roles": ["editor"],
  "permissions": {
    "posts": ["create", "read", "update"],
    "users": ["read"]
  }
}
```

**ABAC (Attribute-Based Access Control):**
```json
{
  "subject": {"userId": "123", "department": "engineering"},
  "action": "update",
  "resource": {"type": "post", "ownerId": "123", "status": "draft"},
  "environment": {"time": "2026-01-14T10:00:00Z", "ip": "192.168.1.1"}
}
```

**Policy Example (OPA Rego):**
```rego
allow {
  input.subject.userId == input.resource.ownerId
  input.action == "update"
  input.resource.status == "draft"
}
```

### Best Practices

1. **Use HTTPS**: Always encrypt in transit
2. **Short-Lived Tokens**: Access tokens expire in 15-60 minutes
3. **Refresh Tokens**: Long-lived, for renewing access tokens
4. **Scope Permissions**: Principle of least privilege
5. **Validate Tokens**: On every request
6. **Rate Limiting**: Prevent abuse
7. **Audit Logging**: Log authentication and authorization events

---

## API Documentation

### OpenAPI (Swagger) Specification

**Example:**
```yaml
openapi: 3.0.3
info:
  title: User API
  version: 1.0.0
  description: API for managing users

servers:
  - url: https://api.example.com/v1
    description: Production
  - url: https://staging-api.example.com/v1
    description: Staging

paths:
  /users:
    get:
      summary: List users
      description: Returns a list of users with pagination
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
            maximum: 100
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
                  pagination:
                    $ref: '#/components/schemas/Pagination'
        '401':
          $ref: '#/components/responses/Unauthorized'
    post:
      summary: Create user
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/CreateUserRequest'
      responses:
        '201':
          description: User created
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'
        '400':
          $ref: '#/components/responses/BadRequest'

components:
  schemas:
    User:
      type: object
      properties:
        id:
          type: string
          example: "123"
        firstName:
          type: string
          example: "John"
        lastName:
          type: string
          example: "Doe"
        email:
          type: string
          format: email
          example: "john.doe@example.com"
        role:
          type: string
          enum: [admin, editor, viewer]
        createdAt:
          type: string
          format: date-time
          
    CreateUserRequest:
      type: object
      required:
        - firstName
        - lastName
        - email
      properties:
        firstName:
          type: string
          minLength: 1
          maxLength: 100
        lastName:
          type: string
        email:
          type: string
          format: email
        role:
          type: string
          enum: [admin, editor, viewer]
          default: viewer
          
    Pagination:
      type: object
      properties:
        page:
          type: integer
        limit:
          type: integer
        total:
          type: integer
        totalPages:
          type: integer
          
  responses:
    Unauthorized:
      description: Unauthorized
      content:
        application/json:
          schema:
            $ref: '#/components/schemas/Error'
            
    BadRequest:
      description: Bad Request
      content:
        application/json:
          schema:
            $ref: '#/components/schemas/Error'
            
  securitySchemes:
    bearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT
      
security:
  - bearerAuth: []
```

### Documentation Best Practices

1. **Complete and Accurate**: All endpoints documented
2. **Examples**: Request and response examples
3. **Error Scenarios**: Document error responses
4. **Authentication**: Clearly specify auth requirements
5. **Rate Limits**: Document rate limit policies
6. **Changelog**: Track API changes
7. **Try It Out**: Interactive documentation (Swagger UI, Postman)

---

## API Testing

### Test Types

**1. Unit Tests (Service Layer)**
```javascript
describe('UserService', () => {
  it('should create a user', async () => {
    const user = await userService.createUser({
      firstName: 'John',
      lastName: 'Doe',
      email: 'john@example.com'
    });
    
    expect(user).toHaveProperty('id');
    expect(user.email).toBe('john@example.com');
  });
});
```

**2. Integration Tests (API Endpoints)**
```javascript
describe('POST /api/users', () => {
  it('should create a user and return 201', async () => {
    const response = await request(app)
      .post('/api/users')
      .send({
        firstName: 'John',
        lastName: 'Doe',
        email: 'john@example.com'
      });
    
    expect(response.status).toBe(201);
    expect(response.body).toHaveProperty('id');
  });
  
  it('should return 400 for invalid email', async () => {
    const response = await request(app)
      .post('/api/users')
      .send({
        firstName: 'John',
        lastName: 'Doe',
        email: 'invalid-email'
      });
    
    expect(response.status).toBe(400);
    expect(response.body.error.details).toContainEqual(
      expect.objectContaining({ field: 'email' })
    );
  });
});
```

**3. Contract Tests (API Contract)**
- Pact: Consumer-driven contract testing
- Ensures API matches consumer expectations

**4. Load Tests (Performance)**
```javascript
import http from 'k6/http';
import { check } from 'k6';

export let options = {
  stages: [
    { duration: '30s', target: 50 },  // Ramp up
    { duration: '1m', target: 100 },   // Stay at 100 users
    { duration: '30s', target: 0 }     // Ramp down
  ]
};

export default function() {
  let response = http.get('https://api.example.com/users');
  check(response, {
    'status is 200': (r) => r.status === 200,
    'response time < 500ms': (r) => r.timings.duration < 500
  });
}
```

### Testing Checklist

- [ ] Unit tests for business logic
- [ ] Integration tests for API endpoints
- [ ] Test all success scenarios
- [ ] Test all error scenarios (400, 401, 403, 404, 500)
- [ ] Test authentication and authorization
- [ ] Test input validation
- [ ] Test pagination, filtering, sorting
- [ ] Test idempotency for POST/PUT/PATCH
- [ ] Test rate limiting
- [ ] Load testing for performance
- [ ] Security testing (OWASP Top 10)

---

## Common API Anti-Patterns

### 1. Chatty APIs

**Problem:** Too many small, fine-grained API calls

**Example (Bad):**
```javascript
// 3 API calls to render a user profile
const user = await fetch('/api/users/123');
const orders = await fetch('/api/users/123/orders');
const preferences = await fetch('/api/users/123/preferences');
```

**Solution:** Composite/aggregate endpoints
```javascript
// 1 API call with includes
const data = await fetch('/api/users/123?include=orders,preferences');
```

### 2. Overfetching

**Problem:** Returning too much data

**Example (Bad):**
```javascript
// Returns all user fields when only name needed
GET /api/users/123  // Returns 50 fields
```

**Solution:** Field selection (GraphQL or REST with fields parameter)
```javascript
GET /api/users/123?fields=id,firstName,lastName
```

### 3. Ignoring HTTP Methods

**Problem:** Using POST for everything

**Example (Bad):**
```
POST /api/getUser
POST /api/createUser
POST /api/updateUser
POST /api/deleteUser
```

**Solution:** Use appropriate HTTP methods
```
GET    /api/users/123
POST   /api/users
PUT    /api/users/123
DELETE /api/users/123
```

### 4. Exposing Implementation Details

**Problem:** Database structure exposed in API

**Example (Bad):**
```
GET /api/user_accounts?user_id=123&status_flag=1
```

**Solution:** Domain-driven API design
```
GET /api/users/123?status=active
```

### 5. No Versioning

**Problem:** Breaking changes without versioning

**Solution:** Version APIs from day one
```
/api/v1/users  (initial version)
/api/v2/users  (breaking change)
```

### 6. Inconsistent Naming

**Problem:** Mixing conventions

**Example (Bad):**
```
/api/users       (plural)
/api/product     (singular)
/api/order-items (kebab-case)
/api/user_roles  (snake_case)
```

**Solution:** Consistent naming
```
/api/users
/api/products
/api/order-items
/api/user-roles
```

### 7. Poor Error Messages

**Problem:** Generic error messages

**Example (Bad):**
```json
{
  "error": "Bad request"
}
```

**Solution:** Detailed, actionable errors
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "details": [
      {
        "field": "email",
        "message": "Email must be valid email format",
        "code": "INVALID_FORMAT"
      }
    ]
  }
}
```

### 8. Synchronous Long-Running Operations

**Problem:** API timeout on long operations

**Solution:** Async processing with status endpoint
```
POST /api/reports
Response 202 Accepted:
{
  "jobId": "abc123",
  "status": "processing",
  "statusUrl": "/api/reports/abc123/status"
}

GET /api/reports/abc123/status
Response 200 OK:
{
  "jobId": "abc123",
  "status": "completed",
  "downloadUrl": "/api/reports/abc123/download"
}
```

---

## Summary

- **REST**: Use proper HTTP methods, status codes, resource naming
- **GraphQL**: Prevent N+1 queries, limit complexity, return errors in payload
- **gRPC**: Never reuse field numbers, choose appropriate streaming patterns
- **Versioning**: Version from day one, support multiple versions
- **Authentication**: Use JWT or OAuth 2.0, short-lived tokens
- **Documentation**: Complete OpenAPI specs, examples, interactive docs
- **Testing**: Unit, integration, contract, load, security tests
- **Avoid Anti-Patterns**: Chatty APIs, overfetching, inconsistent naming

Well-designed APIs are intuitive, consistent, secure, and scalable. They follow established conventions and provide excellent developer experience.
