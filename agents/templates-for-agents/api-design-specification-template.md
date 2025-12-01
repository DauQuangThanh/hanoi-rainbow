# API Design Specification

## API: [API Name]

**Version**: [1.0.0]  
**Prepared By**: [Architect Name]  
**Date Created**: [YYYY-MM-DD]  
**Last Updated**: [YYYY-MM-DD]  
**Base URL**: `https://api.example.com/v1`

---

## 1. Overview

### 1.1 Purpose

[Describe the purpose of this API and what it enables consumers to do]

### 1.2 Intended Audience

* **Internal Developers**: [How they will use the API]
* **External Partners**: [Integration scenarios]
* **Third-party Developers**: [Public API use cases]

### 1.3 API Style

* **Style**: REST / GraphQL / gRPC / WebSocket
* **Protocol**: HTTPS
* **Data Format**: JSON / Protocol Buffers / XML

---

## 2. API Design Principles

### 2.1 Design Guidelines

* **RESTful Conventions**: Follow REST principles (resources, HTTP verbs, status codes)
* **Resource-Oriented**: Design around resources, not actions
* **Consistency**: Consistent naming, structure, and behavior across endpoints
* **Versioning**: Support multiple API versions for backward compatibility
* **Idempotency**: GET, PUT, DELETE operations must be idempotent
* **Error Handling**: Consistent error response format
* **Security First**: All endpoints require authentication unless explicitly public

### 2.2 Naming Conventions

**Resource Names**:

* Use plural nouns: `/users`, `/orders`, `/products`
* Use kebab-case for multi-word resources: `/order-items`, `/user-preferences`
* Avoid verbs in URLs (use HTTP methods instead)

**Query Parameters**:

* Use camelCase: `?sortBy=createdAt&pageSize=20`

**JSON Properties**:

* Use camelCase: `{"firstName": "John", "createdAt": "2025-01-01T00:00:00Z"}`

---

## 3. Authentication & Authorization

### 3.1 Authentication Method

**Method**: OAuth 2.0 with JWT tokens / API Key / Basic Auth

**Token Format**:

```
Authorization: Bearer <JWT_TOKEN>
```

**Token Acquisition**:

```http
POST /auth/token
Content-Type: application/json

{
  "username": "user@example.com",
  "password": "SecurePassword123"
}

Response:
{
  "accessToken": "eyJhbGciOiJIUzI1NiIs...",
  "refreshToken": "dGhpc2lzYXJlZnJlc2h0b2tlbg==",
  "expiresIn": 3600,
  "tokenType": "Bearer"
}
```

### 3.2 Authorization Model

**Model**: Role-Based Access Control (RBAC)

**Roles**:

* **Admin**: Full access to all resources
* **User**: Access to own resources and public data
* **Guest**: Read-only access to public resources

**Permission Format**:

```json
{
  "userId": "12345",
  "roles": ["user"],
  "permissions": ["read:users", "write:own-profile"]
}
```

---

## 4. API Endpoints

### 4.1 Resource: Users

#### GET /users

**Description**: Retrieve a list of users  
**Authentication**: Required (Admin only)  
**Rate Limit**: 100 requests per minute

**Request Parameters**:

| Parameter | Type | Required | Description | Default |
| :--- | :--- | :---: | :--- | :--- |
| `page` | integer | No | Page number (1-indexed) | 1 |
| `pageSize` | integer | No | Number of items per page (max 100) | 20 |
| `sortBy` | string | No | Field to sort by (createdAt, name, email) | createdAt |
| `sortOrder` | string | No | Sort direction (asc, desc) | desc |
| `status` | string | No | Filter by status (active, inactive, pending) | - |
| `search` | string | No | Search in name or email | - |

**Request Example**:

```http
GET /users?page=1&pageSize=20&sortBy=createdAt&sortOrder=desc&status=active
Authorization: Bearer <JWT_TOKEN>
```

**Response (200 OK)**:

```json
{
  "data": [
    {
      "id": "user-123",
      "email": "john.doe@example.com",
      "firstName": "John",
      "lastName": "Doe",
      "status": "active",
      "roles": ["user"],
      "createdAt": "2025-01-01T00:00:00Z",
      "updatedAt": "2025-01-15T10:30:00Z"
    }
  ],
  "pagination": {
    "page": 1,
    "pageSize": 20,
    "totalItems": 150,
    "totalPages": 8
  },
  "links": {
    "self": "/users?page=1&pageSize=20",
    "next": "/users?page=2&pageSize=20",
    "last": "/users?page=8&pageSize=20"
  }
}
```

**Error Responses**:

* `401 Unauthorized`: Missing or invalid authentication token
* `403 Forbidden`: User lacks admin role
* `429 Too Many Requests`: Rate limit exceeded

---

#### GET /users/{userId}

**Description**: Retrieve a specific user by ID  
**Authentication**: Required  
**Rate Limit**: 100 requests per minute

**Path Parameters**:

| Parameter | Type | Description |
| :--- | :--- | :--- |
| `userId` | string | Unique identifier of the user |

**Request Example**:

```http
GET /users/user-123
Authorization: Bearer <JWT_TOKEN>
```

**Response (200 OK)**:

```json
{
  "id": "user-123",
  "email": "john.doe@example.com",
  "firstName": "John",
  "lastName": "Doe",
  "status": "active",
  "roles": ["user"],
  "profile": {
    "bio": "Software engineer",
    "avatarUrl": "https://cdn.example.com/avatars/user-123.jpg",
    "location": "San Francisco, CA"
  },
  "createdAt": "2025-01-01T00:00:00Z",
  "updatedAt": "2025-01-15T10:30:00Z"
}
```

**Error Responses**:

* `401 Unauthorized`: Missing or invalid authentication token
* `403 Forbidden`: User can only access own profile
* `404 Not Found`: User not found

---

#### POST /users

**Description**: Create a new user  
**Authentication**: Required (Admin only)  
**Rate Limit**: 20 requests per minute

**Request Body**:

```json
{
  "email": "jane.smith@example.com",
  "firstName": "Jane",
  "lastName": "Smith",
  "password": "SecurePassword123!",
  "roles": ["user"]
}
```

**Validation Rules**:

* `email`: Valid email format, unique
* `firstName`: 1-50 characters, required
* `lastName`: 1-50 characters, required
* `password`: Min 8 characters, must contain uppercase, lowercase, number, special char
* `roles`: Array of valid role names

**Response (201 Created)**:

```json
{
  "id": "user-456",
  "email": "jane.smith@example.com",
  "firstName": "Jane",
  "lastName": "Smith",
  "status": "active",
  "roles": ["user"],
  "createdAt": "2025-01-20T14:22:00Z",
  "updatedAt": "2025-01-20T14:22:00Z"
}
```

**Error Responses**:

* `400 Bad Request`: Invalid request body or validation failure
* `401 Unauthorized`: Missing or invalid authentication token
* `403 Forbidden`: User lacks admin role
* `409 Conflict`: Email already exists

---

#### PUT /users/{userId}

**Description**: Update an existing user  
**Authentication**: Required  
**Idempotency**: Idempotent  
**Rate Limit**: 20 requests per minute

**Request Body**:

```json
{
  "firstName": "Jane",
  "lastName": "Doe-Smith",
  "profile": {
    "bio": "Updated bio",
    "location": "New York, NY"
  }
}
```

**Response (200 OK)**:
[Same structure as GET /users/{userId}]

**Error Responses**:

* `400 Bad Request`: Invalid request body
* `401 Unauthorized`: Missing or invalid token
* `403 Forbidden`: User can only update own profile
* `404 Not Found`: User not found

---

#### DELETE /users/{userId}

**Description**: Delete a user (soft delete)  
**Authentication**: Required (Admin only)  
**Idempotency**: Idempotent  
**Rate Limit**: 10 requests per minute

**Response (204 No Content)**:
[Empty response body]

**Error Responses**:

* `401 Unauthorized`: Missing or invalid token
* `403 Forbidden`: User lacks admin role
* `404 Not Found`: User not found

---

### 4.2 Resource: [Another Resource]

[Repeat the structure above for each major resource]

---

## 5. Common Patterns

### 5.1 Pagination

All list endpoints support cursor-based or offset-based pagination.

**Offset-based** (Simple, stateless):

```
GET /users?page=2&pageSize=20
```

**Cursor-based** (Efficient for large datasets):

```
GET /users?cursor=eyJpZCI6MTIzfQ==&limit=20
```

**Response includes**:

```json
{
  "data": [...],
  "pagination": {
    "cursor": "nextCursorValue",
    "hasMore": true
  }
}
```

### 5.2 Filtering

Use query parameters for filtering:

```
GET /orders?status=pending&createdAfter=2025-01-01&minAmount=100
```

### 5.3 Sorting

Use `sortBy` and `sortOrder` parameters:

```
GET /products?sortBy=price&sortOrder=asc
```

### 5.4 Field Selection (Sparse Fieldsets)

Clients can request specific fields:

```
GET /users/123?fields=id,email,firstName
```

### 5.5 Expanding Related Resources

Use `expand` parameter to include related resources:

```
GET /orders/456?expand=customer,items
```

---

## 6. HTTP Status Codes

### 6.1 Success Codes

| Code | Status | Usage |
| :--- | :--- | :--- |
| 200 | OK | Successful GET, PUT, PATCH requests |
| 201 | Created | Successful POST request that creates a resource |
| 204 | No Content | Successful DELETE request |

### 6.2 Client Error Codes

| Code | Status | Usage |
| :--- | :--- | :--- |
| 400 | Bad Request | Invalid request body, validation failure |
| 401 | Unauthorized | Missing or invalid authentication |
| 403 | Forbidden | Authenticated but lacks permission |
| 404 | Not Found | Resource does not exist |
| 409 | Conflict | Resource conflict (e.g., duplicate email) |
| 422 | Unprocessable Entity | Validation errors |
| 429 | Too Many Requests | Rate limit exceeded |

### 6.3 Server Error Codes

| Code | Status | Usage |
| :--- | :--- | :--- |
| 500 | Internal Server Error | Unexpected server error |
| 502 | Bad Gateway | Upstream service failure |
| 503 | Service Unavailable | Service temporarily down |
| 504 | Gateway Timeout | Upstream service timeout |

---

## 7. Error Response Format

All error responses follow a consistent structure:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "The request contains invalid data",
    "details": [
      {
        "field": "email",
        "message": "Email is already registered",
        "code": "DUPLICATE_EMAIL"
      }
    ],
    "requestId": "req-abc123",
    "timestamp": "2025-01-20T14:30:00Z"
  }
}
```

**Error Code Categories**:

* `VALIDATION_ERROR`: Input validation failure
* `AUTHENTICATION_ERROR`: Auth token missing or invalid
* `AUTHORIZATION_ERROR`: Insufficient permissions
* `NOT_FOUND`: Resource not found
* `CONFLICT`: Resource conflict
* `RATE_LIMIT_EXCEEDED`: Too many requests
* `INTERNAL_ERROR`: Server error

---

## 8. Rate Limiting

### 8.1 Rate Limit Policy

* **Standard Users**: 100 requests per minute
* **Premium Users**: 1000 requests per minute
* **Admin Users**: 5000 requests per minute

### 8.2 Rate Limit Headers

All responses include rate limit information:

```http
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1642684800
```

### 8.3 Rate Limit Exceeded Response

```http
HTTP/1.1 429 Too Many Requests
Retry-After: 60

{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Too many requests. Please try again later.",
    "retryAfter": 60
  }
}
```

---

## 9. Versioning

### 9.1 Versioning Strategy

**Method**: URL-based versioning (e.g., `/v1/users`, `/v2/users`)

**Version Lifecycle**:

* **Current**: v1 (Stable, fully supported)
* **Deprecated**: v0 (Supported until 2025-12-31)
* **Beta**: v2 (Preview, subject to breaking changes)

### 9.2 Deprecation Policy

* Deprecation announced 6 months in advance
* Deprecated versions supported for 12 months
* Sunset date communicated via API response headers:

```http
Sunset: Sat, 31 Dec 2025 23:59:59 GMT
Deprecation: Mon, 01 Jul 2025 00:00:00 GMT
```

---

## 10. Security

### 10.1 Security Headers

All responses include security headers:

```http
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
Content-Security-Policy: default-src 'self'
```

### 10.2 Input Validation

* All inputs are validated and sanitized
* SQL injection prevention via parameterized queries
* XSS prevention via output encoding
* CSRF protection for state-changing operations

### 10.3 Data Protection

* All data encrypted in transit (TLS 1.3)
* Sensitive data encrypted at rest (AES-256)
* PII masked in logs and monitoring
* GDPR compliance (data export, right to be forgotten)

---

## 11. Testing

### 11.1 Test Environments

* **Development**: `https://dev-api.example.com`
* **Staging**: `https://staging-api.example.com`
* **Production**: `https://api.example.com`

### 11.2 Test Credentials

Contact [email] for test account credentials.

### 11.3 Sandbox Mode

Use `X-Sandbox-Mode: true` header to interact with test data without affecting production.

---

## 12. API Documentation

### 12.1 Interactive Documentation

* **Swagger UI**: `https://api.example.com/docs`
* **OpenAPI Spec**: `https://api.example.com/openapi.json`

### 12.2 Code Examples

Code examples available in multiple languages:

* cURL
* JavaScript (Node.js)
* Python
* Java
* C#

---

## 13. Support & Feedback

### 13.1 Support Channels

* **Email**: <api-support@example.com>
* **Developer Portal**: <https://developers.example.com>
* **Status Page**: <https://status.example.com>

### 13.2 Changelog

* **Changelog**: <https://developers.example.com/changelog>
* **API Updates**: Subscribe to email notifications

---

## 14. Appendices

### Appendix A: Complete OpenAPI Specification

[Link to full OpenAPI/Swagger YAML file]

### Appendix B: Postman Collection

[Link to Postman collection for testing]

### Appendix C: Rate Limit Algorithms

[Technical details on rate limiting implementation]

### Appendix D: Webhook Documentation

[If webhooks are supported, document event types and payload formats]

---

## 15. Revision History

| Version | Date | Author | Changes |
| :--- | :--- | :--- | :--- |
| 1.0 | YYYY-MM-DD | [Name] | Initial API specification |
| 1.1 | YYYY-MM-DD | [Name] | Added pagination support |
