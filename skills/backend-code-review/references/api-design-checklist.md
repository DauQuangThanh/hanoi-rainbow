# API Design Checklist

Comprehensive guidelines for reviewing REST, GraphQL, and gRPC APIs.

## Table of Contents
- [REST API Design](#rest-api-design)
- [GraphQL API Design](#graphql-api-design)
- [gRPC API Design](#grpc-api-design)
- [API Documentation](#api-documentation)
- [Versioning Strategies](#versioning-strategies)
- [Error Handling](#error-handling)
- [Rate Limiting](#rate-limiting)
- [API Security](#api-security)

## REST API Design

### Resource Naming

**Good:**
```
✅ Collections (plural nouns)
GET    /api/users
POST   /api/users
GET    /api/users/{id}
PUT    /api/users/{id}
DELETE /api/users/{id}

✅ Nested resources
GET    /api/users/{userId}/posts
GET    /api/posts/{postId}/comments

✅ Actions as sub-resources
POST   /api/orders/{id}/cancel
POST   /api/users/{id}/activate
```

**Bad:**
```
❌ Inconsistent naming
GET    /api/getUsers
POST   /api/createUser
GET    /api/user/{id}

❌ Verbs in URLs
POST   /api/users/create
GET    /api/users/retrieve
DELETE /api/users/delete

❌ Mixed singular/plural
GET    /api/user
GET    /api/posts
```

### HTTP Methods

**GET - Retrieve Resources:**
```typescript
// ✅ Idempotent, no side effects
app.get('/api/users/:id', async (req, res) => {
  const user = await userService.findById(req.params.id);
  
  if (!user) {
    return res.status(404).json({ error: 'User not found' });
  }
  
  res.json(user);
});

// ✅ List with pagination
app.get('/api/users', async (req, res) => {
  const { page = 1, limit = 20, sort = 'createdAt', order = 'desc' } = req.query;
  
  const result = await userService.findAll({
    page: Number(page),
    limit: Number(limit),
    sort,
    order
  });
  
  res.json({
    data: result.items,
    pagination: {
      page: result.page,
      limit: result.limit,
      total: result.total,
      pages: Math.ceil(result.total / result.limit)
    }
  });
});
```

**POST - Create Resources:**
```typescript
// ✅ Returns created resource with 201 status and Location header
app.post('/api/users', async (req, res) => {
  const userData = req.body;
  
  // Validation
  const errors = validateUser(userData);
  if (errors.length > 0) {
    return res.status(400).json({ errors });
  }
  
  const user = await userService.create(userData);
  
  res.status(201)
    .location(`/api/users/${user.id}`)
    .json(user);
});

// ❌ Bad: Using POST for non-creation
app.post('/api/users/:id/get', async (req, res) => {
  // Should be GET /api/users/:id
});
```

**PUT - Replace Resource:**
```typescript
// ✅ Replaces entire resource
app.put('/api/users/:id', async (req, res) => {
  const { id } = req.params;
  const userData = req.body;
  
  // PUT requires all fields
  if (!userData.email || !userData.name) {
    return res.status(400).json({
      error: 'PUT requires complete resource representation'
    });
  }
  
  const user = await userService.replace(id, userData);
  res.json(user);
});
```

**PATCH - Partial Update:**
```typescript
// ✅ Updates only provided fields
app.patch('/api/users/:id', async (req, res) => {
  const { id } = req.params;
  const updates = req.body;
  
  // PATCH allows partial updates
  const user = await userService.update(id, updates);
  res.json(user);
});
```

**DELETE - Remove Resource:**
```typescript
// ✅ Returns 204 No Content on success
app.delete('/api/users/:id', async (req, res) => {
  await userService.delete(req.params.id);
  res.status(204).send();
});

// ✅ Or return deleted resource
app.delete('/api/users/:id', async (req, res) => {
  const user = await userService.delete(req.params.id);
  res.json(user);
});
```

### Status Codes

**Good:**
```typescript
// ✅ Appropriate status codes

// 200 OK - Successful GET, PUT, PATCH
res.status(200).json(data);

// 201 Created - Successful POST
res.status(201).location('/api/users/123').json(user);

// 204 No Content - Successful DELETE
res.status(204).send();

// 400 Bad Request - Invalid input
res.status(400).json({ error: 'Invalid email format' });

// 401 Unauthorized - Authentication required
res.status(401).json({ error: 'Authentication required' });

// 403 Forbidden - Authenticated but no permission
res.status(403).json({ error: 'Insufficient permissions' });

// 404 Not Found - Resource doesn't exist
res.status(404).json({ error: 'User not found' });

// 409 Conflict - Resource conflict (duplicate)
res.status(409).json({ error: 'Email already registered' });

// 422 Unprocessable Entity - Validation errors
res.status(422).json({
  error: 'Validation failed',
  details: {
    email: ['Email is required'],
    password: ['Password must be at least 8 characters']
  }
});

// 429 Too Many Requests - Rate limit exceeded
res.status(429).json({
  error: 'Rate limit exceeded',
  retryAfter: 60
});

// 500 Internal Server Error - Unexpected errors
res.status(500).json({ error: 'Internal server error' });

// 503 Service Unavailable - Maintenance or overload
res.status(503).json({
  error: 'Service temporarily unavailable',
  retryAfter: 300
});
```

**Bad:**
```typescript
// ❌ Using wrong status codes
res.status(200).json({ error: 'User not found' }); // Should be 404
res.status(500).json({ error: 'Invalid input' });  // Should be 400
res.status(200).json({ success: false });           // Use proper error codes
```

### Request/Response Format

**Good:**
```typescript
// ✅ Consistent JSON structure

// Success response
{
  "id": "123",
  "email": "user@example.com",
  "name": "John Doe",
  "createdAt": "2024-01-14T10:30:00Z"
}

// List response with metadata
{
  "data": [
    { "id": "1", "name": "Item 1" },
    { "id": "2", "name": "Item 2" }
  ],
  "pagination": {
    "page": 1,
    "limit": 20,
    "total": 150,
    "pages": 8
  },
  "links": {
    "self": "/api/items?page=1",
    "next": "/api/items?page=2",
    "last": "/api/items?page=8"
  }
}

// Error response
{
  "error": "Validation failed",
  "code": "VALIDATION_ERROR",
  "details": {
    "email": ["Email is required"],
    "password": ["Password must be at least 8 characters"]
  },
  "timestamp": "2024-01-14T10:30:00Z",
  "path": "/api/users"
}
```

**Bad:**
```typescript
// ❌ Inconsistent structure
// Success
{ success: true, user: { /* ... */ } }

// Error
{ error: true, message: "Something went wrong" }

// Another endpoint
{ data: { /* ... */ }, status: "ok" }
```

### Pagination

**Good:**
```typescript
// ✅ Offset-based pagination
app.get('/api/users', async (req, res) => {
  const page = Number(req.query.page) || 1;
  const limit = Math.min(Number(req.query.limit) || 20, 100); // Max 100
  const offset = (page - 1) * limit;
  
  const { items, total } = await userService.findAll({ offset, limit });
  
  res.json({
    data: items,
    pagination: {
      page,
      limit,
      total,
      pages: Math.ceil(total / limit),
      hasNext: page * limit < total,
      hasPrev: page > 1
    }
  });
});

// ✅ Cursor-based pagination (for large datasets)
app.get('/api/posts', async (req, res) => {
  const { cursor, limit = 20 } = req.query;
  
  const result = await postService.findAll({
    cursor,
    limit: Math.min(Number(limit), 100)
  });
  
  res.json({
    data: result.items,
    pagination: {
      nextCursor: result.nextCursor,
      hasMore: result.hasMore
    }
  });
});
```

### Filtering, Sorting, Searching

**Good:**
```typescript
// ✅ Query parameters for filtering
app.get('/api/users', async (req, res) => {
  const {
    status,        // Filter by status
    role,          // Filter by role
    search,        // Search in name/email
    sort = 'createdAt',
    order = 'desc',
    page = 1,
    limit = 20
  } = req.query;
  
  const filters: any = {};
  
  if (status) filters.status = status;
  if (role) filters.role = role;
  if (search) {
    filters.$or = [
      { name: { $regex: search, $options: 'i' } },
      { email: { $regex: search, $options: 'i' } }
    ];
  }
  
  const result = await userService.findAll({
    filters,
    sort: { [sort]: order === 'desc' ? -1 : 1 },
    page: Number(page),
    limit: Number(limit)
  });
  
  res.json(result);
});

// Usage examples:
// GET /api/users?status=active
// GET /api/users?role=admin&status=active
// GET /api/users?search=john&sort=name&order=asc
```

### Field Selection

**Good:**
```typescript
// ✅ Sparse fieldsets
app.get('/api/users/:id', async (req, res) => {
  const { fields } = req.query;
  
  const user = await userService.findById(req.params.id);
  
  if (!user) {
    return res.status(404).json({ error: 'User not found' });
  }
  
  // Return only requested fields
  if (fields) {
    const fieldList = fields.split(',');
    const filtered = fieldList.reduce((obj, field) => {
      if (user[field] !== undefined) {
        obj[field] = user[field];
      }
      return obj;
    }, {} as any);
    
    return res.json(filtered);
  }
  
  res.json(user);
});

// Usage: GET /api/users/123?fields=id,name,email
```

## GraphQL API Design

### Schema Design

**Good:**
```graphql
# ✅ Clear types with descriptions

"""
Represents a user in the system
"""
type User {
  "Unique identifier"
  id: ID!
  
  "User's email address"
  email: String!
  
  "User's full name"
  name: String!
  
  "User's role"
  role: Role!
  
  "Posts created by this user"
  posts(first: Int = 10, after: String): PostConnection!
  
  "Date the user was created"
  createdAt: DateTime!
}

"""
User roles
"""
enum Role {
  ADMIN
  USER
  GUEST
}

"""
Paginated posts
"""
type PostConnection {
  edges: [PostEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type PostEdge {
  node: Post!
  cursor: String!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
  startCursor: String
  endCursor: String
}
```

### Query Design

**Good:**
```graphql
# ✅ Well-designed queries

type Query {
  # Get single user
  user(id: ID!): User
  
  # List users with pagination and filtering
  users(
    first: Int = 20
    after: String
    filter: UserFilter
    sort: UserSort
  ): UserConnection!
  
  # Search users
  searchUsers(
    query: String!
    first: Int = 20
  ): [User!]!
}

input UserFilter {
  role: Role
  status: UserStatus
  createdAfter: DateTime
  createdBefore: DateTime
}

input UserSort {
  field: UserSortField!
  direction: SortDirection!
}

enum UserSortField {
  NAME
  EMAIL
  CREATED_AT
}

enum SortDirection {
  ASC
  DESC
}
```

### Mutation Design

**Good:**
```graphql
# ✅ Input types for mutations

type Mutation {
  createUser(input: CreateUserInput!): CreateUserPayload!
  updateUser(input: UpdateUserInput!): UpdateUserPayload!
  deleteUser(id: ID!): DeleteUserPayload!
}

input CreateUserInput {
  email: String!
  name: String!
  password: String!
  role: Role = USER
}

type CreateUserPayload {
  user: User
  errors: [UserError!]
}

type UserError {
  field: String
  message: String!
  code: ErrorCode!
}

enum ErrorCode {
  VALIDATION_ERROR
  DUPLICATE_EMAIL
  UNAUTHORIZED
  NOT_FOUND
}
```

### Resolver Implementation

**Good:**
```typescript
// ✅ Efficient resolvers with DataLoader

import DataLoader from 'dataloader';

// Batch loading to prevent N+1 queries
const userLoader = new DataLoader(async (userIds: string[]) => {
  const users = await User.findAll({
    where: { id: userIds }
  });
  
  const userMap = new Map(users.map(u => [u.id, u]));
  return userIds.map(id => userMap.get(id) || null);
});

const postLoader = new DataLoader(async (userIds: string[]) => {
  const posts = await Post.findAll({
    where: { userId: userIds }
  });
  
  const postsByUser = new Map<string, Post[]>();
  posts.forEach(post => {
    if (!postsByUser.has(post.userId)) {
      postsByUser.set(post.userId, []);
    }
    postsByUser.get(post.userId)!.push(post);
  });
  
  return userIds.map(id => postsByUser.get(id) || []);
});

const resolvers = {
  Query: {
    user: async (_parent, { id }, context) => {
      return context.loaders.user.load(id);
    },
    
    users: async (_parent, { first, after, filter, sort }, context) => {
      // Implement pagination and filtering
      const result = await userService.findAll({
        limit: first,
        cursor: after,
        filter,
        sort
      });
      
      return result;
    }
  },
  
  User: {
    posts: async (user, { first, after }, context) => {
      // Use DataLoader to batch requests
      const allPosts = await context.loaders.posts.load(user.id);
      
      // Apply pagination
      return paginatePosts(allPosts, first, after);
    }
  },
  
  Mutation: {
    createUser: async (_parent, { input }, context) => {
      try {
        const user = await userService.create(input);
        return { user, errors: [] };
      } catch (error) {
        return {
          user: null,
          errors: [{
            field: error.field,
            message: error.message,
            code: error.code
          }]
        };
      }
    }
  }
};
```

**Bad:**
```typescript
// ❌ N+1 query problem
const resolvers = {
  User: {
    posts: async (user) => {
      // This runs for EVERY user in a list query!
      return await Post.findAll({ where: { userId: user.id } });
    }
  }
};
```

### Query Complexity

**Good:**
```typescript
// ✅ Limit query complexity and depth

import { createComplexityLimitRule } from 'graphql-validation-complexity';

const server = new ApolloServer({
  typeDefs,
  resolvers,
  validationRules: [
    createComplexityLimitRule(1000, {
      scalarCost: 1,
      objectCost: 10,
      listFactor: 10
    }),
    depthLimit(10) // Max query depth
  ]
});

// ✅ Cost directives
type Query {
  users(first: Int = 20): [User!]! @cost(complexity: 1, multipliers: ["first"])
  posts(first: Int = 20): [Post!]! @cost(complexity: 2, multipliers: ["first"])
}
```

## gRPC API Design

### Protocol Buffer Definition

**Good:**
```protobuf
// ✅ Well-structured proto file

syntax = "proto3";

package user.v1;

option go_package = "github.com/example/user/v1;userv1";

import "google/protobuf/timestamp.proto";
import "google/protobuf/empty.proto";

// User service definition
service UserService {
  // Get a user by ID
  rpc GetUser(GetUserRequest) returns (User);
  
  // List users with pagination
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);
  
  // Create a new user
  rpc CreateUser(CreateUserRequest) returns (User);
  
  // Update an existing user
  rpc UpdateUser(UpdateUserRequest) returns (User);
  
  // Delete a user
  rpc DeleteUser(DeleteUserRequest) returns (google.protobuf.Empty);
  
  // Server streaming: Watch user updates
  rpc WatchUsers(WatchUsersRequest) returns (stream UserUpdate);
  
  // Client streaming: Batch create users
  rpc BatchCreateUsers(stream CreateUserRequest) returns (BatchCreateUsersResponse);
}

// Request/Response messages
message GetUserRequest {
  string id = 1;
}

message ListUsersRequest {
  int32 page_size = 1;
  string page_token = 2;
  UserFilter filter = 3;
  UserSort sort = 4;
}

message ListUsersResponse {
  repeated User users = 1;
  string next_page_token = 2;
  int32 total_count = 3;
}

message CreateUserRequest {
  string email = 1;
  string name = 2;
  string password = 3;
  Role role = 4;
}

message UpdateUserRequest {
  string id = 1;
  optional string email = 2;
  optional string name = 3;
  optional Role role = 4;
}

message DeleteUserRequest {
  string id = 1;
}

// User message
message User {
  string id = 1;
  string email = 2;
  string name = 3;
  Role role = 4;
  google.protobuf.Timestamp created_at = 5;
  google.protobuf.Timestamp updated_at = 6;
}

// Enums
enum Role {
  ROLE_UNSPECIFIED = 0;
  ROLE_USER = 1;
  ROLE_ADMIN = 2;
  ROLE_GUEST = 3;
}

// Filter message
message UserFilter {
  optional Role role = 1;
  optional google.protobuf.Timestamp created_after = 2;
  optional google.protobuf.Timestamp created_before = 3;
}

// Sort message
message UserSort {
  UserSortField field = 1;
  SortDirection direction = 2;
}

enum UserSortField {
  USER_SORT_FIELD_UNSPECIFIED = 0;
  USER_SORT_FIELD_NAME = 1;
  USER_SORT_FIELD_EMAIL = 2;
  USER_SORT_FIELD_CREATED_AT = 3;
}

enum SortDirection {
  SORT_DIRECTION_UNSPECIFIED = 0;
  SORT_DIRECTION_ASC = 1;
  SORT_DIRECTION_DESC = 2;
}

// Streaming messages
message WatchUsersRequest {
  UserFilter filter = 1;
}

message UserUpdate {
  enum UpdateType {
    UPDATE_TYPE_UNSPECIFIED = 0;
    UPDATE_TYPE_CREATED = 1;
    UPDATE_TYPE_UPDATED = 2;
    UPDATE_TYPE_DELETED = 3;
  }
  
  UpdateType type = 1;
  User user = 2;
}

message BatchCreateUsersResponse {
  repeated User users = 1;
  repeated Error errors = 2;
}

message Error {
  string code = 1;
  string message = 2;
  map<string, string> details = 3;
}
```

### Error Handling

**Good:**
```go
// ✅ gRPC error codes

import (
    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"
)

func (s *userService) GetUser(ctx context.Context, req *userv1.GetUserRequest) (*userv1.User, error) {
    if req.Id == "" {
        return nil, status.Error(codes.InvalidArgument, "user ID is required")
    }
    
    user, err := s.repo.FindByID(ctx, req.Id)
    if err != nil {
        if errors.Is(err, ErrNotFound) {
            return nil, status.Error(codes.NotFound, "user not found")
        }
        return nil, status.Error(codes.Internal, "internal server error")
    }
    
    return user, nil
}

// ✅ Rich error details
import "google.golang.org/genproto/googleapis/rpc/errdetails"

func (s *userService) CreateUser(ctx context.Context, req *userv1.CreateUserRequest) (*userv1.User, error) {
    // Validation
    violations := validateCreateUserRequest(req)
    if len(violations) > 0 {
        st := status.New(codes.InvalidArgument, "validation failed")
        br := &errdetails.BadRequest{}
        
        for _, v := range violations {
            br.FieldViolations = append(br.FieldViolations, &errdetails.BadRequest_FieldViolation{
                Field:       v.Field,
                Description: v.Description,
            })
        }
        
        st, _ = st.WithDetails(br)
        return nil, st.Err()
    }
    
    // Create user...
}
```

## API Versioning

### URL Versioning

**Good:**
```typescript
// ✅ Version in URL path
app.use('/api/v1', v1Router);
app.use('/api/v2', v2Router);

// v1/users.ts
v1Router.get('/users', async (req, res) => {
  // Old implementation
});

// v2/users.ts
v2Router.get('/users', async (req, res) => {
  // New implementation with breaking changes
});
```

### Header Versioning

**Good:**
```typescript
// ✅ Version in Accept header
app.use('/api/users', async (req, res) => {
  const version = req.headers['accept']?.includes('v2') ? 'v2' : 'v1';
  
  if (version === 'v2') {
    return v2Handler(req, res);
  }
  
  return v1Handler(req, res);
});

// Client request:
// Accept: application/vnd.api+json; version=2
```

## Rate Limiting

**Good:**
```typescript
// ✅ Rate limiting with express-rate-limit
import rateLimit from 'express-rate-limit';

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per windowMs
  standardHeaders: true, // Return rate limit info in `RateLimit-*` headers
  legacyHeaders: false,
  message: 'Too many requests, please try again later',
  handler: (req, res) => {
    res.status(429).json({
      error: 'Rate limit exceeded',
      retryAfter: req.rateLimit.resetTime
    });
  }
});

app.use('/api/', limiter);

// ✅ Different limits for different endpoints
const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 5, // 5 login attempts per 15 minutes
  skipSuccessfulRequests: true
});

app.post('/api/auth/login', authLimiter, loginHandler);
```

## API Documentation

### OpenAPI/Swagger

**Good:**
```typescript
// ✅ Comprehensive OpenAPI spec
import swaggerJsdoc from 'swagger-jsdoc';
import swaggerUi from 'swagger-ui-express';

const swaggerOptions = {
  definition: {
    openapi: '3.0.0',
    info: {
      title: 'User API',
      version: '1.0.0',
      description: 'User management API',
      contact: {
        name: 'API Support',
        email: 'support@example.com'
      }
    },
    servers: [
      {
        url: 'https://api.example.com',
        description: 'Production'
      },
      {
        url: 'http://localhost:3000',
        description: 'Development'
      }
    ],
    components: {
      securitySchemes: {
        bearerAuth: {
          type: 'http',
          scheme: 'bearer',
          bearerFormat: 'JWT'
        }
      }
    },
    security: [{
      bearerAuth: []
    }]
  },
  apis: ['./routes/*.ts']
};

const specs = swaggerJsdoc(swaggerOptions);
app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(specs));

/**
 * @openapi
 * /api/users/{id}:
 *   get:
 *     summary: Get user by ID
 *     tags: [Users]
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: User ID
 *     responses:
 *       200:
 *         description: User found
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/User'
 *       404:
 *         description: User not found
 *       401:
 *         description: Unauthorized
 */
app.get('/api/users/:id', getUserHandler);
```
