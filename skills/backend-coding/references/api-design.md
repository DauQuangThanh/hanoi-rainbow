# API Design and Implementation

This reference covers REST API design, GraphQL implementation, gRPC services, API versioning, documentation, and best practices.

## RESTful API Design

### Resource Naming Conventions

```
Good Examples:
GET    /api/users              # List users
GET    /api/users/{id}         # Get specific user
POST   /api/users              # Create user
PATCH  /api/users/{id}         # Update user
DELETE /api/users/{id}         # Delete user

GET    /api/users/{id}/posts   # Get user's posts
POST   /api/users/{id}/posts   # Create post for user

GET    /api/posts?author={id}  # Query posts by author
GET    /api/posts?sort=created_at&order=desc

Bad Examples:
GET    /api/getUsers           # Avoid verbs in URLs
POST   /api/user/create        # Avoid verbs
GET    /api/users/get/{id}     # Redundant
DELETE /api/deleteUser/{id}    # Inconsistent naming
```

### HTTP Status Codes

```typescript
// Success codes
200 OK                  // Successful GET, PATCH, PUT, DELETE
201 Created            // Successful POST
204 No Content         // Successful DELETE, no response body

// Client error codes
400 Bad Request        // Invalid request data
401 Unauthorized       // Missing or invalid authentication
403 Forbidden          // Authenticated but not authorized
404 Not Found          // Resource doesn't exist
409 Conflict           // Resource conflict (e.g., duplicate email)
422 Unprocessable Entity  // Validation error
429 Too Many Requests  // Rate limit exceeded

// Server error codes
500 Internal Server Error  // Unexpected server error
502 Bad Gateway           // Upstream service error
503 Service Unavailable   // Temporary unavailability
504 Gateway Timeout       // Upstream timeout
```

### Response Format Standards

```typescript
// Success response
interface SuccessResponse<T> {
  data: T;
  meta?: {
    page?: number;
    limit?: number;
    total?: number;
    totalPages?: number;
  };
}

// Error response
interface ErrorResponse {
  error: {
    code: string;
    message: string;
    details?: any[];
  };
}

// Validation error response
interface ValidationErrorResponse {
  error: {
    code: 'VALIDATION_ERROR';
    message: string;
    details: Array<{
      field: string;
      message: string;
      code: string;
    }>;
  };
}

// Example implementations
app.get('/api/users', async (req, res) => {
  const { page = 1, limit = 10 } = req.query;
  const users = await userService.findAll(page, limit);
  const total = await userService.count();

  res.json({
    data: users,
    meta: {
      page: Number(page),
      limit: Number(limit),
      total,
      totalPages: Math.ceil(total / limit)
    }
  });
});

app.post('/api/users', async (req, res) => {
  try {
    const user = await userService.create(req.body);
    res.status(201).json({ data: user });
  } catch (error) {
    if (error instanceof ValidationError) {
      return res.status(422).json({
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Invalid input data',
          details: error.details
        }
      });
    }
    
    if (error instanceof ConflictError) {
      return res.status(409).json({
        error: {
          code: 'RESOURCE_CONFLICT',
          message: error.message
        }
      });
    }

    throw error;
  }
});
```

### Pagination Strategies

```typescript
// Offset-based pagination
interface OffsetPaginationParams {
  page: number;
  limit: number;
}

async function getUsers(params: OffsetPaginationParams) {
  const offset = (params.page - 1) * params.limit;
  
  const [users, total] = await Promise.all([
    db.user.findMany({
      skip: offset,
      take: params.limit,
      orderBy: { createdAt: 'desc' }
    }),
    db.user.count()
  ]);

  return {
    data: users,
    meta: {
      page: params.page,
      limit: params.limit,
      total,
      totalPages: Math.ceil(total / params.limit),
      hasNext: params.page < Math.ceil(total / params.limit),
      hasPrev: params.page > 1
    }
  };
}

// Cursor-based pagination (better for large datasets)
interface CursorPaginationParams {
  cursor?: string;
  limit: number;
}

async function getUsersCursor(params: CursorPaginationParams) {
  const users = await db.user.findMany({
    take: params.limit + 1, // Fetch one extra to check if there are more
    ...(params.cursor && {
      cursor: { id: params.cursor },
      skip: 1 // Skip the cursor itself
    }),
    orderBy: { createdAt: 'desc' }
  });

  const hasMore = users.length > params.limit;
  const data = hasMore ? users.slice(0, -1) : users;

  return {
    data,
    meta: {
      nextCursor: hasMore ? data[data.length - 1].id : null,
      hasMore
    }
  };
}

// Keyset pagination (most efficient)
interface KeysetPaginationParams {
  afterId?: string;
  afterCreatedAt?: Date;
  limit: number;
}

async function getUsersKeyset(params: KeysetPaginationParams) {
  const users = await db.user.findMany({
    take: params.limit + 1,
    where: params.afterId ? {
      OR: [
        {
          createdAt: { gt: params.afterCreatedAt }
        },
        {
          createdAt: params.afterCreatedAt,
          id: { gt: params.afterId }
        }
      ]
    } : undefined,
    orderBy: [
      { createdAt: 'desc' },
      { id: 'desc' }
    ]
  });

  const hasMore = users.length > params.limit;
  const data = hasMore ? users.slice(0, -1) : users;
  const lastItem = data[data.length - 1];

  return {
    data,
    meta: {
      nextCursor: hasMore ? {
        afterId: lastItem.id,
        afterCreatedAt: lastItem.createdAt
      } : null,
      hasMore
    }
  };
}
```

### Filtering, Sorting, and Search

```typescript
interface QueryParams {
  // Filtering
  status?: string;
  role?: string;
  createdAfter?: string;
  createdBefore?: string;
  
  // Sorting
  sortBy?: string;
  order?: 'asc' | 'desc';
  
  // Search
  search?: string;
  
  // Pagination
  page?: number;
  limit?: number;
}

async function getUsers(params: QueryParams) {
  const {
    status,
    role,
    createdAfter,
    createdBefore,
    sortBy = 'createdAt',
    order = 'desc',
    search,
    page = 1,
    limit = 10
  } = params;

  // Build where clause
  const where: any = {};

  if (status) {
    where.status = status;
  }

  if (role) {
    where.role = role;
  }

  if (createdAfter || createdBefore) {
    where.createdAt = {};
    if (createdAfter) where.createdAt.gte = new Date(createdAfter);
    if (createdBefore) where.createdAt.lte = new Date(createdBefore);
  }

  if (search) {
    where.OR = [
      { name: { contains: search, mode: 'insensitive' } },
      { email: { contains: search, mode: 'insensitive' } }
    ];
  }

  // Build order clause
  const orderBy: any = {};
  orderBy[sortBy] = order;

  const offset = (page - 1) * limit;

  const [users, total] = await Promise.all([
    db.user.findMany({
      where,
      orderBy,
      skip: offset,
      take: limit
    }),
    db.user.count({ where })
  ]);

  return {
    data: users,
    meta: {
      page,
      limit,
      total,
      totalPages: Math.ceil(total / limit)
    }
  };
}

// Usage
// GET /api/users?status=active&role=admin&sortBy=name&order=asc&page=1&limit=20
// GET /api/users?search=john&createdAfter=2024-01-01
```

### API Versioning

```typescript
// URL versioning (most common)
app.use('/api/v1/users', usersV1Router);
app.use('/api/v2/users', usersV2Router);

// Header versioning
app.use('/api/users', (req, res, next) => {
  const version = req.headers['api-version'] || '1';
  if (version === '2') {
    return usersV2Router(req, res, next);
  }
  return usersV1Router(req, res, next);
});

// Accept header versioning
app.use('/api/users', (req, res, next) => {
  const accept = req.headers.accept || '';
  if (accept.includes('application/vnd.api.v2+json')) {
    return usersV2Router(req, res, next);
  }
  return usersV1Router(req, res, next);
});

// Query parameter versioning
app.use('/api/users', (req, res, next) => {
  const version = req.query.version || '1';
  if (version === '2') {
    return usersV2Router(req, res, next);
  }
  return usersV1Router(req, res, next);
});
```

## GraphQL Implementation

### Schema Definition

```graphql
# schema.graphql
type User {
  id: ID!
  email: String!
  name: String!
  role: Role!
  posts: [Post!]!
  createdAt: DateTime!
  updatedAt: DateTime!
}

type Post {
  id: ID!
  title: String!
  content: String!
  published: Boolean!
  author: User!
  comments: [Comment!]!
  createdAt: DateTime!
  updatedAt: DateTime!
}

type Comment {
  id: ID!
  content: String!
  author: User!
  post: Post!
  createdAt: DateTime!
}

enum Role {
  USER
  ADMIN
}

type Query {
  user(id: ID!): User
  users(
    page: Int = 1
    limit: Int = 10
    filter: UserFilter
  ): UserConnection!
  
  post(id: ID!): Post
  posts(
    page: Int = 1
    limit: Int = 10
    filter: PostFilter
  ): PostConnection!
}

type Mutation {
  createUser(input: CreateUserInput!): User!
  updateUser(id: ID!, input: UpdateUserInput!): User!
  deleteUser(id: ID!): Boolean!
  
  createPost(input: CreatePostInput!): Post!
  updatePost(id: ID!, input: UpdatePostInput!): Post!
  deletePost(id: ID!): Boolean!
}

type Subscription {
  postCreated: Post!
  postUpdated(id: ID!): Post!
}

input UserFilter {
  role: Role
  search: String
}

input PostFilter {
  published: Boolean
  authorId: ID
}

input CreateUserInput {
  email: String!
  name: String!
  password: String!
}

input UpdateUserInput {
  email: String
  name: String
}

input CreatePostInput {
  title: String!
  content: String!
  published: Boolean
}

input UpdatePostInput {
  title: String
  content: String
  published: Boolean
}

type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
  totalCount: Int!
}

type UserEdge {
  node: User!
  cursor: String!
}

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

scalar DateTime
```

### Resolvers Implementation

```typescript
// resolvers/user.resolvers.ts
import { GraphQLError } from 'graphql';
import { UserService } from '../services/user.service';
import { PostService } from '../services/post.service';

interface Context {
  user?: {
    id: string;
    email: string;
    role: string;
  };
  services: {
    userService: UserService;
    postService: PostService;
  };
}

export const userResolvers = {
  Query: {
    user: async (_: any, { id }: { id: string }, context: Context) => {
      if (!context.user) {
        throw new GraphQLError('Authentication required', {
          extensions: { code: 'UNAUTHENTICATED' }
        });
      }

      return context.services.userService.getUserById(id);
    },

    users: async (
      _: any,
      { page, limit, filter }: { page: number; limit: number; filter?: any },
      context: Context
    ) => {
      if (!context.user) {
        throw new GraphQLError('Authentication required', {
          extensions: { code: 'UNAUTHENTICATED' }
        });
      }

      const result = await context.services.userService.getUsers({
        page,
        limit,
        filter
      });

      return {
        edges: result.data.map((user, index) => ({
          node: user,
          cursor: Buffer.from(`${user.id}`).toString('base64')
        })),
        pageInfo: {
          hasNextPage: page < result.meta.totalPages,
          hasPreviousPage: page > 1,
          startCursor: result.data.length > 0
            ? Buffer.from(`${result.data[0].id}`).toString('base64')
            : null,
          endCursor: result.data.length > 0
            ? Buffer.from(`${result.data[result.data.length - 1].id}`).toString('base64')
            : null
        },
        totalCount: result.meta.total
      };
    }
  },

  Mutation: {
    createUser: async (
      _: any,
      { input }: { input: CreateUserInput },
      context: Context
    ) => {
      return context.services.userService.createUser(input);
    },

    updateUser: async (
      _: any,
      { id, input }: { id: string; input: UpdateUserInput },
      context: Context
    ) => {
      if (!context.user) {
        throw new GraphQLError('Authentication required', {
          extensions: { code: 'UNAUTHENTICATED' }
        });
      }

      // Check authorization
      if (context.user.id !== id && context.user.role !== 'ADMIN') {
        throw new GraphQLError('Not authorized', {
          extensions: { code: 'FORBIDDEN' }
        });
      }

      return context.services.userService.updateUser(id, input);
    },

    deleteUser: async (_: any, { id }: { id: string }, context: Context) => {
      if (!context.user || context.user.role !== 'ADMIN') {
        throw new GraphQLError('Admin access required', {
          extensions: { code: 'FORBIDDEN' }
        });
      }

      await context.services.userService.deleteUser(id);
      return true;
    }
  },

  User: {
    // Field resolver for posts
    posts: async (parent: any, _: any, context: Context) => {
      return context.services.postService.getPostsByAuthor(parent.id);
    }
  }
};

// server.ts
import { ApolloServer } from '@apollo/server';
import { expressMiddleware } from '@apollo/server/express4';
import { readFileSync } from 'fs';
import { userResolvers } from './resolvers/user.resolvers';
import { postResolvers } from './resolvers/post.resolvers';
import { verifyToken } from './utils/auth';

const typeDefs = readFileSync('./schema.graphql', 'utf-8');

const server = new ApolloServer({
  typeDefs,
  resolvers: [userResolvers, postResolvers],
  formatError: (error) => {
    // Custom error formatting
    return {
      message: error.message,
      code: error.extensions?.code || 'INTERNAL_SERVER_ERROR',
      locations: error.locations,
      path: error.path
    };
  }
});

await server.start();

app.use(
  '/graphql',
  express.json(),
  expressMiddleware(server, {
    context: async ({ req }) => {
      let user;
      const token = req.headers.authorization?.replace('Bearer ', '');
      
      if (token) {
        try {
          user = verifyToken(token);
        } catch (error) {
          // Invalid token, but don't throw - let resolvers handle auth
        }
      }

      return {
        user,
        services: {
          userService: new UserService(),
          postService: new PostService()
        }
      };
    }
  })
);
```

### DataLoader for N+1 Prevention

```typescript
import DataLoader from 'dataloader';
import { UserService } from '../services/user.service';
import { PostService } from '../services/post.service';

export function createLoaders() {
  const userLoader = new DataLoader(async (ids: readonly string[]) => {
    const users = await UserService.findByIds(Array.from(ids));
    const userMap = new Map(users.map(user => [user.id, user]));
    return ids.map(id => userMap.get(id) || null);
  });

  const postsByAuthorLoader = new DataLoader(async (authorIds: readonly string[]) => {
    const posts = await PostService.findByAuthorIds(Array.from(authorIds));
    
    // Group posts by author
    const postsByAuthor = new Map<string, any[]>();
    posts.forEach(post => {
      const authorPosts = postsByAuthor.get(post.authorId) || [];
      authorPosts.push(post);
      postsByAuthor.set(post.authorId, authorPosts);
    });

    return authorIds.map(id => postsByAuthor.get(id) || []);
  });

  return {
    userLoader,
    postsByAuthorLoader
  };
}

// Usage in context
app.use('/graphql', expressMiddleware(server, {
  context: async ({ req }) => {
    return {
      user: await getUserFromToken(req),
      loaders: createLoaders()
    };
  }
}));

// In resolver
User: {
  posts: async (parent, _, context) => {
    return context.loaders.postsByAuthorLoader.load(parent.id);
  }
}
```

## gRPC Services

### Protocol Buffer Definition

```protobuf
// user.proto
syntax = "proto3";

package user;

service UserService {
  rpc GetUser(GetUserRequest) returns (UserResponse);
  rpc ListUsers(ListUsersRequest) returns (ListUsersResponse);
  rpc CreateUser(CreateUserRequest) returns (UserResponse);
  rpc UpdateUser(UpdateUserRequest) returns (UserResponse);
  rpc DeleteUser(DeleteUserRequest) returns (DeleteUserResponse);
  
  // Server streaming
  rpc StreamUsers(StreamUsersRequest) returns (stream UserResponse);
  
  // Client streaming
  rpc CreateUsers(stream CreateUserRequest) returns (CreateUsersResponse);
  
  // Bidirectional streaming
  rpc ChatUsers(stream ChatMessage) returns (stream ChatMessage);
}

message User {
  string id = 1;
  string email = 2;
  string name = 3;
  Role role = 4;
  int64 created_at = 5;
  int64 updated_at = 6;
}

enum Role {
  USER = 0;
  ADMIN = 1;
}

message GetUserRequest {
  string id = 1;
}

message ListUsersRequest {
  int32 page = 1;
  int32 limit = 2;
  UserFilter filter = 3;
}

message UserFilter {
  optional Role role = 1;
  optional string search = 2;
}

message ListUsersResponse {
  repeated User users = 1;
  int32 total = 2;
  int32 page = 3;
  int32 total_pages = 4;
}

message CreateUserRequest {
  string email = 1;
  string name = 2;
  string password = 3;
}

message UpdateUserRequest {
  string id = 1;
  optional string email = 2;
  optional string name = 3;
}

message DeleteUserRequest {
  string id = 1;
}

message DeleteUserResponse {
  bool success = 1;
}

message UserResponse {
  User user = 1;
}

message StreamUsersRequest {
  UserFilter filter = 1;
}

message CreateUsersResponse {
  repeated User users = 1;
  int32 count = 2;
}

message ChatMessage {
  string user_id = 1;
  string message = 2;
  int64 timestamp = 3;
}
```

### gRPC Server Implementation

```typescript
// server.ts
import * as grpc from '@grpc/grpc-js';
import * as protoLoader from '@grpc/proto-loader';
import { UserService } from './services/user.service';

const PROTO_PATH = './proto/user.proto';

const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true
});

const userProto = grpc.loadPackageDefinition(packageDefinition).user as any;

const userService = new UserService();

// Unary RPC
async function getUser(
  call: grpc.ServerUnaryCall<any, any>,
  callback: grpc.sendUnaryData<any>
) {
  try {
    const user = await userService.getUserById(call.request.id);
    callback(null, { user });
  } catch (error) {
    callback({
      code: grpc.status.NOT_FOUND,
      message: error.message
    });
  }
}

async function listUsers(
  call: grpc.ServerUnaryCall<any, any>,
  callback: grpc.sendUnaryData<any>
) {
  try {
    const { page = 1, limit = 10, filter } = call.request;
    const result = await userService.listUsers(page, limit, filter);
    
    callback(null, {
      users: result.users,
      total: result.total,
      page,
      total_pages: Math.ceil(result.total / limit)
    });
  } catch (error) {
    callback({
      code: grpc.status.INTERNAL,
      message: error.message
    });
  }
}

// Server streaming RPC
function streamUsers(call: grpc.ServerWritableStream<any, any>) {
  const { filter } = call.request;
  
  userService.streamUsers(filter, (user) => {
    call.write({ user });
  })
    .then(() => call.end())
    .catch(error => {
      call.emit('error', {
        code: grpc.status.INTERNAL,
        message: error.message
      });
    });
}

// Client streaming RPC
function createUsers(
  call: grpc.ServerReadableStream<any, any>,
  callback: grpc.sendUnaryData<any>
) {
  const users: any[] = [];

  call.on('data', async (request) => {
    try {
      const user = await userService.createUser(request);
      users.push(user);
    } catch (error) {
      call.emit('error', {
        code: grpc.status.INTERNAL,
        message: error.message
      });
    }
  });

  call.on('end', () => {
    callback(null, { users, count: users.length });
  });
}

// Bidirectional streaming RPC
function chatUsers(call: grpc.ServerDuplexStream<any, any>) {
  call.on('data', (message) => {
    // Broadcast to all connected clients
    call.write({
      user_id: message.user_id,
      message: message.message,
      timestamp: Date.now()
    });
  });

  call.on('end', () => {
    call.end();
  });
}

const server = new grpc.Server();

server.addService(userProto.UserService.service, {
  getUser,
  listUsers,
  streamUsers,
  createUsers,
  chatUsers
});

const PORT = process.env.GRPC_PORT || 50051;
server.bindAsync(
  `0.0.0.0:${PORT}`,
  grpc.ServerCredentials.createInsecure(),
  (error, port) => {
    if (error) {
      console.error('Failed to start gRPC server:', error);
      return;
    }
    console.log(`gRPC server running on port ${port}`);
    server.start();
  }
);
```

## Rate Limiting

```typescript
// Redis-based rate limiter
import Redis from 'ioredis';
import { Request, Response, NextFunction } from 'express';

export class RateLimiter {
  private redis: Redis;

  constructor() {
    this.redis = new Redis({
      host: process.env.REDIS_HOST || 'localhost',
      port: parseInt(process.env.REDIS_PORT || '6379')
    });
  }

  async checkLimit(
    key: string,
    limit: number,
    windowSeconds: number
  ): Promise<{ allowed: boolean; remaining: number; resetAt: number }> {
    const now = Date.now();
    const windowStart = now - windowSeconds * 1000;

    // Use sorted set to track requests
    const pipeline = this.redis.pipeline();
    
    // Remove old entries
    pipeline.zremrangebyscore(key, 0, windowStart);
    
    // Add current request
    pipeline.zadd(key, now, `${now}`);
    
    // Count requests in window
    pipeline.zcount(key, windowStart, now);
    
    // Set expiration
    pipeline.expire(key, windowSeconds);

    const results = await pipeline.exec();
    const count = results?.[2]?.[1] as number;

    const allowed = count <= limit;
    const resetAt = now + windowSeconds * 1000;

    return {
      allowed,
      remaining: Math.max(0, limit - count),
      resetAt
    };
  }

  middleware(options: {
    limit: number;
    windowSeconds: number;
    keyGenerator?: (req: Request) => string;
  }) {
    return async (req: Request, res: Response, next: NextFunction) => {
      const key = options.keyGenerator
        ? options.keyGenerator(req)
        : `ratelimit:${req.ip}`;

      const result = await this.checkLimit(
        key,
        options.limit,
        options.windowSeconds
      );

      res.setHeader('X-RateLimit-Limit', options.limit);
      res.setHeader('X-RateLimit-Remaining', result.remaining);
      res.setHeader('X-RateLimit-Reset', result.resetAt);

      if (!result.allowed) {
        return res.status(429).json({
          error: {
            code: 'RATE_LIMIT_EXCEEDED',
            message: 'Too many requests, please try again later'
          }
        });
      }

      next();
    };
  }
}

// Usage
const rateLimiter = new RateLimiter();

// Global rate limit
app.use(rateLimiter.middleware({
  limit: 100,
  windowSeconds: 60 // 100 requests per minute
}));

// Per-user rate limit
app.use('/api/', rateLimiter.middleware({
  limit: 1000,
  windowSeconds: 3600, // 1000 requests per hour
  keyGenerator: (req) => `ratelimit:user:${req.user?.id || req.ip}`
}));

// Endpoint-specific rate limit
app.post('/api/auth/login', 
  rateLimiter.middleware({
    limit: 5,
    windowSeconds: 300 // 5 login attempts per 5 minutes
  }),
  loginHandler
);
```

This reference provides comprehensive API design patterns and implementation techniques for modern backend development.
