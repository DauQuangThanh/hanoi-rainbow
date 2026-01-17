# Backend Design Process


Follow this systematic approach when designing backend systems:

### Phase 1: Requirements Analysis

1. **Functional Requirements**
   - Define API endpoints and operations
   - Identify business entities and relationships
   - Map user workflows and data flows
   - Define integration points with external systems
   - Specify background jobs and scheduled tasks

2. **Non-Functional Requirements**
   - **Performance**: Response time (p50, p95, p99), throughput (req/sec)
   - **Scalability**: Expected load (users, requests, data volume)
   - **Availability**: Uptime SLA (99.9%, 99.99%)
   - **Reliability**: Error rate targets, data consistency requirements
   - **Security**: Authentication, authorization, data protection
   - **Compliance**: GDPR, HIPAA, PCI-DSS requirements

3. **Constraints & Assumptions**
   - Technology constraints (language, frameworks, cloud provider)
   - Team expertise and size
   - Budget and timeline
   - Existing systems and dependencies
   - Data residency and regulatory requirements

### Phase 2: API Design

1. **RESTful API Design**

**Resource Modeling**
```
Users:
  GET    /api/v1/users           - List users
  POST   /api/v1/users           - Create user
  GET    /api/v1/users/{id}      - Get user by ID
  PUT    /api/v1/users/{id}      - Update user
  PATCH  /api/v1/users/{id}      - Partial update
  DELETE /api/v1/users/{id}      - Delete user

Nested Resources:
  GET    /api/v1/users/{id}/posts        - Get user's posts
  POST   /api/v1/users/{id}/posts        - Create post for user
  GET    /api/v1/posts/{id}/comments     - Get post comments
```

**HTTP Status Codes**
- `200 OK`: Successful GET, PUT, PATCH
- `201 Created`: Successful POST
- `204 No Content`: Successful DELETE
- `400 Bad Request`: Invalid input
- `401 Unauthorized`: Missing/invalid authentication
- `403 Forbidden`: Authenticated but not authorized
- `404 Not Found`: Resource doesn't exist
- `409 Conflict`: Resource conflict (duplicate)
- `422 Unprocessable Entity`: Validation errors
- `429 Too Many Requests`: Rate limit exceeded
- `500 Internal Server Error`: Server error
- `503 Service Unavailable`: Service down

**Request/Response Format**
```json
// POST /api/v1/users
{
  "email": "user@example.com",
  "name": "John Doe",
  "role": "admin"
}

// Response: 201 Created
{
  "id": "usr_1234567890",
  "email": "user@example.com",
  "name": "John Doe",
  "role": "admin",
  "created_at": "2026-01-14T10:30:00Z",
  "updated_at": "2026-01-14T10:30:00Z"
}

// Error Response: 422 Unprocessable Entity
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Validation failed",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format"
      }
    ]
  }
}
```

2. **API Design Best Practices**

- Use nouns for resources, not verbs
- Use plural nouns (`/users`, not `/user`)
- Use hyphens for multi-word resources (`/order-items`)
- Version your API (`/api/v1/`)
- Support pagination for collections
- Allow filtering, sorting, and searching
- Use consistent naming conventions
- Document with OpenAPI/Swagger
- Implement HATEOAS (optional)

3. **Pagination**
```json
// Request: GET /api/v1/users?page=2&limit=20

// Response
{
  "data": [...],
  "pagination": {
    "page": 2,
    "limit": 20,
    "total": 150,
    "pages": 8
  },
  "links": {
    "self": "/api/v1/users?page=2&limit=20",
    "first": "/api/v1/users?page=1&limit=20",
    "prev": "/api/v1/users?page=1&limit=20",
    "next": "/api/v1/users?page=3&limit=20",
    "last": "/api/v1/users?page=8&limit=20"
  }
}
```

4. **Filtering & Sorting**
```
GET /api/v1/users?status=active&role=admin
GET /api/v1/users?sort=created_at:desc
GET /api/v1/users?search=john
GET /api/v1/users?fields=id,email,name  (sparse fieldsets)
```

### Phase 3: Database Design

1. **Relational Database Design**

**Normalization**
- **1NF**: Eliminate repeating groups, atomic values
- **2NF**: Remove partial dependencies
- **3NF**: Remove transitive dependencies
- **BCNF**: Every determinant is a candidate key

**Example Schema**
```sql
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) NOT NULL UNIQUE,
  password_hash VARCHAR(255) NOT NULL,
  name VARCHAR(255) NOT NULL,
  role VARCHAR(50) NOT NULL DEFAULT 'user',
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
  deleted_at TIMESTAMP,
  
  INDEX idx_email (email),
  INDEX idx_role (role),
  INDEX idx_created_at (created_at)
);

CREATE TABLE posts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  title VARCHAR(500) NOT NULL,
  content TEXT NOT NULL,
  status VARCHAR(20) NOT NULL DEFAULT 'draft',
  published_at TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
  
  INDEX idx_user_id (user_id),
  INDEX idx_status (status),
  INDEX idx_published_at (published_at),
  FULLTEXT INDEX idx_fulltext (title, content)
);

CREATE TABLE comments (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  post_id UUID NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  content TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
  
  INDEX idx_post_id (post_id),
  INDEX idx_user_id (user_id),
  INDEX idx_created_at (created_at)
);
```

**Indexing Strategy**
- Primary key indexes (automatic)
- Foreign key indexes (for joins)
- Columns used in WHERE clauses
- Columns used in ORDER BY
- Composite indexes for multiple columns
- Partial indexes for subset of rows
- Full-text indexes for search

2. **NoSQL Database Design**

**Document Database (MongoDB)**
```javascript
// User document
{
  _id: ObjectId("..."),
  email: "user@example.com",
  name: "John Doe",
  profile: {
    bio: "Developer",
    avatar_url: "https://...",
    social: {
      twitter: "@johndoe",
      github: "johndoe"
    }
  },
  preferences: {
    theme: "dark",
    notifications: true
  },
  created_at: ISODate("2026-01-14T10:30:00Z"),
  updated_at: ISODate("2026-01-14T10:30:00Z")
}

// Embedding vs. Referencing
// Embed: One-to-few, data accessed together
// Reference: One-to-many, many-to-many, frequently updated
```

**Key-Value Store (Redis)**
```
// Session storage
SET session:usr_123 '{"user_id":"usr_123","role":"admin"}' EX 3600

// Caching
SET cache:user:usr_123 '{"name":"John","email":"..."}' EX 300

// Rate limiting
INCR ratelimit:api:usr_123:2026-01-14-10
EXPIRE ratelimit:api:usr_123:2026-01-14-10 3600
```

### Phase 4: Authentication & Authorization

1. **Authentication Strategies**

**JWT-Based Authentication**
```javascript
// Login endpoint
POST /api/v1/auth/login
{
  "email": "user@example.com",
  "password": "password123"
}

// Response
{
  "access_token": "eyJhbGciOiJIUzI1NiIs...",
  "refresh_token": "eyJhbGciOiJIUzI1NiIs...",
  "expires_in": 3600,
  "token_type": "Bearer"
}

// JWT Payload
{
  "sub": "usr_1234567890",
  "email": "user@example.com",
  "role": "admin",
  "iat": 1705228200,
  "exp": 1705231800
}

// Using token
GET /api/v1/users
Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
```

**OAuth 2.0 Flows**
- **Authorization Code**: Web applications
- **PKCE**: Mobile and SPA applications
- **Client Credentials**: Service-to-service
- **Refresh Token**: Long-lived sessions

2. **Authorization Patterns**

**Role-Based Access Control (RBAC)**
```javascript
// Roles and permissions
const roles = {
  admin: ['users:read', 'users:write', 'posts:write', 'posts:delete'],
  editor: ['posts:write', 'posts:read'],
  user: ['posts:read', 'comments:write']
};

// Middleware
function authorize(permission) {
  return (req, res, next) => {
    const userRole = req.user.role;
    const permissions = roles[userRole] || [];
    
    if (permissions.includes(permission)) {
      next();
    } else {
      res.status(403).json({ error: 'Forbidden' });
    }
  };
}

// Usage
app.delete('/api/v1/posts/:id', 
  authenticate,
  authorize('posts:delete'),
  deletePost
);
```

**Attribute-Based Access Control (ABAC)**
```javascript
// More granular control
function canEditPost(user, post) {
  return user.role === 'admin' || 
         (user.role === 'editor' && post.author_id === user.id);
}
```

### Phase 5: Microservices Architecture

1. **Service Decomposition**

**Domain-Driven Design Approach**
```
User Service:
  - User registration and authentication
  - User profile management
  - User preferences

Product Service:
  - Product catalog
  - Product search
  - Inventory management

Order Service:
  - Order creation and management
  - Order history
  - Order status tracking

Payment Service:
  - Payment processing
  - Refunds
  - Payment methods

Notification Service:
  - Email notifications
  - SMS notifications
  - Push notifications
```

2. **Inter-Service Communication**

**Synchronous (REST)**
```javascript
// Order Service calls Product Service
const response = await fetch('http://product-service/api/v1/products/123', {
  method: 'GET',
  headers: {
    'Authorization': `Bearer ${serviceToken}`,
    'X-Request-ID': requestId
  }
});

const product = await response.json();
```

**Asynchronous (Message Queue)**
```javascript
// Order Service publishes event
await messageQueue.publish('order.created', {
  order_id: 'ord_123',
  user_id: 'usr_456',
  total: 99.99,
  items: [...]
});

// Notification Service subscribes
messageQueue.subscribe('order.created', async (event) => {
  await sendOrderConfirmationEmail(event.user_id, event.order_id);
});

// Inventory Service subscribes
messageQueue.subscribe('order.created', async (event) => {
  await decrementInventory(event.items);
});
```

3. **Service Discovery**
```javascript
// Service registry (Consul, Eureka)
const productServiceUrl = await serviceRegistry.discover('product-service');

// With load balancing
const instance = await serviceRegistry.getHealthyInstance('product-service');
```

4. **API Gateway Pattern**
```
Client → API Gateway → Services

API Gateway responsibilities:
- Request routing
- Authentication/Authorization
- Rate limiting
- Request/response transformation
- Caching
- Monitoring and logging
```

### Phase 6: Caching Strategy

1. **Cache Levels**

**Application Cache (In-Memory)**
```javascript
// Simple in-memory cache
const cache = new Map();

function getUser(userId) {
  const cacheKey = `user:${userId}`;
  
  if (cache.has(cacheKey)) {
    return cache.get(cacheKey);
  }
  
  const user = await db.users.findById(userId);
  cache.set(cacheKey, user);
  
  return user;
}
```

**Distributed Cache (Redis)**
```javascript
// Redis caching
async function getUser(userId) {
  const cacheKey = `user:${userId}`;
  
  // Try cache first
  let user = await redis.get(cacheKey);
  
  if (user) {
    return JSON.parse(user);
  }
  
  // Cache miss - fetch from database
  user = await db.users.findById(userId);
  
  // Store in cache (5 minutes TTL)
  await redis.setex(cacheKey, 300, JSON.stringify(user));
  
  return user;
}

// Cache invalidation
async function updateUser(userId, data) {
  await db.users.update(userId, data);
  
  // Invalidate cache
  await redis.del(`user:${userId}`);
}
```

**HTTP Cache (CDN)**
```javascript
// Set cache headers
app.get('/api/v1/posts/:id', (req, res) => {
  const post = getPost(req.params.id);
  
  res.set({
    'Cache-Control': 'public, max-age=300',  // 5 minutes
    'ETag': generateETag(post),
    'Last-Modified': post.updated_at
  });
  
  res.json(post);
});
```

2. **Cache Patterns**

- **Cache-Aside**: Application manages cache
- **Read-Through**: Cache fetches from database on miss
- **Write-Through**: Write to cache and database simultaneously
- **Write-Behind**: Write to cache first, database asynchronously
- **Refresh-Ahead**: Automatically refresh before expiration

3. **Cache Invalidation Strategies**

- **TTL (Time-To-Live)**: Automatic expiration
- **Explicit Invalidation**: Delete on update
- **Event-Based**: Invalidate on specific events
- **Cache Tags**: Group-based invalidation

### Phase 7: Asynchronous Processing

1. **Message Queue Patterns**

**Job Queue (Bull, BullMQ)**
```javascript
// Producer
await queue.add('send-email', {
  to: 'user@example.com',
  subject: 'Welcome',
  template: 'welcome'
}, {
  attempts: 3,
  backoff: {
    type: 'exponential',
    delay: 5000
  }
});

// Consumer
queue.process('send-email', async (job) => {
  const { to, subject, template } = job.data;
  await emailService.send(to, subject, template);
});
```

**Event Streaming (Kafka)**
```javascript
// Producer
await producer.send({
  topic: 'user-events',
  messages: [{
    key: userId,
    value: JSON.stringify({
      type: 'USER_REGISTERED',
      user_id: userId,
      email: email,
      timestamp: Date.now()
    })
  }]
});

// Consumer
await consumer.run({
  eachMessage: async ({ topic, partition, message }) => {
    const event = JSON.parse(message.value);
    
    if (event.type === 'USER_REGISTERED') {
      await handleUserRegistration(event);
    }
  }
});
```

2. **Background Jobs**

- Image processing (resize, optimization)
- Email sending
- Report generation
- Data import/export
- Scheduled cleanup tasks
- Analytics aggregation

### Phase 8: Security Implementation

1. **Input Validation**
```javascript
// Using validation library (Joi, Yup)
const schema = Joi.object({
  email: Joi.string().email().required(),
  password: Joi.string().min(8).required(),
  age: Joi.number().integer().min(18).max(120)
});

const { error, value } = schema.validate(req.body);
if (error) {
  return res.status(422).json({ error: error.details });
}
```

2. **SQL Injection Prevention**
```javascript
// Bad: String concatenation
const query = `SELECT * FROM users WHERE email = '${email}'`;

// Good: Parameterized queries
const query = 'SELECT * FROM users WHERE email = ?';
const [users] = await db.execute(query, [email]);

// Good: ORM
const user = await User.findOne({ where: { email } });
```

3. **Password Security**
```javascript
const bcrypt = require('bcrypt');

// Hashing (on registration)
const hashedPassword = await bcrypt.hash(password, 10);

// Verification (on login)
const isValid = await bcrypt.compare(password, user.password_hash);
```

4. **Rate Limiting**
```javascript
const rateLimit = require('express-rate-limit');

const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // 100 requests per window
  message: 'Too many requests, please try again later'
});

app.use('/api/', limiter);
```

5. **CORS Configuration**
```javascript
const cors = require('cors');

app.use(cors({
  origin: ['https://example.com', 'https://app.example.com'],
  methods: ['GET', 'POST', 'PUT', 'DELETE'],
  allowedHeaders: ['Content-Type', 'Authorization'],
  credentials: true,
  maxAge: 86400
}));
```

### Phase 9: Observability

1. **Structured Logging**
```javascript
const logger = require('pino')();

logger.info({
  event: 'user_created',
  user_id: 'usr_123',
  email: 'user@example.com',
  ip_address: req.ip,
  timestamp: new Date().toISOString()
}, 'User created successfully');

// Correlation ID for request tracking
app.use((req, res, next) => {
  req.id = req.headers['x-request-id'] || uuid();
  logger.child({ request_id: req.id });
  next();
});
```

2. **Metrics**
```javascript
// Prometheus metrics
const promClient = require('prom-client');

const httpRequestDuration = new promClient.Histogram({
  name: 'http_request_duration_seconds',
  help: 'Duration of HTTP requests in seconds',
  labelNames: ['method', 'route', 'status']
});

// Middleware
app.use((req, res, next) => {
  const start = Date.now();
  
  res.on('finish', () => {
    const duration = (Date.now() - start) / 1000;
    httpRequestDuration.labels(req.method, req.route.path, res.statusCode).observe(duration);
  });
  
  next();
});
```

3. **Distributed Tracing**
```javascript
// OpenTelemetry
const { trace } = require('@opentelemetry/api');

const tracer = trace.getTracer('my-service');

async function processOrder(orderId) {
  const span = tracer.startSpan('process_order');
  span.setAttribute('order_id', orderId);
  
  try {
    // Business logic
    await validateOrder(orderId);
    await chargePayment(orderId);
    await updateInventory(orderId);
    
    span.setStatus({ code: SpanStatusCode.OK });
  } catch (error) {
    span.setStatus({ code: SpanStatusCode.ERROR });
    span.recordException(error);
    throw error;
  } finally {
    span.end();
  }
}
```

4. **Health Checks**
```javascript
app.get('/health', async (req, res) => {
  const health = {
    status: 'healthy',
    timestamp: new Date().toISOString(),
    checks: {
      database: await checkDatabase(),
      redis: await checkRedis(),
      messageQueue: await checkMessageQueue()
    }
  };
  
  const isHealthy = Object.values(health.checks).every(c => c.status === 'up');
  const statusCode = isHealthy ? 200 : 503;
  
  res.status(statusCode).json(health);
});
```
