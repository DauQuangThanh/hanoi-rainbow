# Performance Checklist

Guidelines for reviewing backend performance, optimization, and scalability.

## Table of Contents
- [Caching Strategies](#caching-strategies)
- [Database Performance](#database-performance)
- [API Optimization](#api-optimization)
- [Background Jobs](#background-jobs)
- [Memory Management](#memory-management)
- [Profiling and Monitoring](#profiling-and-monitoring)
- [Scalability Patterns](#scalability-patterns)

## Caching Strategies

### Redis Caching

**Good:**
```typescript
// ✅ Implement caching layer
import Redis from 'ioredis';

const redis = new Redis({
  host: process.env.REDIS_HOST,
  port: 6379,
  retryStrategy: (times) => Math.min(times * 50, 2000)
});

async function getUser(userId: string) {
  const cacheKey = `user:${userId}`;
  
  // ✅ Try cache first
  const cached = await redis.get(cacheKey);
  if (cached) {
    return JSON.parse(cached);
  }
  
  // ✅ Fetch from database
  const user = await User.findByPk(userId);
  
  if (user) {
    // ✅ Cache with TTL
    await redis.setex(cacheKey, 3600, JSON.stringify(user));
  }
  
  return user;
}

// ✅ Cache invalidation
async function updateUser(userId: string, data: Partial<User>) {
  const user = await User.update(data, { where: { id: userId } });
  
  // ✅ Invalidate cache
  await redis.del(`user:${userId}`);
  
  return user;
}

// ✅ Batch operations with pipeline
async function getUsersBatch(userIds: string[]) {
  const pipeline = redis.pipeline();
  
  userIds.forEach(id => {
    pipeline.get(`user:${id}`);
  });
  
  const results = await pipeline.exec();
  return results.map(([err, data]) => data ? JSON.parse(data as string) : null);
}
```

### Cache-Aside Pattern

**Good:**
```typescript
// ✅ Cache-aside with automatic refresh
class CacheService {
  private redis: Redis;
  private readonly TTL = 3600;

  async get<T>(
    key: string,
    fetcher: () => Promise<T>,
    ttl: number = this.TTL
  ): Promise<T> {
    // Try cache
    const cached = await this.redis.get(key);
    if (cached) {
      return JSON.parse(cached);
    }
    
    // Fetch from source
    const data = await fetcher();
    
    // Store in cache
    if (data) {
      await this.redis.setex(key, ttl, JSON.stringify(data));
    }
    
    return data;
  }
  
  async invalidate(key: string): Promise<void> {
    await this.redis.del(key);
  }
  
  async invalidatePattern(pattern: string): Promise<void> {
    const keys = await this.redis.keys(pattern);
    if (keys.length > 0) {
      await this.redis.del(...keys);
    }
  }
}

// Usage
const cache = new CacheService();

async function getProduct(id: string) {
  return cache.get(
    `product:${id}`,
    () => Product.findByPk(id),
    3600
  );
}

async function updateProduct(id: string, data: any) {
  const product = await Product.update(data, { where: { id } });
  await cache.invalidate(`product:${id}`);
  return product;
}
```

### Application-Level Caching

**Good:**
```typescript
// ✅ In-memory cache for hot data
import NodeCache from 'node-cache';

const cache = new NodeCache({
  stdTTL: 600,  // 10 minutes default
  checkperiod: 120,  // Check for expired keys every 2 minutes
  useClones: false  // Better performance, be careful with mutations
});

async function getConfig(): Promise<Config> {
  const cached = cache.get<Config>('app-config');
  if (cached) return cached;
  
  const config = await Config.findOne();
  cache.set('app-config', config, 3600);
  return config;
}

// ✅ Memoization for expensive calculations
function memoize<T extends (...args: any[]) => any>(fn: T): T {
  const cache = new Map();
  
  return ((...args: any[]) => {
    const key = JSON.stringify(args);
    if (cache.has(key)) {
      return cache.get(key);
    }
    
    const result = fn(...args);
    cache.set(key, result);
    return result;
  }) as T;
}

const expensiveCalculation = memoize((a: number, b: number) => {
  // Complex calculation
  return Math.pow(a, b);
});
```

## Database Performance

### Connection Pooling

**Good:**
```typescript
// ✅ Proper connection pool configuration
import { Sequelize } from 'sequelize';

const sequelize = new Sequelize({
  dialect: 'postgres',
  host: process.env.DB_HOST,
  pool: {
    max: 20,  // Maximum connections
    min: 5,   // Minimum connections
    acquire: 30000,  // Max time to get connection (ms)
    idle: 10000  // Max idle time before release (ms)
  },
  logging: false,  // ✅ Disable in production
  benchmark: true  // ✅ Log query execution time
});
```

### Query Optimization

**Good:**
```typescript
// ✅ Efficient queries
async function getActiveUsers() {
  return User.findAll({
    where: { status: 'active' },
    attributes: ['id', 'email', 'name'],  // ✅ Select only needed fields
    include: [{
      model: Profile,
      attributes: ['avatar', 'bio']  // ✅ Limit related fields
    }],
    limit: 100,
    order: [['createdAt', 'DESC']]
  });
}

// ✅ Use DataLoader to prevent N+1
import DataLoader from 'dataloader';

const userLoader = new DataLoader(async (userIds: readonly string[]) => {
  const users = await User.findAll({
    where: { id: userIds as string[] }
  });
  
  const userMap = new Map(users.map(u => [u.id, u]));
  return userIds.map(id => userMap.get(id) || null);
});

// Usage in GraphQL resolver
async function posts(_: any, __: any, context: Context) {
  const posts = await Post.findAll();
  
  // ✅ Batches user queries
  return Promise.all(
    posts.map(async post => ({
      ...post.toJSON(),
      author: await context.loaders.user.load(post.authorId)
    }))
  );
}
```

### Batch Operations

**Good:**
```typescript
// ✅ Bulk inserts
async function createUsers(users: UserData[]) {
  return User.bulkCreate(users, {
    validate: true,
    individualHooks: false  // Faster, but skips hooks
  });
}

// ✅ Batch updates
async function activateUsers(userIds: string[]) {
  return User.update(
    { status: 'active' },
    { where: { id: userIds } }
  );
}
```

**Bad:**
```typescript
// ❌ Individual inserts in loop
async function createUsers(users: UserData[]) {
  for (const userData of users) {
    await User.create(userData);  // Each is a separate transaction!
  }
}
```

## API Optimization

### Response Compression

**Good:**
```typescript
// ✅ Enable compression
import compression from 'compression';

app.use(compression({
  level: 6,  // Balance between speed and compression
  threshold: 1024,  // Only compress responses > 1KB
  filter: (req, res) => {
    if (req.headers['x-no-compression']) {
      return false;
    }
    return compression.filter(req, res);
  }
}));
```

### Pagination

**Good:**
```typescript
// ✅ Cursor-based pagination for large datasets
interface PaginationParams {
  limit: number;
  cursor?: string;
}

async function getUsers({ limit = 20, cursor }: PaginationParams) {
  const where: any = {};
  
  if (cursor) {
    // Decode cursor (base64 encoded timestamp)
    const decodedCursor = Buffer.from(cursor, 'base64').toString();
    where.createdAt = { [Op.lt]: decodedCursor };
  }
  
  const users = await User.findAll({
    where,
    limit: limit + 1,  // Fetch one extra to check if there's more
    order: [['createdAt', 'DESC']]
  });
  
  const hasMore = users.length > limit;
  const results = hasMore ? users.slice(0, -1) : users;
  
  const nextCursor = hasMore
    ? Buffer.from(results[results.length - 1].createdAt.toISOString()).toString('base64')
    : null;
  
  return {
    data: results,
    pagination: {
      nextCursor,
      hasMore
    }
  };
}
```

### Field Selection

**Good:**
```typescript
// ✅ Allow clients to select fields
import { Request, Response } from 'express';

async function getUsers(req: Request, res: Response) {
  const fields = req.query.fields as string;
  const attributes = fields ? fields.split(',') : undefined;
  
  const users = await User.findAll({
    attributes,
    limit: 100
  });
  
  return res.json(users);
}

// Client requests: GET /api/users?fields=id,email,name
```

### Partial Responses

**Good:**
```typescript
// ✅ GraphQL-style field selection for REST
interface FieldSelector {
  [key: string]: boolean | FieldSelector;
}

function selectFields<T>(obj: T, selector: FieldSelector): Partial<T> {
  const result: any = {};
  
  for (const [key, value] of Object.entries(selector)) {
    if (key in obj) {
      if (typeof value === 'boolean' && value) {
        result[key] = (obj as any)[key];
      } else if (typeof value === 'object') {
        result[key] = selectFields((obj as any)[key], value);
      }
    }
  }
  
  return result;
}

// Usage
const user = await User.findByPk(userId);
const partial = selectFields(user, {
  id: true,
  email: true,
  profile: {
    avatar: true,
    bio: true
  }
});
```

## Background Jobs

### Bull Queue

**Good:**
```typescript
// ✅ Use queue for long-running tasks
import Bull from 'bull';

const emailQueue = new Bull('email', {
  redis: { host: 'localhost', port: 6379 },
  defaultJobOptions: {
    attempts: 3,
    backoff: {
      type: 'exponential',
      delay: 2000
    },
    removeOnComplete: true,
    removeOnFail: false
  }
});

// Producer
async function sendWelcomeEmail(userId: string) {
  await emailQueue.add('welcome', { userId }, {
    delay: 1000,  // Send after 1 second
    priority: 1   // Higher priority
  });
}

// Consumer
emailQueue.process('welcome', async (job) => {
  const { userId } = job.data;
  const user = await User.findByPk(userId);
  
  await emailService.send({
    to: user.email,
    subject: 'Welcome!',
    template: 'welcome',
    data: { name: user.name }
  });
  
  return { sent: true };
});

// Error handling
emailQueue.on('failed', (job, err) => {
  logger.error('Email job failed', {
    jobId: job.id,
    data: job.data,
    error: err.message
  });
});
```

### Scheduled Jobs

**Good:**
```typescript
// ✅ Cron jobs for scheduled tasks
import cron from 'node-cron';

// Run every day at 2 AM
cron.schedule('0 2 * * *', async () => {
  logger.info('Running daily cleanup job');
  
  try {
    await cleanupExpiredSessions();
    await generateDailyReports();
  } catch (error) {
    logger.error('Daily job failed', { error });
  }
});

// Or use Bull for scheduled jobs
const dailyQueue = new Bull('daily-jobs', {
  redis: { host: 'localhost', port: 6379 }
});

dailyQueue.add('cleanup', {}, {
  repeat: { cron: '0 2 * * *' }
});
```

## Memory Management

### Stream Processing

**Good:**
```typescript
// ✅ Use streams for large files
import { createReadStream, createWriteStream } from 'fs';
import { pipeline } from 'stream/promises';
import { createGzip } from 'zlib';

async function compressFile(inputPath: string, outputPath: string) {
  await pipeline(
    createReadStream(inputPath),
    createGzip(),
    createWriteStream(outputPath)
  );
}

// ✅ Stream CSV processing
import csv from 'csv-parser';

async function processLargeCsv(filePath: string) {
  const results: any[] = [];
  
  await pipeline(
    createReadStream(filePath),
    csv(),
    async function* (source) {
      for await (const row of source) {
        // Process each row
        const processed = await processRow(row);
        yield processed;
      }
    },
    async function (source) {
      for await (const chunk of source) {
        await saveToDB(chunk);
      }
    }
  );
}
```

**Bad:**
```typescript
// ❌ Loading entire file into memory
async function processLargeCsv(filePath: string) {
  const content = await fs.readFile(filePath, 'utf-8');  // Loads entire file!
  const rows = content.split('\n');
  
  for (const row of rows) {
    await processRow(row);
  }
}
```

### Memory Leaks

**Good:**
```typescript
// ✅ Clean up event listeners
import { EventEmitter } from 'events';

class DataProcessor extends EventEmitter {
  private timer?: NodeJS.Timeout;
  
  start() {
    this.timer = setInterval(() => {
      this.emit('data', Date.now());
    }, 1000);
  }
  
  // ✅ Cleanup method
  stop() {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = undefined;
    }
    this.removeAllListeners();
  }
}

// ✅ Limit event listeners
const emitter = new EventEmitter();
emitter.setMaxListeners(20);  // Prevent memory leaks from too many listeners
```

## Profiling and Monitoring

### Application Metrics

**Good:**
```typescript
// ✅ Collect metrics
import client from 'prom-client';

// Create metrics
const httpRequestDuration = new client.Histogram({
  name: 'http_request_duration_seconds',
  help: 'Duration of HTTP requests in seconds',
  labelNames: ['method', 'route', 'status_code']
});

const activeConnections = new client.Gauge({
  name: 'active_connections',
  help: 'Number of active connections'
});

// Middleware to track metrics
app.use((req, res, next) => {
  const start = Date.now();
  
  res.on('finish', () => {
    const duration = (Date.now() - start) / 1000;
    httpRequestDuration
      .labels(req.method, req.route?.path || req.path, res.statusCode.toString())
      .observe(duration);
  });
  
  next();
});

// Expose metrics endpoint
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', client.register.contentType);
  res.end(await client.register.metrics());
});
```

### Performance Monitoring

**Good:**
```typescript
// ✅ APM integration
import * as Sentry from '@sentry/node';

Sentry.init({
  dsn: process.env.SENTRY_DSN,
  tracesSampleRate: 0.1,  // Sample 10% of transactions
  environment: process.env.NODE_ENV
});

// Request handler middleware
app.use(Sentry.Handlers.requestHandler());
app.use(Sentry.Handlers.tracingHandler());

// Custom transaction
async function processOrder(orderId: string) {
  const transaction = Sentry.startTransaction({
    op: 'process-order',
    name: 'Process Order'
  });
  
  try {
    const span1 = transaction.startChild({ op: 'validate' });
    await validateOrder(orderId);
    span1.finish();
    
    const span2 = transaction.startChild({ op: 'charge' });
    await chargePayment(orderId);
    span2.finish();
    
    return { success: true };
  } catch (error) {
    transaction.setStatus('internal_error');
    Sentry.captureException(error);
    throw error;
  } finally {
    transaction.finish();
  }
}
```

## Scalability Patterns

### Horizontal Scaling

**Good:**
```typescript
// ✅ Stateless application design
// Store session in Redis, not in-memory

import session from 'express-session';
import RedisStore from 'connect-redis';

app.use(session({
  store: new RedisStore({ client: redis }),
  secret: process.env.SESSION_SECRET!,
  resave: false,
  saveUninitialized: false,
  cookie: {
    secure: true,
    httpOnly: true,
    maxAge: 1000 * 60 * 60 * 24  // 24 hours
  }
}));
```

### Load Balancing

**Good:**
```typescript
// ✅ Health check endpoint
app.get('/health', async (req, res) => {
  try {
    // Check database connection
    await sequelize.authenticate();
    
    // Check Redis connection
    await redis.ping();
    
    return res.json({
      status: 'healthy',
      timestamp: new Date().toISOString(),
      uptime: process.uptime()
    });
  } catch (error) {
    return res.status(503).json({
      status: 'unhealthy',
      error: error.message
    });
  }
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, shutting down gracefully');
  
  server.close(async () => {
    await sequelize.close();
    await redis.quit();
    process.exit(0);
  });
  
  // Force shutdown after 30 seconds
  setTimeout(() => {
    logger.error('Forced shutdown after timeout');
    process.exit(1);
  }, 30000);
});
```

### Rate Limiting by User

**Good:**
```typescript
// ✅ Per-user rate limiting
import rateLimit from 'express-rate-limit';
import RedisStore from 'rate-limit-redis';

const limiter = rateLimit({
  store: new RedisStore({
    client: redis,
    prefix: 'rate-limit:'
  }),
  windowMs: 15 * 60 * 1000,
  max: async (req) => {
    // Different limits based on user tier
    if (req.user?.tier === 'premium') {
      return 1000;
    }
    return 100;
  },
  keyGenerator: (req) => {
    // Rate limit by user ID if authenticated
    return req.user?.id || req.ip;
  }
});

app.use('/api/', limiter);
```

**Key Performance Principles:**
1. Cache frequently accessed data
2. Use connection pooling
3. Optimize database queries
4. Implement pagination
5. Process tasks asynchronously
6. Monitor and profile regularly
7. Scale horizontally when possible
8. Handle backpressure properly
