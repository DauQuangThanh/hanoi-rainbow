# Microservices Architecture

This reference covers microservices patterns, service communication, API gateways, distributed tracing, and best practices for building scalable distributed systems.

## Service Communication Patterns

### Synchronous Communication (HTTP/REST)

```typescript
// Service A calling Service B
import axios, { AxiosInstance } from 'axios';
import CircuitBreaker from 'opossum';

export class UserServiceClient {
  private client: AxiosInstance;
  private circuitBreaker: CircuitBreaker;

  constructor(baseURL: string) {
    this.client = axios.create({
      baseURL,
      timeout: 5000,
      headers: {
        'Content-Type': 'application/json'
      }
    });

    // Add request interceptor for tracing
    this.client.interceptors.request.use((config) => {
      // Add correlation ID for distributed tracing
      config.headers['X-Correlation-ID'] = 
        config.headers['X-Correlation-ID'] || this.generateCorrelationId();
      return config;
    });

    // Circuit breaker configuration
    const options = {
      timeout: 5000, // If function takes longer than 5s, trigger failure
      errorThresholdPercentage: 50, // When 50% of requests fail, open circuit
      resetTimeout: 30000 // After 30s, try again
    };

    this.circuitBreaker = new CircuitBreaker(
      this.makeRequest.bind(this),
      options
    );

    // Circuit breaker events
    this.circuitBreaker.on('open', () => {
      console.log('Circuit breaker opened - too many failures');
    });

    this.circuitBreaker.on('halfOpen', () => {
      console.log('Circuit breaker half-open - trying request');
    });

    this.circuitBreaker.on('close', () => {
      console.log('Circuit breaker closed - service recovered');
    });
  }

  private generateCorrelationId(): string {
    return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }

  private async makeRequest<T>(
    method: string,
    url: string,
    data?: any
  ): Promise<T> {
    const response = await this.client.request<T>({
      method,
      url,
      data
    });
    return response.data;
  }

  async getUser(userId: string): Promise<User> {
    try {
      return await this.circuitBreaker.fire('GET', `/users/${userId}`);
    } catch (error) {
      // Circuit is open, return cached data or default
      console.error('Failed to fetch user', error);
      throw error;
    }
  }

  async createUser(userData: CreateUserDto): Promise<User> {
    return this.circuitBreaker.fire('POST', '/users', userData);
  }

  async updateUser(userId: string, userData: UpdateUserDto): Promise<User> {
    return this.circuitBreaker.fire('PUT', `/users/${userId}`, userData);
  }
}

// Retry with exponential backoff
async function fetchWithRetry<T>(
  fn: () => Promise<T>,
  maxRetries: number = 3,
  baseDelay: number = 1000
): Promise<T> {
  let lastError: Error;

  for (let attempt = 0; attempt < maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error as Error;
      
      if (attempt < maxRetries - 1) {
        const delay = baseDelay * Math.pow(2, attempt);
        const jitter = Math.random() * 1000;
        await new Promise(resolve => setTimeout(resolve, delay + jitter));
      }
    }
  }

  throw lastError!;
}
```

### gRPC Communication

```protobuf
// user.proto
syntax = "proto3";

package user;

service UserService {
  rpc GetUser (GetUserRequest) returns (User);
  rpc CreateUser (CreateUserRequest) returns (User);
  rpc UpdateUser (UpdateUserRequest) returns (User);
  rpc DeleteUser (DeleteUserRequest) returns (Empty);
  rpc ListUsers (ListUsersRequest) returns (stream User);
}

message User {
  string id = 1;
  string email = 2;
  string name = 3;
  string role = 4;
  int64 created_at = 5;
}

message GetUserRequest {
  string id = 1;
}

message CreateUserRequest {
  string email = 1;
  string name = 2;
  string password = 3;
}

message UpdateUserRequest {
  string id = 1;
  optional string name = 2;
  optional string email = 3;
}

message DeleteUserRequest {
  string id = 1;
}

message ListUsersRequest {
  int32 page = 1;
  int32 limit = 2;
}

message Empty {}
```

```typescript
// gRPC server
import * as grpc from '@grpc/grpc-js';
import * as protoLoader from '@grpc/proto-loader';

const PROTO_PATH = './proto/user.proto';

const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true
});

const userProto = grpc.loadPackageDefinition(packageDefinition).user as any;

// Service implementation
const userService = {
  async getUser(call: any, callback: any) {
    try {
      const user = await userRepository.findById(call.request.id);
      if (!user) {
        return callback({
          code: grpc.status.NOT_FOUND,
          message: 'User not found'
        });
      }
      callback(null, user);
    } catch (error) {
      callback({
        code: grpc.status.INTERNAL,
        message: 'Internal server error'
      });
    }
  },

  async createUser(call: any, callback: any) {
    try {
      const user = await userRepository.create(call.request);
      callback(null, user);
    } catch (error) {
      callback({
        code: grpc.status.INVALID_ARGUMENT,
        message: error.message
      });
    }
  },

  async listUsers(call: any) {
    try {
      const users = await userRepository.findAll({
        page: call.request.page,
        limit: call.request.limit
      });

      for (const user of users) {
        call.write(user);
      }
      call.end();
    } catch (error) {
      call.destroy(new Error('Failed to list users'));
    }
  }
};

// Start server
const server = new grpc.Server();
server.addService(userProto.UserService.service, userService);

server.bindAsync(
  '0.0.0.0:50051',
  grpc.ServerCredentials.createInsecure(),
  (error, port) => {
    if (error) {
      console.error('Failed to start server:', error);
      return;
    }
    console.log(`gRPC server running on port ${port}`);
    server.start();
  }
);

// gRPC client
export class UserServiceGrpcClient {
  private client: any;

  constructor(address: string) {
    this.client = new userProto.UserService(
      address,
      grpc.credentials.createInsecure()
    );
  }

  async getUser(userId: string): Promise<User> {
    return new Promise((resolve, reject) => {
      this.client.getUser({ id: userId }, (error: any, response: any) => {
        if (error) {
          reject(error);
        } else {
          resolve(response);
        }
      });
    });
  }

  async createUser(userData: CreateUserDto): Promise<User> {
    return new Promise((resolve, reject) => {
      this.client.createUser(userData, (error: any, response: any) => {
        if (error) {
          reject(error);
        } else {
          resolve(response);
        }
      });
    });
  }

  listUsers(page: number, limit: number): AsyncIterable<User> {
    const call = this.client.listUsers({ page, limit });
    
    return {
      [Symbol.asyncIterator]: async function* () {
        for await (const user of call) {
          yield user;
        }
      }
    };
  }
}
```

### Asynchronous Communication (Message Queues)

```typescript
// RabbitMQ publisher
import amqp, { Connection, Channel } from 'amqplib';

export class MessageQueueService {
  private connection: Connection | null = null;
  private channel: Channel | null = null;

  async connect(): Promise<void> {
    this.connection = await amqp.connect(process.env.RABBITMQ_URL || 'amqp://localhost');
    this.channel = await this.connection.createChannel();
  }

  async publishToQueue(queue: string, message: any): Promise<void> {
    if (!this.channel) {
      await this.connect();
    }

    await this.channel!.assertQueue(queue, { durable: true });
    
    this.channel!.sendToQueue(
      queue,
      Buffer.from(JSON.stringify(message)),
      { persistent: true }
    );
  }

  async publishToExchange(
    exchange: string,
    routingKey: string,
    message: any,
    exchangeType: string = 'topic'
  ): Promise<void> {
    if (!this.channel) {
      await this.connect();
    }

    await this.channel!.assertExchange(exchange, exchangeType, { durable: true });
    
    this.channel!.publish(
      exchange,
      routingKey,
      Buffer.from(JSON.stringify(message)),
      { persistent: true }
    );
  }

  async consumeQueue(
    queue: string,
    handler: (message: any) => Promise<void>
  ): Promise<void> {
    if (!this.channel) {
      await this.connect();
    }

    await this.channel!.assertQueue(queue, { durable: true });
    await this.channel!.prefetch(1); // Process one message at a time

    this.channel!.consume(queue, async (msg) => {
      if (msg) {
        try {
          const content = JSON.parse(msg.content.toString());
          await handler(content);
          this.channel!.ack(msg);
        } catch (error) {
          console.error('Failed to process message:', error);
          // Reject and requeue
          this.channel!.nack(msg, false, true);
        }
      }
    });
  }

  async close(): Promise<void> {
    await this.channel?.close();
    await this.connection?.close();
  }
}

// Event-driven architecture
interface DomainEvent {
  eventId: string;
  eventType: string;
  aggregateId: string;
  timestamp: Date;
  data: any;
}

export class EventPublisher {
  constructor(private messageQueue: MessageQueueService) {}

  async publish(event: DomainEvent): Promise<void> {
    await this.messageQueue.publishToExchange(
      'domain-events',
      event.eventType,
      event
    );
  }
}

export class EventSubscriber {
  constructor(private messageQueue: MessageQueueService) {}

  async subscribe(
    eventTypes: string[],
    handler: (event: DomainEvent) => Promise<void>
  ): Promise<void> {
    const queueName = `subscriber-${Date.now()}`;
    
    for (const eventType of eventTypes) {
      await this.messageQueue.channel!.bindQueue(
        queueName,
        'domain-events',
        eventType
      );
    }

    await this.messageQueue.consumeQueue(queueName, handler);
  }
}

// Usage
const messageQueue = new MessageQueueService();
await messageQueue.connect();

const eventPublisher = new EventPublisher(messageQueue);

// User service publishes event
await eventPublisher.publish({
  eventId: '123',
  eventType: 'user.created',
  aggregateId: user.id,
  timestamp: new Date(),
  data: { email: user.email, name: user.name }
});

// Email service subscribes to user events
const eventSubscriber = new EventSubscriber(messageQueue);
await eventSubscriber.subscribe(['user.created'], async (event) => {
  console.log('Received event:', event);
  // Send welcome email
  await emailService.sendWelcomeEmail(event.data.email);
});
```

## API Gateway

```typescript
// Express-based API Gateway
import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import rateLimit from 'express-rate-limit';
import helmet from 'helmet';

const app = express();

// Security
app.use(helmet());

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per window
  message: 'Too many requests from this IP'
});
app.use('/api', limiter);

// Authentication middleware
app.use('/api', async (req, res, next) => {
  const token = req.headers.authorization?.replace('Bearer ', '');
  
  if (!token) {
    return res.status(401).json({ error: 'No token provided' });
  }

  try {
    const user = await verifyToken(token);
    req.user = user;
    next();
  } catch (error) {
    return res.status(401).json({ error: 'Invalid token' });
  }
});

// Service routing with load balancing
const userServiceTargets = [
  'http://user-service-1:3001',
  'http://user-service-2:3001',
  'http://user-service-3:3001'
];

let userServiceIndex = 0;

app.use('/api/users', createProxyMiddleware({
  target: userServiceTargets[0],
  changeOrigin: true,
  pathRewrite: {
    '^/api/users': '/users'
  },
  router: (req) => {
    // Round-robin load balancing
    const target = userServiceTargets[userServiceIndex];
    userServiceIndex = (userServiceIndex + 1) % userServiceTargets.length;
    return target;
  },
  onProxyReq: (proxyReq, req, res) => {
    // Forward user information
    if (req.user) {
      proxyReq.setHeader('X-User-ID', req.user.id);
      proxyReq.setHeader('X-User-Role', req.user.role);
    }
    
    // Add correlation ID for tracing
    const correlationId = req.headers['x-correlation-id'] || generateId();
    proxyReq.setHeader('X-Correlation-ID', correlationId);
  },
  onError: (err, req, res) => {
    console.error('Proxy error:', err);
    res.status(503).json({ error: 'Service unavailable' });
  }
}));

app.use('/api/posts', createProxyMiddleware({
  target: 'http://post-service:3002',
  changeOrigin: true,
  pathRewrite: {
    '^/api/posts': '/posts'
  }
}));

// Request aggregation
app.get('/api/user-dashboard/:userId', async (req, res) => {
  const { userId } = req.params;

  try {
    // Fetch data from multiple services in parallel
    const [user, posts, comments] = await Promise.all([
      userServiceClient.getUser(userId),
      postServiceClient.getUserPosts(userId),
      commentServiceClient.getUserComments(userId)
    ]);

    res.json({
      user,
      posts,
      comments
    });
  } catch (error) {
    res.status(500).json({ error: 'Failed to fetch dashboard data' });
  }
});

// Health check aggregation
app.get('/health', async (req, res) => {
  const services = [
    { name: 'user-service', url: 'http://user-service:3001/health' },
    { name: 'post-service', url: 'http://post-service:3002/health' },
    { name: 'comment-service', url: 'http://comment-service:3003/health' }
  ];

  const results = await Promise.allSettled(
    services.map(async (service) => {
      const response = await axios.get(service.url, { timeout: 2000 });
      return { name: service.name, status: 'healthy', data: response.data };
    })
  );

  const health = results.map((result, index) => {
    if (result.status === 'fulfilled') {
      return result.value;
    } else {
      return { 
        name: services[index].name, 
        status: 'unhealthy', 
        error: result.reason.message 
      };
    }
  });

  const allHealthy = health.every(h => h.status === 'healthy');
  const statusCode = allHealthy ? 200 : 503;

  res.status(statusCode).json({ services: health });
});

app.listen(3000, () => {
  console.log('API Gateway running on port 3000');
});
```

## Service Discovery

```typescript
// Consul-based service discovery
import Consul from 'consul';

export class ServiceRegistry {
  private consul: Consul.Consul;

  constructor() {
    this.consul = new Consul({
      host: process.env.CONSUL_HOST || 'localhost',
      port: process.env.CONSUL_PORT || '8500'
    });
  }

  async registerService(
    serviceName: string,
    serviceId: string,
    port: number,
    health: string = '/health'
  ): Promise<void> {
    await this.consul.agent.service.register({
      id: serviceId,
      name: serviceName,
      address: process.env.SERVICE_HOST || 'localhost',
      port,
      check: {
        http: `http://${process.env.SERVICE_HOST}:${port}${health}`,
        interval: '10s',
        timeout: '5s'
      },
      tags: [process.env.NODE_ENV || 'development']
    });

    console.log(`Service ${serviceName} registered with ID ${serviceId}`);
  }

  async deregisterService(serviceId: string): Promise<void> {
    await this.consul.agent.service.deregister(serviceId);
    console.log(`Service ${serviceId} deregistered`);
  }

  async discoverService(serviceName: string): Promise<string[]> {
    const result = await this.consul.health.service({
      service: serviceName,
      passing: true
    });

    return result.map((entry: any) => {
      const { Address, Port } = entry.Service;
      return `http://${Address}:${Port}`;
    });
  }

  async getServiceInstance(serviceName: string): Promise<string> {
    const instances = await this.discoverService(serviceName);
    
    if (instances.length === 0) {
      throw new Error(`No healthy instances of ${serviceName} found`);
    }

    // Random selection (simple load balancing)
    const index = Math.floor(Math.random() * instances.length);
    return instances[index];
  }
}

// Usage in service
const registry = new ServiceRegistry();

const serviceId = `user-service-${process.env.HOSTNAME || 'local'}`;
await registry.registerService('user-service', serviceId, 3001);

// Graceful shutdown
process.on('SIGTERM', async () => {
  await registry.deregisterService(serviceId);
  process.exit(0);
});

// Client with service discovery
export class DiscoverableHttpClient {
  constructor(
    private serviceName: string,
    private registry: ServiceRegistry
  ) {}

  async request<T>(
    method: string,
    path: string,
    data?: any
  ): Promise<T> {
    const serviceUrl = await this.registry.getServiceInstance(this.serviceName);
    
    const response = await axios.request<T>({
      method,
      url: `${serviceUrl}${path}`,
      data,
      timeout: 5000
    });

    return response.data;
  }
}

const userClient = new DiscoverableHttpClient('user-service', registry);
const user = await userClient.request('GET', '/users/123');
```

## Distributed Tracing

```typescript
// OpenTelemetry setup
import { NodeSDK } from '@opentelemetry/sdk-node';
import { HttpInstrumentation } from '@opentelemetry/instrumentation-http';
import { ExpressInstrumentation } from '@opentelemetry/instrumentation-express';
import { JaegerExporter } from '@opentelemetry/exporter-jaeger';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import { trace, context, SpanStatusCode } from '@opentelemetry/api';

// Initialize tracing
const sdk = new NodeSDK({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: 'user-service',
    [SemanticResourceAttributes.SERVICE_VERSION]: '1.0.0'
  }),
  traceExporter: new JaegerExporter({
    endpoint: process.env.JAEGER_ENDPOINT || 'http://localhost:14268/api/traces'
  }),
  instrumentations: [
    new HttpInstrumentation(),
    new ExpressInstrumentation()
  ]
});

sdk.start();

// Graceful shutdown
process.on('SIGTERM', () => {
  sdk.shutdown()
    .then(() => console.log('Tracing terminated'))
    .catch((error) => console.log('Error terminating tracing', error))
    .finally(() => process.exit(0));
});

// Custom spans
export class UserService {
  private tracer = trace.getTracer('user-service');

  async createUser(userData: CreateUserDto): Promise<User> {
    const span = this.tracer.startSpan('createUser');
    
    try {
      // Add attributes to span
      span.setAttribute('user.email', userData.email);
      span.setAttribute('user.role', userData.role || 'user');

      // Validate data
      const validationSpan = this.tracer.startSpan('validateUser', {
        parent: span
      });
      await this.validateUserData(userData);
      validationSpan.end();

      // Save to database
      const dbSpan = this.tracer.startSpan('saveUser', {
        parent: span
      });
      const user = await this.userRepository.create(userData);
      dbSpan.setAttribute('db.operation', 'insert');
      dbSpan.setAttribute('db.table', 'users');
      dbSpan.end();

      // Publish event
      const eventSpan = this.tracer.startSpan('publishUserCreatedEvent', {
        parent: span
      });
      await this.eventPublisher.publish({
        eventType: 'user.created',
        aggregateId: user.id,
        data: user
      });
      eventSpan.end();

      span.setStatus({ code: SpanStatusCode.OK });
      return user;
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      });
      throw error;
    } finally {
      span.end();
    }
  }

  async getUserWithPosts(userId: string): Promise<UserWithPosts> {
    const span = this.tracer.startSpan('getUserWithPosts');
    span.setAttribute('user.id', userId);

    try {
      // Fetch user
      const userSpan = this.tracer.startSpan('fetchUser', { parent: span });
      const user = await this.userRepository.findById(userId);
      userSpan.end();

      if (!user) {
        throw new NotFoundError('User not found');
      }

      // Fetch posts from another service
      const postsSpan = this.tracer.startSpan('fetchUserPosts', { parent: span });
      const posts = await this.postServiceClient.getUserPosts(userId);
      postsSpan.setAttribute('posts.count', posts.length);
      postsSpan.end();

      span.setStatus({ code: SpanStatusCode.OK });
      return { ...user, posts };
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      });
      throw error;
    } finally {
      span.end();
    }
  }
}

// Middleware to extract trace context
export function tracingMiddleware(req: Request, res: Response, next: NextFunction) {
  // Extract correlation ID from headers
  const correlationId = req.headers['x-correlation-id'] as string || generateId();
  
  // Add to request for later use
  req.correlationId = correlationId;
  
  // Add to response headers
  res.setHeader('X-Correlation-ID', correlationId);
  
  // Add to active span
  const span = trace.getActiveSpan();
  if (span) {
    span.setAttribute('correlation.id', correlationId);
    span.setAttribute('http.method', req.method);
    span.setAttribute('http.url', req.url);
  }
  
  next();
}
```

## Saga Pattern (Distributed Transactions)

```typescript
// Orchestration-based saga
interface SagaStep {
  execute: () => Promise<any>;
  compensate: () => Promise<void>;
}

export class SagaOrchestrator {
  private steps: SagaStep[] = [];
  private executedSteps: SagaStep[] = [];

  addStep(step: SagaStep): void {
    this.steps.push(step);
  }

  async execute(): Promise<void> {
    try {
      for (const step of this.steps) {
        const result = await step.execute();
        this.executedSteps.push(step);
        console.log('Step executed successfully');
      }
    } catch (error) {
      console.error('Saga failed, starting compensation', error);
      await this.compensate();
      throw error;
    }
  }

  private async compensate(): Promise<void> {
    // Compensate in reverse order
    for (const step of this.executedSteps.reverse()) {
      try {
        await step.compensate();
        console.log('Step compensated');
      } catch (error) {
        console.error('Compensation failed', error);
        // Continue compensating other steps
      }
    }
  }
}

// Example: Order creation saga
export class OrderCreationSaga {
  constructor(
    private orderService: OrderService,
    private inventoryService: InventoryService,
    private paymentService: PaymentService,
    private notificationService: NotificationService
  ) {}

  async execute(orderData: CreateOrderDto): Promise<Order> {
    const saga = new SagaOrchestrator();
    let orderId: string;
    let reservationId: string;
    let paymentId: string;

    // Step 1: Create order
    saga.addStep({
      execute: async () => {
        const order = await this.orderService.createOrder(orderData);
        orderId = order.id;
        return order;
      },
      compensate: async () => {
        await this.orderService.cancelOrder(orderId);
      }
    });

    // Step 2: Reserve inventory
    saga.addStep({
      execute: async () => {
        const reservation = await this.inventoryService.reserveItems(
          orderData.items
        );
        reservationId = reservation.id;
        return reservation;
      },
      compensate: async () => {
        await this.inventoryService.releaseReservation(reservationId);
      }
    });

    // Step 3: Process payment
    saga.addStep({
      execute: async () => {
        const payment = await this.paymentService.processPayment({
          orderId,
          amount: orderData.totalAmount,
          customerId: orderData.customerId
        });
        paymentId = payment.id;
        return payment;
      },
      compensate: async () => {
        await this.paymentService.refund(paymentId);
      }
    });

    // Step 4: Send notification
    saga.addStep({
      execute: async () => {
        await this.notificationService.sendOrderConfirmation(orderId);
      },
      compensate: async () => {
        // Notification doesn't need compensation
      }
    });

    await saga.execute();
    
    const order = await this.orderService.getOrder(orderId);
    return order;
  }
}

// Choreography-based saga (event-driven)
export class OrderService {
  constructor(private eventPublisher: EventPublisher) {}

  async createOrder(orderData: CreateOrderDto): Promise<Order> {
    const order = await this.orderRepository.create({
      ...orderData,
      status: 'pending'
    });

    // Publish event
    await this.eventPublisher.publish({
      eventType: 'order.created',
      aggregateId: order.id,
      data: order
    });

    return order;
  }

  async handleInventoryReserved(event: DomainEvent): Promise<void> {
    const { orderId } = event.data;
    
    await this.orderRepository.update(orderId, {
      status: 'inventory_reserved'
    });

    // Request payment
    await this.eventPublisher.publish({
      eventType: 'payment.requested',
      aggregateId: orderId,
      data: { orderId, amount: event.data.amount }
    });
  }

  async handlePaymentProcessed(event: DomainEvent): Promise<void> {
    const { orderId } = event.data;
    
    await this.orderRepository.update(orderId, {
      status: 'completed',
      paymentId: event.data.paymentId
    });

    // Send confirmation
    await this.eventPublisher.publish({
      eventType: 'order.completed',
      aggregateId: orderId,
      data: { orderId }
    });
  }

  async handlePaymentFailed(event: DomainEvent): Promise<void> {
    const { orderId } = event.data;
    
    // Release inventory
    await this.eventPublisher.publish({
      eventType: 'inventory.release_requested',
      aggregateId: orderId,
      data: { orderId }
    });

    await this.orderRepository.update(orderId, {
      status: 'cancelled',
      cancellationReason: 'payment_failed'
    });
  }
}
```

## Docker Compose for Microservices

```yaml
version: '3.8'

services:
  # API Gateway
  api-gateway:
    build: ./api-gateway
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
      - USER_SERVICE_URL=http://user-service:3001
      - POST_SERVICE_URL=http://post-service:3002
    depends_on:
      - user-service
      - post-service
    networks:
      - microservices

  # User Service
  user-service:
    build: ./user-service
    environment:
      - NODE_ENV=production
      - DB_HOST=postgres
      - DB_PORT=5432
      - DB_NAME=users_db
      - DB_USER=postgres
      - DB_PASSWORD=postgres
      - REDIS_HOST=redis
      - RABBITMQ_URL=amqp://rabbitmq:5672
    depends_on:
      - postgres
      - redis
      - rabbitmq
    deploy:
      replicas: 3
    networks:
      - microservices

  # Post Service
  post-service:
    build: ./post-service
    environment:
      - NODE_ENV=production
      - DB_HOST=postgres
      - DB_PORT=5432
      - DB_NAME=posts_db
      - REDIS_HOST=redis
      - RABBITMQ_URL=amqp://rabbitmq:5672
    depends_on:
      - postgres
      - redis
      - rabbitmq
    deploy:
      replicas: 2
    networks:
      - microservices

  # PostgreSQL
  postgres:
    image: postgres:15-alpine
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
    volumes:
      - postgres-data:/var/lib/postgresql/data
      - ./init-scripts:/docker-entrypoint-initdb.d
    ports:
      - "5432:5432"
    networks:
      - microservices

  # Redis
  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data
    networks:
      - microservices

  # RabbitMQ
  rabbitmq:
    image: rabbitmq:3-management-alpine
    ports:
      - "5672:5672"
      - "15672:15672"
    environment:
      - RABBITMQ_DEFAULT_USER=admin
      - RABBITMQ_DEFAULT_PASS=admin
    volumes:
      - rabbitmq-data:/var/lib/rabbitmq
    networks:
      - microservices

  # Consul (Service Discovery)
  consul:
    image: consul:latest
    ports:
      - "8500:8500"
    networks:
      - microservices

  # Jaeger (Distributed Tracing)
  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "5775:5775/udp"
      - "6831:6831/udp"
      - "6832:6832/udp"
      - "5778:5778"
      - "16686:16686"
      - "14268:14268"
      - "14250:14250"
      - "9411:9411"
    networks:
      - microservices

volumes:
  postgres-data:
  redis-data:
  rabbitmq-data:

networks:
  microservices:
    driver: bridge
```

This reference provides comprehensive patterns and best practices for building microservices architectures with proper communication, resilience, and observability.
