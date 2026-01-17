# Architecture Checklist

Guidelines for reviewing backend architecture, design patterns, and system design.

## Table of Contents
- [Layered Architecture](#layered-architecture)
- [Design Patterns](#design-patterns)
- [Microservices](#microservices)
- [Domain-Driven Design](#domain-driven-design)
- [Clean Architecture](#clean-architecture)
- [Event-Driven Architecture](#event-driven-architecture)
- [Error Handling](#error-handling)

## Layered Architecture

### Three-Tier Architecture

**Good:**
```typescript
// ✅ Proper separation of concerns

// 1. Controller Layer (HTTP handling)
// routes/user.routes.ts
import { Router } from 'express';
import { UserController } from '../controllers/user.controller';

const router = Router();
const controller = new UserController();

router.get('/:id', controller.getUser);
router.post('/', controller.createUser);

export default router;

// 2. Controller (Request/Response)
// controllers/user.controller.ts
export class UserController {
  constructor(private userService: UserService) {}
  
  getUser = async (req: Request, res: Response) => {
    try {
      const user = await this.userService.getUserById(req.params.id);
      return res.json(user);
    } catch (error) {
      return res.status(404).json({ error: error.message });
    }
  };
  
  createUser = async (req: Request, res: Response) => {
    try {
      const user = await this.userService.createUser(req.body);
      return res.status(201).json(user);
    } catch (error) {
      return res.status(400).json({ error: error.message });
    }
  };
}

// 3. Service Layer (Business Logic)
// services/user.service.ts
export class UserService {
  constructor(
    private userRepository: UserRepository,
    private emailService: EmailService
  ) {}
  
  async getUserById(id: string): Promise<User> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }
    return user;
  }
  
  async createUser(data: CreateUserDTO): Promise<User> {
    // Validate
    this.validateUserData(data);
    
    // Check uniqueness
    const existing = await this.userRepository.findByEmail(data.email);
    if (existing) {
      throw new Error('Email already exists');
    }
    
    // Create user
    const user = await this.userRepository.create(data);
    
    // Send welcome email
    await this.emailService.sendWelcome(user.email);
    
    return user;
  }
  
  private validateUserData(data: CreateUserDTO): void {
    if (!data.email || !data.name) {
      throw new Error('Invalid user data');
    }
  }
}

// 4. Repository Layer (Data Access)
// repositories/user.repository.ts
export class UserRepository {
  async findById(id: string): Promise<User | null> {
    return User.findByPk(id);
  }
  
  async findByEmail(email: string): Promise<User | null> {
    return User.findOne({ where: { email } });
  }
  
  async create(data: CreateUserDTO): Promise<User> {
    return User.create(data);
  }
  
  async update(id: string, data: Partial<User>): Promise<User> {
    const user = await this.findById(id);
    if (!user) {
      throw new Error('User not found');
    }
    return user.update(data);
  }
}
```

**Bad:**
```typescript
// ❌ Mixed concerns in controller
app.get('/users/:id', async (req, res) => {
  // ❌ Direct database access in route
  const user = await User.findByPk(req.params.id);
  
  // ❌ Business logic in controller
  if (!user) {
    return res.status(404).json({ error: 'Not found' });
  }
  
  // ❌ External service call in controller
  const posts = await fetch(`https://api.example.com/posts/${user.id}`);
  user.posts = await posts.json();
  
  return res.json(user);
});
```

## Design Patterns

### Repository Pattern

**Good:**
```typescript
// ✅ Generic repository interface
export interface IRepository<T> {
  findById(id: string): Promise<T | null>;
  findAll(filter?: Partial<T>): Promise<T[]>;
  create(data: Partial<T>): Promise<T>;
  update(id: string, data: Partial<T>): Promise<T>;
  delete(id: string): Promise<boolean>;
}

// ✅ Base repository implementation
export abstract class BaseRepository<T extends Model> implements IRepository<T> {
  constructor(protected model: typeof Model) {}
  
  async findById(id: string): Promise<T | null> {
    return this.model.findByPk(id) as Promise<T | null>;
  }
  
  async findAll(filter?: Partial<T>): Promise<T[]> {
    return this.model.findAll({ where: filter as any }) as Promise<T[]>;
  }
  
  async create(data: Partial<T>): Promise<T> {
    return this.model.create(data) as Promise<T>;
  }
  
  async update(id: string, data: Partial<T>): Promise<T> {
    const record = await this.findById(id);
    if (!record) {
      throw new Error('Record not found');
    }
    return record.update(data) as Promise<T>;
  }
  
  async delete(id: string): Promise<boolean> {
    const deleted = await this.model.destroy({ where: { id } as any });
    return deleted > 0;
  }
}

// ✅ Specific repository
export class UserRepository extends BaseRepository<User> {
  constructor() {
    super(User);
  }
  
  async findByEmail(email: string): Promise<User | null> {
    return User.findOne({ where: { email } });
  }
  
  async findActiveUsers(): Promise<User[]> {
    return User.findAll({ where: { status: 'active' } });
  }
}
```

### Dependency Injection

**Good:**
```typescript
// ✅ Dependency injection with InversifyJS
import { injectable, inject } from 'inversify';
import 'reflect-metadata';

// Define service identifiers
const TYPES = {
  UserRepository: Symbol.for('UserRepository'),
  EmailService: Symbol.for('EmailService'),
  UserService: Symbol.for('UserService')
};

// ✅ Injectable classes
@injectable()
export class UserService {
  constructor(
    @inject(TYPES.UserRepository) private userRepo: UserRepository,
    @inject(TYPES.EmailService) private emailService: EmailService
  ) {}
  
  async createUser(data: CreateUserDTO): Promise<User> {
    const user = await this.userRepo.create(data);
    await this.emailService.sendWelcome(user.email);
    return user;
  }
}

// ✅ Container configuration
import { Container } from 'inversify';

const container = new Container();
container.bind<UserRepository>(TYPES.UserRepository).to(UserRepository);
container.bind<EmailService>(TYPES.EmailService).to(EmailService);
container.bind<UserService>(TYPES.UserService).to(UserService);

// ✅ Resolve dependencies
const userService = container.get<UserService>(TYPES.UserService);
```

### Factory Pattern

**Good:**
```typescript
// ✅ Abstract factory for different payment providers
interface PaymentProvider {
  processPayment(amount: number, currency: string): Promise<PaymentResult>;
  refund(transactionId: string): Promise<RefundResult>;
}

class StripeProvider implements PaymentProvider {
  async processPayment(amount: number, currency: string) {
    // Stripe-specific implementation
    const charge = await stripe.charges.create({
      amount: amount * 100,
      currency
    });
    return { success: true, transactionId: charge.id };
  }
  
  async refund(transactionId: string) {
    const refund = await stripe.refunds.create({
      charge: transactionId
    });
    return { success: true, refundId: refund.id };
  }
}

class PayPalProvider implements PaymentProvider {
  async processPayment(amount: number, currency: string) {
    // PayPal-specific implementation
  }
  
  async refund(transactionId: string) {
    // PayPal-specific implementation
  }
}

// ✅ Factory
export class PaymentProviderFactory {
  static create(provider: string): PaymentProvider {
    switch (provider) {
      case 'stripe':
        return new StripeProvider();
      case 'paypal':
        return new PayPalProvider();
      default:
        throw new Error(`Unknown payment provider: ${provider}`);
    }
  }
}

// Usage
const provider = PaymentProviderFactory.create('stripe');
await provider.processPayment(100, 'USD');
```

### Strategy Pattern

**Good:**
```typescript
// ✅ Strategy pattern for different shipping methods
interface ShippingStrategy {
  calculateCost(weight: number, distance: number): number;
  estimateDelivery(): Date;
}

class StandardShipping implements ShippingStrategy {
  calculateCost(weight: number, distance: number): number {
    return weight * 0.5 + distance * 0.1;
  }
  
  estimateDelivery(): Date {
    const date = new Date();
    date.setDate(date.getDate() + 5);
    return date;
  }
}

class ExpressShipping implements ShippingStrategy {
  calculateCost(weight: number, distance: number): number {
    return weight * 1.5 + distance * 0.3;
  }
  
  estimateDelivery(): Date {
    const date = new Date();
    date.setDate(date.getDate() + 2);
    return date;
  }
}

class ShippingContext {
  constructor(private strategy: ShippingStrategy) {}
  
  setStrategy(strategy: ShippingStrategy): void {
    this.strategy = strategy;
  }
  
  calculateShipping(weight: number, distance: number) {
    return {
      cost: this.strategy.calculateCost(weight, distance),
      estimatedDelivery: this.strategy.estimateDelivery()
    };
  }
}

// Usage
const shipping = new ShippingContext(new StandardShipping());
const quote = shipping.calculateShipping(5, 100);
```

## Microservices

### Service Boundaries

**Good:**
```typescript
// ✅ Well-defined service boundaries

// User Service
export class UserService {
  async getUser(id: string): Promise<User> {
    return this.userRepo.findById(id);
  }
  
  async createUser(data: CreateUserDTO): Promise<User> {
    return this.userRepo.create(data);
  }
}

// Order Service (separate service)
export class OrderService {
  constructor(
    private orderRepo: OrderRepository,
    private userServiceClient: UserServiceClient  // HTTP client
  ) {}
  
  async createOrder(userId: string, items: OrderItem[]): Promise<Order> {
    // ✅ Call User Service via API
    const user = await this.userServiceClient.getUser(userId);
    
    if (!user) {
      throw new Error('User not found');
    }
    
    const order = await this.orderRepo.create({
      userId,
      items,
      total: this.calculateTotal(items)
    });
    
    return order;
  }
}

// ✅ Service client
export class UserServiceClient {
  constructor(private baseUrl: string) {}
  
  async getUser(id: string): Promise<User> {
    const response = await fetch(`${this.baseUrl}/users/${id}`);
    if (!response.ok) {
      throw new Error('User service error');
    }
    return response.json();
  }
}
```

### Inter-Service Communication

**Good:**
```typescript
// ✅ Event-driven communication with message broker
import { EventEmitter } from 'events';

// Event definitions
interface UserCreatedEvent {
  userId: string;
  email: string;
  name: string;
  timestamp: Date;
}

// Event publisher
export class EventPublisher {
  constructor(private messageQueue: MessageQueue) {}
  
  async publishUserCreated(user: User): Promise<void> {
    const event: UserCreatedEvent = {
      userId: user.id,
      email: user.email,
      name: user.name,
      timestamp: new Date()
    };
    
    await this.messageQueue.publish('user.created', event);
  }
}

// Event subscriber in another service
export class NotificationService {
  async onUserCreated(event: UserCreatedEvent): Promise<void> {
    await this.emailService.sendWelcome(event.email);
    await this.analyticsService.trackUserSignup(event.userId);
  }
}

// Message queue setup
const queue = new MessageQueue();
queue.subscribe('user.created', notificationService.onUserCreated);
```

### Circuit Breaker Pattern

**Good:**
```typescript
// ✅ Circuit breaker for resilient service calls
import CircuitBreaker from 'opossum';

const options = {
  timeout: 3000,  // 3 seconds
  errorThresholdPercentage: 50,
  resetTimeout: 30000  // 30 seconds
};

const breaker = new CircuitBreaker(async (userId: string) => {
  const response = await fetch(`${USER_SERVICE_URL}/users/${userId}`);
  if (!response.ok) {
    throw new Error('User service error');
  }
  return response.json();
}, options);

// Fallback
breaker.fallback((userId: string) => {
  // Return cached data or default value
  return { id: userId, name: 'Unknown', email: 'unknown@example.com' };
});

// Events
breaker.on('open', () => {
  logger.warn('Circuit breaker opened');
});

breaker.on('halfOpen', () => {
  logger.info('Circuit breaker half-open');
});

// Usage
const user = await breaker.fire(userId);
```

## Domain-Driven Design

### Entities and Value Objects

**Good:**
```typescript
// ✅ Entity (has identity)
export class Order {
  constructor(
    public readonly id: string,
    private items: OrderItem[],
    private status: OrderStatus,
    private createdAt: Date
  ) {}
  
  addItem(item: OrderItem): void {
    if (this.status !== OrderStatus.Draft) {
      throw new Error('Cannot add items to non-draft order');
    }
    this.items.push(item);
  }
  
  calculateTotal(): Money {
    return this.items.reduce(
      (sum, item) => sum.add(item.price.multiply(item.quantity)),
      Money.zero('USD')
    );
  }
  
  submit(): void {
    if (this.items.length === 0) {
      throw new Error('Cannot submit empty order');
    }
    this.status = OrderStatus.Submitted;
  }
}

// ✅ Value Object (no identity, immutable)
export class Money {
  constructor(
    public readonly amount: number,
    public readonly currency: string
  ) {
    if (amount < 0) {
      throw new Error('Amount cannot be negative');
    }
  }
  
  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new Error('Cannot add different currencies');
    }
    return new Money(this.amount + other.amount, this.currency);
  }
  
  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }
  
  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }
  
  static zero(currency: string): Money {
    return new Money(0, currency);
  }
}
```

### Aggregates

**Good:**
```typescript
// ✅ Aggregate root
export class ShoppingCart {
  private items: Map<string, CartItem> = new Map();
  
  constructor(
    public readonly id: string,
    public readonly userId: string,
    private createdAt: Date
  ) {}
  
  addItem(productId: string, quantity: number, price: Money): void {
    const existing = this.items.get(productId);
    
    if (existing) {
      existing.increaseQuantity(quantity);
    } else {
      this.items.set(productId, new CartItem(productId, quantity, price));
    }
  }
  
  removeItem(productId: string): void {
    this.items.delete(productId);
  }
  
  clear(): void {
    this.items.clear();
  }
  
  checkout(): Order {
    if (this.items.size === 0) {
      throw new Error('Cannot checkout empty cart');
    }
    
    const orderItems = Array.from(this.items.values()).map(item =>
      new OrderItem(item.productId, item.quantity, item.price)
    );
    
    return new Order(
      generateId(),
      orderItems,
      OrderStatus.Draft,
      new Date()
    );
  }
  
  getTotal(): Money {
    return Array.from(this.items.values()).reduce(
      (sum, item) => sum.add(item.getSubtotal()),
      Money.zero('USD')
    );
  }
}

// ✅ Entity within aggregate
class CartItem {
  constructor(
    public readonly productId: string,
    private quantity: number,
    public readonly price: Money
  ) {
    if (quantity <= 0) {
      throw new Error('Quantity must be positive');
    }
  }
  
  increaseQuantity(amount: number): void {
    this.quantity += amount;
  }
  
  getSubtotal(): Money {
    return this.price.multiply(this.quantity);
  }
}
```

## Clean Architecture

### Use Cases

**Good:**
```typescript
// ✅ Use case (application business rules)
export class CreateUserUseCase {
  constructor(
    private userRepo: UserRepository,
    private emailService: EmailService,
    private eventBus: EventBus
  ) {}
  
  async execute(request: CreateUserRequest): Promise<CreateUserResponse> {
    // 1. Validate
    this.validateRequest(request);
    
    // 2. Check business rules
    const existingUser = await this.userRepo.findByEmail(request.email);
    if (existingUser) {
      throw new UserAlreadyExistsError(request.email);
    }
    
    // 3. Create entity
    const user = User.create({
      email: request.email,
      name: request.name,
      password: await this.hashPassword(request.password)
    });
    
    // 4. Persist
    await this.userRepo.save(user);
    
    // 5. Send notifications
    await this.emailService.sendWelcome(user.email);
    
    // 6. Publish event
    await this.eventBus.publish(new UserCreatedEvent(user));
    
    return {
      userId: user.id,
      email: user.email,
      name: user.name
    };
  }
  
  private validateRequest(request: CreateUserRequest): void {
    if (!request.email || !request.name || !request.password) {
      throw new ValidationError('Missing required fields');
    }
  }
}
```

### Ports and Adapters

**Good:**
```typescript
// ✅ Port (interface)
export interface IUserRepository {
  save(user: User): Promise<void>;
  findById(id: string): Promise<User | null>;
  findByEmail(email: string): Promise<User | null>;
}

// ✅ Adapter (implementation)
export class PostgresUserRepository implements IUserRepository {
  async save(user: User): Promise<void> {
    await UserModel.upsert({
      id: user.id,
      email: user.email,
      name: user.name,
      passwordHash: user.passwordHash
    });
  }
  
  async findById(id: string): Promise<User | null> {
    const model = await UserModel.findByPk(id);
    return model ? this.toDomain(model) : null;
  }
  
  async findByEmail(email: string): Promise<User | null> {
    const model = await UserModel.findOne({ where: { email } });
    return model ? this.toDomain(model) : null;
  }
  
  private toDomain(model: UserModel): User {
    return new User(
      model.id,
      model.email,
      model.name,
      model.passwordHash
    );
  }
}
```

## Event-Driven Architecture

### Domain Events

**Good:**
```typescript
// ✅ Domain event
export class OrderPlacedEvent {
  constructor(
    public readonly orderId: string,
    public readonly userId: string,
    public readonly total: Money,
    public readonly occurredAt: Date
  ) {}
}

// ✅ Event handler
export class OrderPlacedHandler {
  constructor(
    private inventoryService: InventoryService,
    private emailService: EmailService,
    private analyticsService: AnalyticsService
  ) {}
  
  async handle(event: OrderPlacedEvent): Promise<void> {
    // Handle side effects
    await Promise.all([
      this.inventoryService.reserveItems(event.orderId),
      this.emailService.sendOrderConfirmation(event.userId, event.orderId),
      this.analyticsService.trackOrder(event)
    ]);
  }
}

// ✅ Event bus
export class EventBus {
  private handlers: Map<string, Array<(event: any) => Promise<void>>> = new Map();
  
  subscribe<T>(eventType: string, handler: (event: T) => Promise<void>): void {
    if (!this.handlers.has(eventType)) {
      this.handlers.set(eventType, []);
    }
    this.handlers.get(eventType)!.push(handler);
  }
  
  async publish<T>(event: T): Promise<void> {
    const eventType = event.constructor.name;
    const handlers = this.handlers.get(eventType) || [];
    
    await Promise.all(handlers.map(handler => handler(event)));
  }
}
```

## Error Handling

### Custom Errors

**Good:**
```typescript
// ✅ Domain-specific errors
export class DomainError extends Error {
  constructor(message: string) {
    super(message);
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

export class ValidationError extends DomainError {}
export class NotFoundError extends DomainError {}
export class UnauthorizedError extends DomainError {}
export class ConflictError extends DomainError {}

// Usage
export class UserService {
  async getUser(id: string): Promise<User> {
    const user = await this.userRepo.findById(id);
    
    if (!user) {
      throw new NotFoundError(`User ${id} not found`);
    }
    
    return user;
  }
}

// ✅ Global error handler
app.use((error: Error, req: Request, res: Response, next: NextFunction) => {
  if (error instanceof ValidationError) {
    return res.status(400).json({ error: error.message });
  }
  
  if (error instanceof NotFoundError) {
    return res.status(404).json({ error: error.message });
  }
  
  if (error instanceof UnauthorizedError) {
    return res.status(401).json({ error: error.message });
  }
  
  if (error instanceof ConflictError) {
    return res.status(409).json({ error: error.message });
  }
  
  logger.error('Unhandled error', { error });
  return res.status(500).json({ error: 'Internal server error' });
});
```

**Key Architecture Principles:**
1. Separation of concerns
2. Dependency inversion
3. Single responsibility
4. Open/closed principle
5. Interface segregation
6. Loose coupling, high cohesion
7. Don't repeat yourself (DRY)
8. Keep it simple (KISS)
