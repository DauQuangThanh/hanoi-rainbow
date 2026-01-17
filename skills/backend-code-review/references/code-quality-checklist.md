# Code Quality Checklist

Comprehensive criteria for assessing backend code quality across Node.js, Python, Java, Go, and C#.

## Table of Contents
- [Code Organization](#code-organization)
- [Design Patterns](#design-patterns)
- [SOLID Principles](#solid-principles)
- [Error Handling](#error-handling)
- [Dependency Management](#dependency-management)
- [Logging and Monitoring](#logging-and-monitoring)
- [Type Safety](#type-safety)
- [Common Issues by Language](#common-issues-by-language)

## Code Organization

### Layered Architecture

**Good:**
```
project/
├── controllers/     # HTTP handlers, request/response
├── services/        # Business logic
├── repositories/    # Data access layer
├── models/          # Data models/entities
├── middleware/      # Request processing
├── config/          # Configuration
└── utils/           # Shared utilities
```

**Node.js/Express Example:**
```typescript
// ✅ Proper layering

// controllers/userController.ts
export class UserController {
  constructor(private userService: UserService) {}
  
  async getUser(req: Request, res: Response): Promise<void> {
    try {
      const user = await this.userService.getUserById(req.params.id);
      res.json(user);
    } catch (error) {
      res.status(404).json({ error: error.message });
    }
  }
}

// services/userService.ts
export class UserService {
  constructor(private userRepository: UserRepository) {}
  
  async getUserById(id: string): Promise<User> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }
    return user;
  }
}

// repositories/userRepository.ts
export class UserRepository {
  async findById(id: string): Promise<User | null> {
    return await db.users.findOne({ id });
  }
}
```

**Bad:**
```typescript
// ❌ All logic in controller
export async function getUser(req: Request, res: Response) {
  const user = await db.users.findOne({ id: req.params.id });
  if (!user) {
    res.status(404).json({ error: 'Not found' });
    return;
  }
  res.json(user);
}
```

### Separation of Concerns

**Python/Django Example:**

**Good:**
```python
# ✅ Separated concerns

# views.py
class UserViewSet(viewsets.ModelViewSet):
    def create(self, request):
        serializer = UserSerializer(data=request.data)
        if serializer.is_valid():
            user = UserService.create_user(serializer.validated_data)
            return Response(UserSerializer(user).data, status=201)
        return Response(serializer.errors, status=400)

# services.py
class UserService:
    @staticmethod
    def create_user(data: dict) -> User:
        # Business logic
        password = PasswordHasher.hash(data['password'])
        user = User.objects.create(
            email=data['email'],
            password=password
        )
        EmailService.send_welcome_email(user)
        return user

# repositories.py (if using repository pattern)
class UserRepository:
    @staticmethod
    def create(data: dict) -> User:
        return User.objects.create(**data)
```

**Bad:**
```python
# ❌ Everything in view
class UserViewSet(viewsets.ModelViewSet):
    def create(self, request):
        # Validation
        if not request.data.get('email'):
            return Response({'error': 'Email required'}, status=400)
        
        # Hashing
        password = bcrypt.hashpw(
            request.data['password'].encode(),
            bcrypt.gensalt()
        )
        
        # Database
        user = User.objects.create(
            email=request.data['email'],
            password=password
        )
        
        # Email
        send_mail('Welcome', 'Welcome!', 'from@example.com', [user.email])
        
        return Response({'id': user.id}, status=201)
```

## Design Patterns

### Repository Pattern

**Java/Spring Example:**

**Good:**
```java
// ✅ Repository pattern

// UserRepository.java
@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByEmail(String email);
    List<User> findByRoleAndActiveTrue(Role role);
}

// UserService.java
@Service
public class UserService {
    private final UserRepository userRepository;
    
    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
    
    public User createUser(CreateUserDto dto) {
        if (userRepository.findByEmail(dto.getEmail()).isPresent()) {
            throw new UserAlreadyExistsException();
        }
        
        User user = new User();
        user.setEmail(dto.getEmail());
        user.setPassword(passwordEncoder.encode(dto.getPassword()));
        
        return userRepository.save(user);
    }
}
```

**Bad:**
```java
// ❌ Direct EntityManager usage everywhere
@Service
public class UserService {
    @PersistenceContext
    private EntityManager em;
    
    public User createUser(CreateUserDto dto) {
        // Raw queries, no abstraction
        Query query = em.createQuery(
            "SELECT u FROM User u WHERE u.email = :email"
        );
        query.setParameter("email", dto.getEmail());
        
        // Direct persistence logic
        User user = new User();
        em.persist(user);
        em.flush();
        
        return user;
    }
}
```

### Dependency Injection

**Good (Multiple Languages):**

**TypeScript/Node.js:**
```typescript
// ✅ Constructor injection
export class OrderService {
  constructor(
    private orderRepository: OrderRepository,
    private paymentService: PaymentService,
    private emailService: EmailService,
    private logger: Logger
  ) {}
  
  async processOrder(order: CreateOrderDto): Promise<Order> {
    this.logger.info('Processing order', { orderId: order.id });
    
    const savedOrder = await this.orderRepository.save(order);
    await this.paymentService.charge(savedOrder);
    await this.emailService.sendConfirmation(savedOrder);
    
    return savedOrder;
  }
}
```

**C#/.NET:**
```csharp
// ✅ Dependency injection
public class OrderService : IOrderService
{
    private readonly IOrderRepository _orderRepository;
    private readonly IPaymentService _paymentService;
    private readonly ILogger<OrderService> _logger;
    
    public OrderService(
        IOrderRepository orderRepository,
        IPaymentService paymentService,
        ILogger<OrderService> logger)
    {
        _orderRepository = orderRepository;
        _paymentService = paymentService;
        _logger = logger;
    }
    
    public async Task<Order> ProcessOrderAsync(CreateOrderDto dto)
    {
        _logger.LogInformation("Processing order {OrderId}", dto.Id);
        
        var order = await _orderRepository.SaveAsync(dto);
        await _paymentService.ChargeAsync(order);
        
        return order;
    }
}
```

**Bad:**
```typescript
// ❌ Direct instantiation, tight coupling
export class OrderService {
  async processOrder(order: CreateOrderDto): Promise<Order> {
    const repository = new OrderRepository();  // Direct instantiation!
    const payment = new PaymentService();      // Cannot mock!
    const email = new EmailService();          // Hard to test!
    
    const savedOrder = await repository.save(order);
    await payment.charge(savedOrder);
    await email.sendConfirmation(savedOrder);
    
    return savedOrder;
  }
}
```

### Strategy Pattern

**Go Example:**

**Good:**
```go
// ✅ Strategy pattern for payment processing

// Strategy interface
type PaymentStrategy interface {
    ProcessPayment(amount float64, details PaymentDetails) error
}

// Concrete strategies
type CreditCardStrategy struct {
    gateway CreditCardGateway
}

func (s *CreditCardStrategy) ProcessPayment(amount float64, details PaymentDetails) error {
    return s.gateway.Charge(details.CardNumber, amount)
}

type PayPalStrategy struct {
    client PayPalClient
}

func (s *PayPalStrategy) ProcessPayment(amount float64, details PaymentDetails) error {
    return s.client.CreatePayment(details.Email, amount)
}

// Context
type PaymentProcessor struct {
    strategy PaymentStrategy
}

func (p *PaymentProcessor) SetStrategy(strategy PaymentStrategy) {
    p.strategy = strategy
}

func (p *PaymentProcessor) Process(amount float64, details PaymentDetails) error {
    return p.strategy.ProcessPayment(amount, details)
}

// Usage
processor := &PaymentProcessor{}

if paymentMethod == "credit_card" {
    processor.SetStrategy(&CreditCardStrategy{gateway: ccGateway})
} else if paymentMethod == "paypal" {
    processor.SetStrategy(&PayPalStrategy{client: paypalClient})
}

err := processor.Process(100.0, details)
```

**Bad:**
```go
// ❌ Conditional logic everywhere
func ProcessPayment(method string, amount float64, details PaymentDetails) error {
    if method == "credit_card" {
        // Credit card logic
        gateway := NewCreditCardGateway()
        return gateway.Charge(details.CardNumber, amount)
    } else if method == "paypal" {
        // PayPal logic
        client := NewPayPalClient()
        return client.CreatePayment(details.Email, amount)
    } else if method == "crypto" {
        // Crypto logic
        // ...
    }
    // Adding new method requires modifying this function
    return errors.New("unsupported payment method")
}
```

## SOLID Principles

### Single Responsibility Principle (SRP)

**Good:**
```typescript
// ✅ Each class has one responsibility

class UserAuthenticationService {
  async authenticate(email: string, password: string): Promise<User> {
    // Only handles authentication
  }
}

class UserRegistrationService {
  async register(userData: CreateUserDto): Promise<User> {
    // Only handles registration
  }
}

class EmailVerificationService {
  async sendVerificationEmail(user: User): Promise<void> {
    // Only handles email verification
  }
}
```

**Bad:**
```typescript
// ❌ God class doing everything
class UserService {
  async authenticate(email: string, password: string) { }
  async register(userData: CreateUserDto) { }
  async sendEmail(user: User) { }
  async updateProfile(userId: string, data: any) { }
  async deleteUser(userId: string) { }
  async exportUserData(userId: string) { }
  async generateReport() { }
  async sendNewsletter() { }
  // 50+ methods...
}
```

### Open/Closed Principle (OCP)

**Python Example:**

**Good:**
```python
# ✅ Open for extension, closed for modification

from abc import ABC, abstractmethod

class NotificationSender(ABC):
    @abstractmethod
    def send(self, recipient: str, message: str) -> None:
        pass

class EmailNotificationSender(NotificationSender):
    def send(self, recipient: str, message: str) -> None:
        # Send email
        pass

class SMSNotificationSender(NotificationSender):
    def send(self, recipient: str, message: str) -> None:
        # Send SMS
        pass

class PushNotificationSender(NotificationSender):
    def send(self, recipient: str, message: str) -> None:
        # Send push notification
        pass

class NotificationService:
    def __init__(self, senders: list[NotificationSender]):
        self.senders = senders
    
    def notify(self, recipient: str, message: str) -> None:
        for sender in self.senders:
            sender.send(recipient, message)

# Adding new notification type doesn't modify existing code
class SlackNotificationSender(NotificationSender):
    def send(self, recipient: str, message: str) -> None:
        # Send Slack message
        pass
```

**Bad:**
```python
# ❌ Must modify class to add new notification types
class NotificationService:
    def notify(self, recipient: str, message: str, method: str) -> None:
        if method == "email":
            # Email logic
            pass
        elif method == "sms":
            # SMS logic
            pass
        elif method == "push":
            # Push logic
            pass
        # Adding Slack requires modifying this method
```

### Liskov Substitution Principle (LSP)

**Java Example:**

**Good:**
```java
// ✅ Subtypes can replace base type

public interface PaymentMethod {
    PaymentResult process(BigDecimal amount);
    boolean supportsRefund();
}

public class CreditCardPayment implements PaymentMethod {
    @Override
    public PaymentResult process(BigDecimal amount) {
        // Process credit card payment
        return new PaymentResult(true, "Success");
    }
    
    @Override
    public boolean supportsRefund() {
        return true;
    }
}

public class CryptoPayment implements PaymentMethod {
    @Override
    public PaymentResult process(BigDecimal amount) {
        // Process crypto payment
        return new PaymentResult(true, "Success");
    }
    
    @Override
    public boolean supportsRefund() {
        return false; // Crypto doesn't support refunds
    }
}

// Usage - works with any PaymentMethod
public class PaymentProcessor {
    public void processPayment(PaymentMethod method, BigDecimal amount) {
        PaymentResult result = method.process(amount);
        
        if (result.isSuccess() && method.supportsRefund()) {
            // Can safely request refund
        }
    }
}
```

**Bad:**
```java
// ❌ Violates LSP - throws exception instead of proper behavior
public class CryptoPayment implements PaymentMethod {
    @Override
    public PaymentResult process(BigDecimal amount) {
        return new PaymentResult(true, "Success");
    }
    
    @Override
    public void refund(BigDecimal amount) {
        // Violates LSP - throws exception when interface says it should work
        throw new UnsupportedOperationException("Crypto doesn't support refunds");
    }
}
```

### Interface Segregation Principle (ISP)

**C# Example:**

**Good:**
```csharp
// ✅ Specific, focused interfaces

public interface IReadRepository<T>
{
    Task<T> GetByIdAsync(int id);
    Task<IEnumerable<T>> GetAllAsync();
}

public interface IWriteRepository<T>
{
    Task<T> CreateAsync(T entity);
    Task<T> UpdateAsync(T entity);
    Task DeleteAsync(int id);
}

// Classes implement only what they need
public class ReadOnlyUserRepository : IReadRepository<User>
{
    public async Task<User> GetByIdAsync(int id) { /* ... */ }
    public async Task<IEnumerable<User>> GetAllAsync() { /* ... */ }
}

public class UserRepository : IReadRepository<User>, IWriteRepository<User>
{
    // Implements both interfaces
}
```

**Bad:**
```csharp
// ❌ Fat interface forces unnecessary implementation
public interface IRepository<T>
{
    Task<T> GetByIdAsync(int id);
    Task<IEnumerable<T>> GetAllAsync();
    Task<T> CreateAsync(T entity);
    Task<T> UpdateAsync(T entity);
    Task DeleteAsync(int id);
    Task<IEnumerable<T>> SearchAsync(string query);
    Task<int> CountAsync();
}

// Read-only repo forced to implement write methods
public class ReadOnlyUserRepository : IRepository<User>
{
    public async Task<User> GetByIdAsync(int id) { /* ... */ }
    public async Task<IEnumerable<User>> GetAllAsync() { /* ... */ }
    
    // Forced to implement these even though they shouldn't exist
    public async Task<User> CreateAsync(User entity)
    {
        throw new NotSupportedException("Read-only repository");
    }
    
    public async Task<User> UpdateAsync(User entity)
    {
        throw new NotSupportedException("Read-only repository");
    }
    
    public async Task DeleteAsync(int id)
    {
        throw new NotSupportedException("Read-only repository");
    }
}
```

### Dependency Inversion Principle (DIP)

**Good:**
```typescript
// ✅ Depend on abstractions, not concretions

interface IEmailService {
  send(to: string, subject: string, body: string): Promise<void>;
}

interface IUserRepository {
  findById(id: string): Promise<User | null>;
  save(user: User): Promise<User>;
}

// High-level module depends on abstractions
class UserRegistrationService {
  constructor(
    private userRepository: IUserRepository,
    private emailService: IEmailService
  ) {}
  
  async register(userData: CreateUserDto): Promise<User> {
    const user = await this.userRepository.save(userData);
    await this.emailService.send(
      user.email,
      'Welcome',
      'Thank you for registering'
    );
    return user;
  }
}

// Low-level modules implement abstractions
class SendGridEmailService implements IEmailService {
  async send(to: string, subject: string, body: string): Promise<void> {
    // SendGrid implementation
  }
}

class MongoUserRepository implements IUserRepository {
  async findById(id: string): Promise<User | null> {
    // MongoDB implementation
  }
  
  async save(user: User): Promise<User> {
    // MongoDB implementation
  }
}
```

**Bad:**
```typescript
// ❌ High-level module depends on low-level details
class UserRegistrationService {
  private sendGridClient: SendGrid;
  private mongoClient: MongoClient;
  
  constructor() {
    // Direct dependency on concrete implementations
    this.sendGridClient = new SendGrid(process.env.SENDGRID_KEY);
    this.mongoClient = new MongoClient(process.env.MONGO_URL);
  }
  
  async register(userData: CreateUserDto): Promise<User> {
    // Directly using MongoDB API
    const collection = this.mongoClient.db().collection('users');
    const user = await collection.insertOne(userData);
    
    // Directly using SendGrid API
    await this.sendGridClient.send({
      to: user.email,
      from: 'noreply@example.com',
      subject: 'Welcome',
      text: 'Thank you for registering'
    });
    
    return user;
  }
}
```

## Error Handling

### Consistent Error Handling

**Node.js/TypeScript Example:**

**Good:**
```typescript
// ✅ Custom error classes with hierarchy

class AppError extends Error {
  constructor(
    message: string,
    public statusCode: number,
    public isOperational: boolean = true
  ) {
    super(message);
    Object.setPrototypeOf(this, AppError.prototype);
  }
}

class NotFoundError extends AppError {
  constructor(resource: string) {
    super(`${resource} not found`, 404);
  }
}

class ValidationError extends AppError {
  constructor(
    message: string,
    public errors: Record<string, string[]>
  ) {
    super(message, 400);
  }
}

class UnauthorizedError extends AppError {
  constructor(message: string = 'Unauthorized') {
    super(message, 401);
  }
}

// Usage in service
class UserService {
  async getUserById(id: string): Promise<User> {
    const user = await this.userRepository.findById(id);
    
    if (!user) {
      throw new NotFoundError('User');
    }
    
    return user;
  }
  
  async updateUser(id: string, data: UpdateUserDto): Promise<User> {
    const errors = this.validate(data);
    
    if (Object.keys(errors).length > 0) {
      throw new ValidationError('Validation failed', errors);
    }
    
    return await this.userRepository.update(id, data);
  }
}

// Global error handler
app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
  if (err instanceof AppError) {
    return res.status(err.statusCode).json({
      status: 'error',
      message: err.message,
      ...(err instanceof ValidationError && { errors: err.errors })
    });
  }
  
  // Unexpected errors
  console.error('Unexpected error:', err);
  return res.status(500).json({
    status: 'error',
    message: 'Internal server error'
  });
});
```

**Bad:**
```typescript
// ❌ Inconsistent error handling
class UserService {
  async getUserById(id: string): Promise<User | null> {
    try {
      const user = await this.userRepository.findById(id);
      return user; // Returns null instead of throwing
    } catch (error) {
      console.log('Error:', error);
      return null; // Swallows errors
    }
  }
  
  async updateUser(id: string, data: any): Promise<User> {
    if (!data.email) {
      throw new Error('Invalid email'); // Generic error
    }
    
    if (!data.name) {
      throw 'Name required'; // Throwing string!
    }
    
    // No error handling for repository
    return await this.userRepository.update(id, data);
  }
}
```

### Python Exception Handling

**Good:**
```python
# ✅ Custom exception hierarchy

class ApplicationError(Exception):
    """Base application error"""
    def __init__(self, message: str, status_code: int = 500):
        self.message = message
        self.status_code = status_code
        super().__init__(self.message)

class NotFoundError(ApplicationError):
    def __init__(self, resource: str):
        super().__init__(f"{resource} not found", 404)

class ValidationError(ApplicationError):
    def __init__(self, message: str, errors: dict):
        super().__init__(message, 400)
        self.errors = errors

# Usage
class UserService:
    def get_user_by_id(self, user_id: int) -> User:
        user = self.user_repository.find_by_id(user_id)
        
        if not user:
            raise NotFoundError("User")
        
        return user
    
    def create_user(self, data: dict) -> User:
        errors = self._validate(data)
        
        if errors:
            raise ValidationError("Validation failed", errors)
        
        try:
            return self.user_repository.create(data)
        except IntegrityError as e:
            if 'email' in str(e):
                raise ValidationError(
                    "User already exists",
                    {"email": ["Email already registered"]}
                )
            raise

# Error handler
@app.errorhandler(ApplicationError)
def handle_app_error(error: ApplicationError):
    response = {"error": error.message}
    
    if isinstance(error, ValidationError):
        response["errors"] = error.errors
    
    return jsonify(response), error.status_code

@app.errorhandler(Exception)
def handle_unexpected_error(error: Exception):
    logger.exception("Unexpected error occurred")
    return jsonify({"error": "Internal server error"}), 500
```

## Dependency Management

### Version Pinning

**Good:**
```json
// package.json
{
  "dependencies": {
    "express": "4.18.2",
    "mongoose": "7.0.3",
    "jsonwebtoken": "9.0.0"
  },
  "devDependencies": {
    "typescript": "5.0.4",
    "@types/express": "4.17.17"
  }
}
```

```python
# requirements.txt
Django==4.2.1
psycopg2-binary==2.9.6
celery==5.2.7
redis==4.5.5
```

**Bad:**
```json
// ❌ Loose version constraints
{
  "dependencies": {
    "express": "^4.0.0",  // Could install 4.99.99
    "mongoose": "*",       // Could install any version!
    "jsonwebtoken": ">=9.0.0"  // Could install breaking changes
  }
}
```

### Security Audits

```bash
# Node.js
npm audit
npm audit fix

# Python
pip-audit
safety check

# Java
mvn dependency-check:check

# Check for outdated packages
npm outdated
pip list --outdated
```

## Logging and Monitoring

### Structured Logging

**Node.js/Winston Example:**

**Good:**
```typescript
// ✅ Structured logging with context

import winston from 'winston';

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' })
  ]
});

class UserService {
  async createUser(data: CreateUserDto): Promise<User> {
    logger.info('Creating user', {
      email: data.email,
      action: 'user.create'
    });
    
    try {
      const user = await this.userRepository.save(data);
      
      logger.info('User created successfully', {
        userId: user.id,
        email: user.email,
        action: 'user.created'
      });
      
      return user;
    } catch (error) {
      logger.error('Failed to create user', {
        email: data.email,
        action: 'user.create.failed',
        error: error.message,
        stack: error.stack
      });
      throw error;
    }
  }
}
```

**Bad:**
```typescript
// ❌ Unstructured logging, missing context
class UserService {
  async createUser(data: CreateUserDto): Promise<User> {
    console.log('Creating user...');
    
    try {
      const user = await this.userRepository.save(data);
      console.log('User created');
      return user;
    } catch (error) {
      console.log('Error:', error); // No context, no structured data
      throw error;
    }
  }
}
```

### Log Levels

```typescript
// Use appropriate log levels

logger.debug('Detailed debug information', { query, params });  // Development
logger.info('User logged in', { userId, timestamp });            // Informational
logger.warn('Deprecated API used', { endpoint, userId });        // Warnings
logger.error('Failed to process payment', { error, orderId });   // Errors
logger.fatal('Database connection lost', { error });             // Critical
```

## Type Safety

### TypeScript Best Practices

**Good:**
```typescript
// ✅ Strict types, no any

interface User {
  id: string;
  email: string;
  name: string;
  role: 'admin' | 'user' | 'guest';
  createdAt: Date;
}

interface CreateUserDto {
  email: string;
  name: string;
  password: string;
}

// Generic repository with proper types
interface Repository<T> {
  findById(id: string): Promise<T | null>;
  findAll(): Promise<T[]>;
  save(entity: T): Promise<T>;
  delete(id: string): Promise<void>;
}

class UserRepository implements Repository<User> {
  async findById(id: string): Promise<User | null> {
    // Implementation
  }
  
  async findAll(): Promise<User[]> {
    // Implementation
  }
  
  async save(user: User): Promise<User> {
    // Implementation
  }
  
  async delete(id: string): Promise<void> {
    // Implementation
  }
}

// Discriminated unions for type safety
type ApiResponse<T> =
  | { status: 'success'; data: T }
  | { status: 'error'; error: string };

function handleResponse<T>(response: ApiResponse<T>): T {
  if (response.status === 'success') {
    return response.data; // TypeScript knows data exists
  } else {
    throw new Error(response.error); // TypeScript knows error exists
  }
}
```

**Bad:**
```typescript
// ❌ Using any, loose types
interface User {
  id: any;
  data: any;
  metadata: any;
}

function processUser(user: any): any {
  return user.data;
}

// No type safety
const response: any = await api.get('/users');
const user = response.data; // Could be anything!
```

## Common Issues by Language

### Node.js Specific

**Issue 1: Callback Hell**
```typescript
// ❌ Bad
getData((err, data) => {
  if (err) return handleError(err);
  processData(data, (err, result) => {
    if (err) return handleError(err);
    saveResult(result, (err) => {
      if (err) return handleError(err);
      console.log('Done');
    });
  });
});

// ✅ Good
async function workflow() {
  try {
    const data = await getData();
    const result = await processData(data);
    await saveResult(result);
    console.log('Done');
  } catch (error) {
    handleError(error);
  }
}
```

**Issue 2: Unhandled Promise Rejections**
```typescript
// ❌ Bad
app.get('/users', (req, res) => {
  userService.getUsers() // Promise not awaited or caught
    .then(users => res.json(users));
});

// ✅ Good
app.get('/users', async (req, res, next) => {
  try {
    const users = await userService.getUsers();
    res.json(users);
  } catch (error) {
    next(error); // Pass to error handler
  }
});

// Global handler
process.on('unhandledRejection', (reason, promise) => {
  logger.error('Unhandled Rejection', { reason, promise });
  process.exit(1);
});
```

### Python Specific

**Issue: Mutable Default Arguments**
```python
# ❌ Bad
def add_item(item, items=[]):  # Dangerous!
    items.append(item)
    return items

list1 = add_item(1)  # [1]
list2 = add_item(2)  # [1, 2] - Unexpected!

# ✅ Good
def add_item(item, items=None):
    if items is None:
        items = []
    items.append(item)
    return items
```

### Java Specific

**Issue: Resource Management**
```java
// ❌ Bad
public void readFile(String path) throws IOException {
    FileInputStream fis = new FileInputStream(path);
    // If exception occurs, stream never closes
    byte[] data = new byte[fis.available()];
    fis.read(data);
    fis.close(); // Might not be called
}

// ✅ Good - try-with-resources
public void readFile(String path) throws IOException {
    try (FileInputStream fis = new FileInputStream(path)) {
        byte[] data = new byte[fis.available()];
        fis.read(data);
        // Stream automatically closed
    }
}
```

### Go Specific

**Issue: Error Handling**
```go
// ❌ Bad - ignoring errors
func GetUser(id string) *User {
    user, _ := db.FindUser(id) // Ignoring error!
    return user
}

// ✅ Good
func GetUser(id string) (*User, error) {
    user, err := db.FindUser(id)
    if err != nil {
        return nil, fmt.Errorf("failed to get user: %w", err)
    }
    return user, nil
}
```

### C# Specific

**Issue: Async/Await**
```csharp
// ❌ Bad - blocking async code
public User GetUser(int id)
{
    var user = _userRepository.GetByIdAsync(id).Result; // Deadlock risk!
    return user;
}

// ✅ Good
public async Task<User> GetUserAsync(int id)
{
    var user = await _userRepository.GetByIdAsync(id);
    return user;
}
```
