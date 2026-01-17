# Refactoring Process


Follow this systematic approach when refactoring code:

### Phase 1: Assessment & Planning

1. **Identify Refactoring Needs**
   - Code review findings
   - Static analysis tool reports (SonarQube, ESLint, etc.)
   - Performance profiling results
   - Developer pain points
   - Frequent bug locations
   - Difficulty adding new features

2. **Analyze Code Smells**
   - **Bloaters**: Long methods (>20 lines), large classes (>300 lines), long parameter lists (>3 params)
   - **Object-Orientation Abusers**: Switch statements, refused bequest, temporary fields
   - **Change Preventers**: Divergent change, shotgun surgery, parallel inheritance
   - **Dispensables**: Comments (excessive), duplicate code, dead code, speculative generality
   - **Couplers**: Feature envy, inappropriate intimacy, message chains, middle man

3. **Establish Safety Net**
   ```typescript
   // Before refactoring, ensure comprehensive tests exist
   describe('UserService', () => {
     it('should create user with valid data', async () => {
       const userData = { email: 'test@example.com', name: 'Test User' };
       const user = await userService.createUser(userData);
       expect(user).toBeDefined();
       expect(user.email).toBe(userData.email);
     });

     it('should throw error for invalid email', async () => {
       const userData = { email: 'invalid', name: 'Test' };
       await expect(userService.createUser(userData)).rejects.toThrow();
     });

     it('should hash password before saving', async () => {
       const userData = { email: 'test@example.com', password: 'plain' };
       const user = await userService.createUser(userData);
       expect(user.password).not.toBe('plain');
       expect(user.password.length).toBeGreaterThan(20);
     });
   });
   ```

4. **Prioritize Refactoring Tasks**
   - High impact, low effort (quick wins)
   - Critical bugs or security issues
   - Code touched frequently
   - Code blocking new features
   - Code with high cyclomatic complexity

### Phase 2: Method-Level Refactoring

**Extract Method**: Break down long methods

```typescript
// Before: Long method doing too much
class OrderProcessor {
  async processOrder(orderId: string) {
    const order = await this.orderRepository.findById(orderId);
    if (!order) throw new Error('Order not found');
    
    // Validate inventory
    for (const item of order.items) {
      const product = await this.productRepository.findById(item.productId);
      if (!product) throw new Error('Product not found');
      if (product.stock < item.quantity) {
        throw new Error('Insufficient stock');
      }
    }
    
    // Calculate total
    let total = 0;
    for (const item of order.items) {
      const product = await this.productRepository.findById(item.productId);
      total += product.price * item.quantity;
    }
    if (order.couponCode) {
      const coupon = await this.couponRepository.findByCode(order.couponCode);
      if (coupon && coupon.isValid()) {
        total = total * (1 - coupon.discount);
      }
    }
    
    // Process payment
    const payment = await this.paymentService.charge({
      amount: total,
      customerId: order.customerId
    });
    
    // Update inventory
    for (const item of order.items) {
      await this.productRepository.decrementStock(item.productId, item.quantity);
    }
    
    order.status = 'completed';
    order.total = total;
    order.paymentId = payment.id;
    await this.orderRepository.save(order);
    
    return order;
  }
}

// After: Extracted into focused methods
class OrderProcessor {
  async processOrder(orderId: string): Promise<Order> {
    const order = await this.getOrder(orderId);
    await this.validateInventory(order);
    
    const total = await this.calculateTotal(order);
    const payment = await this.processPayment(order.customerId, total);
    await this.updateInventory(order);
    
    return await this.completeOrder(order, total, payment.id);
  }

  private async getOrder(orderId: string): Promise<Order> {
    const order = await this.orderRepository.findById(orderId);
    if (!order) {
      throw new NotFoundError('Order not found');
    }
    return order;
  }

  private async validateInventory(order: Order): Promise<void> {
    for (const item of order.items) {
      const product = await this.productRepository.findById(item.productId);
      if (!product) {
        throw new NotFoundError(`Product ${item.productId} not found`);
      }
      if (product.stock < item.quantity) {
        throw new InsufficientStockError(product.id, item.quantity, product.stock);
      }
    }
  }

  private async calculateTotal(order: Order): Promise<number> {
    const subtotal = await this.calculateSubtotal(order.items);
    const discount = await this.calculateDiscount(order.couponCode, subtotal);
    return subtotal - discount;
  }

  private async calculateSubtotal(items: OrderItem[]): Promise<number> {
    let total = 0;
    for (const item of items) {
      const product = await this.productRepository.findById(item.productId);
      total += product.price * item.quantity;
    }
    return total;
  }

  private async calculateDiscount(couponCode: string | null, subtotal: number): Promise<number> {
    if (!couponCode) return 0;
    
    const coupon = await this.couponRepository.findByCode(couponCode);
    if (!coupon || !coupon.isValid()) return 0;
    
    return subtotal * coupon.discount;
  }

  private async processPayment(customerId: string, amount: number): Promise<Payment> {
    return await this.paymentService.charge({ amount, customerId });
  }

  private async updateInventory(order: Order): Promise<void> {
    for (const item of order.items) {
      await this.productRepository.decrementStock(item.productId, item.quantity);
    }
  }

  private async completeOrder(order: Order, total: number, paymentId: string): Promise<Order> {
    order.status = 'completed';
    order.total = total;
    order.paymentId = paymentId;
    return await this.orderRepository.save(order);
  }
}
```

**Replace Conditional with Polymorphism**

```typescript
// Before: Complex conditionals
class PaymentProcessor {
  processPayment(payment: Payment) {
    if (payment.type === 'credit_card') {
      this.validateCreditCard(payment);
      return this.chargeCreditCard(payment);
    } else if (payment.type === 'paypal') {
      this.validatePayPal(payment);
      return this.chargePayPal(payment);
    } else if (payment.type === 'bank_transfer') {
      this.validateBankTransfer(payment);
      return this.chargeBankTransfer(payment);
    } else {
      throw new Error('Unknown payment type');
    }
  }
}

// After: Polymorphic payment methods
interface PaymentMethod {
  validate(): void;
  charge(amount: number): Promise<PaymentResult>;
}

class CreditCardPayment implements PaymentMethod {
  constructor(private cardNumber: string, private cvv: string, private expiry: string) {}

  validate(): void {
    if (!this.isValidCardNumber(this.cardNumber)) {
      throw new ValidationError('Invalid card number');
    }
    if (!this.isValidCVV(this.cvv)) {
      throw new ValidationError('Invalid CVV');
    }
    if (this.isExpired(this.expiry)) {
      throw new ValidationError('Card expired');
    }
  }

  async charge(amount: number): Promise<PaymentResult> {
    this.validate();
    return await this.creditCardGateway.charge({
      cardNumber: this.cardNumber,
      cvv: this.cvv,
      expiry: this.expiry,
      amount
    });
  }

  private isValidCardNumber(cardNumber: string): boolean {
    // Luhn algorithm
    return true;
  }

  private isValidCVV(cvv: string): boolean {
    return /^\d{3,4}$/.test(cvv);
  }

  private isExpired(expiry: string): boolean {
    const [month, year] = expiry.split('/');
    const expiryDate = new Date(parseInt('20' + year), parseInt(month) - 1);
    return expiryDate < new Date();
  }
}

class PayPalPayment implements PaymentMethod {
  constructor(private email: string, private token: string) {}

  validate(): void {
    if (!this.isValidEmail(this.email)) {
      throw new ValidationError('Invalid email');
    }
    if (!this.token) {
      throw new ValidationError('Token required');
    }
  }

  async charge(amount: number): Promise<PaymentResult> {
    this.validate();
    return await this.paypalGateway.charge({
      email: this.email,
      token: this.token,
      amount
    });
  }

  private isValidEmail(email: string): boolean {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
  }
}

class BankTransferPayment implements PaymentMethod {
  constructor(private accountNumber: string, private routingNumber: string) {}

  validate(): void {
    if (!this.isValidAccountNumber(this.accountNumber)) {
      throw new ValidationError('Invalid account number');
    }
    if (!this.isValidRoutingNumber(this.routingNumber)) {
      throw new ValidationError('Invalid routing number');
    }
  }

  async charge(amount: number): Promise<PaymentResult> {
    this.validate();
    return await this.bankTransferGateway.initiate({
      accountNumber: this.accountNumber,
      routingNumber: this.routingNumber,
      amount
    });
  }

  private isValidAccountNumber(accountNumber: string): boolean {
    return /^\d{8,17}$/.test(accountNumber);
  }

  private isValidRoutingNumber(routingNumber: string): boolean {
    return /^\d{9}$/.test(routingNumber);
  }
}

// Refactored processor
class PaymentProcessor {
  async processPayment(paymentMethod: PaymentMethod, amount: number): Promise<PaymentResult> {
    return await paymentMethod.charge(amount);
  }
}
```

### Phase 3: Class-Level Refactoring

**Extract Class**: Separate responsibilities

```typescript
// Before: God class doing too much
class User {
  id: string;
  email: string;
  password: string;
  name: string;
  avatar: string;
  bio: string;
  
  // Authentication logic
  async login(password: string): Promise<string> {
    const isValid = await bcrypt.compare(password, this.password);
    if (!isValid) throw new Error('Invalid credentials');
    return jwt.sign({ userId: this.id }, process.env.JWT_SECRET);
  }
  
  // Profile management
  updateProfile(data: Partial<User>) {
    this.name = data.name || this.name;
    this.avatar = data.avatar || this.avatar;
    this.bio = data.bio || this.bio;
  }
  
  // Email sending
  async sendWelcomeEmail() {
    const transporter = nodemailer.createTransport({...});
    await transporter.sendMail({
      to: this.email,
      subject: 'Welcome!',
      html: '<h1>Welcome to our platform</h1>'
    });
  }
  
  // Notification preferences
  emailNotifications: boolean;
  pushNotifications: boolean;
  
  updateNotificationPreferences(email: boolean, push: boolean) {
    this.emailNotifications = email;
    this.pushNotifications = push;
  }
}

// After: Separated into focused classes
class User {
  id: string;
  email: string;
  password: string;
  name: string;
  
  constructor(data: UserData) {
    this.id = data.id;
    this.email = data.email;
    this.password = data.password;
    this.name = data.name;
  }
}

class UserProfile {
  userId: string;
  avatar: string;
  bio: string;
  
  update(data: Partial<UserProfile>): void {
    if (data.avatar) this.avatar = data.avatar;
    if (data.bio) this.bio = data.bio;
  }
}

class UserAuthentication {
  constructor(
    private user: User,
    private jwtService: JwtService,
    private hashingService: HashingService
  ) {}
  
  async login(password: string): Promise<string> {
    const isValid = await this.hashingService.compare(password, this.user.password);
    if (!isValid) {
      throw new InvalidCredentialsError();
    }
    return this.jwtService.sign({ userId: this.user.id });
  }
  
  async changePassword(oldPassword: string, newPassword: string): Promise<void> {
    const isValid = await this.hashingService.compare(oldPassword, this.user.password);
    if (!isValid) {
      throw new InvalidCredentialsError();
    }
    this.user.password = await this.hashingService.hash(newPassword);
  }
}

class NotificationPreferences {
  userId: string;
  emailEnabled: boolean;
  pushEnabled: boolean;
  smsEnabled: boolean;
  
  update(preferences: Partial<NotificationPreferences>): void {
    if (preferences.emailEnabled !== undefined) {
      this.emailEnabled = preferences.emailEnabled;
    }
    if (preferences.pushEnabled !== undefined) {
      this.pushEnabled = preferences.pushEnabled;
    }
    if (preferences.smsEnabled !== undefined) {
      this.smsEnabled = preferences.smsEnabled;
    }
  }
}

class UserNotificationService {
  constructor(
    private emailService: EmailService,
    private preferencesRepository: NotificationPreferencesRepository
  ) {}
  
  async sendWelcomeEmail(user: User): Promise<void> {
    const preferences = await this.preferencesRepository.findByUserId(user.id);
    if (!preferences.emailEnabled) return;
    
    await this.emailService.send({
      to: user.email,
      subject: 'Welcome!',
      template: 'welcome',
      data: { name: user.name }
    });
  }
}
```

**Introduce Parameter Object**

```typescript
// Before: Long parameter list
function createUser(
  email: string,
  password: string,
  name: string,
  age: number,
  country: string,
  city: string,
  zipCode: string,
  phoneNumber: string
) {
  // Implementation
}

// After: Parameter object
interface CreateUserParams {
  email: string;
  password: string;
  name: string;
  age: number;
  address: {
    country: string;
    city: string;
    zipCode: string;
  };
  phoneNumber: string;
}

function createUser(params: CreateUserParams) {
  // Implementation with better organization
}

// Even better: Multiple focused parameter objects
interface UserCredentials {
  email: string;
  password: string;
}

interface UserProfile {
  name: string;
  age: number;
}

interface Address {
  country: string;
  city: string;
  zipCode: string;
}

interface ContactInfo {
  phoneNumber: string;
  email: string;
}

function createUser(
  credentials: UserCredentials,
  profile: UserProfile,
  address: Address,
  contact: ContactInfo
) {
  // Clear, organized parameters
}
```

### Phase 4: Architecture-Level Refactoring

**Move Toward Clean Architecture**

```typescript
// Before: Mixed concerns, tight coupling
class UserController {
  async createUser(req: Request, res: Response) {
    const { email, password, name } = req.body;
    
    // Validation mixed with business logic
    if (!email || !email.includes('@')) {
      return res.status(400).json({ error: 'Invalid email' });
    }
    
    // Direct database access from controller
    const existingUser = await db.query(
      'SELECT * FROM users WHERE email = $1',
      [email]
    );
    if (existingUser.rows.length > 0) {
      return res.status(409).json({ error: 'User exists' });
    }
    
    // Business logic in controller
    const hashedPassword = await bcrypt.hash(password, 10);
    
    const result = await db.query(
      'INSERT INTO users (email, password, name) VALUES ($1, $2, $3) RETURNING *',
      [email, hashedPassword, name]
    );
    
    // Email sending in controller
    await sendEmail(email, 'Welcome!', 'Welcome to our platform');
    
    res.status(201).json(result.rows[0]);
  }
}

// After: Clean architecture with separated concerns
// Domain layer
class User {
  constructor(
    public readonly id: string,
    public readonly email: Email,
    public readonly password: Password,
    public readonly name: string
  ) {}
  
  static create(email: string, password: string, name: string): User {
    return new User(
      generateId(),
      Email.create(email),
      Password.create(password),
      name
    );
  }
}

class Email {
  private constructor(public readonly value: string) {}
  
  static create(email: string): Email {
    if (!email || !email.includes('@')) {
      throw new ValidationError('Invalid email format');
    }
    return new Email(email);
  }
}

class Password {
  private constructor(public readonly hashedValue: string) {}
  
  static async create(plainPassword: string): Promise<Password> {
    if (plainPassword.length < 8) {
      throw new ValidationError('Password must be at least 8 characters');
    }
    const hashed = await bcrypt.hash(plainPassword, 10);
    return new Password(hashed);
  }
}

// Application layer (use cases)
interface UserRepository {
  findByEmail(email: Email): Promise<User | null>;
  save(user: User): Promise<void>;
}

interface EventPublisher {
  publish(event: DomainEvent): Promise<void>;
}

class CreateUserUseCase {
  constructor(
    private userRepository: UserRepository,
    private eventPublisher: EventPublisher
  ) {}
  
  async execute(command: CreateUserCommand): Promise<User> {
    // Check if user exists
    const existingUser = await this.userRepository.findByEmail(
      Email.create(command.email)
    );
    if (existingUser) {
      throw new UserAlreadyExistsError(command.email);
    }
    
    // Create user
    const password = await Password.create(command.password);
    const user = new User(
      generateId(),
      Email.create(command.email),
      password,
      command.name
    );
    
    // Save user
    await this.userRepository.save(user);
    
    // Publish event
    await this.eventPublisher.publish(
      new UserCreatedEvent(user.id, user.email.value)
    );
    
    return user;
  }
}

// Infrastructure layer
class PostgresUserRepository implements UserRepository {
  constructor(private db: Database) {}
  
  async findByEmail(email: Email): Promise<User | null> {
    const result = await this.db.query(
      'SELECT * FROM users WHERE email = $1',
      [email.value]
    );
    
    if (result.rows.length === 0) return null;
    
    return this.mapToDomain(result.rows[0]);
  }
  
  async save(user: User): Promise<void> {
    await this.db.query(
      'INSERT INTO users (id, email, password, name) VALUES ($1, $2, $3, $4)',
      [user.id, user.email.value, user.password.hashedValue, user.name]
    );
  }
  
  private mapToDomain(row: any): User {
    return new User(
      row.id,
      Email.create(row.email),
      { hashedValue: row.password } as Password,
      row.name
    );
  }
}

// Presentation layer
class UserController {
  constructor(private createUserUseCase: CreateUserUseCase) {}
  
  async createUser(req: Request, res: Response): Promise<void> {
    try {
      const command = new CreateUserCommand(
        req.body.email,
        req.body.password,
        req.body.name
      );
      
      const user = await this.createUserUseCase.execute(command);
      
      res.status(201).json({
        id: user.id,
        email: user.email.value,
        name: user.name
      });
    } catch (error) {
      if (error instanceof ValidationError) {
        res.status(400).json({ error: error.message });
      } else if (error instanceof UserAlreadyExistsError) {
        res.status(409).json({ error: error.message });
      } else {
        res.status(500).json({ error: 'Internal server error' });
      }
    }
  }
}
```
