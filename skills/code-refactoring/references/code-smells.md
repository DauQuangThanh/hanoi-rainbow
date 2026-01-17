# Code Smells Identification Guide

This reference provides comprehensive guidance on identifying and addressing code smells - indicators of deeper problems in code design.

## Bloaters

Code that has grown so large and unwieldy that it's difficult to work with.

### Long Method

**Symptoms**: Method contains too many lines of code (generally >20-30 lines)

**Problems**:
- Hard to understand
- Difficult to test
- Likely violates Single Responsibility Principle
- Contains hidden duplication

**Detection**:
```typescript
// Red flags:
// - Method spans multiple screens
// - Deeply nested conditionals
// - Multiple levels of abstraction
// - Lots of local variables
// - Comments explaining sections

async function processOrder(orderId: string) {
  // Validate order - 20 lines
  const order = await db.orders.findById(orderId);
  if (!order) throw new Error('Order not found');
  if (order.status !== 'pending') throw new Error('Invalid status');
  // ... 15 more validation lines
  
  // Calculate totals - 30 lines
  let subtotal = 0;
  for (const item of order.items) {
    const product = await db.products.findById(item.productId);
    // ... 25 more calculation lines
  }
  
  // Apply discounts - 25 lines
  if (order.couponCode) {
    // ... 20 discount calculation lines
  }
  
  // Process payment - 30 lines
  const payment = await paymentGateway.charge({...});
  // ... 25 more payment lines
  
  // Update inventory - 20 lines
  for (const item of order.items) {
    // ... 15 inventory update lines
  }
  
  // Send notifications - 15 lines
  await emailService.send({...});
  // ... 10 notification lines
}
```

**Solutions**:
1. Extract Method
2. Replace Temp with Query
3. Introduce Parameter Object
4. Preserve Whole Object
5. Replace Method with Method Object

```typescript
// Fixed version
async function processOrder(orderId: string) {
  const order = await validateAndGetOrder(orderId);
  const total = await calculateTotal(order);
  const payment = await processPayment(order.customerId, total);
  await updateInventory(order);
  await sendConfirmation(order);
  return completeOrder(order, payment);
}
```

### Large Class

**Symptoms**: Class has many fields, methods, or lines of code (>300 lines)

**Problems**:
- Too many responsibilities
- Hard to understand and maintain
- Difficult to test
- High coupling

**Detection**:
```typescript
// Red flags:
// - More than 10 fields
// - More than 20 methods
// - Multiple unrelated responsibilities
// - Fields used only by subset of methods

class UserManager {
  // User data
  id: string;
  email: string;
  password: string;
  name: string;
  avatar: string;
  
  // Authentication
  async login(password: string) { }
  async logout() { }
  async refreshToken() { }
  async resetPassword() { }
  
  // Profile management
  updateProfile(data: any) { }
  uploadAvatar(file: File) { }
  deleteAvatar() { }
  
  // Email operations
  sendWelcomeEmail() { }
  sendPasswordResetEmail() { }
  sendNotificationEmail() { }
  
  // Preferences
  updateEmailPreferences() { }
  updatePrivacySettings() { }
  updateNotificationSettings() { }
  
  // Statistics
  getLoginCount() { }
  getLastLoginDate() { }
  getActivitySummary() { }
  
  // Friends/Social
  addFriend(friendId: string) { }
  removeFriend(friendId: string) { }
  getFriendsList() { }
  
  // Payment
  addPaymentMethod() { }
  removePaymentMethod() { }
  getPaymentHistory() { }
}
```

**Solutions**:
1. Extract Class
2. Extract Subclass
3. Extract Interface
4. Duplicate Observed Data

```typescript
// Fixed: Separated into focused classes
class User {
  constructor(
    public id: string,
    public email: string,
    public name: string
  ) {}
}

class UserAuthentication {
  async login(user: User, password: string): Promise<Token> { }
  async logout(token: Token): Promise<void> { }
  async refreshToken(token: Token): Promise<Token> { }
}

class UserProfile {
  async update(userId: string, data: ProfileData): Promise<void> { }
  async uploadAvatar(userId: string, file: File): Promise<string> { }
}

class UserNotifications {
  async sendWelcome(user: User): Promise<void> { }
  async sendPasswordReset(user: User): Promise<void> { }
}

class UserPreferences {
  async updateEmail(userId: string, prefs: EmailPrefs): Promise<void> { }
  async updatePrivacy(userId: string, prefs: PrivacyPrefs): Promise<void> { }
}

class UserStatistics {
  async getLoginCount(userId: string): Promise<number> { }
  async getActivitySummary(userId: string): Promise<Activity> { }
}
```

### Primitive Obsession

**Symptoms**: Using primitives instead of small objects for simple tasks

**Problems**:
- Validation logic scattered
- Type safety issues
- Harder to extend behavior
- No encapsulation

**Detection**:
```typescript
// Red flags:
// - String/number used for domain concepts
// - Validation repeated across codebase
// - Magic numbers/strings
// - No type safety for domain concepts

class User {
  email: string;           // Just a string, no validation
  phoneNumber: string;     // Could be any format
  zipCode: string;         // No validation
  country: string;         // Could be "USA", "US", "United States"
  status: number;          // 0, 1, 2 - what do they mean?
  
  constructor(
    email: string,
    phoneNumber: string,
    zipCode: string,
    country: string,
    status: number
  ) {
    // Validation scattered here
    if (!email.includes('@')) throw new Error('Invalid email');
    if (phoneNumber.length < 10) throw new Error('Invalid phone');
    // ...
  }
}

// Usage problems
const user = new User(
  'invalid-email',    // No compile-time checking
  '123',              // Wrong format
  'ABC',              // Invalid zip
  'United States',    // Inconsistent
  5                   // What does 5 mean?
);
```

**Solutions**:
1. Replace Data Value with Object
2. Replace Type Code with Class
3. Extract Class
4. Introduce Parameter Object

```typescript
// Fixed: Value objects with validation
class Email {
  private constructor(private readonly value: string) {}
  
  static create(email: string): Email {
    if (!email || !email.includes('@')) {
      throw new ValidationError('Invalid email format');
    }
    return new Email(email.toLowerCase());
  }
  
  getValue(): string {
    return this.value;
  }
  
  equals(other: Email): boolean {
    return this.value === other.value;
  }
}

class PhoneNumber {
  private constructor(private readonly value: string) {}
  
  static create(phone: string): PhoneNumber {
    const cleaned = phone.replace(/\D/g, '');
    if (cleaned.length !== 10) {
      throw new ValidationError('Phone must be 10 digits');
    }
    return new PhoneNumber(cleaned);
  }
  
  getValue(): string {
    return this.value;
  }
  
  format(): string {
    return `(${this.value.slice(0, 3)}) ${this.value.slice(3, 6)}-${this.value.slice(6)}`;
  }
}

class ZipCode {
  private constructor(private readonly value: string) {}
  
  static create(zip: string): ZipCode {
    if (!/^\d{5}(-\d{4})?$/.test(zip)) {
      throw new ValidationError('Invalid ZIP code format');
    }
    return new ZipCode(zip);
  }
  
  getValue(): string {
    return this.value;
  }
}

enum Country {
  US = 'US',
  CA = 'CA',
  UK = 'UK'
}

enum UserStatus {
  ACTIVE = 'ACTIVE',
  INACTIVE = 'INACTIVE',
  SUSPENDED = 'SUSPENDED'
}

class User {
  constructor(
    public email: Email,
    public phoneNumber: PhoneNumber,
    public zipCode: ZipCode,
    public country: Country,
    public status: UserStatus
  ) {}
}

// Usage: Compile-time safety
const user = new User(
  Email.create('user@example.com'),
  PhoneNumber.create('555-123-4567'),
  ZipCode.create('12345'),
  Country.US,
  UserStatus.ACTIVE
);
```

### Long Parameter List

**Symptoms**: Method has more than 3-4 parameters

**Problems**:
- Hard to understand
- Easy to pass parameters in wrong order
- Difficult to extend
- Creates dependencies

**Detection**:
```typescript
// Red flags:
// - More than 3-4 parameters
// - Parameters often changed together
// - Parameters come from same object

function createUser(
  email: string,
  password: string,
  firstName: string,
  lastName: string,
  age: number,
  country: string,
  city: string,
  street: string,
  zipCode: string,
  phoneNumber: string
) {
  // Too many parameters!
}
```

**Solutions**:
1. Replace Parameter with Method Call
2. Preserve Whole Object
3. Introduce Parameter Object

```typescript
// Solution 1: Parameter object
interface CreateUserParams {
  credentials: {
    email: string;
    password: string;
  };
  profile: {
    firstName: string;
    lastName: string;
    age: number;
  };
  address: {
    country: string;
    city: string;
    street: string;
    zipCode: string;
  };
  contact: {
    phoneNumber: string;
  };
}

function createUser(params: CreateUserParams) {
  // Much clearer!
}

// Solution 2: Builder pattern
class UserBuilder {
  private user: Partial<User> = {};
  
  withCredentials(email: string, password: string): this {
    this.user.email = email;
    this.user.password = password;
    return this;
  }
  
  withProfile(firstName: string, lastName: string, age: number): this {
    this.user.firstName = firstName;
    this.user.lastName = lastName;
    this.user.age = age;
    return this;
  }
  
  withAddress(country: string, city: string, street: string, zipCode: string): this {
    this.user.address = { country, city, street, zipCode };
    return this;
  }
  
  withContact(phoneNumber: string): this {
    this.user.phoneNumber = phoneNumber;
    return this;
  }
  
  build(): User {
    if (!this.user.email || !this.user.password) {
      throw new Error('Email and password required');
    }
    return this.user as User;
  }
}

// Usage
const user = new UserBuilder()
  .withCredentials('user@example.com', 'password123')
  .withProfile('John', 'Doe', 30)
  .withAddress('US', 'New York', '123 Main St', '10001')
  .withContact('555-1234')
  .build();
```

### Data Clumps

**Symptoms**: Same group of data items appear together in multiple places

**Problems**:
- Duplication
- Missing abstraction
- Hard to maintain

**Detection**:
```typescript
// Red flags:
// - Same parameters appear together
// - Same fields appear in multiple classes
// - Parameters deleted together

class Customer {
  name: string;
  street: string;
  city: string;
  state: string;
  zipCode: string;
}

class Order {
  shippingStreet: string;
  shippingCity: string;
  shippingState: string;
  shippingZipCode: string;
  
  billingStreet: string;
  billingCity: string;
  billingState: string;
  billingZipCode: string;
}

function printAddress(
  street: string,
  city: string,
  state: string,
  zipCode: string
) {
  // Address parameters always together
}
```

**Solutions**:
1. Extract Class
2. Introduce Parameter Object
3. Preserve Whole Object

```typescript
// Fixed: Extract Address class
class Address {
  constructor(
    public street: string,
    public city: string,
    public state: string,
    public zipCode: string
  ) {}
  
  format(): string {
    return `${this.street}, ${this.city}, ${this.state} ${this.zipCode}`;
  }
  
  validate(): boolean {
    return Boolean(
      this.street && 
      this.city && 
      this.state && 
      /^\d{5}$/.test(this.zipCode)
    );
  }
}

class Customer {
  constructor(
    public name: string,
    public address: Address
  ) {}
}

class Order {
  constructor(
    public shippingAddress: Address,
    public billingAddress: Address
  ) {}
}

function printAddress(address: Address) {
  console.log(address.format());
}
```

## Object-Orientation Abusers

Incomplete or incorrect application of object-oriented principles.

### Switch Statements

**Symptoms**: Complex switch or if-else chains based on type codes

**Problems**:
- Violates Open/Closed Principle
- Duplicated switch logic
- Hard to extend

**Detection**:
```typescript
// Red flags:
// - Switch on type code
// - Same switch appears in multiple places
// - Adding new type requires changes everywhere

class Employee {
  type: 'engineer' | 'manager' | 'salesman';
  
  calculatePay(): number {
    switch (this.type) {
      case 'engineer':
        return this.salary;
      case 'manager':
        return this.salary + this.bonus;
      case 'salesman':
        return this.salary + this.commission;
    }
  }
  
  calculateVacationDays(): number {
    switch (this.type) {
      case 'engineer':
        return 20;
      case 'manager':
        return 25;
      case 'salesman':
        return 15;
    }
  }
  
  getTitle(): string {
    switch (this.type) {
      case 'engineer':
        return 'Software Engineer';
      case 'manager':
        return 'Engineering Manager';
      case 'salesman':
        return 'Sales Representative';
    }
  }
}
```

**Solutions**:
1. Replace Type Code with Polymorphism
2. Replace Type Code with State/Strategy
3. Replace Conditional with Polymorphism

```typescript
// Fixed: Polymorphism
abstract class Employee {
  constructor(protected salary: number) {}
  
  abstract calculatePay(): number;
  abstract calculateVacationDays(): number;
  abstract getTitle(): string;
}

class Engineer extends Employee {
  calculatePay(): number {
    return this.salary;
  }
  
  calculateVacationDays(): number {
    return 20;
  }
  
  getTitle(): string {
    return 'Software Engineer';
  }
}

class Manager extends Employee {
  constructor(salary: number, private bonus: number) {
    super(salary);
  }
  
  calculatePay(): number {
    return this.salary + this.bonus;
  }
  
  calculateVacationDays(): number {
    return 25;
  }
  
  getTitle(): string {
    return 'Engineering Manager';
  }
}

class Salesman extends Employee {
  constructor(salary: number, private commission: number) {
    super(salary);
  }
  
  calculatePay(): number {
    return this.salary + this.commission;
  }
  
  calculateVacationDays(): number {
    return 15;
  }
  
  getTitle(): string {
    return 'Sales Representative';
  }
}
```

### Temporary Field

**Symptoms**: Field used only in certain circumstances

**Problems**:
- Confusing - why is field sometimes empty?
- Hard to understand when field is valid
- Often indicates missing abstraction

**Detection**:
```typescript
// Red flags:
// - Fields that are null/undefined most of the time
// - Fields used only in specific methods
// - Complex null checking

class Order {
  items: OrderItem[];
  customer: Customer;
  
  // These are only used during price calculation
  basePrice?: number;
  discounts?: number;
  taxes?: number;
  
  calculateTotal(): number {
    this.basePrice = this.calculateBasePrice();
    this.discounts = this.calculateDiscounts();
    this.taxes = this.calculateTaxes();
    
    return this.basePrice - this.discounts + this.taxes;
  }
  
  private calculateBasePrice(): number {
    return this.items.reduce((sum, item) => sum + item.price, 0);
  }
  
  private calculateDiscounts(): number {
    // Complex discount logic
    return 0;
  }
  
  private calculateTaxes(): number {
    // Complex tax logic
    return 0;
  }
}
```

**Solutions**:
1. Extract Class
2. Replace Method with Method Object

```typescript
// Fixed: Extract calculation class
class PriceCalculation {
  private basePrice: number;
  private discounts: number;
  private taxes: number;
  
  constructor(private order: Order) {
    this.basePrice = this.calculateBasePrice();
    this.discounts = this.calculateDiscounts();
    this.taxes = this.calculateTaxes();
  }
  
  getTotal(): number {
    return this.basePrice - this.discounts + this.taxes;
  }
  
  getBasePrice(): number {
    return this.basePrice;
  }
  
  getDiscounts(): number {
    return this.discounts;
  }
  
  getTaxes(): number {
    return this.taxes;
  }
  
  private calculateBasePrice(): number {
    return this.order.items.reduce((sum, item) => sum + item.price, 0);
  }
  
  private calculateDiscounts(): number {
    // Complex discount logic
    return 0;
  }
  
  private calculateTaxes(): number {
    // Complex tax logic
    return 0;
  }
}

class Order {
  items: OrderItem[];
  customer: Customer;
  
  calculateTotal(): number {
    const calculation = new PriceCalculation(this);
    return calculation.getTotal();
  }
  
  getPriceBreakdown(): PriceBreakdown {
    const calculation = new PriceCalculation(this);
    return {
      basePrice: calculation.getBasePrice(),
      discounts: calculation.getDiscounts(),
      taxes: calculation.getTaxes(),
      total: calculation.getTotal()
    };
  }
}
```

### Refused Bequest

**Symptoms**: Subclass uses only some of inherited methods/properties

**Problems**:
- Wrong hierarchy
- Violates Liskov Substitution Principle
- Confusing interface

**Detection**:
```typescript
// Red flags:
// - Subclass throws errors for parent methods
// - Subclass leaves parent methods empty
// - Subclass doesn't use parent fields

class Bird {
  fly() {
    console.log('Flying');
  }
  
  eat() {
    console.log('Eating');
  }
}

class Penguin extends Bird {
  fly() {
    throw new Error('Penguins cannot fly!');
  }
  
  swim() {
    console.log('Swimming');
  }
}
```

**Solutions**:
1. Replace Inheritance with Delegation
2. Extract Superclass
3. Push Down Method/Field

```typescript
// Fixed: Better hierarchy
interface Bird {
  eat(): void;
}

interface FlyingBird extends Bird {
  fly(): void;
}

interface SwimmingBird extends Bird {
  swim(): void;
}

class Sparrow implements FlyingBird {
  fly() {
    console.log('Flying');
  }
  
  eat() {
    console.log('Eating');
  }
}

class Penguin implements SwimmingBird {
  swim() {
    console.log('Swimming');
  }
  
  eat() {
    console.log('Eating');
  }
}
```

## Change Preventers

These smells make changes difficult - modifying one thing requires changes in many places.

### Divergent Change

**Symptoms**: One class commonly changed in different ways for different reasons

**Problems**:
- Violates Single Responsibility Principle
- Hard to maintain
- Changes affect multiple concerns

**Detection**:
```typescript
// Red flags:
// - "When we add a new database, we change methods X, Y, Z"
// - "When we add a new payment type, we change methods A, B, C"
// - One class changes for multiple reasons

class Product {
  // Database operations
  async saveToDatabase() { }
  async loadFromDatabase() { }
  async deleteFromDatabase() { }
  
  // Price calculations
  calculatePrice() { }
  applyDiscount() { }
  calculateTax() { }
  
  // Display formatting
  formatForDisplay() { }
  generateHTML() { }
  exportToJSON() { }
  
  // Validation
  validate() { }
  sanitizeInput() { }
}
```

**Solutions**:
1. Extract Class
2. Split up the behavior

```typescript
// Fixed: Separate concerns
class Product {
  constructor(
    public id: string,
    public name: string,
    public basePrice: number
  ) {}
}

class ProductRepository {
  async save(product: Product): Promise<void> { }
  async load(id: string): Promise<Product> { }
  async delete(id: string): Promise<void> { }
}

class ProductPricing {
  calculatePrice(product: Product): number { }
  applyDiscount(price: number, discount: Discount): number { }
  calculateTax(price: number): number { }
}

class ProductFormatter {
  formatForDisplay(product: Product): string { }
  generateHTML(product: Product): string { }
  exportToJSON(product: Product): string { }
}

class ProductValidator {
  validate(product: Product): ValidationResult { }
  sanitize(input: any): Product { }
}
```

This guide helps identify common code smells. For refactoring techniques to fix them, see REFACTORING-PATTERNS.md.
