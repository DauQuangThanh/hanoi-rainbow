# Refactoring Patterns Catalog

This reference provides a comprehensive catalog of refactoring patterns with detailed examples and use cases.

## Composing Methods

### Extract Method

**Problem**: Code fragment that can be grouped together
**Solution**: Turn the fragment into a method with a descriptive name

```typescript
// Before
function printOwing() {
  printBanner();
  
  // Print details
  console.log('name: ' + name);
  console.log('amount: ' + getOutstanding());
}

// After
function printOwing() {
  printBanner();
  printDetails(getOutstanding());
}

function printDetails(outstanding: number) {
  console.log('name: ' + name);
  console.log('amount: ' + outstanding);
}

// Advanced example: Extract method with local variables
// Before
function calculateTotal(order: Order) {
  let basePrice = order.quantity * order.itemPrice;
  let discount = Math.max(0, order.quantity - 500) * order.itemPrice * 0.05;
  let shipping = Math.min(basePrice * 0.1, 100);
  return basePrice - discount + shipping;
}

// After
function calculateTotal(order: Order) {
  const basePrice = calculateBasePrice(order);
  const discount = calculateDiscount(order);
  const shipping = calculateShipping(basePrice);
  return basePrice - discount + shipping;
}

function calculateBasePrice(order: Order): number {
  return order.quantity * order.itemPrice;
}

function calculateDiscount(order: Order): number {
  const discountableQuantity = Math.max(0, order.quantity - 500);
  return discountableQuantity * order.itemPrice * 0.05;
}

function calculateShipping(basePrice: number): number {
  return Math.min(basePrice * 0.1, 100);
}
```

### Inline Method

**Problem**: Method body is as clear as its name
**Solution**: Replace calls with method body

```typescript
// Before
function getRating() {
  return moreThanFiveLateDeliveries() ? 2 : 1;
}

function moreThanFiveLateDeliveries() {
  return numberOfLateDeliveries > 5;
}

// After
function getRating() {
  return numberOfLateDeliveries > 5 ? 2 : 1;
}
```

### Extract Variable

**Problem**: Complex expression is hard to understand
**Solution**: Break expression into intermediate variables

```typescript
// Before
if (
  platform.toUpperCase().indexOf('MAC') > -1 &&
  browser.toUpperCase().indexOf('IE') > -1 &&
  wasInitialized() &&
  resize > 0
) {
  // Do something
}

// After
const isMacOS = platform.toUpperCase().indexOf('MAC') > -1;
const isIE = browser.toUpperCase().indexOf('IE') > -1;
const wasResized = resize > 0;

if (isMacOS && isIE && wasInitialized() && wasResized) {
  // Do something
}
```

### Inline Variable

**Problem**: Variable doesn't provide more clarity than expression
**Solution**: Replace variable references with expression

```typescript
// Before
const basePrice = order.basePrice;
return basePrice > 1000;

// After
return order.basePrice > 1000;
```

### Replace Temp with Query

**Problem**: Temporary variable holds result of expression
**Solution**: Extract expression into method, replace all references

```typescript
// Before
function calculateTotal(order: Order) {
  const basePrice = order.quantity * order.itemPrice;
  if (basePrice > 1000) {
    return basePrice * 0.95;
  }
  return basePrice * 0.98;
}

// After
function calculateTotal(order: Order) {
  if (basePrice(order) > 1000) {
    return basePrice(order) * 0.95;
  }
  return basePrice(order) * 0.98;
}

function basePrice(order: Order): number {
  return order.quantity * order.itemPrice;
}
```

### Split Temporary Variable

**Problem**: Temporary variable assigned more than once (excluding loops)
**Solution**: Use separate variables for each assignment

```typescript
// Before
let temp = 2 * (height + width);
console.log(temp);
temp = height * width;
console.log(temp);

// After
const perimeter = 2 * (height + width);
console.log(perimeter);
const area = height * width;
console.log(area);
```

### Remove Assignments to Parameters

**Problem**: Code assigns values to parameters
**Solution**: Use local variable instead

```typescript
// Before
function discount(inputVal: number, quantity: number): number {
  if (inputVal > 50) inputVal -= 2;
  if (quantity > 100) inputVal -= 1;
  return inputVal;
}

// After
function discount(inputVal: number, quantity: number): number {
  let result = inputVal;
  if (inputVal > 50) result -= 2;
  if (quantity > 100) result -= 1;
  return result;
}
```

### Replace Method with Method Object

**Problem**: Long method with many local variables that can't be extracted
**Solution**: Create class with local variables as fields

```typescript
// Before
function price(order: Order) {
  let primaryBasePrice;
  let secondaryBasePrice;
  let tertiaryBasePrice;
  // Long calculation with these variables
}

// After
class PriceCalculator {
  private primaryBasePrice: number;
  private secondaryBasePrice: number;
  private tertiaryBasePrice: number;
  
  constructor(private order: Order) {}
  
  compute(): number {
    this.primaryBasePrice = this.calculatePrimaryBasePrice();
    this.secondaryBasePrice = this.calculateSecondaryBasePrice();
    this.tertiaryBasePrice = this.calculateTertiaryBasePrice();
    return this.computeFinalPrice();
  }
  
  private calculatePrimaryBasePrice(): number {
    // Calculation
  }
  
  private calculateSecondaryBasePrice(): number {
    // Calculation
  }
  
  private calculateTertiaryBasePrice(): number {
    // Calculation
  }
  
  private computeFinalPrice(): number {
    // Final calculation
  }
}

function price(order: Order): number {
  return new PriceCalculator(order).compute();
}
```

## Moving Features Between Objects

### Move Method

**Problem**: Method used more in another class
**Solution**: Create new method in target class, delegate or remove old method

```typescript
// Before
class Account {
  overdraftCharge() {
    if (this.type.isPremium()) {
      // Premium overdraft calculation
      return this.daysOverdrawn * 2.5;
    } else {
      // Standard overdraft calculation
      return this.daysOverdrawn * 1.75;
    }
  }
}

// After
class AccountType {
  overdraftCharge(daysOverdrawn: number): number {
    if (this.isPremium()) {
      return daysOverdrawn * 2.5;
    } else {
      return daysOverdrawn * 1.75;
    }
  }
}

class Account {
  overdraftCharge() {
    return this.type.overdraftCharge(this.daysOverdrawn);
  }
}
```

### Move Field

**Problem**: Field used more in another class
**Solution**: Create field in target class, redirect all users

```typescript
// Before
class Customer {
  private discountRate: number;
  
  getDiscountRate(): number {
    return this.discountRate;
  }
}

class Order {
  getDiscountedPrice(): number {
    return this.basePrice * (1 - this.customer.getDiscountRate());
  }
}

// After (if discount is based on customer type)
class CustomerType {
  private discountRate: number;
  
  getDiscountRate(): number {
    return this.discountRate;
  }
}

class Customer {
  getDiscountRate(): number {
    return this.type.getDiscountRate();
  }
}

class Order {
  getDiscountedPrice(): number {
    return this.basePrice * (1 - this.customer.getDiscountRate());
  }
}
```

### Extract Class

**Problem**: Class doing work of two
**Solution**: Create new class, move relevant fields and methods

```typescript
// Before
class Person {
  name: string;
  officeAreaCode: string;
  officeNumber: string;
  
  getTelephoneNumber(): string {
    return `(${this.officeAreaCode}) ${this.officeNumber}`;
  }
}

// After
class TelephoneNumber {
  constructor(
    private areaCode: string,
    private number: string
  ) {}
  
  toString(): string {
    return `(${this.areaCode}) ${this.number}`;
  }
  
  getAreaCode(): string {
    return this.areaCode;
  }
  
  getNumber(): string {
    return this.number;
  }
}

class Person {
  name: string;
  private officeTelephone: TelephoneNumber;
  
  getTelephoneNumber(): string {
    return this.officeTelephone.toString();
  }
  
  getOfficeTelephone(): TelephoneNumber {
    return this.officeTelephone;
  }
}
```

### Inline Class

**Problem**: Class not doing much
**Solution**: Move all features to another class, delete original

```typescript
// Before
class Address {
  constructor(private zipCode: string) {}
  
  getZipCode(): string {
    return this.zipCode;
  }
}

class Person {
  constructor(
    private name: string,
    private address: Address
  ) {}
  
  getZipCode(): string {
    return this.address.getZipCode();
  }
}

// After
class Person {
  constructor(
    private name: string,
    private zipCode: string
  ) {}
  
  getZipCode(): string {
    return this.zipCode;
  }
}
```

### Hide Delegate

**Problem**: Client gets object from field of server object, then calls method
**Solution**: Create method on server that hides the delegate

```typescript
// Before
class Person {
  department: Department;
}

class Department {
  manager: Person;
  
  getManager(): Person {
    return this.manager;
  }
}

// Client code
const manager = john.department.getManager();

// After
class Person {
  private department: Department;
  
  getManager(): Person {
    return this.department.getManager();
  }
}

// Client code
const manager = john.getManager();
```

### Remove Middle Man

**Problem**: Class doing too much delegating
**Solution**: Get client to call delegate directly

```typescript
// Before (too much delegation)
class Person {
  private department: Department;
  
  getManager(): Person {
    return this.department.getManager();
  }
  
  getBudget(): number {
    return this.department.getBudget();
  }
  
  getLocation(): string {
    return this.department.getLocation();
  }
  
  // Many more delegating methods...
}

// After
class Person {
  getDepartment(): Department {
    return this.department;
  }
}

// Client code
const manager = john.getDepartment().getManager();
```

## Organizing Data

### Self Encapsulate Field

**Problem**: Direct access to private field
**Solution**: Use getters and setters

```typescript
// Before
class Range {
  private low: number;
  private high: number;
  
  includes(arg: number): boolean {
    return arg >= this.low && arg <= this.high;
  }
}

// After
class Range {
  private low: number;
  private high: number;
  
  includes(arg: number): boolean {
    return arg >= this.getLow() && arg <= this.getHigh();
  }
  
  getLow(): number {
    return this.low;
  }
  
  getHigh(): number {
    return this.high;
  }
}
```

### Replace Data Value with Object

**Problem**: Data item needs additional data or behavior
**Solution**: Turn data item into object

```typescript
// Before
class Order {
  customer: string;
  
  constructor(customerName: string) {
    this.customer = customerName;
  }
}

// After
class Customer {
  constructor(private name: string) {}
  
  getName(): string {
    return this.name;
  }
}

class Order {
  customer: Customer;
  
  constructor(customerName: string) {
    this.customer = new Customer(customerName);
  }
  
  getCustomerName(): string {
    return this.customer.getName();
  }
}
```

### Change Value to Reference

**Problem**: Many identical instances of a class need to be single object
**Solution**: Turn object into reference object

```typescript
// Before
class Customer {
  constructor(private name: string) {}
}

class Order {
  private customer: Customer;
  
  constructor(customerName: string) {
    this.customer = new Customer(customerName);
  }
}

// After
class Customer {
  private static instances = new Map<string, Customer>();
  
  private constructor(private name: string) {}
  
  static get(name: string): Customer {
    if (!Customer.instances.has(name)) {
      Customer.instances.set(name, new Customer(name));
    }
    return Customer.instances.get(name)!;
  }
}

class Order {
  private customer: Customer;
  
  constructor(customerName: string) {
    this.customer = Customer.get(customerName);
  }
}
```

### Replace Array with Object

**Problem**: Array elements mean different things
**Solution**: Replace array with object with fields for each element

```typescript
// Before
const row = ['Liverpool', '15'];
const name = row[0];
const wins = parseInt(row[1]);

// After
interface Performance {
  name: string;
  wins: number;
}

const row: Performance = { name: 'Liverpool', wins: 15 };
const name = row.name;
const wins = row.wins;
```

### Duplicate Observed Data

**Problem**: Domain data stored in GUI components
**Solution**: Copy data to domain object, observe changes

```typescript
// Before
class TextField {
  private text: string;
  
  getText(): string {
    return this.text;
  }
  
  setText(value: string) {
    this.text = value;
    // Update view
  }
}

// After
interface Observer {
  update(value: string): void;
}

class DomainData {
  private observers: Observer[] = [];
  private value: string;
  
  getValue(): string {
    return this.value;
  }
  
  setValue(value: string) {
    this.value = value;
    this.notifyObservers();
  }
  
  addObserver(observer: Observer) {
    this.observers.push(observer);
  }
  
  private notifyObservers() {
    for (const observer of this.observers) {
      observer.update(this.value);
    }
  }
}

class TextField implements Observer {
  private text: string;
  
  constructor(private data: DomainData) {
    data.addObserver(this);
  }
  
  getText(): string {
    return this.text;
  }
  
  setText(value: string) {
    this.text = value;
    this.data.setValue(value);
  }
  
  update(value: string) {
    this.text = value;
    // Update view
  }
}
```

### Change Unidirectional Association to Bidirectional

**Problem**: Two classes need to use each other's features
**Solution**: Add back-pointers

```typescript
// Before
class Order {
  customer: Customer;
  
  getCustomer(): Customer {
    return this.customer;
  }
}

class Customer {
  // No reference to orders
}

// After
class Order {
  private customer: Customer;
  
  constructor(customer: Customer) {
    this.customer = customer;
    customer.addOrder(this);
  }
  
  getCustomer(): Customer {
    return this.customer;
  }
  
  setCustomer(customer: Customer) {
    if (this.customer) {
      this.customer.removeOrder(this);
    }
    this.customer = customer;
    customer.addOrder(this);
  }
}

class Customer {
  private orders: Set<Order> = new Set();
  
  addOrder(order: Order) {
    this.orders.add(order);
  }
  
  removeOrder(order: Order) {
    this.orders.delete(order);
  }
  
  getOrders(): Order[] {
    return Array.from(this.orders);
  }
}
```

### Replace Magic Number with Symbolic Constant

**Problem**: Numeric literal with special meaning
**Solution**: Create constant with human-readable name

```typescript
// Before
function potentialEnergy(mass: number, height: number): number {
  return mass * 9.81 * height;
}

// After
const GRAVITATIONAL_CONSTANT = 9.81;

function potentialEnergy(mass: number, height: number): number {
  return mass * GRAVITATIONAL_CONSTANT * height;
}
```

### Encapsulate Field

**Problem**: Public field
**Solution**: Make private and provide accessors

```typescript
// Before
class Person {
  name: string;
}

// After
class Person {
  private name: string;
  
  getName(): string {
    return this.name;
  }
  
  setName(name: string) {
    this.name = name;
  }
}
```

### Encapsulate Collection

**Problem**: Method returns collection
**Solution**: Return read-only view, provide add/remove methods

```typescript
// Before
class Course {
  private students: Student[] = [];
  
  getStudents(): Student[] {
    return this.students;
  }
  
  setStudents(students: Student[]) {
    this.students = students;
  }
}

// Usage
const course = new Course();
course.getStudents().push(new Student('John')); // Direct manipulation!

// After
class Course {
  private students: Student[] = [];
  
  getStudents(): ReadonlyArray<Student> {
    return Object.freeze([...this.students]);
  }
  
  addStudent(student: Student) {
    this.students.push(student);
  }
  
  removeStudent(student: Student) {
    const index = this.students.indexOf(student);
    if (index !== -1) {
      this.students.splice(index, 1);
    }
  }
  
  getNumberOfStudents(): number {
    return this.students.length;
  }
}

// Usage
const course = new Course();
course.addStudent(new Student('John'));
```

### Replace Type Code with Class

**Problem**: Class has type code that affects behavior
**Solution**: Replace with class or enum

```typescript
// Before
class Person {
  static readonly O = 0;
  static readonly A = 1;
  static readonly B = 2;
  static readonly AB = 3;
  
  private bloodGroup: number;
  
  constructor(bloodGroup: number) {
    this.bloodGroup = bloodGroup;
  }
}

// After
class BloodGroup {
  static readonly O = new BloodGroup('O');
  static readonly A = new BloodGroup('A');
  static readonly B = new BloodGroup('B');
  static readonly AB = new BloodGroup('AB');
  
  private constructor(private code: string) {}
  
  getCode(): string {
    return this.code;
  }
}

class Person {
  private bloodGroup: BloodGroup;
  
  constructor(bloodGroup: BloodGroup) {
    this.bloodGroup = bloodGroup;
  }
  
  getBloodGroup(): BloodGroup {
    return this.bloodGroup;
  }
}

// Or with TypeScript enum
enum BloodGroup {
  O = 'O',
  A = 'A',
  B = 'B',
  AB = 'AB'
}

class Person {
  constructor(private bloodGroup: BloodGroup) {}
  
  getBloodGroup(): BloodGroup {
    return this.bloodGroup;
  }
}
```

### Replace Type Code with Subclasses

**Problem**: Type code affects class behavior
**Solution**: Create subclass for each type code value

```typescript
// Before
class Employee {
  private type: string;
  
  constructor(type: string) {
    this.type = type;
  }
  
  payAmount(): number {
    switch (this.type) {
      case 'engineer':
        return this.monthlySalary;
      case 'salesman':
        return this.monthlySalary + this.commission;
      case 'manager':
        return this.monthlySalary + this.bonus;
      default:
        throw new Error('Invalid employee type');
    }
  }
}

// After
abstract class Employee {
  constructor(protected monthlySalary: number) {}
  
  abstract payAmount(): number;
}

class Engineer extends Employee {
  payAmount(): number {
    return this.monthlySalary;
  }
}

class Salesman extends Employee {
  constructor(
    monthlySalary: number,
    private commission: number
  ) {
    super(monthlySalary);
  }
  
  payAmount(): number {
    return this.monthlySalary + this.commission;
  }
}

class Manager extends Employee {
  constructor(
    monthlySalary: number,
    private bonus: number
  ) {
    super(monthlySalary);
  }
  
  payAmount(): number {
    return this.monthlySalary + this.bonus;
  }
}
```

This catalog provides foundational refactoring patterns. For code smell identification and testing strategies, see the companion reference files.
