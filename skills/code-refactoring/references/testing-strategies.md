# Testing Strategies for Safe Refactoring

This reference provides comprehensive strategies for safely refactoring code through effective testing practices.

## The Testing Safety Net

Before refactoring, establish a comprehensive safety net of tests. The goal is to verify behavior doesn't change during refactoring.

### Testing Pyramid for Refactoring

```
         /\
        /  \  Unit Tests (70%)
       /____\
      /      \
     / Integr \  Integration Tests (20%)
    /__________\
   /            \
  /     E2E      \  End-to-End Tests (10%)
 /________________\
```

**Priorities for Refactoring**:
1. **Before refactoring**: Comprehensive tests at appropriate levels
2. **During refactoring**: Tests remain green
3. **After refactoring**: Tests still pass, code is cleaner

## Test-Driven Refactoring Workflow

### Step 1: Add Tests (if missing)

```typescript
// Legacy code without tests
class OrderProcessor {
  processOrder(order: Order): ProcessedOrder {
    // Complex logic without tests
    const total = order.items.reduce((sum, item) => {
      const price = item.price * item.quantity;
      const discount = item.discount || 0;
      return sum + (price - discount);
    }, 0);
    
    const tax = total * 0.08;
    const shipping = total > 100 ? 0 : 10;
    
    return {
      orderId: order.id,
      subtotal: total,
      tax,
      shipping,
      total: total + tax + shipping
    };
  }
}

// Step 1: Add characterization tests documenting current behavior
describe('OrderProcessor', () => {
  let processor: OrderProcessor;
  
  beforeEach(() => {
    processor = new OrderProcessor();
  });
  
  describe('processOrder', () => {
    it('should calculate subtotal correctly', () => {
      const order = {
        id: '123',
        items: [
          { price: 10, quantity: 2, discount: 0 },
          { price: 20, quantity: 1, discount: 0 }
        ]
      };
      
      const result = processor.processOrder(order);
      
      expect(result.subtotal).toBe(40);
    });
    
    it('should apply discounts', () => {
      const order = {
        id: '123',
        items: [
          { price: 100, quantity: 1, discount: 10 }
        ]
      };
      
      const result = processor.processOrder(order);
      
      expect(result.subtotal).toBe(90);
    });
    
    it('should calculate 8% tax', () => {
      const order = {
        id: '123',
        items: [{ price: 100, quantity: 1, discount: 0 }]
      };
      
      const result = processor.processOrder(order);
      
      expect(result.tax).toBe(8);
    });
    
    it('should add $10 shipping for orders under $100', () => {
      const order = {
        id: '123',
        items: [{ price: 50, quantity: 1, discount: 0 }]
      };
      
      const result = processor.processOrder(order);
      
      expect(result.shipping).toBe(10);
    });
    
    it('should have free shipping for orders over $100', () => {
      const order = {
        id: '123',
        items: [{ price: 150, quantity: 1, discount: 0 }]
      };
      
      const result = processor.processOrder(order);
      
      expect(result.shipping).toBe(0);
    });
    
    it('should calculate correct total', () => {
      const order = {
        id: '123',
        items: [{ price: 100, quantity: 1, discount: 0 }]
      };
      
      const result = processor.processOrder(order);
      
      // 100 + 8 (tax) + 0 (shipping)
      expect(result.total).toBe(108);
    });
  });
});
```

### Step 2: Refactor with Green Tests

```typescript
// Refactor: Extract methods
class OrderProcessor {
  processOrder(order: Order): ProcessedOrder {
    const subtotal = this.calculateSubtotal(order.items);
    const tax = this.calculateTax(subtotal);
    const shipping = this.calculateShipping(subtotal);
    
    return {
      orderId: order.id,
      subtotal,
      tax,
      shipping,
      total: subtotal + tax + shipping
    };
  }
  
  private calculateSubtotal(items: OrderItem[]): number {
    return items.reduce((sum, item) => {
      const price = item.price * item.quantity;
      const discount = item.discount || 0;
      return sum + (price - discount);
    }, 0);
  }
  
  private calculateTax(subtotal: number): number {
    return subtotal * 0.08;
  }
  
  private calculateShipping(subtotal: number): number {
    return subtotal > 100 ? 0 : 10;
  }
}

// Run tests - should still be green!
// npm test
```

### Step 3: Further Refactoring

```typescript
// Extract classes for single responsibilities
class SubtotalCalculator {
  calculate(items: OrderItem[]): number {
    return items.reduce((sum, item) => {
      return sum + this.calculateItemTotal(item);
    }, 0);
  }
  
  private calculateItemTotal(item: OrderItem): number {
    const price = item.price * item.quantity;
    const discount = item.discount || 0;
    return price - discount;
  }
}

class TaxCalculator {
  private readonly TAX_RATE = 0.08;
  
  calculate(amount: number): number {
    return amount * this.TAX_RATE;
  }
}

class ShippingCalculator {
  private readonly FREE_SHIPPING_THRESHOLD = 100;
  private readonly STANDARD_SHIPPING_FEE = 10;
  
  calculate(subtotal: number): number {
    return subtotal > this.FREE_SHIPPING_THRESHOLD 
      ? 0 
      : this.STANDARD_SHIPPING_FEE;
  }
}

class OrderProcessor {
  constructor(
    private subtotalCalculator: SubtotalCalculator,
    private taxCalculator: TaxCalculator,
    private shippingCalculator: ShippingCalculator
  ) {}
  
  processOrder(order: Order): ProcessedOrder {
    const subtotal = this.subtotalCalculator.calculate(order.items);
    const tax = this.taxCalculator.calculate(subtotal);
    const shipping = this.shippingCalculator.calculate(subtotal);
    
    return {
      orderId: order.id,
      subtotal,
      tax,
      shipping,
      total: subtotal + tax + shipping
    };
  }
}

// Tests still pass! Now we can test components independently
describe('SubtotalCalculator', () => {
  it('should calculate item total with discount', () => {
    const calculator = new SubtotalCalculator();
    const items = [{ price: 100, quantity: 1, discount: 10 }];
    
    expect(calculator.calculate(items)).toBe(90);
  });
});

describe('TaxCalculator', () => {
  it('should calculate 8% tax', () => {
    const calculator = new TaxCalculator();
    
    expect(calculator.calculate(100)).toBe(8);
  });
});

describe('ShippingCalculator', () => {
  it('should charge shipping for orders under $100', () => {
    const calculator = new ShippingCalculator();
    
    expect(calculator.calculate(50)).toBe(10);
  });
  
  it('should provide free shipping for orders over $100', () => {
    const calculator = new ShippingCalculator();
    
    expect(calculator.calculate(150)).toBe(0);
  });
});
```

## Testing Strategies by Refactoring Type

### Extract Method Refactoring

```typescript
// Before refactoring: Test the entire method
describe('UserService.createUser', () => {
  it('should create user with hashed password', async () => {
    const service = new UserService();
    const user = await service.createUser({
      email: 'test@example.com',
      password: 'plain123',
      name: 'Test User'
    });
    
    expect(user.email).toBe('test@example.com');
    expect(user.password).not.toBe('plain123');
    expect(user.password.length).toBeGreaterThan(20);
  });
});

// After extract method: Test extracted methods independently
describe('PasswordHasher', () => {
  it('should hash password with bcrypt', async () => {
    const hasher = new PasswordHasher();
    const hashed = await hasher.hash('plain123');
    
    expect(hashed).not.toBe('plain123');
    expect(hashed.length).toBeGreaterThan(20);
  });
  
  it('should verify hashed password', async () => {
    const hasher = new PasswordHasher();
    const hashed = await hasher.hash('plain123');
    
    expect(await hasher.verify('plain123', hashed)).toBe(true);
    expect(await hasher.verify('wrong', hashed)).toBe(false);
  });
});

describe('UserService.createUser', () => {
  it('should create user with hashed password', async () => {
    const mockHasher = {
      hash: jest.fn().mockResolvedValue('hashed_password')
    };
    const service = new UserService(mockHasher);
    
    const user = await service.createUser({
      email: 'test@example.com',
      password: 'plain123',
      name: 'Test User'
    });
    
    expect(mockHasher.hash).toHaveBeenCalledWith('plain123');
    expect(user.password).toBe('hashed_password');
  });
});
```

### Extract Class Refactoring

```typescript
// Before: Single class with multiple responsibilities
class User {
  async sendWelcomeEmail() {
    const transporter = nodemailer.createTransport({...});
    await transporter.sendMail({
      to: this.email,
      subject: 'Welcome',
      html: '<h1>Welcome!</h1>'
    });
  }
}

// Test before refactoring
describe('User.sendWelcomeEmail', () => {
  it('should send welcome email', async () => {
    const mockTransport = {
      sendMail: jest.fn().mockResolvedValue({ messageId: '123' })
    };
    jest.spyOn(nodemailer, 'createTransport').mockReturnValue(mockTransport);
    
    const user = new User({ email: 'test@example.com' });
    await user.sendWelcomeEmail();
    
    expect(mockTransport.sendMail).toHaveBeenCalledWith(
      expect.objectContaining({
        to: 'test@example.com',
        subject: 'Welcome'
      })
    );
  });
});

// After refactoring: Separate email service
class EmailService {
  constructor(private transporter: Transporter) {}
  
  async sendWelcomeEmail(email: string): Promise<void> {
    await this.transporter.sendMail({
      to: email,
      subject: 'Welcome',
      html: '<h1>Welcome!</h1>'
    });
  }
}

class User {
  constructor(
    private email: string,
    private emailService: EmailService
  ) {}
  
  async sendWelcomeEmail() {
    await this.emailService.sendWelcomeEmail(this.email);
  }
}

// Test after refactoring
describe('EmailService', () => {
  it('should send welcome email', async () => {
    const mockTransport = {
      sendMail: jest.fn().mockResolvedValue({ messageId: '123' })
    };
    const emailService = new EmailService(mockTransport);
    
    await emailService.sendWelcomeEmail('test@example.com');
    
    expect(mockTransport.sendMail).toHaveBeenCalledWith(
      expect.objectContaining({
        to: 'test@example.com',
        subject: 'Welcome'
      })
    );
  });
});

describe('User', () => {
  it('should delegate welcome email to email service', async () => {
    const mockEmailService = {
      sendWelcomeEmail: jest.fn().mockResolvedValue(undefined)
    };
    const user = new User('test@example.com', mockEmailService);
    
    await user.sendWelcomeEmail();
    
    expect(mockEmailService.sendWelcomeEmail).toHaveBeenCalledWith('test@example.com');
  });
});
```

### Replace Conditional with Polymorphism

```typescript
// Before: Switch statement
class PaymentProcessor {
  processPayment(payment: Payment): PaymentResult {
    switch (payment.type) {
      case 'credit_card':
        return this.processCreditCard(payment);
      case 'paypal':
        return this.processPayPal(payment);
      case 'bank_transfer':
        return this.processBankTransfer(payment);
      default:
        throw new Error('Unknown payment type');
    }
  }
}

// Test before refactoring
describe('PaymentProcessor', () => {
  it('should process credit card payment', () => {
    const processor = new PaymentProcessor();
    const payment = { type: 'credit_card', amount: 100, cardNumber: '1234' };
    
    const result = processor.processPayment(payment);
    
    expect(result.status).toBe('success');
  });
  
  it('should process PayPal payment', () => {
    const processor = new PaymentProcessor();
    const payment = { type: 'paypal', amount: 100, email: 'user@example.com' };
    
    const result = processor.processPayment(payment);
    
    expect(result.status).toBe('success');
  });
});

// After refactoring: Polymorphic payment methods
interface PaymentMethod {
  process(amount: number): Promise<PaymentResult>;
}

class CreditCardPayment implements PaymentMethod {
  constructor(private cardNumber: string, private cvv: string) {}
  
  async process(amount: number): Promise<PaymentResult> {
    // Process credit card
    return { status: 'success', transactionId: '123' };
  }
}

class PayPalPayment implements PaymentMethod {
  constructor(private email: string, private token: string) {}
  
  async process(amount: number): Promise<PaymentResult> {
    // Process PayPal
    return { status: 'success', transactionId: '456' };
  }
}

class PaymentProcessor {
  async processPayment(paymentMethod: PaymentMethod, amount: number): Promise<PaymentResult> {
    return await paymentMethod.process(amount);
  }
}

// Test after refactoring - test each payment method independently
describe('CreditCardPayment', () => {
  it('should process payment', async () => {
    const payment = new CreditCardPayment('1234', '123');
    
    const result = await payment.process(100);
    
    expect(result.status).toBe('success');
  });
});

describe('PayPalPayment', () => {
  it('should process payment', async () => {
    const payment = new PayPalPayment('user@example.com', 'token123');
    
    const result = await payment.process(100);
    
    expect(result.status).toBe('success');
  });
});

describe('PaymentProcessor', () => {
  it('should delegate to payment method', async () => {
    const mockPaymentMethod = {
      process: jest.fn().mockResolvedValue({ status: 'success' })
    };
    const processor = new PaymentProcessor();
    
    await processor.processPayment(mockPaymentMethod, 100);
    
    expect(mockPaymentMethod.process).toHaveBeenCalledWith(100);
  });
});
```

## Testing Legacy Code

### Characterization Tests

When refactoring code without tests, write characterization tests that document current behavior:

```typescript
// Legacy code with no tests
function calculateDiscount(customer: any, orderTotal: number): number {
  let discount = 0;
  
  if (customer.type === 'gold') {
    if (orderTotal > 1000) {
      discount = orderTotal * 0.15;
    } else {
      discount = orderTotal * 0.10;
    }
  } else if (customer.type === 'silver') {
    if (orderTotal > 500) {
      discount = orderTotal * 0.08;
    } else {
      discount = orderTotal * 0.05;
    }
  } else {
    if (orderTotal > 100) {
      discount = orderTotal * 0.02;
    }
  }
  
  if (customer.loyaltyYears > 5) {
    discount = discount * 1.1;
  }
  
  return Math.min(discount, orderTotal * 0.5);
}

// Step 1: Write characterization tests documenting behavior
describe('calculateDiscount - characterization tests', () => {
  describe('gold customers', () => {
    it('should give 15% discount for orders over $1000', () => {
      const customer = { type: 'gold', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 1500)).toBe(225); // 15%
    });
    
    it('should give 10% discount for orders under $1000', () => {
      const customer = { type: 'gold', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 500)).toBe(50); // 10%
    });
  });
  
  describe('silver customers', () => {
    it('should give 8% discount for orders over $500', () => {
      const customer = { type: 'silver', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 1000)).toBe(80); // 8%
    });
    
    it('should give 5% discount for orders under $500', () => {
      const customer = { type: 'silver', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 300)).toBe(15); // 5%
    });
  });
  
  describe('regular customers', () => {
    it('should give 2% discount for orders over $100', () => {
      const customer = { type: 'regular', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 500)).toBe(10); // 2%
    });
    
    it('should give no discount for orders under $100', () => {
      const customer = { type: 'regular', loyaltyYears: 0 };
      expect(calculateDiscount(customer, 50)).toBe(0);
    });
  });
  
  describe('loyalty bonus', () => {
    it('should increase discount by 10% for customers with 5+ years', () => {
      const customer = { type: 'gold', loyaltyYears: 6 };
      expect(calculateDiscount(customer, 1000)).toBe(110); // 100 * 1.1
    });
  });
  
  describe('maximum discount', () => {
    it('should cap discount at 50% of order total', () => {
      const customer = { type: 'gold', loyaltyYears: 10 };
      expect(calculateDiscount(customer, 1000)).toBe(500); // Capped at 50%
    });
  });
});

// Step 2: Refactor with confidence
class DiscountCalculator {
  private readonly GOLD_HIGH_DISCOUNT = 0.15;
  private readonly GOLD_LOW_DISCOUNT = 0.10;
  private readonly GOLD_THRESHOLD = 1000;
  
  private readonly SILVER_HIGH_DISCOUNT = 0.08;
  private readonly SILVER_LOW_DISCOUNT = 0.05;
  private readonly SILVER_THRESHOLD = 500;
  
  private readonly REGULAR_DISCOUNT = 0.02;
  private readonly REGULAR_THRESHOLD = 100;
  
  private readonly LOYALTY_BONUS = 1.1;
  private readonly LOYALTY_YEARS_THRESHOLD = 5;
  
  private readonly MAX_DISCOUNT_RATE = 0.5;
  
  calculate(customer: Customer, orderTotal: number): number {
    const baseDiscount = this.calculateBaseDiscount(customer, orderTotal);
    const withLoyaltyBonus = this.applyLoyaltyBonus(baseDiscount, customer);
    return this.capDiscount(withLoyaltyBonus, orderTotal);
  }
  
  private calculateBaseDiscount(customer: Customer, orderTotal: number): number {
    switch (customer.type) {
      case 'gold':
        return this.calculateGoldDiscount(orderTotal);
      case 'silver':
        return this.calculateSilverDiscount(orderTotal);
      default:
        return this.calculateRegularDiscount(orderTotal);
    }
  }
  
  private calculateGoldDiscount(orderTotal: number): number {
    const rate = orderTotal > this.GOLD_THRESHOLD 
      ? this.GOLD_HIGH_DISCOUNT 
      : this.GOLD_LOW_DISCOUNT;
    return orderTotal * rate;
  }
  
  private calculateSilverDiscount(orderTotal: number): number {
    const rate = orderTotal > this.SILVER_THRESHOLD 
      ? this.SILVER_HIGH_DISCOUNT 
      : this.SILVER_LOW_DISCOUNT;
    return orderTotal * rate;
  }
  
  private calculateRegularDiscount(orderTotal: number): number {
    return orderTotal > this.REGULAR_THRESHOLD 
      ? orderTotal * this.REGULAR_DISCOUNT 
      : 0;
  }
  
  private applyLoyaltyBonus(discount: number, customer: Customer): number {
    return customer.loyaltyYears > this.LOYALTY_YEARS_THRESHOLD 
      ? discount * this.LOYALTY_BONUS 
      : discount;
  }
  
  private capDiscount(discount: number, orderTotal: number): number {
    return Math.min(discount, orderTotal * this.MAX_DISCOUNT_RATE);
  }
}

// All characterization tests should still pass!
```

## Approval Testing

For complex outputs, use approval testing (golden master testing):

```typescript
import { verify } from 'approvals';

describe('ReportGenerator', () => {
  it('should generate report with correct format', () => {
    const generator = new ReportGenerator();
    const data = {
      customers: [...],
      orders: [...],
      revenue: 10000
    };
    
    const report = generator.generate(data);
    
    // Approve the output - creates .approved file on first run
    verify(report);
    
    // Future runs compare against approved file
    // If different, test fails and shows diff
  });
});
```

## Mutation Testing

Verify test quality by introducing mutations:

```typescript
// Install: npm install --save-dev @stryker-mutator/core

// stryker.conf.json
{
  "mutator": "typescript",
  "packageManager": "npm",
  "testRunner": "jest",
  "coverageAnalysis": "perTest",
  "mutate": [
    "src/**/*.ts",
    "!src/**/*.spec.ts"
  ]
}

// Run: npx stryker run

// Example mutation results:
// Original: if (price > 100)
// Mutant 1: if (price >= 100) - KILLED by test
// Mutant 2: if (price < 100) - KILLED by test
// Mutant 3: if (true) - SURVIVED (weak test!)
```

## Best Practices

1. **Red-Green-Refactor Cycle**
   - Write test (red)
   - Make it pass (green)
   - Refactor (tests stay green)

2. **Test One Thing at a Time**
   - Each test should verify one behavior
   - Clear, focused assertions

3. **Use Test Doubles Appropriately**
   - Mock external dependencies
   - Use real objects for value objects
   - Avoid mocking what you don't own

4. **Keep Tests Fast**
   - Unit tests < 1ms
   - Integration tests < 100ms
   - Full suite < 10 minutes

5. **Maintain Test Quality**
   - Refactor tests too
   - Remove duplicate test code
   - Keep tests readable

This comprehensive testing approach ensures safe, confident refactoring with minimal risk of introducing bugs.
