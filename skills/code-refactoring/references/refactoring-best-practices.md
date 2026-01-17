# Refactoring Best Practices


### 1. Test-Driven Refactoring

Always refactor with a safety net of tests:

```typescript
// Step 1: Write tests first (if they don't exist)
describe('UserService.createUser', () => {
  it('should create user with valid data', async () => {
    const result = await userService.createUser({
      email: 'test@example.com',
      password: 'password123',
      name: 'Test User'
    });
    
    expect(result).toMatchObject({
      email: 'test@example.com',
      name: 'Test User'
    });
    expect(result.id).toBeDefined();
    expect(result.password).not.toBe('password123');
  });
  
  it('should throw error for duplicate email', async () => {
    await userService.createUser({
      email: 'existing@example.com',
      password: 'password123',
      name: 'First User'
    });
    
    await expect(
      userService.createUser({
        email: 'existing@example.com',
        password: 'password456',
        name: 'Second User'
      })
    ).rejects.toThrow('User already exists');
  });
});

// Step 2: Refactor with confidence
// Step 3: Run tests after each small change
// Step 4: Commit frequently
```

### 2. Small, Incremental Changes

Break refactoring into small steps:

```bash
# Bad: Big bang refactoring
git commit -m "Refactored entire codebase"

# Good: Small, focused commits
git commit -m "Extract validateEmail method from createUser"
git commit -m "Extract hashPassword method from createUser"
git commit -m "Introduce UserRepository interface"
git commit -m "Move database logic to PostgresUserRepository"
git commit -m "Extract CreateUserUseCase from controller"
```

### 3. Apply SOLID Principles

**Single Responsibility Principle**
```typescript
// Before: Multiple responsibilities
class UserService {
  createUser(data: any) { }
  sendEmail(email: string) { }
  validateUser(user: any) { }
  generateReport(userId: string) { }
}

// After: Single responsibility
class UserService {
  createUser(data: CreateUserDto): Promise<User> { }
  updateUser(id: string, data: UpdateUserDto): Promise<User> { }
  deleteUser(id: string): Promise<void> { }
}

class EmailService {
  send(email: Email): Promise<void> { }
}

class UserValidator {
  validate(user: User): ValidationResult { }
}

class UserReportGenerator {
  generate(userId: string): Promise<Report> { }
}
```

**Open/Closed Principle**
```typescript
// Before: Modification required for new types
class DiscountCalculator {
  calculate(order: Order): number {
    if (order.customerType === 'regular') {
      return order.total * 0.05;
    } else if (order.customerType === 'premium') {
      return order.total * 0.10;
    } else if (order.customerType === 'vip') {
      return order.total * 0.15;
    }
    return 0;
  }
}

// After: Extension without modification
interface DiscountStrategy {
  calculate(order: Order): number;
}

class RegularDiscount implements DiscountStrategy {
  calculate(order: Order): number {
    return order.total * 0.05;
  }
}

class PremiumDiscount implements DiscountStrategy {
  calculate(order: Order): number {
    return order.total * 0.10;
  }
}

class VIPDiscount implements DiscountStrategy {
  calculate(order: Order): number {
    return order.total * 0.15;
  }
}

class DiscountCalculator {
  constructor(private strategy: DiscountStrategy) {}
  
  calculate(order: Order): number {
    return this.strategy.calculate(order);
  }
}
```

### 4. Eliminate Duplication (DRY)

```typescript
// Before: Duplicated validation logic
class UserController {
  async createUser(req: Request, res: Response) {
    const { email, password } = req.body;
    
    if (!email || !email.includes('@')) {
      return res.status(400).json({ error: 'Invalid email' });
    }
    if (!password || password.length < 8) {
      return res.status(400).json({ error: 'Password too short' });
    }
    
    // Create user...
  }
  
  async updateUser(req: Request, res: Response) {
    const { email } = req.body;
    
    if (email && !email.includes('@')) {
      return res.status(400).json({ error: 'Invalid email' });
    }
    
    // Update user...
  }
}

// After: Shared validation logic
class EmailValidator {
  static validate(email: string): void {
    if (!email || !email.includes('@')) {
      throw new ValidationError('Invalid email format');
    }
  }
}

class PasswordValidator {
  static validate(password: string): void {
    if (!password || password.length < 8) {
      throw new ValidationError('Password must be at least 8 characters');
    }
  }
}

class UserController {
  async createUser(req: Request, res: Response) {
    try {
      EmailValidator.validate(req.body.email);
      PasswordValidator.validate(req.body.password);
      
      // Create user...
    } catch (error) {
      if (error instanceof ValidationError) {
        return res.status(400).json({ error: error.message });
      }
      throw error;
    }
  }
  
  async updateUser(req: Request, res: Response) {
    try {
      if (req.body.email) {
        EmailValidator.validate(req.body.email);
      }
      
      // Update user...
    } catch (error) {
      if (error instanceof ValidationError) {
        return res.status(400).json({ error: error.message });
      }
      throw error;
    }
  }
}
```
