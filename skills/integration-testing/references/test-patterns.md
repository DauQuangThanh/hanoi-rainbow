# Integration Testing Patterns & Best Practices

This guide covers common patterns, anti-patterns, and best practices for integration testing.

## Test Design Patterns

### 1. Arrange-Act-Assert (AAA) Pattern

**Structure**
```javascript
test('should do something', async () => {
  // Arrange - Set up test data and dependencies
  const user = await createTestUser({ email: 'test@example.com' });
  const service = new OrderService();
  
  // Act - Execute the functionality being tested
  const order = await service.createOrder(user.id, { items: [...] });
  
  // Assert - Verify the outcome
  expect(order.status).toBe('pending');
  expect(order.user_id).toBe(user.id);
});
```

**Benefits**
- Clear test structure
- Easy to read and maintain
- Separates concerns
- Identifies missing steps

**Anti-pattern: Multiple Acts**
```javascript
// ❌ Bad - Multiple acts in one test
test('user workflow', async () => {
  const user = await createUser();
  const profile = await updateProfile(user.id); // Act 1
  const order = await createOrder(user.id); // Act 2
  const payment = await processPayment(order.id); // Act 3
  
  // Which assertion belongs to which act?
  expect(profile.name).toBe('Test');
  expect(order.status).toBe('completed');
});

// ✅ Good - Split into separate tests
test('should update user profile', async () => {
  const user = await createUser();
  
  const profile = await updateProfile(user.id);
  
  expect(profile.name).toBe('Test');
});

test('should create order for user', async () => {
  const user = await createUser();
  
  const order = await createOrder(user.id);
  
  expect(order.status).toBe('pending');
});
```

### 2. Test Data Builder Pattern

**Implementation**
```javascript
class UserBuilder {
  constructor() {
    this.user = {
      email: 'default@example.com',
      name: 'Default User',
      role: 'user',
      status: 'active'
    };
  }
  
  withEmail(email) {
    this.user.email = email;
    return this;
  }
  
  withName(name) {
    this.user.name = name;
    return this;
  }
  
  asAdmin() {
    this.user.role = 'admin';
    return this;
  }
  
  inactive() {
    this.user.status = 'inactive';
    return this;
  }
  
  build() {
    return { ...this.user };
  }
  
  async create() {
    return await db.users.create(this.build());
  }
}

// Usage
test('should allow admin to delete users', async () => {
  const admin = await new UserBuilder()
    .withEmail('admin@example.com')
    .asAdmin()
    .create();
  
  const user = await new UserBuilder()
    .withEmail('user@example.com')
    .create();
  
  await deleteUser(admin.id, user.id);
  
  const deleted = await db.users.findById(user.id);
  expect(deleted).toBeNull();
});
```

**Benefits**
- Fluent API for test data creation
- Reduces duplication
- Default values for common cases
- Easy to maintain and extend

### 3. Object Mother Pattern

**Implementation**
```javascript
class UserMother {
  static regular() {
    return {
      email: 'user@example.com',
      name: 'Regular User',
      role: 'user',
      status: 'active'
    };
  }
  
  static admin() {
    return {
      email: 'admin@example.com',
      name: 'Admin User',
      role: 'admin',
      status: 'active'
    };
  }
  
  static inactive() {
    return {
      email: 'inactive@example.com',
      name: 'Inactive User',
      role: 'user',
      status: 'inactive'
    };
  }
  
  static withOrders(orderCount = 5) {
    return {
      ...this.regular(),
      orders: Array.from({ length: orderCount }, (_, i) => 
        OrderMother.forUser(i)
      )
    };
  }
}

// Usage
test('should display user orders', async () => {
  const user = await db.users.create(UserMother.withOrders(3));
  
  const orders = await orderService.getUserOrders(user.id);
  
  expect(orders).toHaveLength(3);
});
```

**Benefits**
- Pre-defined test data scenarios
- Named factory methods
- Reduces test setup code
- Centralized test data management

### 4. Test Fixture Pattern

**Setup**
```javascript
// fixtures/users.js
module.exports = {
  users: [
    {
      id: 'user-1',
      email: 'john@example.com',
      name: 'John Doe',
      role: 'user'
    },
    {
      id: 'user-2',
      email: 'jane@example.com',
      name: 'Jane Smith',
      role: 'admin'
    }
  ],
  posts: [
    {
      id: 'post-1',
      user_id: 'user-1',
      title: 'First Post',
      status: 'published'
    },
    {
      id: 'post-2',
      user_id: 'user-1',
      title: 'Draft Post',
      status: 'draft'
    }
  ]
};

// Test
const fixtures = require('./fixtures/users');

beforeEach(async () => {
  await db.users.createMany(fixtures.users);
  await db.posts.createMany(fixtures.posts);
});

test('should get user posts', async () => {
  const posts = await postService.getUserPosts('user-1');
  
  expect(posts).toHaveLength(2);
});
```

**Benefits**
- Consistent test data across tests
- Easy to version and update
- Can be shared across test suites
- Supports complex data relationships

### 5. Database Transaction Pattern

**Implementation**
```javascript
describe('User Service', () => {
  let transaction;
  
  beforeEach(async () => {
    transaction = await db.beginTransaction();
  });
  
  afterEach(async () => {
    await transaction.rollback();
  });
  
  test('should create user', async () => {
    const user = await userService.createUser({
      email: 'test@example.com'
    }, { transaction });
    
    expect(user.id).toBeDefined();
    
    // Data visible within transaction
    const found = await userService.findById(user.id, { transaction });
    expect(found).toBeDefined();
    
    // After test, transaction rolls back automatically
  });
});
```

**Benefits**
- Fast test cleanup
- Test isolation
- No database cleanup needed
- Prevents data pollution

### 6. Shared Setup Pattern

**Implementation**
```javascript
// setup/test-context.js
class TestContext {
  constructor() {
    this.users = [];
    this.posts = [];
  }
  
  async createUser(data = {}) {
    const user = await db.users.create({
      email: `user-${Date.now()}@example.com`,
      name: 'Test User',
      ...data
    });
    this.users.push(user);
    return user;
  }
  
  async createPost(userId, data = {}) {
    const post = await db.posts.create({
      user_id: userId,
      title: 'Test Post',
      ...data
    });
    this.posts.push(post);
    return post;
  }
  
  async cleanup() {
    await db.posts.deleteMany(this.posts.map(p => p.id));
    await db.users.deleteMany(this.users.map(u => u.id));
  }
}

// Usage
describe('Post Service', () => {
  let context;
  
  beforeEach(() => {
    context = new TestContext();
  });
  
  afterEach(async () => {
    await context.cleanup();
  });
  
  test('should create post', async () => {
    const user = await context.createUser();
    const post = await context.createPost(user.id);
    
    expect(post.user_id).toBe(user.id);
  });
});
```

### 7. Page Object Pattern (E2E)

**Implementation**
```javascript
// pages/LoginPage.js
class LoginPage {
  constructor(page) {
    this.page = page;
    this.emailInput = '#email';
    this.passwordInput = '#password';
    this.submitButton = 'button[type="submit"]';
    this.errorMessage = '.error-message';
  }
  
  async goto() {
    await this.page.goto('/login');
  }
  
  async login(email, password) {
    await this.page.fill(this.emailInput, email);
    await this.page.fill(this.passwordInput, password);
    await this.page.click(this.submitButton);
  }
  
  async getErrorMessage() {
    return await this.page.textContent(this.errorMessage);
  }
  
  async isLoggedIn() {
    return await this.page.url().includes('/dashboard');
  }
}

// Usage in tests
test('should login successfully', async ({ page }) => {
  const loginPage = new LoginPage(page);
  
  await loginPage.goto();
  await loginPage.login('user@example.com', 'Password123!');
  
  expect(await loginPage.isLoggedIn()).toBe(true);
});

test('should show error for invalid credentials', async ({ page }) => {
  const loginPage = new LoginPage(page);
  
  await loginPage.goto();
  await loginPage.login('user@example.com', 'wrongpassword');
  
  const error = await loginPage.getErrorMessage();
  expect(error).toContain('Invalid credentials');
});
```

**Benefits**
- Encapsulates page interactions
- Reduces code duplication
- Makes tests more maintainable
- Abstracts UI changes

## Testing Anti-Patterns

### 1. Test Interdependence

**❌ Bad - Tests depend on each other**
```javascript
describe('User workflow', () => {
  let userId;
  
  test('1. create user', async () => {
    const user = await createUser();
    userId = user.id; // Shared state
    expect(user).toBeDefined();
  });
  
  test('2. update user', async () => {
    const updated = await updateUser(userId); // Depends on test 1
    expect(updated.name).toBe('Updated');
  });
});
```

**✅ Good - Independent tests**
```javascript
describe('User operations', () => {
  test('should create user', async () => {
    const user = await createUser();
    expect(user).toBeDefined();
  });
  
  test('should update user', async () => {
    const user = await createUser(); // Create own data
    const updated = await updateUser(user.id);
    expect(updated.name).toBe('Updated');
  });
});
```

### 2. Hidden Dependencies

**❌ Bad - Hidden database dependency**
```javascript
test('should get user by email', async () => {
  // Assumes user exists in database
  const user = await getUserByEmail('existing@example.com');
  expect(user).toBeDefined();
});
```

**✅ Good - Explicit setup**
```javascript
test('should get user by email', async () => {
  // Explicit setup
  await createUser({ email: 'test@example.com' });
  
  const user = await getUserByEmail('test@example.com');
  expect(user).toBeDefined();
});
```

### 3. Overly Complex Tests

**❌ Bad - Too much logic in test**
```javascript
test('complex workflow', async () => {
  const users = [];
  for (let i = 0; i < 10; i++) {
    const user = await createUser({ email: `user${i}@example.com` });
    users.push(user);
    
    if (i % 2 === 0) {
      await assignRole(user.id, 'admin');
    }
    
    for (let j = 0; j < 3; j++) {
      await createPost(user.id, { title: `Post ${j}` });
    }
  }
  
  // What are we actually testing?
  const admins = users.filter(u => u.role === 'admin');
  expect(admins.length).toBe(5);
});
```

**✅ Good - Simple, focused test**
```javascript
test('should assign admin role to user', async () => {
  const user = await createUser();
  
  await assignRole(user.id, 'admin');
  
  const updated = await getUser(user.id);
  expect(updated.role).toBe('admin');
});
```

### 4. Testing Implementation Details

**❌ Bad - Testing internal implementation**
```javascript
test('should call database with correct query', async () => {
  const spy = jest.spyOn(db, 'query');
  
  await userService.getUser('123');
  
  expect(spy).toHaveBeenCalledWith(
    'SELECT * FROM users WHERE id = $1',
    ['123']
  );
});
```

**✅ Good - Testing behavior**
```javascript
test('should return user by id', async () => {
  const created = await createUser({ id: '123' });
  
  const user = await userService.getUser('123');
  
  expect(user.id).toBe('123');
  expect(user.email).toBe(created.email);
});
```

### 5. Shared Mutable State

**❌ Bad - Shared state across tests**
```javascript
const sharedUser = { email: 'shared@example.com' };

describe('User tests', () => {
  test('should update email', async () => {
    sharedUser.email = 'updated@example.com';
    await updateUser(sharedUser);
  });
  
  test('should have original email', async () => {
    // This will fail! sharedUser was mutated
    expect(sharedUser.email).toBe('shared@example.com');
  });
});
```

**✅ Good - Fresh state per test**
```javascript
describe('User tests', () => {
  let user;
  
  beforeEach(() => {
    user = { email: 'test@example.com' };
  });
  
  test('should update email', async () => {
    user.email = 'updated@example.com';
    await updateUser(user);
    expect(user.email).toBe('updated@example.com');
  });
  
  test('should have original email', async () => {
    expect(user.email).toBe('test@example.com');
  });
});
```

## Best Practices

### Test Organization

**1. Group Related Tests**
```javascript
describe('User API', () => {
  describe('POST /users', () => {
    test('should create user with valid data', () => {});
    test('should return 422 for invalid email', () => {});
    test('should return 409 for duplicate email', () => {});
  });
  
  describe('GET /users/:id', () => {
    test('should return user by id', () => {});
    test('should return 404 for non-existent user', () => {});
  });
  
  describe('PATCH /users/:id', () => {
    test('should update user fields', () => {});
    test('should not update email to existing email', () => {});
  });
});
```

**2. Use Descriptive Test Names**
```javascript
// ❌ Bad
test('user test', () => {});
test('works', () => {});
test('test 1', () => {});

// ✅ Good
test('should create user with valid email and name', () => {});
test('should return 404 when user does not exist', () => {});
test('should require authentication for protected endpoints', () => {});
```

**3. Test One Thing at a Time**
```javascript
// ❌ Bad - Testing multiple things
test('user creation', async () => {
  const user = await createUser({ email: 'test@example.com' });
  expect(user.id).toBeDefined();
  expect(user.email).toBe('test@example.com');
  expect(user.created_at).toBeInstanceOf(Date);
  
  const fromDb = await getUser(user.id);
  expect(fromDb).toBeDefined();
  
  const all = await getAllUsers();
  expect(all).toContain(user);
});

// ✅ Good - Separate focused tests
test('should create user with generated id', async () => {
  const user = await createUser({ email: 'test@example.com' });
  expect(user.id).toBeDefined();
});

test('should persist user to database', async () => {
  const user = await createUser({ email: 'test@example.com' });
  const fromDb = await getUser(user.id);
  expect(fromDb.email).toBe(user.email);
});
```

### Test Data Management

**1. Use Factories for Dynamic Data**
```javascript
// factory.js
const { faker } = require('@faker-js/faker');

function createUserData(overrides = {}) {
  return {
    email: faker.internet.email(),
    name: faker.person.fullName(),
    age: faker.number.int({ min: 18, max: 80 }),
    ...overrides
  };
}

// test
test('should create user', async () => {
  const userData = createUserData({ age: 25 });
  const user = await createUser(userData);
  expect(user.age).toBe(25);
});
```

**2. Use Fixtures for Stable Data**
```javascript
// fixtures/products.json
{
  "products": [
    { "id": "prod-1", "name": "Widget", "price": 19.99 },
    { "id": "prod-2", "name": "Gadget", "price": 29.99 }
  ]
}

// test
const fixtures = require('./fixtures/products.json');

test('should calculate order total', () => {
  const order = calculateTotal(fixtures.products);
  expect(order.total).toBe(49.98);
});
```

**3. Clean Up Test Data**
```javascript
// Using transaction rollback
describe('User tests', () => {
  let transaction;
  
  beforeEach(async () => {
    transaction = await db.beginTransaction();
  });
  
  afterEach(async () => {
    await transaction.rollback();
  });
});

// Using explicit cleanup
describe('User tests', () => {
  const createdIds = [];
  
  afterEach(async () => {
    await db.users.deleteMany(createdIds);
    createdIds.length = 0;
  });
  
  test('should create user', async () => {
    const user = await createUser();
    createdIds.push(user.id);
  });
});
```

### Assertion Best Practices

**1. Use Specific Assertions**
```javascript
// ❌ Bad - Too vague
expect(response.status).toBeTruthy();
expect(user).toBeDefined();

// ✅ Good - Specific assertions
expect(response.status).toBe(200);
expect(user.email).toBe('test@example.com');
```

**2. Assert on Behavior, Not Implementation**
```javascript
// ❌ Bad - Implementation detail
expect(mockFunction).toHaveBeenCalledTimes(1);

// ✅ Good - Behavior
const result = await doSomething();
expect(result.success).toBe(true);
```

**3. Use Appropriate Matchers**
```javascript
// Equality
expect(value).toBe(1); // Strict equality
expect(obj).toEqual({ id: 1 }); // Deep equality
expect(array).toContain(item); // Array contains

// Objects
expect(obj).toHaveProperty('id');
expect(obj).toMatchObject({ id: 1 });

// Strings
expect(str).toMatch(/pattern/);
expect(str).toContain('substring');

// Numbers
expect(num).toBeGreaterThan(0);
expect(num).toBeCloseTo(1.5, 1);

// Async
await expect(promise).resolves.toBe(value);
await expect(promise).rejects.toThrow(Error);
```

### Test Performance

**1. Run Fast Tests First**
```javascript
// package.json
{
  "scripts": {
    "test": "npm run test:unit && npm run test:integration",
    "test:unit": "jest --testPathPattern=unit",
    "test:integration": "jest --testPathPattern=integration"
  }
}
```

**2. Use Parallel Execution**
```javascript
// jest.config.js
module.exports = {
  maxWorkers: '50%', // Use 50% of CPU cores
  testTimeout: 10000
};
```

**3. Mock Slow Dependencies**
```javascript
// Mock external API
jest.mock('../services/external-api');
externalApi.fetchData.mockResolvedValue({ data: 'mocked' });

// Mock database for unit tests
jest.mock('../database');
db.users.findById.mockResolvedValue({ id: '123' });
```

### Test Maintenance

**1. Keep Tests DRY**
```javascript
// Extract common setup
function setupTestUser() {
  return createUser({ email: 'test@example.com' });
}

// Extract common assertions
function assertValidUser(user) {
  expect(user).toHaveProperty('id');
  expect(user).toHaveProperty('email');
  expect(user.email).toMatch(/@/);
}
```

**2. Refactor Tests with Code**
```javascript
// When you rename a function
function createUser() {} // renamed from addUser()

// Update all tests
test('should create user', async () => {
  const user = await createUser(); // was addUser()
});
```

**3. Remove Obsolete Tests**
```javascript
// When feature is removed, remove its tests
describe('Deprecated feature', () => {
  // Delete this entire test suite
});
```

## Test Pyramid Principles

```
           /\
          /  \
         / E2E\      Few (slow, brittle)
        /------\
       /  Integ \    Some (medium speed)
      /----------\
     /    Unit    \  Many (fast, reliable)
    /--------------\
```

**Guidelines**
1. **Unit Tests**: 60-70% of tests, fast, isolated
2. **Integration Tests**: 20-30% of tests, test component interactions
3. **E2E Tests**: 5-10% of tests, test critical user journeys

**Integration Test Focus**
- API endpoints
- Database operations
- Service communication
- Authentication/Authorization
- Data flow across boundaries
- External integrations (with mocks)

**When to Write Integration Tests**
- ✅ Testing API contracts
- ✅ Testing database transactions
- ✅ Testing service interactions
- ✅ Testing authentication flows
- ✅ Testing data transformations
- ❌ Testing business logic (use unit tests)
- ❌ Testing UI interactions (use E2E tests)
- ❌ Testing every edge case (use unit tests)
