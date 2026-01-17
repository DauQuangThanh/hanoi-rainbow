# Integration Testing Process


Follow this systematic approach when designing integration tests:

### Phase 1: Test Strategy & Planning

1. **Define Testing Scope**

**Integration Levels**
- **Component Integration**: Individual modules/classes working together
- **Service Integration**: APIs and services interacting
- **System Integration**: Multiple services/systems integrated
- **End-to-End**: Complete user workflows across entire stack

**Test Objectives**
- Verify interface contracts between components
- Validate data flow across system boundaries
- Ensure proper error handling in integrations
- Test transaction integrity across services
- Validate performance under realistic load

2. **Identify Integration Points**

```
System Architecture:
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Frontend  │────▶│   API GW    │────▶│   Auth Svc  │
└─────────────┘     └─────────────┘     └─────────────┘
                           │
                    ┌──────┴──────┐
                    ▼              ▼
            ┌─────────────┐  ┌─────────────┐
            │  User Svc   │  │ Product Svc │
            └─────────────┘  └─────────────┘
                    │              │
                    ▼              ▼
            ┌─────────────┐  ┌─────────────┐
            │  Database   │  │  Database   │
            └─────────────┘  └─────────────┘
                    │
                    ▼
            ┌─────────────┐
            │ Message Queue│
            └─────────────┘

Integration Points to Test:
1. Frontend ↔ API Gateway
2. API Gateway ↔ Auth Service
3. API Gateway ↔ User Service
4. API Gateway ↔ Product Service
5. Services ↔ Databases
6. Services ↔ Message Queue
7. Cross-service workflows
```

3. **Test Environment Planning**

**Environment Types**
- **Development**: Local testing, mocked external dependencies
- **Integration**: Shared environment with real dependencies
- **Staging**: Production-like environment for final validation
- **Production**: Post-deployment smoke tests

**Infrastructure Requirements**
- Test databases (separate from production)
- Message queues and caches
- External service mocks/stubs
- Test data management tools
- CI/CD runners

### Phase 2: API Integration Testing

1. **RESTful API Testing**

**Test Framework Setup (Jest + Supertest)**
```javascript
// api.test.js
const request = require('supertest');
const app = require('../app');
const db = require('../database');

describe('User API Integration Tests', () => {
  beforeAll(async () => {
    await db.connect();
    await db.migrate();
  });

  afterAll(async () => {
    await db.close();
  });

  beforeEach(async () => {
    await db.seed(); // Load test data
  });

  afterEach(async () => {
    await db.truncate(); // Clean up
  });

  describe('POST /api/v1/users', () => {
    it('should create a new user with valid data', async () => {
      const userData = {
        email: 'test@example.com',
        name: 'Test User',
        password: 'Password123!'
      };

      const response = await request(app)
        .post('/api/v1/users')
        .send(userData)
        .expect(201)
        .expect('Content-Type', /json/);

      expect(response.body).toMatchObject({
        email: userData.email,
        name: userData.name
      });
      expect(response.body).toHaveProperty('id');
      expect(response.body).not.toHaveProperty('password');

      // Verify database
      const user = await db.users.findByEmail(userData.email);
      expect(user).toBeDefined();
      expect(user.name).toBe(userData.name);
    });

    it('should return 422 for invalid email', async () => {
      const response = await request(app)
        .post('/api/v1/users')
        .send({
          email: 'invalid-email',
          name: 'Test User',
          password: 'Password123!'
        })
        .expect(422);

      expect(response.body.error.code).toBe('VALIDATION_ERROR');
      expect(response.body.error.details).toContainEqual(
        expect.objectContaining({
          field: 'email',
          message: expect.stringContaining('email')
        })
      );
    });

    it('should return 409 for duplicate email', async () => {
      const userData = {
        email: 'existing@example.com',
        name: 'Test User',
        password: 'Password123!'
      };

      // First creation succeeds
      await request(app)
        .post('/api/v1/users')
        .send(userData)
        .expect(201);

      // Second creation fails
      const response = await request(app)
        .post('/api/v1/users')
        .send(userData)
        .expect(409);

      expect(response.body.error.code).toBe('DUPLICATE_EMAIL');
    });
  });

  describe('GET /api/v1/users/:id', () => {
    it('should return user by id', async () => {
      const user = await db.users.create({
        email: 'test@example.com',
        name: 'Test User'
      });

      const response = await request(app)
        .get(`/api/v1/users/${user.id}`)
        .expect(200);

      expect(response.body).toMatchObject({
        id: user.id,
        email: user.email,
        name: user.name
      });
    });

    it('should return 404 for non-existent user', async () => {
      await request(app)
        .get('/api/v1/users/non-existent-id')
        .expect(404);
    });
  });

  describe('Authentication Flow Integration', () => {
    it('should complete login-to-protected-resource flow', async () => {
      // 1. Create user
      const userData = {
        email: 'test@example.com',
        password: 'Password123!'
      };
      
      await request(app)
        .post('/api/v1/users')
        .send({ ...userData, name: 'Test User' })
        .expect(201);

      // 2. Login
      const loginResponse = await request(app)
        .post('/api/v1/auth/login')
        .send(userData)
        .expect(200);

      const { access_token } = loginResponse.body;
      expect(access_token).toBeDefined();

      // 3. Access protected resource
      const profileResponse = await request(app)
        .get('/api/v1/users/me')
        .set('Authorization', `Bearer ${access_token}`)
        .expect(200);

      expect(profileResponse.body.email).toBe(userData.email);

      // 4. Verify unauthorized without token
      await request(app)
        .get('/api/v1/users/me')
        .expect(401);
    });
  });
});
```

2. **Schema Validation Testing**

```javascript
// schema-validation.test.js
const Ajv = require('ajv');
const ajv = new Ajv();

describe('API Response Schema Validation', () => {
  const userSchema = {
    type: 'object',
    required: ['id', 'email', 'name', 'created_at'],
    properties: {
      id: { type: 'string', format: 'uuid' },
      email: { type: 'string', format: 'email' },
      name: { type: 'string' },
      created_at: { type: 'string', format: 'date-time' }
    },
    additionalProperties: false
  };

  it('should match user schema', async () => {
    const response = await request(app)
      .get('/api/v1/users/123')
      .expect(200);

    const validate = ajv.compile(userSchema);
    const valid = validate(response.body);
    
    if (!valid) {
      console.log(validate.errors);
    }
    
    expect(valid).toBe(true);
  });
});
```

3. **Contract Testing (Pact)**

```javascript
// user-api.contract.test.js
const { Pact } = require('@pact-foundation/pact');
const { like, iso8601 } = require('@pact-foundation/pact').Matchers;

const provider = new Pact({
  consumer: 'Frontend',
  provider: 'UserAPI'
});

describe('User API Contract', () => {
  beforeAll(() => provider.setup());
  afterAll(() => provider.finalize());
  afterEach(() => provider.verify());

  it('should get user by id', async () => {
    await provider.addInteraction({
      state: 'user exists',
      uponReceiving: 'a request for user',
      withRequest: {
        method: 'GET',
        path: '/api/v1/users/123'
      },
      willRespondWith: {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
        body: {
          id: like('123'),
          email: like('user@example.com'),
          name: like('John Doe'),
          created_at: iso8601()
        }
      }
    });

    // Test your consumer code
    const user = await userApiClient.getUser('123');
    expect(user.email).toBeDefined();
  });
});
```

### Phase 3: Database Integration Testing

1. **Database Setup & Teardown**

```javascript
// db-setup.js
const { Client } = require('pg');
const { migrate } = require('./migrations');
const { seed } = require('./seeds');

class TestDatabase {
  constructor() {
    this.client = new Client({
      host: process.env.TEST_DB_HOST || 'localhost',
      database: process.env.TEST_DB_NAME || 'test_db',
      user: process.env.TEST_DB_USER || 'test_user',
      password: process.env.TEST_DB_PASSWORD
    });
  }

  async connect() {
    await this.client.connect();
  }

  async disconnect() {
    await this.client.end();
  }

  async reset() {
    // Truncate all tables
    await this.client.query(`
      TRUNCATE TABLE users, posts, comments CASCADE;
    `);
  }

  async migrate() {
    await migrate(this.client);
  }

  async seed(fixtures = 'default') {
    await seed(this.client, fixtures);
  }
}

module.exports = TestDatabase;
```

2. **Transaction Testing**

```javascript
// transaction.test.js
describe('Database Transaction Tests', () => {
  it('should rollback on error', async () => {
    const initialBalance = 1000;
    
    // Create accounts
    const account1 = await db.accounts.create({ balance: initialBalance });
    const account2 = await db.accounts.create({ balance: 0 });

    // Attempt transfer that will fail
    await expect(
      transferMoney(account1.id, account2.id, 1500)
    ).rejects.toThrow('Insufficient funds');

    // Verify rollback - balances unchanged
    const acc1After = await db.accounts.findById(account1.id);
    const acc2After = await db.accounts.findById(account2.id);
    
    expect(acc1After.balance).toBe(initialBalance);
    expect(acc2After.balance).toBe(0);
  });

  it('should commit successful transaction', async () => {
    const account1 = await db.accounts.create({ balance: 1000 });
    const account2 = await db.accounts.create({ balance: 0 });

    await transferMoney(account1.id, account2.id, 500);

    const acc1After = await db.accounts.findById(account1.id);
    const acc2After = await db.accounts.findById(account2.id);
    
    expect(acc1After.balance).toBe(500);
    expect(acc2After.balance).toBe(500);
  });
});
```

3. **Data Integrity Testing**

```javascript
// data-integrity.test.js
describe('Data Integrity Tests', () => {
  it('should enforce foreign key constraints', async () => {
    await expect(
      db.posts.create({
        title: 'Test Post',
        user_id: 'non-existent-id'
      })
    ).rejects.toThrow(/foreign key constraint/i);
  });

  it('should enforce unique constraints', async () => {
    await db.users.create({ email: 'test@example.com' });
    
    await expect(
      db.users.create({ email: 'test@example.com' })
    ).rejects.toThrow(/unique constraint/i);
  });

  it('should cascade delete', async () => {
    const user = await db.users.create({ email: 'test@example.com' });
    const post = await db.posts.create({ 
      user_id: user.id, 
      title: 'Test' 
    });

    await db.users.delete(user.id);

    const deletedPost = await db.posts.findById(post.id);
    expect(deletedPost).toBeNull();
  });
});
```

### Phase 4: Microservices Integration Testing

1. **Service-to-Service Communication**

```javascript
// microservices.test.js
describe('Microservices Integration', () => {
  beforeAll(async () => {
    // Start test instances of services
    await startService('user-service', 3001);
    await startService('order-service', 3002);
    await startService('payment-service', 3003);
  });

  afterAll(async () => {
    await stopAllServices();
  });

  it('should complete order workflow across services', async () => {
    // 1. Create user (User Service)
    const userResponse = await request('http://localhost:3001')
      .post('/users')
      .send({ email: 'test@example.com', name: 'Test User' })
      .expect(201);

    const userId = userResponse.body.id;

    // 2. Create order (Order Service)
    const orderResponse = await request('http://localhost:3002')
      .post('/orders')
      .send({
        user_id: userId,
        items: [{ product_id: 'prod_123', quantity: 2 }],
        total: 99.99
      })
      .expect(201);

    const orderId = orderResponse.body.id;

    // 3. Process payment (Payment Service)
    const paymentResponse = await request('http://localhost:3003')
      .post('/payments')
      .send({
        order_id: orderId,
        amount: 99.99,
        method: 'card'
      })
      .expect(200);

    expect(paymentResponse.body.status).toBe('completed');

    // 4. Verify order status updated
    const orderCheck = await request('http://localhost:3002')
      .get(`/orders/${orderId}`)
      .expect(200);

    expect(orderCheck.body.status).toBe('paid');
  });
});
```

2. **Message Queue Testing**

```javascript
// message-queue.test.js
const { connect } = require('amqplib');

describe('Message Queue Integration', () => {
  let connection, channel;

  beforeAll(async () => {
    connection = await connect('amqp://localhost');
    channel = await connection.createChannel();
    await channel.assertQueue('test-queue');
  });

  afterAll(async () => {
    await channel.close();
    await connection.close();
  });

  it('should publish and consume messages', async (done) => {
    const message = { type: 'USER_CREATED', user_id: '123' };

    // Consumer
    await channel.consume('test-queue', (msg) => {
      const received = JSON.parse(msg.content.toString());
      expect(received).toEqual(message);
      channel.ack(msg);
      done();
    });

    // Producer
    await channel.sendToQueue(
      'test-queue',
      Buffer.from(JSON.stringify(message))
    );
  });

  it('should handle message processing failure with retry', async () => {
    let attempts = 0;
    
    await channel.consume('test-queue', async (msg) => {
      attempts++;
      
      if (attempts < 3) {
        // Reject and requeue
        channel.nack(msg, false, true);
      } else {
        // Success on third attempt
        channel.ack(msg);
        expect(attempts).toBe(3);
      }
    });

    await channel.sendToQueue('test-queue', Buffer.from('test'));
  });
});
```

3. **Event-Driven Testing**

```javascript
// event-driven.test.js
describe('Event-Driven Integration', () => {
  it('should trigger downstream events', async () => {
    const events = [];
    
    // Subscribe to events
    eventBus.on('user.created', (data) => {
      events.push({ type: 'user.created', data });
    });
    
    eventBus.on('email.sent', (data) => {
      events.push({ type: 'email.sent', data });
    });

    // Trigger action
    await createUser({ email: 'test@example.com', name: 'Test' });

    // Wait for async events
    await waitFor(() => events.length === 2, { timeout: 5000 });

    expect(events).toContainEqual(
      expect.objectContaining({ type: 'user.created' })
    );
    expect(events).toContainEqual(
      expect.objectContaining({ type: 'email.sent' })
    );
  });
});
```

### Phase 5: End-to-End Testing

1. **UI-Driven Integration Tests (Playwright)**

```javascript
// e2e/user-registration.spec.js
const { test, expect } = require('@playwright/test');

test.describe('User Registration Flow', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('http://localhost:3000');
  });

  test('should complete full registration workflow', async ({ page }) => {
    // Navigate to registration
    await page.click('text=Sign Up');
    await expect(page).toHaveURL(/.*\/register/);

    // Fill registration form
    await page.fill('#email', 'test@example.com');
    await page.fill('#name', 'Test User');
    await page.fill('#password', 'Password123!');
    await page.fill('#confirmPassword', 'Password123!');

    // Submit form
    await page.click('button[type="submit"]');

    // Wait for redirect
    await expect(page).toHaveURL(/.*\/dashboard/);

    // Verify welcome message
    await expect(page.locator('text=Welcome, Test User')).toBeVisible();

    // Verify email sent (check database)
    const emailLog = await db.emails.findOne({ 
      to: 'test@example.com',
      type: 'welcome' 
    });
    expect(emailLog).toBeDefined();
  });

  test('should show validation errors', async ({ page }) => {
    await page.click('text=Sign Up');
    
    // Submit without filling
    await page.click('button[type="submit"]');

    // Check errors
    await expect(page.locator('text=Email is required')).toBeVisible();
    await expect(page.locator('text=Name is required')).toBeVisible();
  });
});
```

2. **Multi-Step Workflow Testing**

```javascript
// e2e/checkout-flow.spec.js
test('should complete checkout workflow', async ({ page }) => {
  // 1. Login
  await page.goto('/login');
  await page.fill('#email', 'test@example.com');
  await page.fill('#password', 'Password123!');
  await page.click('button:text("Login")');

  // 2. Browse products
  await page.goto('/products');
  await page.click('[data-testid="product-1"]');

  // 3. Add to cart
  await page.click('button:text("Add to Cart")');
  await expect(page.locator('[data-testid="cart-count"]')).toHaveText('1');

  // 4. Go to cart
  await page.click('[data-testid="cart-icon"]');
  await expect(page).toHaveURL(/.*\/cart/);

  // 5. Proceed to checkout
  await page.click('button:text("Checkout")');

  // 6. Fill shipping info
  await page.fill('#address', '123 Main St');
  await page.fill('#city', 'New York');
  await page.fill('#zip', '10001');
  await page.click('button:text("Continue")');

  // 7. Enter payment
  await page.fill('#cardNumber', '4242424242424242');
  await page.fill('#expiry', '12/25');
  await page.fill('#cvc', '123');

  // 8. Submit order
  await page.click('button:text("Place Order")');

  // 9. Verify success
  await expect(page.locator('text=Order Confirmed')).toBeVisible();
  
  const orderId = await page.locator('[data-testid="order-id"]').textContent();
  expect(orderId).toMatch(/^ord_/);

  // 10. Verify database
  const order = await db.orders.findById(orderId);
  expect(order.status).toBe('confirmed');
  expect(order.total).toBeGreaterThan(0);
});
```

### Phase 6: Test Data Management

1. **Fixtures**

```javascript
// fixtures/users.js
module.exports = {
  admin: {
    email: 'admin@example.com',
    name: 'Admin User',
    role: 'admin',
    password: 'Admin123!'
  },
  regularUser: {
    email: 'user@example.com',
    name: 'Regular User',
    role: 'user',
    password: 'User123!'
  },
  inactiveUser: {
    email: 'inactive@example.com',
    name: 'Inactive User',
    role: 'user',
    status: 'inactive'
  }
};

// Usage in tests
const fixtures = require('./fixtures/users');

beforeEach(async () => {
  await db.users.create(fixtures.admin);
  await db.users.create(fixtures.regularUser);
});
```

2. **Factory Pattern**

```javascript
// factories/user.factory.js
const { faker } = require('@faker-js/faker');

class UserFactory {
  static create(overrides = {}) {
    return {
      email: faker.internet.email(),
      name: faker.person.fullName(),
      password: 'Password123!',
      role: 'user',
      status: 'active',
      ...overrides
    };
  }

  static createMany(count, overrides = {}) {
    return Array.from({ length: count }, () => this.create(overrides));
  }

  static admin(overrides = {}) {
    return this.create({ role: 'admin', ...overrides });
  }
}

// Usage
const user = UserFactory.create();
const admin = UserFactory.admin({ name: 'Admin User' });
const users = UserFactory.createMany(10);
```

3. **Database Seeding**

```javascript
// seeds/test-data.js
async function seed(db) {
  // Create users
  const users = await db.users.createMany([
    { email: 'admin@example.com', role: 'admin' },
    { email: 'user1@example.com', role: 'user' },
    { email: 'user2@example.com', role: 'user' }
  ]);

  // Create posts
  const posts = await db.posts.createMany([
    { user_id: users[1].id, title: 'First Post', status: 'published' },
    { user_id: users[1].id, title: 'Draft Post', status: 'draft' },
    { user_id: users[2].id, title: 'Another Post', status: 'published' }
  ]);

  // Create comments
  await db.comments.createMany([
    { post_id: posts[0].id, user_id: users[2].id, content: 'Great post!' },
    { post_id: posts[0].id, user_id: users[0].id, content: 'Thanks!' }
  ]);

  return { users, posts };
}

module.exports = { seed };
```

### Phase 7: Mocking & Stubbing

1. **External API Mocking**

```javascript
// Using nock for HTTP mocking
const nock = require('nock');

describe('External API Integration', () => {
  afterEach(() => {
    nock.cleanAll();
  });

  it('should fetch user from external API', async () => {
    nock('https://api.external.com')
      .get('/users/123')
      .reply(200, {
        id: '123',
        name: 'External User',
        email: 'external@example.com'
      });

    const user = await externalApiClient.getUser('123');
    expect(user.name).toBe('External User');
  });

  it('should handle external API errors', async () => {
    nock('https://api.external.com')
      .get('/users/123')
      .reply(500, { error: 'Internal Server Error' });

    await expect(
      externalApiClient.getUser('123')
    ).rejects.toThrow('External API error');
  });

  it('should handle network timeout', async () => {
    nock('https://api.external.com')
      .get('/users/123')
      .delayConnection(5000)
      .reply(200);

    await expect(
      externalApiClient.getUser('123', { timeout: 1000 })
    ).rejects.toThrow('timeout');
  });
});
```

2. **Service Mocking**

```javascript
// Using jest.mock
jest.mock('../services/email.service');
const emailService = require('../services/email.service');

describe('User Registration with Email Mock', () => {
  it('should send welcome email on registration', async () => {
    emailService.sendWelcomeEmail.mockResolvedValue({ sent: true });

    const user = await registerUser({
      email: 'test@example.com',
      name: 'Test User'
    });

    expect(emailService.sendWelcomeEmail).toHaveBeenCalledWith(
      'test@example.com',
      'Test User'
    );
  });
});
```

### Phase 8: CI/CD Integration

1. **GitHub Actions Workflow**

```yaml
# .github/workflows/integration-tests.yml
name: Integration Tests

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

jobs:
  integration-tests:
    runs-on: ubuntu-latest

    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_DB: test_db
          POSTGRES_USER: test_user
          POSTGRES_PASSWORD: test_password
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432

      redis:
        image: redis:7
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 6379:6379

    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'

      - name: Install dependencies
        run: npm ci

      - name: Run migrations
        env:
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/test_db
        run: npm run migrate

      - name: Run integration tests
        env:
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/test_db
          REDIS_URL: redis://localhost:6379
        run: npm run test:integration

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/coverage-final.json
          flags: integration

      - name: Generate test report
        if: always()
        uses: dorny/test-reporter@v1
        with:
          name: Integration Test Results
          path: test-results.json
          reporter: jest-junit
```

2. **Parallel Test Execution**

```javascript
// jest.config.js
module.exports = {
  testMatch: ['**/__tests__/**/*.test.js'],
  maxWorkers: 4, // Run 4 tests in parallel
  testTimeout: 30000, // 30 seconds
  setupFilesAfterEnv: ['./test-setup.js'],
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.test.js'
  ],
  coverageThreshold: {
    global: {
      branches: 70,
      functions: 70,
      lines: 70,
      statements: 70
    }
  }
};
```
