# Testing Frameworks Comprehensive Guide

This guide provides detailed information about testing frameworks across different languages and platforms.

## JavaScript/TypeScript Testing Frameworks

### Jest

**Overview**: Complete testing framework with built-in mocking, coverage, and assertions.

**Installation**
```bash
npm install --save-dev jest @types/jest
```

**Configuration**
```javascript
// jest.config.js
module.exports = {
  testEnvironment: 'node',
  testMatch: ['**/__tests__/**/*.test.js'],
  collectCoverageFrom: ['src/**/*.js'],
  coveragePathIgnorePatterns: ['/node_modules/', '/tests/'],
  setupFilesAfterEnv: ['./jest.setup.js'],
  moduleNameMapper: {
    '^@/(.*)$': '<rootDir>/src/$1'
  },
  maxWorkers: '50%',
  testTimeout: 10000
};
```

**Jest Setup File**
```javascript
// jest.setup.js
beforeAll(() => {
  console.log('Global test setup');
});

afterAll(() => {
  console.log('Global test teardown');
});

// Custom matchers
expect.extend({
  toBeWithinRange(received, floor, ceiling) {
    const pass = received >= floor && received <= ceiling;
    if (pass) {
      return {
        message: () => `expected ${received} not to be within range ${floor} - ${ceiling}`,
        pass: true
      };
    } else {
      return {
        message: () => `expected ${received} to be within range ${floor} - ${ceiling}`,
        pass: false
      };
    }
  }
});
```

**Example Tests**
```javascript
// user.service.test.js
const UserService = require('../services/user.service');
const db = require('../database');

jest.mock('../database');

describe('UserService', () => {
  let userService;

  beforeEach(() => {
    userService = new UserService();
    jest.clearAllMocks();
  });

  describe('createUser', () => {
    it('should create user successfully', async () => {
      const userData = {
        email: 'test@example.com',
        name: 'Test User'
      };

      db.users.create.mockResolvedValue({
        id: '123',
        ...userData,
        created_at: new Date()
      });

      const result = await userService.createUser(userData);

      expect(result).toHaveProperty('id', '123');
      expect(result.email).toBe(userData.email);
      expect(db.users.create).toHaveBeenCalledWith(userData);
      expect(db.users.create).toHaveBeenCalledTimes(1);
    });

    it('should throw error for duplicate email', async () => {
      db.users.create.mockRejectedValue(
        new Error('Unique constraint violation')
      );

      await expect(
        userService.createUser({ email: 'exists@example.com' })
      ).rejects.toThrow('Unique constraint violation');
    });
  });

  describe('getUserById', () => {
    it('should return user when found', async () => {
      const mockUser = { id: '123', email: 'test@example.com' };
      db.users.findById.mockResolvedValue(mockUser);

      const result = await userService.getUserById('123');

      expect(result).toEqual(mockUser);
    });

    it('should return null when user not found', async () => {
      db.users.findById.mockResolvedValue(null);

      const result = await userService.getUserById('999');

      expect(result).toBeNull();
    });
  });
});
```

**Async Testing**
```javascript
// Promises
test('async test with promise', () => {
  return fetchData().then(data => {
    expect(data).toBe('result');
  });
});

// Async/Await
test('async test with async/await', async () => {
  const data = await fetchData();
  expect(data).toBe('result');
});

// Resolves/Rejects
test('async test with resolves', async () => {
  await expect(fetchData()).resolves.toBe('result');
});

test('async test with rejects', async () => {
  await expect(fetchError()).rejects.toThrow('error');
});
```

**Snapshot Testing**
```javascript
test('component snapshot', () => {
  const component = renderComponent({ title: 'Test' });
  expect(component).toMatchSnapshot();
});

// Update snapshots: jest --updateSnapshot
```

### Mocha + Chai

**Installation**
```bash
npm install --save-dev mocha chai chai-http sinon
```

**Configuration**
```javascript
// .mocharc.js
module.exports = {
  require: ['./test/setup.js'],
  spec: 'test/**/*.test.js',
  timeout: 5000,
  recursive: true,
  reporter: 'spec'
};
```

**Example Tests**
```javascript
// user.test.js
const { expect } = require('chai');
const sinon = require('sinon');
const UserService = require('../services/user.service');
const db = require('../database');

describe('UserService', () => {
  let userService;
  let dbStub;

  beforeEach(() => {
    userService = new UserService();
    dbStub = sinon.stub(db.users);
  });

  afterEach(() => {
    sinon.restore();
  });

  describe('#createUser', () => {
    it('should create user successfully', async () => {
      const userData = { email: 'test@example.com', name: 'Test' };
      dbStub.create.resolves({ id: '123', ...userData });

      const result = await userService.createUser(userData);

      expect(result).to.have.property('id', '123');
      expect(result.email).to.equal(userData.email);
      expect(dbStub.create.calledOnce).to.be.true;
    });

    it('should throw error for invalid email', async () => {
      try {
        await userService.createUser({ email: 'invalid' });
        expect.fail('Should have thrown error');
      } catch (error) {
        expect(error.message).to.include('email');
      }
    });
  });
});
```

**HTTP Testing with Chai-HTTP**
```javascript
const chai = require('chai');
const chaiHttp = require('chai-http');
const app = require('../app');

chai.use(chaiHttp);
const { expect } = chai;

describe('User API', () => {
  it('GET /api/users should return users', (done) => {
    chai.request(app)
      .get('/api/users')
      .end((err, res) => {
        expect(res).to.have.status(200);
        expect(res.body).to.be.an('array');
        expect(res.body).to.have.lengthOf.at.least(1);
        done();
      });
  });

  it('POST /api/users should create user', async () => {
    const res = await chai.request(app)
      .post('/api/users')
      .send({ email: 'test@example.com', name: 'Test' });

    expect(res).to.have.status(201);
    expect(res.body).to.have.property('id');
  });
});
```

### Supertest (HTTP Testing)

**Installation**
```bash
npm install --save-dev supertest
```

**Example Tests**
```javascript
const request = require('supertest');
const app = require('../app');

describe('User API Integration', () => {
  describe('POST /api/v1/users', () => {
    it('should create user with valid data', async () => {
      const response = await request(app)
        .post('/api/v1/users')
        .send({
          email: 'test@example.com',
          name: 'Test User',
          password: 'Password123!'
        })
        .expect(201)
        .expect('Content-Type', /json/);

      expect(response.body).toMatchObject({
        email: 'test@example.com',
        name: 'Test User'
      });
      expect(response.body).toHaveProperty('id');
    });

    it('should return validation errors', async () => {
      const response = await request(app)
        .post('/api/v1/users')
        .send({ email: 'invalid-email' })
        .expect(422);

      expect(response.body.errors).toBeDefined();
    });
  });

  describe('Authentication', () => {
    it('should require authentication', async () => {
      await request(app)
        .get('/api/v1/users/me')
        .expect(401);
    });

    it('should accept valid token', async () => {
      const token = 'valid-jwt-token';
      
      const response = await request(app)
        .get('/api/v1/users/me')
        .set('Authorization', `Bearer ${token}`)
        .expect(200);

      expect(response.body).toHaveProperty('email');
    });
  });
});
```

### Playwright (E2E Testing)

**Installation**
```bash
npm install --save-dev @playwright/test
npx playwright install
```

**Configuration**
```javascript
// playwright.config.js
const { defineConfig, devices } = require('@playwright/test');

module.exports = defineConfig({
  testDir: './e2e',
  timeout: 30000,
  expect: {
    timeout: 5000
  },
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: [
    ['html'],
    ['junit', { outputFile: 'test-results.xml' }]
  ],
  use: {
    baseURL: 'http://localhost:3000',
    trace: 'on-first-retry',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure'
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] }
    },
    {
      name: 'firefox',
      use: { ...devices['Desktop Firefox'] }
    },
    {
      name: 'webkit',
      use: { ...devices['Desktop Safari'] }
    },
    {
      name: 'Mobile Chrome',
      use: { ...devices['Pixel 5'] }
    }
  ],
  webServer: {
    command: 'npm run start',
    port: 3000,
    reuseExistingServer: !process.env.CI
  }
});
```

**Example Tests**
```javascript
// e2e/login.spec.js
const { test, expect } = require('@playwright/test');

test.describe('Login Flow', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/login');
  });

  test('successful login', async ({ page }) => {
    await page.fill('#email', 'user@example.com');
    await page.fill('#password', 'Password123!');
    await page.click('button[type="submit"]');

    await expect(page).toHaveURL('/dashboard');
    await expect(page.locator('h1')).toHaveText('Dashboard');
  });

  test('login with invalid credentials', async ({ page }) => {
    await page.fill('#email', 'user@example.com');
    await page.fill('#password', 'wrongpassword');
    await page.click('button[type="submit"]');

    await expect(page.locator('.error')).toHaveText('Invalid credentials');
  });

  test('login form validation', async ({ page }) => {
    await page.click('button[type="submit"]');

    await expect(page.locator('#email-error')).toBeVisible();
    await expect(page.locator('#password-error')).toBeVisible();
  });
});

// Fixtures
test.describe('With authenticated user', () => {
  test.use({
    storageState: 'auth.json' // Pre-authenticated state
  });

  test('can access protected page', async ({ page }) => {
    await page.goto('/profile');
    await expect(page.locator('h1')).toHaveText('My Profile');
  });
});

// API Testing
test('API: create user', async ({ request }) => {
  const response = await request.post('/api/users', {
    data: {
      email: 'test@example.com',
      name: 'Test User'
    }
  });

  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  expect(body.email).toBe('test@example.com');
});
```

## Python Testing Frameworks

### pytest

**Installation**
```bash
pip install pytest pytest-asyncio pytest-cov pytest-mock
```

**Configuration**
```ini
# pytest.ini
[pytest]
testpaths = tests
python_files = test_*.py *_test.py
python_classes = Test*
python_functions = test_*
addopts = 
    --verbose
    --cov=src
    --cov-report=html
    --cov-report=term
markers =
    integration: Integration tests
    slow: Slow running tests
    api: API tests
```

**Example Tests**
```python
# tests/test_user_service.py
import pytest
from unittest.mock import Mock, patch
from src.services.user_service import UserService
from src.models.user import User

@pytest.fixture
def user_service():
    return UserService()

@pytest.fixture
def sample_user():
    return {
        'email': 'test@example.com',
        'name': 'Test User'
    }

class TestUserService:
    def test_create_user_success(self, user_service, sample_user):
        with patch('src.services.user_service.db') as mock_db:
            mock_db.users.create.return_value = User(id='123', **sample_user)
            
            result = user_service.create_user(sample_user)
            
            assert result.id == '123'
            assert result.email == sample_user['email']
            mock_db.users.create.assert_called_once_with(sample_user)
    
    def test_create_user_duplicate_email(self, user_service, sample_user):
        with patch('src.services.user_service.db') as mock_db:
            mock_db.users.create.side_effect = ValueError('Duplicate email')
            
            with pytest.raises(ValueError, match='Duplicate email'):
                user_service.create_user(sample_user)
    
    @pytest.mark.parametrize('email,valid', [
        ('test@example.com', True),
        ('invalid-email', False),
        ('', False),
        ('test@', False)
    ])
    def test_email_validation(self, user_service, email, valid):
        result = user_service.validate_email(email)
        assert result == valid

# Async tests
@pytest.mark.asyncio
async def test_async_create_user(user_service, sample_user):
    result = await user_service.create_user_async(sample_user)
    assert result is not None
```

**API Testing with pytest**
```python
# tests/test_api.py
import pytest
from fastapi.testclient import TestClient
from src.main import app

@pytest.fixture
def client():
    return TestClient(app)

class TestUserAPI:
    def test_create_user(self, client):
        response = client.post('/api/v1/users', json={
            'email': 'test@example.com',
            'name': 'Test User',
            'password': 'Password123!'
        })
        
        assert response.status_code == 201
        data = response.json()
        assert data['email'] == 'test@example.com'
        assert 'id' in data
        assert 'password' not in data
    
    def test_create_user_validation_error(self, client):
        response = client.post('/api/v1/users', json={
            'email': 'invalid-email'
        })
        
        assert response.status_code == 422
        data = response.json()
        assert 'detail' in data
    
    def test_get_user(self, client):
        # Create user first
        create_response = client.post('/api/v1/users', json={
            'email': 'test@example.com',
            'name': 'Test User',
            'password': 'Password123!'
        })
        user_id = create_response.json()['id']
        
        # Get user
        response = client.get(f'/api/v1/users/{user_id}')
        
        assert response.status_code == 200
        data = response.json()
        assert data['id'] == user_id
```

**Database Testing**
```python
# tests/conftest.py
import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from src.database import Base

@pytest.fixture(scope='session')
def db_engine():
    engine = create_engine('sqlite:///:memory:')
    Base.metadata.create_all(engine)
    yield engine
    Base.metadata.drop_all(engine)

@pytest.fixture
def db_session(db_engine):
    Session = sessionmaker(bind=db_engine)
    session = Session()
    yield session
    session.rollback()
    session.close()

# tests/test_user_repository.py
def test_create_user(db_session):
    user = User(email='test@example.com', name='Test User')
    db_session.add(user)
    db_session.commit()
    
    assert user.id is not None
    
    retrieved = db_session.query(User).filter_by(email='test@example.com').first()
    assert retrieved.name == 'Test User'
```

## Java Testing Frameworks

### JUnit 5 + Spring Boot Test

**Dependencies (Maven)**
```xml
<dependencies>
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-test</artifactId>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.testcontainers</groupId>
        <artifactId>junit-jupiter</artifactId>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>io.rest-assured</groupId>
        <artifactId>rest-assured</artifactId>
        <scope>test</scope>
    </dependency>
</dependencies>
```

**Example Tests**
```java
// UserServiceIntegrationTest.java
package com.example.service;

import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import static org.assertj.core.api.Assertions.*;

@SpringBootTest
@ActiveProfiles("test")
@Transactional
class UserServiceIntegrationTest {
    
    @Autowired
    private UserService userService;
    
    @Autowired
    private UserRepository userRepository;
    
    @BeforeEach
    void setUp() {
        userRepository.deleteAll();
    }
    
    @Test
    @DisplayName("Should create user successfully")
    void testCreateUser() {
        // Given
        UserDTO userDTO = new UserDTO("test@example.com", "Test User");
        
        // When
        User created = userService.createUser(userDTO);
        
        // Then
        assertThat(created).isNotNull();
        assertThat(created.getId()).isNotNull();
        assertThat(created.getEmail()).isEqualTo("test@example.com");
        
        // Verify in database
        User fromDb = userRepository.findById(created.getId()).orElse(null);
        assertThat(fromDb).isNotNull();
        assertThat(fromDb.getName()).isEqualTo("Test User");
    }
    
    @Test
    @DisplayName("Should throw exception for duplicate email")
    void testCreateUserDuplicateEmail() {
        // Given
        userService.createUser(new UserDTO("test@example.com", "User 1"));
        
        // When/Then
        assertThatThrownBy(() -> 
            userService.createUser(new UserDTO("test@example.com", "User 2"))
        )
        .isInstanceOf(DuplicateEmailException.class)
        .hasMessageContaining("email");
    }
    
    @Nested
    @DisplayName("User retrieval tests")
    class UserRetrievalTests {
        
        @Test
        void testGetUserById() {
            User user = userService.createUser(new UserDTO("test@example.com", "Test"));
            
            User retrieved = userService.getUserById(user.getId());
            
            assertThat(retrieved).isEqualTo(user);
        }
        
        @Test
        void testGetUserByIdNotFound() {
            assertThatThrownBy(() -> userService.getUserById(999L))
                .isInstanceOf(UserNotFoundException.class);
        }
    }
}
```

**API Testing with REST Assured**
```java
// UserApiIntegrationTest.java
package com.example.api;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class UserApiIntegrationTest {
    
    @LocalServerPort
    private int port;
    
    @BeforeEach
    void setUp() {
        RestAssured.port = port;
        RestAssured.basePath = "/api/v1";
    }
    
    @Test
    void testCreateUser() {
        given()
            .contentType(ContentType.JSON)
            .body("""
                {
                    "email": "test@example.com",
                    "name": "Test User",
                    "password": "Password123!"
                }
                """)
        .when()
            .post("/users")
        .then()
            .statusCode(201)
            .body("email", equalTo("test@example.com"))
            .body("name", equalTo("Test User"))
            .body("id", notNullValue())
            .body("password", nullValue());
    }
    
    @Test
    void testGetUser() {
        // Create user
        String userId = given()
            .contentType(ContentType.JSON)
            .body("""
                {
                    "email": "test@example.com",
                    "name": "Test User"
                }
                """)
            .post("/users")
            .then()
            .extract()
            .path("id");
        
        // Get user
        given()
            .pathParam("id", userId)
        .when()
            .get("/users/{id}")
        .then()
            .statusCode(200)
            .body("id", equalTo(userId))
            .body("email", equalTo("test@example.com"));
    }
    
    @Test
    void testAuthenticationFlow() {
        // Create user
        given()
            .contentType(ContentType.JSON)
            .body("""
                {
                    "email": "test@example.com",
                    "name": "Test User",
                    "password": "Password123!"
                }
                """)
            .post("/users");
        
        // Login
        String token = given()
            .contentType(ContentType.JSON)
            .body("""
                {
                    "email": "test@example.com",
                    "password": "Password123!"
                }
                """)
            .post("/auth/login")
            .then()
            .statusCode(200)
            .extract()
            .path("access_token");
        
        // Access protected endpoint
        given()
            .header("Authorization", "Bearer " + token)
        .when()
            .get("/users/me")
        .then()
            .statusCode(200)
            .body("email", equalTo("test@example.com"));
    }
}
```

**TestContainers for Database Testing**
```java
// AbstractIntegrationTest.java
package com.example;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@SpringBootTest
@Testcontainers
public abstract class AbstractIntegrationTest {
    
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15")
        .withDatabaseName("testdb")
        .withUsername("test")
        .withPassword("test");
    
    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }
}

// Usage
class UserServiceIntegrationTest extends AbstractIntegrationTest {
    // Tests run with real PostgreSQL database in container
}
```

## Comparison Matrix

| Feature | Jest | Pytest | JUnit 5 |
|---------|------|--------|---------|
| Language | JavaScript/TypeScript | Python | Java |
| Built-in Assertions | ✅ | ✅ | ✅ |
| Mocking | ✅ Built-in | ⚠️ Requires pytest-mock | ⚠️ Requires Mockito |
| Async Support | ✅ Native | ✅ pytest-asyncio | ✅ Native |
| Parallel Execution | ✅ | ✅ pytest-xdist | ✅ |
| Coverage | ✅ Built-in | ✅ pytest-cov | ⚠️ JaCoCo plugin |
| Snapshot Testing | ✅ | ⚠️ Plugin | ❌ |
| Parametrized Tests | ✅ | ✅ Native | ✅ Native |
| Test Discovery | ✅ Automatic | ✅ Automatic | ✅ Automatic |
| Setup/Teardown | ✅ | ✅ Fixtures | ✅ Annotations |
| IDE Integration | ✅ Excellent | ✅ Excellent | ✅ Excellent |

## Best Practices

### Framework Selection
1. **Choose based on language ecosystem**
2. **Consider team familiarity**
3. **Evaluate plugin ecosystem**
4. **Check CI/CD integration**
5. **Review documentation quality**

### Test Organization
1. **Group related tests**
2. **Use descriptive names**
3. **Follow AAA pattern**
4. **Keep tests independent**
5. **Use fixtures/factories**

### Performance
1. **Run fast tests first**
2. **Use parallel execution**
3. **Mock external dependencies**
4. **Use test database snapshots**
5. **Optimize setup/teardown**

### Maintenance
1. **Keep tests simple**
2. **Avoid duplication**
3. **Refactor with code**
4. **Monitor flaky tests**
5. **Remove obsolete tests**
