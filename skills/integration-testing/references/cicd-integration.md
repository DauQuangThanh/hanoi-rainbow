# CI/CD Integration for Automated Testing

This guide covers integrating integration tests into CI/CD pipelines across different platforms.

## GitHub Actions

### Complete Integration Test Workflow

```yaml
# .github/workflows/integration-tests.yml
name: Integration Tests

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]
  schedule:
    - cron: '0 2 * * *' # Daily at 2 AM

env:
  NODE_VERSION: '18'
  DATABASE_NAME: test_db

jobs:
  integration-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    strategy:
      matrix:
        node-version: [16, 18, 20]
      fail-fast: false # Continue other jobs if one fails

    services:
      postgres:
        image: postgres:15-alpine
        env:
          POSTGRES_DB: ${{ env.DATABASE_NAME }}
          POSTGRES_USER: test_user
          POSTGRES_PASSWORD: test_password
        ports:
          - 5432:5432
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

      redis:
        image: redis:7-alpine
        ports:
          - 6379:6379
        options: >-
          --health-cmd "redis-cli ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

      rabbitmq:
        image: rabbitmq:3-management-alpine
        env:
          RABBITMQ_DEFAULT_USER: test
          RABBITMQ_DEFAULT_PASS: test
        ports:
          - 5672:5672
          - 15672:15672
        options: >-
          --health-cmd "rabbitmq-diagnostics -q ping"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0 # Full history for better coverage reports

      - name: Setup Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node-version }}
          cache: 'npm'

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.npm
            node_modules
          key: ${{ runner.os }}-node-${{ matrix.node-version }}-${{ hashFiles('**/package-lock.json') }}
          restore-keys: |
            ${{ runner.os }}-node-${{ matrix.node-version }}-
            ${{ runner.os }}-node-

      - name: Install dependencies
        run: npm ci

      - name: Wait for services
        run: |
          echo "Waiting for PostgreSQL..."
          timeout 30 bash -c 'until pg_isready -h localhost -p 5432; do sleep 1; done'
          echo "Waiting for Redis..."
          timeout 30 bash -c 'until redis-cli -h localhost ping; do sleep 1; done'
          echo "Waiting for RabbitMQ..."
          timeout 30 bash -c 'until curl -s -f http://localhost:15672/api/healthchecks/node >/dev/null; do sleep 1; done'

      - name: Setup test database
        env:
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/${{ env.DATABASE_NAME }}
        run: |
          npm run db:migrate
          npm run db:seed:test

      - name: Run linting
        run: npm run lint

      - name: Run unit tests
        run: npm run test:unit -- --coverage

      - name: Run integration tests
        env:
          NODE_ENV: test
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/${{ env.DATABASE_NAME }}
          REDIS_URL: redis://localhost:6379
          RABBITMQ_URL: amqp://test:test@localhost:5672
          JWT_SECRET: test-secret-key
          API_BASE_URL: http://localhost:3000
        run: npm run test:integration -- --coverage

      - name: Run E2E tests
        env:
          NODE_ENV: test
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/${{ env.DATABASE_NAME }}
        run: |
          npm run start:test &
          npx wait-on http://localhost:3000/health --timeout 60000
          npm run test:e2e

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/coverage-final.json
          flags: integration-tests
          name: integration-coverage-${{ matrix.node-version }}
          fail_ci_if_error: false

      - name: Generate test report
        if: always()
        uses: dorny/test-reporter@v1
        with:
          name: Integration Test Results (Node ${{ matrix.node-version }})
          path: test-results.json
          reporter: jest-junit
          fail-on-error: true

      - name: Upload test artifacts
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: test-artifacts-${{ matrix.node-version }}
          path: |
            screenshots/
            test-results/
            logs/
          retention-days: 7

      - name: Comment PR with test results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('test-results.json'));
            const body = `## Test Results (Node ${{ matrix.node-version }})
            
            - ✅ Tests Passed: ${results.numPassedTests}
            - ❌ Tests Failed: ${results.numFailedTests}
            - ⏭️  Tests Skipped: ${results.numPendingTests}
            - ⏱️  Duration: ${results.testResults[0].endTime - results.testResults[0].startTime}ms
            
            [View Full Report](https://github.com/${{ github.repository }}/actions/runs/${{ github.run_id }})`;
            
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: body
            });

  contract-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 18

      - name: Run consumer contract tests
        run: |
          npm ci
          npm run test:contract:consumer

      - name: Publish contracts
        if: github.ref == 'refs/heads/main'
        env:
          PACT_BROKER_BASE_URL: ${{ secrets.PACT_BROKER_URL }}
          PACT_BROKER_TOKEN: ${{ secrets.PACT_BROKER_TOKEN }}
        run: npm run pact:publish

      - name: Verify provider contracts
        env:
          PACT_BROKER_BASE_URL: ${{ secrets.PACT_BROKER_URL }}
          PACT_BROKER_TOKEN: ${{ secrets.PACT_BROKER_TOKEN }}
        run: npm run test:contract:provider

  performance-tests:
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4

      - name: Run performance tests
        run: |
          npm ci
          npm run test:performance

      - name: Compare with baseline
        run: |
          node scripts/compare-performance.js

      - name: Upload performance results
        uses: actions/upload-artifact@v3
        with:
          name: performance-results
          path: performance-results.json

  security-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Run npm audit
        run: npm audit --audit-level=moderate

      - name: Run Snyk security scan
        uses: snyk/actions/node@master
        env:
          SNYK_TOKEN: ${{ secrets.SNYK_TOKEN }}
```

### Parallel Test Execution

```yaml
# .github/workflows/parallel-tests.yml
name: Parallel Integration Tests

on: [push, pull_request]

jobs:
  test-matrix:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        test-group: [api, database, services, auth, integration]
        
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
      
      - name: Run ${{ matrix.test-group }} tests
        run: npm run test:${{ matrix.test-group }}
```

## GitLab CI/CD

### Complete Pipeline Configuration

```yaml
# .gitlab-ci.yml
variables:
  POSTGRES_DB: test_db
  POSTGRES_USER: test_user
  POSTGRES_PASSWORD: test_password
  NODE_ENV: test

stages:
  - build
  - test
  - contract-test
  - deploy

cache:
  key: ${CI_COMMIT_REF_SLUG}
  paths:
    - node_modules/
    - .npm/

before_script:
  - npm ci --cache .npm --prefer-offline

build:
  stage: build
  image: node:18-alpine
  script:
    - npm run build
  artifacts:
    paths:
      - dist/
    expire_in: 1 hour

unit-tests:
  stage: test
  image: node:18-alpine
  script:
    - npm run test:unit -- --coverage
  coverage: '/All files[^|]*\|[^|]*\s+([\d\.]+)/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml
      junit: junit.xml

integration-tests:
  stage: test
  image: node:18-alpine
  services:
    - name: postgres:15-alpine
      alias: postgres
    - name: redis:7-alpine
      alias: redis
    - name: rabbitmq:3-management-alpine
      alias: rabbitmq

  variables:
    DATABASE_URL: postgresql://test_user:test_password@postgres:5432/test_db
    REDIS_URL: redis://redis:6379
    RABBITMQ_URL: amqp://test:test@rabbitmq:5672

  script:
    - echo "Waiting for services..."
    - apk add --no-cache postgresql-client
    - until pg_isready -h postgres -p 5432; do sleep 1; done
    - echo "Running migrations..."
    - npm run db:migrate
    - npm run db:seed:test
    - echo "Running integration tests..."
    - npm run test:integration -- --coverage --ci
  
  artifacts:
    when: always
    reports:
      junit: test-results.xml
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml
    paths:
      - coverage/
      - test-results/
    expire_in: 7 days

  retry:
    max: 2
    when:
      - runner_system_failure
      - stuck_or_timeout_failure

e2e-tests:
  stage: test
  image: mcr.microsoft.com/playwright:v1.40.0-focal
  services:
    - name: postgres:15-alpine
      alias: postgres

  variables:
    DATABASE_URL: postgresql://test_user:test_password@postgres:5432/test_db

  script:
    - npm ci
    - npm run db:migrate
    - npm run start:test &
    - npx wait-on http://localhost:3000/health --timeout 60000
    - npm run test:e2e

  artifacts:
    when: on_failure
    paths:
      - playwright-report/
      - test-results/
    expire_in: 7 days

contract-tests:
  stage: contract-test
  image: node:18-alpine
  script:
    - npm run test:contract
  only:
    - main
    - develop

.test-template: &test-template
  stage: test
  image: node:18-alpine
  services:
    - postgres:15-alpine
    - redis:7-alpine

api-tests:
  <<: *test-template
  script:
    - npm run test:api

database-tests:
  <<: *test-template
  script:
    - npm run test:database

services-tests:
  <<: *test-template
  script:
    - npm run test:services

# Parallel execution
.parallel-tests:
  parallel: 4
  script:
    - npm run test:integration -- --shard=${CI_NODE_INDEX}/${CI_NODE_TOTAL}
```

## Jenkins Pipeline

### Declarative Pipeline

```groovy
// Jenkinsfile
pipeline {
    agent any
    
    options {
        timeout(time: 30, unit: 'MINUTES')
        timestamps()
        buildDiscarder(logRotator(numToKeepStr: '10'))
    }
    
    environment {
        NODE_ENV = 'test'
        DATABASE_URL = credentials('test-database-url')
        REDIS_URL = 'redis://localhost:6379'
        NPM_CONFIG_CACHE = "${WORKSPACE}/.npm"
    }
    
    stages {
        stage('Setup') {
            steps {
                script {
                    sh 'node --version'
                    sh 'npm --version'
                }
            }
        }
        
        stage('Install Dependencies') {
            steps {
                sh 'npm ci'
            }
        }
        
        stage('Start Services') {
            steps {
                script {
                    sh '''
                        docker-compose -f docker-compose.test.yml up -d
                        sleep 10
                        docker-compose -f docker-compose.test.yml ps
                    '''
                }
            }
        }
        
        stage('Database Setup') {
            steps {
                sh 'npm run db:migrate'
                sh 'npm run db:seed:test'
            }
        }
        
        stage('Lint') {
            steps {
                sh 'npm run lint'
            }
        }
        
        stage('Unit Tests') {
            steps {
                sh 'npm run test:unit -- --coverage'
            }
        }
        
        stage('Integration Tests') {
            parallel {
                stage('API Tests') {
                    steps {
                        sh 'npm run test:api'
                    }
                }
                stage('Database Tests') {
                    steps {
                        sh 'npm run test:database'
                    }
                }
                stage('Service Tests') {
                    steps {
                        sh 'npm run test:services'
                    }
                }
            }
        }
        
        stage('E2E Tests') {
            steps {
                sh '''
                    npm run start:test &
                    npx wait-on http://localhost:3000/health
                    npm run test:e2e
                '''
            }
        }
        
        stage('Contract Tests') {
            when {
                branch 'main'
            }
            steps {
                sh 'npm run test:contract'
            }
        }
    }
    
    post {
        always {
            // Publish test results
            junit testResults: 'test-results/**/*.xml', allowEmptyResults: true
            
            // Publish coverage
            publishHTML([
                reportDir: 'coverage/lcov-report',
                reportFiles: 'index.html',
                reportName: 'Coverage Report'
            ])
            
            // Archive artifacts
            archiveArtifacts artifacts: 'test-results/**/*', allowEmptyArchive: true
            
            // Cleanup
            sh 'docker-compose -f docker-compose.test.yml down -v'
        }
        
        success {
            echo 'All tests passed!'
        }
        
        failure {
            echo 'Tests failed!'
            // Send notification
            emailext(
                subject: "Build Failed: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                body: "Check console output at ${env.BUILD_URL}",
                to: "${env.CHANGE_AUTHOR_EMAIL}"
            )
        }
    }
}
```

## CircleCI

### Configuration

```yaml
# .circleci/config.yml
version: 2.1

orbs:
  node: circleci/node@5.1
  codecov: codecov/codecov@3.2

executors:
  node-postgres:
    docker:
      - image: cimg/node:18.19
      - image: cimg/postgres:15.4
        environment:
          POSTGRES_DB: test_db
          POSTGRES_USER: test_user
          POSTGRES_PASSWORD: test_password
      - image: cimg/redis:7.2

commands:
  install-dependencies:
    steps:
      - restore_cache:
          keys:
            - v1-deps-{{ checksum "package-lock.json" }}
            - v1-deps-
      - run:
          name: Install dependencies
          command: npm ci
      - save_cache:
          key: v1-deps-{{ checksum "package-lock.json" }}
          paths:
            - node_modules

  wait-for-services:
    steps:
      - run:
          name: Wait for PostgreSQL
          command: |
            dockerize -wait tcp://localhost:5432 -timeout 1m
      - run:
          name: Wait for Redis
          command: |
            dockerize -wait tcp://localhost:6379 -timeout 1m

jobs:
  unit-tests:
    executor: node-postgres
    steps:
      - checkout
      - install-dependencies
      - run:
          name: Run unit tests
          command: npm run test:unit -- --coverage
      - store_test_results:
          path: test-results
      - store_artifacts:
          path: coverage

  integration-tests:
    executor: node-postgres
    parallelism: 4
    steps:
      - checkout
      - install-dependencies
      - wait-for-services
      - run:
          name: Run migrations
          command: npm run db:migrate
          environment:
            DATABASE_URL: postgresql://test_user:test_password@localhost:5432/test_db
      - run:
          name: Seed test data
          command: npm run db:seed:test
      - run:
          name: Run integration tests
          command: |
            TESTFILES=$(circleci tests glob "tests/integration/**/*.test.js" | circleci tests split --split-by=timings)
            npm run test:integration -- $TESTFILES --coverage
          environment:
            DATABASE_URL: postgresql://test_user:test_password@localhost:5432/test_db
            REDIS_URL: redis://localhost:6379
      - store_test_results:
          path: test-results
      - store_artifacts:
          path: coverage
      - codecov/upload:
          flags: integration

  e2e-tests:
    docker:
      - image: mcr.microsoft.com/playwright:v1.40.0-focal
    steps:
      - checkout
      - install-dependencies
      - run:
          name: Start application
          command: npm run start:test
          background: true
      - run:
          name: Wait for application
          command: npx wait-on http://localhost:3000/health --timeout 60000
      - run:
          name: Run E2E tests
          command: npm run test:e2e
      - store_test_results:
          path: test-results
      - store_artifacts:
          path: playwright-report
          when: on_fail

workflows:
  test-and-deploy:
    jobs:
      - unit-tests
      - integration-tests:
          requires:
            - unit-tests
      - e2e-tests:
          requires:
            - integration-tests
      - deploy:
          requires:
            - e2e-tests
          filters:
            branches:
              only: main
```

## Docker Compose for Testing

```yaml
# docker-compose.test.yml
version: '3.8'

services:
  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_DB: test_db
      POSTGRES_USER: test_user
      POSTGRES_PASSWORD: test_password
    ports:
      - "5432:5432"
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U test_user"]
      interval: 5s
      timeout: 5s
      retries: 5
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 5s
      timeout: 3s
      retries: 5

  rabbitmq:
    image: rabbitmq:3-management-alpine
    environment:
      RABBITMQ_DEFAULT_USER: test
      RABBITMQ_DEFAULT_PASS: test
    ports:
      - "5672:5672"
      - "15672:15672"
    healthcheck:
      test: rabbitmq-diagnostics -q ping
      interval: 10s
      timeout: 5s
      retries: 5

  mongodb:
    image: mongo:7
    environment:
      MONGO_INITDB_ROOT_USERNAME: test
      MONGO_INITDB_ROOT_PASSWORD: test
    ports:
      - "27017:27017"
    healthcheck:
      test: echo 'db.runCommand("ping").ok' | mongosh localhost:27017/test --quiet
      interval: 10s
      timeout: 5s
      retries: 5

volumes:
  postgres_data:
```

## Package.json Scripts

```json
{
  "scripts": {
    "test": "npm run test:unit && npm run test:integration",
    "test:unit": "jest --testPathPattern=tests/unit --coverage",
    "test:integration": "jest --testPathPattern=tests/integration --runInBand",
    "test:api": "jest --testPathPattern=tests/integration/api",
    "test:database": "jest --testPathPattern=tests/integration/database",
    "test:services": "jest --testPathPattern=tests/integration/services",
    "test:e2e": "playwright test",
    "test:contract": "npm run test:contract:consumer && npm run test:contract:provider",
    "test:contract:consumer": "jest --testPathPattern=tests/contract/consumer",
    "test:contract:provider": "jest --testPathPattern=tests/contract/provider",
    "test:performance": "k6 run tests/performance/load-test.js",
    "test:watch": "jest --watch",
    "test:ci": "npm run test -- --ci --coverage --maxWorkers=2",
    "db:migrate": "knex migrate:latest",
    "db:seed:test": "knex seed:run --env=test",
    "start:test": "NODE_ENV=test node server.js",
    "docker:test:up": "docker-compose -f docker-compose.test.yml up -d",
    "docker:test:down": "docker-compose -f docker-compose.test.yml down -v",
    "pact:publish": "pact-broker publish ./pacts --consumer-app-version=$CI_COMMIT_SHA --broker-base-url=$PACT_BROKER_URL --broker-token=$PACT_BROKER_TOKEN"
  }
}
```

## Test Reporting

### Jest JUnit Reporter

```javascript
// jest.config.js
module.exports = {
  reporters: [
    'default',
    [
      'jest-junit',
      {
        outputDirectory: './test-results',
        outputName: 'junit.xml',
        classNameTemplate: '{classname}',
        titleTemplate: '{title}',
        ancestorSeparator: ' › ',
        usePathForSuiteName: true
      }
    ],
    [
      'jest-html-reporters',
      {
        publicPath: './test-results/html-report',
        filename: 'report.html',
        expand: true
      }
    ]
  ]
};
```

### Custom Test Report Script

```javascript
// scripts/generate-test-report.js
const fs = require('fs');
const path = require('path');

function generateReport(resultsFile) {
  const results = JSON.parse(fs.readFileSync(resultsFile, 'utf8'));
  
  const report = {
    summary: {
      total: results.numTotalTests,
      passed: results.numPassedTests,
      failed: results.numFailedTests,
      skipped: results.numPendingTests,
      duration: results.testResults.reduce((sum, r) => 
        sum + (r.endTime - r.startTime), 0
      )
    },
    suites: results.testResults.map(suite => ({
      file: suite.name,
      tests: suite.numPassingTests,
      failures: suite.numFailingTests,
      duration: suite.endTime - suite.startTime,
      failedTests: suite.testResults
        .filter(t => t.status === 'failed')
        .map(t => ({
          name: t.fullName,
          error: t.failureMessages.join('\n')
        }))
    }))
  };
  
  fs.writeFileSync(
    'test-report.json',
    JSON.stringify(report, null, 2)
  );
  
  console.log('Test Report Generated:');
  console.log(`✅ Passed: ${report.summary.passed}`);
  console.log(`❌ Failed: ${report.summary.failed}`);
  console.log(`⏭️  Skipped: ${report.summary.skipped}`);
  console.log(`⏱️  Duration: ${report.summary.duration}ms`);
}

generateReport(process.argv[2] || 'test-results.json');
```

## Best Practices

### 1. Fast Feedback
- Run unit tests before integration tests
- Use parallel execution
- Cache dependencies
- Use service health checks

### 2. Reliable Tests
- Retry flaky tests (max 2-3 times)
- Use proper timeouts
- Wait for services to be ready
- Clean up test data

### 3. Clear Reporting
- Generate JUnit XML for CI systems
- Create HTML reports for humans
- Include coverage reports
- Log failures with context

### 4. Resource Management
- Use Docker services for dependencies
- Clean up containers after tests
- Use test databases (not production)
- Manage secrets properly

### 5. Performance
- Run tests in parallel
- Use test sharding for large suites
- Cache node_modules
- Optimize database seeds

### 6. Security
- Never commit credentials
- Use CI secrets management
- Run security scans
- Audit dependencies

### 7. Maintainability
- Keep pipeline configuration DRY
- Use templates/orbs when available
- Document custom scripts
- Version control pipeline config
