# Common Scenarios

## Feature Addition

```
feat(checkout): add coupon code validation

- Implement coupon validation API endpoint
- Add coupon input field to checkout form
- Display discount amount in order summary
- Handle expired and invalid coupon errors

Closes #234
```

### Bug Fix

```
fix(payment): resolve duplicate charge issue

Race condition in payment processing caused some
transactions to be charged twice. Added transaction
locking to prevent concurrent processing of same order.

- Add distributed lock with 30s timeout
- Implement idempotency key validation
- Add retry logic for lock acquisition failures

Fixes #567
```

### Refactoring

```
refactor(user): extract validation to separate module

Move user validation logic from controller to dedicated
validator class for better testability and reuse.

- Create UserValidator class
- Move validation rules from UserController
- Add comprehensive validation tests
- Update documentation
```

### Documentation

```
docs(api): add authentication flow examples

Add code examples showing:
- OAuth2 authorization flow
- JWT token refresh process
- Error handling patterns

Related to #123
```

### Performance Improvement

```
perf(search): implement query result caching

Add Redis caching layer for search queries to reduce
database load and improve response times.

Results:
- Average response time: 450ms → 45ms
- Database queries reduced by 80%
- Cache hit rate: 92%

Closes #789
```

### Breaking Change

```
feat(api)!: migrate to GraphQL schema v2

BREAKING CHANGE: GraphQL schema updated to v2 with
field naming changes and deprecated field removals.

Changes:
- Rename `userId` to `id` in User type
- Remove deprecated `fullName` field (use `firstName` + `lastName`)
- Change `createdAt` format to ISO 8601

Migration guide: docs/migration/v1-to-v2.md

Closes #456
```

### Multiple Related Changes

```
refactor(auth): modernize authentication system

Comprehensive authentication refactor including:

- Replace session-based auth with JWT tokens
- Implement refresh token rotation
- Add OAuth2 provider support (Google, GitHub)
- Migrate password hashing to bcrypt
- Update security headers and CORS config

This improves security, performance, and enables SSO.
Session data migration script provided in scripts/migrate-sessions.js

Closes #234, #567, #890
```
