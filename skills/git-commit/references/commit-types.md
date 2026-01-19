# Commit Types

## Primary Types

**feat**: New feature for users

```
feat(auth): add OAuth2 authentication
feat(api): implement user profile endpoint
feat: add dark mode toggle
```

**fix**: Bug fix for users

```
fix(login): resolve session timeout issue
fix(api): handle null response from database
fix: prevent memory leak in image processing
```

**docs**: Documentation changes

```
docs(readme): update installation instructions
docs(api): add authentication examples
docs: fix typos in contributing guide
```

**refactor**: Code changes that neither fix bugs nor add features

```
refactor(auth): simplify token validation logic
refactor(db): extract query builder to separate module
refactor: convert callbacks to async/await
```

**perf**: Performance improvements

```
perf(search): optimize query with database indexing
perf(render): reduce component re-renders with memoization
perf: implement lazy loading for images
```

**test**: Adding or updating tests

```
test(auth): add unit tests for login flow
test(api): increase coverage for error scenarios
test: add integration tests for checkout process
```

**build**: Changes to build system or dependencies

```
build(deps): upgrade react to v18.2.0
build(webpack): optimize bundle size configuration
build: add npm script for production build
```

**ci**: Changes to CI/CD configuration

```
ci(github): add automated testing workflow
ci(deploy): configure staging environment
ci: update deployment pipeline timeout
```

**chore**: Other changes that don't modify src or test files

```
chore(deps): update development dependencies
chore: remove unused configuration files
chore(release): bump version to 2.1.0
```

**style**: Code style changes (formatting, missing semicolons, etc.)

```
style(eslint): fix linting errors
style: format code with prettier
style(css): organize stylesheet properties
```

**revert**: Reverting a previous commit

```
revert: revert "feat(api): add user endpoint"

This reverts commit abc123def456
```
