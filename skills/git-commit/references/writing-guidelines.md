# Writing Guidelines

## Subject Line Rules

**DO:**

- ✅ Use imperative mood: "add" not "added" or "adds"
- ✅ Keep under 50 characters
- ✅ Don't end with period
- ✅ Capitalize first letter after type/scope
- ✅ Be specific and descriptive

**Examples:**

```
✅ feat(auth): add password reset functionality
✅ fix(api): resolve race condition in user updates
✅ refactor(db): simplify connection pooling logic
```

**DON'T:**

```
❌ feat: added stuff
❌ fix: fixes
❌ update: updated some files
❌ misc changes
❌ WIP
```

### Scope Selection

**Common Scopes by Project Type:**

**Web Application:**

- `auth`, `api`, `ui`, `db`, `config`, `router`, `state`, `validation`

**Library/SDK:**

- `core`, `utils`, `types`, `docs`, `examples`, `tests`

**Mobile App:**

- `ios`, `android`, `ui`, `navigation`, `storage`, `network`

**Microservices:**

- `user-service`, `payment-service`, `gateway`, `auth`, `logging`

**Omit scope when change affects multiple areas or entire project.**

### Body Content

**When to include a body:**

- Change requires explanation
- Multiple related changes
- Context about "why" not obvious
- Breaking changes need documentation
- Complex bug fixes

**Body Format:**

```
<type>(<scope>): <subject>
[blank line]
- Explain what changed
- Explain why it changed
- Reference relevant context
- Note any side effects
[blank line]
<footer>
```

**Example:**

```
fix(auth): prevent token expiry race condition

- Add mutex lock around token refresh operations
- Implement exponential backoff for retry attempts
- Update token validation to check expiry before use

This fixes an issue where concurrent requests could cause
multiple token refresh attempts, leading to authentication
failures.

Closes #456
```

### Footer Usage

**Breaking Changes:**

```
feat(api): change response format to JSON:API spec

BREAKING CHANGE: API responses now follow JSON:API format.
Update client code to parse data from `data` key instead
of root level. See migration guide in docs/MIGRATION.md
```

**Issue References:**

```
Closes #123
Fixes #456, #789
Resolves #234
Related to #567
```

**Multiple References:**

```
This change affects multiple systems and resolves several issues.

Closes #123, #456
Related to #789
See also: #234
```
