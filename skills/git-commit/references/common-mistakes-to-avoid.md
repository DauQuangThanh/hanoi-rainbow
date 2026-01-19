# Common Mistakes to Avoid

## Vague Messages

```
❌ fix: fixed issue
❌ update: updates
❌ changes
❌ WIP
❌ asdf
```

### Too Much Detail in Subject

```
❌ feat(auth): add new authentication system with OAuth2, JWT tokens, refresh token rotation, and password hashing
```

Better:

```
✅ feat(auth): implement OAuth2 authentication

- Add JWT token generation and validation
- Implement refresh token rotation
- Upgrade password hashing to bcrypt
```

### Missing Context

```
❌ fix(api): fix bug
```

Better:

```
✅ fix(api): prevent null pointer in user lookup

API crashed when querying users with deleted accounts.
Added null check and return 404 for deleted users.

Fixes #456
```

### Multiple Unrelated Changes

```
❌ feat: add login, update button styles, fix typo, refactor utils
```

Better: Split into 4 separate commits

### Wrong Tense/Mood

```
❌ fixed the login bug
❌ adding new feature
❌ updated documentation
```

Better:

```
✅ fix(auth): resolve login bug
✅ feat(api): add new endpoint
✅ docs(readme): update installation guide
```
