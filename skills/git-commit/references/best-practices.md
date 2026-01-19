# Best Practices

## Atomic Commits

**DO:** One logical change per commit

```
✅ Commit 1: feat(auth): add login endpoint
✅ Commit 2: feat(auth): add registration endpoint
✅ Commit 3: test(auth): add authentication tests
```

**DON'T:** Multiple unrelated changes

```
❌ Commit: feat(auth): add login, fix button styling, update docs, refactor utils
```

### Meaningful Messages

**DO:** Describe what and why

```
✅ fix(api): handle null values in user preferences

User preferences API crashed when optional fields were null.
Added null checks and default values.
```

**DON'T:** Vague descriptions

```
❌ fix: bug fix
❌ update: changes
❌ misc: various updates
```

### Present Tense, Imperative Mood

**DO:**

```
✅ add feature
✅ fix bug
✅ update documentation
✅ remove deprecated code
```

**DON'T:**

```
❌ added feature
❌ fixing bug
❌ updated documentation
❌ removes deprecated code
```

### Reference Issues

**Always link to issue tracker:**

```
Closes #123
Fixes #456
Resolves #789
Related to #234
```

### Breaking Changes Visibility

**Use exclamation mark for breaking changes:**

```
feat(api)!: change authentication method

BREAKING CHANGE: detailed description
```

**Or in footer:**

```
feat(api): change authentication method

BREAKING CHANGE: detailed description
```
