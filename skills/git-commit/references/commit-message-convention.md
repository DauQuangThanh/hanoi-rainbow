# Commit Message Convention

All commits must follow conventional commit format:

**Required Types:** feat, fix, docs, refactor, test, chore
**Optional Scopes:** frontend, backend, api, db, auth, ui
**Subject Length:** Maximum 50 characters
**Body:** Required for breaking changes and complex fixes
**Footer:** Must reference issue number when applicable

```

## Enforce with Tools

**commitlint:**
```json
{
  "extends": ["@commitlint/config-conventional"],
  "rules": {
    "type-enum": [2, "always", [
      "feat", "fix", "docs", "refactor", "test", "chore", "perf", "ci", "build"
    ]],
    "subject-max-length": [2, "always", 50],
    "scope-enum": [2, "always", [
      "auth", "api", "ui", "db", "config"
    ]]
  }
}
```

**Git hooks (husky):**

```json
{
  "husky": {
    "hooks": {
      "commit-msg": "commitlint -E HUSKY_GIT_PARAMS"
    }
  }
}
```
