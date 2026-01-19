# Commit Workflow

## 1. Review Changes

Before committing, understand what changed:

```bash
git status          # See modified files
git diff            # See specific changes
git diff --staged   # See staged changes
```

### 2. Stage Appropriately

Create atomic commits (one logical change per commit):

```bash
git add <specific-files>        # Stage specific files
git add -p                      # Stage specific hunks interactively
```

### 3. Generate Message

**For simple changes:**

```bash
git commit -m "type(scope): subject"
```

**For complex changes:**

```bash
git commit
# Opens editor for multi-line message
```

### 4. Verify Message

Check commit before pushing:

```bash
git log -1          # View last commit
git show            # View last commit with diff
git commit --amend  # Modify last commit message
```
