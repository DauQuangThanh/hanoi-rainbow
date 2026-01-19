# Commit Message Templates

## Template Setup

Create `.gitmessage` template:

```
# <type>(<scope>): <subject> (max 50 chars)

# Why is this change necessary?


# How does it address the issue?


# What side effects does this change have?


# Issue references (Closes #123, Fixes #456):

```

Configure git to use template:

```bash
git config --global commit.template ~/.gitmessage
```
