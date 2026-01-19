# Advanced Patterns

## Co-authored Commits

For pair programming:

```
feat(payment): implement Stripe integration

Co-authored-by: Jane Doe <jane@example.com>
Co-authored-by: John Smith <john@example.com>
```

### Signed Commits

For verified commits:

```bash
git commit -S -m "feat(auth): add 2FA support"
```

Message includes:

```
feat(auth): add 2FA support

Signed-off-by: Developer Name <dev@example.com>
```

### Fixup and Squash

For cleaner history:

```bash
git commit --fixup=<commit-hash>   # Mark as fixup
git rebase -i --autosquash         # Squash fixups
```
