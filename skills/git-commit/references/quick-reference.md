# Quick Reference

## Type Decision Tree

```
Does it add new user-facing functionality? → feat
Does it fix a user-facing bug? → fix
Is it only documentation? → docs
Does it change code structure without changing behavior? → refactor
Does it improve performance? → perf
Is it adding/updating tests? → test
Is it dependency/build/CI related? → build/ci/chore
Is it code formatting/style? → style
Are you reverting a commit? → revert
```

### Subject Line Checklist

- [ ] Type and optional scope present: `type(scope):`
- [ ] Imperative mood: "add" not "added"
- [ ] Under 50 characters
- [ ] No period at end
- [ ] Describes what the commit does
- [ ] Specific and meaningful

### Body Checklist

- [ ] Blank line after subject
- [ ] Explains "why" not just "what"
- [ ] Lines wrapped at 72 characters
- [ ] Bullets or paragraphs for multiple points
- [ ] Blank line before footer

### Footer Checklist

- [ ] Breaking changes clearly marked
- [ ] Issue references included
- [ ] Co-author credits if applicable
