# Upgrade Guide

> You have Hanoi Rainbow installed and want to upgrade to the latest version to get new features, bug fixes, or updated slash commands. This guide covers both upgrading the CLI tool and updating your project files.

---

## Quick Reference

| What to Upgrade | Command | When to Use |
|----------------|---------|-------------|
| **CLI Tool Only** | `uv tool install rainbow-cli --force --from git+https://github.com/dauquangthanh/hanoi-rainbow.git` | Get latest CLI features without touching project files |
| **Project Files** | `rainbow init --here --force --ai <your-agent>` | Update slash commands, templates, and scripts in your project |
| **Both** | Run CLI upgrade, then project update | Recommended for major version updates |

---

## Part 1: Upgrade the CLI Tool

The CLI tool (`rainbow`) is separate from your project files. Upgrade it to get the latest features and bug fixes.

### If you installed with `uv tool install`

```bash
uv tool install rainbow-cli --force --from git+https://github.com/dauquangthanh/hanoi-rainbow.git
```

### If you use one-shot `uvx` commands

No upgrade needed—`uvx` always fetches the latest version. Just run your commands as normal:

```bash
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git rainbow init --here --ai copilot
```

### Verify the upgrade

```bash
rainbow check
```

This shows installed tools and confirms the CLI is working.

---

## Part 2: Updating Project Files

When Hanoi Rainbow releases new features (like new slash commands or updated templates), you need to refresh your project's Hanoi Rainbow files.

### What gets updated?

Running `rainbow init --here --force` will update:

- ✅ **Slash command files** (`.claude/commands/`, `.github/prompts/`, etc.)
- ✅ **Script files** (`.rainbow/scripts/`)
- ✅ **Template files** (`.rainbow/templates/`)
- ✅ **Shared memory files** (`.rainbow/memory/`) - **⚠️ See warnings below**

### What stays safe?

These files are **never touched** by the upgrade—the template packages don't even contain them:

- ✅ **Your specifications** (`specs/001-my-feature/spec.md`, etc.) - **CONFIRMED SAFE**
- ✅ **Your implementation plans** (`specs/001-my-feature/plan.md`, `tasks.md`, etc.) - **CONFIRMED SAFE**
- ✅ **Your source code** - **CONFIRMED SAFE**
- ✅ **Your git history** - **CONFIRMED SAFE**

The `specs/` directory is completely excluded from template packages and will never be modified during upgrades.

### Update command

Run this inside your project directory:

```bash
rainbow init --here --force --ai <your-agent>
```

Replace `<your-agent>` with your AI assistant. Refer to this list of [Supported AI Agents](../README.md#-supported-ai-agents)

**Example:**

```bash
rainbow init --here --force --ai copilot
```

### Understanding the `--force` flag

Without `--force`, the CLI warns you and asks for confirmation:

```
Warning: Current directory is not empty (25 items)
Template files will be merged with existing content and may overwrite existing files
Proceed? [y/N]
```

With `--force`, it skips the confirmation and proceeds immediately.

**Important: Your `specs/` directory is always safe.** The `--force` flag only affects template files (commands, scripts, templates, memory). Your feature specifications, plans, and tasks in `specs/` are never included in upgrade packages and cannot be overwritten.

---

## ⚠️ Important Warnings

### 1. Ground rules file will be overwritten

**Known issue:** `rainbow init --here --force` currently overwrites `.rainbow/memory/ground-rules.md` with the default template, erasing any customizations you made.

**Workaround:**

```bash
# 1. Back up your ground rules before upgrading
cp .rainbow/memory/ground-rules.md .rainbow/memory/ground-rules-backup.md

# 2. Run the upgrade
rainbow init --here --force --ai copilot

# 3. Restore your customized ground rules
mv .rainbow/memory/ground-rules-backup.md .rainbow/memory/ground-rules.md
```

Or use git to restore it:

```bash
# After upgrade, restore from git history
git restore .rainbow/memory/ground-rules.md
```

### 2. Custom template modifications

If you customized any templates in `.rainbow/templates/`, the upgrade will overwrite them. Back them up first:

```bash
# Back up custom templates
cp -r .rainbow/templates .rainbow/templates-backup

# After upgrade, merge your changes back manually
```

### 3. Duplicate slash commands (IDE-based agents)

Some IDE-based agents (like Kilo Code, Windsurf) may show **duplicate slash commands** after upgrading—both old and new versions appear.

**Solution:** Manually delete the old command files from your agent's folder.

**Example for Kilo Code:**

```bash
# Navigate to the agent's commands folder
cd .kilocode/rules/

# List files and identify duplicates
ls -la

# Delete old versions (example filenames - yours may differ)
rm rainbow.specify-old.md
rm rainbow.design-v1.md
```

Restart your IDE to refresh the command list.

---

## Common Scenarios

### Scenario 1: "I just want new slash commands"

```bash
# Upgrade CLI (if using persistent install)
uv tool install rainbow-cli --force --from git+https://github.com/dauquangthanh/hanoi-rainbow.git

# Update project files to get new commands
rainbow init --here --force --ai copilot

# Restore your ground rules if customized
git restore .rainbow/memory/ground-rules.md
```

### Scenario 2: "I customized templates and ground-rules"

```bash
# 1. Back up customizations
cp .rainbow/memory/ground-rules.md /tmp/ground-rules-backup.md
cp -r .rainbow/templates /tmp/templates-backup

# 2. Upgrade CLI
uv tool install rainbow-cli --force --from git+https://github.com/dauquangthanh/hanoi-rainbow.git

# 3. Update project
rainbow init --here --force --ai copilot

# 4. Restore customizations
mv /tmp/ground-rules-backup.md .rainbow/memory/ground-rules.md
# Manually merge template changes if needed
```

### Scenario 3: "I see duplicate slash commands in my IDE"

This happens with IDE-based agents (Kilo Code, Windsurf, Roo Code, etc.).

```bash
# Find the agent folder (example: .kilocode/rules/)
cd .kilocode/rules/

# List all files
ls -la

# Delete old command files
rm rainbow.old-command-name.md

# Restart your IDE
```

### Scenario 4: "I'm working on a project without Git"

If you initialized your project with `--no-git`, you can still upgrade:

```bash
# Manually back up files you customized
cp .rainbow/memory/ground-rules.md /tmp/ground-rules-backup.md

# Run upgrade
rainbow init --here --force --ai copilot --no-git

# Restore customizations
mv /tmp/ground-rules-backup.md .rainbow/memory/ground-rules.md
```

The `--no-git` flag skips git initialization but doesn't affect file updates.

---

## Using `--no-git` Flag

The `--no-git` flag tells Hanoi Rainbow to **skip git repository initialization**. This is useful when:

- You manage version control differently (Mercurial, SVN, etc.)
- Your project is part of a larger monorepo with existing git setup
- You're experimenting and don't want version control yet

**During initial setup:**

```bash
rainbow init my-project --ai copilot --no-git
```

**During upgrade:**

```bash
rainbow init --here --force --ai copilot --no-git
```

### What `--no-git` does NOT do

❌ Does NOT prevent file updates
❌ Does NOT skip slash command installation
❌ Does NOT affect template merging

It **only** skips running `git init` and creating the initial commit.

### Working without Git

If you use `--no-git`, you'll need to manage feature directories manually:

**Set the `SPECIFY_FEATURE` environment variable** before using planning commands:

```bash
# Bash/Zsh
export SPECIFY_FEATURE="001-my-feature"

# PowerShell
$env:SPECIFY_FEATURE = "001-my-feature"
```

This tells Hanoi Rainbow which feature directory to use when creating specs, plans, and tasks.

**Why this matters:** Without git, Hanoi Rainbow can't detect your current branch name to determine the active feature. The environment variable provides that context manually.

---

## Troubleshooting

### "Slash commands not showing up after upgrade"

**Cause:** Agent didn't reload the command files.

**Fix:**

1. **Restart your IDE/editor** completely (not just reload window)
2. **For CLI-based agents**, verify files exist:

   ```bash
   ls -la .claude/commands/      # Claude Code
   ls -la .gemini/commands/       # Gemini
   ls -la .cursor/commands/       # Cursor
   ```

3. **Check agent-specific setup:**
   - Codex requires `CODEX_HOME` environment variable
   - Some agents need workspace restart or cache clearing

### "I lost my ground rules customizations"

**Fix:** Restore from git or backup:

```bash
# If you committed before upgrading
git restore .rainbow/memory/ground-rules.md

# If you backed up manually
cp /tmp/ground-rules-backup.md .rainbow/memory/ground-rules.md
```

**Prevention:** Always commit or back up `ground-rules.md` before upgrading.

### "Warning: Current directory is not empty"

**Full warning message:**

```
Warning: Current directory is not empty (25 items)
Template files will be merged with existing content and may overwrite existing files
Do you want to continue? [y/N]
```

**What this means:**

This warning appears when you run `rainbow init --here` (or `rainbow init .`) in a directory that already has files. It's telling you:

1. **The directory has existing content** - In the example, 25 files/folders
2. **Files will be merged** - New template files will be added alongside your existing files
3. **Some files may be overwritten** - If you already have Hanoi Rainbow files (`.claude/`, `.rainbow/`, etc.), they'll be replaced with the new versions

**What gets overwritten:**

Only Hanoi Rainbow infrastructure files:

- Agent command files (`.claude/commands/`, `.github/prompts/`, etc.)
- Scripts in `.rainbow/scripts/`
- Templates in `.rainbow/templates/`
- Memory files in `.rainbow/memory/` (including ground-rules)

**What stays untouched:**

- Your `specs/` directory (specifications, plans, tasks)
- Your source code files
- Your `.git/` directory and git history
- Any other files not part of Hanoi Rainbow templates

**How to respond:**

- **Type `y` and press Enter** - Proceed with the merge (recommended if upgrading)
- **Type `n` and press Enter** - Cancel the operation
- **Use `--force` flag** - Skip this confirmation entirely:

  ```bash
  rainbow init --here --force --ai copilot
  ```

**When you see this warning:**

- ✅ **Expected** when upgrading an existing Hanoi Rainbow project
- ✅ **Expected** when adding Hanoi Rainbow to an existing codebase
- ⚠️ **Unexpected** if you thought you were creating a new project in an empty directory

**Prevention tip:** Before upgrading, commit or back up your `.rainbow/memory/ground-rules.md` if you customized it.

### "CLI upgrade doesn't seem to work"

Verify the installation:

```bash
# Check installed tools
uv tool list

# Should show rainbow-cli

# Verify path
which rainbow

# Should point to the uv tool installation directory
```

If not found, reinstall:

```bash
uv tool uninstall rainbow-cli
uv tool install rainbow-cli --from git+https://github.com/dauquangthanh/hanoi-rainbow.git
```

### "Do I need to run rainbow every time I open my project?"

**Short answer:** No, you only run `rainbow init` once per project (or when upgrading).

**Explanation:**

The `rainbow` CLI tool is used for:

- **Initial setup:** `rainbow init` to bootstrap Hanoi Rainbow in your project
- **Upgrades:** `rainbow init --here --force` to update templates and commands
- **Diagnostics:** `rainbow check` to verify tool installation

Once you've run `rainbow init`, the slash commands (like `/rainbow.specify`, `/rainbow.design`, etc.) are **permanently installed** in your project's agent folder (`.claude/`, `.github/prompts/`, etc.). Your AI assistant reads these command files directly—no need to run `rainbow` again.

**If your agent isn't recognizing slash commands:**

1. **Verify command files exist:**

   ```bash
   # For GitHub Copilot
   ls -la .github/prompts/

   # For Claude
   ls -la .claude/commands/
   ```

2. **Restart your IDE/editor completely** (not just reload window)

3. **Check you're in the correct directory** where you ran `rainbow init`

4. **For some agents**, you may need to reload the workspace or clear cache

**Related issue:** If Copilot can't open local files or uses PowerShell commands unexpectedly, this is typically an IDE context issue, not related to `rainbow`. Try:

- Restarting VS Code
- Checking file permissions
- Ensuring the workspace folder is properly opened

---

## Version Compatibility

Hanoi Rainbow follows semantic versioning for major releases. The CLI and project files are designed to be compatible within the same major version.

**Best practice:** Keep both CLI and project files in sync by upgrading both together during major version changes.

---

## Next Steps

After upgrading:

- **Test new slash commands:** Run `/rainbow.regulate` or another command to verify everything works
- **Review release notes:** Check [GitHub Releases](https://github.com/dauquangthanh/hanoi-rainbow/releases) for new features and breaking changes
- **Update workflows:** If new commands were added, update your team's development workflows
- **Check documentation:** Visit [github.io/spec-kit](https://github.github.io/spec-kit/) for updated guides
