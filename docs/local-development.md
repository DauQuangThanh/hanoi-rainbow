# 💻 Local Development Guide

**Work on Rainbow CLI locally without publishing releases.**

> **Note:** All scripts come in both Bash (`.sh`) and PowerShell (`.ps1`) versions. The CLI auto-picks based on your OS unless you specify `--script sh|ps`.

---

## 🚀 Quick Start

### 1. Clone the Repository

```bash
git clone https://github.com/dauquangthanh/hanoi-rainbow.git
cd hanoi-rainbow

# Work on a feature branch
git checkout -b your-feature-branch
```

---

### 2. Run CLI Directly (Fastest Way)

Test your changes instantly without installing:

```bash
# From repository root
python -m src.rainbow_cli --help
python -m src.rainbow_cli init demo-project --ai claude --ignore-agent-tools --script sh

# Multiple AI agents (comma-separated)
python -m src.rainbow_cli init demo-project --ai claude,gemini,copilot --script sh

# Use local templates (no GitHub download)
python -m src.rainbow_cli init demo-project --ai claude --local-templates --template-path . --script sh
```

**Alternative:** Run the script directly (uses shebang):

```bash
python src/rainbow_cli/__init__.py init demo-project --script ps
```

---

### 3. Use Editable Install (Like Real Users)

Create an isolated environment that matches how users run Rainbow:

```bash
# Create virtual environment (uv manages .venv automatically)
uv venv

# Activate it
source .venv/bin/activate  # Linux/macOS
# or on Windows PowerShell:
.venv\Scripts\Activate.ps1

# Install in editable mode
uv pip install -e .

# Now use 'rainbow' command directly
rainbow --help
```

**Benefit:** No need to reinstall after code changes—it updates automatically!

### 4. Test with uvx (Simulate User Experience)

Test how users will actually run Rainbow:

**From local directory:**

```bash
uvx --from . rainbow init demo-uvx --ai copilot --ignore-agent-tools --script sh
```

**From a specific branch (without merging):**

```bash
# Push your branch first
git push origin your-feature-branch

# Test it
uvx --from git+https://github.com/dauquangthanh/hanoi-rainbow.git@your-feature-branch rainbow init demo-branch-test --script ps
```

#### Run from Anywhere (Absolute Path)

Use absolute paths when you're in a different directory:

```bash
uvx --from /mnt/c/GitHub/hanoi-rainbow rainbow --help
uvx --from /mnt/c/GitHub/hanoi-rainbow rainbow init demo-anywhere --ai copilot --script sh
```

**Make it easier with an environment variable:**

```bash
# Set once
export RAINBOW_SRC=/mnt/c/GitHub/hanoi-rainbow

# Use anywhere
uvx --from "$RAINBOW_SRC" rainbow init demo-env --ai copilot --script ps
```

**Or create a shell function:**

```bash
rainbow-dev() { uvx --from /mnt/c/GitHub/hanoi-rainbow rainbow "$@"; }

# Then just use
rainbow-dev --help
```

---

### 5. Check Script Permissions

After running `init`, verify shell scripts are executable (Linux/macOS only):

```bash
ls -l scripts | grep .sh
# Expect: -rwxr-xr-x (owner execute bit set)
```

> **Note:** Windows PowerShell scripts (`.ps1`) don't need chmod.

---

### 6. Quick Sanity Check

Verify your code imports correctly:

```bash
python -c "import rainbow_cli; print('Import OK')"
```

---

### 7. Build a Wheel (Optional)

Test packaging before publishing:

```bash
uv build
ls dist/
```

Install the built wheel in a fresh environment if needed.

### 8. Use a Temporary Workspace

Test `init --here` without cluttering your repo:

```bash
mkdir /tmp/rainbow-test && cd /tmp/rainbow-test
python -m src.rainbow_cli init --here --ai claude --ignore-agent-tools --script sh
```

---

### 9. Debug Network Issues

Skip TLS validation during local testing (not for production!):

```bash
rainbow check --skip-tls
rainbow init demo --skip-tls --ai gemini --ignore-agent-tools --script ps
```

---

## � Repository Structure

Understanding the Rainbow CLI repository layout:

```
hanoi-rainbow/
├── commands/              # Slash command definitions (copied to agent folders)
│   ├── regulate.md       # Project principles command
│   ├── specify.md        # Requirements command
│   ├── design.md         # Technical planning command
│   └── templates-for-commands/  # Reusable templates
│
├── skills/               # Reusable skill modules (copied to agent skills folders)
│   ├── backend-coding/
│   ├── frontend-design/
│   ├── database-design/
│   └── ... (41 skills total)
│
├── memory/              # Default project memory (ground rules, etc.)
├── scripts/             # Automation scripts (bash + PowerShell)
├── src/rainbow_cli/     # CLI source code
├── docs/                # Documentation
└── .github/workflows/   # CI/CD and release automation
```

**Note:** The `commands/` and `skills/` folders are source templates. When you run `rainbow init`, these are copied into your project's agent-specific folders (`.claude/`, `.github/agents/`, etc.).

---

## �🔄 Quick Reference

| What You Want | Command |
| --------------- | ---------- |
| **Run CLI directly** | `python -m src.rainbow_cli --help` |
| **Editable install** | `uv pip install -e .` then `rainbow ...` |
| **Local uvx (repo root)** | `uvx --from . rainbow ...` |
| **Local uvx (absolute path)** | `uvx --from /path/to/hanoi-rainbow rainbow ...` |
| **Test specific branch** | `uvx --from git+URL@branch rainbow ...` |
| **Build package** | `uv build` |
| **Clean up** | `rm -rf .venv dist build *.egg-info` |

---

## 🧹 Cleanup

Remove build artifacts and virtual environments:

```bash
rm -rf .venv dist build *.egg-info
```

---

## 🛠️ Common Issues

| Problem | Solution |
| --------- | ---------- |
| **`ModuleNotFoundError: typer`** | Run `uv pip install -e .` to install dependencies |
| **Scripts not executable (Linux)** | Re-run init or manually run `chmod +x scripts/*.sh` |
| **Git step skipped** | You passed `--no-git` or Git isn't installed |
| **Wrong script type** | Pass `--script sh` or `--script ps` explicitly |
| **TLS errors (corporate network)** | Try `--skip-tls` (not recommended for production) |

---

## 👉 Next Steps

1. **Test your changes** - Run through the Quick Start guide with your modified CLI
2. **Update docs** - Document any new features or changes
3. **Open a PR** - Share your improvements when ready
4. **Tag a release** - Once merged to `main`, create a release tag (optional)

---

## 📚 Resources

- 📖 [Quick Start Guide](quickstart.md) - Test your changes end-to-end
- 🐛 [Report Issues](https://github.com/dauquangthanh/hanoi-rainbow/issues/new) - Found a bug?
- 💬 [Discussions](https://github.com/dauquangthanh/hanoi-rainbow/discussions) - Ask questions
