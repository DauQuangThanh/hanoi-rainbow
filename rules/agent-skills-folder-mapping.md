Show me the default agent skills folder of the following IDE/CLI: Qoder CLI, Amazon Q Developer CLI,Amp,Auggie CLI,Claude Code,CodeBuddy CLI,Codex CLI,Cursor,Gemini CLI,GitHub Copilot,IBM Bob,Jules,Kilo Code,opencode,Qwen Code,Roo Code,SHAI (OVHcloud),Windsurf, Google Antigravity

Below are the default folder locations for agent skills (or their equivalent custom instructions/commands) for the requested tools.

### Global vs. Project Scope

Most tools support two layers: **Global** (applies to all projects) and **Project/Workspace** (checked into the repository).

| Tool | Project/Workspace Folder | Global/User Folder |
| --- | --- | --- |
| **Claude Code** | `.claude/skills/` | `~/.claude/skills/` |
| **Amazon Q Developer** | `.amazonq/cli-agents/` | `~/.aws/amazonq/cli-agents/` |
| **Amp** | `.agents/skills/` | `~/.config/agents/skills/` |
| **Auggie CLI** | `.augment/rules/` | `~/.claude/skills/` |
| **CodeBuddy CLI** | `.codebuddy/skills/` | `~/.codebuddy/skills/` |
| **Codex CLI** | `.codex/skills/` | `~/.codex/skills/` |
| **Cursor** | `.cursor/rules/` (or `/skills/`) | `~/.cursor/rules/` |
| **Gemini CLI** | `.gemini/extensions/` | `~/.gemini/skills/` |
| **GitHub Copilot** | `.github/skills/` | `~/.copilot/skills/` |
| **IBM Bob** | `.bob/skills/` | `~/.bob/skills/` |
| **Jules** | `skills/` (or `AGENTS.md`) | N/A |
| **Kilo Code** | `.kilocode/skills/` | `~/.kilocode/skills/` |
| **opencode** | `.opencode/skill/` | `~/.config/opencode/skill/` |
| **Qoder CLI** | `.qoder/skills/` | `~/.qoder/skills/` |
| **Qwen Code** | `.qwen/skills/` | `~/.qwen/skills/` |
| **Roo Code** | `.roo/skills/` | `~/.roo/skills/` |
| **SHAI (OVHcloud)** | `.shai/commands/` | `~/.config/shai/agents/` |
| **Windsurf** | `.windsurf/skills/` | `~/.codeium/windsurf/skills/` |
| **Google Antigravity** | `.agent/skills/` | `~/.gemini/antigravity/skills/` |

---

### Notable Differences

* **Google Antigravity**: Uses `.agent/rules/` for passive constraints and `.agent/workflows/` for explicit `/` commands, while `skills/` are for agent-triggered capabilities.
* **Cursor**: Predominantly uses `.cursor/rules` (MDC files) which behave similarly to skills but are often automatically applied based on context.
* **Amazon Q**: Often uses `.amazonq/prompts/` for custom slash-command definitions rather than a pure modular skill folder.
