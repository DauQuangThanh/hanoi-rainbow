Show me the default agent config folder of the following IDE/CLI: Qoder CLI, Amazon Q Developer CLI,Amp,Auggie CLI,Claude Code,CodeBuddy CLI,Codex CLI,Cursor,Gemini CLI,GitHub Copilot,IBM Bob,Jules,Kilo Code,opencode,Qwen Code,Roo Code,SHAI (OVHcloud),Windsurf, Google Antigravity

To manage agent-specific instructions, commands, and rules, most modern AI-driven CLIs and IDEs use a project-level hidden directory.

Based on the [Spec-Kit](https://github.com/github/spec-kit) conventions and official documentation, here is the complete list of the **default agent/command folders** for the tools you mentioned:

### **Project-Level Agent Folders**

| IDE / CLI Tool | Default Agent Folder | Primary File Type |
| --- | --- | --- |
| **Claude Code** | `.claude/commands/` | `.md` |
| **GitHub Copilot** | `.github/agents/` (custom agents) + `.github/prompts/` (slash commands) | `.agent.md` / `.prompt.md` |
| **Google Antigravity** | `.agent/rules/` or `.agent/skills/` | `.md` |
| **Cursor** | `.cursor/rules/` or `.cursor/commands/` | `.mdc` / `.md` |
| **Windsurf** | `.windsurf/workflows/` or `.windsurf/rules/` | `.md` |
| **Amazon Q CLI** | `.amazonq/prompts/` or `.amazonq/cli-agents/` | `.md` / `.json` |
| **Gemini CLI** | `.gemini/commands/` | `.toml` |
| **Kilo Code** | `.kilocode/skills/` or `.kilocode/rules/` | `.md` |
| **Roo Code** | `.roo/rules/` | `.md` |
| **Qoder CLI** | `.qoder/commands/` | `.md` |
| **Auggie CLI** | `.augment/rules/` | `.md` |
| **Qwen Code** | `.qwen/commands/` | `.toml` |
| **Codex CLI** | `.codex/commands/` | `.md` |
| **SHAI (OVHcloud)** | `.shai/commands/` | `.md` |
| **opencode** | `.opencode/command/` | `.md` |
| **CodeBuddy CLI** | `.codebuddy/commands/` | `.md` |
| **Amp (Sourcegraph)** | `.agents/commands/` | `.md` |
| **IBM Bob** | `.bob/commands/` | `.md` |
| **Jules** | `.agent/` or `AGENTS.md` | `.md` |

---

### **Key Nuances**

* **Google Antigravity:** Distinguishes between **Rules** (`.agent/rules/` - passive constraints) and **Skills** (`.agent/skills/` - active tools/capabilities).
* **Windsurf:** Uses a structured "Memory Bank" within `.windsurf/core/` for long-term project context (e.g., `activeContext.md`).
* **Cursor:** Moving toward the `.mdc` format located in `.cursor/rules/` which allows for "Rule Metadata" like `glob` patterns to trigger rules automatically.
* **Global Overrides:** Most of these tools also look in your home directory (e.g., `~/.claude/` or `~/.gemini/antigravity/skills/`) for agents that should be available across **all** your projects.
