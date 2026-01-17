"""Configuration constants and agent definitions for Rainbow CLI."""

from pathlib import Path

# Agent configuration with name, agent_folder, skills_folder, install URL, and CLI tool requirement
AGENT_CONFIG = {
    "copilot": {
        "name": "GitHub Copilot",
        "agent_folder": ".github/agents/",
        "skills_folder": ".github/skills/",
        "install_url": None,  # IDE-based, no CLI check needed
        "requires_cli": False,
    },
    "claude": {
        "name": "Claude Code",
        "agent_folder": ".claude/commands/",
        "skills_folder": ".claude/skills/",
        "install_url": "https://docs.anthropic.com/en/docs/claude-code/setup",
        "requires_cli": True,
    },
    "gemini": {
        "name": "Gemini CLI",
        "agent_folder": ".gemini/commands/",
        "skills_folder": ".gemini/extensions/",
        "install_url": "https://github.com/google-gemini/gemini-cli",
        "requires_cli": True,
    },
    "cursor-agent": {
        "name": "Cursor",
        "agent_folder": ".cursor/commands/",
        "skills_folder": ".cursor/rules/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "qwen": {
        "name": "Qwen Code",
        "agent_folder": ".qwen/commands/",
        "skills_folder": ".qwen/skills/",
        "install_url": "https://github.com/QwenLM/qwen-code",
        "requires_cli": True,
    },
    "opencode": {
        "name": "opencode",
        "agent_folder": ".opencode/command/",
        "skills_folder": ".opencode/skill/",
        "install_url": "https://opencode.ai",
        "requires_cli": True,
    },
    "codex": {
        "name": "Codex CLI",
        "agent_folder": ".codex/commands/",
        "skills_folder": ".codex/skills/",
        "install_url": "https://github.com/openai/codex",
        "requires_cli": True,
    },
    "windsurf": {
        "name": "Windsurf",
        "agent_folder": ".windsurf/workflows/",
        "skills_folder": ".windsurf/skills/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "kilocode": {
        "name": "Kilo Code",
        "agent_folder": ".kilocode/rules/",
        "skills_folder": ".kilocode/skills/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "auggie": {
        "name": "Auggie CLI",
        "agent_folder": ".augment/rules/",
        "skills_folder": ".augment/rules/",  # Uses same folder for rules and skills
        "install_url": "https://docs.augmentcode.com/cli/setup-auggie/install-auggie-cli",
        "requires_cli": True,
    },
    "codebuddy": {
        "name": "CodeBuddy",
        "agent_folder": ".codebuddy/commands/",
        "skills_folder": ".codebuddy/skills/",
        "install_url": "https://www.codebuddy.ai/cli",
        "requires_cli": True,
    },
    "roo": {
        "name": "Roo Code",
        "agent_folder": ".roo/rules/",
        "skills_folder": ".roo/skills/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "q": {
        "name": "Amazon Q Developer CLI",
        "agent_folder": ".amazonq/prompts/",
        "skills_folder": ".amazonq/cli-agents/",
        "install_url": "https://aws.amazon.com/developer/learning/q-developer-cli/",
        "requires_cli": True,
    },
    "amp": {
        "name": "Amp",
        "agent_folder": ".agents/commands/",
        "skills_folder": ".agents/skills/",
        "install_url": "https://ampcode.com/manual#install",
        "requires_cli": True,
    },
    "shai": {
        "name": "SHAI",
        "agent_folder": ".shai/commands/",
        "skills_folder": ".shai/commands/",  # Uses same folder for commands and skills
        "install_url": "https://github.com/ovh/shai",
        "requires_cli": True,
    },
    "bob": {
        "name": "IBM Bob",
        "agent_folder": ".bob/commands/",
        "skills_folder": ".bob/skills/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "jules": {
        "name": "Jules",
        "agent_folder": ".agent/",
        "skills_folder": "skills/",  # Root-level skills folder
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "qoder": {
        "name": "Qoder CLI",
        "agent_folder": ".qoder/commands/",
        "skills_folder": ".qoder/skills/",
        "install_url": "https://qoder.ai",
        "requires_cli": True,
    },
    "antigravity": {
        "name": "Google Antigravity",
        "agent_folder": ".agent/rules/",
        "skills_folder": ".agent/skills/",
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
}

SCRIPT_TYPE_CHOICES = {"sh": "POSIX Shell (bash/zsh)", "ps": "PowerShell"}

CLAUDE_LOCAL_PATH = Path.home() / ".claude" / "local" / "claude"

# Extension mapping for each AI agent
EXTENSION_MAP = {
    "claude": ".md",
    "gemini": ".toml",
    "copilot": ".agent.md",
    "cursor-agent": ".md",
    "qwen": ".toml",
    "opencode": ".md",
    "codex": ".md",
    "windsurf": ".md",
    "kilocode": ".md",
    "auggie": ".md",
    "roo": ".md",
    "codebuddy": ".md",
    "amp": ".md",
    "shai": ".md",
    "q": ".md",
    "bob": ".md",
    "jules": ".md",
    "qoder": ".md",
    "antigravity": ".md",
}

# Args format for each AI agent
ARGS_FORMAT_MAP = {
    "claude": "$ARGUMENTS",
    "gemini": "{{args}}",
    "copilot": "$ARGUMENTS",
    "cursor-agent": "$ARGUMENTS",
    "qwen": "{{args}}",
    "opencode": "$ARGUMENTS",
    "codex": "$ARGUMENTS",
    "windsurf": "$ARGUMENTS",
    "kilocode": "$ARGUMENTS",
    "auggie": "$ARGUMENTS",
    "roo": "$ARGUMENTS",
    "codebuddy": "$ARGUMENTS",
    "amp": "$ARGUMENTS",
    "shai": "$ARGUMENTS",
    "q": "$ARGUMENTS",
    "bob": "$ARGUMENTS",
    "jules": "$ARGUMENTS",
    "qoder": "$ARGUMENTS",
    "antigravity": "$ARGUMENTS",
}

BANNER = """
██╗  ██╗ █████╗ ███╗   ██╗ ██████╗ ██╗    ██████╗  █████╗ ██╗███╗   ██╗██████╗  ██████╗ ██╗    ██╗
██║  ██║██╔══██╗████╗  ██║██╔═══██╗██║    ██╔══██╗██╔══██╗██║████╗  ██║██╔══██╗██╔═══██╗██║    ██║
███████║███████║██╔██╗ ██║██║   ██║██║    ██████╔╝███████║██║██╔██╗ ██║██████╔╝██║   ██║██║ █╗ ██║
██╔══██║██╔══██║██║╚██╗██║██║   ██║██║    ██╔══██╗██╔══██║██║██║╚██╗██║██╔══██╗██║   ██║██║███╗██║
██║  ██║██║  ██║██║ ╚████║╚██████╔╝██║    ██║  ██║██║  ██║██║██║ ╚████║██████╔╝╚██████╔╝╚███╔███╔╝
╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚═╝    ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝╚═════╝  ╚═════╝  ╚══╝╚══╝
"""

TAGLINE = "Hanoi Rainbow - Drive Quality Together with Agentic AI"

# GitHub repository information
GITHUB_REPO_OWNER = "dauquangthanh"
GITHUB_REPO_NAME = "hanoi-rainbow"
