"""Configuration constants and agent definitions for Rainbow CLI."""

from pathlib import Path

# Agent configuration with name, folder, subfolder, install URL, and CLI tool requirement
AGENT_CONFIG = {
    "copilot": {
        "name": "GitHub Copilot",
        "folder": ".github/",
        "subfolder": "agents",  # .github/agents/
        "install_url": None,  # IDE-based, no CLI check needed
        "requires_cli": False,
    },
    "claude": {
        "name": "Claude Code",
        "folder": ".claude/",
        "subfolder": "commands",  # .claude/commands/
        "install_url": "https://docs.anthropic.com/en/docs/claude-code/setup",
        "requires_cli": True,
    },
    "gemini": {
        "name": "Gemini CLI",
        "folder": ".gemini/",
        "subfolder": "commands",  # .gemini/commands/
        "install_url": "https://github.com/google-gemini/gemini-cli",
        "requires_cli": True,
    },
    "cursor-agent": {
        "name": "Cursor",
        "folder": ".cursor/",
        "subfolder": "commands",  # .cursor/commands/
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "qwen": {
        "name": "Qwen Code",
        "folder": ".qwen/",
        "subfolder": "commands",  # .qwen/commands/
        "install_url": "https://github.com/QwenLM/qwen-code",
        "requires_cli": True,
    },
    "opencode": {
        "name": "opencode",
        "folder": ".opencode/",
        "subfolder": "command",  # .opencode/command/ (singular)
        "install_url": "https://opencode.ai",
        "requires_cli": True,
    },
    "codex": {
        "name": "Codex CLI",
        "folder": ".codex/",
        "subfolder": "prompts",  # .codex/prompts/
        "install_url": "https://github.com/openai/codex",
        "requires_cli": True,
    },
    "windsurf": {
        "name": "Windsurf",
        "folder": ".windsurf/",
        "subfolder": "workflows",  # .windsurf/workflows/
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "kilocode": {
        "name": "Kilo Code",
        "folder": ".kilocode/",
        "subfolder": "workflows",  # .kilocode/workflows/
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "auggie": {
        "name": "Auggie CLI",
        "folder": ".augment/",
        "subfolder": "commands",  # .augment/commands/
        "install_url": "https://docs.augmentcode.com/cli/setup-auggie/install-auggie-cli",
        "requires_cli": True,
    },
    "codebuddy": {
        "name": "CodeBuddy",
        "folder": ".codebuddy/",
        "subfolder": "commands",  # .codebuddy/commands/
        "install_url": "https://www.codebuddy.ai/cli",
        "requires_cli": True,
    },
    "roo": {
        "name": "Roo Code",
        "folder": ".roo/",
        "subfolder": "commands",  # .roo/commands/
        "install_url": None,  # IDE-based
        "requires_cli": False,
    },
    "q": {
        "name": "Amazon Q Developer CLI",
        "folder": ".amazonq/",
        "subfolder": "prompts",  # .amazonq/prompts/
        "install_url": "https://aws.amazon.com/developer/learning/q-developer-cli/",
        "requires_cli": True,
    },
    "amp": {
        "name": "Amp",
        "folder": ".agents/",
        "subfolder": "commands",  # .agents/commands/
        "install_url": "https://ampcode.com/manual#install",
        "requires_cli": True,
    },
    "shai": {
        "name": "SHAI",
        "folder": ".shai/",
        "subfolder": "commands",  # .shai/commands/
        "install_url": "https://github.com/ovh/shai",
        "requires_cli": True,
    },
    "bob": {
        "name": "IBM Bob",
        "folder": ".bob/",
        "subfolder": "commands",  # .bob/commands/
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
