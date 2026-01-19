# Changelog

All notable changes to the Hanoi Rainbow project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Support for Jules IDE agent integration
- Support for Qoder CLI agent integration
- Support for Google Antigravity IDE agent integration
- Added jules, qoder, and antigravity to bash update-agent-context.sh script
- Added jules, qoder, and antigravity to PowerShell update-agent-context.ps1 script

## [0.1.0] - 2025-01-XX

### Added
- Initial release of Rainbow CLI
- Support for 19 AI agents and IDEs:
  - Claude Code (CLI)
  - Gemini CLI
  - GitHub Copilot (IDE)
  - Cursor (IDE)
  - Qwen Code (CLI)
  - opencode (CLI)
  - Codex CLI
  - Windsurf (IDE)
  - Kilo Code (IDE)
  - Auggie CLI
  - Roo Code (IDE)
  - CodeBuddy CLI
  - Amp (CLI)
  - SHAI (CLI)
  - Amazon Q Developer CLI
  - IBM Bob (IDE)
- 21 slash commands for Spec-Driven Development workflow
- 41 reusable skill modules organized by category
- Bash and PowerShell script variants for cross-platform support
- Template-based project initialization
- Git integration with semantic commit messages
- Interactive multi-select agent configuration
- Agent context update scripts
- Documentation website using DocFx

### Core Workflow Commands
- `/rainbow.regulate` - Create/update project governing principles
- `/rainbow.specify` - Define requirements and user stories
- `/rainbow.design` - Create technical implementation plans
- `/rainbow.taskify` - Generate actionable task lists
- `/rainbow.implement` - Execute all tasks automatically

### Product-Level Commands
- `/rainbow.architect` - System architecture documentation
- `/rainbow.standardize` - Coding standards and conventions
- `/rainbow.checklist` - Quality assurance checklists
- `/rainbow.convert-batch` - Legacy code conversion strategy

### Quality & Enhancement Commands
- `/rainbow.assess-legacy` - Legacy codebase assessment
- `/rainbow.analyze` - Deep code analysis
- `/rainbow.clarify` - Requirements clarification
- `/rainbow.validate-modernization` - Modernization validation
- `/rainbow.tasks-to-issues` - Export tasks to GitHub Issues

### Legacy Modernization Commands
- `/rainbow.map-business-logic` - Business logic mapping
- `/rainbow.plan-strangler` - Strangler fig migration planning
- `/rainbow.design-screen-migration` - Screen migration design
- `/rainbow.design-data-migration` - Data migration design
- `/rainbow.design-e2e-test` - E2E test design
- `/rainbow.perform-e2e-test` - Execute E2E tests
- `/rainbow.tasks-to-ado` - Export tasks to Azure DevOps

### Features
- Multi-agent support with comma-separated agent list
- Project initialization in current directory with `--here` flag
- Force merge/overwrite with `--force` flag
- SSL/TLS verification skip option for corporate environments
- Debug mode for troubleshooting
- GitHub token support for API requests
- Local template development mode
- Rainbow CLI tool checking with `rainbow check` command
- Version information with `rainbow version` command

### Developer Experience
- Automatic git commits with semantic prefixes
- Interactive agent selection menu
- Color-coded console output
- Step-by-step progress tracking
- Backup of existing agent files during upgrades
- Template validation and error handling

### Documentation
- Comprehensive README with quick start guide
- Installation and upgrade guides
- Local development documentation
- API documentation via DocFx
- Agent integration guide (AGENTS.md)
- Contributing guidelines
- Code of conduct
- Security policy

[Unreleased]: https://github.com/dauquangthanh/hanoi-rainbow/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/dauquangthanh/hanoi-rainbow/releases/tag/v0.1.0
