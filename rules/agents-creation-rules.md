# Agent Creation Rules and Best Practices

```yaml
# DOCUMENT METADATA FOR LLM PROCESSING
document_type: technical_specification
format_version: 1.0
primary_audience: llm_agents
processing_mode: strict_parsing
validation_required: true
language: en-US
last_updated: 2026-01-21
```

## LLM PROCESSING INSTRUCTIONS

**PARSING STRATEGY**:

1. Read entire document into context
2. Index sections by markdown headers (##, ###, ####)
3. Extract all code blocks with language tags
4. Parse YAML/TOML/JSON structures strictly
5. Treat ✅/❌ markers as boolean success/failure indicators
6. Process decision trees as conditional logic flows
7. Extract validation rules from checklist sections
8. Apply patterns from "ANTI-PATTERN vs. GOOD PATTERN" sections

**DOCUMENT STRUCTURE**:

- Universal Principles: APPLY TO ALL commands
- File Format Standards: SELECT based on agent type
- Agent-Specific Guidelines: APPLY only for matched agent
- Command Structure: VALIDATE against these rules
- Prompt Engineering: USE these patterns in generation
- Cross-Platform Scripts: GENERATE for all 3 platforms
- Testing/Validation: RUN these checks before output

---

> **DOCUMENT PURPOSE**: This document provides comprehensive guidelines for creating custom AI agents, commands, and workflows. It is designed to be read by both humans and LLMs to ensure consistent, high-quality agent creation across multiple AI-powered development tools.
>
> **TARGET AUDIENCE**: AI agents, developers, and teams building custom agents for spec-driven development and AI-assisted coding workflows.

---

## Universal Principles

```yaml
# PRINCIPLES SCHEMA
applicability: ALL_AGENT_COMMANDS
enforcement: MANDATORY
validation_level: STRICT
```

> **APPLY THESE PRINCIPLES TO ALL AGENT COMMANDS REGARDLESS OF PLATFORM**

### 1. Clarity and Specificity

```yaml
principle_id: P001
rule_type: CONTENT_QUALITY
validation: REQUIRED
```

**RULE**: Always be explicit about intent and expected outcomes.

**REQUIREMENTS**:

```yaml
requirements:
  - id: R001
    description: Define purpose clearly
    validation: purpose_field_present AND purpose_length >= 10
    
  - id: R002
    description: Specify input requirements
    validation: input_section_present OR arguments_documented
    
  - id: R003
    description: Specify output format
    validation: output_section_present AND output_format_specified
    
  - id: R004
    description: Use concrete examples
    validation: examples_count >= 1
```

**ANTI-PATTERN vs. GOOD PATTERN**:

```markdown
❌ ANTI-PATTERN: "Help with code"
   - Too vague
   - No clear scope
   - Ambiguous outcome

✅ GOOD PATTERN: "Analyze the current codebase for security vulnerabilities, focusing on SQL injection, XSS, and authentication flaws. Provide a ranked list with severity levels and remediation steps."
   - Clear purpose (analyze security)
   - Specific scope (SQL injection, XSS, auth flaws)
   - Explicit output (ranked list with severity + remediation)
```

### 2. Modularity and Reusability

- One clear responsibility per agent/command
- Avoid monolithic agents
- Design for chaining/composition
- Extract reusable components

### 3. Context Awareness

- Reference project docs/conventions
- Use relative paths
- Integrate version control info
- Track development phase

### 4. Guardrails and Constraints

- Define should/should-not behaviors
- Specify tech constraints
- Include quality gates
- Set error handling expectations

### 5. Cross-Platform Compatibility

- Support Windows/macOS/Linux
- Provide .sh + .ps1 OR Python/Node.js
- Test on all platforms
- Document platform requirements
- Naming: `script-name.sh`, `script-name.ps1`, `script-name.py`

---

## File Format Standards

```python
# FORMAT SELECTION ALGORITHM
def select_file_format(agent_name: str) -> str:
    """
    Deterministic format selection based on agent type.
    
    Returns: 'toml' or 'markdown'
    """
    TOML_AGENTS = {'gemini-cli', 'qwen-code'}
    
    if agent_name.lower() in TOML_AGENTS:
        return 'toml'
    else:
        return 'markdown'
```

> **DECISION TREE: CHOOSING THE RIGHT FORMAT**
>
> ```
> Is target agent Gemini CLI or Qwen Code?
>   YES → Use TOML format (.toml)
>   NO  → Use Markdown format (.md)
> ```

### Markdown Format (.md)

**USED BY** (17 agents):

- Claude Code
- Cursor
- GitHub Copilot
- Windsurf
- IBM Bob
- Jules
- Kilo Code
- Roo Code
- Amazon Q Developer CLI
- Amp
- Auggie CLI
- CodeBuddy CLI
- Codex CLI
- opencode
- Qoder CLI
- SHAI
- Google Antigravity

#### Standard Markdown Structure

```yaml
# MARKDOWN TEMPLATE SCHEMA
file_extension: .md
format_version: 1.0

required_sections:
  frontmatter:
    format: YAML
    delimiter: "---"
    required_fields:
      - description: string (1-200 chars)
    optional_fields:
      - mode: string (for GitHub Copilot)
      - glob: string (for Cursor)
      - category: string (for Google Antigravity)
      - deprecated: boolean
      - replacement: string
  
  body:
    format: MARKDOWN
    recommended_sections:
      - "# Command Name" (H1, required)
      - "## Purpose" (H2, recommended)
      - "## Context" (H2, optional)
      - "## Instructions" (H2, required)
      - "## Output Format" (H2, required)
      - "## Examples" (H2, recommended)

argument_patterns:
  markdown_based: "$ARGUMENTS"
  script_placeholder: "{SCRIPT}"
  agent_placeholder: "__AGENT__"
```

**TEMPLATE**:

```markdown
---
description: "Clear, concise description of what this command does"
---

# Command Name

## Purpose
[Explain the command's purpose and when to use it]

## Context
[Describe what context/information is needed]

## Instructions
[Step-by-step instructions for the AI agent]

## Output Format
[Specify the expected output structure]

## Examples
[Provide concrete examples if applicable]
```

#### GitHub Copilot Chat Mode Structure

GitHub Copilot uses a specialized structure with the `mode` field:

```markdown
---
description: "Description of the command"
mode: project-name.command-name
---

[Command instructions and content]
```

**Key Differences:**

- The `mode` field enables slash command functionality
- Format: `{project-identifier}.{command-name}`
- Example: `vinh.specify`, `vinh.implement`

### TOML Format (.toml)

**Used by:** Gemini CLI, Qwen Code

#### TOML Structure

```toml
description = "Clear, concise description of what this command does"

prompt = """
# Command Name

## Purpose
[Explain the command's purpose]

## Instructions
[Detailed instructions for the AI agent]

## Output Format
[Expected output structure]

Use {{args}} for user-provided arguments.
Reference {SCRIPT} for script paths.
"""
```

**Key Points:**

- Use triple quotes (`"""`) for multi-line prompts
- Arguments use double curly braces: `{{args}}`
- Maintain proper TOML syntax for special characters
- Keep descriptions concise (single line preferred)

---

## Agent-Specific Guidelines

### Claude Code (.claude/commands/)

**Best Practices:**

- Use conversational but precise language
- Break complex tasks into clear steps
- Leverage Claude's strong reasoning capabilities
- Include rationale for decisions when appropriate

**COMPLETE EXAMPLE**:

```markdown
---
description: "Design a technical implementation plan with architecture decisions"
---

# Technical Planning

## PURPOSE
You are tasked with creating a detailed technical implementation plan.

## PROCESS

### PHASE 1: Analysis
1. Review the specification in `specs/FEATURE.md`
2. Analyze existing codebase patterns and conventions
3. Identify integration points and dependencies

### PHASE 2: Planning
1. Propose architecture that aligns with project principles
2. Break down into implementable components
3. Identify potential risks and mitigation strategies

## OUTPUT SPECIFICATION
Create a detailed plan in `plans/FEATURE-plan.md` with:
- Architecture overview with diagrams
- Technology stack justification
- Component breakdown
- Testing strategy
- Implementation timeline

## INPUT
Arguments: $ARGUMENTS
```

**KEY ELEMENTS**:

- Clear phases with explicit numbering
- Output specification section
- Input section showing argument usage

### GitHub Copilot (.github/agents/ + .github/prompts/)

**Structure:**

- `.github/agents/` - Custom agents (`.agent.md`)
- `.github/prompts/` - Slash commands (`.prompt.md`)

**Pattern:**

```markdown
---
description: "Command description"
mode: project.command-name
---

[Role/expertise definition]

[Step-by-step instructions]

Arguments: $ARGUMENTS
```

### Cursor (.cursor/commands/ or .cursor/rules/)

**Pattern:**

```markdown
---
description: "Rule description"
glob: "**/*.ext"  # Optional
---

[Rules/constraints organized by category]
```

### Windsurf (.windsurf/workflows/ or .windsurf/rules/)

**Pattern:**

```markdown
---
description: "Workflow description"
---

## Phase 1: [Name]
[Steps...]

## Phase 2: [Name]
[Steps...]

[Continue with phases...]
```

### Gemini CLI & Qwen Code (.gemini/commands/, .qwen/commands/)

**TOML Pattern:**

```toml
description = "Command description"

prompt = """
# Command Name

[Instructions...]

Arguments: {{args}}
"""
```

### Other Agents

**Amazon Q Developer CLI** (.amazonq/prompts/): Standard Markdown, no custom arguments
**Kilo Code/Roo Code/IBM Bob** (.kilocode/rules/, .roo/rules/, .bob/commands/): IDE-integrated rules
**Google Antigravity** (.agent/rules/ or .agent/skills/): Rules (constraints) vs Skills (capabilities)

---

## Command Structure Best Practices

### 1. Command Naming Conventions

```yaml
# NAMING VALIDATION RULES
file_naming:
  pattern: ^[a-z]+(-[a-z]+)*\.md$
  examples:
    valid:
      - specify.md
      - implement.md
      - analyze-coverage.md
    invalid:
      - do-stuff.md  # too vague
      - Specify.md   # uppercase not allowed
      - specify_.md  # underscore not allowed

command_mode_naming:
  pattern: ^[a-z][a-z0-9]*\.[a-z]+(-[a-z]+)*$
  examples:
    valid:
      - vinh.specify
      - project.analyze-coverage
    invalid:
      - Rainbow.specify  # uppercase not allowed
      - vinh..specify # double dot not allowed

description_validation:
  min_length: 10
  max_length: 80
  must_start_with: [verb]
  must_include: [action, outcome]
```

**Follow consistent naming patterns:**

- Use **descriptive verbs**: `analyze`, `implement`, `validate`, `design`
- Keep names **concise** but **meaningful**
- Use **lowercase with hyphens** for file names: `design-architecture.md`
- Use **dot notation** for command modes: `project.command-name`

**Examples:**

```
✅ specify.md          - Create specifications
✅ implement.md        - Execute implementation
✅ analyze-coverage.md - Analyze test coverage
❌ do-stuff.md         - Too vague
❌ thebigthing.md      - Not descriptive
```

### 2. Description Field

**Write clear, actionable descriptions:**

- **One sentence** that explains the command's purpose
- Start with an **action verb**
- Include the **expected outcome**
- Keep under **80 characters** when possible

**Examples:**

```markdown
✅ "Create a detailed specification from user requirements"
✅ "Analyze codebase for security vulnerabilities and generate report"
✅ "Break down implementation plan into actionable tasks"
❌ "This command does some analysis stuff"
❌ "Helper function for various things"
```

### 3. Argument Handling

**CRITICAL**: Different agents use different argument patterns. Use the correct syntax for your target agent.

**ARGUMENT SYNTAX BY AGENT TYPE**:

| Agent Type | Syntax | When to Use | Example |
|------------|--------|-------------|---------|
| **Markdown-based** | `$ARGUMENTS` | Claude, Copilot, Cursor, Windsurf, etc. (most agents) | `/command Create a user profile feature` |
| **TOML-based** | `{{args}}` | Gemini CLI, Qwen Code only | `command analyze --file=app.py` |
| **Script paths** | `{SCRIPT}` | Any agent referencing script files | Replaced with actual script path |
| **Agent name** | `__AGENT__` | When agent name needed in output | Replaced with agent name |

**Best Practices:**

- Always document what arguments are expected
- Provide examples of valid arguments
- Handle missing or invalid arguments gracefully
- Use descriptive variable names in examples

**Example:**

```markdown
## Usage
```

/speckit.specify [feature description]

```

## Arguments
- `feature description`: Natural language description of what you want to build
  
## Examples
```

/speckit.specify Create a user authentication system with email/password login
/speckit.specify Build a photo album organizer with drag-and-drop functionality

```
```

### 4. Multi-Step Commands

**For complex workflows, break down into phases:**

```markdown
# Complex Feature Implementation

## Phase 1: Analysis
[Analysis steps...]

## Phase 2: Planning
[Planning steps...]

## Phase 3: Execution
[Execution steps...]

## Phase 4: Validation
[Validation steps...]
```

**Key Principles:**

- Each phase should have **clear entry/exit criteria**
- Phases should be **sequential** with explicit dependencies
- Include **validation checkpoints** between phases
- Provide **rollback guidance** if a phase fails

---

## Prompt Engineering Principles

### 1. Chain-of-Thought Prompting

**Encourage step-by-step reasoning:**

```markdown
# Code Review

Perform a thorough code review by thinking through each aspect:

1. **First**, read the entire code to understand overall structure
2. **Then**, analyze each function for correctness
3. **Next**, check for security vulnerabilities
4. **After that**, evaluate performance implications
5. **Finally**, provide consolidated recommendations

Think out loud as you work through each step.
```

### 2. Few-Shot Learning

**Provide concrete examples:**

```markdown
# API Endpoint Design

Design RESTful API endpoints following these examples:

## Example 1: User Resource
```

GET    /api/v1/users           - List all users
GET    /api/v1/users/:id       - Get specific user
POST   /api/v1/users           - Create new user
PUT    /api/v1/users/:id       - Update user
DELETE /api/v1/users/:id       - Delete user

```

## Example 2: Nested Resource
```

GET    /api/v1/users/:id/posts       - List user's posts
POST   /api/v1/users/:id/posts       - Create post for user

```

Now design endpoints for: $ARGUMENTS
```

### 3. Role-Based Prompting

**Assign specific expertise roles:**

```markdown
---
description: "Security-focused code review"
---

You are a senior security engineer with 15 years of experience in application security.

Your expertise includes:
- OWASP Top 10 vulnerabilities
- Secure coding practices
- Cryptography and data protection
- Authentication and authorization

When reviewing code:
1. Think like an attacker trying to exploit vulnerabilities
2. Consider both technical and business impact
3. Provide specific, actionable remediation steps
4. Reference relevant security standards (OWASP, CWE)
```

### 4. Constraint-Based Prompting

**Define explicit constraints:**

```markdown
# Database Schema Design

Design a database schema with these constraints:

## Technical Constraints
- Must use PostgreSQL 14+
- Maximum table size: 100M rows
- Query response time: < 100ms for 95th percentile
- Support for full-text search

## Business Constraints
- Support multi-tenancy with data isolation
- Maintain audit trail for all changes
- Enable soft deletes for compliance
- Support for eventual consistency

## Security Constraints
- Encrypt PII data at rest
- Implement row-level security
- No sensitive data in logs
```

### 5. Output Format Specification

**Always specify the expected output format:**

```markdown
## Output Format

Create a JSON file `schema.json` with this structure:

```json
{
  "tables": [
    {
      "name": "table_name",
      "columns": [
        {
          "name": "column_name",
          "type": "data_type",
          "constraints": ["constraint1", "constraint2"]
        }
      ],
      "indexes": [
        {
          "name": "index_name",
          "columns": ["col1", "col2"],
          "unique": true
        }
      ]
    }
  ]
}
```

Include comments explaining design decisions.

```

---

## Cross-Platform Script Development

```yaml
# CROSS-PLATFORM REQUIREMENTS
platforms:
  required:
    - windows: PowerShell 5.1+
    - macos: Bash 4.0+
    - linux: Bash 4.0+

script_generation_strategy:
  option_1_preferred:
    approach: single_cross_platform_file
    languages: [python, nodejs]
    advantages:
      - single_file_maintenance
      - consistent_behavior
      - better_error_handling
    
  option_2_fallback:
    approach: platform_specific_scripts
    files:
      - script-name.sh  # Unix-like systems
      - script-name.ps1 # Windows
    requirement: BOTH files must be provided

validation_matrix:
  platforms:
    - name: windows_10_11
      shell: powershell
      version: ">=5.1"
      
    - name: macos
      shell: [bash, zsh]
      version: ">=4.0"
      
    - name: linux_ubuntu_debian
      shell: bash
      version: ">=4.0"
  
  test_scenarios:
    - path_handling: [forward_slash, backslash, relative]
    - special_chars: [spaces, hyphens, underscores]
    - error_handling: [missing_deps, invalid_input, permission_denied]
    - output_consistency: [format_match_across_platforms]
```

> **REQUIREMENT**: All scripts MUST work on Windows, macOS, and Linux.

**DECISION TREE: CHOOSING SCRIPT APPROACH**

```
Does script have complex logic or many dependencies?
├─ YES → Use Python or Node.js (RECOMMENDED)
│         • Single file for all platforms
│         • No platform-specific versions needed
│         • Better error handling
│
└─ NO  → Use shell scripts
          • Provide BOTH .sh and .ps1 versions
          • .sh for macOS/Linux (Bash)
          • .ps1 for Windows (PowerShell)
```

### 1. Writing Cross-Platform Scripts

**APPROACH 1 (RECOMMENDED)**: Use Python or Node.js for single cross-platform file

**APPROACH 2**: Provide both Bash (.sh) and PowerShell (.ps1) versions

#### Bash Script Example (macOS/Linux)

```bash
#!/usr/bin/env bash
# script-name.sh

set -euo pipefail  # Exit on error, undefined variables, pipe failures

# Check prerequisites
if ! command -v git &> /dev/null; then
    echo "Error: git is required but not installed."
    exit 1
fi

# Main script logic
echo "Processing..."
# Add your commands here
```

#### PowerShell Script Example (Windows)

```powershell
# script-name.ps1

$ErrorActionPreference = 'Stop'  # Exit on error

# Check prerequisites
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Error "Error: git is required but not installed."
    exit 1
}

# Main script logic
Write-Host "Processing..."
# Add your commands here
```

### 2. Cross-Platform Best Practices

**Path Handling:**

```bash
# Bash: Use forward slashes, convert if needed
PROJECT_DIR="$(pwd)"
OUTPUT_FILE="${PROJECT_DIR}/output/report.md"
```

```powershell
# PowerShell: Use Join-Path for cross-platform paths
$projectDir = Get-Location
$outputFile = Join-Path $projectDir "output/report.md"
```

**Environment Variables:**

```bash
# Bash
export MY_VAR="value"
echo "$MY_VAR"
```

```powershell
# PowerShell
$env:MY_VAR = "value"
Write-Host $env:MY_VAR
```

**File Operations:**

```bash
# Bash
mkdir -p "output/logs"
cp "source.txt" "dest.txt"
rm -rf "temp/"
```

```powershell
# PowerShell
New-Item -ItemType Directory -Force -Path "output/logs"
Copy-Item "source.txt" "dest.txt"
Remove-Item -Recurse -Force "temp/"
```

**Command Execution:**

```bash
# Bash
if some-command; then
    echo "Success"
else
    echo "Failed"
    exit 1
fi
```

```powershell
# PowerShell
try {
    some-command
    Write-Host "Success"
} catch {
    Write-Error "Failed: $_"
    exit 1
}
```

### 3. Platform Detection

**In Agent Commands:**

```markdown
## Platform-Specific Instructions

**For macOS/Linux (Bash):**
```bash
sh scripts/setup.sh
```

**For Windows (PowerShell):**

```powershell
powershell -ExecutionPolicy Bypass -File scripts/setup.ps1
```

```

**In Scripts (Auto-detect):**
```bash
#!/usr/bin/env bash
# Detect OS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macos"
elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    OS="windows"
fi
```

```powershell
# Detect OS in PowerShell
if ($IsWindows) {
    $OS = "windows"
} elseif ($IsMacOS) {
    $OS = "macos"
} elseif ($IsLinux) {
    $OS = "linux"
}
```

### 4. Using Cross-Platform Languages

**Python (Recommended for complex logic):**

```python
#!/usr/bin/env python3
# script-name.py - Works on all platforms

import os
import sys
import platform
from pathlib import Path

def main():
    # Cross-platform path handling
    project_dir = Path.cwd()
    output_file = project_dir / "output" / "report.md"
    
    # Platform detection
    system = platform.system()  # 'Windows', 'Darwin', 'Linux'
    
    print(f"Running on {system}")
    # Add your logic here

if __name__ == "__main__":
    main()
```

**Node.js (Alternative for JavaScript environments):**

```javascript
#!/usr/bin/env node
// script-name.js - Works on all platforms

const os = require('os');
const path = require('path');
const fs = require('fs');

function main() {
    // Cross-platform path handling
    const projectDir = process.cwd();
    const outputFile = path.join(projectDir, 'output', 'report.md');
    
    // Platform detection
    const platform = os.platform();  // 'win32', 'darwin', 'linux'
    
    console.log(`Running on ${platform}`);
    // Add your logic here
}

main();
```

### 5. Documentation Requirements

**Always document platform support:**

```markdown
## Prerequisites

### All Platforms
- Git 2.30+
- Text editor

### macOS/Linux
- Bash 4.0+
- `jq` for JSON processing

### Windows
- PowerShell 5.1+ or PowerShell Core 7.0+
- Windows Terminal (recommended)

## Running Scripts

**macOS/Linux:**
```bash
chmod +x scripts/process.sh
./scripts/process.sh
```

**Windows:**

```powershell
powershell -ExecutionPolicy Bypass -File scripts/process.ps1
```

**Cross-Platform (Python):**

```bash
python3 scripts/process.py
```

```

---

## Testing and Validation

### 1. Agent Command Testing

**Test your agents before deployment:**

```bash
# Test with sample input
/speckit.specify Create a simple todo list application

# Test with edge cases
/speckit.specify   # Empty input
/speckit.specify "Very complex feature with multiple systems..." # Long input

# Test with different contexts
# - Empty repository
# - Existing codebase
# - Multiple branches
```

### 2. Validation Checklist

**MANDATORY CHECKS BEFORE COMMITTING**:

#### Content Quality

- [ ] **Description**: Clear, actionable, under 80 characters
  - PASS: "Create feature specification from user requirements"
  - FAIL: "Helps with stuff"

- [ ] **Instructions**: Unambiguous, step-by-step, actionable
  - PASS: "1. Read spec file 2. Extract requirements 3. Generate output"
  - FAIL: "Do the necessary work"

- [ ] **Output Format**: Explicitly specified with examples
  - PASS: "Create JSON file `output.json` with keys: name, version, description"
  - FAIL: "Create output file"

- [ ] **Examples**: Provided with concrete input/output pairs
  - PASS: Shows actual command usage with expected results
  - FAIL: No examples or vague descriptions

- [ ] **Arguments**: Fully documented with types and examples
  - PASS: "`feature-name` (string): Name of feature to analyze"
  - FAIL: "Takes arguments"

#### Technical Quality

- [ ] **Edge Cases**: Common failure scenarios documented
- [ ] **File Paths**: Use correct conventions for target platform
- [ ] **Syntax**: Valid Markdown/TOML (run linter)
- [ ] **Compatibility**: Tested with target agent(s)

#### Cross-Platform (if scripts included)

- [ ] **Platform Coverage**: Works on Windows, macOS, Linux
- [ ] **Documentation**: Platform-specific requirements listed

### 3. Integration Testing

**Test agent commands within workflows:**

```markdown
# Workflow Integration Test

Test the complete spec-driven workflow:

1. Run `/speckit.constitution` → Verify CONSTITUTION.md created
2. Run `/speckit.specify` → Verify spec file in specs/
3. Run `/speckit.plan` → Verify plan file in plans/
4. Run `/speckit.tasks` → Verify tasks file in tasks/
5. Run `/speckit.implement` → Verify implementation matches spec

Check that:
- Files are created in correct locations
- Content follows expected format
- References between files are valid
- Workflow can be executed multiple times
```

---

## Maintenance and Updates

### 1. Version Control

**Manage agent commands with version control:**

```bash
# Commit structure
.claude/commands/
  specify.md          # v1.0.0
  implement.md        # v1.2.0
  README.md           # Changelog

# Git commit messages
git commit -m "feat(commands): add security analysis command"
git commit -m "fix(specify): correct output path formatting"
git commit -m "docs(commands): update examples for implement command"
```

### 2. Documentation

**Maintain comprehensive documentation:**

```markdown
# Commands README

## Available Commands

| Command | Version | Description | Status |
|---------|---------|-------------|--------|
| `/speckit.specify` | 1.0.0 | Create feature specification | Stable |
| `/speckit.implement` | 1.2.0 | Execute implementation tasks | Stable |
| `/speckit.analyze` | 0.9.0 | Analyze coverage | Beta |

## Changelog

### v1.2.0 (2026-01-15)
- Added support for multi-file specifications
- Improved error handling for missing context
- Fixed argument parsing for complex inputs

### v1.1.0 (2026-01-01)
- Added examples section to all commands
- Standardized output format across commands
```

### 3. Deprecation Strategy

**Handle breaking changes gracefully:**

```markdown
---
description: "[DEPRECATED] Old implementation command - use /speckit.implement instead"
deprecated: true
replacement: "/speckit.implement"
---

# ⚠️ DEPRECATED

This command is deprecated and will be removed in v2.0.0.

Please use `/speckit.implement` instead.

## Migration Guide

Old usage:
```

/old-implement "feature description"

```

New usage:
```

/speckit.implement

```

The new command automatically detects the feature context.
```

### 4. Monitoring and Feedback

**Collect feedback and improve:**

- Track command usage frequency
- Monitor error rates and failure patterns
- Collect user feedback on command effectiveness
- Analyze common workflow patterns
- Identify opportunities for new commands
- Refine existing commands based on real usage

**Feedback Template:**

```markdown
# Command Feedback

## Command: /speckit.specify

### What Worked Well
- Clear output format
- Good examples
- Handled edge cases

### What Needs Improvement
- Could be more specific about required context
- Output file naming could be more flexible
- Examples could cover more scenarios

### Suggestions
- Add support for templates
- Allow customization of output location
- Provide validation before writing files
```

---

## Best Practices Summary

### DO ✅

- Write clear, specific, actionable instructions
- Provide concrete examples
- Specify expected output formats
- Use consistent naming conventions
- Test commands thoroughly before deployment
- Document arguments and usage
- Handle edge cases and errors gracefully
- Version control your agent commands
- Maintain compatibility with target agents
- Update documentation when changing commands
- **Provide both Bash and PowerShell script versions**
- **Test scripts on Windows, macOS, and Linux**
- **Document platform-specific requirements clearly**
- **Use cross-platform tools and languages when possible**

### DON'T ❌

- Use vague or ambiguous language
- Create monolithic commands that do too much
- Skip validation and testing
- Forget to document expected inputs/outputs
- Ignore error handling
- Use inconsistent naming or formatting
- Deploy untested commands to production
- Break backward compatibility without notice
- Copy commands without adapting to your context
- Overcomplicate simple commands

---

## Quick Reference

> **PURPOSE**: Use this section for rapid lookup when creating agent commands.
> **USAGE**: Find your target agent, then use the specified directory and file type.

### Agent Directory Structure

**HOW TO READ THIS TABLE**:

1. Find your target agent in the "Agent" column
2. Note the "Directory" where files should be placed
3. Use the correct "File Type" extension
4. Check if "CLI Required" (Yes = needs CLI tool installed, No = IDE-based)

| Agent | Directory | File Type | CLI Required |
|-------|-----------|-----------|--------------|
| **Claude Code** | `.claude/commands/` | `.md` | Yes (`claude`) |
| **GitHub Copilot** | `.github/agents/` + `.github/prompts/` | `.agent.md` / `.prompt.md` | No (IDE-based) |
| **Cursor** | `.cursor/commands/` | `.md` / `.mdc` | No (IDE-based) |
| **Windsurf** | `.windsurf/workflows/` | `.md` | No (IDE-based) |
| **Gemini CLI** | `.gemini/commands/` | `.toml` | Yes (`gemini`) |
| **Qwen Code** | `.qwen/commands/` | `.toml` | Yes (`qwen`) |
| **Amazon Q CLI** | `.amazonq/prompts/` | `.md` | Yes (`q`) |
| **Kilo Code** | `.kilocode/rules/` | `.md` | No (IDE-based) |
| **Roo Code** | `.roo/rules/` | `.md` | No (IDE-based) |
| **Qoder CLI** | `.qoder/commands/` | `.md` | Yes (`qoder`) |
| **Auggie CLI** | `.augment/rules/` | `.md` | Yes (`auggie`) |
| **CodeBuddy CLI** | `.codebuddy/commands/` | `.md` | Yes (`codebuddy`) |
| **Codex CLI** | `.codex/commands/` | `.md` | Yes (`codex`) |
| **SHAI** | `.shai/commands/` | `.md` | Yes (`shai`) |
| **opencode** | `.opencode/command/` | `.md` | Yes (`opencode`) |
| **Amp** | `.agents/commands/` | `.md` | Yes (`amp`) |
| **IBM Bob** | `.bob/commands/` | `.md` | No (IDE-based) |
| **Jules** | `.agent/` | `.md` | No (IDE-based) |
| **Google Antigravity** | `.agent/rules/` or `.agent/skills/` | `.md` | No (IDE-based) |

### Argument Syntax Reference

| Format | Syntax | Example |
|--------|--------|---------|
| Markdown (Standard) | `$ARGUMENTS` | `Create feature: $ARGUMENTS` |
| TOML | `{{args}}` | `Analyze file: {{args}}` |
| Script Path | `{SCRIPT}` | `Run script: {SCRIPT}` |
| Agent Placeholder | `__AGENT__` | `Using agent: __AGENT__` |

---

## Additional Resources

- [Spec-Driven Development Methodology](https://github.com/github/spec-kit/blob/main/spec-driven.md)
- [Anthropic Prompt Engineering Guide](https://docs.anthropic.com/claude/docs/prompt-engineering)
- [GitHub Copilot Documentation](https://docs.github.com/en/copilot)
- [Cursor Documentation](https://cursor.com/docs)
- [Windsurf Documentation](https://docs.windsurf.com/)

---

*This document should be updated as new agents are added and best practices evolve.*
