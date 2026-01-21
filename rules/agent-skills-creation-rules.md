# Agent Skill Creation Rules and Best Practices

```yaml
# DOCUMENT METADATA FOR LLM PROCESSING
document_type: technical_specification
format_version: 1.0
specification_standard: agent_skills_v1
primary_audience: llm_agents
processing_mode: strict_parsing
validation_required: true
language: en-US
last_updated: 2026-01-21
```

## LLM PROCESSING INSTRUCTIONS

**PARSING STRATEGY**:

1. Load entire specification into context
2. Parse SKILL.md frontmatter as strict YAML
3. Validate all required fields (name, description)
4. Apply progressive disclosure: metadata → instructions → resources
5. Extract file paths and validate references
6. Parse description field for keyword extraction
7. Validate directory structure against specification
8. Check cross-platform script compatibility

**VALIDATION SEQUENCE**:

```python
def validate_skill(skill_path: Path) -> ValidationResult:
    results = []
    
    # 1. Structure validation
    results.append(validate_directory_structure(skill_path))
    
    # 2. SKILL.md validation
    skill_md = skill_path / "SKILL.md"
    results.append(validate_frontmatter(skill_md))
    results.append(validate_name_format(skill_md))
    results.append(validate_description(skill_md))
    
    # 3. Script validation (if present)
    if (skill_path / "scripts").exists():
        results.append(validate_cross_platform_scripts(skill_path / "scripts"))
    
    # 4. Reference validation
    results.append(validate_file_references(skill_md))
    
    return aggregate_results(results)
```

---

> **DOCUMENT PURPOSE**: This document provides comprehensive guidelines for creating agent skills following the Agent Skills specification. It is designed to be read by both humans and LLMs.
>
> **WHAT ARE AGENT SKILLS**: Folders of instructions, scripts, and resources that agents can discover and use to perform specialized tasks more accurately and efficiently.
>
> **KEY DIFFERENCE FROM AGENT COMMANDS**:
>
> - **Agent Commands**: Platform-specific slash commands (e.g., `/speckit.specify`)
> - **Agent Skills**: Portable, self-contained capability packages that work across multiple agent platforms
>
> **TARGET AUDIENCE**: AI agents, developers, and teams creating reusable skills for AI-assisted development.

## Core Principles

### 1. Progressive Disclosure

```yaml
# PROGRESSIVE DISCLOSURE SCHEMA
loading_strategy:
  tier_1_metadata:
    trigger: agent_startup
    scope: ALL_SKILLS
    content:
      - name
      - description
    size_limit: 100_tokens_per_skill
    purpose: skill_discovery_and_selection
    
  tier_2_instructions:
    trigger: skill_activation
    scope: SELECTED_SKILL_ONLY
    content:
      - full_SKILL_md_body
    size_limit: 5000_tokens_recommended
    purpose: detailed_execution_instructions
    
  tier_3_resources:
    trigger: explicit_reference
    scope: ON_DEMAND
    content:
      - scripts/*
      - references/*
      - assets/*
    size_limit: variable
    purpose: specialized_tools_and_data

token_calculation:
  formula: |
    total_tokens = metadata_tokens + instructions_tokens + resources_tokens
    
  example_without_progressive_disclosure:
    skills_count: 200
    tokens_per_skill: 5000
    total: 1000000  # CONTEXT OVERFLOW
    
  example_with_progressive_disclosure:
    skills_count: 200
    metadata_tokens_per_skill: 100
    discovery_total: 20000  # EFFICIENT
    selected_skill_instructions: 5000
    optimal_total: 25000  # OPTIMAL
```

**PRINCIPLE**: Load only what's needed, when it's needed.

**THREE-TIER LOADING STRATEGY**:

```
TIER 1: METADATA (Always Loaded)
├─ Size: ~100 tokens per skill
├─ Content: name + description fields only
├─ When: Agent startup (ALL skills)
└─ Purpose: Skill discovery and selection

TIER 2: INSTRUCTIONS (Load on Activation)
├─ Size: <5000 tokens recommended
├─ Content: Full SKILL.md body
├─ When: Skill activated based on task match
└─ Purpose: Detailed execution instructions

TIER 3: RESOURCES (Load on Demand)
├─ Size: Variable
├─ Content: scripts/, references/, assets/
├─ When: Explicitly required by instructions
└─ Purpose: Specialized tools and reference data
```

**WHY THIS MATTERS FOR LLMs**:

- **Context Window**: Limited token budget for all inputs
- **Performance**: Loading all skills fully = context overflow
- **Efficiency**: Metadata scanning is fast, full loading is expensive
- **Accuracy**: Focused context improves task execution

**CONCRETE EXAMPLE**:

```
Agent has 200 skills available:
- Without progressive disclosure: 200 × 5000 = 1,000,000 tokens loaded (OVERFLOW)
- With progressive disclosure: 200 × 100 = 20,000 tokens for discovery (EFFICIENT)
- Then load only 1 skill's full content: 20,000 + 5,000 = 25,000 tokens (OPTIMAL)
```

**RULE**: Keep `SKILL.md` under 500 lines (~5000 tokens). Move details to `references/`.

### 2. Self-Contained Design

- Explicit dependencies
- Portable across implementations
- Version-controlled and auditable
- Independently testable
- Simple deployment

### 3. Clear Communication

- Explicit over implicit
- Actionable error messages
- Document edge cases
- Consistent terminology
- Concrete examples with input/output

## Directory Structure

### Required Structure

```
skill-name/
└── SKILL.md          # Required: Core skill definition
```

### Full Structure with Optional Directories

```
skill-name/
├── SKILL.md              # Required: Core skill definition
├── scripts/              # Optional: Executable code
│   ├── extract.py
│   └── process.sh
├── references/           # Optional: Additional documentation
│   ├── REFERENCE.md
│   ├── FORMS.md
│   └── domain-specific.md
└── assets/               # Optional: Static resources
    ├── templates/
    ├── images/
    └── data/
```

## SKILL.md Format

```markdown
---
name: skill-name  # Required: 1-64 chars, lowercase, hyphens only
description: "[Actions]. Use when [scenarios] or when user mentions [keywords]."  # Required: 1-1024 chars
license: Apache-2.0  # Optional
compatibility: Requires...  # Optional
metadata:  # Optional
  author: org
  version: "1.0"
allowed-tools: Bash(git:*) Read  # Optional, experimental
---

# Skill Name

## When to Use
- [Scenario 1]
- [Scenario 2]

## Prerequisites
[Required tools/packages]

## Instructions
### Step 1: [Action]
[Steps...]

## Examples
**Input:** [example]
**Output:** [example]

## Edge Cases
- **Case**: [handling]

## Error Handling
- **Error**: [action]

## Scripts
```bash
scripts/script.py [args]
```

```

### Frontmatter Fields

#### Required Fields

##### `name`
- **Required**: Yes
- **Format**: 1-64 characters
- **Rules**:
  - Only lowercase letters (a-z), numbers (0-9), and hyphens (-)
  - Must not start or end with hyphen
  - No consecutive hyphens (--)
  - Must match parent directory name

**Valid Examples:**
```yaml
name: pdf-processing
name: data-analysis
name: code-review
```

**Invalid Examples:**

```yaml
name: PDF-Processing      # uppercase not allowed
name: -pdf                # cannot start with hyphen
name: pdf--processing     # consecutive hyphens not allowed
name: pdf_processing      # underscores not allowed
```

### `description`

- **Required**: Yes
- **Format**: 1-1024 characters
- **Purpose**: Helps agents decide when to activate the skill
- **Should Include**:
  - What the skill does
  - When to use it
  - Specific keywords for task identification

**Good Example:**

```yaml
description: Extracts text and tables from PDF files, fills PDF forms, and merges multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction.
```

**Poor Example:**

```yaml
description: Helps with PDFs.  # Too vague, missing use cases and keywords
```

#### Optional Fields

##### `license`

- **Required**: No
- **Format**: Short license name or reference
- **Purpose**: Specify licensing terms

**Example:**

```yaml
license: Apache-2.0
license: Proprietary. LICENSE.txt has complete terms
```

##### `compatibility`

- **Required**: No
- **Format**: 1-500 characters
- **Purpose**: Indicate specific environment requirements
- **Include only if**: Skill has specific requirements

**Examples:**

```yaml
compatibility: Designed for Claude Code (or similar products)
compatibility: Requires git, docker, jq, and access to the internet
```

**Note**: Most skills do not need this field.

##### `metadata`

- **Required**: No
- **Format**: Key-value map (string → string)
- **Purpose**: Store additional properties not in the spec

**Example:**

```yaml
metadata:
  author: example-org
  version: "1.0"
  category: document-processing
  last-updated: "2026-01-20"
```

**Best Practice**: Use reasonably unique key names to avoid conflicts.

##### `allowed-tools`

- **Required**: No
- **Format**: Space-delimited list of pre-approved tools
- **Status**: **Experimental** (support varies by implementation)
- **Purpose**: Pre-approve specific tools the skill may use without additional confirmation

**Example:**

```yaml
allowed-tools: Bash(git:*) Bash(jq:*) Read
```

**Note**: This is an experimental feature. Check with your target agent implementation to see if this field is supported before relying on it. Many agents may ignore this field.

### Body Content

The Markdown body after frontmatter contains skill instructions. There are no strict format restrictions, but follow these guidelines:

#### Recommended Sections

1. **Overview**: Clear explanation of what the skill does
2. **When to Use**: Specific scenarios for skill activation
3. **Prerequisites**: Required tools, packages, or setup
4. **Instructions**: Step-by-step guidance
5. **Examples**: Input/output samples showing usage
6. **Edge Cases**: Common pitfalls and handling
7. **Guidelines**: Specific rules to follow
8. **Error Handling**: How to handle failures
9. **Additional Resources**: References to other files

#### Best Practices for Body Content

- Keep main SKILL.md under 500 lines
- Use clear, imperative language
- Provide concrete examples
- Document assumptions
- Include error handling guidance
- Reference external files for detailed content
- Use bullet points and numbered lists
- Add code blocks for examples
- Structure logically with headers

## Optional Directory Guidelines

### scripts/

Contains executable code that agents can run.

**Cross-Platform Requirements:**

- **Provide both Bash (.sh) and PowerShell (.ps1) versions** for shell scripts
- **Or use cross-platform languages** (Python, Node.js, Ruby) to avoid duplication
- **Test on Windows, macOS, and Linux** before deploying
- **Document platform-specific dependencies** clearly

**Guidelines:**

- Be self-contained or clearly document dependencies
- Include helpful error messages
- Handle edge cases gracefully
- Use cross-platform languages (Python, Node.js) or provide shell-specific versions
- Add usage comments at the top
- Make scripts executable on Unix-like systems (`chmod +x script.sh`)
- Use meaningful filenames with appropriate extensions (.sh, .ps1, .py, .js)

**Naming Conventions:**

- Bash scripts: `script-name.sh`
- PowerShell scripts: `script-name.ps1`
- Python scripts: `script-name.py` (preferred for cross-platform)
- Node.js scripts: `script-name.js`

**Example: Cross-Platform Python Script (Recommended):**

```python
#!/usr/bin/env python3
"""
PDF text extraction script (cross-platform).

Usage:
    python extract.py <input.pdf> [--output output.txt]

Platforms:
    - Windows
    - macOS
    - Linux

Requirements:
    - Python 3.8+
    - pypdf2 >= 3.0.0
"""

import sys
import argparse
from pathlib import Path
# ... rest of script
```

**Example: Bash Script (macOS/Linux):**

```bash
#!/usr/bin/env bash
# extract.sh - Extract text from PDFs (Unix-like systems)

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <input.pdf> [output.txt]"
    exit 1
fi

# Script logic here
```

**Example: PowerShell Script (Windows):**

```powershell
# extract.ps1 - Extract text from PDFs (Windows)

[CmdletBinding()]
param(
    [Parameter(Mandatory=$true)]
    [string]$InputPdf,
    
    [Parameter(Mandatory=$false)]
    [string]$OutputTxt
)

$ErrorActionPreference = 'Stop'

# Script logic here
```

### Optional Directories

```yaml
scripts/:
  purpose: executable_code
  formats: [.py, .sh, .ps1, .js]
  requirements:
    - self_contained_or_documented_deps
    - cross_platform_compatibility
    - error_messages
    - usage_comments
    
references/:
  purpose: additional_documentation
  files: [REFERENCE.md, FORMS.md, domain-specific.md]
  guidelines:
    - focused_topics
    - smaller_files_preferred
    - shallow_nesting
    
assets/:
  purpose: static_resources
  types: [templates, images, data]
  formats: [PNG, JSON, CSV]
```

## File References

```yaml
path_rules:
  format: relative_from_skill_root
  separator: forward_slash
  depth: maximum_one_level
  
anti_pattern:
  description: reference_chains
  example: "SKILL.md → index.md → section.md → subsection.md"
  problem: multiple_round_trips_waste_context
```

**Good structure:**

```
SKILL.md → references/REFERENCE.md ✓
```

**Poor structure:**

```
SKILL.md → references/index.md → references/details/section1.md → references/details/subsection.md ✗
```

## Writing Effective Instructions

### For LLM Comprehension

1. **Be Explicit**: Don't rely on implicit understanding
   - ❌ "Process the document appropriately"
   - ✅ "Extract text from pages 1-10, then parse tables from page 11"

2. **Use Clear Structure**: Organize with headers and lists
   - Use numbered lists for sequential steps
   - Use bullet points for options or features
   - Use subheadings to break up long sections

3. **Provide Context**: Explain the "why" behind instructions
   - "Use this approach for PDFs larger than 10MB to avoid memory errors"

4. **Include Concrete Examples**: Show expected inputs and outputs

   ```markdown
   **Input:** invoice_2024.pdf
   **Output:** 
   {
     "invoice_number": "INV-2024-001",
     "total": 1234.56,
     "date": "2024-01-15"
   }
   ```

5. **Document Edge Cases**: Address unusual scenarios
   - "If the PDF is password-protected, prompt user for password"
   - "For scanned PDFs without text layer, use OCR"

6. **Use Consistent Terminology**: Define and reuse specific terms
   - Define technical terms on first use
   - Use the same term throughout (don't alternate synonyms)

### Writing Style

- **Active voice**: "Extract the text" not "Text should be extracted"
- **Imperative mood**: "Run the script" not "You should run the script"
- **Present tense**: "The script processes files" not "The script will process"
- **Specific verbs**: "Parse" not "handle", "Extract" not "get"
- **Short sentences**: Keep sentences under 25 words when possible
- **Simple language**: Avoid jargon unless necessary for domain
- **Concrete over abstract**: Use specific examples rather than general statements

### Writing for LLMs Specifically

LLMs process instructions differently than humans. Optimize for machine comprehension:

1. **Explicit over implicit**: Don't rely on common sense or context
   - ❌ "Handle the file appropriately"
   - ✅ "If the file size exceeds 10MB, process in chunks of 1MB each"

2. **Structured format**: Use consistent patterns
   - Numbered lists for sequential steps
   - Bullet points for options or features
   - Headers for different sections
   - Code blocks for examples

3. **Complete information**: Don't assume knowledge
   - Define technical terms on first use
   - Specify units (MB not just "large", seconds not just "timeout")
   - Include error codes and meanings

4. **Concrete examples**: Show, don't just tell

   ```markdown
   **Input:** `invoice_2024-01-15.pdf`
   **Expected output:**
   ```json
   {
     "invoice_number": "INV-2024-001",
     "date": "2024-01-15",
     "total": 1234.56
   }
   ```

   ```

5. **Decision trees**: Make conditional logic explicit

   ```markdown
   If file extension is `.txt`:
     - Use UTF-8 encoding
     - Process line by line
   
   If file extension is `.csv`:
     - Detect delimiter (comma, semicolon, or tab)
     - Parse as structured data
   ```

6. **Error scenarios**: Document failure cases
   - What errors can occur
   - How to detect them
   - What action to take

### Organization Tips

1. **Logical flow**: Order steps sequentially
2. **Progressive detail**: Start general, then get specific
3. **Chunking**: Group related information
4. **Visual breaks**: Use whitespace and formatting
5. **Scannable**: Allow quick reading with headers and lists
6. **Reference structure**: Main instructions in SKILL.md, details in references/

## Skill Discovery

```yaml
activation_process:
  step_1_startup:
    action: load_metadata
    scope: all_skills
    fields: [name, description]
    tokens: ~100_per_skill
    
  step_2_task_received:
    action: search_descriptions
    method: keyword_matching
    
  step_3_selection:
    action: load_full_SKILL_md
    scope: selected_skill_only
    tokens: up_to_5000

critical_insight:
  description_field: ONLY_information_during_discovery
  impact: poor_description_equals_never_activated
```

## Description Guidelines

```yaml
# DESCRIPTION FIELD SPECIFICATION
field_name: description
requirement: MANDATORY
length_constraints:
  min: 1
  max: 1024
  recommended: 150-300

generation_algorithm:
  step_1_what_it_does:
    count: 2-4
    format: "[VERB] [OBJECT]"
    examples:
      - "Extracts text and tables from PDF files"
      - "Fills PDF forms"
      - "Merges multiple PDFs"
    
  step_2_when_to_use:
    format: "Use when [SCENARIO]"
    count: 2-3
    examples:
      - "Use when working with PDF documents"
      - "Use when extracting form data"
    
  step_3_keywords:
    format: "when user mentions [KEYWORD]"
    count: 3-5
    examples:
      - "PDFs"
      - "forms"
      - "document extraction"

template_structure: |
  [ACTION_1], [ACTION_2], and [ACTION_3]. 
  Use when [SCENARIO_1], [SCENARIO_2], 
  or when user mentions [KEYWORD_1], [KEYWORD_2], or [KEYWORD_3].

validation_rules:
  must_contain:
    - at_least_2_actions
    - at_least_1_use_case
    - at_least_3_keywords
  must_not_contain:
    - vague_terms: ["helps with", "tool for", "does stuff"]
    - pronouns: ["it", "this", "that"]
  
quality_score:
  excellent: |
    Contains 3+ actions, 2+ scenarios, 4+ keywords, 
    specific scope, clear triggers
  good: |
    Contains 2+ actions, 1+ scenario, 3+ keywords
  poor: |
    Missing actions, scenarios, or keywords
```

> **CRITICAL**: The `description` field is the ONLY information agents have during skill discovery.
> **IMPACT**: Poor description = skill never activated, even if perfect for the task.

### Description Formula

**REQUIRED COMPONENTS**:

```
[What it does] + [When to use it] + [Key terms for matching]
```

**STEP-BY-STEP CONSTRUCTION**:

1. **What it does** (Specific capabilities)
   - List 2-4 concrete actions
   - Use active verbs
   - Be specific, not generic

2. **When to use it** (Use cases)
   - Start with "Use when..."
   - Include 2-3 scenarios
   - Mention user phrases

3. **Key terms** (Matching keywords)
   - Technical terms users might say
   - Related concepts
   - Common variations

**TEMPLATE**:

```yaml
description: "[ACTION 1], [ACTION 2], and [ACTION 3]. Use when [SCENARIO 1], [SCENARIO 2], or when user mentions [KEYWORD 1], [KEYWORD 2], or [KEYWORD 3]."
```

### What Makes a Good Description

1. **Specific capabilities**: List concrete actions the skill can perform
2. **Use cases**: Describe when and why to use the skill
3. **Keywords**: Include terms users might mention that should trigger the skill
4. **Scope**: Be clear about what the skill covers and doesn't cover
5. **Length**: Use the full 1-1024 character limit when needed - more context helps matching

### Description Examples

```yaml
good_example:
  text: "Extracts text and tables from PDF files, fills PDF forms, and merges multiple PDFs. Use when working with PDF documents or when the user mentions PDFs, forms, or document extraction."
  strengths: [specific_capabilities, use_cases, keywords]

poor_examples:
  - "Helps with documents"  # too_vague
  - "PDF tool"  # no_capabilities_listed
  - "Use this for processing"  # no_specifics
```

## Validation

```yaml
# AUTOMATED VALIDATION SCHEMA
validation_tool: skills-ref
validation_commands:
  single_skill: skills-ref validate ./my-skill
  directory: skills-ref validate ./skills/

validation_checks:
  structure:
    - check: SKILL_md_exists
      path: SKILL.md
      required: true
      
    - check: frontmatter_valid_yaml
      parser: yaml_strict
      required: true
      
  required_fields:
    - field: name
      type: string
      pattern: ^[a-z0-9]+(-[a-z0-9]+)*$
      length: [1, 64]
      
    - field: description
      type: string
      length: [1, 1024]
      quality_checks:
        - contains_action_verbs
        - contains_use_cases
        - contains_keywords
        
  name_validation:
    allowed_chars: [a-z, 0-9, "-"]
    forbidden:
      - uppercase: [A-Z]
      - underscore: "_"
      - spaces: " "
      - leading_hyphen: ^-
      - trailing_hyphen: -$
      - consecutive_hyphens: --
    
  directory_match:
    rule: skill_name_must_equal_directory_name
    validation: name_field == parent_directory_name

error_types:
  critical:
    - missing_SKILL_md
    - invalid_frontmatter_yaml
    - missing_required_field
    - name_format_violation
    
  warning:
    - description_too_short
    - missing_examples
    - no_error_handling_documented
    
ci_cd_integration:
  pre_commit:
    - run: skills-ref validate $CHANGED_SKILL
      on_fail: block_commit
      
  pipeline:
    - stage: validate
      run: skills-ref validate ./skills/
      on_fail: fail_pipeline
```

Validation ensures your skill conforms to the Agent Skills specification and can be loaded by compatible agents. Use the [skills-ref](https://github.com/agentskills/agentskills/tree/main/skills-ref) reference library:

```bash
# Validate a single skill
skills-ref validate ./my-skill

# Validate all skills in a directory
skills-ref validate ./skills/
```

**Validation Checks:**

- SKILL.md frontmatter is valid YAML
- Required fields present (name, description)
- Field constraints met (length, format, character restrictions)
- Name matches directory
- Name format follows rules (lowercase, hyphens, no consecutive hyphens, no leading/trailing hyphens)
- No uppercase letters, underscores, or spaces in name
- File structure is correct

**Why validation matters**: Skills that fail validation may not load correctly in agent implementations, causing runtime errors or being ignored entirely. Always validate before deploying or sharing skills.

**Best practice**: Integrate validation into your development workflow:

- Run validation before committing changes
- Add validation to CI/CD pipelines
- Validate after any structural changes to the skill

## Cross-Platform Script Development

```yaml
required_platforms:
  - windows: [PowerShell, CMD]
  - macos: [Bash, Zsh]
  - linux: [Bash]

approach_1_preferred:
  language: [Python, Node.js]
  advantage: single_file_all_platforms
  
approach_2_fallback:
  files:
    - script-name.sh  # Unix
    - script-name.ps1 # Windows
  requirement: both_required
```

### Cross-Platform Script Examples

#### Python Script (Single File, All Platforms)

```python
#!/usr/bin/env python3
"""
File processor skill script (cross-platform).

This script works on Windows, macOS, and Linux.
"""

import os
import sys
import platform
from pathlib import Path

def main():
    # Cross-platform path handling
    current_dir = Path.cwd()
    output_dir = current_dir / "output"
    output_dir.mkdir(exist_ok=True)
    
    # Platform detection (if needed)
    system = platform.system()
    print(f"Running on {system}")
    
    # Your processing logic here
    print("Processing files...")
    
if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
```

#### Bash Script (macOS/Linux)

```bash
#!/usr/bin/env bash
# process-files.sh

set -euo pipefail

# Color output (optional)
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

echo "${GREEN}Processing files on $(uname -s)...${NC}"

# Create output directory
mkdir -p output

# Your processing logic here
for file in input/*.txt; do
    if [ -f "$file" ]; then
        echo "Processing: $file"
        # Process file
    fi
done

echo "${GREEN}Done!${NC}"
```

#### PowerShell Script (Windows)

```powershell
# process-files.ps1

[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

Write-Host "Processing files on Windows..." -ForegroundColor Green

# Create output directory
$outputDir = "output"
if (-not (Test-Path $outputDir)) {
    New-Item -ItemType Directory -Path $outputDir | Out-Null
}

# Your processing logic here
Get-ChildItem -Path "input" -Filter "*.txt" | ForEach-Object {
    Write-Host "Processing: $($_.Name)"
    # Process file
}

Write-Host "Done!" -ForegroundColor Green
```

### SKILL.md Documentation for Cross-Platform Scripts

**Example documentation:**

```markdown
## Scripts

This skill includes scripts for automated processing.

### Cross-Platform Script (Recommended)

Use the Python script for all platforms:

```bash
python3 scripts/process.py --input data.csv
```

**Requirements:**

- Python 3.8 or higher
- pandas library: `pip install pandas`

### Platform-Specific Scripts

Alternatively, use the shell-specific scripts:

**macOS/Linux (Bash):**

```bash
chmod +x scripts/process.sh
./scripts/process.sh --input data.csv
```

**Windows (PowerShell):**

```powershell
powershell -ExecutionPolicy Bypass -File scripts/process.ps1 -Input data.csv
```

```

### Testing Cross-Platform Scripts

```yaml
# CROSS-PLATFORM TEST MATRIX
test_execution_order:
  1. platform_testing
  2. robustness_testing
  3. output_validation

platform_testing:
  windows:
    os: ["Windows 10", "Windows 11"]
    shell: PowerShell
    version: ">=5.1"
    tests:
      - name: script_execution
        pass: exit_code == 0 AND no_errors
        fail: exit_code != 0 OR "command not found"
        
      - name: output_format
        pass: output_matches_expected_format
        fail: output_format_differs
        
  macos:
    os: macOS
    shell: [Bash, Zsh]
    version: ">=4.0"
    tests:
      - name: script_execution_bash
        pass: bash_script_runs AND exit_code == 0
        fail: permission_denied OR command_not_found
        
      - name: script_execution_zsh
        pass: zsh_script_runs AND exit_code == 0
        fail: syntax_error OR command_not_found
        
  linux:
    os: [Ubuntu, Debian]
    shell: Bash
    version: ">=4.0"
    tests:
      - name: script_execution
        pass: script_runs AND dependencies_resolved
        fail: missing_package OR permission_issue

robustness_testing:
  path_handling:
    test_cases:
      - input: "/absolute/path/to/file"
        expected: file_processed_successfully
        
      - input: "C:\\windows\\path\\to\\file"
        expected: file_processed_successfully
        
      - input: "./relative/path/to/file"
        expected: file_processed_successfully
        
    pass_criteria: all_path_formats_handled
    fail_criteria: path_separator_error OR file_not_found
    
  special_characters:
    test_cases:
      - filename: "my file.txt"
        expected: file_processed
        
      - filename: "file-name_v2.0.txt"
        expected: file_processed
        
    pass_criteria: all_files_processed
    fail_criteria: parsing_error OR file_not_found
    
  error_handling:
    test_cases:
      - scenario: missing_dependency
        expected: clear_error_message AND exit_code == 1
        
      - scenario: invalid_input
        expected: validation_error AND exit_code == 1
        
      - scenario: permission_denied
        expected: permission_error AND exit_code == 1
        
    pass_criteria: clear_errors AND correct_exit_codes
    fail_criteria: cryptic_errors OR wrong_exit_codes

automated_testing:
  docker_linux:
    command: |
      docker run -v $(pwd):/workspace -w /workspace python:3.11 \
        python3 scripts/process.py --input test-data.csv
    
  wsl_windows:
    command: |
      wsl bash scripts/process.sh --input test-data.csv
```

**MANDATORY TEST MATRIX**:

#### Platform Testing

- [ ] **Windows 10/11 + PowerShell 5.1+**
  - PASS: Script executes without errors
  - PASS: Output matches expected format
  - FAIL: "Command not found" or syntax errors

- [ ] **macOS + Bash/Zsh**
  - PASS: Script executes on both shells
  - PASS: Handles macOS-specific paths
  - FAIL: Permission errors or "command not found"

- [ ] **Linux (Ubuntu/Debian) + Bash**
  - PASS: Script executes without modifications
  - PASS: Dependencies resolved correctly
  - FAIL: Missing packages or permission issues

#### Robustness Testing

- [ ] **Path Handling**
  - TEST: `/path/to/file`, `C:\path\to\file`, `./relative/path`
  - PASS: All path formats handled correctly
  - FAIL: Path separator errors or "file not found"

- [ ] **Special Characters**
  - TEST: Files with spaces: `my file.txt`
  - TEST: Special chars: `file-name_v2.0.txt`
  - PASS: All files processed correctly
  - FAIL: Parsing errors or file not found

- [ ] **Error Handling**
  - TEST: Missing dependencies
  - TEST: Invalid input
  - TEST: Permission denied
  - PASS: Clear error messages, correct exit codes
  - FAIL: Cryptic errors or wrong exit codes

- [ ] **Output Consistency**
  - PASS: Same output format on all platforms
  - FAIL: Platform-specific output variations

**Testing with Docker (Linux environment):**

```bash
# Test Python script in isolated Linux environment
docker run -v $(pwd):/workspace -w /workspace python:3.11 \
    python3 scripts/process.py --input test-data.csv
```

**Testing on Windows (using WSL):**

```bash
# Test Bash script on Windows via WSL
wsl bash scripts/process.sh --input test-data.csv
```

### Common Cross-Platform Pitfalls

**1. Line Endings**

- Windows: CRLF (`\r\n`)
- Unix: LF (`\n`)
- **Solution**: Configure `.gitattributes`:

  ```
  *.sh text eol=lf
  *.ps1 text eol=crlf
  *.py text eol=lf
  ```

**2. Path Separators**

- Windows: Backslash (`\`)
- Unix: Forward slash (`/`)
- **Solution**: Use `pathlib` in Python or `Join-Path` in PowerShell

**3. Case Sensitivity**

- Windows: Case-insensitive file systems
- Unix: Case-sensitive file systems
- **Solution**: Always use consistent casing for file names

**4. Executable Permissions**

- Unix requires: `chmod +x script.sh`
- Windows: No explicit permission needed
- **Solution**: Document this requirement for Unix users

**5. Command Availability**

- Some commands differ between platforms (e.g., `ls` vs `dir`)
- **Solution**: Check for command existence before using, or use cross-platform alternatives

---

## Common Patterns and Examples

### Pattern 1: Analysis Skill

```markdown
---
name: code-security-analyzer
description: Analyzes code for security vulnerabilities, including SQL injection, XSS, and authentication issues. Use when reviewing code security, conducting security audits, or when user mentions vulnerabilities or security concerns.
---

# Code Security Analyzer

## When to Use
- Security code reviews
- Pre-deployment security checks
- Vulnerability assessments
- Security audit preparation

## Instructions

### 1. Scan for Common Vulnerabilities
Review code for:
- SQL injection vulnerabilities
- Cross-site scripting (XSS)
- Authentication/authorization flaws
- Hardcoded credentials

### 2. Analyze Dependencies
Check for:
- Known vulnerable dependencies
- Outdated packages
- License compliance issues

### 3. Generate Report
Create report including:
- Severity ratings (Critical/High/Medium/Low)
- Specific vulnerable code locations
- Remediation recommendations
```

### Pattern 2: Generation Skill

```markdown
---
name: api-documentation-generator
description: Generates API documentation from code with endpoints, parameters, and examples. Use when creating API docs, documenting REST APIs, or when user mentions API documentation or OpenAPI specifications.
---

# API Documentation Generator

## What It Does
Analyzes API code and generates comprehensive documentation including:
- Endpoint descriptions
- Request/response formats
- Authentication requirements
- Example requests and responses

## Instructions

### 1. Analyze API Structure
- Identify all endpoints (GET, POST, PUT, DELETE)
- Extract path parameters and query strings
- Document request body schemas

### 2. Generate Documentation
For each endpoint, create:
```markdown
### GET /api/users/{id}

**Description**: Retrieves user by ID

**Parameters:**
- `id` (path, required): User ID

**Response:** 200 OK
\```json
{"id": 123, "name": "John Doe"}
\```
```

### 3. Include Examples

Provide curl commands for testing

```

### Pattern 3: Workflow Skill

```markdown
---
name: deployment-workflow
description: Guides through complete deployment process including testing, building, and deploying applications. Use for application deployments, CI/CD workflows, or when user mentions deploy, release, or production deployment.
---

# Deployment Workflow

## Prerequisites
- Git repository with source code
- Access to deployment environment
- Required credentials configured

## Workflow Steps

### Phase 1: Pre-Deployment
1. Run automated tests: `scripts/run-tests.sh`
2. Check code coverage (minimum 80%)
3. Review pending pull requests
4. Verify all checks pass

### Phase 2: Build
1. Update version number in package.json
2. Build production bundle: `npm run build`
3. Run security scan on dependencies
4. Generate changelog

### Phase 3: Deploy
1. Create git tag: `git tag v1.0.0`
2. Push to remote: `git push origin v1.0.0`
3. Deploy to staging: `scripts/deploy.sh staging`
4. Run smoke tests on staging
5. Deploy to production: `scripts/deploy.sh production`

### Phase 4: Post-Deployment
1. Verify application health
2. Monitor error rates for 30 minutes
3. Update documentation
4. Notify team of deployment
```

## Integration Best Practices

### For Skill Authors

1. **Test thoroughly**: Verify skills work across different scenarios
2. **Version control**: Track changes and maintain changelog
3. **Document dependencies**: List all required tools and packages
4. **Keep updated**: Maintain skills as tools and practices evolve
5. **Gather feedback**: Learn from users and improve
6. **Follow conventions**: Adhere to the specification strictly
7. **Optimize size**: Keep SKILL.md concise, use references for details

### For Agent Implementations

1. **Respect progressive disclosure**: Load metadata first, then instructions
2. **Cache intelligently**: Avoid redundant file reads
3. **Handle errors gracefully**: Provide clear feedback on validation failures
4. **Support all required fields**: Implement full spec compliance
5. **Document extensions**: If adding custom features, document clearly
6. **Validate on load**: Check skills conform to specification
7. **Provide debugging**: Help users identify skill issues

### For Teams and Enterprises

1. **Centralize skills**: Maintain organization-wide skill repository
2. **Review process**: Establish skill review and approval workflow
3. **Share knowledge**: Document lessons learned
4. **Standardize**: Create organization-specific templates
5. **Train teams**: Educate on skill creation best practices
6. **Track usage**: Monitor which skills are most valuable
7. **Maintain quality**: Regular audits and updates

## Checklist for Creating a Skill

### Before You Start

- [ ] Identify the task or domain the skill addresses
- [ ] Verify no existing skill covers this capability
- [ ] Determine required tools and dependencies
- [ ] Plan the directory structure

### Creating SKILL.md

- [ ] Choose a valid, descriptive name (lowercase, hyphens only)
- [ ] Write comprehensive description (what, when, keywords)
- [ ] Add appropriate license
- [ ] Specify compatibility requirements (if any)
- [ ] Include metadata (author, version, etc.)
- [ ] Write clear, step-by-step instructions
- [ ] Provide concrete examples
- [ ] Document edge cases
- [ ] Add error handling guidance
- [ ] Keep under 500 lines (move details to references/)

### Optional Content

- [ ] Create scripts with clear documentation
- [ ] Add reference documentation for complex topics
- [ ] Include templates and assets as needed
- [ ] Ensure all file references are correct
- [ ] Test all scripts and verify they work

### Quality Checks

- [ ] Validate with skills-ref tool
- [ ] Test skill with target agent(s)
- [ ] Review for clarity and completeness
- [ ] Verify all links and references work
- [ ] Check for typos and formatting issues
- [ ] Ensure consistent terminology
- [ ] Test with various inputs and scenarios
- [ ] **Test scripts on Windows (PowerShell)**
- [ ] **Test scripts on macOS (Bash/Zsh)**
- [ ] **Test scripts on Linux (Bash)**
- [ ] **Verify cross-platform path handling**
- [ ] **Document platform-specific requirements**

### Finalization

- [ ] Add to version control
- [ ] Document in team/organization knowledge base
- [ ] Share with relevant team members
- [ ] Gather feedback and iterate
- [ ] Monitor usage and effectiveness

## Common Mistakes to Avoid

> **PURPOSE**: Learn from common anti-patterns to avoid skill creation failures.
> **FORMAT**: Each mistake shows ❌ WRONG way and ✅ CORRECT way.

### 1. Vague Descriptions

**WHY THIS FAILS**: Agent cannot determine when to activate skill.

**ANTI-PATTERN**:

```yaml
❌ description: "Helps with data"
```

**PROBLEMS**:

- Too generic (what kind of data?)
- No use cases (when to activate?)
- Missing keywords (how to match?)

**CORRECT PATTERN**:

```yaml
✅ description: "Transforms CSV files into JSON format with validation and error reporting. Use for data format conversions or when user mentions CSV, JSON, or data transformation."
```

**STRENGTHS**:

- Specific capabilities (CSV → JSON)
- Clear use case (format conversions)
- Keywords for matching (CSV, JSON, transformation)

### 2. Missing Use Cases

❌ Only describing what it does
✅ Describing what it does AND when to use it

### 3. Overly Long SKILL.md

❌ 1000+ lines in main file
✅ <500 lines in SKILL.md, detailed content in references/

### 4. Implicit Assumptions

❌ "Process the file normally"
✅ "Extract text from PDF, handling both text-based and scanned documents. For scanned PDFs, apply OCR using Tesseract."

### 5. Poor File Organization

❌ Flat structure with all files in root
✅ Organized with scripts/, references/, assets/

### 6. Undocumented Dependencies

❌ Script fails with "module not found"
✅ Clear prerequisites section listing all dependencies

### 7. Invalid Naming

❌ `My_Skill`, `my skill`, `MySkill`
✅ `my-skill`

### 8. No Examples

❌ Instructions without demonstrations
✅ Multiple examples showing different use cases

### 9. Ignoring Edge Cases

❌ Only happy path documented
✅ Edge cases and error handling included

### 10. Missing Validation

❌ Shipping without testing
✅ Validated with skills-ref and tested with agents

### 11. Platform-Specific Scripts Only

❌ Only providing Bash scripts without PowerShell alternative
✅ Using Python/Node.js for cross-platform compatibility, or providing both .sh and .ps1 versions

### 12. Hard-Coded Path Separators

❌ Using `\` or `/` directly in paths
✅ Using `pathlib.Path` (Python) or `Join-Path` (PowerShell) for cross-platform paths

### 13. Platform-Specific Commands

❌ Using `ls`, `grep`, `sed` without Windows alternatives
✅ Using cross-platform tools or providing platform-specific script versions

### 14. Untested on All Platforms

❌ Only testing on developer's primary OS
✅ Testing on Windows, macOS, and Linux before release

## Agent Skills Ecosystem

### Adoption

Agent Skills are supported by a growing ecosystem of AI development tools and platforms:

**Production-Ready Support:**

- **Claude Code** (Anthropic): Native support via plugin system
- **Claude.ai** (Anthropic): Built-in skills available for paid plans
- **Claude API** (Anthropic): Upload custom skills via API
- **Gemini CLI** (Google): Skills support
- **OpenCode**: Full skills compatibility
- **Cursor**: Skills support
- **VS Code**: Skills integration
- **OpenAI Codex**: Skills support
- **Letta**: Agent skills support
- **Goose** (Block): Skills integration

**Partner Skills:**

- **Notion**: [Notion Skills for Claude](https://www.notion.so/notiondevs/Notion-Skills-for-Claude-28da4445d27180c7af1df7d8615723d0)

### Open Development

The Agent Skills format is:

- **Open standard**: Originally developed by Anthropic, released as open specification
- **Community-driven**: Open to contributions from the broader ecosystem
- **Actively maintained**: Regular updates and improvements
- **Widely adopted**: Growing support across major AI platforms

**Contribute**: <https://github.com/agentskills/agentskills>

## Resources

### Official Resources

- **Specification**: <https://agentskills.io/specification>
- **Agent Skills Home**: <https://agentskills.io/>
- **What are skills?**: <https://agentskills.io/what-are-skills>
- **Integrate skills**: <https://agentskills.io/integrate-skills>
- **Anthropic's Skills Repository**: <https://github.com/anthropics/skills>
- **Reference Library (skills-ref)**: <https://github.com/agentskills/agentskills/tree/main/skills-ref>
- **Anthropic Support Articles**:
  - [What are skills?](https://support.claude.com/en/articles/12512176-what-are-skills)
  - [Using skills in Claude](https://support.claude.com/en/articles/12512180-using-skills-in-claude)
  - [How to create custom skills](https://support.claude.com/en/articles/12512198-creating-custom-skills)
- **Engineering Blog**: [Equipping agents for the real world with Agent Skills](https://anthropic.com/engineering/equipping-agents-for-the-real-world-with-agent-skills)

### Example Skills

- **Anthropic's Skills Repository**: <https://github.com/anthropics/skills>
  - **Creative & Design**: Art, music, mockup creation
  - **Development & Technical**: Testing web apps, MCP server generation, code analysis
  - **Enterprise & Communication**: Branding, communications workflows
  - **Document Skills**: PDF, DOCX, PPTX, XLSX (source-available, powers Claude's document capabilities)

**Note**: Many skills in the repository are open source (Apache 2.0). The document creation & editing skills are source-available (not open source) but provided as reference for complex production skills.

### Tools and Templates

- **skills-ref**: Reference library for validation and prompt XML generation
- **Skill Template**: Available at <https://github.com/anthropics/skills/tree/main/template>
- **Claude Code Plugin**: Install from marketplace with `/plugin marketplace add anthropics/skills`

## Using Skills in Different Platforms

### Claude Code

```bash
# Add the marketplace
/plugin marketplace add anthropics/skills

# Install skills
/plugin install document-skills@anthropic-agent-skills
/plugin install example-skills@anthropic-agent-skills

# Use a skill by mentioning it
"Use the PDF skill to extract form fields from path/to/file.pdf"
```

### Claude.ai

- All example skills are available to paid plans
- Upload custom skills via the UI (see [Using skills in Claude](https://support.claude.com/en/articles/12512180-using-skills-in-claude))

### Claude API

- Upload custom skills via API
- See [Skills API Quickstart](https://docs.claude.com/en/api/skills-guide#creating-a-skill)

### Other Compatible Agents

Refer to each platform's documentation for specific integration instructions.

## Version History

- **v1.0** (2026-01-20): Initial comprehensive guide based on Agent Skills specification

---

*This document is based on the Agent Skills specification (<https://agentskills.io>) and Anthropic's implementation reference (<https://github.com/anthropics/skills>). It is intended to be read by both human developers and LLMs to ensure consistent, high-quality skill creation.*
