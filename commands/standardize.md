---
description: Execute the standardization workflow to create comprehensive coding standards and conventions documentation for the entire product.
handoffs: 
  - label: Create Feature Plan
    agent: rainbow.design
    prompt: Create implementation plan for a specific feature following the established standards
    send: true
  - label: Review Architecture
    agent: rainbow.architect
    prompt: Review product architecture design
scripts:
  sh: scripts/bash/setup-standardize.sh --json
  ps: scripts/powershell/setup-standardize.ps1 -Json
agent_scripts:
  sh: scripts/bash/update-agent-context.sh __AGENT__
  ps: scripts/powershell/update-agent-context.ps1 -AgentType __AGENT__
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add coding standards and conventions') and commit upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for STANDARDS_DOC, DOCS_DIR, ARCH_DOC, CONSTITUTION. For single quotes in args like "I'm Groot", use escape syntax: e.g 'I'\''m Groot' (or double-quote if possible: "I'm Groot").

2. **Load context**: Read `memory/ground-rules.md`, `docs/architecture.md` (if exists), and all feature specifications from `specs/*/spec.md`. Load STANDARDS_DOC template (already copied to docs/). Incorporate the essential rules for maximizing source code readability, simplicity and long-term maintainability.

3. **Execute standardization workflow**: Follow the structure in STANDARDS_DOC template to:
   - Define UI naming conventions (MANDATORY for frontend projects)
   - Establish code naming conventions
   - Document file and directory structure standards
   - Define API design standards
   - Establish database naming conventions
   - Document testing standards
   - Define Git workflow and commit message conventions
   - Establish documentation standards
   - Provide a concise, high-signal standards guide for AI agents to ensure consistent, maintainable, and secure code. Exclude overly detailed information and specific examples

4. **Stop and report**: Command ends after standards document completion. Report STANDARDS_DOC path and generated artifacts.

## Phases

### Phase 0: Standards Analysis & Best Practices Research

1. **Analyze project context**:
   - Read architecture.md to understand technology stack
   - **Detect UI layer presence**: Check if project has frontend/UI components
     - Look for: React, Vue, Angular, HTML, CSS, mobile frameworks (React Native, Flutter, SwiftUI)
     - Check for: UI mockups, design specifications, frontend directories
     - Determine: Frontend project, backend-only, or full-stack
   - Read ground-rules.md for existing constraints
   - Read feature specs to identify naming patterns
   - Identify programming languages and frameworks in use

2. **Research best practices** for each technology:
   - UI naming conventions for the UI framework (React, Vue, Angular, etc.)
   - Code naming conventions for the backend language (Python, Java, TypeScript, etc.)
   - Database naming conventions for the database system
   - API design standards (REST, GraphQL, gRPC, etc.)
   - Testing conventions for the test frameworks

3. **Research domain-specific conventions**:
   - Web applications: Component naming, CSS class naming, accessibility attributes
   - Mobile applications: Screen naming, view controller naming
   - API services: Endpoint naming, HTTP methods, status codes
   - Desktop applications: Window naming, event handler naming

**Output**: Section 1 (Introduction) and initial research for all standards areas

### Phase 1: UI Naming Convention Standards (MANDATORY for Frontend/UI Projects)

**Prerequisites:** Phase 0 complete

**CRITICAL**: This phase is MANDATORY for projects with frontend, UI components, or mockups. Skip this phase ONLY if the project is backend-only with no UI layer (e.g., pure API services, CLI tools, background workers).

**Applicability Check**:

- ✅ REQUIRED: Web applications, mobile apps, desktop apps with GUI, design systems, component libraries
- ✅ REQUIRED: Projects with HTML/CSS, React/Vue/Angular, iOS/Android UI, Electron/Tauri UI
- ✅ REQUIRED: Projects with UI mockups, wireframes, or design specifications
- ❌ SKIP: Pure REST APIs without UI, CLI tools, background services, data processing pipelines, microservices without UI

1. **Define UI component naming conventions**:
   - Component/Widget names (PascalCase, kebab-case, etc.)
   - Props/Attributes naming
   - Event handler naming
   - State variable naming
   - CSS class naming (BEM, utility-first, etc.)
   - ID attribute naming

2. **Define UI file naming conventions**:
   - Component files
   - Style files (CSS, SCSS, styled-components)
   - Test files
   - Story files (Storybook)

3. **Define UI structure conventions**:
   - Component directory structure
   - Asset organization (images, icons, fonts)
   - Layout component patterns
   - Composition patterns

4. **Define accessibility naming**:
   - ARIA attribute naming
   - Role naming
   - Label associations
   - Landmark naming

5. **Research UI framework-specific conventions**:
   - For React: Hook naming (`use` prefix), Context naming, HOC naming
   - For Vue: Composable naming, Directive naming, Plugin naming
   - For Angular: Service naming, Directive naming, Pipe naming
   - For native mobile: ViewController/Activity naming, View naming

**Output**: Section 2 (UI Naming Conventions) - MANDATORY for frontend/UI projects, fully detailed. If backend-only, document "N/A - No UI layer" and proceed to Phase 2.

### Phase 2: Code Naming Convention Standards

**Prerequisites:** Phase 1 complete

1. **Define variable naming conventions**:
   - Local variables (camelCase, snake_case, etc.)
   - Constants (SCREAMING_SNAKE_CASE, etc.)
   - Global variables (if allowed)
   - Class/Instance variables

2. **Define function/method naming conventions**:
   - Function names (verbs, camelCase, etc.)
   - Method names
   - Constructor names
   - Getter/Setter naming
   - Boolean function prefixes (is, has, should, can, etc.)

3. **Define class/type naming conventions**:
   - Class names (PascalCase, etc.)
   - Interface names (prefix with 'I' or not)
   - Enum names
   - Type alias names
   - Generic type parameter names (T, TKey, TValue, etc.)

4. **Define module/package naming conventions**:
   - Module/Package names
   - Namespace conventions
   - Import alias conventions

**Output**: Section 3 (Code Naming Conventions) completed

### Phase 3: File, Directory & Project Structure Standards

**Prerequisites:** Phase 2 complete

1. **Define file naming conventions**:
   - Source code files
   - Test files
   - Configuration files
   - Documentation files

2. **Define directory structure standards**:
   - Source code organization
   - Test directory structure
   - Asset directories
   - Configuration directories
   - Documentation directories

3. **Document project structure patterns**:
   - Monorepo vs multi-repo
   - Feature-based vs layer-based organization
   - Shared code organization

**Output**: Section 4 (File and Directory Structure) completed

### Phase 4: API, Database & Integration Standards

**Prerequisites:** Phase 3 complete

1. **Define API design standards**:
   - Endpoint naming conventions (plural nouns, verbs, etc.)
   - HTTP methods usage (GET, POST, PUT, PATCH, DELETE)
   - Query parameter naming
   - Request/Response body structure
   - Error response format
   - API versioning strategy
   - GraphQL naming (if applicable): Type names, Field names, Mutation names

2. **Define database naming conventions**:
   - Table names (plural, snake_case, etc.)
   - Column names
   - Primary key naming
   - Foreign key naming
   - Index naming
   - Constraint naming
   - View naming
   - Stored procedure naming

3. **Define integration conventions**:
   - Message queue naming
   - Event naming
   - Webhook naming
   - External service integration patterns

**Output**: Section 5 (API Design Standards) and Section 6 (Database Standards) completed

### Phase 5: Testing, Git & Documentation Standards

**Prerequisites:** Phase 4 complete

1. **Define testing standards**:
   - Test file naming
   - Test case naming (describe, it, test, etc.)
   - Test data naming
   - Mock/Stub naming
   - Fixture naming
   - Coverage requirements

2. **Define Git workflow standards**:
   - Branch naming conventions (feature/, bugfix/, hotfix/, release/)
   - Commit message format (Conventional Commits, etc.)
   - PR/MR naming and description format
   - Tag naming for releases

3. **Define documentation standards**:
   - Code comment conventions
   - Docstring/JSDoc format
   - README structure
   - Inline documentation
   - API documentation format

**Output**: Sections 7 (Testing Standards), 8 (Git Workflow), 9 (Documentation Standards) completed

### Phase 6: Code Quality & Style Guide

**Prerequisites:** Phase 5 complete

1. **Define code formatting standards**:
   - Indentation (spaces vs tabs, size)
   - Line length limits
   - Blank line usage
   - Brace style
   - Import/Include ordering

2. **Define code quality standards**:
   - Complexity limits (cyclomatic complexity, cognitive complexity)
   - Function/Method length limits
   - File length limits
   - Duplication thresholds
   - Code smell detection

3. **Define linting and formatting tools**:
   - Linter configurations (ESLint, Pylint, RuboCop, etc.)
   - Code formatter configurations (Prettier, Black, gofmt, etc.)
   - Pre-commit hooks

**Output**: Section 10 (Code Style Guide) completed

### Phase 7: Finalization & Agent Context Update

**Prerequisites:** Phase 6 complete

1. **Generate enforcement tools configuration**:
   - `.editorconfig` for consistent editor settings
   - Linter configuration files
   - Formatter configuration files
   - Pre-commit hook scripts

2. **Create quick reference guides**:
   - `docs/standards-cheatsheet.md` - One-page reference
   - `docs/ui-naming-quick-ref.md` - UI naming quick reference

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent-specific context with standards
   - Add naming conventions and code style rules
   - Preserve manual additions between markers

4. **Validation**:
   - Ensure all sections are complete
   - Verify UI naming conventions are detailed (MANDATORY)
   - Check consistency with architecture and ground-rules
   - Validate all examples are language-appropriate

**Output**: Complete standards.md in docs/, configuration files, quick reference guides, updated agent context

## Key Rules

- Use absolute paths for all file operations
- Standards document goes to `docs/standards.md` (product-level), NOT `specs/` (feature-level)
- UI naming conventions are MANDATORY for frontend/UI projects - must be comprehensive and detailed
- UI naming conventions can be skipped ONLY for pure backend projects (APIs, CLI, workers) without any UI layer
- All examples MUST match the project's programming languages and frameworks
- Standards MUST be consistent with architecture decisions
- Ground-rules constraints MUST be respected
- ERROR if UI naming conventions section is incomplete or missing for frontend/UI projects
- If backend-only project (no UI), document "N/A" for UI naming section and proceed
- Reference architecture.md for technology stack context
- Research best practices specific to the project's tech stack
- Provide concrete examples for every convention

## Research Guidelines

When researching naming conventions and standards:

1. **For UI frameworks**:
   - Research: "{framework} component naming conventions best practices"
   - Research: "{framework} file structure best practices"
   - Examples: "React component naming conventions", "Vue composable naming"

2. **For backend languages**:
   - Research: "{language} naming conventions PEP8/style guide"
   - Research: "{language} project structure best practices"
   - Examples: "Python PEP 8 naming conventions", "Java naming conventions Oracle"

3. **For databases**:
   - Research: "{database} naming conventions best practices"
   - Examples: "PostgreSQL table naming conventions", "MongoDB collection naming"

4. **For APIs**:
   - Research: "RESTful API naming conventions best practices"
   - Research: "GraphQL schema naming conventions"
   - Research: "API versioning strategies"

5. **For design systems**:
   - Research: "{design system} naming conventions"
   - Examples: "Material Design naming conventions", "Bootstrap class naming"

## UI Naming Convention Requirements (For Frontend/UI Projects)

**Applicability**: Apply this section ONLY if the project has frontend, UI components, or mockups.

The UI naming conventions section MUST include:

- [ ] Component naming patterns with examples
- [ ] Props/Attributes naming with examples
- [ ] Event handler naming with examples
- [ ] State management naming (hooks, stores, etc.)
- [ ] CSS class naming methodology (BEM, utility, etc.)
- [ ] ID attribute naming rules
- [ ] File naming for UI components
- [ ] Directory structure for UI code
- [ ] Accessibility attribute naming (ARIA)
- [ ] Framework-specific conventions
- [ ] Do's and Don'ts with examples
- [ ] Common anti-patterns to avoid

## Success Criteria

The standards document is complete when:

- [ ] UI naming conventions are comprehensive and detailed (MANDATORY for frontend/UI projects)
- [ ] For backend-only projects without UI, Section 2 documents "N/A - No UI layer"
- [ ] All major code elements have naming conventions defined
- [ ] File and directory structure is documented
- [ ] API design standards are specified
- [ ] Database naming conventions are established
- [ ] Testing standards are documented
- [ ] Git workflow and commit conventions are defined
- [ ] Documentation standards are specified
- [ ] Code style guide is complete
- [ ] All conventions have concrete examples
- [ ] All examples use project-specific technologies
- [ ] Enforcement tools are configured
- [ ] Quick reference guides are generated
- [ ] Agent context is updated with standards
- [ ] Standards are consistent with architecture and ground-rules

## Output Files

Expected output structure:

```
docs/
├── standards.md              # Main standards document (from standards-template.md)
├── standards-cheatsheet.md   # One-page quick reference
├── ui-naming-quick-ref.md    # UI naming quick reference
└── examples/                 # Optional: Code examples
    ├── component-examples/
    ├── api-examples/
    └── test-examples/

# Configuration files (root level)
.editorconfig                 # Editor configuration
.eslintrc.js / .pylintrc      # Language-specific linter config
.prettierrc / .black          # Language-specific formatter config
```

## Example Usage

```bash
# Generate standards for the entire product
/rainbow.standardize

# Generate standards with specific focus
/rainbow.standardize Focus on React component naming and TypeScript conventions

# Generate standards for specific domain
/rainbow.standardize Healthcare application with HIPAA compliance considerations
```

## Notes

- This command creates **product-level standards**, not feature-level standards
- Run this AFTER ground-rules, rainbow, and architect commands
- Run this BEFORE creating feature implementation plans (`/rainbow.design`)
- The standards guide ALL development work across ALL features
- Update the standards document as the project evolves
- UI naming conventions are non-negotiable and must be thorough
- Standards should be enforced through automated tools (linters, formatters)
- Consider creating a style guide website for easy team reference
- Regularly review and update standards based on team feedback
