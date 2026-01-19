---
description: Analyze existing modern codebase to understand architecture, patterns, and conventions before adding new features
handoffs:
  - label: Set Project Principles
    agent: rainbow.regulate
    prompt: Create project principles based on context assessment
    send: true
  - label: Specify New Feature
    agent: rainbow.specify
    prompt: Create specification for new feature based on context assessment
  - label: Get Architecture Review
    agent: hanoi.architect
    prompt: Review architecture assessment and provide recommendations
scripts:
  sh: scripts/bash/setup-assess-context.sh --json
  ps: scripts/powershell/setup-assess-context.ps1 -Json
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

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add codebase context assessment') and commit context-assessment.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON for CONTEXT_ASSESSMENT, DOCS_DIR, REPO_ROOT, HAS_GIT.

2. **Load context**: Read `memory/ground-rules.md` (if exists), `docs/architecture.md` (if exists), `docs/standards.md` (if exists). Load CONTEXT_ASSESSMENT template (already copied). Adhere to the principles for maximizing system clarity, structural simplicity, and long-term maintainability.

3. **Execute context assessment workflow**: Follow the structure in CONTEXT_ASSESSMENT template to:
   - Identify technology stack and frameworks
   - Analyze project structure and organization
   - Document architectural patterns and design decisions
   - Extract coding conventions and standards
   - Map key components and their relationships
   - Identify data models and storage patterns
   - Document API contracts and integration patterns
   - Assess testing approach and coverage
   - Review build and deployment processes
   - Calculate technical health score

4. **Update agent context**: Run `{AGENT_SCRIPT}` to update agent-specific context with assessment findings.

5. **Stop and report**: Command ends after assessment completion. Report CONTEXT_ASSESSMENT path and technical health score.

## Phases

### Phase 0: Technology Stack Discovery

1. **Identify core technologies**:
   - Programming languages and versions (package.json, requirements.txt, pom.xml, etc.)
   - Frameworks and major libraries
   - Runtime environment (Node.js, Python, Java, .NET, etc.)
   - Database systems (PostgreSQL, MongoDB, Redis, etc.)
   - Build tools (npm, pip, Maven, Gradle, etc.)

2. **Document development environment**:
   - Required tools and versions
   - Environment setup requirements
   - Local development scripts
   - Configuration management approach

3. **Review dependencies**:
   - Key dependencies and their purpose
   - Dependency update strategy
   - Deprecated or outdated dependencies
   - Security vulnerabilities

**Output**: Executive Summary and Technology Stack sections completed

### Phase 1: Project Structure Analysis

**Prerequisites:** Phase 0 complete

1. **Analyze directory structure**:
   - Top-level folder organization
   - Module/package structure
   - Separation of concerns (frontend/backend, domain layers)
   - Configuration file locations
   - Asset organization (images, styles, etc.)

2. **Identify architectural layers**:
   - Presentation layer structure
   - Business logic organization
   - Data access layer patterns
   - Common/shared utilities
   - External service integrations

3. **Document entry points**:
   - Application startup/bootstrap
   - API routes/endpoints
   - Background jobs/workers
   - CLI commands

**Output**: Project Structure section with directory tree and layer descriptions

### Phase 2: Architectural Patterns

**Prerequisites:** Phase 1 complete

1. **Identify architectural style**:
   - Monolith, microservices, serverless, or hybrid
   - Layered architecture (MVC, Clean, Hexagonal, etc.)
   - Design patterns used (Repository, Factory, Strategy, etc.)
   - Event-driven components
   - CQRS or similar patterns

2. **Analyze component relationships**:
   - Module dependencies
   - Service boundaries
   - Communication patterns (REST, GraphQL, events, etc.)
   - Shared libraries/common code
   - External service dependencies

3. **Review key design decisions**:
   - Why this architecture was chosen
   - Trade-offs and constraints
   - Scalability approach
   - Performance optimization strategies

**Output**: Architecture Patterns section

### Phase 3: Coding Conventions & Standards

**Prerequisites:** Phase 2 complete

1. **Extract naming conventions**:
   - File naming patterns
   - Class/function naming styles
   - Variable naming patterns
   - Constant and configuration naming

2. **Identify code organization patterns**:
   - How features are organized
   - Module/package structure conventions
   - Test file organization
   - Documentation patterns

3. **Review code quality practices**:
   - Linting/formatting tools (ESLint, Prettier, Black, etc.)
   - Code review practices
   - Error handling patterns
   - Logging conventions
   - Comment and documentation standards

**Output**: Coding Conventions section

### Phase 4: Data Layer Assessment

**Prerequisites:** Phase 3 complete

1. **Analyze data models**:
   - Entity/model definitions
   - Schema design patterns
   - Relationships and associations
   - Validation rules
   - Migration strategy

2. **Review data access patterns**:
   - ORM/query builder usage (Prisma, TypeORM, SQLAlchemy, etc.)
   - Repository patterns
   - Caching strategy
   - Transaction management
   - Connection pooling

3. **Document data storage**:
   - Primary database(s)
   - Caching layers
   - File storage approach
   - State management (frontend/backend)

**Output**: Data Layer section

### Phase 5: API & Integration Patterns

**Prerequisites:** Phase 4 complete

1. **Catalog API endpoints**:
   - REST/GraphQL endpoints
   - Authentication/authorization approach
   - Request/response patterns
   - Error handling conventions
   - API versioning strategy

2. **Review integration patterns**:
   - External service clients
   - Third-party API usage
   - Webhook handling
   - Message queues/event buses
   - Background job processing

3. **Document API contracts**:
   - Request/response schemas
   - Authentication requirements
   - Rate limiting
   - API documentation approach (Swagger, OpenAPI, etc.)

**Output**: API & Integration Patterns section

### Phase 6: Testing Strategy

**Prerequisites:** Phase 5 complete

1. **Identify testing approach**:
   - Test frameworks (Jest, pytest, JUnit, etc.)
   - Test coverage expectations
   - Testing pyramid balance (unit/integration/e2e)
   - Test organization patterns

2. **Review test patterns**:
   - Mocking/stubbing approach
   - Test data management
   - Fixture patterns
   - Test utilities and helpers

3. **Assess test coverage**:
   - Current coverage percentage
   - Critical paths covered
   - Areas lacking tests
   - Testing gaps

**Output**: Testing Strategy section

### Phase 7: Build & Deployment

**Prerequisites:** Phase 6 complete

1. **Document build process**:
   - Build scripts and commands
   - Compilation/transpilation steps
   - Asset bundling approach
   - Environment-specific builds

2. **Review deployment strategy**:
   - Deployment targets (cloud, containers, serverless)
   - CI/CD pipeline
   - Environment configurations
   - Deployment scripts

3. **Identify DevOps practices**:
   - Infrastructure as Code
   - Monitoring and logging
   - Health checks
   - Rollback strategies

**Output**: Build & Deployment section

### Phase 8: Technical Health Assessment

**Prerequisites:** Phase 7 complete

1. **Calculate technical health score**:
   - Code Quality (30%): Linting, complexity, duplication
   - Architecture (25%): Pattern consistency, modularity, coupling
   - Testing (20%): Coverage, test quality, CI integration
   - Documentation (15%): Code comments, README, API docs
   - Dependencies (10%): Up-to-date, security, licensing

2. **Identify strengths**:
   - Well-implemented patterns
   - Strong test coverage
   - Clear documentation
   - Modern practices

3. **Identify improvement areas**:
   - Technical debt hotspots
   - Missing tests
   - Documentation gaps
   - Inconsistent patterns
   - Outdated dependencies

**Output**: Technical Health Assessment section with score

### Phase 9: Feature Integration Readiness

**Prerequisites:** Phase 8 complete

1. **Assess readiness for new features**:
   - How to add new endpoints/routes
   - How to create new entities/models
   - How to add new UI components
   - How to add new tests
   - How to add new dependencies

2. **Document common workflows**:
   - Adding a new feature (typical steps)
   - Extending existing functionality
   - Modifying data models
   - Adding integrations

3. **Identify constraints**:
   - Performance considerations
   - Security requirements
   - Compatibility requirements
   - Breaking change policies

**Output**: Feature Integration Readiness section

### Phase 10: Finalization & Agent Context Update

**Prerequisites:** Phase 9 complete

1. **Complete executive summary**:
   - Key findings and insights
   - Technical health score summary
   - Strengths and improvement areas
   - Recommendations for adding new features
   - Integration guidelines

2. **Validate completeness**:
   - Ensure all sections are complete
   - Verify metrics and assessments
   - Check that all "ACTION REQUIRED" comments are addressed
   - Validate consistency with ground-rules (if exists)

3. **Agent context update**:
   - Run `{AGENT_SCRIPT}`
   - Update agent-specific context with assessment findings
   - Add key patterns and conventions
   - Preserve manual additions between markers

**Output**: Complete context-assessment.md, updated agent context

## Key Rules

- Use absolute paths for all file operations
- Assessment document goes to `docs/context-assessment.md` (project-level, not feature-specific)
- Focus on PATTERNS and CONVENTIONS, not exhaustive code listing
- Extract examples from actual code (show, don't just describe)
- Every major finding MUST have code examples
- Recommendations MUST align with existing patterns
- ERROR if assessment lacks concrete examples
- Reference ground-rules constraints (if exists)
- Consider team conventions and project history

## Technical Health Scoring Formula

```
Technical Health Score (0-100) = (
  Code Quality × 0.30 +
  Architecture × 0.25 +
  Testing × 0.20 +
  Documentation × 0.15 +
  Dependencies × 0.10
)

Where:
- Code Quality: Linting compliance, complexity metrics, duplication
- Architecture: Pattern consistency, modularity, coupling metrics
- Testing: Coverage percentage, test quality, CI integration
- Documentation: README quality, API docs, inline comments
- Dependencies: Up-to-date packages, security, license compliance
```

**Interpretation**:

- 80-100: Excellent (Well-maintained, easy to extend)
- 60-79: Good (Some areas need attention)
- 40-59: Fair (Significant technical debt)
- 0-39: Poor (Major refactoring needed)

## Success Criteria

The context assessment is complete when:

- [ ] Technology stack documented with versions
- [ ] Project structure analyzed with clear layer separation
- [ ] Architectural patterns identified with examples
- [ ] Coding conventions extracted from actual code
- [ ] Data layer patterns documented
- [ ] API patterns and contracts documented
- [ ] Testing strategy and coverage assessed
- [ ] Build and deployment process documented
- [ ] Technical health score calculated with formula shown
- [ ] Feature integration guidelines provided
- [ ] Executive summary completed
- [ ] Agent context updated

## Example Usage

```bash
# First time with existing project (brownfield workflow)
/rainbow.assess-context Analyze our e-commerce platform to understand architecture and patterns
# Then follow up with:
# /rainbow.regulate (create principles based on assessment)
# /rainbow.specify (add new features)

# Assess full stack Node.js/React application
/rainbow.assess-context Analyze our e-commerce platform before adding new payment gateway

# Assess Python Django application
/rainbow.assess-context Review our Django REST API codebase structure and conventions

# Assess with specific focus
/rainbow.assess-context Focus on testing patterns and API integration conventions for our microservices

# Quick assessment for small projects
/rainbow.assess-context Quick assessment of project structure and conventions
```

## Notes

- Run this ONCE per project when first adding Rainbow to existing brownfield codebase
- Run this BEFORE creating project principles in brownfield projects
- The assessment guides how new features should integrate with existing code
- For legacy mainframe/midrange systems, use `/rainbow.assess-legacy` instead
- Assessment is stored at `docs/context-assessment.md` (project-level)
- **Recommended workflow**: `/rainbow.assess-context` → `/rainbow.regulate` → `/rainbow.specify`
- Follow up with `/rainbow.regulate` to create project principles based on assessment
- Then use `/rainbow.specify` to create feature specifications
