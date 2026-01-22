# Context Assessment: [Project Name]

**Date**: [Date]  
**Assessor**: AI Agent  
**Assessment Type**: Modern Codebase Analysis

---

## Executive Summary

<!-- Complete this section LAST after all analysis is done -->

**Purpose**: [Brief description of why this assessment was conducted]

**Key Findings**:

- [Major finding 1]
- [Major finding 2]
- [Major finding 3]

**Technical Health Score**: [X/100]

**Overall Assessment**: [2-3 sentences summarizing the codebase state]

**Integration Readiness**: [High/Medium/Low] - [Brief justification]

**Recommendations**:

1. [Key recommendation for adding new features]
2. [Key recommendation for maintaining consistency]
3. [Key recommendation for technical improvements]

---

## Technology Stack

### Programming Languages & Runtimes

**Primary Language**: [Language] ([Version])  
**Runtime**: [Runtime environment and version]

**Additional Languages**:

- [Language]: [Usage context]
- [Language]: [Usage context]

### Frameworks & Libraries

**Core Framework**: [Framework name and version]

**Key Libraries**:

| Library | Version | Purpose |
|---------|---------|---------|
| [Name] | [Version] | [What it does] |
| [Name] | [Version] | [What it does] |
| [Name] | [Version] | [What it does] |

### Databases & Storage

**Primary Database**: [Database type and version]  
**Caching Layer**: [If applicable]  
**File Storage**: [If applicable]

### Build & Package Management

**Package Manager**: [npm, pip, Maven, etc.]  
**Build Tool**: [Webpack, Vite, Maven, Gradle, etc.]  
**Task Runner**: [If applicable]

### Development Tools

**Code Quality**:

- Linter: [Tool and config]
- Formatter: [Tool and config]
- Type Checking: [If applicable]

**Testing**:

- Unit Testing: [Framework]
- Integration Testing: [Framework]
- E2E Testing: [Framework, if used]

---

## Project Structure

### Directory Tree

```
[Project Root]/
├── [Folder 1]/           # [Description]
│   ├── [Subfolder]/     # [Description]
│   └── [Files]          # [Description]
├── [Folder 2]/           # [Description]
│   ├── [Subfolder]/     # [Description]
│   └── [Files]          # [Description]
├── [Config files]        # [Description]
└── [Documentation]       # [Description]
```

### Key Directories

| Directory | Purpose | Key Files |
|-----------|---------|-----------|
| [Path] | [What it contains] | [Notable files] |
| [Path] | [What it contains] | [Notable files] |
| [Path] | [What it contains] | [Notable files] |

### Architectural Layers

**Presentation Layer**:

- Location: [Directory paths]
- Responsibilities: [What it handles]
- Key files: [Examples]

**Business Logic Layer**:

- Location: [Directory paths]
- Responsibilities: [What it handles]
- Key files: [Examples]

**Data Access Layer**:

- Location: [Directory paths]
- Responsibilities: [What it handles]
- Key files: [Examples]

**Common/Shared Layer**:

- Location: [Directory paths]
- Responsibilities: [What it handles]
- Key files: [Examples]

### Entry Points

**Main Application**:

```[language]
// File: [path/to/entry.file]
[Code snippet showing application entry point]
```

**API Entry Points**:

```[language]
// File: [path/to/routes.file]
[Code snippet showing route definitions]
```

**Background Jobs** (if applicable):

```[language]
// File: [path/to/workers.file]
[Code snippet showing job definitions]
```

---

## Architecture Patterns

### Architectural Style

**Primary Architecture**: [Monolith/Microservices/Serverless/Hybrid]

**Pattern**: [MVC/Clean Architecture/Hexagonal/Layered/Domain-Driven Design/etc.]

**Rationale**: [Why this architecture was chosen based on code analysis]

### Design Patterns

**Commonly Used Patterns**:

1. **[Pattern Name]** (e.g., Repository Pattern)
   - **Usage**: [Where and why it's used]
   - **Example**:

   ```[language]
   // File: [path/to/example.file]
   [Code snippet demonstrating the pattern]
   ```

2. **[Pattern Name]** (e.g., Factory Pattern)
   - **Usage**: [Where and why it's used]
   - **Example**:

   ```[language]
   // File: [path/to/example.file]
   [Code snippet demonstrating the pattern]
   ```

3. **[Pattern Name]** (e.g., Dependency Injection)
   - **Usage**: [Where and why it's used]
   - **Example**:

   ```[language]
   // File: [path/to/example.file]
   [Code snippet demonstrating the pattern]
   ```

### Component Relationships

**Module Dependencies**:

```
[Module A] --> [Module B]
[Module A] --> [Module C]
[Module B] --> [Module D]
```

**Service Boundaries** (if microservices):

**Communication Patterns**:

- **Internal**: [How components communicate - function calls, events, etc.]
- **External**: [REST, GraphQL, WebSockets, message queues, etc.]

---

## Coding Conventions & Standards

### Naming Conventions

**Files**:

- Components/Classes: [Convention - e.g., PascalCase.tsx, snake_case.py]
- Utilities: [Convention]
- Tests: [Convention - e.g., *.test.ts, test_*.py]

**Code Elements**:

```[language]
// File: [representative example file]

// Classes/Components: [Convention]
class UserProfile { }

// Functions: [Convention]
function getUserData() { }

// Variables: [Convention]
const userName = '';

// Constants: [Convention]
const MAX_RETRY_ATTEMPTS = 3;

// Interfaces/Types: [Convention]
interface IUserData { }
```

### File Organization

**Feature Organization Pattern**:

```
[Typical feature structure]
feature-name/
├── [file pattern 1]
├── [file pattern 2]
└── [file pattern 3]
```

**Example**:

```
// Example from actual codebase: [path/to/feature]
[Show actual structure from the code]
```

### Code Quality Practices

**Linting**:

- Tool: [ESLint, Pylint, etc.]
- Config: [Reference to config file]
- Rules: [Notable enabled rules]

**Formatting**:

- Tool: [Prettier, Black, etc.]
- Config: [Reference to config file]
- Style: [Notable style choices]

**Type Checking** (if applicable):

- Tool: [TypeScript, mypy, etc.]
- Strictness: [Configuration level]

### Error Handling Pattern

**Standard Approach**:

```[language]
// File: [representative example]
[Code snippet showing error handling pattern]
```

**Error Types**:

- [Error type 1]: [When used]
- [Error type 2]: [When used]

### Logging Pattern

**Logger**: [Winston, Python logging, etc.]

**Example**:

```[language]
// File: [representative example]
[Code snippet showing logging pattern]
```

**Log Levels**: [How different levels are used]

### Documentation Standards

**Inline Comments**:

```[language]
[Example of comment style used in codebase]
```

**Function Documentation**:

```[language]
[Example of function/method documentation pattern - JSDoc, docstrings, etc.]
```

---

## Data Layer

### Data Models

**ORM/Query Builder**: [Prisma, TypeORM, SQLAlchemy, Hibernate, etc.]

**Entity Example**:

```[language]
// File: [path/to/model.file]
[Code snippet showing entity/model definition]
```

**Key Entities**:

| Entity | Purpose | Key Fields |
|--------|---------|-----------|
| [Name] | [What it represents] | [Important fields] |
| [Name] | [What it represents] | [Important fields] |
| [Name] | [What it represents] | [Important fields] |

### Schema Design

**Database Schema Approach**: [Normalized, denormalized, document-based, etc.]

**Relationships**:

- [Entity A] → [Entity B]: [Relationship type and description]
- [Entity C] → [Entity D]: [Relationship type and description]

**Validation Rules**:

```[language]
// Example validation from: [file path]
[Code snippet showing validation approach]
```

### Data Access Patterns

**Repository Pattern Usage**:

```[language]
// File: [path/to/repository.file]
[Code snippet showing repository pattern]
```

**Query Patterns**:

- **Simple queries**: [How they're written]
- **Complex queries**: [How they're structured]
- **Transactions**: [How they're handled]

**Caching Strategy**:

- **What is cached**: [Types of data]
- **Cache invalidation**: [Strategy used]
- **Cache provider**: [Redis, in-memory, etc.]

**Example**:

```[language]
// File: [path/to/caching.file]
[Code snippet showing caching pattern]
```

### Migration Strategy

**Migration Tool**: [Prisma Migrate, Alembic, Flyway, etc.]

**Migration Location**: [Directory path]

**Migration Pattern**:

```[language]
// Example migration: [migration file]
[Code snippet showing migration structure]
```

---

## API & Integration Patterns

### API Architecture

**API Style**: [REST/GraphQL/gRPC/Hybrid]

**Base URL Pattern**: [How URLs are structured]

**Versioning**: [How API versions are managed]

### Endpoint Patterns

**Route Definition Example**:

```[language]
// File: [path/to/routes.file]
[Code snippet showing route definitions]
```

**Key Endpoints**:

| Method | Path | Purpose |
|--------|------|---------|
| [GET] | [/path] | [Description] |
| [POST] | [/path] | [Description] |
| [PUT] | [/path] | [Description] |

### Request/Response Patterns

**Request Structure**:

```json
// Example request body
{
  "field1": "value",
  "field2": "value"
}
```

**Response Structure**:

```json
// Success response
{
  "status": "success",
  "data": { }
}

// Error response
{
  "status": "error",
  "message": "...",
  "errors": []
}
```

### Authentication & Authorization

**Authentication Method**: [JWT, Sessions, OAuth, etc.]

**Implementation**:

```[language]
// File: [path/to/auth.file]
[Code snippet showing auth implementation]
```

**Authorization Pattern**:

```[language]
// File: [path/to/authorization.file]
[Code snippet showing authorization checks]
```

### External Integrations

**Third-Party Services**:

| Service | Purpose | Integration Pattern |
|---------|---------|-------------------|
| [Name] | [What it does] | [How integrated] |
| [Name] | [What it does] | [How integrated] |

**Integration Example**:

```[language]
// File: [path/to/integration.file]
[Code snippet showing external service client]
```

### Error Handling in APIs

**HTTP Status Code Usage**:

- 200: [When used]
- 400: [When used]
- 401: [When used]
- 404: [When used]
- 500: [When used]

**Error Response Example**:

```[language]
// File: [path/to/error-handler.file]
[Code snippet showing error response pattern]
```

---

## Testing Strategy

### Testing Framework

**Unit Testing**: [Jest, pytest, JUnit, etc.]  
**Integration Testing**: [Supertest, pytest-django, etc.]  
**E2E Testing**: [Playwright, Cypress, Selenium, etc.]

### Test Organization

**Test File Pattern**:

```
[How tests are organized relative to source files]
src/
  feature.ts
  feature.test.ts  # or
tests/
  feature.test.ts
```

**Test Example**:

```[language]
// File: [path/to/test.file]
[Code snippet showing representative test]
```

### Testing Patterns

**Mocking/Stubbing**:

```[language]
// Example: [test file]
[Code snippet showing mocking pattern]
```

**Test Data**:

- **Fixtures**: [How managed]
- **Factories**: [If used]
- **Seed data**: [Approach]

**Test Utilities**:

- Location: [Where helper functions are]
- Common patterns: [What utilities exist]

### Test Coverage

**Current Coverage**: [X%] (if measurable)

**Coverage Tool**: [Jest coverage, pytest-cov, etc.]

**Well-Tested Areas**:

- [Area 1]: [Coverage level]
- [Area 2]: [Coverage level]

**Testing Gaps**:

- [Area lacking tests 1]
- [Area lacking tests 2]

**Critical Paths Covered**:

- [ ] [Critical path 1]
- [ ] [Critical path 2]
- [ ] [Critical path 3]

---

## Build & Deployment

### Build Process

**Build Command**: `[command to build]`

**Build Steps**:

1. [Step 1 - e.g., Install dependencies]
2. [Step 2 - e.g., Type checking]
3. [Step 3 - e.g., Compile/Transpile]
4. [Step 4 - e.g., Bundle assets]

**Build Configuration**:

```[language]
// File: [build config file]
[Key configuration snippets]
```

**Output**:

- **Location**: [Build output directory]
- **Artifacts**: [What is produced]

### Environment Configuration

**Environment Variables**:

| Variable | Purpose | Example Value |
|----------|---------|--------------|
| [VAR_NAME] | [What it controls] | [Sample value] |
| [VAR_NAME] | [What it controls] | [Sample value] |

**Environment Files**:

- `.env.example`: [Template]
- `.env.development`: [Local dev]
- `.env.production`: [Production config]

**Configuration Loading**:

```[language]
// File: [config file]
[Code snippet showing how config is loaded]
```

### Deployment Strategy

**Deployment Target**: [Cloud provider, containers, serverless, etc.]

**Deployment Method**: [Manual, CI/CD, etc.]

**Deployment Steps**:

1. [Step 1]
2. [Step 2]
3. [Step 3]

### CI/CD Pipeline

**CI Tool**: [GitHub Actions, GitLab CI, Jenkins, etc.]

**Pipeline Stages**:

```yaml
# File: [pipeline config file]
[Key pipeline configuration snippets]
```

**Automated Checks**:

- [ ] Linting
- [ ] Type checking
- [ ] Unit tests
- [ ] Integration tests
- [ ] Build verification

### DevOps Practices

**Infrastructure as Code**: [If used - Terraform, CloudFormation, etc.]

**Monitoring**: [Tools and approach]

**Logging**: [Centralized logging solution]

**Health Checks**:

```[language]
// File: [health check file]
[Code snippet showing health check endpoint]
```

---

## Technical Health Assessment

### Health Score: [X/100]

**Calculation**:

```
Score = (
  Code Quality (0-100) × 0.30 +
  Architecture (0-100) × 0.25 +
  Testing (0-100) × 0.20 +
  Documentation (0-100) × 0.15 +
  Dependencies (0-100) × 0.10
)

Code Quality:    [X/100] → [Justification]
Architecture:    [X/100] → [Justification]
Testing:         [X/100] → [Justification]
Documentation:   [X/100] → [Justification]
Dependencies:    [X/100] → [Justification]

Total Score: [X/100]
```

### Strengths

1. **[Strength 1]**
   - Evidence: [Specific examples or metrics]
   - Impact: [Why this is beneficial]

2. **[Strength 2]**
   - Evidence: [Specific examples or metrics]
   - Impact: [Why this is beneficial]

3. **[Strength 3]**
   - Evidence: [Specific examples or metrics]
   - Impact: [Why this is beneficial]

### Areas for Improvement

1. **[Area 1]**
   - Issue: [What needs improvement]
   - Impact: [Why this matters]
   - Recommendation: [How to address]

2. **[Area 2]**
   - Issue: [What needs improvement]
   - Impact: [Why this matters]
   - Recommendation: [How to address]

3. **[Area 3]**
   - Issue: [What needs improvement]
   - Impact: [Why this matters]
   - Recommendation: [How to address]

### Technical Debt Hotspots

**High Priority**:

- [Location/Component]: [Description of technical debt]
- [Location/Component]: [Description of technical debt]

**Medium Priority**:

- [Location/Component]: [Description of technical debt]
- [Location/Component]: [Description of technical debt]

**Low Priority**:

- [Location/Component]: [Description of technical debt]

---

## Feature Integration Readiness

### Adding New Features

**Overall Readiness**: [High/Medium/Low]

**Typical Feature Addition Workflow**:

1. **Create Feature Specification**:
   - Use `/rainbow.specify` with brownfield context
   - Align with existing patterns documented here

2. **Design Technical Implementation**:
   - Follow architectural patterns in [Architecture Patterns](#architecture-patterns)
   - Use coding conventions from [Coding Conventions](#coding-conventions--standards)
   - Integrate with data layer patterns from [Data Layer](#data-layer)

3. **Implementation Steps**:
   - Create feature directory following [File Organization](#file-organization)
   - Implement using design patterns documented in [Design Patterns](#design-patterns)
   - Follow API patterns from [API & Integration Patterns](#api--integration-patterns)
   - Write tests following [Testing Strategy](#testing-strategy)

### Common Workflows

**Adding a New API Endpoint**:

```[language]
// 1. Define route in: [routes file]
[Code snippet showing route addition pattern]

// 2. Implement controller in: [controllers dir]
[Code snippet showing controller pattern]

// 3. Add service logic in: [services dir]
[Code snippet showing service pattern]

// 4. Write tests in: [tests dir]
[Code snippet showing test pattern]
```

**Adding a New Data Entity**:

```[language]
// 1. Define model in: [models dir]
[Code snippet showing model definition pattern]

// 2. Create migration: [migration command]
[Code snippet showing migration pattern]

// 3. Add repository in: [repositories dir]
[Code snippet showing repository pattern]

// 4. Write tests in: [tests dir]
[Code snippet showing test pattern]
```

**Adding a New UI Component** (if applicable):

```[language]
// 1. Create component in: [components dir]
[Code snippet showing component pattern]

// 2. Add styles in: [styles location]
[Code snippet showing styling pattern]

// 3. Integrate in parent: [parent component]
[Code snippet showing integration pattern]

// 4. Write tests in: [tests dir]
[Code snippet showing test pattern]
```

### Integration Constraints

**Performance Considerations**:

- [Constraint 1]: [Description and rationale]
- [Constraint 2]: [Description and rationale]

**Security Requirements**:

**Compatibility Requirements**:

**Breaking Change Policy**:

- [How breaking changes are handled]
- [Versioning strategy]

### Recommendations for New Features

1. **[Recommendation 1]**
   - What: [Specific guidance]
   - Why: [Rationale]
   - How: [Implementation approach]

2. **[Recommendation 2]**
   - What: [Specific guidance]
   - Why: [Rationale]
   - How: [Implementation approach]

3. **[Recommendation 3]**
   - What: [Specific guidance]
   - Why: [Rationale]
   - How: [Implementation approach]

---

## Conclusion

**Assessment Summary**: [Brief overall assessment of codebase health]

**Integration Readiness**: [Final verdict on readiness for new features]

**Next Steps**:

1. [Next action - e.g., "Use /rainbow.specify to create feature specification"]
2. [Follow-up action]
3. [Additional consideration]

**Assessment Completed**: [Date/Time]

---

## Appendix

### Dependency List

**Production Dependencies**:

```
[Full list or link to package.json/requirements.txt/pom.xml]
```

**Development Dependencies**:

```
[Full list or link to package.json/requirements.txt/pom.xml]
```

### Security Considerations

**Known Vulnerabilities**: [If any found during assessment]

**Security Audit Date**: [Last audit if known]

**Security Tools**: [Tools used for security scanning]

### Performance Metrics

**Benchmarks** (if available):

**Performance Targets**:

### References

- **Project Documentation**: [Links to docs]
- **Architecture Diagrams**: [Links to diagrams if exist]
- **Decision Records**: [Links to ADRs if exist]
- **Team Conventions**: [Links to team guides]
