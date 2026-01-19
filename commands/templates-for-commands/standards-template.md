# Coding Standards: [PRODUCT/PROJECT NAME]

**Version**: 1.0 | **Date**: [DATE] | **Status**: Active

**Purpose**: A concise, high-signal standards guide for **AI agents** to ensure consistent, maintainable, and secure code.

---

## Table of Contents

1. Executive Summary
2. Core Conventions
3. UI Naming (Mandatory for frontend projects)
4. Code Naming
5. Files & Directories
6. API Standards
7. Database Standards
8. Testing
9. Git Workflow
10. Documentation
11. Code Style
12. Enforcement
13. Agent Checklist

---

## 1. Executive Summary

<!-- ACTION REQUIRED: 2–3 sentences on stack and naming baseline. -->
- **Stack**: [Frontend], [Backend], [DB], [Testing]
- **Naming**: Components (PascalCase), props (camelCase), constants (SCREAMING_SNAKE_CASE), functions (verb_case), classes (PascalCase)

---

## 2. Core Conventions

- <!-- ACTION REQUIRED: Confirm selected conventions; list any project-specific overrides. -->
- **Consistency over preference**: Choose once, apply everywhere
- **Descriptive > abbreviations**: Use clear names and types
- **Security-by-default**: Validate inputs, encode outputs, least privilege
- **Testability**: Pure functions, clear interfaces, dependency injection where relevant

---

## 3. UI Naming (Mandatory for frontend projects)

<!-- ACTION REQUIRED: Choose and document component/file, props/booleans, handlers, hooks/state, and CSS methodology. -->
### 3.1 Components & Files

- **Components**: PascalCase nouns (e.g., `UserProfile`, `ProductCard`)
- **Files**: Match component names exactly (e.g., `UserProfile.tsx`)

### 3.2 Props & Booleans

- **Props**: camelCase, descriptive
- **Boolean props**: Prefix with `is`, `has`, `should`, `can` (e.g., `isOpen`, `hasErrors`)

### 3.3 Event Handlers

- **Internal**: `handle` + EventName (e.g., `handleSubmit`)
- **Props**: `on` + EventName (e.g., `onSubmit`)

### 3.4 Hooks & State

- **Custom hooks**: `use` prefix (e.g., `useAuth`)
- **useState**: `[value, setValue]` with descriptive names (e.g., `isLoading`, `setIsLoading`)

### 3.5 CSS Naming

- **Choose one**: BEM or utility-first
- **BEM**: `Block__Element--Modifier`
- **CSS Modules**: kebab-case in CSS, camelCase in JS imports

---

## 4. Code Naming

<!-- ACTION REQUIRED: Define variable/constant/function/class/interface/enum naming rules. -->
### 4.1 Variables & Constants

- **Variables**: camelCase (JS/TS), snake_case (Python)
- **Constants**: SCREAMING_SNAKE_CASE

### 4.2 Functions & Booleans

- **Functions**: Verb-based (e.g., `calculateTotal`)
- **Boolean functions**: `is_`, `has_`, `should_`, `can_`

### 4.3 Classes & Types

- **Classes**: PascalCase nouns (e.g., `PaymentProcessor`)
- **Interfaces (TS)**: Pick `IUser` or `User` convention and stick to it
- **Enums (TS)**: PascalCase enum; SCREAMING_SNAKE_CASE values

---

## 5. Files & Directories

<!-- ACTION REQUIRED: Adopt structure; specify tests adjacency and configuration files used. -->
- **Tests adjacent to source**
- **Feature-based structure** for large projects; layer-based acceptable for small ones
- **Configs**: `.editorconfig`, linters, formatters, `.env`

---

## 6. API Standards

<!-- ACTION REQUIRED: Define REST param casing, endpoint patterns, statuses, response shape; add GraphQL if applicable. -->
### 6.1 REST

- **Endpoints**: plural, lowercase, hyphen-separated (e.g., `/api/users`)
- **Query params**: choose snake_case or camelCase and be consistent
- **Status codes**: Use standard HTTP codes (200, 201, 204, 400, 401, 403, 404, 409, 422, 500)
- **Responses**: Consistent `data` + `meta` for success; `error` object for failures

### 6.2 GraphQL (if applicable)

- **Types**: PascalCase
- **Mutations**: verb-based, camelCase (e.g., `createUser`)

---

## 7. Database Standards

<!-- ACTION REQUIRED: Confirm table/column naming, PK/FK patterns, indexes, and constraints. -->
- **Tables**: plural, snake_case (e.g., `order_items`)
- **Columns**: snake_case, descriptive
- **PK**: `id`; **FK**: `{table}_id`
- **Indexes**: `idx_{table}_{columns}`; **Constraints**: `uq_`, `chk_`, `fk_`

---

## 8. Testing

<!-- ACTION REQUIRED: Establish naming patterns, AAA usage, mocks/fixtures; set coverage targets if needed. -->
- **Naming**: Descriptive test names
- **Pattern**: Arrange–Act–Assert
- **Mocks/Fixtures**: Clear names (e.g., `mockDatabase`, `sampleUser`)

---

## 9. Git Workflow

<!-- ACTION REQUIRED: Choose branch and commit conventions; enforce via CI checks. -->
- **Branches**: `{type}/{ticket-id}-{description}` (e.g., `feature/ABC-123-add-auth`)
- **Commits**: Conventional Commits (`feat`, `fix`, `docs`, `refactor`, `test`, `chore`)
- **PRs**: Match commit style; include context and tests

---

## 10. Documentation

<!-- ACTION REQUIRED: Pick docstring/JSDoc style; outline README essentials. -->
- **Comments**: Explain "why", not "what"
- **Docstrings/JSDoc**: Use a single chosen style and enforce
- **README**: Provide quick start, config, testing, deployment

---

## 11. Code Style

<!-- ACTION REQUIRED: Set indentation, line length, import order, and brace style per language. -->
- **Indentation**: [2 spaces JS/TS, 4 spaces Python]
- **Line length**: Max [e.g., 100 or 120]
- **Imports**: Std lib → third-party → local
- **Brace style**: Pick one (K&R or Allman) per language and stick to it

---

## 12. Enforcement

<!-- ACTION REQUIRED: List linters/formatters/pre-commit/CI; link to configuration files. -->
- **Linters**: ESLint (JS/TS), Flake8/Black (Python)
- **Formatters**: Prettier (JS/TS), Black (Python)
- **Pre-commit**: Trailing whitespace, EOF fixers, lint/format hooks
- **CI**: Lint + format checks on push/PR

---

## 13. Agent Checklist

<!-- ACTION REQUIRED: Fill precisely; this anchors agent behavior. -->
- Naming rules: components, props, functions, classes, enums
- API: endpoints, params style, response shape, error model
- DB: table/column naming, keys, indexes
- Tests: required coverage or critical paths
- Git: branch + commit format
- Docs: comment/docstring style
- Style: indentation, max line length, import order
- Enforcement: tools and configs

---

**Notes**

- Prefer explicit names; avoid abbreviations
- Keep decisions documented (short ADRs)
- Link to code, schemas, and configs where possible
