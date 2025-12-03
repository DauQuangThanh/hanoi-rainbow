# Coding Standards & Conventions: [PRODUCT/PROJECT NAME]

**Version**: 1.0 | **Date**: [DATE] | **Status**: Active  
**Maintained by**: [TEAM/ARCHITECT] | **Last Reviewed**: [DATE]

**Note**: This document defines the coding standards, naming conventions, and best practices for the entire product. All developers must follow these standards to ensure consistency, maintainability, and quality across the codebase.

---

## Document Control

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | [DATE] | [NAME] | Initial standards document |
| | | | |

**Related Documents**:

- Architecture: `docs/architecture.md`
- Ground Rules: `memory/ground-rules.md`
- Feature Specifications: `specs/[###-feature]/spec.md`

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [UI Naming Conventions](#2-ui-naming-conventions) ⭐ **MANDATORY**
3. [Code Naming Conventions](#3-code-naming-conventions)
4. [File and Directory Structure](#4-file-and-directory-structure)
5. [API Design Standards](#5-api-design-standards)
6. [Database Standards](#6-database-standards)
7. [Testing Standards](#7-testing-standards)
8. [Git Workflow](#8-git-workflow)
9. [Documentation Standards](#9-documentation-standards)
10. [Code Style Guide](#10-code-style-guide)
11. [Enforcement](#11-enforcement)
12. [Appendices](#12-appendices)

---

## 1. Introduction

### 1.1 Purpose

This document establishes comprehensive coding standards and naming conventions for [PRODUCT/PROJECT NAME]. Following these standards ensures:

- **Consistency**: Code looks uniform across the codebase
- **Maintainability**: Code is easier to understand and modify
- **Collaboration**: Team members can read and work with each other's code
- **Quality**: Automated tools can enforce standards
- **Onboarding**: New team members can quickly understand conventions

### 1.2 Scope

These standards apply to:

- All source code in the repository
- All documentation
- All configuration files
- All database schemas
- All API definitions
- All UI components

### 1.3 Technology Stack

<!--
  ACTION REQUIRED: Fill in your actual technology stack
-->

**Frontend**: [e.g., React 18, TypeScript 5, Tailwind CSS 3]  
**Backend**: [e.g., FastAPI 0.104, Python 3.11]  
**Database**: [e.g., PostgreSQL 15]  
**Mobile**: [e.g., React Native 0.72 or Flutter 3.13]  
**Testing**: [e.g., Jest, Pytest, Cypress]

### 1.4 How to Use This Document

- **Developers**: Follow these standards in all code you write
- **Code Reviewers**: Verify adherence to these standards in PRs
- **Team Leads**: Enforce standards and update this document as needed
- **New Team Members**: Read this document during onboarding

---

## 2. UI Naming Conventions

<!--
  ⭐ MANDATORY SECTION - Must be comprehensive and detailed
  This section is the most critical for frontend development
-->

### 2.1 Component Naming

#### 2.1.1 Component Files and Names

<!--
  ACTION REQUIRED: Choose and document your component naming convention
-->

**Convention**: [e.g., "PascalCase for component names and files"]

**Component Names**:

```typescript
// ✅ Good - PascalCase, descriptive, noun-based
<UserProfile />
<NavigationBar />
<ProductCard />
<ShoppingCart />
<SearchInput />

// ❌ Bad - lowercase, abbreviations, verbs
<userprofile />
<navBar />
<ProdCard />
<DoSearch />
```

**Component Files**:

```
// ✅ Good - Match component name exactly
UserProfile.tsx
NavigationBar.tsx
ProductCard.tsx

// ❌ Bad - Inconsistent with component name
user-profile.tsx
navbar.tsx
prod-card.tsx
```

#### 2.1.2 Component Types and Suffixes

**Naming by Purpose**:

```typescript
// Page/Screen components - suffix with Page or Screen
HomePage.tsx
ProfilePage.tsx
CheckoutScreen.tsx (for mobile)

// Layout components - suffix with Layout
DashboardLayout.tsx
MainLayout.tsx
AuthLayout.tsx

// Container components - suffix with Container
UserListContainer.tsx
CartContainer.tsx

// Presentational components - no suffix
Button.tsx
Card.tsx
Modal.tsx

// HOC components - prefix with with
withAuth.tsx
withLoading.tsx
withTheme.tsx
```

#### 2.1.3 Composition and Subcomponents

**Dot Notation for Related Components**:

```typescript
// Parent component
export const Card = ({ children }) => {
  return <div className="card">{children}</div>
}

// Subcomponents using dot notation
Card.Header = ({ children }) => {
  return <div className="card-header">{children}</div>
}

Card.Body = ({ children }) => {
  return <div className="card-body">{children}</div>
}

Card.Footer = ({ children }) => {
  return <div className="card-footer">{children}</div>
}

// Usage
<Card>
  <Card.Header>Title</Card.Header>
  <Card.Body>Content</Card.Body>
  <Card.Footer>Actions</Card.Footer>
</Card>
```

### 2.2 Props and Attributes

#### 2.2.1 Prop Naming

**Convention**: [e.g., "camelCase for props, specific and descriptive"]

```typescript
// ✅ Good - camelCase, descriptive
interface ButtonProps {
  onClick: () => void
  isDisabled: boolean
  variant: 'primary' | 'secondary'
  size: 'small' | 'medium' | 'large'
  ariaLabel: string
  children: React.ReactNode
}

// ❌ Bad - unclear, inconsistent
interface ButtonProps {
  click: () => void
  disabled: boolean  // Not prefixed with 'is'
  type: string        // Too generic
  sz: string          // Abbreviation
  label: string       // Conflicts with children
}
```

#### 2.2.2 Boolean Props

**Convention**: Prefix with `is`, `has`, `should`, or `can`

```typescript
// ✅ Good - Clear boolean intent
<Modal isOpen={true} />
<Button isDisabled={false} />
<Form hasErrors={true} />
<Card shouldAnimate={true} />
<User canEdit={false} />

// ❌ Bad - Ambiguous
<Modal open={true} />      // Could be string or number
<Button disabled={false} />  // Inconsistent with 'is' prefix
<Form errors={true} />      // Unclear type
```

#### 2.2.3 Callback Props

**Convention**: Prefix with `on` for event handlers

```typescript
// ✅ Good - Clear event handlers
interface FormProps {
  onSubmit: (data: FormData) => void
  onChange: (field: string, value: any) => void
  onError: (error: Error) => void
  onSuccess: () => void
  onCancel: () => void
}

// ❌ Bad - Not clearly callbacks
interface FormProps {
  submit: (data: FormData) => void
  change: (field: string, value: any) => void
  error: (error: Error) => void
}
```

### 2.3 Event Handlers

#### 2.3.1 Event Handler Naming

**Convention**: `handle` + `EventName` (internal) or `on` + `EventName` (prop)

```typescript
// ✅ Good - Clear event handler naming
function UserForm() {
  const handleSubmit = (e: FormEvent) => {
    // Handle form submission
  }
  
  const handleInputChange = (e: ChangeEvent<HTMLInputElement>) => {
    // Handle input change
  }
  
  const handleDeleteClick = () => {
    // Handle delete button click
  }
  
  return (
    <form onSubmit={handleSubmit}>
      <input onChange={handleInputChange} />
      <button onClick={handleDeleteClick}>Delete</button>
    </form>
  )
}

// ❌ Bad - Unclear or inconsistent
function UserForm() {
  const submit = (e) => { }       // Missing 'handle' prefix
  const onInputChange = (e) => { } // 'on' prefix for internal handler
  const deleteUser = () => { }     // Not clearly an event handler
}
```

### 2.4 State Variables

#### 2.4.1 useState Naming

**Convention**: Variable name + `set` + Variable name (PascalCase)

```typescript
// ✅ Good - Clear state variable naming
const [user, setUser] = useState<User | null>(null)
const [isLoading, setIsLoading] = useState(false)
const [hasError, setHasError] = useState(false)
const [items, setItems] = useState<Item[]>([])
const [selectedId, setSelectedId] = useState<string | null>(null)

// ❌ Bad - Inconsistent or unclear
const [user, updateUser] = useState(null)        // Use 'set' not 'update'
const [loading, setLoading] = useState(false)    // Use 'isLoading'
const [error, setError] = useState(false)        // Use 'hasError'
const [data, setData] = useState([])             // Too generic
```

#### 2.4.2 useReducer Naming

**Convention**: State variable + `dispatch` or action-specific name

```typescript
// ✅ Good - Clear reducer state naming
const [state, dispatch] = useReducer(reducer, initialState)

// Or more specific:
const [cartState, dispatchCartAction] = useReducer(cartReducer, initialCartState)
const [formState, dispatchFormAction] = useReducer(formReducer, initialFormState)

// ❌ Bad - Unclear
const [s, d] = useReducer(reducer, initial)
const [data, update] = useReducer(reducer, initial)
```

### 2.5 Hooks

#### 2.5.1 Custom Hook Naming

**Convention**: Always prefix with `use`, followed by descriptive name

```typescript
// ✅ Good - Clear custom hook naming
function useAuth() {
  // Authentication logic
  return { user, login, logout }
}

function useLocalStorage(key: string, initialValue: any) {
  // Local storage logic
  return [value, setValue]
}

function useDebounce(value: string, delay: number) {
  // Debounce logic
  return debouncedValue
}

function useFetchUser(userId: string) {
  // Fetch user logic
  return { user, isLoading, error }
}

// ❌ Bad - Missing 'use' prefix
function auth() { }
function localStorage(key: string) { }
function debounce(value: string) { }
```

### 2.6 CSS Class Naming

<!--
  ACTION REQUIRED: Choose your CSS naming methodology (BEM, utility-first, etc.)
-->

#### 2.6.1 BEM Methodology (if using BEM)

**Convention**: Block__Element--Modifier

```css
/* ✅ Good - BEM naming */
.card { }
.card__header { }
.card__body { }
.card__footer { }
.card--featured { }
.card__header--large { }

/* Usage in HTML/JSX */
<div className="card card--featured">
  <div className="card__header card__header--large">
    <h2 className="card__title">Title</h2>
  </div>
  <div className="card__body">
    <p className="card__text">Content</p>
  </div>
</div>
```

#### 2.6.2 Utility-First (if using Tailwind CSS)

**Convention**: Use Tailwind utility classes, create semantic components for complex UI

```tsx
// ✅ Good - Semantic component with utilities
export const PrimaryButton = ({ children, onClick }) => {
  return (
    <button 
      onClick={onClick}
      className="bg-blue-600 hover:bg-blue-700 text-white font-semibold py-2 px-4 rounded-lg shadow-md transition-colors duration-200"
    >
      {children}
    </button>
  )
}

// For one-off layouts, use utilities directly
<div className="flex flex-col gap-4 p-6 bg-white rounded-lg shadow">
  <h1 className="text-2xl font-bold text-gray-900">Title</h1>
  <p className="text-gray-600">Description</p>
</div>
```

#### 2.6.3 CSS Modules (if using CSS Modules)

**Convention**: camelCase for class names in JS, kebab-case in CSS

```css
/* Button.module.css */
.primary-button { }
.secondary-button { }
.button-icon { }
```

```tsx
// Button.tsx
import styles from './Button.module.css'

<button className={styles.primaryButton}>
  <span className={styles.buttonIcon}>Icon</span>
  Click me
</button>
```

### 2.7 ID Attributes

**Convention**: kebab-case, descriptive, unique across the application

```html
<!-- ✅ Good - kebab-case, descriptive -->
<input id="user-email" />
<div id="shopping-cart-modal" />
<button id="submit-payment-btn" />
<section id="featured-products-section" />

<!-- ❌ Bad - camelCase, generic, unclear -->
<input id="email1" />
<div id="modal" />
<button id="btn" />
<section id="section1" />
```

### 2.8 Accessibility (ARIA) Naming

#### 2.8.1 ARIA Attributes

**Convention**: Use semantic HTML first, ARIA when necessary

```tsx
// ✅ Good - Proper ARIA usage
<button 
  aria-label="Close modal"
  aria-pressed={isPressed}
  role="button"
>
  <CloseIcon aria-hidden="true" />
</button>

<nav aria-label="Main navigation">
  <ul role="list">
    <li><a href="/" aria-current="page">Home</a></li>
  </ul>
</nav>

<div role="alert" aria-live="polite">
  {errorMessage}
</div>

// ❌ Bad - Redundant or missing ARIA
<button aria-label="button">Click</button>  // Redundant
<div onClick={handler}>Click me</div>        // Missing role
<img src="logo.png" />                       // Missing alt
```

### 2.9 File Naming for UI Code

#### 2.9.1 Component Files

```
components/
├── Button/
│   ├── Button.tsx           # Component implementation
│   ├── Button.test.tsx      # Component tests
│   ├── Button.stories.tsx   # Storybook stories
│   ├── Button.module.css    # Component styles (if CSS Modules)
│   └── index.ts             # Barrel export
├── Card/
│   ├── Card.tsx
│   ├── CardHeader.tsx       # Subcomponent (alternative to dot notation)
│   ├── CardBody.tsx
│   ├── CardFooter.tsx
│   └── index.ts
```

#### 2.9.2 Page/Screen Files

```
pages/                       # For Next.js or route-based frameworks
├── index.tsx                # Home page
├── about.tsx                # About page
├── users/
│   ├── [id].tsx            # Dynamic route
│   └── index.tsx
└── api/
    └── users.ts             # API route

screens/                     # For React Native
├── HomeScreen.tsx
├── ProfileScreen.tsx
└── CheckoutScreen.tsx
```

#### 2.9.3 Utility and Hook Files

```
hooks/
├── useAuth.ts
├── useFetchData.ts
└── useLocalStorage.ts

utils/
├── formatDate.ts
├── validateEmail.ts
└── calculateTotal.ts
```

### 2.10 Directory Structure for UI

```
src/
├── components/              # Reusable UI components
│   ├── common/             # Basic components (Button, Input, etc.)
│   ├── forms/              # Form-related components
│   ├── layout/             # Layout components (Header, Footer, Sidebar)
│   └── features/           # Feature-specific components
├── pages/                  # Page components or route components
├── hooks/                  # Custom React hooks
├── contexts/               # React Context providers
├── styles/                 # Global styles
│   ├── globals.css
│   ├── variables.css
│   └── themes/
├── assets/                 # Static assets
│   ├── images/
│   ├── icons/
│   └── fonts/
├── services/               # API service functions
├── utils/                  # Utility functions
├── types/                  # TypeScript type definitions
└── constants/              # Constants and configuration
```

### 2.11 Common UI Naming Anti-Patterns

**❌ Avoid These Patterns**:

```typescript
// ❌ Generic component names
<Wrapper />
<Container />
<Component />
<Thing />

// ❌ Abbreviations
<UsrPrf />
<NavBar />
<BtnGrp />

// ❌ Inconsistent naming
<user-card />       // kebab-case for component
<UserCard/>         // No space
< UserCard />       // Extra space

// ❌ Unclear boolean props
<Modal open />      // Use isOpen
<Button active />   // Use isActive
<Form valid />      // Use isValid

// ❌ Verb-based component names
<FetchUser />       // Components are nouns, not verbs
<DisplayData />
<ProcessPayment />
```

---

## 3. Code Naming Conventions

### 3.1 Variables

#### 3.1.1 Local Variables

**Convention**: [e.g., "camelCase, descriptive nouns or noun phrases"]

```python
# ✅ Good - camelCase (JavaScript/TypeScript) or snake_case (Python)
user_name = "John"
total_amount = 100.50
is_active = True
items_list = []
current_index = 0

# ❌ Bad - unclear, abbreviated
usr = "John"
amt = 100.50
active = True
list = []
i = 0  # (except in short loops)
```

#### 3.1.2 Constants

**Convention**: SCREAMING_SNAKE_CASE

```python
# ✅ Good - SCREAMING_SNAKE_CASE
MAX_RETRY_COUNT = 3
DEFAULT_TIMEOUT = 30
API_BASE_URL = "https://api.example.com"
DATABASE_CONNECTION_STRING = "postgresql://..."

# ❌ Bad - inconsistent casing
maxRetryCount = 3
Max_Retry_Count = 3
MAXRETRYCOUNT = 3
```

#### 3.1.3 Global Variables

**Convention**: [Rainbow if global variables are allowed and how to name them]

```python
# ✅ If globals are necessary, use module-level prefix
_INTERNAL_CACHE = {}  # Leading underscore for internal use
APP_CONFIG = {}       # SCREAMING_SNAKE_CASE for public globals

# ❌ Avoid global variables when possible
```

### 3.2 Functions and Methods

#### 3.2.1 Function Names

**Convention**: [e.g., "camelCase (JS/TS) or snake_case (Python), verb-based"]

```python
# ✅ Good - verb + noun, descriptive
def calculate_total(items):
    pass

def fetch_user_data(user_id):
    pass

def validate_email(email):
    pass

def format_date(date, format):
    pass

# ❌ Bad - noun-based, unclear
def total(items):        # Missing verb
def user(id):            # Not descriptive
def check(email):        # Too generic
def do_it():             # Meaningless
```

#### 3.2.2 Boolean Functions

**Convention**: Prefix with `is_`, `has_`, `should_`, `can_`

```python
# ✅ Good - boolean intent clear
def is_valid(data):
    return True

def has_permission(user, action):
    return False

def should_retry(attempt_count):
    return attempt_count < 3

def can_edit(user, resource):
    return user.is_admin

# ❌ Bad - unclear return type
def valid(data):         # Could return anything
def permission(user):    # Not clearly boolean
def retry(count):        # Unclear intent
```

#### 3.2.3 Getter and Setter Methods

```python
# ✅ Good - consistent getter/setter pattern
class User:
    def get_name(self):
        return self._name
    
    def set_name(self, name):
        self._name = name
    
    # Or use property decorators (Python)
    @property
    def email(self):
        return self._email
    
    @email.setter
    def email(self, value):
        self._email = value
```

### 3.3 Classes and Types

#### 3.3.1 Class Names

**Convention**: PascalCase, nouns

```python
# ✅ Good - PascalCase, descriptive nouns
class UserAccount:
    pass

class ShoppingCart:
    pass

class PaymentProcessor:
    pass

class DatabaseConnection:
    pass

# ❌ Bad - camelCase, verbs, abbreviations
class userAccount:       # Wrong case
class ProcessPayment:    # Verb-based
class DBConn:            # Abbreviation
```

#### 3.3.2 Interface Names (TypeScript/Java)

**Convention**: [Choose: PascalCase with or without 'I' prefix]

```typescript
// ✅ Option 1: No prefix (modern TypeScript convention)
interface User {
  id: string
  name: string
}

interface Serializable {
  serialize(): string
}

// ✅ Option 2: 'I' prefix (traditional convention)
interface IUser {
  id: string
  name: string
}

interface ISerializable {
  serialize(): string
}

// Pick ONE convention and stick to it throughout the project
```

#### 3.3.3 Enum Names

**Convention**: PascalCase for enum, SCREAMING_SNAKE_CASE for values

```typescript
// ✅ Good
enum UserRole {
  ADMIN = 'ADMIN',
  USER = 'USER',
  GUEST = 'GUEST'
}

enum HttpStatus {
  OK = 200,
  NOT_FOUND = 404,
  SERVER_ERROR = 500
}

// ❌ Bad
enum userRole {           // Wrong case for enum
  admin = 'admin',        // Wrong case for values
  user = 'user'
}
```

#### 3.3.4 Generic Type Parameters

**Convention**: Single uppercase letter or descriptive PascalCase

```typescript
// ✅ Good - standard single letters
function identity<T>(arg: T): T {
  return arg
}

class Container<TItem> {
  items: TItem[]
}

// ✅ Good - descriptive for complex generics
class Repository<TEntity, TKey> {
  find(key: TKey): TEntity | null
}

// ❌ Bad - lowercase, unclear
function identity<t>(arg: t): t { }
class Container<item> { }
```

### 3.4 Modules and Packages

**Convention**: [Rainbow based on language]

```python
# Python - lowercase with underscores
user_service.py
payment_processor.py
data_validators.py

# JavaScript/TypeScript - camelCase or kebab-case
userService.ts
payment-processor.ts
dataValidators.ts

# Packages - lowercase, no underscores
mypackage/
  __init__.py
  user_service.py
```

---

## 4. File and Directory Structure

### 4.1 File Naming

#### 4.1.1 Source Code Files

```
# Backend (Python)
user_service.py
payment_processor.py
database_models.py

# Backend (TypeScript/Node.js)
userService.ts
paymentProcessor.ts
databaseModels.ts

# Frontend - see Section 2.9
```

#### 4.1.2 Test Files

```
# Python
test_user_service.py
user_service_test.py

# JavaScript/TypeScript
userService.test.ts
userService.spec.ts

# Keep test files adjacent to source files
src/
  userService.ts
  userService.test.ts
```

#### 4.1.3 Configuration Files

```
.env                    # Environment variables
.editorconfig          # Editor configuration
.eslintrc.js           # ESLint configuration
tsconfig.json          # TypeScript configuration
pytest.ini             # Pytest configuration
```

### 4.2 Directory Structure

#### 4.2.1 Backend Directory Structure

```
backend/
├── src/
│   ├── api/                    # API routes and controllers
│   │   ├── v1/                 # API version
│   │   │   ├── users.py
│   │   │   └── products.py
│   │   └── middleware/
│   ├── services/               # Business logic
│   │   ├── user_service.py
│   │   └── payment_service.py
│   ├── models/                 # Data models
│   │   ├── user.py
│   │   └── product.py
│   ├── repositories/           # Data access layer
│   │   ├── user_repository.py
│   │   └── product_repository.py
│   ├── utils/                  # Utility functions
│   ├── config/                 # Configuration
│   └── main.py                 # Application entry point
├── tests/
│   ├── unit/
│   ├── integration/
│   └── fixtures/
├── docs/
└── scripts/
```

#### 4.2.2 Frontend Directory Structure

See Section 2.10 for detailed frontend structure.

### 4.3 Project Organization Patterns

**Feature-Based** (Recommended for large projects):

```
src/
├── features/
│   ├── auth/
│   │   ├── components/
│   │   ├── hooks/
│   │   ├── services/
│   │   └── types/
│   ├── products/
│   │   ├── components/
│   │   ├── hooks/
│   │   └── services/
│   └── cart/
└── shared/
    ├── components/
    ├── hooks/
    └── utils/
```

**Layer-Based** (Traditional):

```
src/
├── components/
├── services/
├── models/
├── utils/
└── types/
```

---

## 5. API Design Standards

### 5.1 RESTful API Conventions

#### 5.1.1 Endpoint Naming

**Convention**: Plural nouns, lowercase, hyphen-separated

```
# ✅ Good - RESTful conventions
GET    /api/users
GET    /api/users/{id}
POST   /api/users
PUT    /api/users/{id}
PATCH  /api/users/{id}
DELETE /api/users/{id}

GET    /api/users/{id}/orders
GET    /api/products/featured
POST   /api/shopping-cart/items

# ❌ Bad - verbs, singular, camelCase
GET    /api/getUsers
GET    /api/user
POST   /api/createUser
GET    /api/usersById/{id}
GET    /api/productList
```

#### 5.1.2 Query Parameters

**Convention**: snake_case or camelCase (choose one and be consistent)

```
# ✅ Good - consistent snake_case
GET /api/users?page=1&page_size=20&sort_by=name&order=asc
GET /api/products?category=electronics&min_price=100&max_price=500

# ✅ Also acceptable - consistent camelCase
GET /api/users?page=1&pageSize=20&sortBy=name&order=asc

# ❌ Bad - inconsistent
GET /api/users?page=1&pageSize=20&sort_by=name
```

#### 5.1.3 HTTP Methods

```
GET    - Retrieve resource(s), idempotent, no side effects
POST   - Create new resource, not idempotent
PUT    - Replace entire resource, idempotent
PATCH  - Partial update resource, idempotent
DELETE - Remove resource, idempotent

# Special cases
POST   /api/users/{id}/activate    # Action on existing resource
POST   /api/orders/{id}/cancel     # Non-CRUD operations
```

#### 5.1.4 Status Codes

```
200 OK                  - Successful GET, PUT, PATCH
201 Created            - Successful POST
204 No Content         - Successful DELETE
400 Bad Request        - Invalid request data
401 Unauthorized       - Missing or invalid authentication
403 Forbidden          - Authenticated but not authorized
404 Not Found          - Resource doesn't exist
409 Conflict           - Conflict (e.g., duplicate email)
422 Unprocessable      - Validation errors
500 Internal Error     - Server error
```

#### 5.1.5 Response Format

```json
// ✅ Good - consistent structure
{
  "data": {
    "id": "123",
    "name": "John Doe",
    "email": "john@example.com"
  },
  "meta": {
    "timestamp": "2024-01-01T00:00:00Z"
  }
}

// List responses
{
  "data": [
    { "id": "1", "name": "Item 1" },
    { "id": "2", "name": "Item 2" }
  ],
  "meta": {
    "total": 100,
    "page": 1,
    "page_size": 20
  }
}

// Error responses
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid email format",
    "details": [
      {
        "field": "email",
        "message": "Must be a valid email address"
      }
    ]
  }
}
```

### 5.2 GraphQL Conventions (if applicable)

#### 5.2.1 Type Naming

```graphql
# ✅ Good - PascalCase, descriptive
type User {
  id: ID!
  name: String!
  email: String!
}

type Product {
  id: ID!
  name: String!
  price: Float!
}

input CreateUserInput {
  name: String!
  email: String!
}

# ❌ Bad
type user { }           # Wrong case
type UserType { }       # Redundant suffix
```

#### 5.2.2 Query and Mutation Naming

```graphql
# ✅ Good - camelCase, verb-based mutations
type Query {
  user(id: ID!): User
  users(limit: Int, offset: Int): [User!]!
  searchProducts(query: String!): [Product!]!
}

type Mutation {
  createUser(input: CreateUserInput!): User!
  updateUser(id: ID!, input: UpdateUserInput!): User!
  deleteUser(id: ID!): Boolean!
}

# ❌ Bad
type Mutation {
  UserCreate(input: CreateUserInput!): User!  # Wrong case
  new_user(input: CreateUserInput!): User!    # snake_case
}
```

---

## 6. Database Standards

### 6.1 Table Naming

**Convention**: [e.g., "plural, snake_case"]

```sql
-- ✅ Good - plural, snake_case
CREATE TABLE users ( ... );
CREATE TABLE products ( ... );
CREATE TABLE order_items ( ... );
CREATE TABLE shopping_carts ( ... );

-- ❌ Bad - singular, camelCase
CREATE TABLE User ( ... );
CREATE TABLE product ( ... );
CREATE TABLE orderItem ( ... );
```

### 6.2 Column Naming

**Convention**: snake_case, descriptive

```sql
-- ✅ Good - snake_case, clear
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    first_name VARCHAR(100),
    last_name VARCHAR(100),
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- ❌ Bad - camelCase, abbreviations
CREATE TABLE users (
    ID INTEGER,
    fname VARCHAR(100),
    lname VARCHAR(100),
    email_addr VARCHAR(255)
);
```

### 6.3 Primary and Foreign Keys

**Convention**:

- Primary key: `id`
- Foreign key: `{table_name}_id`

```sql
-- ✅ Good
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    ...
);

CREATE TABLE orders (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    ...
);

CREATE TABLE order_items (
    id SERIAL PRIMARY KEY,
    order_id INTEGER REFERENCES orders(id),
    product_id INTEGER REFERENCES products(id),
    ...
);
```

### 6.4 Indexes

**Convention**: `idx_{table}_{column(s)}`

```sql
-- ✅ Good - descriptive index names
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_products_category_price ON products(category, price);

-- ❌ Bad - unclear
CREATE INDEX user_idx ON users(email);
CREATE INDEX idx1 ON orders(user_id);
```

### 6.5 Constraints

**Convention**:

- Unique: `uq_{table}_{column}`
- Check: `chk_{table}_{column}`
- Foreign key: `fk_{table}_{ref_table}`

```sql
-- ✅ Good - descriptive constraint names
ALTER TABLE users 
    ADD CONSTRAINT uq_users_email UNIQUE (email);

ALTER TABLE products 
    ADD CONSTRAINT chk_products_price CHECK (price >= 0);

ALTER TABLE orders 
    ADD CONSTRAINT fk_orders_users FOREIGN KEY (user_id) REFERENCES users(id);
```

---

## 7. Testing Standards

### 7.1 Test File Naming

See Section 4.1.2

### 7.2 Test Case Naming

**Convention**: Descriptive sentences that explain the test

```python
# ✅ Good - descriptive test names
def test_user_creation_with_valid_data():
    pass

def test_user_creation_fails_with_duplicate_email():
    pass

def test_login_succeeds_with_correct_credentials():
    pass

def test_login_fails_with_incorrect_password():
    pass

# JavaScript/TypeScript
describe('UserService', () => {
  describe('createUser', () => {
    it('should create user with valid data', () => {})
    it('should throw error when email is duplicate', () => {})
  })
})

# ❌ Bad - unclear test names
def test_user_1():
    pass

def test_create():
    pass
```

### 7.3 Test Structure

**Convention**: Arrange-Act-Assert (AAA) pattern

```python
def test_calculate_total_with_discount():
    # Arrange
    items = [
        Item(name="Item 1", price=10),
        Item(name="Item 2", price=20)
    ]
    discount = 0.1  # 10% discount
    
    # Act
    total = calculate_total(items, discount)
    
    # Assert
    assert total == 27.0  # (10 + 20) * 0.9
```

### 7.4 Mock and Fixture Naming

```python
# ✅ Good - clear mock/fixture names
@pytest.fixture
def mock_database():
    return MockDatabase()

@pytest.fixture
def sample_user():
    return User(name="Test User", email="test@example.com")

# ❌ Bad
@pytest.fixture
def db():
    pass

@pytest.fixture
def data():
    pass
```

---

## 8. Git Workflow

### 8.1 Branch Naming

**Convention**: `{type}/{ticket-id}-{brief-description}`

```bash
# ✅ Good - structured branch names
feature/ABC-123-add-user-authentication
bugfix/ABC-456-fix-login-error
hotfix/ABC-789-patch-security-vulnerability
release/v1.2.0
chore/ABC-321-update-dependencies

# ❌ Bad - unclear branches
john-branch
fix-bug
new-feature
```

**Branch Types**:

- `feature/` - New features
- `bugfix/` - Bug fixes
- `hotfix/` - Critical production fixes
- `release/` - Release preparation
- `chore/` - Maintenance tasks

### 8.2 Commit Messages

**Convention**: Conventional Commits format

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

**Types**:

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, no logic changes)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

**Examples**:

```bash
# ✅ Good - clear, conventional commits
feat(auth): add password reset functionality
fix(cart): resolve total calculation error with discounts
docs(api): update API endpoint documentation
refactor(user-service): simplify user validation logic
test(payment): add unit tests for payment processor

# With body and footer
feat(auth): add OAuth2 authentication

Implement OAuth2 authentication flow with Google and GitHub providers.
Includes token refresh mechanism and user profile synchronization.

Closes #123
Breaking Change: Removes legacy JWT authentication

# ❌ Bad - unclear commits
update code
fix bug
changes
WIP
```

### 8.3 Pull Request Naming

**Convention**: Same as commit message format

```
feat(auth): Add password reset functionality
fix(cart): Resolve total calculation error
docs(api): Update endpoint documentation
```

---

## 9. Documentation Standards

### 9.1 Code Comments

**Convention**:

- Use comments to explain "why", not "what"
- Keep comments up-to-date with code changes

```python
# ✅ Good - explains why
def calculate_price(base_price, quantity):
    # Apply bulk discount for orders over 100 units
    # to incentivize larger purchases
    if quantity > 100:
        return base_price * quantity * 0.9
    return base_price * quantity

# ❌ Bad - states the obvious
def calculate_price(base_price, quantity):
    # Check if quantity is greater than 100
    if quantity > 100:
        # Return base_price times quantity times 0.9
        return base_price * quantity * 0.9
    # Return base_price times quantity
    return base_price * quantity
```

### 9.2 Docstrings

**Convention**: [Choose format: Google, NumPy, or Sphinx style]

```python
# ✅ Good - Google style docstring
def calculate_total(items: List[Item], discount: float) -> float:
    """Calculate the total price of items with discount applied.
    
    Args:
        items: List of items to calculate total for.
        discount: Discount percentage as decimal (0.1 for 10%).
        
    Returns:
        Total price after discount is applied.
        
    Raises:
        ValueError: If discount is not between 0 and 1.
        
    Example:
        >>> items = [Item(price=10), Item(price=20)]
        >>> calculate_total(items, 0.1)
        27.0
    """
    if not 0 <= discount <= 1:
        raise ValueError("Discount must be between 0 and 1")
    
    subtotal = sum(item.price for item in items)
    return subtotal * (1 - discount)
```

### 9.3 JSDoc (JavaScript/TypeScript)

```typescript
/**
 * Calculate the total price of items with discount applied.
 * 
 * @param items - Array of items to calculate total for
 * @param discount - Discount percentage as decimal (0.1 for 10%)
 * @returns Total price after discount is applied
 * @throws {Error} If discount is not between 0 and 1
 * 
 * @example
 * ```typescript
 * const items = [{ price: 10 }, { price: 20 }];
 * const total = calculateTotal(items, 0.1);
 * // Returns: 27.0
 * ```
 */
function calculateTotal(items: Item[], discount: number): number {
  if (discount < 0 || discount > 1) {
    throw new Error('Discount must be between 0 and 1');
  }
  
  const subtotal = items.reduce((sum, item) => sum + item.price, 0);
  return subtotal * (1 - discount);
}
```

### 9.4 README Structure

```markdown
# Project Name

Brief description of the project.

## Features

- Feature 1
- Feature 2

## Installation

\`\`\`bash
npm install
\`\`\`

## Usage

\`\`\`bash
npm start
\`\`\`

## Configuration

Explain configuration options.

## Development

Instructions for local development.

## Testing

\`\`\`bash
npm test
\`\`\`

## Deployment

Deployment instructions.

## Contributing

Contribution guidelines.

## License

License information.
```

---

## 10. Code Style Guide

### 10.1 Indentation

**Convention**: [e.g., "2 spaces for JS/TS, 4 spaces for Python"]

```typescript
// ✅ Good - 2 spaces (JavaScript/TypeScript)
function example() {
  if (condition) {
    doSomething();
  }
}
```

```python
# ✅ Good - 4 spaces (Python)
def example():
    if condition:
        do_something()
```

### 10.2 Line Length

**Convention**: Maximum [80 or 100 or 120] characters per line

```python
# ✅ Good - break long lines
user = create_user(
    name="John Doe",
    email="john@example.com",
    role="admin",
    department="Engineering"
)

# ❌ Bad - exceeds line length
user = create_user(name="John Doe", email="john@example.com", role="admin", department="Engineering", status="active")
```

### 10.3 Blank Lines

```python
# ✅ Good - strategic blank lines
class UserService:
    """Service for user operations."""
    
    def __init__(self):
        self.users = []
    
    def create_user(self, name, email):
        """Create a new user."""
        user = User(name, email)
        self.users.append(user)
        return user
    
    def get_user(self, user_id):
        """Retrieve user by ID."""
        return next((u for u in self.users if u.id == user_id), None)
```

### 10.4 Import Ordering

```python
# ✅ Good - organized imports
# Standard library imports
import os
import sys
from datetime import datetime

# Third-party imports
import requests
from fastapi import FastAPI

# Local imports
from .models import User
from .services import UserService
```

### 10.5 Brace Style

**Convention**: [Choose: K&R, Allman, etc.]

```typescript
// ✅ K&R style (JavaScript/TypeScript)
function example() {
  if (condition) {
    doSomething();
  } else {
    doSomethingElse();
  }
}

// ❌ Allman style (inconsistent with JS convention)
function example()
{
  if (condition)
  {
    doSomething();
  }
}
```

---

## 11. Enforcement

### 11.1 Automated Tools

**Linters**:

- JavaScript/TypeScript: ESLint
- Python: Pylint, Flake8
- Ruby: RuboCop
- Java: Checkstyle

**Formatters**:

- JavaScript/TypeScript: Prettier
- Python: Black, autopep8
- Go: gofmt
- Rust: rustfmt

### 11.2 Pre-commit Hooks

```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.4.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-json
  
  - repo: https://github.com/psf/black
    rev: 23.3.0
    hooks:
      - id: black
  
  - repo: https://github.com/pre-commit/mirrors-eslint
    rev: v8.42.0
    hooks:
      - id: eslint
        files: \.(js|ts|tsx)$
```

### 11.3 CI/CD Integration

```yaml
# .github/workflows/lint.yml
name: Lint

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run ESLint
        run: npm run lint
      - name: Run Black
        run: black --check .
```

### 11.4 Code Review Checklist

- [ ] Follows naming conventions
- [ ] Has appropriate comments and documentation
- [ ] Passes linter checks
- [ ] Has tests with descriptive names
- [ ] Follows file and directory structure
- [ ] Uses consistent formatting
- [ ] Has meaningful commit messages

---

## 12. Appendices

### 12.1 Glossary

| Term | Definition |
|------|------------|
| PascalCase | Capitalized first letter of each word: `UserProfile` |
| camelCase | Lowercase first letter, capitalized subsequent words: `userProfile` |
| snake_case | Lowercase with underscores: `user_profile` |
| kebab-case | Lowercase with hyphens: `user-profile` |
| SCREAMING_SNAKE_CASE | Uppercase with underscores: `USER_PROFILE` |
| BEM | Block Element Modifier CSS methodology |

### 12.2 Quick Reference Checklist

**UI Naming**:

- [ ] Components: PascalCase
- [ ] Props: camelCase, boolean with `is`/`has` prefix
- [ ] Event handlers: `handle` + EventName
- [ ] State: descriptive + `set` + Name
- [ ] Hooks: `use` prefix
- [ ] CSS classes: Follow chosen methodology (BEM/utility)

**Code Naming**:

- [ ] Variables: camelCase (JS/TS) or snake_case (Python)
- [ ] Constants: SCREAMING_SNAKE_CASE
- [ ] Functions: Verb-based, camelCase/snake_case
- [ ] Classes: PascalCase, nouns
- [ ] Files: Match convention for language

**API**:

- [ ] Endpoints: Plural nouns, lowercase
- [ ] Query params: Consistent case
- [ ] Status codes: Appropriate HTTP codes

**Database**:

- [ ] Tables: Plural, snake_case
- [ ] Columns: snake_case
- [ ] Keys: `id` for PK, `{table}_id` for FK

**Git**:

- [ ] Branches: `type/ticket-description`
- [ ] Commits: Conventional Commits format
- [ ] PRs: Descriptive titles

### 12.3 Tool Configuration Files

See `docs/examples/` for sample configuration files:

- `.editorconfig`
- `.eslintrc.js`
- `.prettierrc`
- `pyproject.toml` (Black configuration)
- `.pre-commit-config.yaml`

### 12.4 Resources

- [Naming Conventions - Wikipedia](https://en.wikipedia.org/wiki/Naming_convention_(programming))
- [Conventional Commits](https://www.conventionalcommits.org/)
- [BEM Methodology](http://getbem.com/)
- [REST API Design Best Practices](https://restfulapi.net/)
- [Google JavaScript Style Guide](https://google.github.io/styleguide/jsguide.html)
- [PEP 8 – Style Guide for Python Code](https://peps.python.org/pep-0008/)

### 12.5 Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | [DATE] | [NAME] | Initial standards document |
| | | | |

---

**END OF STANDARDS DOCUMENT**

---

## Maintenance Notes

This document should be:

- **Reviewed quarterly** by the team
- **Updated** when new technologies are adopted
- **Referenced** in code reviews
- **Shared** with new team members during onboarding
- **Enforced** through automated tools

For questions or suggestions, contact: [TEAM/ARCHITECT EMAIL]
