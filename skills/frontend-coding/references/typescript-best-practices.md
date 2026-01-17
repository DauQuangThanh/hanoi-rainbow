# TypeScript Best Practices


### Type Safety and Inference

```typescript
// Utility Types
type Nullable<T> = T | null;
type Optional<T> = T | undefined;
type DeepPartial<T> = {
  [P in keyof T]?: T[P] extends object ? DeepPartial<T[P]> : T[P];
};

// Generic API Response
interface ApiResponse<T> {
  data: T;
  status: number;
  message: string;
}

async function fetchApi<T>(url: string): Promise<ApiResponse<T>> {
  const response = await fetch(url);
  return response.json();
}

// Usage with type inference
const userResponse = await fetchApi<User>('/api/users/1');
// userResponse.data is typed as User

// Discriminated Unions
type Result<T, E = Error> =
  | { success: true; data: T }
  | { success: false; error: E };

function divide(a: number, b: number): Result<number> {
  if (b === 0) {
    return { success: false, error: new Error('Division by zero') };
  }
  return { success: true, data: a / b };
}

const result = divide(10, 2);
if (result.success) {
  console.log(result.data); // TypeScript knows this exists
} else {
  console.error(result.error); // TypeScript knows this exists
}

// Type Guards
function isUser(value: unknown): value is User {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'name' in value &&
    'email' in value
  );
}

function processData(data: unknown) {
  if (isUser(data)) {
    // TypeScript knows data is User here
    console.log(data.name);
  }
}

// Const Assertions
const ROUTES = {
  HOME: '/',
  DASHBOARD: '/dashboard',
  PROFILE: '/profile'
} as const;

type Route = typeof ROUTES[keyof typeof ROUTES];
// Route = '/' | '/dashboard' | '/profile'

// Template Literal Types
type HTTPMethod = 'GET' | 'POST' | 'PUT' | 'DELETE';
type Endpoint = `/api/${string}`;
type HTTPRequest = `${HTTPMethod} ${Endpoint}`;

const request: HTTPRequest = 'GET /api/users'; // Valid
// const invalid: HTTPRequest = 'GET /users'; // Error
```

### Advanced Patterns

```typescript
// Builder Pattern with Fluent API
class QueryBuilder<T> {
  private filters: Array<(item: T) => boolean> = [];
  private sortFn?: (a: T, b: T) => number;
  private limitValue?: number;

  where(predicate: (item: T) => boolean): this {
    this.filters.push(predicate);
    return this;
  }

  sortBy(fn: (a: T, b: T) => number): this {
    this.sortFn = fn;
    return this;
  }

  limit(n: number): this {
    this.limitValue = n;
    return this;
  }

  execute(data: T[]): T[] {
    let result = data;

    // Apply filters
    for (const filter of this.filters) {
      result = result.filter(filter);
    }

    // Apply sort
    if (this.sortFn) {
      result = [...result].sort(this.sortFn);
    }

    // Apply limit
    if (this.limitValue) {
      result = result.slice(0, this.limitValue);
    }

    return result;
  }
}

// Usage
const query = new QueryBuilder<User>()
  .where(user => user.role === 'admin')
  .where(user => user.age > 18)
  .sortBy((a, b) => a.name.localeCompare(b.name))
  .limit(10);

const results = query.execute(users);

// Mapped Types
type ReadOnly<T> = {
  readonly [P in keyof T]: T[P];
};

type Mutable<T> = {
  -readonly [P in keyof T]: T[P];
};

// Conditional Types
type IsArray<T> = T extends any[] ? true : false;
type ArrayElement<T> = T extends (infer E)[] ? E : never;

// Example
type X = IsArray<number[]>; // true
type Y = IsArray<string>;   // false
type Z = ArrayElement<User[]>; // User
```
