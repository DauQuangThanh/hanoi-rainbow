# Code Quality Checklist

Comprehensive criteria for assessing frontend code quality.

## Table of Contents
- [Component Structure](#component-structure)
- [JavaScript/TypeScript Quality](#javascripttypescript-quality)
- [State Management](#state-management)
- [Common Issues and Fixes](#common-issues-and-fixes)
- [Code Smells](#code-smells)

## Component Structure

### React Components

#### Single Responsibility Principle

**Good:**
```jsx
// ✅ Each component has one clear purpose
function UserAvatar({ user }) {
  return <img src={user.avatar} alt={user.name} />;
}

function UserName({ user }) {
  return <span>{user.name}</span>;
}

function UserProfile({ user }) {
  return (
    <div>
      <UserAvatar user={user} />
      <UserName user={user} />
    </div>
  );
}
```

**Bad:**
```jsx
// ❌ Component doing too much
function UserProfile({ user }) {
  const [posts, setPosts] = useState([]);
  const [comments, setComments] = useState([]);
  const [settings, setSettings] = useState({});
  
  // Fetching data, rendering UI, handling forms, etc.
  // 300+ lines of mixed concerns
}
```

#### Component Composition

**Good:**
```jsx
// ✅ Composition over inheritance
function Card({ children, className }) {
  return <div className={`card ${className}`}>{children}</div>;
}

function ProductCard({ product }) {
  return (
    <Card className="product-card">
      <CardHeader>{product.name}</CardHeader>
      <CardBody>{product.description}</CardBody>
      <CardFooter>{product.price}</CardFooter>
    </Card>
  );
}
```

**Bad:**
```jsx
// ❌ Trying to use inheritance
class BaseCard extends React.Component { /* ... */ }
class ProductCard extends BaseCard { /* ... */ }
class UserCard extends BaseCard { /* ... */ }
```

#### Props Design

**Good:**
```tsx
// ✅ Well-typed, specific props
interface ButtonProps {
  variant: 'primary' | 'secondary' | 'danger';
  size: 'small' | 'medium' | 'large';
  onClick: () => void;
  disabled?: boolean;
  children: React.ReactNode;
}

function Button({ variant, size, onClick, disabled, children }: ButtonProps) {
  return (
    <button 
      className={`btn btn-${variant} btn-${size}`}
      onClick={onClick}
      disabled={disabled}
    >
      {children}
    </button>
  );
}
```

**Bad:**
```jsx
// ❌ Generic, untyped props
function Button(props) {
  return <button {...props} />;
}
```

### Vue Components

#### Component Organization

**Good:**
```vue
<script setup lang="ts">
// ✅ Clear organization: imports → types → props → composables → computed → methods
import { ref, computed } from 'vue'
import type { User } from '@/types'

interface Props {
  user: User
}

const props = defineProps<Props>()
const isEditing = ref(false)

const displayName = computed(() => 
  `${props.user.firstName} ${props.user.lastName}`
)

function toggleEdit() {
  isEditing.value = !isEditing.value
}
</script>

<template>
  <div class="user-profile">
    <span>{{ displayName }}</span>
    <button @click="toggleEdit">Edit</button>
  </div>
</template>
```

**Bad:**
```vue
<script>
// ❌ Mixed concerns, no types, unclear organization
export default {
  data() {
    return {
      user: null,
      editing: false,
      loading: false,
      error: null,
      // 50+ data properties
    }
  },
  // Methods, computed, watch all mixed
}
</script>
```

### Angular Components

**Good:**
```typescript
// ✅ OnPush strategy, typed, clear separation
@Component({
  selector: 'app-user-profile',
  templateUrl: './user-profile.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true
})
export class UserProfileComponent {
  @Input() user!: User;
  @Output() userUpdated = new EventEmitter<User>();
  
  protected isEditing = signal(false);
  
  protected toggleEdit(): void {
    this.isEditing.update(v => !v);
  }
}
```

**Bad:**
```typescript
// ❌ Default change detection, any types, mixed concerns
@Component({
  selector: 'app-user-profile',
  template: `...` // Inline template for complex component
})
export class UserProfileComponent {
  user: any;
  data: any;
  state: any;
  
  // 50+ methods handling everything
}
```

## JavaScript/TypeScript Quality

### Type Safety (TypeScript)

**Good:**
```typescript
// ✅ Strict types, no any
interface User {
  id: string;
  name: string;
  email: string;
  age: number;
  roles: Role[];
}

type Role = 'admin' | 'user' | 'guest';

function getUserRole(user: User): Role {
  return user.roles[0] ?? 'guest';
}

// Discriminated unions for state
type RequestState<T> =
  | { status: 'idle' }
  | { status: 'loading' }
  | { status: 'success'; data: T }
  | { status: 'error'; error: Error };
```

**Bad:**
```typescript
// ❌ Using any, loose types
function getUserRole(user: any): any {
  return user.roles[0];
}

// No discriminated unions
interface RequestState {
  status: string;
  data?: any;
  error?: any;
}
```

### Function Complexity

**Good:**
```javascript
// ✅ Small, focused functions (< 50 lines, cyclomatic complexity < 10)
function validateEmail(email) {
  return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
}

function validateUser(user) {
  const errors = [];
  
  if (!validateEmail(user.email)) {
    errors.push('Invalid email');
  }
  
  if (user.age < 18) {
    errors.push('Must be 18 or older');
  }
  
  return errors.length === 0 ? null : errors;
}
```

**Bad:**
```javascript
// ❌ Large, complex function (100+ lines, high cyclomatic complexity)
function validateAndProcessUser(user) {
  // Validation
  if (!user.email || !user.email.includes('@')) {
    // Email validation
    if (user.email.split('@').length !== 2) {
      if (!user.email.split('@')[1].includes('.')) {
        // Nested conditions continue...
        // 100+ lines of nested logic
      }
    }
  }
  
  // Processing
  // More nested logic
  
  // Saving
  // Even more nested logic
}
```

### Error Handling

**Good:**
```typescript
// ✅ Proper error handling with types
async function fetchUser(id: string): Promise<User> {
  try {
    const response = await fetch(`/api/users/${id}`);
    
    if (!response.ok) {
      throw new Error(`Failed to fetch user: ${response.statusText}`);
    }
    
    return await response.json();
  } catch (error) {
    if (error instanceof Error) {
      console.error('Error fetching user:', error.message);
    }
    throw error;
  }
}

// Usage with error boundary
function UserProfile({ userId }: { userId: string }) {
  const { data, error, isLoading } = useQuery(['user', userId], () => fetchUser(userId));
  
  if (isLoading) return <Spinner />;
  if (error) return <ErrorMessage error={error} />;
  if (!data) return <NotFound />;
  
  return <UserDetails user={data} />;
}
```

**Bad:**
```javascript
// ❌ No error handling, ignoring promises
async function fetchUser(id) {
  const response = fetch(`/api/users/${id}`); // Not awaited!
  return response.json(); // Will fail
}

function UserProfile({ userId }) {
  const [user, setUser] = useState();
  
  fetchUser(userId).then(setUser); // No error handling
  
  return <UserDetails user={user} />; // Will crash if user is undefined
}
```

### Code Duplication (DRY)

**Good:**
```typescript
// ✅ DRY - extracted common logic
function useFormField<T>(initialValue: T) {
  const [value, setValue] = useState<T>(initialValue);
  const [error, setError] = useState<string | null>(null);
  
  const validate = (validator: (val: T) => string | null) => {
    const validationError = validator(value);
    setError(validationError);
    return !validationError;
  };
  
  return { value, setValue, error, validate };
}

// Reused across multiple forms
function LoginForm() {
  const email = useFormField('');
  const password = useFormField('');
  
  const handleSubmit = () => {
    const isValid = 
      email.validate(validateEmail) &&
      password.validate(validatePassword);
      
    if (isValid) {
      // Submit
    }
  };
  
  return (/* ... */);
}
```

**Bad:**
```typescript
// ❌ Repeated code across components
function LoginForm() {
  const [email, setEmail] = useState('');
  const [emailError, setEmailError] = useState(null);
  const [password, setPassword] = useState('');
  const [passwordError, setPasswordError] = useState(null);
  
  // Validation logic repeated
  const validateEmail = () => { /* ... */ };
  const validatePassword = () => { /* ... */ };
}

function SignupForm() {
  const [email, setEmail] = useState('');
  const [emailError, setEmailError] = useState(null);
  const [password, setPassword] = useState('');
  const [passwordError, setPasswordError] = useState(null);
  
  // Same validation logic repeated
  const validateEmail = () => { /* ... */ };
  const validatePassword = () => { /* ... */ };
}
```

## State Management

### Local vs Global State

**Good:**
```typescript
// ✅ Local state for UI, global for shared data
// Component-specific UI state
function Accordion() {
  const [isOpen, setIsOpen] = useState(false); // Local
  
  return (/* ... */);
}

// Shared data in global store
const useAuthStore = create<AuthStore>((set) => ({
  user: null,
  isAuthenticated: false,
  login: async (credentials) => {
    const user = await authApi.login(credentials);
    set({ user, isAuthenticated: true });
  },
  logout: () => set({ user: null, isAuthenticated: false }),
}));
```

**Bad:**
```typescript
// ❌ Everything in global store, including UI state
const useStore = create((set) => ({
  accordionOpen: false, // Should be local!
  modalOpen: false, // Should be local!
  tooltipVisible: false, // Should be local!
  user: null,
  // 100+ state properties
}));
```

### Immutability

**Good:**
```typescript
// ✅ Immutable updates
function todosReducer(state: Todo[], action: Action): Todo[] {
  switch (action.type) {
    case 'ADD_TODO':
      return [...state, action.payload];
    
    case 'TOGGLE_TODO':
      return state.map(todo =>
        todo.id === action.payload
          ? { ...todo, completed: !todo.completed }
          : todo
      );
    
    case 'DELETE_TODO':
      return state.filter(todo => todo.id !== action.payload);
    
    default:
      return state;
  }
}
```

**Bad:**
```javascript
// ❌ Mutating state directly
function todosReducer(state, action) {
  switch (action.type) {
    case 'ADD_TODO':
      state.push(action.payload); // Mutation!
      return state;
    
    case 'TOGGLE_TODO':
      const todo = state.find(t => t.id === action.payload);
      todo.completed = !todo.completed; // Mutation!
      return state;
    
    default:
      return state;
  }
}
```

## Common Issues and Fixes

### Issue 1: Unnecessary Re-renders

**Problem:**
```jsx
// ❌ Creating new objects/arrays in render
function TodoList({ todos }) {
  return (
    <ul>
      {todos.map(todo => (
        <TodoItem 
          key={todo.id}
          todo={todo}
          onUpdate={() => updateTodo(todo)} // New function every render!
        />
      ))}
    </ul>
  );
}
```

**Solution:**
```jsx
// ✅ Memoize callbacks, use React.memo
const TodoItem = React.memo(({ todo, onUpdate }) => {
  return <li onClick={onUpdate}>{todo.text}</li>;
});

function TodoList({ todos }) {
  const handleUpdate = useCallback((todo) => {
    updateTodo(todo);
  }, []);
  
  return (
    <ul>
      {todos.map(todo => (
        <TodoItem 
          key={todo.id}
          todo={todo}
          onUpdate={() => handleUpdate(todo)}
        />
      ))}
    </ul>
  );
}
```

### Issue 2: Missing Cleanup

**Problem:**
```jsx
// ❌ No cleanup for subscriptions/timers
function Timer() {
  const [count, setCount] = useState(0);
  
  useEffect(() => {
    const interval = setInterval(() => {
      setCount(c => c + 1);
    }, 1000);
    // Missing cleanup!
  }, []);
  
  return <div>{count}</div>;
}
```

**Solution:**
```jsx
// ✅ Proper cleanup
function Timer() {
  const [count, setCount] = useState(0);
  
  useEffect(() => {
    const interval = setInterval(() => {
      setCount(c => c + 1);
    }, 1000);
    
    return () => clearInterval(interval); // Cleanup
  }, []);
  
  return <div>{count}</div>;
}
```

### Issue 3: Prop Drilling

**Problem:**
```jsx
// ❌ Passing props through many levels
function App() {
  const [user, setUser] = useState(null);
  return <Layout user={user} setUser={setUser} />;
}

function Layout({ user, setUser }) {
  return <Sidebar user={user} setUser={setUser} />;
}

function Sidebar({ user, setUser }) {
  return <UserMenu user={user} setUser={setUser} />;
}

function UserMenu({ user, setUser }) {
  // Finally using the props here
}
```

**Solution:**
```jsx
// ✅ Use Context or state management
const UserContext = createContext();

function App() {
  const [user, setUser] = useState(null);
  
  return (
    <UserContext.Provider value={{ user, setUser }}>
      <Layout />
    </UserContext.Provider>
  );
}

function Layout() {
  return <Sidebar />;
}

function Sidebar() {
  return <UserMenu />;
}

function UserMenu() {
  const { user, setUser } = useContext(UserContext);
  // Use directly
}
```

## Code Smells

### Smell 1: God Component

**Indicator:** Component with 300+ lines, handling multiple concerns

**Fix:** Split into smaller, focused components using composition

### Smell 2: Deep Nesting

**Indicator:** More than 4 levels of nested JSX or logic

**Fix:** Extract nested sections into separate components

### Smell 3: Magic Numbers

**Indicator:** Hardcoded numbers without explanation

**Fix:** Extract to named constants with clear meaning

```typescript
// ❌ Magic numbers
setTimeout(() => { /* ... */ }, 300);
if (items.length > 50) { /* ... */ }

// ✅ Named constants
const DEBOUNCE_DELAY = 300;
const MAX_ITEMS_PER_PAGE = 50;

setTimeout(() => { /* ... */ }, DEBOUNCE_DELAY);
if (items.length > MAX_ITEMS_PER_PAGE) { /* ... */ }
```

### Smell 4: Boolean Parameters

**Indicator:** Multiple boolean parameters making calls unclear

**Fix:** Use configuration objects or discriminated unions

```typescript
// ❌ Unclear boolean parameters
renderButton(true, false, true);

// ✅ Clear configuration object
renderButton({
  primary: true,
  disabled: false,
  loading: true
});
```

### Smell 5: Inconsistent Naming

**Indicator:** Mixed conventions (camelCase, snake_case, PascalCase)

**Fix:** Follow consistent conventions:
- Components: PascalCase (UserProfile)
- Variables/functions: camelCase (getUserProfile)
- Constants: UPPER_SNAKE_CASE (API_BASE_URL)
- CSS classes: kebab-case (user-profile)
