# Vue.js Patterns and Best Practices

This reference provides advanced Vue.js patterns, Composition API techniques, and architectural approaches for building scalable Vue applications.

## Composition API Patterns

### Composables Organization

```typescript
// composables/useUser.ts
import { ref, computed, Ref } from 'vue';

interface User {
  id: string;
  name: string;
  email: string;
  role: string;
}

export function useUser() {
  const user = ref<User | null>(null);
  const loading = ref(false);
  const error = ref<Error | null>(null);

  const isAdmin = computed(() => user.value?.role === 'admin');
  const displayName = computed(() => user.value?.name || 'Guest');

  async function fetchUser(id: string) {
    loading.value = true;
    error.value = null;

    try {
      const response = await fetch(`/api/users/${id}`);
      if (!response.ok) throw new Error('Failed to fetch user');
      user.value = await response.json();
    } catch (err) {
      error.value = err instanceof Error ? err : new Error('Unknown error');
    } finally {
      loading.value = false;
    }
  }

  async function updateUser(updates: Partial<User>) {
    if (!user.value) return;

    try {
      const response = await fetch(`/api/users/${user.value.id}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(updates)
      });

      if (!response.ok) throw new Error('Failed to update user');
      user.value = await response.json();
    } catch (err) {
      error.value = err instanceof Error ? err : new Error('Unknown error');
      throw err;
    }
  }

  function logout() {
    user.value = null;
  }

  return {
    user,
    loading,
    error,
    isAdmin,
    displayName,
    fetchUser,
    updateUser,
    logout
  };
}
```

### Generic Fetch Composable

```typescript
// composables/useFetch.ts
import { ref, Ref, watchEffect } from 'vue';

interface UseFetchOptions<T> {
  immediate?: boolean;
  onSuccess?: (data: T) => void;
  onError?: (error: Error) => void;
}

interface UseFetchReturn<T> {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<Error | null>;
  execute: () => Promise<void>;
  refresh: () => Promise<void>;
}

export function useFetch<T>(
  url: string | Ref<string>,
  options: UseFetchOptions<T> = {}
): UseFetchReturn<T> {
  const data = ref<T | null>(null);
  const loading = ref(false);
  const error = ref<Error | null>(null);

  const execute = async () => {
    loading.value = true;
    error.value = null;

    try {
      const urlValue = typeof url === 'string' ? url : url.value;
      const response = await fetch(urlValue);

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      const result = await response.json();
      data.value = result;
      options.onSuccess?.(result);
    } catch (err) {
      const errorObj = err instanceof Error ? err : new Error('Unknown error');
      error.value = errorObj;
      options.onError?.(errorObj);
    } finally {
      loading.value = false;
    }
  };

  const refresh = execute;

  // Auto-execute on URL change
  if (typeof url !== 'string') {
    watchEffect(() => {
      if (options.immediate !== false) {
        execute();
      }
    });
  } else if (options.immediate !== false) {
    execute();
  }

  return {
    data,
    loading,
    error,
    execute,
    refresh
  };
}

// Usage
<script setup lang="ts">
import { ref } from 'vue';
import { useFetch } from '@/composables/useFetch';

const userId = ref('123');
const { data: user, loading, error, refresh } = useFetch<User>(
  computed(() => `/api/users/${userId.value}`),
  {
    onSuccess: (user) => console.log('User loaded:', user),
    onError: (err) => console.error('Failed to load user:', err)
  }
);
</script>

<template>
  <div>
    <div v-if="loading">Loading...</div>
    <div v-else-if="error">Error: {{ error.message }}</div>
    <div v-else-if="user">
      <h2>{{ user.name }}</h2>
      <button @click="refresh">Refresh</button>
    </div>
  </div>
</template>
```

### Form Validation Composable

```typescript
// composables/useForm.ts
import { ref, reactive, computed, Ref } from 'vue';

type ValidationRule<T> = (value: T) => string | undefined;

interface FieldConfig<T> {
  value: T;
  rules?: ValidationRule<T>[];
}

interface FormConfig {
  [key: string]: FieldConfig<any>;
}

export function useForm<T extends FormConfig>(config: T) {
  type FormValues = { [K in keyof T]: T[K]['value'] };
  type FormErrors = Partial<Record<keyof T, string>>;
  type FormTouched = Partial<Record<keyof T, boolean>>;

  const values = reactive<FormValues>({} as FormValues);
  const errors = reactive<FormErrors>({});
  const touched = reactive<FormTouched>({});
  const isSubmitting = ref(false);

  // Initialize values
  for (const key in config) {
    values[key] = config[key].value;
  }

  function validateField(name: keyof T): string | undefined {
    const field = config[name];
    if (!field.rules) return undefined;

    for (const rule of field.rules) {
      const error = rule(values[name]);
      if (error) return error;
    }
    return undefined;
  }

  function validateAll(): boolean {
    let isValid = true;

    for (const key in config) {
      const error = validateField(key);
      if (error) {
        errors[key] = error;
        isValid = false;
      } else {
        delete errors[key];
      }
    }

    return isValid;
  }

  function handleBlur(name: keyof T) {
    touched[name] = true;
    const error = validateField(name);
    if (error) {
      errors[name] = error;
    } else {
      delete errors[name];
    }
  }

  function handleChange(name: keyof T, value: any) {
    values[name] = value;

    if (touched[name]) {
      const error = validateField(name);
      if (error) {
        errors[name] = error;
      } else {
        delete errors[name];
      }
    }
  }

  async function handleSubmit(onSubmit: (values: FormValues) => void | Promise<void>) {
    // Mark all as touched
    for (const key in config) {
      touched[key] = true;
    }

    if (!validateAll()) {
      return;
    }

    isSubmitting.value = true;
    try {
      await onSubmit(values);
    } finally {
      isSubmitting.value = false;
    }
  }

  function resetForm() {
    for (const key in config) {
      values[key] = config[key].value;
      delete errors[key];
      delete touched[key];
    }
    isSubmitting.value = false;
  }

  const isValid = computed(() => Object.keys(errors).length === 0);

  return {
    values,
    errors,
    touched,
    isSubmitting,
    isValid,
    handleChange,
    handleBlur,
    handleSubmit,
    resetForm,
    validateAll
  };
}

// Validation rules
export const required = (message = 'This field is required') =>
  (value: any) => value ? undefined : message;

export const email = (message = 'Invalid email address') =>
  (value: string) => /\S+@\S+\.\S+/.test(value) ? undefined : message;

export const minLength = (min: number, message?: string) =>
  (value: string) =>
    value.length >= min
      ? undefined
      : message || `Must be at least ${min} characters`;

export const maxLength = (max: number, message?: string) =>
  (value: string) =>
    value.length <= max
      ? undefined
      : message || `Must be at most ${max} characters`;

export const pattern = (regex: RegExp, message: string) =>
  (value: string) => regex.test(value) ? undefined : message;

// Usage
<script setup lang="ts">
import { useForm, required, email, minLength } from '@/composables/useForm';

const form = useForm({
  name: {
    value: '',
    rules: [required(), minLength(2, 'Name must be at least 2 characters')]
  },
  email: {
    value: '',
    rules: [required(), email()]
  },
  password: {
    value: '',
    rules: [required(), minLength(8, 'Password must be at least 8 characters')]
  }
});

async function onSubmit() {
  console.log('Submitting:', form.values);
  await fetch('/api/signup', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(form.values)
  });
}
</script>

<template>
  <form @submit.prevent="form.handleSubmit(onSubmit)">
    <div>
      <input
        :value="form.values.name"
        @input="e => form.handleChange('name', e.target.value)"
        @blur="() => form.handleBlur('name')"
        placeholder="Name"
      />
      <span v-if="form.touched.name && form.errors.name" class="error">
        {{ form.errors.name }}
      </span>
    </div>

    <div>
      <input
        type="email"
        :value="form.values.email"
        @input="e => form.handleChange('email', e.target.value)"
        @blur="() => form.handleBlur('email')"
        placeholder="Email"
      />
      <span v-if="form.touched.email && form.errors.email" class="error">
        {{ form.errors.email }}
      </span>
    </div>

    <div>
      <input
        type="password"
        :value="form.values.password"
        @input="e => form.handleChange('password', e.target.value)"
        @blur="() => form.handleBlur('password')"
        placeholder="Password"
      />
      <span v-if="form.touched.password && form.errors.password" class="error">
        {{ form.errors.password }}
      </span>
    </div>

    <button type="submit" :disabled="form.isSubmitting || !form.isValid">
      {{ form.isSubmitting ? 'Submitting...' : 'Submit' }}
    </button>
  </form>
</template>
```

### Pagination Composable

```typescript
// composables/usePagination.ts
import { ref, computed, Ref } from 'vue';

interface UsePaginationOptions {
  initialPage?: number;
  initialPageSize?: number;
}

export function usePagination<T>(
  items: Ref<T[]>,
  options: UsePaginationOptions = {}
) {
  const currentPage = ref(options.initialPage || 1);
  const pageSize = ref(options.initialPageSize || 10);

  const totalItems = computed(() => items.value.length);
  const totalPages = computed(() => Math.ceil(totalItems.value / pageSize.value));

  const paginatedItems = computed(() => {
    const start = (currentPage.value - 1) * pageSize.value;
    const end = start + pageSize.value;
    return items.value.slice(start, end);
  });

  const hasNext = computed(() => currentPage.value < totalPages.value);
  const hasPrev = computed(() => currentPage.value > 1);

  function nextPage() {
    if (hasNext.value) {
      currentPage.value++;
    }
  }

  function prevPage() {
    if (hasPrev.value) {
      currentPage.value--;
    }
  }

  function goToPage(page: number) {
    if (page >= 1 && page <= totalPages.value) {
      currentPage.value = page;
    }
  }

  function setPageSize(size: number) {
    pageSize.value = size;
    currentPage.value = 1; // Reset to first page
  }

  const pageNumbers = computed(() => {
    const pages: number[] = [];
    const maxVisible = 7;

    if (totalPages.value <= maxVisible) {
      for (let i = 1; i <= totalPages.value; i++) {
        pages.push(i);
      }
    } else {
      pages.push(1);

      let start = Math.max(2, currentPage.value - 2);
      let end = Math.min(totalPages.value - 1, currentPage.value + 2);

      if (start > 2) pages.push(-1); // Ellipsis

      for (let i = start; i <= end; i++) {
        pages.push(i);
      }

      if (end < totalPages.value - 1) pages.push(-1); // Ellipsis

      pages.push(totalPages.value);
    }

    return pages;
  });

  return {
    currentPage,
    pageSize,
    totalItems,
    totalPages,
    paginatedItems,
    hasNext,
    hasPrev,
    pageNumbers,
    nextPage,
    prevPage,
    goToPage,
    setPageSize
  };
}

// Usage
<script setup lang="ts">
import { ref } from 'vue';
import { usePagination } from '@/composables/usePagination';

const allUsers = ref<User[]>([/* ... many users ... */]);
const pagination = usePagination(allUsers, {
  initialPage: 1,
  initialPageSize: 20
});
</script>

<template>
  <div>
    <div class="users-list">
      <UserCard
        v-for="user in pagination.paginatedItems.value"
        :key="user.id"
        :user="user"
      />
    </div>

    <div class="pagination">
      <button
        @click="pagination.prevPage"
        :disabled="!pagination.hasPrev.value"
      >
        Previous
      </button>

      <button
        v-for="page in pagination.pageNumbers.value"
        :key="page"
        @click="() => page > 0 && pagination.goToPage(page)"
        :class="{ active: page === pagination.currentPage.value }"
        :disabled="page === -1"
      >
        {{ page === -1 ? '...' : page }}
      </button>

      <button
        @click="pagination.nextPage"
        :disabled="!pagination.hasNext.value"
      >
        Next
      </button>
    </div>

    <div class="page-size-selector">
      <select
        :value="pagination.pageSize.value"
        @change="e => pagination.setPageSize(Number(e.target.value))"
      >
        <option :value="10">10 per page</option>
        <option :value="20">20 per page</option>
        <option :value="50">50 per page</option>
        <option :value="100">100 per page</option>
      </select>
    </div>

    <div class="pagination-info">
      Showing {{ (pagination.currentPage.value - 1) * pagination.pageSize.value + 1 }}
      to {{ Math.min(pagination.currentPage.value * pagination.pageSize.value, pagination.totalItems.value) }}
      of {{ pagination.totalItems.value }} items
    </div>
  </div>
</template>
```

## Pinia Advanced Patterns

### Store Composition

```typescript
// stores/modules/auth.ts
import { defineStore } from 'pinia';
import { ref, computed } from 'vue';

export const useAuthStore = defineStore('auth', () => {
  const user = ref<User | null>(null);
  const token = ref<string | null>(null);
  const loading = ref(false);

  const isAuthenticated = computed(() => !!token.value);
  const isAdmin = computed(() => user.value?.role === 'admin');

  async function login(email: string, password: string) {
    loading.value = true;
    try {
      const response = await fetch('/api/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password })
      });

      const data = await response.json();
      user.value = data.user;
      token.value = data.token;

      // Store token in localStorage
      localStorage.setItem('auth_token', data.token);
    } finally {
      loading.value = false;
    }
  }

  async function logout() {
    user.value = null;
    token.value = null;
    localStorage.removeItem('auth_token');
  }

  async function checkAuth() {
    const storedToken = localStorage.getItem('auth_token');
    if (!storedToken) return;

    try {
      const response = await fetch('/api/auth/me', {
        headers: { Authorization: `Bearer ${storedToken}` }
      });

      if (response.ok) {
        user.value = await response.json();
        token.value = storedToken;
      } else {
        logout();
      }
    } catch (error) {
      logout();
    }
  }

  return {
    user,
    token,
    loading,
    isAuthenticated,
    isAdmin,
    login,
    logout,
    checkAuth
  };
});

// stores/products.ts
import { defineStore } from 'pinia';
import { ref, computed } from 'vue';
import { useAuthStore } from './modules/auth';

export const useProductsStore = defineStore('products', () => {
  const authStore = useAuthStore();

  const products = ref<Product[]>([]);
  const loading = ref(false);
  const error = ref<string | null>(null);

  const featuredProducts = computed(() =>
    products.value.filter(p => p.featured)
  );

  const canEdit = computed(() => authStore.isAdmin);

  async function fetchProducts() {
    loading.value = true;
    error.value = null;

    try {
      const response = await fetch('/api/products', {
        headers: authStore.token
          ? { Authorization: `Bearer ${authStore.token}` }
          : {}
      });

      products.value = await response.json();
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Failed to fetch';
    } finally {
      loading.value = false;
    }
  }

  async function addProduct(product: Omit<Product, 'id'>) {
    if (!canEdit.value) {
      throw new Error('Unauthorized');
    }

    const response = await fetch('/api/products', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `Bearer ${authStore.token}`
      },
      body: JSON.stringify(product)
    });

    const newProduct = await response.json();
    products.value.push(newProduct);
    return newProduct;
  }

  return {
    products,
    loading,
    error,
    featuredProducts,
    canEdit,
    fetchProducts,
    addProduct
  };
});
```

### Store Plugins

```typescript
// stores/plugins/persistence.ts
import { PiniaPluginContext } from 'pinia';

export function persistencePlugin({ store }: PiniaPluginContext) {
  const storageKey = `pinia-${store.$id}`;

  // Restore state from localStorage
  const savedState = localStorage.getItem(storageKey);
  if (savedState) {
    store.$patch(JSON.parse(savedState));
  }

  // Save state to localStorage on change
  store.$subscribe((mutation, state) => {
    localStorage.setItem(storageKey, JSON.stringify(state));
  });
}

// stores/plugins/logger.ts
export function loggerPlugin({ store }: PiniaPluginContext) {
  store.$subscribe((mutation, state) => {
    console.log(`[${store.$id}] ${mutation.type}`, mutation.payload);
  });

  // Log actions
  store.$onAction(({ name, args, after, onError }) => {
    const startTime = Date.now();
    console.log(`[${store.$id}] Action "${name}" started with args:`, args);

    after(() => {
      console.log(
        `[${store.$id}] Action "${name}" completed in ${Date.now() - startTime}ms`
      );
    });

    onError((error) => {
      console.error(`[${store.$id}] Action "${name}" failed:`, error);
    });
  });
}

// main.ts
import { createPinia } from 'pinia';
import { persistencePlugin } from './stores/plugins/persistence';
import { loggerPlugin } from './stores/plugins/logger';

const pinia = createPinia();
pinia.use(persistencePlugin);
if (import.meta.env.DEV) {
  pinia.use(loggerPlugin);
}

app.use(pinia);
```

## Component Patterns

### Provide/Inject Pattern

```typescript
// Parent Component
<script setup lang="ts">
import { provide, ref } from 'vue';

interface Theme {
  primaryColor: string;
  secondaryColor: string;
  fontSize: number;
}

const theme = ref<Theme>({
  primaryColor: '#007bff',
  secondaryColor: '#6c757d',
  fontSize: 16
});

function updateTheme(updates: Partial<Theme>) {
  theme.value = { ...theme.value, ...updates };
}

// Provide to all descendants
provide('theme', theme);
provide('updateTheme', updateTheme);
</script>

<template>
  <div class="app" :style="{ fontSize: theme.fontSize + 'px' }">
    <Header />
    <MainContent />
    <Footer />
  </div>
</template>

// Child Component (any level deep)
<script setup lang="ts">
import { inject, Ref } from 'vue';

const theme = inject<Ref<Theme>>('theme');
const updateTheme = inject<(updates: Partial<Theme>) => void>('updateTheme');

if (!theme || !updateTheme) {
  throw new Error('Theme context not provided');
}

function increaseFontSize() {
  updateTheme({ fontSize: theme.value.fontSize + 2 });
}
</script>

<template>
  <div :style="{ color: theme.primaryColor }">
    <p>Themed content</p>
    <button @click="increaseFontSize">Increase Font Size</button>
  </div>
</template>

// Typed Injection Keys
// types/injectionKeys.ts
import { InjectionKey, Ref } from 'vue';

export const themeKey: InjectionKey<Ref<Theme>> = Symbol('theme');
export const updateThemeKey: InjectionKey<(updates: Partial<Theme>) => void> =
  Symbol('updateTheme');

// Usage with typed keys
provide(themeKey, theme);
provide(updateThemeKey, updateTheme);

const theme = inject(themeKey); // Type is Ref<Theme> | undefined
const updateTheme = inject(updateThemeKey);
```

### Slot Patterns

```typescript
// Basic Slots
<script setup lang="ts">
interface Props {
  title: string;
}

defineProps<Props>();
</script>

<template>
  <div class="card">
    <header class="card-header">
      <h2>{{ title }}</h2>
      <slot name="header-actions" />
    </header>

    <div class="card-body">
      <slot /> <!-- Default slot -->
    </div>

    <footer class="card-footer">
      <slot name="footer" />
    </footer>
  </div>
</template>

<!-- Usage -->
<Card title="User Profile">
  <template #header-actions>
    <button>Edit</button>
  </template>

  <p>User content goes here</p>

  <template #footer>
    <button>Save</button>
    <button>Cancel</button>
  </template>
</Card>

// Scoped Slots
<script setup lang="ts">
interface Item {
  id: string;
  name: string;
}

interface Props {
  items: Item[];
}

const props = defineProps<Props>();
</script>

<template>
  <ul class="list">
    <li v-for="item in items" :key="item.id">
      <slot name="item" :item="item" :index="index">
        <!-- Fallback content -->
        {{ item.name }}
      </slot>
    </li>
  </ul>
</template>

<!-- Usage with scoped slot -->
<List :items="users">
  <template #item="{ item, index }">
    <div class="user-item">
      <span>{{ index + 1 }}. {{ item.name }}</span>
      <button @click="editUser(item)">Edit</button>
    </div>
  </template>
</List>

// Renderless Component
<script setup lang="ts">
import { ref, computed } from 'vue';

interface Props {
  initialValue?: number;
  min?: number;
  max?: number;
  step?: number;
}

const props = withDefaults(defineProps<Props>(), {
  initialValue: 0,
  min: 0,
  max: 100,
  step: 1
});

const value = ref(props.initialValue);

const canIncrement = computed(() => value.value < props.max);
const canDecrement = computed(() => value.value > props.min);

function increment() {
  if (canIncrement.value) {
    value.value = Math.min(value.value + props.step, props.max);
  }
}

function decrement() {
  if (canDecrement.value) {
    value.value = Math.max(value.value - props.step, props.min);
  }
}

function reset() {
  value.value = props.initialValue;
}
</script>

<template>
  <slot
    :value="value"
    :canIncrement="canIncrement"
    :canDecrement="canDecrement"
    :increment="increment"
    :decrement="decrement"
    :reset="reset"
  />
</template>

<!-- Usage - completely custom UI -->
<Counter :min="0" :max="10" :step="2">
  <template #default="{ value, increment, decrement, canIncrement, canDecrement }">
    <div class="custom-counter">
      <button @click="decrement" :disabled="!canDecrement">-</button>
      <span class="value">{{ value }}</span>
      <button @click="increment" :disabled="!canIncrement">+</button>
    </div>
  </template>
</Counter>
```

### Dynamic Components

```typescript
<script setup lang="ts">
import { ref, Component, shallowRef } from 'vue';
import UserProfile from './UserProfile.vue';
import Settings from './Settings.vue';
import Dashboard from './Dashboard.vue';

interface Tab {
  name: string;
  component: Component;
  props?: Record<string, any>;
}

const tabs: Tab[] = [
  { name: 'Dashboard', component: Dashboard },
  { name: 'Profile', component: UserProfile, props: { userId: '123' } },
  { name: 'Settings', component: Settings }
];

const currentTab = ref(0);
const currentComponent = shallowRef(tabs[0].component);
const currentProps = ref(tabs[0].props || {});

function switchTab(index: number) {
  currentTab.value = index;
  currentComponent.value = tabs[index].component;
  currentProps.value = tabs[index].props || {};
}
</script>

<template>
  <div class="tabs">
    <div class="tab-buttons">
      <button
        v-for="(tab, index) in tabs"
        :key="tab.name"
        @click="switchTab(index)"
        :class="{ active: currentTab === index }"
      >
        {{ tab.name }}
      </button>
    </div>

    <div class="tab-content">
      <!-- Keep-alive caches component instances -->
      <KeepAlive>
        <component
          :is="currentComponent"
          v-bind="currentProps"
        />
      </KeepAlive>
    </div>
  </div>
</template>

// Async Components
<script setup lang="ts">
import { defineAsyncComponent } from 'vue';

const HeavyComponent = defineAsyncComponent({
  loader: () => import('./HeavyComponent.vue'),
  loadingComponent: LoadingSpinner,
  errorComponent: ErrorDisplay,
  delay: 200, // Show loading after 200ms
  timeout: 3000 // Show error after 3s
});
</script>

<template>
  <Suspense>
    <template #default>
      <HeavyComponent />
    </template>
    <template #fallback>
      <LoadingSpinner />
    </template>
  </Suspense>
</template>
```

## Advanced Reactivity

### Computed Setters

```typescript
<script setup lang="ts">
import { ref, computed } from 'vue';

const firstName = ref('John');
const lastName = ref('Doe');

const fullName = computed({
  get() {
    return `${firstName.value} ${lastName.value}`;
  },
  set(newValue: string) {
    const [first, last] = newValue.split(' ');
    firstName.value = first;
    lastName.value = last;
  }
});

// Can read and write
console.log(fullName.value); // "John Doe"
fullName.value = 'Jane Smith'; // Updates firstName and lastName
</script>

<template>
  <input v-model="fullName" placeholder="Full name" />
  <p>First: {{ firstName }}</p>
  <p>Last: {{ lastName }}</p>
</template>
```

### Custom Refs

```typescript
import { customRef } from 'vue';

// Debounced Ref
function useDebouncedRef<T>(value: T, delay = 200) {
  let timeout: number;

  return customRef<T>((track, trigger) => {
    return {
      get() {
        track(); // Track dependency
        return value;
      },
      set(newValue: T) {
        clearTimeout(timeout);
        timeout = setTimeout(() => {
          value = newValue;
          trigger(); // Trigger update
        }, delay);
      }
    };
  });
}

// Usage
<script setup lang="ts">
const searchQuery = useDebouncedRef('', 500);

watch(searchQuery, (newValue) => {
  // Only called after 500ms of no changes
  console.log('Searching for:', newValue);
  performSearch(newValue);
});
</script>

<template>
  <input v-model="searchQuery" placeholder="Search..." />
</template>

// Throttled Ref
function useThrottledRef<T>(value: T, delay = 200) {
  let lastUpdate = 0;
  let pendingValue: T | null = null;
  let timeout: number;

  return customRef<T>((track, trigger) => {
    return {
      get() {
        track();
        return value;
      },
      set(newValue: T) {
        const now = Date.now();
        const timeSinceLastUpdate = now - lastUpdate;

        if (timeSinceLastUpdate >= delay) {
          value = newValue;
          lastUpdate = now;
          trigger();
        } else {
          pendingValue = newValue;
          clearTimeout(timeout);
          timeout = setTimeout(() => {
            if (pendingValue !== null) {
              value = pendingValue;
              pendingValue = null;
              lastUpdate = Date.now();
              trigger();
            }
          }, delay - timeSinceLastUpdate);
        }
      }
    };
  });
}
```

### Watchers

```typescript
<script setup lang="ts">
import { ref, watch, watchEffect } from 'vue';

const count = ref(0);
const doubled = ref(0);

// Simple watch
watch(count, (newValue, oldValue) => {
  console.log(`Count changed from ${oldValue} to ${newValue}`);
});

// Watch multiple sources
watch([count, doubled], ([newCount, newDoubled], [oldCount, oldDoubled]) => {
  console.log('Either count or doubled changed');
});

// Immediate execution
watch(count, (value) => {
  doubled.value = value * 2;
}, { immediate: true });

// Deep watch
const user = ref({ name: 'John', address: { city: 'NYC' } });

watch(user, (newUser) => {
  console.log('User changed:', newUser);
}, { deep: true });

// Watch getter
const state = reactive({ count: 0, nested: { value: 1 } });

watch(
  () => state.nested.value,
  (newValue) => {
    console.log('Nested value changed:', newValue);
  }
);

// watchEffect - auto-tracks dependencies
watchEffect(() => {
  console.log(`Count is ${count.value}, doubled is ${doubled.value}`);
  // Automatically re-runs when count or doubled changes
});

// Stop watching
const stopWatch = watch(count, () => {
  console.log('Count changed');
});

// Later...
stopWatch(); // Stops watching

// Flush timing
watch(count, () => {
  // Runs after component updates (post)
}, { flush: 'post' });

watch(count, () => {
  // Runs before component updates (pre) - default
}, { flush: 'pre' });

watch(count, () => {
  // Runs synchronously
}, { flush: 'sync' });
</script>
```

## Performance Optimization

### Virtual Scrolling

```vue
<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue';

interface Props {
  items: any[];
  itemHeight: number;
  containerHeight: number;
  buffer?: number;
}

const props = withDefaults(defineProps<Props>(), {
  buffer: 5
});

const scrollTop = ref(0);
const containerRef = ref<HTMLElement>();

const visibleRange = computed(() => {
  const start = Math.max(0, Math.floor(scrollTop.value / props.itemHeight) - props.buffer);
  const visibleCount = Math.ceil(props.containerHeight / props.itemHeight);
  const end = Math.min(
    props.items.length,
    start + visibleCount + props.buffer * 2
  );
  return { start, end };
});

const visibleItems = computed(() => {
  const { start, end } = visibleRange.value;
  return props.items.slice(start, end).map((item, index) => ({
    item,
    index: start + index
  }));
});

const totalHeight = computed(() => props.items.length * props.itemHeight);
const offsetY = computed(() => visibleRange.value.start * props.itemHeight);

function handleScroll(event: Event) {
  scrollTop.value = (event.target as HTMLElement).scrollTop;
}

onMounted(() => {
  containerRef.value?.addEventListener('scroll', handleScroll);
});

onUnmounted(() => {
  containerRef.value?.removeEventListener('scroll', handleScroll);
});
</script>

<template>
  <div
    ref="containerRef"
    class="virtual-scroll-container"
    :style="{ height: containerHeight + 'px', overflow: 'auto' }"
  >
    <div :style="{ height: totalHeight + 'px', position: 'relative' }">
      <div :style="{ transform: `translateY(${offsetY}px)` }">
        <div
          v-for="{ item, index } in visibleItems"
          :key="index"
          :style="{ height: itemHeight + 'px' }"
        >
          <slot :item="item" :index="index" />
        </div>
      </div>
    </div>
  </div>
</template>
```

### V-memo

```vue
<template>
  <!-- Re-render only when user.id changes -->
  <div v-memo="[user.id]">
    <UserCard :user="user" />
  </div>

  <!-- Never re-render -->
  <div v-memo="[]">
    <StaticContent />
  </div>

  <!-- Re-render when any dependency changes -->
  <div v-memo="[count, isActive, theme]">
    <ComplexComponent :count="count" :active="isActive" :theme="theme" />
  </div>
</template>
```

This reference provides advanced Vue.js patterns and techniques for building maintainable applications. Use these patterns based on your specific needs and project requirements.
