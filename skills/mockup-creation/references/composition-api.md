# Vue 3 Composition API Patterns

Advanced patterns for Vue 3 Composition API with TypeScript in mockup development.

## Table of Contents

- [Composables](#composables)
- [State Management](#state-management)
- [Lifecycle Hooks](#lifecycle-hooks)
- [Reactive Data](#reactive-data)
- [Computed Properties](#computed-properties)
- [Watchers](#watchers)
- [Event Handling](#event-handling)
- [Template Refs](#template-refs)
- [Provide/Inject](#provideinject)
- [Async Data](#async-data)

---

## Composables

### Basic Composable Pattern

```typescript
// composables/useCounter.ts
import { ref, computed } from 'vue'

export function useCounter(initialValue = 0) {
  const count = ref(initialValue)
  
  const increment = () => {
    count.value++
  }
  
  const decrement = () => {
    count.value--
  }
  
  const reset = () => {
    count.value = initialValue
  }
  
  const doubleCount = computed(() => count.value * 2)
  
  return {
    count,
    increment,
    decrement,
    reset,
    doubleCount
  }
}
```

**Usage:**
```vue
<script setup lang="ts">
import { useCounter } from '@/composables/useCounter'

const { count, increment, decrement, doubleCount } = useCounter(10)
</script>

<template>
  <div>
    <p>Count: {{ count }}</p>
    <p>Double: {{ doubleCount }}</p>
    <button @click="increment">+</button>
    <button @click="decrement">-</button>
  </div>
</template>
```

### useToggle

```typescript
// composables/useToggle.ts
import { ref } from 'vue'

export function useToggle(initialState = false) {
  const state = ref(initialState)
  
  const toggle = () => {
    state.value = !state.value
  }
  
  const setTrue = () => {
    state.value = true
  }
  
  const setFalse = () => {
    state.value = false
  }
  
  return {
    state,
    toggle,
    setTrue,
    setFalse
  }
}
```

### useLocalStorage

```typescript
// composables/useLocalStorage.ts
import { ref, watch } from 'vue'

export function useLocalStorage<T>(key: string, defaultValue: T) {
  const storedValue = localStorage.getItem(key)
  const value = ref<T>(
    storedValue ? JSON.parse(storedValue) : defaultValue
  )
  
  watch(value, (newValue) => {
    localStorage.setItem(key, JSON.stringify(newValue))
  }, { deep: true })
  
  return value
}
```

**Usage:**
```vue
<script setup lang="ts">
import { useLocalStorage } from '@/composables/useLocalStorage'

interface User {
  name: string
  email: string
}

const user = useLocalStorage<User>('user', {
  name: '',
  email: ''
})
</script>
```

### useDebounce

```typescript
// composables/useDebounce.ts
import { ref, watch } from 'vue'

export function useDebounce<T>(value: Ref<T>, delay = 300) {
  const debouncedValue = ref<T>(value.value)
  
  let timeout: ReturnType<typeof setTimeout>
  
  watch(value, (newValue) => {
    clearTimeout(timeout)
    timeout = setTimeout(() => {
      debouncedValue.value = newValue
    }, delay)
  })
  
  return debouncedValue
}
```

**Usage:**
```vue
<script setup lang="ts">
import { ref } from 'vue'
import { useDebounce } from '@/composables/useDebounce'

const searchQuery = ref('')
const debouncedQuery = useDebounce(searchQuery, 500)

watch(debouncedQuery, (query) => {
  // API call with debounced query
  console.log('Searching for:', query)
})
</script>

<template>
  <input v-model="searchQuery" placeholder="Search..." />
</template>
```

### useClickOutside

```typescript
// composables/useClickOutside.ts
import { onMounted, onUnmounted } from 'vue'
import type { Ref } from 'vue'

export function useClickOutside(
  elementRef: Ref<HTMLElement | null>,
  callback: () => void
) {
  const handleClick = (event: MouseEvent) => {
    if (elementRef.value && !elementRef.value.contains(event.target as Node)) {
      callback()
    }
  }
  
  onMounted(() => {
    document.addEventListener('click', handleClick)
  })
  
  onUnmounted(() => {
    document.removeEventListener('click', handleClick)
  })
}
```

**Usage:**
```vue
<script setup lang="ts">
import { ref } from 'vue'
import { useClickOutside } from '@/composables/useClickOutside'

const dropdownRef = ref<HTMLElement | null>(null)
const isOpen = ref(false)

useClickOutside(dropdownRef, () => {
  isOpen.value = false
})
</script>

<template>
  <div ref="dropdownRef" class="relative">
    <button @click="isOpen = !isOpen">Toggle</button>
    <div v-if="isOpen" class="absolute">
      Dropdown content
    </div>
  </div>
</template>
```

---

## State Management

### Simple Reactive Store

```typescript
// stores/useStore.ts
import { reactive, readonly } from 'vue'

interface State {
  user: User | null
  theme: 'light' | 'dark'
  notifications: Notification[]
}

const state = reactive<State>({
  user: null,
  theme: 'light',
  notifications: []
})

const setUser = (user: User | null) => {
  state.user = user
}

const setTheme = (theme: 'light' | 'dark') => {
  state.theme = theme
}

const addNotification = (notification: Notification) => {
  state.notifications.push(notification)
}

const removeNotification = (id: string) => {
  const index = state.notifications.findIndex(n => n.id === id)
  if (index !== -1) {
    state.notifications.splice(index, 1)
  }
}

export function useStore() {
  return {
    state: readonly(state),
    setUser,
    setTheme,
    addNotification,
    removeNotification
  }
}
```

**Usage:**
```vue
<script setup lang="ts">
import { useStore } from '@/stores/useStore'

const { state, setTheme } = useStore()
</script>

<template>
  <div>
    <p>Current theme: {{ state.theme }}</p>
    <button @click="setTheme('dark')">Dark Mode</button>
  </div>
</template>
```

### Pinia Store (Recommended)

```typescript
// stores/user.ts
import { defineStore } from 'pinia'

interface User {
  id: string
  name: string
  email: string
}

export const useUserStore = defineStore('user', {
  state: () => ({
    user: null as User | null,
    isAuthenticated: false
  }),
  
  getters: {
    userName: (state) => state.user?.name ?? 'Guest',
    userEmail: (state) => state.user?.email ?? ''
  },
  
  actions: {
    setUser(user: User) {
      this.user = user
      this.isAuthenticated = true
    },
    
    logout() {
      this.user = null
      this.isAuthenticated = false
    }
  }
})
```

**Setup:**
```typescript
// main.ts
import { createPinia } from 'pinia'
import { createApp } from 'vue'
import App from './App.vue'

const pinia = createPinia()
const app = createApp(App)

app.use(pinia)
app.mount('#app')
```

**Usage:**
```vue
<script setup lang="ts">
import { useUserStore } from '@/stores/user'

const userStore = useUserStore()
</script>

<template>
  <div>
    <p v-if="userStore.isAuthenticated">
      Welcome, {{ userStore.userName }}!
    </p>
    <button @click="userStore.logout">Logout</button>
  </div>
</template>
```

---

## Lifecycle Hooks

### Hook Usage Patterns

```vue
<script setup lang="ts">
import {
  onBeforeMount,
  onMounted,
  onBeforeUpdate,
  onUpdated,
  onBeforeUnmount,
  onUnmounted
} from 'vue'

// Before component is mounted
onBeforeMount(() => {
  console.log('Before mount')
})

// After component is mounted
onMounted(() => {
  console.log('Mounted')
  // Fetch data, set up event listeners
})

// Before component updates
onBeforeUpdate(() => {
  console.log('Before update')
})

// After component updates
onUpdated(() => {
  console.log('Updated')
})

// Before component is unmounted
onBeforeUnmount(() => {
  console.log('Before unmount')
  // Clean up event listeners, timers
})

// After component is unmounted
onUnmounted(() => {
  console.log('Unmounted')
})
</script>
```

### Fetch Data on Mount

```vue
<script setup lang="ts">
import { ref, onMounted } from 'vue'

interface Product {
  id: string
  name: string
  price: number
}

const products = ref<Product[]>([])
const loading = ref(false)
const error = ref<string | null>(null)

onMounted(async () => {
  loading.value = true
  
  try {
    const response = await fetch('/api/products')
    products.value = await response.json()
  } catch (e) {
    error.value = 'Failed to load products'
  } finally {
    loading.value = false
  }
})
</script>

<template>
  <div>
    <div v-if="loading">Loading...</div>
    <div v-else-if="error">{{ error }}</div>
    <div v-else>
      <div v-for="product in products" :key="product.id">
        {{ product.name }} - ${{ product.price }}
      </div>
    </div>
  </div>
</template>
```

---

## Reactive Data

### ref vs reactive

```vue
<script setup lang="ts">
import { ref, reactive } from 'vue'

// ref - for primitives and single values
const count = ref(0)
const name = ref('John')

// Access with .value
count.value++
console.log(name.value)

// reactive - for objects
const state = reactive({
  count: 0,
  name: 'John',
  user: {
    id: 1,
    email: 'john@example.com'
  }
})

// Direct access (no .value)
state.count++
console.log(state.name)

// Nested reactivity
state.user.email = 'new@example.com'
</script>
```

### toRefs

```vue
<script setup lang="ts">
import { reactive, toRefs } from 'vue'

const state = reactive({
  count: 0,
  name: 'John'
})

// Destructure while maintaining reactivity
const { count, name } = toRefs(state)

// Now these are refs
count.value++
console.log(name.value)
</script>
```

### shallowRef & shallowReactive

```vue
<script setup lang="ts">
import { shallowRef, shallowReactive } from 'vue'

// Only top level is reactive
const state = shallowReactive({
  count: 0,
  nested: {
    value: 1 // Not reactive
  }
})

// Triggers reactivity
state.count++

// Does NOT trigger reactivity
state.nested.value++

// Must replace entire nested object
state.nested = { value: 2 } // Triggers reactivity
</script>
```

---

## Computed Properties

### Basic Computed

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

const firstName = ref('John')
const lastName = ref('Doe')

const fullName = computed(() => {
  return `${firstName.value} ${lastName.value}`
})
</script>

<template>
  <p>{{ fullName }}</p>
</template>
```

### Writable Computed

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

const firstName = ref('John')
const lastName = ref('Doe')

const fullName = computed({
  get() {
    return `${firstName.value} ${lastName.value}`
  },
  set(value: string) {
    [firstName.value, lastName.value] = value.split(' ')
  }
})
</script>

<template>
  <input v-model="fullName" />
</template>
```

### Computed with Complex Logic

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

interface Product {
  id: string
  name: string
  price: number
  category: string
}

const products = ref<Product[]>([])
const searchQuery = ref('')
const selectedCategory = ref('')
const sortBy = ref<'name' | 'price'>('name')

const filteredProducts = computed(() => {
  let result = products.value
  
  // Filter by search query
  if (searchQuery.value) {
    result = result.filter(p =>
      p.name.toLowerCase().includes(searchQuery.value.toLowerCase())
    )
  }
  
  // Filter by category
  if (selectedCategory.value) {
    result = result.filter(p => p.category === selectedCategory.value)
  }
  
  // Sort
  result.sort((a, b) => {
    if (sortBy.value === 'name') {
      return a.name.localeCompare(b.name)
    }
    return a.price - b.price
  })
  
  return result
})
</script>
```

---

## Watchers

### Basic Watch

```vue
<script setup lang="ts">
import { ref, watch } from 'vue'

const count = ref(0)

watch(count, (newValue, oldValue) => {
  console.log(`Count changed from ${oldValue} to ${newValue}`)
})
</script>
```

### Watch Multiple Sources

```vue
<script setup lang="ts">
import { ref, watch } from 'vue'

const firstName = ref('John')
const lastName = ref('Doe')

watch([firstName, lastName], ([newFirst, newLast], [oldFirst, oldLast]) => {
  console.log(`Name changed from ${oldFirst} ${oldLast} to ${newFirst} ${newLast}`)
})
</script>
```

### Deep Watch

```vue
<script setup lang="ts">
import { reactive, watch } from 'vue'

const state = reactive({
  user: {
    name: 'John',
    settings: {
      theme: 'light'
    }
  }
})

watch(
  () => state.user,
  (newUser) => {
    console.log('User changed:', newUser)
  },
  { deep: true }
)
</script>
```

### Immediate Watch

```vue
<script setup lang="ts">
import { ref, watch } from 'vue'

const count = ref(0)

watch(
  count,
  (value) => {
    console.log('Count:', value)
  },
  { immediate: true } // Runs immediately with current value
)
</script>
```

### watchEffect

```vue
<script setup lang="ts">
import { ref, watchEffect } from 'vue'

const count = ref(0)
const doubled = ref(0)

// Automatically tracks dependencies
watchEffect(() => {
  doubled.value = count.value * 2
  console.log(`Count: ${count.value}, Doubled: ${doubled.value}`)
})
</script>
```

---

## Event Handling

### Type-Safe Events

```vue
<script setup lang="ts">
import { ref } from 'vue'

const handleClick = (event: MouseEvent) => {
  console.log('Clicked at:', event.clientX, event.clientY)
}

const handleInput = (event: Event) => {
  const target = event.target as HTMLInputElement
  console.log('Input value:', target.value)
}

const handleKeydown = (event: KeyboardEvent) => {
  if (event.key === 'Enter') {
    console.log('Enter pressed')
  }
}
</script>

<template>
  <button @click="handleClick">Click Me</button>
  <input @input="handleInput" @keydown="handleKeydown" />
</template>
```

### Event Modifiers

```vue
<template>
  <!-- Prevent default -->
  <form @submit.prevent="handleSubmit">
    <button type="submit">Submit</button>
  </form>
  
  <!-- Stop propagation -->
  <div @click="handleParent">
    <button @click.stop="handleChild">Click</button>
  </div>
  
  <!-- Once -->
  <button @click.once="handleOnce">Click Once</button>
  
  <!-- Key modifiers -->
  <input @keyup.enter="handleEnter" />
  <input @keyup.ctrl.s="handleSave" />
  
  <!-- Mouse modifiers -->
  <button @click.left="handleLeft">Left Click</button>
  <button @click.right.prevent="handleRight">Right Click</button>
</template>
```

---

## Template Refs

### Basic Template Ref

```vue
<script setup lang="ts">
import { ref, onMounted } from 'vue'

const inputRef = ref<HTMLInputElement | null>(null)

onMounted(() => {
  inputRef.value?.focus()
})
</script>

<template>
  <input ref="inputRef" />
</template>
```

### Component Ref

```vue
<script setup lang="ts">
import { ref } from 'vue'
import ChildComponent from './ChildComponent.vue'

const childRef = ref<InstanceType<typeof ChildComponent> | null>(null)

const callChildMethod = () => {
  childRef.value?.someMethod()
}
</script>

<template>
  <ChildComponent ref="childRef" />
  <button @click="callChildMethod">Call Child Method</button>
</template>
```

---

## Provide/Inject

### Provide Data

```vue
<script setup lang="ts">
import { provide, ref } from 'vue'

const theme = ref('light')

provide('theme', theme)
</script>
```

### Inject Data

```vue
<script setup lang="ts">
import { inject } from 'vue'
import type { Ref } from 'vue'

const theme = inject<Ref<string>>('theme')
</script>

<template>
  <div :class="theme">Content</div>
</template>
```

### Type-Safe Provide/Inject

```typescript
// keys.ts
import type { InjectionKey, Ref } from 'vue'

export const ThemeKey: InjectionKey<Ref<string>> = Symbol('theme')
```

```vue
<script setup lang="ts">
import { provide, ref } from 'vue'
import { ThemeKey } from './keys'

const theme = ref('light')
provide(ThemeKey, theme)
</script>
```

```vue
<script setup lang="ts">
import { inject } from 'vue'
import { ThemeKey } from './keys'

const theme = inject(ThemeKey)
// theme is typed as Ref<string> | undefined
</script>
```

---

## Async Data

### useFetch Composable

```typescript
// composables/useFetch.ts
import { ref } from 'vue'

export function useFetch<T>(url: string) {
  const data = ref<T | null>(null)
  const loading = ref(false)
  const error = ref<string | null>(null)
  
  const fetch = async () => {
    loading.value = true
    error.value = null
    
    try {
      const response = await window.fetch(url)
      if (!response.ok) throw new Error('Failed to fetch')
      data.value = await response.json()
    } catch (e) {
      error.value = e instanceof Error ? e.message : 'Unknown error'
    } finally {
      loading.value = false
    }
  }
  
  return {
    data,
    loading,
    error,
    fetch
  }
}
```

**Usage:**
```vue
<script setup lang="ts">
import { onMounted } from 'vue'
import { useFetch } from '@/composables/useFetch'

interface User {
  id: number
  name: string
}

const { data: user, loading, error, fetch } = useFetch<User>('/api/user')

onMounted(() => {
  fetch()
})
</script>

<template>
  <div v-if="loading">Loading...</div>
  <div v-else-if="error">{{ error }}</div>
  <div v-else-if="user">{{ user.name }}</div>
</template>
```

---

## Form Validation Patterns

### Complete Form with Validation

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

interface FormData {
  email: string
  password: string
  confirmPassword: string
}

const form = ref<FormData>({
  email: '',
  password: '',
  confirmPassword: ''
})

const errors = ref<Partial<Record<keyof FormData, string>>>({})
const touched = ref<Partial<Record<keyof FormData, boolean>>>({})

const isValid = computed(() => {
  return form.value.email.length > 0 && 
         form.value.password.length >= 8 &&
         form.value.password === form.value.confirmPassword
})

const validateField = (field: keyof FormData) => {
  touched.value[field] = true
  
  switch (field) {
    case 'email':
      if (!form.value.email) {
        errors.value.email = 'Email is required'
      } else if (!form.value.email.includes('@')) {
        errors.value.email = 'Invalid email address'
      } else {
        delete errors.value.email
      }
      break
    
    case 'password':
      if (!form.value.password) {
        errors.value.password = 'Password is required'
      } else if (form.value.password.length < 8) {
        errors.value.password = 'Password must be at least 8 characters'
      } else {
        delete errors.value.password
      }
      break
    
    case 'confirmPassword':
      if (form.value.password !== form.value.confirmPassword) {
        errors.value.confirmPassword = 'Passwords do not match'
      } else {
        delete errors.value.confirmPassword
      }
      break
  }
}

const validateAll = () => {
  (Object.keys(form.value) as Array<keyof FormData>).forEach(validateField)
  return Object.keys(errors.value).length === 0
}

const handleSubmit = () => {
  if (validateAll()) {
    console.log('Form submitted:', form.value)
  }
}
</script>

<template>
  <form @submit.prevent="handleSubmit" class="space-y-4">
    <div>
      <label class="block text-sm font-medium text-gray-700">Email</label>
      <input
        v-model="form.email"
        @blur="validateField('email')"
        type="email"
        class="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
        :class="{ 'border-red-500': touched.email && errors.email }"
      />
      <p v-if="touched.email && errors.email" class="mt-1 text-sm text-red-600">
        {{ errors.email }}
      </p>
    </div>
    
    <div>
      <label class="block text-sm font-medium text-gray-700">Password</label>
      <input
        v-model="form.password"
        @blur="validateField('password')"
        type="password"
        class="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
        :class="{ 'border-red-500': touched.password && errors.password }"
      />
      <p v-if="touched.password && errors.password" class="mt-1 text-sm text-red-600">
        {{ errors.password }}
      </p>
    </div>
    
    <div>
      <label class="block text-sm font-medium text-gray-700">Confirm Password</label>
      <input
        v-model="form.confirmPassword"
        @blur="validateField('confirmPassword')"
        type="password"
        class="mt-1 block w-full rounded-md border-gray-300 shadow-sm"
        :class="{ 'border-red-500': touched.confirmPassword && errors.confirmPassword }"
      />
      <p v-if="touched.confirmPassword && errors.confirmPassword" class="mt-1 text-sm text-red-600">
        {{ errors.confirmPassword }}
      </p>
    </div>
    
    <button
      type="submit"
      :disabled="!isValid"
      class="w-full bg-blue-600 text-white py-2 rounded-md disabled:bg-gray-400 disabled:cursor-not-allowed"
    >
      Submit
    </button>
  </form>
</template>
```

### Reusable useForm Composable

```typescript
// composables/useForm.ts
import { ref, computed } from 'vue'

type ValidationRule<T> = (value: T) => string | undefined

interface FieldConfig<T> {
  initialValue: T
  rules?: ValidationRule<T>[]
}

export function useForm<T extends Record<string, any>>(
  config: Record<keyof T, FieldConfig<any>>
) {
  const form = ref<T>(
    Object.keys(config).reduce((acc, key) => {
      acc[key as keyof T] = config[key as keyof T].initialValue
      return acc
    }, {} as T)
  )
  
  const errors = ref<Partial<Record<keyof T, string>>>({})
  const touched = ref<Partial<Record<keyof T, boolean>>>({})
  
  const validateField = (field: keyof T) => {
    touched.value[field] = true
    const rules = config[field].rules || []
    
    for (const rule of rules) {
      const error = rule(form.value[field])
      if (error) {
        errors.value[field] = error
        return false
      }
    }
    
    delete errors.value[field]
    return true
  }
  
  const validateAll = () => {
    let isValid = true
    for (const field of Object.keys(config) as Array<keyof T>) {
      if (!validateField(field)) {
        isValid = false
      }
    }
    return isValid
  }
  
  const reset = () => {
    form.value = Object.keys(config).reduce((acc, key) => {
      acc[key as keyof T] = config[key as keyof T].initialValue
      return acc
    }, {} as T)
    errors.value = {}
    touched.value = {}
  }
  
  const isValid = computed(() => Object.keys(errors.value).length === 0)
  
  return {
    form,
    errors,
    touched,
    validateField,
    validateAll,
    reset,
    isValid
  }
}

// Validation helpers
export const required = (message = 'This field is required') => 
  (value: any) => value ? undefined : message

export const minLength = (min: number, message?: string) =>
  (value: string) => value.length >= min ? undefined : message || `Minimum ${min} characters`

export const email = (message = 'Invalid email address') =>
  (value: string) => value.includes('@') ? undefined : message

export const match = (otherField: any, message = 'Fields do not match') =>
  (value: any) => value === otherField.value ? undefined : message
```

**Usage:**
```vue
<script setup lang="ts">
import { useForm, required, minLength, email } from '@/composables/useForm'

const { form, errors, touched, validateField, validateAll, isValid } = useForm({
  email: {
    initialValue: '',
    rules: [required(), email()]
  },
  password: {
    initialValue: '',
    rules: [required(), minLength(8)]
  }
})

const handleSubmit = () => {
  if (validateAll()) {
    console.log('Form submitted:', form.value)
  }
}
</script>
```

---

These patterns provide a solid foundation for building complex Vue 3 applications with TypeScript and the Composition API.
