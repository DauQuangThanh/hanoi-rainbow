# React Hooks for Mockups

Custom React hooks for state management and reusable logic in Next.js mockups.

## Basic State Management

### useToggle

Toggle boolean state with actions.

```typescript
// lib/hooks/useToggle.ts
import { useState, useCallback } from 'react'

export function useToggle(initialValue = false) {
  const [value, setValue] = useState(initialValue)
  
  const toggle = useCallback(() => setValue(v => !v), [])
  const setTrue = useCallback(() => setValue(true), [])
  const setFalse = useCallback(() => setValue(false), [])
  
  return { value, toggle, setTrue, setFalse }
}
```

**Usage:**

```typescript
'use client'

import { useToggle } from '@/lib/hooks/useToggle'

export default function Sidebar() {
  const { value: isOpen, toggle, setFalse } = useToggle(false)
  
  return (
    <>
      <button onClick={toggle}>Toggle Sidebar</button>
      {isOpen && <aside>Sidebar content</aside>}
    </>
  )
}
```

### useCounter

Counter state with increment/decrement.

```typescript
// lib/hooks/useCounter.ts
import { useState, useCallback } from 'react'

export function useCounter(initialValue = 0, step = 1) {
  const [count, setCount] = useState(initialValue)
  
  const increment = useCallback(() => setCount(c => c + step), [step])
  const decrement = useCallback(() => setCount(c => c - step), [step])
  const reset = useCallback(() => setCount(initialValue), [initialValue])
  const set = useCallback((value: number) => setCount(value), [])
  
  return { count, increment, decrement, reset, set }
}
```

**Usage:**

```typescript
'use client'

import { useCounter } from '@/lib/hooks/useCounter'

export default function CounterDemo() {
  const { count, increment, decrement, reset } = useCounter(0, 1)
  
  return (
    <div className="space-y-4">
      <div className="text-4xl font-bold">{count}</div>
      <div className="flex gap-2">
        <button onClick={decrement} className="btn">-</button>
        <button onClick={increment} className="btn">+</button>
        <button onClick={reset} className="btn">Reset</button>
      </div>
    </div>
  )
}
```

## Modal Management

### useModal

Manage modal open/close state.

```typescript
// lib/hooks/useModal.ts
import { useState, useCallback } from 'react'

export function useModal() {
  const [isOpen, setIsOpen] = useState(false)
  
  const open = useCallback(() => setIsOpen(true), [])
  const close = useCallback(() => setIsOpen(false), [])
  const toggle = useCallback(() => setIsOpen(v => !v), [])
  
  return { isOpen, open, close, toggle }
}
```

**Usage:**

```typescript
'use client'

import { useModal } from '@/lib/hooks/useModal'
import { Modal } from '@/components/ui/Modal'

export default function ModalDemo() {
  const { isOpen, open, close } = useModal()
  
  return (
    <>
      <button onClick={open} className="btn">Open Modal</button>
      <Modal isOpen={isOpen} onClose={close}>
        <h2>Modal Content</h2>
        <p>This is a modal dialog.</p>
      </Modal>
    </>
  )
}
```

## Form Handling

### useForm

Simple form state management with validation.

```typescript
// lib/hooks/useForm.ts
import { useState, useCallback, FormEvent } from 'react'

interface UseFormOptions<T> {
  initialValues: T
  onSubmit: (values: T) => void | Promise<void>
  validate?: (values: T) => Partial<Record<keyof T, string>>
}

export function useForm<T extends Record<string, any>>({
  initialValues,
  onSubmit,
  validate
}: UseFormOptions<T>) {
  const [values, setValues] = useState<T>(initialValues)
  const [errors, setErrors] = useState<Partial<Record<keyof T, string>>>({})
  const [isSubmitting, setIsSubmitting] = useState(false)
  
  const handleChange = useCallback((name: keyof T, value: any) => {
    setValues(prev => ({ ...prev, [name]: value }))
    // Clear error when user types
    setErrors(prev => ({ ...prev, [name]: undefined }))
  }, [])
  
  const handleSubmit = useCallback(async (e: FormEvent) => {
    e.preventDefault()
    
    // Validate if validator provided
    if (validate) {
      const validationErrors = validate(values)
      if (Object.keys(validationErrors).length > 0) {
        setErrors(validationErrors)
        return
      }
    }
    
    setIsSubmitting(true)
    try {
      await onSubmit(values)
    } finally {
      setIsSubmitting(false)
    }
  }, [values, validate, onSubmit])
  
  const reset = useCallback(() => {
    setValues(initialValues)
    setErrors({})
  }, [initialValues])
  
  return {
    values,
    errors,
    isSubmitting,
    handleChange,
    handleSubmit,
    reset
  }
}
```

**Usage:**

```typescript
'use client'

import { useForm } from '@/lib/hooks/useForm'

interface LoginForm {
  email: string
  password: string
}

export default function LoginForm() {
  const { values, errors, isSubmitting, handleChange, handleSubmit } = useForm<LoginForm>({
    initialValues: { email: '', password: '' },
    onSubmit: async (values) => {
      console.log('Submitting:', values)
      // API call here
    },
    validate: (values) => {
      const errors: Partial<Record<keyof LoginForm, string>> = {}
      if (!values.email) errors.email = 'Email is required'
      if (!values.password) errors.password = 'Password is required'
      return errors
    }
  })
  
  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div>
        <label htmlFor="email">Email</label>
        <input
          id="email"
          type="email"
          value={values.email}
          onChange={(e) => handleChange('email', e.target.value)}
          className="input"
        />
        {errors.email && <span className="text-red-600">{errors.email}</span>}
      </div>
      
      <div>
        <label htmlFor="password">Password</label>
        <input
          id="password"
          type="password"
          value={values.password}
          onChange={(e) => handleChange('password', e.target.value)}
          className="input"
        />
        {errors.password && <span className="text-red-600">{errors.password}</span>}
      </div>
      
      <button type="submit" disabled={isSubmitting} className="btn">
        {isSubmitting ? 'Submitting...' : 'Login'}
      </button>
    </form>
  )
}
```

## Data Fetching

### useFetch

Client-side data fetching with loading and error states.

```typescript
// lib/hooks/useFetch.ts
import { useState, useEffect } from 'react'

interface UseFetchOptions {
  skip?: boolean
}

export function useFetch<T>(url: string, options?: UseFetchOptions) {
  const [data, setData] = useState<T | null>(null)
  const [error, setError] = useState<Error | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    if (options?.skip) {
      setIsLoading(false)
      return
    }
    
    let cancelled = false
    
    async function fetchData() {
      try {
        setIsLoading(true)
        const response = await fetch(url)
        if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`)
        const json = await response.json()
        if (!cancelled) {
          setData(json)
          setError(null)
        }
      } catch (e) {
        if (!cancelled) {
          setError(e instanceof Error ? e : new Error('Unknown error'))
          setData(null)
        }
      } finally {
        if (!cancelled) {
          setIsLoading(false)
        }
      }
    }
    
    fetchData()
    
    return () => {
      cancelled = true
    }
  }, [url, options?.skip])
  
  return { data, error, isLoading }
}
```

**Usage:**

```typescript
'use client'

import { useFetch } from '@/lib/hooks/useFetch'

interface User {
  id: number
  name: string
  email: string
}

export default function UserList() {
  const { data, error, isLoading } = useFetch<User[]>('/api/users')
  
  if (isLoading) return <div>Loading...</div>
  if (error) return <div>Error: {error.message}</div>
  if (!data) return <div>No data</div>
  
  return (
    <ul>
      {data.map(user => (
        <li key={user.id}>{user.name} - {user.email}</li>
      ))}
    </ul>
  )
}
```

**Note:** For production, prefer Next.js Server Components with async/await for data fetching, or use libraries like SWR or React Query for client-side fetching.

## Local Storage

### useLocalStorage

Persist state in localStorage with TypeScript support.

```typescript
// lib/hooks/useLocalStorage.ts
import { useState, useEffect } from 'react'

export function useLocalStorage<T>(key: string, initialValue: T) {
  const [storedValue, setStoredValue] = useState<T>(initialValue)
  
  useEffect(() => {
    // Only run on client
    try {
      const item = window.localStorage.getItem(key)
      if (item) {
        setStoredValue(JSON.parse(item))
      }
    } catch (error) {
      console.error(`Error loading ${key} from localStorage:`, error)
    }
  }, [key])
  
  const setValue = (value: T | ((val: T) => T)) => {
    try {
      const valueToStore = value instanceof Function ? value(storedValue) : value
      setStoredValue(valueToStore)
      if (typeof window !== 'undefined') {
        window.localStorage.setItem(key, JSON.stringify(valueToStore))
      }
    } catch (error) {
      console.error(`Error saving ${key} to localStorage:`, error)
    }
  }
  
  return [storedValue, setValue] as const
}
```

**Usage:**

```typescript
'use client'

import { useLocalStorage } from '@/lib/hooks/useLocalStorage'

export default function ThemeToggle() {
  const [theme, setTheme] = useLocalStorage<'light' | 'dark'>('theme', 'light')
  
  return (
    <button onClick={() => setTheme(theme === 'light' ? 'dark' : 'light')}>
      Current theme: {theme}
    </button>
  )
}
```

## Media Queries

### useMediaQuery

Respond to CSS media queries in React.

```typescript
// lib/hooks/useMediaQuery.ts
import { useState, useEffect } from 'react'

export function useMediaQuery(query: string): boolean {
  const [matches, setMatches] = useState(false)
  
  useEffect(() => {
    const media = window.matchMedia(query)
    
    // Set initial value
    setMatches(media.matches)
    
    // Create listener
    const listener = (e: MediaQueryListEvent) => setMatches(e.matches)
    
    // Add listener
    media.addEventListener('change', listener)
    
    // Cleanup
    return () => media.removeEventListener('change', listener)
  }, [query])
  
  return matches
}
```

**Usage:**

```typescript
'use client'

import { useMediaQuery } from '@/lib/hooks/useMediaQuery'

export default function ResponsiveComponent() {
  const isMobile = useMediaQuery('(max-width: 768px)')
  const isDesktop = useMediaQuery('(min-width: 1024px)')
  
  return (
    <div>
      {isMobile && <div>Mobile view</div>}
      {isDesktop && <div>Desktop view</div>}
      {!isMobile && !isDesktop && <div>Tablet view</div>}
    </div>
  )
}
```

## Debounce

### useDebounce

Debounce rapidly changing values.

```typescript
// lib/hooks/useDebounce.ts
import { useState, useEffect } from 'react'

export function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState<T>(value)
  
  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value)
    }, delay)
    
    return () => {
      clearTimeout(handler)
    }
  }, [value, delay])
  
  return debouncedValue
}
```

**Usage:**

```typescript
'use client'

import { useState } from 'react'
import { useDebounce } from '@/lib/hooks/useDebounce'

export default function SearchInput() {
  const [searchTerm, setSearchTerm] = useState('')
  const debouncedSearchTerm = useDebounce(searchTerm, 500)
  
  // Effect runs only when debouncedSearchTerm changes
  useEffect(() => {
    if (debouncedSearchTerm) {
      console.log('Searching for:', debouncedSearchTerm)
      // API call here
    }
  }, [debouncedSearchTerm])
  
  return (
    <input
      type="text"
      value={searchTerm}
      onChange={(e) => setSearchTerm(e.target.value)}
      placeholder="Search..."
      className="input"
    />
  )
}
```

## Best Practices

1. **Custom Hooks**: Extract reusable logic into custom hooks
2. **TypeScript**: Always type your hooks with generics for reusability
3. **useCallback**: Memoize functions to prevent unnecessary re-renders
4. **Cleanup**: Always cleanup effects (event listeners, timers, etc.)
5. **Server vs Client**: Remember Next.js Server Components don't support hooks
6. **Dependencies**: Be careful with effect dependencies to avoid infinite loops
7. **Testing**: Write unit tests for custom hooks using React Testing Library

## See Also

- [Component Library](component-library.md) - Reusable UI components (Vue & React)
- [Animations](animations.md) - Animation patterns
- [Examples](examples.md) - Complete mockup examples
