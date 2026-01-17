# Component Library Reference

Complete collection of production-ready Vue 3 + TypeScript + TailwindCSS components for mockup creation.

## Table of Contents

- [UI Components](#ui-components)
  - [Button](#button)
  - [Card](#card)
  - [Modal](#modal)
  - [Input](#input)
  - [Select](#select)
  - [Checkbox](#checkbox)
  - [Radio](#radio)
  - [Tabs](#tabs)
  - [Accordion](#accordion)
  - [Badge](#badge)
  - [Alert](#alert)
  - [Toast](#toast)
- [Layout Components](#layout-components)
  - [Container](#container)
  - [Grid](#grid)
  - [Sidebar](#sidebar)
  - [Header](#header)
  - [Footer](#footer)
- [Section Components](#section-components)
  - [Hero](#hero)
  - [Features](#features)
  - [Testimonials](#testimonials)
  - [Pricing](#pricing)
  - [CTA](#cta)

---

## UI Components

### Button

Flexible button component with multiple variants and sizes.

```vue
<script setup lang="ts">
import { computed } from 'vue'

interface Props {
  variant?: 'primary' | 'secondary' | 'outline' | 'ghost' | 'danger'
  size?: 'xs' | 'sm' | 'md' | 'lg' | 'xl'
  disabled?: boolean
  loading?: boolean
  fullWidth?: boolean
  icon?: string
}

const props = withDefaults(defineProps<Props>(), {
  variant: 'primary',
  size: 'md',
  disabled: false,
  loading: false,
  fullWidth: false
})

const emit = defineEmits<{
  click: [event: MouseEvent]
}>()

const buttonClasses = computed(() => {
  const base = 'inline-flex items-center justify-center font-semibold rounded-lg transition-all duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2'
  
  const variants = {
    primary: 'bg-blue-600 text-white hover:bg-blue-700 focus:ring-blue-500',
    secondary: 'bg-gray-600 text-white hover:bg-gray-700 focus:ring-gray-500',
    outline: 'border-2 border-blue-600 text-blue-600 hover:bg-blue-50 focus:ring-blue-500',
    ghost: 'text-blue-600 hover:bg-blue-50 focus:ring-blue-500',
    danger: 'bg-red-600 text-white hover:bg-red-700 focus:ring-red-500'
  }
  
  const sizes = {
    xs: 'px-2.5 py-1.5 text-xs',
    sm: 'px-3 py-1.5 text-sm',
    md: 'px-4 py-2 text-base',
    lg: 'px-6 py-3 text-lg',
    xl: 'px-8 py-4 text-xl'
  }
  
  const width = props.fullWidth ? 'w-full' : ''
  const opacity = props.disabled || props.loading ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'
  
  return `${base} ${variants[props.variant]} ${sizes[props.size]} ${width} ${opacity}`
})
</script>

<template>
  <button
    :class="buttonClasses"
    :disabled="disabled || loading"
    @click="emit('click', $event)"
  >
    <span v-if="loading" class="mr-2 animate-spin">⚪</span>
    <span v-if="icon && !loading" class="mr-2">{{ icon }}</span>
    <slot />
  </button>
</template>
```

**Usage:**
```vue
<Button variant="primary" size="md" @click="handleClick">
  Click Me
</Button>

<Button variant="outline" size="lg" loading>
  Loading...
</Button>

<Button variant="danger" icon="🗑️" @click="deleteItem">
  Delete
</Button>
```

---

### Card

Flexible card component for content containers.

```vue
<script setup lang="ts">
interface Props {
  hoverable?: boolean
  clickable?: boolean
  padding?: 'none' | 'sm' | 'md' | 'lg'
  shadow?: 'none' | 'sm' | 'md' | 'lg' | 'xl'
}

const props = withDefaults(defineProps<Props>(), {
  hoverable: false,
  clickable: false,
  padding: 'md',
  shadow: 'md'
})

const emit = defineEmits<{
  click: [event: MouseEvent]
}>()

const cardClasses = computed(() => {
  const base = 'bg-white rounded-lg transition-all duration-200'
  
  const paddings = {
    none: '',
    sm: 'p-3',
    md: 'p-4',
    lg: 'p-6'
  }
  
  const shadows = {
    none: '',
    sm: 'shadow-sm',
    md: 'shadow-md',
    lg: 'shadow-lg',
    xl: 'shadow-xl'
  }
  
  const hover = props.hoverable ? 'hover:shadow-xl hover:-translate-y-1' : ''
  const cursor = props.clickable ? 'cursor-pointer' : ''
  
  return `${base} ${paddings[props.padding]} ${shadows[props.shadow]} ${hover} ${cursor}`
})
</script>

<template>
  <div :class="cardClasses" @click="clickable && emit('click', $event)">
    <slot />
  </div>
</template>
```

**Usage:**
```vue
<Card hoverable clickable @click="handleCardClick">
  <h3 class="text-xl font-bold">Card Title</h3>
  <p class="text-gray-600 mt-2">Card content goes here</p>
</Card>
```

---

### Modal

Full-featured modal dialog with backdrop.

```vue
<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue'

interface Props {
  modelValue: boolean
  title?: string
  size?: 'sm' | 'md' | 'lg' | 'xl' | 'full'
  closeOnBackdrop?: boolean
  showClose?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  size: 'md',
  closeOnBackdrop: true,
  showClose: true
})

const emit = defineEmits<{
  'update:modelValue': [value: boolean]
  close: []
}>()

const close = () => {
  emit('update:modelValue', false)
  emit('close')
}

const handleBackdropClick = () => {
  if (props.closeOnBackdrop) {
    close()
  }
}

const handleEscape = (e: KeyboardEvent) => {
  if (e.key === 'Escape' && props.modelValue) {
    close()
  }
}

onMounted(() => {
  document.addEventListener('keydown', handleEscape)
})

onUnmounted(() => {
  document.removeEventListener('keydown', handleEscape)
})

const sizeClasses = {
  sm: 'max-w-md',
  md: 'max-w-lg',
  lg: 'max-w-2xl',
  xl: 'max-w-4xl',
  full: 'max-w-full mx-4'
}
</script>

<template>
  <Teleport to="body">
    <Transition name="modal">
      <div
        v-if="modelValue"
        class="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black bg-opacity-50"
        @click.self="handleBackdropClick"
      >
        <div
          :class="['bg-white rounded-lg shadow-xl w-full', sizeClasses[size]]"
          @click.stop
        >
          <!-- Header -->
          <div v-if="title || showClose" class="flex items-center justify-between p-4 border-b">
            <h3 v-if="title" class="text-xl font-semibold">{{ title }}</h3>
            <button
              v-if="showClose"
              @click="close"
              class="text-gray-400 hover:text-gray-600 transition-colors"
            >
              ✕
            </button>
          </div>
          
          <!-- Body -->
          <div class="p-6">
            <slot />
          </div>
          
          <!-- Footer -->
          <div v-if="$slots.footer" class="p-4 border-t bg-gray-50">
            <slot name="footer" />
          </div>
        </div>
      </div>
    </Transition>
  </Teleport>
</template>

<style scoped>
.modal-enter-active,
.modal-leave-active {
  transition: opacity 0.3s ease;
}

.modal-enter-from,
.modal-leave-to {
  opacity: 0;
}

.modal-enter-active .bg-white,
.modal-leave-active .bg-white {
  transition: transform 0.3s ease;
}

.modal-enter-from .bg-white,
.modal-leave-to .bg-white {
  transform: scale(0.9);
}
</style>
```

**Usage:**
```vue
<script setup lang="ts">
import { ref } from 'vue'

const showModal = ref(false)
</script>

<template>
  <Button @click="showModal = true">Open Modal</Button>
  
  <Modal v-model="showModal" title="Confirmation" size="md">
    <p>Are you sure you want to proceed?</p>
    
    <template #footer>
      <div class="flex justify-end gap-2">
        <Button variant="ghost" @click="showModal = false">Cancel</Button>
        <Button variant="primary" @click="handleConfirm">Confirm</Button>
      </div>
    </template>
  </Modal>
</template>
```

---

### Input

Text input with validation support.

```vue
<script setup lang="ts">
import { computed } from 'vue'

interface Props {
  modelValue: string
  type?: 'text' | 'email' | 'password' | 'number' | 'tel' | 'url'
  label?: string
  placeholder?: string
  error?: string
  disabled?: boolean
  required?: boolean
  icon?: string
}

const props = withDefaults(defineProps<Props>(), {
  type: 'text',
  disabled: false,
  required: false
})

const emit = defineEmits<{
  'update:modelValue': [value: string]
  blur: [event: FocusEvent]
  focus: [event: FocusEvent]
}>()

const inputClasses = computed(() => {
  const base = 'w-full px-4 py-2 border rounded-lg focus:outline-none focus:ring-2 transition-all duration-200'
  const state = props.error
    ? 'border-red-500 focus:ring-red-500'
    : 'border-gray-300 focus:ring-blue-500 focus:border-blue-500'
  const disabled = props.disabled ? 'bg-gray-100 cursor-not-allowed' : 'bg-white'
  const withIcon = props.icon ? 'pl-10' : ''
  
  return `${base} ${state} ${disabled} ${withIcon}`
})
</script>

<template>
  <div class="w-full">
    <label v-if="label" class="block text-sm font-medium text-gray-700 mb-1">
      {{ label }}
      <span v-if="required" class="text-red-500">*</span>
    </label>
    
    <div class="relative">
      <span v-if="icon" class="absolute left-3 top-1/2 -translate-y-1/2 text-gray-400">
        {{ icon }}
      </span>
      
      <input
        :type="type"
        :value="modelValue"
        :placeholder="placeholder"
        :disabled="disabled"
        :class="inputClasses"
        @input="emit('update:modelValue', ($event.target as HTMLInputElement).value)"
        @blur="emit('blur', $event)"
        @focus="emit('focus', $event)"
      />
    </div>
    
    <p v-if="error" class="mt-1 text-sm text-red-600">
      {{ error }}
    </p>
  </div>
</template>
```

**Usage:**
```vue
<Input
  v-model="email"
  type="email"
  label="Email Address"
  placeholder="you@example.com"
  icon="📧"
  :error="emailError"
  required
/>
```

---

### Select

Dropdown select component.

```vue
<script setup lang="ts">
import { computed } from 'vue'

interface Option {
  value: string | number
  label: string
  disabled?: boolean
}

interface Props {
  modelValue: string | number
  options: Option[]
  label?: string
  placeholder?: string
  error?: string
  disabled?: boolean
  required?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  placeholder: 'Select an option',
  disabled: false,
  required: false
})

const emit = defineEmits<{
  'update:modelValue': [value: string | number]
  change: [value: string | number]
}>()

const selectClasses = computed(() => {
  const base = 'w-full px-4 py-2 border rounded-lg focus:outline-none focus:ring-2 transition-all duration-200'
  const state = props.error
    ? 'border-red-500 focus:ring-red-500'
    : 'border-gray-300 focus:ring-blue-500 focus:border-blue-500'
  const disabled = props.disabled ? 'bg-gray-100 cursor-not-allowed' : 'bg-white'
  
  return `${base} ${state} ${disabled}`
})

const handleChange = (event: Event) => {
  const value = (event.target as HTMLSelectElement).value
  emit('update:modelValue', value)
  emit('change', value)
}
</script>

<template>
  <div class="w-full">
    <label v-if="label" class="block text-sm font-medium text-gray-700 mb-1">
      {{ label }}
      <span v-if="required" class="text-red-500">*</span>
    </label>
    
    <select
      :value="modelValue"
      :disabled="disabled"
      :class="selectClasses"
      @change="handleChange"
    >
      <option value="" disabled>{{ placeholder }}</option>
      <option
        v-for="option in options"
        :key="option.value"
        :value="option.value"
        :disabled="option.disabled"
      >
        {{ option.label }}
      </option>
    </select>
    
    <p v-if="error" class="mt-1 text-sm text-red-600">
      {{ error }}
    </p>
  </div>
</template>
```

---

### Tabs

Tab navigation component.

```vue
<script setup lang="ts">
import { ref, provide } from 'vue'

interface Props {
  modelValue?: string | number
}

const props = defineProps<Props>()

const emit = defineEmits<{
  'update:modelValue': [value: string | number]
}>()

const activeTab = ref(props.modelValue)

const setActiveTab = (value: string | number) => {
  activeTab.value = value
  emit('update:modelValue', value)
}

provide('activeTab', activeTab)
provide('setActiveTab', setActiveTab)
</script>

<template>
  <div class="w-full">
    <div class="border-b border-gray-200">
      <nav class="flex space-x-8">
        <slot name="tabs" />
      </nav>
    </div>
    
    <div class="mt-4">
      <slot />
    </div>
  </div>
</template>
```

**Tab Item:**
```vue
<script setup lang="ts">
import { inject, computed } from 'vue'

interface Props {
  value: string | number
  label: string
  disabled?: boolean
}

const props = defineProps<Props>()

const activeTab = inject<Ref<string | number>>('activeTab')
const setActiveTab = inject<(value: string | number) => void>('setActiveTab')

const isActive = computed(() => activeTab?.value === props.value)

const tabClasses = computed(() => {
  const base = 'py-2 px-1 border-b-2 font-medium text-sm transition-colors duration-200'
  const active = isActive.value
    ? 'border-blue-500 text-blue-600'
    : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
  const disabled = props.disabled ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'
  
  return `${base} ${active} ${disabled}`
})
</script>

<template>
  <button
    :class="tabClasses"
    :disabled="disabled"
    @click="setActiveTab?.(value)"
  >
    {{ label }}
  </button>
</template>
```

**Tab Panel:**
```vue
<script setup lang="ts">
import { inject, computed } from 'vue'

interface Props {
  value: string | number
}

const props = defineProps<Props>()

const activeTab = inject<Ref<string | number>>('activeTab')

const isActive = computed(() => activeTab?.value === props.value)
</script>

<template>
  <div v-if="isActive">
    <slot />
  </div>
</template>
```

**Usage:**
```vue
<Tabs v-model="activeTab">
  <template #tabs>
    <TabItem value="profile" label="Profile" />
    <TabItem value="settings" label="Settings" />
    <TabItem value="security" label="Security" />
  </template>
  
  <TabPanel value="profile">
    <h3>Profile Content</h3>
  </TabPanel>
  
  <TabPanel value="settings">
    <h3>Settings Content</h3>
  </TabPanel>
  
  <TabPanel value="security">
    <h3>Security Content</h3>
  </TabPanel>
</Tabs>
```

---

## Layout Components

### Container

Responsive container with max-width.

```vue
<script setup lang="ts">
interface Props {
  size?: 'sm' | 'md' | 'lg' | 'xl' | 'full'
  padding?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  size: 'lg',
  padding: true
})

const sizeClasses = {
  sm: 'max-w-2xl',
  md: 'max-w-4xl',
  lg: 'max-w-6xl',
  xl: 'max-w-7xl',
  full: 'max-w-full'
}
</script>

<template>
  <div :class="['mx-auto', sizeClasses[size], padding && 'px-4 sm:px-6 lg:px-8']">
    <slot />
  </div>
</template>
```

---

### Grid

Responsive grid layout.

```vue
<script setup lang="ts">
interface Props {
  cols?: 1 | 2 | 3 | 4 | 6 | 12
  gap?: 0 | 2 | 4 | 6 | 8
  responsive?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  cols: 3,
  gap: 4,
  responsive: true
})

const gridClasses = computed(() => {
  const base = 'grid'
  const gaps = `gap-${props.gap}`
  
  if (props.responsive) {
    return `${base} grid-cols-1 md:grid-cols-2 lg:grid-cols-${props.cols} ${gaps}`
  }
  
  return `${base} grid-cols-${props.cols} ${gaps}`
})
</script>

<template>
  <div :class="gridClasses">
    <slot />
  </div>
</template>
```

---

## Section Components

### Hero

Full-width hero section.

```vue
<script setup lang="ts">
interface Props {
  title: string
  subtitle?: string
  image?: string
  alignment?: 'left' | 'center' | 'right'
  overlay?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  alignment: 'center',
  overlay: false
})
</script>

<template>
  <section
    class="relative py-20 md:py-32"
    :style="image ? `background-image: url(${image}); background-size: cover; background-position: center;` : ''"
  >
    <div v-if="overlay && image" class="absolute inset-0 bg-black bg-opacity-50" />
    
    <Container>
      <div
        :class="[
          'relative z-10',
          alignment === 'center' && 'text-center',
          alignment === 'right' && 'text-right'
        ]"
      >
        <h1
          :class="[
            'text-4xl md:text-5xl lg:text-6xl font-bold',
            image ? 'text-white' : 'text-gray-900'
          ]"
        >
          {{ title }}
        </h1>
        
        <p
          v-if="subtitle"
          :class="[
            'mt-6 text-xl md:text-2xl',
            image ? 'text-gray-200' : 'text-gray-600'
          ]"
        >
          {{ subtitle }}
        </p>
        
        <div class="mt-10">
          <slot />
        </div>
      </div>
    </Container>
  </section>
</template>
```

**Usage:**
```vue
<Hero
  title="Welcome to Our Platform"
  subtitle="Build amazing things with Vue.js"
  image="/hero-bg.jpg"
  :overlay="true"
>
  <Button variant="primary" size="lg">Get Started</Button>
  <Button variant="outline" size="lg" class="ml-4">Learn More</Button>
</Hero>
```

---

### Features

Feature grid section.

```vue
<script setup lang="ts">
interface Feature {
  icon: string
  title: string
  description: string
}

interface Props {
  title?: string
  subtitle?: string
  features: Feature[]
  columns?: 2 | 3 | 4
}

const props = withDefaults(defineProps<Props>(), {
  columns: 3
})
</script>

<template>
  <section class="py-16 bg-gray-50">
    <Container>
      <div v-if="title || subtitle" class="text-center mb-12">
        <h2 v-if="title" class="text-3xl md:text-4xl font-bold text-gray-900">
          {{ title }}
        </h2>
        <p v-if="subtitle" class="mt-4 text-xl text-gray-600">
          {{ subtitle }}
        </p>
      </div>
      
      <Grid :cols="columns" :gap="8">
        <Card
          v-for="(feature, index) in features"
          :key="index"
          hoverable
          padding="lg"
        >
          <div class="text-4xl mb-4">{{ feature.icon }}</div>
          <h3 class="text-xl font-semibold text-gray-900 mb-2">
            {{ feature.title }}
          </h3>
          <p class="text-gray-600">
            {{ feature.description }}
          </p>
        </Card>
      </Grid>
    </Container>
  </section>
</template>
```

This component library provides a solid foundation for creating production-quality mockups. Each component is fully typed, accessible, and follows TailwindCSS best practices.
