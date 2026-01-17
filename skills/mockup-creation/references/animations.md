# Animations & Transitions Guide

Advanced animation patterns for Vue.js 3 mockups using TailwindCSS and native CSS.

## Table of Contents

- [CSS Transitions](#css-transitions)
- [Vue Transitions](#vue-transitions)
- [Tailwind Animations](#tailwind-animations)
- [Custom Animations](#custom-animations)
- [Loading States](#loading-states)
- [Micro-interactions](#micro-interactions)
- [Page Transitions](#page-transitions)

---

## CSS Transitions

### Basic Transition Properties

```vue
<template>
  <!-- Transition all properties -->
  <button class="transition-all duration-300 hover:scale-110">
    Hover Me
  </button>
  
  <!-- Transition specific properties -->
  <div class="transition-colors duration-200 hover:bg-blue-600">
    Color Transition
  </div>
  
  <!-- Multiple properties -->
  <div class="transition-[background-color,transform] duration-300 ease-in-out">
    Multiple Properties
  </div>
</template>
```

### Timing Functions

```vue
<template>
  <!-- Linear -->
  <div class="transition-all duration-300 ease-linear">Linear</div>
  
  <!-- Ease (default) -->
  <div class="transition-all duration-300 ease-in-out">Ease In Out</div>
  
  <!-- Custom cubic-bezier -->
  <div class="transition-all duration-300" style="transition-timing-function: cubic-bezier(0.68, -0.55, 0.265, 1.55)">
    Bounce
  </div>
</template>
```

---

## Vue Transitions

### Basic Vue Transition

```vue
<script setup lang="ts">
import { ref } from 'vue'

const show = ref(true)
</script>

<template>
  <button @click="show = !show">Toggle</button>
  
  <Transition name="fade">
    <div v-if="show" class="p-4 bg-blue-100 rounded">
      Fade transition content
    </div>
  </Transition>
</template>

<style scoped>
.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.3s ease;
}

.fade-enter-from,
.fade-leave-to {
  opacity: 0;
}
</style>
```

### Transition with TailwindCSS Classes

```vue
<template>
  <Transition
    enter-active-class="transition-all duration-300 ease-out"
    enter-from-class="opacity-0 scale-90"
    enter-to-class="opacity-100 scale-100"
    leave-active-class="transition-all duration-200 ease-in"
    leave-from-class="opacity-100 scale-100"
    leave-to-class="opacity-0 scale-90"
  >
    <div v-if="show" class="p-4 bg-white rounded-lg shadow-lg">
      Content with scale and fade
    </div>
  </Transition>
</template>
```

### Slide Transitions

```vue
<template>
  <!-- Slide from right -->
  <Transition
    enter-active-class="transition-transform duration-300"
    enter-from-class="translate-x-full"
    enter-to-class="translate-x-0"
    leave-active-class="transition-transform duration-300"
    leave-from-class="translate-x-0"
    leave-to-class="translate-x-full"
  >
    <div v-if="show" class="fixed right-0 top-0 h-full w-80 bg-white shadow-xl p-6">
      Sidebar content
    </div>
  </Transition>
  
  <!-- Slide from top -->
  <Transition
    enter-active-class="transition-transform duration-300"
    enter-from-class="-translate-y-full"
    enter-to-class="translate-y-0"
    leave-active-class="transition-transform duration-300"
    leave-from-class="translate-y-0"
    leave-to-class="-translate-y-full"
  >
    <div v-if="show">Dropdown content</div>
  </Transition>
</template>
```

### TransitionGroup

```vue
<script setup lang="ts">
import { ref } from 'vue'

interface Item {
  id: number
  text: string
}

const items = ref<Item[]>([
  { id: 1, text: 'Item 1' },
  { id: 2, text: 'Item 2' },
  { id: 3, text: 'Item 3' }
])

const addItem = () => {
  items.value.push({
    id: Date.now(),
    text: `Item ${items.value.length + 1}`
  })
}

const removeItem = (id: number) => {
  items.value = items.value.filter(item => item.id !== id)
}
</script>

<template>
  <button @click="addItem" class="mb-4 px-4 py-2 bg-blue-600 text-white rounded">
    Add Item
  </button>
  
  <TransitionGroup
    name="list"
    tag="div"
    class="space-y-2"
  >
    <div
      v-for="item in items"
      :key="item.id"
      class="p-4 bg-white rounded-lg shadow flex items-center justify-between"
    >
      <span>{{ item.text }}</span>
      <button @click="removeItem(item.id)" class="text-red-600">Remove</button>
    </div>
  </TransitionGroup>
</template>

<style scoped>
.list-enter-active,
.list-leave-active {
  transition: all 0.5s ease;
}

.list-enter-from {
  opacity: 0;
  transform: translateX(-30px);
}

.list-leave-to {
  opacity: 0;
  transform: translateX(30px);
}

/* Move animation */
.list-move {
  transition: transform 0.5s ease;
}
</style>
```

---

## Tailwind Animations

### Built-in Animations

```vue
<template>
  <!-- Spin -->
  <div class="w-8 h-8 border-4 border-blue-600 border-t-transparent rounded-full animate-spin" />
  
  <!-- Ping -->
  <div class="relative">
    <span class="absolute inline-flex h-full w-full rounded-full bg-blue-400 opacity-75 animate-ping" />
    <span class="relative inline-flex rounded-full h-3 w-3 bg-blue-600" />
  </div>
  
  <!-- Pulse -->
  <div class="w-4 h-4 bg-blue-600 rounded-full animate-pulse" />
  
  <!-- Bounce -->
  <div class="animate-bounce">↓</div>
</template>
```

### Custom Animation Utilities

```javascript
// tailwind.config.js
export default {
  theme: {
    extend: {
      animation: {
        'fade-in': 'fadeIn 0.5s ease-in',
        'slide-in': 'slideIn 0.3s ease-out',
        'scale-in': 'scaleIn 0.3s ease-out',
        'shake': 'shake 0.5s ease-in-out',
        'wiggle': 'wiggle 1s ease-in-out infinite',
      },
      keyframes: {
        fadeIn: {
          '0%': { opacity: '0' },
          '100%': { opacity: '1' }
        },
        slideIn: {
          '0%': { transform: 'translateY(-10px)', opacity: '0' },
          '100%': { transform: 'translateY(0)', opacity: '1' }
        },
        scaleIn: {
          '0%': { transform: 'scale(0.9)', opacity: '0' },
          '100%': { transform: 'scale(1)', opacity: '1' }
        },
        shake: {
          '0%, 100%': { transform: 'translateX(0)' },
          '10%, 30%, 50%, 70%, 90%': { transform: 'translateX(-10px)' },
          '20%, 40%, 60%, 80%': { transform: 'translateX(10px)' }
        },
        wiggle: {
          '0%, 100%': { transform: 'rotate(-3deg)' },
          '50%': { transform: 'rotate(3deg)' }
        }
      }
    }
  }
}
```

**Usage:**
```vue
<template>
  <div class="animate-fade-in">Fades in</div>
  <div class="animate-slide-in">Slides in</div>
  <div class="animate-scale-in">Scales in</div>
  <div class="animate-shake">Shakes</div>
  <div class="animate-wiggle">Wiggles</div>
</template>
```

---

## Loading States

### Skeleton Loader

```vue
<script setup lang="ts">
import { ref, onMounted } from 'vue'

const loading = ref(true)

onMounted(() => {
  setTimeout(() => {
    loading.value = false
  }, 2000)
})
</script>

<template>
  <div v-if="loading" class="space-y-4">
    <!-- Skeleton card -->
    <div class="bg-white rounded-lg p-6 shadow-md">
      <div class="animate-pulse space-y-4">
        <!-- Header -->
        <div class="flex items-center space-x-4">
          <div class="w-12 h-12 bg-gray-300 rounded-full" />
          <div class="flex-1 space-y-2">
            <div class="h-4 bg-gray-300 rounded w-1/4" />
            <div class="h-3 bg-gray-300 rounded w-1/3" />
          </div>
        </div>
        
        <!-- Body -->
        <div class="space-y-2">
          <div class="h-4 bg-gray-300 rounded" />
          <div class="h-4 bg-gray-300 rounded w-5/6" />
          <div class="h-4 bg-gray-300 rounded w-4/6" />
        </div>
      </div>
    </div>
  </div>
  
  <div v-else>
    <!-- Actual content -->
  </div>
</template>
```

### Spinner Component

```vue
<script setup lang="ts">
interface Props {
  size?: 'sm' | 'md' | 'lg'
  color?: string
}

const props = withDefaults(defineProps<Props>(), {
  size: 'md',
  color: 'blue'
})

const sizeClasses = {
  sm: 'w-4 h-4 border-2',
  md: 'w-8 h-8 border-4',
  lg: 'w-12 h-12 border-4'
}
</script>

<template>
  <div
    :class="[
      'rounded-full animate-spin',
      sizeClasses[size],
      `border-${color}-600 border-t-transparent`
    ]"
  />
</template>
```

### Progress Bar

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

interface Props {
  value: number
  max?: number
  showLabel?: boolean
  animated?: boolean
}

const props = withDefaults(defineProps<Props>(), {
  max: 100,
  showLabel: true,
  animated: true
})

const percentage = computed(() => {
  return Math.min(100, (props.value / props.max) * 100)
})
</script>

<template>
  <div class="w-full">
    <div v-if="showLabel" class="flex justify-between mb-1 text-sm">
      <span>Progress</span>
      <span>{{ Math.round(percentage) }}%</span>
    </div>
    
    <div class="w-full bg-gray-200 rounded-full h-2">
      <div
        class="bg-blue-600 h-2 rounded-full transition-all duration-500"
        :class="{ 'animate-pulse': animated }"
        :style="{ width: `${percentage}%` }"
      />
    </div>
  </div>
</template>
```

---

## Micro-interactions

### Button Hover Effects

```vue
<template>
  <!-- Scale on hover -->
  <button class="px-4 py-2 bg-blue-600 text-white rounded-lg transition-transform hover:scale-105">
    Scale
  </button>
  
  <!-- Lift effect -->
  <button class="px-4 py-2 bg-blue-600 text-white rounded-lg transition-all hover:-translate-y-1 hover:shadow-lg">
    Lift
  </button>
  
  <!-- Glow effect -->
  <button class="px-4 py-2 bg-blue-600 text-white rounded-lg transition-shadow hover:shadow-[0_0_20px_rgba(59,130,246,0.5)]">
    Glow
  </button>
  
  <!-- Ripple effect -->
  <button class="relative overflow-hidden px-4 py-2 bg-blue-600 text-white rounded-lg group">
    <span class="relative z-10">Ripple</span>
    <span class="absolute inset-0 bg-white opacity-0 group-hover:opacity-20 transition-opacity" />
  </button>
</template>
```

### Card Hover Effects

```vue
<template>
  <!-- Lift with shadow -->
  <div class="bg-white rounded-lg p-6 shadow-md transition-all hover:-translate-y-2 hover:shadow-xl">
    Card content
  </div>
  
  <!-- Border highlight -->
  <div class="bg-white rounded-lg p-6 border-2 border-transparent transition-colors hover:border-blue-600">
    Card content
  </div>
  
  <!-- Background gradient -->
  <div class="bg-white rounded-lg p-6 transition-all hover:bg-gradient-to-br hover:from-blue-50 hover:to-purple-50">
    Card content
  </div>
</template>
```

### Input Focus Effects

```vue
<template>
  <!-- Expanding border -->
  <input
    class="w-full px-4 py-2 border-2 border-gray-300 rounded-lg focus:outline-none focus:border-blue-600 focus:scale-105 transition-all"
  />
  
  <!-- Glow ring -->
  <input
    class="w-full px-4 py-2 border-2 border-gray-300 rounded-lg focus:outline-none focus:ring-4 focus:ring-blue-200 focus:border-blue-600 transition-all"
  />
  
  <!-- Bottom border animation -->
  <div class="relative">
    <input
      class="w-full px-4 py-2 border-b-2 border-gray-300 focus:outline-none peer"
    />
    <div class="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 peer-focus:w-full transition-all duration-300" />
  </div>
</template>
```

---

## Page Transitions

### Route Transitions with Vue Router

```typescript
// router/index.ts
import { createRouter, createWebHistory } from 'vue-router'

const router = createRouter({
  history: createWebHistory(),
  routes: [
    // Your routes
  ],
  scrollBehavior(to, from, savedPosition) {
    if (savedPosition) {
      return savedPosition
    }
    return { top: 0 }
  }
})

export default router
```

```vue
<!-- App.vue -->
<script setup lang="ts">
import { useRoute } from 'vue-router'
import { computed } from 'vue'

const route = useRoute()
const transitionName = computed(() => {
  // Customize based on route
  return 'fade'
})
</script>

<template>
  <Transition
    :name="transitionName"
    mode="out-in"
  >
    <router-view :key="route.path" />
  </Transition>
</template>

<style>
/* Fade transition */
.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.3s ease;
}

.fade-enter-from,
.fade-leave-to {
  opacity: 0;
}

/* Slide transition */
.slide-enter-active,
.slide-leave-active {
  transition: transform 0.3s ease, opacity 0.3s ease;
}

.slide-enter-from {
  transform: translateX(20px);
  opacity: 0;
}

.slide-leave-to {
  transform: translateX(-20px);
  opacity: 0;
}
</style>
```

### Modal Transitions

```vue
<script setup lang="ts">
import { ref } from 'vue'

const show = ref(false)
</script>

<template>
  <button @click="show = true">Open Modal</button>
  
  <Teleport to="body">
    <Transition name="modal">
      <div
        v-if="show"
        class="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50"
        @click="show = false"
      >
        <div
          class="bg-white rounded-lg p-6 max-w-md w-full mx-4"
          @click.stop
        >
          <h2 class="text-xl font-bold mb-4">Modal Title</h2>
          <p>Modal content</p>
          <button @click="show = false" class="mt-4 px-4 py-2 bg-blue-600 text-white rounded">
            Close
          </button>
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

.modal-enter-active > div,
.modal-leave-active > div {
  transition: transform 0.3s ease;
}

.modal-enter-from > div {
  transform: scale(0.9) translateY(-20px);
}

.modal-leave-to > div {
  transform: scale(0.9) translateY(-20px);
}
</style>
```

These animation patterns create engaging, performant user experiences while maintaining accessibility and code quality.
