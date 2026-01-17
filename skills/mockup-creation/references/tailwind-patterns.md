# TailwindCSS Patterns

Advanced TailwindCSS patterns and best practices for mockup creation.

## Table of Contents

- [Responsive Design](#responsive-design)
- [Dark Mode](#dark-mode)
- [Animations & Transitions](#animations--transitions)
- [Custom Utilities](#custom-utilities)
- [Layout Patterns](#layout-patterns)
- [Color Palettes](#color-palettes)
- [Typography System](#typography-system)
- [Spacing Patterns](#spacing-patterns)
- [Component Variants](#component-variants)

---

## Responsive Design

### Mobile-First Approach

Always start with mobile styles, then enhance for larger screens:

```vue
<template>
  <!-- Mobile: single column, Desktop: three columns -->
  <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
    <Card />
  </div>
  
  <!-- Mobile: stacked, Desktop: side-by-side -->
  <div class="flex flex-col lg:flex-row gap-6">
    <div class="w-full lg:w-1/3">Sidebar</div>
    <div class="w-full lg:w-2/3">Main Content</div>
  </div>
  
  <!-- Hidden on mobile, visible on desktop -->
  <div class="hidden lg:block">Desktop Only</div>
  
  <!-- Visible on mobile, hidden on desktop -->
  <div class="lg:hidden">Mobile Only</div>
</template>
```

### Breakpoint Reference

| Breakpoint | Class | Min Width |
|------------|-------|-----------|
| Small | `sm:` | 640px |
| Medium | `md:` | 768px |
| Large | `lg:` | 1024px |
| Extra Large | `xl:` | 1280px |
| 2X Large | `2xl:` | 1536px |

### Container Queries

For component-based responsive design:

```javascript
// tailwind.config.js
export default {
  theme: {
    extend: {
      containers: {
        '2xs': '16rem',
        'xs': '20rem',
        'sm': '24rem',
        'md': '28rem',
        'lg': '32rem',
        'xl': '36rem',
      }
    }
  },
  plugins: [
    require('@tailwindcss/container-queries'),
  ],
}
```

```vue
<template>
  <div class="@container">
    <div class="@md:flex @md:gap-4">
      <!-- Responds to container width, not viewport -->
    </div>
  </div>
</template>
```

---

## Dark Mode

### Setup Dark Mode

```javascript
// tailwind.config.js
export default {
  darkMode: 'class', // or 'media' for system preference
  // ... rest of config
}
```

### Dark Mode Utilities

```vue
<template>
  <!-- Background colors -->
  <div class="bg-white dark:bg-gray-900">
    <!-- Text colors -->
    <h1 class="text-gray-900 dark:text-white">Title</h1>
    <p class="text-gray-600 dark:text-gray-400">Content</p>
  </div>
  
  <!-- Borders -->
  <div class="border border-gray-200 dark:border-gray-700">
    Content
  </div>
  
  <!-- Hover states -->
  <button class="bg-blue-600 hover:bg-blue-700 dark:bg-blue-500 dark:hover:bg-blue-600">
    Click Me
  </button>
</template>
```

### Dark Mode Toggle Component

```vue
<script setup lang="ts">
import { ref, onMounted } from 'vue'

const isDark = ref(false)

const toggleDark = () => {
  isDark.value = !isDark.value
  
  if (isDark.value) {
    document.documentElement.classList.add('dark')
    localStorage.setItem('theme', 'dark')
  } else {
    document.documentElement.classList.remove('dark')
    localStorage.setItem('theme', 'light')
  }
}

onMounted(() => {
  // Check saved preference
  const savedTheme = localStorage.getItem('theme')
  if (savedTheme === 'dark') {
    isDark.value = true
    document.documentElement.classList.add('dark')
  }
})
</script>

<template>
  <button
    @click="toggleDark"
    class="p-2 rounded-lg bg-gray-200 dark:bg-gray-700 text-gray-800 dark:text-gray-200"
  >
    {{ isDark ? '🌙' : '☀️' }}
  </button>
</template>
```

---

## Animations & Transitions

### Built-in Animations

```vue
<template>
  <!-- Spin -->
  <div class="animate-spin">⚪</div>
  
  <!-- Ping -->
  <div class="relative">
    <span class="animate-ping absolute inline-flex h-full w-full rounded-full bg-blue-400 opacity-75"></span>
    <span class="relative inline-flex rounded-full h-3 w-3 bg-blue-500"></span>
  </div>
  
  <!-- Pulse -->
  <div class="animate-pulse bg-gray-200 h-4 w-full rounded"></div>
  
  <!-- Bounce -->
  <div class="animate-bounce">↓</div>
</template>
```

### Custom Transitions

```javascript
// tailwind.config.js
export default {
  theme: {
    extend: {
      transitionProperty: {
        'height': 'height',
        'spacing': 'margin, padding',
      },
      transitionDuration: {
        '2000': '2000ms',
      },
      transitionTimingFunction: {
        'bounce-in': 'cubic-bezier(0.68, -0.55, 0.265, 1.55)',
      },
    }
  }
}
```

```vue
<template>
  <!-- Smooth transitions -->
  <button class="transition-all duration-300 hover:scale-110 hover:shadow-lg">
    Hover Me
  </button>
  
  <!-- Transform transitions -->
  <div class="transition-transform duration-500 hover:rotate-180">
    Rotate
  </div>
  
  <!-- Multiple properties -->
  <div class="transition-[background-color,transform] duration-300 hover:bg-blue-500 hover:scale-105">
    Transform
  </div>
</template>
```

### Vue Transitions with Tailwind

```vue
<template>
  <Transition
    enter-active-class="transition-all duration-300"
    enter-from-class="opacity-0 scale-90"
    enter-to-class="opacity-100 scale-100"
    leave-active-class="transition-all duration-300"
    leave-from-class="opacity-100 scale-100"
    leave-to-class="opacity-0 scale-90"
  >
    <div v-if="show">Content</div>
  </Transition>
</template>
```

---

## Custom Utilities

### Extending Tailwind Config

```javascript
// tailwind.config.js
export default {
  theme: {
    extend: {
      colors: {
        brand: {
          50: '#f0f9ff',
          100: '#e0f2fe',
          200: '#bae6fd',
          300: '#7dd3fc',
          400: '#38bdf8',
          500: '#0ea5e9',
          600: '#0284c7',
          700: '#0369a1',
          800: '#075985',
          900: '#0c4a6e',
        }
      },
      spacing: {
        '128': '32rem',
        '144': '36rem',
      },
      borderRadius: {
        '4xl': '2rem',
      },
      boxShadow: {
        'inner-lg': 'inset 0 2px 4px 0 rgb(0 0 0 / 0.1)',
      },
      fontFamily: {
        display: ['Inter', 'system-ui', 'sans-serif'],
        body: ['Inter', 'system-ui', 'sans-serif'],
      },
    }
  }
}
```

### Custom Component Classes

```javascript
// tailwind.config.js
export default {
  plugins: [
    function({ addComponents }) {
      addComponents({
        '.btn-primary': {
          '@apply px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors': {},
        },
        '.card': {
          '@apply bg-white rounded-lg shadow-md p-6': {},
        },
        '.input': {
          '@apply w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500': {},
        }
      })
    }
  ]
}
```

---

## Layout Patterns

### Sticky Header

```vue
<template>
  <header class="sticky top-0 z-50 bg-white shadow-md">
    <Container>
      <nav class="flex items-center justify-between h-16">
        <Logo />
        <Navigation />
      </nav>
    </Container>
  </header>
</template>
```

### Full-Height Layout

```vue
<template>
  <div class="min-h-screen flex flex-col">
    <Header class="flex-shrink-0" />
    <main class="flex-1">
      <slot />
    </main>
    <Footer class="flex-shrink-0" />
  </div>
</template>
```

### Sidebar Layout

```vue
<template>
  <div class="flex h-screen">
    <!-- Sidebar -->
    <aside class="w-64 bg-gray-900 text-white overflow-y-auto">
      <Sidebar />
    </aside>
    
    <!-- Main Content -->
    <div class="flex-1 flex flex-col overflow-hidden">
      <header class="bg-white shadow-sm">
        <TopBar />
      </header>
      
      <main class="flex-1 overflow-y-auto p-6 bg-gray-50">
        <slot />
      </main>
    </div>
  </div>
</template>
```

### Centered Card

```vue
<template>
  <div class="min-h-screen flex items-center justify-center bg-gray-100 p-4">
    <Card class="w-full max-w-md">
      <slot />
    </Card>
  </div>
</template>
```

### Masonry Grid

```vue
<template>
  <!-- Using CSS columns -->
  <div class="columns-1 md:columns-2 lg:columns-3 gap-4">
    <div v-for="item in items" :key="item.id" class="break-inside-avoid mb-4">
      <Card>{{ item.content }}</Card>
    </div>
  </div>
</template>
```

---

## Color Palettes

### Professional Color Schemes

**Blue Theme (Trust, Corporate):**
```javascript
colors: {
  primary: {
    50: '#eff6ff',
    500: '#3b82f6',
    900: '#1e3a8a',
  },
  secondary: {
    500: '#64748b',
  }
}
```

**Green Theme (Growth, Health):**
```javascript
colors: {
  primary: {
    50: '#f0fdf4',
    500: '#10b981',
    900: '#064e3b',
  }
}
```

**Purple Theme (Creative, Luxury):**
```javascript
colors: {
  primary: {
    50: '#faf5ff',
    500: '#a855f7',
    900: '#581c87',
  }
}
```

### Semantic Colors

```vue
<template>
  <!-- Success -->
  <div class="bg-green-50 border-l-4 border-green-500 text-green-700 p-4">
    Success message
  </div>
  
  <!-- Warning -->
  <div class="bg-yellow-50 border-l-4 border-yellow-500 text-yellow-700 p-4">
    Warning message
  </div>
  
  <!-- Error -->
  <div class="bg-red-50 border-l-4 border-red-500 text-red-700 p-4">
    Error message
  </div>
  
  <!-- Info -->
  <div class="bg-blue-50 border-l-4 border-blue-500 text-blue-700 p-4">
    Info message
  </div>
</template>
```

---

## Typography System

### Type Scale

```javascript
// tailwind.config.js
export default {
  theme: {
    extend: {
      fontSize: {
        'xs': '0.75rem',      // 12px
        'sm': '0.875rem',     // 14px
        'base': '1rem',       // 16px
        'lg': '1.125rem',     // 18px
        'xl': '1.25rem',      // 20px
        '2xl': '1.5rem',      // 24px
        '3xl': '1.875rem',    // 30px
        '4xl': '2.25rem',     // 36px
        '5xl': '3rem',        // 48px
        '6xl': '3.75rem',     // 60px
        '7xl': '4.5rem',      // 72px
      }
    }
  }
}
```

### Typography Components

```vue
<template>
  <!-- Heading 1 -->
  <h1 class="text-4xl md:text-5xl lg:text-6xl font-bold text-gray-900 leading-tight">
    Main Heading
  </h1>
  
  <!-- Heading 2 -->
  <h2 class="text-3xl md:text-4xl font-bold text-gray-900 leading-snug">
    Section Heading
  </h2>
  
  <!-- Body Text -->
  <p class="text-base md:text-lg text-gray-600 leading-relaxed">
    Body paragraph with comfortable reading line height.
  </p>
  
  <!-- Caption -->
  <p class="text-sm text-gray-500">
    Small caption text
  </p>
  
  <!-- Link -->
  <a class="text-blue-600 hover:text-blue-700 underline underline-offset-2">
    Link text
  </a>
</template>
```

### Line Height System

```vue
<template>
  <!-- Tight for headings -->
  <h1 class="leading-tight">Heading</h1>
  
  <!-- Relaxed for body -->
  <p class="leading-relaxed">Body text</p>
  
  <!-- Loose for large text -->
  <p class="text-2xl leading-loose">Large text</p>
</template>
```

---

## Spacing Patterns

### Consistent Spacing

```vue
<template>
  <section class="py-12 md:py-16 lg:py-20">
    <Container>
      <!-- Section header -->
      <div class="mb-8 md:mb-12">
        <h2 class="text-3xl font-bold mb-4">Section Title</h2>
        <p class="text-xl text-gray-600">Section subtitle</p>
      </div>
      
      <!-- Content grid -->
      <Grid class="gap-6 md:gap-8">
        <Card class="p-6 md:p-8">
          <h3 class="text-xl font-semibold mb-3">Card Title</h3>
          <p class="text-gray-600 mb-4">Card content</p>
          <Button>Action</Button>
        </Card>
      </Grid>
    </Container>
  </section>
</template>
```

### Vertical Rhythm

```vue
<template>
  <article class="prose max-w-none">
    <h1 class="mb-4">Article Title</h1>
    <p class="mb-4">First paragraph</p>
    <p class="mb-4">Second paragraph</p>
    <h2 class="mt-8 mb-4">Subheading</h2>
    <p class="mb-4">Content under subheading</p>
    <ul class="mb-4 space-y-2">
      <li>List item 1</li>
      <li>List item 2</li>
    </ul>
  </article>
</template>
```

---

## Component Variants

### Button Variants

```vue
<template>
  <!-- Solid -->
  <button class="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
    Solid
  </button>
  
  <!-- Outline -->
  <button class="px-4 py-2 border-2 border-blue-600 text-blue-600 rounded-lg hover:bg-blue-50">
    Outline
  </button>
  
  <!-- Ghost -->
  <button class="px-4 py-2 text-blue-600 rounded-lg hover:bg-blue-50">
    Ghost
  </button>
  
  <!-- Gradient -->
  <button class="px-4 py-2 bg-gradient-to-r from-blue-600 to-purple-600 text-white rounded-lg hover:from-blue-700 hover:to-purple-700">
    Gradient
  </button>
  
  <!-- Icon Button -->
  <button class="p-2 rounded-full hover:bg-gray-100">
    🔍
  </button>
</template>
```

### Card Variants

```vue
<template>
  <!-- Default -->
  <div class="bg-white rounded-lg shadow-md p-6">
    Default Card
  </div>
  
  <!-- Bordered -->
  <div class="bg-white border-2 border-gray-200 rounded-lg p-6">
    Bordered Card
  </div>
  
  <!-- Elevated -->
  <div class="bg-white rounded-lg shadow-xl hover:shadow-2xl transition-shadow p-6">
    Elevated Card
  </div>
  
  <!-- Colored -->
  <div class="bg-gradient-to-br from-blue-500 to-purple-600 text-white rounded-lg shadow-lg p-6">
    Colored Card
  </div>
  
  <!-- With Image -->
  <div class="bg-white rounded-lg shadow-md overflow-hidden">
    <img src="/image.jpg" class="w-full h-48 object-cover" />
    <div class="p-6">
      Card with Image
    </div>
  </div>
</template>
```

### Badge Variants

```vue
<template>
  <!-- Default -->
  <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
    Badge
  </span>
  
  <!-- Pill -->
  <span class="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-green-100 text-green-800">
    Success
  </span>
  
  <!-- Dot -->
  <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
    <span class="w-2 h-2 mr-1.5 rounded-full bg-red-400"></span>
    Error
  </span>
  
  <!-- Outlined -->
  <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border border-blue-300 text-blue-700">
    Outlined
  </span>
</template>
```

---

## Accessibility Patterns

### Focus States

```vue
<template>
  <!-- Visible focus ring -->
  <button class="focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 rounded-lg px-4 py-2">
    Accessible Button
  </button>
  
  <!-- Custom focus style -->
  <input class="focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500 border rounded-lg px-4 py-2" />
</template>
```

### Screen Reader Support

```vue
<template>
  <!-- Accessible button -->
  <button
    aria-label="Close modal"
    class="p-2 hover:bg-gray-100 rounded-full"
  >
    ✕
  </button>
  
  <!-- Skip to content -->
  <a href="#main-content" class="sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4">
    Skip to content
  </a>
</template>
```

This reference covers the most commonly used TailwindCSS patterns. For more advanced techniques, consult the official TailwindCSS documentation.
