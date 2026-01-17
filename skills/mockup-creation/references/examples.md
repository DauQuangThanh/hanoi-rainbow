# Complete Mockup Examples

Production-ready mockup examples using Vue.js 3, TypeScript, Vite, and TailwindCSS.

## Table of Contents

- [E-commerce Product Page](#e-commerce-product-page)
- [SaaS Dashboard](#saas-dashboard)
- [Portfolio Landing Page](#portfolio-landing-page)
- [Admin Panel](#admin-panel)
- [Blog Homepage](#blog-homepage)

---

## E-commerce Product Page

Complete product page with gallery, reviews, and cart functionality.

### ProductPage.vue

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useCart } from '@/composables/useCart'

interface Product {
  id: string
  name: string
  price: number
  originalPrice?: number
  rating: number
  reviews: number
  images: string[]
  description: string
  features: string[]
  sizes: string[]
  colors: { name: string; hex: string }[]
}

const product = ref<Product>({
  id: '1',
  name: 'Premium Cotton T-Shirt',
  price: 29.99,
  originalPrice: 49.99,
  rating: 4.5,
  reviews: 128,
  images: [
    '/product-1.jpg',
    '/product-2.jpg',
    '/product-3.jpg',
    '/product-4.jpg'
  ],
  description: 'Made from 100% organic cotton, this premium t-shirt offers exceptional comfort and durability. Perfect for everyday wear.',
  features: [
    '100% Organic Cotton',
    'Machine Washable',
    'Breathable Fabric',
    'Eco-Friendly Dyes',
    'Regular Fit'
  ],
  sizes: ['XS', 'S', 'M', 'L', 'XL', 'XXL'],
  colors: [
    { name: 'Black', hex: '#000000' },
    { name: 'White', hex: '#FFFFFF' },
    { name: 'Navy', hex: '#1e3a8a' },
    { name: 'Gray', hex: '#6b7280' }
  ]
})

const selectedImage = ref(0)
const selectedSize = ref('')
const selectedColor = ref('')
const quantity = ref(1)

const { addToCart } = useCart()

const discount = computed(() => {
  if (product.value.originalPrice) {
    return Math.round(
      ((product.value.originalPrice - product.value.price) / product.value.originalPrice) * 100
    )
  }
  return 0
})

const handleAddToCart = () => {
  if (!selectedSize.value || !selectedColor.value) {
    alert('Please select size and color')
    return
  }
  
  addToCart({
    productId: product.value.id,
    name: product.value.name,
    price: product.value.price,
    quantity: quantity.value,
    size: selectedSize.value,
    color: selectedColor.value,
    image: product.value.images[0]
  })
}
</script>

<template>
  <div class="min-h-screen bg-gray-50">
    <Container class="py-8">
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-12">
        <!-- Image Gallery -->
        <div>
          <!-- Main Image -->
          <div class="bg-white rounded-lg overflow-hidden shadow-lg mb-4">
            <img
              :src="product.images[selectedImage]"
              :alt="product.name"
              class="w-full aspect-square object-cover"
            />
          </div>
          
          <!-- Thumbnail Gallery -->
          <div class="grid grid-cols-4 gap-4">
            <button
              v-for="(image, index) in product.images"
              :key="index"
              @click="selectedImage = index"
              :class="[
                'relative overflow-hidden rounded-lg border-2 transition-all',
                selectedImage === index
                  ? 'border-blue-600'
                  : 'border-gray-200 hover:border-gray-300'
              ]"
            >
              <img :src="image" :alt="`${product.name} ${index + 1}`" class="w-full aspect-square object-cover" />
            </button>
          </div>
        </div>
        
        <!-- Product Info -->
        <div>
          <h1 class="text-3xl font-bold text-gray-900 mb-2">
            {{ product.name }}
          </h1>
          
          <!-- Rating -->
          <div class="flex items-center gap-2 mb-4">
            <div class="flex">
              <span v-for="i in 5" :key="i" class="text-yellow-400">
                {{ i <= product.rating ? '★' : '☆' }}
              </span>
            </div>
            <span class="text-gray-600">{{ product.rating }} ({{ product.reviews }} reviews)</span>
          </div>
          
          <!-- Price -->
          <div class="flex items-baseline gap-3 mb-6">
            <span class="text-4xl font-bold text-gray-900">
              ${{ product.price }}
            </span>
            <span v-if="product.originalPrice" class="text-2xl text-gray-500 line-through">
              ${{ product.originalPrice }}
            </span>
            <span v-if="discount" class="px-3 py-1 bg-red-100 text-red-700 rounded-full text-sm font-semibold">
              -{{ discount }}%
            </span>
          </div>
          
          <!-- Description -->
          <p class="text-gray-600 mb-6">
            {{ product.description }}
          </p>
          
          <!-- Features -->
          <div class="mb-6">
            <h3 class="text-lg font-semibold mb-3">Features</h3>
            <ul class="space-y-2">
              <li v-for="feature in product.features" :key="feature" class="flex items-center text-gray-600">
                <span class="text-green-500 mr-2">✓</span>
                {{ feature }}
              </li>
            </ul>
          </div>
          
          <!-- Color Selection -->
          <div class="mb-6">
            <h3 class="text-sm font-semibold mb-3">Color</h3>
            <div class="flex gap-3">
              <button
                v-for="color in product.colors"
                :key="color.name"
                @click="selectedColor = color.name"
                :class="[
                  'w-10 h-10 rounded-full border-2 transition-all',
                  selectedColor === color.name
                    ? 'border-blue-600 scale-110'
                    : 'border-gray-300 hover:border-gray-400'
                ]"
                :style="{ backgroundColor: color.hex }"
                :title="color.name"
              />
            </div>
          </div>
          
          <!-- Size Selection -->
          <div class="mb-6">
            <h3 class="text-sm font-semibold mb-3">Size</h3>
            <div class="flex gap-2">
              <button
                v-for="size in product.sizes"
                :key="size"
                @click="selectedSize = size"
                :class="[
                  'px-4 py-2 border-2 rounded-lg font-medium transition-all',
                  selectedSize === size
                    ? 'border-blue-600 bg-blue-50 text-blue-600'
                    : 'border-gray-300 hover:border-gray-400'
                ]"
              >
                {{ size }}
              </button>
            </div>
          </div>
          
          <!-- Quantity -->
          <div class="mb-6">
            <h3 class="text-sm font-semibold mb-3">Quantity</h3>
            <div class="flex items-center gap-3">
              <button
                @click="quantity = Math.max(1, quantity - 1)"
                class="w-10 h-10 border-2 border-gray-300 rounded-lg hover:border-gray-400"
              >
                -
              </button>
              <span class="text-xl font-semibold w-12 text-center">{{ quantity }}</span>
              <button
                @click="quantity++"
                class="w-10 h-10 border-2 border-gray-300 rounded-lg hover:border-gray-400"
              >
                +
              </button>
            </div>
          </div>
          
          <!-- Actions -->
          <div class="flex gap-4">
            <button
              @click="handleAddToCart"
              class="flex-1 bg-blue-600 text-white py-4 rounded-lg font-semibold hover:bg-blue-700 transition-colors"
            >
              Add to Cart
            </button>
            <button class="px-6 py-4 border-2 border-gray-300 rounded-lg hover:border-gray-400 transition-colors">
              ♡
            </button>
          </div>
        </div>
      </div>
    </Container>
  </div>
</template>
```

---

## SaaS Dashboard

Full-featured dashboard with metrics, charts, and data tables.

### Dashboard.vue

```vue
<script setup lang="ts">
import { ref, computed } from 'vue'

interface Metric {
  label: string
  value: string
  change: number
  icon: string
}

const metrics = ref<Metric[]>([
  { label: 'Total Revenue', value: '$45,231', change: 12.5, icon: '💰' },
  { label: 'Active Users', value: '2,345', change: 8.2, icon: '👥' },
  { label: 'Conversion Rate', value: '3.24%', change: -2.4, icon: '📊' },
  { label: 'Avg. Order Value', value: '$124', change: 5.1, icon: '🛒' }
])

interface Activity {
  id: string
  user: string
  action: string
  time: string
  avatar: string
}

const recentActivity = ref<Activity[]>([
  { id: '1', user: 'John Doe', action: 'Made a purchase of $299', time: '2 minutes ago', avatar: '👨' },
  { id: '2', user: 'Jane Smith', action: 'Signed up for premium plan', time: '15 minutes ago', avatar: '👩' },
  { id: '3', user: 'Mike Johnson', action: 'Updated profile information', time: '1 hour ago', avatar: '👨‍💼' },
  { id: '4', user: 'Sarah Williams', action: 'Left a 5-star review', time: '2 hours ago', avatar: '👩‍💼' }
])

const chartData = ref([
  { month: 'Jan', value: 4000 },
  { month: 'Feb', value: 3000 },
  { month: 'Mar', value: 5000 },
  { month: 'Apr', value: 4500 },
  { month: 'May', value: 6000 },
  { month: 'Jun', value: 5500 }
])

const maxValue = computed(() => Math.max(...chartData.value.map(d => d.value)))
</script>

<template>
  <div class="min-h-screen bg-gray-50">
    <!-- Header -->
    <header class="bg-white border-b border-gray-200 sticky top-0 z-10">
      <Container>
        <div class="flex items-center justify-between h-16">
          <h1 class="text-2xl font-bold text-gray-900">Dashboard</h1>
          
          <div class="flex items-center gap-4">
            <button class="relative p-2 text-gray-600 hover:text-gray-900">
              🔔
              <span class="absolute top-1 right-1 w-2 h-2 bg-red-500 rounded-full"></span>
            </button>
            
            <div class="flex items-center gap-3">
              <div class="text-right">
                <p class="text-sm font-medium text-gray-900">Admin User</p>
                <p class="text-xs text-gray-500">admin@example.com</p>
              </div>
              <div class="w-10 h-10 bg-blue-600 rounded-full flex items-center justify-center text-white">
                👤
              </div>
            </div>
          </div>
        </div>
      </Container>
    </header>
    
    <!-- Main Content -->
    <Container class="py-8">
      <!-- Metrics Grid -->
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <div
          v-for="metric in metrics"
          :key="metric.label"
          class="bg-white rounded-lg shadow-sm p-6 hover:shadow-md transition-shadow"
        >
          <div class="flex items-center justify-between mb-4">
            <span class="text-3xl">{{ metric.icon }}</span>
            <span
              :class="[
                'text-sm font-semibold px-2 py-1 rounded',
                metric.change > 0
                  ? 'bg-green-100 text-green-700'
                  : 'bg-red-100 text-red-700'
              ]"
            >
              {{ metric.change > 0 ? '+' : '' }}{{ metric.change }}%
            </span>
          </div>
          
          <p class="text-sm text-gray-600 mb-1">{{ metric.label }}</p>
          <p class="text-3xl font-bold text-gray-900">{{ metric.value }}</p>
        </div>
      </div>
      
      <!-- Charts and Activity -->
      <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
        <!-- Revenue Chart -->
        <div class="lg:col-span-2 bg-white rounded-lg shadow-sm p-6">
          <h2 class="text-lg font-semibold text-gray-900 mb-6">Revenue Overview</h2>
          
          <div class="flex items-end justify-between h-64 gap-4">
            <div
              v-for="data in chartData"
              :key="data.month"
              class="flex-1 flex flex-col items-center gap-2"
            >
              <div class="w-full bg-gray-200 rounded-t-lg relative" style="height: 100%">
                <div
                  class="absolute bottom-0 w-full bg-blue-600 rounded-t-lg transition-all duration-500"
                  :style="{ height: `${(data.value / maxValue) * 100}%` }"
                />
              </div>
              <span class="text-sm text-gray-600">{{ data.month }}</span>
            </div>
          </div>
        </div>
        
        <!-- Recent Activity -->
        <div class="bg-white rounded-lg shadow-sm p-6">
          <h2 class="text-lg font-semibold text-gray-900 mb-6">Recent Activity</h2>
          
          <div class="space-y-4">
            <div
              v-for="activity in recentActivity"
              :key="activity.id"
              class="flex gap-3"
            >
              <div class="w-10 h-10 bg-gray-100 rounded-full flex items-center justify-center flex-shrink-0">
                {{ activity.avatar }}
              </div>
              
              <div class="flex-1 min-w-0">
                <p class="text-sm font-medium text-gray-900">{{ activity.user }}</p>
                <p class="text-sm text-gray-600 truncate">{{ activity.action }}</p>
                <p class="text-xs text-gray-500 mt-1">{{ activity.time }}</p>
              </div>
            </div>
          </div>
        </div>
      </div>
      
      <!-- Data Table -->
      <div class="mt-8 bg-white rounded-lg shadow-sm overflow-hidden">
        <div class="p-6 border-b border-gray-200">
          <h2 class="text-lg font-semibold text-gray-900">Recent Orders</h2>
        </div>
        
        <div class="overflow-x-auto">
          <table class="w-full">
            <thead class="bg-gray-50">
              <tr>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Order ID</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Customer</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Status</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Amount</th>
                <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Date</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-gray-200">
              <tr v-for="i in 5" :key="i" class="hover:bg-gray-50">
                <td class="px-6 py-4 text-sm font-medium text-gray-900">#{{ 1000 + i }}</td>
                <td class="px-6 py-4 text-sm text-gray-600">Customer {{ i }}</td>
                <td class="px-6 py-4">
                  <span class="px-2 py-1 text-xs font-semibold rounded-full bg-green-100 text-green-700">
                    Completed
                  </span>
                </td>
                <td class="px-6 py-4 text-sm text-gray-900">${{ (Math.random() * 500 + 50).toFixed(2) }}</td>
                <td class="px-6 py-4 text-sm text-gray-600">{{ new Date().toLocaleDateString() }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </Container>
  </div>
</template>
```

---

## Portfolio Landing Page

Modern portfolio landing page with hero, projects, and contact sections.

### Portfolio.vue

```vue
<script setup lang="ts">
import { ref } from 'vue'

interface Project {
  id: string
  title: string
  description: string
  image: string
  tags: string[]
  link: string
}

const projects = ref<Project[]>([
  {
    id: '1',
    title: 'E-Commerce Platform',
    description: 'Full-stack e-commerce solution with payment integration',
    image: '/project-1.jpg',
    tags: ['Vue.js', 'Node.js', 'MongoDB'],
    link: '#'
  },
  {
    id: '2',
    title: 'Task Management App',
    description: 'Collaborative task management with real-time updates',
    image: '/project-2.jpg',
    tags: ['React', 'Firebase', 'TailwindCSS'],
    link: '#'
  },
  {
    id: '3',
    title: 'Analytics Dashboard',
    description: 'Data visualization dashboard with advanced analytics',
    image: '/project-3.jpg',
    tags: ['Vue.js', 'D3.js', 'TypeScript'],
    link: '#'
  }
])

const skills = ref([
  'Vue.js', 'React', 'TypeScript', 'Node.js', 'TailwindCSS',
  'MongoDB', 'PostgreSQL', 'AWS', 'Docker', 'Git'
])
</script>

<template>
  <div class="min-h-screen">
    <!-- Header -->
    <header class="fixed top-0 w-full bg-white/80 backdrop-blur-md z-50 border-b border-gray-200">
      <Container>
        <nav class="flex items-center justify-between h-16">
          <a href="#" class="text-xl font-bold text-gray-900">Portfolio</a>
          
          <div class="hidden md:flex items-center gap-8">
            <a href="#about" class="text-gray-600 hover:text-gray-900 transition-colors">About</a>
            <a href="#projects" class="text-gray-600 hover:text-gray-900 transition-colors">Projects</a>
            <a href="#skills" class="text-gray-600 hover:text-gray-900 transition-colors">Skills</a>
            <a href="#contact" class="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
              Contact
            </a>
          </div>
        </nav>
      </Container>
    </header>
    
    <!-- Hero Section -->
    <section class="pt-32 pb-20 bg-gradient-to-br from-blue-50 to-purple-50">
      <Container>
        <div class="text-center max-w-3xl mx-auto">
          <h1 class="text-5xl md:text-6xl font-bold text-gray-900 mb-6">
            Hi, I'm <span class="text-blue-600">John Doe</span>
          </h1>
          <p class="text-xl md:text-2xl text-gray-600 mb-8">
            Full-Stack Developer specializing in modern web applications
          </p>
          <div class="flex justify-center gap-4">
            <a
              href="#projects"
              class="px-8 py-3 bg-blue-600 text-white rounded-lg font-semibold hover:bg-blue-700 transition-colors"
            >
              View My Work
            </a>
            <a
              href="#contact"
              class="px-8 py-3 border-2 border-gray-900 text-gray-900 rounded-lg font-semibold hover:bg-gray-900 hover:text-white transition-colors"
            >
              Get in Touch
            </a>
          </div>
        </div>
      </Container>
    </section>
    
    <!-- Projects Section -->
    <section id="projects" class="py-20">
      <Container>
        <h2 class="text-4xl font-bold text-center text-gray-900 mb-12">Featured Projects</h2>
        
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
          <div
            v-for="project in projects"
            :key="project.id"
            class="bg-white rounded-lg shadow-lg overflow-hidden hover:shadow-xl transition-shadow"
          >
            <div class="h-48 bg-gray-200">
              <img :src="project.image" :alt="project.title" class="w-full h-full object-cover" />
            </div>
            
            <div class="p-6">
              <h3 class="text-xl font-semibold text-gray-900 mb-2">{{ project.title }}</h3>
              <p class="text-gray-600 mb-4">{{ project.description }}</p>
              
              <div class="flex flex-wrap gap-2 mb-4">
                <span
                  v-for="tag in project.tags"
                  :key="tag"
                  class="px-3 py-1 bg-blue-100 text-blue-700 rounded-full text-sm"
                >
                  {{ tag }}
                </span>
              </div>
              
              <a
                :href="project.link"
                class="inline-flex items-center text-blue-600 hover:text-blue-700 font-semibold"
              >
                View Project →
              </a>
            </div>
          </div>
        </div>
      </Container>
    </section>
    
    <!-- Skills Section -->
    <section id="skills" class="py-20 bg-gray-50">
      <Container>
        <h2 class="text-4xl font-bold text-center text-gray-900 mb-12">Skills & Technologies</h2>
        
        <div class="flex flex-wrap justify-center gap-4">
          <span
            v-for="skill in skills"
            :key="skill"
            class="px-6 py-3 bg-white text-gray-900 rounded-lg shadow-md font-medium hover:shadow-lg transition-shadow"
          >
            {{ skill }}
          </span>
        </div>
      </Container>
    </section>
    
    <!-- Contact Section -->
    <section id="contact" class="py-20">
      <Container>
        <div class="max-w-2xl mx-auto text-center">
          <h2 class="text-4xl font-bold text-gray-900 mb-6">Let's Work Together</h2>
          <p class="text-xl text-gray-600 mb-8">
            Have a project in mind? Get in touch and let's create something amazing.
          </p>
          
          <form class="space-y-4">
            <input
              type="text"
              placeholder="Your Name"
              class="w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
            />
            <input
              type="email"
              placeholder="Your Email"
              class="w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
            />
            <textarea
              placeholder="Your Message"
              rows="5"
              class="w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
            />
            <button
              type="submit"
              class="w-full px-8 py-3 bg-blue-600 text-white rounded-lg font-semibold hover:bg-blue-700 transition-colors"
            >
              Send Message
            </button>
          </form>
        </div>
      </Container>
    </section>
    
    <!-- Footer -->
    <footer class="bg-gray-900 text-white py-8">
      <Container>
        <div class="text-center">
          <p>&copy; 2026 John Doe. All rights reserved.</p>
        </div>
      </Container>
    </footer>
  </div>
</template>
```

These examples demonstrate production-ready mockups with real-world patterns and best practices. Each can be customized and extended based on specific project requirements.
