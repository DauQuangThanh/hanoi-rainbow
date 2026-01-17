# Performance Optimization and Testing

This reference covers frontend performance optimization techniques, bundle optimization, testing strategies, and accessibility best practices.

## Performance Optimization

### Code Splitting and Lazy Loading

#### React Lazy Loading
```typescript
import React, { lazy, Suspense } from 'react';
import { Routes, Route } from 'react-router-dom';

// Lazy load route components
const Dashboard = lazy(() => import('./pages/Dashboard'));
const UserProfile = lazy(() => import('./pages/UserProfile'));
const Settings = lazy(() => import('./pages/Settings'));
const AdminPanel = lazy(() => import('./pages/AdminPanel'));

// Loading component
const PageLoader = () => (
  <div className="page-loader">
    <div className="spinner" />
    <p>Loading...</p>
  </div>
);

function App() {
  return (
    <Suspense fallback={<PageLoader />}>
      <Routes>
        <Route path="/" element={<Dashboard />} />
        <Route path="/user/:id" element={<UserProfile />} />
        <Route path="/settings" element={<Settings />} />
        <Route path="/admin" element={<AdminPanel />} />
      </Routes>
    </Suspense>
  );
}

// Preload on hover
function NavigationLink({ to, preload, children }: {
  to: string;
  preload: () => Promise<any>;
  children: React.ReactNode;
}) {
  const handleMouseEnter = () => {
    // Start preloading the component
    preload();
  };

  return (
    <Link to={to} onMouseEnter={handleMouseEnter}>
      {children}
    </Link>
  );
}

// Usage
<NavigationLink
  to="/admin"
  preload={() => import('./pages/AdminPanel')}
>
  Admin Panel
</NavigationLink>

// Named exports lazy loading
const { UserList, UserDetail } = lazy(() =>
  import('./components/Users').then(module => ({
    default: {
      UserList: module.UserList,
      UserDetail: module.UserDetail
    }
  }))
);
```

#### Vue Lazy Loading
```typescript
// Router lazy loading
import { createRouter, createWebHistory } from 'vue-router';

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: '/',
      component: () => import('./views/Dashboard.vue')
    },
    {
      path: '/user/:id',
      component: () => import('./views/UserProfile.vue')
    },
    {
      path: '/admin',
      component: () => import('./views/AdminPanel.vue'),
      // Webpack magic comments for chunk naming
      // component: () => import(/* webpackChunkName: "admin" */ './views/AdminPanel.vue')
    }
  ]
});

// Component lazy loading
<script setup lang="ts">
import { defineAsyncComponent } from 'vue';

const HeavyChart = defineAsyncComponent({
  loader: () => import('./components/HeavyChart.vue'),
  loadingComponent: LoadingSpinner,
  errorComponent: ErrorDisplay,
  delay: 200,
  timeout: 10000
});
</script>

<template>
  <Suspense>
    <template #default>
      <HeavyChart :data="chartData" />
    </template>
    <template #fallback>
      <LoadingSpinner />
    </template>
  </Suspense>
</template>

// Prefetch on route navigation
router.beforeEach((to, from, next) => {
  // Prefetch next likely routes
  if (to.path === '/dashboard') {
    import('./views/UserProfile.vue'); // Prefetch in background
  }
  next();
});
```

### Bundle Optimization

#### Webpack Configuration
```javascript
// webpack.config.js
const webpack = require('webpack');
const TerserPlugin = require('terser-webpack-plugin');
const CompressionPlugin = require('compression-webpack-plugin');
const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer');

module.exports = {
  mode: 'production',
  
  optimization: {
    minimize: true,
    minimizer: [
      new TerserPlugin({
        terserOptions: {
          compress: {
            drop_console: true, // Remove console.logs in production
            drop_debugger: true,
            pure_funcs: ['console.log', 'console.info'] // Remove specific console methods
          }
        }
      })
    ],
    
    // Split chunks
    splitChunks: {
      chunks: 'all',
      cacheGroups: {
        // Vendor chunk
        vendor: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          priority: 10
        },
        // Common chunks used by multiple routes
        common: {
          minChunks: 2,
          priority: 5,
          reuseExistingChunk: true
        },
        // Large libraries in separate chunks
        react: {
          test: /[\\/]node_modules[\\/](react|react-dom)[\\/]/,
          name: 'react',
          priority: 20
        },
        lodash: {
          test: /[\\/]node_modules[\\/]lodash[\\/]/,
          name: 'lodash',
          priority: 20
        }
      }
    },
    
    // Runtime chunk for webpack runtime
    runtimeChunk: 'single'
  },
  
  plugins: [
    // Gzip compression
    new CompressionPlugin({
      algorithm: 'gzip',
      test: /\.(js|css|html|svg)$/,
      threshold: 8192,
      minRatio: 0.8
    }),
    
    // Brotli compression (better than gzip)
    new CompressionPlugin({
      algorithm: 'brotliCompress',
      test: /\.(js|css|html|svg)$/,
      compressionOptions: { level: 11 },
      threshold: 8192,
      minRatio: 0.8,
      filename: '[path][base].br'
    }),
    
    // Bundle analyzer (only in development)
    process.env.ANALYZE && new BundleAnalyzerPlugin()
  ].filter(Boolean),
  
  // Performance hints
  performance: {
    hints: 'warning',
    maxEntrypointSize: 512000, // 500kb
    maxAssetSize: 512000
  }
};
```

#### Vite Configuration
```typescript
// vite.config.ts
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { visualizer } from 'rollup-plugin-visualizer';
import viteCompression from 'vite-plugin-compression';

export default defineConfig({
  plugins: [
    react(),
    
    // Gzip compression
    viteCompression({
      algorithm: 'gzip',
      ext: '.gz'
    }),
    
    // Brotli compression
    viteCompression({
      algorithm: 'brotliCompress',
      ext: '.br'
    }),
    
    // Bundle analyzer
    visualizer({
      open: true,
      gzipSize: true,
      brotliSize: true
    })
  ],
  
  build: {
    // Output directory
    outDir: 'dist',
    
    // Minification
    minify: 'terser',
    terserOptions: {
      compress: {
        drop_console: true,
        drop_debugger: true
      }
    },
    
    // Rollup options
    rollupOptions: {
      output: {
        // Manual chunks
        manualChunks: {
          'react-vendor': ['react', 'react-dom'],
          'router': ['react-router-dom'],
          'ui-library': ['@mui/material', '@emotion/react', '@emotion/styled'],
          'charts': ['recharts', 'd3']
        }
      }
    },
    
    // Chunk size warnings
    chunkSizeWarningLimit: 500, // 500kb
    
    // Source maps (disable in production)
    sourcemap: false
  },
  
  // Optimize dependencies
  optimizeDeps: {
    include: ['react', 'react-dom', 'react-router-dom'],
    exclude: ['some-large-lib']
  }
});
```

### Tree Shaking

```typescript
// Good - Named imports (tree-shakeable)
import { map, filter, reduce } from 'lodash-es';

// Bad - Default import (includes entire library)
import _ from 'lodash';

// Good - Specific imports
import Button from '@mui/material/Button';
import TextField from '@mui/material/TextField';

// Bad - Imports entire library
import * as MaterialUI from '@mui/material';

// Package.json - Mark as side-effect free
{
  "name": "my-library",
  "sideEffects": false,  // Or array of files with side effects
  "sideEffects": [
    "*.css",
    "*.scss",
    "./src/polyfills.ts"
  ]
}

// Conditional imports
if (process.env.NODE_ENV === 'development') {
  // This will be removed in production builds
  const DevTools = require('./DevTools');
  DevTools.install();
}

// Dynamic imports for conditional features
async function loadFeature() {
  if (user.hasPremium) {
    const { PremiumFeature } = await import('./PremiumFeature');
    return new PremiumFeature();
  }
  return null;
}
```

### Image Optimization

```typescript
// React - Lazy loading images
import { useState, useEffect, useRef } from 'react';

interface LazyImageProps {
  src: string;
  alt: string;
  placeholder?: string;
  className?: string;
}

function LazyImage({ src, alt, placeholder, className }: LazyImageProps) {
  const [imageSrc, setImageSrc] = useState(placeholder || '');
  const [loaded, setLoaded] = useState(false);
  const imgRef = useRef<HTMLImageElement>(null);

  useEffect(() => {
    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            setImageSrc(src);
            observer.disconnect();
          }
        });
      },
      { rootMargin: '50px' } // Start loading 50px before visible
    );

    if (imgRef.current) {
      observer.observe(imgRef.current);
    }

    return () => observer.disconnect();
  }, [src]);

  return (
    <img
      ref={imgRef}
      src={imageSrc}
      alt={alt}
      className={`${className} ${loaded ? 'loaded' : 'loading'}`}
      onLoad={() => setLoaded(true)}
      loading="lazy" // Native lazy loading
    />
  );
}

// Next.js Image component (optimized)
import Image from 'next/image';

function OptimizedImage() {
  return (
    <Image
      src="/hero.jpg"
      alt="Hero"
      width={1200}
      height={600}
      priority // Load immediately (above fold)
      placeholder="blur"
      blurDataURL="data:image/jpeg;base64,..."
    />
  );
}

// WebP with fallback
function ResponsiveImage() {
  return (
    <picture>
      <source srcSet="/image.webp" type="image/webp" />
      <source srcSet="/image.jpg" type="image/jpeg" />
      <img src="/image.jpg" alt="Fallback" />
    </picture>
  );
}

// Responsive images
<img
  srcSet="
    /image-320w.jpg 320w,
    /image-640w.jpg 640w,
    /image-1280w.jpg 1280w
  "
  sizes="(max-width: 320px) 280px, (max-width: 640px) 600px, 1200px"
  src="/image-640w.jpg"
  alt="Responsive"
/>
```

### Memoization and Optimization

```typescript
// React.memo with custom comparison
const ExpensiveList = React.memo<{ items: Item[] }>(
  ({ items }) => {
    console.log('Rendering list');
    return (
      <ul>
        {items.map(item => (
          <li key={item.id}>{item.name}</li>
        ))}
      </ul>
    );
  },
  (prevProps, nextProps) => {
    // Custom comparison - only re-render if IDs change
    if (prevProps.items.length !== nextProps.items.length) return false;
    return prevProps.items.every((item, i) => item.id === nextProps.items[i].id);
  }
);

// useMemo for expensive computations
function DataProcessor({ data }: { data: number[] }) {
  const processedData = useMemo(() => {
    console.log('Processing data...');
    // Expensive operation
    return data
      .map(x => x * 2)
      .filter(x => x > 10)
      .reduce((acc, x) => acc + x, 0);
  }, [data]); // Only recompute when data changes

  const stats = useMemo(() => ({
    sum: processedData,
    average: processedData / data.length,
    max: Math.max(...data)
  }), [data, processedData]);

  return <div>Result: {processedData}</div>;
}

// useCallback for event handlers
function ParentComponent() {
  const [count, setCount] = useState(0);
  const [items, setItems] = useState<string[]>([]);

  // Recreated on every render - bad
  const handleClick = () => {
    console.log('Clicked');
  };

  // Same reference - good
  const handleClickMemoized = useCallback(() => {
    console.log('Clicked');
  }, []); // No dependencies

  // With dependencies
  const handleAddItem = useCallback((item: string) => {
    setItems(prev => [...prev, item]);
  }, []); // setItems is stable

  const handleLogCount = useCallback(() => {
    console.log('Count:', count);
  }, [count]); // Recreated when count changes

  return (
    <div>
      <MemoizedChild onClick={handleClickMemoized} />
      <button onClick={() => setCount(count + 1)}>Increment</button>
    </div>
  );
}

const MemoizedChild = React.memo<{ onClick: () => void }>(({ onClick }) => {
  console.log('Child rendered');
  return <button onClick={onClick}>Click me</button>;
});

// Vue - computed caching
<script setup lang="ts">
import { ref, computed } from 'vue';

const items = ref<Item[]>([]);

// Cached - only recomputes when items change
const expensiveComputation = computed(() => {
  console.log('Computing...');
  return items.value
    .map(item => heavyProcess(item))
    .sort((a, b) => b.score - a.score);
});

// Computed with getter and setter
const sortedItems = computed({
  get: () => expensiveComputation.value.slice(0, 10),
  set: (newValue) => {
    items.value = newValue;
  }
});
</script>
```

## Testing

### React Component Testing

```typescript
import { render, screen, fireEvent, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { BrowserRouter } from 'react-router-dom';
import { UserProfile } from './UserProfile';

// Test utilities
function renderWithProviders(
  ui: React.ReactElement,
  {
    queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } }
    }),
    ...renderOptions
  } = {}
) {
  function Wrapper({ children }: { children: React.ReactNode }) {
    return (
      <QueryClientProvider client={queryClient}>
        <BrowserRouter>
          {children}
        </BrowserRouter>
      </QueryClientProvider>
    );
  }

  return render(ui, { wrapper: Wrapper, ...renderOptions });
}

describe('UserProfile', () => {
  const mockUser = {
    id: '1',
    name: 'John Doe',
    email: 'john@example.com',
    role: 'admin'
  };

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
  });

  it('renders user information correctly', () => {
    renderWithProviders(<UserProfile user={mockUser} />);

    expect(screen.getByText(mockUser.name)).toBeInTheDocument();
    expect(screen.getByText(mockUser.email)).toBeInTheDocument();
    expect(screen.getByRole('heading', { name: mockUser.name })).toBeInTheDocument();
  });

  it('handles user interactions', async () => {
    const handleEdit = jest.fn();
    const user = userEvent.setup();

    renderWithProviders(
      <UserProfile user={mockUser} onEdit={handleEdit} />
    );

    const editButton = screen.getByRole('button', { name: /edit/i });
    await user.click(editButton);

    expect(handleEdit).toHaveBeenCalledWith(mockUser);
    expect(handleEdit).toHaveBeenCalledTimes(1);
  });

  it('handles form submission', async () => {
    const handleSubmit = jest.fn();
    const user = userEvent.setup();

    renderWithProviders(<UserForm onSubmit={handleSubmit} />);

    // Fill form
    await user.type(screen.getByLabelText(/name/i), 'Jane Doe');
    await user.type(screen.getByLabelText(/email/i), 'jane@example.com');
    await user.selectOptions(screen.getByLabelText(/role/i), 'user');

    // Submit
    await user.click(screen.getByRole('button', { name: /submit/i }));

    await waitFor(() => {
      expect(handleSubmit).toHaveBeenCalledWith({
        name: 'Jane Doe',
        email: 'jane@example.com',
        role: 'user'
      });
    });
  });

  it('shows loading state', () => {
    renderWithProviders(<UserProfile loading />);
    expect(screen.getByTestId('loading-spinner')).toBeInTheDocument();
  });

  it('shows error state', () => {
    const error = new Error('Failed to load user');
    renderWithProviders(<UserProfile error={error} />);

    expect(screen.getByText(/failed to load user/i)).toBeInTheDocument();
  });

  it('fetches and displays user data', async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: async () => mockUser
      })
    ) as jest.Mock;

    renderWithProviders(<UserProfileContainer userId="1" />);

    // Initial loading state
    expect(screen.getByText(/loading/i)).toBeInTheDocument();

    // Wait for data to load
    await waitFor(() => {
      expect(screen.getByText(mockUser.name)).toBeInTheDocument();
    });

    expect(fetch).toHaveBeenCalledWith('/api/users/1');
  });

  it('handles API errors', async () => {
    global.fetch = jest.fn(() =>
      Promise.reject(new Error('Network error'))
    ) as jest.Mock;

    renderWithProviders(<UserProfileContainer userId="1" />);

    await waitFor(() => {
      expect(screen.getByText(/network error/i)).toBeInTheDocument();
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA labels', () => {
      renderWithProviders(<UserProfile user={mockUser} />);

      expect(screen.getByRole('region', { name: /user profile/i })).toBeInTheDocument();
      expect(screen.getByLabelText(/user email/i)).toBeInTheDocument();
    });

    it('supports keyboard navigation', async () => {
      const handleEdit = jest.fn();
      const user = userEvent.setup();

      renderWithProviders(<UserProfile user={mockUser} onEdit={handleEdit} />);

      const editButton = screen.getByRole('button', { name: /edit/i });
      editButton.focus();
      
      expect(editButton).toHaveFocus();
      
      await user.keyboard('{Enter}');
      expect(handleEdit).toHaveBeenCalled();
    });
  });
});

// Snapshot testing
describe('UserCard snapshots', () => {
  it('matches snapshot', () => {
    const { container } = render(<UserCard user={mockUser} />);
    expect(container).toMatchSnapshot();
  });
});
```

### Vue Component Testing

```typescript
import { mount, flushPromises } from '@vue/test-utils';
import { createPinia, setActivePinia } from 'pinia';
import UserProfile from './UserProfile.vue';

describe('UserProfile.vue', () => {
  const mockUser = {
    id: '1',
    name: 'John Doe',
    email: 'john@example.com',
    role: 'admin'
  };

  beforeEach(() => {
    setActivePinia(createPinia());
  });

  it('renders user information', () => {
    const wrapper = mount(UserProfile, {
      props: { user: mockUser }
    });

    expect(wrapper.text()).toContain(mockUser.name);
    expect(wrapper.text()).toContain(mockUser.email);
    expect(wrapper.find('h2').text()).toBe(mockUser.name);
  });

  it('emits update event on edit', async () => {
    const wrapper = mount(UserProfile, {
      props: { user: mockUser }
    });

    await wrapper.find('button.edit').trigger('click');

    expect(wrapper.emitted('edit')).toBeTruthy();
    expect(wrapper.emitted('edit')?.[0]).toEqual([mockUser]);
  });

  it('handles form input', async () => {
    const wrapper = mount(UserForm);

    await wrapper.find('input[name="name"]').setValue('Jane Doe');
    await wrapper.find('input[name="email"]').setValue('jane@example.com');
    await wrapper.find('select[name="role"]').setValue('user');

    await wrapper.find('form').trigger('submit.prevent');

    expect(wrapper.emitted('submit')?.[0]).toEqual([{
      name: 'Jane Doe',
      email: 'jane@example.com',
      role: 'user'
    }]);
  });

  it('shows loading state', () => {
    const wrapper = mount(UserProfile, {
      props: { loading: true }
    });

    expect(wrapper.find('.loading-spinner').exists()).toBe(true);
  });

  it('fetches user data', async () => {
    global.fetch = jest.fn(() =>
      Promise.resolve({
        ok: true,
        json: async () => mockUser
      })
    ) as jest.Mock;

    const wrapper = mount(UserProfileContainer, {
      props: { userId: '1' }
    });

    expect(wrapper.text()).toContain('Loading');

    await flushPromises();

    expect(wrapper.text()).toContain(mockUser.name);
    expect(fetch).toHaveBeenCalledWith('/api/users/1');
  });

  it('uses composable correctly', async () => {
    const wrapper = mount({
      template: '<div>{{ displayName }}</div>',
      setup() {
        const { user, displayName, fetchUser } = useUser();
        fetchUser('1');
        return { displayName };
      }
    });

    await flushPromises();

    expect(wrapper.text()).toContain('John Doe');
  });

  describe('Slots', () => {
    it('renders default slot', () => {
      const wrapper = mount(UserCard, {
        props: { user: mockUser },
        slots: {
          default: '<p>Custom content</p>'
        }
      });

      expect(wrapper.html()).toContain('<p>Custom content</p>');
    });

    it('renders named slots', () => {
      const wrapper = mount(UserCard, {
        props: { user: mockUser },
        slots: {
          header: '<h1>Custom Header</h1>',
          footer: '<button>Custom Button</button>'
        }
      });

      expect(wrapper.find('h1').text()).toBe('Custom Header');
      expect(wrapper.find('button').text()).toBe('Custom Button');
    });

    it('passes slot props', () => {
      const wrapper = mount(UserList, {
        props: { users: [mockUser] },
        slots: {
          item: `
            <template #item="{ user, index }">
              <div>{{ index }}: {{ user.name }}</div>
            </template>
          `
        }
      });

      expect(wrapper.text()).toContain('0: John Doe');
    });
  });
});
```

### E2E Testing with Playwright

```typescript
import { test, expect } from '@playwright/test';

test.describe('User Profile', () => {
  test.beforeEach(async ({ page }) => {
    // Login before each test
    await page.goto('/login');
    await page.fill('[name="email"]', 'test@example.com');
    await page.fill('[name="password"]', 'password123');
    await page.click('button[type="submit"]');
    await expect(page).toHaveURL('/dashboard');
  });

  test('should display user profile', async ({ page }) => {
    await page.goto('/profile');

    await expect(page.locator('h1')).toHaveText('John Doe');
    await expect(page.locator('[data-testid="email"]')).toHaveText('john@example.com');
  });

  test('should edit user profile', async ({ page }) => {
    await page.goto('/profile');

    await page.click('button:has-text("Edit")');
    await page.fill('[name="name"]', 'Jane Doe');
    await page.fill('[name="email"]', 'jane@example.com');
    await page.click('button:has-text("Save")');

    await expect(page.locator('.success-message')).toBeVisible();
    await expect(page.locator('h1')).toHaveText('Jane Doe');
  });

  test('should handle form validation', async ({ page }) => {
    await page.goto('/profile/edit');

    await page.fill('[name="email"]', 'invalid-email');
    await page.click('button:has-text("Save")');

    await expect(page.locator('.error-message')).toContainText('Invalid email');
  });

  test('should load data progressively', async ({ page }) => {
    await page.goto('/users');

    // Initial load
    await expect(page.locator('.user-card')).toHaveCount(20);

    // Scroll to bottom
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));

    // Wait for more items
    await expect(page.locator('.user-card')).toHaveCount(40);
  });

  test('should handle API errors', async ({ page }) => {
    // Intercept API call and return error
    await page.route('/api/users/*', route =>
      route.fulfill({
        status: 500,
        body: JSON.stringify({ error: 'Server error' })
      })
    );

    await page.goto('/users');

    await expect(page.locator('.error-message')).toContainText('Server error');
  });

  test('should work offline', async ({ page, context }) => {
    await page.goto('/dashboard');

    // Go offline
    await context.setOffline(true);

    // Should show cached data
    await expect(page.locator('.offline-indicator')).toBeVisible();
    await expect(page.locator('.dashboard-content')).toBeVisible();

    // Go back online
    await context.setOffline(false);
    await expect(page.locator('.offline-indicator')).not.toBeVisible();
  });
});

// Visual regression testing
test('visual regression', async ({ page }) => {
  await page.goto('/dashboard');
  await expect(page).toHaveScreenshot('dashboard.png');
});

// Performance testing
test('page performance', async ({ page }) => {
  await page.goto('/dashboard');

  const performanceMetrics = await page.evaluate(() => {
    const navigation = performance.getEntriesByType('navigation')[0] as PerformanceNavigationTiming;
    return {
      domContentLoaded: navigation.domContentLoadedEventEnd - navigation.domContentLoadedEventStart,
      loadComplete: navigation.loadEventEnd - navigation.loadEventStart,
      firstPaint: performance.getEntriesByType('paint')[0]?.startTime
    };
  });

  expect(performanceMetrics.domContentLoaded).toBeLessThan(1000); // < 1s
  expect(performanceMetrics.loadComplete).toBeLessThan(2000); // < 2s
});
```

## Accessibility

### ARIA and Semantic HTML

```typescript
// React accessible component
function AccessibleModal({ isOpen, onClose, title, children }: {
  isOpen: boolean;
  onClose: () => void;
  title: string;
  children: React.ReactNode;
}) {
  const modalRef = useRef<HTMLDivElement>(null);
  const previousFocus = useRef<HTMLElement | null>(null);

  useEffect(() => {
    if (isOpen) {
      previousFocus.current = document.activeElement as HTMLElement;
      modalRef.current?.focus();

      // Trap focus
      const handleTabKey = (e: KeyboardEvent) => {
        if (e.key === 'Tab' && modalRef.current) {
          const focusableElements = modalRef.current.querySelectorAll(
            'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
          );
          const first = focusableElements[0] as HTMLElement;
          const last = focusableElements[focusableElements.length - 1] as HTMLElement;

          if (e.shiftKey && document.activeElement === first) {
            e.preventDefault();
            last.focus();
          } else if (!e.shiftKey && document.activeElement === last) {
            e.preventDefault();
            first.focus();
          }
        }

        if (e.key === 'Escape') {
          onClose();
        }
      };

      document.addEventListener('keydown', handleTabKey);
      return () => {
        document.removeEventListener('keydown', handleTabKey);
        previousFocus.current?.focus();
      };
    }
  }, [isOpen, onClose]);

  if (!isOpen) return null;

  return (
    <div
      className="modal-overlay"
      role="dialog"
      aria-modal="true"
      aria-labelledby="modal-title"
      onClick={onClose}
    >
      <div
        ref={modalRef}
        className="modal-content"
        onClick={(e) => e.stopPropagation()}
        tabIndex={-1}
      >
        <header className="modal-header">
          <h2 id="modal-title">{title}</h2>
          <button
            onClick={onClose}
            aria-label="Close modal"
            className="close-button"
          >
            ×
          </button>
        </header>
        <div className="modal-body">
          {children}
        </div>
      </div>
    </div>
  );
}

// Accessible form
function AccessibleForm() {
  const [errors, setErrors] = useState<Record<string, string>>({});

  return (
    <form onSubmit={handleSubmit} noValidate>
      <div className="form-group">
        <label htmlFor="email">
          Email <span aria-label="required">*</span>
        </label>
        <input
          id="email"
          type="email"
          aria-required="true"
          aria-invalid={!!errors.email}
          aria-describedby={errors.email ? 'email-error' : undefined}
        />
        {errors.email && (
          <span id="email-error" role="alert" className="error">
            {errors.email}
          </span>
        )}
      </div>

      <button type="submit" aria-busy={isSubmitting}>
        {isSubmitting ? 'Submitting...' : 'Submit'}
      </button>
    </form>
  );
}

// Skip to content link
function Layout({ children }: { children: React.ReactNode }) {
  return (
    <>
      <a href="#main-content" className="skip-link">
        Skip to main content
      </a>
      <nav aria-label="Main navigation">
        {/* Navigation links */}
      </nav>
      <main id="main-content" tabIndex={-1}>
        {children}
      </main>
    </>
  );
}

// Accessible tabs
function AccessibleTabs() {
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div>
      <div role="tablist" aria-label="Content tabs">
        {tabs.map((tab, index) => (
          <button
            key={tab.id}
            role="tab"
            aria-selected={activeTab === index}
            aria-controls={`panel-${tab.id}`}
            id={`tab-${tab.id}`}
            tabIndex={activeTab === index ? 0 : -1}
            onClick={() => setActiveTab(index)}
          >
            {tab.label}
          </button>
        ))}
      </div>
      {tabs.map((tab, index) => (
        <div
          key={tab.id}
          role="tabpanel"
          id={`panel-${tab.id}`}
          aria-labelledby={`tab-${tab.id}`}
          hidden={activeTab !== index}
        >
          {tab.content}
        </div>
      ))}
    </div>
  );
}
```

### Screen Reader Testing

```bash
# Testing with screen readers
# - NVDA (Windows - free)
# - JAWS (Windows - commercial)
# - VoiceOver (macOS - built-in, Cmd+F5)
# - TalkBack (Android - built-in)
# - Orca (Linux - free)

# Automated accessibility testing
npm install --save-dev @axe-core/react
npm install --save-dev jest-axe
```

```typescript
// Axe testing
import { axe, toHaveNoViolations } from 'jest-axe';

expect.extend(toHaveNoViolations);

test('should have no accessibility violations', async () => {
  const { container } = render(<UserProfile user={mockUser} />);
  const results = await axe(container);
  expect(results).toHaveNoViolations();
});
```

This reference provides comprehensive performance optimization, testing strategies, and accessibility best practices for frontend development.
