# Performance Checklist

Comprehensive guidelines for frontend performance optimization and Web Vitals.

## Table of Contents
- [Core Web Vitals](#core-web-vitals)
- [Rendering Performance](#rendering-performance)
- [Resource Loading](#resource-loading)
- [Bundle Optimization](#bundle-optimization)
- [Memory Management](#memory-management)
- [Network Optimization](#network-optimization)
- [Performance Monitoring](#performance-monitoring)

## Core Web Vitals

### Largest Contentful Paint (LCP)

**Target:** < 2.5 seconds

**Common Issues and Fixes:**

#### Issue 1: Large Images

**Problem:**
```jsx
// ❌ Large unoptimized image
<img src="/hero-image.jpg" alt="Hero" />
```

**Solution:**
```jsx
// ✅ Optimized with modern formats, lazy loading, responsive images
<picture>
  <source 
    srcSet="/hero-image.avif 1x, /hero-image-2x.avif 2x" 
    type="image/avif"
  />
  <source 
    srcSet="/hero-image.webp 1x, /hero-image-2x.webp 2x" 
    type="image/webp"
  />
  <img
    src="/hero-image.jpg"
    srcSet="/hero-image.jpg 1x, /hero-image-2x.jpg 2x"
    alt="Hero"
    loading="eager"
    fetchpriority="high"
    width={1200}
    height={600}
  />
</picture>
```

#### Issue 2: Blocking Resources

**Problem:**
```html
<!-- ❌ Blocking CSS and scripts -->
<head>
  <link rel="stylesheet" href="/styles.css">
  <script src="/analytics.js"></script>
</head>
```

**Solution:**
```html
<!-- ✅ Non-blocking resources -->
<head>
  <!-- Critical CSS inline -->
  <style>
    /* Critical above-the-fold styles */
  </style>
  
  <!-- Non-critical CSS with media query -->
  <link rel="stylesheet" href="/styles.css" media="print" onload="this.media='all'">
  
  <!-- Defer non-critical scripts -->
  <script defer src="/analytics.js"></script>
</head>
```

#### Issue 3: Slow Server Response

**Problem:**
```typescript
// ❌ No caching, slow data fetching
export async function getServerSideProps() {
  const data = await fetch('https://api.example.com/data');
  return { props: { data } };
}
```

**Solution:**
```typescript
// ✅ Static generation with revalidation
export async function getStaticProps() {
  const data = await fetch('https://api.example.com/data');
  
  return {
    props: { data },
    revalidate: 60 // ISR - revalidate every 60 seconds
  };
}

// Or use CDN caching
export async function GET() {
  const data = await fetchData();
  
  return Response.json(data, {
    headers: {
      'Cache-Control': 'public, s-maxage=60, stale-while-revalidate=120'
    }
  });
}
```

### First Input Delay (FID) / Interaction to Next Paint (INP)

**Target:** FID < 100ms, INP < 200ms

#### Issue 1: Long Tasks Blocking Main Thread

**Problem:**
```javascript
// ❌ Synchronous processing blocking UI
function processLargeDataset(data) {
  // Processing 10,000 items synchronously
  const results = data.map(item => {
    // Heavy computation
    return heavyTransformation(item);
  });
  
  updateUI(results);
}
```

**Solution:**
```javascript
// ✅ Break into chunks using scheduler
function processLargeDataset(data) {
  const chunkSize = 100;
  const results = [];
  let index = 0;
  
  function processChunk() {
    const chunk = data.slice(index, index + chunkSize);
    
    for (const item of chunk) {
      results.push(heavyTransformation(item));
    }
    
    index += chunkSize;
    
    if (index < data.length) {
      // Use scheduler API or setTimeout
      if ('scheduler' in window) {
        scheduler.postTask(processChunk, { priority: 'background' });
      } else {
        setTimeout(processChunk, 0);
      }
    } else {
      updateUI(results);
    }
  }
  
  processChunk();
}

// ✅ Or use Web Worker for heavy computation
// worker.js
self.onmessage = (e) => {
  const results = e.data.map(item => heavyTransformation(item));
  self.postMessage(results);
};

// main.js
const worker = new Worker('worker.js');
worker.postMessage(data);
worker.onmessage = (e) => updateUI(e.data);
```

#### Issue 2: Expensive Event Handlers

**Problem:**
```javascript
// ❌ No debouncing on expensive operations
input.addEventListener('input', (e) => {
  // Expensive operation on every keystroke
  searchDatabase(e.target.value);
  updateSuggestions(e.target.value);
  logAnalytics(e.target.value);
});
```

**Solution:**
```javascript
// ✅ Debounce expensive operations
function debounce(fn, delay) {
  let timeoutId;
  return (...args) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => fn(...args), delay);
  };
}

input.addEventListener('input', debounce((e) => {
  searchDatabase(e.target.value);
  updateSuggestions(e.target.value);
  logAnalytics(e.target.value);
}, 300));

// ✅ React version with useTransition
function SearchInput() {
  const [query, setQuery] = useState('');
  const [isPending, startTransition] = useTransition();
  
  const handleChange = (e) => {
    const value = e.target.value;
    setQuery(value); // Update immediately
    
    // Mark expensive updates as transitions
    startTransition(() => {
      searchDatabase(value);
      updateSuggestions(value);
    });
  };
  
  return (
    <input 
      value={query}
      onChange={handleChange}
      aria-busy={isPending}
    />
  );
}
```

### Cumulative Layout Shift (CLS)

**Target:** < 0.1

#### Issue 1: Images Without Dimensions

**Problem:**
```jsx
// ❌ No dimensions specified
<img src="/image.jpg" alt="Content" />
```

**Solution:**
```jsx
// ✅ Always specify dimensions
<img 
  src="/image.jpg" 
  alt="Content"
  width={800}
  height={600}
  style={{ aspectRatio: '4/3' }}
/>

// ✅ Or use aspect ratio box
<div style={{ aspectRatio: '16/9', position: 'relative' }}>
  <img 
    src="/image.jpg"
    alt="Content"
    style={{ position: 'absolute', width: '100%', height: '100%', objectFit: 'cover' }}
  />
</div>
```

#### Issue 2: Dynamic Content Insertion

**Problem:**
```jsx
// ❌ Inserting content without reserved space
function Article() {
  const [ad, setAd] = useState(null);
  
  useEffect(() => {
    loadAd().then(setAd);
  }, []);
  
  return (
    <article>
      <h1>Article Title</h1>
      {ad && <AdBanner ad={ad} />} {/* Causes layout shift! */}
      <p>Article content...</p>
    </article>
  );
}
```

**Solution:**
```jsx
// ✅ Reserve space for dynamic content
function Article() {
  const [ad, setAd] = useState(null);
  
  useEffect(() => {
    loadAd().then(setAd);
  }, []);
  
  return (
    <article>
      <h1>Article Title</h1>
      {/* Reserve space with min-height */}
      <div style={{ minHeight: '250px' }}>
        {ad ? <AdBanner ad={ad} /> : <AdPlaceholder />}
      </div>
      <p>Article content...</p>
    </article>
  );
}
```

#### Issue 3: Web Fonts Causing FOIT/FOUT

**Problem:**
```css
/* ❌ No font loading strategy */
@font-face {
  font-family: 'CustomFont';
  src: url('/fonts/custom-font.woff2');
}

body {
  font-family: 'CustomFont', sans-serif;
}
```

**Solution:**
```css
/* ✅ Use font-display for better loading */
@font-face {
  font-family: 'CustomFont';
  src: url('/fonts/custom-font.woff2') format('woff2');
  font-display: swap; /* or optional */
}

/* ✅ Preload critical fonts */
```

```html
<link 
  rel="preload" 
  href="/fonts/custom-font.woff2" 
  as="font" 
  type="font/woff2"
  crossorigin
>
```

## Rendering Performance

### React Rendering Optimization

#### Issue 1: Unnecessary Re-renders

**Problem:**
```jsx
// ❌ Child re-renders on every parent update
function Parent() {
  const [count, setCount] = useState(0);
  
  return (
    <>
      <button onClick={() => setCount(c => c + 1)}>Count: {count}</button>
      <ExpensiveChild /> {/* Re-renders unnecessarily */}
    </>
  );
}
```

**Solution:**
```jsx
// ✅ Memoize to prevent unnecessary re-renders
const ExpensiveChild = React.memo(function ExpensiveChild({ data }) {
  // Expensive rendering logic
  return <div>{/* ... */}</div>;
});

function Parent() {
  const [count, setCount] = useState(0);
  const data = useMemo(() => computeData(), []);
  
  return (
    <>
      <button onClick={() => setCount(c => c + 1)}>Count: {count}</button>
      <ExpensiveChild data={data} />
    </>
  );
}
```

#### Issue 2: Large Lists Without Virtualization

**Problem:**
```jsx
// ❌ Rendering 10,000 items
function LargeList({ items }) {
  return (
    <ul>
      {items.map(item => (
        <li key={item.id}>{item.name}</li>
      ))}
    </ul>
  );
}
```

**Solution:**
```jsx
// ✅ Virtual scrolling with react-window
import { FixedSizeList } from 'react-window';

function LargeList({ items }) {
  const Row = ({ index, style }) => (
    <div style={style}>
      {items[index].name}
    </div>
  );
  
  return (
    <FixedSizeList
      height={600}
      itemCount={items.length}
      itemSize={35}
      width="100%"
    >
      {Row}
    </FixedSizeList>
  );
}
```

### CSS Performance

#### Issue 1: Expensive CSS Selectors

**Problem:**
```css
/* ❌ Universal selectors, deep descendant selectors */
* {
  box-sizing: border-box;
}

.container div div div p {
  color: blue;
}

[data-attr*="value"] {
  /* Attribute selectors with wildcards */
}
```

**Solution:**
```css
/* ✅ Specific, shallow selectors */
html {
  box-sizing: border-box;
}

*, *::before, *::after {
  box-sizing: inherit;
}

.paragraph-text {
  color: blue;
}

/* Use classes instead of complex selectors */
.specific-element {
  /* ... */
}
```

#### Issue 2: Layout Thrashing

**Problem:**
```javascript
// ❌ Interleaving reads and writes
elements.forEach(el => {
  const height = el.offsetHeight; // Read (forces layout)
  el.style.height = height + 10 + 'px'; // Write
  const width = el.offsetWidth; // Read (forces layout again)
  el.style.width = width + 10 + 'px'; // Write
});
```

**Solution:**
```javascript
// ✅ Batch reads, then batch writes
const measurements = elements.map(el => ({
  height: el.offsetHeight,
  width: el.offsetWidth
}));

elements.forEach((el, i) => {
  el.style.height = measurements[i].height + 10 + 'px';
  el.style.width = measurements[i].width + 10 + 'px';
});
```

## Resource Loading

### Code Splitting

**Problem:**
```jsx
// ❌ Single large bundle
import Dashboard from './Dashboard';
import Analytics from './Analytics';
import Settings from './Settings';

function App() {
  return (
    <Routes>
      <Route path="/dashboard" element={<Dashboard />} />
      <Route path="/analytics" element={<Analytics />} />
      <Route path="/settings" element={<Settings />} />
    </Routes>
  );
}
```

**Solution:**
```jsx
// ✅ Route-based code splitting
const Dashboard = lazy(() => import('./Dashboard'));
const Analytics = lazy(() => import('./Analytics'));
const Settings = lazy(() => import('./Settings'));

function App() {
  return (
    <Suspense fallback={<LoadingSpinner />}>
      <Routes>
        <Route path="/dashboard" element={<Dashboard />} />
        <Route path="/analytics" element={<Analytics />} />
        <Route path="/settings" element={<Settings />} />
      </Routes>
    </Suspense>
  );
}

// ✅ Component-based code splitting
function UserProfile({ userId }) {
  const [showSettings, setShowSettings] = useState(false);
  
  const UserSettings = lazy(() => import('./UserSettings'));
  
  return (
    <div>
      <UserInfo userId={userId} />
      <button onClick={() => setShowSettings(true)}>Settings</button>
      
      {showSettings && (
        <Suspense fallback={<Spinner />}>
          <UserSettings userId={userId} />
        </Suspense>
      )}
    </div>
  );
}
```

### Preloading and Prefetching

```jsx
// ✅ Strategic resource hints
function App() {
  useEffect(() => {
    // Preload critical resources
    const link = document.createElement('link');
    link.rel = 'preload';
    link.as = 'script';
    link.href = '/critical-script.js';
    document.head.appendChild(link);
    
    // Prefetch likely next navigation
    const prefetchLink = document.createElement('link');
    prefetchLink.rel = 'prefetch';
    prefetchLink.href = '/next-page-bundle.js';
    document.head.appendChild(prefetchLink);
  }, []);
  
  return (/* ... */);
}

// ✅ Next.js Link with prefetch
<Link href="/dashboard" prefetch={true}>
  Dashboard
</Link>
```

## Bundle Optimization

### Tree Shaking

**Problem:**
```javascript
// ❌ Importing entire library
import _ from 'lodash';
import * as dateFns from 'date-fns';

const result = _.debounce(fn, 300);
const formatted = dateFns.format(date, 'yyyy-MM-dd');
```

**Solution:**
```javascript
// ✅ Import only what you need
import debounce from 'lodash/debounce';
import { format } from 'date-fns';

const result = debounce(fn, 300);
const formatted = format(date, 'yyyy-MM-dd');
```

### Dependency Analysis

**Check bundle size regularly:**

```bash
# Analyze bundle
npm run build -- --analyze

# Check individual package sizes
npx bundlephobia lodash
npx bundlephobia date-fns

# Find duplicate dependencies
npx duplicate-package-checker-webpack-plugin
```

**Common heavy dependencies to watch:**
- moment.js (replace with date-fns or dayjs)
- lodash (import individual functions)
- material-ui (use individual imports)

## Memory Management

### Memory Leaks

**Common Causes:**

#### 1. Event Listeners Not Removed

**Problem:**
```javascript
// ❌ Event listener not cleaned up
useEffect(() => {
  window.addEventListener('resize', handleResize);
  // Missing cleanup!
}, []);
```

**Solution:**
```javascript
// ✅ Proper cleanup
useEffect(() => {
  window.addEventListener('resize', handleResize);
  return () => window.removeEventListener('resize', handleResize);
}, []);
```

#### 2. Timers Not Cleared

**Problem:**
```javascript
// ❌ Timer continues after unmount
useEffect(() => {
  setInterval(() => {
    updateData();
  }, 1000);
}, []);
```

**Solution:**
```javascript
// ✅ Clear timer on cleanup
useEffect(() => {
  const intervalId = setInterval(() => {
    updateData();
  }, 1000);
  
  return () => clearInterval(intervalId);
}, []);
```

#### 3. Closures Holding References

**Problem:**
```javascript
// ❌ Closure holding reference to large object
function useDataCache() {
  const cache = useRef(new Map());
  
  const addToCache = (key, value) => {
    cache.current.set(key, value);
    // Cache never cleared, grows indefinitely
  };
  
  return { addToCache };
}
```

**Solution:**
```javascript
// ✅ Implement cache eviction
function useDataCache(maxSize = 100) {
  const cache = useRef(new Map());
  
  const addToCache = (key, value) => {
    if (cache.current.size >= maxSize) {
      // Remove oldest entry (FIFO)
      const firstKey = cache.current.keys().next().value;
      cache.current.delete(firstKey);
    }
    cache.current.set(key, value);
  };
  
  useEffect(() => {
    return () => cache.current.clear();
  }, []);
  
  return { addToCache };
}
```

## Network Optimization

### API Request Optimization

**Problem:**
```javascript
// ❌ Multiple sequential requests
async function loadUserData(userId) {
  const user = await fetch(`/api/users/${userId}`);
  const posts = await fetch(`/api/users/${userId}/posts`);
  const comments = await fetch(`/api/users/${userId}/comments`);
  
  return { user, posts, comments };
}
```

**Solution:**
```javascript
// ✅ Parallel requests
async function loadUserData(userId) {
  const [user, posts, comments] = await Promise.all([
    fetch(`/api/users/${userId}`),
    fetch(`/api/users/${userId}/posts`),
    fetch(`/api/users/${userId}/comments`)
  ]);
  
  return { user, posts, comments };
}

// ✅ Or use GraphQL to fetch in single request
const query = gql`
  query GetUserData($userId: ID!) {
    user(id: $userId) {
      id
      name
      posts {
        id
        title
      }
      comments {
        id
        text
      }
    }
  }
`;
```

### Request Deduplication

```typescript
// ✅ Deduplicate identical requests
const requestCache = new Map<string, Promise<any>>();

async function fetchWithDedup<T>(url: string): Promise<T> {
  if (requestCache.has(url)) {
    return requestCache.get(url)!;
  }
  
  const promise = fetch(url).then(r => r.json());
  requestCache.set(url, promise);
  
  // Clear after completion
  promise.finally(() => {
    setTimeout(() => requestCache.delete(url), 1000);
  });
  
  return promise;
}
```

## Performance Monitoring

### Performance Metrics Collection

```typescript
// ✅ Collect real user metrics
function reportWebVitals(metric: Metric) {
  // Send to analytics
  analytics.track('web-vital', {
    name: metric.name,
    value: metric.value,
    rating: metric.rating,
    delta: metric.delta,
    id: metric.id,
  });
}

// Next.js
export function reportWebVitals(metric: NextWebVitalsMetric) {
  console.log(metric);
}

// React with web-vitals library
import { onCLS, onFID, onLCP, onFCP, onTTFB } from 'web-vitals';

onCLS(reportWebVitals);
onFID(reportWebVitals);
onLCP(reportWebVitals);
onFCP(reportWebVitals);
onTTFB(reportWebVitals);
```

### Performance Budget

```javascript
// webpack.config.js
module.exports = {
  performance: {
    maxAssetSize: 244000, // 244 KB
    maxEntrypointSize: 244000,
    hints: 'error'
  }
};

// lighthouse-budget.json
{
  "resourceSizes": [
    {
      "resourceType": "script",
      "budget": 300
    },
    {
      "resourceType": "stylesheet",
      "budget": 50
    }
  ],
  "timings": [
    {
      "metric": "interactive",
      "budget": 3000
    },
    {
      "metric": "first-contentful-paint",
      "budget": 1500
    }
  ]
}
```

### Performance Testing

```typescript
// ✅ Automated performance testing
describe('Performance', () => {
  it('should load homepage within budget', async () => {
    const metrics = await measurePageLoad('/');
    
    expect(metrics.LCP).toBeLessThan(2500);
    expect(metrics.FID).toBeLessThan(100);
    expect(metrics.CLS).toBeLessThan(0.1);
    expect(metrics.FCP).toBeLessThan(1800);
    expect(metrics.TTFB).toBeLessThan(600);
  });
  
  it('should maintain 60fps during scroll', async () => {
    const fps = await measureScrollPerformance();
    expect(fps).toBeGreaterThanOrEqual(60);
  });
});
```
