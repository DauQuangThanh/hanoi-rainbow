# Responsive Design Patterns Reference

Comprehensive guide to responsive web design patterns and techniques.

## Responsive Breakpoints

### Standard Breakpoints

```css
/* Mobile First Approach */
/* Mobile (default): 0-767px */
.container { width: 100%; }

/* Tablet: 768px and up */
@media (min-width: 768px) {
  .container { width: 750px; }
}

/* Desktop: 1024px and up */
@media (min-width: 1024px) {
  .container { width: 970px; }
}

/* Large Desktop: 1440px and up */
@media (min-width: 1440px) {
  .container { width: 1200px; }
}
```

### Common Breakpoint Sets

**Bootstrap Approach**
```css
/* Extra small: <576px */
/* Small: ≥576px */
@media (min-width: 576px) { }

/* Medium: ≥768px */
@media (min-width: 768px) { }

/* Large: ≥992px */
@media (min-width: 992px) { }

/* Extra large: ≥1200px */
@media (min-width: 1200px) { }

/* XXL: ≥1400px */
@media (min-width: 1400px) { }
```

**Tailwind CSS Approach**
```css
/* sm: ≥640px */
/* md: ≥768px */
/* lg: ≥1024px */
/* xl: ≥1280px */
/* 2xl: ≥1536px */
```

### Device-Based Breakpoints

```css
/* Phones (portrait) */
@media (max-width: 767px) { }

/* Tablets (portrait) */
@media (min-width: 768px) and (max-width: 1023px) { }

/* Tablets (landscape) */
@media (min-width: 1024px) and (max-width: 1365px) { }

/* Laptops */
@media (min-width: 1366px) and (max-width: 1919px) { }

/* Desktop */
@media (min-width: 1920px) { }
```

---

## Layout Patterns

### 1. Fluid Grid

**Description**: Columns expand/contract proportionally

```css
.grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}
```

**Use Cases**
- Product grids
- Image galleries
- Card layouts

---

### 2. Column Drop

**Description**: Columns stack vertically on smaller screens

```css
.column-drop {
  display: grid;
  gap: 1rem;
}

/* Mobile: Single column */
.column-drop { grid-template-columns: 1fr; }

/* Tablet: Two columns */
@media (min-width: 768px) {
  .column-drop { grid-template-columns: 1fr 1fr; }
}

/* Desktop: Three columns */
@media (min-width: 1024px) {
  .column-drop { grid-template-columns: 1fr 1fr 1fr; }
}
```

**Use Cases**
- Blog layouts
- Feature sections
- Team member grids

---

### 3. Layout Shifter

**Description**: Complete layout reorganization at breakpoints

```css
/* Mobile: Stacked */
.layout-shifter {
  display: grid;
  grid-template-areas:
    "header"
    "main"
    "sidebar"
    "footer";
}

/* Desktop: Sidebar layout */
@media (min-width: 1024px) {
  .layout-shifter {
    grid-template-areas:
      "header header"
      "sidebar main"
      "sidebar footer";
    grid-template-columns: 250px 1fr;
  }
}
```

**Use Cases**
- Dashboard layouts
- Application interfaces
- Admin panels

---

### 4. Tiny Tweaks

**Description**: Minor adjustments (padding, font size)

```css
.content {
  padding: 1rem;
  font-size: 16px;
}

@media (min-width: 768px) {
  .content {
    padding: 2rem;
    font-size: 18px;
  }
}

@media (min-width: 1024px) {
  .content {
    padding: 3rem;
    font-size: 20px;
  }
}
```

**Use Cases**
- Single-page sites
- Landing pages
- Simple layouts

---

### 5. Off Canvas

**Description**: Navigation hidden off-screen on mobile

```css
.nav {
  position: fixed;
  top: 0;
  left: -250px;
  width: 250px;
  height: 100%;
  transition: left 0.3s;
}

.nav.open {
  left: 0;
}

/* Desktop: Always visible */
@media (min-width: 1024px) {
  .nav {
    position: static;
    left: 0;
    width: auto;
  }
}
```

**Use Cases**
- Mobile navigation
- Side drawers
- Filter panels

---

## Responsive Typography

### Fluid Typography

**Using Clamp**
```css
/* Formula: clamp(min, preferred, max) */
h1 {
  font-size: clamp(2rem, 5vw, 4rem);
  /* Min: 32px, Preferred: 5% of viewport, Max: 64px */
}

body {
  font-size: clamp(1rem, 2.5vw, 1.25rem);
  /* Min: 16px, scales with viewport, Max: 20px */
}
```

**Using Calc**
```css
h1 {
  font-size: calc(1.5rem + 2vw);
  /* Scales from 24px + viewport width */
}
```

### Type Scale

```css
/* Mobile */
:root {
  --text-xs: 0.75rem;   /* 12px */
  --text-sm: 0.875rem;  /* 14px */
  --text-base: 1rem;    /* 16px */
  --text-lg: 1.125rem;  /* 18px */
  --text-xl: 1.25rem;   /* 20px */
  --text-2xl: 1.5rem;   /* 24px */
  --text-3xl: 1.875rem; /* 30px */
  --text-4xl: 2.25rem;  /* 36px */
}

/* Desktop: Increase scale */
@media (min-width: 1024px) {
  :root {
    --text-base: 1.125rem; /* 18px */
    --text-lg: 1.25rem;    /* 20px */
    --text-xl: 1.5rem;     /* 24px */
    --text-2xl: 1.875rem;  /* 30px */
    --text-3xl: 2.25rem;   /* 36px */
    --text-4xl: 3rem;      /* 48px */
  }
}
```

---

## Responsive Images

### 1. Flexible Images

```css
img {
  max-width: 100%;
  height: auto;
  display: block;
}
```

### 2. Responsive Images (srcset)

```html
<!-- Different resolutions -->
<img
  src="image-800.jpg"
  srcset="
    image-400.jpg 400w,
    image-800.jpg 800w,
    image-1200.jpg 1200w
  "
  sizes="(max-width: 600px) 100vw, 50vw"
  alt="Description"
/>
```

### 3. Art Direction (picture)

```html
<!-- Different images for different screens -->
<picture>
  <source media="(max-width: 767px)" srcset="mobile.jpg" />
  <source media="(max-width: 1023px)" srcset="tablet.jpg" />
  <img src="desktop.jpg" alt="Description" />
</picture>
```

### 4. Modern Image Formats

```html
<picture>
  <source type="image/avif" srcset="image.avif" />
  <source type="image/webp" srcset="image.webp" />
  <img src="image.jpg" alt="Fallback" />
</picture>
```

---

## Responsive Navigation Patterns

### 1. Hamburger Menu

```html
<nav class="navbar">
  <button class="menu-toggle" aria-expanded="false">
    <span class="sr-only">Menu</span>
    ☰
  </button>
  
  <ul class="nav-menu">
    <li><a href="/">Home</a></li>
    <li><a href="/about">About</a></li>
    <li><a href="/contact">Contact</a></li>
  </ul>
</nav>
```

```css
/* Mobile: Hidden menu */
.nav-menu {
  display: none;
  flex-direction: column;
}

.menu-toggle {
  display: block;
}

.nav-menu.open {
  display: flex;
}

/* Desktop: Always visible */
@media (min-width: 768px) {
  .menu-toggle {
    display: none;
  }
  
  .nav-menu {
    display: flex;
    flex-direction: row;
  }
}
```

### 2. Priority+ Navigation

```html
<nav class="priority-nav">
  <ul class="nav-items">
    <li class="nav-item">Home</li>
    <li class="nav-item">Products</li>
    <li class="nav-item">About</li>
    <li class="nav-item">Contact</li>
  </ul>
  <button class="more-button">More</button>
  <ul class="overflow-menu" hidden>
    <!-- Overflow items appear here -->
  </ul>
</nav>
```

### 3. Bottom Tab Bar (Mobile)

```html
<nav class="tab-bar">
  <a href="/" class="tab-item active">
    <svg>...</svg>
    <span>Home</span>
  </a>
  <a href="/search" class="tab-item">
    <svg>...</svg>
    <span>Search</span>
  </a>
  <a href="/profile" class="tab-item">
    <svg>...</svg>
    <span>Profile</span>
  </a>
</nav>
```

```css
/* Mobile: Fixed bottom tabs */
.tab-bar {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
  display: flex;
  justify-content: space-around;
  background: white;
  border-top: 1px solid #e5e7eb;
}

/* Desktop: Convert to top nav */
@media (min-width: 768px) {
  .tab-bar {
    position: static;
    border-top: none;
    border-bottom: 1px solid #e5e7eb;
  }
}
```

---

## Responsive Tables

### 1. Horizontal Scroll

```css
.table-container {
  overflow-x: auto;
  -webkit-overflow-scrolling: touch;
}

table {
  min-width: 600px;
}
```

**Pros**: Simple, preserves table structure
**Cons**: Not ideal for accessibility, can hide content

### 2. Stacked Layout

```css
/* Mobile: Stack rows */
@media (max-width: 767px) {
  table, thead, tbody, th, td, tr {
    display: block;
  }
  
  thead {
    display: none;
  }
  
  tr {
    margin-bottom: 1rem;
    border: 1px solid #ddd;
  }
  
  td {
    text-align: right;
    padding-left: 50%;
    position: relative;
  }
  
  td::before {
    content: attr(data-label);
    position: absolute;
    left: 0;
    width: 50%;
    padding-left: 1rem;
    font-weight: bold;
    text-align: left;
  }
}
```

```html
<tr>
  <td data-label="Name">John Doe</td>
  <td data-label="Email">john@example.com</td>
  <td data-label="Role">Admin</td>
</tr>
```

### 3. Hide Columns

```css
/* Hide less important columns on mobile */
@media (max-width: 767px) {
  .hide-mobile {
    display: none;
  }
}
```

### 4. Card Layout

```css
/* Mobile: Convert to cards */
@media (max-width: 767px) {
  .responsive-table {
    display: grid;
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .table-row {
    background: white;
    border: 1px solid #ddd;
    border-radius: 8px;
    padding: 1rem;
  }
}
```

---

## Responsive Forms

### Stacked on Mobile, Grid on Desktop

```css
.form-grid {
  display: grid;
  gap: 1rem;
}

/* Mobile: Single column */
.form-grid {
  grid-template-columns: 1fr;
}

/* Desktop: Two columns */
@media (min-width: 768px) {
  .form-grid {
    grid-template-columns: 1fr 1fr;
  }
  
  /* Full width fields */
  .form-grid .full-width {
    grid-column: 1 / -1;
  }
}
```

### Touch-Friendly Inputs

```css
/* Larger inputs for mobile */
input, select, textarea {
  font-size: 16px; /* Prevents zoom on iOS */
  padding: 12px;
  min-height: 44px; /* Touch target size */
}

@media (min-width: 768px) {
  input, select, textarea {
    font-size: 14px;
    padding: 8px 12px;
  }
}
```

---

## Container Queries

```css
/* Container query (modern browsers) */
.card-container {
  container-type: inline-size;
}

/* When container is >400px wide */
@container (min-width: 400px) {
  .card {
    display: grid;
    grid-template-columns: 150px 1fr;
  }
}

/* When container is >600px wide */
@container (min-width: 600px) {
  .card {
    grid-template-columns: 200px 1fr;
  }
}
```

**Benefits**: Component-based responsive design, not viewport-based

---

## Viewport Units

```css
/* Viewport Width (vw) and Height (vh) */
.hero {
  height: 100vh; /* Full viewport height */
}

.sidebar {
  width: 30vw; /* 30% of viewport width */
}

/* Dynamic Viewport Units (mobile address bar) */
.hero-mobile {
  height: 100dvh; /* Dynamic viewport height */
}

/* Minimum/Maximum viewport */
.text {
  font-size: clamp(1rem, 2vw, 2rem);
}
```

---

## Modern CSS Features

### Grid Auto-Fit

```css
.grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}
```

**Result**: Columns automatically wrap based on available space

### Flexbox Wrapping

```css
.flex-container {
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
}

.flex-item {
  flex: 1 1 300px; /* Grow, shrink, base 300px */
}
```

### Aspect Ratio

```css
.video-container {
  aspect-ratio: 16 / 9;
}

.square {
  aspect-ratio: 1;
}
```

---

## Performance Optimization

### Lazy Loading Images

```html
<img src="image.jpg" loading="lazy" alt="Description" />
```

### Critical CSS

```html
<style>
  /* Inline critical above-the-fold CSS */
  .hero { background: blue; }
</style>

<link rel="stylesheet" href="styles.css" media="print" onload="this.media='all'" />
```

### Responsive Preloading

```html
<link
  rel="preload"
  as="image"
  href="hero-mobile.jpg"
  media="(max-width: 767px)"
/>
<link
  rel="preload"
  as="image"
  href="hero-desktop.jpg"
  media="(min-width: 768px)"
/>
```

---

## Testing Responsive Designs

### Browser DevTools
- Chrome DevTools (Device Mode)
- Firefox Responsive Design Mode
- Safari Responsive Design Mode

### Device Testing
- Physical devices (iOS, Android)
- BrowserStack (cloud testing)
- Sauce Labs (cloud testing)

### Viewport Sizes to Test
- 320px (iPhone SE)
- 375px (iPhone 12/13)
- 390px (iPhone 14)
- 768px (iPad portrait)
- 1024px (iPad landscape)
- 1366px (Laptop)
- 1920px (Desktop)

### Checklist
- [ ] Text readable at all sizes
- [ ] Touch targets adequate (44x44px minimum)
- [ ] Images scale properly
- [ ] Navigation accessible
- [ ] Forms usable
- [ ] Tables readable
- [ ] No horizontal scroll (unless intentional)
- [ ] Performance acceptable on mobile networks

---

## Common Responsive Mistakes

1. **Fixed widths** → Use percentages or max-width
2. **Viewport meta tag missing** → Add `<meta name="viewport">`
3. **Desktop-first approach** → Start mobile-first
4. **Too many breakpoints** → Keep it simple (3-4 max)
5. **Neglecting touch targets** → Minimum 44x44px
6. **Ignoring landscape** → Test both orientations
7. **Large image files** → Optimize and use srcset
8. **Horizontal scroll** → Use max-width: 100%

---

## Mobile-First Media Queries

```css
/* Mobile default styles */
.element {
  width: 100%;
  padding: 1rem;
}

/* Tablet and up */
@media (min-width: 768px) {
  .element {
    width: 50%;
    padding: 2rem;
  }
}

/* Desktop and up */
@media (min-width: 1024px) {
  .element {
    width: 33.333%;
    padding: 3rem;
  }
}
```

**Benefits of Mobile-First**
- Better performance (less CSS for mobile)
- Progressive enhancement
- Forces prioritization of content
- Easier to maintain

This reference provides comprehensive patterns for creating responsive, mobile-friendly interfaces.
