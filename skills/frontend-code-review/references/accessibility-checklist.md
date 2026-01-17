# Accessibility Checklist

Comprehensive guidelines for WCAG 2.1 AA/AAA compliance and inclusive design.

## Table of Contents
- [Semantic HTML](#semantic-html)
- [ARIA Best Practices](#aria-best-practices)
- [Keyboard Navigation](#keyboard-navigation)
- [Focus Management](#focus-management)
- [Screen Reader Support](#screen-reader-support)
- [Color and Contrast](#color-and-contrast)
- [Forms and Validation](#forms-and-validation)
- [Media Accessibility](#media-accessibility)
- [Testing Tools](#testing-tools)

## Semantic HTML

### Proper Document Structure

**Problem:**
```html
<!-- ❌ Div soup with no semantic meaning -->
<div class="header">
  <div class="logo">Logo</div>
  <div class="nav">
    <div class="nav-item">Home</div>
    <div class="nav-item">About</div>
  </div>
</div>

<div class="content">
  <div class="article">
    <div class="title">Article Title</div>
    <div class="text">Article content...</div>
  </div>
</div>

<div class="footer">
  <div>© 2024</div>
</div>
```

**Solution:**
```html
<!-- ✅ Semantic HTML5 elements -->
<header>
  <img src="logo.png" alt="Company Name Logo">
  <nav aria-label="Main navigation">
    <ul>
      <li><a href="/">Home</a></li>
      <li><a href="/about">About</a></li>
    </ul>
  </nav>
</header>

<main>
  <article>
    <h1>Article Title</h1>
    <p>Article content...</p>
  </article>
</main>

<footer>
  <p>© 2024 Company Name</p>
</footer>
```

### Heading Hierarchy

**Problem:**
```html
<!-- ❌ Skipping heading levels, multiple h1 -->
<h1>Page Title</h1>
<h3>Section</h3> <!-- Skipped h2 -->
<h1>Another Section</h1> <!-- Multiple h1 -->
<h4>Subsection</h4> <!-- Skipped h3 -->
```

**Solution:**
```html
<!-- ✅ Logical heading hierarchy -->
<h1>Page Title</h1>

<section>
  <h2>Main Section</h2>
  <p>Content...</p>
  
  <h3>Subsection</h3>
  <p>Content...</p>
  
  <h4>Sub-subsection</h4>
  <p>Content...</p>
</section>

<section>
  <h2>Another Main Section</h2>
  <p>Content...</p>
</section>
```

### Lists

**Problem:**
```html
<!-- ❌ Not using list elements for lists -->
<div>
  <div>• Item 1</div>
  <div>• Item 2</div>
  <div>• Item 3</div>
</div>
```

**Solution:**
```html
<!-- ✅ Proper list markup -->
<ul>
  <li>Item 1</li>
  <li>Item 2</li>
  <li>Item 3</li>
</ul>

<!-- For navigation -->
<nav aria-label="Site navigation">
  <ul>
    <li><a href="/">Home</a></li>
    <li><a href="/about">About</a></li>
    <li><a href="/contact">Contact</a></li>
  </ul>
</nav>
```

## ARIA Best Practices

### First Rule of ARIA

**No ARIA is better than bad ARIA.**

Use semantic HTML first, ARIA only when necessary.

**Problem:**
```html
<!-- ❌ Unnecessary ARIA on semantic elements -->
<button role="button" aria-label="Submit">Submit</button>
<nav role="navigation">...</nav>
<header role="banner">...</header>
```

**Solution:**
```html
<!-- ✅ Semantic HTML without redundant ARIA -->
<button>Submit</button>
<nav>...</nav>
<header>...</header>
```

### ARIA Labels and Descriptions

**Problem:**
```jsx
// ❌ Icon buttons without labels
<button onClick={handleDelete}>
  <TrashIcon />
</button>

// ❌ Vague labels
<button aria-label="Click here">
  <DownloadIcon />
</button>
```

**Solution:**
```jsx
// ✅ Descriptive aria-label
<button onClick={handleDelete} aria-label="Delete item">
  <TrashIcon aria-hidden="true" />
</button>

// ✅ Specific and contextual
<button 
  onClick={handleDownload}
  aria-label="Download user manual PDF"
>
  <DownloadIcon aria-hidden="true" />
</button>

// ✅ Using aria-labelledby for complex labels
<div>
  <h3 id="section-title">Important Document</h3>
  <button 
    aria-labelledby="section-title download-label"
    onClick={handleDownload}
  >
    <span id="download-label">Download</span>
    <DownloadIcon aria-hidden="true" />
  </button>
</div>
```

### Live Regions

**Problem:**
```jsx
// ❌ No announcement for dynamic updates
function Notifications() {
  const [message, setMessage] = useState('');
  
  useEffect(() => {
    const unsubscribe = subscribeToNotifications((msg) => {
      setMessage(msg);
    });
    return unsubscribe;
  }, []);
  
  return <div>{message}</div>;
}
```

**Solution:**
```jsx
// ✅ Using aria-live for announcements
function Notifications() {
  const [message, setMessage] = useState('');
  
  useEffect(() => {
    const unsubscribe = subscribeToNotifications((msg) => {
      setMessage(msg);
    });
    return unsubscribe;
  }, []);
  
  return (
    <div 
      role="status" 
      aria-live="polite"
      aria-atomic="true"
    >
      {message}
    </div>
  );
}

// ✅ For urgent messages
function ErrorAlert({ error }) {
  return (
    <div 
      role="alert"
      aria-live="assertive"
      aria-atomic="true"
    >
      {error}
    </div>
  );
}
```

### ARIA States

**Problem:**
```jsx
// ❌ Visual state not conveyed to AT
<button onClick={toggleMenu}>
  Menu {isOpen ? '▼' : '▶'}
</button>
<div style={{ display: isOpen ? 'block' : 'none' }}>
  {/* Menu items */}
</div>
```

**Solution:**
```jsx
// ✅ Proper ARIA states
<button 
  onClick={toggleMenu}
  aria-expanded={isOpen}
  aria-controls="menu-content"
>
  Menu
</button>
<div 
  id="menu-content"
  hidden={!isOpen}
>
  {/* Menu items */}
</div>

// ✅ Toggle button
<button
  onClick={toggleMute}
  aria-pressed={isMuted}
  aria-label={isMuted ? 'Unmute audio' : 'Mute audio'}
>
  {isMuted ? <MutedIcon /> : <UnmutedIcon />}
</button>
```

## Keyboard Navigation

### Keyboard Accessibility Basics

**All interactive elements must be keyboard accessible:**

#### Focus Order

**Problem:**
```jsx
// ❌ Incorrect tab order with tabindex
<div>
  <button tabIndex={3}>First</button>
  <button tabIndex={1}>Second</button>
  <button tabIndex={2}>Third</button>
</div>
```

**Solution:**
```jsx
// ✅ Natural DOM order (no positive tabindex)
<div>
  <button>First</button>
  <button>Second</button>
  <button>Third</button>
</div>

// ✅ Only use tabindex="-1" to remove from tab order
<div tabIndex={-1} onClick={handleClick}>
  Not keyboard focusable
</div>

// ✅ Use tabindex="0" to add non-interactive elements to tab order
<div 
  tabIndex={0}
  role="button"
  onClick={handleClick}
  onKeyDown={handleKeyDown}
>
  Custom interactive element
</div>
```

### Keyboard Event Handlers

**Problem:**
```jsx
// ❌ Only mouse events
<div onClick={handleClick}>
  Click me
</div>

// ❌ Missing Enter key support
<div 
  tabIndex={0}
  onKeyDown={(e) => {
    if (e.key === ' ') handleClick();
  }}
>
  Press Space
</div>
```

**Solution:**
```jsx
// ✅ Use button for interactive elements
<button onClick={handleClick}>
  Click me
</button>

// ✅ If custom element is necessary, support both Space and Enter
function CustomButton({ onClick, children }) {
  const handleKeyDown = (e) => {
    if (e.key === ' ' || e.key === 'Enter') {
      e.preventDefault();
      onClick();
    }
  };
  
  return (
    <div
      role="button"
      tabIndex={0}
      onClick={onClick}
      onKeyDown={handleKeyDown}
    >
      {children}
    </div>
  );
}

// ✅ Reusable hook for keyboard activation
function useKeyboardActivation(callback) {
  return {
    onClick: callback,
    onKeyDown: (e) => {
      if (e.key === ' ' || e.key === 'Enter') {
        e.preventDefault();
        callback();
      }
    },
    role: 'button',
    tabIndex: 0,
  };
}
```

### Skip Links

**Problem:**
```jsx
// ❌ No way to skip repetitive navigation
<div>
  <header>
    <nav>{/* 20+ navigation links */}</nav>
  </header>
  <main>{/* Content */}</main>
</div>
```

**Solution:**
```jsx
// ✅ Skip link for keyboard users
<div>
  <a href="#main-content" className="skip-link">
    Skip to main content
  </a>
  
  <header>
    <nav>{/* 20+ navigation links */}</nav>
  </header>
  
  <main id="main-content" tabIndex={-1}>
    {/* Content */}
  </main>
</div>
```

```css
/* Skip link styles */
.skip-link {
  position: absolute;
  top: -40px;
  left: 0;
  background: #000;
  color: #fff;
  padding: 8px;
  text-decoration: none;
  z-index: 100;
}

.skip-link:focus {
  top: 0;
}
```

### Complex Widgets

#### Dropdown Menu

```jsx
// ✅ Accessible dropdown with keyboard support
function Dropdown({ trigger, items }) {
  const [isOpen, setIsOpen] = useState(false);
  const [focusedIndex, setFocusedIndex] = useState(-1);
  const triggerRef = useRef(null);
  const menuRef = useRef(null);
  
  const handleTriggerKeyDown = (e) => {
    switch (e.key) {
      case 'Enter':
      case ' ':
      case 'ArrowDown':
        e.preventDefault();
        setIsOpen(true);
        setFocusedIndex(0);
        break;
      case 'Escape':
        setIsOpen(false);
        triggerRef.current?.focus();
        break;
    }
  };
  
  const handleMenuKeyDown = (e) => {
    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        setFocusedIndex((prev) => 
          prev < items.length - 1 ? prev + 1 : 0
        );
        break;
      case 'ArrowUp':
        e.preventDefault();
        setFocusedIndex((prev) => 
          prev > 0 ? prev - 1 : items.length - 1
        );
        break;
      case 'Home':
        e.preventDefault();
        setFocusedIndex(0);
        break;
      case 'End':
        e.preventDefault();
        setFocusedIndex(items.length - 1);
        break;
      case 'Escape':
        e.preventDefault();
        setIsOpen(false);
        triggerRef.current?.focus();
        break;
      case 'Enter':
      case ' ':
        e.preventDefault();
        items[focusedIndex]?.onClick();
        setIsOpen(false);
        break;
    }
  };
  
  return (
    <div>
      <button
        ref={triggerRef}
        aria-haspopup="true"
        aria-expanded={isOpen}
        onClick={() => setIsOpen(!isOpen)}
        onKeyDown={handleTriggerKeyDown}
      >
        {trigger}
      </button>
      
      {isOpen && (
        <ul
          ref={menuRef}
          role="menu"
          onKeyDown={handleMenuKeyDown}
        >
          {items.map((item, index) => (
            <li
              key={index}
              role="menuitem"
              tabIndex={focusedIndex === index ? 0 : -1}
              onClick={item.onClick}
            >
              {item.label}
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}
```

#### Modal Dialog

```jsx
// ✅ Accessible modal with focus trap
function Modal({ isOpen, onClose, title, children }) {
  const modalRef = useRef(null);
  const previousFocusRef = useRef(null);
  
  useEffect(() => {
    if (isOpen) {
      previousFocusRef.current = document.activeElement;
      
      // Focus first focusable element in modal
      const focusableElements = modalRef.current?.querySelectorAll(
        'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
      );
      focusableElements?.[0]?.focus();
      
      // Trap focus
      const handleTabKey = (e) => {
        if (e.key !== 'Tab') return;
        
        const firstElement = focusableElements[0];
        const lastElement = focusableElements[focusableElements.length - 1];
        
        if (e.shiftKey && document.activeElement === firstElement) {
          e.preventDefault();
          lastElement.focus();
        } else if (!e.shiftKey && document.activeElement === lastElement) {
          e.preventDefault();
          firstElement.focus();
        }
      };
      
      document.addEventListener('keydown', handleTabKey);
      return () => document.removeEventListener('keydown', handleTabKey);
    } else {
      // Restore focus on close
      previousFocusRef.current?.focus();
    }
  }, [isOpen]);
  
  const handleKeyDown = (e) => {
    if (e.key === 'Escape') {
      onClose();
    }
  };
  
  if (!isOpen) return null;
  
  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-labelledby="modal-title"
      ref={modalRef}
      onKeyDown={handleKeyDown}
    >
      <h2 id="modal-title">{title}</h2>
      <div>{children}</div>
      <button onClick={onClose}>Close</button>
    </div>
  );
}
```

## Focus Management

### Focus Indicators

**Problem:**
```css
/* ❌ Removing focus outline */
*:focus {
  outline: none;
}
```

**Solution:**
```css
/* ✅ Custom focus styles that meet contrast requirements */
*:focus {
  outline: 2px solid #0066cc;
  outline-offset: 2px;
}

/* ✅ Different styles for keyboard vs mouse (using :focus-visible) */
*:focus {
  outline: none; /* Remove default */
}

*:focus-visible {
  outline: 2px solid #0066cc;
  outline-offset: 2px;
}

/* ✅ High contrast focus for better visibility */
@media (prefers-contrast: high) {
  *:focus-visible {
    outline: 3px solid currentColor;
    outline-offset: 3px;
  }
}
```

### Programmatic Focus

**Problem:**
```jsx
// ❌ Not managing focus after actions
function DeleteButton({ itemId, onDelete }) {
  return (
    <button onClick={() => onDelete(itemId)}>
      Delete
    </button>
  );
}

// After deletion, focus is lost
```

**Solution:**
```jsx
// ✅ Restore focus after deletion
function ItemList({ items }) {
  const listRef = useRef(null);
  
  const handleDelete = (itemId, index) => {
    deleteItem(itemId);
    
    // Focus next item, or previous, or container
    const focusTarget = 
      listRef.current?.querySelector(`[data-index="${index}"]`) ||
      listRef.current?.querySelector(`[data-index="${index - 1}"]`) ||
      listRef.current;
    
    focusTarget?.focus();
  };
  
  return (
    <ul ref={listRef} tabIndex={-1}>
      {items.map((item, index) => (
        <li key={item.id} data-index={index}>
          {item.name}
          <button onClick={() => handleDelete(item.id, index)}>
            Delete
          </button>
        </li>
      ))}
    </ul>
  );
}
```

## Screen Reader Support

### Accessible Names

**Problem:**
```jsx
// ❌ Empty links and buttons
<a href="/next">
  <img src="arrow.png" />
</a>

<button>
  <SearchIcon />
</button>
```

**Solution:**
```jsx
// ✅ Accessible names via multiple methods
<a href="/next" aria-label="Next page">
  <img src="arrow.png" alt="" aria-hidden="true" />
</a>

<button aria-label="Search">
  <SearchIcon aria-hidden="true" />
</button>

// ✅ Visually hidden text
<button>
  <SearchIcon aria-hidden="true" />
  <span className="sr-only">Search</span>
</button>
```

```css
.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border-width: 0;
}
```

### Status Messages

```jsx
// ✅ Announce status changes
function SaveButton({ onSave }) {
  const [status, setStatus] = useState('idle');
  
  const handleSave = async () => {
    setStatus('saving');
    try {
      await onSave();
      setStatus('saved');
      setTimeout(() => setStatus('idle'), 3000);
    } catch (error) {
      setStatus('error');
    }
  };
  
  return (
    <>
      <button onClick={handleSave} disabled={status === 'saving'}>
        Save
      </button>
      
      <div role="status" aria-live="polite" aria-atomic="true">
        {status === 'saving' && 'Saving...'}
        {status === 'saved' && 'Saved successfully'}
        {status === 'error' && 'Error saving'}
      </div>
    </>
  );
}
```

## Color and Contrast

### WCAG Contrast Requirements

**Levels:**
- **AA (Minimum):** 4.5:1 for normal text, 3:1 for large text
- **AAA (Enhanced):** 7:1 for normal text, 4.5:1 for large text

**Problem:**
```css
/* ❌ Insufficient contrast */
.text {
  color: #999; /* 2.8:1 on white background */
  background: #fff;
}

.button {
  color: #0066cc;
  background: #4da6ff; /* 2.1:1 - fails */
}
```

**Solution:**
```css
/* ✅ Meets WCAG AA contrast */
.text {
  color: #595959; /* 7:1 on white background */
  background: #fff;
}

.button {
  color: #fff;
  background: #0066cc; /* 4.5:1 with white text */
}

/* ✅ High contrast mode support */
@media (prefers-contrast: high) {
  .button {
    outline: 2px solid currentColor;
    outline-offset: 2px;
  }
}
```

### Color-Independent Information

**Problem:**
```jsx
// ❌ Color as only indicator
<input type="text" style={{ borderColor: isValid ? 'green' : 'red' }} />
<p style={{ color: isValid ? 'green' : 'red' }}>
  {isValid ? 'Valid' : 'Invalid'}
</p>
```

**Solution:**
```jsx
// ✅ Multiple indicators (color + icon + text)
<div>
  <input
    type="text"
    aria-invalid={!isValid}
    aria-describedby="validation-message"
    style={{ borderColor: isValid ? 'green' : 'red' }}
  />
  <div id="validation-message" role="alert">
    {!isValid && (
      <>
        <ErrorIcon aria-hidden="true" />
        <span>Invalid input. Please enter a valid email.</span>
      </>
    )}
    {isValid && (
      <>
        <SuccessIcon aria-hidden="true" />
        <span>Valid</span>
      </>
    )}
  </div>
</div>
```

## Forms and Validation

### Form Labels

**Problem:**
```jsx
// ❌ Placeholder as label
<input type="text" placeholder="Email" />

// ❌ Missing label association
<label>Email</label>
<input type="email" />
```

**Solution:**
```jsx
// ✅ Proper label association
<label htmlFor="email">Email</label>
<input type="email" id="email" />

// ✅ Using aria-labelledby
<div>
  <h3 id="email-label">Email Address</h3>
  <input 
    type="email"
    aria-labelledby="email-label"
    aria-describedby="email-hint"
  />
  <p id="email-hint">We'll never share your email</p>
</div>
```

### Error Messages

**Problem:**
```jsx
// ❌ Errors not associated with inputs
<input type="email" />
{error && <p>{error}</p>}
```

**Solution:**
```jsx
// ✅ Accessible error messages
<div>
  <label htmlFor="email">Email</label>
  <input
    type="email"
    id="email"
    aria-invalid={!!error}
    aria-describedby={error ? 'email-error' : undefined}
  />
  {error && (
    <p id="email-error" role="alert">
      {error}
    </p>
  )}
</div>
```

## Media Accessibility

### Images

**Problem:**
```jsx
// ❌ Missing or poor alt text
<img src="photo.jpg" alt="Photo" />
<img src="logo.png" />
```

**Solution:**
```jsx
// ✅ Descriptive alt text
<img src="photo.jpg" alt="Golden retriever playing fetch in a park" />

// ✅ Empty alt for decorative images
<img src="decorative-line.png" alt="" />

// ✅ Complex images with longer descriptions
<figure>
  <img 
    src="chart.png"
    alt="Bar chart showing sales growth"
    aria-describedby="chart-description"
  />
  <figcaption id="chart-description">
    Sales increased from $10M in Q1 to $15M in Q2, with a peak of $18M in Q3.
  </figcaption>
</figure>
```

### Videos

**Problem:**
```jsx
// ❌ Video without captions or transcript
<video src="presentation.mp4" controls />
```

**Solution:**
```jsx
// ✅ Accessible video
<video controls>
  <source src="presentation.mp4" type="video/mp4" />
  <track
    kind="captions"
    src="captions-en.vtt"
    srclang="en"
    label="English"
    default
  />
  <track
    kind="descriptions"
    src="descriptions-en.vtt"
    srclang="en"
    label="English"
  />
</video>

<details>
  <summary>Transcript</summary>
  <p>Full video transcript here...</p>
</details>
```

## Testing Tools

### Automated Testing

```bash
# Lighthouse CI
npm install -g @lhci/cli
lhci autorun --collect.url=http://localhost:3000

# axe-core
npm install --save-dev @axe-core/react
```

```jsx
// React integration
import React from 'react';
import ReactDOM from 'react-dom';

if (process.env.NODE_ENV !== 'production') {
  const axe = require('@axe-core/react');
  axe(React, ReactDOM, 1000);
}
```

### Manual Testing Checklist

1. **Keyboard Navigation**
   - Can you navigate the entire site using only Tab, Shift+Tab, Enter, Space, and Arrow keys?
   - Is the focus indicator always visible?
   - Can you access all interactive elements?

2. **Screen Reader Testing**
   - Test with NVDA (Windows), JAWS (Windows), or VoiceOver (Mac/iOS)
   - Are all elements properly announced?
   - Do ARIA labels make sense in context?

3. **Zoom and Text Resize**
   - Test at 200% zoom (WCAG AA requirement)
   - Does content remain readable and functional?
   - No horizontal scrolling required?

4. **Color Contrast**
   - Use browser dev tools or online checkers
   - Verify all text meets minimum contrast ratios
   - Test in high contrast mode

5. **Forms**
   - Are all form fields properly labeled?
   - Do error messages announce to screen readers?
   - Can you complete forms using only keyboard?
