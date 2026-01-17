# Accessibility Guidelines (WCAG 2.1/2.2)

Comprehensive guide for designing and implementing accessible user interfaces.

## WCAG Principles: POUR

### 1. Perceivable
Information and UI components must be presentable to users in ways they can perceive.

### 2. Operable
UI components and navigation must be operable.

### 3. Understandable
Information and operation of UI must be understandable.

### 4. Robust
Content must be robust enough to be interpreted by a wide variety of user agents, including assistive technologies.

---

## WCAG Conformance Levels

### Level A (Minimum)
Basic accessibility features. Legal requirement in many jurisdictions.

### Level AA (Recommended)
Addresses most common accessibility barriers. Widely adopted as standard.

### Level AAA (Enhanced)
Highest level of accessibility. Not required for entire sites, but aim for critical content.

**Recommendation**: Target Level AA for all public-facing websites and applications.

---

## Color & Contrast

### Contrast Ratios (WCAG 2.1)

**Level AA**
- Normal text: 4.5:1
- Large text (18pt+ or 14pt+ bold): 3:1
- UI components and graphics: 3:1

**Level AAA**
- Normal text: 7:1
- Large text: 4.5:1

### Best Practices

1. **Don't rely on color alone**
   - Bad: Red = error, green = success (color only)
   - Good: Red + error icon + "Error" text
   
2. **Test with color blindness simulators**
   - Protanopia (red-blind)
   - Deuteranopia (green-blind)
   - Tritanopia (blue-blind)
   - Achromatopsia (no color)

3. **Use patterns or textures**
   - Charts: Use patterns in addition to colors
   - Status indicators: Icons + color
   
4. **Provide high contrast mode**
   - Support prefers-contrast media query
   - Windows high contrast mode compatibility

### Tools
- WebAIM Contrast Checker
- Chrome DevTools Contrast Checker
- Stark (Figma plugin)
- Color Oracle (color blindness simulator)

---

## Keyboard Accessibility

### Essential Requirements

1. **All functionality via keyboard**
   - No mouse-only features
   - Tab to navigate
   - Enter/Space to activate
   - Escape to close/cancel

2. **Logical tab order**
   - Follow visual layout
   - Top to bottom, left to right
   - Use tabindex="0" to include in order
   - Avoid positive tabindex values

3. **Visible focus indicator**
   - Minimum 2px outline
   - Sufficient contrast (3:1)
   - Never remove outline without alternative
   ```css
   /* Good */
   :focus {
     outline: 2px solid #0066CC;
     outline-offset: 2px;
   }
   
   /* Also good - custom focus style */
   :focus {
     outline: none; /* Remove default */
     box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.5);
   }
   
   /* Bad */
   :focus {
     outline: none; /* No alternative provided */
   }
   ```

4. **Skip links**
   - "Skip to main content" at page top
   - Hidden until focused
   ```html
   <a href="#main" class="skip-link">Skip to main content</a>
   
   <style>
   .skip-link {
     position: absolute;
     top: -40px;
     left: 0;
     background: #000;
     color: #fff;
     padding: 8px;
     z-index: 100;
   }
   
   .skip-link:focus {
     top: 0;
   }
   </style>
   ```

5. **Focus management**
   - Move focus to modal when opened
   - Return focus to trigger when closed
   - Focus first error on form submission
   - Trap focus in modal dialogs

### Keyboard Shortcuts

**Common Patterns**
- Tab: Next focusable element
- Shift+Tab: Previous focusable element
- Enter: Activate button/link
- Space: Activate button, check checkbox
- Arrow keys: Navigate menu, tabs, radio buttons
- Escape: Close modal, clear search, cancel operation
- Home/End: First/last item in list

**Custom Shortcuts**
- Document them clearly
- Avoid conflicts with browser/screen reader shortcuts
- Provide alternatives (don't require shortcuts)
- Make them discoverable

---

## Screen Reader Support

### Semantic HTML

**Use correct elements**
```html
<!-- Good -->
<button>Click me</button>
<a href="/page">Link</a>
<nav>...</nav>
<main>...</main>
<header>...</header>
<footer>...</footer>

<!-- Bad -->
<div onclick="...">Click me</div>
<span class="link">Link</span>
<div class="nav">...</div>
```

### ARIA (Accessible Rich Internet Applications)

**When to use ARIA**
- When semantic HTML is insufficient
- For complex widgets (tree view, tabs, accordion)
- To enhance existing HTML

**ARIA Rules**
1. Use semantic HTML first
2. Don't change native semantics
3. All interactive elements must be keyboard accessible
4. Don't use aria-hidden on focusable elements
5. All interactive elements must have accessible names

### Essential ARIA Attributes

**Roles**
```html
<div role="button">...</div>
<div role="dialog">...</div>
<div role="navigation">...</div>
<div role="alert">...</div>
<div role="status">...</div>
```

**States & Properties**
```html
<!-- Expanded/collapsed -->
<button aria-expanded="false" aria-controls="menu">Menu</button>

<!-- Current page -->
<a href="/about" aria-current="page">About</a>

<!-- Disabled -->
<button aria-disabled="true">Submit</button>

<!-- Required field -->
<input aria-required="true" />

<!-- Invalid field -->
<input aria-invalid="true" aria-describedby="error" />
<span id="error">Email is invalid</span>

<!-- Labels and descriptions -->
<button aria-label="Close dialog">×</button>
<input aria-labelledby="label" aria-describedby="help" />
```

**Live Regions**
```html
<!-- Polite: Wait for pause -->
<div aria-live="polite">Search returned 10 results</div>

<!-- Assertive: Interrupt immediately -->
<div aria-live="assertive" role="alert">Error: Connection lost</div>

<!-- Off: No announcements (default) -->
<div aria-live="off">...</div>
```

### Alternative Text

**Images**
```html
<!-- Informative image -->
<img src="chart.png" alt="Sales increased 25% in Q4" />

<!-- Decorative image -->
<img src="decoration.png" alt="" />
<!-- or -->
<img src="decoration.png" role="presentation" />

<!-- Linked image -->
<a href="/profile">
  <img src="avatar.png" alt="View profile" />
</a>

<!-- Complex image -->
<img src="complex-chart.png" alt="Quarterly sales data" 
     longdesc="chart-description.html" />
```

**Icon buttons**
```html
<!-- With text -->
<button>
  <svg aria-hidden="true">...</svg>
  Delete
</button>

<!-- Icon only -->
<button aria-label="Delete item">
  <svg aria-hidden="true">...</svg>
</button>
```

### Form Accessibility

**Labels**
```html
<!-- Good: Explicit label -->
<label for="email">Email</label>
<input type="email" id="email" />

<!-- Good: Implicit label -->
<label>
  Email
  <input type="email" />
</label>

<!-- Bad: Placeholder as label -->
<input type="email" placeholder="Email" /> <!-- No label! -->
```

**Helper text and errors**
```html
<label for="password">Password</label>
<input
  type="password"
  id="password"
  aria-describedby="password-help"
  aria-invalid="true"
  aria-errormessage="password-error"
/>
<p id="password-help">Must be at least 8 characters</p>
<p id="password-error" role="alert">Password is too short</p>
```

**Required fields**
```html
<!-- Visual and programmatic indication -->
<label for="name">
  Name <span aria-label="required">*</span>
</label>
<input type="text" id="name" required aria-required="true" />
```

**Fieldset and legend** (for grouped inputs)
```html
<fieldset>
  <legend>Contact preferences</legend>
  <label><input type="checkbox" /> Email</label>
  <label><input type="checkbox" /> SMS</label>
  <label><input type="checkbox" /> Phone</label>
</fieldset>
```

---

## Focus Management

### Modal Dialogs

```javascript
// Open modal
function openModal(modal) {
  // Store last focused element
  const lastFocused = document.activeElement;
  
  // Show modal
  modal.classList.add('open');
  
  // Move focus to modal
  modal.focus();
  
  // Trap focus
  trapFocus(modal);
  
  // Close handler
  modal.addEventListener('close', () => {
    // Return focus to trigger
    lastFocused.focus();
  });
}

// Focus trap
function trapFocus(element) {
  const focusableElements = element.querySelectorAll(
    'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
  );
  const firstFocusable = focusableElements[0];
  const lastFocusable = focusableElements[focusableElements.length - 1];
  
  element.addEventListener('keydown', (e) => {
    if (e.key === 'Tab') {
      if (e.shiftKey) {
        if (document.activeElement === firstFocusable) {
          e.preventDefault();
          lastFocusable.focus();
        }
      } else {
        if (document.activeElement === lastFocusable) {
          e.preventDefault();
          firstFocusable.focus();
        }
      }
    }
  });
}
```

### Single Page Apps (SPA)

```javascript
// Announce route changes
function navigateToPage(page) {
  // Load content
  loadContent(page);
  
  // Update page title
  document.title = page.title;
  
  // Move focus to main content
  const main = document.querySelector('main');
  main.tabIndex = -1;
  main.focus();
  
  // Announce to screen readers
  const announcement = document.createElement('div');
  announcement.setAttribute('role', 'status');
  announcement.setAttribute('aria-live', 'polite');
  announcement.textContent = `Navigated to ${page.title}`;
  document.body.appendChild(announcement);
  
  // Remove announcement after it's read
  setTimeout(() => announcement.remove(), 1000);
}
```

---

## Touch & Mobile Accessibility

### Touch Target Sizes

**Minimum sizes**
- iOS: 44x44 points
- Android: 48x48 dp
- WCAG 2.1 Level AAA: 44x44 CSS pixels

**Implementation**
```css
/* Button may be visually smaller */
.button {
  display: inline-flex;
  padding: 8px 16px;
  /* Ensure at least 44px height */
  min-height: 44px;
  align-items: center;
}

/* Increase clickable area with pseudo-element */
.icon-button {
  position: relative;
  width: 24px;
  height: 24px;
}

.icon-button::before {
  content: '';
  position: absolute;
  top: -10px;
  left: -10px;
  right: -10px;
  bottom: -10px;
  /* Creates 44x44px touch target */
}
```

### Spacing Between Targets

- Minimum 8px spacing between touch targets
- Prevents accidental activation

### Gestures

- Provide alternatives to complex gestures
- Swipe: Also provide buttons
- Pinch-to-zoom: Also provide zoom buttons
- Multi-finger: Also provide single-finger alternative

---

## Motion & Animation

### Prefers Reduced Motion

```css
/* Default: with animation */
.button {
  transition: background 0.3s ease;
}

/* Respect user preference */
@media (prefers-reduced-motion: reduce) {
  .button {
    transition: none;
  }
  
  * {
    animation-duration: 0.01ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.01ms !important;
  }
}
```

### Safe Animations

**Avoid**
- Flashing more than 3 times per second
- Parallax scrolling (can cause vestibular issues)
- Excessive motion
- Auto-playing videos with sound

**Best Practices**
- Provide play/pause controls
- Respect reduced motion preference
- Use subtle animations
- Avoid animations for critical information

---

## Content Accessibility

### Headings

**Hierarchy**
```html
<h1>Page Title</h1>
  <h2>Section</h2>
    <h3>Subsection</h3>
    <h3>Subsection</h3>
  <h2>Section</h2>
```

- Don't skip levels (h1 → h3)
- Use for structure, not styling
- One h1 per page (usually)

### Link Text

```html
<!-- Good: Descriptive -->
<a href="/report.pdf">Download Q4 Financial Report (PDF, 2MB)</a>

<!-- Bad: Generic -->
<a href="/report.pdf">Click here</a>

<!-- Bad: URL as text -->
<a href="https://example.com">https://example.com</a>
```

### Language

```html
<!-- Page language -->
<html lang="en">

<!-- Different language section -->
<p>The French word for hello is <span lang="fr">bonjour</span>.</p>
```

### Abbreviations

```html
<abbr title="World Wide Web Consortium">W3C</abbr>
```

---

## Testing Checklist

### Automated Testing

**Tools**
- Lighthouse (Chrome DevTools)
- axe DevTools
- WAVE Browser Extension
- Pa11y
- Jest-axe (for React)

**What they catch**
- Missing alt text
- Insufficient contrast
- Missing labels
- ARIA errors
- Heading hierarchy

**Limitations**
- Only catch ~30% of issues
- Can't test keyboard navigation
- Can't test screen reader experience
- Can't test logical reading order

### Manual Testing

**Keyboard Navigation**
- [ ] Tab through entire page
- [ ] All interactive elements reachable
- [ ] Visible focus indicator
- [ ] Logical tab order
- [ ] No keyboard traps
- [ ] Modal focus trapped correctly
- [ ] Escape closes modals

**Screen Reader Testing**
- [ ] Test with NVDA (Windows, free)
- [ ] Test with JAWS (Windows, paid)
- [ ] Test with VoiceOver (macOS/iOS, built-in)
- [ ] Test with TalkBack (Android, built-in)
- [ ] All images have alt text
- [ ] Form labels announced
- [ ] Errors announced
- [ ] Dynamic content announced
- [ ] Headings navigable
- [ ] Links descriptive

**Visual Testing**
- [ ] Zoom to 200% (WCAG)
- [ ] Test with browser zoom
- [ ] Test with Windows high contrast mode
- [ ] Simulate color blindness
- [ ] Check contrast ratios
- [ ] Test without images
- [ ] Test without CSS

**Mobile Testing**
- [ ] Touch targets at least 44x44
- [ ] Adequate spacing between targets
- [ ] Pinch to zoom allowed
- [ ] Orientation changes supported
- [ ] Screen reader gestures work

---

## Quick Wins

Easy improvements with high impact:

1. **Add alt text to images**
2. **Ensure sufficient color contrast**
3. **Make focus visible**
4. **Use semantic HTML**
5. **Label all form inputs**
6. **Add skip link**
7. **Make links descriptive**
8. **Use heading hierarchy**
9. **Don't disable zoom**
10. **Test with keyboard**

---

## Common Mistakes

1. **Placeholder as label** → Always provide label
2. **Removing focus outline** → Provide alternative
3. **Click div/span** → Use button/link
4. **Color-only indicators** → Add icon/text
5. **Low contrast text** → Increase contrast
6. **Auto-playing media** → Provide controls
7. **Small touch targets** → Minimum 44x44px
8. **Keyboard traps** → Allow escape
9. **Missing alt text** → Describe image
10. **Generic link text** → Be descriptive

---

## Resources

**Official Guidelines**
- WCAG 2.1: https://www.w3.org/WAI/WCAG21/quickref/
- ARIA Authoring Practices: https://www.w3.org/WAI/ARIA/apg/

**Testing Tools**
- axe DevTools: https://www.deque.com/axe/
- WAVE: https://wave.webaim.org/
- Lighthouse: Built into Chrome DevTools

**Learning Resources**
- WebAIM: https://webaim.org/
- A11y Project: https://www.a11yproject.com/
- Inclusive Components: https://inclusive-components.design/

**Screen Readers**
- NVDA (Windows): https://www.nvaccess.org/
- VoiceOver (Mac/iOS): Built-in
- TalkBack (Android): Built-in
- JAWS (Windows): https://www.freedomscientific.com/
