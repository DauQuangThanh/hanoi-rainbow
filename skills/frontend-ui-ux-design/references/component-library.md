# Design System Components Reference

Comprehensive guide for designing and implementing common UI components.

## Basic Components

### Button

**Purpose**: Trigger actions, submit forms, navigate

**Variants**
- **Primary**: Main call-to-action (solid background, high contrast)
- **Secondary**: Less prominent actions (outlined or light fill)
- **Tertiary/Ghost**: Subtle actions (text-only or minimal style)
- **Danger/Destructive**: Delete, remove actions (red/warning color)
- **Icon Button**: Icon-only for compact spaces

**Sizes**
- Small: 32px height (dense UIs, tables)
- Medium: 40px height (default)
- Large: 48px height (hero sections, mobile CTAs)

**States**
- Default
- Hover (subtle background change)
- Active/Pressed (darker/pressed effect)
- Focus (visible outline for keyboard)
- Disabled (reduced opacity, no interaction)
- Loading (spinner, disabled interaction)

**Specifications**
```css
/* Primary Button */
.button-primary {
  background: var(--color-primary);
  color: var(--color-white);
  border: none;
  border-radius: 4px;
  padding: 10px 16px;
  font-weight: 600;
  min-width: 80px;
  cursor: pointer;
  transition: background 0.2s ease;
}

.button-primary:hover {
  background: var(--color-primary-dark);
}

.button-primary:focus {
  outline: 2px solid var(--color-primary);
  outline-offset: 2px;
}
```

**Accessibility**
- Minimum size: 44x44px (touch devices)
- Clear focus indicator
- Descriptive text or aria-label
- Disabled state prevents interaction
- Loading state announces to screen readers

---

### Input Fields

**Types**
- Text input
- Email input
- Password input
- Number input
- Textarea (multi-line)
- Search input

**Anatomy**
- Label (above input)
- Input field
- Helper text (below, descriptive)
- Error message (replaces helper text)
- Optional indicator or required asterisk

**States**
- Default (empty)
- Filled (with value)
- Focus (active, outlined)
- Disabled (non-editable)
- Error (validation failed)
- Success (validation passed)

**Specifications**
```html
<div class="form-field">
  <label for="email" class="form-label">
    Email Address <span class="required">*</span>
  </label>
  <input
    type="email"
    id="email"
    class="form-input"
    placeholder="you@example.com"
    aria-describedby="email-helper"
    required
  />
  <p id="email-helper" class="form-helper">
    We'll never share your email.
  </p>
</div>
```

**Best Practices**
- Labels always visible (not placeholder-only)
- Clear placeholder as example
- Helper text for format or requirements
- Inline validation after blur
- Error messages specific and actionable
- Appropriate input type for mobile keyboards

**Accessibility**
- Label associated with input (for attribute)
- Helper text associated (aria-describedby)
- Error messages announced (aria-live)
- Required fields indicated (aria-required)

---

### Checkbox

**Purpose**: Select multiple options from a list

**States**
- Unchecked
- Checked
- Indeterminate (partial selection, parent checkbox)
- Disabled
- Focus

**Specifications**
```html
<div class="checkbox-wrapper">
  <input
    type="checkbox"
    id="terms"
    class="checkbox-input"
  />
  <label for="terms" class="checkbox-label">
    I agree to the terms and conditions
  </label>
</div>
```

**Best Practices**
- Minimum size: 20x20px (input), 44x44px (clickable area including label)
- Label clickable (increases target size)
- Clear visual checked state
- Use for independent selections

**Accessibility**
- Associated label
- Keyboard accessible (Space to toggle)
- Clear focus indicator
- aria-checked for custom checkboxes

---

### Radio Button

**Purpose**: Select one option from a list

**States**
- Unselected
- Selected
- Disabled
- Focus

**Best Practices**
- Group related options
- Pre-select default when appropriate
- Clear visual selected state
- Use when only one choice allowed

**Accessibility**
- Grouped with fieldset and legend
- Arrow keys navigate between options
- Only one focusable in group (selected one)

---

### Select Dropdown

**Purpose**: Choose one option from many

**Variants**
- Single select
- Multi-select
- Searchable select
- Grouped options

**States**
- Closed
- Open/Expanded
- Selected
- Focus
- Disabled

**Best Practices**
- Default placeholder or selected value
- Searchable for 7+ options
- Group related options
- Scroll long lists
- Clear selected value option

**Accessibility**
- Keyboard navigation (arrows, type to search)
- Announce selected value
- Escape to close
- Focus management

---

## Navigation Components

### Navigation Bar

**Types**
- **Top Nav**: Horizontal, global navigation
- **Sidebar Nav**: Vertical, many items, hierarchical
- **Bottom Tab Nav**: Mobile, 3-5 primary sections

**Components**
- Logo/Brand
- Primary navigation links
- Search bar (optional)
- User menu/profile
- Notifications (optional)
- CTA button (optional)

**Responsive Behavior**
- Desktop: Full horizontal menu
- Tablet: Condensed menu or icons
- Mobile: Hamburger menu or bottom tabs

**Specifications**
```html
<nav class="navbar" role="navigation" aria-label="Main navigation">
  <div class="navbar-brand">
    <a href="/" class="navbar-logo">Brand</a>
  </div>
  
  <ul class="navbar-menu">
    <li><a href="/products" class="navbar-link">Products</a></li>
    <li><a href="/about" class="navbar-link">About</a></li>
    <li><a href="/contact" class="navbar-link">Contact</a></li>
  </ul>
  
  <div class="navbar-actions">
    <button class="navbar-user">Profile</button>
  </div>
</nav>
```

**Accessibility**
- Semantic nav element
- Current page indicator (aria-current)
- Skip to main content link
- Keyboard accessible
- Focus visible

---

### Breadcrumbs

**Purpose**: Show current location in hierarchy

**Best Practices**
- Show hierarchy (Home > Category > Subcategory > Page)
- Make all levels clickable except current
- Use chevron or slash separators
- Don't include on homepage
- Mobile: Show only last 2-3 levels

**Accessibility**
- nav element with aria-label="Breadcrumb"
- Semantic ordered list
- aria-current="page" on current item

---

### Tabs

**Purpose**: Organize related content into separate views

**Types**
- Horizontal tabs (default)
- Vertical tabs (sidebar)
- Pills (rounded background)

**Best Practices**
- 3-7 tabs ideal
- Clear, concise labels
- Indicate active tab clearly
- Don't use for sequential steps (use stepper)
- Load content on demand or pre-load

**Accessibility**
- ARIA tablist, tab, tabpanel roles
- Arrow keys navigate tabs
- Tab key enters content
- aria-selected indicates active
- aria-controls links tab to panel

---

### Pagination

**Purpose**: Navigate through multiple pages of content

**Components**
- Previous button
- Page numbers
- Current page indicator
- Next button
- Optional: First/Last buttons
- Optional: Items per page selector

**Best Practices**
- Show 5-7 page numbers
- Truncate with ellipsis (1 ... 5 6 7 ... 20)
- Disable Previous on first page, Next on last
- Highlight current page
- Include total pages or items

---

## Feedback Components

### Alert / Notification

**Types by Severity**
- **Info**: Informational messages (blue)
- **Success**: Successful operations (green)
- **Warning**: Caution messages (yellow/orange)
- **Error**: Errors and failures (red)

**Variants**
- **Inline Alert**: Contextual, stays on page
- **Toast/Snackbar**: Temporary, auto-dismiss
- **Banner**: Page-level, persistent

**Components**
- Icon (semantic for type)
- Title (optional)
- Message text
- Action button (optional)
- Close button

**Specifications**
```html
<div class="alert alert-success" role="alert">
  <svg class="alert-icon" aria-hidden="true">...</svg>
  <div class="alert-content">
    <h4 class="alert-title">Success!</h4>
    <p class="alert-message">Your changes have been saved.</p>
  </div>
  <button class="alert-close" aria-label="Close alert">×</button>
</div>
```

**Best Practices**
- Clear, actionable messages
- Appropriate severity level
- Auto-dismiss for non-critical (3-5 seconds)
- Allow manual dismiss
- Stack multiple toasts
- Position consistently (top-right common)

**Accessibility**
- role="alert" for important messages
- aria-live="polite" or "assertive"
- Focus on alert for critical errors
- Keyboard dismissible

---

### Loading States

**Types**
- **Spinner**: Indeterminate progress
- **Progress Bar**: Determinate progress
- **Skeleton Screen**: Content placeholder
- **Overlay Loader**: Block interaction

**When to Use**
- **Spinner**: Quick operations (< 2s)
- **Progress Bar**: Long operations with known duration
- **Skeleton**: Initial page load, anticipate content
- **Overlay**: Prevent interaction during critical operation

**Best Practices**
- Show immediately (< 300ms delay)
- Provide context ("Loading products...")
- Use skeleton for perceived performance
- Never use generic "Loading..." for >3s
- Allow cancel for long operations

---

### Modal / Dialog

**Purpose**: Focus user on specific task or information

**Types**
- **Modal Dialog**: Blocks background, requires action
- **Non-Modal Dialog**: Can interact with background
- **Alert Dialog**: Requires acknowledgment
- **Confirmation Dialog**: Confirm action

**Components**
- Overlay/backdrop
- Dialog container
- Header with title
- Content area
- Footer with actions
- Close button (top right)

**Best Practices**
- Use sparingly (disruptive)
- Clear title describing purpose
- Primary action prominent (right)
- Secondary action less prominent (left)
- Close on overlay click (non-critical)
- Escape key closes
- Focus trap inside modal

**Accessibility**
- role="dialog" or role="alertdialog"
- aria-labelledby pointing to title
- aria-describedby pointing to content
- Focus moved to modal on open
- Focus returned to trigger on close
- Keyboard trap (Tab cycles within)
- Escape closes modal

---

## Data Display Components

### Table

**Purpose**: Display structured, tabular data

**Components**
- Table header (column names)
- Table rows
- Cells
- Optional: Row actions
- Optional: Row selection
- Optional: Sorting indicators
- Optional: Pagination

**Features**
- Sortable columns
- Filterable columns
- Searchable
- Row selection (checkbox)
- Expandable rows
- Sticky header
- Horizontal scroll (responsive)

**Responsive Strategies**
- **Horizontal Scroll**: Simple, less accessible
- **Stacked Layout**: Cards on mobile
- **Hide Columns**: Show essential columns only
- **Accordion Rows**: Expand for details

**Accessibility**
- Semantic table elements (table, thead, tbody, tr, th, td)
- scope attribute on headers (col or row)
- Caption or aria-label describing table
- Sortable headers indicate sort state
- Keyboard navigation for interactive cells

---

### Card

**Purpose**: Contain related information and actions

**Components**
- Header (optional, title/subtitle)
- Image/Media (optional)
- Content area
- Footer (optional, actions/metadata)

**Variants**
- Horizontal card (image left, content right)
- Vertical card (image top, content bottom)
- Clickable card (entire card is link)
- Interactive card (multiple actions)

**Best Practices**
- Consistent card sizes in grid
- Clear visual hierarchy within card
- Limit actions to 2-3
- Use hover effects for interactivity
- Adequate padding and spacing

---

### Accordion

**Purpose**: Show/hide content sections

**Best Practices**
- Clear section headers
- Chevron indicator (down when collapsed, up when expanded)
- Allow multiple open (optional)
- Animate expansion smoothly
- Remember state (optional)

**Accessibility**
- button for header (clickable)
- aria-expanded indicates state
- aria-controls links to content
- Content has unique ID
- Keyboard accessible (Enter/Space to toggle)

---

## Form Patterns

### Multi-Step Form

**Components**
- Stepper/Progress indicator
- Form sections
- Previous/Next buttons
- Save draft (optional)
- Review step (before submit)

**Best Practices**
- Show progress clearly
- Allow back navigation
- Validate per step
- Save progress automatically
- Show review before final submit

---

### Search

**Variants**
- Simple search (input + button)
- Autocomplete search (suggestions)
- Advanced search (filters, facets)

**Best Practices**
- Prominent placement
- Appropriate scope (site-wide vs. section)
- Show recent searches
- Autocomplete after 2-3 characters
- Highlight matching text
- Keyboard navigation (arrows, Enter)

---

## Animation & Motion

### Animation Durations
- **Instant**: 0ms (immediate changes)
- **Fast**: 100ms (tooltips, highlights)
- **Normal**: 200-300ms (most transitions)
- **Slow**: 400-500ms (large elements, page transitions)

### Easing Functions
- **Ease-in**: Slow start (elements leaving screen)
- **Ease-out**: Slow end (elements entering screen)
- **Ease-in-out**: Slow start and end (position changes)
- **Linear**: Constant speed (loading spinners)

### Common Animations
- **Fade**: Opacity change
- **Slide**: Position change
- **Scale**: Size change
- **Rotate**: Rotation
- **Shake**: Error indication

### Best Practices
- Respect prefers-reduced-motion
- Animate transform and opacity (GPU accelerated)
- Avoid animating layout properties (width, height)
- Keep under 300ms for most interactions
- Use consistent easing across app

---

## Micro-interactions

**Examples**
- Button press (scale down slightly)
- Like/favorite (heart animation)
- Add to cart (item flies to cart)
- Form validation (shake on error)
- Loading state (pulse or shimmer)
- Menu open (slide + fade)

**Purpose**
- Provide feedback
- Guide user attention
- Create delight
- Reinforce brand

**Best Practices**
- Subtle, not distracting
- Consistent across app
- Purposeful, not decorative
- Respect reduced motion preference

---

## Component States Checklist

For every interactive component, design these states:

- [ ] Default (resting state)
- [ ] Hover (mouse over)
- [ ] Focus (keyboard navigation)
- [ ] Active/Pressed (being clicked)
- [ ] Disabled (not interactive)
- [ ] Loading (async operation)
- [ ] Error (validation failed)
- [ ] Success (operation succeeded)
- [ ] Empty (no data)
- [ ] Populated (with data)

---

## Implementation Example: Button Component

```jsx
// React + Tailwind Example
import { forwardRef } from 'react';
import { cva, type VariantProps } from 'class-variance-authority';

const buttonVariants = cva(
  // Base styles
  'inline-flex items-center justify-center rounded-md font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50',
  {
    variants: {
      variant: {
        primary: 'bg-blue-600 text-white hover:bg-blue-700',
        secondary: 'bg-gray-200 text-gray-900 hover:bg-gray-300',
        ghost: 'hover:bg-gray-100 text-gray-700',
        danger: 'bg-red-600 text-white hover:bg-red-700',
      },
      size: {
        sm: 'h-8 px-3 text-sm',
        md: 'h-10 px-4',
        lg: 'h-12 px-6 text-lg',
      },
    },
    defaultVariants: {
      variant: 'primary',
      size: 'md',
    },
  }
);

interface ButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement>,
    VariantProps<typeof buttonVariants> {
  loading?: boolean;
}

export const Button = forwardRef<HTMLButtonElement, ButtonProps>(
  ({ className, variant, size, loading, children, ...props }, ref) => {
    return (
      <button
        ref={ref}
        className={buttonVariants({ variant, size, className })}
        disabled={loading || props.disabled}
        {...props}
      >
        {loading && <Spinner className="mr-2" />}
        {children}
      </button>
    );
  }
);
```

This reference provides comprehensive specifications for building consistent, accessible UI components.
