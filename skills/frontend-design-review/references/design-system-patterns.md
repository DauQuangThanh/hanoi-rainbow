# Design System Patterns Reference

This document provides comprehensive guidance on evaluating and validating design system patterns, component libraries, design tokens, and design system governance during frontend design reviews.

## Table of Contents

1. [Design Tokens](#design-tokens)
2. [Component Patterns](#component-patterns)
3. [Layout Patterns](#layout-patterns)
4. [Navigation Patterns](#navigation-patterns)
5. [Form Patterns](#form-patterns)
6. [Feedback Patterns](#feedback-patterns)
7. [Data Display Patterns](#data-display-patterns)
8. [Design System Governance](#design-system-governance)

---

## Design Tokens

Design tokens are the visual design atoms of a design system—specifically, they are named entities that store visual design attributes. They ensure consistency across products and platforms.

### Color Tokens

**Token Structure:**
```yaml
# Semantic color tokens (preferred)
color:
  brand:
    primary: '#0066CC'
    secondary: '#6B46C1'
  feedback:
    success: '#10B981'
    warning: '#F59E0B'
    error: '#EF4444'
    info: '#3B82F6'
  text:
    primary: '#1F2937'
    secondary: '#6B7280'
    tertiary: '#9CA3AF'
    inverse: '#FFFFFF'
  background:
    primary: '#FFFFFF'
    secondary: '#F9FAFB'
    tertiary: '#F3F4F6'
  border:
    default: '#E5E7EB'
    strong: '#D1D5DB'
```

**Review Checklist:**
- [ ] Semantic naming used (not colors like "blue-500")
- [ ] Sufficient contrast ratios for accessibility
- [ ] Dark mode tokens defined if applicable
- [ ] Consistent color usage across designs
- [ ] Colors mapped to design tokens, not hardcoded
- [ ] Limited color palette prevents color chaos
- [ ] Color tokens documented with usage guidelines

**Common Issues:**
- ❌ Hardcoded hex values instead of token references
- ❌ Too many color variations (e.g., 15 shades of blue)
- ❌ Inconsistent semantic naming (success vs. positive)
- ❌ Missing dark mode color tokens
- ❌ Poor contrast ratios not caught in token definition

### Typography Tokens

**Token Structure:**
```yaml
typography:
  font-family:
    primary: 'Inter, system-ui, sans-serif'
    mono: 'Fira Code, Consolas, monospace'
  
  font-size:
    xs: '0.75rem'    # 12px
    sm: '0.875rem'   # 14px
    base: '1rem'     # 16px
    lg: '1.125rem'   # 18px
    xl: '1.25rem'    # 20px
    2xl: '1.5rem'    # 24px
    3xl: '1.875rem'  # 30px
    4xl: '2.25rem'   # 36px
  
  font-weight:
    normal: 400
    medium: 500
    semibold: 600
    bold: 700
  
  line-height:
    tight: 1.25
    normal: 1.5
    relaxed: 1.75
  
  letter-spacing:
    tight: '-0.025em'
    normal: '0'
    wide: '0.025em'
```

**Review Checklist:**
- [ ] Type scale follows consistent ratio (1.2x, 1.25x, 1.5x)
- [ ] Line heights appropriate for font sizes
- [ ] Font weights available and used consistently
- [ ] Limited number of font sizes (6-8 typically sufficient)
- [ ] Responsive typography strategy defined
- [ ] Font loading strategy considered
- [ ] Fallback fonts specified

### Spacing Tokens

**Token Structure:**
```yaml
spacing:
  0: '0'
  1: '0.25rem'   # 4px
  2: '0.5rem'    # 8px
  3: '0.75rem'   # 12px
  4: '1rem'      # 16px
  5: '1.25rem'   # 20px
  6: '1.5rem'    # 24px
  8: '2rem'      # 32px
  10: '2.5rem'   # 40px
  12: '3rem'     # 48px
  16: '4rem'     # 64px
  20: '5rem'     # 80px
```

**Review Checklist:**
- [ ] Spacing scale follows consistent progression (typically 4px base)
- [ ] Limited number of spacing values
- [ ] Spacing tokens used for margins, padding, gaps
- [ ] Optical adjustments documented where needed
- [ ] Consistent spacing between related elements
- [ ] Spacing scales appropriately at different breakpoints

### Shadow Tokens

**Token Structure:**
```yaml
shadow:
  sm: '0 1px 2px 0 rgba(0, 0, 0, 0.05)'
  base: '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)'
  md: '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)'
  lg: '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)'
  xl: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)'
```

**Review Checklist:**
- [ ] Shadow scale indicates elevation hierarchy
- [ ] Consistent shadow usage across similar components
- [ ] Shadows subtle and not distracting
- [ ] Dark mode shadow variants defined

### Border Radius Tokens

**Token Structure:**
```yaml
border-radius:
  none: '0'
  sm: '0.125rem'   # 2px
  base: '0.25rem'  # 4px
  md: '0.375rem'   # 6px
  lg: '0.5rem'     # 8px
  xl: '0.75rem'    # 12px
  2xl: '1rem'      # 16px
  full: '9999px'   # Pill shape
```

**Review Checklist:**
- [ ] Consistent border radius usage
- [ ] Limited number of radius values
- [ ] Border radius appropriate for component size
- [ ] Consistent across similar components

---

## Component Patterns

### Button Component

**States Required:**
- Default (rest state)
- Hover
- Active/Pressed
- Focus (keyboard)
- Disabled
- Loading

**Variants:**
- **Style**: Primary, Secondary, Tertiary, Danger, Ghost
- **Size**: Small, Medium, Large
- **Icon**: Icon left, Icon right, Icon only

**Review Checklist:**
- [ ] All states designed and visually distinct
- [ ] Focus indicator clearly visible (3px outline minimum)
- [ ] Disabled state clearly indicates unavailability (reduced opacity, no hover)
- [ ] Loading state shows spinner or loading indicator
- [ ] Minimum touch target 44x44px on mobile
- [ ] Icon alignment consistent across sizes
- [ ] Text truncation handled for long labels
- [ ] Variants limited and purposeful
- [ ] Destructive actions use danger variant

**Example Specifications:**
```yaml
Button:
  sizes:
    sm:
      height: 32px
      padding-x: 12px
      font-size: 14px
    md:
      height: 40px
      padding-x: 16px
      font-size: 16px
    lg:
      height: 48px
      padding-x: 20px
      font-size: 18px
  
  variants:
    primary:
      background: color.brand.primary
      text: color.text.inverse
      hover-background: color.brand.primary-dark
    
    secondary:
      background: color.background.secondary
      text: color.text.primary
      border: 1px solid color.border.default
```

### Input Field Component

**States Required:**
- Empty (placeholder visible)
- Filled (user entered text)
- Focus
- Disabled
- Error
- Success
- Read-only

**Elements:**
- Label (required)
- Input field
- Helper text
- Error message
- Success message
- Optional indicator
- Character count (if applicable)

**Review Checklist:**
- [ ] Label always visible (not using placeholder as label)
- [ ] Placeholder text provides example, not instructions
- [ ] Error messages specific and actionable
- [ ] Error state shows red border and error icon
- [ ] Focus state has clear visual indicator
- [ ] Required fields clearly marked (asterisk or "required" text)
- [ ] Helper text provides useful guidance
- [ ] Disabled state clearly indicates unavailability
- [ ] Touch target includes label area (easier to tap)
- [ ] Input height minimum 44px on mobile

### Card Component

**Elements:**
- Container
- Header (optional)
- Content area
- Footer (optional)
- Image/media (optional)
- Actions (optional)

**Review Checklist:**
- [ ] Padding consistent across card sections
- [ ] Clickable cards have hover state
- [ ] Shadow or border defines card boundaries
- [ ] Content hierarchy clear within card
- [ ] Responsive behavior defined (stacking, wrapping)
- [ ] Loading state designed if content loads async
- [ ] Empty state designed if content might be missing

### Modal/Dialog Component

**Elements:**
- Backdrop (overlay)
- Container
- Header with title
- Close button
- Content area
- Footer with actions

**Review Checklist:**
- [ ] Backdrop dims background content
- [ ] Modal centered and responsive
- [ ] Close button clearly visible (top-right corner)
- [ ] Focus trapped within modal when open
- [ ] Escape key closes modal
- [ ] Scrollable content area if content long
- [ ] Primary and secondary actions clear
- [ ] Modal width limited (max-width: 600px typical)
- [ ] Animation smooth (fade in/scale up)

### Dropdown/Select Component

**States:**
- Closed
- Open
- Selected
- Hover (on options)
- Focus
- Disabled
- Error

**Review Checklist:**
- [ ] Current selection clearly visible when closed
- [ ] Dropdown icon indicates interactivity
- [ ] Options list easy to scan (good spacing, typography)
- [ ] Selected option highlighted in open list
- [ ] Keyboard navigation supported (arrow keys)
- [ ] Search functionality for long lists
- [ ] Max height with scrolling for long lists
- [ ] Option grouping if applicable
- [ ] Multi-select uses checkboxes, not just highlighting

---

## Layout Patterns

### Grid Systems

**Common Grid Systems:**
- 12-column grid (most flexible)
- 4-column grid mobile, 8-column tablet, 12-column desktop
- Flexbox-based responsive grid
- CSS Grid-based layout system

**Review Checklist:**
- [ ] Grid system consistent across designs
- [ ] Column count appropriate for breakpoint
- [ ] Gutters (gaps) consistent
- [ ] Content aligned to grid columns
- [ ] Grid adapts logically at breakpoints
- [ ] Maximum content width defined for readability

**Example Specifications:**
```yaml
Grid:
  mobile:
    columns: 4
    gutter: 16px
    margin: 16px
  
  tablet:
    columns: 8
    gutter: 24px
    margin: 32px
  
  desktop:
    columns: 12
    gutter: 32px
    margin: 40px
    max-width: 1280px
```

### Container Patterns

**Container Types:**
- **Fluid Container**: Full width, adapts to viewport
- **Fixed Container**: Maximum width with centered content
- **Constrained Container**: Maximum width with side padding

**Review Checklist:**
- [ ] Container type appropriate for content
- [ ] Maximum width prevents line length issues
- [ ] Side padding prevents edge collision on mobile
- [ ] Container breakpoints defined
- [ ] Nested containers handled appropriately

### Spacing Patterns

**Vertical Rhythm:**
- Consistent spacing between sections
- Larger spacing between major sections
- Smaller spacing between related elements
- Spacing scales with heading hierarchy

**Review Checklist:**
- [ ] Vertical spacing consistent throughout design
- [ ] Related content grouped with less spacing
- [ ] Unrelated content separated with more spacing
- [ ] Spacing follows spacing token system
- [ ] Spacing adapts at different breakpoints

---

## Navigation Patterns

### Primary Navigation

**Common Patterns:**
- **Horizontal Navigation Bar**: Links in header, always visible
- **Hamburger Menu**: Mobile menu, expandable sidebar
- **Tab Navigation**: Content switching within a page
- **Sidebar Navigation**: Persistent navigation on left/right

**Review Checklist:**
- [ ] Current location clearly indicated (active state)
- [ ] Navigation structure logical and intuitive
- [ ] Mobile navigation appropriate (hamburger, bottom nav)
- [ ] Dropdown menus keyboard accessible
- [ ] Logo/home link in top-left corner
- [ ] Important actions accessible (login, CTA)
- [ ] Sticky navigation if scrolling long pages

### Breadcrumb Navigation

**Review Checklist:**
- [ ] Breadcrumbs show hierarchy, not history
- [ ] Current page shown but not clickable
- [ ] Separators clear (/, >, chevron)
- [ ] Truncation handled for long paths
- [ ] Positioned above page content
- [ ] Links to all ancestor pages

### Pagination

**Patterns:**
- **Numbered Pages**: 1, 2, 3, 4, 5
- **Previous/Next**: Simple navigation
- **Load More**: Button to fetch more results
- **Infinite Scroll**: Automatic loading

**Review Checklist:**
- [ ] Current page clearly indicated
- [ ] Previous/Next buttons always available
- [ ] Total pages visible
- [ ] Jump to page functionality for long lists
- [ ] Loading state designed
- [ ] Results count shown

---

## Form Patterns

### Form Layout

**Patterns:**
- **Single Column**: Easiest to scan, recommended for most forms
- **Two Column**: For related fields (first name/last name)
- **Multi-step**: Complex forms broken into steps
- **Inline Editing**: Edit in place without form submission

**Review Checklist:**
- [ ] Single column layout preferred for simplicity
- [ ] Related fields grouped visually
- [ ] Required vs. optional fields clearly marked
- [ ] Field labels always visible (not placeholder-only)
- [ ] Logical tab order
- [ ] Clear submit button

### Form Validation

**Validation Timing:**
- **On Submit**: Validate when user submits form
- **On Blur**: Validate when user leaves field
- **On Change**: Real-time validation (passwords, usernames)
- **Progressive**: More validation as user progresses

**Review Checklist:**
- [ ] Inline validation provides immediate feedback
- [ ] Error messages specific and actionable
- [ ] Error styling clear (red border, error icon)
- [ ] Success validation shown (green checkmark)
- [ ] Error summary at top of form
- [ ] Focus moves to first error field
- [ ] Required field validation clear

### Form States

**Review Checklist:**
- [ ] Empty state (placeholders, helper text)
- [ ] Filling state (focus indicators, inline validation)
- [ ] Submitting state (loading spinner, disabled submit)
- [ ] Success state (confirmation message)
- [ ] Error state (error messages, red styling)
- [ ] Disabled state (when form unavailable)

---

## Feedback Patterns

### Toast/Snackbar Notifications

**Properties:**
- **Duration**: 3-5 seconds typically
- **Position**: Top-right, bottom-center, or top-center
- **Types**: Success, Error, Warning, Info
- **Actions**: Optional close button or action button

**Review Checklist:**
- [ ] Icon indicates message type (✓ success, ✕ error)
- [ ] Color coding semantic (green success, red error)
- [ ] Message concise and understandable
- [ ] Positioned to not block critical content
- [ ] Dismissible with close button
- [ ] Auto-dismiss after appropriate duration
- [ ] Multiple toasts stack appropriately

### Loading States

**Patterns:**
- **Spinner**: Indeterminate loading (circular or linear)
- **Skeleton Screens**: Content placeholders showing layout
- **Progress Bar**: Determinate loading with percentage
- **Optimistic UI**: Show expected result immediately

**Review Checklist:**
- [ ] Loading indicator shown for operations > 1 second
- [ ] Loading message provides context ("Loading users...")
- [ ] Skeleton screens match final content layout
- [ ] Progress bar shows accurate progress
- [ ] Cancel option for long operations
- [ ] Spinner appropriate size and position

### Empty States

**Elements:**
- Illustration or icon
- Headline explaining emptiness
- Description text
- Primary action (CTA)

**Review Checklist:**
- [ ] Friendly and helpful tone
- [ ] Illustration appropriate and not distracting
- [ ] Clear explanation of why state is empty
- [ ] Action provided to populate (if applicable)
- [ ] Helpful suggestions provided

### Error States

**Review Checklist:**
- [ ] Error message explains what went wrong
- [ ] Error message explains how to fix it
- [ ] Technical details hidden (shown in details link)
- [ ] Error styling clear (red color, error icon)
- [ ] Retry action provided if applicable
- [ ] Support contact information for critical errors

---

## Data Display Patterns

### Table Pattern

**Elements:**
- Table header
- Sortable columns (with indicators)
- Row selection (checkboxes)
- Row actions (buttons/icons)
- Pagination
- Search/filter

**Review Checklist:**
- [ ] Column headers clear and concise
- [ ] Sortable columns have indicators
- [ ] Zebra striping or row borders improve scannability
- [ ] Hover state on rows provides feedback
- [ ] Sticky header for long tables
- [ ] Responsive behavior defined (card view on mobile)
- [ ] Row actions accessible and clear
- [ ] Pagination visible and functional
- [ ] Loading state for async data

### List Pattern

**Types:**
- **Simple List**: Text items only
- **List with Avatars**: User lists, contact lists
- **List with Actions**: Buttons or icons per item
- **List with Sections**: Grouped lists with headers

**Review Checklist:**
- [ ] List items have adequate spacing
- [ ] Clickable lists show hover state
- [ ] Selected item clearly indicated
- [ ] Loading skeleton matches list layout
- [ ] Empty state designed
- [ ] Section headers distinct from items

### Chart/Graph Pattern

**Common Charts:**
- Line chart (trends over time)
- Bar chart (comparisons)
- Pie/donut chart (proportions)
- Area chart (cumulative values)

**Review Checklist:**
- [ ] Chart type appropriate for data
- [ ] Axes labeled clearly
- [ ] Legend provided if needed
- [ ] Colors distinguishable (accessible)
- [ ] Interactive tooltips on hover
- [ ] Responsive behavior defined
- [ ] Loading state designed
- [ ] No data state designed

---

## Design System Governance

### Component Approval Process

**Stages:**
1. **Proposal**: Designer proposes new component or variant
2. **Review**: Design system team reviews for necessity
3. **Design**: Component designed with all states and variants
4. **Documentation**: Usage guidelines written
5. **Implementation**: Component built in code
6. **Release**: Component added to library

**Review Checklist:**
- [ ] Clear process for proposing new components
- [ ] Design system team reviews proposals
- [ ] Documentation written before release
- [ ] Component tested across platforms
- [ ] Versioning strategy defined

### Documentation Standards

**Required Documentation:**
- **Component Name**: Clear, descriptive name
- **Description**: What the component does and when to use it
- **Variants**: All size/style variants documented
- **States**: All interactive states shown
- **Props/Options**: Configurable properties listed
- **Usage Guidelines**: Do's and don'ts with examples
- **Accessibility Notes**: Keyboard support, ARIA labels
- **Code Examples**: Implementation examples

**Review Checklist:**
- [ ] Complete documentation for all components
- [ ] Visual examples provided
- [ ] Code examples provided
- [ ] Do's and don'ts section
- [ ] Accessibility guidance included
- [ ] Related components linked
- [ ] Version history tracked

### Version Management

**Versioning Strategy:**
- **Major Version** (1.0 → 2.0): Breaking changes
- **Minor Version** (1.0 → 1.1): New features, backward compatible
- **Patch Version** (1.0.0 → 1.0.1): Bug fixes

**Review Checklist:**
- [ ] Version number follows semantic versioning
- [ ] Breaking changes clearly documented
- [ ] Migration guide provided for major versions
- [ ] Changelog maintained
- [ ] Deprecated components marked clearly
- [ ] Sunset timeline communicated

### Design Token Management

**Governance:**
- Centralized token repository
- Token naming conventions enforced
- Token change request process
- Impact analysis before token changes
- Versioning for token libraries

**Review Checklist:**
- [ ] All colors, spacing, typography defined as tokens
- [ ] Token naming follows conventions
- [ ] New tokens justified (not duplicating existing)
- [ ] Token changes impact assessed
- [ ] Dark mode tokens maintained in sync

### Component Library Maintenance

**Activities:**
- Regular audits for unused components
- Deprecation of redundant components
- Updates for accessibility improvements
- Performance optimizations
- Platform consistency checks

**Review Checklist:**
- [ ] Regular component audits scheduled
- [ ] Unused components identified and removed
- [ ] Accessibility continuously improved
- [ ] Performance monitored
- [ ] Cross-platform consistency maintained

---

## Design System Review Checklist

Use this comprehensive checklist when reviewing design system compliance:

### Component Library
- [ ] Components follow design system patterns
- [ ] No duplicate components created unnecessarily
- [ ] Component variants purposeful and limited
- [ ] All component states designed
- [ ] Component naming consistent with system
- [ ] New components justified and documented

### Design Tokens
- [ ] Colors reference tokens, not hardcoded values
- [ ] Spacing uses token values consistently
- [ ] Typography references token values
- [ ] Border radius, shadows use tokens
- [ ] Semantic token naming followed
- [ ] No one-off values created outside token system

### Documentation
- [ ] New components fully documented
- [ ] Usage guidelines clear and helpful
- [ ] Do's and don'ts provided
- [ ] Accessibility notes included
- [ ] Code examples provided
- [ ] Related components cross-referenced

### Accessibility
- [ ] WCAG AA compliance verified
- [ ] Keyboard navigation supported
- [ ] Focus indicators visible
- [ ] Color contrast sufficient
- [ ] Semantic HTML used
- [ ] ARIA labels provided where needed

### Consistency
- [ ] Visual consistency across all screens
- [ ] Interaction patterns consistent
- [ ] Terminology consistent
- [ ] Spacing consistent
- [ ] Color usage semantic and consistent
- [ ] Typography hierarchy consistent

### Governance
- [ ] Changes follow approval process
- [ ] Breaking changes documented
- [ ] Version numbers appropriate
- [ ] Deprecations communicated
- [ ] Migration guides provided
- [ ] Impact assessment completed

---

## Summary

Design system patterns ensure consistency, efficiency, and quality across products. When reviewing designs:

1. **Verify Token Usage**: Ensure all visual attributes use design tokens
2. **Check Component Compliance**: Confirm components follow established patterns
3. **Validate States**: Ensure all interaction states are designed
4. **Review Documentation**: Verify thorough documentation exists
5. **Assess Governance**: Confirm proper approval and versioning processes
6. **Maintain Consistency**: Ensure patterns used consistently across designs

A well-governed design system accelerates design and development while ensuring consistent, high-quality user experiences.
