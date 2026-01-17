# Component Patterns


### Forms Best Practices

1. **Input Design**
   - Clear labels above inputs
   - Helper text below inputs
   - Inline validation (as user types or on blur)
   - Clear error messages
   - Appropriate input types (email, tel, number)

2. **Form Layout**
   - Single column for better completion rates
   - Group related fields
   - Logical field order
   - Smart defaults and autofill
   - Progress indicators for multi-step forms

3. **Validation**
   - Real-time validation (after field blur)
   - Clear error messages
   - Highlight invalid fields
   - Disable submit until valid (optional)
   - Success feedback on submission

### Modal/Dialog Patterns

1. **When to Use**
   - Require user decision
   - Display critical information
   - Capture user input without context switch
   - Confirm destructive actions

2. **Best Practices**
   - Focus management (trap focus in modal)
   - Close on overlay click or Escape key
   - Disable background scroll
   - Clear close button
   - Descriptive title
   - Primary and secondary actions

### Navigation Patterns

1. **Top Navigation**
   - Best for: 5-7 main sections
   - Global navigation
   - Always visible
   - Horizontal layout

2. **Sidebar Navigation**
   - Best for: Many navigation items
   - Hierarchical content
   - Collapsible for space
   - Vertical layout

3. **Bottom Tab Navigation (Mobile)**
   - Best for: 3-5 primary sections
   - Quick switching between sections
   - Always visible
   - Touch-optimized

4. **Hamburger Menu**
   - Use sparingly (reduces discoverability)
   - Best for: Secondary navigation
   - Mobile-friendly
   - Animate open/close
