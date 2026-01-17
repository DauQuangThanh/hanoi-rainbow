# Accessibility Guidelines Reference

This document provides comprehensive guidance on evaluating web accessibility during frontend design reviews, covering WCAG 2.1 AA compliance, assistive technology support, and inclusive design principles.

## Table of Contents

1. [WCAG 2.1 Principles](#wcag-21-principles)
2. [Perceivable Guidelines](#perceivable-guidelines)
3. [Operable Guidelines](#operable-guidelines)
4. [Understandable Guidelines](#understandable-guidelines)
5. [Robust Guidelines](#robust-guidelines)
6. [Common Accessibility Issues](#common-accessibility-issues)
7. [Testing Accessibility](#testing-accessibility)
8. [Assistive Technology Support](#assistive-technology-support)

---

## WCAG 2.1 Principles

The Web Content Accessibility Guidelines (WCAG) 2.1 are organized around four principles, often abbreviated as **POUR**:

### 1. **Perceivable**
Information and user interface components must be presentable to users in ways they can perceive.

### 2. **Operable**
User interface components and navigation must be operable.

### 3. **Understandable**
Information and the operation of the user interface must be understandable.

### 4. **Robust**
Content must be robust enough to be interpreted reliably by a wide variety of user agents, including assistive technologies.

### Conformance Levels

- **Level A**: Minimum level of conformance
- **Level AA**: Recommended level (addresses most common barriers)
- **Level AAA**: Highest level (not required for all content)

**Target for Most Projects**: WCAG 2.1 AA compliance

---

## Perceivable Guidelines

### 1.1 Text Alternatives (Level A)

**Guideline**: Provide text alternatives for non-text content.

#### 1.1.1 Non-text Content (Level A)

**Requirements:**
- All images must have appropriate alt text
- Decorative images must have empty alt attribute (`alt=""`)
- Functional images (buttons, links) must describe function
- Complex images (charts, diagrams) need detailed descriptions

**Review Checklist:**
- [ ] All informative images have descriptive alt text
- [ ] Alt text concise and meaningful (not "image of...")
- [ ] Decorative images marked with `alt=""`
- [ ] Icons paired with accessible labels
- [ ] Charts/graphs have text alternatives or data tables
- [ ] Logo alt text includes company name
- [ ] Image buttons describe action, not appearance

**Examples:**

✅ **Good:**
```html
<img src="profile.jpg" alt="Sarah Johnson, CEO">
<img src="chart.png" alt="Sales increased 25% from Q1 to Q2">
<img src="decorative-line.png" alt="">
<button><img src="search-icon.svg" alt="Search"></button>
```

❌ **Bad:**
```html
<img src="profile.jpg" alt="image">
<img src="chart.png" alt="chart">
<img src="decorative-line.png" alt="decorative line">
<button><img src="search-icon.svg" alt="magnifying glass icon"></button>
```

**Common Patterns:**

| Image Type | Alt Text Approach |
|------------|-------------------|
| Informative | Describe the information conveyed |
| Functional | Describe the action/function |
| Decorative | Use `alt=""` |
| Complex (charts) | Provide data table or long description |
| Text in images | Include the text verbatim |
| Logo | Company/product name |

### 1.3 Adaptable (Level A)

**Guideline**: Create content that can be presented in different ways without losing information.

#### 1.3.1 Info and Relationships (Level A)

**Requirements:**
- Information conveyed through visual layout must be available programmatically
- Use proper semantic HTML elements
- Form labels associated with inputs
- Headings properly nested (h1 → h2 → h3)

**Review Checklist:**
- [ ] Semantic HTML used (header, nav, main, article, section, aside, footer)
- [ ] Headings create logical document outline
- [ ] Lists use `<ul>`, `<ol>`, or `<dl>` elements
- [ ] Tables use `<th>` for headers with proper scope
- [ ] Form labels use `<label>` element with `for` attribute
- [ ] Related form fields grouped with `<fieldset>` and `<legend>`
- [ ] ARIA landmarks used when semantic HTML insufficient

#### 1.3.2 Meaningful Sequence (Level A)

**Requirements:**
- Reading order makes sense when linearized
- Tab order follows logical sequence
- CSS positioning doesn't break content order

**Review Checklist:**
- [ ] DOM order matches visual order
- [ ] Tab order logical and predictable
- [ ] CSS absolute/fixed positioning doesn't break flow
- [ ] Two-column layouts maintain logical reading order
- [ ] Modals/dialogs inserted at logical point in DOM

#### 1.3.3 Sensory Characteristics (Level A)

**Requirements:**
- Instructions don't rely solely on sensory characteristics
- Don't use only shape, size, location, or sound to convey information

**Review Checklist:**
- [ ] Instructions don't say "click the round button" (include text label)
- [ ] Don't say "see the sidebar on the right" (use heading or label)
- [ ] Color not the only indicator (use icons, text, patterns)
- [ ] Sound cues supplemented with visual alternatives

**Examples:**

❌ **Bad:**
- "Click the green button to continue"
- "Required fields are marked in red"
- "See the panel on the right for details"

✅ **Good:**
- "Click the green Continue button"
- "Required fields are marked with an asterisk (*) in red"
- "See the Related Articles section for details"

### 1.4 Distinguishable (Level AA)

**Guideline**: Make it easier for users to see and hear content.

#### 1.4.3 Contrast (Minimum) (Level AA)

**Requirements:**
- Normal text: Minimum 4.5:1 contrast ratio
- Large text (18pt+ or 14pt+ bold): Minimum 3:1 contrast ratio
- UI components and graphical objects: Minimum 3:1 contrast ratio

**Review Checklist:**
- [ ] Body text meets 4.5:1 contrast ratio
- [ ] Heading text meets 4.5:1 or 3:1 (if large)
- [ ] Link text meets 4.5:1 contrast ratio
- [ ] Button text meets 4.5:1 contrast ratio
- [ ] Button borders meet 3:1 contrast ratio
- [ ] Form field borders meet 3:1 contrast ratio
- [ ] Icon colors meet 3:1 contrast ratio
- [ ] Focus indicators meet 3:1 contrast ratio
- [ ] Disabled state text may have lower contrast (not required to meet ratio)

**Testing Tools:**
- Chrome DevTools: Inspect element → Contrast ratio shown in color picker
- WebAIM Contrast Checker: https://webaim.org/resources/contrastchecker/
- Figma plugins: Stark, A11y - Color Contrast Checker

**Common Issues:**
- Light gray text on white background (#999 on #FFF = 2.8:1 ❌)
- White text on light blue background
- Placeholder text too light
- Disabled buttons with insufficient contrast

**Color Contrast Quick Reference:**

| Foreground | Background | Ratio | AA Pass | AAA Pass |
|------------|------------|-------|---------|----------|
| #767676 | #FFFFFF | 4.54:1 | ✅ Normal | ❌ Normal |
| #595959 | #FFFFFF | 7.00:1 | ✅ Normal | ✅ Normal |
| #FFFFFF | #0066CC | 4.55:1 | ✅ Normal | ❌ Normal |
| #FFFFFF | #0052A3 | 6.00:1 | ✅ Normal | ✅ Normal |
| #000000 | #FFFF00 | 1.07:1 | ❌ | ❌ |
| #000000 | #FFD700 | 1.63:1 | ❌ | ❌ |

#### 1.4.4 Resize Text (Level AA)

**Requirements:**
- Text can be resized up to 200% without loss of content or functionality
- Users can zoom browser without horizontal scrolling (at 1280px viewport)

**Review Checklist:**
- [ ] Text sizes use relative units (rem, em, %)
- [ ] Layouts don't break at 200% zoom
- [ ] No horizontal scrolling at 200% zoom (1280px viewport)
- [ ] Touch targets remain accessible when zoomed
- [ ] Fixed pixel sizes avoided for text

#### 1.4.10 Reflow (Level AA, WCAG 2.1)

**Requirements:**
- Content reflows without horizontal scrolling at 320px width (mobile)
- Content reflows without vertical scrolling at 256px height (for horizontal content)

**Review Checklist:**
- [ ] No horizontal scrolling on mobile (320px width)
- [ ] Responsive design adapts to narrow viewports
- [ ] Data tables may scroll horizontally (exception allowed)
- [ ] Images scale appropriately

#### 1.4.11 Non-text Contrast (Level AA, WCAG 2.1)

**Requirements:**
- UI components must have 3:1 contrast against adjacent colors
- Graphical objects must have 3:1 contrast

**Review Checklist:**
- [ ] Button borders meet 3:1 contrast
- [ ] Form input borders meet 3:1 contrast
- [ ] Focus indicators meet 3:1 contrast
- [ ] Icon colors meet 3:1 contrast against background
- [ ] Chart elements distinguishable with 3:1 contrast
- [ ] Active/selected states meet 3:1 contrast

#### 1.4.12 Text Spacing (Level AA, WCAG 2.1)

**Requirements:**
- Content must be readable when user overrides spacing:
  - Line height: At least 1.5x font size
  - Paragraph spacing: At least 2x font size
  - Letter spacing: At least 0.12x font size
  - Word spacing: At least 0.16x font size

**Review Checklist:**
- [ ] Designs accommodate increased line height
- [ ] Text doesn't overlap with increased spacing
- [ ] Buttons/components don't break with increased spacing
- [ ] Content remains readable and functional

#### 1.4.13 Content on Hover or Focus (Level AA, WCAG 2.1)

**Requirements:**
- Tooltips and popovers must be:
  - **Dismissible**: Can be closed without moving pointer/focus
  - **Hoverable**: User can move pointer over tooltip content
  - **Persistent**: Remains visible until dismissed or no longer relevant

**Review Checklist:**
- [ ] Tooltips dismissible with Escape key
- [ ] User can move mouse over tooltip without it disappearing
- [ ] Tooltip doesn't disappear on accidental mouse movement
- [ ] Focus-triggered content remains until focus moves
- [ ] Tooltip timeout sufficient (at least 5 seconds)

---

## Operable Guidelines

### 2.1 Keyboard Accessible (Level A)

**Guideline**: Make all functionality available from a keyboard.

#### 2.1.1 Keyboard (Level A)

**Requirements:**
- All interactive elements operable via keyboard
- No keyboard traps
- Keyboard shortcuts documented

**Review Checklist:**
- [ ] All buttons keyboard accessible (Enter/Space activates)
- [ ] All links keyboard accessible (Enter activates)
- [ ] Form fields keyboard accessible (Tab navigates, arrows select)
- [ ] Dropdowns keyboard accessible (arrows navigate options)
- [ ] Modals keyboard accessible (Tab cycles through, Escape closes)
- [ ] Custom controls have keyboard support
- [ ] No keyboard traps (can always escape)

**Common Keyboard Patterns:**

| Component | Keyboard Behavior |
|-----------|-------------------|
| Button | Enter or Space to activate |
| Link | Enter to follow |
| Checkbox | Space to toggle |
| Radio | Arrow keys to select, Tab to move to next group |
| Dropdown | Arrow keys to open and navigate, Enter to select |
| Modal | Tab/Shift+Tab to cycle, Escape to close |
| Tabs | Arrow keys to switch tabs, Tab to move to panel |
| Menu | Arrow keys to navigate, Enter to select, Escape to close |

#### 2.1.2 No Keyboard Trap (Level A)

**Requirements:**
- Focus can always move away from any component
- If focus trapped (modal), Escape key provides exit

**Review Checklist:**
- [ ] Can Tab out of all components
- [ ] Modals provide Escape key to close
- [ ] Infinite scrolling allows keyboard exit
- [ ] Media players allow keyboard escape
- [ ] Custom widgets allow focus to move away

#### 2.1.4 Character Key Shortcuts (Level A, WCAG 2.1)

**Requirements:**
- Single character shortcuts can be turned off, remapped, or only active when component focused

**Review Checklist:**
- [ ] Single-key shortcuts (like "s" for search) can be disabled
- [ ] Shortcuts require modifier key (Ctrl, Alt, Cmd)
- [ ] Shortcuts only active when component has focus
- [ ] Users can customize keyboard shortcuts

### 2.2 Enough Time (Level A)

**Guideline**: Provide users enough time to read and use content.

#### 2.2.1 Timing Adjustable (Level A)

**Requirements:**
- Users can turn off, adjust, or extend time limits
- Exceptions: Real-time events, essential timing, 20+ hour time limits

**Review Checklist:**
- [ ] Session timeouts can be extended (warning before expiration)
- [ ] Auto-advancing carousels can be paused
- [ ] Timed quizzes allow time extension
- [ ] Warning provided before timeout (at least 20 seconds to respond)

#### 2.2.2 Pause, Stop, Hide (Level A)

**Requirements:**
- Users can pause, stop, or hide moving, blinking, or auto-updating content

**Review Checklist:**
- [ ] Carousels have pause button
- [ ] Auto-playing videos have pause control
- [ ] Animations can be paused
- [ ] Auto-updating content can be paused
- [ ] No content blinks more than 3 times per second

### 2.3 Seizures and Physical Reactions (Level A)

**Guideline**: Do not design content that could cause seizures.

#### 2.3.1 Three Flashes or Below Threshold (Level A)

**Requirements:**
- No content flashes more than 3 times per second
- Or flashes are below general flash and red flash thresholds

**Review Checklist:**
- [ ] Animations don't flash rapidly
- [ ] GIFs checked for flash rate
- [ ] Strobe effects avoided
- [ ] Transitions smooth, not flashy

### 2.4 Navigable (Level AA)

**Guideline**: Provide ways to help users navigate, find content, and determine where they are.

#### 2.4.1 Bypass Blocks (Level A)

**Requirements:**
- Skip links to bypass repeated navigation
- Landmarks allow screen readers to jump to sections

**Review Checklist:**
- [ ] "Skip to main content" link at top of page
- [ ] Skip link visible on keyboard focus
- [ ] Landmarks defined (header, nav, main, aside, footer)
- [ ] Heading structure allows section navigation

#### 2.4.2 Page Titled (Level A)

**Requirements:**
- Every page has descriptive, unique title

**Review Checklist:**
- [ ] Page title describes page content
- [ ] Page title unique across site
- [ ] Page title format consistent (e.g., "Page Name | Site Name")
- [ ] Page title changes on SPA navigation

#### 2.4.3 Focus Order (Level A)

**Requirements:**
- Tab order follows logical, intuitive sequence

**Review Checklist:**
- [ ] Tab order matches visual order
- [ ] Tab order follows reading order (left-to-right, top-to-bottom)
- [ ] Hidden elements not in tab order
- [ ] Modal/dialog content in logical tab order
- [ ] Avoid positive tabindex values

#### 2.4.4 Link Purpose (In Context) (Level A)

**Requirements:**
- Purpose of each link clear from link text or context

**Review Checklist:**
- [ ] Link text describes destination ("Read more about accessibility" not "Click here")
- [ ] Duplicate link texts lead to same destination
- [ ] Icon-only links have aria-label
- [ ] Links distinct from surrounding text (underline, color + another indicator)

**Examples:**

❌ **Bad:**
```html
<a href="/article-1">Click here</a>
<a href="/article-2">Read more</a>
<a href="/docs"><img src="icon.svg" alt="icon"></a>
```

✅ **Good:**
```html
<a href="/article-1">Read our accessibility guide</a>
<a href="/article-2">Read more about WCAG 2.1</a>
<a href="/docs" aria-label="Documentation"><img src="icon.svg" aria-hidden="true"></a>
```

#### 2.4.5 Multiple Ways (Level AA)

**Requirements:**
- Multiple ways to locate pages (menu, search, sitemap)

**Review Checklist:**
- [ ] Primary navigation menu present
- [ ] Search functionality available
- [ ] Sitemap or breadcrumbs provided
- [ ] Related links on pages

#### 2.4.6 Headings and Labels (Level AA)

**Requirements:**
- Headings and labels descriptive

**Review Checklist:**
- [ ] Headings describe topic or purpose
- [ ] Form labels clearly describe field purpose
- [ ] Button labels describe action
- [ ] Section labels clear

#### 2.4.7 Focus Visible (Level AA)

**Requirements:**
- Keyboard focus indicator clearly visible

**Review Checklist:**
- [ ] Focus indicator visible on all interactive elements
- [ ] Focus indicator has sufficient contrast (3:1 against background)
- [ ] Focus indicator not removed with CSS (`outline: none` without alternative)
- [ ] Focus indicator visible against all backgrounds
- [ ] Focus indicator style consistent across site

**Common Focus Indicator Styles:**
- Outline (default browser style)
- Border change (thicker, different color)
- Background color change
- Glow/shadow effect
- Combination of above

**Minimum Requirements:**
- Visible change when element receives focus
- At least 3:1 contrast ratio against background
- Doesn't obscure the focused element

### 2.5 Input Modalities (Level AA, WCAG 2.1)

**Guideline**: Make it easier for users to operate functionality through various inputs.

#### 2.5.1 Pointer Gestures (Level A)

**Requirements:**
- All multipoint or path-based gestures have single-pointer alternative

**Review Checklist:**
- [ ] Pinch-to-zoom has +/- buttons alternative
- [ ] Swipe gestures have button alternatives
- [ ] Drag-and-drop has keyboard alternative
- [ ] Two-finger gestures have single-pointer alternative

#### 2.5.2 Pointer Cancellation (Level A)

**Requirements:**
- Actions triggered on up-event (not down-event)
- Allows user to abort action by moving away before release

**Review Checklist:**
- [ ] Click actions complete on mouseup (not mousedown)
- [ ] Touch actions complete on touchend (not touchstart)
- [ ] User can cancel by moving pointer away before release

#### 2.5.3 Label in Name (Level A)

**Requirements:**
- Visible label text included in accessible name

**Review Checklist:**
- [ ] Button visible text matches aria-label (if aria-label used)
- [ ] Link visible text matches aria-label (if aria-label used)
- [ ] Voice control users can activate by saying visible label

#### 2.5.4 Motion Actuation (Level A)

**Requirements:**
- Functions triggered by device motion (shake, tilt) have UI alternative

**Review Checklist:**
- [ ] Shake-to-undo has button alternative
- [ ] Tilt controls have button alternatives
- [ ] Motion can be disabled in settings

#### 2.5.5 Target Size (Level AAA, but recommended)

**Recommended (not AA):**
- Touch targets at least 44x44 CSS pixels

**Review Checklist:**
- [ ] Buttons at least 44x44px
- [ ] Links have adequate clickable area
- [ ] Form controls at least 44px tall
- [ ] Icon buttons at least 44x44px
- [ ] Spacing between targets prevents misclicks

---

## Understandable Guidelines

### 3.1 Readable (Level AA)

**Guideline**: Make text content readable and understandable.

#### 3.1.1 Language of Page (Level A)

**Requirements:**
- Default language of page programmatically determined

**Review Checklist:**
- [ ] `<html lang="en">` attribute set
- [ ] Correct language code used (en, es, fr, etc.)

#### 3.1.2 Language of Parts (Level AA)

**Requirements:**
- Language changes indicated programmatically

**Review Checklist:**
- [ ] Foreign phrases marked with `lang` attribute
- [ ] Example: `<span lang="fr">Bonjour</span>`

### 3.2 Predictable (Level AA)

**Guideline**: Make web pages appear and operate in predictable ways.

#### 3.2.1 On Focus (Level A)

**Requirements:**
- Focus doesn't trigger context change

**Review Checklist:**
- [ ] Focusing input doesn't submit form
- [ ] Focusing link doesn't navigate
- [ ] Focus doesn't open modals
- [ ] Focus doesn't change page content unexpectedly

#### 3.2.2 On Input (Level A)

**Requirements:**
- Changing input doesn't automatically trigger context change

**Review Checklist:**
- [ ] Typing in search doesn't auto-submit (wait for Enter or button)
- [ ] Selecting radio button doesn't auto-submit form
- [ ] Checkbox change doesn't navigate away
- [ ] Dropdown selection doesn't change page (unless warned)

#### 3.2.3 Consistent Navigation (Level AA)

**Requirements:**
- Navigation consistent across pages

**Review Checklist:**
- [ ] Navigation menu same location on all pages
- [ ] Navigation items same order across pages
- [ ] Breadcrumbs consistent across pages
- [ ] Footer links consistent

#### 3.2.4 Consistent Identification (Level AA)

**Requirements:**
- Components with same functionality labeled consistently

**Review Checklist:**
- [ ] Submit buttons always labeled "Submit" (not "Submit" on one page, "Send" on another)
- [ ] Icons used consistently (same icon = same function)
- [ ] Error messages formatted consistently

### 3.3 Input Assistance (Level AA)

**Guideline**: Help users avoid and correct mistakes.

#### 3.3.1 Error Identification (Level A)

**Requirements:**
- Input errors identified and described to user

**Review Checklist:**
- [ ] Required field errors clearly stated
- [ ] Invalid format errors explain expected format
- [ ] Errors announced to screen readers
- [ ] Error styling clear (not just color)

#### 3.3.2 Labels or Instructions (Level A)

**Requirements:**
- Labels or instructions provided for user input

**Review Checklist:**
- [ ] All form fields have labels
- [ ] Required fields clearly marked
- [ ] Expected format explained (e.g., "MM/DD/YYYY")
- [ ] Helpful examples provided
- [ ] Complex inputs have instructions

#### 3.3.3 Error Suggestion (Level AA)

**Requirements:**
- Suggestions provided for fixing input errors

**Review Checklist:**
- [ ] Error messages explain how to fix
- [ ] Suggestions specific and actionable
- [ ] Examples provided for correct format
- [ ] Alternative options suggested

**Examples:**

❌ **Bad:**
- "Invalid email"
- "Error in date field"

✅ **Good:**
- "Email address must include @ symbol. For example: user@example.com"
- "Date must be in MM/DD/YYYY format. For example: 12/31/2025"

#### 3.3.4 Error Prevention (Legal, Financial, Data) (Level AA)

**Requirements:**
- Submissions reversible, checked, or confirmed for legal, financial, or data transactions

**Review Checklist:**
- [ ] Confirmation page before finalizing purchase
- [ ] Review step before submitting important data
- [ ] Ability to edit before final submission
- [ ] Confirmation checkbox for critical actions
- [ ] Delete actions have confirmation dialog

---

## Robust Guidelines

### 4.1 Compatible (Level A)

**Guideline**: Maximize compatibility with current and future user agents, including assistive technologies.

#### 4.1.1 Parsing (Level A, deprecated in WCAG 2.2)

**Requirements:**
- HTML validates (no duplicate IDs, proper nesting)

**Review Checklist:**
- [ ] No duplicate IDs
- [ ] Elements properly nested
- [ ] Closing tags present
- [ ] Attributes quoted properly

#### 4.1.2 Name, Role, Value (Level A)

**Requirements:**
- UI components have accessible name, role, and state

**Review Checklist:**
- [ ] Custom controls have ARIA roles
- [ ] Interactive elements have accessible names
- [ ] State changes announced (checked, expanded, selected)
- [ ] Dynamic content updates announced

**ARIA Roles for Custom Controls:**

| Component | ARIA Role | States/Properties |
|-----------|-----------|-------------------|
| Tab panels | `role="tablist"`, `role="tab"`, `role="tabpanel"` | `aria-selected`, `aria-controls` |
| Accordion | `role="button"` or `<button>` | `aria-expanded`, `aria-controls` |
| Modal | `role="dialog"` | `aria-labelledby`, `aria-describedby`, `aria-modal` |
| Alert | `role="alert"` | Announces immediately |
| Tooltip | `role="tooltip"` | `aria-describedby` |
| Menu | `role="menu"`, `role="menuitem"` | `aria-haspopup`, `aria-expanded` |

#### 4.1.3 Status Messages (Level AA, WCAG 2.1)

**Requirements:**
- Status messages announced to screen readers without focus change

**Review Checklist:**
- [ ] Success messages use `role="status"` or `aria-live="polite"`
- [ ] Error messages use `role="alert"` or `aria-live="assertive"`
- [ ] Loading states announced
- [ ] Search results count announced
- [ ] Form submission feedback announced

**ARIA Live Regions:**

```html
<!-- Polite announcement (waits for pause) -->
<div role="status" aria-live="polite">
  3 results found
</div>

<!-- Assertive announcement (immediate) -->
<div role="alert" aria-live="assertive">
  Error: Form submission failed
</div>
```

---

## Common Accessibility Issues

### 1. Missing Alt Text
**Impact**: Screen reader users can't understand images  
**Fix**: Add descriptive alt text to all images

### 2. Low Color Contrast
**Impact**: Users with low vision can't read text  
**Fix**: Ensure 4.5:1 contrast for text, 3:1 for UI components

### 3. No Keyboard Access
**Impact**: Keyboard users can't interact with custom controls  
**Fix**: Make all interactive elements keyboard accessible

### 4. Missing Focus Indicators
**Impact**: Keyboard users can't see where focus is  
**Fix**: Ensure visible focus indicators on all interactive elements

### 5. No Form Labels
**Impact**: Screen reader users don't know field purpose  
**Fix**: Use `<label>` element for all form inputs

### 6. Poor Heading Structure
**Impact**: Screen reader users can't navigate by headings  
**Fix**: Use proper heading hierarchy (h1 → h2 → h3)

### 7. Non-descriptive Link Text
**Impact**: Screen reader users hear "click here" without context  
**Fix**: Use descriptive link text that explains destination

### 8. Auto-playing Media
**Impact**: Distracts users, especially those with cognitive disabilities  
**Fix**: Don't auto-play, or provide pause control

### 9. Tiny Touch Targets
**Impact**: Mobile users, especially those with motor disabilities, can't tap accurately  
**Fix**: Ensure 44x44px minimum touch target size

### 10. Inaccessible Modals
**Impact**: Keyboard users trapped, screen reader users confused  
**Fix**: Trap focus within modal, provide Escape key to close

---

## Testing Accessibility

### Automated Testing Tools

**Browser Extensions:**
- **axe DevTools**: Comprehensive automated testing
- **WAVE**: Visual feedback on accessibility issues
- **Lighthouse**: Built into Chrome DevTools

**Limitations:**
- Automated tools catch only ~30% of issues
- Manual testing required for full coverage

### Manual Testing

**Keyboard Testing:**
1. Unplug mouse
2. Tab through entire page
3. Verify all interactive elements accessible
4. Check focus indicators visible
5. Ensure no keyboard traps

**Screen Reader Testing:**
- **macOS**: VoiceOver (Cmd+F5)
- **Windows**: NVDA (free) or JAWS
- **Mobile**: iOS VoiceOver, Android TalkBack

**Screen Reader Test Checklist:**
- [ ] All content announced
- [ ] Images have alt text
- [ ] Headings navigable
- [ ] Landmarks present
- [ ] Form labels associated
- [ ] Buttons/links clearly identified
- [ ] Dynamic content updates announced

### Color Contrast Testing

**Tools:**
- WebAIM Contrast Checker
- Chrome DevTools color picker
- Figma plugins (Stark, A11y - Color Contrast Checker)

**Process:**
1. Check all text against background
2. Check all UI components against adjacent colors
3. Check focus indicators
4. Test in dark mode (if applicable)

### Zoom Testing

**Process:**
1. Zoom browser to 200%
2. Verify no horizontal scrolling
3. Verify content remains readable
4. Verify interactive elements remain accessible

### Mobile Accessibility Testing

**Process:**
1. Test on real devices (iOS, Android)
2. Enable screen reader (VoiceOver, TalkBack)
3. Verify touch target sizes (44x44px minimum)
4. Test landscape and portrait orientations
5. Test with assistive touch enabled

---

## Assistive Technology Support

### Screen Readers

**Popular Screen Readers:**
- **NVDA** (Windows, free)
- **JAWS** (Windows, commercial)
- **VoiceOver** (macOS, iOS, built-in)
- **TalkBack** (Android, built-in)
- **Narrator** (Windows, built-in)

**Screen Reader Considerations:**
- Semantic HTML crucial for proper announcement
- ARIA labels supplement visual labels
- Live regions announce dynamic content
- Focus management critical for SPAs

### Voice Control

**Voice Control Software:**
- Dragon NaturallySpeaking
- iOS Voice Control
- Android Voice Access

**Voice Control Considerations:**
- Visible labels match accessible names
- Interactive elements labeled clearly
- Links and buttons distinguishable
- Form fields clearly labeled

### Switch Control

**Switch Control Users:**
- Users with motor disabilities
- May use single switch, sip-and-puff, head pointer

**Switch Control Considerations:**
- All functionality keyboard accessible
- No time limits or can be extended
- Grouped actions accessible
- Large touch targets

### Screen Magnification

**Screen Magnifier Software:**
- ZoomText (Windows)
- macOS Zoom
- Windows Magnifier

**Screen Magnification Considerations:**
- Content doesn't break when zoomed
- Focus indicators visible when magnified
- Important info not hidden off-screen
- Horizontal scrolling minimized

---

## Accessibility Review Checklist Summary

### Critical (Must Fix)

- [ ] **Color Contrast**: All text meets 4.5:1 (normal) or 3:1 (large), UI components meet 3:1
- [ ] **Keyboard Access**: All interactive elements keyboard accessible, no keyboard traps
- [ ] **Focus Indicators**: Visible focus indicators on all interactive elements (3:1 contrast)
- [ ] **Alt Text**: All images have appropriate alt text or empty alt for decorative
- [ ] **Form Labels**: All form inputs have associated labels
- [ ] **Heading Structure**: Proper heading hierarchy (h1 → h2 → h3)
- [ ] **Touch Targets**: Minimum 44x44px on mobile

### High Priority (Should Fix)

- [ ] **Link Text**: Descriptive link text (not "click here")
- [ ] **Error Messages**: Specific, actionable error messages
- [ ] **ARIA Labels**: Custom controls have proper ARIA roles and labels
- [ ] **Skip Links**: Skip navigation link present
- [ ] **Page Titles**: Unique, descriptive page titles
- [ ] **Language**: HTML lang attribute set
- [ ] **Live Regions**: Dynamic content changes announced

### Medium Priority (Recommended)

- [ ] **Consistent Navigation**: Navigation consistent across pages
- [ ] **Landmarks**: Semantic HTML landmarks (header, nav, main, footer)
- [ ] **Button Labels**: Clear, action-oriented button labels
- [ ] **Text Spacing**: Designs accommodate increased text spacing
- [ ] **Reflow**: No horizontal scrolling at 320px width
- [ ] **Multiple Navigation**: Multiple ways to find content (menu, search, breadcrumbs)

---

## Resources

### Official Guidelines
- WCAG 2.1: https://www.w3.org/WAI/WCAG21/quickref/
- WAI-ARIA Practices: https://www.w3.org/WAI/ARIA/apg/

### Testing Tools
- axe DevTools: https://www.deque.com/axe/devtools/
- WAVE: https://wave.webaim.org/
- WebAIM Contrast Checker: https://webaim.org/resources/contrastchecker/

### Learning Resources
- WebAIM: https://webaim.org/
- A11y Project: https://www.a11yproject.com/
- Inclusive Components: https://inclusive-components.design/
