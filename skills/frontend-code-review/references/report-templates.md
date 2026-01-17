# Review Report Templates

Templates and examples for frontend code review reports.

## Table of Contents
- [Executive Summary Template](#executive-summary-template)
- [Detailed Review Report](#detailed-review-report)
- [Quick Checklist Report](#quick-checklist-report)
- [Issue Report Template](#issue-report-template)
- [Pull Request Review Template](#pull-request-review-template)
- [Security Audit Report](#security-audit-report)
- [Performance Audit Report](#performance-audit-report)
- [Accessibility Audit Report](#accessibility-audit-report)

---

## Executive Summary Template

```markdown
# Frontend Code Review - Executive Summary

**Project:** [Project Name]
**Review Date:** [Date]
**Reviewer:** [Name]
**Review Scope:** [Description of what was reviewed]

## Overall Assessment

**Rating:** ⭐⭐⭐⭐☆ (4/5)

### Summary
[2-3 sentence high-level summary of code quality and main findings]

### Key Findings

#### ✅ Strengths
- [Strength 1]
- [Strength 2]
- [Strength 3]

#### ⚠️ Areas for Improvement
- [Issue 1 - Priority: High/Medium/Low]
- [Issue 2 - Priority: High/Medium/Low]
- [Issue 3 - Priority: High/Medium/Low]

#### 🚨 Critical Issues
- [Critical issue 1]
- [Critical issue 2]

## Scores by Category

| Category | Score | Notes |
|----------|-------|-------|
| Code Quality | 8/10 | [Brief note] |
| Performance | 7/10 | [Brief note] |
| Accessibility | 6/10 | [Brief note] |
| Security | 9/10 | [Brief note] |
| Testing | 7/10 | [Brief note] |
| Documentation | 8/10 | [Brief note] |

## Recommendations

### Immediate Actions (Must Fix)
1. [Action 1]
2. [Action 2]

### Short-term Improvements (Should Fix)
1. [Action 1]
2. [Action 2]

### Long-term Enhancements (Nice to Have)
1. [Action 1]
2. [Action 2]

## Next Steps
[Description of recommended next steps and timeline]
```

---

## Detailed Review Report

```markdown
# Frontend Code Review - Detailed Report

**Project:** [Project Name]
**Codebase:** [Repository/Branch]
**Review Period:** [Start Date] - [End Date]
**Reviewer(s):** [Names]

---

## Table of Contents
1. [Code Quality](#code-quality)
2. [Performance](#performance)
3. [Accessibility](#accessibility)
4. [Security](#security)
5. [CSS/Styling](#cssstyling)
6. [Testing](#testing)
7. [Documentation](#documentation)
8. [Recommendations](#recommendations)

---

## 1. Code Quality

### Overview
[General assessment of code quality]

### Component Structure

#### ✅ Good Practices Observed
- **Single Responsibility:** Components like `UserAvatar`, `UserProfile` follow SRP well
- **Composition:** Good use of composition in `Card` component family
- **Props Design:** Type-safe props with clear interfaces

**Example:**
```typescript
// Good example found at: src/components/UserProfile.tsx
interface UserProfileProps {
  user: User;
  onUpdate: (user: User) => void;
}
```

#### ⚠️ Issues Found

**Issue 1: God Component**
- **Location:** `src/components/Dashboard.tsx` (lines 45-350)
- **Severity:** High
- **Description:** Dashboard component handles too many responsibilities
- **Impact:** Hard to maintain, test, and reuse
- **Recommendation:**
```typescript
// Split into smaller components:
- DashboardHeader
- DashboardStats
- DashboardChart
- DashboardActivity
```

**Issue 2: Prop Drilling**
- **Location:** `src/components/Layout.tsx` → `Sidebar.tsx` → `UserMenu.tsx`
- **Severity:** Medium
- **Description:** User state passed through 3 levels unnecessarily
- **Recommendation:** Use Context or state management library

### TypeScript Usage

#### ✅ Good Practices
- Strict mode enabled
- No use of `any` in core business logic
- Good use of discriminated unions

#### ⚠️ Issues
- 12 instances of `any` type in `src/utils/` directory
- Missing return types on several functions
- Inconsistent use of interfaces vs types

**Recommendations:**
```typescript
// Fix: Add explicit return types
function calculateTotal(items: Item[]): number {
  return items.reduce((sum, item) => sum + item.price, 0);
}

// Fix: Replace any with proper types
// Before:
function processData(data: any) { ... }

// After:
interface ApiResponse {
  data: Array<{ id: string; value: number }>;
  metadata: { timestamp: string };
}
function processData(data: ApiResponse) { ... }
```

### State Management

#### Analysis
- Using Redux Toolkit (good choice)
- Selectors properly memoized
- Actions well-typed

#### Issues
- Too much state in global store (UI state should be local)
- Some components accessing store directly instead of props

---

## 2. Performance

### Overview
**Current Performance:**
- LCP: 3.2s (⚠️ Should be < 2.5s)
- FID: 85ms (✅ Good)
- CLS: 0.08 (✅ Good)

### Bundle Size Analysis

```
Main bundle: 342 KB (gzipped: 98 KB)
Vendor bundle: 512 KB (gzipped: 156 KB) ⚠️ Too large

Largest dependencies:
- moment.js: 72 KB (recommended: replace with date-fns)
- lodash: 48 KB (recommended: import individual functions)
```

### Issues Found

**Issue 1: Large Bundle Size**
- **Severity:** High
- **Impact:** Slow initial load time
- **Files:** webpack.config.js, package.json
- **Recommendations:**
  1. Replace moment.js with date-fns (save ~50 KB)
  2. Use individual lodash imports (save ~30 KB)
  3. Implement code splitting for routes

**Issue 2: Unoptimized Images**
- **Location:** `/public/images/`
- **Severity:** Medium
- **Impact:** Slow LCP, unnecessary bandwidth
- **Recommendations:**
  1. Convert to WebP/AVIF formats
  2. Implement responsive images
  3. Add lazy loading for below-fold images

**Issue 3: No Virtualization for Long Lists**
- **Location:** `src/components/DataTable.tsx`
- **Severity:** Medium
- **Impact:** Poor performance with 1000+ rows
- **Recommendation:** Implement react-window or similar

### Rendering Performance

**Unnecessary Re-renders Found:**
- `ProductCard` re-renders on every parent update (line 45)
- Inline function creation in render (15 instances)

**Recommendations:**
```typescript
// Use React.memo
const ProductCard = React.memo(({ product }) => {
  return <Card>{product.name}</Card>;
});

// Use useCallback for handlers
const handleClick = useCallback(() => {
  updateProduct(product);
}, [product]);
```

---

## 3. Accessibility

### WCAG Compliance Level: **AA (Partial)**

### Automated Testing Results
- axe-core: 23 issues found
- Lighthouse: Accessibility score 78/100

### Critical Issues

**Issue 1: Missing Form Labels**
- **Location:** `src/components/SearchForm.tsx` (lines 34-42)
- **WCAG:** 3.3.2 (Level A)
- **Impact:** Screen readers cannot identify input purpose
- **Recommendation:**
```tsx
// Add proper labels
<label htmlFor="search-input">Search products</label>
<input 
  id="search-input"
  type="search"
  placeholder="Search..."
  aria-label="Search products"
/>
```

**Issue 2: Insufficient Color Contrast**
- **Location:** `src/styles/theme.css` (lines 23-45)
- **WCAG:** 1.4.3 (Level AA)
- **Contrast Ratio:** 3.2:1 (should be 4.5:1)
- **Recommendation:** Change secondary text color from `#999` to `#666`

**Issue 3: Keyboard Navigation Issues**
- **Location:** `src/components/Dropdown.tsx`
- **WCAG:** 2.1.1 (Level A)
- **Impact:** Cannot navigate dropdown with keyboard
- **Recommendation:** Implement arrow key navigation and Escape to close

### Summary by Category

| Category | Issues | Severity |
|----------|--------|----------|
| Keyboard Navigation | 8 | High |
| Color Contrast | 12 | Medium |
| Form Labels | 5 | High |
| ARIA Usage | 7 | Medium |
| Semantic HTML | 3 | Low |

---

## 4. Security

### Security Audit Level: **Good with some concerns**

### Findings

#### ✅ Good Security Practices
- Using HTTPS exclusively
- HttpOnly cookies for authentication
- Input sanitization with DOMPurify
- CSRF protection implemented

#### 🚨 Critical Security Issues

**Issue 1: XSS Vulnerability**
- **Location:** `src/components/UserComment.tsx` (line 67)
- **Severity:** Critical
- **CWE:** CWE-79 (Cross-site Scripting)
- **Description:**
```tsx
// Vulnerable code:
<div dangerouslySetInnerHTML={{ __html: comment }} />
```
- **Recommendation:**
```tsx
// Sanitize before rendering:
import DOMPurify from 'dompurify';

<div dangerouslySetInnerHTML={{ 
  __html: DOMPurify.sanitize(comment, { 
    ALLOWED_TAGS: ['b', 'i', 'em', 'strong', 'a'] 
  }) 
}} />
```

**Issue 2: Insecure Direct Object Reference**
- **Location:** `src/api/users.ts` (line 34)
- **Severity:** High
- **Description:** User ID from URL used directly without authorization check
- **Recommendation:** Add authorization check on server, validate on client

#### ⚠️ Medium Priority Issues

**Issue 3: Sensitive Data in Console Logs**
- **Location:** Multiple files
- **Severity:** Medium
- **Found in:**
  - `src/hooks/useAuth.ts` (line 45) - logs password
  - `src/utils/api.ts` (line 122) - logs API tokens
- **Recommendation:** Remove all console.logs with sensitive data, use proper logging library

### Dependency Vulnerabilities

```bash
npm audit results:
- 2 high severity vulnerabilities
- 5 moderate severity vulnerabilities

High severity:
1. Regular Expression Denial of Service in validator@13.5.2
   Fix: npm install validator@13.9.0
   
2. Prototype Pollution in lodash@4.17.20
   Fix: npm install lodash@4.17.21
```

---

## 5. CSS/Styling

### Overview
- Using CSS Modules (good choice)
- Responsive design implemented
- Some inconsistencies in approach

### Issues Found

**Issue 1: Inconsistent Styling Approach**
- **Severity:** Medium
- **Description:** Mix of inline styles, CSS Modules, and styled-components
- **Recommendation:** Standardize on one approach (CSS Modules preferred)

**Issue 2: Unused CSS**
- **Impact:** 45 KB of unused CSS in bundle
- **Files:** `global.css`, `legacy.css`
- **Recommendation:** Use PurgeCSS or remove unused files

**Issue 3: No Design System**
- **Impact:** Inconsistent spacing, colors, typography
- **Recommendation:** Create design tokens file

```css
/* design-tokens.css */
:root {
  /* Colors */
  --color-primary: #0066cc;
  --color-secondary: #6c757d;
  
  /* Spacing */
  --space-xs: 0.25rem;
  --space-sm: 0.5rem;
  --space-md: 1rem;
  --space-lg: 1.5rem;
  --space-xl: 2rem;
  
  /* Typography */
  --font-size-sm: 0.875rem;
  --font-size-base: 1rem;
  --font-size-lg: 1.25rem;
  --font-size-xl: 1.5rem;
}
```

---

## 6. Testing

### Test Coverage

```
Overall coverage: 68%

By category:
- Components: 72%
- Utils: 81%
- Hooks: 65%
- API clients: 45% ⚠️ Low
```

### Issues Found

**Issue 1: Low API Client Coverage**
- **Files:** `src/api/*.ts`
- **Current:** 45%
- **Target:** 80%+
- **Recommendation:** Add integration tests for API clients

**Issue 2: Missing E2E Tests**
- **Severity:** Medium
- **Impact:** Critical user flows not tested
- **Recommendation:** Add Playwright/Cypress tests for:
  1. Login flow
  2. Checkout flow
  3. User registration

**Issue 3: Incomplete Unit Tests**
- Several components have tests only for happy path
- Error cases not tested
- Edge cases missing

**Example:**
```typescript
// Add error case tests
describe('UserProfile', () => {
  it('should render user data', () => { ... }); // ✅ Exists
  
  it('should handle loading state', () => { ... }); // ❌ Missing
  it('should handle error state', () => { ... }); // ❌ Missing
  it('should handle missing user data', () => { ... }); // ❌ Missing
});
```

---

## 7. Documentation

### Code Documentation

#### ✅ Good Practices
- JSDoc comments on utility functions
- README with setup instructions
- Component props documented with TypeScript

#### ⚠️ Areas for Improvement

**Issue 1: Missing Complex Logic Documentation**
- `src/utils/calculations.ts` - complex algorithms not explained
- `src/hooks/useDataSync.ts` - intricate state management not documented

**Recommendation:**
```typescript
/**
 * Calculates the weighted average of product ratings based on
 * recency and user reputation.
 * 
 * Algorithm:
 * 1. Newer ratings (< 30 days) get 1.5x weight
 * 2. Ratings from verified users get 1.2x weight
 * 3. Final score normalized to 0-5 range
 * 
 * @param ratings - Array of rating objects
 * @returns Weighted average score (0-5)
 * @example
 * calculateWeightedRating([
 *   { score: 5, date: '2024-01-01', verified: true },
 *   { score: 3, date: '2023-06-01', verified: false }
 * ]) // Returns 4.2
 */
export function calculateWeightedRating(ratings: Rating[]): number {
  // Implementation
}
```

**Issue 2: No Architecture Documentation**
- Recommendation: Add `ARCHITECTURE.md` explaining:
  - Directory structure
  - State management approach
  - Data flow
  - Component patterns used

**Issue 3: Missing API Documentation**
- Recommendation: Document API client usage, error handling, authentication

---

## 8. Recommendations

### Immediate Actions (1-2 weeks)

#### Priority 1: Critical Security Issues
1. **Fix XSS vulnerability in UserComment component**
   - Estimated effort: 2 hours
   - Files: `src/components/UserComment.tsx`
   - Add DOMPurify sanitization

2. **Update vulnerable dependencies**
   - Estimated effort: 4 hours
   - Run `npm audit fix`
   - Test for breaking changes
   - Update lodash and validator

3. **Fix accessibility issues preventing keyboard navigation**
   - Estimated effort: 8 hours
   - Files: Dropdown, Modal, Form components
   - Implement proper keyboard event handlers

#### Priority 2: Performance Improvements
1. **Optimize bundle size**
   - Estimated effort: 1 day
   - Replace moment.js with date-fns
   - Use individual lodash imports
   - Implement code splitting

2. **Optimize images**
   - Estimated effort: 4 hours
   - Convert to WebP
   - Add responsive images
   - Implement lazy loading

### Short-term Improvements (1-2 months)

1. **Refactor Dashboard component**
   - Break into smaller components
   - Improve maintainability

2. **Increase test coverage to 80%+**
   - Add missing unit tests
   - Implement E2E tests for critical flows

3. **Create design system**
   - Define design tokens
   - Create component library documentation

4. **Improve accessibility to WCAG AA compliance**
   - Fix all color contrast issues
   - Add proper ARIA labels
   - Ensure all interactive elements are keyboard accessible

### Long-term Enhancements (3-6 months)

1. **Migrate to TypeScript strict mode**
2. **Implement automated accessibility testing in CI/CD**
3. **Set up performance monitoring**
4. **Create comprehensive component documentation**

---

## Appendix

### Files Reviewed
```
Total files reviewed: 127
- Components: 45 files
- Hooks: 12 files
- Utils: 18 files
- API clients: 8 files
- Styles: 32 files
- Tests: 12 files
```

### Review Methodology
- Manual code review
- Automated testing (ESLint, axe-core, Lighthouse)
- Performance profiling
- Security scanning
- Dependency audit

### Tools Used
- ESLint
- Prettier
- axe DevTools
- Chrome Lighthouse
- React DevTools Profiler
- npm audit
- Snyk

### Reviewer Notes
[Any additional context or notes for the development team]
```

---

## Quick Checklist Report

```markdown
# Frontend Code Review Checklist

**Project:** [Name]
**Date:** [Date]
**Reviewer:** [Name]

## Code Quality
- [ ] Components follow single responsibility principle
- [ ] No code duplication (DRY)
- [ ] TypeScript types properly defined (no `any`)
- [ ] Proper error handling implemented
- [ ] Code is readable and well-formatted
- [ ] No console.logs in production code

## Performance
- [ ] Bundle size optimized (< 250 KB)
- [ ] Images optimized (WebP, lazy loading)
- [ ] Code splitting implemented
- [ ] No unnecessary re-renders
- [ ] Large lists use virtualization
- [ ] LCP < 2.5s
- [ ] FID < 100ms
- [ ] CLS < 0.1

## Accessibility
- [ ] All interactive elements keyboard accessible
- [ ] Proper heading hierarchy
- [ ] Form labels present and associated
- [ ] ARIA attributes used correctly
- [ ] Color contrast meets WCAG AA (4.5:1)
- [ ] Alt text on images
- [ ] Focus indicators visible
- [ ] Screen reader tested

## Security
- [ ] No XSS vulnerabilities
- [ ] Input sanitized
- [ ] CSRF protection implemented
- [ ] No sensitive data in localStorage
- [ ] Dependencies up to date
- [ ] No SQL injection risks
- [ ] HTTPS enforced
- [ ] CSP headers configured

## Testing
- [ ] Unit tests present (coverage > 80%)
- [ ] Integration tests for critical flows
- [ ] E2E tests for main user journeys
- [ ] Edge cases tested
- [ ] Error cases tested

## Documentation
- [ ] README with setup instructions
- [ ] Complex logic documented
- [ ] API client usage documented
- [ ] Component props documented

## Overall Assessment
**Pass / Fail / Pass with Conditions**

**Critical Issues:** [Number]
**High Priority Issues:** [Number]
**Medium Priority Issues:** [Number]
**Low Priority Issues:** [Number]

**Notes:**
[Additional comments]
```

---

## Issue Report Template

```markdown
# Issue Report

**Issue ID:** FR-001
**Date Reported:** 2024-01-15
**Severity:** High
**Status:** Open

## Summary
[One-line description of the issue]

## Location
- **File:** `src/components/UserProfile.tsx`
- **Lines:** 45-67
- **Function/Component:** `UserProfile`

## Category
- [ ] Code Quality
- [x] Performance
- [ ] Accessibility
- [ ] Security
- [ ] Testing
- [ ] Documentation

## Description
[Detailed description of the issue]

## Current Behavior
```typescript
// Current problematic code
function UserProfile({ userId }) {
  const [user, setUser] = useState(null);
  
  useEffect(() => {
    fetch(`/api/users/${userId}`)
      .then(res => res.json())
      .then(setUser);
  }, []); // Missing userId dependency!
  
  return <div>{user?.name}</div>;
}
```

## Expected Behavior
[Description of what should happen instead]

## Impact
- **User Impact:** High - Profile may show wrong user data
- **Performance Impact:** None
- **Security Impact:** Medium - Can access wrong user data
- **Accessibility Impact:** None

## Recommendation
```typescript
// Fixed code
function UserProfile({ userId }) {
  const [user, setUser] = useState(null);
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(false);
  
  useEffect(() => {
    let cancelled = false;
    
    async function fetchUser() {
      setLoading(true);
      try {
        const response = await fetch(`/api/users/${userId}`);
        if (!response.ok) throw new Error('Failed to fetch');
        const data = await response.json();
        
        if (!cancelled) {
          setUser(data);
        }
      } catch (err) {
        if (!cancelled) {
          setError(err.message);
        }
      } finally {
        if (!cancelled) {
          setLoading(false);
        }
      }
    }
    
    fetchUser();
    
    return () => {
      cancelled = true;
    };
  }, [userId]); // Correct dependency
  
  if (loading) return <Spinner />;
  if (error) return <Error message={error} />;
  if (!user) return null;
  
  return <div>{user.name}</div>;
}
```

## Estimated Effort
**Time:** 2 hours
**Complexity:** Medium

## Related Issues
- FR-002: Similar issue in UserSettings component
- FR-015: General patterns for data fetching

## Notes
[Any additional context or information]
```

---

## Pull Request Review Template

```markdown
# Pull Request Review

**PR:** #123 - Add user authentication
**Author:** @developer
**Reviewer:** @reviewer
**Date:** 2024-01-15

## Summary
[Brief description of what this PR does]

## Review Checklist

### Code Quality
- [x] Code follows project style guide
- [x] No unnecessary code duplication
- [ ] ⚠️ TypeScript types could be more specific
- [x] Error handling is appropriate
- [x] Code is self-documenting

### Functionality
- [x] Feature works as described
- [x] No regressions in existing features
- [x] Edge cases handled
- [ ] ⚠️ Error states need improvement

### Testing
- [x] Unit tests added
- [x] Tests cover happy path
- [ ] ❌ Missing tests for error cases
- [ ] ❌ No E2E tests added

### Performance
- [x] No performance regressions
- [x] Bundle size increase acceptable (+12 KB)
- [x] No unnecessary re-renders

### Accessibility
- [x] Keyboard accessible
- [ ] ⚠️ Missing ARIA labels on icon buttons
- [x] Color contrast acceptable

### Security
- [x] No security vulnerabilities introduced
- [x] Input validation present
- [x] Authentication properly implemented

## Feedback

### 🚨 Must Fix (Blocking)

**1. Missing error case tests**
- **Location:** `src/hooks/useAuth.test.ts`
- **Issue:** Only happy path tested
- **Action:** Add tests for network errors, invalid credentials, expired tokens

**2. Missing ARIA labels**
- **Location:** `src/components/LoginForm.tsx` (lines 34, 56)
- **Issue:** Icon buttons not labeled for screen readers
```tsx
// Change this:
<button onClick={handleTogglePassword}>
  <EyeIcon />
</button>

// To this:
<button 
  onClick={handleTogglePassword}
  aria-label="Toggle password visibility"
>
  <EyeIcon aria-hidden="true" />
</button>
```

### ⚠️ Should Fix (Important)

**1. Type definitions too loose**
- **Location:** `src/types/auth.ts`
- **Current:**
```typescript
interface User {
  id: string;
  data: any; // Too loose!
}
```
- **Suggested:**
```typescript
interface User {
  id: string;
  email: string;
  name: string;
  role: 'admin' | 'user';
  createdAt: string;
}
```

**2. Error messages not user-friendly**
```typescript
// Current:
catch (error) {
  setError(error.message); // Technical error shown to user
}

// Suggested:
catch (error) {
  const userMessage = error.code === 'auth/wrong-password'
    ? 'Invalid email or password'
    : 'An error occurred. Please try again.';
  setError(userMessage);
}
```

### 💡 Nice to Have (Optional)

**1. Consider extracting validation logic**
```typescript
// Current: Validation inline in component
// Suggested: Extract to separate validator functions
export const validators = {
  email: (value: string) => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(value),
  password: (value: string) => value.length >= 8
};
```

**2. Add JSDoc comments for public APIs**

### ✅ Excellent Work

- Clean component structure
- Good use of TypeScript
- Proper error boundaries
- Well-organized file structure

## Decision

**Status:** ⏸️ Changes Requested

**Summary:**
Good implementation overall, but needs fixes for:
1. Missing error case tests (blocking)
2. Accessibility improvements (blocking)
3. Tighter TypeScript types (important)

Please address the "Must Fix" items and we can merge.

## Additional Comments
[Any other feedback or discussion points]
```

---

## Security Audit Report

```markdown
# Security Audit Report

**Project:** [Name]
**Audit Date:** 2024-01-15
**Auditor:** [Name]
**Scope:** Frontend application security review

## Executive Summary

**Overall Security Rating:** 🟡 Medium Risk

**Critical Issues:** 2
**High Priority:** 5
**Medium Priority:** 8
**Low Priority:** 3

## Findings by Category

### 1. Cross-Site Scripting (XSS)

#### 🚨 Critical: Unescaped User Content
- **CWE:** CWE-79
- **Location:** `src/components/UserComment.tsx:67`
- **Risk:** Attackers can inject malicious scripts
- **Proof of Concept:**
```javascript
// Malicious input:
const comment = '<img src=x onerror="alert(document.cookie)">';
```
- **Remediation:** Add DOMPurify sanitization

### 2. Authentication & Session Management

#### ⚠️ High: Token Storage in localStorage
- **Location:** `src/utils/auth.ts:23`
- **Risk:** Tokens accessible via XSS
- **Recommendation:** Use HttpOnly cookies

### 3. Data Exposure

#### ⚠️ Medium: Sensitive Data in Redux DevTools
- **Risk:** User data visible in browser tools
- **Recommendation:** Disable Redux DevTools in production

### 4. Dependencies

**Vulnerable Dependencies Found:** 7

| Package | Version | Severity | Fix Available |
|---------|---------|----------|---------------|
| lodash | 4.17.20 | High | 4.17.21 |
| validator | 13.5.2 | High | 13.9.0 |
| axios | 0.21.1 | Medium | 1.6.0 |

## Remediation Plan

### Immediate (24-48 hours)
1. Fix XSS vulnerabilities
2. Update vulnerable dependencies
3. Move tokens to HttpOnly cookies

### Short-term (1-2 weeks)
1. Implement CSP headers
2. Add rate limiting for API calls
3. Security headers audit

### Long-term (1-3 months)
1. Implement automated security scanning in CI/CD
2. Regular dependency audits
3. Security awareness training for team
```

---

## Performance Audit Report

```markdown
# Performance Audit Report

**Project:** [Name]
**Audit Date:** 2024-01-15
**Auditor:** [Name]

## Core Web Vitals

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| LCP | 3.2s | < 2.5s | ❌ Fail |
| FID | 85ms | < 100ms | ✅ Pass |
| CLS | 0.08 | < 0.1 | ✅ Pass |
| FCP | 1.9s | < 1.8s | ⚠️ Almost |
| TTFB | 420ms | < 600ms | ✅ Pass |

## Detailed Analysis

### LCP Issues (3.2s → Target: 2.5s)

**Root Causes:**
1. Large hero image (1.2 MB) - Add 800ms
2. Blocking JavaScript - Add 400ms
3. Slow server response - Add 200ms

**Recommendations:**
1. Optimize hero image (WebP, responsive)
2. Defer non-critical JavaScript
3. Implement CDN

**Expected Impact:** 3.2s → 2.1s

### Bundle Size Analysis

```
Current: 512 KB (gzipped: 156 KB)
Target: < 300 KB (gzipped: < 100 KB)

Breakdown:
- moment.js: 72 KB → Replace with date-fns (15 KB) - Save 57 KB
- lodash: 48 KB → Individual imports - Save 30 KB
- Unused code: 35 KB → Tree shaking - Save 35 KB

Total potential savings: 122 KB (23% reduction)
```

## Action Plan

### Priority 1: LCP Improvements
- [ ] Optimize images
- [ ] Implement code splitting
- [ ] Add CDN

**Expected Impact:** 🟢 LCP: 3.2s → 2.1s

### Priority 2: Bundle Size
- [ ] Replace moment.js
- [ ] Use individual lodash imports
- [ ] Enable tree shaking

**Expected Impact:** 🟢 Bundle: 512 KB → 390 KB
```

---

## Accessibility Audit Report

```markdown
# Accessibility Audit Report

**Project:** [Name]
**Audit Date:** 2024-01-15
**Auditor:** [Name]
**WCAG Version:** 2.1
**Target Level:** AA

## Compliance Summary

**Current Compliance:** 67% (AA Level)

| Success Criterion | Level | Status |
|-------------------|-------|--------|
| 1.1.1 Non-text Content | A | ⚠️ Partial |
| 1.3.1 Info and Relationships | A | ✅ Pass |
| 1.4.3 Contrast (Minimum) | AA | ❌ Fail |
| 2.1.1 Keyboard | A | ⚠️ Partial |
| 2.4.3 Focus Order | A | ✅ Pass |
| 3.3.2 Labels or Instructions | A | ❌ Fail |
| 4.1.2 Name, Role, Value | A | ⚠️ Partial |

## Critical Issues

### 1. Insufficient Color Contrast (1.4.3)
**Impact:** 12 instances
**Severity:** High

| Element | Location | Current | Required | Fix |
|---------|----------|---------|----------|-----|
| Secondary text | theme.css:23 | 3.2:1 | 4.5:1 | #999 → #666 |
| Button text | Button.tsx:45 | 2.8:1 | 4.5:1 | Adjust colors |

### 2. Missing Form Labels (3.3.2)
**Impact:** 5 forms
**Severity:** Critical

**Files:**
- SearchForm.tsx
- LoginForm.tsx
- ContactForm.tsx

### 3. Keyboard Navigation Issues (2.1.1)
**Impact:** 8 components
**Severity:** High

**Components:**
- Dropdown menu
- Modal dialog
- Tabs component

## Remediation Roadmap

### Phase 1: Critical Issues (Week 1-2)
- Fix color contrast
- Add form labels
- Implement keyboard navigation

**Expected:** 67% → 85% compliance

### Phase 2: Enhancement (Week 3-4)
- Add ARIA landmarks
- Improve screen reader support
- Add skip links

**Expected:** 85% → 95% compliance

### Phase 3: Polish (Month 2)
- User testing with assistive technology
- Documentation
- Automated testing in CI/CD

**Expected:** 95% → 100% AA compliance
```

---

## Notes

These templates can be customized based on:
- Project size and complexity
- Team preferences
- Stakeholder requirements
- Review frequency
- Specific focus areas

Remember to:
1. Be specific with locations and line numbers
2. Provide code examples for issues and fixes
3. Include severity and impact assessments
4. Give actionable recommendations
5. Set realistic timelines
6. Follow up on previous reviews
