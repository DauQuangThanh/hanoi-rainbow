# Design Review Report Template

Use this comprehensive template for frontend design review reports.

## Report Structure

```markdown
# Frontend Design Review Report

## Executive Summary

**Project**: [Project Name]  
**Review Date**: [Date]  
**Reviewer**: [Reviewer Name/Team]  
**Design Files**: [Links to Figma/Sketch/Adobe XD files]  
**Overall Assessment**: [Brief 2-3 sentence summary of design quality]

**Key Statistics:**
- Total screens reviewed: [number]
- Critical issues found: [number]
- High priority issues: [number]
- Medium priority issues: [number]
- Low priority issues: [number]
- Accessibility violations: [number]

**Summary**: [1-2 paragraph overview of review findings, major concerns, and recommendations]

---

## Review Scope

### Included in Review
- [List of screens/flows reviewed]
- [Design components evaluated]
- [Specific aspects covered: accessibility, responsive design, etc.]

### Excluded from Review
- [Items not included in scope]
- [Future work to be reviewed separately]

### Review Focus Areas
- [ ] UI/UX Design Quality
- [ ] Design System Consistency
- [ ] Accessibility Compliance (WCAG 2.1 AA)
- [ ] Responsive Design Patterns
- [ ] Component Architecture
- [ ] Visual Design Consistency

---

## Findings Summary

### Critical Issues (Blocking)
[Issues that must be fixed before launch]

| ID | Issue | Screen/Component | Severity | Impact |
|----|-------|------------------|----------|---------|
| C-01 | [Issue description] | [Location] | Critical | [Impact description] |

### High Priority Issues
[Issues that significantly impact usability or accessibility]

| ID | Issue | Screen/Component | Severity | Impact |
|----|-------|------------------|----------|---------|
| H-01 | [Issue description] | [Location] | High | [Impact description] |

### Medium Priority Issues
[Issues that affect experience but aren't blocking]

| ID | Issue | Screen/Component | Severity | Impact |
|----|-------|------------------|----------|---------|
| M-01 | [Issue description] | [Location] | Medium | [Impact description] |

### Low Priority Issues
[Minor improvements and polish items]

| ID | Issue | Screen/Component | Severity | Impact |
|----|-------|------------------|----------|---------|
| L-01 | [Issue description] | [Location] | Low | [Impact description] |

---

## Detailed Findings

### 1. UI/UX Design Quality

#### Typography
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Finding 1 with specific examples]
- [Finding 2 with specific examples]

**Recommendations:**
- [Specific actionable recommendation]

#### Color System
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Finding 1 with contrast ratios]
- [Finding 2 with color usage issues]

**Recommendations:**
- [Specific actionable recommendation]

#### Layout & Spacing
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Finding 1]
- [Finding 2]

**Recommendations:**
- [Specific actionable recommendation]

#### Visual Hierarchy
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Finding 1]
- [Finding 2]

**Recommendations:**
- [Specific actionable recommendation]

### 2. Design System Consistency

#### Component Usage
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Deviations from design system]
- [Custom components that should use system components]

**Recommendations:**
- [Specific actionable recommendation]

#### Design Tokens
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Hard-coded values found]
- [Token misuse]

**Recommendations:**
- [Specific actionable recommendation]

### 3. Accessibility Compliance

#### WCAG 2.1 AA Compliance
**Status**: ✅ Compliant | ⚠️ Partial | ❌ Non-Compliant

**Color Contrast:**
- [Pass/Fail for each text element with ratios]
- Example: Body text on white: 4.8:1 ✅ (meets 4.5:1 requirement)

**Keyboard Navigation:**
- [Tab order issues]
- [Focus indicators missing/unclear]

**Screen Reader Support:**
- [Missing ARIA labels]
- [Semantic structure issues]

**Recommendations:**
- [Specific actionable accessibility fixes]

### 4. Responsive Design

#### Breakpoint Strategy
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Breakpoint analysis]
- [Content adaptation issues]

**Recommendations:**
- [Specific actionable recommendation]

#### Mobile Experience
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Touch target sizing issues]
- [Mobile-specific usability problems]

**Recommendations:**
- [Specific actionable recommendation]

### 5. Component Architecture

#### Component Design
**Status**: ✅ Pass | ⚠️ Issues Found | ❌ Major Issues

**Findings:**
- [Component reusability issues]
- [Missing component states]
- [Variant design problems]

**Recommendations:**
- [Specific actionable recommendation]

---

## Positive Observations

[Highlight what's working well in the design]

- ✅ **[Strength 1]**: [Description of what's done well]
- ✅ **[Strength 2]**: [Description of what's done well]
- ✅ **[Strength 3]**: [Description of what's done well]

---

## Recommendations Summary

### Immediate Actions (Before Development)
1. **[Critical fix 1]**
   - Impact: [Description]
   - Effort: [Low/Medium/High]
   - Owner: [Design/Development]

2. **[Critical fix 2]**
   - Impact: [Description]
   - Effort: [Low/Medium/High]
   - Owner: [Design/Development]

### Short-Term Improvements (Next Sprint)
1. **[High priority improvement 1]**
   - Impact: [Description]
   - Effort: [Low/Medium/High]

2. **[High priority improvement 2]**
   - Impact: [Description]
   - Effort: [Low/Medium/High]

### Long-Term Enhancements (Backlog)
1. **[Medium/Low priority item 1]**
   - Impact: [Description]
   - Effort: [Low/Medium/High]

---

## Action Items

| Priority | Action | Owner | Due Date | Status |
|----------|--------|-------|----------|--------|
| Critical | [Action item] | [Name] | [Date] | Not Started |
| High | [Action item] | [Name] | [Date] | Not Started |
| Medium | [Action item] | [Name] | [Date] | Not Started |

---

## Next Steps

1. **Design Team Actions:**
   - [Specific design revisions needed]
   - [Design system updates required]

2. **Development Team Actions:**
   - [Implementation considerations]
   - [Technical feasibility review needed]

3. **Stakeholder Review:**
   - [Items requiring business/stakeholder input]
   - [Decision points needed]

4. **Follow-up Review:**
   - Schedule: [Date for re-review]
   - Focus areas: [Specific areas to re-evaluate]

---

## Appendix

### A. Severity Level Definitions

**Critical**: Issues that must be fixed before launch. Block user tasks or violate legal requirements (accessibility laws).

**High**: Significant usability or accessibility issues that severely degrade experience but don't completely block tasks.

**Medium**: Issues that negatively impact user experience but have workarounds. Should be fixed soon.

**Low**: Minor polish items, nice-to-haves, and optimizations that can be addressed over time.

### B. Accessibility Standards Reference

- **WCAG 2.1 AA**: Web Content Accessibility Guidelines Level AA
- **Minimum Contrast Ratios**: 
  - Normal text: 4.5:1
  - Large text (18pt+ or 14pt+ bold): 3:1
  - UI components: 3:1
- **Minimum Touch Targets**: 44x44 CSS pixels

### C. Design Files & Resources

- Design files: [Links]
- Design system documentation: [Links]
- Brand guidelines: [Links]
- Previous review reports: [Links]

### D. Review Methodology

**Tools Used:**
- Figma/Sketch inspection
- Color contrast checkers (e.g., WebAIM, Stark)
- Accessibility evaluation (manual inspection)
- Responsive design analysis

**Standards Applied:**
- WCAG 2.1 AA
- Company design system guidelines
- Platform-specific design guidelines (iOS HIG, Material Design, etc.)
- Industry best practices
```

## Usage Notes

**When to use this template:**
- Formal design reviews before development
- Accessibility audits
- Design system compliance checks
- Pre-release design validation

**Customization:**
- Remove sections not relevant to your review scope
- Add project-specific sections as needed
- Adjust severity levels to match your team's definitions
- Include visual examples/screenshots where helpful

**Report delivery tips:**
- Export as PDF for formal documentation
- Create Figma/Sketch comments linking to specific findings
- Schedule walkthrough meeting to discuss findings
- Prioritize findings collaboratively with team
