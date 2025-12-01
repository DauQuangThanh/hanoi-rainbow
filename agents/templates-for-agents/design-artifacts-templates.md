# Design Artifacts Templates

## User Persona Template

**Name**: [User Name / Persona Name]
**Age**: [Age range]
**Role/Job Title**: [Occupation]

### Demographics

- **Location**: [City/Country]
- **Education**: [Education level]
- **Tech Savviness**: [Low/Medium/High]

### Goals

- [Primary goal 1]
- [Primary goal 2]
- [Primary goal 3]

### Pain Points / Frustrations

- [Pain point 1]
- [Pain point 2]
- [Pain point 3]

### Behaviors

- [Key behavior 1]
- [Key behavior 2]
- [Usage pattern]

### Motivations

- [What drives them]
- [What they value]

### Quote
>
> "[A characteristic quote that captures their attitude or need]"

### Devices/Platforms

- Primary: [Device/Platform]
- Secondary: [Device/Platform]

---

## User Flow Template

**Flow Name**: [Name of the user journey]
**User Goal**: [What the user wants to accomplish]

### Steps

1. **Entry Point**: [Where user starts - homepage, email link, etc.]
   - Trigger: [What brings them here]

2. **[Action/Screen Name]**
   - User Action: [What they do]
   - System Response: [What happens]
   - Decision Point: [Any choices they need to make]

3. **[Action/Screen Name]**
   - User Action: [What they do]
   - System Response: [What happens]
   - Alternative Path: [If they go a different way]

4. **Outcome/Exit Point**: [Success state or where they end up]
   - Success Criteria: [What indicates success]
   - Error Handling: [What happens if something goes wrong]

### Alternative Flows

- **If [condition]**: [Alternative path]
- **If [condition]**: [Alternative path]

---

## Wireframe Specification Template

**Screen/Page Name**: [Name]
**Screen Type**: [Desktop/Mobile/Tablet]
**Resolution**: [Width x Height]

### Layout Structure

- **Header**: [Height, content]
- **Main Content Area**: [Sections, grid structure]
- **Sidebar** (if applicable): [Width, content]
- **Footer**: [Height, content]

### Components on Page

1. **[Component Name]**
   - Location: [Position on page]
   - Purpose: [What it does]
   - Content: [What's displayed]
   - Interactions: [Click, hover, etc.]

2. **[Component Name]**
   - Location: [Position on page]
   - Purpose: [What it does]
   - Content: [What's displayed]
   - Interactions: [Click, hover, etc.]

### Responsive Behavior

- **Mobile (320-767px)**: [How layout adapts]
- **Tablet (768-1023px)**: [How layout adapts]
- **Desktop (1024px+)**: [How layout adapts]

### Annotations

- [Important note 1]
- [Important note 2]
- [Developer note]

---

## Design System Component Template

**Component Name**: [Button/Input/Card/etc.]
**Version**: [1.0]
**Last Updated**: [Date]

### Variants

- **Primary**: [Use case and appearance]
- **Secondary**: [Use case and appearance]
- **Tertiary**: [Use case and appearance]
- **Disabled**: [Appearance]

### States

- Default
- Hover
- Active/Pressed
- Focused
- Disabled
- Loading (if applicable)
- Error (if applicable)

### Design Tokens

```
Colors:
  Background: [Hex/RGB value]
  Text: [Hex/RGB value]
  Border: [Hex/RGB value]

Typography:
  Font Family: [Font name]
  Font Size: [Size]
  Font Weight: [Weight]
  Line Height: [Height]
  Letter Spacing: [Spacing]

Spacing:
  Padding: [Top, Right, Bottom, Left]
  Margin: [Values]
  Height: [Min/Max]
  Width: [Min/Max]

Border:
  Radius: [Value]
  Width: [Value]
  Style: [Solid/Dashed/etc.]

Shadow:
  Box Shadow: [Values]
```

### Usage Guidelines

**When to use**:

- [Use case 1]
- [Use case 2]

**When NOT to use**:

- [Anti-pattern 1]
- [Anti-pattern 2]

### Accessibility

- Color Contrast Ratio: [Ratio] (WCAG AA/AAA)
- Keyboard Navigation: [How it works]
- Screen Reader: [ARIA labels and roles]
- Focus Indicator: [Visible focus state]
- Touch Target: [Minimum 44x44px]

### Code Reference

- React: `<ComponentName />`
- CSS Class: `.component-name`
- Figma Link: [Link to Figma component]

### Examples

[Visual examples or screenshots]

---

## Usability Test Plan Template

**Study Name**: [Name of the usability study]
**Date**: [Date]
**Facilitator**: [Name]

### Objectives

- [What you want to learn]
- [What you want to validate]

### Participants

- **Number**: [5-8 participants recommended]
- **Criteria**: [Who should participate]
- **Recruitment**: [How to find them]

### Test Scenarios

#### Scenario 1: [Task Name]

**Goal**: [What user should accomplish]
**Starting Point**: [Where they begin]
**Success Criteria**: [What indicates success]

**Task Instructions**:
"[Exact instructions to give participant]"

**Metrics**:

- Task Completion: Yes/No
- Time on Task: [Target time]
- Errors: [Count]
- Satisfaction: [Rating scale]

#### Scenario 2: [Task Name]

[Repeat structure]

### Test Flow

1. **Introduction** (5 min)
   - Welcome participant
   - Explain think-aloud protocol
   - Get consent

2. **Background Questions** (5 min)
   - [Question 1]
   - [Question 2]

3. **Task Execution** (30 min)
   - Execute scenarios
   - Observe and take notes

4. **Post-Test Questions** (10 min)
   - Overall impression
   - Specific questions about pain points
   - SUS (System Usability Scale) questionnaire

5. **Wrap-up** (5 min)
   - Thank participant
   - Compensation

### Data to Collect

- Task completion rate
- Time to complete tasks
- Number and type of errors
- User satisfaction ratings
- Verbatim quotes
- Observations of confusion/frustration

### Analysis Plan

- Identify common patterns across participants
- Calculate success metrics
- Prioritize issues by severity and frequency
- Create recommendations for design improvements
