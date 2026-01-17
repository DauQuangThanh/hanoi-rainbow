# Agile Requirements Management Guide

This guide covers requirements management in Agile environments, including user stories, backlog management, refinement, and sprint planning.

## Agile Requirements Principles

### Agile Manifesto Values Applied to Requirements
1. **Individuals and interactions** over processes and documentation
   - Favor conversations over lengthy specs
   - Collaborate continuously with stakeholders
   - Embrace changing requirements

2. **Working software** over comprehensive documentation
   - Document just enough to build
   - Use working prototypes to validate
   - Let code serve as documentation where appropriate

3. **Customer collaboration** over contract negotiation
   - Involve customers throughout development
   - Frequent demonstrations and feedback
   - Adjust priorities based on learnings

4. **Responding to change** over following a plan
   - Welcome requirement changes
   - Short iterations enable flexibility
   - Prioritize based on current business value

### Agile Requirements Best Practices
- Write requirements just in time
- Keep requirements at appropriate level of detail
- Focus on outcomes over outputs
- Validate requirements frequently
- Maintain a prioritized backlog
- Involve the whole team
- Use acceptance test-driven development

## User Story Framework

### Anatomy of a User Story

```markdown
# User Story Structure

**As a** [user role]
**I want** [capability or feature]
**So that** [business value or benefit]

## Example 1: Simple Format
**As a** customer
**I want** to save items to a wishlist
**So that** I can purchase them later

## Example 2: Detailed Format
**As a** registered customer
**I want** to receive email notifications when wishlist items go on sale
**So that** I can purchase items at the best price and save money

**Acceptance Criteria**:
- Given I have items in my wishlist
  When any item goes on sale
  Then I receive an email within 1 hour
  
- Given I receive a sale notification
  When I click the item link
  Then I am taken directly to the item page with sale price displayed

**Priority**: Medium
**Story Points**: 5
**Sprint**: Sprint 23
**Dependencies**: Email service, Price monitoring system
```

### INVEST Criteria

**Independent**
- Stories should not depend on other stories
- Can be developed in any order
- Reduces coordination overhead

```markdown
# ❌ Bad - Dependent Stories
Story 1: Create user registration form
Story 2: Add email field to registration form (depends on Story 1)

# ✅ Good - Independent Stories  
Story 1: Create user registration with name, email, and password fields
Story 2: Add social media login option to registration
```

**Negotiable**
- Details are not fixed
- Allows for team creativity
- Encourages collaboration

```markdown
# ❌ Bad - Prescriptive
As a user, I want a blue Submit button at the bottom right
that uses Arial 14pt font and has a 10px border radius

# ✅ Good - Negotiable
As a user, I want to submit my form easily
so that I can complete my registration quickly
```

**Valuable**
- Delivers value to user or business
- Not a technical task
- Tied to business outcome

```markdown
# ❌ Bad - Technical Task
As a developer, I want to refactor the database schema

# ✅ Good - Valuable
As a customer support agent, I want to search customers by multiple criteria
so that I can find customer records faster and handle more support requests
```

**Estimable**
- Team can estimate effort
- Has enough detail
- Team understands what to build

```markdown
# ❌ Bad - Not Estimable
As a user, I want an improved dashboard

# ✅ Good - Estimable
As a sales manager, I want to see top 10 sales opportunities on my dashboard
sorted by deal value, so that I can focus on the biggest deals
```

**Small**
- Can be completed in one sprint
- Typically 1-13 story points
- Can be split if too large

```markdown
# ❌ Bad - Too Large (Epic)
As a customer, I want a complete e-commerce platform

# ✅ Good - Appropriately Sized
As a customer, I want to add items to my shopping cart
so that I can purchase multiple items in one order
```

**Testable**
- Clear acceptance criteria
- Can verify when done
- Observable outcomes

```markdown
# ❌ Bad - Not Testable
As a user, I want the system to be fast

# ✅ Good - Testable
As a user, I want search results to appear within 2 seconds
so that I can find products quickly

Acceptance Criteria:
- Search completes in < 2 seconds for 95% of queries
- Search works with up to 100,000 products
- Results are sorted by relevance
```

## User Story Formats

### 1. Standard Format
```markdown
As a [role]
I want [feature]
So that [benefit]
```

### 2. Job Story Format
```markdown
When [situation]
I want to [motivation]
So I can [expected outcome]

Example:
When I'm browsing products on my mobile phone during my commute
I want to quickly save interesting items
So I can review them later on my desktop and make a purchase decision
```

### 3. Feature Injection Format
```markdown
In order to [achieve value]
As a [role]
I want [feature]

Example:
In order to increase repeat purchases by 15%
As an e-commerce business owner
I want to send personalized product recommendations to customers
```

## Story Mapping

### Story Map Structure

```
                    User Activity 1        User Activity 2       User Activity 3
                    ↓                      ↓                     ↓
Backbone    →    Browse Products    →   Add to Cart    →    Complete Purchase
                    |                      |                     |
                    ↓                      ↓                     ↓
Walking     →    Search Products        View Cart            Enter Shipping
Skeleton           View Details          Update Quantity      Enter Payment
(MVP)              Filter Results        Apply Coupon         Review Order
                   Compare Items                              Confirm Purchase
                    |                      |                     |
                    ↓                      ↓                     ↓
Later          Save to Wishlist         Share Cart           Save Payment Info
Releases       Product Reviews          Gift Wrapping        Order Tracking
               Recommendations          Bulk Discounts       Wishlist to Cart
```

### Creating a Story Map

**Step 1: Identify User Activities**
High-level tasks users need to accomplish
```markdown
- Discover products
- Research products
- Make purchase decision
- Complete transaction
- Receive order
- Post-purchase support
```

**Step 2: Break Down into User Tasks**
Specific actions within each activity
```markdown
Discover Products:
- Browse by category
- Search by keyword
- View promotions
- Get recommendations

Research Products:
- View product details
- Read reviews
- Compare products
- Check availability
```

**Step 3: Identify MVP (Walking Skeleton)**
Minimum viable path through the map
```markdown
MVP Release:
- Search products
- View product details
- Add to cart
- Checkout
- Email confirmation
```

**Step 4: Plan Subsequent Releases**
Add features in priority order
```markdown
Release 2:
- User accounts
- Order history
- Wishlist
- Product reviews

Release 3:
- Recommendations
- Advanced search
- Gift wrapping
- Loyalty points
```

## Backlog Management

### Product Backlog Structure

```markdown
# Product Backlog

## Epics (Large Features)
├── Epic 1: User Management
│   ├── Story: User Registration
│   ├── Story: User Login
│   ├── Story: Password Reset
│   └── Story: Profile Management
│
├── Epic 2: Shopping Cart
│   ├── Story: Add to Cart
│   ├── Story: Update Quantities
│   ├── Story: Apply Coupons
│   └── Story: Save Cart
│
└── Epic 3: Checkout Process
    ├── Story: Shipping Information
    ├── Story: Payment Processing
    ├── Story: Order Review
    └── Story: Order Confirmation

## Backlog Prioritization
1. [Must Have] User Registration & Login
2. [Must Have] Browse & Search Products
3. [Must Have] Shopping Cart
4. [Must Have] Basic Checkout
5. [Should Have] Wishlist
6. [Should Have] Product Reviews
7. [Could Have] Recommendations
8. [Could Have] Social Sharing

## Technical Debt
- Refactor authentication module
- Optimize database queries
- Update deprecated dependencies

## Bugs
- [Critical] Payment gateway timeout
- [High] Search returns incorrect results
- [Medium] Email formatting issues
```

### Backlog Refinement (Grooming)

**Purpose**: Prepare stories for upcoming sprints

**Activities**:
1. **Review**: Examine upcoming backlog items
2. **Detail**: Add acceptance criteria and details
3. **Estimate**: Assign story points
4. **Split**: Break large stories into smaller ones
5. **Clarify**: Answer questions and remove ambiguity
6. **Order**: Re-prioritize based on current knowledge

**Refinement Meeting Structure**:
```markdown
Duration: 1-2 hours per week
Attendees: Product Owner, Scrum Master, Development Team
Frequency: Mid-sprint

Agenda:
1. Review top 10-15 backlog items (30 min)
   - PO presents stories
   - Team asks clarifying questions
   - Discuss implementation approach

2. Estimate stories (30 min)
   - Planning poker for story points
   - Identify dependencies
   - Flag risks

3. Split large stories (20 min)
   - Identify stories > 13 points
   - Break into smaller stories
   - Maintain vertical slices

4. Update backlog (10 min)
   - Refine acceptance criteria
   - Add technical notes
   - Update priorities
```

### Definition of Ready (DoR)

Stories are ready for sprint planning when:

```markdown
# Definition of Ready Checklist

User Story Quality:
□ Written in user story format (As a... I want... So that...)
□ Has clear business value
□ Is independent (or dependencies identified)
□ Is small enough to complete in one sprint
□ Has detailed description

Acceptance Criteria:
□ Clear, testable acceptance criteria defined
□ Edge cases and error conditions documented
□ Performance requirements specified (if applicable)
□ Security requirements identified (if applicable)

Design & UX:
□ Wireframes or mockups available (if UI changes)
□ UX considerations documented
□ Accessibility requirements noted

Technical:
□ Technical approach discussed
□ Dependencies identified
□ APIs or services needed are documented
□ Test strategy outlined

Team Understanding:
□ Product Owner available for questions
□ Team understands the story
□ Story has been estimated
□ Team commits to delivering the story

Documentation:
□ Links to related documents provided
□ Previous work or context referenced
□ Risks and assumptions documented
```

## Sprint Planning

### Sprint Planning Meeting

**Duration**: 2-4 hours for 2-week sprint

**Part 1: What? (1-2 hours)**
```markdown
Purpose: Select stories for sprint

Activities:
1. Review sprint goal
2. Review team capacity
3. Review prioritized backlog
4. Select stories that fit capacity
5. Confirm Definition of Ready

Output:
- Sprint Goal
- Committed stories
- Sprint backlog
```

**Part 2: How? (1-2 hours)**
```markdown
Purpose: Create execution plan

Activities:
1. Break stories into tasks
2. Estimate tasks in hours
3. Identify technical approach
4. Assign initial owners
5. Identify blockers

Output:
- Task breakdown
- Hour estimates
- Technical approach
- Identified risks
```

### Sprint Goal

**Good Sprint Goal Characteristics**:
- Focused on business value
- Inspiring to the team
- Achievable within sprint
- Guides decision-making

```markdown
# ❌ Bad Sprint Goals
- Complete 15 story points
- Finish user authentication
- Fix bugs

# ✅ Good Sprint Goals
- Enable users to create and manage their profiles
- Launch MVP shopping cart functionality
- Improve checkout conversion rate by reducing friction
```

### Story Point Estimation

**Planning Poker Process**:

```markdown
1. Product Owner presents story
2. Team asks clarifying questions
3. Each team member privately selects estimate
4. All reveal simultaneously
5. Discuss differences (especially high/low)
6. Re-estimate until consensus

Story Point Scale (Modified Fibonacci):
1 - Very simple, a few hours
2 - Simple, less than a day
3 - Medium, 1-2 days
5 - Complex, 3-5 days
8 - Very complex, full sprint
13 - Epic, needs splitting
20, 40, 100 - Way too large, definitely split
```

**Estimation Examples**:

```markdown
1 Point Stories:
- Add a new field to existing form
- Update text on a page
- Add validation to existing field

3 Point Stories:
- Create simple CRUD form
- Implement basic search
- Add new report with existing data

5 Point Stories:
- Create complex form with validation
- Implement search with filters
- Add new API endpoint with tests

8 Point Stories:
- Implement OAuth integration
- Create complex reporting feature
- Build new dashboard page

13+ Point Stories:
- These should be split into smaller stories
- Usually indicates unclear requirements
- May contain multiple features
```

## Acceptance Criteria

### Given-When-Then Format (Gherkin)

```markdown
# User Story: Product Search

As a customer
I want to search for products by keyword
So that I can quickly find what I'm looking for

## Acceptance Criteria

Scenario 1: Successful search
Given I am on the home page
When I enter "laptop" in the search box
And I click the search button
Then I should see a list of laptops
And the results should be sorted by relevance
And I should see at least 10 results per page

Scenario 2: No results found
Given I am on the search page
When I enter "xyzabc123" in the search box
And I click the search button
Then I should see "No products found" message
And I should see search suggestions
And I should see popular products

Scenario 3: Search with filters
Given I have performed a search for "laptop"
When I filter by price range $500-$1000
And I filter by brand "Dell"
Then I should see only Dell laptops priced between $500-$1000
And the filter selections should be visible
And I should be able to remove filters

Scenario 4: Search performance
Given I am on any page
When I perform a search
Then results should appear within 2 seconds
And the search should handle 100 concurrent users
```

### Checklist Format

```markdown
# User Story: User Registration

As a new user
I want to create an account
So that I can access personalized features

## Acceptance Criteria

Functional Requirements:
□ Registration form includes: email, password, name
□ Email validation checks for valid email format
□ Password must be minimum 8 characters
□ Password must contain uppercase, lowercase, and number
□ Email must be unique (no duplicates)
□ Confirmation email sent after registration
□ User can click link in email to verify account
□ User redirected to dashboard after successful registration

UI/UX Requirements:
□ Form displays clear error messages for invalid input
□ Password field has show/hide toggle
□ Password strength indicator displayed
□ "Already have an account?" link to login page
□ Form is mobile responsive
□ All form fields have proper labels and placeholders

Security Requirements:
□ Password is hashed before storage
□ HTTPS required for registration page
□ CAPTCHA prevents bot registrations
□ Rate limiting prevents brute force attacks

Performance Requirements:
□ Registration completes in < 3 seconds
□ Confirmation email sent within 1 minute
□ Form validates input in real-time (< 500ms)
```

## Agile Ceremonies for Requirements

### Daily Standup
```markdown
Duration: 15 minutes
Purpose: Sync on progress and impediments

Questions related to requirements:
- Did requirements become clearer yesterday?
- Any requirements questions blocking you?
- Need clarification from Product Owner?
```

### Sprint Review (Demo)
```markdown
Duration: 1-2 hours
Purpose: Demonstrate completed work

Activities:
1. Review sprint goal
2. Demo completed stories
3. Get feedback from stakeholders
4. Discuss what to do next
5. Update backlog based on feedback

Requirements Focus:
- Validate against acceptance criteria
- Get feedback on implementation
- Identify new requirements
- Adjust priorities based on learnings
```

### Sprint Retrospective
```markdown
Duration: 1.5 hours
Purpose: Improve the process

Questions:
- Were requirements clear enough?
- Did we have enough detail to start?
- What requirements issues slowed us down?
- How can we improve requirements quality?
```

## Managing Changing Requirements

### Change Request Process

```markdown
# Change Request Template

## Change Details
- **Requested By**: [Stakeholder name]
- **Date**: [Request date]
- **Priority**: [Critical/High/Medium/Low]
- **Type**: [New Feature/Enhancement/Bug Fix]

## Current Functionality
[Describe how it works now]

## Requested Change
[Describe the requested change]

## Business Justification
[Why is this change needed?]
[What business value does it provide?]
[What happens if we don't make this change?]

## Impact Analysis

### Effort Estimate
- Development: [X story points / Y hours]
- Testing: [X hours]
- Documentation: [X hours]
- Total: [X hours]

### Affected Components
- [ ] Frontend
- [ ] Backend API
- [ ] Database
- [ ] Mobile app
- [ ] Documentation

### Dependencies
- Depends on: [Other stories/features]
- Blocks: [Other stories/features]

### Risks
- Technical risks
- Schedule risks
- Resource risks

## Decision
- [ ] Approved - Add to backlog with priority: _______
- [ ] Approved - Add to current sprint
- [ ] Deferred - Add to future release: _______
- [ ] Rejected - Reason: _______

**Decided By**: [Name]
**Date**: [Decision date]
```

### Handling Mid-Sprint Changes

**Options**:

1. **Add to Backlog** (Preferred)
   - Most changes go here
   - Prioritize for future sprints
   - Maintain sprint commitment

2. **Swap with Equal Story**
   - Remove uncompleted story
   - Add new story of equal size
   - Team must agree
   - Happens rarely

3. **Emergency Change**
   - Critical production issues only
   - Stop sprint and address
   - Document impact
   - Adjust sprint goal

## Requirements Documentation in Agile

### Lightweight Documentation

**Just Enough Documentation**:
```markdown
Required:
- User stories with acceptance criteria
- Sprint planning notes
- Definition of Done
- Key technical decisions

Optional (as needed):
- Architecture diagrams
- Data models
- API specifications
- User journey maps
- Wireframes/mockups
```

### Living Documentation

**Code as Documentation**:
```javascript
// Self-documenting code
class UserRegistrationService {
  async registerUser(email, password, name) {
    this.validateEmail(email);
    this.validatePasswordStrength(password);
    
    const hashedPassword = await this.hashPassword(password);
    const user = await this.createUser(email, hashedPassword, name);
    await this.sendVerificationEmail(user);
    
    return user;
  }
}
```

**Automated Tests as Documentation**:
```javascript
describe('User Registration', () => {
  it('should create user with valid email and password', async () => {
    // Acceptance criteria documented in test
  });
  
  it('should reject registration with invalid email', async () => {
    // Edge case documented in test
  });
  
  it('should send verification email after registration', async () => {
    // Behavior documented in test
  });
});
```

## Best Practices

### 1. Keep Stories Vertical
```markdown
# ❌ Bad - Horizontal slices (by layer)
Story 1: Create database tables for user management
Story 2: Create API endpoints for user management
Story 3: Create UI for user management

# ✅ Good - Vertical slices (by feature)
Story 1: As a user, I want to register an account
Story 2: As a user, I want to login to my account
Story 3: As a user, I want to reset my password
```

### 2. Split Stories Effectively
```markdown
Splitting Techniques:

By Workflow Steps:
- Story 1: Add items to cart
- Story 2: Apply coupon to cart
- Story 3: Proceed to checkout

By Business Rules:
- Story 1: Calculate shipping for domestic orders
- Story 2: Calculate shipping for international orders

By Data Variations:
- Story 1: Search products by name
- Story 2: Search products by category
- Story 3: Search products by price range

By Operations (CRUD):
- Story 1: Create user profile
- Story 2: Edit user profile
- Story 3: Delete user profile
```

### 3. Maintain Backlog Health
- Groom regularly (weekly)
- Keep top 2-3 sprints ready
- Archive completed items
- Remove obsolete stories
- Consolidate duplicates
- Update estimates as you learn

### 4. Collaborate Continuously
- PO available daily for questions
- Team attends refinement
- Include QA in story creation
- Share demos frequently
- Adjust based on feedback

### 5. Balance Detail
- Too little: Team can't build
- Too much: Wastes time, reduces flexibility
- Rule: Add detail just in time
- If unsure, add acceptance test

## Common Anti-Patterns

**Anti-Pattern 1: Writing Technical Tasks as User Stories**
```markdown
# ❌ Bad
As a developer, I want to implement caching
so that the application is faster

# ✅ Good
As a user, I want search results to appear within 2 seconds
so that I can find products quickly
```

**Anti-Pattern 2: Too Much Detail Up Front**
```markdown
# ❌ Bad
Writing 200-page requirements document
before starting development

# ✅ Good
High-level epics → Stories with acceptance criteria
→ Detail emerges through conversation and testing
```

**Anti-Pattern 3: No Acceptance Criteria**
```markdown
# ❌ Bad
As a user, I want better search

# ✅ Good
As a user, I want to search products by keyword
Acceptance Criteria:
- Search returns results in < 2 seconds
- Results are sorted by relevance
- Search handles typos and suggests corrections
```

**Anti-Pattern 4: Stories That Are Really Epics**
```markdown
# ❌ Bad
As a customer, I want a complete e-commerce platform

# ✅ Good
Break into smaller stories:
- Browse products by category
- Add products to cart
- Checkout and pay
- Track order status
```

**Anti-Pattern 5: Ignoring Non-Functional Requirements**
```markdown
# ❌ Bad
Only focus on features

# ✅ Good
Include NFRs in Definition of Done or as separate stories:
- Performance: < 2 second response time
- Security: All data encrypted at rest
- Scalability: Support 10,000 concurrent users
- Accessibility: WCAG 2.1 AA compliance
```

## Tools for Agile Requirements

**Backlog Management**:
- Jira
- Azure DevOps
- Trello
- Asana
- Monday.com

**User Story Mapping**:
- Miro
- Mural
- Storiesonboard
- Cardboard

**Acceptance Testing**:
- Cucumber (BDD)
- SpecFlow
- Behave
- JBehave

**Collaboration**:
- Confluence
- Notion
- Google Docs
- Microsoft Teams
