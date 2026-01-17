# Core Planning Workflow


### 1. Initial Project Setup

**Define Project Scope:**
- **Vision Statement**: Clear project purpose and objectives
- **Success Criteria**: Measurable outcomes that define project success
- **Stakeholders**: Identify all stakeholders and their roles
- **Constraints**: Budget, timeline, resources, technical limitations
- **Assumptions**: Document all assumptions being made

**Example Vision Statement:**
```
Build a customer relationship management (CRM) system that enables 
sales teams to track leads, manage customer interactions, and generate 
reports. Success means 100 active users within 3 months and 30% 
improvement in lead conversion rates.
```

**Identify Deliverables:**
- Core features and functionality
- Documentation requirements
- Training materials
- Deployment artifacts
- Support handover materials

### 2. Requirements Breakdown

**Transform Requirements into Epics:**

Organize high-level requirements into epics (large bodies of work):

```markdown
Epic: User Management
Description: Complete user authentication, authorization, and profile management
Business Value: Enables secure access and personalized experience
Estimated Effort: 3-4 sprints

Epic: Lead Tracking
Description: Capture, track, and manage sales leads through pipeline
Business Value: Core CRM functionality for sales team
Estimated Effort: 5-6 sprints
```

**Break Epics into User Stories:**

Use the format: "As a [role], I want [feature] so that [benefit]"

```markdown
**User Story**: User Registration
As a new user
I want to create an account with email and password
So that I can access the CRM system

Acceptance Criteria:
- User can enter email, password, and confirm password
- Email validation ensures proper format
- Password must be 8+ characters with 1 uppercase, 1 lowercase, 1 number
- System sends verification email
- User receives confirmation message
- Duplicate emails are rejected with clear error

Priority: High
Estimated Effort: 5 story points
Dependencies: None
```

**Create Technical Tasks:**

Break user stories into implementation tasks:

```markdown
Story: User Registration

Tasks:
1. Design database schema for user table (2h)
   - Email, password hash, created_at, verified fields
   - Add unique constraint on email
   
2. Implement backend API endpoint POST /api/auth/register (4h)
   - Input validation
   - Password hashing
   - Database insert
   - Error handling
   
3. Create email verification service (3h)
   - Generate verification token
   - Send email via provider
   - Token expiry logic
   
4. Build frontend registration form (4h)
   - Form validation
   - Password strength indicator
   - Error display
   - Success confirmation
   
5. Write unit tests (3h)
6. Write integration tests (2h)
7. Update API documentation (1h)

Total Estimate: 19 hours (~2.5 days)
```

### 3. Estimation Techniques

**Story Point Estimation:**

Use relative sizing (Fibonacci: 1, 2, 3, 5, 8, 13, 21):

- **1 point**: Trivial change, < 2 hours, no complexity
- **2 points**: Simple task, 2-4 hours, well understood
- **3 points**: Small feature, 4-8 hours, some complexity
- **5 points**: Medium feature, 1-2 days, moderate complexity
- **8 points**: Large feature, 2-3 days, significant complexity
- **13 points**: Very large, 3-5 days, high complexity (consider splitting)
- **21+ points**: Too large, must be broken down

**Planning Poker Process:**

1. Present user story to team
2. Discuss requirements and acceptance criteria
3. Each team member privately selects estimate
4. Reveal estimates simultaneously
5. Discuss differences (especially outliers)
6. Re-estimate until consensus

**T-Shirt Sizing (High-Level):**

For early estimation:
- **XS**: 1-2 days
- **S**: 3-5 days
- **M**: 1-2 weeks
- **L**: 2-4 weeks
- **XL**: 1-2 months (break down further)

**Three-Point Estimation:**

For critical or uncertain tasks:

```
Optimistic (O): Best-case scenario
Most Likely (M): Expected duration
Pessimistic (P): Worst-case scenario

Expected Time = (O + 4M + P) / 6

Example:
O = 2 days, M = 4 days, P = 10 days
Expected = (2 + 16 + 10) / 6 = 4.67 days
```

### 4. Sprint Planning

**Sprint Structure:**

```markdown
Sprint: Sprint 15
Duration: 2 weeks (Jan 15 - Jan 28)
Team Capacity: 80 story points (4 developers × 20 points each)
Sprint Goal: Complete user authentication and basic profile management

Stories Committed:
1. User Registration (5 pts) - High Priority
2. User Login (3 pts) - High Priority
3. Password Reset (5 pts) - High Priority
4. Email Verification (3 pts) - High Priority
5. User Profile View (5 pts) - Medium Priority
6. Profile Edit (8 pts) - Medium Priority

Total: 29 story points
Buffer: 51 story points remaining for discoveries and bugs
```

**Calculate Team Velocity:**

Track completed story points per sprint:

```
Sprint 12: 32 points
Sprint 13: 28 points
Sprint 14: 35 points

Average Velocity: 31.7 points
Use for planning: ~30 points per sprint
```

**Sprint Ceremonies:**

1. **Sprint Planning** (4 hours for 2-week sprint)
   - Review and refine backlog items
   - Select stories for sprint
   - Break stories into tasks
   - Assign ownership

2. **Daily Standup** (15 minutes)
   - What did I complete yesterday?
   - What will I work on today?
   - Any blockers?

3. **Sprint Review** (2 hours)
   - Demo completed work to stakeholders
   - Gather feedback
   - Update product backlog

4. **Sprint Retrospective** (1.5 hours)
   - What went well?
   - What could improve?
   - Action items for next sprint

### 5. Backlog Management

**Backlog Structure:**

```markdown
Product Backlog (Prioritized):
