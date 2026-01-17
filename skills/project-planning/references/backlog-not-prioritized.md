# Backlog (Not prioritized)

- [ ] Dark mode support
- [ ] Email templates customization
- [ ] Audit logging
```

**Definition of Ready (DoR):**

Story is ready for sprint when it has:
- [ ] Clear user story format
- [ ] Acceptance criteria defined
- [ ] Dependencies identified and resolved
- [ ] Estimated by the team
- [ ] Design mockups (if UI work)
- [ ] Technical approach discussed
- [ ] Small enough to complete in one sprint

**Definition of Done (DoD):**

Story is complete when:
- [ ] Code written and reviewed
- [ ] Unit tests written (>80% coverage)
- [ ] Integration tests written
- [ ] Acceptance criteria validated
- [ ] Documentation updated
- [ ] No critical bugs
- [ ] Deployed to staging environment
- [ ] Accepted by Product Owner

### 6. Prioritization Frameworks

**MoSCoW Method:**

- **Must Have**: Critical, project fails without it
- **Should Have**: Important, significant value but not critical
- **Could Have**: Nice to have, improves user experience
- **Won't Have**: Not in this release, but maybe future

**Value vs. Effort Matrix:**

```
High Value, Low Effort  → Do First (Quick Wins)
High Value, High Effort → Do Next (Major Projects)
Low Value, Low Effort   → Do Later (Fill-ins)
Low Value, High Effort  → Don't Do (Time Wasters)
```

**RICE Scoring:**

```
RICE = (Reach × Impact × Confidence) / Effort

Reach: Number of users affected per time period
Impact: 0.25 (minimal), 0.5 (low), 1 (medium), 2 (high), 3 (massive)
Confidence: 0-100% (how certain are estimates)
Effort: Person-months required

Example:
Feature A: (1000 × 2 × 80%) / 2 = 800
Feature B: (500 × 3 × 60%) / 1 = 900

Feature B has higher RICE score → prioritize first
```

**Kano Model:**

- **Basic**: Expected features, dissatisfaction if missing
- **Performance**: Satisfaction increases with quality
- **Excitement**: Unexpected delighters, differentiation
- **Indifferent**: Users don't care either way
- **Reverse**: Some users prefer absence

### 7. Resource Allocation

**Team Capacity Planning:**

```markdown
Sprint 15 Capacity Analysis:

Developer 1 (Senior): 
- Available: 10 days (80 hours)
- Meetings: 10 hours
- Code reviews: 5 hours
- Technical debt: 5 hours
- Development capacity: 60 hours (30 points)

Developer 2 (Mid-level):
- Available: 9 days (72 hours) - 1 day PTO
- Meetings: 10 hours
- Development capacity: 62 hours (25 points)

Developer 3 (Junior):
- Available: 10 days (80 hours)
- Meetings: 10 hours
- Mentoring: 8 hours
- Development capacity: 62 hours (20 points)

Total Team Capacity: 75 story points

Allocation:
- Committed work: 60 points (80%)
- Buffer for bugs/discoveries: 15 points (20%)
```

**Skill Matrix Mapping:**

```markdown
Feature: Payment Integration

Required Skills:
- Backend (Node.js): 16 hours → Developer 1, 2
- Frontend (React): 8 hours → Developer 3
- DevOps (CI/CD): 4 hours → Developer 1
- Testing (E2E): 4 hours → Developer 2

Assignments:
- Developer 1: Backend API + DevOps setup
- Developer 2: Backend integration + Testing
- Developer 3: Frontend UI components
```

### 8. Risk Management

**Risk Identification:**

```markdown
Risk Register:
