# Requirements Review Examples

Practical examples showing common issues and their improvements.

## Table of Contents
- [Ambiguous Requirements Examples](#ambiguous-requirements-examples)
- [Untestable Requirements Examples](#untestable-requirements-examples)
- [Conflict Examples](#conflict-examples)
- [User Story Examples](#user-story-examples)
- [Non-Functional Requirements Examples](#non-functional-requirements-examples)
- [Complete Review Examples](#complete-review-examples)

## Ambiguous Requirements Examples

### Example 1: Performance Requirements

**❌ Poor (Ambiguous)**:
```
REQ-045: The system shall be fast and responsive.
```

**Issues**:
- "Fast" is subjective - what's fast for one user may be slow for another
- "Responsive" is vague - no measurable criteria
- No context - fast for which operations?
- No baseline - how fast is acceptable?

**✅ Good (Clear)**:
```
REQ-045: The system shall respond to user search queries within 2 seconds for 95% of requests under normal load (up to 1,000 concurrent users).

Acceptance Criteria:
- Search results displayed within 2 seconds for 95th percentile
- Search results displayed within 5 seconds for 99th percentile
- Measured with 1,000 concurrent users performing searches
- Measured over 1-hour sustained load test
- Baseline test data: 1 million product records
```

### Example 2: User Interface Requirements

**❌ Poor (Ambiguous)**:
```
REQ-067: The application shall have a user-friendly interface that is intuitive and easy to use.
```

**Issues**:
- "User-friendly", "intuitive", "easy" are subjective
- No measurable success criteria
- Cannot be tested objectively
- Different users have different skill levels

**✅ Good (Clear)**:
```
REQ-067: The application shall enable new users to complete core tasks without training.

Measurable Criteria:
- 80% of first-time users complete account registration within 5 minutes without assistance
- 70% of new users successfully place first order within 10 minutes without help documentation
- Task completion measured via usability testing with 20+ participants
- Users rate perceived ease-of-use ≥ 4.0/5.0 on post-task survey
- Critical workflows have a maximum of 3 steps from start to completion
```

### Example 3: Security Requirements

**❌ Poor (Ambiguous)**:
```
REQ-089: The system shall be secure and protect user data.
```

**Issues**:
- "Secure" is too broad - what aspects of security?
- "Protect" is vague - protect from what threats?
- No specific mechanisms defined
- Not testable as written

**✅ Good (Clear)**:
```
REQ-089: The system shall protect user data through the following security controls:

1. Authentication:
   - Support OAuth 2.0 and SAML 2.0 authentication
   - Enforce password complexity: min 12 characters, 1 uppercase, 1 lowercase, 1 number, 1 special character
   - Lock account after 5 failed login attempts within 15 minutes
   - Require password change every 90 days

2. Data Protection:
   - Encrypt all data in transit using TLS 1.3
   - Encrypt sensitive PII at rest using AES-256
   - Hash passwords using bcrypt with work factor ≥ 12
   - Mask credit card numbers (show last 4 digits only)

3. Audit & Monitoring:
   - Log all authentication attempts (success and failure) with timestamp, username, IP
   - Log all data access for sensitive fields with user ID and timestamp
   - Retain audit logs for 2 years
   - Alert security team on > 10 failed logins from same IP within 5 minutes
```

## Untestable Requirements Examples

### Example 4: Scalability Requirements

**❌ Poor (Untestable)**:
```
REQ-102: The system shall be highly scalable to support future growth.
```

**Issues**:
- "Highly scalable" has no measurable definition
- "Future growth" is undefined - how much growth?
- No clear test pass/fail criteria
- Cannot verify "shall be scalable"

**✅ Good (Testable)**:
```
REQ-102: The system architecture shall support horizontal scaling to handle increased load.

Testable Criteria:
- System scales from 1,000 to 10,000 concurrent users by adding application servers without code changes
- Response time degrades < 10% when load increases from 1,000 to 5,000 concurrent users (with proportional infrastructure)
- Database supports read replicas with < 100ms replication lag
- Stateless application design allows adding/removing servers without session loss
- Load balancer distributes traffic across N application servers with < 5% variance

Verification Method:
- Load testing with JMeter: measure response times at 1K, 5K, 10K concurrent users
- Infrastructure test: add servers and verify auto-scaling triggers
- Database replication lag monitoring over 24-hour period
```

### Example 5: Maintainability Requirements

**❌ Poor (Untestable)**:
```
REQ-115: The application code shall be maintainable and follow best practices.
```

**Issues**:
- "Maintainable" is subjective
- "Best practices" vary by team/organization
- No objective measurement
- Cannot determine pass/fail

**✅ Good (Testable)**:
```
REQ-115: The application code shall meet the following maintainability criteria:

Code Quality Metrics:
- Maintain ≥ 80% unit test coverage (measured by coverage tool)
- Cyclomatic complexity ≤ 10 per function (measured by SonarQube)
- No code duplication blocks exceeding 10 lines (detected by SonarQube)
- All functions/methods have JSDoc/XML documentation with parameter descriptions

Code Standards:
- Follow ESLint rules defined in .eslintrc (zero errors on production build)
- Pass Prettier formatting checks (automated in pre-commit hook)
- All public APIs documented in OpenAPI 3.0 specification
- Code review approval required before merge (min 1 approver)

Verification Method:
- Automated: SonarQube analysis on every commit, build fails if thresholds not met
- Automated: ESLint/Prettier checks in CI pipeline
- Manual: Code review checklist includes documentation review
```

## Conflict Examples

### Example 6: Priority Conflicts

**❌ Conflicting Requirements**:
```
REQ-125: Users shall provide a phone number during registration. [Priority: Critical]
REQ-187: Phone number shall be optional to reduce registration friction. [Priority: Critical]
```

**Issues**:
- Direct contradiction: mandatory vs optional
- Both marked Critical priority - cannot both be essential
- Different stakeholders likely have different objectives
- Developers cannot implement both

**✅ Resolved Requirement**:
```
REQ-125: Users shall provide an email address during registration (mandatory).

REQ-125a: Users may optionally provide a phone number during registration for:
- SMS notifications (opt-in)
- Two-factor authentication (opt-in)
- Order status updates via SMS (opt-in)

REQ-125b: System shall send verification email to confirm email address before account activation.

REQ-125c: If user provides phone number, system shall send SMS verification code before enabling SMS features.

Priority: Critical (email), Medium (phone number)
Rationale: Email is critical for account recovery and communication. Phone is value-add for users who want SMS features.
Resolution Date: 2024-01-15
Stakeholders Consulted: Product Owner (Sarah), UX Lead (Mike), Security (Tom)
```

### Example 7: Technical Conflicts

**❌ Conflicting Requirements**:
```
REQ-145: System shall provide real-time synchronization across all devices with < 1 second delay.
REQ-167: System shall work offline and sync changes when connection is restored.
```

**Issues**:
- Real-time sync requires constant connection
- Offline mode means no connectivity
- Conflict in architectural approach
- Need to clarify expected behavior

**✅ Resolved Requirements**:
```
REQ-145: System shall provide near-real-time synchronization when devices are online.

Criteria:
- Changes sync across devices within 5 seconds when all devices have active internet connection
- WebSocket connection maintains sync state
- Users see "Syncing..." indicator during synchronization
- Users see "Synced" confirmation when complete

REQ-167: System shall support offline mode with deferred synchronization.

Criteria:
- Users can view and edit locally cached data when offline
- System queues changes locally while offline
- UI shows "Offline Mode" indicator clearly
- When connection restored, system syncs queued changes automatically
- Conflict resolution: Last Write Wins (LWW) with timestamp, user notified of conflicts
- User can view sync history showing which device made which changes

Priority: REQ-145 (High), REQ-167 (Medium)
Architecture: Event sourcing with local-first approach using CRDTs for conflict-free merges
```

## User Story Examples

### Example 8: User Story with Poor Acceptance Criteria

**❌ Poor User Story**:
```
US-045: As a user, I want to search for products so that I can find what I need.

Acceptance Criteria:
- Search works
- Results are displayed
- User can click on results
```

**Issues**:
- Criteria are vague ("search works" - how?)
- No specific scenarios defined
- No error cases handled
- No performance criteria
- No edge cases

**✅ Good User Story**:
```
US-045: As a customer, I want to search for products by keyword so that I can quickly find items I'm interested in purchasing.

Acceptance Criteria:

Given I am on any page of the website
When I enter "laptop" in the search box and press Enter or click Search
Then I should see a list of products matching "laptop" within 2 seconds

Given I search for "laptop"
When the search completes
Then results should be sorted by relevance (best match first)
And each result shows: product image, title, price, rating, availability
And I see the number of results found (e.g., "1,247 results for 'laptop'")

Given I search for "xyzabc123nonsense"
When no products match my search
Then I see "No results found for 'xyzabc123nonsense'" message
And I see search suggestions or popular categories

Given I search for a product
When I click on a search result
Then I am taken to that product's detail page

Given I am viewing search results
When there are more than 20 results
Then results are paginated (20 per page)
And I can navigate to next/previous pages
And I can see total pages (e.g., "Page 1 of 63")

Given I use the search feature
When I enter SQL injection attempt "'; DROP TABLE products--"
Then system sanitizes input and treats it as a normal search string
And no database errors occur

Non-Functional:
- Search response time: < 2 seconds for 95% of queries
- Search handles up to 1,000 concurrent search requests
- Search indexing updates within 5 minutes of product changes
- Search supports common misspellings (e.g., "labtop" → "laptop")

Definition of Done:
- Code reviewed and approved
- Unit tests: 80%+ coverage
- Integration tests pass
- Performance test: verified < 2s response
- Security test: SQL injection protection verified
- Deployed to staging and validated by PO
```

### Example 9: User Story Following INVEST Principles

**✅ Good User Story (INVEST)**:
```
US-078: As a returning customer, I want to reorder items from my order history with one click so that I can quickly purchase items I buy regularly.

Story Points: 5

Acceptance Criteria:

Given I am logged in and viewing my order history
When I click "Reorder" button on a previous order
Then all items from that order are added to my cart
And I am taken to the cart page
And I see a confirmation message "12 items added to cart"

Given I click "Reorder" on order #12345
When some items are no longer available
Then available items are added to cart
And I see a message listing unavailable items: "Note: 2 items no longer available: [Item A], [Item B]"
And I can proceed with available items

Given I click "Reorder" on order #12345
When my cart already contains items
Then reordered items are added to existing cart items
And cart shows updated total item count

Given I click "Reorder" on order #12345
When items have price changes since original order
Then items are added at current prices
And I see message "Prices may have changed since your original order"

Definition of Done:
- UI button displays on order history page
- Backend API endpoint: POST /api/orders/{orderId}/reorder
- Unit tests cover: available items, unavailable items, price changes
- E2E test: complete reorder workflow
- Analytics event fired: "reorder_clicked"
- Performance: reorder completes in < 1 second

Dependencies:
- Requires completed: US-034 (Order history page)
- Requires: Product availability API

Notes:
- Product team priority: High (top customer request)
- Analytics show 35% of customers reorder within 30 days
- Expected to increase conversion by 8% (based on A/B test)

INVEST Check:
✅ Independent: Can be developed without other stories
✅ Negotiable: Could simplify to "single item" reorder if needed
✅ Valuable: Directly improves customer convenience and conversion
✅ Estimable: Team can estimate at 5 points
✅ Small: Fits in one sprint
✅ Testable: Clear acceptance criteria with specific scenarios
```

## Non-Functional Requirements Examples

### Example 10: Performance Requirements

**❌ Poor NFR**:
```
NFR-001: The system shall have good performance.
```

**✅ Good NFR**:
```
NFR-001: Performance - Response Time

The system shall meet the following response time targets measured from user browser:

Page Load Times (Cold Load):
- Homepage: < 3 seconds for 95th percentile
- Product listing page: < 3 seconds for 95th percentile
- Product detail page: < 2 seconds for 95th percentile
- Checkout page: < 2 seconds for 95th percentile

API Response Times:
- Search API: < 500ms for 95th percentile
- Product API: < 300ms for 95th percentile
- Cart operations: < 200ms for 95th percentile
- Checkout API: < 1 second for 95th percentile

Measurement Conditions:
- User location: Within same geographic region as server (< 50ms network latency)
- Device: Desktop with modern browser (Chrome, Firefox, Safari latest versions)
- Network: Broadband connection (10+ Mbps)
- Load: Up to 5,000 concurrent users

Verification:
- Load testing with Apache JMeter simulating 5,000 concurrent users
- Real User Monitoring (RUM) tracking actual user experience
- Weekly performance reports from production RUM data
- Performance regression testing in CI/CD pipeline

Priority: Critical
Acceptance: All targets must be met before production release
```

### Example 11: Availability Requirements

**❌ Poor NFR**:
```
NFR-015: The system should be highly available.
```

**✅ Good NFR**:
```
NFR-015: Availability & Uptime

The system shall maintain 99.9% uptime measured monthly (allows 43.2 minutes downtime per month).

Uptime Measurement:
- Measured via external monitoring service (e.g., Pingdom) from 3 geographic locations
- Service considered "down" if HTTP 200 response not received within 30 seconds
- Planned maintenance windows excluded from uptime calculation

Scheduled Maintenance:
- Maximum 4 hours per month for planned maintenance
- Scheduled maintenance must be announced 7 days in advance
- Maintenance window: Sundays 2:00 AM - 6:00 AM EST (lowest traffic period)
- During maintenance, display maintenance page with estimated restoration time

High Availability Architecture:
- Load balancer distributes across min 3 application servers
- Database: Primary-replica setup with automatic failover
- Database failover time: < 60 seconds
- Application servers: Auto-scaling based on CPU (add server when >70%)
- Health checks every 30 seconds, auto-replace unhealthy instances

Disaster Recovery:
- Recovery Time Objective (RTO): 4 hours
- Recovery Point Objective (RPO): 15 minutes
- Database backups: Every 15 minutes (incremental), daily (full)
- Backup stored in geographically separate region
- DR site can handle 100% of production load

Monitoring & Alerting:
- 24/7 monitoring via PagerDuty
- Alert when availability drops below 99.5% in rolling 24-hour window
- Alert on any service downtime > 5 minutes
- Escalation to on-call engineer within 5 minutes

Verification:
- Monthly uptime reports from monitoring service
- Quarterly DR drills (full failover test)
- Load balancer failover test (monthly)

Priority: Critical
Penalties: SLA credits if uptime < 99.9% (per contract terms)
```

## Complete Review Examples

### Example 12: BRD Section Review

**Original BRD Section**:
```markdown
## 3.2 User Management

The system needs user management capabilities. Users should be able to register, login, and manage their profiles. The system should be secure and protect user information. Administrators need to be able to manage users.
```

**Review Findings**:

**Issues Identified**:

1. **Completeness (Critical)**: Missing key requirements
   - No password requirements specified
   - No user role definitions (regular user vs admin)
   - No account activation/verification process
   - No password reset workflow
   - No session management requirements
   - No multi-factor authentication requirements

2. **Clarity (Major)**: Vague statements
   - "secure" is not defined - what security controls?
   - "manage profiles" is vague - what can users edit?
   - "manage users" is vague - what admin capabilities?

3. **Testability (Major)**: Not testable as written
   - No acceptance criteria for any requirement
   - No measurable security criteria

**Improved BRD Section**:
```markdown
## 3.2 User Management

### 3.2.1 User Registration

REQ-201: The system shall allow new users to register by providing:
- Email address (required, unique, validated format)
- Password (required, meets complexity requirements per REQ-202)
- First name (required, 2-50 characters)
- Last name (required, 2-50 characters)
- Phone number (optional, validated format if provided)

REQ-202: The system shall enforce password complexity requirements:
- Minimum 12 characters
- At least 1 uppercase letter
- At least 1 lowercase letter
- At least 1 number
- At least 1 special character (!@#$%^&*)
- Cannot be same as previous 5 passwords
- Cannot contain user's name or email

REQ-203: The system shall send email verification upon registration:
- Verification email sent immediately after registration
- Email contains secure verification link (expires in 24 hours)
- User cannot login until email is verified
- User can request resend of verification email

REQ-204: The system shall validate that email addresses are unique (prevent duplicate accounts)

### 3.2.2 User Authentication

REQ-210: The system shall support username/password authentication:
- Users login with email address and password
- Passwords transmitted over HTTPS only
- Passwords stored as bcrypt hash (work factor ≥ 12)

REQ-211: The system shall lock accounts after 5 failed login attempts within 15 minutes:
- Display message: "Account locked due to multiple failed login attempts. Try again in 15 minutes or reset your password."
- Account automatically unlocks after 15 minutes
- User can unlock by completing password reset

REQ-212: The system shall support "Remember Me" functionality:
- Optional checkbox on login page
- If checked, session lasts 30 days
- If unchecked, session expires after 30 minutes of inactivity
- User can logout manually at any time

REQ-213: The system shall support optional two-factor authentication (2FA):
- Users can enable 2FA in account settings
- Support TOTP-based 2FA (Google Authenticator, Authy compatible)
- 2FA required for admin users (mandatory)
- Backup codes provided when 2FA is enabled (10 single-use codes)

### 3.2.3 Password Reset

REQ-220: The system shall allow users to reset forgotten passwords:
- "Forgot Password" link on login page
- User enters email address
- System sends password reset email (if account exists)
- Reset link expires in 1 hour
- New password must meet complexity requirements (REQ-202)

REQ-221: The system shall not reveal whether an email exists in the system:
- Always display "If an account exists, reset email has been sent"
- Prevents account enumeration attacks

### 3.2.4 Profile Management

REQ-230: Users shall be able to view and edit their profile:
- Editable fields: First name, last name, phone number
- Cannot edit: Email address (must verify new email if changing)
- Profile picture upload (optional, max 5MB, JPG/PNG only)

REQ-231: Users shall be able to change their email address:
- Verify new email address before change takes effect
- Verification link sent to new email address
- Email change confirmed only after clicking verification link

REQ-232: Users shall be able to change their password:
- Must provide current password
- New password must meet complexity requirements (REQ-202)
- New password cannot match previous 5 passwords

REQ-233: Users shall be able to enable/disable 2FA in account settings

REQ-234: Users shall be able to view login history:
- Last 30 days of login activity
- Display: Date/time, IP address, location (city/country), device type

### 3.2.5 Admin User Management

REQ-240: Administrators shall be able to view all user accounts:
- Searchable list: by email, name, or user ID
- Filterable by: account status (active/locked/disabled), role, registration date
- Sortable by: name, email, registration date, last login

REQ-241: Administrators shall be able to disable/enable user accounts:
- Disabled users cannot login
- Reason for disabling must be recorded in audit log

REQ-242: Administrators shall be able to unlock locked accounts:
- Reset failed login counter
- Unlock reason recorded in audit log

REQ-243: Administrators shall be able to assign user roles:
- Roles: Regular User, Admin
- Role changes recorded in audit log with admin user ID and timestamp

REQ-244: Administrators cannot delete user accounts:
- Rationale: Preserve data integrity and audit trail
- Disabled accounts retain all data but prevent login

### 3.2.6 Session Management

REQ-250: The system shall manage user sessions securely:
- Session tokens stored as secure, HTTP-only cookies
- Session tokens are randomly generated (256-bit entropy)
- Sessions expire after 30 minutes of inactivity (unless "Remember Me" checked)
- User can have max 3 concurrent sessions (oldest automatically logged out)

REQ-251: The system shall support explicit logout:
- "Logout" button visible on all pages when logged in
- Logout invalidates current session token immediately
- Redirect to login page after logout

### 3.2.7 Audit & Compliance

REQ-260: The system shall log all authentication and authorization events:
- Login attempts (success and failure): timestamp, user, IP address
- Logout events: timestamp, user
- Password changes: timestamp, user
- Password resets: timestamp, user, IP address
- Profile updates: timestamp, user, fields changed
- Admin actions: timestamp, admin user, action, target user
- Logs retained for 2 years

REQ-261: The system shall comply with GDPR requirements:
- Users can download their data (JSON format)
- Users can request account deletion (admin processes request)
- Privacy policy acceptance required during registration
```

**Review Summary**:
- Original: 4 vague sentences
- Improved: 21 specific, testable requirements
- Added: Security controls, workflows, admin capabilities
- Quality improvement: 30% → 95% completeness

This example shows how a vague section transforms into comprehensive, implementable requirements.

---

## Key Takeaways

When reviewing requirements, look for:

1. **Specificity**: Replace vague terms with measurable criteria
2. **Testability**: Every requirement should have clear pass/fail criteria
3. **Completeness**: All scenarios including happy path, errors, edge cases
4. **Consistency**: No conflicts between requirements
5. **Traceability**: Link to business goals and downstream artifacts

Quality requirements enable quality software!
