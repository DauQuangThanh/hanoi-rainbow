# Quality Checklists

Detailed checklists for assessing requirement quality across different attributes.

## Table of Contents
- [Individual Requirement Checklist](#individual-requirement-checklist)
- [Document-Specific Checklists](#document-specific-checklists)
- [User Story Quality (INVEST)](#user-story-quality-invest)

## Individual Requirement Checklist

For each requirement, evaluate against these criteria:

### Completeness
□ All necessary information included
□ No missing details or TBD placeholders
□ Context and rationale provided
□ Dependencies identified and documented
□ Constraints documented
□ Assumptions stated explicitly

### Clarity
□ Unambiguous language used
□ No vague terms ("user-friendly", "fast", "easy", "intuitive")
□ Clear subject and action
□ Specific and concrete
□ Understandable by all stakeholders
□ No jargon without definition

### Correctness
□ Technically accurate
□ Aligned with business needs
□ Reflects stakeholder intent
□ No technical errors
□ Factually correct
□ Feasible to implement

### Consistency
□ No conflicts with other requirements
□ Consistent terminology throughout
□ Consistent level of detail
□ Compatible priorities
□ Aligned with project scope
□ Consistent formatting

### Testability
□ Can be verified/tested
□ Measurable acceptance criteria defined
□ Clear pass/fail conditions
□ Observable outcomes
□ Defined test approach
□ Quantifiable where needed

### Traceability
□ Unique identifier assigned
□ Source documented (stakeholder, regulation, etc.)
□ Rationale provided
□ Linked to business objectives
□ Mapped to architecture/design elements
□ Linked to test cases

### Necessity
□ Directly supports business goals
□ Adds clear value to users/business
□ Not a duplicate of existing requirement
□ Priority justified
□ Cost justified against value

### Feasibility
□ Technically feasible with available technology
□ Achievable within constraints (time, budget, resources)
□ Technology exists or is obtainable
□ Team has or can acquire needed capability
□ Within budget allocation
□ Within project timeline

## Document-Specific Checklists

### Business Requirements Document (BRD)

#### Business Context
□ Business problem clearly stated
□ Business objectives defined and measurable
□ Success criteria quantified
□ Stakeholders identified with roles
□ Business case present with ROI/benefits analysis
□ Strategic alignment documented

#### Scope Definition
□ In-scope items clearly listed
□ Out-of-scope items explicitly stated
□ Boundaries well-defined
□ Assumptions documented
□ Constraints identified
□ Dependencies on external factors listed

#### Business Requirements
□ Requirements state "what" not "how"
□ Business rules documented
□ Process flows included
□ Business metrics defined
□ Regulatory requirements covered
□ Compliance needs stated

### Software Requirements Specification (SRS)

#### Document Structure
□ IEEE 830 compliant structure (if applicable)
□ Version control information present
□ Table of contents complete
□ Glossary of terms included
□ References section complete
□ Appropriate sections present

#### Functional Requirements
□ All system features specified
□ System behaviors defined
□ Input/output specifications complete
□ Business logic detailed
□ Error handling scenarios covered
□ Edge cases addressed

#### Non-Functional Requirements
**Performance**
□ Response time requirements specified
□ Throughput requirements defined
□ Resource utilization limits set
□ Capacity requirements stated

**Security**
□ Authentication mechanisms defined
□ Authorization rules specified
□ Data protection requirements stated
□ Audit logging needs documented

**Reliability**
□ Availability targets specified (uptime %)
□ MTBF/MTTR defined
□ Backup requirements stated
□ Disaster recovery needs addressed

**Usability**
□ User experience goals defined
□ Accessibility requirements stated (WCAG level)
□ Internationalization needs documented
□ Training requirements specified

**Maintainability**
□ Code quality standards defined
□ Documentation requirements stated
□ Modularity requirements specified
□ Update/patch strategy defined

#### Technical Details
□ Architecture constraints stated
□ Technology stack requirements listed
□ Integration points defined with APIs/protocols
□ Data requirements specified with schemas
□ Interface requirements clear

#### Verification
□ All requirements have verification method
□ Acceptance criteria defined for each requirement
□ Test approach specified
□ Traceability matrix complete or referenced

### User Story Backlog

#### Epic Level
□ Epics align with business goals
□ Epics appropriately sized (decomposable into 5-10 stories)
□ Epic dependencies mapped
□ Epic-level acceptance criteria defined
□ Value proposition clear for each epic

#### Story Level Format
□ Follows standard format: "As a [role], I want [feature], so that [benefit]"
□ Role clearly identified (specific user type)
□ Feature/capability specifically described
□ Business value/benefit explicitly stated
□ Story is self-contained and understandable

#### Story Completeness
□ UI/UX considerations noted where relevant
□ Technical constraints documented
□ Dependencies on other stories identified
□ Size estimate provided (story points)
□ Priority assigned (MoSCoW, numeric, etc.)
□ Epic or feature area linked

## User Story Quality (INVEST)

Evaluate each user story against INVEST criteria:

### Independent
□ Story can be developed and tested separately from other stories
□ No tight coupling with other stories
□ Can be scheduled flexibly
□ Dependencies are documented but minimal

### Negotiable
□ Details can be discussed and refined
□ Not an explicit contract or detailed specification
□ Allows for collaboration between team and stakeholder
□ Room for technical implementation choices

### Valuable
□ Delivers clear value to end user or business
□ Value statement is explicit in the "so that" clause
□ Stakeholder can understand and prioritize the value
□ Contributes to business goals or user needs

### Estimable
□ Team has enough information to provide size estimate
□ Complexity is understood
□ No major unknowns that prevent estimation
□ If not estimable, needs spike or research story first

### Small
□ Can be completed within one sprint/iteration
□ Fits within team's definition of "small"
□ Large stories are broken down (epics → stories → tasks)
□ Typically 1-5 days of effort

### Testable
□ Clear acceptance criteria provided
□ Can be verified/validated
□ Pass/fail conditions are objective
□ Testing approach is obvious

## Common Quality Issues

### Ambiguity Examples

❌ **Bad**: "The system shall be fast"
✅ **Good**: "The system shall respond to user search queries within 2 seconds for 95% of requests under normal load"

❌ **Bad**: "The interface shall be user-friendly"
✅ **Good**: "New users shall be able to complete their first purchase within 5 minutes without requiring help documentation"

❌ **Bad**: "The system shall handle many concurrent users"
✅ **Good**: "The system shall support 10,000 concurrent users with 99.9% uptime and response times under 3 seconds"

### Incompleteness Examples

❌ **Bad**: "The system shall send notifications"
✅ **Good**: "The system shall send email notifications to users within 5 minutes when their order status changes to 'Shipped', including order number, new status, tracking link, and estimated delivery date"

❌ **Bad**: "Users shall be able to search products"
✅ **Good**: "Users shall be able to search products by keyword, category, price range ($-$$$$), and brand, with results displayed in relevance order and pagination (20 items per page) within 2 seconds"

### Untestability Examples

❌ **Bad**: "The system shall be secure"
✅ **Good**: "The system shall: (1) enforce password complexity (min 8 chars, 1 uppercase, 1 number, 1 special char), (2) lock accounts after 5 failed login attempts for 15 minutes, (3) encrypt all data in transit using TLS 1.3, (4) encrypt sensitive data at rest using AES-256"

❌ **Bad**: "The application shall be maintainable"
✅ **Good**: "The application code shall maintain: (1) 80% unit test coverage, (2) cyclomatic complexity below 10 per function, (3) no duplicated code blocks over 10 lines, (4) JSDoc comments for all public APIs"

### Inconsistency Examples

**Conflicting Requirements**
❌ Requirement A: "Users must provide phone number during registration"
   Requirement B: "Phone number is optional during account creation"
   → **Conflict**: Mandatory vs optional field

❌ Requirement A (Priority: High): "System must support real-time data synchronization"
   Requirement B (Priority: High): "System processes data in nightly batch jobs only"
   → **Conflict**: Real-time vs batch processing approach

**Terminology Inconsistencies**
❌ Using "User", "Customer", and "Client" interchangeably
✅ Use one consistent term throughout, defined in glossary

❌ Using "Order", "Purchase", and "Transaction" to mean the same thing
✅ Define precise meanings or use one term consistently

### Over-Specification Examples

❌ **Bad**: "The primary action button shall be colored #0066CC with 12px Arial font, 5px padding, 2px border radius, and box-shadow: 0 2px 4px rgba(0,0,0,0.1)"
✅ **Good**: "Primary action buttons shall follow the design system style guide for primary CTAs"

❌ **Bad**: "Error messages shall be displayed in 14pt Helvetica Neue font, colored #D32F2F, with icon error_outline from Material Design icons"
✅ **Good**: "Error messages shall follow the design system error message pattern for visibility and accessibility"

## Acceptance Criteria Formats

### Given-When-Then (Behavior-Driven)

```gherkin
Given: User is logged in with a valid account
  And: User has items in shopping cart
When: User clicks "Place Order" button
Then: Order is created with unique order ID
  And: User receives confirmation email within 5 minutes
  And: Order appears in user's order history within 2 seconds
  And: Inventory is decremented immediately
  And: Payment is processed successfully
```

### Checklist Format

```markdown
**Acceptance Criteria for User Login**

□ User can enter email and password
□ System validates email format before submission
□ System displays error for invalid credentials
□ Successful login redirects to user dashboard
□ Session persists for 30 days with "Remember me"
□ Failed login shows clear error message
□ Account locks after 5 failed attempts
□ Password field masks characters
□ Forgot password link is visible and functional
```

### Scenario-Based Format

```markdown
**Scenario 1: Successful Product Search**
- User enters "laptop" in search box
- System returns results within 2 seconds
- Results show 10-50 relevant products
- Each result displays: image, name, price, rating
- User can filter by price range and brand

**Scenario 2: No Results Found**
- User enters "xyzabc12345" (non-existent)
- System displays "No results found" message
- System suggests: (1) Check spelling, (2) Try different keywords, (3) Browse categories
- Search took less than 2 seconds

**Scenario 3: Partial Match**
- User enters "lapto" (typo)
- System suggests "Did you mean: laptop?"
- Clicking suggestion performs correct search
- Alternative results still shown for "lapto"
```

## Quality Scoring Guidelines

### Scoring Scale (0-100%)

**90-100% (Excellent)**
- All criteria met or exceeded
- No critical or major issues
- Minor issues only (cosmetic, formatting)
- Requirements are production-ready

**70-89% (Good)**
- Most criteria met
- No critical issues
- Few major issues with clear resolution path
- Requirements ready with minor revisions

**50-69% (Fair)**
- Some criteria met
- Few critical issues OR multiple major issues
- Significant revision needed
- Requirements need substantial work

**0-49% (Poor)**
- Many criteria not met
- Multiple critical issues
- Major rework required
- Requirements not ready for next phase

### Issue Severity Definitions

**Critical (Must Fix)**
- Blocks project progress
- Causes major business impact
- Prevents requirement from being implemented
- Creates legal/compliance risk
- Examples: Missing security requirements, fundamental conflicts, incomplete scope

**Major (Should Fix)**
- Significantly impacts quality or implementation
- Causes confusion or ambiguity
- Increases risk of rework
- Affects multiple stakeholders
- Examples: Ambiguous performance criteria, missing acceptance criteria, terminology inconsistencies

**Minor (Nice to Fix)**
- Does not block progress
- Has workarounds available
- Cosmetic or formatting issues
- Minimal impact on implementation
- Examples: Formatting inconsistencies, minor typos, missing diagrams (if content is clear)

## Traceability Assessment

### Backward Traceability
Each requirement should trace to:
□ Business objective or strategic goal
□ Stakeholder need (who requested and why)
□ Business case element (ROI, benefit)
□ Market requirement or competitive analysis
□ Regulatory/compliance mandate (if applicable)
□ User research finding or usability study

### Forward Traceability
Each requirement should trace to:
□ Design elements (architecture diagrams, UI mockups)
□ Architecture components (services, modules, APIs)
□ Implementation tasks or user stories
□ Test cases (unit, integration, acceptance)
□ Validation criteria and acceptance tests
□ Documentation and help content (if user-facing)

### Traceability Matrix Quality
□ Complete coverage - no orphan requirements
□ Unique, consistent identifiers for all items
□ Clear, documented relationships between items
□ Version tracking for requirement changes
□ Impact analysis capability (what breaks if requirement changes)
□ Maintained throughout project lifecycle

### Gap Analysis
Identify and document:
- **Missing Requirements**: Business needs without corresponding requirements
- **Orphan Requirements**: Requirements without business justification
- **Untested Requirements**: Requirements without mapped test cases
- **Undesigned Requirements**: Requirements without design coverage
- **Unstakeholdered Needs**: Potential stakeholder needs not yet captured

## Standards Compliance

### IEEE 830-1998 (SRS Standard)

#### Correct Requirements Characteristics
□ **Unambiguous**: Only one interpretation possible
□ **Complete**: All needed information present
□ **Correct**: Accurately represents stakeholder needs
□ **Understandable**: Clear to all stakeholders
□ **Verifiable**: Can objectively test if met
□ **Consistent**: No internal contradictions
□ **Modifiable**: Structure allows easy changes
□ **Traceable**: Origin and forward path clear
□ **Ranked**: Priority and stability indicated

#### Required SRS Document Structure
□ Introduction (purpose, scope, definitions, references, overview)
□ Overall description (product perspective, functions, user characteristics, constraints)
□ Specific requirements (functional, non-functional, interface)
□ Supporting information (appendices, index)

### Industry-Specific Standards

**Healthcare**
□ HIPAA compliance for data privacy
□ HL7 standards for health information exchange
□ FDA requirements for medical devices
□ ICD-10 coding standards

**Finance**
□ PCI-DSS for payment card data
□ SOX for financial reporting accuracy
□ PSD2 for payment services
□ AML/KYC requirements

**Automotive**
□ ISO 26262 for functional safety
□ AUTOSAR for software architecture
□ ASPICE for process quality

**Aerospace**
□ DO-178C for airborne software
□ DO-254 for airborne hardware

**General Quality/Security**
□ ISO 9001 for quality management
□ ISO 27001 for information security
□ GDPR for data protection (EU)
□ CCPA for consumer privacy (California)

### Agile Best Practices

□ User stories follow INVEST principles
□ Acceptance criteria are specific and testable
□ Definition of Done exists and is followed
□ Stories appropriately sized for sprint
□ Epics properly decomposed into stories
□ Backlog is prioritized and groomed regularly
□ Dependencies identified and managed
□ Technical debt tracked and addressed
