# Requirements Elicitation Techniques

This guide provides comprehensive techniques for gathering requirements from stakeholders, users, and other sources.

## Interview Techniques

### Structured Interviews

**Purpose**: Gather specific, predetermined information

**Best For**:
- Compliance requirements
- Technical specifications
- Data requirements
- Process documentation

**Structure**:
```markdown
# Structured Interview Template

## Pre-Interview Preparation
1. Research stakeholder's role and responsibilities
2. Review existing documentation
3. Prepare specific questions
4. Set clear objectives
5. Schedule adequate time (45-90 minutes)

## Opening (5-10 minutes)
- Introduce yourself and project
- Explain interview purpose
- Set expectations for time
- Ask for permission to record/take notes
- Build rapport

## Question Categories

### Current State Questions
1. What is your role in the organization?
2. What are your primary responsibilities?
3. What tools/systems do you currently use?
4. Walk me through a typical day/workflow
5. What data do you work with?

### Pain Points
6. What are the biggest challenges you face?
7. How much time do these issues cost you?
8. What workarounds have you developed?
9. What would make your job easier?
10. What frustrates you most about current processes?

### Future State
11. What is your vision for the ideal solution?
12. What features are must-haves vs. nice-to-haves?
13. How would you measure success?
14. What business outcomes are you hoping for?
15. What would make this project a failure?

### Technical Requirements
16. What systems need to integrate?
17. What data needs to be accessed?
18. Are there performance requirements?
19. What security/compliance requirements exist?
20. What browsers/devices must be supported?

### Constraints
21. What is the budget?
22. What is the timeline?
23. Who are the key decision-makers?
24. Are there any technical constraints?
25. What are the regulatory requirements?

## Closing (5 minutes)
- Summarize key points
- Confirm understanding
- Ask if anything was missed
- Request follow-up availability
- Ask for referrals to other stakeholders
- Thank them for their time

## Post-Interview
- Send notes within 24 hours
- Confirm key decisions
- Document requirements
- Identify gaps
- Schedule follow-ups if needed
```

**Example Questions**:
```markdown
Process Questions:
- "Walk me through how you handle a customer order from start to finish"
- "What happens when an exception occurs?"
- "How do you handle rush orders?"

Data Questions:
- "What reports do you generate?"
- "Where does this data come from?"
- "How often is this data updated?"

Problem Questions:
- "On a scale of 1-10, how big is this problem?"
- "How much time/money does this cost?"
- "What would happen if this wasn't fixed?"

Future State Questions:
- "If you had a magic wand, what would the solution look like?"
- "What would success look like 6 months from now?"
- "How would this change your daily work?"
```

### Semi-Structured Interviews

**Purpose**: Balance preparation with flexibility

**Best For**:
- Exploratory research
- User needs discovery
- Innovation ideation
- Complex domains

**Structure**:
```markdown
# Semi-Structured Interview Approach

## Preparation
- Prepare key themes/topics
- Have core questions ready
- Allow room for exploration
- Be ready to follow interesting threads

## Flexible Question Flow

Start with open questions:
- "Tell me about your experience with..."
- "How do you typically..."
- "What challenges do you face when..."

Follow up with probing:
- "Can you tell me more about that?"
- "Why is that important?"
- "Can you give me an example?"
- "What would happen if..."

Adapt based on responses:
- Follow unexpected insights
- Dig deeper into pain points
- Explore mentioned workarounds
- Ask about edge cases

## Active Listening Techniques
- Pause before responding
- Summarize what you heard
- Ask clarifying questions
- Watch for non-verbal cues
- Note emotional responses
```

**Five Whys Technique**:
```markdown
# Example: Understanding Root Cause

Problem: Sales team wants a faster quote system

Interview dialogue:
Q: "Why do you need a faster quote system?"
A: "Because customers are waiting too long for quotes"

Q: "Why are customers waiting too long?"
A: "Because it takes 2-3 days to generate a quote"

Q: "Why does it take 2-3 days?"
A: "Because we have to manually look up prices in multiple systems"

Q: "Why do you have to manually look up prices?"
A: "Because our CRM doesn't integrate with the pricing system"

Q: "Why doesn't the CRM integrate with pricing?"
A: "Because they're on different platforms with no API connection"

Root Cause Identified: Lack of system integration
Real Requirement: API integration between CRM and pricing system
```

### Unstructured Interviews

**Purpose**: Explore unknown territory, build relationships

**Best For**:
- Initial discovery
- Understanding culture
- Building stakeholder relationships
- Innovative solutions

**Approach**:
```markdown
# Unstructured Interview Guidelines

Start with broad, open questions:
- "Tell me about your work"
- "What's on your mind these days?"
- "What keeps you up at night?"

Let conversation flow naturally:
- Follow the stakeholder's lead
- Don't force your agenda
- Listen more than talk (80/20 rule)
- Observe their environment
- Note what they emphasize

Look for:
- Repeated themes
- Emotional reactions
- Workarounds they've created
- Wish lists and frustrations
- Contradictions
```

## Workshop Techniques

### Requirements Workshop

**Purpose**: Collaborative requirements definition with multiple stakeholders

**Setup**:
```markdown
# Workshop Planning Checklist

□ Define clear objectives
□ Identify right participants (6-12 people)
□ Book appropriate space (in-person or virtual)
□ Schedule 2-4 hours
□ Prepare materials and tools
□ Send pre-read materials 2 days before
□ Arrange for refreshments (if in-person)
□ Set up virtual collaboration tools (if remote)

Participants Should Include:
- Business stakeholders (decision-makers)
- End users (actual system users)
- Technical representatives
- Business analyst (facilitator)
- Subject matter experts
- QA representative
```

**Facilitation Techniques**:

**1. Round Robin**
```markdown
Purpose: Ensure all voices are heard

Process:
1. Pose a question
2. Go around the room
3. Each person shares one idea
4. No discussion yet
5. Repeat until ideas exhausted
6. Then discuss as group

Good for:
- Brainstorming features
- Identifying risks
- Gathering concerns
```

**2. Silent Brainstorming**
```markdown
Purpose: Generate many ideas quickly

Process:
1. Present the prompt/question
2. Everyone writes ideas on sticky notes (5-10 minutes)
3. Post all sticky notes on wall
4. Group similar ideas
5. Discuss and prioritize

Benefits:
- Introverts can contribute
- No groupthink
- Many ideas quickly
- Democratic participation
```

**3. Affinity Diagramming**
```markdown
Purpose: Organize large amounts of information

Process:
1. Collect all ideas (sticky notes)
2. Silently group related items
3. Name each group
4. Discuss groupings
5. Create hierarchy if needed

Example:
User Management
├── Registration
├── Login
├── Profile
└── Permissions

Product Catalog
├── Browse
├── Search
├── Details
└── Compare
```

**4. Dot Voting**
```markdown
Purpose: Prioritize democratically

Process:
1. List all options on wall
2. Give each person 3-5 votes (dots)
3. People place dots on their priorities
4. Can put multiple dots on one item
5. Tally votes
6. Discuss results

Variations:
- Use different colored dots (Must Have, Should Have)
- Give stakeholders more votes
- Do multiple rounds
```

### Design Thinking Workshop

**Purpose**: User-centered problem solving and ideation

**5-Phase Process**:

```markdown
# Design Thinking Workshop (Full Day)

## Phase 1: Empathize (1 hour)
Understand user needs and pain points

Activities:
- Review user research
- Share user stories
- Create empathy maps
- Watch user videos

Output: Deep understanding of user problems

## Phase 2: Define (1 hour)
Frame the problem clearly

Activities:
- Synthesize research findings
- Identify patterns
- Create problem statements
- Define success criteria

Output: Clear problem definition
Example: "Sales reps need a way to generate quotes on-site 
because current 2-3 day turnaround causes lost deals"

## Phase 3: Ideate (1.5 hours)
Generate many solution ideas

Activities:
- Brainstorm solutions (quantity over quality)
- "How Might We" questions
- Crazy 8s exercise
- SCAMPER technique

Output: 30-50 solution ideas

## Phase 4: Prototype (2 hours)
Create low-fidelity prototypes

Activities:
- Paper prototyping
- Storyboarding
- Wireframing
- Create scenarios

Output: Testable prototypes

## Phase 5: Test (1.5 hours)
Get feedback on prototypes

Activities:
- User testing with prototypes
- Gather feedback
- Iterate based on learnings
- Refine requirements

Output: Validated requirements and concepts
```

**How Might We Questions**:
```markdown
Problem: Sales reps can't access CRM in the field

Generate HMW questions:
- How might we make CRM accessible anywhere?
- How might we eliminate the need for desktop access?
- How might we enable offline CRM use?
- How might we sync data seamlessly?
- How might we simplify mobile data entry?

Each HMW becomes a potential solution direction
```

### JAD (Joint Application Design) Sessions

**Purpose**: Intensive collaborative requirements definition

**Structure**:
```markdown
# JAD Session Plan (2-5 days)

Day 1: Scope and Context
- Project overview
- Current state review
- Stakeholder presentations
- Define scope boundaries

Day 2: Requirements Elicitation
- Process modeling
- Use case development
- Data modeling
- Business rules

Day 3: Solution Design
- Prototype review
- Design alternatives
- Technology discussion
- Integration points

Day 4: Validation and Refinement
- Review all requirements
- Resolve conflicts
- Prioritize features
- Identify risks

Day 5: Documentation and Sign-off
- Finalize documentation
- Review action items
- Obtain approvals
- Plan next steps

Outputs:
- Comprehensive requirements document
- Process models
- Data models
- Prototypes
- Signed approvals
```

## Observation Techniques

### Job Shadowing

**Purpose**: Understand actual work practices

**Process**:
```markdown
# Job Shadowing Plan

## Preparation
1. Identify representative users
2. Schedule 2-4 hour sessions
3. Explain shadowing purpose
4. Set expectations (minimal interruption)
5. Prepare observation checklist

## During Observation

What to Watch:
□ Actual vs. documented process
□ Workarounds being used
□ Pain points and frustrations
□ Time spent on tasks
□ Tools and systems used
□ Communication patterns
□ Decision-making process
□ Error handling
□ Context switching

Questions to Ask:
- "Why did you do that?"
- "How often does this happen?"
- "What happens when...?"
- "Is this the usual way?"
- "What would make this easier?"

What to Document:
- Screenshots of systems
- Workflow diagrams
- Time estimates
- Pain points
- Workarounds
- Quotes from users
- Environmental factors

## Post-Observation
1. Write up detailed notes
2. Create workflow diagrams
3. Identify requirements
4. Validate findings with user
5. Share insights with team
```

**Example Observations**:
```markdown
# Job Shadowing Notes: Sales Representative

Date: January 14, 2026
User: Sarah, 5 years experience
Duration: 3 hours

Key Observations:

1. Manual Data Entry (30 min observed)
   - Copies data from emails to CRM
   - Uses 3 different tabs
   - Re-enters same customer info multiple times
   - Frequent typos, has to correct
   
   Requirement: Auto-populate from email integration

2. Quote Generation (45 min observed)
   - Opens Excel template
   - Manually looks up prices in separate system
   - Calculates discounts with calculator
   - Formats in Word
   - Emails to manager for approval
   
   Requirement: Integrated quote builder with approval workflow

3. Workarounds Noticed
   - Keeps sticky notes with common product codes
   - Has personal spreadsheet of customer preferences
   - Takes photos of handwritten meeting notes
   
   Requirement: Quick reference, note capture, OCR

4. Pain Points
   - Can't access CRM on phone (mentioned 3x)
   - Forgets to update records (loses information)
   - Doesn't know when quotes are approved
   
   Requirements: Mobile app, notifications, status tracking
```

### Ethnographic Research

**Purpose**: Understand users in their natural environment

**Techniques**:
```markdown
# Ethnographic Research Approach

## Immersion
- Spend extended time with users (days/weeks)
- Become part of their environment
- Observe natural behavior
- Understand culture and context

## Participant Observation
- Not just watching, but participating
- Experience the work firsthand
- Understand unspoken rules
- Feel the pain points personally

## Field Notes
- Detailed observations
- Context and environment
- User emotions and reactions
- Unexpected behaviors
- Cultural norms

## Artifacts Collection
- Take photos (with permission)
- Collect work samples
- Document tools and systems
- Note physical workspace setup
```

### Contextual Inquiry

**Purpose**: Observe and interview simultaneously

**Four Principles**:

**1. Context**: Work in user's actual environment
```markdown
- Go to their workspace
- Use their equipment
- Experience their interruptions
- Understand their constraints
```

**2. Partnership**: User as expert, analyst as apprentice
```markdown
- Let user lead
- Ask questions as apprentice
- "Teach me how you do this"
- Validate understanding
```

**3. Interpretation**: Develop shared understanding
```markdown
- Discuss observations together
- "It seems like you do X because Y, is that right?"
- Confirm or correct interpretations
- Build shared mental model
```

**4. Focus**: Keep inquiry goal-directed
```markdown
- Stay aligned with project goals
- Explore relevant areas deeply
- Politely redirect tangents
- Balance breadth and depth
```

**Example Session**:
```markdown
# Contextual Inquiry Session

User: Customer Service Representative
Location: Call center
Duration: 2 hours

Approach:
1. Observe rep handling calls
2. Ask questions between calls
3. Have rep explain their thinking
4. Validate interpretations

Sample Dialogue:
BA: "I noticed you had three screens open. Can you show me what each one is for?"
Rep: "This one is the CRM, this is the order system, and this is the knowledge base."

BA: "Why do you need three different systems?"
Rep: "The CRM doesn't have order info, and the order system doesn't have customer history."

BA: "How does that impact your work?"
Rep: "I have to switch between screens constantly. Sometimes I give customers wrong info because I'm looking at the wrong screen."

BA: "It sounds like having all information in one place would help. Is that right?"
Rep: "Yes! That would save me so much time and reduce errors."

Requirement Identified: Unified customer view with order history
```

## Document Analysis

### Types of Documents to Analyze

**Business Documents**:
```markdown
- Business strategy documents
- Annual reports
- Market research
- Competitor analysis
- Customer feedback/surveys
- Support tickets
- Sales reports
```

**Process Documents**:
```markdown
- Process maps and workflows
- Standard Operating Procedures (SOPs)
- Policy manuals
- Training materials
- Work instructions
- Audit reports
```

**Technical Documents**:
```markdown
- System documentation
- API specifications
- Database schemas
- Architecture diagrams
- Technical specifications
- Change logs
- Incident reports
```

**Regulatory Documents**:
```markdown
- Industry regulations
- Compliance requirements
- Legal contracts
- SLAs (Service Level Agreements)
- Privacy policies
- Security standards
```

### Document Analysis Process

```markdown
# Document Analysis Checklist

## 1. Collect Documents
□ Request from stakeholders
□ Search shared drives
□ Review previous projects
□ Check regulatory sources
□ Gather competitive intelligence

## 2. Review and Categorize
□ Skim for relevance
□ Organize by topic
□ Note version and date
□ Identify authors
□ Flag inconsistencies

## 3. Extract Requirements

Business Rules:
- "Orders over $1000 require manager approval"
- "Customers get 10% discount after 5 purchases"
- "Refunds must be processed within 48 hours"

Data Requirements:
- Customer: ID, Name, Email, Phone, Address
- Order: ID, Date, Status, Total, Items
- Product: SKU, Name, Description, Price, Stock

Process Steps:
1. Customer places order
2. System validates inventory
3. Payment is processed
4. Order is shipped
5. Customer receives confirmation

Constraints:
- Must comply with GDPR
- Must support 10,000 concurrent users
- Must integrate with SAP ERP
- Must be mobile-responsive

## 4. Validate Findings
□ Confirm with stakeholders
□ Check if still current
□ Resolve ambiguities
□ Update outdated info
□ Document assumptions

## 5. Document Gaps
□ Missing information
□ Unclear requirements
□ Conflicting information
□ Outdated content
□ Areas needing clarification
```

## Prototyping

### Prototype Fidelity Levels

**1. Paper Prototypes (Low Fidelity)**
```markdown
Purpose: Quick exploration of concepts

Materials:
- Paper and markers
- Sticky notes
- Index cards
- Scissors

Benefits:
- Very fast to create (minutes)
- Easy to change
- No technical skills needed
- Forces focus on concepts
- Stakeholders feel comfortable giving feedback

Process:
1. Sketch screens on paper
2. Create interactive elements on sticky notes
3. Move pieces to simulate navigation
4. Have user "click" with finger
5. Change based on feedback

Best for:
- Initial concept validation
- Workshop exercises
- Rapid iteration
- Early stakeholder feedback
```

**2. Wireframes (Low-Medium Fidelity)**
```markdown
Purpose: Define layout and functionality

Tools:
- Balsamiq
- Sketch
- Figma
- Adobe XD
- PowerPoint/Keynote

Characteristics:
- Grayscale or limited color
- Placeholder content
- Basic interactions
- Focus on structure, not style

Benefits:
- Clarifies functionality
- Defines information architecture
- Shows user flow
- Documents requirements visually

Best for:
- Defining page layouts
- User flow documentation
- Developer handoff
- Requirements validation
```

**3. Mockups (High Fidelity)**
```markdown
Purpose: Show final visual design

Characteristics:
- Real colors and branding
- Actual content (or realistic)
- Detailed styling
- High visual polish

Benefits:
- Looks like real product
- Tests visual design
- Marketing and sales use
- Stakeholder buy-in

Tools:
- Figma
- Sketch
- Adobe XD
- Photoshop

Best for:
- Design approval
- Usability testing
- Documentation
- Marketing materials
```

**4. Interactive Prototypes (High Fidelity)**
```markdown
Purpose: Simulate actual product

Tools:
- InVision
- Figma
- Adobe XD
- Axure
- Proto.io

Features:
- Clickable elements
- Navigation between screens
- Animations and transitions
- Form interactions
- Conditional logic

Benefits:
- Realistic user testing
- Demonstrates functionality
- Identifies usability issues
- Validates requirements

Best for:
- Usability testing
- Stakeholder demonstrations
- Requirements validation
- Developer reference
```

### Prototyping Best Practices

```markdown
# Prototype Creation Guidelines

## Start Low Fidelity
- Begin with sketches
- Test concepts quickly
- Fail fast and cheap
- Don't invest in wrong direction

## Increment Fidelity
- Add detail as you validate
- Match fidelity to certainty
- Don't polish prematurely

## Make It Realistic Enough
- Use real content when possible
- Reflect actual complexity
- Include edge cases
- Show error states

## Test Early and Often
- Get feedback on rough drafts
- Test with actual users
- Observe, don't explain
- Iterate based on learnings

## Use Appropriate Tools
- Match tool to fidelity level
- Don't over-engineer
- Learn tool before workshop
- Have backups ready

## Document Feedback
- Record test sessions
- Take notes on reactions
- Track common issues
- Prioritize changes
```

## Survey and Questionnaire Techniques

### Survey Design

```markdown
# Survey Design Best Practices

## Define Objectives
- What do you need to know?
- Who should respond?
- How will you use results?

## Keep It Short
- Under 10 minutes to complete
- 10-15 questions maximum
- Progress indicator
- Save and return later option

## Question Types

1. Multiple Choice (Easy to analyze)
   How often do you use the mobile app?
   ○ Daily
   ○ 2-3 times per week
   ○ Weekly
   ○ Monthly
   ○ Rarely
   ○ Never

2. Rating Scale (Quantitative feedback)
   Rate your satisfaction with quote generation:
   1 ☆ - 2 ☆ - 3 ☆ - 4 ☆ - 5 ☆
   Very Dissatisfied ←→ Very Satisfied

3. Ranking (Prioritization)
   Rank these features by importance (1=most important):
   ___ Mobile access
   ___ Faster quotes
   ___ Better reporting
   ___ Email integration

4. Open-Ended (Rich feedback, harder to analyze)
   What is your biggest challenge with the current system?
   [text box]

## Question Writing Tips
- Use clear, simple language
- Avoid leading questions
- One concept per question
- Avoid negatives
- Provide "Other" options
- Include "N/A" when appropriate

## Question Order
1. Start with easy, engaging questions
2. Group related questions
3. Put demographics at end
4. Save sensitive questions for last

## Testing
- Pilot with 5-10 people
- Check for:
  - Confusing questions
  - Missing options
  - Technical issues
  - Appropriate length
  - Question flow
```

### Survey Distribution

```markdown
# Distribution Strategy

## Choose Right Channel
- Email (if you have addresses)
- In-app popup (current users)
- Website banner
- Social media
- QR codes (physical locations)

## Maximize Response Rate
- Explain purpose and value
- Estimate completion time
- Offer incentive if appropriate
- Send reminders (not too many)
- Thank respondents
- Share results

## Timing
- Avoid holidays and vacations
- Consider time zones
- Allow 1-2 weeks for responses
- Send mid-week, mid-morning

## Sample Size
- Aim for statistical significance
- Consider population size
- Calculate confidence level
- 100+ responses for most surveys
```

## Elicitation Technique Selection Matrix

```markdown
| Technique | Best For | Time Required | Stakeholder Involvement | Detail Level |
|-----------|----------|---------------|------------------------|--------------|
| Structured Interview | Specific info, compliance | 1-2 hours | Low (1-2 people) | High |
| Semi-Structured Interview | Exploration, complex topics | 1-2 hours | Low (1-2 people) | Medium-High |
| Requirements Workshop | Consensus, prioritization | 2-4 hours | High (6-12 people) | Medium |
| Job Shadowing | Understanding workflows | 2-8 hours | Low (observe 1-2) | High |
| Survey | Large user base, quantitative | 1 week | High (many responses) | Low-Medium |
| Document Analysis | Existing rules, data models | 1-3 days | None (solo activity) | Medium-High |
| Prototyping | Design validation, usability | 1-2 weeks | Medium (review sessions) | Visual |
| Focus Group | Diverse perspectives | 2-3 hours | Medium (6-8 people) | Medium |
```

## Best Practices Across All Techniques

1. **Prepare Thoroughly**
   - Research beforehand
   - Have clear objectives
   - Prepare materials
   - Test technology

2. **Build Rapport**
   - Start with small talk
   - Explain purpose clearly
   - Show genuine interest
   - Respect their time

3. **Listen Actively**
   - Don't interrupt
   - Ask clarifying questions
   - Paraphrase to confirm
   - Note non-verbal cues

4. **Document Everything**
   - Take detailed notes
   - Record if permitted
   - Capture exact quotes
   - Document context

5. **Follow Up**
   - Send summary within 24 hours
   - Confirm understanding
   - Thank participants
   - Share how input was used

6. **Validate Continuously**
   - Confirm understanding
   - Check with multiple sources
   - Test with prototypes
   - Iterate based on feedback
