# Mainframe/Midrange Modernization - Enhancement Proposal

## Executive Summary

This document proposes enhancements to Hanoi Rainbow to support mainframe and midrange system migration and modernization initiatives. These additions will enable teams to systematically migrate legacy systems (IBM Mainframe, IBM i/AS400, Unix midrange) to modern technology stacks using the Spec-Driven Development methodology.

---

## 1. New Rainbow Commands

### 1.1 `/rainbow.assess-legacy`
**Purpose**: Analyze existing legacy systems to understand current state before modernization

**Description**: Systematically analyze legacy codebases, dependencies, business logic, and data structures to create a comprehensive assessment report.

**Key Activities**:
- Parse legacy code (COBOL, RPG, PL/I, JCL, CL)
- Identify business logic and data flows
- Map dependencies and integration points
- Assess complexity and technical debt
- Identify reusable vs. replaceable components
- Generate modernization readiness score

**Output**: `specs/<feature>/legacy-assessment.md` containing:
- Code inventory (LOC, file counts, languages)
- Business capability map
- Data model analysis
- Integration landscape
- Complexity heatmap
- Recommended modernization approach

**Auto Commit**: `docs:` prefix (e.g., "docs: add legacy system assessment for feature-name")

---

### 1.2 `/rainbow.map-business-logic`
**Purpose**: Extract and document business rules from legacy code

**Description**: Analyze legacy code to extract embedded business rules, validations, calculations, and workflows, documenting them in technology-agnostic format for reimplementation.

**Key Activities**:
- Extract business rules from COBOL/RPG paragraphs, copybooks, and screen layouts
- Identify validation logic and data transformations
- Document calculation formulas
- Map workflow sequences
- Identify undocumented business logic
- Create decision tables and flow diagrams

**Output**: `specs/<feature>/business-logic-map.md` containing:
- Business rules catalog
- Data validation rules
- Calculation formulas
- Workflow diagrams
- Decision tables
- Edge cases and exceptions

**Auto Commit**: `docs:` prefix

---

### 1.3 `/rainbow.design-data-migration`
**Purpose**: Create comprehensive data migration strategy from legacy databases

**Description**: Design detailed data migration approach for moving from mainframe databases (DB2, IMS, VSAM, IDMS) or midrange databases to modern data stores.

**Key Activities**:
- Analyze source data structures (hierarchical, sequential, indexed)
- Design target schema (normalized relational or NoSQL)
- Create data transformation rules
- Plan for data quality issues
- Define cutover strategy
- Design reconciliation approach

**Output**: `specs/<feature>/data-migration.md` containing:
- Source-to-target mapping
- Transformation logic
- Data cleansing rules
- Migration tooling recommendations
- Testing strategy
- Rollback procedures

**Auto Commit**: `docs:` prefix

---

### 1.4 `/rainbow.convert-batch`
**Purpose**: Design modern equivalents for mainframe batch processing

**Description**: Analyze JCL/batch jobs and design modern replacements using cloud-native batch processing, event-driven architectures, or streaming platforms.

**Key Activities**:
- Catalog batch jobs and schedules
- Analyze dependencies between jobs
- Identify critical vs. nice-to-have jobs
- Design modern alternatives (Kubernetes Jobs, Lambda, Azure Batch, Airflow)
- Plan for job orchestration
- Define monitoring and alerting

**Output**: `specs/<feature>/batch-modernization.md` containing:
- Batch job inventory
- Dependency graph
- Modern architecture design
- Orchestration strategy
- Error handling approach
- Monitoring and observability plan

**Auto Commit**: `docs:` prefix

---

### 1.5 `/rainbow.design-screen-migration`
**Purpose**: Convert green-screen/5250 interfaces to modern UI/UX

**Description**: Analyze legacy screen definitions (CICS maps, DDS, RPG display files) and design modern web/mobile interfaces that preserve business workflows while improving user experience.

**Key Activities**:
- Extract screen layouts and navigation flows
- Identify input validations and field edits
- Map function keys to modern UI patterns
- Design improved user workflows
- Create wireframes and mockups
- Plan for accessibility and responsive design

**Output**: `specs/<feature>/screen-migration.md` containing:
- Legacy screen inventory
- Screen-to-screen navigation map
- Modern UI wireframes
- User workflow improvements
- Accessibility requirements
- Design system integration

**Auto Commit**: `docs:` prefix

---

### 1.6 `/rainbow.plan-strangler`
**Purpose**: Create detailed strangler pattern implementation plan

**Description**: Design phased migration using the strangler pattern to incrementally replace legacy system components while maintaining business continuity.

**Key Activities**:
- Identify bounded contexts and service boundaries
- Prioritize components for extraction
- Design routing/proxy layer
- Plan for dual-write scenarios
- Define phase gates and validation criteria
- Create rollback strategies

**Output**: `specs/<feature>/strangler-plan.md` containing:
- Component extraction sequence
- Service boundary definitions
- Routing architecture
- Data synchronization strategy
- Phase-by-phase roadmap
- Risk mitigation plans

**Auto Commit**: `docs:` prefix

---

### 1.7 `/rainbow.validate-modernization`
**Purpose**: Verify that modernized system matches legacy behavior

**Description**: Create comprehensive validation strategy to ensure the modern system replicates critical legacy functionality, with special focus on regulatory compliance and financial accuracy.

**Key Activities**:
- Design parallel run strategy
- Create data reconciliation approach
- Define acceptance criteria
- Plan for regression testing
- Identify edge cases from legacy
- Design compliance validation

**Output**: `specs/<feature>/modernization-validation.md` containing:
- Parallel run plan
- Reconciliation procedures
- Test scenarios (functional, integration, performance)
- Compliance checklist
- User acceptance criteria
- Go-live readiness criteria

**Auto Commit**: `test:` prefix

---

## 2. New Specialized Agents

### 2.1 `/hanoi.legacy-analyst`
**Role**: Legacy System Analysis Specialist

**Description**: Expert in analyzing legacy codebases (COBOL, RPG, PL/I, JCL, Assembler) to extract business logic, understand data structures, and assess modernization complexity.

**Capabilities**:
- Parse and analyze legacy code syntax
- Identify business rules embedded in code
- Map data flows and dependencies
- Assess code quality and technical debt
- Identify modernization candidates
- Generate comprehension documentation

**Deliverables**:
- Code analysis reports
- Business logic extraction documents
- Dependency maps
- Complexity assessments
- Modernization recommendations

---

### 2.2 `/hanoi.mainframe-architect`
**Role**: Mainframe Modernization Architect

**Description**: Specialized system architect with deep expertise in mainframe/midrange systems and modern cloud-native architectures. Bridges legacy and modern worlds.

**Capabilities**:
- Design mainframe-to-cloud migration strategies
- Recommend appropriate modernization patterns (rehost, replatform, refactor, replace)
- Design hybrid architectures (mainframe + cloud)
- Plan for mainframe offloading strategies
- Ensure regulatory compliance during migration
- Optimize for performance and cost

**Deliverables**:
- Mainframe modernization architecture documents
- Technology mapping (legacy → modern)
- Hybrid integration patterns
- Performance optimization plans
- Cost-benefit analysis

---

### 2.3 `/hanoi.data-migration-engineer`
**Role**: Data Migration Specialist

**Description**: Expert in migrating data from legacy databases (DB2, IMS, VSAM, IDMS) to modern data platforms (PostgreSQL, SQL Server, MongoDB, cloud data warehouses).

**Capabilities**:
- Analyze legacy data models
- Design target schemas
- Create ETL/ELT pipelines
- Plan data quality improvements
- Design reconciliation strategies
- Optimize migration performance

**Deliverables**:
- Data migration specifications
- ETL/ELT pipeline designs
- Data mapping documents
- Quality assurance plans
- Performance tuning guides

---

### 2.4 `/hanoi.batch-modernization-engineer`
**Role**: Batch Processing Modernization Specialist

**Description**: Expert in converting mainframe batch processing (JCL, batch COBOL) to modern batch architectures (Kubernetes Jobs, serverless, streaming).

**Capabilities**:
- Analyze JCL and batch job dependencies
- Design modern batch orchestration
- Convert batch to streaming where appropriate
- Plan for job scheduling and monitoring
- Optimize batch performance
- Design error handling and recovery

**Deliverables**:
- Batch modernization designs
- Job orchestration specifications
- Monitoring and alerting strategies
- Performance benchmarks

---

### 2.5 `/hanoi.cobol-translator`
**Role**: COBOL/RPG Code Translator

**Description**: Specialist in translating COBOL, RPG, and PL/I business logic to modern programming languages (Java, C#, Python, Go) while preserving exact behavior.

**Capabilities**:
- Parse COBOL/RPG/PL/I syntax
- Extract business logic patterns
- Generate equivalent modern code
- Preserve calculation precision (decimal arithmetic)
- Maintain regulatory compliance
- Create unit tests to validate equivalence

**Deliverables**:
- Translated code modules
- Translation mapping documents
- Unit test suites
- Behavior validation reports

---

### 2.6 `/hanoi.mainframe-tester`
**Role**: Legacy System Testing Specialist

**Description**: Expert in testing mainframe applications and validating modernized replacements for functional equivalence, performance, and compliance.

**Capabilities**:
- Design parallel run strategies
- Create data reconciliation procedures
- Develop regression test suites
- Validate financial calculations
- Test integration points
- Verify compliance requirements

**Deliverables**:
- Test strategies and plans
- Automated test suites
- Reconciliation reports
- Compliance validation documents

---

## 3. New Templates

### 3.1 `legacy-assessment-template.md`
**Purpose**: Document comprehensive legacy system analysis

**Sections**:
- Executive Summary
- System Inventory
  - Hardware/Software Configuration
  - Programming Languages & Versions
  - Databases & File Systems
  - Integration Points
- Code Analysis
  - Lines of Code by Language
  - Complexity Metrics
  - Code Quality Assessment
  - Technical Debt Inventory
- Business Logic Catalog
  - Business Capabilities
  - Business Rules
  - Critical Processes
- Data Architecture
  - Data Models
  - Data Volumes
  - Data Quality Issues
- Integration Landscape
  - External Systems
  - APIs and Interfaces
  - Batch Interfaces
- Non-Functional Characteristics
  - Performance Baselines
  - Availability Requirements
  - Security & Compliance
- Modernization Readiness
  - Complexity Score
  - Risk Assessment
  - Recommended Approach
  - Estimated Effort

---

### 3.2 `business-logic-extraction-template.md`
**Purpose**: Document business rules extracted from legacy code

**Sections**:
- Overview
- Business Rules Catalog
  - Rule ID, Description, Source Location, Criticality
- Data Validations
  - Field Edits
  - Cross-Field Validations
  - Format Validations
- Calculations & Formulas
  - Financial Calculations
  - Tax Calculations
  - Derived Fields
- Business Workflows
  - Process Flows
  - State Machines
  - Decision Trees
- Exception Handling
  - Error Conditions
  - Compensating Transactions
- Regulatory Requirements
  - Compliance Rules
  - Audit Requirements

---

### 3.3 `data-migration-specification-template.md`
**Purpose**: Detailed data migration plan

**Sections**:
- Executive Summary
- Source System Analysis
  - Data Models (hierarchical/relational/sequential)
  - Data Volumes
  - Data Quality Assessment
- Target System Design
  - Target Schema
  - Normalization Strategy
  - Partitioning/Sharding Strategy
- Data Mapping
  - Source-to-Target Mapping Tables
  - Transformation Rules
  - Data Type Conversions
- Data Cleansing
  - Quality Issues to Address
  - Cleansing Rules
  - Exception Handling
- Migration Approach
  - One-time vs. Incremental
  - Cutover Strategy
  - Synchronization Approach
- ETL/ELT Pipeline
  - Tool Selection
  - Pipeline Architecture
  - Error Handling
- Testing & Validation
  - Reconciliation Procedures
  - Data Quality Checks
  - Performance Validation
- Rollback Plan
- Post-Migration Optimization

---

### 3.4 `batch-modernization-template.md`
**Purpose**: Design modern replacements for batch processing

**Sections**:
- Batch Job Inventory
  - Job Name, Frequency, Runtime, Dependencies
- Dependency Analysis
  - Job Dependency Graph
  - Critical Path
- Modernization Options
  - Kubernetes Jobs
  - Serverless Functions
  - Event-Driven Architecture
  - Stream Processing
- Target Architecture
  - Job Orchestration (Airflow, Temporal, Azure Batch)
  - Execution Environment
  - Resource Management
- Migration Strategy
  - Job-by-Job vs. Wholesale Replacement
  - Phased Approach
- Monitoring & Observability
  - Metrics Collection
  - Alerting Strategy
  - Log Aggregation
- Error Handling & Recovery
  - Retry Logic
  - Dead Letter Queues
  - Incident Response
- Performance Optimization
  - Parallelization Strategy
  - Resource Tuning
  - Cost Optimization

---

### 3.5 `screen-migration-specification-template.md`
**Purpose**: Convert green-screen interfaces to modern UI

**Sections**:
- Legacy Screen Inventory
  - Screen ID, Function, Usage Frequency
- Screen Analysis
  - Layout Analysis
  - Field Definitions
  - Validations
  - Navigation Flow
- User Research
  - Pain Points
  - Desired Improvements
  - User Personas
- Modern UI Design
  - Wireframes
  - User Workflows
  - Responsive Design
  - Accessibility Requirements
- Screen Mapping
  - Legacy-to-Modern Screen Mapping
  - Navigation Changes
  - Workflow Improvements
- API Requirements
  - Backend Services Needed
  - Data Requirements
- Testing Strategy
  - Usability Testing
  - A/B Testing
  - Accessibility Testing

---

### 3.6 `strangler-pattern-implementation-template.md`
**Purpose**: Detailed strangler pattern migration plan

**Sections**:
- Overview
- Bounded Context Analysis
  - Domain Boundaries
  - Service Candidates
- Prioritization
  - Extraction Sequence
  - Risk vs. Value Matrix
- Routing Architecture
  - Proxy/Gateway Design
  - Request Routing Rules
  - Session Management
- Phase Plans (for each phase)
  - Components to Extract
  - Service Design
  - Data Synchronization
  - Testing Strategy
  - Rollback Plan
  - Success Criteria
- Integration Patterns
  - Legacy-to-Modern Communication
  - Modern-to-Legacy Communication
  - Event Propagation
- Monitoring Strategy
  - Health Checks
  - Performance Metrics
  - Error Tracking

---

### 3.7 `mainframe-to-cloud-mapping-template.md`
**Purpose**: Map mainframe components to cloud equivalents

**Sections**:
- Component Mapping
  - CICS → Modern App Server/Container
  - IMS → Modern Database/Message Queue
  - DB2 → Cloud Database
  - VSAM → Cloud Storage/Database
  - JCL Jobs → Cloud Batch Service
  - MQ Series → Cloud Message Queue
- Technology Recommendations
  - Compute (VMs, Containers, Serverless)
  - Storage (Block, Object, Database)
  - Networking (VPC, Load Balancers, API Gateway)
  - Batch (Kubernetes Jobs, Azure Batch, AWS Batch)
  - Integration (Event Grid, Service Bus, SQS)
- Cost Analysis
  - TCO Comparison
  - Ongoing Operational Costs
- Performance Considerations
  - Latency Analysis
  - Throughput Requirements
  - Optimization Strategies

---

## 4. New Checklists

### 4.1 `mainframe-assessment-checklist.md`
**Purpose**: Ensure comprehensive legacy system assessment

**Checklist Items**:
- [ ] Hardware/software inventory completed
- [ ] All programming languages identified and versioned
- [ ] Source code repository established
- [ ] Database schemas documented
- [ ] File layouts (VSAM, sequential) documented
- [ ] Integration points mapped
- [ ] Batch job inventory completed
- [ ] Screen/report inventory completed
- [ ] Business capability map created
- [ ] Critical business rules extracted
- [ ] Performance baselines established
- [ ] Security requirements documented
- [ ] Compliance requirements identified
- [ ] Technical debt assessed
- [ ] SME interviews conducted
- [ ] Documentation reviewed
- [ ] Modernization readiness scored

---

### 4.2 `data-migration-readiness-checklist.md`
**Purpose**: Validate readiness for data migration

**Checklist Items**:
- [ ] Source data model documented
- [ ] Target schema designed and reviewed
- [ ] Data volumes quantified
- [ ] Data quality issues identified
- [ ] Transformation rules defined
- [ ] ETL/ELT pipeline designed
- [ ] Test data created
- [ ] Reconciliation procedures defined
- [ ] Performance testing completed
- [ ] Rollback procedures documented
- [ ] Cutover plan approved
- [ ] Team trained on migration tools
- [ ] Monitoring dashboards configured
- [ ] Stakeholder communication plan ready

---

### 4.3 `batch-modernization-checklist.md`
**Purpose**: Ensure complete batch job modernization

**Checklist Items**:
- [ ] All batch jobs inventoried
- [ ] Job dependencies mapped
- [ ] Critical path identified
- [ ] Target orchestration platform selected
- [ ] Job templates created
- [ ] Scheduling strategy defined
- [ ] Error handling designed
- [ ] Monitoring configured
- [ ] Alerting rules established
- [ ] Logging aggregation setup
- [ ] Resource limits defined
- [ ] Cost estimates validated
- [ ] Performance benchmarks met
- [ ] Rollback procedures tested
- [ ] Operations team trained

---

### 4.4 `modernization-validation-checklist.md`
**Purpose**: Verify modernized system matches legacy behavior

**Checklist Items**:
- [ ] Functional test cases created from legacy
- [ ] Data reconciliation completed
- [ ] Performance comparison completed
- [ ] Regulatory compliance validated
- [ ] Financial calculation accuracy verified
- [ ] Edge cases tested
- [ ] Integration points tested
- [ ] Error handling validated
- [ ] Security requirements met
- [ ] Accessibility requirements met
- [ ] User acceptance testing completed
- [ ] Operations runbooks created
- [ ] Rollback procedures tested
- [ ] Go-live criteria defined and met

---

## 5. New Process Workflows

### 5.1 **Mainframe Discovery & Assessment Process**

**Steps**:
1. `/rainbow.regulate` - Establish modernization principles
2. `/hanoi.legacy-analyst` - Conduct initial code analysis
3. `/rainbow.assess-legacy` - Create comprehensive assessment
4. `/hanoi.mainframe-architect` - Review assessment, recommend approach
5. Review `mainframe-assessment-checklist.md`
6. Present findings to stakeholders

**Outputs**:
- `legacy-assessment.md`
- Modernization approach recommendation (rehost/replatform/refactor/replace)

---

### 5.2 **Business Logic Extraction Process**

**Steps**:
1. `/hanoi.legacy-analyst` - Analyze legacy code
2. `/rainbow.map-business-logic` - Extract business rules
3. `/hanoi.business-analyst` - Validate business rules with SMEs
4. Create business rules catalog
5. Identify gaps in understanding

**Outputs**:
- `business-logic-map.md`
- Business rules catalog
- SME validation report

---

### 5.3 **Strangler Pattern Migration Process**

**Steps**:
1. `/hanoi.mainframe-architect` - Define bounded contexts
2. `/rainbow.plan-strangler` - Create strangler implementation plan
3. `/rainbow.specify` - Spec first service to extract
4. `/rainbow.clarify` - Clarify requirements
5. `/rainbow.design` - Design service architecture
6. `/rainbow.taskify` - Break down implementation
7. `/rainbow.implement` - Build first service
8. `/rainbow.validate-modernization` - Parallel run validation
9. Repeat steps 3-8 for each service
10. Decommission legacy when complete

**Outputs**:
- `strangler-plan.md`
- Service specifications (one per extracted service)
- Validation reports

---

### 5.4 **Data Migration Process**

**Steps**:
1. `/hanoi.data-migration-engineer` - Analyze source data
2. `/rainbow.design-data-migration` - Design migration strategy
3. Build ETL/ELT pipeline
4. Test with sample data
5. Validate reconciliation procedures
6. Execute phased migration
7. Validate data quality
8. Monitor post-migration

**Outputs**:
- `data-migration.md`
- ETL/ELT pipeline code
- Reconciliation reports
- Data quality reports

---

### 5.5 **Batch Modernization Process**

**Steps**:
1. `/hanoi.batch-modernization-engineer` - Analyze batch jobs
2. `/rainbow.convert-batch` - Design modern replacements
3. `/rainbow.specify` - Specify batch processing requirements
4. `/rainbow.design` - Design batch orchestration
5. `/rainbow.implement` - Build batch jobs
6. Test batch jobs
7. Parallel run with legacy
8. Cutover to modern batch

**Outputs**:
- `batch-modernization.md`
- Batch job implementations
- Orchestration configuration
- Test reports

---

### 5.6 **Screen Migration Process**

**Steps**:
1. `/hanoi.ux-ui-designer` - Analyze legacy screens
2. `/rainbow.design-screen-migration` - Design modern UI
3. `/hanoi.product-owner` - Prioritize screens
4. `/rainbow.specify` - Specify screen requirements
5. `/rainbow.design` - Design frontend architecture
6. `/rainbow.implement` - Build screens
7. Usability testing
8. User training

**Outputs**:
- `screen-migration.md`
- UI wireframes/mockups
- Frontend implementation
- Usability test reports

---

## 6. Integration with Existing Framework

### 6.1 How New Commands Fit

The new commands integrate seamlessly into the existing workflow:

```
Existing Flow:
/rainbow.regulate → /rainbow.specify → /rainbow.design → /rainbow.taskify → /rainbow.implement

Mainframe Modernization Flow:
/rainbow.regulate → /rainbow.assess-legacy → /rainbow.map-business-logic →
/rainbow.specify (modern system) → /rainbow.design-data-migration →
/rainbow.design-screen-migration → /rainbow.convert-batch → /rainbow.plan-strangler →
/rainbow.design → /rainbow.taskify → /rainbow.implement → /rainbow.validate-modernization
```

---

### 6.2 Template Location

All new templates should be stored in:
- `commands/templates-for-commands/` - Command templates
- `agents/templates-for-agents/` - Agent-specific templates

---

### 6.3 Script Support

New scripts needed:
- `scripts/bash/analyze-legacy-code.sh` - Parse legacy code syntax
- `scripts/bash/extract-business-rules.sh` - Extract rules from code
- `scripts/bash/generate-data-mapping.sh` - Create data mapping docs
- PowerShell equivalents in `scripts/powershell/`

---

## 7. Technology Considerations

### 7.1 Legacy Code Parsing

For code analysis, consider integrating:
- **COBOL Parser**: Parse COBOL syntax to extract structure
- **RPG Parser**: Parse RPG programs
- **JCL Parser**: Analyze job control language
- **Copybook Parser**: Extract data structures
- **Static Analysis Tools**: Complexity metrics, dependency analysis

### 7.2 Data Migration Tools

Recommend support for:
- **Open source**: Apache NiFi, Talend, Pentaho
- **Commercial**: Informatica, IBM InfoSphere, AWS DMS
- **Custom**: Python/Java ETL frameworks

### 7.3 Batch Orchestration

Support design for:
- **Kubernetes**: CronJobs, Jobs
- **Cloud-native**: AWS Batch, Azure Batch, Google Cloud Batch
- **Workflow engines**: Apache Airflow, Temporal, Prefect
- **Serverless**: AWS Lambda, Azure Functions, Google Cloud Functions

---

## 8. Success Metrics

### 8.1 Assessment Phase
- % of legacy code analyzed
- % of business rules documented
- Modernization readiness score

### 8.2 Migration Phase
- % of functionality migrated
- Number of parallel validation cycles passed
- Defect density (new vs. legacy)

### 8.3 Validation Phase
- Data reconciliation pass rate
- Performance comparison (modern vs. legacy)
- User acceptance score

---

## 9. Implementation Priority

### Phase 1: Foundation (Weeks 1-4)
1. `/rainbow.assess-legacy` command
2. `/hanoi.legacy-analyst` agent
3. `legacy-assessment-template.md`
4. `mainframe-assessment-checklist.md`

### Phase 2: Analysis (Weeks 5-8)
1. `/rainbow.map-business-logic` command
2. `/hanoi.mainframe-architect` agent
3. `business-logic-extraction-template.md`
4. `mainframe-to-cloud-mapping-template.md`

### Phase 3: Migration Planning (Weeks 9-12)
1. `/rainbow.design-data-migration` command
2. `/rainbow.convert-batch` command
3. `/rainbow.plan-strangler` command
4. `/hanoi.data-migration-engineer` agent
5. Data migration templates

### Phase 4: Execution & Validation (Weeks 13-16)
1. `/rainbow.design-screen-migration` command
2. `/rainbow.validate-modernization` command
3. `/hanoi.cobol-translator` agent
4. `/hanoi.mainframe-tester` agent
5. Validation checklists

---

## 10. Documentation Updates

Update existing documentation:
- `README.md` - Add mainframe modernization use case
- `AGENTS.md` - Document new agents
- `docs/` - Create mainframe modernization guide
- Add examples for each new command

---

## 11. Example Use Case

### Scenario: Modernizing Banking Core System

**Legacy System**:
- COBOL/CICS applications
- DB2 database
- JCL batch jobs
- 3270 green screens
- 5 million LOC

**Modernization Journey**:

1. **Assessment** (Week 1-2)
   - `/rainbow.assess-legacy` → Analyze 5M LOC COBOL
   - `/hanoi.legacy-analyst` → Extract business capabilities
   - Output: 127 business capabilities, 2,300 business rules

2. **Business Logic Mapping** (Week 3-4)
   - `/rainbow.map-business-logic` → Document account processing rules
   - `/hanoi.business-analyst` → Validate with SMEs
   - Output: Business rules catalog with 2,300 rules

3. **Architecture Design** (Week 5-6)
   - `/hanoi.mainframe-architect` → Design microservices architecture
   - `/rainbow.plan-strangler` → Plan phased extraction
   - Output: 12 microservices, 8-phase migration plan

4. **Data Migration** (Week 7-10)
   - `/rainbow.design-data-migration` → DB2 → PostgreSQL + MongoDB
   - `/hanoi.data-migration-engineer` → Design ETL pipelines
   - Output: Data migration specification, ETL pipelines

5. **First Service** (Week 11-14)
   - `/rainbow.specify` → Specify Customer Service
   - `/rainbow.design` → Design REST API + Spring Boot
   - `/rainbow.implement` → Build Customer Service
   - `/rainbow.validate-modernization` → Parallel run validation

6. **Repeat for remaining services** (Week 15-52)

7. **Batch Modernization** (Week 20-28)
   - `/rainbow.convert-batch` → Convert JCL to Kubernetes Jobs
   - `/hanoi.batch-modernization-engineer` → Design orchestration

8. **Screen Migration** (Week 30-40)
   - `/rainbow.design-screen-migration` → Convert 3270 to React
   - `/hanoi.ux-ui-designer` → Design modern UX

**Results**:
- 12 microservices deployed
- 300 GB data migrated
- 85 batch jobs modernized
- 200+ screens converted
- System decommissioned after 18 months

---

## 12. Conclusion

These enhancements will make Hanoi Rainbow the **definitive framework for mainframe and midrange modernization** using Spec-Driven Development. The systematic approach ensures:

✅ **Comprehensive analysis** before migration
✅ **Business continuity** through phased approach
✅ **Quality assurance** via parallel validation
✅ **Knowledge capture** through documentation
✅ **Risk mitigation** through structured process

**Next Steps**:
1. Review and approve this proposal
2. Prioritize features for implementation
3. Create implementation plan
4. Build Phase 1 features
5. Pilot with real mainframe modernization project
6. Iterate based on feedback

---

## Appendix A: Command Quick Reference

| Command | Purpose | Output |
|---------|---------|--------|
| `/rainbow.assess-legacy` | Analyze legacy system | `legacy-assessment.md` |
| `/rainbow.map-business-logic` | Extract business rules | `business-logic-map.md` |
| `/rainbow.design-data-migration` | Plan data migration | `data-migration.md` |
| `/rainbow.convert-batch` | Modernize batch jobs | `batch-modernization.md` |
| `/rainbow.design-screen-migration` | Convert green screens | `screen-migration.md` |
| `/rainbow.plan-strangler` | Plan strangler pattern | `strangler-plan.md` |
| `/rainbow.validate-modernization` | Validate modern system | `modernization-validation.md` |

---

## Appendix B: Agent Quick Reference

| Agent | Role | Primary Templates |
|-------|------|-------------------|
| `/hanoi.legacy-analyst` | Legacy code analysis | legacy-assessment, business-logic-extraction |
| `/hanoi.mainframe-architect` | Migration architecture | mainframe-to-cloud-mapping, strangler-pattern |
| `/hanoi.data-migration-engineer` | Data migration | data-migration-specification |
| `/hanoi.batch-modernization-engineer` | Batch processing | batch-modernization |
| `/hanoi.cobol-translator` | Code translation | N/A (generates code) |
| `/hanoi.mainframe-tester` | Testing & validation | modernization-validation |

---

**Document Version**: 1.0
**Date**: 2025-12-18
**Author**: Claude Sonnet 4.5 (Analysis of Hanoi Rainbow Project)
