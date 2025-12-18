---
description: Design modern equivalents for mainframe batch processing jobs
handoffs:
  - label: Design Screen Migration
    agent: rainbow.design-screen-migration
    prompt: Design modern UI after batch processing is planned
    send: true
  - label: Get Batch Engineer Support
    agent: hanoi.batch-modernization-engineer
    prompt: Get expert guidance on batch modernization
scripts:
  sh: scripts/bash/setup-convert-batch.sh --json
  ps: scripts/powershell/setup-convert-batch.ps1 -Json
agent_scripts:
  sh: scripts/bash/update-agent-context.sh __AGENT__
  ps: scripts/powershell/update-agent-context.ps1 -AgentType __AGENT__
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

**IMPORTANT**: Automatically generate a 'docs:' prefixed git commit message (e.g., 'docs: add batch modernization plan for feature-name') and commit batch-modernization.md upon completion.

1. **Setup**: Run `{SCRIPT}` from repo root and parse JSON output.

2. **Load context**: Read FEATURE_SPEC, LEGACY_ASSESSMENT (if exists), `memory/ground-rules.md`.

3. **Execute batch modernization design**: Follow template to:
   - Catalog batch jobs and schedules
   - Analyze dependencies between jobs
   - Design modern alternatives (Kubernetes Jobs, serverless, streaming)
   - Plan job orchestration
   - Define monitoring and alerting

4. **Update agent context**: Run `{AGENT_SCRIPT}`.

5. **Report completion**.

## Phases

### Phase 0: Batch Job Inventory

1. **Catalog all batch jobs**:
   - JCL job names and descriptions
   - Execution frequency (daily, weekly, monthly, ad-hoc)
   - Runtime characteristics
   - Dependencies on other jobs
   - Input/output files and databases

2. **Analyze job complexity**:
   - Number of steps per job
   - Data volumes processed
   - Resource requirements (CPU, memory)

**Output**: Batch Job Inventory section

### Phase 1: Dependency Analysis

1. **Build dependency graph**:
   - Job-to-job dependencies
   - Data dependencies
   - Critical path analysis
   - Identify parallel execution opportunities

2. **Classify by criticality**:
   - Business-critical vs. nice-to-have
   - SLA requirements
   - Error tolerance

**Output**: Dependency Analysis section with diagrams

### Phase 2: Modernization Options

1. **Evaluate modernization approaches**:
   - **Kubernetes Jobs/CronJobs**: Container-based batch
   - **Serverless Functions** (Lambda, Azure Functions): Event-driven
   - **Managed Batch Services** (AWS Batch, Azure Batch): Cloud-native
   - **Stream Processing** (Kafka Streams, Flink): Real-time alternative
   - **Workflow Orchestration** (Airflow, Temporal, Step Functions)

2. **Map jobs to modern alternatives**:
   - Short-running → Serverless
   - Long-running → Kubernetes/Managed Batch
   - Scheduled → CronJobs/EventBridge
   - Data transformation → Stream processing

**Output**: Modernization Options and Target Architecture sections

### Phase 3: Orchestration & Monitoring

1. **Design job orchestration**:
   - Workflow engine selection (Airflow, Temporal, etc.)
   - Scheduling strategy
   - Error handling and retries
   - Alerting rules

2. **Plan monitoring**:
   - Job execution metrics
   - Duration tracking
   - Failure notifications
   - Resource utilization

**Output**: Orchestration and Monitoring sections

## Success Criteria

- [ ] All batch jobs cataloged
- [ ] Dependency graph created
- [ ] Modern alternatives selected
- [ ] Orchestration platform chosen
- [ ] Monitoring strategy defined
