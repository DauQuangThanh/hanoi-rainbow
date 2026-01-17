# Monitoring & Operations

## Table of Contents
- [Cloud Monitoring](#cloud-monitoring)
- [Log Analysis](#log-analysis)
- [Activity Tracker](#activity-tracker)
- [Cost Management](#cost-management)
- [Incident Management](#incident-management)
- [Performance Optimization](#performance-optimization)

---

## Cloud Monitoring

### Overview

Monitor metrics, set alerts, and visualize performance across IBM Cloud resources.

### Create Monitoring Instance

```bash
# Create monitoring instance
ibmcloud resource service-instance-create my-monitoring \
  sysdig-monitor graduated-tier us-south

# Get monitoring key
ibmcloud resource service-key-create my-monitoring-key \
  Manager --instance-name my-monitoring
```

### Configure Agents

**Kubernetes:**
```bash
# Get access key
MONITORING_KEY=$(ibmcloud resource service-key my-monitoring-key --output json | jq -r '.credentials.Sysdig Access Key')

# Install agent
kubectl create ns ibm-observe
kubectl create secret generic sysdig-agent \
  --from-literal=access-key=$MONITORING_KEY \
  -n ibm-observe

kubectl apply -f https://raw.githubusercontent.com/draios/sysdig-cloud-scripts/master/agent_deploy/kubernetes/sysdig-agent-daemonset-v2.yaml \
  -n ibm-observe
```

**Virtual Server:**
```bash
# Download and install agent
curl -sL https://ibm.biz/install-sysdig-agent | \
  sudo bash -s -- --access_key $MONITORING_KEY \
  --collector ingest.us-south.monitoring.cloud.ibm.com \
  --tags "env:prod,app:myapp"
```

### Metrics and Dashboards

**Key Metrics:**
- CPU utilization
- Memory usage
- Disk I/O
- Network traffic
- Application response time
- Error rates

**Create Alert:**
```bash
# Via API
curl -X POST https://us-south.monitoring.cloud.ibm.com/api/alerts \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "alert": {
      "name": "High CPU Usage",
      "description": "CPU > 80% for 5 minutes",
      "severity": 4,
      "timespan": 300000000,
      "condition": "avg(cpu.used.percent) > 80",
      "enabled": true,
      "notificationChannelIds": ["<CHANNEL-ID>"]
    }
  }'
```

---

## Log Analysis

### Overview

Centralized logging with search, filtering, and analysis capabilities.

### Create Log Analysis Instance

```bash
# Create log analysis instance
ibmcloud resource service-instance-create my-logs \
  logdna 7-day us-south

# Create service key
ibmcloud resource service-key-create my-logs-key \
  Manager --instance-name my-logs
```

### Configure Log Sources

**Kubernetes:**
```bash
# Get ingestion key
LOGGING_KEY=$(ibmcloud resource service-key my-logs-key --output json | jq -r '.credentials."ingestion_key"')

# Install agent
kubectl create secret generic logdna-agent-key \
  --from-literal=logdna-agent-key=$LOGGING_KEY \
  -n ibm-observe

kubectl apply -f https://assets.us-south.logging.cloud.ibm.com/clients/logdna-agent-ds.yaml
```

**Application Logging (Node.js):**
```javascript
const logger = require('@logdna/logger');

const log = logger.createLogger('<INGESTION-KEY>', {
  hostname: 'my-app',
  app: 'myapp',
  env: 'production',
  index_meta: true
});

log.log('Application started');
log.info('User logged in', { userId: 123 });
log.warn('High memory usage', { memory: '85%' });
log.error('Database connection failed', { error: err.message });
```

### Log Queries

**Search Examples:**
```
# Find errors
level:error

# Filter by source
source:kubernetes

# Time range + filter
timestamp:[now-1h TO now] AND app:myapp AND level:error

# Multiple conditions
(status:500 OR status:502) AND host:prod-server

# Regex search
message:/user.*failed.*login/
```

### Create Views and Alerts

```bash
# Via UI or API
# Create view for error logs
# Set alert for error rate > 10/min
```

---

## Activity Tracker

### Overview

Audit trail for account and resource management activities.

### Configure Activity Tracker

```bash
# Create Activity Tracker instance
ibmcloud resource service-instance-create my-activity-tracker \
  logdnaat 7-day us-south

# Configure routing
ibmcloud at route create \
  --target-type logdna \
  --target-id <AT-INSTANCE-ID> \
  --target-region us-south
```

### Common Audit Queries

**IAM Changes:**
```
action:iam-access-management.policy.*
```

**Resource Creation:**
```
action:*.create outcome:success
```

**Failed Login Attempts:**
```
action:iam-identity.user-apikey.login outcome:failure
```

**Data Access (COS):**
```
target.typeURI:cloud-object-storage/bucket action:cloud-object-storage.object.read
```

**Database Changes:**
```
target.typeURI:databases-for-postgresql action:databases-for-postgresql.*
```

---

## Cost Management

### View Costs

```bash
# View account usage
ibmcloud billing account-usage --output json

# View resource usage
ibmcloud billing resource-instances-usage \
  --start 2024-01 --end 2024-01

# View service usage
ibmcloud billing org-usage <ORG-ID> --month 2024-01
```

### Cost Estimation

**Terraform Cost Estimation:**
```bash
# Using Infracost
infracost breakdown --path .

# Output
Project: myapp
 Name                                     Monthly Qty  Unit   Monthly Cost
 ibm_container_vpc_cluster.cluster
 ├─ Worker nodes (bx2.4x16)                         3  nodes       $360.00
 ├─ Public bandwidth                            1,000  GB           $87.00
 └─ Persistent volumes                            300  GB           $36.00
 
 ibm_database.postgresql
 ├─ Memory                                      4,096  MB          $150.00
 ├─ Disk                                       20,480  MB           $80.00
 └─ CPU                                             3  cores        $90.00

 OVERALL TOTAL                                                     $803.00
```

### Budget Alerts

```bash
# Set spending threshold via UI
# Notifications sent when:
# - 80% of budget reached
# - 90% of budget reached
# - 100% of budget reached
```

### Cost Optimization Tips

**Compute:**
- Use Code Engine/Functions for variable workloads
- Right-size virtual servers and worker nodes
- Use reserved instances for predictable workloads
- Enable auto-scaling
- Stop dev/test resources after hours

**Storage:**
- Use Smart Tier for Object Storage
- Delete unused snapshots
- Implement lifecycle policies
- Use appropriate storage classes

**Database:**
- Right-size CPU, memory, disk
- Use read replicas efficiently
- Delete old backups
- Monitor query performance

**Network:**
- Use private endpoints (free data transfer)
- Enable CDN caching
- Optimize Direct Link bandwidth

---

## Incident Management

### Incident Response Process

**1. Detection:**
- Monitoring alerts
- Log analysis
- User reports
- Health checks

**2. Triage:**
- Assess severity (P1-P4)
- Identify affected services
- Determine impact

**3. Investigation:**
- Check logs and metrics
- Review recent changes
- Analyze error patterns

**4. Resolution:**
- Apply fix
- Verify recovery
- Document root cause

**5. Post-Mortem:**
- Root cause analysis
- Action items
- Prevention measures

### Severity Levels

**P1 - Critical:**
- Complete service outage
- Data loss
- Security breach
- Response: Immediate (24/7)

**P2 - High:**
- Significant degradation
- Partial outage
- Workaround available
- Response: < 1 hour

**P3 - Medium:**
- Minor impact
- Minimal users affected
- Response: < 4 hours

**P4 - Low:**
- Cosmetic issues
- Enhancement requests
- Response: Best effort

### IBM Cloud Support

```bash
# Open support case
ibmcloud sl ticket create \
  --title "Database connection timeout" \
  --body "PostgreSQL database timing out after 30 seconds" \
  --priority 2

# List tickets
ibmcloud sl ticket list

# Update ticket
ibmcloud sl ticket update <TICKET-ID> \
  --body "Tried increasing connection pool, still failing"
```

---

## Performance Optimization

### Application Performance

**Best Practices:**
1. **Caching**: Use Redis for session/data caching
2. **CDN**: Serve static assets via Cloud Internet Services
3. **Database**: Optimize queries, add indexes
4. **Connection Pooling**: Reuse database connections
5. **Async Processing**: Use message queues for long tasks
6. **Code Optimization**: Profile and optimize hot paths
7. **Compression**: Enable gzip/brotli compression

### Database Performance

**PostgreSQL Optimization:**
```sql
-- Analyze table statistics
ANALYZE users;

-- Create index
CREATE INDEX idx_users_email ON users(email);

-- Identify slow queries
SELECT query, mean_exec_time, calls
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;

-- Optimize query
EXPLAIN ANALYZE SELECT * FROM users WHERE email = 'user@example.com';
```

### Monitoring Performance

**Key Metrics:**
- Response time (p50, p95, p99)
- Throughput (requests/sec)
- Error rate
- CPU utilization
- Memory usage
- Database connections
- Cache hit rate

**Set Performance Budgets:**
- Page load: < 2 seconds
- API response: < 500ms
- Database query: < 100ms
- Cache hit rate: > 90%

---

## Best Practices

### Monitoring

1. **Comprehensive Coverage**: Monitor all critical components
2. **Appropriate Alerts**: Set thresholds to avoid alert fatigue
3. **Dashboards**: Create role-specific dashboards
4. **SLOs/SLIs**: Define and track service level objectives
5. **On-Call**: Maintain on-call rotation

### Logging

1. **Structured Logging**: Use JSON format
2. **Log Levels**: Use appropriate levels (DEBUG, INFO, WARN, ERROR)
3. **Context**: Include request IDs, user IDs, timestamps
4. **Retention**: Define retention policies
5. **Security**: Sanitize sensitive data

### Cost Management

1. **Budget**: Set and monitor budgets
2. **Tagging**: Tag resources for cost allocation
3. **Right-Sizing**: Regular resource optimization reviews
4. **Reserved Capacity**: Commit for predictable workloads
5. **Monitoring**: Track usage trends

### Operations

1. **Automation**: Automate repetitive tasks
2. **Documentation**: Maintain runbooks
3. **Disaster Recovery**: Test DR procedures
4. **Change Management**: Track and review changes
5. **Continuous Improvement**: Regular retrospectives
