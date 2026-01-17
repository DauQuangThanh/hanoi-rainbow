# Compute Services

## ECS (Elastic Compute Service)

### Instance Families

**General Purpose (g series)**
- **g8i**: Latest generation, Intel Ice Lake, balanced compute/memory/network
- **g7**: Intel Xeon, high performance for web/app servers
- **g6**: Cost-effective, suitable for small-medium workloads
- Use cases: Web servers, application servers, development/test environments

**Compute Optimized (c series)**
- **c8i**: Latest Intel Ice Lake, highest CPU performance
- **c7**: High frequency processors, compute-intensive
- **c6**: Cost-effective compute performance
- Use cases: High-traffic web servers, batch processing, video encoding, gaming

**Memory Optimized (r series)**
- **r8i**: Latest generation, high memory to CPU ratio
- **r7**: Large memory capacity for in-memory databases
- **r6**: Cost-effective memory-intensive workloads
- Use cases: Relational databases, in-memory caching, big data analytics

**GPU Instances (gn series)**
- **gn7i**: NVIDIA A10 GPUs, AI inference and training
- **gn6v**: NVIDIA V100, deep learning training
- **gn6i**: NVIDIA T4, cost-effective inference
- Use cases: Deep learning, AI/ML training, video rendering, HPC

### Instance Selection Guide

```
Workload Type           → Recommended Family
--------------------------------------------------
Web/App Server          → g7, g8i (2-4 vCPU, 4-8GB RAM)
Database                → r7, r8i (4-8+ vCPU, 16-64GB+ RAM)
Big Data Processing     → r7, d2s with local SSD
Batch Jobs              → c7, c8i with auto-scaling
AI/ML Training          → gn7i, gn6v with GPU
Video Processing        → c7, gn6i
Development/Test        → g6, t6 burstable instances
```

### Storage Options

**System Disk**
- **ESSD PL0**: Entry-level SSD, 10K IOPS
- **ESSD PL1**: Standard SSD, up to 50K IOPS (recommended)
- **ESSD PL2**: High-performance SSD, up to 100K IOPS
- **ESSD PL3**: Ultra high-performance, up to 1M IOPS

**Data Disk**
- Attach multiple data disks up to 64 disks per instance
- Use LVM for multiple disk aggregation
- Enable encryption for sensitive data

### Auto Scaling Configuration

**Scaling Policies**
```yaml
# Target Tracking Policy
Metric: CPU Utilization
Target: 70%
Warm-up: 300 seconds
Cooldown: 300 seconds

# Scheduled Policy
Schedule: 0 8 * * * (8 AM daily)
Action: Add 5 instances
Min Instances: 2
Max Instances: 20

# Step Scaling Policy
When CPU > 80%: Add 3 instances
When CPU > 90%: Add 5 instances
When CPU < 30%: Remove 2 instances
```

**Best Practices**
- Set appropriate min/max instance counts
- Use target tracking for predictable workloads
- Combine scheduled and dynamic scaling
- Configure sufficient warm-up time
- Use custom metrics for application-specific scaling
- Test scaling policies in non-production first

### Network Configuration

**Network Types**
- **VPC**: Recommended for production, isolated network
- **Classic Network**: Legacy, not recommended for new deployments

**Security Groups**
```
Inbound Rules:
- Allow HTTP (80) from 0.0.0.0/0
- Allow HTTPS (443) from 0.0.0.0/0
- Allow SSH (22) from specific IP ranges only
- Allow application ports from SLB security group only

Outbound Rules:
- Allow all traffic (default)
- Or restrict to specific destinations for security
```

**ENI (Elastic Network Interface)**
- Primary ENI automatically attached
- Attach secondary ENIs for multi-network scenarios
- Each ENI can have multiple private IPs
- Associate EIP for public internet access

### Instance Initialization

**Cloud-init Example**
```yaml
#cloud-config
packages:
  - docker
  - git
  - nginx

runcmd:
  - systemctl enable docker
  - systemctl start docker
  - docker pull myapp:latest
  - docker run -d -p 80:80 myapp:latest

write_files:
  - path: /etc/app/config.yaml
    content: |
      app:
        env: production
        port: 80
```

### Monitoring and Management

**CloudMonitor Metrics**
- CPU utilization
- Memory usage (requires agent)
- Disk IOPS and throughput
- Network in/out traffic
- Disk usage (requires agent)

**Alerting Rules**
```
CPU > 80% for 5 minutes → Send notification
Disk usage > 85% → Send alert
Instance status check failed → Trigger auto-restart
```

### Pricing Models

**Pay-As-You-Go**
- Billed per second
- No long-term commitment
- Most flexible, higher unit cost

**Subscription (Reserved Instances)**
- 1 month to 5 years
- Save up to 70% vs pay-as-you-go
- Best for stable workloads

**Preemptible Instances**
- Up to 90% discount
- May be reclaimed with 5-minute notice
- Best for fault-tolerant, stateless workloads

## Function Compute

### Function Types

**Event Functions**
- Triggered by events (OSS, Log Service, API Gateway)
- Async execution
- Use for: Image processing, log analysis, data ETL

**HTTP Functions**
- Triggered by HTTP requests
- Sync execution
- Use for: APIs, webhooks, microservices

### Configuration Best Practices

**Memory and Timeout**
```
Light processing:   512 MB, 3 seconds
Data processing:    1024-2048 MB, 60 seconds
Video processing:   3072 MB, 600 seconds
Batch jobs:         4096 MB, 900 seconds
```

**Environment Variables**
- Store configuration in environment variables
- Use sensitive data store for secrets
- Avoid hardcoding credentials

**VPC Access**
- Enable VPC access to connect to RDS, Redis
- Configure NAT Gateway for internet access
- Use VPC security groups for network control

### Trigger Configuration

**OSS Trigger**
```yaml
Event: oss:ObjectCreated:PutObject
Prefix: uploads/
Suffix: .jpg
Use case: Image thumbnail generation
```

**API Gateway Trigger**
```yaml
Method: POST
Path: /api/users
Auth: JWT token validation
Use case: RESTful API endpoints
```

**Timer Trigger**
```yaml
Cron: 0 2 * * * (2 AM daily)
Use case: Data cleanup, report generation
```

### Cold Start Optimization

**Best Practices**
- Keep deployment package small (< 50MB)
- Use layers for common dependencies
- Initialize connections outside handler
- Use provisioned instances for critical functions
- Implement connection pooling
- Cache frequently accessed data

**Example Structure**
```python
import json
import redis

# Initialize outside handler (reused across invocations)
redis_client = redis.Redis(
    host='your-redis.redis.rds.aliyuncs.com',
    port=6379,
    decode_responses=True
)

def handler(event, context):
    # Handler code here
    data = redis_client.get('key')
    return {
        'statusCode': 200,
        'body': json.dumps(data)
    }
```

### Monitoring and Logging

**Built-in Metrics**
- Invocations
- Errors
- Duration
- Throttles
- Memory usage

**Custom Logging**
```python
import logging
logger = logging.getLogger()

def handler(event, context):
    logger.info(f'Processing event: {event}')
    # Function logic
    logger.error(f'Error occurred: {error}')
```

### Cost Optimization

**Best Practices**
- Right-size memory allocation (charges based on GB-seconds)
- Optimize function duration
- Use async processing where possible
- Implement caching to reduce invocations
- Use scheduled functions during off-peak hours
- Monitor and eliminate unused functions
