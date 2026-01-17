# AWS Best Practices by Service

## EC2 (Elastic Compute Cloud)

### Instance Selection
- Select appropriate instance types based on workload characteristics:
  - **General Purpose** (T, M): Balanced compute, memory, networking
  - **Compute Optimized** (C): High-performance processors for CPU-intensive workloads
  - **Memory Optimized** (R, X): Large memory for in-memory databases and caches
  - **Storage Optimized** (I, D): High IOPS and sequential throughput
  - **Accelerated Computing** (P, G, Inf): GPU/FPGA for ML and graphics

### Security
- Use AMIs (Amazon Machine Images) for consistent, secure deployments
- Configure security groups with least privilege principle (deny by default)
- Use Instance Metadata Service v2 (IMDSv2) for enhanced security
- Enable EBS encryption for data volumes and snapshots
- Use Systems Manager Session Manager instead of SSH keys
- Implement VPC endpoints for AWS service access without internet
- Apply security patches regularly using Systems Manager Patch Manager

### High Availability
- Enable Auto Scaling for dynamic capacity management
- Distribute instances across multiple Availability Zones
- Use Elastic Load Balancing for traffic distribution
- Implement health checks for automatic replacement of unhealthy instances
- Use spot instances with Auto Scaling for fault-tolerant workloads

### Performance
- Use EBS-optimized instances for consistent storage performance
- Choose appropriate EBS volume types (gp3, io2, st1, sc1)
- Use placement groups for low-latency applications
- Enable Enhanced Networking (SR-IOV) for high throughput
- Monitor CloudWatch metrics and set up alarms

### Cost Optimization
- Use user data scripts for automated instance initialization
- Tag resources for cost allocation and management
- Right-size instances based on CloudWatch utilization metrics
- Use Reserved Instances or Savings Plans for steady-state workloads
- Implement Instance Scheduler for dev/test environments
- Delete unused AMIs and associated snapshots

## S3 (Simple Storage Service)

### Security
- Block public access by default at account and bucket level
- Use bucket policies and IAM policies for access control
- Enable MFA delete for critical buckets
- Implement S3 Object Lock for compliance (WORM)
- Use S3 Access Points for application-specific access
- Enable CloudTrail logging for S3 data events
- Use AWS PrivateLink for VPC endpoints

### Encryption
- Enable server-side encryption by default
- Choose encryption method:
  - **SSE-S3**: AWS-managed keys (simplest)
  - **SSE-KMS**: Customer-managed keys with audit trail
  - **SSE-C**: Customer-provided keys (full control)
- Enable encryption in transit (HTTPS)
- Use bucket policies to enforce encryption

### Data Management
- Enable versioning for critical data protection
- Configure lifecycle policies for automatic tiering:
  - S3 Standard → S3 Standard-IA (30+ days)
  - S3 Standard-IA → S3 Glacier (90+ days)
  - S3 Glacier → S3 Glacier Deep Archive (180+ days)
- Use S3 Intelligent-Tiering for automatic optimization
- Implement S3 Replication (CRR/SRR) for disaster recovery
- Use S3 Inventory for asset management

### Performance
- Use CloudFront for content delivery and edge caching
- Implement multipart upload for objects >100MB
- Use S3 Transfer Acceleration for long-distance uploads
- Design key names to avoid hot partitions
- Use byte-range fetches for large objects
- Enable S3 Select for in-place query processing

### Cost Optimization
- Use appropriate storage classes based on access patterns
- Delete incomplete multipart uploads automatically
- Use lifecycle policies to transition or expire objects
- Monitor storage metrics with S3 Storage Lens
- Use Requester Pays for sharing data

## RDS and Aurora

### High Availability
- Enable Multi-AZ deployment for automatic failover
- Use read replicas for read scaling and disaster recovery
- Configure automated backups with appropriate retention (7-35 days)
- Take manual snapshots before major changes
- Enable deletion protection for production databases

### Security
- Deploy in private subnets with no internet access
- Use security groups to restrict database access
- Enable encryption at rest using KMS
- Enable encryption in transit (SSL/TLS connections)
- Use IAM database authentication where supported
- Store credentials in AWS Secrets Manager
- Enable CloudWatch Logs for audit and error logs

### Performance
- Choose appropriate instance classes based on workload
- Use Parameter Groups for database configuration
- Enable Performance Insights for query analysis
- Enable Enhanced Monitoring for detailed metrics
- Use appropriate storage type (gp3, io1)
- Configure connection pooling in applications
- Implement read replicas to offload read traffic

### Aurora Specific
- Use Aurora Serverless v2 for variable workloads
- Implement Aurora Global Database for disaster recovery
- Use Aurora cluster endpoints for read/write splitting
- Enable Aurora backtrack for point-in-time recovery
- Use Aurora parallel query for analytical workloads

### Cost Optimization
- Use Reserved Instances for production databases
- Stop non-production instances during off-hours
- Right-size instances based on CloudWatch metrics
- Use Aurora Serverless for intermittent workloads
- Monitor storage growth and optimize indexes

## VPC and Networking

### VPC Design
- Design VPC CIDR blocks carefully to avoid conflicts
- Use /16 for VPC, /24 for subnets (allows 251 hosts)
- Reserve IP space for future growth
- Use public subnets for internet-facing resources
- Use private subnets for internal resources
- Create subnets in multiple Availability Zones

### Security
- Configure security groups (stateful) as primary defense
- Use Network ACLs (stateless) for additional layer
- Implement defense in depth with multiple security layers
- Enable VPC Flow Logs for network monitoring
- Use AWS Network Firewall for advanced protection
- Implement VPC endpoints for private AWS service access

### Connectivity
- Use NAT Gateways (high availability) instead of NAT Instances
- Deploy NAT Gateways in multiple AZs for redundancy
- Use Transit Gateway for multi-VPC connectivity
- Implement AWS PrivateLink for SaaS connectivity
- Use VPN or Direct Connect for hybrid connectivity
- Configure route tables properly for each subnet

### Load Balancing
- Use Application Load Balancer (ALB) for HTTP/HTTPS
- Use Network Load Balancer (NLB) for TCP/UDP
- Enable access logs for troubleshooting
- Configure health checks appropriately
- Use target groups for routing flexibility
- Enable cross-zone load balancing

### DNS and CDN
- Use Route 53 for DNS with health checks
- Implement failover routing for disaster recovery
- Use CloudFront for content delivery
- Configure CloudFront origin failover
- Enable CloudFront access logs

## Lambda and Serverless

### Function Design
- Design functions with single responsibility principle
- Keep deployment packages small (<50MB)
- Initialize SDK clients outside handler function
- Use Lambda Layers for shared dependencies
- Set appropriate timeout (default 3s, max 900s)
- Configure appropriate memory (128MB-10GB)
- Use environment variables for configuration

### Performance
- Configure memory to optimize CPU allocation (1,769MB = 1 vCPU)
- Use Provisioned Concurrency for consistent performance
- Enable SnapStart for Java functions (faster cold starts)
- Minimize cold start time:
  - Reduce package size
  - Minimize dependencies
  - Avoid VPC unless necessary
  - Use ARM (Graviton2) for better performance

### Security
- Use IAM roles with least privilege
- Store secrets in Secrets Manager or Parameter Store
- Enable X-Ray tracing for debugging
- Use VPC endpoints for private resource access
- Implement function URL authentication
- Enable code signing for deployment integrity

### Reliability
- Use Dead Letter Queues (DLQ) for failed events
- Configure retry attempts appropriately
- Use Step Functions for complex workflows
- Implement circuit breakers for external dependencies
- Monitor with CloudWatch metrics and alarms
- Use Lambda Destinations for async invocations

### Cost Optimization
- Right-size memory allocation
- Use ARM (Graviton2) for 20% cost savings
- Minimize execution duration
- Use reserved concurrency only when needed
- Clean up old function versions
- Monitor invocation costs with Cost Explorer

## ECS and EKS

### ECS Best Practices
- Use Fargate for serverless container execution
- Use EC2 launch type for more control and cost optimization
- Configure task definitions with appropriate CPU/memory limits
- Implement container health checks
- Use Application Load Balancers for service discovery
- Enable Container Insights for monitoring
- Store secrets in Secrets Manager
- Use ECR with image scanning
- Implement auto-scaling policies
- Use task IAM roles for AWS service access

### EKS Best Practices
- Use managed node groups for simplified operations
- Implement cluster autoscaler for dynamic scaling
- Configure RBAC for proper access control
- Use IAM roles for service accounts (IRSA)
- Implement network policies for pod-to-pod traffic
- Use Kubernetes secrets or external secrets operator
- Enable control plane logging
- Use EKS add-ons (VPC CNI, CoreDNS, kube-proxy)
- Implement pod security policies
- Use AWS Load Balancer Controller for ingress

### Container Security
- Scan images for vulnerabilities
- Use minimal base images (Alpine, Distroless)
- Run containers as non-root users
- Implement read-only root filesystems
- Use AWS App Mesh for service mesh
- Enable encryption in transit
- Implement network segmentation

## IAM (Identity and Access Management)

### Access Control
- Follow principle of least privilege
- Use IAM roles instead of access keys for AWS resources
- Use temporary credentials (STS) for users
- Enable MFA for root account and privileged users
- Use IAM policies with conditions for fine-grained control
- Implement resource-based policies where appropriate
- Use service control policies (SCPs) in AWS Organizations

### Credential Management
- Rotate access keys regularly (every 90 days)
- Use IAM Access Analyzer to identify unintended access
- Enable CloudTrail for API auditing
- Use AWS Single Sign-On (AWS SSO) for workforce access
- Implement cross-account roles for multi-account access
- Store secrets in AWS Secrets Manager
- Use Systems Manager Parameter Store for configuration

### Policy Management
- Use managed policies for common permissions
- Create customer-managed policies for custom permissions
- Avoid inline policies (hard to audit)
- Use policy variables for dynamic permissions
- Test policies with IAM Policy Simulator
- Document policy purpose and approvals
- Review policies regularly for over-permissions

### Monitoring and Compliance
- Enable CloudTrail in all regions
- Use AWS Config for compliance monitoring
- Set up CloudWatch alarms for suspicious activities
- Use Access Analyzer for cross-account access
- Implement automated remediation with Lambda
- Conduct regular access reviews
- Use permission boundaries for delegated administration

## Security Best Practices

### Data Protection
- Enable encryption at rest for all data stores (EBS, S3, RDS)
- Use AWS KMS for key management
- Implement encryption in transit (TLS/SSL)
- Use VPC endpoints for private connectivity
- Implement data classification and tagging
- Enable S3 Object Lock for compliance
- Use AWS Backup for centralized backup management

### Threat Detection
- Enable GuardDuty for threat detection
- Use Security Hub for centralized security findings
- Enable CloudTrail for API auditing
- Use AWS Config for configuration compliance
- Implement AWS Macie for sensitive data discovery
- Use Amazon Detective for security investigation
- Set up CloudWatch alarms for security events

### Network Security
- Implement security groups with least privilege
- Use Network ACLs for subnet-level protection
- Enable VPC Flow Logs for network monitoring
- Use AWS Network Firewall for advanced protection
- Implement AWS WAF for web application protection
- Use AWS Shield for DDoS protection
- Restrict internet gateway access

### Application Security
- Implement AWS WAF rules for common attacks
- Use AWS WAF managed rules for OWASP Top 10
- Enable AWS Shield Advanced for critical applications
- Use CloudFront with origin access identity
- Implement rate limiting and geo-blocking
- Use AWS Certificate Manager for TLS certificates
- Conduct regular security assessments

### Compliance
- Use AWS Artifact for compliance reports
- Implement tagging for resource tracking
- Enable AWS Config rules for compliance checks
- Use AWS Audit Manager for audit preparation
- Document security controls and procedures
- Conduct regular compliance assessments
- Implement automated compliance remediation

## Cost Optimization

### Compute Cost Optimization
- Use Reserved Instances for predictable workloads (1-3 year terms)
- Use Savings Plans for flexible commitment-based discounts
- Use Spot Instances for fault-tolerant workloads (up to 90% savings)
- Right-size instances based on CloudWatch metrics
- Use Auto Scaling to match demand
- Stop non-production instances during off-hours
- Use Graviton instances for 20% better price-performance

### Storage Cost Optimization
- Use S3 lifecycle policies to transition to cheaper tiers
- Use S3 Intelligent-Tiering for automatic optimization
- Delete incomplete multipart uploads
- Use EBS gp3 instead of gp2 (20% cheaper)
- Delete unused EBS volumes and snapshots
- Use EFS Infrequent Access for rarely accessed files
- Compress data before storing

### Database Cost Optimization
- Use Reserved Instances for production databases
- Right-size database instances
- Use Aurora Serverless for variable workloads
- Delete old manual snapshots
- Use read replicas for read scaling instead of larger instances
- Optimize queries and indexes
- Use DynamoDB on-demand for unpredictable workloads

### Monitoring and Management
- Enable AWS Cost Explorer for cost analysis
- Set up AWS Budgets with alerts
- Implement cost allocation tags
- Use AWS Cost Anomaly Detection
- Review Trusted Advisor recommendations
- Use AWS Compute Optimizer for right-sizing
- Implement FinOps practices with stakeholder engagement

### Network Cost Optimization
- Use VPC endpoints to avoid data transfer charges
- Use CloudFront to reduce data transfer costs
- Keep data transfer within same AZ when possible
- Use Direct Connect for large data transfers
- Implement data compression
- Monitor data transfer costs with Cost Explorer
- Use Regional Data Transfer instead of Internet Transfer

### Architectural Optimization
- Use serverless architectures (Lambda, Fargate)
- Implement caching (CloudFront, ElastiCache)
- Use managed services to reduce operational costs
- Optimize for multi-tenancy where appropriate
- Implement workload scheduling for batch jobs
- Use spot instances for batch processing
- Leverage AWS Free Tier for development
