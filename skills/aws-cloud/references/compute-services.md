# AWS Compute Services

## EC2 (Elastic Compute Cloud)

### Instance Types and Families

**General Purpose** (T, M series)
- T4g, T3, T3a: Burstable performance for variable workloads
- M7g, M6i, M5: Balanced compute, memory, networking
- Use cases: Web servers, development environments, small databases

**Compute Optimized** (C series)
- C7g, C6i, C5: High-performance processors
- Use cases: Batch processing, media transcoding, HPC, game servers, scientific modeling

**Memory Optimized** (R, X, High Memory series)
- R7g, R6i, R5: Memory-intensive applications
- X2idn, X2iedn: Highest memory-to-vCPU ratio
- Use cases: In-memory databases, big data processing, SAP HANA

**Storage Optimized** (I, D, H series)
- I4i, I3: High IOPS NVMe SSD storage
- D3: High sequential read/write for data warehouses
- Use cases: NoSQL databases, data warehousing, Elasticsearch

**Accelerated Computing** (P, G, Inf series)
- P4, P3: GPU for machine learning training
- G5, G4: Graphics-intensive applications
- Inf2, Inf1: Machine learning inference
- Use cases: ML training/inference, video rendering, genomics

**Graviton Processors** (instances ending with 'g')
- ARM-based AWS Graviton2/3 processors
- Up to 40% better price-performance vs x86
- Available across T4g, M7g, C7g, R7g families

### Instance Purchasing Options

**On-Demand**
- Pay by the second (Linux) or hour (Windows)
- No upfront commitment
- Use for: Short-term, unpredictable workloads

**Reserved Instances**
- 1 or 3-year commitment
- Up to 75% discount vs On-Demand
- Standard RI: Specific instance type, region
- Convertible RI: Can change instance type
- Use for: Steady-state, predictable workloads

**Savings Plans**
- 1 or 3-year commitment to consistent usage ($$/hour)
- Up to 72% discount vs On-Demand
- Compute Savings Plans: Flexible across instance families, regions
- EC2 Instance Savings Plans: Specific instance family in region
- Use for: Flexible workloads with predictable usage

**Spot Instances**
- Up to 90% discount vs On-Demand
- Can be interrupted with 2-minute warning
- Use for: Fault-tolerant, flexible workloads (batch processing, CI/CD, big data)

**Dedicated Hosts**
- Physical server dedicated for your use
- Compliance requirements (licensing, regulatory)
- Most expensive option

### Auto Scaling

**Launch Templates**
```yaml
LaunchTemplate:
  Type: AWS::EC2::LaunchTemplate
  Properties:
    LaunchTemplateName: my-app-template
    LaunchTemplateData:
      ImageId: ami-0123456789abcdef0
      InstanceType: t3.micro
      KeyName: my-keypair
      SecurityGroupIds:
        - sg-0123456789abcdef0
      IamInstanceProfile:
        Arn: !GetAtt EC2InstanceProfile.Arn
      UserData:
        Fn::Base64: !Sub |
          #!/bin/bash
          yum update -y
          yum install -y httpd
          systemctl start httpd
          systemctl enable httpd
      TagSpecifications:
        - ResourceType: instance
          Tags:
            - Key: Name
              Value: my-app-instance
```

**Auto Scaling Group**
```yaml
AutoScalingGroup:
  Type: AWS::AutoScaling::AutoScalingGroup
  Properties:
    AutoScalingGroupName: my-app-asg
    LaunchTemplate:
      LaunchTemplateId: !Ref LaunchTemplate
      Version: $Latest
    MinSize: 2
    MaxSize: 10
    DesiredCapacity: 2
    HealthCheckType: ELB
    HealthCheckGracePeriod: 300
    VPCZoneIdentifier:
      - !Ref PrivateSubnet1
      - !Ref PrivateSubnet2
    TargetGroupARNs:
      - !Ref TargetGroup
    Tags:
      - Key: Name
        Value: my-app-instance
        PropagateAtLaunch: true
```

**Scaling Policies**

Target Tracking Scaling:
```yaml
ScalingPolicy:
  Type: AWS::AutoScaling::ScalingPolicy
  Properties:
    AutoScalingGroupName: !Ref AutoScalingGroup
    PolicyType: TargetTrackingScaling
    TargetTrackingConfiguration:
      PredefinedMetricSpecification:
        PredefinedMetricType: ASGAverageCPUUtilization
      TargetValue: 70.0
```

Step Scaling:
```yaml
StepScalingPolicy:
  Type: AWS::AutoScaling::ScalingPolicy
  Properties:
    AutoScalingGroupName: !Ref AutoScalingGroup
    PolicyType: StepScaling
    AdjustmentType: PercentChangeInCapacity
    MetricAggregationType: Average
    EstimatedInstanceWarmup: 60
    StepAdjustments:
      - MetricIntervalLowerBound: 0
        MetricIntervalUpperBound: 10
        ScalingAdjustment: 10
      - MetricIntervalLowerBound: 10
        ScalingAdjustment: 20
```

### EC2 Best Practices

1. **Use Latest Generation Instances**: Better price-performance
2. **Enable Detailed Monitoring**: 1-minute metrics for better scaling decisions
3. **Use IMDSv2**: Enhanced security for instance metadata
4. **Enable EBS Optimization**: Better storage performance
5. **Use Placement Groups**:
   - Cluster: Low latency, high throughput
   - Spread: Reduce correlated failures
   - Partition: Large distributed workloads
6. **Configure Health Checks**: ALB health checks for Auto Scaling
7. **Use Systems Manager**: Fleet management, patching, automation

## AWS Lambda

### Lambda Function Configuration

**Memory and CPU**
- Memory: 128 MB to 10,240 MB (10 GB)
- CPU scales with memory (1,769 MB = 1 vCPU)
- Timeout: Maximum 15 minutes (900 seconds)
- Ephemeral storage (/tmp): 512 MB to 10,240 MB

**Execution Model**
- Synchronous: API Gateway, ALB, SDK invoke
- Asynchronous: S3, SNS, EventBridge, SES
- Stream-based: DynamoDB Streams, Kinesis

**Cold Start Optimization**
- Use Provisioned Concurrency for predictable performance
- Initialize SDK clients outside handler
- Use Lambda Layers for shared dependencies
- Minimize deployment package size
- Use SnapStart for Java functions (up to 10x faster)

### Lambda Function Patterns

**API Backend**
```python
import json
import boto3

dynamodb = boto3.resource('dynamodb')
table = dynamodb.Table('users')

def lambda_handler(event, context):
    # Parse API Gateway event
    http_method = event['httpMethod']
    path = event['path']
    
    if http_method == 'GET' and path == '/users':
        response = table.scan()
        return {
            'statusCode': 200,
            'headers': {'Content-Type': 'application/json'},
            'body': json.dumps(response['Items'])
        }
    
    return {
        'statusCode': 404,
        'body': json.dumps({'error': 'Not found'})
    }
```

**S3 Event Processing**
```python
import boto3
import json
from PIL import Image
import io

s3 = boto3.client('s3')

def lambda_handler(event, context):
    for record in event['Records']:
        bucket = record['s3']['bucket']['name']
        key = record['s3']['object']['key']
        
        # Download image
        obj = s3.get_object(Bucket=bucket, Key=key)
        image_data = obj['Body'].read()
        
        # Process image
        image = Image.open(io.BytesIO(image_data))
        thumbnail = image.resize((200, 200))
        
        # Upload thumbnail
        buffer = io.BytesIO()
        thumbnail.save(buffer, 'JPEG')
        buffer.seek(0)
        
        thumbnail_key = f"thumbnails/{key}"
        s3.put_object(Bucket=bucket, Key=thumbnail_key, Body=buffer)
```

**DynamoDB Stream Processing**
```python
import boto3
import json

def lambda_handler(event, context):
    for record in event['Records']:
        event_name = record['eventName']
        
        if event_name == 'INSERT':
            new_image = record['dynamodb']['NewImage']
            # Process new record
            process_new_record(new_image)
            
        elif event_name == 'MODIFY':
            old_image = record['dynamodb']['OldImage']
            new_image = record['dynamodb']['NewImage']
            # Process modification
            process_update(old_image, new_image)
            
        elif event_name == 'REMOVE':
            old_image = record['dynamodb']['OldImage']
            # Process deletion
            process_deletion(old_image)
```

### Lambda Best Practices

1. **Use Environment Variables**: Configuration without code changes
2. **Enable X-Ray Tracing**: Distributed tracing for debugging
3. **Use Lambda Layers**: Share code across functions
4. **Implement Error Handling**: Try/catch and retry logic
5. **Use Dead Letter Queues**: Capture failed async invocations
6. **Monitor with CloudWatch**: Metrics, logs, and alarms
7. **Use Secrets Manager**: Store sensitive configuration
8. **Version and Alias**: Blue/green deployments
9. **Reserve Concurrent Executions**: Prevent throttling critical functions
10. **VPC Access**: Only when needed (adds cold start time)

## Elastic Beanstalk

### Platform Support
- Docker, Go, Java, .NET, Node.js, PHP, Python, Ruby
- Preconfigured platforms with web server (nginx, Apache)
- Custom platforms with Packer

### Deployment Strategies

**All at Once**
- Fastest deployment
- Brief downtime
- Use for: Development environments

**Rolling**
- Deploy in batches
- Reduced capacity during deployment
- No downtime
- Use for: Production with acceptable temporary capacity reduction

**Rolling with Additional Batch**
- Deploy to new instances first
- Maintain full capacity
- No downtime
- Use for: Production requiring full capacity

**Immutable**
- Deploy to new instances in new ASG
- Zero downtime
- Quick rollback
- Use for: Production requiring safest deployment

**Blue/Green**
- Deploy to separate environment
- Swap CNAMEs when ready
- Zero downtime
- Instant rollback
- Use for: Mission-critical production

### Configuration

**.ebextensions/01-app.config**
```yaml
option_settings:
  aws:elasticbeanstalk:container:nodejs:
    NodeCommand: "npm start"
  aws:autoscaling:launchconfiguration:
    InstanceType: t3.small
    EC2KeyName: my-keypair
  aws:autoscaling:asg:
    MinSize: 2
    MaxSize: 10
  aws:elasticbeanstalk:environment:
    EnvironmentType: LoadBalanced
    LoadBalancerType: application

Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: my-app-bucket
```

## AWS Batch

### Use Cases
- Batch processing jobs
- ETL (Extract, Transform, Load)
- Financial modeling
- Drug discovery simulations
- Image/video processing

### Components

**Compute Environment**
- Managed: AWS provisions and manages EC2/Fargate
- Unmanaged: You manage compute resources
- Can use Spot Instances for cost savings

**Job Queue**
- Jobs submitted to queue
- Priority-based scheduling
- Multiple queues for different workloads

**Job Definition**
- Container properties (image, vCPUs, memory)
- IAM role for task execution
- Environment variables and secrets
- Retry strategy

### Example Job Definition
```json
{
  "jobDefinitionName": "my-batch-job",
  "type": "container",
  "containerProperties": {
    "image": "123456789012.dkr.ecr.us-east-1.amazonaws.com/my-app:latest",
    "vcpus": 2,
    "memory": 4096,
    "command": ["python", "process.py"],
    "jobRoleArn": "arn:aws:iam::123456789012:role/BatchJobRole",
    "environment": [
      {"name": "ENVIRONMENT", "value": "production"}
    ],
    "resourceRequirements": [
      {"type": "GPU", "value": "1"}
    ]
  },
  "retryStrategy": {
    "attempts": 3
  },
  "timeout": {
    "attemptDurationSeconds": 3600
  }
}
```

### Best Practices

1. **Use Managed Compute**: Simplifies operations
2. **Use Spot for Cost Savings**: 70-90% savings for interruptible jobs
3. **Set Appropriate Timeouts**: Prevent runaway jobs
4. **Configure Retry Strategy**: Handle transient failures
5. **Use Array Jobs**: Process large datasets efficiently
6. **Monitor with CloudWatch**: Track job metrics and logs
7. **Use Multi-node Parallel Jobs**: For MPI workloads
