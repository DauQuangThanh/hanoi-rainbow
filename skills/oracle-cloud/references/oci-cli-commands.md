# OCI CLI Commands Reference

## Installation and Configuration

### Install OCI CLI

**macOS/Linux**:
```bash
bash -c "$(curl -L https://raw.githubusercontent.com/oracle/oci-cli/master/scripts/install/install.sh)"
```

**Python pip**:
```bash
pip install oci-cli
```

### Configure OCI CLI

**Interactive Setup**:
```bash
oci setup config
```

**Manual Configuration** (~/.oci/config):
```ini
[DEFAULT]
user=ocid1.user.oc1..aaaa...
fingerprint=12:34:56:78:90:ab:cd:ef:12:34:56:78:90:ab:cd:ef
tenancy=ocid1.tenancy.oc1..aaaa...
region=us-ashburn-1
key_file=~/.oci/oci_api_key.pem

[PRODUCTION]
user=ocid1.user.oc1..bbbb...
fingerprint=aa:bb:cc:dd:ee:ff:00:11:22:33:44:55:66:77:88:99
tenancy=ocid1.tenancy.oc1..bbbb...
region=us-phoenix-1
key_file=~/.oci/prod_api_key.pem
```

**Use Specific Profile**:
```bash
oci compute instance list --profile PRODUCTION
```

### Verify Configuration

```bash
# Test connection
oci iam region list

# Get current user
oci iam user get --user-id $(oci iam user list --query 'data[0].id' --raw-output)
```

## Compute Commands

### List Instances

```bash
# List all instances in compartment
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --all

# List instances with specific display name
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --display-name "web-server"

# List running instances only
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --lifecycle-state RUNNING

# Get instance details
oci compute instance get \
  --instance-id ocid1.instance...
```

### Launch Instance

```bash
oci compute instance launch \
  --availability-domain "AD-1" \
  --compartment-id ocid1.compartment... \
  --shape "VM.Standard.E4.Flex" \
  --shape-config '{"ocpus": 2, "memoryInGBs": 16}' \
  --display-name "web-server-01" \
  --image-id ocid1.image... \
  --subnet-id ocid1.subnet... \
  --assign-public-ip true \
  --ssh-authorized-keys-file ~/.ssh/id_rsa.pub \
  --wait-for-state RUNNING
```

### Manage Instance State

```bash
# Stop instance
oci compute instance action \
  --instance-id ocid1.instance... \
  --action STOP \
  --wait-for-state STOPPED

# Start instance
oci compute instance action \
  --instance-id ocid1.instance... \
  --action START \
  --wait-for-state RUNNING

# Restart instance
oci compute instance action \
  --instance-id ocid1.instance... \
  --action RESET

# Terminate instance
oci compute instance terminate \
  --instance-id ocid1.instance... \
  --preserve-boot-volume false \
  --force
```

### Console Connection

```bash
# Create instance console connection
oci compute instance-console-connection create \
  --instance-id ocid1.instance... \
  --ssh-public-key-file ~/.ssh/id_rsa.pub

# Get console connection string
oci compute instance-console-connection get \
  --instance-console-connection-id ocid1.instanceconsoleconnection...

# Connect to console
ssh -o ProxyCommand='ssh -W %h:%p -p 443 ocid1.instanceconsoleconnection...@instance-console.us-ashburn-1.oraclecloud.com' ocid1.instance...
```

## Networking Commands

### VCN Operations

```bash
# List VCNs
oci network vcn list \
  --compartment-id ocid1.compartment...

# Create VCN
oci network vcn create \
  --compartment-id ocid1.compartment... \
  --display-name "main-vcn" \
  --cidr-block "10.0.0.0/16" \
  --dns-label "mainvcn"

# Get VCN details
oci network vcn get \
  --vcn-id ocid1.vcn...

# Delete VCN
oci network vcn delete \
  --vcn-id ocid1.vcn... \
  --force
```

### Subnet Operations

```bash
# List subnets
oci network subnet list \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn...

# Create subnet
oci network subnet create \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn... \
  --display-name "public-subnet" \
  --cidr-block "10.0.1.0/24" \
  --dns-label "public" \
  --route-table-id ocid1.routetable...

# Update subnet
oci network subnet update \
  --subnet-id ocid1.subnet... \
  --route-table-id ocid1.routetable... \
  --force
```

### Security Lists

```bash
# List security lists
oci network security-list list \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn...

# Create security list
oci network security-list create \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn... \
  --display-name "web-security-list" \
  --ingress-security-rules '[{"source":"0.0.0.0/0","protocol":"6","tcpOptions":{"destinationPortRange":{"min":80,"max":80}}}]' \
  --egress-security-rules '[{"destination":"0.0.0.0/0","protocol":"all"}]'
```

### Network Security Groups

```bash
# List NSGs
oci network nsg list \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn...

# Create NSG
oci network nsg create \
  --compartment-id ocid1.compartment... \
  --vcn-id ocid1.vcn... \
  --display-name "web-nsg"

# Add NSG rule
oci network nsg rules add \
  --nsg-id ocid1.networksecuritygroup... \
  --security-rules '[{
    "direction": "INGRESS",
    "protocol": "6",
    "source": "0.0.0.0/0",
    "tcpOptions": {"destinationPortRange": {"min": 443, "max": 443}}
  }]'
```

### Load Balancer

```bash
# List load balancers
oci lb load-balancer list \
  --compartment-id ocid1.compartment...

# Create load balancer
oci lb load-balancer create \
  --compartment-id ocid1.compartment... \
  --display-name "app-lb" \
  --shape-name "flexible" \
  --subnet-ids '["ocid1.subnet..."]' \
  --shape-details '{"minimumBandwidthInMbps": 10, "maximumBandwidthInMbps": 100}' \
  --wait-for-state SUCCEEDED

# Get load balancer details
oci lb load-balancer get \
  --load-balancer-id ocid1.loadbalancer...
```

## Storage Commands

### Block Volumes

```bash
# List volumes
oci bv volume list \
  --compartment-id ocid1.compartment... \
  --availability-domain "AD-1"

# Create volume
oci bv volume create \
  --compartment-id ocid1.compartment... \
  --availability-domain "AD-1" \
  --display-name "data-volume" \
  --size-in-gbs 100 \
  --vpus-per-gb 20 \
  --wait-for-state AVAILABLE

# Attach volume to instance
oci compute volume-attachment attach \
  --instance-id ocid1.instance... \
  --type paravirtualized \
  --volume-id ocid1.volume... \
  --device "/dev/oracleoci/oraclevdb" \
  --wait-for-state ATTACHED

# Detach volume
oci compute volume-attachment detach \
  --volume-attachment-id ocid1.volumeattachment... \
  --force \
  --wait-for-state DETACHED

# Create volume backup
oci bv backup create \
  --volume-id ocid1.volume... \
  --display-name "data-volume-backup" \
  --type INCREMENTAL
```

### Object Storage

```bash
# List buckets
oci os bucket list \
  --compartment-id ocid1.compartment... \
  --namespace-name your-namespace

# Create bucket
oci os bucket create \
  --compartment-id ocid1.compartment... \
  --name "my-bucket" \
  --namespace-name your-namespace \
  --public-access-type NoPublicAccess

# Upload object
oci os object put \
  --bucket-name "my-bucket" \
  --namespace-name your-namespace \
  --file /path/to/file.txt \
  --name "file.txt"

# Download object
oci os object get \
  --bucket-name "my-bucket" \
  --namespace-name your-namespace \
  --name "file.txt" \
  --file /path/to/download/file.txt

# List objects
oci os object list \
  --bucket-name "my-bucket" \
  --namespace-name your-namespace

# Delete object
oci os object delete \
  --bucket-name "my-bucket" \
  --namespace-name your-namespace \
  --name "file.txt" \
  --force

# Generate pre-authenticated request
oci os preauth-request create \
  --bucket-name "my-bucket" \
  --namespace-name your-namespace \
  --name "download-link" \
  --access-type ObjectRead \
  --time-expires "2024-12-31T23:59:59+00:00" \
  --object-name "file.txt"
```

## Database Commands

### Autonomous Database

```bash
# List Autonomous Databases
oci db autonomous-database list \
  --compartment-id ocid1.compartment...

# Create Autonomous Database
oci db autonomous-database create \
  --compartment-id ocid1.compartment... \
  --db-name "ATPDB" \
  --display-name "ATP Database" \
  --admin-password "MyPassword123!" \
  --cpu-core-count 1 \
  --data-storage-size-in-tbs 1 \
  --db-workload "OLTP" \
  --is-auto-scaling-enabled true \
  --wait-for-state AVAILABLE

# Stop Autonomous Database
oci db autonomous-database stop \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --wait-for-state STOPPED

# Start Autonomous Database
oci db autonomous-database start \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --wait-for-state AVAILABLE

# Download wallet
oci db autonomous-database generate-wallet \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --file wallet.zip \
  --password "WalletPassword123!"

# Create backup
oci db autonomous-database create-backup \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --display-name "manual-backup"

# Restore from backup
oci db autonomous-database restore \
  --autonomous-database-id ocid1.autonomousdatabase... \
  --timestamp "2024-01-15T10:00:00.000Z"
```

### MySQL Database

```bash
# List MySQL DB Systems
oci mysql db-system list \
  --compartment-id ocid1.compartment...

# Create MySQL DB System
oci mysql db-system create \
  --compartment-id ocid1.compartment... \
  --shape-name "MySQL.VM.Standard.E4.1.8GB" \
  --subnet-id ocid1.subnet... \
  --admin-username "admin" \
  --admin-password "MyPassword123!" \
  --availability-domain "AD-1" \
  --display-name "main-mysql" \
  --data-storage-size-in-gbs 50 \
  --is-highly-available true \
  --wait-for-state ACTIVE

# Stop MySQL DB System
oci mysql db-system stop \
  --db-system-id ocid1.mysqldbsystem... \
  --wait-for-state INACTIVE

# Create backup
oci mysql backup create \
  --db-system-id ocid1.mysqldbsystem... \
  --display-name "manual-backup"
```

## Container and Kubernetes Commands

### Container Registry

```bash
# List container repositories
oci artifacts container repository list \
  --compartment-id ocid1.compartment...

# Create repository
oci artifacts container repository create \
  --compartment-id ocid1.compartment... \
  --display-name "myapp"

# List images
oci artifacts container image list \
  --compartment-id ocid1.compartment... \
  --repository-name "myapp"
```

### OKE (Oracle Kubernetes Engine)

```bash
# List clusters
oci ce cluster list \
  --compartment-id ocid1.compartment...

# Create cluster
oci ce cluster create \
  --compartment-id ocid1.compartment... \
  --name "oke-cluster" \
  --vcn-id ocid1.vcn... \
  --kubernetes-version "v1.28.2" \
  --wait-for-state SUCCEEDED

# Get cluster details
oci ce cluster get \
  --cluster-id ocid1.cluster...

# Generate kubeconfig
oci ce cluster create-kubeconfig \
  --cluster-id ocid1.cluster... \
  --file $HOME/.kube/config \
  --region us-ashburn-1 \
  --token-version 2.0.0

# List node pools
oci ce node-pool list \
  --compartment-id ocid1.compartment... \
  --cluster-id ocid1.cluster...

# Create node pool
oci ce node-pool create \
  --cluster-id ocid1.cluster... \
  --compartment-id ocid1.compartment... \
  --name "node-pool-1" \
  --node-shape "VM.Standard.E4.Flex" \
  --node-shape-config '{"ocpus": 2, "memoryInGBs": 16}' \
  --node-image-id ocid1.image... \
  --size 3 \
  --subnet-ids '["ocid1.subnet..."]'
```

## IAM Commands

### User Management

```bash
# List users
oci iam user list \
  --compartment-id ocid1.tenancy...

# Create user
oci iam user create \
  --compartment-id ocid1.tenancy... \
  --name "john.doe@example.com" \
  --description "Developer user" \
  --email "john.doe@example.com"

# Upload API key
oci iam user api-key upload \
  --user-id ocid1.user... \
  --key-file ~/.ssh/oci_api_key_public.pem
```

### Group Management

```bash
# List groups
oci iam group list \
  --compartment-id ocid1.tenancy...

# Create group
oci iam group create \
  --compartment-id ocid1.tenancy... \
  --name "Developers" \
  --description "Developer group"

# Add user to group
oci iam group add-user \
  --group-id ocid1.group... \
  --user-id ocid1.user...
```

### Policy Management

```bash
# List policies
oci iam policy list \
  --compartment-id ocid1.compartment...

# Create policy
oci iam policy create \
  --compartment-id ocid1.compartment... \
  --name "DeveloperPolicy" \
  --description "Developer access policy" \
  --statements '["Allow group Developers to manage instances in compartment App1-Dev"]'

# Update policy
oci iam policy update \
  --policy-id ocid1.policy... \
  --statements '["Allow group Developers to manage all-resources in compartment App1-Dev"]' \
  --force
```

### Compartment Management

```bash
# List compartments
oci iam compartment list \
  --compartment-id ocid1.tenancy... \
  --all

# Create compartment
oci iam compartment create \
  --compartment-id ocid1.tenancy... \
  --name "App1-Prod" \
  --description "Production compartment for App1"

# Move compartment
oci iam compartment move \
  --compartment-id ocid1.compartment... \
  --target-compartment-id ocid1.compartment.new...
```

## Monitoring and Logging

### Monitoring Metrics

```bash
# List metrics
oci monitoring metric list \
  --compartment-id ocid1.compartment... \
  --namespace oci_computeagent

# Query metric data
oci monitoring metric-data summarize-metrics-data \
  --compartment-id ocid1.compartment... \
  --namespace oci_computeagent \
  --query-text "CpuUtilization[1m].mean()" \
  --start-time "2024-01-15T00:00:00.000Z" \
  --end-time "2024-01-15T23:59:59.000Z"
```

### Alarms

```bash
# List alarms
oci monitoring alarm list \
  --compartment-id ocid1.compartment...

# Create alarm
oci monitoring alarm create \
  --compartment-id ocid1.compartment... \
  --display-name "high-cpu-alarm" \
  --metric-compartment-id ocid1.compartment... \
  --namespace oci_computeagent \
  --query "CpuUtilization[1m].mean() > 80" \
  --severity "CRITICAL" \
  --destinations '["ocid1.onstopic..."]' \
  --is-enabled true
```

### Logging

```bash
# List logs
oci logging log list \
  --log-group-id ocid1.loggroup...

# Create log
oci logging log create \
  --log-group-id ocid1.loggroup... \
  --display-name "app-logs" \
  --log-type SERVICE \
  --configuration file://log-config.json
```

## Query and Filtering

### Using JMESPath Queries

```bash
# Get only instance IDs
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --query 'data[*].id' \
  --raw-output

# Get instance names and IPs
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --query 'data[*].{"Name":"display-name","IP":"private-ip"}'

# Filter by lifecycle state
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --query 'data[?lifecycle-state==`RUNNING`].{Name:"display-name",State:"lifecycle-state"}'

# Count instances
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --query 'length(data)'
```

### Output Formats

```bash
# JSON output (default)
oci compute instance list --compartment-id ocid1.compartment...

# Table output
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --output table

# Raw output (no formatting)
oci compute instance get \
  --instance-id ocid1.instance... \
  --query 'data."display-name"' \
  --raw-output
```

## Bulk Operations

### Bulk Instance Management

```bash
# Stop all instances in compartment
for instance in $(oci compute instance list \
  --compartment-id ocid1.compartment... \
  --lifecycle-state RUNNING \
  --query 'data[*].id' \
  --raw-output); do
    echo "Stopping instance: $instance"
    oci compute instance action \
      --instance-id $instance \
      --action STOP
done

# Tag all instances
for instance in $(oci compute instance list \
  --compartment-id ocid1.compartment... \
  --query 'data[*].id' \
  --raw-output); do
    oci compute instance update \
      --instance-id $instance \
      --freeform-tags '{"Environment":"Production","ManagedBy":"OCI-CLI"}'
done
```

## Troubleshooting Commands

### Debug Mode

```bash
# Enable debug output
export OCI_CLI_DEBUG=true
oci compute instance list --compartment-id ocid1.compartment...

# Or use --debug flag
oci compute instance list \
  --compartment-id ocid1.compartment... \
  --debug
```

### Connection Testing

```bash
# Test connectivity
curl -I https://objectstorage.us-ashburn-1.oraclecloud.com

# Verify authentication
oci iam region list

# Check API endpoint
oci iam availability-domain list \
  --compartment-id ocid1.tenancy...
```

### Common Issues

**Issue**: `ServiceError: Authorization failed or requested resource not found`
**Solution**: Verify compartment ID and IAM policies

**Issue**: `ServiceError: Service limit exceeded`
**Solution**: Check service limits and request increase

**Issue**: `Connection timeout`
**Solution**: Check network connectivity and proxy settings

## Best Practices

1. **Use Config Profiles**: Separate profiles for different environments
2. **Leverage Queries**: Use --query for efficient data extraction
3. **Wait for State**: Use --wait-for-state for synchronous operations
4. **Enable Pagination**: Use --all for complete results
5. **Script Carefully**: Check exit codes and handle errors
6. **Use Variables**: Store OCIDs in variables for reusability
7. **Audit Commands**: Log all CLI operations for compliance
