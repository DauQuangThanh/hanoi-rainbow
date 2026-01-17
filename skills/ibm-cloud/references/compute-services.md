# Compute Services

## Table of Contents
- [Virtual Private Cloud (VPC)](#virtual-private-cloud-vpc)
- [Virtual Server Instances](#virtual-server-instances)
- [IBM Kubernetes Service (IKS)](#ibm-kubernetes-service-iks)
- [Red Hat OpenShift](#red-hat-openshift)
- [Code Engine](#code-engine)
- [Cloud Functions](#cloud-functions)
- [Bare Metal Servers](#bare-metal-servers)

---

## Virtual Private Cloud (VPC)

### Overview

VPC provides isolated, software-defined network infrastructure in IBM Cloud with complete control over IP addressing, routing, security, and connectivity.

### VPC Architecture

```
VPC (10.240.0.0/16)
├── Zone 1 (us-south-1)
│   ├── Subnet 1 (10.240.1.0/24) - Web Tier
│   └── Subnet 2 (10.240.2.0/24) - App Tier
├── Zone 2 (us-south-2)
│   ├── Subnet 3 (10.240.3.0/24) - Web Tier
│   └── Subnet 4 (10.240.4.0/24) - App Tier
└── Zone 3 (us-south-3)
    ├── Subnet 5 (10.240.5.0/24) - Web Tier
    └── Subnet 6 (10.240.6.0/24) - Database Tier
```

### Create VPC

**CLI:**
```bash
# Create VPC
ibmcloud is vpc-create my-vpc \
  --resource-group-name my-rg \
  --address-prefix-management manual

# Add address prefix
ibmcloud is vpc-address-prefix-create my-vpc \
  us-south-1 vpc-prefix-1 10.240.1.0/24

# Create subnet
ibmcloud is subnet-create my-subnet-1 my-vpc \
  --ipv4-address-count 256 \
  --zone us-south-1 \
  --ipv4-cidr-block 10.240.1.0/24

# Create public gateway (for internet access)
ibmcloud is public-gateway-create my-pgw \
  my-vpc us-south-1

# Attach public gateway to subnet
ibmcloud is subnet-update my-subnet-1 \
  --public-gateway my-pgw
```

**Terraform:**
```hcl
resource "ibm_is_vpc" "vpc" {
  name                      = "my-vpc"
  resource_group            = ibm_resource_group.rg.id
  address_prefix_management = "manual"
}

resource "ibm_is_vpc_address_prefix" "prefix" {
  name = "vpc-prefix-1"
  zone = "us-south-1"
  vpc  = ibm_is_vpc.vpc.id
  cidr = "10.240.1.0/24"
}

resource "ibm_is_subnet" "subnet" {
  name                     = "my-subnet-1"
  vpc                      = ibm_is_vpc.vpc.id
  zone                     = "us-south-1"
  ipv4_cidr_block          = "10.240.1.0/24"
  public_gateway           = ibm_is_public_gateway.pgw.id
  resource_group           = ibm_resource_group.rg.id
}

resource "ibm_is_public_gateway" "pgw" {
  name           = "my-pgw"
  vpc            = ibm_is_vpc.vpc.id
  zone           = "us-south-1"
  resource_group = ibm_resource_group.rg.id
}
```

### Security Groups

```bash
# Create security group
ibmcloud is security-group-create my-sg my-vpc \
  --resource-group-name my-rg

# Allow SSH (port 22)
ibmcloud is security-group-rule-add my-sg inbound tcp \
  --port-min 22 --port-max 22 \
  --remote 0.0.0.0/0

# Allow HTTP (port 80)
ibmcloud is security-group-rule-add my-sg inbound tcp \
  --port-min 80 --port-max 80 \
  --remote 0.0.0.0/0

# Allow HTTPS (port 443)
ibmcloud is security-group-rule-add my-sg inbound tcp \
  --port-min 443 --port-max 443 \
  --remote 0.0.0.0/0

# Allow all outbound
ibmcloud is security-group-rule-add my-sg outbound all \
  --remote 0.0.0.0/0
```

### Network ACLs

```bash
# Create network ACL
ibmcloud is network-acl-create my-acl my-vpc

# Add inbound rule (allow HTTP)
ibmcloud is network-acl-rule-add my-acl allow inbound all \
  0.0.0.0/0 80 80

# Add outbound rule (allow all)
ibmcloud is network-acl-rule-add my-acl allow outbound all \
  0.0.0.0/0

# Attach to subnet
ibmcloud is subnet-update my-subnet-1 --network-acl my-acl
```

---

## Virtual Server Instances

### Instance Profiles

**Balanced Profiles (bx):**
- `bx2-2x8`: 2 vCPU, 8GB RAM
- `bx2-4x16`: 4 vCPU, 16GB RAM
- `bx2-8x32`: 8 vCPU, 32GB RAM
- `bx2-16x64`: 16 vCPU, 64GB RAM

**Compute Profiles (cx):**
- `cx2-2x4`: 2 vCPU, 4GB RAM (CPU-optimized)
- `cx2-4x8`: 4 vCPU, 8GB RAM

**Memory Profiles (mx):**
- `mx2-2x16`: 2 vCPU, 16GB RAM (memory-optimized)
- `mx2-4x32`: 4 vCPU, 32GB RAM

### Create Virtual Server

**CLI:**
```bash
# List available images
ibmcloud is images

# Create SSH key
ibmcloud is key-create my-ssh-key @~/.ssh/id_rsa.pub

# Create instance
ibmcloud is instance-create my-vsi my-vpc us-south-1 \
  bx2-2x8 my-subnet-1 \
  --image ibm-ubuntu-20-04-minimal-amd64-1 \
  --keys my-ssh-key \
  --security-groups my-sg \
  --resource-group-name my-rg

# Reserve floating IP
ibmcloud is floating-ip-reserve my-fip \
  --zone us-south-1

# Attach floating IP to instance
ibmcloud is instance-network-interface-floating-ip-add \
  my-vsi <NIC-ID> my-fip

# SSH to instance
ssh root@<FLOATING-IP>
```

**Terraform:**
```hcl
resource "ibm_is_ssh_key" "ssh_key" {
  name       = "my-ssh-key"
  public_key = file("~/.ssh/id_rsa.pub")
}

resource "ibm_is_instance" "vsi" {
  name    = "my-vsi"
  image   = data.ibm_is_image.ubuntu.id
  profile = "bx2-2x8"
  vpc     = ibm_is_vpc.vpc.id
  zone    = "us-south-1"
  keys    = [ibm_is_ssh_key.ssh_key.id]

  primary_network_interface {
    name            = "eth0"
    subnet          = ibm_is_subnet.subnet.id
    security_groups = [ibm_is_security_group.sg.id]
  }

  boot_volume {
    name = "my-boot-volume"
  }

  resource_group = ibm_resource_group.rg.id
}

resource "ibm_is_floating_ip" "fip" {
  name   = "my-fip"
  target = ibm_is_instance.vsi.primary_network_interface[0].id
}
```

### Instance Operations

```bash
# Start instance
ibmcloud is instance-start my-vsi

# Stop instance
ibmcloud is instance-stop my-vsi

# Reboot instance
ibmcloud is instance-reboot my-vsi

# Delete instance
ibmcloud is instance-delete my-vsi

# Get instance details
ibmcloud is instance my-vsi

# List instances
ibmcloud is instances
```

### User Data / Cloud-init

```bash
# Create instance with user data
ibmcloud is instance-create my-vsi my-vpc us-south-1 \
  bx2-2x8 my-subnet-1 \
  --image ibm-ubuntu-20-04-minimal-amd64-1 \
  --keys my-ssh-key \
  --user-data @cloud-init.yaml
```

**cloud-init.yaml:**
```yaml
#cloud-config
package_update: true
package_upgrade: true

packages:
  - nginx
  - docker.io
  - git

runcmd:
  - systemctl enable nginx
  - systemctl start nginx
  - usermod -aG docker ubuntu
  - echo "Hello from IBM Cloud" > /var/www/html/index.html
```

---

## IBM Kubernetes Service (IKS)

### Cluster Types

**Free Cluster:**
- 1 worker node (2 vCPU, 4GB RAM)
- Single zone
- 30-day expiration
- Good for testing

**Standard Cluster:**
- Multiple worker nodes
- Multi-zone for high availability
- Scalable
- Production-ready

### Create IKS Cluster

**CLI:**
```bash
# List available zones
ibmcloud ks zones --provider vpc-gen2

# List available flavors
ibmcloud ks flavors --zone us-south-1 --provider vpc-gen2

# Create cluster
ibmcloud ks cluster create vpc-gen2 \
  --name my-cluster \
  --version 1.28 \
  --flavor bx2.4x16 \
  --workers 3 \
  --vpc-id <VPC-ID> \
  --subnet-id <SUBNET-ID> \
  --zone us-south-1 \
  --disable-public-service-endpoint

# Check cluster status
ibmcloud ks cluster get --cluster my-cluster

# Get cluster config
ibmcloud ks cluster config --cluster my-cluster

# Verify connection
kubectl get nodes
```

**Multi-Zone Cluster:**
```bash
# Create cluster in zone 1
ibmcloud ks cluster create vpc-gen2 \
  --name my-cluster \
  --flavor bx2.4x16 \
  --workers 2 \
  --vpc-id <VPC-ID> \
  --subnet-id <SUBNET-ID-ZONE-1> \
  --zone us-south-1

# Add zone 2
ibmcloud ks zone add vpc-gen2 \
  --cluster my-cluster \
  --subnet-id <SUBNET-ID-ZONE-2> \
  --zone us-south-2 \
  --worker-pool default

# Add zone 3
ibmcloud ks zone add vpc-gen2 \
  --cluster my-cluster \
  --subnet-id <SUBNET-ID-ZONE-3> \
  --zone us-south-3 \
  --worker-pool default
```

**Terraform:**
```hcl
resource "ibm_container_vpc_cluster" "cluster" {
  name              = "my-cluster"
  vpc_id            = ibm_is_vpc.vpc.id
  flavor            = "bx2.4x16"
  worker_count      = 3
  kube_version      = "1.28"
  resource_group_id = ibm_resource_group.rg.id

  zones {
    name      = "us-south-1"
    subnet_id = ibm_is_subnet.subnet_1.id
  }

  zones {
    name      = "us-south-2"
    subnet_id = ibm_is_subnet.subnet_2.id
  }

  zones {
    name      = "us-south-3"
    subnet_id = ibm_is_subnet.subnet_3.id
  }

  disable_public_service_endpoint = false
}
```

### Worker Pool Management

```bash
# List worker pools
ibmcloud ks worker-pool ls --cluster my-cluster

# Create new worker pool
ibmcloud ks worker-pool create vpc-gen2 \
  --name my-pool \
  --cluster my-cluster \
  --flavor bx2.8x32 \
  --size-per-zone 2

# Add zone to worker pool
ibmcloud ks zone add vpc-gen2 \
  --cluster my-cluster \
  --worker-pool my-pool \
  --subnet-id <SUBNET-ID> \
  --zone us-south-2

# Resize worker pool
ibmcloud ks worker-pool resize \
  --cluster my-cluster \
  --worker-pool my-pool \
  --size-per-zone 3

# Enable autoscaling
ibmcloud ks cluster autoscale set \
  --cluster my-cluster \
  --worker-pool my-pool \
  --min 2 --max 10 \
  --enable
```

### Deploy Application

```bash
# Deploy nginx
kubectl create deployment nginx --image=nginx:latest

# Expose deployment
kubectl expose deployment nginx \
  --type=LoadBalancer \
  --port=80 \
  --target-port=80

# Get service details
kubectl get svc nginx

# Scale deployment
kubectl scale deployment nginx --replicas=3

# Update image
kubectl set image deployment/nginx nginx=nginx:1.21
```

### Ingress Configuration

```yaml
# ingress.yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: my-ingress
  annotations:
    kubernetes.io/ingress.class: "public-iks-k8s-nginx"
spec:
  tls:
  - hosts:
    - myapp.example.com
    secretName: my-tls-secret
  rules:
  - host: myapp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: my-service
            port:
              number: 80
```

```bash
# Apply ingress
kubectl apply -f ingress.yaml

# Get ingress details
kubectl get ingress
```

---

## Red Hat OpenShift

### Create OpenShift Cluster

**CLI:**
```bash
# Create OpenShift cluster
ibmcloud oc cluster create vpc-gen2 \
  --name my-openshift-cluster \
  --version 4.12_openshift \
  --flavor bx2.4x16 \
  --workers 3 \
  --vpc-id <VPC-ID> \
  --subnet-id <SUBNET-ID> \
  --zone us-south-1

# Get cluster config
ibmcloud oc cluster config --cluster my-openshift-cluster

# Login to OpenShift
oc login

# Get cluster info
oc cluster-info
```

### Deploy Application

```bash
# Create new project
oc new-project my-app

# Deploy from source code
oc new-app https://github.com/username/repo

# Expose service
oc expose svc/my-app

# Get route
oc get route
```

---

## Code Engine

### Overview

Serverless container platform that auto-scales applications and runs batch jobs.

**Key Features:**
- Auto-scaling from 0 to N instances
- Pay only for actual usage (CPU, memory, requests)
- Source-to-image builds
- Event-driven architecture
- Integrated with IBM Cloud services

### Create Project

```bash
# Create Code Engine project
ibmcloud ce project create --name my-project

# Select project
ibmcloud ce project select --name my-project

# List projects
ibmcloud ce project list
```

### Deploy Application

**From Container Image:**
```bash
# Deploy application
ibmcloud ce application create \
  --name my-app \
  --image us.icr.io/namespace/my-image:latest \
  --registry-secret my-registry-secret \
  --cpu 0.5 \
  --memory 1G \
  --min-scale 0 \
  --max-scale 10 \
  --port 8080 \
  --env KEY=value

# Get application URL
ibmcloud ce application get --name my-app
```

**From Source Code:**
```bash
# Build and deploy from Git
ibmcloud ce application create \
  --name my-app \
  --build-source https://github.com/username/repo \
  --build-context-dir /src \
  --strategy dockerfile \
  --dockerfile Dockerfile \
  --min-scale 0 \
  --max-scale 10

# Check build status
ibmcloud ce buildrun list
```

### Environment Variables and Secrets

```bash
# Create secret
ibmcloud ce secret create --name my-secret \
  --from-literal DB_PASSWORD=mysecretpassword

# Create configmap
ibmcloud ce configmap create --name my-config \
  --from-literal API_URL=https://api.example.com

# Update application with secrets
ibmcloud ce application update --name my-app \
  --env-from-secret my-secret \
  --env-from-configmap my-config
```

### Batch Jobs

```bash
# Create job
ibmcloud ce job create \
  --name my-batch-job \
  --image us.icr.io/namespace/batch-processor:latest \
  --cpu 2 \
  --memory 4G \
  --env-from-secret my-secret

# Run job
ibmcloud ce jobrun submit --job my-batch-job

# Check job status
ibmcloud ce jobrun list

# Get job logs
ibmcloud ce jobrun logs --name my-batch-job-run-1
```

### Event Subscriptions

```bash
# Subscribe to Object Storage events
ibmcloud ce subscription cos create \
  --name cos-sub \
  --destination my-app \
  --bucket my-bucket \
  --event-type write

# Subscribe to periodic timer (cron)
ibmcloud ce subscription cron create \
  --name timer-sub \
  --destination my-app \
  --schedule "0 */6 * * *" \
  --data '{"message": "Scheduled execution"}'
```

---

## Cloud Functions

### Overview

Serverless FaaS platform based on Apache OpenWhisk.

**Supported Runtimes:**
- Node.js (12, 14, 16, 18)
- Python (3.9, 3.10, 3.11)
- Go (1.17, 1.19)
- PHP (7.4, 8.0)
- Java (8, 11)
- Ruby (2.5)
- .NET Core (3.1)

### Create Function

**Node.js Example:**
```javascript
// hello.js
function main(params) {
  const name = params.name || 'World';
  return { message: `Hello, ${name}!` };
}

module.exports.main = main;
```

**Deploy:**
```bash
# Create function
ibmcloud fn action create hello hello.js \
  --kind nodejs:18 \
  --web true

# Invoke function
ibmcloud fn action invoke hello --result \
  --param name "IBM Cloud"

# Get function URL
ibmcloud fn action get hello --url
```

**Python Example:**
```python
# handler.py
def main(params):
    name = params.get('name', 'World')
    return {'message': f'Hello, {name}!'}
```

```bash
# Deploy Python function
ibmcloud fn action create hello handler.py \
  --kind python:3.11 \
  --web true
```

### Sequences

```bash
# Create sequence of actions
ibmcloud fn action create mySequence \
  --sequence action1,action2,action3

# Invoke sequence
ibmcloud fn action invoke mySequence --result
```

### Triggers and Rules

```bash
# Create trigger
ibmcloud fn trigger create myTrigger

# Create rule (connect trigger to action)
ibmcloud fn rule create myRule myTrigger hello

# Fire trigger
ibmcloud fn trigger fire myTrigger --param name "Triggered"
```

### Web Actions

```bash
# Create web action
ibmcloud fn action create webHello hello.js \
  --web true \
  --web-secure <SECRET>

# Get web action URL
ibmcloud fn action get webHello --url

# Access via HTTP
curl https://<REGION>.functions.cloud.ibm.com/api/v1/web/<NAMESPACE>/default/webHello?name=User
```

---

## Bare Metal Servers

### Overview

Dedicated physical servers for high-performance workloads.

**Use Cases:**
- High-performance computing (HPC)
- Database servers requiring sustained I/O
- Compliance requirements (no virtualization)
- GPU-accelerated workloads

### Server Profiles

**Compute:**
- `md2-32x192x2-100gb`: 32 cores, 192GB RAM, 2x100GB network

**Balanced:**
- `md3-8x100x100gb`: 8 cores, 100GB RAM

**Memory:**
- `mr3-16x256x100gb`: 16 cores, 256GB RAM

**GPU:**
- `mgp2-48x384x4-100gb`: 48 cores, 384GB RAM, 4x NVIDIA V100

### Provision Bare Metal

```bash
# List profiles
ibmcloud is bare-metal-server-profiles

# Create bare metal server
ibmcloud is bare-metal-server-create \
  --name my-bm-server \
  --zone us-south-1 \
  --profile md2-32x192x2-100gb \
  --image ibm-ubuntu-20-04-minimal-amd64-1 \
  --keys my-ssh-key \
  --pnic-subnet <SUBNET-ID> \
  --vpc <VPC-ID>
```

---

## Best Practices

### High Availability

1. **Multi-Zone Deployment**: Deploy across 3 availability zones
2. **Load Balancing**: Use Application Load Balancer (ALB)
3. **Auto-Scaling**: Configure horizontal pod autoscaler (HPA)
4. **Health Checks**: Implement liveness and readiness probes
5. **Circuit Breakers**: Use service mesh (Istio) for resilience

### Security

1. **Private Endpoints**: Disable public service endpoints
2. **Network Policies**: Restrict pod-to-pod communication
3. **RBAC**: Implement role-based access control
4. **Image Scanning**: Scan container images for vulnerabilities
5. **Secrets Management**: Use Secrets Manager, not env vars

### Performance

1. **Right-Sizing**: Choose appropriate instance/pod sizes
2. **Resource Limits**: Set CPU/memory requests and limits
3. **Caching**: Use Redis/Memcached for session/data caching
4. **CDN**: Serve static assets via Cloud Internet Services
5. **Monitoring**: Track metrics with Cloud Monitoring

### Cost Optimization

1. **Reserved Capacity**: Commit to reserved instances
2. **Spot Instances**: Use for fault-tolerant workloads
3. **Auto-Scaling**: Scale down during low usage
4. **Serverless**: Use Code Engine/Functions for variable load
5. **Storage Tiers**: Use appropriate Object Storage classes
