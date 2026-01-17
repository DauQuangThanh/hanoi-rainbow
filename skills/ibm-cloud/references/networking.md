# Networking

## Table of Contents
- [VPC Networking](#vpc-networking)
- [Load Balancers](#load-balancers)
- [Cloud Internet Services](#cloud-internet-services)
- [Direct Link](#direct-link)
- [Transit Gateway](#transit-gateway)
- [VPN](#vpn)

---

## VPC Networking

### Subnets and Routing

```bash
# Create subnet
ibmcloud is subnet-create my-subnet my-vpc \
  --ipv4-cidr-block 10.240.1.0/24 \
  --zone us-south-1

# Create routing table
ibmcloud is vpc-routing-table-create my-vpc \
  --name custom-routes

# Add route
ibmcloud is vpc-routing-table-route-create my-vpc <RT-ID> \
  --zone us-south-1 \
  --destination 192.168.1.0/24 \
  --next-hop <GATEWAY-IP>
```

### Security Groups

```bash
# Create security group
ibmcloud is security-group-create web-sg my-vpc

# Add rules
ibmcloud is security-group-rule-add web-sg inbound tcp \
  --port-min 443 --port-max 443 --remote 0.0.0.0/0
```

### Network ACLs

```bash
# Create ACL
ibmcloud is network-acl-create my-acl my-vpc

# Add inbound rule
ibmcloud is network-acl-rule-add my-acl allow inbound all \
  0.0.0.0/0 443 443 --before <RULE-ID>
```

---

## Load Balancers

### Application Load Balancer

**Create ALB:**
```bash
# Create load balancer
ibmcloud is load-balancer-create my-alb public \
  --subnet <SUBNET-ID-1> \
  --subnet <SUBNET-ID-2> \
  --subnet <SUBNET-ID-3>

# Create backend pool
ibmcloud is load-balancer-pool-create my-pool my-alb \
  --algorithm round_robin \
  --protocol http \
  --health-delay 5 \
  --health-max-retries 2 \
  --health-timeout 2 \
  --health-type http \
  --health-monitor-url /health

# Add pool members
ibmcloud is load-balancer-pool-member-create my-alb my-pool \
  8080 <VSI-IP> --weight 50

# Create listener
ibmcloud is load-balancer-listener-create my-alb \
  443 https \
  --certificate-instance <CERT-CRN> \
  --default-pool my-pool
```

**Terraform:**
```hcl
resource "ibm_is_lb" "alb" {
  name    = "my-alb"
  subnets = [
    ibm_is_subnet.subnet_1.id,
    ibm_is_subnet.subnet_2.id,
    ibm_is_subnet.subnet_3.id
  ]
  type = "public"
}

resource "ibm_is_lb_pool" "pool" {
  lb                      = ibm_is_lb.alb.id
  name                    = "my-pool"
  protocol                = "http"
  algorithm               = "round_robin"
  health_delay            = 5
  health_retries          = 2
  health_timeout          = 2
  health_type             = "http"
  health_monitor_url      = "/health"
  health_monitor_port     = 8080
}

resource "ibm_is_lb_pool_member" "member" {
  lb             = ibm_is_lb.alb.id
  pool           = ibm_is_lb_pool.pool.id
  port           = 8080
  target_address = ibm_is_instance.vsi.primary_network_interface[0].primary_ipv4_address
  weight         = 50
}

resource "ibm_is_lb_listener" "listener" {
  lb                   = ibm_is_lb.alb.id
  port                 = 443
  protocol             = "https"
  certificate_instance = ibm_sm_certificate.cert.crn
  default_pool         = ibm_is_lb_pool.pool.id
}
```

### Network Load Balancer

```bash
# Create NLB
ibmcloud is load-balancer-create my-nlb public \
  --subnet <SUBNET-ID> \
  --profile network-fixed
```

---

## Cloud Internet Services

### DNS Management

```bash
# Create CIS instance
ibmcloud resource service-instance-create my-cis \
  internet-svcs standard global

# Add domain
ibmcloud cis domain-add <CIS-INSTANCE-ID> example.com

# Create DNS record
ibmcloud cis dns-record-create <CIS-INSTANCE-ID> <DOMAIN-ID> \
  --type A --name www --content 203.0.113.10 --proxied true
```

### CDN Configuration

```bash
# Enable CDN
ibmcloud cis cache-settings-update <CIS-INSTANCE-ID> <DOMAIN-ID> \
  --value aggressive
```

### DDoS Protection

Automatically enabled with Cloud Internet Services.

### Web Application Firewall

```bash
# Enable WAF
ibmcloud cis waf-package-set <CIS-INSTANCE-ID> <DOMAIN-ID> <PACKAGE-ID> \
  --mode on
```

---

## Direct Link

### Overview

Dedicated private connection between on-premises and IBM Cloud.

**Types:**
- **Direct Link Dedicated**: Single-tenant 1-10 Gbps
- **Direct Link Connect**: Multi-tenant via partners

### Create Direct Link

```bash
# Create Direct Link
ibmcloud dl gateway-create \
  --name my-direct-link \
  --type dedicated \
  --speed-mbps 1000 \
  --bgp-asn 65000 \
  --bgp-base-cidr 169.254.0.0/16 \
  --location <LOCATION>

# Create virtual connection to VPC
ibmcloud dl gateway-vc-create <GATEWAY-ID> \
  --name to-prod-vpc \
  --type vpc \
  --network-id <VPC-CRN>
```

---

## Transit Gateway

### Overview

Connect multiple VPCs and on-premises networks.

```bash
# Create transit gateway
ibmcloud tg gateway-create \
  --name my-tg \
  --location us-south \
  --routing global

# Connect VPC
ibmcloud tg connection-create <TG-ID> \
  --name vpc1-connection \
  --network-type vpc \
  --network-id <VPC-CRN>

# Connect Direct Link
ibmcloud tg connection-create <TG-ID> \
  --name dl-connection \
  --network-type directlink \
  --network-id <DL-CRN>
```

**Terraform:**
```hcl
resource "ibm_tg_gateway" "tg" {
  name     = "my-tg"
  location = "us-south"
  global   = true
}

resource "ibm_tg_connection" "vpc1" {
  gateway      = ibm_tg_gateway.tg.id
  network_type = "vpc"
  name         = "vpc1-connection"
  network_id   = ibm_is_vpc.vpc1.crn
}

resource "ibm_tg_connection" "vpc2" {
  gateway      = ibm_tg_gateway.tg.id
  network_type = "vpc"
  name         = "vpc2-connection"
  network_id   = ibm_is_vpc.vpc2.crn
}
```

---

## VPN

### Site-to-Site VPN

```bash
# Create VPN gateway
ibmcloud is vpn-gateway-create my-vpn-gateway \
  --subnet <SUBNET-ID>

# Create VPN connection
ibmcloud is vpn-gateway-connection-create my-connection my-vpn-gateway \
  --peer-address 203.0.113.5 \
  --psk mysecretkey \
  --local-cidr 10.240.0.0/16 \
  --peer-cidr 192.168.0.0/16
```

**Terraform:**
```hcl
resource "ibm_is_vpn_gateway" "vpn" {
  name   = "my-vpn-gateway"
  subnet = ibm_is_subnet.subnet.id
}

resource "ibm_is_vpn_gateway_connection" "connection" {
  name          = "my-connection"
  vpn_gateway   = ibm_is_vpn_gateway.vpn.id
  peer_address  = "203.0.113.5"
  preshared_key = "mysecretkey"
  local_cidrs   = ["10.240.0.0/16"]
  peer_cidrs    = ["192.168.0.0/16"]
}
```

### Client VPN

Use IBM Cloud VPN for Clients or third-party solutions.

---

## Best Practices

### High Availability

1. **Multi-Zone**: Deploy across 3 zones
2. **Load Balancing**: Use ALB/NLB
3. **Redundant Connections**: Multiple Direct Link/VPN
4. **Health Checks**: Configure proper monitoring
5. **Auto-Scaling**: Scale based on traffic

### Security

1. **Private Endpoints**: Minimize public exposure
2. **Security Groups**: Least privilege firewall rules
3. **Network ACLs**: Subnet-level protection
4. **DDoS Protection**: Enable CIS
5. **WAF**: Protect web applications

### Performance

1. **CDN**: Use CIS for static content
2. **Direct Link**: Low-latency private connectivity
3. **Transit Gateway**: Optimize inter-VPC routing
4. **DNS**: Use CIS for global load balancing
5. **Compression**: Enable at load balancer

### Cost Optimization

1. **Private Networking**: Free data transfer
2. **Regional Traffic**: Stay within region
3. **CDN Caching**: Reduce origin requests
4. **Right-Size Bandwidth**: Match Direct Link to needs
5. **Compression**: Reduce data transfer costs
