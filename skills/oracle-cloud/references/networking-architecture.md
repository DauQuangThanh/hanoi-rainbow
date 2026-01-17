# Networking Architecture

## Virtual Cloud Network (VCN) Design

### VCN CIDR Planning

**Best Practices**:
- Use RFC 1918 private address space (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
- Plan for growth: Use /16 for VCN, /24 for subnets
- Avoid overlapping CIDRs with on-premises networks
- Reserve space for peering with other VCNs

**Example CIDR Layout**:
```
VCN: 10.0.0.0/16
├── Public Subnet (Web Tier): 10.0.1.0/24
├── Private Subnet (App Tier): 10.0.2.0/24
├── Private Subnet (DB Tier): 10.0.3.0/24
└── Reserved for future: 10.0.4.0/22
```

### VCN Creation

**Terraform Example**:
```hcl
resource "oci_core_vcn" "main_vcn" {
  compartment_id = var.compartment_id
  cidr_blocks    = ["10.0.0.0/16"]
  display_name   = "main-vcn"
  dns_label      = "mainvcn"
  
  freeform_tags = {
    Environment = "Production"
  }
}

resource "oci_core_internet_gateway" "igw" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "internet-gateway"
  enabled        = true
}

resource "oci_core_nat_gateway" "nat" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "nat-gateway"
}

resource "oci_core_service_gateway" "service_gw" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "service-gateway"
  
  services {
    service_id = data.oci_core_services.all_services.services[0].id
  }
}
```

## Subnet Configuration

### Public Subnet

**Use Cases**: Web servers, load balancers, bastion hosts

```hcl
resource "oci_core_subnet" "public_subnet" {
  compartment_id    = var.compartment_id
  vcn_id            = oci_core_vcn.main_vcn.id
  cidr_block        = "10.0.1.0/24"
  display_name      = "public-subnet"
  dns_label         = "public"
  prohibit_public_ip_on_vnic = false

  route_table_id    = oci_core_route_table.public_rt.id
  security_list_ids = [oci_core_security_list.public_sl.id]
}

resource "oci_core_route_table" "public_rt" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "public-route-table"

  route_rules {
    network_entity_id = oci_core_internet_gateway.igw.id
    destination       = "0.0.0.0/0"
    destination_type  = "CIDR_BLOCK"
  }
}
```

### Private Subnet

**Use Cases**: Application servers, databases, internal services

```hcl
resource "oci_core_subnet" "private_subnet" {
  compartment_id    = var.compartment_id
  vcn_id            = oci_core_vcn.main_vcn.id
  cidr_block        = "10.0.2.0/24"
  display_name      = "private-subnet"
  dns_label         = "private"
  prohibit_public_ip_on_vnic = true

  route_table_id    = oci_core_route_table.private_rt.id
  security_list_ids = [oci_core_security_list.private_sl.id]
}

resource "oci_core_route_table" "private_rt" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "private-route-table"

  route_rules {
    network_entity_id = oci_core_nat_gateway.nat.id
    destination       = "0.0.0.0/0"
    destination_type  = "CIDR_BLOCK"
  }

  route_rules {
    network_entity_id = oci_core_service_gateway.service_gw.id
    destination       = data.oci_core_services.all_services.services[0].cidr_block
    destination_type  = "SERVICE_CIDR_BLOCK"
  }
}
```

## Security Lists

### Public Subnet Security List

```hcl
resource "oci_core_security_list" "public_sl" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "public-security-list"

  # Ingress Rules
  ingress_security_rules {
    protocol    = "6"  # TCP
    source      = "0.0.0.0/0"
    source_type = "CIDR_BLOCK"
    stateless   = false
    
    tcp_options {
      min = 80
      max = 80
    }
    description = "Allow HTTP from internet"
  }

  ingress_security_rules {
    protocol    = "6"  # TCP
    source      = "0.0.0.0/0"
    source_type = "CIDR_BLOCK"
    stateless   = false
    
    tcp_options {
      min = 443
      max = 443
    }
    description = "Allow HTTPS from internet"
  }

  ingress_security_rules {
    protocol    = "6"  # TCP
    source      = var.admin_cidr
    source_type = "CIDR_BLOCK"
    stateless   = false
    
    tcp_options {
      min = 22
      max = 22
    }
    description = "Allow SSH from admin network"
  }

  # Egress Rules
  egress_security_rules {
    protocol         = "all"
    destination      = "0.0.0.0/0"
    destination_type = "CIDR_BLOCK"
    stateless        = false
    description      = "Allow all outbound"
  }
}
```

### Private Subnet Security List

```hcl
resource "oci_core_security_list" "private_sl" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "private-security-list"

  # Ingress from public subnet
  ingress_security_rules {
    protocol    = "6"
    source      = oci_core_subnet.public_subnet.cidr_block
    source_type = "CIDR_BLOCK"
    stateless   = false
    
    tcp_options {
      min = 8080
      max = 8080
    }
    description = "Allow app traffic from public subnet"
  }

  # Ingress within private subnet
  ingress_security_rules {
    protocol    = "all"
    source      = oci_core_subnet.private_subnet.cidr_block
    source_type = "CIDR_BLOCK"
    stateless   = false
    description = "Allow all traffic within private subnet"
  }

  # Egress Rules
  egress_security_rules {
    protocol         = "all"
    destination      = "0.0.0.0/0"
    destination_type = "CIDR_BLOCK"
    stateless        = false
    description      = "Allow all outbound"
  }
}
```

## Network Security Groups (NSG)

### NSG for Web Servers

```hcl
resource "oci_core_network_security_group" "web_nsg" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "web-nsg"
}

resource "oci_core_network_security_group_security_rule" "web_http_ingress" {
  network_security_group_id = oci_core_network_security_group.web_nsg.id
  direction                 = "INGRESS"
  protocol                  = "6"
  source                    = "0.0.0.0/0"
  source_type               = "CIDR_BLOCK"
  stateless                 = false
  
  tcp_options {
    destination_port_range {
      min = 80
      max = 80
    }
  }
  description = "Allow HTTP"
}

resource "oci_core_network_security_group_security_rule" "web_https_ingress" {
  network_security_group_id = oci_core_network_security_group.web_nsg.id
  direction                 = "INGRESS"
  protocol                  = "6"
  source                    = "0.0.0.0/0"
  source_type               = "CIDR_BLOCK"
  stateless                 = false
  
  tcp_options {
    destination_port_range {
      min = 443
      max = 443
    }
  }
  description = "Allow HTTPS"
}
```

### NSG for Database

```hcl
resource "oci_core_network_security_group" "db_nsg" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.main_vcn.id
  display_name   = "db-nsg"
}

resource "oci_core_network_security_group_security_rule" "db_ingress_from_app" {
  network_security_group_id = oci_core_network_security_group.db_nsg.id
  direction                 = "INGRESS"
  protocol                  = "6"
  source                    = oci_core_network_security_group.app_nsg.id
  source_type               = "NETWORK_SECURITY_GROUP"
  stateless                 = false
  
  tcp_options {
    destination_port_range {
      min = 3306
      max = 3306
    }
  }
  description = "Allow MySQL from app tier"
}
```

## Load Balancer Configuration

### Public Load Balancer

```hcl
resource "oci_load_balancer_load_balancer" "public_lb" {
  compartment_id = var.compartment_id
  display_name   = "public-load-balancer"
  shape          = "flexible"
  
  shape_details {
    minimum_bandwidth_in_mbps = 10
    maximum_bandwidth_in_mbps = 100
  }

  subnet_ids = [
    oci_core_subnet.public_subnet.id
  ]

  is_private = false
}

resource "oci_load_balancer_backend_set" "web_backend" {
  load_balancer_id = oci_load_balancer_load_balancer.public_lb.id
  name             = "web-backend-set"
  policy           = "ROUND_ROBIN"

  health_checker {
    protocol          = "HTTP"
    port              = 80
    url_path          = "/health"
    interval_ms       = 10000
    timeout_in_millis = 3000
    retries           = 3
    return_code       = 200
  }
}

resource "oci_load_balancer_listener" "http_listener" {
  load_balancer_id         = oci_load_balancer_load_balancer.public_lb.id
  name                     = "http-listener"
  default_backend_set_name = oci_load_balancer_backend_set.web_backend.name
  port                     = 80
  protocol                 = "HTTP"
}

resource "oci_load_balancer_listener" "https_listener" {
  load_balancer_id         = oci_load_balancer_load_balancer.public_lb.id
  name                     = "https-listener"
  default_backend_set_name = oci_load_balancer_backend_set.web_backend.name
  port                     = 443
  protocol                 = "HTTP"

  ssl_configuration {
    certificate_name        = "ssl-cert"
    verify_peer_certificate = false
  }
}
```

### SSL Certificate Management

```hcl
resource "oci_load_balancer_certificate" "ssl_cert" {
  load_balancer_id   = oci_load_balancer_load_balancer.public_lb.id
  certificate_name   = "ssl-cert"
  ca_certificate     = file("ca-cert.pem")
  private_key        = file("private-key.pem")
  public_certificate = file("public-cert.pem")

  lifecycle {
    create_before_destroy = true
  }
}
```

## VPN and Hybrid Connectivity

### Site-to-Site VPN

```hcl
resource "oci_core_drg" "main_drg" {
  compartment_id = var.compartment_id
  display_name   = "main-drg"
}

resource "oci_core_drg_attachment" "vcn_attachment" {
  drg_id = oci_core_drg.main_drg.id
  vcn_id = oci_core_vcn.main_vcn.id
}

resource "oci_core_cpe" "on_prem_cpe" {
  compartment_id = var.compartment_id
  display_name   = "on-premises-cpe"
  ip_address     = var.on_prem_public_ip
}

resource "oci_core_ipsec" "vpn_connection" {
  compartment_id = var.compartment_id
  cpe_id         = oci_core_cpe.on_prem_cpe.id
  drg_id         = oci_core_drg.main_drg.id
  static_routes  = ["192.168.0.0/16"]
  display_name   = "vpn-connection"
}
```

### FastConnect

```hcl
resource "oci_core_virtual_circuit" "fastconnect" {
  compartment_id = var.compartment_id
  type           = "PRIVATE"
  
  bandwidth_shape_name = "1 Gbps"
  
  customer_bgp_asn = var.customer_bgp_asn
  
  gateway_id = oci_core_drg.main_drg.id
  
  provider_service_id = var.provider_service_id
  
  region = var.region
  
  display_name = "fastconnect-circuit"
}
```

## VCN Peering

### Local Peering (Same Region)

```hcl
resource "oci_core_local_peering_gateway" "lpg1" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.vcn1.id
  display_name   = "lpg-vcn1"
}

resource "oci_core_local_peering_gateway" "lpg2" {
  compartment_id = var.compartment_id
  vcn_id         = oci_core_vcn.vcn2.id
  display_name   = "lpg-vcn2"
  peer_id        = oci_core_local_peering_gateway.lpg1.id
}
```

### Remote Peering (Cross-Region)

```hcl
resource "oci_core_remote_peering_connection" "rpc1" {
  compartment_id = var.compartment_id
  drg_id         = oci_core_drg.drg1.id
  display_name   = "rpc-region1"
}

resource "oci_core_remote_peering_connection" "rpc2" {
  compartment_id = var.compartment_id
  drg_id         = oci_core_drg.drg2.id
  display_name   = "rpc-region2"
  peer_id        = oci_core_remote_peering_connection.rpc1.id
  peer_region_name = var.peer_region
}
```

## DNS Configuration

### Private DNS

```hcl
resource "oci_dns_zone" "private_zone" {
  compartment_id = var.compartment_id
  name           = "internal.example.com"
  zone_type      = "PRIMARY"
  scope          = "PRIVATE"
  
  view_id = oci_dns_view.private_view.id
}

resource "oci_dns_rrset" "app_record" {
  zone_name_or_id = oci_dns_zone.private_zone.id
  domain          = "app.internal.example.com"
  rtype           = "A"
  scope           = "PRIVATE"
  view_id         = oci_dns_view.private_view.id

  items {
    domain = "app.internal.example.com"
    rdata  = "10.0.2.10"
    rtype  = "A"
    ttl    = 300
  }
}
```

## Network Monitoring

### Flow Logs

```hcl
resource "oci_core_capture_filter" "flow_logs_filter" {
  compartment_id = var.compartment_id
  display_name   = "flow-logs-filter"
  
  filter_type = "VTAP"
  
  vtap_capture_filter_rules {
    traffic_direction = "INGRESS"
  }
}
```

## Best Practices Summary

1. **VCN Design**: Use /16 for VCN, /24 for subnets, plan for growth
2. **Security**: Use NSGs for fine-grained control, Security Lists for subnet-level
3. **High Availability**: Deploy across multiple availability domains
4. **Load Balancing**: Use flexible shapes, configure health checks properly
5. **Hybrid Connectivity**: Use FastConnect for production, VPN for dev/test
6. **DNS**: Use private DNS for internal service discovery
7. **Monitoring**: Enable VCN flow logs for troubleshooting
