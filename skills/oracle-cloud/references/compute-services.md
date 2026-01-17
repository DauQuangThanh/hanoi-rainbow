# Compute Services

## VM Instance Configuration

### Instance Shapes

**Flexible Shapes** (VM.Standard.E4.Flex, VM.Optimized3.Flex):
- Configure custom OCPU and memory allocation
- Best for cost optimization and workload-specific sizing
- Memory-to-OCPU ratio: 1:1 to 64:1 GB per OCPU

**Fixed Shapes** (VM.Standard2.x, VM.Standard3.x):
- Pre-configured OCPU and memory
- Predictable pricing
- Use when workload requirements are well-defined

**Bare Metal Shapes** (BM.Standard.E4, BM.DenseIO):
- Dedicated physical server
- No hypervisor overhead
- Use for high-performance, latency-sensitive workloads

### Instance Creation

```bash
# Using OCI CLI
oci compute instance launch \
  --availability-domain "AD-1" \
  --compartment-id "ocid1.compartment..." \
  --shape "VM.Standard.E4.Flex" \
  --shape-config '{"ocpus": 2, "memoryInGBs": 16}' \
  --subnet-id "ocid1.subnet..." \
  --image-id "ocid1.image..." \
  --display-name "web-server-01" \
  --assign-public-ip true \
  --ssh-authorized-keys-file ~/.ssh/id_rsa.pub
```

**Terraform Example**:
```hcl
resource "oci_core_instance" "web_server" {
  availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
  compartment_id      = var.compartment_id
  shape               = "VM.Standard.E4.Flex"
  
  shape_config {
    ocpus         = 2
    memory_in_gbs = 16
  }

  create_vnic_details {
    subnet_id        = oci_core_subnet.public_subnet.id
    assign_public_ip = true
  }

  source_details {
    source_type = "image"
    source_id   = var.image_id
    boot_volume_size_in_gbs = 100
  }

  metadata = {
    ssh_authorized_keys = file("~/.ssh/id_rsa.pub")
    user_data          = base64encode(file("cloud-init.yaml"))
  }

  freeform_tags = {
    Environment = "Production"
    Application = "WebServer"
  }
}
```

## Instance Pools and Autoscaling

### Instance Configuration

Create reusable instance configuration:

```hcl
resource "oci_core_instance_configuration" "app_config" {
  compartment_id = var.compartment_id
  display_name   = "app-instance-config"

  instance_details {
    instance_type = "compute"

    launch_details {
      compartment_id = var.compartment_id
      shape          = "VM.Standard.E4.Flex"
      
      shape_config {
        ocpus         = 2
        memory_in_gbs = 16
      }

      create_vnic_details {
        subnet_id = oci_core_subnet.private_subnet.id
      }

      source_details {
        source_type             = "image"
        image_id                = var.image_id
        boot_volume_size_in_gbs = 50
      }
    }
  }
}
```

### Instance Pool

```hcl
resource "oci_core_instance_pool" "app_pool" {
  compartment_id            = var.compartment_id
  instance_configuration_id = oci_core_instance_configuration.app_config.id
  size                      = 2
  display_name              = "app-instance-pool"

  placement_configurations {
    availability_domain = data.oci_identity_availability_domains.ads.availability_domains[0].name
    primary_subnet_id   = oci_core_subnet.private_subnet.id
  }

  load_balancers {
    backend_set_name = oci_load_balancer_backend_set.app_backend.name
    load_balancer_id = oci_load_balancer.app_lb.id
    port             = 80
    vnic_selection   = "PrimaryVnic"
  }
}
```

### Autoscaling Configuration

```hcl
resource "oci_autoscaling_auto_scaling_configuration" "app_autoscaling" {
  compartment_id       = var.compartment_id
  cool_down_in_seconds = 300
  display_name         = "app-autoscaling"
  
  policies {
    display_name = "cpu-based-autoscaling"
    capacity {
      initial = 2
      max     = 10
      min     = 2
    }
    
    policy_type = "threshold"
    
    rules {
      action {
        type  = "CHANGE_COUNT_BY"
        value = 1
      }
      
      display_name = "scale-out-rule"
      
      metric {
        metric_type = "CPU_UTILIZATION"
        
        threshold {
          operator = "GT"
          value    = 75
        }
      }
    }
    
    rules {
      action {
        type  = "CHANGE_COUNT_BY"
        value = -1
      }
      
      display_name = "scale-in-rule"
      
      metric {
        metric_type = "CPU_UTILIZATION"
        
        threshold {
          operator = "LT"
          value    = 25
        }
      }
    }
  }
  
  auto_scaling_resources {
    id   = oci_core_instance_pool.app_pool.id
    type = "instancePool"
  }
}
```

## Boot Volume Management

### Boot Volume Backups

**Automatic Backups**:
```hcl
resource "oci_core_volume_backup_policy" "daily_backup" {
  compartment_id = var.compartment_id
  display_name   = "daily-boot-volume-backup"

  schedules {
    backup_type       = "INCREMENTAL"
    period            = "ONE_DAY"
    retention_seconds = 604800  # 7 days
    time_zone         = "UTC"
    hour_of_day       = 2
  }
}

resource "oci_core_volume_backup_policy_assignment" "boot_volume_backup" {
  asset_id  = oci_core_instance.web_server.boot_volume_id
  policy_id = oci_core_volume_backup_policy.daily_backup.id
}
```

### Custom Images

Create custom image from instance:

```bash
# Create custom image
oci compute image create \
  --compartment-id "ocid1.compartment..." \
  --instance-id "ocid1.instance..." \
  --display-name "custom-web-server-image"

# Launch instance from custom image
oci compute instance launch \
  --availability-domain "AD-1" \
  --compartment-id "ocid1.compartment..." \
  --shape "VM.Standard.E4.Flex" \
  --image-id "ocid1.image.custom..."
```

## Cloud-Init Configuration

### Basic Cloud-Init

```yaml
#cloud-config
package_update: true
package_upgrade: true

packages:
  - nginx
  - docker
  - git

runcmd:
  - systemctl enable nginx
  - systemctl start nginx
  - usermod -aG docker opc
  - echo "Hello from OCI instance" > /var/www/html/index.html

write_files:
  - path: /etc/nginx/conf.d/app.conf
    content: |
      server {
        listen 80;
        server_name _;
        location / {
          root /var/www/html;
          index index.html;
        }
      }
```

### Advanced Configuration

```yaml
#cloud-config
bootcmd:
  - echo "Boot command executed"

package_update: true
package_upgrade: true

packages:
  - docker
  - docker-compose

groups:
  - docker

users:
  - default
  - name: appuser
    groups: docker
    shell: /bin/bash
    sudo: ALL=(ALL) NOPASSWD:ALL

runcmd:
  - systemctl enable docker
  - systemctl start docker
  - docker pull nginx:latest
  - docker run -d -p 80:80 --name web nginx

final_message: "System setup completed in $UPTIME seconds"
```

## Instance Metadata Service

### Retrieve Instance Metadata

```bash
# Get instance ID
curl -H "Authorization: Bearer Oracle" -L http://169.254.169.254/opc/v2/instance/id

# Get availability domain
curl -H "Authorization: Bearer Oracle" -L http://169.254.169.254/opc/v2/instance/availabilityDomain

# Get region
curl -H "Authorization: Bearer Oracle" -L http://169.254.169.254/opc/v2/instance/region

# Get all metadata
curl -H "Authorization: Bearer Oracle" -L http://169.254.169.254/opc/v2/instance/
```

## Performance Optimization

### Compute Performance Best Practices

1. **Use Latest Shapes**: E4/E5 shapes offer better price-performance
2. **Enable Burstable Instances**: For variable workloads
3. **Optimize OCPU/Memory Ratio**: Match workload requirements
4. **Use Ultra High Performance Block Volumes**: For I/O intensive applications
5. **Enable TRIM on Boot Volumes**: Improve SSD performance over time

### Monitoring Key Metrics

- **CPU Utilization**: Target < 80% for sustained workloads
- **Memory Usage**: Monitor for memory pressure
- **Network Throughput**: Ensure within shape limits
- **Disk IOPS**: Monitor for storage bottlenecks
- **Instance Health**: Check for any instance alerts
