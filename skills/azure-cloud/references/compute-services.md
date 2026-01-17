# Azure Compute Services

## Azure Virtual Machines

### VM Size Selection

**General Purpose (B, D, DC, DS)**
- **B-series**: Burstable, cost-effective for low baseline CPU usage (dev/test, small web servers)
- **D-series**: Balanced CPU-to-memory ratio (application servers, databases)
- **DC-series**: Confidential computing with Intel SGX enclaves

**Compute Optimized (F, FS)**
- **F-series**: High CPU-to-memory ratio (batch processing, web servers, analytics)
- Best for CPU-intensive workloads with less memory requirements

**Memory Optimized (E, ES, M, MS)**
- **E-series**: High memory-to-CPU ratio (in-memory databases, large caches)
- **M-series**: Largest memory configurations (SAP HANA, massive databases)

**Storage Optimized (L, LS)**
- **L-series**: High disk throughput and IOPS (big data, NoSQL databases)

**GPU (N-series)**
- **NC-series**: NVIDIA Tesla for compute workloads (ML training, simulations)
- **NV-series**: NVIDIA Tesla for visualization (VDI, rendering)
- **ND-series**: Deep learning training

**High Performance Compute (H)**
- **H-series**: HPC workloads with InfiniBand networking (molecular dynamics, CFD)

### VM Best Practices

```bash
# Create VM with best practices
az vm create \
  --resource-group myResourceGroup \
  --name myVM \
  --image Ubuntu2204 \
  --size Standard_D4s_v5 \
  --admin-username azureuser \
  --generate-ssh-keys \
  --public-ip-sku Standard \
  --nsg-rule SSH \
  --enable-agent true \
  --enable-auto-update true \
  --patch-mode AutomaticByPlatform \
  --storage-sku Premium_LRS \
  --os-disk-encryption-set myEncryptionSet \
  --availability-zone 1
```

### Managed Disks

**Disk Types:**
- **Ultra Disk**: Sub-millisecond latency, up to 160,000 IOPS
- **Premium SSD v2**: Configurable performance, no disk size requirements
- **Premium SSD**: Consistent low latency, production workloads
- **Standard SSD**: Cost-effective for web servers, dev/test
- **Standard HDD**: Lowest cost, infrequent access

**Disk Performance Tiers:**
```bash
# Change disk performance tier without downtime
az disk update \
  --resource-group myResourceGroup \
  --name myDisk \
  --set tier=P40
```

### Availability Options

**Availability Zones**
- Physically separate zones within a region
- 99.99% SLA with zone-redundant deployment
- Protect from datacenter failures

```bash
# Create VM in availability zone
az vm create \
  --resource-group myResourceGroup \
  --name myVM \
  --image Ubuntu2204 \
  --zone 1
```

**Availability Sets**
- Logical grouping within a datacenter
- 99.95% SLA with 2+ VMs in availability set
- Protection from hardware failures

**Virtual Machine Scale Sets**
- Auto-scaling capabilities
- Load balancer integration
- Up to 1,000 VM instances

## Azure App Service

### Service Plans

**Pricing Tiers:**
- **Free/Shared**: Dev/test, shared infrastructure
- **Basic**: Dedicated compute, manual scaling
- **Standard**: Auto-scaling, staging slots, custom domains
- **Premium**: Enhanced performance, VNet integration, more slots
- **Isolated**: App Service Environment (ASE), network isolation

### Deployment Slots

```bash
# Create deployment slot
az webapp deployment slot create \
  --name myWebApp \
  --resource-group myResourceGroup \
  --slot staging

# Swap slots (blue-green deployment)
az webapp deployment slot swap \
  --name myWebApp \
  --resource-group myResourceGroup \
  --slot staging \
  --target-slot production
```

### App Service Configuration

```bash
# Configure app settings
az webapp config appsettings set \
  --name myWebApp \
  --resource-group myResourceGroup \
  --settings \
    DATABASE_URL="@Microsoft.KeyVault(SecretUri=https://myvault.vault.azure.net/secrets/db-connection)" \
    ENVIRONMENT="production"

# Enable application insights
az webapp config appsettings set \
  --name myWebApp \
  --resource-group myResourceGroup \
  --settings APPINSIGHTS_INSTRUMENTATIONKEY="your-key"

# Configure auto-scaling
az monitor autoscale create \
  --resource-group myResourceGroup \
  --resource myWebApp \
  --resource-type Microsoft.Web/serverFarms \
  --min-count 2 \
  --max-count 10 \
  --count 2
```

### VNet Integration

```bash
# Integrate App Service with VNet
az webapp vnet-integration add \
  --name myWebApp \
  --resource-group myResourceGroup \
  --vnet myVNet \
  --subnet appServiceSubnet
```

## Azure Functions

### Hosting Plans

**Consumption Plan**
- Pay-per-execution
- Automatic scaling
- 5-minute execution timeout (10 minutes configurable)
- Best for event-driven, sporadic workloads

**Premium Plan**
- No cold start with pre-warmed instances
- VNet connectivity
- Unlimited execution duration
- Best for long-running, VNet-connected functions

**Dedicated (App Service) Plan**
- Run on existing App Service plan
- Predictable billing
- Best for continuous workloads

### Function Triggers and Bindings

```javascript
// HTTP Trigger with Blob output binding
module.exports = async function (context, req) {
    context.log('Processing request');
    
    const name = req.query.name || req.body?.name;
    
    context.bindings.outputBlob = {
        name: name,
        timestamp: new Date()
    };
    
    return {
        status: 200,
        body: `Hello, ${name}`
    };
};
```

**Function.json:**
```json
{
  "bindings": [
    {
      "authLevel": "function",
      "type": "httpTrigger",
      "direction": "in",
      "name": "req",
      "methods": ["get", "post"]
    },
    {
      "type": "http",
      "direction": "out",
      "name": "res"
    },
    {
      "type": "blob",
      "direction": "out",
      "name": "outputBlob",
      "path": "output-container/{name}.json",
      "connection": "AzureWebJobsStorage"
    }
  ]
}
```

### Durable Functions

```javascript
// Orchestrator function for workflow
const df = require("durable-functions");

module.exports = df.orchestrator(function* (context) {
    const outputs = [];
    
    outputs.push(yield context.df.callActivity("ProcessOrder", { orderId: 123 }));
    outputs.push(yield context.df.callActivity("SendNotification", { email: "user@example.com" }));
    outputs.push(yield context.df.callActivity("UpdateInventory", { productId: 456 }));
    
    return outputs;
});
```

### Best Practices

1. **Keep functions small and focused** - Single responsibility
2. **Use async/await** - Non-blocking I/O operations
3. **Implement retry policies** - Handle transient failures
4. **Use managed identities** - Avoid storing credentials
5. **Monitor with Application Insights** - Track performance and errors
6. **Configure timeout appropriately** - Based on expected execution time
7. **Use output bindings** - Reduce code for Azure service integration

## Azure Container Instances

### Quick Container Deployment

```bash
# Deploy container with public IP
az container create \
  --resource-group myResourceGroup \
  --name mycontainer \
  --image nginx:latest \
  --cpu 1 \
  --memory 1 \
  --ports 80 \
  --dns-name-label myapp-unique-dns \
  --location eastus

# Deploy with environment variables and persistent storage
az container create \
  --resource-group myResourceGroup \
  --name myapp \
  --image myregistry.azurecr.io/myapp:v1 \
  --registry-login-server myregistry.azurecr.io \
  --registry-username myregistry \
  --registry-password myPassword \
  --environment-variables 'DATABASE_URL'='connection-string' \
  --azure-file-volume-account-name mystorageaccount \
  --azure-file-volume-account-key storagekey \
  --azure-file-volume-share-name myshare \
  --azure-file-volume-mount-path /data \
  --cpu 2 \
  --memory 4 \
  --ports 80 443
```

### Multi-Container Groups

```yaml
# container-group.yaml
apiVersion: 2019-12-01
location: eastus
name: mycontainergroup
properties:
  containers:
  - name: web
    properties:
      image: nginx:latest
      resources:
        requests:
          cpu: 1.0
          memoryInGb: 1.5
      ports:
      - port: 80
        protocol: TCP
  - name: sidecar
    properties:
      image: fluentd:latest
      resources:
        requests:
          cpu: 0.5
          memoryInGb: 0.5
  osType: Linux
  ipAddress:
    type: Public
    ports:
    - protocol: TCP
      port: 80
  restartPolicy: Always
tags: {}
type: Microsoft.ContainerInstance/containerGroups
```

```bash
# Deploy container group
az container create --resource-group myResourceGroup --file container-group.yaml
```

### Use Cases for ACI

1. **Burst scaling from AKS** - Virtual Kubelet integration
2. **CI/CD build agents** - Ephemeral build environments
3. **Batch processing** - Short-lived compute tasks
4. **Development/testing** - Quick container deployments
5. **Event-driven processing** - Azure Functions alternative for containers
