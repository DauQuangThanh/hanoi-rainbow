# Azure Monitoring and Diagnostics

## Azure Monitor Overview

Azure Monitor collects, analyzes, and acts on telemetry data from Azure and on-premises environments.

**Key Components:**
- **Metrics**: Numerical time-series data
- **Logs**: Event and diagnostic data
- **Application Insights**: Application performance monitoring
- **Log Analytics**: Query and analyze log data
- **Alerts**: Proactive notifications
- **Dashboards**: Visualize monitoring data

## Log Analytics Workspace

### Creating Workspace

```bash
# Create Log Analytics workspace
az monitor log-analytics workspace create \
  --resource-group myResourceGroup \
  --workspace-name myWorkspace \
  --location eastus \
  --retention-time 90 \
  --sku PerGB2018

# Get workspace ID and key
WORKSPACE_ID=$(az monitor log-analytics workspace show \
  --resource-group myResourceGroup \
  --workspace-name myWorkspace \
  --query customerId -o tsv)

WORKSPACE_KEY=$(az monitor log-analytics workspace get-shared-keys \
  --resource-group myResourceGroup \
  --workspace-name myWorkspace \
  --query primarySharedKey -o tsv)
```

### KQL (Kusto Query Language) Queries

```kusto
// Query all logs from last 24 hours
AzureActivity
| where TimeGenerated > ago(24h)
| project TimeGenerated, OperationName, ResourceProvider, ResourceId
| limit 100

// Count operations by resource provider
AzureActivity
| where TimeGenerated > ago(7d)
| summarize Count = count() by ResourceProvider
| order by Count desc

// Failed requests in Application Insights
requests
| where success == false
| where timestamp > ago(1h)
| project timestamp, name, url, resultCode, duration
| order by timestamp desc

// Performance counters
Perf
| where TimeGenerated > ago(1h)
| where ObjectName == "Processor" and CounterName == "% Processor Time"
| summarize avg(CounterValue) by Computer, bin(TimeGenerated, 5m)
| render timechart

// Container CPU usage
Perf
| where ObjectName == "K8SContainer" and CounterName == "cpuUsageNanoCores"
| where TimeGenerated > ago(1h)
| extend CPU = CounterValue / 1000000000
| summarize avg(CPU) by bin(TimeGenerated, 5m), InstanceName
| render timechart

// Security events
SecurityEvent
| where TimeGenerated > ago(24h)
| where EventID == 4625  // Failed logon
| summarize Count = count() by Account, IpAddress
| order by Count desc
| take 20

// Storage account operations
StorageBlobLogs
| where TimeGenerated > ago(1h)
| where StatusCode != 200
| project TimeGenerated, OperationName, StatusCode, Uri, CallerIpAddress
| order by TimeGenerated desc

// AKS logs
ContainerLog
| where TimeGenerated > ago(1h)
| where Namespace == "production"
| project TimeGenerated, Computer, ContainerName, LogEntry
| order by TimeGenerated desc
```

### Running Queries

```bash
# Run query from CLI
az monitor log-analytics query \
  --workspace $WORKSPACE_ID \
  --analytics-query "AzureActivity | where TimeGenerated > ago(1h) | limit 10" \
  --output table

# Save query for reuse
az monitor log-analytics workspace saved-search create \
  --resource-group myResourceGroup \
  --workspace-name myWorkspace \
  --name "Failed Requests" \
  --category "Application" \
  --query "requests | where success == false | where timestamp > ago(1h)"
```

## Application Insights

### Creating Application Insights

```bash
# Create Application Insights
az monitor app-insights component create \
  --app myAppInsights \
  --location eastus \
  --resource-group myResourceGroup \
  --application-type web \
  --kind web \
  --workspace $WORKSPACE_ID

# Get instrumentation key
INSTRUMENTATION_KEY=$(az monitor app-insights component show \
  --app myAppInsights \
  --resource-group myResourceGroup \
  --query instrumentationKey -o tsv)

# Get connection string
CONNECTION_STRING=$(az monitor app-insights component show \
  --app myAppInsights \
  --resource-group myResourceGroup \
  --query connectionString -o tsv)
```

### Instrumenting Applications

**Node.js:**
```javascript
// Import Application Insights
const appInsights = require('applicationinsights');

// Initialize with connection string
appInsights.setup(process.env.APPLICATIONINSIGHTS_CONNECTION_STRING)
  .setAutoDependencyCorrelation(true)
  .setAutoCollectRequests(true)
  .setAutoCollectPerformance(true, true)
  .setAutoCollectExceptions(true)
  .setAutoCollectDependencies(true)
  .setAutoCollectConsole(true)
  .setUseDiskRetryCaching(true)
  .setSendLiveMetrics(true)
  .start();

// Get telemetry client
const client = appInsights.defaultClient;

// Track custom events
client.trackEvent({ name: "UserLogin", properties: { userId: "123" } });

// Track custom metrics
client.trackMetric({ name: "OrderValue", value: 299.99 });

// Track dependencies
client.trackDependency({
  target: "https://api.example.com",
  name: "GET /users",
  data: "GET /users",
  duration: 150,
  resultCode: 200,
  success: true,
  dependencyTypeName: "HTTP"
});
```

**Python:**
```python
from applicationinsights import TelemetryClient
from applicationinsights.flask.ext import AppInsights

# Flask application
app = Flask(__name__)
app.config['APPINSIGHTS_INSTRUMENTATIONKEY'] = os.getenv('APPINSIGHTS_INSTRUMENTATIONKEY')
appinsights = AppInsights(app)

# Standalone client
tc = TelemetryClient(instrumentation_key)

# Track events
tc.track_event('UserLogin', {'userId': '123'})

# Track metrics
tc.track_metric('OrderValue', 299.99)

# Track exceptions
try:
    result = 1 / 0
except Exception as e:
    tc.track_exception()

tc.flush()
```

**.NET:**
```csharp
using Microsoft.ApplicationInsights;
using Microsoft.ApplicationInsights.Extensibility;

// Create telemetry client
var config = TelemetryConfiguration.CreateDefault();
config.ConnectionString = Environment.GetEnvironmentVariable("APPLICATIONINSIGHTS_CONNECTION_STRING");
var client = new TelemetryClient(config);

// Track events
client.TrackEvent("UserLogin", new Dictionary<string, string> { { "userId", "123" } });

// Track metrics
client.TrackMetric("OrderValue", 299.99);

// Track dependencies
client.TrackDependency("SQL", "GetUser", "SELECT * FROM Users", DateTimeOffset.Now, TimeSpan.FromMilliseconds(150), true);
```

### Availability Tests

```bash
# Create availability test
az monitor app-insights web-test create \
  --resource-group myResourceGroup \
  --name myAvailabilityTest \
  --location eastus \
  --web-test-kind ping \
  --frequency 300 \
  --timeout 30 \
  --enabled true \
  --locations "us-va-ash-azr" "us-ca-sjc-azr" \
  --geo-locations "East US" "West US" \
  --retry-enabled true \
  --test-url "https://myapp.example.com/health" \
  --defined-web-test-name myAppInsights
```

## Diagnostic Settings

### Enabling Diagnostics

```bash
# Enable diagnostics for storage account
az monitor diagnostic-settings create \
  --name storage-diagnostics \
  --resource /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Storage/storageAccounts/mystorageaccount \
  --workspace $WORKSPACE_ID \
  --logs '[
    {
      "category": "StorageRead",
      "enabled": true,
      "retentionPolicy": { "enabled": true, "days": 30 }
    },
    {
      "category": "StorageWrite",
      "enabled": true,
      "retentionPolicy": { "enabled": true, "days": 30 }
    }
  ]' \
  --metrics '[
    {
      "category": "Transaction",
      "enabled": true,
      "retentionPolicy": { "enabled": true, "days": 30 }
    }
  ]'

# Enable diagnostics for AKS
az aks enable-addons \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --addons monitoring \
  --workspace-resource-id $WORKSPACE_ID

# Enable diagnostics for Key Vault
az monitor diagnostic-settings create \
  --name keyvault-diagnostics \
  --resource /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.KeyVault/vaults/myKeyVault \
  --workspace $WORKSPACE_ID \
  --logs '[
    {
      "category": "AuditEvent",
      "enabled": true,
      "retentionPolicy": { "enabled": true, "days": 90 }
    }
  ]' \
  --metrics '[
    {
      "category": "AllMetrics",
      "enabled": true,
      "retentionPolicy": { "enabled": true, "days": 30 }
    }
  ]'
```

## Metrics and Alerts

### Viewing Metrics

```bash
# Get VM CPU metrics
az monitor metrics list \
  --resource /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Compute/virtualMachines/myVM \
  --metric "Percentage CPU" \
  --start-time 2024-01-01T00:00:00Z \
  --end-time 2024-01-01T23:59:59Z \
  --interval PT1H \
  --output table

# Get storage account metrics
az monitor metrics list \
  --resource /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Storage/storageAccounts/mystorageaccount \
  --metric "UsedCapacity" \
  --output table
```

### Creating Alerts

```bash
# Create action group for notifications
az monitor action-group create \
  --resource-group myResourceGroup \
  --name devops-team \
  --short-name devops \
  --email-receiver name=admin email=admin@example.com \
  --sms-receiver name=oncall countrycode=1 phonenumber=5551234567 \
  --webhook-receiver name=slack service-uri=https://hooks.slack.com/services/xxx/yyy/zzz

# Create metric alert for high CPU
az monitor metrics alert create \
  --name high-cpu-alert \
  --resource-group myResourceGroup \
  --scopes /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Compute/virtualMachines/myVM \
  --condition "avg Percentage CPU > 80" \
  --window-size 5m \
  --evaluation-frequency 1m \
  --action devops-team \
  --description "Alert when CPU usage exceeds 80%"

# Create log alert
az monitor scheduled-query create \
  --name failed-requests-alert \
  --resource-group myResourceGroup \
  --scopes $WORKSPACE_ID \
  --condition "count 'requests | where success == false' > 10" \
  --window-size 5m \
  --evaluation-frequency 5m \
  --action devops-team \
  --description "Alert when failed requests exceed 10 in 5 minutes"

# Create activity log alert
az monitor activity-log alert create \
  --resource-group myResourceGroup \
  --name vm-stopped-alert \
  --description "Alert when VM is stopped" \
  --scopes /subscriptions/{sub-id}/resourceGroups/myResourceGroup \
  --condition category=Administrative and operationName=Microsoft.Compute/virtualMachines/deallocate/action \
  --action-groups devops-team
```

## Dashboards

### Creating Dashboard

```json
// dashboard.json
{
  "properties": {
    "lenses": [
      {
        "order": 0,
        "parts": [
          {
            "position": {
              "x": 0,
              "y": 0,
              "rowSpan": 4,
              "colSpan": 6
            },
            "metadata": {
              "type": "Extension/Microsoft_Azure_Monitoring/PartType/MetricsChartPart",
              "settings": {
                "content": {
                  "options": {
                    "chart": {
                      "metrics": [
                        {
                          "resourceMetadata": {
                            "id": "/subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Compute/virtualMachines/myVM"
                          },
                          "name": "Percentage CPU",
                          "aggregationType": 4,
                          "namespace": "Microsoft.Compute/virtualMachines"
                        }
                      ],
                      "title": "VM CPU Usage",
                      "titleKind": 1,
                      "visualization": {
                        "chartType": 2
                      }
                    }
                  }
                }
              }
            }
          }
        ]
      }
    ],
    "metadata": {
      "model": {
        "timeRange": {
          "type": "MsPortalFx.Composition.Configuration.ValueTypes.TimeRange",
          "value": {
            "relative": {
              "duration": 24,
              "timeUnit": 1
            }
          }
        }
      }
    }
  },
  "name": "Production Dashboard",
  "type": "Microsoft.Portal/dashboards",
  "location": "eastus",
  "tags": {
    "hidden-title": "Production Dashboard"
  }
}
```

```bash
# Create dashboard
az portal dashboard create \
  --resource-group myResourceGroup \
  --name production-dashboard \
  --location eastus \
  --input-path dashboard.json
```

## Workbooks

### Creating Workbook

```bash
# Create workbook for application monitoring
az monitor app-insights workbook create \
  --resource-group myResourceGroup \
  --name app-monitoring \
  --location eastus \
  --display-name "Application Monitoring" \
  --category workbook \
  --serialized-data @workbook.json
```

## Container Insights

### Monitoring AKS

```bash
# Enable Container Insights
az aks enable-addons \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --addons monitoring \
  --workspace-resource-id $WORKSPACE_ID

# Query container logs
az monitor log-analytics query \
  --workspace $WORKSPACE_ID \
  --analytics-query "
    ContainerLog
    | where TimeGenerated > ago(1h)
    | where Name contains 'myapp'
    | project TimeGenerated, Computer, Name, LogEntry
  " \
  --output table

# Query container performance
az monitor log-analytics query \
  --workspace $WORKSPACE_ID \
  --analytics-query "
    Perf
    | where TimeGenerated > ago(1h)
    | where ObjectName == 'K8SContainer'
    | where CounterName == 'cpuUsageNanoCores'
    | summarize avg(CounterValue) by InstanceName, bin(TimeGenerated, 5m)
  " \
  --output table
```

## Network Monitoring

### Network Watcher

```bash
# Enable Network Watcher
az network watcher configure \
  --resource-group NetworkWatcherRG \
  --locations eastus \
  --enabled true

# Test IP flow
az network watcher test-ip-flow \
  --resource-group myResourceGroup \
  --vm myVM \
  --direction Outbound \
  --protocol TCP \
  --local 10.0.0.4:80 \
  --remote 20.30.40.50:443

# Next hop analysis
az network watcher show-next-hop \
  --resource-group myResourceGroup \
  --vm myVM \
  --source-ip 10.0.0.4 \
  --dest-ip 20.30.40.50

# Packet capture
az network watcher packet-capture create \
  --resource-group myResourceGroup \
  --vm myVM \
  --name myCapture \
  --storage-account mystorageaccount \
  --filters "[{\"protocol\":\"TCP\",\"localPort\":\"80\"}]" \
  --time-limit 60
```

### Connection Monitor

```bash
# Create connection monitor
az network watcher connection-monitor create \
  --resource-group myResourceGroup \
  --name myConnectionMonitor \
  --location eastus \
  --endpoint-source-resource-id /subscriptions/{sub-id}/resourceGroups/myResourceGroup/providers/Microsoft.Compute/virtualMachines/myVM \
  --endpoint-dest-address www.example.com \
  --endpoint-dest-port 443 \
  --test-config-protocol Tcp \
  --test-config-test-frequency-sec 60
```

### Flow Logs

```bash
# Enable NSG flow logs
az network watcher flow-log create \
  --resource-group myResourceGroup \
  --name myFlowLog \
  --nsg myNSG \
  --storage-account mystorageaccount \
  --enabled true \
  --retention 90 \
  --traffic-analytics true \
  --workspace $WORKSPACE_ID
```

## Cost Monitoring

```bash
# Get cost and usage data
az consumption usage list \
  --start-date 2024-01-01 \
  --end-date 2024-01-31 \
  --output table

# Create budget
az consumption budget create \
  --resource-group myResourceGroup \
  --budget-name monthly-budget \
  --amount 1000 \
  --category Cost \
  --time-grain Monthly \
  --start-date 2024-01-01T00:00:00Z \
  --end-date 2024-12-31T23:59:59Z \
  --notifications '[
    {
      "enabled": true,
      "operator": "GreaterThan",
      "threshold": 80,
      "contactEmails": ["admin@example.com"],
      "contactRoles": ["Owner"]
    }
  ]'
```

## Best Practices

1. **Enable diagnostics on all resources** - Capture logs and metrics
2. **Use Log Analytics workspace** - Centralize log data
3. **Create meaningful alerts** - Actionable, not noisy
4. **Implement availability tests** - Monitor application uptime
5. **Use Application Insights** - Track application performance
6. **Configure action groups** - Multiple notification channels
7. **Create dashboards** - Visualize key metrics
8. **Set up cost alerts** - Monitor spending
9. **Enable Network Watcher** - Troubleshoot network issues
10. **Regular review of metrics** - Optimize performance and costs
