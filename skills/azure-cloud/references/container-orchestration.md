# Azure Container Orchestration (AKS)

## AKS Cluster Architecture

### Cluster Components

**Control Plane (Microsoft-managed)**
- API Server
- etcd (cluster state storage)
- Scheduler
- Controller Manager

**Node Pools (Customer-managed)**
- System node pool (required, runs system pods)
- User node pools (optional, runs application workloads)

### Node Pool Types

**System Node Pool**
- Required for cluster operation
- Runs CoreDNS, metrics-server, tunnelfront
- Minimum 2 nodes recommended
- Use taints to prevent user workloads

**User Node Pools**
- Run application workloads
- Can be scaled to zero
- Support Windows and Linux nodes
- Multiple pools for different workload types

## Creating AKS Cluster

### Basic Cluster Creation

```bash
# Create AKS cluster with managed identity
az aks create \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --node-count 3 \
  --node-vm-size Standard_D4s_v5 \
  --network-plugin azure \
  --network-policy azure \
  --enable-managed-identity \
  --enable-addons monitoring \
  --generate-ssh-keys \
  --kubernetes-version 1.28.5 \
  --zones 1 2 3

# Get credentials
az aks get-credentials \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --overwrite-existing
```

### Production-Ready Cluster

```bash
# Create resource group
az group create --name aks-production-rg --location eastus

# Create Log Analytics workspace for monitoring
az monitor log-analytics workspace create \
  --resource-group aks-production-rg \
  --workspace-name aks-logs

WORKSPACE_ID=$(az monitor log-analytics workspace show \
  --resource-group aks-production-rg \
  --workspace-name aks-logs \
  --query id -o tsv)

# Create AKS cluster with production settings
az aks create \
  --resource-group aks-production-rg \
  --name aks-production \
  --location eastus \
  --kubernetes-version 1.28.5 \
  --node-count 3 \
  --min-count 2 \
  --max-count 10 \
  --enable-cluster-autoscaler \
  --node-vm-size Standard_D8s_v5 \
  --node-osdisk-size 128 \
  --network-plugin azure \
  --network-policy azure \
  --load-balancer-sku standard \
  --outbound-type loadBalancer \
  --vnet-subnet-id /subscriptions/{sub-id}/resourceGroups/{rg}/providers/Microsoft.Network/virtualNetworks/{vnet}/subnets/{subnet} \
  --enable-managed-identity \
  --enable-aad \
  --aad-admin-group-object-ids {AAD-GROUP-ID} \
  --enable-azure-rbac \
  --enable-addons monitoring,azure-keyvault-secrets-provider \
  --workspace-resource-id $WORKSPACE_ID \
  --enable-secret-rotation \
  --rotation-poll-interval 2m \
  --enable-pod-security-policy \
  --zones 1 2 3 \
  --node-osdisk-type Ephemeral \
  --tags Environment=production ManagedBy=AzureCLI
```

## Node Pools

### Adding Node Pools

```bash
# Add user node pool
az aks nodepool add \
  --resource-group myResourceGroup \
  --cluster-name myAKSCluster \
  --name userpool \
  --node-count 3 \
  --node-vm-size Standard_D8s_v5 \
  --enable-cluster-autoscaler \
  --min-count 2 \
  --max-count 10 \
  --zones 1 2 3 \
  --labels workload=application tier=web \
  --tags team=backend

# Add GPU node pool
az aks nodepool add \
  --resource-group myResourceGroup \
  --cluster-name myAKSCluster \
  --name gpupool \
  --node-count 1 \
  --node-vm-size Standard_NC6s_v3 \
  --enable-cluster-autoscaler \
  --min-count 0 \
  --max-count 3 \
  --labels workload=ml gpu=nvidia \
  --node-taints sku=gpu:NoSchedule

# Add Windows node pool
az aks nodepool add \
  --resource-group myResourceGroup \
  --cluster-name myAKSCluster \
  --name winpool \
  --node-count 2 \
  --os-type Windows \
  --node-vm-size Standard_D4s_v5

# List node pools
az aks nodepool list \
  --resource-group myResourceGroup \
  --cluster-name myAKSCluster -o table

# Scale node pool
az aks nodepool scale \
  --resource-group myResourceGroup \
  --cluster-name myAKSCluster \
  --name userpool \
  --node-count 5
```

## Azure Container Registry (ACR) Integration

### Creating ACR

```bash
# Create ACR
az acr create \
  --resource-group myResourceGroup \
  --name myContainerRegistry \
  --sku Premium \
  --location eastus \
  --admin-enabled false

# Enable geo-replication
az acr replication create \
  --registry myContainerRegistry \
  --location westus

# Attach ACR to AKS
az aks update \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --attach-acr myContainerRegistry
```

### Building and Pushing Images

```bash
# Build image in ACR
az acr build \
  --registry myContainerRegistry \
  --image myapp:v1 \
  --file Dockerfile \
  .

# Import image from Docker Hub
az acr import \
  --name myContainerRegistry \
  --source docker.io/library/nginx:latest \
  --image nginx:latest

# Enable vulnerability scanning
az acr task run \
  --registry myContainerRegistry \
  --name quickscan \
  --image nginx:latest
```

## Kubernetes Deployments on AKS

### Deployment Manifest

```yaml
# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  namespace: production
  labels:
    app: myapp
    version: v1
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
        version: v1
    spec:
      nodeSelector:
        workload: application
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchExpressions:
                - key: app
                  operator: In
                  values:
                  - myapp
              topologyKey: kubernetes.io/hostname
      containers:
      - name: myapp
        image: myContainerRegistry.azurecr.io/myapp:v1
        ports:
        - containerPort: 8080
          name: http
        resources:
          requests:
            cpu: 250m
            memory: 512Mi
          limits:
            cpu: 1000m
            memory: 1Gi
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: app-secrets
              key: database-url
        volumeMounts:
        - name: secrets-store
          mountPath: "/mnt/secrets-store"
          readOnly: true
      volumes:
      - name: secrets-store
        csi:
          driver: secrets-store.csi.k8s.io
          readOnly: true
          volumeAttributes:
            secretProviderClass: "azure-keyvault"
---
apiVersion: v1
kind: Service
metadata:
  name: myapp
  namespace: production
spec:
  type: ClusterIP
  selector:
    app: myapp
  ports:
  - name: http
    port: 80
    targetPort: 8080
---
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: myapp
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

### Ingress with Application Gateway

```yaml
# ingress.yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: myapp-ingress
  namespace: production
  annotations:
    kubernetes.io/ingress.class: azure/application-gateway
    appgw.ingress.kubernetes.io/ssl-redirect: "true"
    appgw.ingress.kubernetes.io/health-probe-path: "/health"
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
spec:
  tls:
  - hosts:
    - myapp.example.com
    secretName: myapp-tls
  rules:
  - host: myapp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: myapp
            port:
              number: 80
```

## Azure Key Vault Integration

### SecretProviderClass

```yaml
# secretproviderclass.yaml
apiVersion: secrets-store.csi.x-k8s.io/v1
kind: SecretProviderClass
metadata:
  name: azure-keyvault
  namespace: production
spec:
  provider: azure
  parameters:
    usePodIdentity: "false"
    useVMManagedIdentity: "true"
    userAssignedIdentityID: "{IDENTITY_CLIENT_ID}"
    keyvaultName: "mykeyvault"
    cloudName: ""
    objects: |
      array:
        - |
          objectName: database-connection-string
          objectType: secret
          objectVersion: ""
        - |
          objectName: api-key
          objectType: secret
          objectVersion: ""
    tenantId: "{TENANT_ID}"
  secretObjects:
  - secretName: app-secrets
    type: Opaque
    data:
    - objectName: database-connection-string
      key: database-url
    - objectName: api-key
      key: api-key
```

## Monitoring and Logging

### Container Insights

```bash
# Enable Container Insights
az aks enable-addons \
  --resource-group myResourceGroup \
  --name myAKSCluster \
  --addons monitoring \
  --workspace-resource-id $WORKSPACE_ID

# Query logs with KQL
az monitor log-analytics query \
  --workspace $WORKSPACE_ID \
  --analytics-query "ContainerLog | where TimeGenerated > ago(1h) | limit 100" \
  --output table
```

### Prometheus and Grafana

```bash
# Add Prometheus Helm repo
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo update

# Install kube-prometheus-stack
helm install prometheus prometheus-community/kube-prometheus-stack \
  --namespace monitoring \
  --create-namespace \
  --set prometheus.prometheusSpec.serviceMonitorSelectorNilUsesHelmValues=false \
  --set prometheus.prometheusSpec.podMonitorSelectorNilUsesHelmValues=false

# Access Grafana
kubectl port-forward -n monitoring svc/prometheus-grafana 3000:80
```

## Network Policies

```yaml
# network-policy.yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-app-traffic
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: myapp
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: ingress-nginx
    ports:
    - protocol: TCP
      port: 8080
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: database
    ports:
    - protocol: TCP
      port: 5432
  - to:
    - podSelector: {}
    ports:
    - protocol: TCP
      port: 53
    - protocol: UDP
      port: 53
```

## Backup and Disaster Recovery

### Velero for AKS

```bash
# Install Velero
AZURE_BACKUP_RESOURCE_GROUP=aks-backup-rg
AZURE_STORAGE_ACCOUNT_ID=aksbackupstorage

# Create storage account for backups
az storage account create \
  --name $AZURE_STORAGE_ACCOUNT_ID \
  --resource-group $AZURE_BACKUP_RESOURCE_GROUP \
  --sku Standard_GRS \
  --location eastus

# Create blob container
az storage container create \
  --name velero \
  --public-access off \
  --account-name $AZURE_STORAGE_ACCOUNT_ID

# Install Velero CLI
wget https://github.com/vmware-tanzu/velero/releases/download/v1.12.0/velero-v1.12.0-linux-amd64.tar.gz
tar -xvf velero-v1.12.0-linux-amd64.tar.gz
sudo mv velero-v1.12.0-linux-amd64/velero /usr/local/bin/

# Install Velero in cluster
velero install \
  --provider azure \
  --plugins velero/velero-plugin-for-microsoft-azure:v1.8.0 \
  --bucket velero \
  --secret-file ./credentials-velero \
  --backup-location-config resourceGroup=$AZURE_BACKUP_RESOURCE_GROUP,storageAccount=$AZURE_STORAGE_ACCOUNT_ID \
  --snapshot-location-config apiTimeout=5m,resourceGroup=$AZURE_BACKUP_RESOURCE_GROUP

# Create backup
velero backup create mybackup --include-namespaces production

# Schedule daily backups
velero schedule create daily-backup --schedule="@daily" --include-namespaces production

# Restore from backup
velero restore create --from-backup mybackup
```

## Best Practices Summary

1. **Use managed identity** - Avoid service principals
2. **Enable Azure RBAC** - Integrate with Azure AD
3. **Multiple node pools** - Separate system and user workloads
4. **Auto-scaling** - Cluster autoscaler and HPA
5. **Network policies** - Restrict pod-to-pod communication
6. **Private cluster** - API server not publicly accessible
7. **Pod security** - Use Pod Security Standards
8. **Resource limits** - Set requests and limits on all containers
9. **Health probes** - Configure liveness and readiness probes
10. **Monitoring** - Enable Container Insights and Prometheus
11. **GitOps** - Use Flux or ArgoCD for deployments
12. **Backup** - Regular cluster and data backups
