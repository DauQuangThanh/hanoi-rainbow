# Common Commands


### Kubernetes
```bash
# Get resources
kubectl get pods -n namespace
kubectl get deployments -n namespace
kubectl get services -n namespace

# Describe resources
kubectl describe pod pod-name -n namespace
kubectl describe deployment deployment-name -n namespace

# Logs
kubectl logs pod-name -n namespace
kubectl logs -f pod-name -n namespace --tail=100
kubectl logs pod-name -c container-name -n namespace

# Execute commands in pod
kubectl exec -it pod-name -n namespace -- /bin/sh
kubectl exec pod-name -n namespace -- command

# Port forwarding
kubectl port-forward pod-name 8080:8080 -n namespace
kubectl port-forward service/service-name 8080:80 -n namespace

# Apply/Delete resources
kubectl apply -f manifest.yaml
kubectl delete -f manifest.yaml

# Scaling
kubectl scale deployment deployment-name --replicas=5 -n namespace

# Rollout management
kubectl rollout status deployment/deployment-name -n namespace
kubectl rollout history deployment/deployment-name -n namespace
kubectl rollout undo deployment/deployment-name -n namespace
```

### Docker
```bash
# Build
docker build -t image-name:tag .
docker build -t image-name:tag -f Dockerfile.prod .

# Run
docker run -d -p 8080:8080 --name container-name image-name:tag
docker run -it --rm image-name:tag /bin/sh

# Manage containers
docker ps
docker ps -a
docker stop container-name
docker start container-name
docker restart container-name
docker rm container-name

# Manage images
docker images
docker rmi image-name:tag
docker pull image-name:tag
docker push image-name:tag

# Logs and debugging
docker logs container-name
docker logs -f container-name --tail=100
docker exec -it container-name /bin/sh
docker inspect container-name
```

### Terraform
```bash
# Initialize
terraform init
terraform init -upgrade

# Plan and apply
terraform plan
terraform plan -out=plan.tfplan
terraform apply
terraform apply plan.tfplan
terraform apply -auto-approve

# Destroy
terraform destroy
terraform destroy -auto-approve

# State management
terraform state list
terraform state show resource-name
terraform state rm resource-name

# Workspace management
terraform workspace list
terraform workspace new environment
terraform workspace select environment

# Import existing resources
terraform import resource-type.name resource-id
```

---
