# Container Registry Migration


```bash
# Migrate from Docker Hub to AWS ECR

# 1. Authenticate to source
docker login

# 2. Authenticate to target
aws ecr get-login-password --region us-east-1 | \
  docker login --username AWS --password-stdin \
  123456789012.dkr.ecr.us-east-1.amazonaws.com

# 3. Pull, tag, and push
SOURCE_REPO="myorg/myapp"
TARGET_REPO="123456789012.dkr.ecr.us-east-1.amazonaws.com/myapp"

for tag in v1.0 v1.1 v1.2 latest; do
  docker pull $SOURCE_REPO:$tag
  docker tag $SOURCE_REPO:$tag $TARGET_REPO:$tag
  docker push $TARGET_REPO:$tag
done

# 4. Update Kubernetes deployments
kubectl set image deployment/myapp \
  myapp=$TARGET_REPO:v1.2

# 5. Verify
kubectl rollout status deployment/myapp
```
