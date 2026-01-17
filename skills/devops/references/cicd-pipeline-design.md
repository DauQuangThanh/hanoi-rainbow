# CI/CD Pipeline Design


### Pipeline Structure

Design pipelines with these stages:

**1. Source Stage**
- Trigger on code commit/merge
- Checkout source code
- Validate branch policies

**2. Build Stage**
- Compile code
- Run unit tests
- Generate artifacts
- Create container images
- Version artifacts (semantic versioning)

**3. Test Stage**
- Integration tests
- Security scanning (SAST, dependency scanning)
- Code quality analysis (SonarQube, linting)
- Performance tests

**4. Deploy to Staging**
- Deploy to staging environment
- Run smoke tests
- Run E2E tests
- Load testing (if applicable)

**5. Approval Gate**
- Manual approval for production
- Automated approval criteria
- Change management integration

**6. Deploy to Production**
- Deployment strategy execution
- Health checks
- Rollback capability
- Post-deployment validation

### Pipeline Configuration Examples

**GitHub Actions:**
```yaml
name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Set up environment
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'
      
      - name: Install dependencies
        run: npm ci
      
      - name: Run tests
        run: npm test
      
      - name: Build
        run: npm run build
      
      - name: Build Docker image
        run: |
          docker build -t ${{ secrets.REGISTRY }}/app:${{ github.sha }} .
          docker tag ${{ secrets.REGISTRY }}/app:${{ github.sha }} \
                     ${{ secrets.REGISTRY }}/app:latest
      
      - name: Push to registry
        run: |
          echo ${{ secrets.REGISTRY_TOKEN }} | docker login -u ${{ secrets.REGISTRY_USER }} --password-stdin
          docker push ${{ secrets.REGISTRY }}/app:${{ github.sha }}
          docker push ${{ secrets.REGISTRY }}/app:latest

  deploy-staging:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/develop'
    steps:
      - name: Deploy to staging
        run: |
          kubectl set image deployment/app \
            app=${{ secrets.REGISTRY }}/app:${{ github.sha }} \
            -n staging
      
      - name: Wait for rollout
        run: kubectl rollout status deployment/app -n staging

  deploy-production:
    needs: build
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    environment:
      name: production
      url: https://app.example.com
    steps:
      - name: Deploy to production
        run: |
          kubectl set image deployment/app \
            app=${{ secrets.REGISTRY }}/app:${{ github.sha }} \
            -n production
      
      - name: Wait for rollout
        run: kubectl rollout status deployment/app -n production
```

**GitLab CI:**
```yaml
stages:
  - build
  - test
  - deploy-staging
  - deploy-production

variables:
  DOCKER_DRIVER: overlay2
  IMAGE_TAG: $CI_REGISTRY_IMAGE:$CI_COMMIT_SHORT_SHA

build:
  stage: build
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
    - docker build -t $IMAGE_TAG .
    - docker push $IMAGE_TAG
  only:
    - main
    - develop

test:
  stage: test
  image: node:18
  script:
    - npm ci
    - npm test
    - npm run lint
  coverage: '/Lines\s*:\s*(\d+\.\d+)%/'

security-scan:
  stage: test
  image: aquasec/trivy:latest
  script:
    - trivy image --severity HIGH,CRITICAL $IMAGE_TAG

deploy-staging:
  stage: deploy-staging
  image: bitnami/kubectl:latest
  script:
    - kubectl config use-context staging
    - kubectl set image deployment/app app=$IMAGE_TAG -n staging
    - kubectl rollout status deployment/app -n staging
  only:
    - develop
  environment:
    name: staging
    url: https://staging.example.com

deploy-production:
  stage: deploy-production
  image: bitnami/kubectl:latest
  script:
    - kubectl config use-context production
    - kubectl set image deployment/app app=$IMAGE_TAG -n production
    - kubectl rollout status deployment/app -n production
  only:
    - main
  when: manual
  environment:
    name: production
    url: https://app.example.com
```

### Best Practices

- **Fast feedback**: Keep pipeline execution under 10 minutes
- **Fail fast**: Run quick tests first, expensive tests later
- **Parallel execution**: Run independent jobs concurrently
- **Artifact caching**: Cache dependencies and build artifacts
- **Secrets management**: Use platform secret stores, never commit secrets
- **Pipeline as code**: Version control all pipeline definitions
- **Idempotent scripts**: Scripts should be safely re-runnable

---
