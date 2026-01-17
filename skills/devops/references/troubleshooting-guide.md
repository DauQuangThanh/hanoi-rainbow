# Troubleshooting Guide


### Pod Issues

**Pod not starting:**
```bash
# Check pod status and events
kubectl describe pod pod-name -n namespace

# Common causes:
# - ImagePullBackOff: Check image name/tag, registry credentials
# - CrashLoopBackOff: Check logs for application errors
# - Pending: Check resource availability, node selectors
```

**Application errors:**
```bash
# Check application logs
kubectl logs pod-name -n namespace --tail=100

# Check previous container logs (if restarted)
kubectl logs pod-name -n namespace --previous

# Check events
kubectl get events -n namespace --sort-by='.lastTimestamp'
```

### Deployment Issues

**Rollout stuck:**
```bash
# Check rollout status
kubectl rollout status deployment/deployment-name -n namespace

# Check replica set status
kubectl get rs -n namespace

# Rollback if needed
kubectl rollout undo deployment/deployment-name -n namespace
```

### Network Issues

**Service not reachable:**
```bash
# Check service endpoints
kubectl get endpoints service-name -n namespace

# Test from within cluster
kubectl run test-pod --rm -it --image=alpine -- /bin/sh
# Inside pod: wget -O- http://service-name.namespace.svc.cluster.local

# Check network policies
kubectl get networkpolicies -n namespace
```

---
