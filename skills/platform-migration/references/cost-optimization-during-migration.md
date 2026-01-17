# Cost Optimization During Migration


```markdown
Strategies:

1. Right-Sizing
   - Don't replicate exact instance types
   - Use target cloud's recommendations
   - Start smaller, scale up if needed

2. Reserved Instances / Savings Plans
   - Wait until migration complete
   - Analyze usage patterns first
   - Commit once stable

3. Spot/Preemptible Instances
   - Use for non-critical workloads
   - Test early in migration
   - Significant cost savings (60-90%)

4. Storage Optimization
   - Use appropriate storage tiers
   - Implement lifecycle policies
   - Compress data where possible
   - Delete unused snapshots

5. Data Transfer Costs
   - Minimize cross-region transfers
   - Use compression for large transfers
   - Plan network topology carefully
   - Consider AWS DataSync, GCP Transfer Service
```
