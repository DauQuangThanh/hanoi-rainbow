# Rollback Procedures


```markdown
Rollback Triggers:
- Data corruption detected
- Performance degradation > 50%
- Application failures
- Data inconsistencies
- Unable to resolve issues within SLA

Rollback Steps:

1. Decision Point
   - Assess severity
   - Estimate fix time
   - Make rollback decision
   - Communicate to stakeholders

2. Stop New Database Operations
   - Stop application writes to new DB
   - Prevent further data divergence

3. Revert Application Connections
   - Update connection strings
   - Point to old database
   - Restart application if needed

4. Validate Old Database
   - Check data integrity
   - Verify service functionality
   - Monitor performance

5. Resume Operations
   - Announce rollback complete
   - Normal operations restored

6. Post-Rollback
   - Root cause analysis
   - Fix identified issues
   - Reschedule migration
```
