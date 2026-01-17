# Troubleshooting & Diagnostics

## Common Issues

### 1. Users Cannot Login

**Symptoms:** Login fails, error messages, redirects fail

**Diagnosis:**
- Check user status: Enabled, email verified
- Check required actions: Password expired, update profile
- Verify client redirect URIs: Must match exactly
- Check authentication flow: Ensure flow is correct
- Review login events: Events → Login Events

**Solutions:**
- Reset user password (temporary)
- Clear required actions
- Fix redirect URI configuration
- Adjust authentication flow requirements
- Check SSO session status

### 2. Token Validation Failures

**Symptoms:** Applications reject tokens, signature validation errors

**Diagnosis:**
- Verify token signature with realm public key
- Check token expiration time
- Validate issuer URL (must match KeyCloak URL)
- Verify audience claim matches client ID
- Ensure application uses correct realm endpoint

**Solutions:**
- Use correct realm public key for validation
- Increase token lifespan if too short
- Fix issuer URL in token validation
- Add correct audience to client configuration
- Update application realm endpoint URLs

### 3. LDAP Sync Issues

**Symptoms:** Users not syncing, authentication fails for LDAP users

**Diagnosis:**
- Test LDAP connection: User Federation → Test Connection
- Check bind credentials: Must have read access
- Verify LDAP user DN path
- Check LDAP mappers configuration
- Review KeyCloak server logs

**Solutions:**
- Fix LDAP connection settings
- Update bind DN credentials
- Correct user DN base path
- Adjust LDAP attribute mappers
- Run manual sync: User Federation → Sync Users

### 4. Session Expiration Issues

**Symptoms:** Users logged out unexpectedly, session timeouts

**Diagnosis:**
- Check SSO session settings: Idle and Max timeouts
- Verify client session settings
- Review remember me configuration
- Check token refresh behavior
- Review events for session termination

**Solutions:**
- Increase SSO session idle/max timeouts
- Enable remember me for longer sessions
- Implement token refresh in application
- Check for explicit logout calls
- Adjust client session overrides

### 5. Client Authentication Errors

**Symptoms:** "Invalid client" or "Unauthorized" errors

**Diagnosis:**
- Verify client ID and secret
- Check client authentication toggle (ON for confidential)
- Verify redirect URIs match exactly
- Check client enabled status
- Review client credentials in application

**Solutions:**
- Regenerate client secret if compromised
- Enable client authentication for confidential clients
- Fix redirect URI configuration (remove wildcards)
- Enable client in KeyCloak console
- Update application with correct credentials

## Logging & Debugging

### Enable Debug Logging

```bash
# Edit standalone.xml or standalone-ha.xml
<logger category="org.keycloak">
    <level name="DEBUG"/>
</logger>

# Or via CLI
bin/kcadm.sh config credentials --server http://localhost:8080 --realm master --user admin
bin/kcadm.sh update realms/master -s eventsEnabled=true -s adminEventsEnabled=true
```

### Log Locations

- Standalone: `standalone/log/server.log`
- Docker: `docker logs keycloak`
- Kubernetes: `kubectl logs <pod-name>`

### Useful Log Patterns

```bash
# Authentication failures
grep "FAILED_LOGIN" server.log

# Token validation issues
grep "Token verification" server.log

# LDAP sync errors
grep "LDAPStorageProvider" server.log

# Database errors
grep "SQLException" server.log
```

## Diagnostic Commands

### Check Realm Configuration

```bash
# Export realm configuration
bin/kcadm.sh get realms/{realm-name}

# Export users
bin/kcadm.sh get users -r {realm-name}

# Export clients
bin/kcadm.sh get clients -r {realm-name}
```

### Test Client Configuration

```bash
# Get access token (test client credentials)
curl -X POST https://keycloak.example.com/realms/{realm}/protocol/openid-connect/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "client_id={client-id}" \
  -d "client_secret={client-secret}" \
  -d "grant_type=client_credentials"

# Test password grant
curl -X POST https://keycloak.example.com/realms/{realm}/protocol/openid-connect/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "client_id={client-id}" \
  -d "client_secret={client-secret}" \
  -d "grant_type=password" \
  -d "username={username}" \
  -d "password={password}"
```

### Token Introspection

```bash
# Validate token
curl -X POST https://keycloak.example.com/realms/{realm}/protocol/openid-connect/token/introspect \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -u "{client-id}:{client-secret}" \
  -d "token={access-token}"
```

## Performance Issues

**Symptoms:** Slow login, high response times, timeouts

**Diagnosis:**
- Check database query performance
- Review connection pool usage
- Monitor JVM memory and GC
- Check cache hit ratios
- Review concurrent session count

**Solutions:**
- Increase database connection pool
- Optimize database indexes
- Increase JVM heap size (`-Xmx`, `-Xms`)
- Tune cache settings
- Scale horizontally (add cluster nodes)
- Enable persistent sessions if using ephemeral storage

## Quick Reference Commands

```bash
# Start KeyCloak development mode
bin/kc.sh start-dev

# Start KeyCloak production mode
bin/kc.sh start --optimized

# Build for specific database
bin/kc.sh build --db=postgres

# Export realm
bin/kc.sh export --dir /backup --realm my-realm

# Import realm
bin/kc.sh import --dir /backup

# Admin CLI login
bin/kcadm.sh config credentials --server http://localhost:8080 --realm master --user admin

# Create realm via CLI
bin/kcadm.sh create realms -s realm=my-realm -s enabled=true

# Create user via CLI
bin/kcadm.sh create users -r my-realm -s username=john -s enabled=true

# Reset user password via CLI
bin/kcadm.sh set-password -r my-realm --username john --new-password secret

# Get realm info
bin/kcadm.sh get realms/my-realm

# Update realm settings
bin/kcadm.sh update realms/my-realm -s sslRequired=EXTERNAL
```
