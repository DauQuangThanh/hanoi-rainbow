# High Availability & Scalability

## Clustering

### Configure Cluster

1. Use shared database (PostgreSQL, MySQL)
2. Enable distributed cache (Infinispan)
3. Configure cluster nodes in `cache-ispn.xml`
4. Set up load balancer (HAProxy, NGINX, AWS ALB)

### Cluster Requirements

- Shared database for persistent data
- Multicast or TCP discovery for cluster formation
- Session replication across nodes
- Consistent configuration across all nodes

### Load Balancer Configuration

- Sticky sessions: Recommended for performance
- Health checks: `/health` endpoint
- SSL termination: At load balancer or KeyCloak
- Connection timeouts: 60+ seconds for admin operations

## Database Performance

### Connection Pool Settings

```properties
# Increase pool size for high load
KC_DB_POOL_INITIAL_SIZE=10
KC_DB_POOL_MIN_SIZE=10
KC_DB_POOL_MAX_SIZE=50
```

### Database Optimization

- Index on username, email columns
- Regular vacuum (PostgreSQL)
- Monitor slow queries
- Use connection pooling (HikariCP built-in)
- Database replication for read scaling

## Caching Strategy

### Cache Types

- Realm cache: Realm configuration
- User cache: User data from user federation
- Keys cache: Signing and encryption keys
- Authorization cache: Permissions and policies

### Cache Configuration

- Max entries: Limit memory usage
- Lifespan: Balance freshness vs performance
- Eviction policy: LRU (Least Recently Used)
- Invalidation: Distributed in clustered environments

## Monitoring

### Health Checks

- Liveness: `/health/live` (is KeyCloak running)
- Readiness: `/health/ready` (is KeyCloak ready to serve requests)

### Metrics

- Enable metrics endpoint: `/metrics`
- Integrate with Prometheus/Grafana
- Monitor:
  - Active sessions
  - Token issuance rate
  - Login success/failure rate
  - Database connection pool usage
  - Cache hit/miss ratio
  - JVM memory and GC

### Alerting

- High failed login rate
- Database connection pool exhaustion
- High JVM memory usage
- Increased response times
- SSL certificate expiration

## Backup & Disaster Recovery

### Backup Strategy

**Database Backup:**
- Regular automated backups (daily minimum)
- Point-in-time recovery capability
- Off-site backup storage
- Test restore procedures regularly

**Configuration Backup:**
- Export realm configurations periodically
- Version control for realm exports
- Document custom configurations
- Backup custom themes and extensions

**Export Realm:**
```bash
# Export single realm
bin/kc.sh export --dir /backup --realm {realm-name}

# Export all realms
bin/kc.sh export --dir /backup

# Export with users (careful: large file)
bin/kc.sh export --dir /backup --realm {realm-name} --users realm_file
```

### Disaster Recovery

**Recovery Steps:**
1. Restore database from backup
2. Deploy KeyCloak with same version
3. Import realm configurations
4. Verify DNS and SSL certificates
5. Test authentication flows
6. Validate client integrations

**Recovery Time Objective (RTO):**
- Target: < 4 hours for production
- Keep documentation updated
- Maintain runbooks for recovery procedures
- Regular DR testing (quarterly)

## Migration & Upgrades

### Version Upgrades

**Pre-Upgrade Checklist:**
- [ ] Backup database
- [ ] Export realm configurations
- [ ] Review release notes for breaking changes
- [ ] Test upgrade in non-production environment
- [ ] Plan rollback procedure
- [ ] Schedule maintenance window

**Upgrade Process:**
1. Stop KeyCloak service
2. Backup database
3. Deploy new KeyCloak version
4. Run database migration (automatic on startup)
5. Start KeyCloak
6. Verify functionality
7. Monitor logs for errors

**Rollback Plan:**
- Keep previous version binaries
- Restore database backup
- Redeploy previous version
- Document rollback decision

### Migration from Other Systems

**From Legacy IAM (e.g., proprietary systems):**
1. Export users and groups (CSV, LDAP, API)
2. Map roles and permissions
3. Import users via Admin API or User Federation
4. Migrate client configurations manually
5. Update applications to use KeyCloak
6. Run parallel for testing period
7. Cutover and decommission legacy system

**From Other OAuth/OIDC Providers:**
1. Export client configurations
2. Recreate clients in KeyCloak
3. Update application endpoints
4. Migrate user database or use federation
5. Test authentication flows
6. Gradual cutover by application

## Performance Best Practices

- Use database connection pooling
- Enable caching for realm and user data
- Configure distributed caching in clustered setups
- Monitor and tune JVM settings
- Use sticky sessions with load balancers
- Implement CDN for static resources
- Regular database maintenance (vacuum, analyze)
- Monitor and optimize slow queries
- Scale horizontally for high load
- Use persistent sessions for stateful applications
