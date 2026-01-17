# Security Hardening & Best Practices

## Authentication Security

### Password Policies

1. Realm Settings → Authentication → Password Policy
2. Add policies:
   - Minimum length: 12 characters
   - Uppercase characters: 1
   - Lowercase characters: 1
   - Digits: 1
   - Special characters: 1
   - Not username: Prevent username in password
   - Password history: 5 (prevent reuse)
   - Expire password: 90 days
   - Not email: Prevent email in password

### Brute Force Detection

1. Realm Settings → Security Defenses → Brute Force Detection
2. Enable brute force detection
3. Permanent lockout: Disable (use temporary)
4. Max login failures: 5
5. Wait increment: 60 seconds
6. Max wait: 900 seconds (15 minutes)
7. Failure reset time: 12 hours

### SSL/TLS Configuration

- Require SSL: All requests (production)
- Use valid SSL certificates (Let's Encrypt, commercial CA)
- TLS 1.2 or higher
- Strong cipher suites only

## Token Security

### Token Settings

- Access token lifespan: Short (5-15 minutes)
- Refresh token: Medium (30 minutes to 1 hour idle)
- Use refresh token rotation
- Revoke refresh tokens on logout

### Token Validation

- Verify signature (RSA, HMAC)
- Validate issuer (`iss` claim)
- Validate audience (`aud` claim)
- Check expiration (`exp` claim)
- Validate not before (`nbf` claim)

## Admin Security

### Admin Account Protection

- Strong passwords (min 16 characters)
- Enable MFA for all admin accounts
- Limit admin accounts to minimum necessary
- Use separate admin realm (master realm)
- Disable admin account when not in use
- Audit admin activities regularly

### Admin Console Access

- Restrict IP addresses if possible
- Use VPN for remote admin access
- Enable admin events logging
- Set up alerts for admin actions

## Auditing & Monitoring

### Enable Event Logging

1. Realm Settings → Events
2. Save events: Enable
3. Event listeners: Add `jboss-logging` or custom listeners
4. Login events: Enable (retention: 30-90 days)
5. Admin events: Enable (retention: 90-180 days)

### Event Types to Monitor

- Failed login attempts
- Password changes
- Role/permission changes
- Client configuration changes
- Token issuance and revocation
- Admin actions

### Integration with SIEM

- Export events to centralized logging
- Forward to Splunk, ELK, or other SIEM tools
- Set up alerts for suspicious activities
- Regular log review and analysis

## Security Checklist

### Production Deployment

- [ ] SSL/TLS enabled with valid certificates
- [ ] Strong password policies enforced
- [ ] Brute force protection enabled
- [ ] MFA enabled for privileged accounts
- [ ] Admin console access restricted
- [ ] Event logging enabled and monitored
- [ ] Regular security audits scheduled
- [ ] Backup and disaster recovery tested
- [ ] Network segmentation implemented
- [ ] Database credentials secured

### Regular Maintenance

- [ ] Review and update security policies quarterly
- [ ] Audit user accounts and permissions monthly
- [ ] Monitor and analyze security events weekly
- [ ] Update KeyCloak version within 30 days of release
- [ ] Rotate admin passwords every 90 days
- [ ] Review and remove inactive accounts monthly
- [ ] Test disaster recovery procedures quarterly
- [ ] Security penetration testing annually
