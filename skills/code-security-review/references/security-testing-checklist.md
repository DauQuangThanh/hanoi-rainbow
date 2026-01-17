# Security Testing Checklist


Use this for systematic security reviews:

**Authentication & Authorization:**
- [ ] All endpoints require authentication
- [ ] Authorization checks on every operation
- [ ] No horizontal privilege escalation possible
- [ ] No vertical privilege escalation possible
- [ ] Session management secure
- [ ] Password policy enforced
- [ ] MFA available for sensitive operations

**Input Validation:**
- [ ] All input validated (whitelist approach)
- [ ] SQL injection prevention (parameterized queries)
- [ ] XSS prevention (output encoding)
- [ ] Command injection prevention
- [ ] Path traversal prevention
- [ ] File upload restrictions

**Cryptography:**
- [ ] Strong algorithms only (AES-256, RSA-2048+)
- [ ] Secure password hashing (bcrypt, Argon2)
- [ ] TLS 1.2+ enforced
- [ ] No hard-coded secrets
- [ ] Secure random number generation
- [ ] Certificate validation

**Session Management:**
- [ ] Secure session ID generation
- [ ] Session fixation protection
- [ ] Proper session timeout
- [ ] HttpOnly and Secure flags set
- [ ] SameSite attribute configured
- [ ] Session invalidation on logout

**Error Handling:**
- [ ] Generic error messages to users
- [ ] Detailed errors logged server-side
- [ ] No stack traces exposed
- [ ] No sensitive data in errors

**Logging & Monitoring:**
- [ ] Security events logged
- [ ] Sufficient log detail
- [ ] Logs protected from tampering
- [ ] Alerting on suspicious activity
- [ ] Log retention policy

**API Security:**
- [ ] API authentication required
- [ ] Rate limiting implemented
- [ ] Input validation on all endpoints
- [ ] No excessive data exposure
- [ ] CORS properly configured
- [ ] API versioning

**Data Protection:**
- [ ] Sensitive data encrypted at rest
- [ ] Sensitive data encrypted in transit
- [ ] PII handling compliant with regulations
- [ ] Secure data deletion
- [ ] Backup encryption

**Dependencies:**
- [ ] No known vulnerable dependencies
- [ ] Dependency scanning in CI/CD
- [ ] Regular dependency updates
- [ ] SCA (Software Composition Analysis) tools

**Configuration:**
- [ ] Security headers configured
- [ ] Default credentials changed
- [ ] Unnecessary features disabled
- [ ] Secure defaults
- [ ] Environment-specific configs
