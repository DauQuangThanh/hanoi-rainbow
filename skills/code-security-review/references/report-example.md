# Security Report Example

This document provides a complete example of a security review report. Use this as a template when generating security reports.

---

# Security Review Report

**Application:** E-Commerce Web Application
**Review Date:** January 14, 2026
**Reviewer:** Security Team
**Scope:** 152 files, 23,450 lines of code
**Technology Stack:** Node.js, Express, PostgreSQL, React

---

## Executive Summary

**Security Rating:** 🔴 **HIGH RISK**

**Vulnerability Summary:**
- **Critical:** 2
- **High:** 5
- **Medium:** 11
- **Low:** 7
- **Informational:** 4

**Overall Assessment:**

The application contains multiple critical security vulnerabilities that pose significant risk to user data and business operations. Most critical issues involve SQL injection, broken authentication, and insecure cryptographic storage. The application **should NOT be deployed to production** until all critical and high-severity vulnerabilities are remediated.

**Key Findings:**
1. SQL injection vulnerability in user search functionality (CRITICAL)
2. Authentication bypass through JWT manipulation (CRITICAL)
3. Sensitive data (credit cards) stored in plain text (HIGH)
4. Missing rate limiting on API endpoints (HIGH)
5. Cross-site scripting (XSS) in product reviews (HIGH)

**Compliance Status:**
- **PCI-DSS:** ❌ Non-Compliant (3 critical requirements failed)
- **GDPR:** ⚠️ Partially Compliant (right to erasure not implemented)
- **OWASP Top 10:** ❌ Multiple vulnerabilities present

**Immediate Actions Required:**
1. Fix SQL injection in user search (Week 1)
2. Implement proper JWT validation (Week 1)
3. Encrypt sensitive data at rest (Week 1-2)
4. Add rate limiting (Week 2)
5. Sanitize user-generated content (Week 2)

**Estimated Remediation Time:** 4-6 weeks

---

## Detailed Findings

### Critical Vulnerabilities

---

#### CRITICAL-1: SQL Injection in User Search

**Severity:** Critical
**CWE ID:** CWE-89 (SQL Injection)
**OWASP:** A03:2021 - Injection
**CVSS Score:** 9.8 (Critical)

**Description:**

The user search functionality constructs SQL queries using unvalidated user input, allowing attackers to inject arbitrary SQL commands. This can lead to unauthorized data access, data modification, or complete database compromise.

**Location:**
- **File:** `src/controllers/userController.js`
- **Lines:** 45-52
- **Function:** `searchUsers()`

**Vulnerable Code:**

```javascript
async function searchUsers(req, res) {
    const searchTerm = req.query.q;
    
    // Vulnerable: Direct string concatenation
    const query = `SELECT * FROM users WHERE username LIKE '%${searchTerm}%' OR email LIKE '%${searchTerm}%'`;
    
    const results = await db.query(query);
    res.json(results);
}
```

**Exploit Scenario:**

An attacker can craft a malicious search query to extract sensitive data:

```
Step 1: Send request to /api/users/search?q=test
Step 2: Modify to: /api/users/search?q=test' UNION SELECT id, username, password_hash, email, credit_card FROM users--
Step 3: Response contains all user data including hashed passwords and credit cards
Step 4: Attacker downloads entire user database
```

**Proof of Concept:**
```bash
curl "https://api.example.com/api/users/search?q=test'%20UNION%20SELECT%20id,%20username,%20password_hash,%20email,%20credit_card%20FROM%20users--"
```

**Impact:**
- **Confidentiality:** Complete database compromise
- **Integrity:** Attacker can modify or delete database records
- **Availability:** Attacker can drop tables or cause denial of service
- **Compliance:** PCI-DSS 6.5.1 violation

**Remediation:**

Use parameterized queries to prevent SQL injection:

```javascript
async function searchUsers(req, res) {
    const searchTerm = req.query.q;
    
    // Input validation
    if (!searchTerm || searchTerm.length < 2) {
        return res.status(400).json({ error: 'Search term too short' });
    }
    
    if (searchTerm.length > 100) {
        return res.status(400).json({ error: 'Search term too long' });
    }
    
    // Secure: Parameterized query
    const query = `
        SELECT id, username, email, created_at 
        FROM users 
        WHERE username ILIKE $1 OR email ILIKE $1
        LIMIT 50
    `;
    
    const searchPattern = `%${searchTerm}%`;
    const results = await db.query(query, [searchPattern]);
    
    // Log search for audit
    auditLog.record({
        action: 'USER_SEARCH',
        user_id: req.user.id,
        search_term: searchTerm,
        results_count: results.length
    });
    
    res.json(results);
}
```

**Additional Recommendations:**
- Implement input validation and sanitization
- Use ORM (Sequelize, TypeORM) with built-in protections
- Apply least privilege to database user
- Enable database query logging and monitoring
- Implement Web Application Firewall (WAF) rules

**References:**
- CWE-89: https://cwe.mitre.org/data/definitions/89.html
- OWASP SQL Injection: https://owasp.org/www-community/attacks/SQL_Injection
- Node.js Parameterized Queries: https://node-postgres.com/features/queries

**Priority:** P0 - Fix immediately before any production deployment

---

#### CRITICAL-2: Authentication Bypass via JWT Manipulation

**Severity:** Critical
**CWE ID:** CWE-347 (Improper Verification of Cryptographic Signature)
**OWASP:** A07:2021 - Identification and Authentication Failures
**CVSS Score:** 9.1 (Critical)

**Description:**

The JWT verification middleware accepts tokens with the "none" algorithm, allowing attackers to forge tokens and authenticate as any user without knowing the secret key.

**Location:**
- **File:** `src/middleware/auth.js`
- **Lines:** 12-23
- **Function:** `verifyToken()`

**Vulnerable Code:**

```javascript
const jwt = require('jsonwebtoken');

function verifyToken(req, res, next) {
    const token = req.headers.authorization?.split(' ')[1];
    
    if (!token) {
        return res.status(401).json({ error: 'No token provided' });
    }
    
    // Vulnerable: Accepts 'none' algorithm
    const decoded = jwt.verify(token, process.env.JWT_SECRET);
    req.user = decoded;
    next();
}
```

**Exploit Scenario:**

```
Step 1: Capture a valid JWT token from network traffic
Step 2: Decode the JWT payload (it's base64-encoded)
Step 3: Modify the payload: change "user_id": 123 to "user_id": 1 (admin)
Step 4: Change algorithm in header to "none"
Step 5: Create new token without signature
Step 6: Send request with forged token
Step 7: Successfully authenticated as admin user
```

**Proof of Concept:**

```javascript
// Forged token with admin privileges
const header = {
    "alg": "none",
    "typ": "JWT"
};

const payload = {
    "user_id": 1,
    "username": "admin",
    "role": "admin",
    "exp": 9999999999
};

const forgedToken = 
    base64url(JSON.stringify(header)) + '.' +
    base64url(JSON.stringify(payload)) + '.';

// Use this token to access admin endpoints
fetch('/api/admin/users', {
    headers: { 'Authorization': `Bearer ${forgedToken}` }
});
```

**Impact:**
- Complete authentication bypass
- Privilege escalation to administrator
- Access to all user accounts
- Ability to modify or delete any data
- PCI-DSS 8.2 violation

**Remediation:**

```javascript
const jwt = require('jsonwebtoken');

function verifyToken(req, res, next) {
    const token = req.headers.authorization?.split(' ')[1];
    
    if (!token) {
        return res.status(401).json({ error: 'No token provided' });
    }
    
    try {
        // Secure: Explicitly specify allowed algorithms
        const decoded = jwt.verify(token, process.env.JWT_SECRET, {
            algorithms: ['HS256'],  // Only allow HMAC SHA-256
            issuer: 'your-app-name',
            audience: 'your-app-users'
        });
        
        // Additional validation
        if (!decoded.user_id || !decoded.role) {
            return res.status(401).json({ error: 'Invalid token payload' });
        }
        
        // Check token blacklist (for revoked tokens)
        if (await tokenBlacklist.isBlacklisted(token)) {
            return res.status(401).json({ error: 'Token has been revoked' });
        }
        
        req.user = decoded;
        next();
    } catch (error) {
        if (error.name === 'TokenExpiredError') {
            return res.status(401).json({ error: 'Token expired' });
        }
        if (error.name === 'JsonWebTokenError') {
            return res.status(401).json({ error: 'Invalid token' });
        }
        return res.status(500).json({ error: 'Authentication error' });
    }
}
```

**Additional Recommendations:**
- Implement token refresh mechanism
- Add token blacklist/revocation
- Use short token expiration times (15 minutes)
- Implement rate limiting on authentication endpoints
- Add MFA for sensitive operations
- Monitor for suspicious token usage patterns

**References:**
- CWE-347: https://cwe.mitre.org/data/definitions/347.html
- JWT Best Practices: https://datatracker.ietf.org/doc/html/rfc8725
- OWASP JWT Cheat Sheet: https://cheatsheetseries.owasp.org/cheatsheets/JSON_Web_Token_for_Java_Cheat_Sheet.html

**Priority:** P0 - Fix immediately before any production deployment

---

### High Severity Vulnerabilities

---

#### HIGH-1: Sensitive Data Stored in Plain Text

**Severity:** High
**CWE ID:** CWE-311 (Missing Encryption of Sensitive Data)
**OWASP:** A02:2021 - Cryptographic Failures
**CVSS Score:** 7.5 (High)

**Description:**

Credit card numbers and CVV codes are stored in the database without encryption. If the database is compromised, all payment information is immediately accessible to attackers.

**Location:**
- **File:** `src/models/payment.js`
- **Lines:** 34-42
- **Table:** `payments`

**Vulnerable Code:**

```javascript
const Payment = sequelize.define('Payment', {
    user_id: DataTypes.INTEGER,
    card_number: DataTypes.STRING(16),  // Plain text!
    cvv: DataTypes.STRING(3),           // Should NEVER be stored!
    expiry_date: DataTypes.STRING(5),
    amount: DataTypes.DECIMAL(10, 2)
});
```

**Impact:**
- PCI-DSS 3.4 critical violation
- Immediate fines if discovered during audit
- Complete exposure of payment data if database compromised
- Reputational damage and loss of customer trust
- Legal liability for data breach

**Remediation:**

```javascript
const crypto = require('crypto');
const { Fernet } = require('cryptography');

// Load encryption key from secure vault (not environment variables!)
const encryptionKey = await keyVault.getSecret('payment-encryption-key');
const cipher = new Fernet(encryptionKey);

const Payment = sequelize.define('Payment', {
    user_id: DataTypes.INTEGER,
    // Only store last 4 digits for display
    card_last_four: DataTypes.STRING(4),
    // Store full PAN encrypted (if absolutely necessary)
    encrypted_pan: DataTypes.TEXT,
    // NEVER store CVV - use tokenization instead
    payment_token: DataTypes.STRING,  // Token from payment processor
    expiry_date: DataTypes.STRING(5),
    amount: DataTypes.DECIMAL(10, 2)
});

// Encrypt before storing
Payment.beforeCreate(async (payment) => {
    if (payment.card_number) {
        // Extract last 4 for display
        payment.card_last_four = payment.card_number.slice(-4);
        
        // Encrypt full PAN
        payment.encrypted_pan = cipher.encrypt(
            Buffer.from(payment.card_number)
        ).toString();
        
        // Remove plaintext from model
        delete payment.card_number;
    }
});

// Decrypt when needed (should be rare!)
Payment.prototype.getDecryptedPAN = function() {
    if (!this.encrypted_pan) return null;
    const decrypted = cipher.decrypt(this.encrypted_pan);
    return decrypted.toString();
};
```

**Better Approach - Use Payment Tokenization:**

```javascript
// Use Stripe, PayPal, or other payment processor tokenization
const stripe = require('stripe')(process.env.STRIPE_SECRET_KEY);

async function processPayment(cardDetails, amount) {
    // Create payment method (card never touches your server)
    const paymentMethod = await stripe.paymentMethods.create({
        type: 'card',
        card: cardDetails  // Tokenized by Stripe.js in browser
    });
    
    // Store only the payment method ID
    const payment = await Payment.create({
        user_id: req.user.id,
        payment_method_id: paymentMethod.id,
        card_last_four: paymentMethod.card.last4,
        card_brand: paymentMethod.card.brand,
        amount: amount
    });
    
    // Process payment
    const charge = await stripe.paymentIntents.create({
        amount: amount * 100,  // Stripe uses cents
        currency: 'usd',
        payment_method: paymentMethod.id,
        confirm: true
    });
    
    return { payment, charge };
}
```

**Priority:** P0 - Critical for PCI-DSS compliance

---

#### HIGH-2: Missing Rate Limiting on API Endpoints

**Severity:** High
**CWE ID:** CWE-770 (Allocation of Resources Without Limits)
**OWASP:** A04:2021 - Insecure Design
**CVSS Score:** 7.2 (High)

**Description:**

API endpoints lack rate limiting, allowing attackers to perform brute force attacks, credential stuffing, and denial of service attacks without restrictions.

**Location:**
- **All API endpoints**
- **Particularly critical:** `/api/auth/login`, `/api/password/reset`

**Impact:**
- Brute force attacks on user accounts
- Credential stuffing with leaked passwords
- API abuse and resource exhaustion
- Denial of service
- Increased infrastructure costs

**Remediation:**

```javascript
const rateLimit = require('express-rate-limit');
const RedisStore = require('rate-limit-redis');
const redis = require('redis');

// Create Redis client for distributed rate limiting
const redisClient = redis.createClient({
    host: process.env.REDIS_HOST,
    port: process.env.REDIS_PORT
});

// Strict rate limit for authentication endpoints
const authLimiter = rateLimit({
    store: new RedisStore({
        client: redisClient,
        prefix: 'rl:auth:'
    }),
    windowMs: 15 * 60 * 1000, // 15 minutes
    max: 5, // 5 requests per window
    message: 'Too many login attempts, please try again later',
    standardHeaders: true,
    legacyHeaders: false,
    // Return remaining attempts
    handler: (req, res) => {
        res.status(429).json({
            error: 'Too many attempts',
            retryAfter: Math.ceil(req.rateLimit.resetTime / 1000)
        });
    }
});

// General API rate limit
const apiLimiter = rateLimit({
    store: new RedisStore({
        client: redisClient,
        prefix: 'rl:api:'
    }),
    windowMs: 60 * 1000, // 1 minute
    max: 100, // 100 requests per minute
    message: 'Too many requests, please slow down'
});

// Apply to routes
app.post('/api/auth/login', authLimiter, loginController);
app.post('/api/auth/register', authLimiter, registerController);
app.post('/api/password/reset', authLimiter, resetPasswordController);
app.use('/api/', apiLimiter);

// User-specific rate limiting
function userRateLimiter(req, res, next) {
    const userId = req.user?.id;
    if (!userId) return next();
    
    const key = `user:${userId}:requests`;
    
    redisClient.incr(key, (err, requests) => {
        if (requests === 1) {
            redisClient.expire(key, 60); // 1 minute window
        }
        
        if (requests > 200) { // 200 requests per minute per user
            return res.status(429).json({
                error: 'Rate limit exceeded'
            });
        }
        
        next();
    });
}

app.use('/api/', userRateLimiter);
```

**Priority:** P1 - Implement within 1 week

---

## Remediation Timeline

### Phase 1: Critical Issues (Week 1)
**Risk Reduction:** 70%

- [ ] **CRITICAL-1:** Fix SQL injection in user search
  - **Effort:** 1 day
  - **Owner:** Backend team
  - **Dependencies:** None

- [ ] **CRITICAL-2:** Fix JWT authentication bypass
  - **Effort:** 2 days
  - **Owner:** Security team
  - **Dependencies:** None

- [ ] **HIGH-1:** Encrypt sensitive payment data
  - **Effort:** 3-4 days
  - **Owner:** Backend + DevOps
  - **Dependencies:** Key management setup

**Deliverables:**
- Code fixes merged to main branch
- Unit tests for SQL injection prevention
- Integration tests for JWT validation
- Encryption key rotation procedure

---

### Phase 2: High Priority (Weeks 2-3)
**Risk Reduction:** 20%

- [ ] **HIGH-2:** Implement rate limiting
- [ ] **HIGH-3:** Fix XSS in product reviews
- [ ] **HIGH-4:** Add CSRF protection
- [ ] **HIGH-5:** Implement secure session management

**Deliverables:**
- Rate limiting deployed to all environments
- XSS test suite
- CSRF tokens on all forms
- Session security audit

---

### Phase 3: Medium Priority (Month 2)
**Risk Reduction:** 8%

- [ ] Fix all medium severity vulnerabilities
- [ ] Implement security headers
- [ ] Add input validation framework
- [ ] Enable database query logging
- [ ] Implement WAF rules

---

### Phase 4: Low Priority & Hardening (Month 3)
**Risk Reduction:** 2%

- [ ] Fix low severity issues
- [ ] Implement security monitoring
- [ ] Add intrusion detection
- [ ] Perform penetration testing
- [ ] Document security architecture

---

## Compliance Assessment

### PCI-DSS Status: ❌ NON-COMPLIANT

| Requirement | Status | Findings | Priority |
|-------------|--------|----------|----------|
| 3.4: Encrypt cardholder data | ❌ Failed | Plain text storage | P0 |
| 6.5.1: Injection flaws | ❌ Failed | SQL injection present | P0 |
| 6.5.10: Authentication | ❌ Failed | JWT bypass possible | P0 |
| 8.2: Multi-factor auth | ⚠️ Partial | Only for admins | P1 |
| 10.1: Audit trails | ⚠️ Partial | Incomplete logging | P2 |

**Estimated Time to Compliance:** 4-6 weeks
**Compliance Owner:** CISO

---

### GDPR Status: ⚠️ PARTIALLY COMPLIANT

| Requirement | Status | Findings |
|-------------|--------|----------|
| Right to access (Art 15) | ✅ Compliant | Export function implemented |
| Right to erasure (Art 17) | ❌ Non-Compliant | No deletion mechanism |
| Data minimization (Art 5) | ⚠️ Partial | Excessive logging |
| Encryption (Art 32) | ⚠️ Partial | Missing for some data |
| Breach notification (Art 33) | ✅ Compliant | Process documented |

**Priority Actions:**
1. Implement user data deletion (P1)
2. Reduce log data retention (P2)
3. Encrypt all personal data (P1)

---

## Security Metrics

### Vulnerability Distribution

```
Critical:  ██ 2 (7%)
High:      █████ 5 (17%)
Medium:    ███████████ 11 (38%)
Low:       ███████ 7 (24%)
Info:      ████ 4 (14%)
```

### OWASP Top 10 Coverage

| Category | Vulnerabilities | Severity |
|----------|----------------|----------|
| A01: Broken Access Control | 2 | High |
| A02: Cryptographic Failures | 3 | Critical/High |
| A03: Injection | 2 | Critical/High |
| A04: Insecure Design | 4 | High/Medium |
| A05: Security Misconfiguration | 6 | Medium/Low |
| A06: Vulnerable Components | 3 | Medium |
| A07: Authentication Failures | 2 | Critical/High |
| A08: Data Integrity Failures | 2 | Medium |
| A09: Logging Failures | 3 | Medium/Low |
| A10: SSRF | 0 | - |

### CWE Distribution

Most common vulnerability categories:
1. CWE-89: SQL Injection (2 findings)
2. CWE-79: Cross-site Scripting (3 findings)
3. CWE-311: Missing Encryption (3 findings)
4. CWE-352: CSRF (2 findings)
5. CWE-770: Missing Rate Limiting (4 findings)

---

## Conclusion

This application requires immediate security remediation before production deployment. The presence of critical SQL injection and authentication bypass vulnerabilities poses unacceptable risk to user data and business operations.

**Recommendations:**
1. **Immediate:** Fix all critical vulnerabilities (Week 1)
2. **Short-term:** Address high severity issues (Weeks 2-3)
3. **Medium-term:** Achieve PCI-DSS and GDPR compliance (4-6 weeks)
4. **Ongoing:** Implement security testing in CI/CD pipeline
5. **Long-term:** Establish security champions program

**Next Steps:**
1. Convene security review meeting with stakeholders
2. Assign ownership for each vulnerability
3. Create JIRA tickets with detailed remediation steps
4. Establish weekly security sync meetings
5. Schedule retest after Phase 1 completion

---

## Appendix A: Testing Methodology

**Tools Used:**
- Manual code review (primary)
- OWASP ZAP for dynamic testing
- Snyk for dependency scanning
- SonarQube for static analysis
- Custom security test scripts

**Limitations:**
- Testing performed in staging environment
- No social engineering testing
- No physical security assessment
- Infrastructure security not in scope
- Denial of service testing limited

---

## Appendix B: Security Team Contacts

**Questions or Concerns:**
- Security Team: security@example.com
- CISO: ciso@example.com
- On-call Security: +1-555-SECURITY

**For Urgent Security Issues:**
- Slack: #security-incidents
- PagerDuty: Security Team escalation

---

**Report Version:** 1.0  
**Classification:** CONFIDENTIAL  
**Distribution:** Security team, Engineering leadership, CISO
