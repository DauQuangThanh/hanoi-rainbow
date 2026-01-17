# Compliance Requirements

## Overview

This document provides guidance on assessing code security compliance with common regulatory and industry standards. Use this when evaluating code against specific compliance frameworks.

## Table of Contents

- [PCI-DSS (Payment Card Industry Data Security Standard)](#pci-dss)
- [GDPR (General Data Protection Regulation)](#gdpr)
- [HIPAA (Health Insurance Portability and Accountability Act)](#hipaa)
- [SOC 2](#soc-2)
- [Compliance Assessment Process](#compliance-assessment-process)

---

## PCI-DSS

### Requirement 3: Protect Stored Cardholder Data

**3.4: Render PAN unreadable anywhere it is stored**

**What to Check:**
- Primary Account Number (PAN) encryption at rest
- Strong cryptography (AES-256, RSA 2048+)
- Key management practices
- Avoid storing sensitive authentication data (CVV, PIN)

**Code Review Points:**
```python
# ❌ BAD: Weak encryption
import hashlib
card_hash = hashlib.md5(card_number.encode()).hexdigest()  # MD5 is not encryption

# ❌ BAD: Plain text storage
db.execute("INSERT INTO payments (card_number) VALUES (?)", (card_number,))

# ✅ GOOD: Strong encryption with proper key management
from cryptography.fernet import Fernet
cipher = Fernet(get_encryption_key_from_secure_vault())
encrypted_pan = cipher.encrypt(card_number.encode())
db.execute("INSERT INTO payments (encrypted_pan) VALUES (?)", (encrypted_pan,))
```

**Compliance Validation:**
- [ ] PAN is encrypted using strong cryptography (AES-256 minimum)
- [ ] Encryption keys stored separately from encrypted data
- [ ] Key rotation policy implemented
- [ ] CVV/CVV2/PIN never stored
- [ ] Disk-level encryption enabled

---

### Requirement 6: Develop and Maintain Secure Systems

**6.5: Address common coding vulnerabilities in software-development processes**

**Required Coverage:**
- 6.5.1: Injection flaws (SQL, OS, LDAP)
- 6.5.2: Buffer overflows
- 6.5.3: Insecure cryptographic storage
- 6.5.4: Insecure communications
- 6.5.5: Improper error handling
- 6.5.6: All "high risk" vulnerabilities identified in vulnerability identification process
- 6.5.7: Cross-site scripting (XSS)
- 6.5.8: Improper access control
- 6.5.9: Cross-site request forgery (CSRF)
- 6.5.10: Broken authentication and session management

**Compliance Validation:**
- [ ] All inputs validated and sanitized
- [ ] Parameterized queries used (no dynamic SQL)
- [ ] TLS 1.2+ for all cardholder data transmission
- [ ] Secure session management
- [ ] CSRF protection implemented
- [ ] XSS prevention (output encoding, CSP)

---

### Requirement 8: Identify and Authenticate Access

**8.1: Define and implement policies to ensure proper user identification management**

**What to Check:**
- Unique user IDs for all users
- Multi-factor authentication for remote access
- Password complexity requirements
- Account lockout after failed attempts

**Code Review Points:**
```javascript
// ❌ BAD: Weak password requirements
if (password.length >= 6) { /* accept */ }

// ❌ BAD: No account lockout
if (bcrypt.compare(password, user.password_hash)) { /* login */ }

// ✅ GOOD: Strong password policy and lockout
const PASSWORD_MIN_LENGTH = 12;
const MAX_LOGIN_ATTEMPTS = 3;
const LOCKOUT_DURATION = 30 * 60 * 1000; // 30 minutes

if (user.failed_attempts >= MAX_LOGIN_ATTEMPTS) {
    if (Date.now() - user.last_failed_attempt < LOCKOUT_DURATION) {
        throw new Error('Account temporarily locked');
    }
    user.failed_attempts = 0;
}

if (!validatePasswordComplexity(password)) {
    throw new Error('Password must be 12+ chars with upper, lower, number, special char');
}
```

**8.2: Ensure proper user-authentication management**

**Compliance Validation:**
- [ ] Passwords at least 12 characters or 8 with complexity
- [ ] MFA required for administrative access
- [ ] MFA required for all remote network access
- [ ] Sessions timeout after 15 minutes of inactivity
- [ ] Failed login attempts limited (max 6 attempts)
- [ ] Account locked for at least 30 minutes after max attempts
- [ ] Password history maintained (prevent reuse of last 4 passwords)

---

### Requirement 10: Track and Monitor Network Resources

**10.1: Implement audit trails to link all access to system components**

**What to Check:**
- User identification logged
- Event type logged
- Date and time logged
- Success or failure indication
- Origination of event
- Identity or name of affected data/system/resource

**Code Review Points:**
```python
# ❌ BAD: No audit logging
def update_payment(payment_id, amount):
    db.execute("UPDATE payments SET amount = ? WHERE id = ?", (amount, payment_id))

# ✅ GOOD: Comprehensive audit logging
def update_payment(payment_id, amount, user_id, ip_address):
    db.execute("UPDATE payments SET amount = ? WHERE id = ?", (amount, payment_id))
    
    audit_log.record({
        'event_type': 'PAYMENT_UPDATE',
        'user_id': user_id,
        'payment_id': payment_id,
        'old_amount': old_amount,
        'new_amount': amount,
        'timestamp': datetime.utcnow(),
        'ip_address': ip_address,
        'success': True
    })
```

**Compliance Validation:**
- [ ] All individual user access logged
- [ ] All actions by privileged users logged
- [ ] Access to audit trails logged
- [ ] Invalid logical access attempts logged
- [ ] All authentication mechanisms logged
- [ ] Initialization of audit logs logged
- [ ] Creation and deletion of system objects logged

---

### PCI-DSS Compliance Assessment Template

```markdown
## PCI-DSS Compliance Status

| Requirement | Status | Findings | Remediation |
|-------------|--------|----------|-------------|
| 3.4: Encrypt PAN | ⚠️ Partial | Weak encryption (MD5) | Implement AES-256 |
| 6.5: Secure coding | ❌ Non-Compliant | SQL injection found | Use parameterized queries |
| 8.1: User identification | ✅ Compliant | Proper implementation | - |
| 8.2: Authentication | ⚠️ Partial | No MFA for admins | Implement MFA |
| 10.1: Audit trails | ❌ Non-Compliant | Incomplete logging | Add comprehensive logging |

**Overall Status:** ❌ Non-Compliant
**Critical Issues:** 2
**Estimated Remediation Time:** 4-6 weeks
```

---

## GDPR

### Data Protection Principles (Article 5)

**What to Check:**
- Lawfulness, fairness, transparency
- Purpose limitation
- Data minimization
- Accuracy
- Storage limitation
- Integrity and confidentiality
- Accountability

### Technical and Organizational Measures (Article 32)

**Required Security Measures:**
- Pseudonymization and encryption of personal data
- Ongoing confidentiality, integrity, availability
- Ability to restore data after incident
- Regular testing and evaluation

**Code Review Points:**

```python
# ❌ BAD: Excessive data collection
user_data = {
    'email': email,
    'password': password,
    'ip_address': request.ip,
    'user_agent': request.user_agent,
    'location': geolocate(request.ip),
    'device_fingerprint': generate_fingerprint(),
    # Too much data without clear purpose
}

# ✅ GOOD: Data minimization
user_data = {
    'email': email,
    'password_hash': bcrypt.hash(password),
    # Only collect what's necessary for the stated purpose
}
```

**Compliance Validation:**
- [ ] Personal data encrypted at rest and in transit
- [ ] Data retention policies implemented
- [ ] Right to erasure (deletion) implemented
- [ ] Data portability features available
- [ ] Consent management system in place
- [ ] Data breach notification procedures
- [ ] Privacy by design principles followed
- [ ] Data minimization applied
- [ ] Purpose limitation documented

---

### Right to Erasure (Article 17)

**Code Review Points:**
```python
# ✅ GOOD: Complete data deletion
def delete_user_data(user_id):
    # Delete from all systems
    db.execute("DELETE FROM users WHERE id = ?", (user_id,))
    db.execute("DELETE FROM user_profiles WHERE user_id = ?", (user_id,))
    db.execute("DELETE FROM user_preferences WHERE user_id = ?", (user_id,))
    
    # Anonymize audit logs (retain for legal compliance)
    db.execute(
        "UPDATE audit_logs SET user_id = 'DELETED_USER' WHERE user_id = ?",
        (user_id,)
    )
    
    # Remove from backup systems (schedule for next backup cycle)
    queue_backup_deletion(user_id)
    
    # Log the deletion (required for accountability)
    compliance_log.record({
        'action': 'USER_DATA_DELETION',
        'user_id': user_id,
        'timestamp': datetime.utcnow(),
        'request_source': 'USER_REQUEST'
    })
```

**Compliance Validation:**
- [ ] User can request data deletion
- [ ] All personal data deleted within 30 days
- [ ] Data deleted from backups
- [ ] Anonymization applied where deletion not possible
- [ ] Deletion confirmation provided to user

---

### GDPR Compliance Assessment Template

```markdown
## GDPR Compliance Status

| Requirement | Status | Findings |
|-------------|--------|----------|
| Lawful basis for processing | ✅ | Consent mechanism implemented |
| Data minimization | ⚠️ | Excessive logging of IP addresses |
| Encryption | ✅ | TLS 1.3, AES-256 at rest |
| Right to erasure | ❌ | No deletion mechanism |
| Data portability | ⚠️ | Export available, format not machine-readable |
| Breach notification | ✅ | Process documented and tested |

**Overall Status:** ⚠️ Partially Compliant
**Critical Gaps:** Right to erasure
```

---

## HIPAA

### Security Rule - Technical Safeguards (45 CFR § 164.312)

**Access Control (§ 164.312(a)(1))**

**Required Implementation:**
- Unique user identification (Required)
- Emergency access procedures (Required)
- Automatic logoff (Addressable)
- Encryption and decryption (Addressable)

**Code Review Points:**
```java
// ❌ BAD: Shared credentials
String DB_USER = "app_user";
String DB_PASS = "shared_password";

// ✅ GOOD: Individual user accounts
public class DataAccess {
    private String userId;
    private AuditLogger auditLogger;
    
    public void accessPatientRecord(String patientId) {
        // Check user authorization
        if (!authService.canAccessPatient(userId, patientId)) {
            auditLogger.logUnauthorizedAccess(userId, patientId);
            throw new UnauthorizedException();
        }
        
        // Log access
        auditLogger.logAccess(userId, patientId, "READ");
        
        // Retrieve data
        return patientRepository.findById(patientId);
    }
}
```

**Compliance Validation:**
- [ ] Unique user ID for all users
- [ ] Emergency access procedures documented
- [ ] Session timeout implemented (15 minutes)
- [ ] PHI encrypted at rest (AES-256)
- [ ] PHI encrypted in transit (TLS 1.2+)

---

**Audit Controls (§ 164.312(b))**

**Required:** Hardware, software, and procedural mechanisms to record and examine access to ePHI

**Code Review Points:**
```python
# ✅ GOOD: Comprehensive HIPAA audit logging
class HIPAAAuditLogger:
    def log_access(self, user_id, patient_id, action, phi_fields):
        """Log access to Protected Health Information"""
        self.write_audit_entry({
            'timestamp': datetime.utcnow().isoformat(),
            'user_id': user_id,
            'patient_id': patient_id,
            'action': action,  # CREATE, READ, UPDATE, DELETE
            'phi_fields_accessed': phi_fields,
            'ip_address': request.remote_addr,
            'success': True
        })
    
    def log_failed_access(self, user_id, patient_id, reason):
        """Log unauthorized access attempts"""
        self.write_audit_entry({
            'timestamp': datetime.utcnow().isoformat(),
            'user_id': user_id,
            'patient_id': patient_id,
            'action': 'UNAUTHORIZED_ACCESS',
            'reason': reason,
            'ip_address': request.remote_addr,
            'success': False
        })
```

**Compliance Validation:**
- [ ] All PHI access logged
- [ ] All modifications to PHI logged
- [ ] All deletions of PHI logged
- [ ] All disclosure of PHI logged
- [ ] Failed access attempts logged
- [ ] Audit logs retained for 6 years
- [ ] Audit logs protected from modification

---

**Integrity (§ 164.312(c)(1))**

**Required:** Mechanisms to ensure ePHI is not improperly altered or destroyed

**Code Review Points:**
```python
# ✅ GOOD: Data integrity controls
def update_patient_record(patient_id, updates, user_id):
    # Verify data integrity before update
    current_record = get_patient_record(patient_id)
    if current_record.checksum != calculate_checksum(current_record):
        raise IntegrityError("Record has been tampered with")
    
    # Create audit trail before modification
    create_version_snapshot(current_record)
    
    # Apply updates
    new_record = apply_updates(current_record, updates)
    new_record.checksum = calculate_checksum(new_record)
    new_record.last_modified_by = user_id
    new_record.last_modified_at = datetime.utcnow()
    
    # Save with integrity verification
    save_with_transaction(new_record)
    
    # Log modification
    audit_log.record_modification(patient_id, user_id, updates)
```

**Compliance Validation:**
- [ ] Mechanisms to detect unauthorized alterations
- [ ] Data integrity verification implemented
- [ ] Version control for PHI records
- [ ] Backup and recovery procedures tested

---

### HIPAA Compliance Assessment Template

```markdown
## HIPAA Security Rule Compliance

| Requirement | Standard | Status | Findings |
|-------------|----------|--------|----------|
| Access Control | § 164.312(a) | ⚠️ | No automatic logoff |
| Audit Controls | § 164.312(b) | ✅ | Comprehensive logging |
| Integrity | § 164.312(c) | ⚠️ | No data integrity checksums |
| Transmission Security | § 164.312(e) | ✅ | TLS 1.3 enforced |

**Overall Status:** ⚠️ Partially Compliant
**Required Actions:** Implement automatic logoff, add integrity controls
```

---

## SOC 2

### Trust Services Criteria

**CC6.1: Logical and Physical Access Controls**

**What to Check:**
- User authentication (CC6.1)
- Authorization mechanisms (CC6.1)
- Access provisioning and termination (CC6.2)
- Credential lifecycle management (CC6.2)

**Compliance Validation:**
- [ ] Multi-factor authentication implemented
- [ ] Role-based access control (RBAC)
- [ ] Least privilege principle enforced
- [ ] Access reviews performed quarterly
- [ ] Terminated user access revoked within 24 hours

---

**CC6.6: Encryption**

**Code Review Points:**
```javascript
// ❌ BAD: No encryption in transit
app.get('/api/customer-data', (req, res) => {
    res.json(customerData);  // Sent over HTTP
});

// ✅ GOOD: Encryption in transit
const https = require('https');
const app = express();

// Force HTTPS
app.use((req, res, next) => {
    if (!req.secure) {
        return res.redirect('https://' + req.get('host') + req.url);
    }
    next();
});

// Set security headers
app.use((req, res, next) => {
    res.setHeader('Strict-Transport-Security', 'max-age=31536000; includeSubDomains');
    next();
});
```

**Compliance Validation:**
- [ ] TLS 1.2+ for data in transit
- [ ] AES-256 for data at rest
- [ ] Key management procedures documented
- [ ] Certificate lifecycle management

---

**CC7.2: System Monitoring**

**Compliance Validation:**
- [ ] Security monitoring implemented
- [ ] Alerting for suspicious activity
- [ ] Log aggregation and analysis
- [ ] Incident response procedures
- [ ] Regular security testing

---

### SOC 2 Compliance Assessment Template

```markdown
## SOC 2 Type II Compliance

| Control | Status | Evidence | Gaps |
|---------|--------|----------|------|
| CC6.1: Access Controls | ✅ | MFA implemented, RBAC in place | None |
| CC6.2: Access Provisioning | ⚠️ | Manual process | Automate deprovisioning |
| CC6.6: Encryption | ✅ | TLS 1.3, AES-256 | None |
| CC7.2: Monitoring | ⚠️ | Basic logging | Enhance alerting |

**Overall Readiness:** 75%
**Audit-Ready Timeline:** 2-3 months
```

---

## Compliance Assessment Process

### 1. Identify Applicable Standards

Determine which compliance frameworks apply based on:
- Industry sector (healthcare, finance, retail)
- Geographic location (EU, US, other)
- Data types processed (PII, PHI, payment cards)
- Business relationships (customers, partners)

### 2. Map Requirements to Code

For each applicable standard:
- List specific technical requirements
- Identify relevant code sections
- Document security controls
- Note any gaps or deficiencies

### 3. Perform Gap Analysis

Compare current implementation against requirements:
- Compliant: Meets all requirements
- Partially Compliant: Meets some requirements
- Non-Compliant: Does not meet requirements
- Not Applicable: Requirement doesn't apply

### 4. Prioritize Remediation

Order by:
1. Regulatory risk (fines, legal action)
2. Security risk (likelihood × impact)
3. Effort required (quick wins first)
4. Business impact (customer trust, contracts)

### 5. Document Findings

Include in security report:
- Compliance status summary
- Specific requirement violations
- Risk assessment for each gap
- Detailed remediation steps
- Estimated timeline and effort
- Responsible parties

---

## Multi-Standard Compliance Matrix

Use this to track compliance across multiple standards:

| Security Control | PCI-DSS | GDPR | HIPAA | SOC 2 |
|------------------|---------|------|-------|-------|
| Encryption at rest | 3.4 | Art 32 | §312(a) | CC6.6 |
| Encryption in transit | 4.1 | Art 32 | §312(e) | CC6.6 |
| Access control | 7.1 | Art 25 | §312(a) | CC6.1 |
| Audit logging | 10.1 | Art 30 | §312(b) | CC7.2 |
| MFA | 8.3 | - | §312(a) | CC6.1 |
| Data retention | 3.1 | Art 5 | §310 | - |
| Incident response | 12.10 | Art 33 | §308 | CC7.3 |

This allows efficient review of controls that satisfy multiple requirements simultaneously.
