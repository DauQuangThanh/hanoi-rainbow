# Security Review Workflow


### Step 1: Initial Security Assessment

**Gather Context:**
- Identify application type (web app, API, mobile, desktop, embedded)
- Determine data sensitivity level (PII, financial, healthcare, etc.)
- Note compliance requirements (PCI-DSS, GDPR, HIPAA, SOC 2)
- Check authentication/authorization mechanisms
- Identify external integrations and APIs
- Review security configuration files

**Threat Modeling:**
- Identify assets (data, functions, resources)
- Determine threat actors (external attackers, insiders, automated bots)
- Map attack surfaces (user inputs, APIs, file operations, network)
- Assess security controls in place

### Step 2: OWASP Top 10 Analysis

Review code for OWASP Top 10 vulnerabilities:

#### 2.1 A01: Broken Access Control

**What to Check:**
- Authorization checks on all sensitive operations
- Horizontal privilege escalation (accessing other users' data)
- Vertical privilege escalation (accessing admin functions)
- Direct object references without authorization
- Missing function-level access control
- CORS misconfiguration
- Force browsing to unauthorized pages

**Vulnerable Examples:**

❌ **Missing Authorization Check:**
```python
@app.route('/user/<user_id>/profile')
def get_profile(user_id):
    # No check if current user can access this profile
    user = User.query.get(user_id)
    return jsonify(user.to_dict())
```

❌ **Insecure Direct Object Reference (IDOR):**
```javascript
// User can modify user_id to access others' orders
app.get('/api/orders/:order_id', (req, res) => {
    const order = db.getOrder(req.params.order_id);
    res.json(order);  // No ownership check
});
```

✅ **Secure Implementation:**
```python
@app.route('/user/<user_id>/profile')
@login_required
def get_profile(user_id):
    current_user_id = get_current_user_id()
    
    # Verify user can access this profile
    if user_id != current_user_id and not current_user_has_admin_role():
        abort(403, "Access denied")
    
    user = User.query.get_or_404(user_id)
    return jsonify(user.to_dict())
```

#### 2.2 A02: Cryptographic Failures

**What to Check:**
- Sensitive data transmitted over plain HTTP
- Weak encryption algorithms (MD5, SHA1, DES, RC4)
- Hard-coded encryption keys
- Insufficient key length
- Missing encryption for sensitive data at rest
- Predictable random number generation
- Password storage without proper hashing

**Vulnerable Examples:**

❌ **Weak Hashing:**
```java
// MD5 is cryptographically broken
String passwordHash = DigestUtils.md5Hex(password);
```

❌ **Hard-Coded Secret:**
```javascript
const jwt = require('jsonwebtoken');
const token = jwt.sign(payload, 'my-secret-key-123');  // Hard-coded
```

❌ **Insecure Random:**
```python
import random
token = random.randint(100000, 999999)  # Predictable
```

✅ **Secure Implementation:**
```java
// Use bcrypt with sufficient work factor
BCryptPasswordEncoder encoder = new BCryptPasswordEncoder(12);
String passwordHash = encoder.encode(password);
```

```javascript
// Load secret from environment
const token = jwt.sign(payload, process.env.JWT_SECRET, {
    algorithm: 'HS256',
    expiresIn: '1h'
});
```

```python
import secrets
token = secrets.token_urlsafe(32)  # Cryptographically secure
```

#### 2.3 A03: Injection

**What to Check:**
- SQL Injection in database queries
- NoSQL Injection in MongoDB/other NoSQL
- Command Injection in system calls
- LDAP Injection
- XML Injection
- XPath Injection
- Template Injection
- Expression Language Injection

**Vulnerable Examples:**

❌ **SQL Injection:**
```python
# Never concatenate user input into SQL
query = f"SELECT * FROM users WHERE username = '{username}' AND password = '{password}'"
cursor.execute(query)
```

❌ **Command Injection:**
```javascript
const { exec } = require('child_process');
// User input directly in command
exec(`ping -c 4 ${req.body.host}`, (err, stdout) => {
    res.send(stdout);
});
```

❌ **NoSQL Injection:**
```javascript
// Vulnerable to {$ne: null} injection
db.users.find({ username: req.body.username, password: req.body.password });
```

✅ **Secure Implementation:**
```python
# Use parameterized queries
query = "SELECT * FROM users WHERE username = ? AND password = ?"
cursor.execute(query, (username, password_hash))
```

```javascript
// Validate and sanitize input
const host = req.body.host;
if (!/^[\w.-]+$/.test(host)) {
    return res.status(400).send('Invalid host');
}
// Use execFile with array arguments
execFile('ping', ['-c', '4', host], (err, stdout) => {
    res.send(stdout);
});
```

```javascript
// Ensure inputs are strings, not objects
db.users.findOne({
    username: String(req.body.username),
    password: String(req.body.password)
});
```

#### 2.4 A04: Insecure Design

**What to Check:**
- Missing security requirements in design
- Insufficient threat modeling
- No rate limiting on sensitive operations
- Lack of defense in depth
- Missing input validation architecture
- No security logging and monitoring
- Insecure default configurations

**Example Issues:**

❌ **No Rate Limiting:**
```python
@app.route('/login', methods=['POST'])
def login():
    # No rate limiting - vulnerable to brute force
    username = request.json['username']
    password = request.json['password']
    # authenticate...
```

✅ **Rate Limiting Implemented:**
```python
from flask_limiter import Limiter

limiter = Limiter(app, key_func=get_remote_address)

@app.route('/login', methods=['POST'])
@limiter.limit("5 per minute")  # Max 5 attempts per minute
def login():
    username = request.json['username']
    password = request.json['password']
    # authenticate...
```

#### 2.5 A05: Security Misconfiguration

**What to Check:**
- Default credentials still enabled
- Unnecessary features enabled
- Error messages revealing sensitive info
- Security headers missing
- Outdated software versions
- Unnecessary services running
- Improper permissions

**Vulnerable Examples:**

❌ **Detailed Error Messages:**
```python
try:
    # database operation
except Exception as e:
    # Exposes internal details
    return jsonify({'error': str(e), 'traceback': traceback.format_exc()}), 500
```

❌ **Missing Security Headers:**
```javascript
// No security headers set
app.get('/', (req, res) => {
    res.send('<h1>Welcome</h1>');
});
```

✅ **Secure Implementation:**
```python
try:
    # database operation
except DatabaseError as e:
    logger.error(f"Database error: {e}")
    # Generic error message to user
    return jsonify({'error': 'An error occurred. Please try again later.'}), 500
```

```javascript
const helmet = require('helmet');
app.use(helmet());  // Sets secure headers

// Or manually:
app.use((req, res, next) => {
    res.setHeader('X-Content-Type-Options', 'nosniff');
    res.setHeader('X-Frame-Options', 'DENY');
    res.setHeader('X-XSS-Protection', '1; mode=block');
    res.setHeader('Strict-Transport-Security', 'max-age=31536000; includeSubDomains');
    res.setHeader('Content-Security-Policy', "default-src 'self'");
    next();
});
```

#### 2.6 A06: Vulnerable and Outdated Components

**What to Check:**
- Outdated dependencies with known CVEs
- Unused dependencies increasing attack surface
- Dependencies from untrusted sources
- No dependency scanning in CI/CD
- Transitive dependency vulnerabilities

**Detection Methods:**
```bash
# Python
pip-audit
safety check

# Node.js
npm audit
yarn audit

# Java
mvn dependency-check:check
./gradlew dependencyCheckAnalyze

# .NET
dotnet list package --vulnerable

# Go
go list -json -m all | nancy sleuth
```

**Report Format:**
```
Vulnerability: lodash Prototype Pollution
Severity: High (CVSS 7.4)
CVE: CVE-2020-8203
Affected: lodash@4.17.15
Fixed in: lodash@4.17.21
Impact: Remote code execution possible
Remediation: Update to version 4.17.21 or higher
```

#### 2.7 A07: Identification and Authentication Failures

**What to Check:**
- Weak password requirements
- No multi-factor authentication
- Session fixation vulnerabilities
- Exposed session identifiers in URLs
- Session timeout not implemented
- Credential stuffing protection missing
- Permits brute force attacks

**Vulnerable Examples:**

❌ **Weak Session Management:**
```php
// Session ID in URL - vulnerable to fixation
$session_id = $_GET['sessionid'];
session_id($session_id);
session_start();
```

❌ **No Session Timeout:**
```javascript
// Session never expires
app.use(session({
    secret: 'keyboard cat',
    resave: false,
    saveUninitialized: true
    // No maxAge set
}));
```

✅ **Secure Implementation:**
```php
// Regenerate session ID on login
session_start();
session_regenerate_id(true);
$_SESSION['user_id'] = $authenticated_user_id;
```

```javascript
app.use(session({
    secret: process.env.SESSION_SECRET,
    resave: false,
    saveUninitialized: false,
    cookie: {
        secure: true,      // HTTPS only
        httpOnly: true,    // Not accessible via JavaScript
        maxAge: 3600000,   // 1 hour timeout
        sameSite: 'strict' // CSRF protection
    }
}));
```

#### 2.8 A08: Software and Data Integrity Failures

**What to Check:**
- No integrity verification for updates
- Unsigned packages/artifacts
- Insecure deserialization
- No CI/CD pipeline security
- Auto-update without verification
- Untrusted data in pipelines

**Vulnerable Examples:**

❌ **Insecure Deserialization:**
```python
import pickle

# Dangerous - can execute arbitrary code
user_data = pickle.loads(request.data)
```

❌ **No Integrity Check:**
```javascript
// Downloads script without verification
const script = await fetch('https://cdn.example.com/lib.js').then(r => r.text());
eval(script);  // Extremely dangerous
```

✅ **Secure Implementation:**
```python
import json

# Use safe serialization
user_data = json.loads(request.data)
# Add schema validation
validate_schema(user_data, USER_SCHEMA)
```

```html
<!-- Use Subresource Integrity -->
<script src="https://cdn.example.com/lib.js" 
        integrity="sha384-oqVuAfXRKap7fdgcCY5uykM6+R9GqQ8K/uxy9rx7HNQlGYl1kPzQho1wx4JwY8wC"
        crossorigin="anonymous"></script>
```

#### 2.9 A09: Security Logging and Monitoring Failures

**What to Check:**
- No logging of security events
- Logs don't include sufficient context
- No alerting on suspicious activity
- Logs stored insecurely
- No log retention policy
- Missing audit trail

**What to Log:**

**Security Events:**
- Authentication attempts (success/failure)
- Authorization failures
- Input validation failures
- Privilege escalations
- Account changes (password reset, permission changes)
- Data access to sensitive resources
- Security configuration changes

**Example Implementation:**
```python
import logging

security_logger = logging.getLogger('security')

@app.route('/admin/users/<user_id>/promote')
@require_admin
def promote_user(user_id):
    current_admin = get_current_user()
    
    # Log security-relevant action
    security_logger.info(
        f"Admin privilege escalation",
        extra={
            'event': 'privilege_escalation',
            'admin_id': current_admin.id,
            'admin_username': current_admin.username,
            'target_user_id': user_id,
            'ip_address': request.remote_addr,
            'user_agent': request.user_agent.string,
            'timestamp': datetime.utcnow().isoformat()
        }
    )
    
    # Perform action
    promote_to_admin(user_id)
    return jsonify({'success': True})
```

#### 2.10 A10: Server-Side Request Forgery (SSRF)

**What to Check:**
- User-controlled URLs in backend requests
- No URL validation/allowlisting
- Access to internal services possible
- Cloud metadata endpoints accessible
- No network segmentation

**Vulnerable Examples:**

❌ **SSRF Vulnerability:**
```python
@app.route('/fetch')
def fetch_url():
    # User can access internal services
    url = request.args.get('url')
    response = requests.get(url)
    return response.content
```

✅ **Secure Implementation:**
```python
from urllib.parse import urlparse
import ipaddress

ALLOWED_DOMAINS = ['api.example.com', 'cdn.example.com']

@app.route('/fetch')
def fetch_url():
    url = request.args.get('url')
    
    # Parse and validate URL
    parsed = urlparse(url)
    
    # Only allow HTTPS
    if parsed.scheme != 'https':
        abort(400, "Only HTTPS URLs allowed")
    
    # Check domain allowlist
    if parsed.netloc not in ALLOWED_DOMAINS:
        abort(400, "Domain not allowed")
    
    # Prevent access to private IP ranges
    try:
        ip = socket.gethostbyname(parsed.netloc)
        if ipaddress.ip_address(ip).is_private:
            abort(400, "Private IP addresses not allowed")
    except socket.gaierror:
        abort(400, "Invalid domain")
    
    # Make request with timeout
    response = requests.get(url, timeout=5, allow_redirects=False)
    return response.content
```

### Step 3: Additional Security Checks

#### 3.1 Cross-Site Scripting (XSS)

**Check for:**
- User input rendered without escaping
- DOM-based XSS
- Reflected XSS
- Stored XSS
- Content-Security-Policy missing

**Examples:**

❌ **XSS Vulnerable:**
```javascript
// React - dangerous HTML injection
<div dangerouslySetInnerHTML={{__html: userInput}} />

// jQuery - XSS vulnerable
$('#output').html(userInput);

// Plain JS - XSS vulnerable
element.innerHTML = userInput;
```

✅ **XSS Protected:**
```javascript
// React - auto-escapes
<div>{userInput}</div>

// jQuery - text() escapes
$('#output').text(userInput);

// Plain JS - textContent escapes
element.textContent = userInput;

// DOMPurify for HTML sanitization
import DOMPurify from 'dompurify';
element.innerHTML = DOMPurify.sanitize(userInput);
```

#### 3.2 Cross-Site Request Forgery (CSRF)

**Check for:**
- State-changing operations without CSRF token
- CSRF token validation missing
- SameSite cookie attribute not set
- GET requests causing state changes

**Examples:**

❌ **No CSRF Protection:**
```python
@app.route('/account/delete', methods=['POST'])
@login_required
def delete_account():
    # No CSRF token verification
    current_user.delete()
    return redirect('/goodbye')
```

✅ **CSRF Protected:**
```python
from flask_wtf.csrf import CSRFProtect

csrf = CSRFProtect(app)

@app.route('/account/delete', methods=['POST'])
@login_required
def delete_account():
    # CSRF token automatically verified by Flask-WTF
    current_user.delete()
    return redirect('/goodbye')
```

#### 3.3 API Security

**Check for:**
- Missing authentication on endpoints
- No rate limiting
- Excessive data exposure
- Mass assignment vulnerabilities
- GraphQL query depth attacks
- API keys in client-side code

**Examples:**

❌ **Mass Assignment:**
```python
@app.route('/user/update', methods=['PUT'])
def update_user():
    user = get_current_user()
    # User can set any field including is_admin
    user.update(**request.json)
    db.commit()
```

✅ **Protected:**
```python
ALLOWED_FIELDS = {'name', 'email', 'phone'}

@app.route('/user/update', methods=['PUT'])
def update_user():
    user = get_current_user()
    # Only allow specific fields
    update_data = {k: v for k, v in request.json.items() if k in ALLOWED_FIELDS}
    user.update(**update_data)
    db.commit()
```

#### 3.4 File Upload Security

**Check for:**
- No file type validation
- Executable files allowed
- Path traversal in filenames
- No file size limits
- Files stored in web-accessible directory

**Examples:**

❌ **Insecure Upload:**
```python
@app.route('/upload', methods=['POST'])
def upload():
    file = request.files['file']
    # Dangerous - user controls filename
    file.save(f'/uploads/{file.filename}')
```

✅ **Secure Upload:**
```python
import os
from werkzeug.utils import secure_filename

ALLOWED_EXTENSIONS = {'png', 'jpg', 'jpeg', 'gif', 'pdf'}
MAX_FILE_SIZE = 10 * 1024 * 1024  # 10MB

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

@app.route('/upload', methods=['POST'])
def upload():
    file = request.files['file']
    
    # Validate file exists
    if not file or file.filename == '':
        abort(400, "No file provided")
    
    # Validate file type
    if not allowed_file(file.filename):
        abort(400, "File type not allowed")
    
    # Check file size
    file.seek(0, os.SEEK_END)
    if file.tell() > MAX_FILE_SIZE:
        abort(400, "File too large")
    file.seek(0)
    
    # Secure filename
    filename = secure_filename(file.filename)
    # Generate unique filename
    unique_filename = f"{uuid.uuid4()}_{filename}"
    
    # Store outside web root
    filepath = os.path.join('/secure/uploads', unique_filename)
    file.save(filepath)
    
    return jsonify({'filename': unique_filename})
```

#### 3.5 XML External Entity (XXE)

**Check for:**
- XML parsers with external entity processing enabled
- SOAP endpoints processing user XML
- SVG file uploads

**Examples:**

❌ **XXE Vulnerable:**
```python
from xml.etree.ElementTree import parse

# Vulnerable to XXE
tree = parse(user_xml_file)
```

✅ **XXE Protected:**
```python
from defusedxml.ElementTree import parse

# Protected against XXE
tree = parse(user_xml_file)
```

### Step 4: Language-Specific Security

**Java/Spring:**
- Check for SQL injection (use PreparedStatement)
- Validate @RequestBody with constraints
- Use Spring Security properly
- CSRF protection enabled
- Session fixation protection

**Node.js/Express:**
- helmet middleware for security headers
- express-rate-limit for rate limiting
- csrf middleware for CSRF protection
- Avoid eval() and new Function()
- Validate environment variables

**Python/Flask/Django:**
- SQLAlchemy parameterized queries
- Django ORM (safe by default)
- CSRF middleware enabled
- Secure session configuration
- Jinja2 auto-escaping

**.NET/C#:**
- Entity Framework parameterized queries
- AntiForgeryToken for CSRF
- ValidateInput attribute
- Authentication/authorization filters
- Data annotations for validation

**PHP:**
- Prepared statements (PDO or MySQLi)
- htmlspecialchars() for output escaping
- FILTER_* constants for input validation
- password_hash() for passwords
- CSRF token validation

### Step 5: Security Configuration Review

**Check:**

**Web Server:**
- TLS 1.2+ only
- Strong cipher suites
- HSTS enabled
- Security headers configured
- Directory listing disabled
- Default pages removed

**Database:**
- Strong authentication
- Encrypted connections
- Principle of least privilege
- Audit logging enabled
- Backup encryption

**Cloud/Infrastructure:**
- IAM roles properly scoped
- Security groups restrictive
- Encryption at rest enabled
- Logging enabled
- Secrets management (not hard-coded)

### Step 6: Compliance Validation

#### PCI-DSS (Payment Card Industry)

**Key Requirements:**
- Cardholder data encrypted in transit and at rest
- Strong access control measures
- Regular security testing
- Network segmentation
- Secure authentication

**Check for:**
- Credit card data in logs
- PAN (Primary Account Number) storage
- CVV/CVC storage (prohibited)
- Encryption key management

#### GDPR (General Data Protection Regulation)

**Key Requirements:**
- User consent for data processing
- Right to data deletion
- Right to data portability
- Data breach notification
- Privacy by design

**Check for:**
- PII data inventory
- Consent management
- Data retention policies
- Deletion mechanisms
- Data export functionality

#### HIPAA (Health Insurance Portability and Accountability Act)

**Key Requirements:**
- PHI (Protected Health Information) encryption
- Access controls and audit logs
- Data integrity controls
- Breach notification

**Check for:**
- Health data encryption
- Audit logging of PHI access
- Authentication mechanisms
- Data integrity verification

### Step 7: Generate Security Report
