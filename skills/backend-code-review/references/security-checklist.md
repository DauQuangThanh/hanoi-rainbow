# Security Checklist

Comprehensive security guidelines for backend code review.

## Table of Contents
- [Authentication](#authentication)
- [Authorization](#authorization)
- [Input Validation](#input-validation)
- [SQL/NoSQL Injection Prevention](#sqlnosql-injection-prevention)
- [Cryptography](#cryptography)
- [Secret Management](#secret-management)
- [API Security](#api-security)
- [Secure Dependencies](#secure-dependencies)

## Authentication

### JWT Implementation

**Good:**
```typescript
// ✅ Secure JWT implementation
import jwt from 'jsonwebtoken';
import bcrypt from 'bcrypt';

const JWT_SECRET = process.env.JWT_SECRET!;  // From environment
const JWT_EXPIRY = '15m';  // Short-lived access token
const REFRESH_EXPIRY = '7d';

interface TokenPayload {
  userId: string;
  email: string;
}

async function login(email: string, password: string) {
  const user = await User.findOne({ email });
  if (!user) {
    throw new Error('Invalid credentials');
  }
  
  // ✅ Use bcrypt for password comparison
  const isValid = await bcrypt.compare(password, user.passwordHash);
  if (!isValid) {
    throw new Error('Invalid credentials');
  }
  
  // ✅ Include minimal claims
  const payload: TokenPayload = {
    userId: user.id,
    email: user.email
  };
  
  // ✅ Sign with strong algorithm
  const accessToken = jwt.sign(payload, JWT_SECRET, {
    expiresIn: JWT_EXPIRY,
    algorithm: 'HS256'
  });
  
  const refreshToken = jwt.sign(payload, JWT_SECRET, {
    expiresIn: REFRESH_EXPIRY,
    algorithm: 'HS256'
  });
  
  return { accessToken, refreshToken };
}

// ✅ Verify middleware
function authenticateToken(req: Request, res: Response, next: NextFunction) {
  const authHeader = req.headers['authorization'];
  const token = authHeader?.split(' ')[1];  // Bearer TOKEN
  
  if (!token) {
    return res.status(401).json({ error: 'Authentication required' });
  }
  
  try {
    const payload = jwt.verify(token, JWT_SECRET) as TokenPayload;
    req.user = payload;
    next();
  } catch (error) {
    return res.status(403).json({ error: 'Invalid or expired token' });
  }
}
```

**Bad:**
```typescript
// ❌ Insecure authentication
async function login(email: string, password: string) {
  const user = await User.findOne({ email });
  
  // ❌ Plain text password comparison!
  if (user.password !== password) {
    throw new Error('Invalid credentials');
  }
  
  // ❌ Long expiry
  // ❌ Includes sensitive data in token
  const token = jwt.sign({
    userId: user.id,
    email: user.email,
    password: user.password,  // Never include password!
    creditCard: user.creditCard  // Never include sensitive data!
  }, 'hardcoded-secret', {  // ❌ Hardcoded secret!
    expiresIn: '365d'  // ❌ Too long!
  });
  
  return token;
}
```

### Password Hashing

**Good:**
```typescript
// ✅ Use bcrypt with proper salt rounds
import bcrypt from 'bcrypt';

const SALT_ROUNDS = 12;  // Adjust based on performance requirements

async function hashPassword(password: string): Promise<string> {
  // ✅ bcrypt automatically handles salting
  return await bcrypt.hash(password, SALT_ROUNDS);
}

async function verifyPassword(password: string, hash: string): Promise<boolean> {
  return await bcrypt.compare(password, hash);
}

// ✅ Password requirements
function validatePassword(password: string): boolean {
  const minLength = 12;
  const hasUpperCase = /[A-Z]/.test(password);
  const hasLowerCase = /[a-z]/.test(password);
  const hasNumber = /[0-9]/.test(password);
  const hasSpecialChar = /[!@#$%^&*(),.?":{}|<>]/.test(password);
  
  return password.length >= minLength &&
         hasUpperCase &&
         hasLowerCase &&
         hasNumber &&
         hasSpecialChar;
}
```

**Bad:**
```python
# ❌ Insecure password handling
import hashlib

def hash_password(password):
    # ❌ MD5 is cryptographically broken
    return hashlib.md5(password.encode()).hexdigest()

def hash_password_v2(password):
    # ❌ SHA256 without salt is vulnerable to nghe tables
    return hashlib.sha256(password.encode()).hexdigest()
```

## Authorization

### Role-Based Access Control (RBAC)

**Good:**
```typescript
// ✅ Proper RBAC implementation

enum Permission {
  READ_USER = 'users:read',
  WRITE_USER = 'users:write',
  DELETE_USER = 'users:delete',
  READ_ADMIN = 'admin:read',
  WRITE_ADMIN = 'admin:write'
}

enum Role {
  USER = 'user',
  MODERATOR = 'moderator',
  ADMIN = 'admin'
}

const rolePermissions: Record<Role, Permission[]> = {
  [Role.USER]: [Permission.READ_USER],
  [Role.MODERATOR]: [Permission.READ_USER, Permission.WRITE_USER],
  [Role.ADMIN]: [
    Permission.READ_USER,
    Permission.WRITE_USER,
    Permission.DELETE_USER,
    Permission.READ_ADMIN,
    Permission.WRITE_ADMIN
  ]
};

// ✅ Authorization middleware
function requirePermission(...permissions: Permission[]) {
  return (req: Request, res: Response, next: NextFunction) => {
    const userRole = req.user?.role as Role;
    
    if (!userRole) {
      return res.status(401).json({ error: 'Authentication required' });
    }
    
    const userPermissions = rolePermissions[userRole] || [];
    const hasPermission = permissions.every(p => userPermissions.includes(p));
    
    if (!hasPermission) {
      return res.status(403).json({ error: 'Insufficient permissions' });
    }
    
    next();
  };
}

// Usage
app.delete('/api/users/:id',
  authenticateToken,
  requirePermission(Permission.DELETE_USER),
  deleteUser
);
```

### Resource-Level Authorization

**Good:**
```typescript
// ✅ Verify resource ownership
async function updatePost(req: Request, res: Response) {
  const postId = req.params.id;
  const userId = req.user!.userId;
  
  const post = await Post.findByPk(postId);
  
  if (!post) {
    return res.status(404).json({ error: 'Post not found' });
  }
  
  // ✅ Check ownership or admin role
  if (post.authorId !== userId && req.user!.role !== Role.ADMIN) {
    return res.status(403).json({ error: 'Not authorized to update this post' });
  }
  
  await post.update(req.body);
  return res.json(post);
}
```

## Input Validation

### Request Validation

**Good:**
```typescript
// ✅ Use validation library
import { z } from 'zod';

const createUserSchema = z.object({
  email: z.string().email().max(255),
  name: z.string().min(1).max(100),
  age: z.number().int().min(18).max(120).optional(),
  role: z.enum(['user', 'moderator', 'admin']).default('user')
});

app.post('/api/users', async (req, res) => {
  try {
    // ✅ Validate and parse
    const data = createUserSchema.parse(req.body);
    
    const user = await User.create(data);
    return res.status(201).json(user);
  } catch (error) {
    if (error instanceof z.ZodError) {
      return res.status(400).json({
        error: 'Validation error',
        details: error.errors
      });
    }
    throw error;
  }
});

// ✅ Express-validator alternative
import { body, validationResult } from 'express-validator';

app.post('/api/users',
  body('email').isEmail().normalizeEmail(),
  body('name').trim().isLength({ min: 1, max: 100 }),
  body('age').optional().isInt({ min: 18, max: 120 }),
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }
    
    // Process validated data
  }
);
```

**Bad:**
```typescript
// ❌ No validation
app.post('/api/users', async (req, res) => {
  // ❌ Directly using untrusted input
  const user = await User.create(req.body);
  return res.json(user);
});
```

### Path Traversal Prevention

**Good:**
```typescript
// ✅ Secure file handling
import path from 'path';
import fs from 'fs/promises';

const UPLOAD_DIR = '/var/app/uploads';

async function getFile(req: Request, res: Response) {
  const filename = req.params.filename;
  
  // ✅ Validate filename
  if (!/^[a-zA-Z0-9_-]+\.[a-zA-Z0-9]+$/.test(filename)) {
    return res.status(400).json({ error: 'Invalid filename' });
  }
  
  // ✅ Use path.join and resolve to prevent traversal
  const filePath = path.resolve(path.join(UPLOAD_DIR, filename));
  
  // ✅ Ensure path is within UPLOAD_DIR
  if (!filePath.startsWith(UPLOAD_DIR)) {
    return res.status(403).json({ error: 'Access denied' });
  }
  
  try {
    const data = await fs.readFile(filePath);
    return res.send(data);
  } catch (error) {
    return res.status(404).json({ error: 'File not found' });
  }
}
```

**Bad:**
```typescript
// ❌ Path traversal vulnerability
async function getFile(req: Request, res: Response) {
  const filename = req.params.filename;
  
  // ❌ Vulnerable to path traversal (../../etc/passwd)
  const filePath = `/var/app/uploads/${filename}`;
  const data = await fs.readFile(filePath);
  return res.send(data);
}
```

## SQL/NoSQL Injection Prevention

### SQL Injection

**Good:**
```typescript
// ✅ Parameterized queries
async function getUser(email: string) {
  // Sequelize (automatically parameterized)
  return await User.findOne({ where: { email } });
}

// ✅ Raw query with parameters
async function searchUsers(searchTerm: string) {
  const [results] = await sequelize.query(
    'SELECT * FROM users WHERE name LIKE :search OR email LIKE :search',
    {
      replacements: { search: `%${searchTerm}%` },
      type: QueryTypes.SELECT
    }
  );
  return results;
}

// ✅ Python with parameters
def get_user(email: str):
    cursor.execute(
        "SELECT * FROM users WHERE email = %s",
        (email,)
    )
    return cursor.fetchone()
```

**Bad:**
```typescript
// ❌ SQL injection vulnerability!
async function searchUsers(searchTerm: string) {
  const query = `SELECT * FROM users WHERE name = '${searchTerm}'`;
  const [results] = await sequelize.query(query);
  return results;
}
```

### NoSQL Injection

**Good:**
```typescript
// ✅ Sanitize MongoDB queries
import mongoSanitize from 'express-mongo-sanitize';

app.use(mongoSanitize());  // Remove $ and . from user input

async function getUser(email: string) {
  // ✅ Type checking
  if (typeof email !== 'string') {
    throw new Error('Invalid email');
  }
  
  return await User.findOne({ email });
}
```

**Bad:**
```typescript
// ❌ NoSQL injection vulnerability
async function login(req: Request, res: Response) {
  const { email, password } = req.body;
  
  // ❌ If email = { $ne: null }, this bypasses authentication!
  const user = await User.findOne({ email, password });
  
  if (user) {
    return res.json({ token: generateToken(user) });
  }
}
```

## Cryptography

### Data Encryption

**Good:**
```typescript
// ✅ Encrypt sensitive data at rest
import crypto from 'crypto';

const ALGORITHM = 'aes-256-gcm';
const KEY = Buffer.from(process.env.ENCRYPTION_KEY!, 'hex');  // 32 bytes

function encrypt(text: string): { encrypted: string; iv: string; authTag: string } {
  // ✅ Generate random IV for each encryption
  const iv = crypto.randomBytes(16);
  const cipher = crypto.createCipheriv(ALGORITHM, KEY, iv);
  
  let encrypted = cipher.update(text, 'utf8', 'hex');
  encrypted += cipher.final('hex');
  
  const authTag = cipher.getAuthTag();
  
  return {
    encrypted,
    iv: iv.toString('hex'),
    authTag: authTag.toString('hex')
  };
}

function decrypt(encrypted: string, ivHex: string, authTagHex: string): string {
  const iv = Buffer.from(ivHex, 'hex');
  const authTag = Buffer.from(authTagHex, 'hex');
  
  const decipher = crypto.createDecipheriv(ALGORITHM, KEY, iv);
  decipher.setAuthTag(authTag);
  
  let decrypted = decipher.update(encrypted, 'hex', 'utf8');
  decrypted += decipher.final('utf8');
  
  return decrypted;
}
```

### Random Token Generation

**Good:**
```typescript
// ✅ Cryptographically secure random tokens
import crypto from 'crypto';

function generateToken(length: number = 32): string {
  return crypto.randomBytes(length).toString('hex');
}

function generateResetToken(): string {
  return crypto.randomBytes(32).toString('hex');
}
```

**Bad:**
```typescript
// ❌ Predictable tokens
function generateToken(): string {
  return Math.random().toString(36).substring(7);  // NOT SECURE!
}
```

## Secret Management

**Good:**
```typescript
// ✅ Use environment variables
import dotenv from 'dotenv';
dotenv.config();

const config = {
  database: {
    host: process.env.DB_HOST!,
    password: process.env.DB_PASSWORD!,
  },
  jwt: {
    secret: process.env.JWT_SECRET!,
  },
  encryption: {
    key: process.env.ENCRYPTION_KEY!,
  }
};

// ✅ Validate required secrets at startup
function validateConfig() {
  const required = ['DB_HOST', 'DB_PASSWORD', 'JWT_SECRET', 'ENCRYPTION_KEY'];
  const missing = required.filter(key => !process.env[key]);
  
  if (missing.length > 0) {
    throw new Error(`Missing required environment variables: ${missing.join(', ')}`);
  }
}

validateConfig();

// ✅ Never log secrets
logger.info('Database connection', { host: config.database.host });  // OK
// ❌ logger.info('Database connection', config.database);  // Would log password!
```

**Bad:**
```typescript
// ❌ Hardcoded secrets
const config = {
  database: {
    password: 'MyPassword123!',  // ❌ Hardcoded!
  },
  jwt: {
    secret: 'super-secret-key',  // ❌ Hardcoded!
  }
};
```

## API Security

### Rate Limiting

**Good:**
```typescript
// ✅ Implement rate limiting
import rateLimit from 'express-rate-limit';

// General API limiter
const apiLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,  // 15 minutes
  max: 100,  // Limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP, please try again later',
  standardHeaders: true,
  legacyHeaders: false,
});

// Stricter limiter for authentication endpoints
const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 5,  // 5 attempts per 15 minutes
  skipSuccessfulRequests: true,  // Don't count successful logins
  message: 'Too many login attempts, please try again later'
});

app.use('/api/', apiLimiter);
app.use('/api/auth/login', authLimiter);
```

### CORS Configuration

**Good:**
```typescript
// ✅ Restrictive CORS
import cors from 'cors';

const allowedOrigins = [
  'https://example.com',
  'https://www.example.com'
];

app.use(cors({
  origin: (origin, callback) => {
    if (!origin || allowedOrigins.includes(origin)) {
      callback(null, true);
    } else {
      callback(new Error('Not allowed by CORS'));
    }
  },
  credentials: true,  // Allow cookies
  maxAge: 86400  // Cache preflight for 24 hours
}));
```

**Bad:**
```typescript
// ❌ Overly permissive CORS
app.use(cors({
  origin: '*',  // Allows all origins!
  credentials: true  // Dangerous with origin: '*'
}));
```

### Security Headers

**Good:**
```typescript
// ✅ Use helmet for security headers
import helmet from 'helmet';

app.use(helmet({
  contentSecurityPolicy: {
    directives: {
      defaultSrc: ["'self'"],
      styleSrc: ["'self'", "'unsafe-inline'"],
      scriptSrc: ["'self'"],
      imgSrc: ["'self'", 'data:', 'https:'],
    },
  },
  hsts: {
    maxAge: 31536000,
    includeSubDomains: true,
    preload: true
  }
}));
```

## Secure Dependencies

**Good:**
```bash
# ✅ Regularly audit dependencies
npm audit
npm audit fix

# ✅ Use automated tools
npm install -g snyk
snyk test
snyk monitor

# ✅ Keep dependencies updated
npm outdated
npm update
```

**Best Practices:**
1. Pin dependency versions in package.json
2. Review dependency licenses
3. Minimize number of dependencies
4. Use lock files (package-lock.json, yarn.lock)
5. Set up automated security alerts (Dependabot, Snyk)
