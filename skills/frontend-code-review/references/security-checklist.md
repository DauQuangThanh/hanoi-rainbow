# Security Checklist

Comprehensive guidelines for frontend security best practices.

## Table of Contents
- [Cross-Site Scripting (XSS)](#cross-site-scripting-xss)
- [Cross-Site Request Forgery (CSRF)](#cross-site-request-forgery-csrf)
- [Authentication and Authorization](#authentication-and-authorization)
- [Content Security Policy (CSP)](#content-security-policy-csp)
- [Secure Communication](#secure-communication)
- [Input Validation](#input-validation)
- [Dependency Security](#dependency-security)
- [Data Protection](#data-protection)
- [Common Vulnerabilities](#common-vulnerabilities)

## Cross-Site Scripting (XSS)

### Types of XSS

1. **Stored XSS:** Malicious script stored in database
2. **Reflected XSS:** Malicious script in URL parameters
3. **DOM-based XSS:** Malicious script manipulates DOM

### XSS Prevention

#### React (Built-in Protection)

**Problem:**
```jsx
// ❌ Dangerous: Direct HTML injection
function UserComment({ comment }) {
  return <div dangerouslySetInnerHTML={{ __html: comment }} />;
}

// ❌ Dangerous: Inline event handlers from user input
function SearchResults({ query }) {
  return <div onClick={eval(query)}>Click me</div>;
}

// ❌ Dangerous: href with javascript:
function Link({ url }) {
  return <a href={url}>Click here</a>; // User provides "javascript:alert('XSS')"
}
```

**Solution:**
```jsx
// ✅ React automatically escapes by default
function UserComment({ comment }) {
  return <div>{comment}</div>; // Safe - React escapes HTML
}

// ✅ If HTML is necessary, sanitize first
import DOMPurify from 'dompurify';

function UserComment({ comment }) {
  const sanitized = DOMPurify.sanitize(comment, {
    ALLOWED_TAGS: ['b', 'i', 'em', 'strong', 'a'],
    ALLOWED_ATTR: ['href']
  });
  
  return <div dangerouslySetInnerHTML={{ __html: sanitized }} />;
}

// ✅ Validate and sanitize URLs
function Link({ url }) {
  const isSafe = /^https?:\/\//.test(url); // Only http/https
  
  if (!isSafe) {
    return null;
  }
  
  return <a href={url} rel="noopener noreferrer">Click here</a>;
}

// ✅ Better: Use allowlist
const ALLOWED_DOMAINS = ['example.com', 'trusted.com'];

function Link({ url }) {
  try {
    const urlObj = new URL(url);
    const isAllowed = ALLOWED_DOMAINS.some(domain => 
      urlObj.hostname === domain || urlObj.hostname.endsWith(`.${domain}`)
    );
    
    if (!isAllowed) {
      return null;
    }
    
    return <a href={url} rel="noopener noreferrer">Click here</a>;
  } catch {
    return null;
  }
}
```

#### Vue (Built-in Protection)

**Problem:**
```vue
<!-- ❌ Dangerous: v-html with user input -->
<template>
  <div v-html="userComment"></div>
</template>

<!-- ❌ Dangerous: Dynamic event handlers -->
<template>
  <div @click="eval(userInput)">Click me</div>
</template>
```

**Solution:**
```vue
<!-- ✅ Vue automatically escapes by default -->
<template>
  <div>{{ userComment }}</div>
</template>

<!-- ✅ If HTML is necessary, sanitize first -->
<template>
  <div v-html="sanitizedComment"></div>
</template>

<script setup>
import DOMPurify from 'dompurify';
import { computed } from 'vue';

const props = defineProps<{ userComment: string }>();

const sanitizedComment = computed(() => 
  DOMPurify.sanitize(props.userComment, {
    ALLOWED_TAGS: ['b', 'i', 'em', 'strong', 'a'],
    ALLOWED_ATTR: ['href']
  })
);
</script>
```

#### Angular (Built-in Protection)

**Problem:**
```typescript
// ❌ Dangerous: Bypassing sanitization
import { DomSanitizer } from '@angular/platform-browser';

@Component({
  template: `<div [innerHTML]="unsafeHtml"></div>`
})
export class UnsafeComponent {
  constructor(private sanitizer: DomSanitizer) {}
  
  unsafeHtml = this.sanitizer.bypassSecurityTrustHtml(userInput);
}
```

**Solution:**
```typescript
// ✅ Angular automatically sanitizes
@Component({
  template: `<div [innerHTML]="userInput"></div>` // Safe - Angular sanitizes
})
export class SafeComponent {
  userInput = '<script>alert("XSS")</script>'; // Will be sanitized
}

// ✅ If you need to bypass, use DOMPurify first
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import DOMPurify from 'dompurify';

@Component({
  template: `<div [innerHTML]="sanitizedHtml"></div>`
})
export class SaferComponent {
  sanitizedHtml: SafeHtml;
  
  constructor(private sanitizer: DomSanitizer) {
    const purified = DOMPurify.sanitize(userInput);
    this.sanitizedHtml = this.sanitizer.bypassSecurityTrustHtml(purified);
  }
}
```

### Content Sanitization

```typescript
// ✅ Comprehensive sanitization utility
import DOMPurify from 'dompurify';

interface SanitizeOptions {
  allowedTags?: string[];
  allowedAttributes?: Record<string, string[]>;
  allowDataAttributes?: boolean;
}

function sanitizeHTML(
  dirty: string,
  options: SanitizeOptions = {}
): string {
  const config = {
    ALLOWED_TAGS: options.allowedTags || [
      'p', 'br', 'strong', 'em', 'u', 'h1', 'h2', 'h3',
      'ul', 'ol', 'li', 'a', 'blockquote', 'code'
    ],
    ALLOWED_ATTR: options.allowedAttributes || {
      'a': ['href', 'title', 'target'],
      '*': options.allowDataAttributes ? ['data-*'] : []
    },
    ALLOW_DATA_ATTR: options.allowDataAttributes || false,
    ALLOW_UNKNOWN_PROTOCOLS: false,
    SAFE_FOR_TEMPLATES: true,
  };
  
  return DOMPurify.sanitize(dirty, config);
}

// Usage
const userInput = '<script>alert("XSS")</script><p>Hello</p>';
const safe = sanitizeHTML(userInput); // Returns: '<p>Hello</p>'
```

## Cross-Site Request Forgery (CSRF)

### CSRF Prevention

**Problem:**
```typescript
// ❌ No CSRF protection
async function deleteAccount() {
  await fetch('/api/account/delete', {
    method: 'POST',
    credentials: 'include' // Sends cookies automatically
  });
}
```

**Solution:**
```typescript
// ✅ CSRF token in headers
async function deleteAccount() {
  const csrfToken = getCsrfToken(); // From meta tag or cookie
  
  await fetch('/api/account/delete', {
    method: 'POST',
    credentials: 'include',
    headers: {
      'X-CSRF-Token': csrfToken,
      'Content-Type': 'application/json'
    }
  });
}

// ✅ Get CSRF token from meta tag
function getCsrfToken(): string {
  const meta = document.querySelector('meta[name="csrf-token"]');
  return meta?.getAttribute('content') || '';
}

// ✅ Or from cookie
function getCsrfTokenFromCookie(): string {
  const match = document.cookie.match(/XSRF-TOKEN=([^;]+)/);
  return match ? decodeURIComponent(match[1]) : '';
}

// ✅ Axios automatically handles XSRF-TOKEN cookie
import axios from 'axios';

const api = axios.create({
  baseURL: '/api',
  xsrfCookieName: 'XSRF-TOKEN',
  xsrfHeaderName: 'X-XSRF-TOKEN'
});
```

### SameSite Cookies

```typescript
// ✅ Server should set SameSite attribute (client can verify)
// Server-side cookie setting:
// Set-Cookie: sessionId=xyz; SameSite=Strict; Secure; HttpOnly

// Client-side verification (for debugging)
function checkCookieSecurity() {
  // Note: Can't read HttpOnly cookies in JS (that's good!)
  const cookies = document.cookie.split(';');
  
  console.warn('Visible cookies (should be minimal):', cookies);
  
  // Check if secure context
  if (!window.isSecureContext) {
    console.error('Not in secure context - cookies may not be protected');
  }
}
```

## Authentication and Authorization

### Token Storage

**Problem:**
```typescript
// ❌ Storing tokens in localStorage (vulnerable to XSS)
function login(credentials) {
  const response = await fetch('/api/login', {
    method: 'POST',
    body: JSON.stringify(credentials)
  });
  
  const { token } = await response.json();
  localStorage.setItem('token', token); // Vulnerable to XSS!
}
```

**Solution:**
```typescript
// ✅ Option 1: HttpOnly cookies (preferred)
// Server sets: Set-Cookie: token=xyz; HttpOnly; Secure; SameSite=Strict
async function login(credentials) {
  await fetch('/api/login', {
    method: 'POST',
    credentials: 'include', // Include cookies
    body: JSON.stringify(credentials)
  });
  // Token is in HttpOnly cookie - not accessible to JS
}

// ✅ Option 2: In-memory storage (for SPAs)
class AuthService {
  private token: string | null = null;
  
  async login(credentials: Credentials) {
    const response = await fetch('/api/login', {
      method: 'POST',
      body: JSON.stringify(credentials)
    });
    
    const { token } = await response.json();
    this.token = token; // Stored in memory only
  }
  
  getToken(): string | null {
    return this.token;
  }
  
  logout() {
    this.token = null;
  }
}

// ✅ Option 3: sessionStorage (better than localStorage, but still vulnerable to XSS)
// Only use if you must store in storage
function login(credentials) {
  const response = await fetch('/api/login', {
    method: 'POST',
    body: JSON.stringify(credentials)
  });
  
  const { token } = await response.json();
  sessionStorage.setItem('token', token); // Cleared on tab close
}
```

### JWT Best Practices

**Problem:**
```typescript
// ❌ Storing sensitive data in JWT
const token = {
  userId: 123,
  password: 'secret123', // Never!
  creditCard: '1234-5678', // Never!
  role: 'admin'
};

// ❌ No expiration
const token = jwt.sign({ userId: 123 }, secret);

// ❌ Not verifying token expiration
function isAuthenticated() {
  const token = getToken();
  return !!token; // Not checking expiration!
}
```

**Solution:**
```typescript
// ✅ Only store non-sensitive data with expiration
// Server-side JWT creation:
const token = jwt.sign(
  {
    userId: 123,
    role: 'admin',
    exp: Math.floor(Date.now() / 1000) + (60 * 60) // 1 hour
  },
  secret,
  { algorithm: 'HS256' }
);

// ✅ Client-side token validation
import jwtDecode from 'jwt-decode';

interface TokenPayload {
  userId: number;
  role: string;
  exp: number;
}

function isTokenValid(token: string): boolean {
  try {
    const decoded = jwtDecode<TokenPayload>(token);
    const currentTime = Date.now() / 1000;
    
    return decoded.exp > currentTime;
  } catch {
    return false;
  }
}

// ✅ Automatic token refresh
class AuthService {
  private token: string | null = null;
  private refreshTimer: number | null = null;
  
  async login(credentials: Credentials) {
    const { token, refreshToken } = await fetchToken(credentials);
    this.setToken(token);
    this.scheduleRefresh(token);
  }
  
  private scheduleRefresh(token: string) {
    const decoded = jwtDecode<TokenPayload>(token);
    const expiresIn = (decoded.exp * 1000) - Date.now();
    const refreshAt = expiresIn - (5 * 60 * 1000); // 5 min before expiration
    
    this.refreshTimer = window.setTimeout(() => {
      this.refreshToken();
    }, refreshAt);
  }
  
  private async refreshToken() {
    const { token } = await fetch('/api/refresh', {
      method: 'POST',
      credentials: 'include'
    }).then(r => r.json());
    
    this.setToken(token);
    this.scheduleRefresh(token);
  }
}
```

### Role-Based Access Control (RBAC)

```typescript
// ✅ Secure permission checking
interface User {
  id: number;
  role: 'admin' | 'user' | 'guest';
  permissions: string[];
}

function hasPermission(user: User, permission: string): boolean {
  return user.permissions.includes(permission);
}

// ✅ React protected route
function ProtectedRoute({ 
  children, 
  requiredPermission 
}: { 
  children: React.ReactNode;
  requiredPermission: string;
}) {
  const user = useUser();
  
  if (!user) {
    return <Navigate to="/login" />;
  }
  
  if (!hasPermission(user, requiredPermission)) {
    return <Navigate to="/unauthorized" />;
  }
  
  return <>{children}</>;
}

// Usage
<Route path="/admin" element={
  <ProtectedRoute requiredPermission="admin.access">
    <AdminPanel />
  </ProtectedRoute>
} />

// ⚠️ Important: Always verify permissions on the server!
// Client-side checks are for UX only, not security
```

## Content Security Policy (CSP)

### CSP Headers

**Problem:**
```html
<!-- ❌ No CSP -->
<html>
  <head>
    <script src="https://random-cdn.com/script.js"></script>
  </head>
</html>
```

**Solution:**
```html
<!-- ✅ Strict CSP -->
<html>
  <head>
    <meta http-equiv="Content-Security-Policy" content="
      default-src 'self';
      script-src 'self' 'nonce-{random}' https://trusted-cdn.com;
      style-src 'self' 'nonce-{random}';
      img-src 'self' data: https:;
      font-src 'self' https://fonts.gstatic.com;
      connect-src 'self' https://api.example.com;
      frame-ancestors 'none';
      base-uri 'self';
      form-action 'self';
      upgrade-insecure-requests;
    ">
  </head>
</html>
```

### Using Nonces with CSP

```jsx
// ✅ Next.js with CSP nonces
// middleware.ts
import { NextResponse } from 'next/server';
import { v4 as uuidv4 } from 'uuid';

export function middleware(request) {
  const nonce = uuidv4();
  const cspHeader = `
    default-src 'self';
    script-src 'self' 'nonce-${nonce}' 'strict-dynamic';
    style-src 'self' 'nonce-${nonce}';
    img-src 'self' blob: data:;
    font-src 'self';
    connect-src 'self';
    frame-ancestors 'none';
  `;
  
  const response = NextResponse.next();
  response.headers.set('Content-Security-Policy', cspHeader);
  response.headers.set('X-Nonce', nonce);
  
  return response;
}

// ✅ Use nonce in components
function Page() {
  const nonce = headers().get('X-Nonce');
  
  return (
    <html>
      <head>
        <script nonce={nonce} src="/script.js" />
      </head>
    </html>
  );
}
```

## Secure Communication

### HTTPS Only

**Problem:**
```typescript
// ❌ Mixed content (HTTPS page loading HTTP resources)
function loadData() {
  fetch('http://api.example.com/data'); // Insecure!
}

// ❌ No validation of HTTPS
<img src={userProvidedUrl} />
```

**Solution:**
```typescript
// ✅ Always use HTTPS
function loadData() {
  fetch('https://api.example.com/data');
}

// ✅ Validate HTTPS URLs
function isSecureUrl(url: string): boolean {
  try {
    const urlObj = new URL(url);
    return urlObj.protocol === 'https:';
  } catch {
    return false;
  }
}

function SecureImage({ src }: { src: string }) {
  if (!isSecureUrl(src)) {
    return <div>Invalid image URL</div>;
  }
  
  return <img src={src} />;
}

// ✅ Upgrade insecure requests with CSP
// Content-Security-Policy: upgrade-insecure-requests
```

### API Security

```typescript
// ✅ Secure API client
class SecureApiClient {
  private baseURL = 'https://api.example.com';
  private timeout = 10000;
  
  async request<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<T> {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), this.timeout);
    
    try {
      const response = await fetch(`${this.baseURL}${endpoint}`, {
        ...options,
        signal: controller.signal,
        credentials: 'include', // Send cookies
        headers: {
          'Content-Type': 'application/json',
          'X-CSRF-Token': this.getCsrfToken(),
          ...options.headers,
        },
      });
      
      clearTimeout(timeoutId);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }
      
      return response.json();
    } catch (error) {
      if (error.name === 'AbortError') {
        throw new Error('Request timeout');
      }
      throw error;
    }
  }
  
  private getCsrfToken(): string {
    return document
      .querySelector('meta[name="csrf-token"]')
      ?.getAttribute('content') || '';
  }
}
```

## Input Validation

### Client-Side Validation

**Remember: Client-side validation is for UX, not security. Always validate on server!**

```typescript
// ✅ Comprehensive input validation
interface ValidationRule {
  pattern?: RegExp;
  minLength?: number;
  maxLength?: number;
  custom?: (value: string) => boolean;
  message: string;
}

function validateInput(
  value: string,
  rules: ValidationRule[]
): string | null {
  for (const rule of rules) {
    if (rule.minLength && value.length < rule.minLength) {
      return rule.message;
    }
    
    if (rule.maxLength && value.length > rule.maxLength) {
      return rule.message;
    }
    
    if (rule.pattern && !rule.pattern.test(value)) {
      return rule.message;
    }
    
    if (rule.custom && !rule.custom(value)) {
      return rule.message;
    }
  }
  
  return null;
}

// Usage
const emailRules: ValidationRule[] = [
  {
    pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
    message: 'Invalid email format'
  },
  {
    maxLength: 254,
    message: 'Email too long'
  }
];

const passwordRules: ValidationRule[] = [
  {
    minLength: 12,
    message: 'Password must be at least 12 characters'
  },
  {
    pattern: /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[@$!%*?&])/,
    message: 'Password must contain uppercase, lowercase, number, and special character'
  }
];

function SignupForm() {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [errors, setErrors] = useState<Record<string, string>>({});
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    
    const emailError = validateInput(email, emailRules);
    const passwordError = validateInput(password, passwordRules);
    
    if (emailError || passwordError) {
      setErrors({
        email: emailError || '',
        password: passwordError || ''
      });
      return;
    }
    
    // Submit to server (which will validate again!)
    await signup({ email, password });
  };
  
  return (/* form */);
}
```

## Dependency Security

### Regular Audits

```bash
# Check for vulnerabilities
npm audit

# Fix automatically (if possible)
npm audit fix

# Check for outdated packages
npm outdated

# Use Snyk for continuous monitoring
npm install -g snyk
snyk test
snyk monitor
```

### Secure Dependencies

```json
// package.json
{
  "scripts": {
    "preinstall": "npx npm-force-resolutions",
    "audit": "npm audit --audit-level=high"
  },
  "resolutions": {
    "vulnerable-package": "^2.0.0"
  }
}
```

## Data Protection

### Sensitive Data Handling

**Problem:**
```typescript
// ❌ Logging sensitive data
console.log('User logged in:', { email, password });

// ❌ Exposing sensitive data in client
function UserProfile({ user }) {
  return (
    <div>
      <p>SSN: {user.ssn}</p>
      <p>Credit Card: {user.creditCard}</p>
    </div>
  );
}
```

**Solution:**
```typescript
// ✅ Never log sensitive data
console.log('User logged in:', { email }); // Omit password

// ✅ Never send sensitive data to client
// Server should filter sensitive fields
interface PublicUser {
  id: number;
  email: string;
  name: string;
  // ssn, creditCard, etc. NOT included
}

// ✅ Mask sensitive data if must display
function maskCreditCard(cc: string): string {
  return `****-****-****-${cc.slice(-4)}`;
}

// ✅ Use separate types for client vs server
interface ServerUser {
  id: number;
  email: string;
  passwordHash: string; // Never send to client!
  ssn: string; // Never send to client!
}

interface ClientUser {
  id: number;
  email: string;
  name: string;
}
```

## Common Vulnerabilities

### 1. Open Redirects

**Problem:**
```typescript
// ❌ Unvalidated redirect
function handleRedirect(url: string) {
  window.location.href = url; // Can redirect to evil.com!
}

// Usage: /login?redirect=https://evil.com
```

**Solution:**
```typescript
// ✅ Validate redirect URLs
const ALLOWED_REDIRECT_DOMAINS = ['example.com'];

function isAllowedRedirect(url: string): boolean {
  try {
    const urlObj = new URL(url, window.location.origin);
    
    // Only allow same origin or allowed domains
    if (urlObj.origin === window.location.origin) {
      return true;
    }
    
    return ALLOWED_REDIRECT_DOMAINS.some(domain =>
      urlObj.hostname === domain || urlObj.hostname.endsWith(`.${domain}`)
    );
  } catch {
    return false;
  }
}

function handleRedirect(url: string) {
  if (isAllowedRedirect(url)) {
    window.location.href = url;
  } else {
    window.location.href = '/'; // Default safe redirect
  }
}
```

### 2. Clickjacking

**Problem:**
```html
<!-- ❌ No frame protection -->
<html>
  <body>
    <button>Delete Account</button>
  </body>
</html>
```

**Solution:**
```html
<!-- ✅ Prevent framing -->
<html>
  <head>
    <meta http-equiv="X-Frame-Options" content="DENY">
    <!-- Or use CSP -->
    <meta http-equiv="Content-Security-Policy" content="frame-ancestors 'none'">
  </head>
  <body>
    <button>Delete Account</button>
  </body>
</html>
```

```typescript
// ✅ JavaScript frame-busting (defense in depth)
if (window.top !== window.self) {
  window.top.location = window.self.location;
}
```

### 3. Prototype Pollution

**Problem:**
```typescript
// ❌ Dangerous object manipulation
function merge(target: any, source: any) {
  for (const key in source) {
    target[key] = source[key]; // Can pollute prototype!
  }
  return target;
}

// Attack:
merge({}, JSON.parse('{"__proto__": {"isAdmin": true}}'));
// Now ALL objects have isAdmin: true!
```

**Solution:**
```typescript
// ✅ Safe object merging
function safeMerge<T extends object>(target: T, source: Partial<T>): T {
  return Object.assign({}, target, source);
}

// ✅ Or use libraries with protection
import merge from 'lodash/merge'; // Has prototype pollution protection

// ✅ Filter dangerous keys
const DANGEROUS_KEYS = ['__proto__', 'constructor', 'prototype'];

function isValidKey(key: string): boolean {
  return !DANGEROUS_KEYS.includes(key);
}

function safeMerge2(target: any, source: any) {
  for (const key in source) {
    if (isValidKey(key) && Object.hasOwn(source, key)) {
      target[key] = source[key];
    }
  }
  return target;
}
```
