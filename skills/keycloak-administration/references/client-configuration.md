# Client Configuration

## Client Types

### 1. OpenID Connect (OIDC) Clients

- Standard flow: Authorization code flow (web applications)
- Implicit flow: Legacy, not recommended
- Direct access grants: Resource owner password credentials (use sparingly)
- Service accounts: Client credentials for machine-to-machine

### 2. SAML Clients

- SAML 2.0 protocol
- For legacy enterprise applications
- Supports SSO and SLO (Single Logout)

## Creating OIDC Clients

### Standard Web Application

1. Clients → Create Client
2. Client type: OpenID Connect
3. Client ID: Unique identifier (e.g., `my-app`)
4. Client protocol: openid-connect

**Configuration:**
- Root URL: Base application URL
- Valid redirect URIs: Allowed callback URLs (e.g., `https://app.example.com/callback`, `http://localhost:3000/callback`)
- Valid post logout redirect URIs: Allowed logout callbacks
- Web origins: CORS allowed origins (e.g., `https://app.example.com`)
- Admin URL: For backchannel communication

**Access Settings:**
- Standard flow: Enable for authorization code flow
- Direct access grants: Enable for password grant (caution)
- Implicit flow: Disable (deprecated)
- Service accounts: Enable for client credentials flow

**Authentication Flow:**
- Client authentication: On (confidential) or Off (public)
- Client authenticator: Client secret, JWT, or X509 certificate

**Advanced Settings:**
- Access token lifespan: Override realm default if needed
- Proof Key for Code Exchange (PKCE): Required for public clients
- OAuth 2.0 Device Authorization Grant: For limited-input devices

## Service Account Clients

**For machine-to-machine authentication:**
1. Create client with service accounts enabled
2. Disable standard/implicit flows
3. Assign service account roles in "Service Account Roles" tab
4. Use client_credentials grant type

## Client Scopes

**Purpose:** Control what information tokens contain

**Default Scopes:**
- openid: Required for OIDC
- profile: User profile information
- email: Email address
- address: Physical address
- phone: Phone number
- roles: User roles
- web-origins: CORS origins

**Custom Scopes:**
1. Client Scopes → Create
2. Add protocol mappers for custom claims
3. Assign to clients (default or optional)

## Protocol Mappers

- User attribute: Map user attributes to token claims
- User property: Map username, email, etc.
- Group membership: Include user groups
- Hardcoded claim: Static values
- Audience: Add audience claims

## Client Security Best Practices

### Confidential Clients

- Client authentication: ON
- Use client secrets or client certificates
- Rotate client secrets periodically
- Store secrets securely (vault, environment variables)

### Public Clients

- Client authentication: OFF
- Require PKCE (Proof Key for Code Exchange)
- Restrict redirect URIs strictly
- Set valid web origins for CORS

### Redirect URI Validation

- Use exact URLs (no wildcards in production)
- Avoid `localhost` in production configurations
- Never use `*` wildcard
- Use HTTPS only (except localhost development)
