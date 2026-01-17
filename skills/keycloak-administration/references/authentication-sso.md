# Authentication & Single Sign-On (SSO)

## Authentication Flows

### Browser Flow (default login)

1. Cookie check
2. Kerberos (if configured)
3. Identity Provider redirector
4. Username/password form
5. OTP (if MFA enabled)
6. WebAuthn (if configured)

### Direct Grant Flow (API login)

1. Username/password validation
2. OTP validation (if required)
3. Conditional OTP

### Registration Flow

1. Profile validation
2. Password validation
3. reCAPTCHA (if configured)
4. Terms and conditions

### Customizing Flows

1. Authentication → Flows
2. Duplicate existing flow (don't modify built-in flows)
3. Add/remove/reorder authenticators
4. Set requirements: Required, Alternative, Disabled, Conditional
5. Bind custom flow to realm (Browser Flow, Direct Grant Flow, etc.)

## Multi-Factor Authentication (MFA)

### OTP (Time-based)

1. Authentication → Required Actions → Enable "Configure OTP"
2. Users configure with authenticator apps (Google Authenticator, Authy)
3. Recovery codes: Generate backup codes

### WebAuthn (FIDO2)

1. Enable WebAuthn authenticators in authentication flow
2. Users register hardware keys or biometrics
3. Passwordless authentication option

### Conditional MFA

- Configure "Conditional OTP" in authentication flow
- Set conditions: IP ranges, user attributes, authentication age

## Single Sign-On (SSO)

### OIDC SSO Configuration

1. Configure OIDC client for each application
2. Set valid redirect URIs
3. Applications share SSO session via cookies
4. Configure session timeouts appropriately

### SAML SSO Configuration

1. Create SAML client
2. Download/provide SAML metadata
3. Configure assertion consumer service URL
4. Set up attribute mappings
5. Configure name ID format (email, username, persistent)

### SSO Session Management

- SSO Session Idle: Extend on activity
- SSO Session Max: Absolute limit
- Remember Me: Long-lived sessions
- Single Logout: Centralized logout across applications

## Identity Brokering

**Purpose:** Allow users to login with external identity providers

### Configure Identity Provider

1. Identity Providers → Add provider (Google, Facebook, OIDC, SAML)
2. Set client ID and secret (from provider)
3. Configure scopes and claim mappings
4. Set default scopes: `openid profile email`

### Common Providers

**Google:**
- Client ID and Secret from Google Cloud Console
- Authorized redirect URI: `https://keycloak.example.com/realms/[realm]/broker/google/endpoint`

**Azure AD (OIDC):**
- Register application in Azure AD
- Set redirect URI
- Configure groups/claims

**SAML Identity Provider:**
- Import SAML metadata from provider
- Configure attribute mappings
- Set name ID format

### Identity Provider Mappers

- Map external claims to KeyCloak user attributes
- Attribute importer: Import specific claims
- Hardcoded attribute: Set fixed values for users from this IdP
- Username template: Define username format

## Social Login

### Supported Providers

Google, Facebook, GitHub, LinkedIn, Twitter, Microsoft, Apple, Instagram, PayPal, Stack Overflow

### Configuration Steps

1. Create OAuth application with provider
2. Set callback URL: `https://keycloak.example.com/realms/[realm]/broker/[provider]/endpoint`
3. Copy client ID and secret to KeyCloak
4. Configure scopes (typically `openid profile email`)
5. Enable on login screen
