# Realm Management

## Creating and Configuring Realms

**Realm Hierarchy:**
- Master realm: Administrative realm (do not use for applications)
- Application realms: Create separate realms per application/environment

**Create New Realm:**
1. Admin Console → Realm dropdown → Create Realm
2. Set realm name (e.g., `production`, `staging`, `app-name`)
3. Enable/disable realm as needed

## Essential Realm Settings

### 1. General Settings

- Display name: User-friendly name
- HTML display name: Branded name with styling
- Frontend URL: Public-facing URL for realm
- Require SSL: All requests (production) or external requests (dev)

### 2. Login Settings

- User registration: Enable if public registration allowed
- Edit username: Allow/disallow username changes
- Forgot password: Enable password reset flow
- Remember me: Session persistence option
- Verify email: Require email verification for new users
- Login with email: Allow email as username

### 3. Email Settings

- From: Display name and email address
- Reply To: Support email address
- Envelope From: Technical sender address

### 4. Themes

- Login theme: Customize login pages
- Account theme: User account management UI
- Admin console theme: Admin UI appearance
- Email theme: Email template styling

### 5. Token Settings

- Access token lifespan: 5-15 minutes (default: 5 min)
- SSO session idle: 30 minutes
- SSO session max: 10 hours
- Client session idle: 30 minutes
- Offline session idle: 30 days

### 6. Session Management

- SSO Session Idle: Inactivity timeout
- SSO Session Max: Absolute session timeout
- Offline Session Idle: Remember-me duration

## User & Group Administration

### User Management

**Create Users:**
1. Realm → Users → Add User
2. Set username (required), email, first/last name
3. Enable/disable user account
4. Email verified: Mark as verified or require verification
5. Required actions: Set password, verify email, update profile, etc.

**User Attributes:**
- Username: Unique identifier (immutable if configured)
- Email: Must be unique if email as username enabled
- First Name / Last Name: Display names
- Custom attributes: Key-value pairs for application metadata

**Manage Credentials:**
- Password: Temporary (user must change) or permanent
- OTP (One-Time Password): TOTP/HOTP configuration
- WebAuthn: Hardware security keys, biometrics
- Reset password: Admin-initiated or user self-service

**User Actions:**
- Send verify email
- Send password reset
- Impersonate user (for troubleshooting)
- View sessions and events
- Assign roles and groups

### Group Management

**Create Groups:**
1. Realm → Groups → Create Group
2. Set group name and attributes
3. Create subgroups for hierarchical organization

**Group Features:**
- Hierarchical structure: Parent/child relationships
- Attribute inheritance: Child groups inherit parent attributes
- Role mapping: Assign realm/client roles to groups
- Default groups: Auto-assign to new users

**Best Practices:**
- Use groups for organizational structure (departments, teams)
- Use roles for permissions and access control
- Assign roles to groups, not individual users when possible
- Leverage group hierarchy for inherited permissions
