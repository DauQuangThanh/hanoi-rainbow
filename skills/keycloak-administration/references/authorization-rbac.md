# Authorization & Role-Based Access Control (RBAC)

## Roles

### Realm Roles

- Global roles across all clients in realm
- Example: `admin`, `user`, `manager`

### Client Roles

- Specific to individual clients
- Example: `app-admin`, `app-viewer`

### Creating Roles

1. Realm Settings → Roles → Create Role
2. Set role name and description
3. Add composite roles (role inherits other roles)

### Composite Roles

- Aggregate multiple roles into one
- Example: `admin` role includes `user` and `manager` roles
- Simplifies role assignment

### Default Roles

- Automatically assigned to new users
- Set in Realm Settings → Roles → Default Roles

## Role Mapping

### Assign Roles to Users

1. Users → Select user → Role Mappings
2. Assign realm roles and/or client roles
3. View effective roles (includes inherited roles)

### Assign Roles to Groups

1. Groups → Select group → Role Mappings
2. All group members inherit these roles
3. Preferred method for scalable access control

## Fine-Grained Authorization (UMA)

**Purpose:** Resource-level authorization with policies

### Enable Authorization

1. Client → Authorization tab → Enable
2. Define resources, scopes, and policies
3. Evaluate permissions at runtime

### Authorization Components

**Resources:** Protected objects (e.g., `/api/documents`, `document-123`)

**Scopes:** Actions on resources (e.g., `read`, `write`, `delete`)

**Policies:** Rules defining access (role-based, time-based, user-based, JavaScript)

**Permissions:** Connect resources/scopes to policies

### Policy Types

- **Role policy**: Based on realm/client roles
- **User policy**: Specific users
- **Group policy**: Group membership
- **Time policy**: Date/time restrictions
- **JavaScript policy**: Custom logic
- **Aggregated policy**: Combine multiple policies

### Example Authorization Flow

1. Define resource: `/api/documents/{id}`
2. Define scopes: `read`, `write`, `delete`
3. Create policy: "Only document owners can delete"
4. Create permission: Connect `delete` scope to policy
5. Application enforces by querying KeyCloak authorization endpoint

## Authorization Best Practices

- Use realm roles for global permissions
- Use client roles for application-specific permissions
- Assign roles to groups, not individual users
- Use composite roles to simplify management
- Implement fine-grained authorization for complex requirements
- Document role hierarchies and permissions
- Regular role audits and cleanup
